# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Author J. Henrichs, Bureau of Meteorology

'''This module contains the ModuleInfo class, which is used to store
and cache information about a module: the filename, source code (if requested)
and the fparser tree (if requested), and information about routine it
includes, and external symbol usage.
'''

import os

from fparser.common.readfortran import FortranStringReader
from fparser.two.Fortran2003 import (Function_Subprogram, Interface_Block,
                                     Interface_Stmt, Procedure_Stmt,
                                     Subroutine_Subprogram, Use_Stmt)
from fparser.two.parser import ParserFactory
from fparser.two.utils import walk

from psyclone.errors import InternalError, PSycloneError
from psyclone.psyir.nodes import FileContainer, Routine
from psyclone.parse.routine_info import GenericRoutineInfo, RoutineInfo
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.symbols import SymbolError


# ============================================================================
class ModuleInfoError(PSycloneError):
    '''
    PSyclone-specific exception for use when an error with the module manager
    happens - typically indicating that some module information cannot be
    found.

    :param str value: the message associated with the error.

    '''
    def __init__(self, value):
        PSycloneError.__init__(self, value)
        self.value = "ModuleInfo error: "+str(value)


# ============================================================================
class ModuleInfo:
    # pylint: disable=too-many-instance-attributes
    '''This class stores and cahces  information about modules: it stores
    the original filename, if requested it will read the file and then caches
    the plain text file, and if required it will parse the file, and then
    cache the fparser AST.

    :param str name: the module name.
    :param str filename: the name of the source file that stores this module \
        (including path).

    '''

    def __init__(self, name, filename):
        self._name = name
        self._filename = filename
        # A cache for the source code:
        self._source_code = None

        # A cache for the fparser tree
        self._parse_tree = None

        # A cache for the PSyIR representation
        self._psyir = None

        # A cache for the module dependencies: this is just a set
        # of all modules used by this module. Type: Set[str]
        self._used_modules = None

        # This is a dictionary containing the sets of symbols imported from
        # each module, indexed by the module names: Dict[str, Set(str)].
        self._used_symbols_from_module = None

        # This dictionary will store the mapping of routine name to
        # RoutineInfo objects.
        self._routine_info = None

        # This is a dictionary that will cache non-local symbols used in
        # each routine. The key is the lowercase routine name, and the
        # value is a list of triplets:
        # - the type ('subroutine', 'function', 'reference', 'unknown').
        #   The latter is used for array references or function calls,
        #   which we cannot distinguish till #1314 is done.
        # - the name of the module (lowercase)
        # - the name of the symbol (lowercase)
        self._routine_non_locals = None

        self._processor = Fparser2Reader()

    # ------------------------------------------------------------------------
    @property
    def name(self):
        ''':returns: the name of this module.
        :rtype: str

        '''
        return self._name

    # ------------------------------------------------------------------------
    @property
    def filename(self):
        ''':returns: the filename that contains the source code for this \
            module.
        :rtype: str

        '''
        return self._filename

    # ------------------------------------------------------------------------
    def get_source_code(self):
        '''Returns the source code for the module. The first time, it
        will be read from the file, but the data is then cached.

        :returns: the source code.
        :rtype: str

        :raises ModuleInfoError: when the file cannot be read.

        '''
        if self._source_code is None:
            try:
                with open(self._filename, "r", encoding='utf-8') as file_in:
                    self._source_code = file_in.read()
            except FileNotFoundError as err:
                raise ModuleInfoError(
                    f"Could not find file '{self._filename}' when trying to "
                    f"read source code for module '{self._name}'") from err

        return self._source_code

    # ------------------------------------------------------------------------
    def get_parse_tree(self):
        '''Returns the fparser AST for this module. The first time, the file
        will be parsed by fparser using the Fortran 2008 standard. The AST is
        then cached for any future uses.

        :returns: the fparser AST for this module.
        :rtype: :py:class:`fparser.two.Fortran2003.Program`

        '''
        if not self._parse_tree:
            reader = FortranStringReader(self.get_source_code())
            parser = ParserFactory().create(std="f2008")
            self._parse_tree = parser(reader)
            self._routine_info = {}
            # First collect information about all subroutines/functions.
            # Store information about generic interface to be handled later
            # (so we only walk the tree once):
            all_generic_interfaces = []
            for routine in walk(self._parse_tree, (Function_Subprogram,
                                                   Subroutine_Subprogram,
                                                   Interface_Block)):
                if isinstance(routine, Interface_Block):
                    all_generic_interfaces.append(routine)
                    continue
                routine_info = RoutineInfo(self, routine)
                self._routine_info[routine_info.name.lower()] = routine_info

            # Then handle all generic interfaces, which will internally
            # use references to the RoutineInfo objects collected above:
            for interface in all_generic_interfaces:
                # Get the name of the interface from the Interface_Stmt:
                name = str(walk(interface, Interface_Stmt)[0].items[0])
                # Now collect all specific functions
                routine_names = []
                for proc_stmt in walk(interface, Procedure_Stmt):
                    # Convert the items to strings:
                    routine_names.extend([str(i) for i in
                                          proc_stmt.items[0].items])
                # Create a GenericRoutineInfo object to store this information:
                generic_info = GenericRoutineInfo(self, name, routine_names)
                self._routine_info[name.lower()] = generic_info

        return self._parse_tree

    # ------------------------------------------------------------------------
    def get_routine_info(self, routine_name):
        '''Returns the routine information for the specified routine name.

        :param str routine_name: the name of the routine.

        :returns: the RoutineInfo object for the specified routine.
        :rtype: :py:class:`psyclone.parse.RoutineInfo`

        '''
        if self._routine_info is None:
            # This will trigger adding routine information:
            self.get_parse_tree()

        return self._routine_info[routine_name.lower()]

    # ------------------------------------------------------------------------
    def contains_routine(self, routine_name):
        ''':returns: whether the specified routine name is part of this \
            module or not.
        :rtype: bool

        '''
        if self._routine_info is None:
            # This will trigger adding routine information
            self.get_parse_tree()

        return routine_name.lower() in self._routine_info

    # ------------------------------------------------------------------------
    def _extract_import_information(self):
        '''This internal function analyses a given module source file and
        caches which modules are imported (in self._used_modules), and which
        symbol is imported from each of these modules (in
        self._used_symbols_from_module).

        '''
        if self._used_modules is not None or \
                self._used_symbols_from_module is not None:
            raise ModuleInfoError(f"_extract_import_information for "
                                  f"'{self._name}' should not be called "
                                  f"twice.")
        # Initialise the caches:
        self._used_modules = set()
        self._used_symbols_from_module = {}

        parse_tree = self.get_parse_tree()
        for use in walk(parse_tree, Use_Stmt):
            # Ignore intrinsic modules:
            if str(use.items[0]) == "INTRINSIC":
                continue

            mod_name = str(use.items[2])
            self._used_modules.add(mod_name)
            all_symbols = set()

            only_list = use.items[4]
            # If there is no only_list, then the set of symbols
            # will stay empty
            if only_list:
                # Parse the only list:
                for symbol in only_list.children:
                    all_symbols.add(str(symbol))

            self._used_symbols_from_module[mod_name] = all_symbols

    # ------------------------------------------------------------------------
    def get_used_modules(self):
        '''This function returns a set of all modules `used` in this
        module. Fortran `intrinsic` modules will be ignored. The information
        is based on the fparser parse tree of the module (since fparser can
        handle more files than PSyir, like LFRic's `constants_mod` which has
        pre-processor directives).

        :returns: a set with all imported module names.
        :rtype: Set[str]

        '''
        if self._used_modules is None:
            self._extract_import_information()

        return self._used_modules

    # ------------------------------------------------------------------------
    def get_used_symbols_from_modules(self):
        '''This function returns information about which modules are used by
        this module, and also which symbols are imported. The return value is
        a dictionary with the used module name as key, and a set of all
        imported symbol names as value.

        :returns: a dictionary that gives for each module name the set \
            of symbols imported from it.
        :rtype: Dict[str, Set[str]]

        '''
        if self._used_symbols_from_module is None:
            self._extract_import_information()

        return self._used_symbols_from_module

    # ------------------------------------------------------------------------
    def get_psyir(self):
        '''Returns the PSyIR representation of this module. This is based
        on the fparser tree (see get_parse_tree), and the information is
        cached. If the PSyIR must be modified, it needs to be copied,
        otherwise the modified tree will be returned from the cache in the
        future.
        If the conversion to PSyIR fails, an empty FileContainer is returned.
        #TODO: Maybe return a copy of the tree??

        :returns: PSyIR representing this module.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        if self._psyir is None:
            try:
                self._psyir = \
                    self._processor.generate_psyir(self.get_parse_tree())
            except (KeyError, SymbolError, InternalError):
                self._psyir = FileContainer(os.path.basename(self._filename))
            # Now update all RoutineInfo objects:
            for routine in self._psyir.walk(Routine):
                routine_info = self._routine_info[routine.name.lower()]
                routine_info.set_psyir(routine)

        return self._psyir

    # ------------------------------------------------------------------------
    def get_symbol(self, name):
        '''Returns the symbol with the specified name from the module symbol
        table.

        :param str name: name of the symbol to look up.

        :returns: the symbol with the give name.
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        psyir = self.get_psyir()
        # Get the symbol table from the module
        symbol_table = psyir.children[0].symbol_table
        return symbol_table.lookup(name)
