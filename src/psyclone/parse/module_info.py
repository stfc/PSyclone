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
and the fparser tree (if requested). '''

import os

from fparser.common.readfortran import FortranStringReader
from fparser.two.Fortran2003 import Use_Stmt
from fparser.two.parser import ParserFactory
from fparser.two.utils import walk

from psyclone.errors import InternalError, PSycloneError
from psyclone.psyir.nodes import Container, FileContainer, Reference
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.symbols import (ArgumentInterface, ImportInterface,
                                    SymbolError)


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
    '''This class stores mostly cached information about modules: it stores
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

        # This is a dictionary, with the modules from the list of all used
        # modules as key, and it stores the set of all symbols imported from
        # this module: Dict[str, Set(str)]
        self._used_symbols_from_module = None

        self._processor = Fparser2Reader()

    # ------------------------------------------------------------------------
    @property
    def filename(self):
        ''':returns: the filename that contains the source code for this
        module.
        :rtype: str

        '''
        return self._filename

    # ------------------------------------------------------------------------
    def get_source_code(self):
        '''Returns the source code for the module. The first time, it
        will be read from the file, but the data is then cached.

        :returns: the source code as string.
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
        will be parsed by fpaser, then the AST is cached for any future uses.
        :returns: the fparser AST for this module.
        :rtype:

        '''
        if not self._parse_tree:
            reader = FortranStringReader(self.get_source_code())
            parser = ParserFactory().create(std="f2008")
            self._parse_tree = parser(reader)
        return self._parse_tree

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
            # If there is no only_list, then the set of symbols is
            # will stay empty
            if only_list:
                # Parse the only list:
                for symbol in use.items[4].children:
                    all_symbols.add(str(symbol))

            self._used_symbols_from_module[mod_name] = all_symbols

    # ------------------------------------------------------------------------
    def get_used_modules(self):
        '''This function analyses a given module source file and returns
        a list of 2-tuples containing the module name, and a list of
        all imported symbols from that module. If all symbols are imported,
        the list of symbols will be empty.

        :param str module_name: the file name (including path if required) \
            for which all modules should be found.

        :returns: a set with all imported module names.
        :rtype: Set[str]

        '''
        if self._used_modules is None:
            self._extract_import_information()

        return self._used_modules

    # ------------------------------------------------------------------------
    def get_used_symbols_from_modules(self):
        '''This function analyses a given module source file and returns
        a list of 2-tuples containing the module name, and a list of
        all imported symbols from that module. If all symbols are imported,
        the list of symbols will be empty.

        :param str module_name: the file name (including path if required) \
            for which all modules should be found.

        :returns: a dictionary that gives for each module name the list \
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

        return self._psyir

    # ------------------------------------------------------------------------
    def get_external_symbols(self):
        '''This function determines all external symbols used in any routine
        inside this module. An external symbol in this context is either a
        symbol imported from another module, or a symbol declared in this
        module, and referenced in a routine.

        :return: a list of tuples indicating the symbol and the module name \
            it is imported from.
        :rtype: List[Tuple[:py:class:`psyclone.psyir.symbols.Symbol`, str]]

        '''

        result = []
        done = set()
        for ref in self.get_psyir().walk(Reference):
            sym = ref.symbol
            if sym.name in done:
                continue
            done.add(sym.name)

            # If the symbol is known to he imported, add it to the list
            # of external symbols and continue
            if isinstance(sym.interface, ImportInterface):
                result.append((sym, sym.interface.container_symbol.name))
                continue
            if isinstance(sym.interface, ArgumentInterface):
                # Arguments are not external symbols
                continue

            # Now it's a local interface - it still could be a local variable,
            # or a variable declared in the surrounding module (TODO: #1089)
            # So we need to identify the symbol table in which the symbol is
            # actually declared, and check if it's in the routine, or further
            # up in the tree:
            node = ref
            while node:
                if hasattr(node, "_symbol_table"):
                    if sym.name in node.symbol_table:
                        if isinstance(node, Container):
                            # It is a variable from the module in which the
                            # current function is, so it is an external
                            # reference
                            result.append((sym, node.name))
                        # If the node is not the container, it is a local
                        # variable and does not need to be returned.
                        break
                # Otherwise keep on looking
                node = node.parent

        return result
