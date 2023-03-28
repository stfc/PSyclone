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
from psyclone.psyir.nodes import (Call, Container, FileContainer, Reference,
                                  Routine)
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.symbols import (ArgumentInterface, ImportInterface,
                                    RoutineSymbol, SymbolError,
                                    UnknownFortranType)


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
    @staticmethod
    def _compute_non_locals_references(access, sym):
        '''This function analyses if the symbol is a local variable, or if
        it was declared in the container, which is considered a non-local
        access. The symbol's interface is LocalInterface in any case.
        So we need to identify the symbol table in which the symbol is
        actually declared, and check if it is declared in the routine, or
        further up in the tree in the container (i.e. module).
        # TODO #1089: this should simplify the implementation.

        '''
        node = access
        while node:
            # A routine has its own name as a symbol in its symbol table.
            # That reference is not useful to decide what kind of symbol
            # it is (i.e. does it belong to this routine's container, in
            # which case it is a non-local access)
            if hasattr(node, "_symbol_table") and \
                    sym.name in node.symbol_table and node.name != sym.name:
                existing_sym = node.symbol_table.lookup(sym.name)
                if isinstance(node, Container):
                    # It is a variable from the module in which the
                    # current function is, so it is a non-local access
                    if isinstance(existing_sym, RoutineSymbol):
                        return ("function", node.name, sym.name)
                    return ("reference", node.name, sym.name)

            # Otherwise keep on looking
            node = node.parent
        return None

    # ------------------------------------------------------------------------
    def _compute_all_routine_non_locals(self):
        # pylint: disable=too-many-branches
        '''This function computes and caches all non-local access of each
        routine declared in this module.

        '''
        # Circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.psyGen import BuiltIn, Kern

        self._routine_non_locals = {}
        # First set up the dictionary of lists for all routine names:
        for routine in self.get_psyir().walk(Routine):
            self._routine_non_locals[routine.name] = []

        # Save information about generic interfaces. Generic interfaces are
        # not yet fuolly supported in PSyclone. So we look for routine
        # symbols with unknown Fortran types, then we check if any routine
        # in this module is mentioned in the datatype declaration (which
        # contains the texture representation of the interface statement).
        # If a routine name is found in the declaration, we add the
        # generic and specific name to the mapping of generic names. This
        # mapping is used later to duplicate the non-local information from
        # all specific subroutines to the generic one.
        psyir = self.get_psyir()
        # Mapping of specific names to generic name:
        generic_names = {}
        for container in psyir.walk(Container):
            for symbol in container.symbol_table.symbols_dict.values():
                if not isinstance(symbol, RoutineSymbol) or \
                        not isinstance(symbol.datatype, UnknownFortranType):
                    continue
                generic_statement = symbol.datatype.declaration.lower()
                generic_name = symbol.name
                for routine_name in self._routine_non_locals:
                    if routine_name in generic_statement:
                        # The name of the current routine is mentioned in the
                        # interface declaration, so assume it is a generic
                        # interface:
                        generic_names[routine_name] = generic_name
        for generic_name in generic_names.values():
            self._routine_non_locals[generic_name] = []

        for routine in psyir.walk(Routine):
            # Handy shortcut to the dictionary to shorten code
            non_locals = self._routine_non_locals[routine.name]
            # Find all references to non-local symbols in the current routine.
            # They can occur as three node types in the routine tree:
            # - a reference (which can include a function call, since
            #   functions are not properly identified, see TODO #1314)
            # - a Kernel (which will be converted to a call of a subroutine in
            #   an external module when lowering), or
            # - a call (if e.g. the tree has already been lowered, or it was
            #   based on NEMO API)
            for access in routine.walk((Kern, Call, Reference)):
                # Builtins are certainly not externals, so ignore them.
                if isinstance(access, BuiltIn):
                    continue

                if isinstance(access, Kern):
                    # A kernel is a subroutine call from a module:
                    non_locals.append(("subroutine", access.module_name,
                                       access.name))
                    continue

                if isinstance(access, Call):
                    sym = access.routine
                    if isinstance(sym.interface, ImportInterface):
                        non_locals.append(("subroutine",
                                           sym.interface.container_symbol.name,
                                           sym.name))
                    else:
                        # No import. This could either be a subroutine from
                        # this module, or just a global function.
                        if sym.name in self._routine_non_locals:
                            # A local function call from this module:
                            non_locals.append(("subroutine", self._name,
                                               sym.name))
                        else:
                            # We don't know where the subroutine comes from
                            non_locals.append(("subroutine", None, sym.name))
                    continue

                # Now it's either a variable, or a function call (TODO #1314):
                sym = access.symbol
                if isinstance(sym.interface, ArgumentInterface):
                    # Arguments are not external symbols and can be ignored
                    continue

                if isinstance(sym.interface, ImportInterface):
                    # It is imported, record the information. The type needs
                    # to be identified when parsing the corresponding module:
                    non_locals.append(("unknown",
                                       sym.interface.container_symbol.name,
                                       sym.name))
                    continue

                # Check for an assignment of a result in a function, which
                # does not need to be reported:
                if routine.return_symbol and \
                        sym.name == routine.return_symbol.name:
                    continue

                info = self._compute_non_locals_references(access, sym)
                if info:
                    non_locals.append(info)
            # Check if the current routine is part of a generic interface. If
            # so, we add the information for the current routine to the
            # generic subroutine.
            if routine.name in generic_names:
                generic_name = generic_names[routine.name]
                self._routine_non_locals[generic_name].extend(non_locals)

    # ------------------------------------------------------------------------
    def get_non_local_symbols_for_routine(self, routine_name):
        '''This function returns a list of non-local accesses in the given
        routine. It returns a list of triplets, each one containing:
        - the type ('subroutine', 'function', 'reference', 'unknown').
          The latter is used for array references or function calls,
          which we cannot distinguish till #1314 is done.
        - the name of the module (lowercase). This can be 'None' if no
          module information is available.
        - the name of the symbol (lowercase)

        :param str routine_name: the name of the routine.

        :returns: the non-local accesses in the given routine.
        :rtype: List[Tuple[str, str, str]]

        :raises ModuleInfoError: if the given routine name is not defined \
            in this module.

        '''
        if self._routine_non_locals is None:
            self._compute_all_routine_non_locals()

        routine_name = routine_name.lower()

        if routine_name not in self._routine_non_locals:
            raise ModuleInfoError(f"Could not find '{routine_name}' in module "
                                  f"'{self._name}'.")
        return self._routine_non_locals[routine_name]

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
