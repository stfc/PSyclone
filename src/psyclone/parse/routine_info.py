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

'''This module contains the RoutineInfo class, which is used to store
and cache information about a routine (i.e. a subroutine or a function) in a
module.
'''

from psyclone.core import Signature, VariablesAccessInfo
from psyclone.psyir.nodes import (Call, Container, Reference)
from psyclone.psyir.symbols import (ArgumentInterface, ImportInterface)


# ============================================================================
class RoutineInfoBase:

    '''This is the base class for all classes that store information about
    subroutines, functions or generic interfaces. It stores the
    ModuleInformation object to which the routine belongs, the name and
    provides an interface to store the PSyIR (if it is created by the
    ModuleInfo object).

    :param module_info: the ModuleInfo object to which this routine belongs.
    :type module_info: :py:class:`psyclone.parse.ModuleInfo`
    :param str name: the name of the routine or generic interface.

    '''
    def __init__(self, module_info, name):
        self._module_info = module_info
        self._name = name
        self._psyir = None

    # -------------------------------------------------------------------------
    @property
    def name(self):
        ''':returns: the name of the routine.
        :rtype: str

        '''
        return self._name

    # -------------------------------------------------------------------------
    def set_psyir(self, psyir):
        '''Sets the PSyIR representation of this routine. This is called from
        the module info object that this object is managed by.

        :param psyir: the PSyIR of this routine.
        :type psyir: :py:class:`psyclone.psyir.nodes.Node`

        '''
        self._psyir = psyir

    # -------------------------------------------------------------------------
    def get_psyir(self):
        ''':returns: the PSyIR of this routine.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        if self._psyir is None:
            # Parsing the PSyIR in the parent ModuleInfo will populate the
            # PSyIR information for each routine, including this one:
            self.module_info.get_psyir()

        return self._psyir

    # -------------------------------------------------------------------------
    @property
    def module_info(self):
        ''':returns: the ModuleInfo object to which this Routine belongs.
        :rtype: :py:class:`psyclone.parse.ModuleInfo`

        '''
        return self._module_info

    # -------------------------------------------------------------------------
    def get_non_local_symbols(self):
        '''This function returns a list of non-local accesses in the given
        routine. It returns a list of triplets, each one containing:
        - the type ('routine', 'function', 'reference', 'unknown').
          The latter is used for array references or function calls,
          which we cannot distinguish till #1314 is done.
        - the name of the module (lowercase). This can be 'None' if no
          module information is available.
        - the name of the symbol (lowercase)
        - the access information for the given variable

        :returns: the non-local accesses in this routine.
        :rtype: List[Tuple[str, str, :py:class:`psyclone.core.Signature`, \
                          :py:class:`psyclone.core.SingleVariableAccessInfo`]]

        '''

    # ------------------------------------------------------------------------
    def get_var_accesses(self):
        ''':returns: the variable access information for this routine.
        :rtype: :py:class:`psyclone.core.VariablesAccessInfo`

        '''


# ============================================================================
class RoutineInfo(RoutineInfoBase):
    '''This class stores information about a routine (function, subroutine).

    :param module_info: the ModuleInfo object which manages this object.
    :type module_info: :py:class:`psyclone.parse.ModuleInfo`
    :param ast: the AST of this routine.
    :type ast: Union[:py:class:`fparser.two.Fortran2003.Function_Subprogram`,
                     :py:class:`fparser.two.Fortran2003.Subroutine_Subprogram`]

    '''
    def __init__(self, module_info, ast):
        name = str(ast.content[0].items[1])
        super().__init__(module_info, name)
        self._ast = ast

        # List[Tuple[str, str, :py:class:`psyclone.core.Signature`, \
        #           :py:class:`psyclone.core.SingleVariableAccessInfo`]]
        self._non_locals = None
        self._var_accesses = None

    # ------------------------------------------------------------------------
    def get_var_accesses(self):
        ''':returns: the variable access information for this routine.
        :rtype: :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        if self._var_accesses is None:
            self._var_accesses = \
                VariablesAccessInfo(self.get_psyir(),
                                    options={"USE-ORIGINAL-NAMES": True})

        return self._var_accesses

    # ------------------------------------------------------------------------
    @staticmethod
    def _compute_non_locals_references(reference, sym):
        '''This function analyses if the symbol is a local variable, or if
        it was declared in the container, which is considered a non-local
        access. The symbol's interface is LocalInterface in any case.
        So we need to identify the symbol table in which the symbol is
        actually declared, and check if it is declared in the routine, or
        further up in the tree in the container (i.e. module).
        # TODO #1089: this should simplify the implementation.

        :param reference: the Reference node which accessed the specified \
            symbol.
        :type reference: :py:class:`psyclone.psyir.nodes.Reference`
        :param sym: the symbol which needs to be identified to be either \
            local or not.
        :type sym: :py:class:`psyclone.psyir.symbols.Symbol`

        :returns: either None (if the symbol cannot be found or is a \
            constant), or a tuple indicating type, module name and symbol \
            name.
        :rtype: Union[None, Tuple[str, str, str]]

        '''
        node = reference
        while node:
            # A routine has its own name as a symbol in its symbol table.
            # That reference is not useful to decide what kind of symbol
            # it is (i.e. does it belong to this routine's container, in
            # which case it is a non-local access)
            if hasattr(node, "_symbol_table") and \
                    sym.name in node.symbol_table and node.name != sym.name:
                existing_sym = node.symbol_table.lookup(sym.name)
                if existing_sym.is_automatic:
                    return None
                if isinstance(node, Container):
                    if sym.is_constant:
                        # Constants don't need to be saved
                        return None
                    sig = reference.get_signature_and_indices()[0]
                    return ("reference", node.name, sig)

            # Otherwise keep on looking
            node = node.parent
        return None

    # ------------------------------------------------------------------------
    def _compute_all_non_locals(self):
        # pylint: disable=too-many-branches
        '''This function computes and caches all non-local access of this
        routine.

        '''
        # Circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.psyGen import BuiltIn, Kern

        self._non_locals = []

        # Even if the file could not be parsed, there will be a dummy
        # psyir returned, so no need to handle parsing errors here.
        # TODO #2010
        for access in self.get_psyir().walk((Kern, Call, Reference)):
            # Builtins are certainly not externals, so ignore them.
            if isinstance(access, BuiltIn):
                continue

            if isinstance(access, Kern):
                # A kernel is a subroutine call from a module:
                self._non_locals.append(("routine", access.module_name,
                                         Signature(access.name)))
                continue

            if isinstance(access, Call):
                sym = access.routine
                if isinstance(sym.interface, ImportInterface):
                    module_name = sym.interface.container_symbol.name
                    self._non_locals.append(("routine", module_name,
                                             Signature(sym.name)))
                    continue
                # No import. This could either be a routine from
                # this module, or just a global function.
                try:
                    self.module_info.get_routine_info(sym.name)
                    # A local function that is in the same module:
                    self._non_locals.append(("routine",
                                             self.module_info.name,
                                             Signature(sym.name)))
                except KeyError:
                    # We don't know where the subroutine comes from
                    self._non_locals.append(("routine", None, sym.name))

                continue

            # Now it's either a variable, or a function call (TODO #1314),
            # both currently end up as a Reference:
            sym = access.symbol
            if isinstance(sym.interface, ArgumentInterface):
                # Arguments are not external symbols and can be ignored
                continue

            if isinstance(sym.interface, ImportInterface):
                # It is imported, record the information. The type needs
                # to be identified when parsing the corresponding module,
                # so for now set the type as unknown:
                module_name = sym.interface.container_symbol.name
                sig = access.get_signature_and_indices()[0]
                # If a symbol is renamed, use the original name:
                if sym.interface.orig_name:
                    sig = Signature(sym.interface.orig_name, sig[1:])
                self._non_locals.append(("unknown", module_name, sig))
                continue

            # Check for an assignment of a result in a function, which
            # does not need to be reported:
            if self._psyir.return_symbol and \
                    sym.name == self._psyir.return_symbol.name:
                continue

            info = self._compute_non_locals_references(access, sym)
            if info:
                self._non_locals.append(info)

    # ------------------------------------------------------------------------
    def get_non_local_symbols(self):
        '''This function returns a list of non-local accesses in this
        routine. It returns a list of triplets, each one containing:
        - the type ('routine', 'function', 'reference', 'unknown').
          The latter is used for array references or function calls,
          which we cannot distinguish till #1314 is done.
        - the name of the module (lowercase). This can be 'None' if no
          module information is available.
        - the Signature of the symbol
        - the access information for the given variable

        :returns: the non-local accesses in this routine.
        :rtype: List[Tuple[str, str, :py:class:`psyclone.core.Signature`, \
                          :py:class:`psyclone.core.SingleVariableAccessInfo`]]

        '''
        if self._non_locals is None:
            self._compute_all_non_locals()

        var_accesses = self.get_var_accesses()
        result = []
        for (symbol_type, module, sym_name) in self._non_locals:
            if symbol_type == "routine":
                result.append((symbol_type, module, Signature(sym_name), None))
                continue
            sig = Signature(sym_name)
            result.append((symbol_type, module, sym_name,
                           var_accesses[sig]))

        return result


# ============================================================================
class GenericRoutineInfo(RoutineInfoBase):
    '''This class provides information about a generic function. It uses
    the RoutineInfo objects of the various routines it applies to to
    query the required data.

    :param module_info: the ModuleInfo object which manages this object.
    :type module_info: :py:class:`psyclone.parse.ModuleInfo`
    :param str name: name of the generic routine.
    :param List[str] list_of_routines: the list of routine names which are \
        part of this generic interface.

    '''
    def __init__(self, module_info, name, list_of_routines):
        super().__init__(module_info, name)

        self._all_routines = []
        for routine_name in list_of_routines:
            routine_info = module_info.get_routine_info(routine_name)
            self._all_routines.append(routine_info)

    # ------------------------------------------------------------------------
    def get_non_local_symbols(self):
        '''This function returns a list of non-local accesses in the functions
        that are part of this generic interface. Since PSyclone in general has
        no information about which specific function will be called (since this
        can depend on compile-time information like precision of variables),
        it will combine the information from each specific routine.
        It returns a list of triplets, each one containing:
        - the type ('routine', 'function', 'reference', 'unknown').
          The latter is used for array references or function calls,
          which we cannot distinguish till #1314 is done.
        - the name of the module (lowercase). This can be 'None' if no
          module information is available.
        - the name of the symbol (lowercase)
        - the access information for the given variable

        :returns: the non-local accesses for all specific routines of this \
            generic interface.
        :rtype: List[Tuple[str, str, :py:class:`psyclone.core.Signature`, \
                          :py:class:`psyclone.core.SingleVariableAccessInfo`]]

        '''
        non_locals = []

        for routine_info in self._all_routines:
            non_locals.extend(routine_info.get_non_local_symbols())

        return non_locals
