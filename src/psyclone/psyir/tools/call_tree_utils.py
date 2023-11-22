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
# -----------------------------------------------------------------------------

''' This module provides tools analyse the sequence of calls
across different subroutines and modules.'''

from psyclone.psyir.nodes.call import Call
from psyclone.psyir.nodes import Reference, Routine
from psyclone.psyir.symbols import (ArgumentInterface, ImportInterface)
from psyclone.core import Signature, VariablesAccessInfo


# pylint: disable=too-few-public-methods
class CallTreeUtils():
    '''This class provides functions functions to analyse the sequence of
    calls.
    '''

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
        # Circular import:
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes.container import Container
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
    def _compute_all_non_locals(self, routine):
        # pylint: disable=too-many-branches
        '''This function computes all non-local access of the specified
        routine node. It returns a list of 3-tuples containing:
        - the type of the non-local as string, one of:
          - routine: A routine that is called
          - unknown: an imported symbol, which could still be a function or
                     an array. This will be resolved later when checking
                     the module the symbol is imported from.
          - reference: a variable access
        - the name of the module the symbol is imported from
        - the signature of the access

        :param routine: the routine for which to collect all non-local
            accesses
        :type routine: :py:class:`psyclone.psyir.nodes.Routine`

        :returns: list of non-local references
        :rtype: List[Tuple[str, str, :py:class:`psyclone.core.Signature`]]


        '''
        # Circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.psyGen import BuiltIn, Kern

        non_locals = []

        for access in routine.walk((Kern, Call, Reference)):
            # Builtins are certainly not externals, so ignore them.
            if isinstance(access, BuiltIn):
                continue

            if isinstance(access, Kern):
                # A kernel is a subroutine call from a module:
                non_locals.append(("routine", access.module_name,
                                   Signature(access.name)))
                continue

            if isinstance(access, Call):
                sym = access.routine
                if isinstance(sym.interface, ImportInterface):
                    module_name = sym.interface.container_symbol.name
                    non_locals.append(("routine", module_name,
                                       Signature(sym.name)))
                    continue
                # No import. This could either be a routine from
                # this module, or just a global function.
                parent = routine.parent
                for called_routine in parent.walk(Routine):
                    if called_routine.name == sym.name:
                        # A local function that is in the same module:
                        non_locals.append(("routine", parent.name,
                                           Signature(sym.name)))
                        break
                else:
                    # We don't know where the subroutine comes from
                    non_locals.append(("routine", None, sym.name))
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
                non_locals.append(("unknown", module_name, sig))
                continue

            # Check for an assignment of a result in a function, which
            # does not need to be reported:
            if routine.return_symbol and \
                    sym.name == routine.return_symbol.name:
                continue

            info = self._compute_non_locals_references(access, sym)
            if info:
                non_locals.append(info)

        return non_locals

    # ------------------------------------------------------------------------
    def get_non_local_symbols(self, routine):
        '''This function returns a list of non-local accesses in this
        routine. It returns a list of triplets, each one containing:

        - the type ('routine', 'function', 'reference', 'unknown').
          The latter is used for array references or function calls,
          which we cannot distinguish till #1314 is done.
        - the name of the module (lowercase). This can be 'None' if no
          module information is available.
        - the Signature of the symbol
        - the access information for the given variable

        :param routine: the routine for which to collect all non-local
            accesses
        :type routine: :py:class:`psyclone.psyir.nodes.Routine`

        :returns: the non-local accesses in this routine.
        :rtype: List[Tuple[str, str, :py:class:`psyclone.core.Signature`, \
            :py:class:`psyclone.core.SingleVariableAccessInfo`]]

        '''
        non_locals = self._compute_all_non_locals(routine)

        var_accesses = \
            VariablesAccessInfo(routine, options={"USE-ORIGINAL-NAMES": True})

        result = []
        for (symbol_type, module, sym_name) in non_locals:
            if symbol_type == "routine":
                result.append((symbol_type, module, Signature(sym_name), None))
                continue
            sig = Signature(sym_name)
            result.append((symbol_type, module, sym_name,
                           var_accesses[sig]))

        return result
