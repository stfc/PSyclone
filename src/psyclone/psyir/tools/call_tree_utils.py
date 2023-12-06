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

from psyclone.parse import ModuleManager
from psyclone.psyGen import BuiltIn, Kern
from psyclone.psyir.nodes import (Call, Container, IntrinsicCall, Reference)
from psyclone.psyir.symbols import (ArgumentInterface, DefaultModuleInterface,
                                    ImportInterface)
from psyclone.core import Signature, VariablesAccessInfo


# pylint: disable=too-few-public-methods
class CallTreeUtils():
    '''This class provides functions functions to analyse the sequence of
    calls.
    '''

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
        non_locals = []

        outer_module = routine.ancestor(Container)

        for access in routine.walk((Kern, Call, Reference)):
            # Builtins are certainly not externals, so ignore them.
            if isinstance(access, BuiltIn):
                continue

            if isinstance(access, Kern):
                # A kernel is a subroutine call from a module:
                non_locals.append(("routine", access.module_name,
                                   Signature(access.name)))
                continue

            if isinstance(access, IntrinsicCall):
                # Intrinsic calls can be ignored
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
                if isinstance(sym.interface, DefaultModuleInterface):
                    # A function defined in the same module
                    non_locals.append(("routine", outer_module.name,
                                       Signature(sym.name)))
                    continue

                # We don't know where the subroutine comes from
                non_locals.append(("routine", None, Signature(sym.name)))
                continue

            sym = access.symbol
            if isinstance(sym.interface, ArgumentInterface):
                # Arguments are not external symbols and can be ignored
                continue

            # Now it's either a variable, or a function call (TODO #1314)
            # that we could not yet identify. Both currently end up as a
            # Reference, but will be added as 'unknown' in case of an
            # imported symbol. When handling the corresponding module we
            # can identify it properly later.
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

            if isinstance(sym.interface, DefaultModuleInterface):
                # The symbol is defined in the current module. This is
                # still a non-local access, since this module might be
                # called from other modules
                if not sym.is_constant:
                    sig = access.get_signature_and_indices()[0]
                    non_locals.append(("reference", outer_module.name, sig))
                    continue

            # Check for an assignment of a result in a function, which
            # does not need to be reported:
            if hasattr(routine, "return_symbol") and \
                    routine.return_symbol and \
                    sym.name == routine.return_symbol.name:
                continue

        return non_locals

    # -------------------------------------------------------------------------
    def get_non_local_read_write_info(self, node_list, read_write_info):
        '''Returns the information about non-local variables that are read
        or written.

        '''

        # First collect all non-local symbols from the kernels called. They
        # are collected in the todo list. This list will initially contain
        # unknown accesses, since at this stage we cannot always differentiate
        # between function calls and array accesses. In the resolve step
        # that is following the corresponding modules will be queried and
        # the right accesses (functions or variables) will be used.
        todo = []
        mod_manager = ModuleManager.get()
        for node in node_list:
            for kernel in node.walk(Kern):
                if isinstance(kernel, BuiltIn):
                    # Builtins don't have non-local accesses
                    continue

                # Get the non-local access information from the kernel
                # by querying the module that contains the kernel:
                try:
                    mod_info = mod_manager.get_module_info(kernel.module_name)
                except FileNotFoundError:
                    print(f"Could not find module '{kernel.module_name}' - "
                          f"ignored.")
                    continue

                all_possible_routines = mod_info.resolve_routine(kernel.name)
                for routine_name in all_possible_routines:
                    psyir = mod_info.get_psyir(routine_name)
                    todo.extend(self.get_non_local_symbols(psyir))
        return self._resolve_calls_and_unknowns(todo, read_write_info)

    # -------------------------------------------------------------------------
    def _resolve_calls_and_unknowns(self, todo, read_write_info):
        '''This function updates the list of non-local symbols by:
        1. replacing all subroutine calls with the list of their corresponding
            non-local symbols.
        2. Resolving unknown types to be either function calls (which then
            get resolved as above), or variables.
        This is done recursively until only variable accesses remain.

        The actual non-local accesses will then be added to the ReadWriteInfo
        object.

        :param todo: the information about symbol type, module_name, \
            symbol_name and access information
        :type todo: List[Tuple[str,str, str,\
                              :py:class:`psyclone.core.Signature`,str]]

        :param read_write_info: information about all input and output \
            parameters.
        :type read_write_info: :py:class:`psyclone.psyir.tools.ReadWriteInfo`

        '''
        # pylint: disable=too-many-branches
        mod_manager = ModuleManager.get()
        done = set()
        # Using a set here means that duplicated entries will automatically
        # be filtered out.
        in_vars = set()
        out_vars = set()
        while todo:
            info = todo.pop()
            if info in done:
                continue
            done.add(info)
            external_type, module_name, signature, access_info = info
            if module_name in mod_manager.ignores():
                continue
            if external_type == "routine":
                if module_name is None:
                    # We don't know where the subroutine comes from.
                    # For now ignore this
                    print(f"Unknown routine '{signature[0]} - ignored.")
                    continue
                try:
                    mod_info = mod_manager.get_module_info(module_name)
                except FileNotFoundError:
                    print(f"Cannot find module '{module_name}' - ignored.")
                    continue
                try:
                    # Add the list of non-locals to our todo list:
                    todo.extend(self.get_non_local_symbols(
                        mod_info.get_psyir(signature[0])))
                except KeyError:
                    print(f"Cannot find symbol '{signature[0]}' in module "
                          f"'{module_name}' - ignored.")
                continue

            if external_type == "unknown":
                # It could be a function (TODO #1314) or a variable. Check if
                # there is a routine with that name in the module information:
                try:
                    mod_info = mod_manager.get_module_info(module_name)
                except FileNotFoundError:
                    print(f"Cannot find module '{module_name}' - ignoring "
                          f"unknown symbol '{signature}'.")
                    continue

                if mod_info.contains_routine(str(signature)):
                    # It is a routine, which we need to analyse for the use
                    # of non-local symbols:
                    todo.append(("routine", module_name, signature,
                                 access_info))
                    continue
                # Otherwise fall through to the code that adds a reference:

            # Now it must be a reference, so add it to the list of input-
            # and output-variables as appropriate:
            if access_info.is_written():
                out_vars.add((module_name, signature))
            if not access_info.is_written_first():
                in_vars.add((module_name, signature))

        # Now add all accesses to the access. Note that in_vars and out_vars
        # are sets, so there will be no duplicate entry.
        for module_name, signature in in_vars:
            read_write_info.add_read(signature, module_name)
        for module_name, signature in out_vars:
            read_write_info.add_write(signature, module_name)

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
        :rtype: List[Tuple[str, str, :py:class:`psyclone.core.Signature`,
            :py:class:`psyclone.core.SingleVariableAccessInfo`]]

        '''
        non_locals = self._compute_all_non_locals(routine)

        var_accesses = \
            VariablesAccessInfo(routine, options={"USE-ORIGINAL-NAMES": True})

        result = []
        for (symbol_type, module, signature) in non_locals:
            if symbol_type == "routine":
                result.append((symbol_type, module, signature, None))
                continue
            result.append((symbol_type, module, signature,
                           var_accesses[signature]))

        return result
