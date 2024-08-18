# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024, Science and Technology Facilities Council.
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

''' This module provides tools to analyse the sequence of calls
across different subroutines and modules.'''

from psyclone.core import Signature, VariablesAccessInfo
from psyclone.parse import ModuleManager
from psyclone.psyGen import BuiltIn, Kern
from psyclone.psyir.nodes import (Call, Container, IntrinsicCall, Reference)
from psyclone.psyir.symbols import (ArgumentInterface, DefaultModuleInterface,
                                    ImportInterface)
from psyclone.psyir.tools.read_write_info import ReadWriteInfo


# pylint: disable=too-few-public-methods
class CallTreeUtils():
    '''This class provides functions to analyse the sequence of
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
        :rtype: list[tuple[str, str, :py:class:`psyclone.core.Signature`]]


        '''
        non_locals = []

        outer_module = routine.ancestor(Container)

        for access in routine.walk((Kern, Call, Reference)):
            if isinstance(access, (BuiltIn, IntrinsicCall)):
                # Builtins and Intrinsicsd are certainly not externals,
                # so ignore them.
                continue

            if isinstance(access, Kern):
                # A kernel is a subroutine call from a module:
                # TODO #2054: This will not be necessary anymore once
                # a Kernel is also a Call.
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
                if isinstance(sym.interface, DefaultModuleInterface):
                    # A function defined in the same module
                    non_locals.append(("routine", outer_module.name,
                                       Signature(sym.name)))
                    continue

                # We don't know where the subroutine comes from
                non_locals.append(("routine", None, Signature(sym.name)))
                continue

            # Now access must be a Reference
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
    def get_input_parameters(self, read_write_info, node_list,
                             variables_info=None, options=None):
        '''Adds all variables that are input parameters (i.e. are read before
        potentially being written) to the read_write_info object.

        :param read_write_info: this object stores the information about
            all input parameters.
        :type read_write_info: :py:class:`psyclone.psyir.tools.ReadWriteInfo`
        :param node_list: list of PSyIR nodes to be analysed.
        :type node_list: list[:py:class:`psyclone.psyir.nodes.Node`]
        :param variables_info: optional variable usage information,
            can be used to avoid repeatedly collecting this information.
        :type variables_info:
            :py:class:`psyclone.core.variables_info.VariablesAccessInfo`
        :param options: a dictionary with options for the CallTreeUtils
            which will also be used when creating the VariablesAccessInfo
            instance if required.
        :type param: Optional[dict[str, Any]]
        :param Any options["COLLECT-ARRAY-SHAPE-READS"]: if this option is
            set to a True value, arrays used as first parameter to the
            PSyIR operators lbound, ubound, or size will be reported as
            'read'. Otherwise, these accesses will be ignored.

        '''
        # Collect the information about all variables used:
        if not variables_info:
            variables_info = VariablesAccessInfo(node_list, options=options)

        for signature in variables_info.all_signatures:
            # If the first access is a write, the variable is not an input
            # parameter and does not need to be saved. Note that loop variables
            # have a WRITE before a READ access, so they will be ignored
            # automatically.
            if not variables_info[signature].is_written_first():
                read_write_info.add_read(signature)

    # -------------------------------------------------------------------------
    def get_output_parameters(self, read_write_info, node_list,
                              variables_info=None, options=None):
        '''Adds all variables that are output parameters (i.e. are written)
        to the read_write_info object.

        :param read_write_info: this object stores the information about
            output parameters.
        :type read_write_info: :py:class:`psyclone.psyir.tools.ReadWriteInfo`
        :param node_list: list of PSyIR nodes to be analysed.
        :type node_list: list[:py:class:`psyclone.psyir.nodes.Node`]
        :param variables_info: optional variable usage information,
            can be used to avoid repeatedly collecting this information.
        :type variables_info: \
        Optional[:py:class:`psyclone.core.variables_info.VariablesAccessInfo`]
        :param options: a dictionary with options for the CallTreeUtils
            which will also be used when creating the VariablesAccessInfo
            instance if required.
        :type param: Optional[dict[str, Any]]
        :param Any options["COLLECT-ARRAY-SHAPE-READS"]: if this option is
            set to a True value, arrays used as first parameter to the
            PSyIR operators lbound, ubound, or size will be reported as
            'read'. Otherwise, these accesses will be ignored.

        '''
        # Collect the information about all variables used:
        if not variables_info:
            variables_info = VariablesAccessInfo(node_list, options=options)

        for signature in variables_info.all_signatures:
            if variables_info.is_written(signature):
                read_write_info.add_write(signature)

    # -------------------------------------------------------------------------
    def get_in_out_parameters(self, node_list, options=None):
        '''Returns a ReadWriteInfo object that contains all variables that are
        input and output parameters to the specified node list. This function
        calls `get_input_parameter` and `get_output_parameter`, but avoids the
        repeated computation of the variable usage.

        :param node_list: list of PSyIR nodes to be analysed.
        :type node_list: list[:py:class:`psyclone.psyir.nodes.Node`]
        :param options: a dictionary with options for the CallTreeUtils
            which will also be used when creating the VariablesAccessInfo
            instance if required.
        :type options: Optional[dict[str, Any]]
        :param Any options["COLLECT-ARRAY-SHAPE-READS"]: if this option is
            set to a True value, arrays used as first parameter to the
            PSyIR operators lbound, ubound, or size will be reported as
            'read'. Otherwise, these accesses will be ignored.

        :returns: a ReadWriteInfo object with the information about input-
            and output parameters.
        :rtype: :py:class:`psyclone.psyir.tools.ReadWriteInfo`

        '''
        variables_info = VariablesAccessInfo(node_list, options=options)
        read_write_info = ReadWriteInfo()
        self.get_input_parameters(read_write_info, node_list, variables_info)
        self.get_output_parameters(read_write_info, node_list, variables_info)
        return read_write_info

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
            # TODO #2494 - we need to support calls in order to work for
            # generic PSyIR.
            for kernel in node.walk(Kern):
                if isinstance(kernel, BuiltIn):
                    # Builtins don't have non-local accesses
                    continue

                # Get the non-local access information from the kernel
                # by querying the module that contains the kernel:
                try:
                    mod_info = mod_manager.get_module_info(kernel.module_name)
                except FileNotFoundError:
                    # TODO #11: Add proper logging
                    # TODO #2120: Handle error
                    print(f"[CallTreeUtils.get_non_local_read_write_info] "
                          f"Could not find module '{kernel.module_name}' - "
                          f"ignored.")
                    continue

                # TODO #2435: once we have interface support, this will be
                # handled by the container node.
                all_possible_routines = mod_info.resolve_routine(kernel.name)
                for routine_name in all_possible_routines:
                    psyir = \
                        mod_info.get_psyir().get_routine_psyir(routine_name)
                    todo.extend(self.get_non_local_symbols(psyir))
        return self._resolve_calls_and_unknowns(todo, read_write_info)

    # -------------------------------------------------------------------------
    def _resolve_calls_and_unknowns(self, outstanding_nonlocals,
                                    read_write_info):
        '''This function updates the list of non-local symbols by:
        1. replacing all subroutine calls with the list of their corresponding
            non-local symbols.
        2. Resolving unknown types to be either function calls (which then
            get resolved as above), or variables.
        This is done recursively until only variable accesses remain.

        The actual non-local accesses will then be added to the ReadWriteInfo
        object.

        :param outstanding_nonlocals: the information about symbol type,
            module_name, symbol_name and access information
        :type outstanding_nonlocals: list[tuple[str, str, str,
                              :py:class:`psyclone.core.Signature`,str]]
        :param read_write_info: information about all input and output
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
        while outstanding_nonlocals:
            info = outstanding_nonlocals.pop()
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
                    # TODO #11: Add proper logging
                    # TODO #2120: Handle error
                    print(f"[CallTreeUtils._resolve_calls_and_unknowns] "
                          f"Unknown routine '{signature[0]} - ignored.")
                    continue
                try:
                    mod_info = mod_manager.get_module_info(module_name)
                except FileNotFoundError:
                    # TODO #11: Add proper logging
                    # TODO #2120: Handle error
                    print(f"[CallTreeUtils._resolve_calls_and_unknowns] "
                          f"Cannot find module '{module_name}' - ignored.")
                    continue
                routine = mod_info.get_psyir().get_routine_psyir(signature[0])
                if routine:
                    # Add the list of non-locals to our todo list:
                    outstanding_nonlocals.extend(
                        self.get_non_local_symbols(routine))
                else:
                    # TODO #11: Add proper logging
                    # TODO #2120: Handle error
                    print(f"[CallTreeUtils._resolve_calls_and_unknowns] "
                          f"Cannot find symbol '{signature[0]}' in module "
                          f"'{module_name}' - ignored.")
                continue

            if external_type == "unknown":
                # It could be a function (TODO #1314) or a variable. Check if
                # there is a routine with that name in the module information:
                try:
                    mod_info = mod_manager.get_module_info(module_name)
                except FileNotFoundError:
                    # TODO #11: Add proper logging
                    # TODO #2120: Handle error
                    print(f"[CallTreeUtils._resolve_calls_and_unknowns] "
                          f"Cannot find module '{module_name}' - ignoring "
                          f"unknown symbol '{signature}'.")
                    continue

                if mod_info.contains_routine(str(signature)):
                    # It is a routine, which we need to analyse for the use
                    # of non-local symbols:
                    outstanding_nonlocals.append(("routine", module_name,
                                                  signature, access_info))
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
        :rtype: list[tuple[str, str, :py:class:`psyclone.core.Signature`,
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
