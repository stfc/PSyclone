# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024-2025, Science and Technology Facilities Council.
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

import logging

from psyclone.core import Signature, VariablesAccessMap
from psyclone.parse import ModuleManager
from psyclone.psyGen import BuiltIn, Kern
from psyclone.psyir.nodes import Container, Reference
from psyclone.psyir.symbols import (
    ArgumentInterface, DefaultModuleInterface, GenericInterfaceSymbol,
    ImportInterface, IntrinsicSymbol, RoutineSymbol)
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

        for access in routine.walk((Kern, Reference)):
            if isinstance(access, BuiltIn) or (
                    isinstance(access, Reference) and
                    isinstance(access.symbol, IntrinsicSymbol)
            ):
                # Builtins and Intrinsics are certainly not externals,
                # so ignore them.
                continue

            if isinstance(access, Kern):
                # A kernel is a subroutine call from a module:
                # TODO #2054: This will not be necessary anymore once
                # a Kernel is also a Call.
                non_locals.append(("routine", access.module_name,
                                   Signature(access.name)))
                continue

            if isinstance(access.symbol, RoutineSymbol):
                if isinstance(access.symbol.interface, ImportInterface):
                    module_name = access.symbol.interface.container_symbol.name
                    non_locals.append(("routine", module_name,
                                       Signature(access.symbol.name)))
                    continue

                # No import. This could either be a routine from
                # this module, or just a global function.
                if isinstance(access.symbol.interface, DefaultModuleInterface):
                    # A function defined in the same module
                    non_locals.append(("routine", outer_module.name,
                                       Signature(access.symbol.name)))
                    continue

                # We don't know where the subroutine comes from
                non_locals.append(("routine", None,
                                   Signature(access.symbol.name)))
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
                             variables_info=None,
                             include_non_data_accesses=False):
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
            :py:class:`psyclone.core.variables_info.VariablesAccessMap`

        '''
        # Collect the information about all variables used:
        if not variables_info:
            variables_info = VariablesAccessMap()
            for node in node_list:
                variables_info.update(node.reference_accesses())

        if include_non_data_accesses:
            all_accesses = variables_info.all_signatures
        else:
            all_accesses = variables_info.all_data_accesses

        for signature in all_accesses:
            # If the first access is a write, the variable is not an input
            # parameter and does not need to be saved. Note that loop variables
            # have a WRITE before a READ access, so they will be ignored
            # automatically.
            if (variables_info[signature].is_read_only() or
                    not variables_info[signature].is_written_first()):
                read_write_info.add_read(signature)

    # -------------------------------------------------------------------------
    def get_output_parameters(self, read_write_info, node_list,
                              variables_info=None):
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
        Optional[:py:class:`psyclone.core.variables_info.VariablesAccessMap`]

        '''
        # Collect the information about all variables used:
        if not variables_info:
            variables_info = VariablesAccessMap()
            for node in node_list:
                variables_info.update(node.reference_accesses())

        for signature in variables_info.all_signatures:
            if variables_info.is_written(signature):
                read_write_info.add_write(signature)

    # -------------------------------------------------------------------------
    def get_in_out_parameters(self, node_list, collect_non_local_symbols=False,
                              include_non_data_accesses=False):
        '''Returns a ReadWriteInfo object that contains all variables that are
        input and output parameters to the specified node list. This function
        calls `get_input_parameter` and `get_output_parameter`, but avoids the
        repeated computation of the variable usage. If
        `collect_non_local_symbols` is set to True, the code will also include
        non-local symbols used directly or indirectly, i.e. it will follow the
        call tree as much as possible (e.g. it cannot resolve a procedure
        pointer, since then it is not known which function is actually called)
        and collect any other variables that will be read or written when
        executing the nodes specified in the node list. The corresponding
        module name for these variables will be included in the ReadWriteInfo
        result object. For this to work it is essential that the correct
        search paths are specified for the module manager.

        :param node_list: list of PSyIR nodes to be analysed.
        :type node_list: list[:py:class:`psyclone.psyir.nodes.Node`] |
            :py:class:`psyclone.psyir.nodes.Node`
        :param bool collect_non_local_symbols: whether non-local symbols
            (i.e. symbols used in other modules either directly or
            indirectly) should be included in the in/out information.

        :returns: a ReadWriteInfo object with the information about input-
            and output parameters.
        :rtype: :py:class:`psyclone.psyir.tools.ReadWriteInfo`

        '''
        node_list = node_list if isinstance(node_list, list) else [node_list]
        variables_info = VariablesAccessMap()
        for node in node_list:
            variables_info.update(node.reference_accesses())
        read_write_info = ReadWriteInfo()
        self.get_input_parameters(
            read_write_info, node_list, variables_info,
            include_non_data_accesses=include_non_data_accesses)
        self.get_output_parameters(read_write_info, node_list, variables_info)
        if collect_non_local_symbols:
            self.get_non_local_read_write_info(node_list, read_write_info)

        return read_write_info

    # -------------------------------------------------------------------------
    def get_non_local_read_write_info(self, node_list, read_write_info):
        '''Returns the information about non-local variables that are read
        or written.

        :param node_list: list of nodes containing Kernel calls to interrogate.
        :type node_list: list[:py:class:`psyclone.psyGen.Kern`]
        :param read_write_info: the object to update with the read/write
                                information obtained.
        :type read_write_info: :py:class:`psyclone.psyir.tools.ReadWriteInfo`

        '''
        # First collect all non-local symbols from the kernels called. They
        # are collected in the todo list. This list will initially contain
        # unknown accesses, since at this stage we cannot always differentiate
        # between function calls and array accesses. In the resolve step
        # that is following the corresponding modules will be queried and
        # the right accesses (functions or variables) will be used.
        todo = []
        mod_manager = ModuleManager.get()
        logger = logging.getLogger(__name__)
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
                except FileNotFoundError as err:
                    # TODO #2120: Handle error
                    logger.warning(
                        "[CallTreeUtils.get_non_local_read_write_info] "
                        "Could not find module '%s' - "
                        "ignored.\n%s", kernel.module_name, str(err))
                    continue

                # Get the Container for this module.
                cntr = mod_info.get_psyir()
                if not cntr:
                    logger.warning(
                        "[CallTreeUtils.get_non_local_read_write_info] "
                        "Could not get PSyIR for module "
                        "'%s' - ignored.", kernel.module_name)
                    continue
                all_possible_routines = cntr.resolve_routine(kernel.name)
                if not all_possible_routines:
                    logger.warning(
                        "[CallTreeUtils.get_non_local_read_write_info] "
                        "Could not get PSyIR for Routine '%s' from module "
                        "'%s' as no possible routines  were found - ignored.",
                        kernel.name, kernel.module_name)
                for routine_name in all_possible_routines:
                    psyir = cntr.find_routine_psyir(routine_name)
                    if not psyir:
                        logger.warning(
                            "[CallTreeUtils.get_non_local_read_write_info] "
                            "Could not get PSyIR for Routine '%s' from module "
                            "'%s' - ignored.", routine_name,
                            kernel.module_name)
                        continue
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
            module_name, symbol_name, signature and access information.
        :type outstanding_nonlocals: list[tuple[
            str, str, str, :py:class:`psyclone.core.Signature`,
            :py:class:`psyclone.core.AccessSequence`]]
        :param read_write_info: information about all input and output
            parameters.
        :type read_write_info: :py:class:`psyclone.psyir.tools.ReadWriteInfo`

        '''
        # pylint: disable=too-many-branches, too-many-locals
        # pylint: disable=too-many-statements
        mod_manager = ModuleManager.get()
        logger = logging.getLogger(__name__)
        # Using a set here means that duplicated entries will automatically
        # be filtered out.
        in_vars = set()
        out_vars = set()
        # pylint: disable=too-many-nested-blocks
        while outstanding_nonlocals:
            info = outstanding_nonlocals.pop()
            external_type, module_name, signature, access_info = info
            if module_name in mod_manager.ignores():
                continue
            if external_type == "routine":
                if module_name is None:
                    # We don't know where the subroutine comes from.
                    # For now ignore this
                    # TODO #2120: Handle error
                    logger.warning(
                        "[CallTreeUtils._resolve_calls_and_unknowns] "
                        "Unknown routine '%s - ignored.", signature[0])
                    continue
                try:
                    mod_info = mod_manager.get_module_info(module_name)
                except FileNotFoundError:
                    # TODO #2120: Handle error
                    logger.warning(
                        "[CallTreeUtils._resolve_calls_and_unknowns] "
                        "Cannot find module '%s' - ignored.", module_name)
                    continue
                cntr = mod_info.get_psyir()
                if not cntr:
                    logger.warning(
                        "[CallTreeUtils._resolve_calls_and_unknowns] "
                        "Cannot get PSyIR for module '%s' - "
                        "ignoring unknown symbol '%s'.",
                        module_name, signature[0])
                    continue
                # Check that we find at least one valid routine (several
                # could be found in case of a generic interface):
                at_least_one_routine_found = False
                for routine_name in cntr.resolve_routine(signature[0]):
                    routine = cntr.find_routine_psyir(routine_name)
                    if not routine:
                        # TODO #2120: Handle error
                        logger.warning(
                            "[CallTreeUtils._resolve_calls_and_unknowns] "
                            "Cannot find routine '%s' in module"
                            " '%s' - ignored.", routine_name, module_name)
                        continue
                    # Add the list of non-locals to our todo list:
                    outstanding_nonlocals.extend(
                        self.get_non_local_symbols(routine))
                    at_least_one_routine_found = True

                if not at_least_one_routine_found:
                    logger.warning(
                        "[CallTreeUtils._resolve_calls_and_unknowns] "
                        "Cannot resolve routine '%s' in module "
                        "'%s' - ignored.", signature[0], module_name)
                continue

            if external_type == "unknown":
                # It could be a function (TODO #1314) or a variable. Check if
                # there is a routine with that name in the module information:
                try:
                    mod_info = mod_manager.get_module_info(module_name)
                except FileNotFoundError:
                    # TODO #2120: Handle error
                    logger.warning(
                        "[CallTreeUtils._resolve_calls_and_unknowns] "
                        "Cannot find module '%s' - ignoring "
                        "unknown symbol '%s'.", module_name, signature)
                    continue

                cntr = mod_info.get_psyir()
                if not cntr:
                    logger.warning(
                        "[CallTreeUtils._resolve_calls_and_unknowns] "
                        "Cannot get PSyIR for module '%s' - "
                        "ignoring unknown symbol '%s'.",
                        module_name, signature)
                else:
                    psyir = cntr.find_routine_psyir(str(signature))
                    if psyir:
                        # It is a routine, which we need to analyse for the use
                        # of non-local symbols:
                        outstanding_nonlocals.append(("routine", module_name,
                                                      signature, access_info))
                        continue

                    # Check whether it is a generic function (the symbol
                    # should always be found, but if a module cannot be
                    # parsed then the symbol table won't have been populated)
                    sym_tab = cntr.symbol_table
                    try:
                        sym = sym_tab.lookup(signature[0])
                    except KeyError:
                        logger.warning(
                            "[CallTreeUtils._resolve_calls_and_unknowns]"
                            "Cannot find symbol '%s'.", signature[0])
                        continue
                    # Check if we have a generic interface (of a function):
                    if isinstance(sym, GenericInterfaceSymbol):
                        all_possible_routines = \
                            cntr.resolve_routine(signature[0])
                        for function_name in all_possible_routines:
                            psyir = cntr.find_routine_psyir(function_name)
                            if not psyir:
                                logger.warning(
                                    "[CallTreeUtils._resolve_calls_and_"
                                    "unknowns] Could not get PSyIR for "
                                    "Function '%s' from module '%s' - "
                                    "ignored.", function_name, module_name)
                                continue
                            outstanding_nonlocals.append(
                                ("routine", module_name,
                                 Signature(function_name),
                                 access_info))
                        # It was a generic interface. All possible functions
                        # have been added, so this entry is done.
                        continue

                    # Check whether the unknown symbol is a constant, which
                    # can be ignored
                    if sym.is_constant:
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
            :py:class:`psyclone.core.AccessSequence`]]

        '''
        non_locals = self._compute_all_non_locals(routine)

        var_accesses = routine.reference_accesses()

        # Lookup the external names instead of the reference name which could
        # be renamed
        name_accesses = {}
        for sig, access in var_accesses.items():
            sym = routine.symbol_table.lookup(sig.var_name, otherwise=None)
            if sym and sym.is_import and sym.interface.orig_name:
                name = sym.interface.orig_name
            else:
                name = sig.var_name
            name_accesses[name] = access

        result = []
        for (symbol_type, module, signature) in non_locals:
            if symbol_type == "routine":
                result.append((symbol_type, module, signature, None))
                continue
            result.append((symbol_type, module, signature,
                           name_accesses[signature.var_name]))

        return result
