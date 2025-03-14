# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         A. B. G. Chalk STFC Daresbury Lab
#         J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, Met Office

''' This module provides the KernelModuleInlineTrans transformation.

TODO #2683 - rename this to {Privatise,Copy,Move}RoutineToLocalContainerTrans
and move it to psyir/transformations/.

'''
from typing import Union

from psyclone.core import VariablesAccessInfo
from psyclone.psyGen import Transformation, CodedKern
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.symbols import (
    ContainerSymbol, DataSymbol, DataTypeSymbol,
    ImportInterface, RoutineSymbol, Symbol, SymbolError)
from psyclone.psyir.nodes import (
    Call, Container, FileContainer, Routine, ScopingNode, IntrinsicCall)


class KernelModuleInlineTrans(Transformation):
    ''' Brings the routine being called into the same Container as the call
    site. For example:

    .. code-block:: python

        from psyclone.domain.common.transformations import \\
                KernelModuleInlineTrans

        inline_trans = KernelModuleInlineTrans()
        inline_trans.apply(schedule.walk(CodedKern)[0])

        print(schedule.parent.view())


    .. warning ::
        Not all Routines can be moved. This transformation will reject
        attempts to move routines that access private data in the
        original Container.

    '''

    def __str__(self):
        return ("Copy the routine associated with a (Kernel) call into the "
                "Container of the call site.")

    # pylint: disable=too-many-branches
    def validate(self, node, options=None):
        '''
        Checks that the supplied node is a Kernel or Call and that it is
        possible to inline its PSyIR into the parent Container.

        :param node: the kernel or call which is the target of the
                     transformation.
        :type node: :py:class:`psyclone.psyGen.CodedKern` |
                    :py:class:`psyclone.psyir.nodes.Call`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the target node is not a sub-class of
            psyGen.CodedKern or psyir.nodes.Call or is an IntrinsicCall.
        :raises TransformationError: if there is no explicit import of the
            called Routine and there is already a Routine of that name in the
            parent Container.
        :raises TransformationError: if the kernel cannot be safely inlined.

        '''
        if isinstance(node, CodedKern):
            routine_sym = None
            kname = node.name
            kern_or_call = "Kernel"
        elif isinstance(node, Call):
            if isinstance(node, IntrinsicCall):
                raise TransformationError(
                    f"Cannot module-inline a call to an intrinsic (got "
                    f"'{node.debug_string()}')")
            routine_sym = node.routine.symbol
            kname = routine_sym.name
            kern_or_call = "routine"
        else:
            raise TransformationError(
                f"Target of a {self.name} must be a sub-class of "
                f"psyGen.CodedKern or psyir.nodes.Call but got "
                f"'{type(node).__name__}'")

        # Check that the PSyIR of the routine/kernel can be retrieved.
        try:
            kernels, _ = KernelModuleInlineTrans._get_psyir_to_inline(node)
        except Exception as error:
            raise TransformationError(
                f"{self.name} failed to retrieve PSyIR for {kern_or_call} "
                f"'{kname}' due to: {error}"
            ) from error

        # Validate the PSyIR of each routine/kernel.
        for kernel_schedule in kernels:
            self._validate_schedule(node, kname, kern_or_call, kernel_schedule)

    def _validate_schedule(self, node, kname, kern_or_call, kernel_schedule):
        '''
        Validates that the supplied schedule can be module-inlined.

        :param node: the candidate kernel/routine call to inline.
        :type node: :py:class:`psyclone.psyGen.CodedKern` |
                    :py:class:`psyclone.psyir.nodes.Call`
        :param str kname: the name of the kernel/routine.
        :param str kern_or_call: text for readable error messages.
        :param kernel_schedule: the schedule of the routine to inline.
        :type kernel_schedule: :py:class:`psyclone.psyir.nodes.Schedule`

        :raises TransformationError: if the called routine contains accesses
             to data declared in the same module scope or of unknown origin.
        :raises TransformationError: if the called routine has the same
             name as an existing Symbol in the calling scope (other than the
             one representing the routine itself).

        '''
        # We do not support kernels that use symbols representing data
        # declared in their own parent module (we would need to add new imports
        # from this module at the call site, and we don't do this yet).
        self.check_data_accesses(node, kernel_schedule, kern_or_call)

        # We can't transform subroutines that shadow top-level symbol module
        # names, because we won't be able to bring this into the subroutine
        symtab = kernel_schedule.ancestor(Container).symbol_table
        for scope in kernel_schedule.walk(ScopingNode):
            for symbol in scope.symbol_table.symbols:
                for mod in symtab.containersymbols:
                    if (symbol.name.lower() == mod.name.lower() and
                            not isinstance(symbol, ContainerSymbol)):
                        raise TransformationError(
                            f"{kern_or_call} '{kname}' cannot be module-"
                            f"inlined because the subroutine shadows the "
                            f"symbol name of the module container "
                            f"'{symbol.name}'.")

        # If the symbol already exists at the call site it must be referring
        # to a Routine
        existing_symbol = node.scope.symbol_table.lookup(kernel_schedule.name,
                                                         otherwise=None)
        if not existing_symbol:
            return
        if not isinstance(existing_symbol, RoutineSymbol):
            raise TransformationError(
                f"Cannot module-inline {kern_or_call} '{kname}' because "
                f"symbol '{existing_symbol}' with the same name already "
                f"exists and changing the name of module-inlined "
                f"subroutines is not supported yet.")

        # Check that the associated Routine isn't already present in the
        # Container. Strictly speaking, we should check that the interface of
        # any existing Routine matches that required by the Call but for now
        # we live with the possibility of a false positive resulting in a
        # refusal to module inline.
        parent_container = node.ancestor(Container)
        for routine in parent_container.walk(Routine, stop_type=Routine):
            if routine.name.lower() == kname.lower():
                # Compare the routine to be inlined with the one that
                # is already present.
                (new_rt, ) = self._prepare_code_to_inline([kernel_schedule])
                if routine == new_rt:
                    # It's the same so we can proceed (although all we need to
                    # do is update the RoutineSymbol referenced by the Call.)
                    return
                raise TransformationError(
                    f"{kern_or_call} '{kname}' cannot be module inlined "
                    f"into Container '{parent_container.name}' because "
                    f"a *different* routine with that name "
                    f"already exists and versioning of module-inlined "
                    f"subroutines is not implemented yet.")

    @staticmethod
    def check_data_accesses(call: Union[CodedKern, Call],
                            schedule: Routine,
                            kern_or_call: str):
        '''
        Check for unresolved symbols or for any accessed from the Container
        containing the target routine.

        :param call: the node representing the call to the routine that is to
            be inlined.
        :param schedule: the PSyIR schedule of the routine to be inlined.
        :param kern_or_call: text appropriate to whether we have a PSyKAl
            Kernel or a generic routine.

        :raises TransformationError: if there is an access to an unresolved
            symbol.
        :raises TransformationError: if there is an access to a symbol that is
            declared in the Container (module) holding the target routine.

        '''
        # TODO #2424 - this suffers from the limitation that
        # VariablesAccessInfo does not work with nested scopes. (e.g. 2
        # different symbols with the same name but declared in different,
        # nested scopes will be assumed to be the same symbol).
        vai = VariablesAccessInfo(schedule)
        table = schedule.symbol_table
        name = schedule.name
        for sig in vai.all_signatures:
            symbol = table.lookup(sig.var_name, otherwise=None)
            if not symbol:
                raise TransformationError(
                    f"{kern_or_call} '{name}' contains accesses to "
                    f"'{sig.var_name}' but the origin of this signature is "
                    f"unknown.")
            if symbol.is_unresolved:
                routine_wildcards = table.wildcard_imports()
                try:
                    # If there's more than one Container with a wildcard import
                    # this will raise a ValueError.
                    (csym,) = routine_wildcards
                    # Now we know the origin of this symbol we can update it.
                    symbol.interface = ImportInterface(csym)
                except (ValueError, KeyError):
                    try:
                        # We have more than one wildcard import.
                        table.resolve_imports(
                            container_symbols=routine_wildcards,
                            symbol_target=symbol)
                        if symbol.is_unresolved:
                            # Symbol is still not resolved (must be an indirect
                            # import) so we give up.
                            raise KeyError(
                                f"Failed to resolve the type of Symbol "
                                f"'{symbol.name}'. It is probably an indirect "
                                f"import.")
                    except KeyError as err:
                        raise TransformationError(
                            f"{kern_or_call} '{name}' contains accesses to "
                            f"'{symbol.name}' which is unresolved. It is being"
                            f" brought into scope from one of "
                            f"{[sym.name for sym in routine_wildcards]}. "
                            f"Original error was: {err}") from err
            if not symbol.is_import and symbol.name not in table:
                sym_at_call_site = call.scope.symbol_table.lookup(
                    sig.var_name, otherwise=None)
                if sym_at_call_site is not symbol:
                    raise TransformationError(
                        f"{kern_or_call} '{name}' contains accesses to "
                        f"'{symbol.name}' which is declared in the callee "
                        f"module scope. Cannot inline such a {kern_or_call}.")

        # We can't handle a clash between (apparently) different symbols that
        # share a name but are imported from different containers.
        routine_arg_list = schedule.symbol_table.argument_list[:]
        callsite_scopes = []
        cursor = call
        while cursor.ancestor(ScopingNode):
            callsite_scopes.append(cursor.ancestor(ScopingNode))
            cursor = cursor.ancestor(ScopingNode)
        for scope in schedule.walk(ScopingNode):
            scope_table = scope.symbol_table
            for callsite_scope in callsite_scopes:
                table = callsite_scope.symbol_table
                try:
                    table.check_for_clashes(
                        scope_table,
                        symbols_to_skip=routine_arg_list)
                except SymbolError as err:
                    raise TransformationError(
                        f"One or more symbols from routine '{name}' "
                        f"cannot be added to the table at the call site. "
                        f"Error was: {err}") from err

    @staticmethod
    def _prepare_code_to_inline(routines_to_inline):
        '''Prepare the PSyIR tree to inline by bringing in to the subroutine
        all referenced symbols so that the implementation is self contained.

        The provided routines are copied so that the original PSyIR is left
        unmodified.

        :param code_to_inline: the routine(s) to module-inline.
        :type code_to_inline: list[:py:class:`psyclone.psyir.node.Routine`]

        :returns: the updated routine(s) to module-inline.
        :rtype: list[:py:class:`psyclone.psyir.node.Routine`]

        '''
        # pylint: disable=too-many-branches
        orig_container = routines_to_inline[0].ancestor(Container)
        # Since we will be detaching Routines, we work with a copy of
        # the Container that encapsulates them.
        source_container = orig_container.copy()
        new_routines = dict()
        for routine in source_container.walk(Routine):
            new_routines[routine.name] = routine

        copied_routines = []
        for orig_routine in routines_to_inline:
            code_to_inline = new_routines[orig_routine.name]
            copied_routines.append(code_to_inline)

            vai = VariablesAccessInfo(code_to_inline)

            # First make a set with all symbols used inside the subroutine
            all_symbols = set()
            for sig in vai.all_signatures:
                all_symbols.add(
                    code_to_inline.symbol_table.lookup(sig.var_name))

            # Decide which symbols need to be brought inside the subroutine
            symbols_to_bring_in = set()
            for symbol in all_symbols:
                if symbol.is_unresolved or symbol.is_import:
                    # This symbol is already in the symbol table, but adding it
                    # to the 'symbols_to_bring_in' will make the next step
                    # bring into the subroutine all modules that it could come
                    # from.
                    symbols_to_bring_in.add(symbol)
                if isinstance(symbol, DataSymbol):
                    # DataTypes can reference other symbols
                    if isinstance(symbol.datatype, DataTypeSymbol):
                        symbols_to_bring_in.add(symbol.datatype)
                    elif hasattr(symbol.datatype, 'precision'):
                        if isinstance(symbol.datatype.precision, Symbol):
                            symbols_to_bring_in.add(symbol.datatype.precision)

            # Bring the selected symbols inside the subroutine
            for symbol in symbols_to_bring_in:
                if symbol.name not in code_to_inline.symbol_table:
                    code_to_inline.symbol_table.add(symbol)
                # And when necessary the modules where they come from
                if symbol.is_unresolved:
                    # We don't know where this comes from, we need to bring
                    # in all top-level imports with wildcard imports
                    for mod in source_container.symbol_table.containersymbols:
                        if mod.wildcard_import:
                            if mod.name not in code_to_inline.symbol_table:
                                code_to_inline.symbol_table.add(mod)
                            else:
                                code_to_inline.symbol_table.lookup(mod.name).\
                                    wildcard_import = True
                elif symbol.is_import:
                    module_symbol = symbol.interface.container_symbol
                    if module_symbol.name not in code_to_inline.symbol_table:
                        code_to_inline.symbol_table.add(module_symbol)
                    else:
                        # If it already exists, we know it's a container (from
                        # the validation) so we just need to point to it
                        symbol.interface.container_symbol = \
                            code_to_inline.symbol_table.lookup(
                                module_symbol.name)
        return copied_routines

    @staticmethod
    def _get_psyir_to_inline(node):
        '''
        Wrapper that gets the PSyIR of the routine or kernel
        corresponding to the call described by `node`. This supports calls to
        routines or kernels which are polymorphic by returning a list of
        Routine objects, as well as the associated interface symbol.

        :param node: the Call or CodedKern to resolve.
        :type node: :py:class:`psyclone.psyir.nodes.Call` |
                    :py:class:`psyclone.psyGen.CodedKern`

        :returns: the PSyIR of the routine implementation(s) and the associated
            interface symbol if it is polymorphic.
        :rtype: tuple[
            list[:py:class:`psyclone.psyir.nodes.Routine`],
            :py:class:`psyclone.psyir.symbols.GenericInterfaceSymbol`)]

        '''
        # TODO #2054 - once CodedKern has been migrated so that it subclasses
        # Call then this if/else (and thus this whole routine) can be removed.
        if isinstance(node, CodedKern):
            # We have a call to a Kernel in a PSyKAl API.
            # Where mixed-precision kernels are supported (e.g. in LFRic) the
            # call to get_kernel_schedule() will return the one which has an
            # interface matching the arguments in the call.
            interface_sym, routines = node.get_kernel_schedule()
            return (routines, interface_sym)

        # We have a generic routine call.
        routines = node.get_callees()
        caller_name = node.routine.name.lower()
        interface_sym = None
        if len(routines) > 1:
            interface_sym = routines[0].symbol_table.lookup(caller_name)

        return (routines, interface_sym)

    @staticmethod
    def _rm_imported_symbol(name, table):
        '''
        If the named symbol is in the supplied table (or an ancestor) *and* is
        an import then it is removed. If the Container from which it was being
        imported no longer has any imports associated with it then the
        ContainerSymbol is also removed.

        :param str name: the name of the symbol to remove.
        :param table: the symbol table from which to search for the symbol.
        :type table: :py:class:`psyclone.psyir.symbols.SymbolTable`

        '''
        symbol = table.lookup(name, otherwise=None)
        if not symbol or not symbol.is_import:
            return

        # The RoutineSymbol is in the table (or an outer scope) and is
        # imported. We therefore remove it and potentially the ContainerSymbol
        # from which it is imported.
        csym = symbol.interface.container_symbol

        actual_table = (symbol.find_symbol_table(table.node) if
                        symbol.name not in table else table)
        remove_csym = actual_table.symbols_imported_from(csym) == [symbol]
        # We have to force the removal as there will be calls that reference
        # this Symbol. (These calls will subsequently be updated to refer to
        # the Symbol of the inlined routine.)
        # pylint:disable-next=protected-access
        actual_table._symbols.pop(symbol.name)
        if remove_csym:
            actual_table.remove(csym)

    @staticmethod
    def _rm_imported_routine_symbol(name, table):
        '''
        If the named symbol is in the supplied table (or an ancestor) *and* is
        an import then it is removed. If the Container from which it was being
        imported no longer has any imports associated with it then the
        ContainerSymbol is also removed.

        :param str name: the name of the symbol to remove.
        :param table: the symbol table from which to search for the symbol.
        :type table: :py:class:`psyclone.psyir.symbols.SymbolTable`

        '''
        symbol = table.lookup(name, otherwise=None)
        if not symbol or not symbol.is_import:
            return

        # The symbol is in the table (or an outer scope) and is
        # imported. We therefore remove it and potentially the ContainerSymbol
        # from which it is imported.
        csym = symbol.interface.container_symbol
        # Find the table containing the symbol we're going to remove.
        actual_table = (symbol.find_symbol_table(table.node) if
                        symbol.name not in table else table)
        # Find the table containing the ContainerSymbol from which
        # the symbol is imported.
        ctable = (csym.find_symbol_table(table.node) if
                  csym.name not in table else table)
        remove_csym = ctable.symbols_imported_from(csym) == [symbol]
        if csym.wildcard_import:
            # The Routine is brought into scope via a wildcard import. We have
            # to rename it on import to avoid a clash with the newly inlined
            # Routine.
            KernelModuleInlineTrans._rename_import(ctable, csym, symbol.name)
            # pylint:disable-next=protected-access
            actual_table._symbols.pop(symbol.name)
        elif remove_csym:
            # We have to force the removal as there will be calls that
            # reference this Symbol. (These calls will subsequently be updated
            # to refer to the Symbol of the inlined routine.)
            # pylint:disable-next=protected-access
            actual_table._symbols.pop(symbol.name)
            actual_table.remove(csym)
        else:
            # pylint:disable-next=protected-access
            actual_table._symbols.pop(symbol.name)

    @staticmethod
    def _rename_import(table, csym, name):
        '''
        Adds a new RoutineSymbol imported from `table` with its original name
        set to be the supplied name while having an auto-generated actual name.
        If there is already such a Symbol then this routine does nothing.

        :param table:
        '''
        for isym in table.symbols_imported_from(csym):
            if (isym.interface.orig_name and
                    isym.interface.orig_name.lower() == name.lower()):
                # We already have a suitable import so we don't need
                # another one.
                return

        table.new_symbol(
            f"old_{name}",
            symbol_type=RoutineSymbol,
            interface=ImportInterface(
                csym, orig_name=name))


    def apply(self, node, options=None):
        ''' Bring the kernel/subroutine into this Container.

        NOTE: when applying this transformation to a Kernel in a PSyKAl invoke,
        *all* Kernels of that name in that invoke are marked as inlined.
        Similarly, when applied to a Call to a Routine in a particular scope,
        all Calls to a routine of that name in that scope are updated.

        :param node: the Kernel or Call to module-inline.
        :type node: :py:class:`psyclone.psyGen.CodedKern` |
                    :py:class:`psyclone.psyir.nodes.Call`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the called Routine cannot be brought
            into this Container because of a name clash with another Routine.
        :raises NotImplementedError: if node is a Call (rather than a
            CodedKern) and the name of the called routine does not match that
            of the caller.

        '''
        if isinstance(node, CodedKern) and node.module_inline:
            # This PSyKal Kernel is already module inlined.
            return

        if not options:
            options = {}

        self.validate(node, options)

        # Get the PSyIR of the routine to module inline as well as the name
        # with which it is being called.
        # Note that we use the resolved callee subroutine name and not the
        # caller one; this is important because if it is an interface it will
        # use the concrete implementation name. When this happens the new name
        # may already be in use, but the equality check below guarantees
        # that if it exists it is only valid when it references the exact same
        # implementation.
        codes_to_inline, interface_sym = (
            KernelModuleInlineTrans._get_psyir_to_inline(node))
        local_table = node.scope.symbol_table

        if interface_sym:
            local_sym = local_table.lookup(interface_sym.name, otherwise=None)
            if interface_sym is local_sym and not local_sym.is_import:
                # Interface symbol is already local so nothing to do.
                if isinstance(node, CodedKern):
                    node.module_inline = True
                # TODO #11 - log this.
                return
        else:
            for routine in codes_to_inline:
                local_sym = local_table.lookup(routine.symbol.name,
                                               otherwise=None)
                if (not local_sym or local_sym is not routine.symbol or
                        (local_sym.is_import or local_sym.is_unresolved)):
                    # This routine is not module-inlined.
                    break
            else:
                # All routines are module-inlined so there's nothing to do.
                # TODO #11 - log this.
                return

        if local_sym and (local_sym.is_import or local_sym.is_unresolved):
            # Double check that this import is not shadowing a routine we've
            # already module-inlined.
            table = local_sym.find_symbol_table(node)
            outer_sym = None
            if table.node.parent:
                outer_table = table.node.parent.scope.symbol_table
                outer_sym = outer_table.lookup(local_sym.name,
                                               otherwise=None)
            if outer_sym:
                outer_table = outer_sym.find_symbol_table(table.node.parent)
                if not isinstance(outer_table.node, FileContainer):
                    # It is shadowing an outer symbol that is in a Container
                    # (not a FileContainer) so we need to remove this local
                    # symbol and update the call to point to the outer one.
                    self._rm_imported_routine_symbol(local_sym.name, table)
                    node.routine.symbol = outer_sym
                    if not (outer_sym.is_import or outer_sym.is_unresolved):
                        # The outer symbol is local to this Container so
                        # there's nothing else to do.
                        return

        updated_routines = self._prepare_code_to_inline(codes_to_inline)
        # Update the Kernel to point to the updated PSyIR.
        if isinstance(node, CodedKern):
            # TODO - add setter for these properties to Kern?
            # pylint: disable=protected-access
            node._kern_schedules = updated_routines
            if interface_sym:
                node._interface_symbol = (
                    updated_routines[0].scope.symbol_table.lookup(
                        interface_sym.name))

        # The Container into which we will inline the Routine(s).
        container = node.ancestor(Container)

        for code_to_inline in updated_routines:

            # Does the Container already have this Routine?
            sym_in_ctr = container.symbol_table.lookup(code_to_inline.name,
                                                       scope_limit=container,
                                                       otherwise=None)
            if not sym_in_ctr:
                # If it doesn't exist already, module-inline the subroutine by
                # inserting the relevant code into the tree.
                # We need to set the visibility of the routine's symbol to
                # be private.
                code_to_inline.symbol.visibility = Symbol.Visibility.PRIVATE
                container.addchild(code_to_inline.detach())
                continue

            if sym_in_ctr.is_import:
                # The RoutineSymbol is imported into the table. We must
                # therefore update its interface and potentially remove the
                # ContainerSymbol (from which it is imported) altogether.
                self._rm_imported_routine_symbol(sym_in_ctr.name,
                                                 container.symbol_table)
                # Inline the code. This will automatically add the
                # associated RoutineSymbol into the Container.
                code_to_inline = code_to_inline.detach()
                container.addchild(code_to_inline)
                sym = container.symbol_table.lookup(code_to_inline.name)
                sym.visibility = Symbol.Visibility.PRIVATE

            elif sym_in_ctr.is_unresolved:
                # The Symbol in the Container scope is unresolved. However,
                # we've found the source so we now know where it comes from.
                cntr = code_to_inline.ancestor(Container,
                                               excluding=FileContainer)
                if cntr:
                    cntr_name = cntr.name
                    cntr_sym = container.symbol_table.lookup(cntr_name)
                    # Now we have a ContainerSymbol, we can change the
                    # interface of sym_in_ctr and proceed in exactly the
                    # same way as if it had been resolved originally.
                    sym_in_ctr.interface = ImportInterface(cntr_sym)
                    self._rm_imported_routine_symbol(sym_in_ctr.name,
                                                     container.symbol_table)
                    # Inline the code. This will automatically add the
                    # associated RoutineSymbol into the Container.
                    code_to_inline = code_to_inline.detach()
                    container.addchild(code_to_inline)
                    sym = container.symbol_table.lookup(code_to_inline.name)
                    sym.visibility = Symbol.Visibility.PRIVATE
                else:
                    # The routine is in the FileContainer containing
                    # the callsite so is not imported from a Container.
                    pass
            else:
                # The Routine is present in the Container.
                sym = sym_in_ctr
            # All Calls in the same scope to a routine of the same
            # name must refer to the same Symbol.
            target_name = sym.name.lower()
            for call in node.ancestor(Routine).walk(Call):
                name = call.routine.symbol.name.lower()
                if name == target_name:
                    call.routine.symbol = sym
            # All Calls that referred to this Symbol must also be updated.
            for call in container.walk(Call):
                if call.routine.symbol is sym_in_ctr:
                    call.routine.symbol = sym

        if interface_sym:
            self._rm_imported_symbol(interface_sym.name, local_table)
            if interface_sym.name not in container.symbol_table:
                container.symbol_table.add(interface_sym)
                interface_sym.replace_symbols_using(container.symbol_table)

        # Set the module-inline flag to avoid generating the kernel imports
        # TODO #1823. If the kernel imports were generated at PSy-layer
        # creation time, we could just remove it here instead of setting a
        # flag.
        node.module_inline = True
