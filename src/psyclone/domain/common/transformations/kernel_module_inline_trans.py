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
from typing import List

from psyclone.core import VariablesAccessInfo
from psyclone.psyGen import Transformation, CodedKern
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.symbols import (
    ContainerSymbol, DataSymbol, DataTypeSymbol,
    ImportInterface, RoutineSymbol, Symbol, SymbolError, SymbolTable)
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
        try:
            kernel_schedule.check_outer_scope_accesses(node, kern_or_call)
        except SymbolError as err:
            raise TransformationError(
                f"Cannot apply {self.name} to {kern_or_call} '{kname}' "
                f"because it accesses data from its outer scope: "
                f"{err.value}") from err

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
        if existing_symbol and not isinstance(existing_symbol, RoutineSymbol):
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
    def _prepare_code_to_inline(
            routines_to_inline: List[Routine]) -> List[Routine]:
        '''Prepare the PSyIR tree to inline by bringing in to the subroutine
        all referenced symbols so that the implementation is self contained.

        The provided routines are copied so that the original PSyIR is left
        unmodified.

        :param routines_to_inline: the routine(s) to module-inline.

        :returns: the updated routine(s) to module-inline.
        :rtype: list[:py:class:`psyclone.psyir.node.Routine`]

        '''
        # pylint: disable=too-many-branches
        orig_container = routines_to_inline[0].ancestor(Container)
        # Since we will be detaching Routines, we work with a copy of
        # the Container that encapsulates them.
        source_container = orig_container.copy()
        new_routines = {}
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
    def _rm_imported_routine_symbol(symbol: Symbol,
                                    schedule: Routine,
                                    table: SymbolTable):
        '''
        If the named symbol is in the supplied table (or an ancestor) *and* is
        an import then it is removed. If the Container from which it was being
        imported no longer has any imports associated with it then the
        ContainerSymbol is also removed.

        TODO #2846 - this method is only required because at present we do not
        rename inlined routines. However, not only is removing an imported
        routine symbol dangerous, it also will not work if it should happen
        that the same Routine is brought into scope through multiple
        wildcard imports.

        :param symbol: the symbol to remove.
        :param schedule: the Routine that is associated with this symbol.
        :type table: the table from which to remove the symbol.

        '''
        if symbol.is_unresolved:
            # The symbol is unresolved (at the call site) but we now have the
            # source of the target routine and thus can find out which
            # Container it is in.
            cntr = schedule.ancestor(Container, excluding=FileContainer)
            if cntr:
                cntr_name = cntr.name
                cntr_sym = table.lookup(cntr_name)
                # Now we have a ContainerSymbol, we can change the
                # interface of called_sym and proceed in exactly the
                # same way as if it had been resolved originally.
                symbol.interface = ImportInterface(cntr_sym)

        if not symbol.is_import:
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
        remove_csym = (ctable.symbols_imported_from(csym) == [symbol] and
                       not csym.wildcard_import)
        if csym.wildcard_import:
            # The Routine is brought into scope via a wildcard import. We have
            # to rename it on import to avoid a clash with the newly inlined
            # Routine.
            # TODO #2846 - if the same Routine is also brought into scope
            # through some other wildcard import then this renaming doesn't
            # help. The only solution to this is to rename the module-inlined
            # Routine (or proceed to fully inline it).
            KernelModuleInlineTrans._rename_import(ctable, csym, symbol.name)
        # pylint:disable-next=protected-access
        actual_table._symbols.pop(symbol.name.lower())
        if remove_csym:
            actual_table.remove(csym)

    @staticmethod
    def _rename_import(table: SymbolTable,
                       csym: ContainerSymbol,
                       name: str) -> None:
        '''
        Adds a new RoutineSymbol imported from `csym` with its original name
        set to be the supplied name while having an auto-generated actual name.
        If there is already such a Symbol then this routine does nothing.

        :param table: the SymbolTable to update.
        :param csym: the ContainerSymbol from which the routine symbol is
            imported.
        :param name: the name of the imported symbol.

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
        callsite_table = node.scope.symbol_table

        if interface_sym:
            called_sym = callsite_table.lookup(interface_sym.name,
                                               otherwise=None)
            if interface_sym is called_sym and not called_sym.is_import:
                # Interface symbol is already local so nothing to do.
                if isinstance(node, CodedKern):
                    node.module_inline = True
                # TODO #11 - log this.
                return
        else:
            caller_name = codes_to_inline[0].name
            for routine in codes_to_inline:
                # N.B.in a PSyKAl DSL, we won't have a RoutineSymbol for the
                # Kernel that is being called, so we look it up instead of
                # using node.symbol.
                called_sym = callsite_table.lookup(caller_name,
                                                   otherwise=None)
                if (not called_sym or called_sym is not routine.symbol or
                        (called_sym.is_import or called_sym.is_unresolved)):
                    # This routine is not module-inlined.
                    break
            else:
                # All routines are module-inlined so there's nothing to do.
                # TODO #11 - log this.
                if isinstance(node, CodedKern):
                    node.module_inline = True
                return

        # Deal with the RoutineSymbol that is in scope at the call site.
        sym_in_ctr = None

        if called_sym and (called_sym.is_import or called_sym.is_unresolved):
            table = called_sym.find_symbol_table(node)
            if isinstance(table.node, Container):
                # The RoutineSymbol is declared in the ancestor Container.
                # Therefore, we need to keep a reference to it so that we can
                # update any other Calls to it (at the end of this method).
                sym_in_ctr = called_sym

            self._rm_imported_routine_symbol(called_sym, codes_to_inline[0],
                                             table)

            # Double check that this import is not shadowing a routine we've
            # already module-inlined.
            if table.node.parent:
                # There is a scope outside the one that contained the
                # RoutineSymbol.
                caller_cntr_table = table.node.parent.scope.symbol_table
                # Look to see whether it also contains a symbol matching
                # the name of the called routine.
                caller_cntr_sym = caller_cntr_table.lookup(called_sym.name,
                                                           otherwise=None)
                if caller_cntr_sym:
                    caller_cntr_table = caller_cntr_sym.find_symbol_table(
                        table.node.parent)
                    if not isinstance(caller_cntr_table.node, FileContainer):
                        # It is shadowing an outer symbol that is in a
                        # Container (not a FileContainer) so we just need to
                        # update the call to point to the outer symbol.
                        node.routine.symbol = caller_cntr_sym
                        if not (caller_cntr_sym.is_import or
                                caller_cntr_sym.is_unresolved):
                            # The outer symbol is local to this Container so
                            # there's nothing else to do.
                            return

        updated_routines = self._prepare_code_to_inline(codes_to_inline)

        # Update the Kernel to point to the updated PSyIR.
        if isinstance(node, CodedKern):
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
            sym = container.symbol_table.lookup(
                code_to_inline.name,
                scope_limit=container,
                otherwise=None)
            if not sym:
                # If it doesn't exist already, module-inline the subroutine by
                # inserting the relevant code into the tree. We need to set
                # the visibility of the routine's symbol to be private.
                sym = code_to_inline.symbol
                sym.visibility = Symbol.Visibility.PRIVATE
                container.addchild(code_to_inline.detach())

            elif sym.is_import:
                # The RoutineSymbol is imported into the table. We must
                # therefore update its interface and potentially remove the
                # ContainerSymbol (from which it is imported) altogether.
                self._rm_imported_routine_symbol(sym,
                                                 code_to_inline,
                                                 container.symbol_table)
                # Inline the code. This will automatically add the
                # associated RoutineSymbol into the Container.
                code_to_inline = code_to_inline.detach()
                container.addchild(code_to_inline)
                newsym = container.symbol_table.lookup(code_to_inline.name)
                newsym.visibility = Symbol.Visibility.PRIVATE

            elif sym.is_unresolved:
                # The Symbol in the Container scope is unresolved. However,
                # we've found the source so we now know where it comes from.
                self._rm_imported_routine_symbol(sym,
                                                 code_to_inline,
                                                 container.symbol_table)
                cntr = code_to_inline.ancestor(Container,
                                               excluding=FileContainer)
                if cntr:
                    # Inline the code. This will automatically add the
                    # associated RoutineSymbol into the Container.
                    code_to_inline = code_to_inline.detach()
                    container.addchild(code_to_inline)
                    newsym = container.symbol_table.lookup(code_to_inline.name)
                    newsym.visibility = Symbol.Visibility.PRIVATE

            # All Calls to a routine of the same name in the same scope as the
            # target node must refer to the same Symbol.
            target_name = sym.name.lower()
            target_sym = node.scope.symbol_table.lookup(target_name)
            for call in node.ancestor(Routine).walk(Call):
                name = call.routine.symbol.name.lower()
                if name == target_name:
                    call.routine.symbol = target_sym
            # All Calls that referred to this Symbol must also be updated.
            if sym_in_ctr:
                for call in container.walk(Call):
                    if call.routine.symbol is sym_in_ctr:
                        call.routine.symbol = target_sym

        if interface_sym:
            self._rm_imported_routine_symbol(interface_sym,
                                             codes_to_inline[0],
                                             callsite_table)
            if interface_sym.name not in container.symbol_table:
                container.symbol_table.add(interface_sym)
                interface_sym.replace_symbols_using(container.symbol_table)

        # Set the module-inline flag to avoid generating the kernel imports
        # TODO #1823. If the kernel imports were generated at PSy-layer
        # creation time, we could just remove it here instead of setting a
        # flag.
        if isinstance(node, CodedKern):
            node.module_inline = True
