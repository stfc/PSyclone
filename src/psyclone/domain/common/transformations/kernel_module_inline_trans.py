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
import warnings

from psyclone.psyGen import Transformation, CodedKern
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.symbols import (
    ContainerSymbol, ImportInterface,
    GenericInterfaceSymbol, RoutineSymbol, Symbol, SymbolError, SymbolTable)
from psyclone.psyir.nodes import (
    Call, Container, FileContainer, Routine, ScopingNode,
    IntrinsicCall, )
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
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
    def validate(self, node, options=None, **kwargs):
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
        :raises TransformationError: if the call is to a polymorphic routine
            and there's no Container at the call site to which to add the
            interface definition.
        :raises TransformationError: if the kernel cannot be safely inlined.
        :raises TransformationError: if the target of the supplied call is
            already module inlined.

        '''
        if not options:
            self.validate_options(**kwargs)

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
            kernels = node.get_callees()
        except Exception as error:
            raise TransformationError(
                f"{self.name} failed to retrieve PSyIR for {kern_or_call} "
                f"'{kname}' due to: {error}"
            ) from error

        needs_inline = False

        if len(kernels) > 1:
            # We can't 'module' inline a call to an interface if there's no
            # ancestor Container in which to put the interface.
            cntr = node
            while cntr:
                cntr = cntr.ancestor(Container)
                if cntr and not isinstance(cntr, FileContainer):
                    break
            else:
                raise TransformationError(
                    f"Cannot module-inline the call to '{kname}' since it is "
                    f"a polymorphic routine (i.e. an interface) and the call-"
                    f"site is not within a module.")
            iface_sym = node.scope.symbol_table.lookup(kname, otherwise=None)
            if (not iface_sym or (iface_sym.is_import or
                                  iface_sym.is_unresolved)):
                needs_inline = True

        # Validate the PSyIR of each routine/kernel.
        for kernel_schedule in kernels:
            self._validate_schedule(node, kname, kern_or_call, kernel_schedule)
            rt_sym = node.scope.symbol_table.lookup(kernel_schedule.name,
                                                    otherwise=None)
            if (not rt_sym or (rt_sym is not kernel_schedule.symbol) or
                    (rt_sym.is_import or rt_sym.is_unresolved)):
                needs_inline = True

        if not needs_inline:
            raise TransformationError(
                f"The target of '{node.debug_string().strip()}' is already "
                f"module inlined.")

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
                new_rts = self._prepare_code_to_inline([kernel_schedule])
                if len(new_rts) == 1 and routine == new_rts[0]:
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

            vam = code_to_inline.reference_accesses()

            # First make a set with all symbols used inside the subroutine
            all_symbols = set()
            for sig in vam.all_signatures:
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
        # TODO #1734 - this *should* always be the same as `actual_table` but
        # this is not currently guaranteed.
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

    def apply(self, node, options=None, **kwargs):
        ''' Bring the implementation of this kernel/call into this Container.

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

        if options:
            # TODO 2668 - options dict is deprecated.
            warnings.warn(self._deprecation_warning, DeprecationWarning, 2)
        if not options:
            options = {}

        self.validate(node, options, **kwargs)

        external_callee_name = None
        if isinstance(node, CodedKern):
            caller_name = node.name
        else:
            caller_name = node.routine.symbol.name
            if (node.routine.symbol.is_import and
                    node.routine.symbol.interface.orig_name):
                external_callee_name = node.routine.symbol.interface.orig_name
        if not external_callee_name:
            external_callee_name = caller_name

        # Get the PSyIR of the routine to module inline as well as the name
        # with which it is being called.
        # Note that we use the resolved callee subroutine name and not the
        # caller one; this is important because if it is an interface it will
        # use the concrete implementation name. When this happens the new name
        # may already be in use, but the equality check below guarantees
        # that if it exists it is only valid when it references the exact same
        # implementation.
        codes_to_inline = node.get_callees()
        interface_sym = None
        if len(codes_to_inline) > 1:
            interface_sym = codes_to_inline[0].symbol_table.lookup(
                external_callee_name)

        callsite_table = node.scope.symbol_table

        if interface_sym:
            called_sym = callsite_table.lookup(caller_name, otherwise=None)
        else:
            for routine in codes_to_inline:
                # N.B. in a PSyKAl DSL, we won't have a RoutineSymbol for the
                # Kernel that is being called, so we look it up instead of
                # using node.symbol.
                called_sym = callsite_table.lookup(caller_name,
                                                   otherwise=None)
                if (not called_sym or called_sym is not routine.symbol or
                        (called_sym.is_import or called_sym.is_unresolved)):
                    # This routine is not module-inlined.
                    break

        # Deal with the RoutineSymbol that is in scope at the call site.
        sym_in_ctr = None
        shadowed_sym = None

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
                shadowed_sym = caller_cntr_table.lookup(called_sym.name,
                                                        otherwise=None)
                if shadowed_sym:
                    caller_cntr_table = shadowed_sym.find_symbol_table(
                        table.node.parent)
                    if not isinstance(caller_cntr_table.node, FileContainer):
                        # It is shadowing an outer symbol that is in a
                        # Container (not a FileContainer) so we just need to
                        # update the call to point to the outer symbol.
                        node.routine.symbol = shadowed_sym
                        if not (shadowed_sym.is_import or
                                shadowed_sym.is_unresolved):
                            # The outer symbol is local to this Container so
                            # there's nothing else to do.
                            return

        updated_routines = self._prepare_code_to_inline(codes_to_inline)

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
            # All Calls that referred to this Symbol must also be updated. Take
            # care that the name matches as sym_in_ctr might be an interface.
            if sym_in_ctr and sym_in_ctr.name == target_sym.name:
                for call in container.walk(Call):
                    if call.routine.symbol is sym_in_ctr:
                        call.routine.symbol = target_sym

        if interface_sym:
            # Deal with the interface symbol - remove any existing import and
            # then make sure the local symbol is private.
            self._rm_imported_routine_symbol(interface_sym,
                                             codes_to_inline[0],
                                             callsite_table)
            if shadowed_sym:
                self._rm_imported_routine_symbol(shadowed_sym,
                                                 codes_to_inline[0],
                                                 caller_cntr_table)
            if caller_name != interface_sym.name:
                # If the interface was originally renamed on import, then we
                # must create a new symbol with the local name.
                new_sym = GenericInterfaceSymbol(
                    caller_name, routines=[(RoutineSymbol("dummy"), True)])
                new_sym.copy_properties(interface_sym)
            else:
                # Otherwise we can use the existing symbol.
                new_sym = interface_sym
            container.symbol_table.add(new_sym)
            interface_sym.visibility = Symbol.Visibility.PRIVATE
            interface_sym.replace_symbols_using(container.symbol_table)
        else:
            # No interface but was the original routine symbol renamed
            # on import?
            if caller_name != external_callee_name:
                # It was so we need to rename the inlined routine.
                sym = node.scope.symbol_table.lookup(external_callee_name)
                table = sym.find_symbol_table(node)
                table.rename_symbol(sym, caller_name)

        # Update the Kernel to point to the updated PSyIR and set
        # the module-inline flag to avoid generating the kernel imports
        # TODO #1823. If the kernel imports were generated at PSy-layer
        # creation time, we could just remove it here instead of setting a
        # flag.
        if isinstance(node, CodedKern):
            cntr = node.ancestor(Container)
            # TODO #2846 - since we do not currently rename module-inlined
            # routines, inlining just one instance of a Kernel call and
            # subsequently transforming it (e.g. by adding ACC ROUTINE) would
            # inhibit all further transformations of any other calls to that
            # kernel (that relied upon inlining). Therefore, for now, we update
            # *all* calls to this particular kernel in the current module to
            # point to the module-inlined version.
            for kern in cntr.walk(CodedKern, stop_type=CodedKern):
                if kern.name == node.name:
                    kern.module_inline = True
                    # pylint: disable=protected-access
                    kern._schedules = updated_routines
