# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module provides the KernelModuleInlineTrans transformation.

TODO #2683 - rename this to {Privatise,Copy,Move}RoutineToLocalContainerTrans
and move it to psyir/transformations/.

'''
import logging
from typing import Any, Optional, Union
import warnings

from psyclone.psyGen import Transformation, CodedKern
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.symbols import (
    ContainerSymbol, GenericInterfaceSymbol, RoutineSymbol, Symbol,
    SymbolError)
from psyclone.psyir.nodes import (
    Call, Container, FileContainer, IntrinsicCall, Reference, Routine,
    Schedule, ScopingNode)
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class KernelModuleInlineTrans(Transformation):
    ''' Brings the routine being called into the same Container as the call
    site. For example:

    >>> from psyclone.domain.common.transformations import \\
    ...     KernelModuleInlineTrans
    >>> from psyclone.psyGen import CodedKern
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Call
    >>>
    >>> psyir = FortranReader().psyir_from_source("""
    ...     module one
    ...       contains
    ...       subroutine my_subroutine()
    ...         integer, dimension(10, 10) :: A
    ...         A(:,:) = 0
    ...       end subroutine my_subroutine
    ...     end module one
    ...     module two
    ...       use one, only: my_subroutine
    ...       contains
    ...       subroutine call_it()
    ...         call my_subroutine()
    ...         call my_subroutine()
    ...       end subroutine call_it
    ...     end module two
    ...     """)
    >>> call = psyir.walk(Call)[-1]
    >>> inline_trans = KernelModuleInlineTrans()
    >>> inline_trans.apply(call)
    >>>
    >>> print(call.parent.parent.debug_string())
    module two
      use one, only : my_subroutine
      implicit none
      public
    <BLANKLINE>
      private :: my_subroutine_inlined_
    <BLANKLINE>
      contains
      subroutine call_it()
    <BLANKLINE>
        call my_subroutine_inlined_()
        call my_subroutine_inlined_()
    <BLANKLINE>
      end subroutine call_it
      subroutine my_subroutine_inlined_()
        integer, dimension(10,10) :: a
    <BLANKLINE>
        a(:,:) = 0
    <BLANKLINE>
      end subroutine my_subroutine_inlined_
    <BLANKLINE>
    end module two
    <BLANKLINE>

    .. warning ::
        Not all Routines can be moved. This transformation will reject
        attempts to move routines that access private data in the
        original Container.

    If the target routine itself contains calls to other routines within
    the same module, this transformation will first module-inline those
    routines in order to permit the target one to be inlined.

    '''
    def __str__(self):
        return ("Copy the routine associated with a (Kernel) call into the "
                "Container of the call site.")

    # pylint: disable=too-many-branches
    def validate(self,
                 node: Union[CodedKern, Call],
                 options: Optional[dict[str, Any]] = None,
                 **kwargs) -> None:
        '''
        Checks that the supplied node is a Kernel or Call and that it is
        possible to copy its PSyIR into the parent Container.

        If the target of the supplied (Kernel) Call is already in local
        scope then further checks are skipped.

        :param node: the kernel or call which is the target of the
                     transformation.
        :param options: a dictionary with options for transformations.

        :raises TransformationError: if the target node is not a sub-class of
            psyGen.CodedKern or psyir.nodes.Call or is an IntrinsicCall.
        :raises TransformationError: if the call is to a polymorphic routine
            and there's no Container at the call site to which to add the
            interface definition.
        :raises TransformationError: if the kernel cannot be safely inlined.

        '''
        if not options:
            self.validate_options(**kwargs)

        if isinstance(node, CodedKern):
            kern_or_call = "Kernel"
        elif isinstance(node, Call):
            if isinstance(node, IntrinsicCall):
                raise TransformationError(
                    f"Cannot module-inline a call to an intrinsic (got "
                    f"'{node.debug_string()}')")
            kern_or_call = "routine"
        else:
            raise TransformationError(
                f"Target of a {self.name} must be a sub-class of "
                f"psyGen.CodedKern or psyir.nodes.Call but got "
                f"'{type(node).__name__}'")

        # Check that the PSyIR of the routine/kernel can be retrieved.
        kname = node.routine.symbol.name
        try:
            kernels = node.get_callees()
        except Exception as error:
            raise TransformationError(
                f"{self.name} failed to retrieve PSyIR for {kern_or_call} "
                f"'{kname}' due to: {error}"
            ) from error

        # Return early if the target of the (Kernel) Call is already local.
        if self._target_is_local(node):
            return

        if len(kernels) > 1:
            # We can't bring the target of a call to an interface into local
            # scope if there's no Container in which to put the interface.
            cntr = node
            while cntr:
                cntr = cntr.ancestor(Container)
                if cntr and not isinstance(cntr, FileContainer):
                    break
            else:
                raise TransformationError(
                    f"Cannot copy the target of the call to '{kname}' since "
                    f"it is a polymorphic routine (i.e. an interface) and the "
                    f"call-site is not within a module.")

        # Validate the PSyIR of each routine/kernel.
        for kernel_schedule in kernels:
            self._validate_schedule(node, kname, kern_or_call, kernel_schedule)

    def _validate_schedule(self,
                           node: Union[CodedKern, Call],
                           kname: str,
                           kern_or_call: str,
                           kernel_schedule: Schedule):
        '''
        Validates that the supplied schedule can be module-inlined.

        :param node: the candidate kernel/routine call to inline.
        :param kname: the name of the kernel/routine.
        :param kern_or_call: text for readable error messages.
        :param kernel_schedule: the schedule of the routine to inline.

        :raises TransformationError: if the schedule contains accesses
            to data declared in the same module scope or of unknown origin.
        :raises TransformationError: if the schedule contains static Symbols.
        :raises TransformationError: if the schedule contains a local
            Symbol that shadows a module name in its outer scope.

        '''
        # We do not support kernels that use symbols representing data
        # declared in their own parent module (we would need to add new imports
        # from this module at the call site, and we don't do this yet).
        try:
            kernel_schedule.check_outer_scope_accesses(
                node, kern_or_call, ignore_non_data_accesses=True)
        except SymbolError as err:
            raise TransformationError(
                f"Cannot apply {self.name} to {kern_or_call} '{kname}' "
                f"because it accesses data from its outer scope: "
                f"{err.value}") from err

        # Check for any static Symbols. We can't permit these because if the
        # target routine is called from other places then we'll change the
        # results.
        static_syms = [sym for sym in kernel_schedule.symbol_table.datasymbols
                       if sym.is_static]
        if static_syms:
            names = ", ".join(f"'{sym.name}'" for sym in static_syms)
            raise TransformationError(
                f"Cannot apply {self.name} to {kern_or_call} '{kname}' "
                f"because it contains static data symbol(s): {names}")

        # If this Schedule itself contains Calls to local routines then
        # we can only module-inline it if the targets of those Calls can
        # also be module inlined.
        container = kernel_schedule.ancestor(Container)
        for call in kernel_schedule.walk(Call):
            symbol = call.routine.symbol
            if symbol.is_import or symbol.is_unresolved:
                continue
            local_routines = container.resolve_routine(symbol.name)
            for lrt in local_routines:
                rt_psyir = container.find_routine_psyir(
                    lrt, allow_private=True)
                # Recursively check the schedule of the target routine.
                self._validate_schedule(node, f"{kname}->{lrt}",
                                        "routine", rt_psyir)

        # We handle cases where the target routine accesses symbols that
        # are imported into an outer scope by bringing those imports inside
        # the target routine. However, if the target routine already contains
        # a symbol that shadows the name of the source module of such an
        # import then we cannot do this. (We could attempt to rename the local
        # symbol.)
        symtab = kernel_schedule.ancestor(Container).symbol_table
        ctr_names = [sym.name.lower() for sym in symtab.containersymbols]
        for scope in kernel_schedule.walk(ScopingNode):
            for symbol in scope.symbol_table.symbols:
                if (symbol.name.lower() in ctr_names and
                        not isinstance(symbol, ContainerSymbol)):
                    raise TransformationError(
                        f"{kern_or_call} '{kname}' cannot be module-"
                        f"inlined because the subroutine contains a symbol "
                        f"'{symbol.name}' which shadows the name of a module "
                        f"in the outer scope.")

    @staticmethod
    def _prepare_code_to_inline(
            routines_to_inline: list[Routine]) -> list[Routine]:
        '''Prepare the PSyIR tree to inline by bringing in to the subroutine
        all referenced symbols so that the implementation is self contained.

        The supplied routines are copied so that the original PSyIR is left
        unmodified.

        :param routines_to_inline: the routine(s) to module-inline.

        :returns: the updated routine(s) to module-inline.

        '''
        # pylint: disable=too-many-branches
        orig_container = routines_to_inline[0].ancestor(Container)
        # Since we will be detaching Routines, we work with a copy of
        # the Container that encapsulates them.
        source_container = orig_container.copy()
        # Make a dict containing *all* Routines in the Container, keyed by
        # routine name.
        new_routines = {}
        for routine in source_container.walk(Routine):
            new_routines[routine.name] = routine

        # Recursively collect any local routines that the target routines
        # themselves call.
        all_routines_to_inline: dict[str, Routine] = {}
        all_interfaces: dict[str, list[str]] = {}
        KernelModuleInlineTrans._get_all_routines_to_inline(
            all_routines_to_inline, all_interfaces,
            source_container, new_routines, routines_to_inline)

        copied_routines = []
        for orig_routine in all_routines_to_inline.values():
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
                    # This symbol may already be in the local symbol table,
                    # but adding it to the 'symbols_to_bring_in' will make the
                    # next step bring into the subroutine all modules that it
                    # could come from.
                    symbols_to_bring_in.add(symbol)

            # Bring the selected symbols inside the subroutine
            for symbol in symbols_to_bring_in:
                if symbol.name not in code_to_inline.symbol_table:
                    if symbol.is_import:
                        # We must update its import interface (to ensure it
                        # references a ContainerSymbol in the correct scope)
                        # before it can be added to the table.
                        code_to_inline.symbol_table.\
                            localise_import_interface_of(symbol)
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
        return copied_routines, all_interfaces

    @staticmethod
    def _get_all_routines_to_inline(
            routines_to_copy: dict[str, Routine],
            interfaces_to_copy: dict[str, list[str]],
            container: Container,
            routine_map: dict[str, Routine],
            routines_to_examine: list[Routine]
    ) -> None:
        '''
        Recursively examine each Routine in the supplied list
        `routines_to_examine` and add the *local* targets of any Calls to the
        list of routines/interfaces to copy into the Container of the call
        site.

        :param routines_to_copy: the Routines that need to be copied to the
            call site. Keys are routine names, values are the Routine objects.
        :param interfaces_to_copy: the generic interfaces that need to be
            copied to the call site. Keys are interface names, corresponding
            value is a list of the routine names in the interface.
        :param container: the Container holding the routines.
        :param routine_map: dict holding all of the Routines in the current
            Container, indexed by name.
        :param routines_to_examine: the list of Routines to check for calls to
                                    local Routines.
        '''
        for routine in routines_to_examine:
            for call in routine.walk(Call):
                if isinstance(call, IntrinsicCall):
                    continue
                # Is this a Call of a 'local' routine? (Allow for interface
                # symbols which will have an 'automatic' interface.)
                if ((call.symbol.is_modulevar or call.symbol.is_automatic) and
                        not call.symbol.is_import):
                    names = container.resolve_routine(call.symbol.name)
                    if len(names) > 1:
                        # This is a call to an interface. Add its name and
                        # constituent routine (names) to the dict.
                        interfaces_to_copy[call.symbol.name] = names
                    # Add any local routines called by the target(s) of this
                    # call.
                    KernelModuleInlineTrans._get_all_routines_to_inline(
                        routines_to_copy,
                        interfaces_to_copy,
                        container,
                        routine_map,
                        [routine_map[name] for name in names])
            # Add this routine to the dict of routines to be copied.
            routines_to_copy[routine.symbol.name] = routine

    def _target_is_local(self, node: Union[Call, CodedKern]) -> bool:
        '''
        :returns: whether or not the target of the supplied call/kernel
                  is already in local scope.
        '''
        kname = node.routine.symbol.name

        routines = node.get_callees()

        if len(routines) > 1:
            iface_sym = node.scope.symbol_table.lookup(kname, otherwise=None)
            if (not iface_sym or (iface_sym.is_import or
                                  iface_sym.is_unresolved)):
                return False

        for kernel_schedule in routines:
            rt_sym = node.scope.symbol_table.lookup(kernel_schedule.name,
                                                    otherwise=None)
            if (not rt_sym or (rt_sym is not kernel_schedule.symbol) or
                    (node.ancestor(Container) is not
                     kernel_schedule.ancestor(Container)) or
                    (rt_sym.is_import or rt_sym.is_unresolved)):
                return False

        logger = logging.getLogger(__name__)
        logger.info(
            f"The target of '{node.debug_string().strip()}' is already "
            f"present in the local scope.")

        return True

    def apply(self,
              node: Union[CodedKern, Call],
              options: dict[str, Any] = None,
              update_all: bool = True,
              **kwargs):
        ''' Bring the implementation of this kernel/call into this Container.

        NOTE: when applying this transformation to a Kernel in a PSyKAl invoke,
        by default *all* calls to that Kernel are updated.
        Similarly, when applied to a Call to a Routine in a particular scope,
        *all* such Calls are updated. This behaviour may be changed using the
        `update_all=False` option.

        :param node: the Kernel or Call to module-inline.
        :param options: a dictionary with options for transformations.
        :param update_all: whether or not to update *all* (kernel) calls to the
           target Kernel/routine within the program unit.

        '''
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

        # Get the PSyIR of the routine(s) to module inline.
        codes_to_inline = node.get_callees()

        # We will construct a dict of interfaces to copy in. Keys are the
        # interface names, values are a list of procedure names.
        inlined_interfaces: dict[str, list[str]] = {}
        if len(codes_to_inline) > 1:
            # If there are multiple routines then the target of the call must
            # be an interface.
            inlined_interfaces[external_callee_name] = [
                rt.symbol.name for rt in codes_to_inline]

        if self._target_is_local(node):
            return

        # Prepare the routines to be copied into the local scope (take copies,
        # move imports inside them). Also collect any interfaces that have to
        # be copied over.
        updated_routines, interfaces = self._prepare_code_to_inline(
            codes_to_inline)
        inlined_interfaces.update(interfaces)

        # The Container into which we will inline the Routine(s).
        container = node.ancestor(Container)

        # Mapping from original name to new RoutineSymbol.
        name_map: dict[str, RoutineSymbol] = {}

        for code_to_inline in updated_routines:
            # Create a new name for the routine.
            new_name = f"{code_to_inline.name}_inlined_"
            new_sym = container.symbol_table.new_symbol(
                new_name, symbol_type=RoutineSymbol)
            new_sym.copy_properties(code_to_inline.symbol,
                                    exclude_interface=True)
            # Do not expose the new symbol externally to prevent unexpected
            # collisions.
            new_sym.visibility = Symbol.Visibility.PRIVATE
            # Add the new symbol to the map.
            name_map[code_to_inline.name] = new_sym
            # Add the routine code into this Container
            code_to_inline = code_to_inline.detach()
            code_to_inline.symbol = new_sym
            container.addchild(code_to_inline)

        # TODO #3142 - once we have support for giving Symbols an 'EXTERNAL'
        # interface then we should do this if `container` is a FileContainer.

        # We have to create new interface symbols for any interfaces that we
        # are bringing into this Container.
        for iface_name, member_names in inlined_interfaces.items():
            # Create a new name for the interface
            new_name = f"{iface_name}_inlined_"
            new_sym = container.symbol_table.new_symbol(
                new_name, symbol_type=GenericInterfaceSymbol,
                routines=[(name_map[name], True) for name in member_names],
                visibility=Symbol.Visibility.PRIVATE)
            name_map[iface_name] = new_sym

        # Update any calls to other routines the routines we have moved into
        # this Container as they may now also point to local copies (renamed)
        # of the routines.
        for code_to_inline in updated_routines:
            for call in code_to_inline.walk(Call):
                if isinstance(call, IntrinsicCall):
                    continue
                if call.symbol.name in name_map:
                    call.routine.symbol = name_map[call.symbol.name]

        if update_all:
            # We will update all Calls/Kernels associated with the
            # target routine.
            all_calls = container.walk(type(node))
        else:
            # Only update the supplied Call/Kernel.
            all_calls = [node]

        target_sym = name_map.get(caller_name, None)
        if not target_sym:
            # If we haven't copied in a routine of 'caller_name' then it must
            # be because the target of the call is renamed on import.
            target_sym = name_map.get(external_callee_name)

        for call in all_calls:
            name = call.routine.symbol.name.lower()
            if name == caller_name:
                if isinstance(node, Call):
                    call.routine.symbol = target_sym
                else:
                    # Otherwise node is a CodedKern.
                    call.routine = Reference(target_sym)
                    call._schedules = updated_routines

        # In theory we could remove the import of the original routine but
        # that is dangerous (e.g. if it's a subroutine-scoped or
        # module-private symbol) and unnecessary so we don't.
