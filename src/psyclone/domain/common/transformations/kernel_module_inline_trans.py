# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2026, Science and Technology Facilities Council.
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
from typing import Any, List, Optional, Union
import warnings

from psyclone.psyGen import Transformation, CodedKern
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.symbols import (
    ContainerSymbol, GenericInterfaceSymbol, RoutineSymbol, Symbol,
    SymbolError)
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
    def validate(self,
                 node: Union[CodedKern, Call],
                 options: Optional[dict[str, Any]] = None,
                 **kwargs):
        '''
        Checks that the supplied node is a Kernel or Call and that it is
        possible to inline its PSyIR into the parent Container.

        :param node: the kernel or call which is the target of the
                     transformation.
        :param options: a dictionary with options for transformations.

        :raises TransformationError: if the target node is not a sub-class of
            psyGen.CodedKern or psyir.nodes.Call or is an IntrinsicCall.
        :raises TransformationError: if there is no explicit import of the
            called Routine and there is already a Routine of that name in the
            parent Container.
        :raises TransformationError: if the call is to a polymorphic routine
            and there's no Container at the call site to which to add the
            interface definition.
        :raises TransformationError: if the kernel cannot be safely inlined.

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
        :raises TransformationError: if the called routine contains a local
             Symbol that shadows a module name in its outer scope.

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
        # names, because we won't be able to bring them into the subroutine.
        # (We could attempt to rename the local symbol.)
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

    def apply(self,
              node: Union[CodedKern, Call],
              options: dict[str, Any] = None,
              **kwargs):
        ''' Bring the implementation of this kernel/call into this Container.

        NOTE: when applying this transformation to a Kernel in a PSyKAl invoke,
        *only* that Kernel call is updated (and marked as inlined).
        Similarly, when applied to a Call to a Routine in a particular scope,
        *only* that Call is updated.

        :param node: the Kernel or Call to module-inline.
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

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

        updated_routines = self._prepare_code_to_inline(codes_to_inline)

        # The Container into which we will inline the Routine(s).
        container = node.ancestor(Container)

        name_map = {}
        for code_to_inline in updated_routines:

            new_name = code_to_inline.name+"_inlined_"
            new_sym = container.symbol_table.new_symbol(
                new_name, symbol_type=RoutineSymbol)
            new_sym.copy_properties(code_to_inline.symbol,
                                    exclude_interface=True)
            new_sym.visibility = Symbol.Visibility.PRIVATE
            code_to_inline = code_to_inline.detach()
            code_to_inline.symbol = new_sym
            container.addchild(code_to_inline)
            # Keep a record of the new and original names.
            name_map[code_to_inline.name] = new_sym

        if interface_sym:
            # Deal with the interface symbol - create a new, local
            # private version.
            new_sym = container.symbol_table.new_symbol(
                interface_sym.name+"_inlined_",
                symbol_type=GenericInterfaceSymbol,
                routines=[(sym, True) for sym in name_map.values()],
                visibility=Symbol.Visibility.PRIVATE)

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
                    kern._name = new_sym.name
                    kern.module_inline = True
                    # pylint: disable=protected-access
                    kern._schedules = updated_routines
        else:
            # Update the Call to point to the inlined routine.
            node.routine.symbol = new_sym
