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

from psyclone.core import VariablesAccessInfo
from psyclone.psyGen import Transformation, CodedKern
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.symbols import (
    ContainerSymbol, DataSymbol, DataTypeSymbol, RoutineSymbol, Symbol,
    SymbolError)
from psyclone.psyir.nodes import (
    Container, Reference, Routine, ScopingNode,
    Literal, CodeBlock, Call, IntrinsicCall)


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

        parent_container = node.ancestor(Container)

        # Check that the associated Routine isn't already present in the
        # Container. Strictly speaking, we should check that the interface of
        # any existing Routine matches that required by the Call but for now
        # we live with the possibility of a false positive resulting in a
        # refusal to module inline.
        if routine_sym and not routine_sym.is_import:
            for routine in parent_container.walk(Routine, stop_type=Routine):
                if routine.name.lower() == kname.lower():
                    raise TransformationError(
                        f"{kern_or_call} '{kname}' cannot be module inlined "
                        f"into Container '{parent_container.name}' because "
                        f"there is no explicit import of it ('USE ..., ONLY: "
                        f"{kname}' in Fortran) and a Routine with that name "
                        f"is already present in the Container.")

        # Check that the PSyIR  of the routine/kernel can be retrieved.
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
        # TODO #2424 - this suffers from the limitation that
        # VariablesAccessInfo does not work with nested scopes. (e.g. 2
        # different symbols with the same name but declared in different,
        # nested scopes will be assumed to be the same symbol).
        vai = VariablesAccessInfo(kernel_schedule)
        rt_table = kernel_schedule.symbol_table
        for sig in vai.all_signatures:
            access = vai[sig].all_accesses[0]
            try:
                # The 'node' associated with an access may be a Symbol (if
                # the access is part of a symbol definition) or an
                # orphaned Node (e.g. within an initialisation expression).
                table = access.node.scope.symbol_table
            except (SymbolError, AttributeError):
                table = rt_table
            symbol = table.lookup(sig.var_name, otherwise=None,
                                  scope_limit=kernel_schedule)
            if not symbol:
                # The corresponding Symbol was not found within the scope
                # of the target Routine.
                outer_sym = kernel_schedule.symbol_table.lookup(sig.var_name,
                                                                otherwise=None)
                if outer_sym and outer_sym.is_modulevar:
                    raise TransformationError(
                        f"{kern_or_call} '{kname}' contains accesses to "
                        f"'{outer_sym.name}' which is declared in the callee "
                        f"module scope. Cannot inline such a {kern_or_call}.")

                raise TransformationError(
                    f"{kern_or_call} '{kname}' contains accesses to "
                    f"'{sig.var_name}' but the origin of this signature is "
                    f"unknown.")

        # We can't transform subroutines that shadow top-level symbol module
        # names, because we won't be able to bring this into the subroutine
        symtab = kernel_schedule.ancestor(Container).symbol_table
        for scope in kernel_schedule.walk(ScopingNode):
            for symbol in scope.symbol_table.symbols:
                for mod in symtab.containersymbols:
                    if (symbol.name == mod.name and
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

    @staticmethod
    def _prepare_code_to_inline(routines_to_inline):
        '''Prepare the PSyIR tree to inline by bringing in to the subroutine
        all referenced symbols so that the implementation is self contained.

        The provided routines are copied so that the original PSyIR is left
        unmodified.

        TODO #2271 will improve this method and could potentially
        avoid the need for debug_string() within get_kernel_schedule()
        in dynamo0p3.py. Sergi suggests that we may be missing the
        traversal of the declaration init expressions here and that
        might solve the problem. I'm not so sure and explain why in
        get_kernel_schedule() but still referencing this issue.

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
            # First make a set with all symbols used inside the subroutine
            all_symbols = set()
            init_exprns = []
            for scope in code_to_inline.walk(ScopingNode):
                for symbol in scope.symbol_table.symbols:
                    all_symbols.add(symbol)
                for symbol in scope.symbol_table.datasymbols:
                    if symbol.initial_value:
                        init_exprns.append(symbol.initial_value)
            for tree in init_exprns + [code_to_inline]:
                for reference in tree.walk(Reference):
                    all_symbols.add(reference.symbol)
                for literal in tree.walk(Literal):
                    # Literals may reference symbols in their precision
                    if isinstance(literal.datatype.precision, Symbol):
                        all_symbols.add(literal.datatype.precision)
            for caller in code_to_inline.walk(Call):
                all_symbols.add(caller.routine.symbol)
            for cblock in code_to_inline.walk(CodeBlock):
                for name in cblock.get_symbol_names():
                    all_symbols.add(cblock.scope.symbol_table.lookup(name))

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

    def apply(self, node, options=None):
        ''' Bring the kernel/subroutine into this Container.

        NOTE: when applying this transformation to a Kernel in a PSyKAl invoke,
        *all* Kernels of that name in that invoke are marked as inlined.

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

        container = node.ancestor(Container)
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
                        local_sym.is_import):
                    # This routine is not module-inlined.
                    break
            else:
                # All routines are module-inlined so there's nothing to do.
                # TODO #11 - log this.
                if isinstance(node, CodedKern):
                    node.module_inline = True
                return

        if local_sym and local_sym.is_import:
            # Double check that this import is not shadowing a routine we've
            # already module-inlined.
            table = local_sym.find_symbol_table(node)
            outer_sym = None
            if table.node.parent:
                outer_table = table.node.parent.scope.symbol_table
                outer_sym = outer_table.lookup(local_sym.name,
                                               otherwise=None)
            if outer_sym:
                # It is shadowing an outer symbol so we need to remove this
                # local symbol and update the call to point to the outer one.
                self._rm_imported_symbol(local_sym.name, table)
                node.routine.symbol = outer_sym
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

        for code_to_inline in updated_routines:
            try:
                existing_symbol = node.scope.symbol_table.lookup(
                    code_to_inline.name)
            except KeyError:
                existing_symbol = None

            if not existing_symbol:
                # If it doesn't exist already, module-inline the subroutine by
                # inserting the relevant code into the tree.
                # We need to set the visibility of the routine's symbol to
                # be private.
                code_to_inline.symbol.visibility = Symbol.Visibility.PRIVATE
                container.addchild(code_to_inline.detach())
            else:
                if existing_symbol.is_import:
                    # The RoutineSymbol is in the table but that is
                    # because it is imported. We must therefore update
                    # its interface and potentially remove the
                    # ContainerSymbol (from which it is imported)
                    # altogether.
                    csym = existing_symbol.interface.container_symbol
                    # The import of the routine symbol may be in an
                    # outer scope.
                    ctable = csym.find_symbol_table(node)
                    remove_csym = (ctable.symbols_imported_from(csym) ==
                                   [existing_symbol])
                    if code_to_inline.name == existing_symbol.name:
                        # Have to remove Symbol as adding the Routine into
                        # the Container will insert it again.
                        ctable._symbols.pop(existing_symbol.name)
                    if remove_csym:
                        ctable.remove(csym)
                    # Inline the code. This will automatically add the
                    # associated RoutineSymbol into the Container.
                    code_to_inline = code_to_inline.detach()
                    container.addchild(code_to_inline)
                    sym = ctable.lookup(code_to_inline.name)
                    sym.visibility = Symbol.Visibility.PRIVATE
                else:
                    # The routine symbol already exists, and we know from the
                    # validation that it's a Routine. Now check if they are
                    # exactly the same.
                    for routine in container.walk(Routine, stop_type=Routine):
                        if routine.name.lower() == code_to_inline.name.lower():
                            # This TransformationError happens here and not in
                            # the validation because it needs the
                            # symbols_to_bring_in applied to effectively
                            # compare both versions.  This will be fixed when
                            # module-inlining versioning is implemented.  (It
                            # is OK to fail here because we have not yet made
                            # any modifications to the tree - code_to_inline
                            # is a detached copy.)
                            if routine != code_to_inline:
                                raise TransformationError(
                                    f"Cannot inline subroutine '{node.name}'"
                                    f" because another, different, subroutine "
                                    f"with the same name already exists and "
                                    f"versioning of module-inlined subroutines"
                                    f" is not implemented yet.")

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
