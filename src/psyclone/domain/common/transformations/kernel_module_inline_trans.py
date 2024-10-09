# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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

from psyclone.psyGen import Transformation, CodedKern
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.symbols import (
    ContainerSymbol, DataSymbol, DataTypeSymbol,
    IntrinsicSymbol, RoutineSymbol, Symbol)
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
        :raises TransformationError: if the PSyIR of the implementation of the
            called Routine/kernel cannot be retrieved.
        :raises TransformationError: if the name of the routine that
            implements the kernel is not the same as the kernel name. This
            will happen if the kernel is polymorphic (uses a Fortran
            INTERFACE) and will be resolved by #1824.
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
            _, kernels, _ = (
                KernelModuleInlineTrans._get_psyir_to_inline(node))
        except Exception as error:
            raise TransformationError(
                f"{self.name} failed to retrieve PSyIR for {kern_or_call} "
                f"'{kname}' due to: {error}"
            ) from error

        # TODO ARPDBG - need to examine every kernel implementation, not just
        # the first one.
        kernel_schedule = kernels[0]
        # We do not support kernels that use symbols representing data
        # declared in their own parent module (we would need to new imports
        # from this module for those, and we don't do this yet).
        # These can only be found in References and CodeBlocks.
        for var in kernel_schedule.walk(Reference):
            symbol = var.symbol
            if isinstance(symbol, IntrinsicSymbol):
                continue
            if not symbol.is_import:
                if not var.scope.symbol_table.lookup(
                        symbol.name, scope_limit=kernel_schedule,
                        otherwise=False):
                    raise TransformationError(
                        f"{kern_or_call} '{kname}' contains accesses to "
                        f"'{symbol.name}' which is declared in the same "
                        f"module scope. Cannot inline such a {kern_or_call}.")
        for block in kernel_schedule.walk(CodeBlock):
            for name in block.get_symbol_names():
                # Is this quantity declared within the kernel?
                sym = block.scope.symbol_table.lookup(
                    name, scope_limit=kernel_schedule, otherwise=None)
                if not sym:
                    # It isn't declared in the kernel.
                    # Can we find the corresponding symbol at all?
                    sym = block.scope.symbol_table.lookup(name, otherwise=None)
                    if not sym:
                        raise TransformationError(
                            f"{kern_or_call} '{kname}' contains accesses to "
                            f"'{name}' in a CodeBlock but the origin of this "
                            f"symbol is unknown.")
                    # We found it in an outer scope - is it from an import or a
                    # declaration?
                    if not sym.is_import:
                        raise TransformationError(
                            f"{kern_or_call} '{kname}' contains accesses to "
                            f"'{name}' in a CodeBlock that is declared in the "
                            f"same module scope. Cannot inline such a "
                            f"{kern_or_call}.")

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

        # If the symbol already exist at the call site it must be referring
        # to a Routine
        existing_symbol = node.scope.symbol_table.lookup(kernel_schedule.name,
                                                         otherwise=None)
        if existing_symbol and not isinstance(existing_symbol, RoutineSymbol):
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
            for scope in code_to_inline.walk(ScopingNode):
                for symbol in scope.symbol_table.symbols:
                    all_symbols.add(symbol)
            for reference in code_to_inline.walk(Reference):
                all_symbols.add(reference.symbol)
            for literal in code_to_inline.walk(Literal):
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
        Wrapper that gets the name and PSyIR of the routine or kernel
        corresponding to the call described by `node`.

        :param node: the Call or CodedKern to resolve.
        :type node: :py:class:`psyclone.psyir.nodes.Call` |
                    :py:class:`psyclone.psyGen.CodedKern`

        :returns: the name of the routine as seen by the caller and the
                  PSyIR of the routine implementation.
        :rtype: Tuple(str, list[:py:class:`psyclone.psyir.nodes.Routine`],
                      :py:class:`psyclone.psyir.symbols.Symbol`)

        :raises TransformationError: if we have a call to a language-level
            Routine that maps to an Interface block as this is not yet
            supported (TODO #924).
        '''
        # TODO #2054 - once CodedKern has been migrated so that it subclasses
        # Call then this if/else (and thus this whole routine) can be removed.
        if isinstance(node, CodedKern):
            # We have a call to a Kernel in a PSyKAl API.
            # Where mixed-precision kernels are supported (e.g. in LFRic) the
            # call to get_kernel_schedule() will return the one which has an
            # interface matching the arguments in the call.
            interface_sym, routines = node.get_kernel_schedule()
            caller_name = node.name.lower()
        else:
            # We have a generic routine call.
            routines = node.get_callees()
            caller_name = node.routine.name.lower()
            interface_sym = None
            if len(routines) > 1:
                interface_sym = routines[0].symbol_table.lookup(caller_name)

        return (caller_name, routines, interface_sym)

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
        # this Symbol.
        # pylint:disable-next=protected-access
        actual_table._symbols.pop(symbol.name)
        if remove_csym:
            actual_table.remove(csym)

    def apply(self, node, options=None):
        ''' Bring the kernel/subroutine into this Container.

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
        # TODO - get rid of 'caller name' return value from
        # _get_psyir_to_inline?
        _, codes_to_inline, interface_sym = (
            KernelModuleInlineTrans._get_psyir_to_inline(node))

        updated_routines = self._prepare_code_to_inline(codes_to_inline)
        # Update the Kernel to point to the updated PSyIR.
        if isinstance(node, CodedKern):
            # TODO - add setter for these properties to Kern?
            # pylint: disable=protected-access
            node._kern_schedule = updated_routines
            if interface_sym:
                node._interface_symbol = (
                    updated_routines[0].scope.symbol_table.lookup(
                        interface_sym.name))

        container = node.ancestor(Container)
        local_table = node.scope.symbol_table

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
                node.ancestor(Container).addchild(code_to_inline.detach())
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
                        if routine.name == existing_symbol.name:
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
