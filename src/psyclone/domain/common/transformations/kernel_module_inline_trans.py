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
from psyclone.errors import InternalError
from psyclone.psyGen import Transformation, CodedKern
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.symbols import (
    ContainerSymbol, DataSymbol, DataTypeSymbol, DefaultModuleInterface,
    RoutineSymbol, Symbol)
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
            _, kernel_schedule = (
                KernelModuleInlineTrans._get_psyir_to_inline(node))
        except Exception as error:
            raise TransformationError(
                f"{self.name} failed to retrieve PSyIR for {kern_or_call} "
                f"'{kname}' due to: {error}"
            ) from error

        # We do not support kernels that use symbols representing data
        # declared in their own parent module (we would need to add new imports
        # from this module at the call site, and we don't do this yet).
        # TODO #2424 - this suffers from the limitation that
        # VariablesAccessInfo does not work with nested scopes. (e.g. 2
        # different symbols with the same name but declared in different,
        # nested scopes will be assumed to be the same symbol).
        vai = VariablesAccessInfo(kernel_schedule)
        table = kernel_schedule.symbol_table
        for sig in vai.all_signatures:
            symbol = table.lookup(sig.var_name, otherwise=None)
            if not symbol:
                raise TransformationError(
                    f"{kern_or_call} '{kname}' contains accesses to "
                    f"'{sig.var_name}' but the origin of this signature is "
                    f"unknown.")
            if not symbol.is_import and symbol.name not in table:
                raise TransformationError(
                    f"{kern_or_call} '{kname}' contains accesses to "
                    f"'{symbol.name}' which is declared in the callee "
                    f"module scope. Cannot inline such a {kern_or_call}.")

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
    def _prepare_code_to_inline(code_to_inline):
        '''Prepare the PSyIR tree to inline by bringing in to the subroutine
        all referenced symbols so that the implementation is self contained.

        TODO #2271 will improve this method and could potentially
        avoid the need for debug_string() within get_kernel_schedule()
        in dynamo0p3.py. Sergi suggests that we may be missing the
        traversal of the declaration init expressions here and that
        might solve the problem. I'm not so sure and explain why in
        get_kernel_schedule() but still referencing this issue.

        :param code_to_inline: the subroutine to module-inline.
        :type code_to_inline: :py:class:`psyclone.psyir.node.Routine`

        '''
        # pylint: disable=too-many-branches
        source_container = code_to_inline.ancestor(Container)

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

        # Then decide which symbols need to be brought inside the subroutine
        symbols_to_bring_in = set()
        for symbol in all_symbols:
            if symbol.is_unresolved or symbol.is_import:
                # This symbol is already in the symbol table, but adding it
                # to the 'symbols_to_bring_in' will make the next step bring
                # into the subroutine all modules that it could come from.
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
                    # If it already exists, we know its a container (from the
                    # validation) so we just need to point to it
                    symbol.interface.container_symbol = \
                        code_to_inline.symbol_table.lookup(module_symbol.name)

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
        :rtype: Tuple(str, :py:class:`psyclone.psyir.nodes.Call`)

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
            routines = [node.get_kernel_schedule()]
            caller_name = node.name.lower()
        else:
            # We have a generic routine call.
            routines = node.get_callees()
            caller_name = node.routine.name.lower()
            # TODO #924 - at this point we may have found (an interface to)
            # multiple implementations. We can try to work out which one this
            # call will map to. Failing that, we'll have to insert all of them
            # plus the interface definition.
            if len(routines) > 1:
                raise TransformationError(
                    f"The target of the call to '{caller_name}' cannot be "
                    f"inserted because multiple implementations were found: "
                    f"{[rout.name for rout in routines]}. TODO #924")
        return (caller_name, routines[0])

    def apply(self, node, options=None):
        ''' Bring the kernel subroutine into this Container.

        :param node: the kernel to module-inline.
        :type node: :py:class:`psyclone.psyGen.CodedKern`
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
        caller_name, code_to_inline = (
            KernelModuleInlineTrans._get_psyir_to_inline(node))
        callee_name = code_to_inline.name

        try:
            existing_symbol = node.scope.symbol_table.lookup(callee_name)
        except KeyError:
            existing_symbol = None

        self._prepare_code_to_inline(code_to_inline)

        container = node.ancestor(Container)
        if not existing_symbol:
            # If it doesn't exist already, module-inline the subroutine by
            # inserting the relevant code into the tree.
            # We need to set the visibility of the routine's symbol to
            # be private.
            code_to_inline.symbol.visibility = Symbol.Visibility.PRIVATE
            node.ancestor(Container).addchild(code_to_inline.detach())
        else:
            if existing_symbol.is_import:
                # The RoutineSymbol is in the table but that is because it is
                # imported. We must therefore update its interface and
                # potentially remove the ContainerSymbol (from which it is
                # imported) altogether.
                csym = existing_symbol.interface.container_symbol
                # The import of the routine symbol may be in an outer scope.
                ctable = csym.find_symbol_table(node)
                remove_csym = (ctable.symbols_imported_from(csym) ==
                               [existing_symbol])
                existing_symbol.interface = DefaultModuleInterface()
                existing_symbol.visibility = Symbol.Visibility.PRIVATE
                if remove_csym:
                    ctable.remove(csym)
                code_to_inline = code_to_inline.detach()
                # Set the routine's symbol to the existing_symbol
                code_to_inline.symbol = existing_symbol
                container.addchild(code_to_inline)
            else:
                # The routine symbol already exists, and we know from the
                # validation that it's a Routine. Now check if they are
                # exactly the same.
                for routine in container.walk(Routine, stop_type=Routine):
                    if routine.name == caller_name:
                        # This TransformationError happens here and not in the
                        # validation because it needs the symbols_to_bring_in
                        # applied to effectively compare both versions.
                        # This will be fixed when module-inlining versioning is
                        # implemented.
                        # (It is OK to fail here because we have not yet made
                        # any modifications to the tree - code_to_inline is a
                        # detached copy.)
                        if routine != code_to_inline:
                            raise TransformationError(
                                f"Cannot inline subroutine '{caller_name}' "
                                f"because another, different, subroutine with "
                                f"the same name already exists and versioning "
                                f"of module-inlined subroutines is not "
                                f"implemented yet.")
            # Finally, ensure that the RoutineSymbol for the inlined routine is
            # in the correct symbol table.
            routine_symbol = existing_symbol
            table = routine_symbol.find_symbol_table(node)
            if table.node is not container:
                # Set the visibility of the symbol to always be private.
                sym = container.symbol_table.lookup(routine_symbol.name)
                sym.visibility = Symbol.Visibility.PRIVATE
                # Force removal of the routine_symbol if its also present in
                # the Routine's symbol table.
                table.lookup(routine_symbol.name)
                norm_name = table._normalize(routine_symbol.name)
                table._symbols.pop(norm_name)

        # We only modify the kernel call name after the equality check to
        # ensure the apply will succeed and we don't leave with an inconsistent
        # tree.
        if callee_name != caller_name:
            if isinstance(node, CodedKern):
                node.name = callee_name
            else:
                # TODO #924 - we can't currently resolve a subroutine if its
                # name doesn't match that in the caller (as will be the case
                # if it's being called via an Interface in Fortran). This
                # should have been picked-up in validate() so this is just a
                # safety check.
                raise InternalError(
                    f"Cannot module-inline call to '{caller_name}' because its"
                    f" name does not match that of the callee: "
                    f"'{callee_name}'. TODO #924.")

        # Set the module-inline flag to avoid generating the kernel imports
        # TODO #1823. If the kernel imports were generated at PSy-layer
        # creation time, we could just remove it here instead of setting a
        # flag.
        node.module_inline = True
