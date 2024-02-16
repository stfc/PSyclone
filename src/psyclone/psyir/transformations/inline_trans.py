# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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
# Authors: A. R. Porter, R. W. Ford, A. Chalk and S. Siso, STFC Daresbury Lab

'''
This module contains the InlineTrans transformation.

'''
from psyclone.errors import InternalError, LazyString
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import (
    ArrayReference, ArrayOfStructuresReference, BinaryOperation, Call,
    CodeBlock, Container, IntrinsicCall, Node, Range, Routine, Reference,
    Return, Literal, Assignment, StructureMember, StructureReference)
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.symbols import (
    ArgumentInterface, ArrayType, DataSymbol, UnresolvedType, INTEGER_TYPE,
    StaticInterface, Symbol, SymbolError, UnknownInterface, UnsupportedType)
from psyclone.psyir.transformations.reference2arrayrange_trans import (
    Reference2ArrayRangeTrans)
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)


_ONE = Literal("1", INTEGER_TYPE)


class InlineTrans(Transformation):
    '''
    This transformation takes a Call (which may have a return value)
    and replaces it with the body of the target routine. It is used as
    follows:

    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Call, Routine
    >>> from psyclone.psyir.transformations import InlineTrans
    >>> code = """
    ... module test_mod
    ... contains
    ...   subroutine run_it()
    ...     integer :: i
    ...     real :: a(10)
    ...     do i=1,10
    ...       a(i) = 1.0
    ...       call sub(a(i))
    ...     end do
    ...   end subroutine run_it
    ...   subroutine sub(x)
    ...     real, intent(inout) :: x
    ...     x = 2.0*x
    ...   end subroutine sub
    ... end module test_mod"""
    >>> psyir = FortranReader().psyir_from_source(code)
    >>> call = psyir.walk(Call)[0]
    >>> inline_trans = InlineTrans()
    >>> inline_trans.apply(call)
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(psyir.walk(Routine)[0].view())
    >>> print(FortranWriter()(psyir.walk(Routine)[0]))
    subroutine run_it()
      integer :: i
      real, dimension(10) :: a
    <BLANKLINE>
      do i = 1, 10, 1
        a(i) = 1.0
        a(i) = 2.0 * a(i)
      enddo
    <BLANKLINE>
    end subroutine run_it
    <BLANKLINE>

    .. warning::
        Routines/calls with any of the following characteristics are not
        supported and will result in a TransformationError:

        * the routine is not in the same file as the call;
        * the routine contains an early Return statement;
        * the routine contains a variable with UnknownInterface;
        * the routine contains a variable with StaticInterface;
        * the routine contains an UnsupportedType variable with
          ArgumentInterface;
        * the routine has a named argument;
        * the shape of any array arguments as declared inside the routine does
          not match the shape of the arrays being passed as arguments;
        * the routine accesses an un-resolved symbol;
        * the routine accesses a symbol declared in the Container to which it
          belongs.

        Some of these restrictions will be lifted by #924.

    '''
    def apply(self, node, options=None):
        '''
        Takes the body of the routine that is the target of the supplied
        call and replaces the call with it.

        :param node: target PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Routine`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        '''
        self.validate(node, options)
        # The table associated with the scoping region holding the Call.
        table = node.scope.symbol_table
        # Find the routine to be inlined.
        orig_routine = self._find_routine(node)

        if not orig_routine.children or isinstance(orig_routine.children[0],
                                                   Return):
            # Called routine is empty so just remove the call.
            node.detach()
            return

        # Ensure we don't modify the original Routine by working with a
        # copy of it.
        routine = orig_routine.copy()
        routine_table = routine.symbol_table

        # Construct lists of the nodes that will be inserted and all of the
        # References that they contain.
        new_stmts = []
        refs = []
        for child in routine.children:
            new_stmts.append(child.copy())
            refs.extend(new_stmts[-1].walk(Reference))

        # Shallow copy the symbols from the routine into the table at the
        # call site.
        table.merge(routine_table, include_arguments=False)
        # When constructing new references to replace references to formal
        # args, we need to know whether any of the actual arguments are array
        # accesses. If they use 'array notation' (i.e. represent a whole array)
        # then they won't have index expressions and will have been captured
        # as a Reference.
        ref2arraytrans = Reference2ArrayRangeTrans()

        for child in node.children:
            try:
                # TODO #1858, this won't yet work for arrays inside structures.
                ref2arraytrans.apply(child)
            except (TransformationError, ValueError):
                pass

        # Replace any references to formal arguments with copies of the
        # actual arguments.
        formal_args = routine_table.argument_list
        for ref in refs[:]:
            self._replace_formal_arg(ref, node, formal_args)

        # Store the Routine level symbol table and node's current scope
        # so we can merge symbol tables later if required.
        ancestor_table = node.ancestor(Routine).scope.symbol_table
        scope = node.scope
        # Copy the nodes from the Routine into the call site.
        if isinstance(new_stmts[-1], Return):
            # If the final statement of the routine is a return then
            # remove it from the list.
            del new_stmts[-1]

        if routine.return_symbol:
            # This is a function
            assignment = node.ancestor(Assignment)
            parent = assignment.parent
            idx = assignment.position-1
            for child in new_stmts:
                idx += 1
                parent.addchild(child, idx)
            table = parent.scope.symbol_table
            # Avoid a potential name clash with the original function
            table.rename_symbol(
                routine.return_symbol, table.next_available_name(
                    f"inlined_{routine.return_symbol.name}"))
            node.replace_with(Reference(routine.return_symbol))
        else:
            # This is a call
            parent = node.parent
            idx = node.position
            node.replace_with(new_stmts[0])
            for child in new_stmts[1:]:
                idx += 1
                parent.addchild(child, idx)

        # If the scope we merged the inlined functions symbol table into
        # is not a Routine scope then we now merge that symbol table into
        # the ancestor Routine. This avoids issues like #2424 when
        # applying ParallelLoopTrans to loops containing inlined calls.
        if ancestor_table is not scope.symbol_table:
            ancestor_table.merge(scope.symbol_table)
            replacement = type(scope.symbol_table)()
            scope.symbol_table.detach()
            replacement.attach(scope)

    def _replace_formal_arg(self, ref, call_node, formal_args):
        '''
        Recursively combines any References to formal arguments in the supplied
        PSyIR expression with the corresponding Reference (actual argument)
        from the call site to make a new Reference for use in the inlined code.
        If the supplied node is not a Reference to a formal argument then it is
        just returned (after we have recursed to any children).

        :param ref: the expression to update.
        :type ref: :py:class:`psyclone.psyir.nodes.Node`
        :param call_node: the call site.
        :type call_node: :py:class:`psyclone.psyir.nodes.Call`
        :param formal_args: the formal arguments of the called routine.
        :type formal_args: List[:py:class:`psyclone.psyir.symbols.DataSymbol`]

        :returns: the replacement reference.
        :rtype: :py:class:`psyclone.psyir.nodes.Reference`

        '''
        if not isinstance(ref, Reference):
            # Recurse down in case this is e.g. an Operation or Range.
            for child in ref.children[:]:
                self._replace_formal_arg(child, call_node, formal_args)
            return ref

        if ref.symbol not in formal_args:
            # The supplied reference is not to a formal argument.
            return ref

        # Lookup the actual argument that corresponds to this formal argument.
        actual_arg = call_node.children[formal_args.index(ref.symbol)]

        # If the local reference is a simple Reference then we can just
        # replace it with a copy of the actual argument, e.g.
        #
        #   call my_sub(my_struc%data(i,j))
        #
        #   subroutine my_sub(var)
        #     ...
        #     var = 0.0
        #
        # pylint: disable=unidiomatic-typecheck
        if type(ref) is Reference:
            arg_copy = actual_arg.copy()
            # If the local reference we are replacing has a parent then we
            # must ensure the parent's child list is updated. (It may not
            # have a parent if we are in the process of constructing a brand
            # new reference.)
            if ref.parent:
                ref.replace_with(arg_copy)
            return arg_copy

        # Local reference is not simple but the actual argument is, e.g.:
        #
        #   call my_sub(my_struc)
        #
        #   subroutine my_sub(var)
        #     ...
        #     var%data(i,j) = 0.0
        #
        if type(actual_arg) is Reference:
            ref.symbol = actual_arg.symbol
            return ref

        # Neither the actual or local references are simple, i.e. they
        # include array accesses and/or structure accesses.
        new_ref = self._replace_formal_struc_arg(actual_arg, ref, call_node,
                                                 formal_args)
        # If the local reference we are replacing has a parent then we must
        # ensure the parent's child list is updated. (It may not have a parent
        # if we are in the process of constructing a brand new reference.)
        if ref.parent:
            ref.replace_with(new_ref)
        return new_ref

    def _create_inlined_idx(self, call_node, formal_args,
                            local_idx, decln_start, actual_start):
        '''
        Utility that creates the PSyIR for an inlined array-index access
        expression. This is not trivial since a formal argument may be
        declared with bounds that are shifted relative to those of an
        actual argument.

        If local_idx is the index of the access in the routine;
           local_decln_start is the starting index of the dimension as
                        declared in the routine;
           actual_start is the starting index of the slice at the callsite
                        (whether from the array declaration or a slice);

        then the index of the inlined access will be::

            inlined_idx = local_idx - local_decln_start + 1 + actual_start - 1
                        = local_idx - local_decln_start + actual_start

        :param call_node: the Call that we are inlining.
        :type call_node: :py:class:`psyclone.psyir.nodes.Call`
        :param formal_args: the formal arguments of the routine being called.
        :type formal_args: List[:py:class:`psyclone.psyir.symbols.DataSymbol`]
        :param local_idx: a local array-index expression (i.e. appearing \
            within the routine being inlined).
        :type local_idx: :py:class:`psyclone.psyir.nodes.Node`
        :param decln_start: the lower bound of the corresponding array \
            dimension, as declared inside the routine being inlined.
        :type decln_start: :py:class:`psyclone.psyir.nodes.Node`
        :param actual_start: the lower bound of the corresponding array \
            dimension, as defined at the call site.
        :type actual_start: :py:class:`psyclone.psyir.nodes.Node`

        :returns: PSyIR for the corresponding inlined array index.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        if isinstance(local_idx, Range):
            lower = self._create_inlined_idx(call_node, formal_args,
                                             local_idx.start, decln_start,
                                             actual_start)
            upper = self._create_inlined_idx(call_node, formal_args,
                                             local_idx.stop, decln_start,
                                             actual_start)
            step = self._replace_formal_arg(local_idx.step, call_node,
                                            formal_args)
            return Range.create(lower.copy(), upper.copy(), step.copy())

        uidx = self._replace_formal_arg(local_idx, call_node, formal_args)
        if decln_start == actual_start:
            # If the starting indices in the actual and formal arguments are
            # the same then we don't need to shift the index.
            return uidx

        ustart = self._replace_formal_arg(decln_start, call_node, formal_args)
        start_sub = BinaryOperation.create(BinaryOperation.Operator.SUB,
                                           uidx.copy(), ustart.copy())
        return BinaryOperation.create(BinaryOperation.Operator.ADD,
                                      start_sub, actual_start.copy())

    def _update_actual_indices(self, actual_arg, local_ref,
                               call_node, formal_args):
        '''
        Create a new list of indices for the supplied actual argument
        (ArrayMixin) by replacing any Ranges with the appropriate expressions
        from the local access in the called routine. If there are no Ranges
        then the returned list of indices just contains copies of the inputs.

        :param actual_arg: (part of) the actual argument to the routine.
        :type actual_arg: :py:class:`psyclone.psyir.nodes.ArrayMixin`
        :param local_ref: the corresponding Reference in the called routine.
        :param call_node: the call site.
        :type call_node: :py:class:`psyclone.psyir.nodes.Call`
        :param formal_args: the formal arguments of the called routine.
        :type formal_args: List[:py:class:`psyclone.psyir.symbols.DataSymbol`]

        :returns: new indices for the actual argument.
        :rtype: List[:py:class:`psyclone.psyir.nodes.Node`]

        '''
        if isinstance(local_ref, ArrayMixin):
            local_indices = [idx.copy() for idx in local_ref.indices]
        # Get the locally-declared shape of the formal argument in case its
        # bounds are shifted relative to the caller.
        if isinstance(local_ref.symbol.datatype, ArrayType):
            local_decln_shape = local_ref.symbol.datatype.shape
        else:
            local_decln_shape = []

        new_indices = [idx.copy() for idx in actual_arg.indices]
        local_idx_posn = 0
        for pos, idx in enumerate(new_indices[:]):

            if not isinstance(idx, Range):
                continue

            # Starting index of slice of actual argument.
            if actual_arg.is_lower_bound(pos):
                # Range starts at lower bound of argument so that's what
                # we store.
                actual_start = actual_arg.get_lbound_expression(pos)
            else:
                actual_start = idx.start

            local_decln_start = None
            if local_decln_shape:
                if isinstance(local_decln_shape[local_idx_posn],
                              ArrayType.ArrayBounds):
                    # The formal argument declaration has a shape.
                    local_shape = local_decln_shape[local_idx_posn]
                    local_decln_start = local_shape.lower
                elif (local_decln_shape[local_idx_posn] ==
                      ArrayType.Extent.DEFERRED):
                    # The formal argument is declared to be allocatable and
                    # therefore has the same bounds as the actual argument.
                    local_shape = None
                    local_decln_start = actual_start
            if not local_decln_start:
                local_shape = None
                local_decln_start = _ONE

            if local_ref.is_full_range(local_idx_posn):
                # If the local Range is for the full extent of the formal
                # argument then the actual Range is defined by that of the
                # actual argument and no change is required unless the formal
                # argument is declared as having a Range with an extent that is
                # less than that supplied. In general we're not going to know
                # that so we have to be conservative.
                if local_shape:
                    new = Range.create(local_shape.lower.copy(),
                                       local_shape.upper.copy())
                    new_indices[pos] = self._create_inlined_idx(
                        call_node, formal_args,
                        new, local_decln_start, actual_start)
            else:
                # Otherwise, the local index expression replaces the Range.
                new_indices[pos] = self._create_inlined_idx(
                    call_node, formal_args,
                    local_indices[local_idx_posn],
                    local_decln_start, actual_start)
            # Each Range corresponds to one dimension of the formal argument.
            local_idx_posn += 1
        return new_indices

    def _replace_formal_struc_arg(self, actual_arg, ref, call_node,
                                  formal_args):
        '''
        Called by _replace_formal_arg() whenever a formal or actual argument
        involves an array or structure access that can't be handled with a
        simple substitution, e.g.

        .. code-block:: fortran

            call my_sub(my_struc%grid(:,2,:), 10)

            subroutine my_sub(grid, ngrids)
              ...
              do igrid = 1, ngrids
                do jgrid = ...
                  do i = 1, 10
                    do j = 1, 10
                      grid(igrid, jgrid)%data(i,j) = 0.0

        The assignment in the inlined code should become

        .. code-block:: fortran

            my_struc%grid(igrid,2,jgrid)%data(i,j) = 0.0

        This routine therefore recursively combines any References to formal
        arguments in the supplied Reference (including any array-index
        expressions) with the corresponding Reference
        from the call site to make a new Reference for use in the inlined code.

        :param actual_arg: an actual argument to the routine being inlined.
        :type actual_arg: :py:class:`psyclone.psyir.nodes.Reference`
        :param ref: the corresponding reference to a formal argument.
        :type ref: :py:class:`psyclone.psyir.nodes.Reference`
        :param call_node: the call site.
        :type call_node: :py:class:`psyclone.psyir.nodes.Call`
        :param formal_args: the formal arguments of the called routine.
        :type formal_args: List[:py:class:`psyclone.psyir.symbols.DataSymbol`]

        :returns: the replacement reference.
        :rtype: :py:class:`psyclone.psyir.nodes.Reference`

        '''
        # The final stage of this method creates a brand new
        # [ArrayOf]Structure[s]Reference so we have to collect the indices and
        # members as we walk down both the actual and local references.
        local_indices = None
        members = []

        # Actual arg could be var, var(:)%index, var(i,j)%grid(:) or
        # var(j)%data(i) etc. Any Ranges must correspond to dimensions of the
        # formal argument. The validate() method has already ensured that we
        # do not have any indirect accesses or non-unit strides.

        if isinstance(ref, ArrayMixin):
            local_indices = [idx.copy() for idx in ref.indices]

        # Since a Range can occur at any level of a Structure access in the
        # actual argument, we walk down it and check each Member. Any Ranges
        # are updated according to how that dimension is accessed by the
        # reference inside the routine.
        cursor = actual_arg
        while True:
            if isinstance(cursor, ArrayMixin):
                new_indices = self._update_actual_indices(
                    cursor, ref, call_node, formal_args)
                members.append((cursor.name, new_indices))
            else:
                members.append(cursor.name)

            if not isinstance(cursor, (StructureMember, StructureReference)):
                break
            cursor = cursor.member

        if not actual_arg.walk(Range) and local_indices:
            # There are no Ranges in the actual argument but the local
            # reference is an array access.
            # Create updated index expressions for that access.
            new_indices = []
            for idx in local_indices:
                new_indices.append(
                    self._replace_formal_arg(
                        idx.copy(), call_node, formal_args))
            # Replace the last entry in the `members` list with a new array
            # access.
            members[-1] = (cursor.name, new_indices)

        # We now walk down the *local* access, skipping its head (as that is
        # replaced by the actual arg). We don't need to worry about updating
        # index expressions in the actual argument as they are independent of
        # any array accesses within a structure passed as a formal argument.
        cursor = ref
        while isinstance(cursor, (StructureReference, StructureMember)):
            cursor = cursor.member
            if isinstance(cursor, ArrayMixin):
                new_indices = []
                for idx in cursor.indices:
                    # Update each index expression in case it refers to
                    # formal arguments.
                    new_indices.append(
                        self._replace_formal_arg(
                            idx.copy(), call_node, formal_args))
                members.append((cursor.name, new_indices))
            else:
                members.append(cursor.name)

        # Finally, construct the new Reference using the information we've
        # collected from both the actual argument and local access.
        if len(members) > 1:
            # We have some form of Structure reference.
            if isinstance(members[0], tuple):
                # Root of access is an array access.
                return ArrayOfStructuresReference.create(actual_arg.symbol,
                                                         members[0][1],
                                                         members[1:])
            return StructureReference.create(actual_arg.symbol, members[1:])

        # Just an array reference.
        return ArrayReference.create(actual_arg.symbol, members[0][1])

    def validate(self, node, options=None):
        '''
        Checks that the supplied node is a valid target for inlining.

        :param node: target PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyir.nodes.Routine`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the supplied node is not a Call or is \
            an IntrinsicCall.
        :raises TransformationError: if the routine has a return value.
        :raises TransformationError: if the routine body contains a Return \
            that is not the first or last statement.
        :raises TransformationError: if the routine body contains a CodeBlock.
        :raises TransformationError: if the called routine has a named \
            argument.
        :raises TransformationError: if any of the variables declared within \
            the called routine are of UnknownInterface.
        :raises TransformationError: if any of the variables declared within \
            the called routine have a StaticInterface.
        :raises TransformationError: if any of the subroutine arguments is of \
            UnsupportedType.
        :raises TransformationError: if a symbol of a given name is imported \
            from different containers at the call site and within the routine.
        :raises TransformationError: if the routine accesses an un-resolved \
            symbol.
        :raises TransformationError: if the number of arguments in the call \
            does not match the number of formal arguments of the routine.
        :raises TransformationError: if a symbol declared in the parent \
            container is accessed in the target routine.
        :raises TransformationError: if the shape of an array formal argument \
            does not match that of the corresponding actual argument.

        '''
        super().validate(node, options=options)

        # The node should be a Call.
        if not isinstance(node, Call):
            raise TransformationError(
                f"The target of the InlineTrans transformation "
                f"should be a Call but found '{type(node).__name__}'.")

        if isinstance(node, IntrinsicCall):
            raise TransformationError(
                f"Cannot inline an IntrinsicCall ('{node.routine.name}')")
        name = node.routine.name
        # Check that we can find the source of the routine being inlined.
        routine = self._find_routine(node)

        if not routine.children or isinstance(routine.children[0], Return):
            # An empty routine is fine.
            return

        return_stmts = routine.walk(Return)
        if return_stmts:
            if len(return_stmts) > 1 or not isinstance(routine.children[-1],
                                                       Return):
                # Either there is more than one Return statement or there is
                # just one but it isn't the last statement of the Routine.
                raise TransformationError(
                    f"Routine '{name}' contains one or more "
                    f"Return statements and therefore cannot be inlined.")

        if routine.walk(CodeBlock):
            raise TransformationError(
                f"Routine '{name}' contains one or more "
                f"CodeBlocks and therefore cannot be inlined.")

        # Support for routines with named arguments is not yet implemented.
        # TODO #924.
        for arg in node.argument_names:
            if arg:
                raise TransformationError(
                    f"Routine '{routine.name}' cannot be inlined because it "
                    f"has a named argument '{arg}' (TODO #924).")

        table = node.scope.symbol_table
        routine_table = routine.symbol_table

        for sym in routine_table.datasymbols:
            # We don't inline symbols that have an UnsupportedType and are
            # arguments since we don't know if a simple assingment if
            # enough (e.g. pointers)
            if isinstance(sym.interface, ArgumentInterface):
                if isinstance(sym.datatype, UnsupportedType):
                    raise TransformationError(
                        f"Routine '{routine.name}' cannot be inlined because "
                        f"it contains a Symbol '{sym.name}' which is an "
                        f"Argument of UnsupportedType: "
                        f"'{sym.datatype.declaration}'")
            # We don't inline symbols that have an UnknownInterface, as we
            # don't know how they are brought into this scope.
            if isinstance(sym.interface, UnknownInterface):
                raise TransformationError(
                    f"Routine '{routine.name}' cannot be inlined because it "
                    f"contains a Symbol '{sym.name}' with an UnknownInterface:"
                    f" '{sym.datatype.declaration}'")
            # Check that there are no static variables in the routine (because
            # we don't know whether the routine is called from other places).
            if (isinstance(sym.interface, StaticInterface) and
                    not sym.is_constant):
                raise TransformationError(
                    f"Routine '{routine.name}' cannot be inlined because it "
                    f"has a static (Fortran SAVE) interface for Symbol "
                    f"'{sym.name}'.")

        # We can't handle a clash between (apparently) different symbols that
        # share a name but are imported from different containers.
        try:
            table.check_for_clashes(routine_table)
        except SymbolError as err:
            raise TransformationError(
                f"One or more symbols from routine '{routine.name}' cannot be "
                f"added to the table at the call site.") from err

        # Check for unresolved symbols or for any accessed from the Container
        # containing the target routine.
        # TODO #1792 - kind parameters will not be found by simply doing
        # `walk(Reference)`. Although SymbolTable has the
        # `precision_datasymbols` property, this only returns those Symbols
        # that are used to define the precision of other Symbols in the same
        # table. If a precision symbol is only used within Statements then we
        # don't currently capture the fact that it is a precision symbol.
        ref_or_lits = routine.walk((Reference, Literal))
        # Check for symbols in any initial-value expressions
        # (including Fortran parameters) or array dimensions.
        for sym in routine_table.datasymbols:
            if sym.initial_value:
                ref_or_lits.extend(
                    sym.initial_value.walk((Reference, Literal)))
            if isinstance(sym.datatype, ArrayType):
                for dim in sym.shape:
                    if isinstance(dim, ArrayType.ArrayBounds):
                        if isinstance(dim.lower, Node):
                            ref_or_lits.extend(dim.lower.walk(Reference,
                                                              Literal))
                        if isinstance(dim.upper, Node):
                            ref_or_lits.extend(dim.upper.walk(Reference,
                                                              Literal))
        # Keep a reference to each Symbol that we check so that we can avoid
        # repeatedly checking the same Symbol.
        _symbol_cache = set()
        for lnode in ref_or_lits:
            if isinstance(lnode, Literal):
                if not isinstance(lnode.datatype.precision, DataSymbol):
                    continue
                sym = lnode.datatype.precision
            else:
                sym = lnode.symbol
            # If we've already seen this Symbol then we can skip it.
            if sym in _symbol_cache:
                continue
            _symbol_cache.add(sym)
            # We haven't seen this Symbol before.
            if sym.is_unresolved:
                try:
                    routine_table.resolve_imports(symbol_target=sym)
                except KeyError:
                    # The symbol is not (directly) imported into the symbol
                    # table local to the routine.
                    # pylint: disable=raise-missing-from
                    raise TransformationError(
                        f"Routine '{routine.name}' cannot be inlined "
                        f"because it accesses variable '{sym.name}' and this "
                        f"cannot be found in any of the containers directly "
                        f"imported into its symbol table.")
            else:
                if sym.name not in routine_table:
                    raise TransformationError(
                        f"Routine '{routine.name}' cannot be inlined because "
                        f"it accesses variable '{sym.name}' from its "
                        f"parent container.")

        # Check that the shapes of any formal array arguments are the same as
        # those at the call site.
        if len(routine_table.argument_list) != len(node.children):
            raise TransformationError(LazyString(
                lambda: f"Cannot inline '{node.debug_string().strip()}' "
                f"because the number of arguments supplied to the call "
                f"({len(node.children)}) does not match the number of "
                f"arguments the routine is declared to have "
                f"({len(routine_table.argument_list)})."))

        for formal_arg, actual_arg in zip(routine_table.argument_list,
                                          node.children):
            # If the formal argument is an array with non-default bounds then
            # we also need to know the bounds of that array at the call site.
            if not isinstance(formal_arg.datatype, ArrayType):
                # Formal argument is not an array so we don't need to do any
                # further checks.
                continue

            if not isinstance(actual_arg, (Reference, Literal)):
                # TODO #1799 this really needs the `datatype` method to be
                # extended to support all nodes. For now we have to abort
                # if we encounter an argument that is not a scalar (according
                # to the corresponding formal argument) but is not a
                # Reference or a Literal as we don't know whether the result
                # of any general expression is or is not an array.
                # pylint: disable=cell-var-from-loop
                raise TransformationError(LazyString(
                    lambda: f"The call '{node.debug_string()}' cannot be "
                            f"inlined because actual argument "
                            f"'{actual_arg.debug_string()}' corresponds to a "
                            f"formal argument with array type but is not a "
                            f"Reference or a Literal."))

            # We have an array argument. We are only able to check that the
            # argument is not re-shaped in the called routine if we have full
            # type information on the actual argument.
            # TODO #924. It would be useful if the `datatype` property was
            # a method that took an optional 'resolve' argument to indicate
            # that it should attempt to resolve any UnresolvedTypes.
            if (isinstance(actual_arg.datatype,
                           (UnresolvedType, UnsupportedType)) or
                (isinstance(actual_arg.datatype, ArrayType) and
                 isinstance(actual_arg.datatype.intrinsic,
                            (UnresolvedType, UnsupportedType)))):
                raise TransformationError(
                    f"Routine '{routine.name}' cannot be inlined because "
                    f"the type of the actual argument "
                    f"'{actual_arg.symbol.name}' corresponding to an array"
                    f" formal argument ('{formal_arg.name}') is unknown.")

            formal_rank = 0
            actual_rank = 0
            if isinstance(formal_arg.datatype, ArrayType):
                formal_rank = len(formal_arg.datatype.shape)
            if isinstance(actual_arg.datatype, ArrayType):
                actual_rank = len(actual_arg.datatype.shape)
            if formal_rank != actual_rank:
                # It's OK to use the loop variable in the lambda definition
                # because if we get to this point then we're going to quit
                # the loop.
                # pylint: disable=cell-var-from-loop
                raise TransformationError(LazyString(
                        lambda: f"Cannot inline routine '{routine.name}' "
                        f"because it reshapes an argument: actual argument "
                        f"'{actual_arg.debug_string()}' has rank {actual_rank}"
                        f" but the corresponding formal argument, "
                        f"'{formal_arg.name}', has rank {formal_rank}"))
            if actual_rank:
                ranges = actual_arg.walk(Range)
                for rge in ranges:
                    ancestor_ref = rge.ancestor(Reference)
                    if ancestor_ref is not actual_arg:
                        # Have a range in an indirect access.
                        # pylint: disable=cell-var-from-loop
                        raise TransformationError(LazyString(
                            lambda: f"Cannot inline routine '{routine.name}' "
                            f"because argument '{actual_arg.debug_string()}' "
                            f"has an array range in an indirect access (TODO "
                            f"#924)."))
                    if rge.step != _ONE:
                        # TODO #1646. We could resolve this problem by making
                        # a new array and copying the necessary values into it.
                        # pylint: disable=cell-var-from-loop
                        raise TransformationError(LazyString(
                            lambda: f"Cannot inline routine '{routine.name}' "
                            f"because one of its arguments is an array slice "
                            f"with a non-unit stride: "
                            f"'{actual_arg.debug_string()}' (TODO #1646)"))

    @staticmethod
    def _find_routine(call_node):
        '''Searches for the definition of the routine that is being called by
        the supplied Call.

        :param call_node: the Call that is to be inlined.
        :type call_node: :py:class:`psyclone.psyir.nodes.Call`

        :returns: the PSyIR for the target routine.
        :rtype: :py:class:`psyclone.psyir.nodes.Routine`

        :raises InternalError: if the routine symbol is local but the \
            routine definition is not found.
        :raises TransformationError: if the routine definition cannot be found.

        '''
        name = call_node.routine.name
        routine_sym = call_node.routine

        if routine_sym.is_modulevar:
            table = routine_sym.find_symbol_table(call_node)
            for routine in table.node.walk(Routine):
                if routine.name.lower() == name.lower():
                    return routine
            raise InternalError(
                f"Failed to find the source code of the local routine "
                f"'{routine_sym.name}'.")

        if routine_sym.is_unresolved:

            # First check for any wildcard imports and see if they can
            # be used to resolve the symbol.
            wildcard_names = []
            current_table = call_node.scope.symbol_table
            while current_table:
                for container_symbol in current_table.containersymbols:
                    if container_symbol.wildcard_import:
                        wildcard_names.append(container_symbol.name)
                        routine = InlineTrans._find_routine_in_container(
                            call_node, container_symbol)
                        if routine:
                            return routine
                current_table = current_table.parent_symbol_table()

            # Next check for any "raw" Routines, i.e. ones that are not
            # in a Container.  Such Routines would exist in the PSyIR
            # as a child of a FileContainer (if the PSyIR contains a
            # FileContainer). Note, if the PSyIR does contain a
            # FileContainer, it will be the root node of the PSyIR.
            for routine in call_node.root.children:
                if (isinstance(routine, Routine) and
                        routine.name.lower() == name.lower()):
                    return routine
            raise TransformationError(
                f"Failed to find the source code of the unresolved "
                f"routine '{name}' after trying wildcard imports from "
                f"{wildcard_names} and all routines that are not in "
                f"containers.")

        if routine_sym.is_import:
            container_symbol = routine_sym.interface.container_symbol
            routine = InlineTrans._find_routine_in_container(
                call_node, container_symbol)
            if routine:
                return routine
            raise TransformationError(
                f"Failed to find the source for routine '{routine_sym.name}' "
                f"imported from '{container_symbol.name}' and therefore "
                f"cannot inline it.")

        raise InternalError(
            f"Routine Symbol '{routine_sym.name}' is not local, "
            f"unresolved or imported.")

    @staticmethod
    def _find_routine_in_container(call_node, container_symbol):
        '''Searches for the definition of a routine that is being called by
        the supplied Call. If present, this routine must exist within a
        container specified by the supplied container symbol.

        :param call_node: the Call that is to be inlined.
        :type call_node: :py:class:`psyclone.psyir.nodes.Call`

        :param container_symbol: the symbol of the container to search.
        :type container_symbol: \
            :py:class:`psyclone.psyir.symbols.ContainerSymbol`

        :returns: the PSyIR for the target routine, if found.
        :rtype: Optional[:py:class:`psyclone.psyir.nodes.Routine`]

        '''
        # The required Routine will exist within a Container and
        # that Container could exist in the PSyIR as a child of a
        # FileContainer (if the PSyIR contains a
        # FileContainer). If the PSyIR does contain a
        # FileContainer, it will be the root node of the PSyIR.
        call_routine_sym = call_node.routine
        for container in call_node.root.children:
            if (isinstance(container, Container) and
                    container.name.lower() == container_symbol.name.lower()):
                for routine in container.children:
                    if (isinstance(routine, Routine) and
                            routine.name.lower() ==
                            call_routine_sym.name.lower()):
                        # Check this routine is public
                        routine_sym = container.symbol_table.lookup(
                            routine.name)
                        if routine_sym.visibility == Symbol.Visibility.PUBLIC:
                            return routine
                # The Container has been found but it does not contain
                # the expected Routine or the Routine is not public.

                # Look in the import that names the routine if there is one.
                table = container.symbol_table
                try:
                    routine_sym = table.lookup(call_routine_sym.name)
                    if routine_sym.is_import:
                        child_container_symbol = \
                            routine_sym.interface.container_symbol
                        return (InlineTrans._find_routine_in_container(
                            call_node, child_container_symbol))
                except KeyError:
                    pass

                # Look in any wildcard imports.
                for child_container_symbol in table.containersymbols:
                    if child_container_symbol.wildcard_import:
                        result = InlineTrans._find_routine_in_container(
                            call_node, child_container_symbol)
                        if result:
                            return result
                # The required Symbol was not found in the Container.
                return None
        # The specified Container was not found in the PSyIR.
        return None


# For AutoAPI auto-documentation generation.
__all__ = ["InlineTrans"]
