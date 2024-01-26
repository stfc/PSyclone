# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council
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
# Author: R. W. Ford, STFC Daresbury Lab
# Modified: S. Siso, STFC Daresbury Lab

'''Module providing common functionality to transformation from a
PSyIR array-reduction intrinsic to PSyIR code.

'''
from abc import ABC, abstractmethod

from psyclone.psyir.nodes import (
    Assignment, Reference, ArrayReference, IfBlock, Loop,
    IntrinsicCall, Node, UnaryOperation, BinaryOperation)
from psyclone.psyir.symbols import ArrayType, DataSymbol, ScalarType
from psyclone.psyGen import Transformation
from psyclone.psyir.transformations.reference2arrayrange_trans import \
    Reference2ArrayRangeTrans
from psyclone.psyir.transformations.transformation_error import \
    TransformationError


class ArrayReductionBaseTrans(Transformation, ABC):
    '''An abstract parent class providing common functionality to
    array-reduction intrinsic transformations which translate the
    intrinsics into an equivalent loop structure.

    '''
    _INTRINSIC_NAME = None
    _INTRINSIC_TYPE = None

    @staticmethod
    def _get_args(node):
        '''Utility method that returns the array-reduction intrinsic arguments
        (array reference, dimension and mask).

        :param node: an array-reduction intrinsic.
        :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

        returns: a tuple containing the 3 arguments.
        rtype: Tuple[py:class:`psyclone.psyir.nodes.reference.Reference`,
            py:class:`psyclone.psyir.nodes.Literal` |
            :py:class:`psyclone.psyir.nodes.Reference`,
            Optional[:py:class:`psyclone.psyir.nodes.Node`]]

        '''
        # Determine the arguments to the intrinsic
        args = [None, None, None]
        arg_names_map = {"array": 0, "dim": 1, "mask": 2}
        for idx, child in enumerate(node.children):
            if not node.argument_names[idx]:
                # positional arg
                args[idx] = child
            else:
                # named arg
                name = node.argument_names[idx].lower()
                args[arg_names_map[name]] = child
        return tuple(args)

    def __str__(self):
        return (f"Convert the PSyIR {self._INTRINSIC_NAME} intrinsic "
                "to equivalent PSyIR code.")

    # pylint: disable=too-many-branches
    def validate(self, node, options=None):
        '''Check that the input node is valid before applying the
        transformation.

        :param node: an array-reduction intrinsic.
        :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`
        :param options: options for the transformation.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the supplied node is not an
            intrinsic.
        :raises TransformationError: if the supplied node is not an
            array-reduction intrinsic.
        :raises TransformationError: if there is a dimension argument.
        :raises TransformationError: if the array argument is not an array.
        :raises TransformationError: if the shape of the array is not
            supported.
        :raises TransformationError: if the array datatype is not
            supported.
        :raises TransformationError: if the intrinsic is not part of
            an assignment.

        '''
        if not isinstance(node, IntrinsicCall):
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied node "
                f"argument is not an intrinsic, found "
                f"'{type(node).__name__}'.")

        if node.routine.name.upper() != self._INTRINSIC_NAME:
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied node "
                f"argument is not a {self._INTRINSIC_NAME.lower()} "
                f"intrinsic, found '{node.routine.name}'.")

        array_ref, dim_ref, _ = self._get_args(node)

        # dim_ref is not yet supported by this transformation.
        if dim_ref:
            raise TransformationError(
                f"The dimension argument to {self._INTRINSIC_NAME} is not "
                f"yet supported.")

        # There should be at least one arrayreference or reference to
        # an array in the expression
        # pylint: disable=unidiomatic-typecheck
        for reference in array_ref.walk(Reference):
            if (isinstance(reference, ArrayReference) or
                    type(reference) is Reference and
                    reference.symbol.is_array):
                break
        else:
            raise TransformationError(
                f"Error, no ArrayReference's found in the expression "
                f"'{array_ref.debug_string()}'.")

        if not node.ancestor(Assignment):
            raise TransformationError(
                f"{self.name} only works when the intrinsic is part "
                f"of an Assignment.")

        assignment = array_ref.ancestor(Assignment)
        for this_node in assignment.lhs.walk(Node):
            if this_node == array_ref:
                raise TransformationError(
                    "Error, intrinsics on the lhs of an assignment are not "
                    "currently supported.")

        if len(array_ref.children) == 0:
            for shape in array_ref.symbol.shape:
                if not (shape in [
                        ArrayType.Extent.DEFERRED, ArrayType.Extent.ATTRIBUTE]
                        or isinstance(shape, ArrayType.ArrayBounds)):
                    raise TransformationError(
                        f"Unexpected shape for array. Expecting one of "
                        f"Deferred, Attribute or Bounds but found '{shape}'.")

        # If the lhs symbol is used anywhere on the assignment rhs, we need
        # to create a temporary, and for this we need to resolve its datatype
        for rhs_reference in assignment.rhs.walk(Reference):
            if rhs_reference.symbol is assignment.lhs.symbol:
                if not (isinstance(assignment.lhs.symbol, DataSymbol) and
                        isinstance(assignment.lhs.datatype, ScalarType)):
                    line = assignment.debug_string().strip('\n')
                    raise TransformationError(
                        f"To loopify '{line}'"
                        f" we need a temporary variable, but the type of "
                        f"'{assignment.lhs.debug_string()}' can not be "
                        f"resolved or is unsupported.")

    # pylint: disable=too-many-locals
    def apply(self, node, options=None):
        '''Apply the array-reduction intrinsic conversion transformation to
        the specified node. This node must be one of these intrinsic
        operations which is converted to an equivalent loop structure.

        :param node: an array-reduction intrinsic.
        :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`
        :param options: options for the transformation.
        :type options: Optional[Dict[str, Any]]

        '''
        self.validate(node)

        orig_lhs = node.ancestor(Assignment).lhs.copy()
        orig_rhs = node.ancestor(Assignment).rhs.copy()

        # Determine whether the assignment is an increment (as we have
        # to use a temporary if so) e.g. x = x + MAXVAL(a) and store a
        # reference to the appropriate variable in new_lhs for future
        # use.
        lhs_symbol = orig_lhs.symbol
        increment = False
        for rhs_reference in orig_rhs.walk(Reference):
            if rhs_reference.symbol is lhs_symbol:
                increment = True
        if increment:
            new_lhs_symbol = node.scope.symbol_table.new_symbol(
                root_name="tmp_var", symbol_type=DataSymbol,
                datatype=orig_lhs.datatype)
            new_lhs = Reference(new_lhs_symbol)
        else:
            new_lhs = orig_lhs.copy()

        expr, _, mask_ref = self._get_args(node)

        # Step 1: replace all references to arrays within the
        # intrinsic expressions and mask argument (if it exists) to
        # array ranges. For example, 'maxval(a+b, mask=mod(c,2.0)==1)'
        # becomes 'maxval(a(:,:)+b(:,:), mask=mod(c(:,:),2.0)==1)' if
        # 'a', 'b' and 'c' are 2 dimensional arrays.
        rhs = expr.copy()
        _ = UnaryOperation.create(UnaryOperation.Operator.PLUS, rhs)
        reference2arrayrange = Reference2ArrayRangeTrans()
        # The reference to rhs becomes invalid in the following
        # transformation so we keep a copy of the parent here and
        # reset rhs to rhs_parent.children[0] after the
        # transformation.
        rhs_parent = rhs.parent
        for reference in rhs.walk(Reference):
            try:
                reference2arrayrange.apply(reference)
            except TransformationError:
                pass
        # Reset rhs from its parent as the previous transformation
        # makes the value of rhs become invalid. We know there is only
        # one child so can safely use children[0].
        rhs = rhs_parent.children[0]
        if mask_ref:
            mask_ref_parent = mask_ref.parent
            mask_ref_index = mask_ref.position
            for reference in mask_ref.walk(Reference):
                try:
                    reference2arrayrange.apply(reference)
                except TransformationError:
                    pass
            mask_ref = mask_ref_parent.children[mask_ref_index]

        # Step 2: Put the intrinsic's extracted expression (stored in
        # the 'rhs' variable) on the rhs of an argument with one of
        # the arrays within the expression being added to the lhs of
        # the argument. For example if:
        # x = maxval(a(:,:)+b(:,:))
        # then
        # rhs = a(:,:)+b(:,:)
        # resulting in the following code being created:
        # a(:,:) = a(:,:)+b(:,:)
        array_refs = rhs.walk(ArrayReference)
        # The lhs of the created expression needs to be an array
        # reference from the expression itself because the
        # ArrayRange2Loop transformation uses it to obtain the loop
        # bounds.
        lhs = array_refs[0].copy()

        assignment = Assignment.create(lhs, rhs.detach())
        # Replace existing code so the new code gets access to symbol
        # tables etc.
        orig_assignment = node.ancestor(Assignment)
        orig_assignment.replace_with(assignment)

        # Step 3 call nemoarrayrange2loop_trans to create loop bounds
        # and array indexing from the array ranges created in step 2
        # (keeping track of where the new loop nest is created). Also
        # extract the mask if it exists. For example:
        # a(:,:) = a(:,:)+b(:,:)
        # becomes
        # do idx2 = LBOUND(a,2), UBOUND(a,2)
        #   do idx = LBOUND(a,1), UBOUND(a,1)
        #     a(idx,idx2) = a(idx,idx2) + b(idx,idx2)
        #   enddo
        # enddo
        if mask_ref:
            # add mask to the rhs of the assignment
            assignment_rhs = BinaryOperation.create(
                BinaryOperation.Operator.AND, assignment.rhs.copy(),
                mask_ref.copy())
            assignment.rhs.replace_with(assignment_rhs)

        assignment_parent = assignment.parent
        assignment_position = assignment.position
        # Must be placed here to avoid circular imports
        # pylint: disable=import-outside-toplevel
        from psyclone.domain.nemo.transformations import \
            NemoAllArrayRange2LoopTrans
        array_range = NemoAllArrayRange2LoopTrans()
        array_range.apply(assignment)
        outer_loop = assignment_parent.children[assignment_position]
        if not isinstance(outer_loop, Loop):
            # The NemoAllArrayRange2LoopTrans could fail to convert the
            # ranges without raising a TransformationError, unfortunately
            # this can not be tested before previous modifications to the
            # tree (e.g. in the validate), so the best we can do is reverting
            # to the orginal statement (with maybe some leftover tmp variable)
            # and produce the error here.
            assignment.replace_with(orig_assignment)
            raise TransformationError(
                f"NemoAllArrayRange2LoopTrans could not convert the "
                f"expression '{assignment.debug_string()}' into a loop.")
        if mask_ref:
            # remove mask from the rhs of the assignment
            orig_assignment = assignment_rhs.children[0].copy()
            indexed_mask_ref = assignment_rhs.children[1].copy()
            assignment_rhs.replace_with(orig_assignment)

        # Step 4 convert the original assignment (now within a loop
        # and indexed) to its intrinsic form by replacing the
        # assignment with new_lhs=INTRINSIC(orig_lhs,<expr>). Also
        # add in the mask if one has been specified. For example:
        # do idx2 = LBOUND(a,2), UBOUND(a,2)
        #   do idx = LBOUND(a,1), UBOUND(a,1)
        #     a(idx,idx2) = a(idx,idx2) + b(idx,idx2)
        #   enddo
        # enddo
        # becomes
        # do idx2 = LBOUND(a,2), UBOUND(a,2)
        #   do idx = LBOUND(a,1), UBOUND(a,1)
        #     if (mod(c(idx,idx2),2.0)==1) then
        #       x = max(x, a(idx,idx2) + b(idx,idx2))
        #     end if
        #   enddo
        # enddo
        new_assignment = Assignment.create(
            new_lhs.copy(), self._loop_body(
                new_lhs.copy(), assignment.rhs.copy()))
        if mask_ref:
            # Place the indexed mask around the statement.
            new_assignment = IfBlock.create(
                indexed_mask_ref.copy(), [new_assignment])

        assignment.replace_with(new_assignment)

        # Step 5 initialise the variable and place it before the newly
        # created outer loop (in step 2) and deal with any additional
        # arguments on the rhs of the original expression. For
        # example, if the original code looks like the following:
        # x = value1 + maxval(a+b, mask=mod(c,2.0)==1) * value2
        # and the newly created loop looks like the following:
        # do idx2 = LBOUND(a,2), UBOUND(a,2)
        #   do idx = LBOUND(a,1), UBOUND(a,1)
        #     if (mod(c(idx,idx2),2.0)==1) then
        #       x = max(x, a(idx,idx2) + b(idx,idx2))
        #     end ifx
        #   enddo
        # enddo
        # then the result becomes:
        # x = tiny(x)
        # do idx2 = LBOUND(a,2), UBOUND(a,2)
        #   do idx = LBOUND(a,1), UBOUND(a,1)
        #     if (mod(c(idx,idx2),2.0)==1) then
        #       x = max(x, a(idx,idx2) + b(idx,idx2))
        #     end if
        #   enddo
        # enddo
        # x = value1 + x * value2
        lhs = new_lhs.copy()
        rhs = self._init_var(lhs)
        assignment = Assignment.create(lhs, rhs)
        outer_loop.parent.children.insert(outer_loop.position, assignment)
        if not (isinstance(orig_rhs, IntrinsicCall) and
                orig_rhs.intrinsic is self._INTRINSIC_TYPE):
            # The intrinsic call is not the only thing on the rhs of
            # the expression, so we need to deal with the additional
            # computation.
            rhs = orig_rhs.copy()
            for child in rhs.walk(IntrinsicCall):
                if child.intrinsic is self._INTRINSIC_TYPE:
                    child.replace_with(new_lhs.copy())
                    break
            assignment = Assignment.create(orig_lhs.copy(), rhs)
            outer_loop.parent.children.insert(
                outer_loop.position+1, assignment)

    @abstractmethod
    def _loop_body(self, lhs, rhs):
        '''The intrinsic-specific content of the created loop body.'''

    @abstractmethod
    def _init_var(self, reference):
        '''The intrinsic-specific initial value for the temporary variable.

        :param reference: the reference used to store the final result.
        :type reference: :py:class:`psyclone.psyir.node.Reference`

        '''
