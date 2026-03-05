# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2026, Science and Technology Facilities Council
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
# Modified: A. B. G. Chalk, STFC Daresbury Lab

'''Module providing common functionality to transformation from a
PSyIR array-reduction intrinsic to PSyIR code.

'''
from abc import ABC, abstractmethod
import warnings

from psyclone.psyir.nodes import (
    Assignment, Reference, ArrayReference, IfBlock, IntrinsicCall, Node,
    UnaryOperation, BinaryOperation)
from psyclone.psyir.symbols import (
    ArrayType, DataSymbol, UnresolvedType, UnsupportedType)
from psyclone.psyGen import Transformation
from psyclone.psyir.transformations.reference2arrayrange_trans import \
    Reference2ArrayRangeTrans
from psyclone.psyir.transformations.transformation_error import \
    TransformationError
from psyclone.utils import transformation_documentation_wrapper
from psyclone.psyir.transformations import ArrayAssignment2LoopsTrans


@transformation_documentation_wrapper
class ArrayReductionBaseTrans(Transformation, ABC):
    '''An abstract parent class providing common functionality to
    array-reduction intrinsic transformations which translate the
    intrinsics into an equivalent loop structure.

    '''
    _INTRINSIC_NAME = None
    _INTRINSIC_TYPE = None

    def __str__(self):
        return (f"Convert the PSyIR {self._INTRINSIC_NAME} intrinsic "
                "to equivalent PSyIR code.")

    # pylint: disable=too-many-branches
    def validate(self, node, options=None, **kwargs):
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

        if not options:
            self.validate_options(**kwargs)

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

        try:
            node.compute_argument_names()
        except (ValueError, NotImplementedError) as err:
            raise TransformationError(
                f"Error in {self.name} transformation. Cannot "
                f"disambiguate the arguments names in '{node.debug_string()}'"
            ) from err

        array_ref = node.argument_by_name("array")
        dim_ref = node.argument_by_name("dim")

        if dim_ref:
            raise TransformationError(
                f"The dimension argument to {self._INTRINSIC_NAME} is not "
                f"yet supported.")

        if isinstance(node.datatype, (UnresolvedType, UnsupportedType)):
            raise TransformationError(
                f"Error in {self.name} transformation. Cannot create "
                f"a temporary variable for '{node.debug_string()}'")

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

        # FIXME: are they now?
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

    # pylint: disable=too-many-locals
    def apply(self, node, options=None, verbose: bool = False, **kwargs):
        '''Apply the array-reduction intrinsic conversion transformation to
        the specified node. This node must be one of these intrinsic
        operations which is converted to an equivalent loop structure.

        :param node: an array-reduction intrinsic.
        :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`
        :param options: options for the transformation.
        :type options: Optional[Dict[str, Any]]

        '''
        # TODO 2668: options are now deprecated:
        if options:
            warnings.warn(self._deprecation_warning, DeprecationWarning, 2)
            verbose = options.get("verbose", False)

        self.validate(node, options, **kwargs)

        # Get nodes of interest
        orig_assignment = node.ancestor(Assignment)
        array_arg = node.argument_by_name("array")
        mask_arg = node.argument_by_name("mask")

        # Create a temporary symbol to accomulate the reduction value
        tmp_symbol = node.scope.symbol_table.new_symbol(
                root_name="reduction_var", symbol_type=DataSymbol,
                datatype=node.datatype)
        tmp_ref = Reference(tmp_symbol)

        # Step 1: Replace all references to arrays within the intrinsic array
        # and mask arguments (if it exists) to array ranges.
        # For example, 'maxval(a+b, mask=mod(c,2.0)==1)'
        # becomes 'maxval(a(:,:)+b(:,:), mask=mod(c(:,:),2.0)==1)' if
        # 'a', 'b' and 'c' are 2 dimensional arrays.
        # We need to do this in a detached copy because Reference2ArrayRange
        # does not normally expand arguments of non-elemental functions.
        dummy = UnaryOperation.create(UnaryOperation.Operator.PLUS,
                                      array_arg.copy())
        for reference in dummy.walk(Reference):
            Reference2ArrayRangeTrans().apply(reference)
        new_array_expr = dummy.children[0]
        if mask_arg:
            dummy = UnaryOperation.create(UnaryOperation.Operator.PLUS,
                                          mask_arg.copy())
            for reference in dummy.walk(Reference):
                Reference2ArrayRangeTrans().apply(reference)
            new_mask_expr = dummy.children[0]

        # Step 2: Put the intrinsic's extracted expression on the rhs of an
        # assignment with one of the arrays within the expression being added
        # to the lhs of the assignment (because ArrayAssignment2Loops uses it
        # to obtain the loop bounds). For example if:
        # x = maxval(a(:,:)+b(:,:))
        # then
        # rhs = a(:,:)+b(:,:)
        # resulting in the following code being created:
        # a(:,:) = a(:,:)+b(:,:)
        array_refs = new_array_expr.walk(ArrayReference)
        assignment = Assignment.create(array_refs[0].copy(),
                                       new_array_expr.detach())

        # If there is a mask, also add its expression to the assignment
        if mask_arg:
            combined_expression = BinaryOperation.create(
                BinaryOperation.Operator.AND, assignment.rhs.detach(),
                new_mask_expr.detach())
            assignment.addchild(combined_expression)

        # Replace the existing expression so the new code gets replaces by
        # loops in-place and the new iteration symbols have access to the scope
        orig_assignment.replace_with(assignment)

        # Step 3: call ArrayAssignment2Loops to create loop and array indexing
        # to the array ranges created in step 2. For example:
        # a(:,:) = a(:,:)+b(:,:)
        # becomes
        # do idx2 = LBOUND(a,2), UBOUND(a,2)
        #   do idx = LBOUND(a,1), UBOUND(a,1)
        #     a(idx,idx2) = a(idx,idx2) + b(idx,idx2)
        #   enddo
        # enddo

        assignment_parent = assignment.parent
        assignment_position = assignment.position
        # Must be placed here to avoid circular imports
        # pylint: disable=import-outside-toplevel
        try:
            ArrayAssignment2LoopsTrans().apply(assignment)
        except TransformationError as err:
            # The ArrayAssignment2LoopsTrans could fail to convert the ranges,
            # unfortunately this can not be tested before modifications to the
            # tree (e.g. in the validate), so the best we can do is reverting
            # to the original statement (with maybe some leftover tmp variable)
            # and produce the error here.
            assignment.replace_with(orig_assignment)
            # pylint: disable=raise-missing-from
            raise TransformationError(
                f"ArrayAssignment2LoopsTrans could not convert the "
                f"expression:\n{assignment.debug_string()}\n into a loop "
                f"because:\n{err.value}")
        outer_loop = assignment_parent.children[assignment_position]
        if mask_arg:
            # remove mask from the rhs of the assignment
            indexed_mask_expr = combined_expression.children[1].detach()
            assignment.rhs.replace_with(
                combined_expression.children[0].detach()
            )

        # Step 4: convert the original assignment (now within a loop
        # and indices) to its intrinsic form by replacing the
        # assignment with tmp=INTRINSIC(orig_lhs,<expr>). Also
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
            tmp_ref.copy(), self._loop_body(
                tmp_ref.copy(), assignment.rhs.copy()))
        if mask_arg:
            # Place the indexed mask around the statement.
            new_assignment = IfBlock.create(
                indexed_mask_expr.copy(), [new_assignment])

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
        # Initialisation statement:
        lhs = tmp_ref.copy()
        rhs = self._init_var(lhs)
        assignment = Assignment.create(lhs, rhs)
        if verbose:
            assignment.preceding_comment = (
                f"{self.name} expansion of: {orig_assignment.debug_string()}"
            )
        outer_loop.parent.children.insert(outer_loop.position, assignment)
        # Update original assignment with the reduced value
        for child in orig_assignment.walk(Node):
            if child is node:
                child.replace_with(tmp_ref.copy())
                break
        outer_loop.parent.children.insert(outer_loop.position+1,
                                          orig_assignment)

    @abstractmethod
    def _loop_body(self, lhs, rhs):
        '''The intrinsic-specific content of the created loop body.'''

    @abstractmethod
    def _init_var(self, reference):
        '''The intrinsic-specific initial value for the temporary variable.

        :param reference: the reference used to store the final result.
        :type reference: :py:class:`psyclone.psyir.node.Reference`

        '''


# For AutoAPI auto-documentation generation.
__all__ = ["ArrayReductionBaseTrans"]
