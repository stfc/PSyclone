# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council
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
PSyIR SUM, MINVAL or MAXVAL intrinsic to PSyIR code.

'''
from abc import ABC, abstractmethod

from psyclone.psyir.nodes import (
    Assignment, Reference, Literal, Loop, ArrayReference, IfBlock, Range,
    IntrinsicCall)
from psyclone.psyir.symbols import (
    DataSymbol, INTEGER_TYPE, ScalarType, ArrayType)
from psyclone.psyGen import Transformation
from psyclone.psyir.transformations.transformation_error import \
    TransformationError


class MMSBaseTrans(Transformation, ABC):
    '''An abstract parent class providing common functionality to the
    sum2code_trans, minval2code_trans and maxval2_code trans
    transformations.

    '''
    _INTRINSIC_NAME = None
    _CHILD_INTRINSIC = None

    @staticmethod
    def _get_args(node):
        '''Utility method that returns the minval, maxval or sum arguments,
        (array reference, dimension and mask).

        :param node: a minval, maxval or sum intrinsic.
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

        :param node: a Sum, Minval or Maxval intrinsic.
        :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`
        :param options: options for the transformation.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the supplied node is not an
            intrinsic.
        :raises TransformationError: if the supplied node is not a sum,
            minval, or maxval intrinsic.
        :raises TransformationError: if a valid value for the
            dimension argument can't be determined.
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
        for reference in array_ref.walk(Reference):
            if (isinstance(reference, ArrayReference) or
                    type(reference) == Reference and
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
        from psyclone.psyir.nodes import Node
        for node in assignment.lhs.walk(Node):
            if node == array_ref:
                raise TransformationError(
                    "Error, intrinsics on the lhs of an assignment are not "
                    "currently supported.")

        if len(array_ref.children) == 0:
            if not array_ref.symbol.is_array:
                raise TransformationError(
                    f"Expected '{array_ref.name}' to be an array.")
            for shape in array_ref.symbol.shape:
                if not (shape in [
                        ArrayType.Extent.DEFERRED, ArrayType.Extent.ATTRIBUTE]
                        or isinstance(shape, ArrayType.ArrayBounds)):
                    raise TransformationError(
                        f"Unexpected shape for array. Expecting one of "
                        f"Deferred, Attribute or Bounds but found '{shape}'.")

    def apply(self, node, options=None):
        '''Apply the SUM, MINVAL or MAXVAL intrinsic conversion transformation
        to the specified node. This node must be one of these
        intrinsic operations which is converted to equivalent inline
        code.

        :param node: a Sum, Minval or Maxval intrinsic.
        :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`
        :param options: options for the transformation.
        :type options: Optional[Dict[str, Any]]

        '''
        self.validate(node)

        expr, dimension_ref, mask_ref = self._get_args(node)

        # Step 1 extract the expression within the intrinsic and put
        # it on the rhs of an argument with one of the arrays within
        # the expression being added to the lhs of the argument.
        rhs = expr.copy()
        if not rhs.parent:
            # If the the reference is the only thing on the rhs it
            # will not have a parent which is required by the
            # replace_with() method. Add a fake parent (+)
            from psyclone.psyir.nodes import UnaryOperation
            unary_op = UnaryOperation.create(
                UnaryOperation.Operator.PLUS, rhs)
        
        # Convert references to arrays to array ranges where appropriate
        from psyclone.psyir.transformations import Reference2ArrayRangeTrans
        reference2arrayrange = Reference2ArrayRangeTrans()
        rhs_parent = rhs.parent
        for reference in rhs.walk(Reference):
            try:
                reference_parent = reference.parent
                reference2arrayrange.apply(reference)
            except TransformationError:
                pass
        # We know there is only one child
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

        orig_lhs = node.ancestor(Assignment).lhs.copy()
        array_refs = rhs.walk(ArrayReference)
        lhs = array_refs[0].copy()
        assignment = Assignment.create(lhs, rhs.detach())
        # Replace existing code so the new code gets access to symbol
        # tables etc.
        orig_assignment = node.ancestor(Assignment)
        orig_assignment.replace_with(assignment)

        # Step 2 call nemoarrayrange2loop_trans to create loop bounds
        # and array indexing (keeping track of where the new loop nest
        # is created). Also deal with the mask if it exists.
        if mask_ref:
            # add mask to the rhs of the assignment
            from psyclone.psyir.nodes import BinaryOperation
            assignment_rhs = BinaryOperation.create(
                BinaryOperation.Operator.AND, assignment.rhs.copy(), mask_ref.copy())
            assignment.rhs.replace_with(assignment_rhs)

        assignment_parent = assignment.parent
        assignment_position = assignment.position
        # Must be placed here to avoid circular imports
        from psyclone.domain.nemo.transformations import NemoAllArrayRange2LoopTrans
        array_range = NemoAllArrayRange2LoopTrans()
        array_range.apply(assignment)        
        outer_loop = assignment_parent.children[assignment_position]
        if mask_ref:
            # remove mask from the rhs of the assignment
            orig_assignment = assignment_rhs.children[0].copy()
            indexed_mask_ref = assignment_rhs.children[1].copy()
            assignment_rhs.replace_with(orig_assignment)

        # Step 3 convert the original assignment (now within a loop
        # and indexed) to its intrinsic form by replacing the
        # assignment with orig_lhs=INTRINSIC(orig_lhs,<expr>)
        new_assignment = Assignment.create(
            orig_lhs.copy(), self._loop_body(orig_lhs.copy(), assignment.rhs.copy()))
        if mask_ref:
            # Place the indexed mask around the statement.
            new_assignment = IfBlock.create(indexed_mask_ref.copy(), [new_assignment])

        assignment.replace_with(new_assignment)
        
        # Step 4 initialise the variable and place it before the newly
        # created outer loop (in step 2).
        lhs = orig_lhs.copy()
        rhs = self._init_var(lhs.symbol)
        assignment = Assignment.create(lhs, rhs)
        outer_loop.parent.children.insert(outer_loop.position, assignment)

        return

    @abstractmethod
    def _loop_body(self, lhs, rhs):
        '''The intrinsic-specific content of the created loop body.'''

    @abstractmethod
    def _init_var(self, var_symbol):
        '''The intrinsic-specific initial value for the temporary variable.

        '''
