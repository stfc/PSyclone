# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2022, Science and Technology Facilities Council.
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
# Author R. W. Ford and N. Nobre, STFC Daresbury Lab
# Modified by J. Henrichs, Bureau of Meteorology

'''Module providing a transformation from a PSyIR Array Range to a
PSyIR Loop. This could be useful for e.g. performance reasons, to
allow further transformations e.g. loop fusion or if the back-end does
not support array ranges.

'''

from __future__ import absolute_import

from psyclone.core import SymbolicMaths
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import Loop, Range, Reference, ArrayReference, \
    Assignment, Operation, BinaryOperation
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE
from psyclone.psyir.transformations.transformation_error \
    import TransformationError


class ArrayRange2LoopTrans(Transformation):
    '''Provides a transformation from a PSyIR Array Range to a PSyIR
    Loop. For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "nemo"
    >>> filename = "tra_adv_compute.F90"
    >>> ast, invoke_info = parse(filename, api=api)
    >>> psy = PSyFactory(api).create(invoke_info)
    >>> schedule = psy.invokes.invoke_list[0].schedule
    >>>
    >>> from psyclone.psyir.nodes import Assignment
    >>> from psyclone.psyir.transformations import ArrayRange2LoopTrans, \
    >>>     TransformationError
    >>>
    >>> print(schedule.view())
    >>> trans = ArrayRange2LoopTrans()
    >>> for assignment in schedule.walk(Assignment):
    >>>     while True:
    >>>         try:
    >>>             trans.apply(assignment)
    >>>         except TransformationError:
    >>>             break
    >>> print(schedule.view())

    '''

    @staticmethod
    def same_range(array1, idx1, array2, idx2):
        '''This method compares the range node at position 'idx1' in array
        access 'array1' with the range node at position 'idx2' in
        array access 'array2'.

        The natural place to test the equivalence of two ranges is in
        the Range class. However, the test required here is slightly
        different as it is valid to assume that two array slices are
        the same even if their actual sizes are not known (as the code
        would raise a runtime exception if this were not the case).

        :param array1: an array node containing a range node at index idx1.
        :type array1: py:class:`psyclone.psyir.node.ArrayReference`
        :param int idx1: an index indicating the location of a range \
            node in array1 (in its children list).
        :param array2: an array node containing a range node at index idx2.
        :type array2: py:class:`psyclone.psyir.node.ArrayReference`
        :param int idx2: an index indicating the location of a range \
            node in array2 (in its children list).

        :returns: True if the ranges are the same and False if they \
            are not the same, or if it is not possible to determine.
        :rtype: bool

        :raises: TypeError if one or more of the arguments are of the \
            wrong type.

        '''
        # pylint: disable=too-many-branches
        if not isinstance(array1, ArrayReference):
            raise TypeError(
                f"The first argument to the same_range() method should be an "
                f"ArrayReference but found '{type(array1).__name__}'.")
        if not isinstance(idx1, int):
            raise TypeError(
                f"The second argument to the same_range() method should be an "
                f"int but found '{type(idx1).__name__}'.")
        if not isinstance(array2, ArrayReference):
            raise TypeError(
                f"The third argument to the same_range() method should be an "
                f"ArrayReference but found '{type(array2).__name__}'.")
        if not isinstance(idx2, int):
            raise TypeError(
                f"The fourth argument to the same_range() method should be an "
                f"int but found '{type(idx2).__name__}'.")
        if not idx1 < len(array1.children):
            raise IndexError(
                f"The value of the second argument to the same_range() method "
                f"'{idx1}' should be less than the number of dimensions "
                f"'{len(array1.children)}' in the associated array 'array1'.")
        if not idx2 < len(array2.children):
            raise IndexError(
                f"The value of the fourth argument to the same_range() method "
                f"'{idx2}' should be less than the number of dimensions "
                f"'{len(array2.children)}' in the associated array 'array2'.")
        if not isinstance(array1.children[idx1], Range):
            raise TypeError(
                f"The child of the first array argument at the specified index"
                f" ({idx1}) should be a Range node, but found "
                f"'{type(array1.children[idx1]).__name__}'.")
        if not isinstance(array2.children[idx2], Range):
            raise TypeError(
                f"The child of the second array argument at the specified "
                f"index ({idx2}) should be a Range node, but found "
                f"'{type(array2.children[idx2]).__name__}'.")

        range1 = array1.children[idx1]
        range2 = array2.children[idx2]

        sym_maths = SymbolicMaths.get()
        # compare lower bounds
        if array1.is_lower_bound(idx1) and array2.is_lower_bound(idx2):
            # Both array1 and array2 use the lbound() intrinsic to
            # specify the lower bound of the array dimension. We may
            # not be able to determine what the lower bounds of these
            # arrays are statically but at runtime the code will fail
            # if the ranges do not match so we assume that the lower
            # bounds are consistent.
            pass
        elif array1.is_lower_bound(idx1) or array2.is_lower_bound(idx2):
            # One and only one of array1 and array2 use the lbound()
            # intrinsic to specify the lower bound of the array
            # dimension. In this case assume that the ranges are
            # different (although they could potentially be the same).
            return False
        elif not sym_maths.equal(range1.start, range2.start):
            # Neither array1 nor array2 use the lbound() intrinsic to
            # specify the lower bound of the array dimension. Try to
            # determine if they are the same by matching the
            # text. Use symbolic maths to do the comparison.
            return False

        # compare upper bounds
        if array1.is_upper_bound(idx1) and array2.is_upper_bound(idx2):
            # Both array1 and array2 use the ubound() intrinsic to
            # specify the upper bound of the array dimension. We may
            # not be able to determine what the upper bounds of these
            # arrays are statically but at runtime the code will fail
            # if the ranges do not match so we assume that the upper
            # bounds are consistent.
            pass
        elif array1.is_upper_bound(idx1) or array2.is_upper_bound(idx2):
            # One and only one of array1 and array2 use the ubound()
            # intrinsic to specify the upper bound of the array
            # dimension. In this case assume that the ranges are
            # different (although they could potentially be the same).
            return False
        elif not sym_maths.equal(range1.stop, range2.stop):
            # Neither array1 nor array2 use the ubound() intrinsic to
            # specify the upper bound of the array dimension. Use
            # symbolic maths to check if they are equal.
            return False

        # compare steps
        if not sym_maths.equal(range1.step, range2.step):
            return False

        # Everything matches.
        return True

    def apply(self, node, options=None):
        '''Apply the ArrayRange2Loop transformation to the specified node. The
        node must be an assignment. The rightmost range node in each array
        within the assignment is replaced with a loop index and the
        assignment is placed within a loop iterating over that
        index. The bounds of the loop are determined from the bounds
        of the array range on the left hand side of the assignment.

        :param node: an Assignment node.
        :type node: :py:class:`psyclone.psyir.nodes.Assignment`

        '''
        self.validate(node)

        parent = node.parent
        symbol_table = node.scope.symbol_table
        loop_variable = symbol_table.new_symbol("idx", symbol_type=DataSymbol,
                                                datatype=INTEGER_TYPE)

        # Replace the rightmost range found in all arrays with the
        # iterator and use the range from the LHS range for the loop
        # iteration space.
        for array in node.walk(ArrayReference):
            for idx, child in reversed(list(enumerate(array.children))):
                if isinstance(child, Range):
                    if array is node.lhs:
                        # Save this range to determine indexing
                        lhs_range = child
                    array.children[idx] = Reference(
                        loop_variable, parent=array)
                    break
        position = node.position
        # Issue #806: If Loop bounds were a Range we would just
        # need to provide the range node which would be simpler.
        start, stop, step = lhs_range.pop_all_children()
        loop = Loop.create(loop_variable, start, stop, step, [node.detach()])
        parent.children.insert(position, loop)

    def __str__(self):
        return ("Convert a PSyIR assignment to an array Range into a "
                "PSyIR Loop.")

    @property
    def name(self):
        '''
        :returns: the name of the transformation as a string.
        :rtype: str

        '''
        return type(self).__name__

    def validate(self, node, options=None):
        '''Perform various checks to ensure that it is valid to apply the
        ArrayRange2LoopTrans transformation to the supplied PSyIR Node.

        :param node: the node that is being checked.
        :type node: :py:class:`psyclone.psyir.nodes.Assignment`

        :raises TransformationError: if the node argument is not an \
            Assignment.
        :raises TransformationError: if the node argument is an \
            Assignment whose left hand side is not an ArrayReference.
        :raises TransformationError: if the node argument is an \
            Assignment whose left hand side is an ArrayReference that does \
            not have Range specifying the access to at least one of its \
            dimensions.
        :raises TransformationError: if two or more of the loop ranges \
            in the assignment are different or are not known to be the \
            same.

        '''
        if not isinstance(node, Assignment):
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied node "
                f"argument should be a PSyIR Assignment, but found "
                f"'{type(node).__name__}'.")

        if not isinstance(node.lhs, ArrayReference):
            raise TransformationError(
                f"Error in {self.name} transformation. The lhs of the "
                f"supplied Assignment node should be a PSyIR ArrayReference, "
                f"but found '{type(node.lhs).__name__}'.")

        if not [dim for dim in node.lhs.children if isinstance(dim, Range)]:
            raise TransformationError(
                f"Error in {self.name} transformation. The lhs of the supplied"
                f" Assignment node should be a PSyIR ArrayReference with at "
                f"least one of its dimensions being a Range, but found None "
                f"in '{node.lhs}'.")

        # If an operator on the rhs only returns an array then we are
        # not able to turn the assignment into an explicit loop. At
        # the moment we do not capture what an operator returns and
        # therefore can not check this. This will be addressed in
        # issue #685. For the moment we check with an explicit list
        # which happens to only contain a single operator (MATMUL)
        # because at this time all other operators in the PSyIR can be
        # performed elementwise.
        if [operation for operation in node.rhs.walk(Operation)
                if operation.operator in [BinaryOperation.Operator.MATMUL]]:
            raise TransformationError(
                f"Error in {self.name} transformation. The rhs of the "
                f"supplied Assignment node '{node.rhs}' contains the MATMUL "
                f"operator which can't be performed elementwise.")

        # Find the outermost range for the array on the lhs of the
        # assignment and save its index.
        for idx, child in reversed(list(enumerate(node.lhs.children))):
            if isinstance(child, Range):
                lhs_index = idx
                break

        # For each array on the rhs of the assignment find the
        # outermost range if there is one, then compare this range
        # with the one on the lhs.
        for array in node.walk(ArrayReference):
            for idx, child in reversed(list(enumerate(array.children))):
                if isinstance(child, Range):
                    # Issue #814 We should add support for adding
                    # loop variables where the ranges are
                    # different, or occur in different index
                    # locations.
                    if not ArrayRange2LoopTrans.same_range(
                            node.lhs, lhs_index, array, idx):
                        # Ranges are, or may be, different so we
                        # can't safely replace this range with a
                        # loop iterator.
                        raise TransformationError(
                            f"The ArrayRange2LoopTrans transformation only "
                            f"supports ranges that are known to be the "
                            f"same as each other but array access "
                            f"'{node.lhs.name}' dimension {lhs_index} and "
                            f"'{array.name}' dimension {idx} are either "
                            f"different or can't be determined in the "
                            f"assignment '{node}'.")
                    break


__all__ = [
    'ArrayRange2LoopTrans']
