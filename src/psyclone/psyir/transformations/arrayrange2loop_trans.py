# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
# Author R. W. Ford, N. Nobre and S. Siso, STFC Daresbury Lab
# Modified by J. Henrichs, Bureau of Meteorology

'''Module providing a transformation from a PSyIR Array Range to a
PSyIR Loop. This could be useful for e.g. performance reasons, to
allow further transformations e.g. loop fusion or if the back-end does
not support array ranges.

'''

from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import Loop, Range, Reference, ArrayReference, \
    Assignment, Call, IntrinsicCall
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

        # TODO #2004: Note that the NEMOArrayRange2Loop transforamtion has
        # a different implementation that accepts many more statemetns (e.g.
        # elemental function calls) but lacks in the use of symbolics. Both
        # implementation should be merged (as well as their tests) to one
        # combining the advantages of both.

        # Currently we don't accept calls (with the exception of L/UBOUND)
        for call in node.rhs.walk(Call):
            if isinstance(call, IntrinsicCall) and call.intrinsic in \
                (IntrinsicCall.Intrinsic.LBOUND,
                 IntrinsicCall.Intrinsic.UBOUND):
                continue
            raise TransformationError(
                f"Error in {self.name} transformation. The rhs of the supplied"
                f" Assignment contains a call '{call.debug_string()}'.")

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
                    if not node.lhs.same_range(lhs_index, array, idx):
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
