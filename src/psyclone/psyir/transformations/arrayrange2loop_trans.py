# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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
# Author R. W. Ford, STFC Daresbury Lab

'''Module providing a transformation from a PSyIR Array Range to a
PSyIR Loop. This could be useful for e.g. performance reasons, to
allow further transformations e.g. loop fusion or if the back-end does
not support array ranges.

'''

from __future__ import absolute_import
from psyclone.psyGen import Kern, Transformation
from psyclone.psyir.transformations.transformation_error \
    import TransformationError
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE
from psyclone.psyir.nodes import Loop, Literal, Range, Reference, \
    BinaryOperation, Array, Assignment


class ArrayRange2LoopTrans(Transformation):
    '''Provides a transformation from a PSyIR Array Range to a PSyIR
    Loop.

    '''

    @staticmethod
    def same_range(array1, idx1, array2, idx2):
        '''This method compares the range node at position 'idx1' in array
        access 'array1' with the range node at position 'idx2' in
        array access 'array2'.

        The natural place to test the equivalence of two ranges is in
        the Range class. However, the test required here is slightly
        different as we can assume that two array slices are the same
        even if we don't know their actual sizes (as the code itself
        would raise an exception if this were not the case).

        :param array1: an array node containing a range node at index idx1.
        :type array1: py:class:`psyclone.psyir.node.Array`
        :param int idx1: an index indicating the location of a range \
            node in array1 (in its children list).
        :param array2: an array node containing a range node at index idx2.
        :type array2: py:class:`psyclone.psyir.node.Array`
        :param int idx2: an index indicating the location of a range \
            node in array2 (in its children list).

        :returns: True if the ranges are the same and False if they \
            are not the same, or if it is not possible to determine.
        :rtype: bool

        :raises: TransformationError if one or more of the arguments \
            are invalid.

        '''
        if not isinstance(array1, Range):
            raise TransformationError(
                "The first argument to the same_range() method should be a "
                "Range but found '{0}'.".format(type(array1).__name__)
        if not isinstance(idx1, int):
            raise TransformationError(
                "The second argument to the same_range() method should be an "
                "int but found '{0}'.".format(type(array2).__name__)
        if not isinstance(array2, Range): 
            raise TransformationError(
                "The third argument to the same_range() method should be a "
                "Range but found '{0}'.".format(type(array2).__name__)
        if not isinstance(idx2, int):
                "The fourth argument to the same_range() method should be an "
                "int but found '{0}'.".format(type(array2).__name__)
        if not isinstance(array1.children[idx1], Range):
            raise TransformationError(
                "The child of array1 at index idx1 is not a Range node.")
        if not isinstance(array2.children[idx2], Range):
            raise TransformationError("XXX")

        range1 = array1.children[idx1]
        range2 = array2.children[idx2]

        # lower bounds
        if (array1.is_lower_bound(idx1) != array2.is_lower_bound(idx2) and
            not range1.lb_same_as(range2)):
            return False
        # upper bounds
        if (array1.is_upper_bound(idx1) != array2.is_upper_bound(idx2) and
            not range1.ub_same_as(range2)):
            return False
        # steps
        if not range1.step_same_as(range2):
            return False
        return True
        
    def apply(self, node):
        ''' xxx '''
        self.validate(node)

        parent = node.parent
        symbol_table = node.scope.symbol_table
        loop_variable_name = symbol_table.new_symbol_name(root_name="idx")
        loop_variable_symbol = DataSymbol(loop_variable_name, INTEGER_TYPE)
        symbol_table.add(loop_variable_symbol)

        # Replace the first range found in all arrays with the
        # iterator and use the range from the LHS range for the loop
        # iteration space.
        for array in node.walk(Array):
            for idx, child in enumerate(array.children):
                if isinstance(child, Range):
                    if array is node.lhs:
                        # Save this range to determine indexing
                        lhs_range = child
                    array.children[idx] = Reference(loop_variable_symbol,
                                                        parent=array)
                    break
        # Issue #XXX: If Loop bounds were a Range we would just
        # need to provide the range node which would be simpler.
        loop = Loop.create(loop_variable_symbol, lhs_range.children[0],
                           lhs_range.children[1], lhs_range.children[2],
                           [node])
        parent.children[node.position] = loop
        loop.parent = parent

    def __str__(self):
        return "Convert a PSyIR Array Range to a PSyIR Loop."

    @property
    def name(self):
        '''
        :returns: the name of the transformation as a string.
        :rtype: str

        '''
        return type(self).__name__

    def validate(self, node):
        '''Perform various checks to ensure that it is valid to apply the
        ArrayRange2LoopTrans transformation to the supplied PSyIR Node.

        :param node: the node that is being checked.
        :type node: :py:class:`psyclone.psyir.nodes.Assignment`

        :raises TransformationError: if the node argument is not an \
            Assignment.
        :raises TransformationError: if the node argument is an \
            Assignment whose left had side is not an Array.
        :raises TransformationError: if the node argument is an \
            Assignment whose left hand side is an Array that does not \
            have Range specifying the access to at least one of its \
            dimensions.
        :raises TransformationError: if two or more of the loop ranges \
        in the assignment are different or are not known to be the \
        same.

        '''
        if not isinstance(node, Assignment):
            raise TransformationError(
                "Error in {0} transformation. The supplied node argument "
                "should be a PSyIR Assignment, but found '{1}'."
                "".format(self.name, type(node).__name__))

        if not isinstance(node.lhs, Array):
            raise TransformationError(
                "Error in {0} transformation. The lhs of the supplied "
                "Assignment node should be a PSyIR Array, but found '{1}'."
                "".format(self.name, type(node.lhs).__name__))

        if not [dim for dim in node.lhs.children if isinstance(dim, Range)]:
            raise TransformationError(
                "Error in {0} transformation. The lhs of the supplied "
                "Assignment node should be a PSyIR Array with at least one "
                "of its dimensions being a Range, but found None."
                "".format(self.name))

        for array in node.walk(Array):
            for idx, child in enumerate(array.children):
                if isinstance(child, Range):
                    if array is node.lhs:
                        # Save this range to allow comparison with
                        # other ranges.
                        lhs_index = idx
                        lhs_range = child
                    else:
                        # We could add support for adding loop
                        # variables where the ranges are different.
                        if not ArrayRange2LoopTrans.same_range(
                                node.lhs, lhs_index, child, idx):
                            # Ranges are, or may be, different so we
                            # can't safely replace this range with a
                            # loop iterator.
                            raise TransformationError(
                                "The ArrayRange2LoopTrans transformation only "
                                "supports ranges that are known to be the same "
                                "as each other but array access '{0}' "
                                "dimension {1} and '{2}' dimension {3} are "
                                "either different or can't be determined."
                                "".format(node.lhs.name, lhs_index, array.name,
                                          idx))
                    break
