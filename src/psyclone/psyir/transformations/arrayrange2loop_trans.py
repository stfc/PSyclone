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
from psyclone.psyir.nodes import Loop, Literal, Range, Reference, BinaryOperation, Array, Assignment

class ArrayRange2LoopTrans(Transformation):
    '''Provides a transformation from a PSyIR Array Range to a PSyIR
    Loop.

    '''
    def apply(self, node):
        ''' xxx '''
        self.validate(node)

        parent = node.parent
        symbol_table = node.scope.symbol_table
        loop_variable_name = symbol_table.new_symbol_name(root_name="idx")
        loop_variable_symbol = DataSymbol(loop_variable_name, INTEGER_TYPE)
        symbol_table.add(loop_variable_symbol)
        
        # replace the first range found in all arrays with the
        # iterator and use the range from the LHS range for the loop
        # iteration space.
        for array in node.walk(Array):
            for idx, child in enumerate(array.children):
                if isinstance(child, Range):
                    # find any lbounds
                    for lbound in [op for op in child.walk(BinaryOperation) if op.operator == BinaryOperation.Operator.LBOUND]:
                        array_ref = lbound.children[0]
                        array_index = int(lbound.children[1].value)-1
                        array_symbol = array_ref.symbol
                        dimension = array_symbol.shape[array_index]
                        if isinstance(dimension, int):
                            # lbound is 1
                            index = lbound.parent.children.index(lbound)
                            lbound.parent.children[index] = Literal("1", INTEGER_TYPE)
                        else:
                            raise NotImplementedError("Implement me")

                    # find any ubounds
                    for ubound in [op for op in child.walk(BinaryOperation) if op.operator == BinaryOperation.Operator.UBOUND]:
                        array_ref = ubound.children[0]
                        array_index = int(ubound.children[1].value)-1
                        array_symbol = array_ref.symbol
                        dimension = array_symbol.shape[array_index]
                        if isinstance(dimension, int):
                            # ubound is the value
                            index = ubound.parent.children.index(ubound)
                            ubound.parent.children[index] = Literal(str(dimension), INTEGER_TYPE)
                        else:
                            raise NotImplementedError("Implement me")

                    if array is node.lhs:
                        lhs_index = idx
                        lhs_range = child
                    else:
                        # Issue #YYY: Support loop variables where the
                        # ranges are different.
                        if not lhs_range.same_as(child):
                            # Ranges are, or may be, different.
                            raise GenerationError(
                                "The ArrayRange2LoopTrans transformation only "
                                "supports ranges that are known to be the same "
                                "as each other but array access '{0}' "
                                "dimension {1} and '{2}' dimension {3} are "
                                "either different or can't be determined."
                                "".format(node.lhs.name, lhs_index, array.name,
                                          idx))
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
