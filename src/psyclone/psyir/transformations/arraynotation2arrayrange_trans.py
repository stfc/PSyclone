# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# Author R. W. Ford

'''Module providing a transformation from PSyIR Array notation to an
array range.  This can be useful to determine when we have array
accesses (as it is not clear with array notation) and can allow
further optimisations such as transforming to explicit loops.

'''

from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import Range, Reference, ArrayReference, Literal, \
    BinaryOperation
from psyclone.psyir.symbols import INTEGER_TYPE
from psyclone.psyir.transformations.transformation_error \
    import TransformationError


class ArrayNotation2ArrayRangeTrans(Transformation):
    '''Provides a transformation from PSyIR Array Notation to a PSyIR
    Range. For example:

    xxx

    '''
    def validate(self, node, options=None):
        '''Check that the node is a Reference node and that the symbol it
        references is an array.

        :param node: a Reference node.
        :type node: :py:class:`psyclone.psyir.nodes.Reference`
        :param options: a dict with options for transformations.
        :type options: Optional[Dict[str]]

        :raises TransformationError: if the node is not a Reference \
            node or the Reference node not does not reference an array \
            symbol.

        '''
        if not type(node) is Reference:
            raise TransformationError(
                f"The supplied node should be a Reference but found "
                f"'{type(node).__name__}'.")
        if not node.symbol.is_array:
            raise TransformationError(
                f"The supplied node should be a Reference that references a "
                f"symbol that is an array, but '{node.symbol.name}' is not.")

    def apply(self, node, options=None):
        '''Apply the ArrayNotation2ArrayRangeTrans transformation to the
        specified node. The node must be a Reference. If the Reference
        is to an array then the reference is replaced by an
        ArrayReference with appropriate explicit range nodes (termed
        colon notation in Fortran).

        :param node: a Reference node.
        :type node: :py:class:`psyclone.psyir.nodes.Reference`
        :param options: a dict with options for transformations.
        :type options: Optional[Dict[str]]

        '''
        self.validate(node, options=None)

        symbol = node.symbol
        indices = []
        for idx, _ in enumerate(symbol.shape):
            lbound = BinaryOperation.create(
                BinaryOperation.Operator.LBOUND, Reference(symbol),
                Literal(str(idx+1), INTEGER_TYPE))
            ubound = BinaryOperation.create(
                BinaryOperation.Operator.UBOUND, Reference(symbol),
                Literal(str(idx+1), INTEGER_TYPE))
            indices.append(Range.create(lbound, ubound))

        array_ref = ArrayReference.create(symbol, indices)
        node.replace_with(array_ref)
