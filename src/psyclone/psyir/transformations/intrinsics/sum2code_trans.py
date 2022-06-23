# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council
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

'''Module providing a transformation from a PSyIR SUM operator to
PSyIR code. This could be useful if the SUM operator is not supported
by the back-end or if the performance in the inline code is better
than the intrinsic.

'''

from psyclone.psyir.nodes import (
    UnaryOperation, BinaryOperation, NaryOperation, Assignment, Reference,
    Literal, Loop, ArrayReference)
from psyclone.psyir.symbols import DataSymbol, REAL_TYPE, INTEGER_TYPE, ScalarType
from psyclone.psyir.transformations.intrinsics.operator2code_trans import (
    Operator2CodeTrans)


class Sum2CodeTrans(Operator2CodeTrans):
    '''Provides a transformation from a PSyIR SUM Operator node to
    equivalent code in a PSyIR tree. Validity checks are also
    performed.

    If SUM contains a single positional argument which is an array,
    all element on that array are summed and the result returned in
    the scalar R.

    .. code-block:: python

        R = SUM(ARRAY)

    For example, if the array is two dimensional, the equivalent code
    for real data is:

    .. code-block:: python

        R = 0.0
        DO J=LBOUND(A,2),UBOUND(A,2)
          DO I=LBOUND(A,1),UBOUND(A,1)
            R += A(I,J)

    TODO ADD dimension example
    TODO ADD mask example

    '''
    def __init__(self):
        super(Sum2CodeTrans, self).__init__()
        self._operator_name = "SUM"
        self._classes = (UnaryOperation, BinaryOperation, NaryOperation)
        self._operators = (UnaryOperation.Operator.SUM, BinaryOperation.Operator.SUM, NaryOperation.Operator.SUM)

    def validate(self, node, options=None):
        ''' xxx '''
        super(Sum2CodeTrans, self).validate(node, options)

        # Is this really a constraint for us (no, I don't think so).
        ## Check the sum is the only code on the rhs of an assignment i.e. ... = sum(a)
        #if not isinstance(node.parent, Assignment):
        #    raise TransformationError(
        #        "Sum2CodeTrans only supports the transformation of a "
        #        "SUM operation when it is the sole operation on the rhs "
        #        "of an assignment.")

    def apply(self, node, options=None):
        '''Apply the SUM intrinsic conversion transformation to the specified
        node. This node must be a SUM Operation which is converted to equivalent inline code.
        '''
        self.validate(node)

        # TODO get args 1) array, 2) dimension 3) mask
        array_ref = node.children[0]

        ndims = None
        # Determine the dimension of the array
        if len(array_ref.children) == 0:
            if not array_ref.symbol.is_array:
                raise Exception("Expected an array")
            ndims = len(array_ref.symbol.shape)
        else:
            # This is an array reference
            ndims = len(array_ref.children)

        # Determine the datatype of the array's values and create a
        # scalar of that type
        array_intrinsic = array_ref.symbol.datatype.intrinsic
        array_precision =  array_ref.symbol.datatype.precision
        scalar_type = ScalarType(array_intrinsic, array_precision)

        # Try to determine the bounds of the array dimensions

        # sum each dimension into lhs scalar with optional mask.
        # zero scalar sum (real or integer)
        # Create loop for each dimension as a nest
        # if mask, add condition
        # if dimension:
        #   a(i) += b(i,j)
        # else
        #   a += b(i,j)

        symbol_table = node.scope.symbol_table
        assignment = node.ancestor(Assignment)

        # Create temporary sum variable (sum_var)
        symbol_sum_var = symbol_table.new_symbol(
            "sum_var", symbol_type=DataSymbol, datatype=scalar_type)

        # Replace operation with a temporary variable (sum_var).
        node.replace_with(Reference(symbol_sum_var))

        # sum_var=0.0 or 0
        lhs = Reference(symbol_sum_var)
        # TODO "0" if integer
        rhs = Literal("0.0", scalar_type)
        new_assignment = Assignment.create(lhs, rhs)
        assignment.parent.children.insert(assignment.position, new_assignment)

        # sum_var = sum_var + array(i,...)
        lhs = Reference(symbol_sum_var)
        rhs_child1 = Reference(symbol_sum_var)
        rhs_child2 = node.children[0].detach()
        rhs = BinaryOperation.create(BinaryOperation.Operator.ADD, rhs_child1,
                                     rhs_child2)

        statement = Assignment.create(lhs, rhs)
        array_indices = []
        for idx in range(ndims):
            symbol_iterator = symbol_table.new_symbol(
            f"i_{idx}", symbol_type=DataSymbol, datatype=INTEGER_TYPE)
            statement = Loop.create(symbol_iterator, Literal("1", INTEGER_TYPE), Literal("1", INTEGER_TYPE), Literal("1", INTEGER_TYPE), [statement])
            array_indices.append(Reference(symbol_iterator))
        assignment.parent.children.insert(assignment.position, statement)
        rhs_child2.replace_with(ArrayReference.create(rhs_child2.symbol, array_indices))
