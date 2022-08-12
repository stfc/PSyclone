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
    Literal, Loop, ArrayReference, IfBlock)
from psyclone.psyir.symbols import (
    DataSymbol, REAL_TYPE, INTEGER_TYPE, ScalarType, ArrayType)
from psyclone.psyir.transformations.intrinsics.operator2code_trans import (
    Operator2CodeTrans)


def get_args(node):
    ''' xxx '''
    # Determine the arguments to sum
    args = [None, None, None]
    arg_names_map = {"array": 0, "dimension": 1, "mask": 2}
    for idx in range(len(node.children)):
        if not node.argument_names[idx]:
            # positional arg
            args[idx] = node.children[idx]
        else:
            # named arg
            name = node.argument_names[idx].lower()
            args[arg_names_map[name]] = node.children[idx]
    array_ref = args[0]
    dimension_ref = args[1]
    mask_ref = args[2]
    return (array_ref, dimension_ref, mask_ref)


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
        ''' Check that the input node is valid before applying the transformation.

        :param node: xxx
        :type node: yyy
        :param options: xxx
        :type options: yyy

        :raises TransformationError: xxxx

        '''
        # Avoid circular import error
        from psyclone.psyir.transformations import TransformationError

        super(Sum2CodeTrans, self).validate(node, options)

        array_ref, dim_ref, mask_ref = get_args(node)
        if dim_ref and not isinstance(dim_ref, (Literal, Reference)):
            raise TransformationError(
                f"Can't find the value of the dimension argument. Expected "
                f"it to be a literal or a reference but found "
                f"'{self._writer(dim_ref)}' which is a "
                f"'{type(dim_ref).__name__}'.")

        if len(array_ref.children) == 0:
            if not array_ref.symbol.is_array:
                raise TransformationError(f"Expected '{array_ref.name}' to be an array.")

        for idx, shape in enumerate(array_ref.symbol.shape):
            if not (shape in [
                    ArrayType.Extent.DEFERRED, ArrayType.Extent.ATTRIBUTE]
                    or isinstance(shape, ArrayType.ArrayBounds)):
                raise TransformationError(
                    f"Unexpected shape for array. Expecting one of Deferred, "
                    f"Attribute or Bounds but found '{shape}'.")

        array_intrinsic = array_ref.symbol.datatype.intrinsic
        if array_intrinsic not in [ScalarType.Intrinsic.REAL,
                                   ScalarType.Intrinsic.INTEGER]:
            raise TransformationError(
                f"Only real and integer types supported for array "
                f"'{array_ref.name}', but found '{array_intrinsic.name}'.")


    def apply(self, node, options=None):
        '''Apply the SUM intrinsic conversion transformation to the specified
        node. This node must be a SUM Operation which is converted to
        equivalent inline code.

        '''
        self.validate(node)

        array_ref, dimension_ref, mask_ref = get_args(node)

        # Determine the literal value of the dimension argument
        dimension_literal = None
        if not dimension_ref:
            # there is no dimension argument
            pass
        elif isinstance(dimension_ref, Literal):
            dimension_literal = dimension_ref
        elif (isinstance(dimension_ref, Reference) and
              dimension_ref.symbol.is_constant):
            dimension_literal = dimension_ref.symbol.constant_value
        # else exception is handled by the validate method.

        # Determine the dimension and extent of the array
        ndims = None
        if len(array_ref.children) == 0:
            if not array_ref.symbol.is_array:
                # Exception handled by validate method
                pass
            ndims = len(array_ref.symbol.shape)

            loop_bounds = []
            for idx, shape in enumerate(array_ref.symbol.shape):
                if shape in [ArrayType.Extent.DEFERRED, ArrayType.Extent.ATTRIBUTE]:
                    # runtime extent using LBOUND and UBOUND required
                    lbound = BinaryOperation.create(
                        BinaryOperation.Operator.LBOUND,
                        Reference(array_ref.symbol),
                        Literal(str(idx+1), INTEGER_TYPE))
                    ubound = BinaryOperation.create(
                        BinaryOperation.Operator.UBOUND,
                        Reference(array_ref.symbol),
                        Literal(str(idx+1), INTEGER_TYPE))
                    loop_bounds.append((lbound, ubound))
                elif isinstance(shape, ArrayType.ArrayBounds):
                    # array extent is defined in the array declaration
                    loop_bounds.append(shape)
                else:
                    # Exception handled by validate method
                    pass
        else:
            # This is an array reference
            loop_bounds = []
            ndims = len(array_ref.children)
            for shape in array_ref.children:
                loop_bounds.append((shape.start.copy(), shape.stop.copy()))

        # Determine the datatype of the array's values and create a
        # scalar of that type
        array_intrinsic = array_ref.symbol.datatype.intrinsic
        array_precision =  array_ref.symbol.datatype.precision
        scalar_type = ScalarType(array_intrinsic, array_precision)

        symbol_table = node.scope.symbol_table
        assignment = node.ancestor(Assignment)

        datatype = scalar_type
        array_reduction = False
        if dimension_ref and ndims>1:
            array_reduction = True
            # We are reducing from one array to another
            shape = []
            for idx, bounds in enumerate(loop_bounds):
                if int(dimension_literal.value)-1 == idx:
                    pass
                else:
                    shape.append(bounds)
            datatype = ArrayType(scalar_type, shape)

        # Create temporary sum variable (sum_var)
        symbol_sum_var = symbol_table.new_symbol(
            "sum_var", symbol_type=DataSymbol, datatype=datatype)

        # Replace operation with a temporary variable (sum_var).
        node.replace_with(Reference(symbol_sum_var))

        # sum_var=0.0 or 0
        lhs = Reference(symbol_sum_var)
        if scalar_type.intrinsic == scalar_type.Intrinsic.REAL:
            rhs = Literal("0.0", scalar_type)
        elif scalar_type.intrinsic == scalar_type.Intrinsic.INTEGER:
            rhs = Literal("0", scalar_type)
        else:
            # exception handled by validate method
            pass

        new_assignment = Assignment.create(lhs, rhs)
        assignment.parent.children.insert(assignment.position, new_assignment)

        # Create the loop iterators
        loop_iterators = []
        array_iterators = []
        for idx in range(ndims):
            loop_iterator = symbol_table.new_symbol(
                f"i_{idx}", symbol_type=DataSymbol, datatype=INTEGER_TYPE)
            loop_iterators.append(loop_iterator)
            if array_reduction and idx != int(dimension_literal.value)-1:
                array_iterators.append(loop_iterator)

        if array_reduction:
            # sum_var(i,...) = sum_var(i,...) + array(i,...)
            array_indices = [Reference(iterator) for iterator in array_iterators]
            lhs = ArrayReference.create(symbol_sum_var, array_indices)
            array_indices = [Reference(iterator) for iterator in array_iterators]                
            rhs_child1 = ArrayReference.create(symbol_sum_var, array_indices)
        else:
            # sum_var = sum_var + array(i,...)
            lhs = Reference(symbol_sum_var)
            rhs_child1 = Reference(symbol_sum_var)

        rhs_child2 = node.children[0].detach()
        rhs = BinaryOperation.create(BinaryOperation.Operator.ADD, rhs_child1,
                                     rhs_child2)
        statement = Assignment.create(lhs, rhs)
        if mask_ref:
            # A mask argument has been provided
            for ref in mask_ref.walk(Reference):
                if ref.name == array_ref.name:
                    # The array needs indexing
                    shape = [Reference(obj) for obj in loop_iterators]
                    reference = ArrayReference.create(ref.symbol, shape)
                    ref.replace_with(reference)
            statement = IfBlock.create(mask_ref.detach(), [statement])

        array_indices = []
        for idx in range(ndims):
            statement = Loop.create(
                loop_iterators[idx], loop_bounds[idx][0], loop_bounds[idx][1],
                Literal("1", INTEGER_TYPE), [statement])
            array_indices.append(Reference(loop_iterators[idx]))

        assignment.parent.children.insert(assignment.position, statement)
        rhs_child2.replace_with(
            ArrayReference.create(rhs_child2.symbol, array_indices))
