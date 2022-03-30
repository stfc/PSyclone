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
# Author: R. W. Ford, STFC Daresbury Laboratory
# Modified: S. Siso, STFC Daresbury Laboratory
#           A. R. Porter, STFC Daresbury Laboratory

'''Module providing a transformation from a PSyIR MATMUL operator to
PSyIR code. This could be useful if the MATMUL operator is not
supported by the back-end or if the performance in the inline code is
better than the intrinsic. MATMUL supports both matrix multiply and
matrix vector multiply. At the moment this transformation is limited
to matrix vector multiply.

'''
from psyclone.psyir.nodes import BinaryOperation, Assignment, Reference, \
    Loop, Literal, ArrayReference, Range
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, REAL_TYPE, \
    ArrayType
from psyclone.psyir.transformations.intrinsics.operator2code_trans import \
    Operator2CodeTrans


def _get_array_bound(array, index):
    '''A utility function that returns the appropriate loop bounds (lower,
    upper and step) for a particular index of an array reference. At
    the moment all entries for the index are assumed to be accessed.
    If the array symbol is declared with known bounds (an integer or a
    symbol) then these bound values are used. If the size is unknown
    (a deferred or attribute type) then the LBOUND and UBOUND PSyIR
    nodes are used.

    :param array: the reference that we are interested in.
    :type array: :py:class:`psyir.nodes.Reference`
    :param int index: the (array) reference index that we are \
        interested in.
    :returns: the loop bounds for this array index.
    :rtype: (Literal, Literal, Literal) or \
        (BinaryOperation, BinaryOperation, Literal)

    :raises TransformationError: if the shape of the array's symbol is \
        not supported.

    '''
    # Added import here to avoid circular dependencies.
    # pylint: disable=import-outside-toplevel
    from psyclone.psyir.transformations import TransformationError

    my_dim = array.symbol.shape[index]
    if isinstance(my_dim, ArrayType.ArrayBounds):
        # Use .copy() to ensure we return new nodes.
        lower_bound = my_dim.lower.copy()
        upper_bound = my_dim.upper.copy()
    elif my_dim in [ArrayType.Extent.DEFERRED, ArrayType.Extent.ATTRIBUTE]:
        lower_bound = BinaryOperation.create(
            BinaryOperation.Operator.LBOUND, Reference(array.symbol),
            Literal(str(index), INTEGER_TYPE))
        upper_bound = BinaryOperation.create(
            BinaryOperation.Operator.UBOUND, Reference(array.symbol),
            Literal(str(index), INTEGER_TYPE))
    else:
        raise TransformationError(
            f"Unsupported index type '{type(my_dim).__name__}' found for "
            f"dimension {index+1} of array '{array.name}'.")
    step = Literal("1", INTEGER_TYPE)
    return (lower_bound, upper_bound, step)


class Matmul2CodeTrans(Operator2CodeTrans):
    '''Provides a transformation from a PSyIR MATMUL Operator node to
    equivalent code in a PSyIR tree. Validity checks are also
    performed.

    For a matrix-vector multiplication, if the dimensions of ``R``, ``A``,
    and ``B`` are ``R(N)``, ``A(N,M)``, ``B(M)``, the transformation replaces:

    .. code-block:: fortran

        R=MATMUL(A,B)

    with the following code:

    .. code-block:: fortran

        do i=1,N
            R(i) = 0.0
                do j=1,M
                    R(i) = R(i) + A(i,j) * B(j)

    For a matrix-matrix multiplication, if the dimensions of ``R``, ``A``,
    and ``B`` are ``R(P,M)``, ``A(N,M)``, ``B(P,N)``, the MATMUL is replaced
    with the following code:

    .. code-block:: fortran

        do i=1,P
            do j=1,M
                R(i,j) = 0.0
                do ii=1,N
                    R(i,j) = R(i,j) + A(ii,i) * B(j,ii)

    '''
    def __init__(self):
        super(Matmul2CodeTrans, self).__init__()
        self._operator_name = "MATMUL"
        self._classes = (BinaryOperation,)
        self._operators = (BinaryOperation.Operator.MATMUL,)

    def validate(self, node, options=None):
        '''Perform checks to ensure that it is valid to apply the
        Matmul2CodeTran transformation to the supplied node.

        :param node: the node that is being checked.
        :type node: :py:class:`psyclone.psyir.nodes.BinaryOperation`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the node argument is not the \
            expected type.
        :raises TransformationError: if the parent of the MATMUL \
            operation is not an assignment.
        :raises TransformationError: if the matmul arguments are not in \
            the required form.
        :raises NotImplementedError: if sub-sections of an array are present \
            in the arguments.

        '''
        # pylint: disable=too-many-branches

        # Import here to avoid circular dependencies.
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.transformations import TransformationError

        super(Matmul2CodeTrans, self).validate(node, options)

        # Check the matmul is the only code on the rhs of an assignment
        # i.e. ... = matmul(a,b)
        if not isinstance(node.parent, Assignment):
            raise TransformationError(
                "Matmul2CodeTrans only supports the transformation of a "
                "MATMUL operation when it is the sole operation on the rhs "
                "of an assignment.")

        matrix = node.children[0]
        vector = node.children[1]
        # The children of matvec should be References
        if not (isinstance(matrix, Reference) and
                isinstance(vector, Reference)):
            raise TransformationError(
                f"Expected children of a MATMUL BinaryOperation to be "
                f"references, but found '{type(matrix).__name__}', "
                f"'{type(vector).__name__}'.")

        # The children of matvec should be References to arrays
        if not (matrix.symbol.shape or vector.symbol.shape):
            raise TransformationError(
                f"Expected children of a MATMUL BinaryOperation to be "
                f"references to arrays, but found "
                f"'{type(matrix.symbol).__name__}', "
                f"'{type(vector.symbol).__name__}'.")

        # The first child (the matrix) should be declared as an array
        # with at least 2 dimensions
        if len(matrix.symbol.shape) < 2:
            raise TransformationError(
                f"Expected 1st child of a MATMUL BinaryOperation to be a "
                f"matrix with at least 2 dimensions, but found "
                f"'{len(matrix.symbol.shape)}'.")

        if len(matrix.symbol.shape) > 2 and not matrix.children:
            # If the matrix has no children then it is a reference. If
            # it is a reference then the number of dimensions must be 2.
            raise TransformationError(
                f"Expected 1st child of a MATMUL BinaryOperation to have 2 "
                f"dimensions, but found '{len(matrix.symbol.shape)}'.")
        if len(matrix.symbol.shape) == 2 and not matrix.children:
            # If the matrix only has 2 dimensions and all of its data is
            # used in the matrix vector multiply then the reference does
            # not need to supply any dimension information.
            pass
        else:
            # There should be one index per dimension. This is enforced
            # by the array create method so is not tested here.

            # The first two indices should be ranges. This is a
            # limitation of this transformation, not the PSyIR, as it
            # would be valid to have any two indices being
            # ranges. Further, this transformation is currently
            # limited to Ranges which specify the full extent of the
            # dimension.
            if not (matrix.is_full_range(0) and matrix.is_full_range(1)):
                raise NotImplementedError(
                    f"To use matmul2code_trans on matmul, the first two "
                    f"indices of the 1st argument '{matrix.name}' must be "
                    f"full ranges.")

            if len(matrix.children) > 2:
                # The 3rd index and onwards must not be ranges.
                for (count, index) in enumerate(matrix.children[2:]):
                    if isinstance(index, Range):
                        raise NotImplementedError(
                            f"To use matmul2code_trans on matmul, only the "
                            f"first two indices of the 1st argument are "
                            f"permitted to be Ranges but found "
                            f"{type(index).__name__} at index {count+2}.")

        if len(vector.symbol.shape) > 2 and not vector.children:
            # If the vector has no children then it is a reference. If
            # it is a reference then the number of dimensions must be
            # 1 or 2.
            raise TransformationError(
                f"Expected 2nd child of a MATMUL BinaryOperation to have 1 "
                f"or 2 dimensions, but found '{len(vector.symbol.shape)}'.")
        if len(vector.symbol.shape) in [1, 2] and not vector.children:
            # If the 2nd argument only has 1 or 2 dimensions and all of its
            # data is used in the matrix multiply then the reference does
            # not need to supply any dimension information.
            pass
        else:
            # There should be one index per dimension. This is enforced
            # by the array create method so is not tested here.

            # The first index should be a range. This
            # transformation is currently limited to Ranges which
            # specify the full extent of the dimension.
            if not vector.is_full_range(0):
                raise NotImplementedError(
                    f"To use matmul2code_trans on matmul, the first index of "
                    f"the 2nd argument '{vector.name}' must be a full range.")
            # Check that the second dimension is a full range if it is
            # a range.
            if (len(vector.symbol.shape) > 1 and isinstance(vector.children[1],
                                                            Range)
                    and not vector.is_full_range(1)):
                raise NotImplementedError(
                    f"To use matmul2code_trans on matmul for a matrix-matrix "
                    f"multiplication, the second index of the 2nd "
                    f"argument '{vector.name}' must be a full range.")
            if len(vector.children) > 2:
                # The 3rd index and onwards must not be ranges.
                for (count, index) in enumerate(vector.children[2:]):
                    if isinstance(index, Range):
                        raise NotImplementedError(
                            f"To use matmul2code_trans on matmul, only the "
                            f"first two indices of the 2nd argument are "
                            f"permitted to be a Range but found "
                            f"{type(index).__name__} at index {count+1}.")

    def apply(self, node, options=None):
        '''Apply the MATMUL intrinsic conversion transformation to the
        specified node. This node must be a MATMUL
        BinaryOperation. Each argument is permitted to have
        additional dimensions (i.e. more than 2) but in each case it is only
        the first two which may be ranges. Further, the ranges must currently
        be for the full index space for that dimension (i.e. array
        subsections are not supported). If the transformation is
        successful then an assignment which includes a MATMUL
        BinaryOperation node is converted to equivalent inline code.

        :param node: a MATMUL Binary-Operation node.
        :type node: :py:class:`psyclone.psyir.nodes.BinaryOperation`
        :param options: options for the transformation.
        :type options: Optional[Dict[str, str]]

        '''
        self.validate(node)

        arg2 = node.children[1]
        if (len(arg2.children) > 1 and isinstance(arg2.children[1], Range) or
                not arg2.children and len(arg2.symbol.shape) == 2):
            self._apply_matrix_matrix(node)
        else:
            self._apply_matrix_vector(node)

    @staticmethod
    def _apply_matrix_vector(node):
        '''
        Apply the transformation for the case of a matrix-vector
        multiplication.

        :param node: a MATMUL Binary-Operation node.
        :type node: :py:class:`psyclone.psyir.nodes.BinaryOperation`

        '''
        # pylint: disable=too-many-locals
        assignment = node.parent
        matrix = node.children[0]
        vector = node.children[1]
        result = node.parent.lhs
        result_symbol = result.symbol

        # Create new i and j loop iterators.
        symbol_table = node.scope.symbol_table
        i_loop_symbol = symbol_table.new_symbol("i", symbol_type=DataSymbol,
                                                datatype=INTEGER_TYPE)
        j_loop_symbol = symbol_table.new_symbol("j", symbol_type=DataSymbol,
                                                datatype=INTEGER_TYPE)

        # Create "result(i)"
        result_dims = [Reference(i_loop_symbol)]
        if len(result.children) > 1:
            # Add any additional dimensions (in case of an array slice)
            for child in result.children[1:]:
                result_dims.append(child.copy())
        result_ref = ArrayReference.create(result_symbol, result_dims)
        # Create "vector(j)"
        vector_dims = [Reference(j_loop_symbol)]
        if len(vector.children) > 1:
            # Add any additional dimensions (in case of an array slice)
            for child in vector.children[1:]:
                vector_dims.append(child.copy())
        vector_array_reference = ArrayReference.create(
            vector.symbol, vector_dims)
        # Create "matrix(i,j)"
        array_dims = [Reference(i_loop_symbol), Reference(j_loop_symbol)]
        if len(matrix.children) > 2:
            # Add any additional dimensions (in case of an array slice)
            for child in matrix.children[2:]:
                array_dims.append(child.copy())
        matrix_array_reference = ArrayReference.create(matrix.symbol,
                                                       array_dims)
        # Create "matrix(i,j) * vector(j)"
        multiply = BinaryOperation.create(
            BinaryOperation.Operator.MUL, matrix_array_reference,
            vector_array_reference)
        # Create "result(i) + matrix(i,j) * vector(j)"
        rhs = BinaryOperation.create(
            BinaryOperation.Operator.ADD, result_ref, multiply)
        # Create "result(i) = result(i) + matrix(i,j) * vector(j)"
        assign = Assignment.create(result_ref.copy(), rhs)
        # Create j loop and add the above code as a child
        # Work out the bounds
        lower_bound, upper_bound, step = _get_array_bound(vector, 0)
        jloop = Loop.create(j_loop_symbol, lower_bound, upper_bound, step,
                            [assign])
        # Create "result(i) = 0.0"
        assign = Assignment.create(result_ref.copy(),
                                   Literal("0.0", REAL_TYPE))
        # Create i loop and add assigment and j loop as children
        lower_bound, upper_bound, step = _get_array_bound(matrix, 0)
        iloop = Loop.create(i_loop_symbol, lower_bound, upper_bound, step,
                            [assign, jloop])
        # Add the new code to the PSyIR
        assignment.parent.children.insert(assignment.position, iloop)
        # remove the original matmul
        assignment.parent.children.remove(assignment)

    @staticmethod
    def _apply_matrix_matrix(node):
        '''
        Apply the transformation for the case of a matrix-matrix
        multiplication.

        :param node: a MATMUL Binary-Operation node.
        :type node: :py:class:`psyclone.psyir.nodes.BinaryOperation`

        '''
        # pylint: disable=too-many-locals
        assignment = node.parent
        matrix1 = node.children[0]
        matrix2 = node.children[1]
        result = node.parent.lhs
        result_symbol = result.symbol

        # Create new i, j and ii loop iterators.
        symbol_table = node.scope.symbol_table
        i_loop_symbol = symbol_table.new_symbol("i", symbol_type=DataSymbol,
                                                datatype=INTEGER_TYPE)
        j_loop_symbol = symbol_table.new_symbol("j", symbol_type=DataSymbol,
                                                datatype=INTEGER_TYPE)
        ii_loop_symbol = symbol_table.new_symbol("ii", symbol_type=DataSymbol,
                                                 datatype=INTEGER_TYPE)
        # Create "result(i,j)"
        result_dims = [Reference(i_loop_symbol), Reference(j_loop_symbol)]
        if len(result.children) > 2:
            # Add any additional dimensions (in case of an array slice)
            for child in result.children[2:]:
                result_dims.append(child.copy())
        result_ref = ArrayReference.create(result_symbol, result_dims)
        # Create "matrix2(j,ii)"
        m2_dims = [Reference(j_loop_symbol), Reference(ii_loop_symbol)]
        if len(matrix2.children) > 2:
            # Add any additional dimensions (in case of an array slice)
            for child in matrix2.children[2:]:
                m2_dims.append(child.copy())
        m2_array_reference = ArrayReference.create(matrix2.symbol, m2_dims)
        # Create "matrix1(ii,i)"
        m1_dims = [Reference(ii_loop_symbol), Reference(i_loop_symbol)]
        if len(matrix1.children) > 2:
            # Add any additional dimensions (in case of an array slice)
            for child in matrix1.children[2:]:
                m1_dims.append(child.copy())
        m1_array_reference = ArrayReference.create(matrix1.symbol, m1_dims)
        # Create "matrix1(ii,i) * matrix2(j,ii)"
        multiply = BinaryOperation.create(
            BinaryOperation.Operator.MUL, m1_array_reference,
            m2_array_reference)
        # Create "result(i,j) + matrix1(ii,i) * matrix2(j,ii)"
        rhs = BinaryOperation.create(
            BinaryOperation.Operator.ADD, result_ref, multiply)
        # Create "result(i,j) = result(i,j) + matrix1(ii,i) * matrix2(j,ii)"
        assign = Assignment.create(result_ref.copy(), rhs)
        # Create ii loop and add the above code as a child
        # Work out the bounds
        lower_bound, upper_bound, step = _get_array_bound(matrix1, 0)
        iiloop = Loop.create(ii_loop_symbol, lower_bound, upper_bound, step,
                             [assign])
        # Create "result(i,j) = 0.0"
        assign = Assignment.create(result_ref.copy(),
                                   Literal("0.0", REAL_TYPE))
        # Create j loop and add assignment and ii loop as children.
        lower_bound, upper_bound, step = _get_array_bound(matrix1, 1)
        jloop = Loop.create(j_loop_symbol, lower_bound, upper_bound, step,
                            [assign, iiloop])
        # Create i loop and add  j loop as child.
        lower_bound, upper_bound, step = _get_array_bound(matrix2, 0)
        iloop = Loop.create(i_loop_symbol, lower_bound, upper_bound, step,
                            [jloop])
        # Add the new code to the PSyIR
        assignment.parent.children.insert(assignment.position, iloop)
        # remove the original matmul
        assignment.parent.children.remove(assignment)
