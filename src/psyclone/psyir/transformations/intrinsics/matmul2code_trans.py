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
# Author: R. W. Ford, STFC Daresbury Laboratory
# Modified: S. Siso, A. R. Porter and N. Nobre, STFC Daresbury Lab
#           T. Vockerodt, Met Office

'''Module providing a transformation from a PSyIR MATMUL operator to
PSyIR code. This could be useful if the MATMUL operator is not
supported by the back-end or if the performance in the inline code is
better than the intrinsic. MATMUL supports both matrix multiply and
matrix vector multiply. This transformation supports both with the
restriction that the first matrix must be of at least rank 2.

'''
from psyclone.psyir.nodes import (
    BinaryOperation, Assignment, Reference,
    Loop, Literal, ArrayReference, Range, IntrinsicCall)
from psyclone.psyir.symbols import (
    DataSymbol, INTEGER_TYPE, REAL_TYPE, ArrayType, UnsupportedType)
from psyclone.psyir.transformations.intrinsics.intrinsic2code_trans import (
    Intrinsic2CodeTrans)


def _create_matrix_ref(matrix_symbol, loop_idx_symbols, other_dims, order):
    '''
    Utility function to create a reference to a matrix element being
    accessed using one or more loop indices followed by zero or more
    other index expressions.

    :param matrix_symbol: the symbol representing the matrix.
    :type matrix_symbol: :py:class:`psyclone.psyir.symbols.DataSymbol`
    :param loop_idx_symbols: loop indices which will index into the matrix.
    :type loop_idx_symbols: List[:py:class:`psyclone.psyir.symbols.DataSymbol`]
    :param other_dims: index expressions for any other dimensions that are \
                       not being looped over. (If there are none then this \
                       must be an empty list.)
    :type other_dims: List[:py:class:`psyclone.psyir.nodes.ExpressionNode`]
    :param order:      list of indices in the original matrix where the
                       other_dims are.
    :type order:      List[int]

    :returns: the new reference to a matrix element.
    :rtype: :py:class:`psyclone.psyir.nodes.ArrayReference`

    '''
    n_indices = len(loop_idx_symbols) + len(other_dims)
    indices = [-1]*n_indices
    for it, order_idx in enumerate(order):
        indices[order_idx] = other_dims[it].copy()
    # Fill the remaining indices with loop index symbols
    for it, sym in enumerate(loop_idx_symbols):
        for i in range(n_indices):
            if indices[i] == -1:
                indices[i] = Reference(sym)
                break
    return ArrayReference.create(matrix_symbol, indices)


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
    :param int index: the (array) reference index that we are
        interested in.
    :returns: the loop bounds for this array index.
    :rtype: (Literal, Literal, Literal) or
        (BinaryOperation, BinaryOperation, Literal)

   :raises TransformationError: if the shape of the array's symbol is
        not supported.

    '''
    # The 'shape' getter performs validation checks.
    try:
        my_dim = array.symbol.shape[index]
    except TypeError as err:
        # Added import here to avoid circular dependencies.
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.transformations import TransformationError
        raise TransformationError(
            f"Unsupported index type found for array '{array.name}': "
            f"{err}") from err

    dim_index = index + 1  # The Fortran dim argument is 1-indexed
    if isinstance(my_dim, ArrayType.ArrayBounds):
        # Use .copy() to ensure we return new nodes.
        lower_bound = my_dim.lower.copy()
        if my_dim.upper == ArrayType.Extent.ATTRIBUTE:
            # Assumed-shape array.
            upper_bound = IntrinsicCall.create(
                IntrinsicCall.Intrinsic.UBOUND,
                [Reference(array.symbol),
                 ("dim", Literal(str(dim_index), INTEGER_TYPE))])
        else:
            upper_bound = my_dim.upper.copy()
    else:
        lower_bound = IntrinsicCall.create(
            IntrinsicCall.Intrinsic.LBOUND,
            [Reference(array.symbol),
             ("dim", Literal(str(dim_index), INTEGER_TYPE))])
        upper_bound = IntrinsicCall.create(
            IntrinsicCall.Intrinsic.UBOUND,
            [Reference(array.symbol),
             ("dim", Literal(str(dim_index), INTEGER_TYPE))])

    step = Literal("1", INTEGER_TYPE)
    return (lower_bound, upper_bound, step)


def _get_full_range_split(array):
    '''A utility function that returns the number of full ranges
       and the list of indices which are not full ranges.

    :param array: the reference that we are interested in.
    :type array: :py:class:`psyir.nodes.Reference`
    :returns: tuple with number of full ranges and
              the list of non full range indices for this array.
    :rtype: (int, List[psyclone.psyir.nodes.DataNode])

   :raises TransformationError: if any Range nodes are not full, or
        if there are more than two full range nodes.

    '''
    n_full_ranges = 0
    non_full_ranges = []
    order = []
    for it, idx in enumerate(array.children):
        if isinstance(idx, Range):
            if array.is_full_range(it):
                n_full_ranges += 1
            else:
                from psyclone.psyir.transformations import TransformationError
                raise TransformationError(
                    f"To use matmul2code_trans on matmul, each Range index "
                    f"of the argument '{array.name}' must be a full range "
                    f"but found non full range at position {it}.")
        else:
            non_full_ranges.append(idx)
            order.append(it)
        # Early error raising if we go above 2 full ranges
        if n_full_ranges > 2:
            from psyclone.psyir.transformations import TransformationError
            raise TransformationError(
                f"To use matmul2code_trans on matmul, no more than "
                f"two indices of the argument '{array.name}' "
                f"must be full ranges but found {n_full_ranges}.")
    return (n_full_ranges, non_full_ranges, order)


def _get_first_full_range_idx(array, reverse = False):

    # Find last index if asked to
    child_enumerate = enumerate(array.children)
    if reverse:
        child_enumerate = reversed(list(child_enumerate))

    for it, idx in child_enumerate:
        if isinstance(idx, Range):
            if array.is_full_range(it):
                return it
            else:
                from psyclone.psyir.transformations import TransformationError
                raise TransformationError(
                    f"To use matmul2code_trans on matmul, each Range index "
                    f"of the argument '{array.name}' must be a full range "
                    f"but found non full range at position {it}.")
    # Default behaviour
    return -int(reverse == True)


class Matmul2CodeTrans(Intrinsic2CodeTrans):
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
    and ``B`` are ``R(P,M)``, ``A(P,N)``, ``B(N,M)``, the MATMUL is replaced
    with the following code:

    .. code-block:: fortran

        do j=1,M
            do i=1,P
                R(i,j) = 0.0
                do ii=1,N
                    R(i,j) = R(i,j) + A(i,ii) * B(ii,j)

    Note that this transformation does *not* support the case where ``A`` is
    a rank-1 array.

    '''
    def __init__(self):
        super().__init__()
        self._intrinsic = IntrinsicCall.Intrinsic.MATMUL

    def validate(self, node, options=None):
        '''Perform checks to ensure that it is valid to apply the
        Matmul2CodeTran transformation to the supplied node.

        :param node: the node that is being checked.
        :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`
        :param options: options for the transformation.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the node argument is not the \
            expected type.
        :raises TransformationError: if the parent of the MATMUL \
            operation is not an assignment.
        :raises TransformationError: if the matmul arguments are not in \
            the required form.
        :raises TransformationError: if sub-sections of an array are present \
            in the arguments.

        '''
        # pylint: disable=too-many-branches

        # Import here to avoid circular dependencies.
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.transformations import TransformationError

        super().validate(node, options)

        # Check the matmul is the only code on the rhs of an assignment
        # i.e. ... = matmul(a,b)
        if not isinstance(node.parent, Assignment):
            raise TransformationError(
                "Matmul2CodeTrans only supports the transformation of a "
                "MATMUL operation when it is the sole operation on the rhs "
                "of an assignment.")

        matrix1 = node.arguments[0]
        matrix2 = node.arguments[1]
        result = node.parent.lhs

        # The arguments of matvec should be References
        if not (isinstance(matrix1, Reference) and
                isinstance(matrix2, Reference) and
                isinstance(result, Reference)):
            raise TransformationError(
                f"Expected result and operands of MATMUL IntrinsicCall to "
                f"be references, but found: '{node.parent.debug_string()}'.")

        # The arguments of matvec should be References to arrays
        if any(isinstance(var.symbol.datatype, UnsupportedType) for var
               in [matrix1, matrix2, result]):
            raise TransformationError(
                f"Must have full type information for result and operands of "
                f"MATMUL IntrinsicCall but found '{result.symbol}', "
                f"'{matrix1.symbol}' and '{matrix2.symbol}'.")
        if (len(matrix1.symbol.shape) == 0 or len(matrix2.symbol.shape) == 0 or
                len(result.symbol.shape) == 0):
            raise TransformationError(
                f"Expected result and operands of MATMUL IntrinsicCall to "
                f"be references to arrays but found '{result.symbol}', "
                f"'{matrix1.symbol}' and '{matrix2.symbol}'.")

        # The first child (matrix1) should be declared as an array
        # with at least 2 dimensions.
        if len(matrix1.symbol.shape) < 2:
            raise TransformationError(
                f"Expected 1st child of a MATMUL IntrinsicCall to be a "
                f"matrix with at least 2 dimensions, but found "
                f"'{len(matrix1.symbol.shape)}'.")

        if len(matrix1.symbol.shape) > 2 and not matrix1.children:
            # If matrix1 has no children then it is a reference. If
            # it is a reference then the number of dimensions must be 2.
            raise TransformationError(
                f"Expected 1st child of a MATMUL IntrinsicCall to have 2 "
                f"dimensions, but found '{len(matrix1.symbol.shape)}'.")
        if len(matrix1.symbol.shape) == 2 and not matrix1.children:
            # If matrix1 only has 2 dimensions and all of its data is
            # used in the matrix multiply then the reference does
            # not need to supply any dimension information.
            pass
        else:
            # There should be one index per dimension. This is enforced
            # by the array create method so is not tested here.
            # For matrix1, we must have exactly 2 full ranges.
            n_full_ranges, _, _ = _get_full_range_split(matrix1)
            if n_full_ranges != 2:
                raise TransformationError(
                    f"To use matmul2code_trans on matmul, exactly two "
                    f"indices of the 1st argument '{matrix1.name}' "
                    f"must be full ranges but found {n_full_ranges}.")

        if len(matrix2.symbol.shape) > 2 and not matrix2.children:
            # If matrix2 has no children then it is a reference. If
            # it is a reference then the number of dimensions must be
            # 1 or 2.
            raise TransformationError(
                f"Expected 2nd child of a MATMUL IntrinsicCall to have 1 "
                f"or 2 dimensions, but found '{len(matrix2.symbol.shape)}'.")
        if len(matrix2.symbol.shape) in [1, 2] and not matrix2.children:
            # If the 2nd argument only has 1 or 2 dimensions and all of its
            # data is used in the matrix multiply then the reference does
            # not need to supply any dimension information.
            pass
        else:
            # There should be one index per dimension. This is enforced
            # by the array create method so is not tested here.
            # For matrix2, we can have 1 or 2 full ranges.
            n_full_ranges, _, _ = _get_full_range_split(matrix2)
            if n_full_ranges not in [1,2]:
                raise TransformationError(
                    f"To use matmul2code_trans on matmul, one or two "
                    f"indices of the 2nd argument '{matrix2.name}' "
                    f"must be full ranges but found {n_full_ranges}.")

        # Make sure the result has as many full range as needed
        if result.children:
            for idx, child in enumerate(result.children):
                if isinstance(child, Range) and not result.is_full_range(idx):
                    raise TransformationError(
                        f"To use matmul2code_trans on matmul, each range on "
                        f"the result variable '{result.name}' must be a full "
                        f"range but found {result.debug_string()}")

        # Make sure the result is not one of the MATMUL operands
        if result.symbol in (matrix1.symbol, matrix2.symbol):
            raise TransformationError(
                f"'{result.symbol.name}' is the result location and one of "
                f"the MATMUL operators. This is not supported.")

    def apply(self, node, options=None):
        '''Apply the MATMUL intrinsic conversion transformation to the
        specified node. This node must be a MATMUL IntrinsicCall. The first
        argument must currently have two dimensions while the second must have
        either one or two dimensions. Each argument is permitted to have
        additional dimensions (i.e. more than 2) but in each case it is only
        the first one or two which may be ranges. Further, the ranges must
        currently be for the full index space for that dimension (i.e. array
        subsections are not supported). If the transformation is
        successful then an assignment which includes a MATMUL
        IntrinsicCall node is converted to equivalent inline code.

        :param node: a MATMUL IntrinsicCall node.
        :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`
        :param options: options for the transformation.
        :type options: Optional[Dict[str, Any]]

        '''
        self.validate(node, options)

        arg2 = node.arguments[1]
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

        :param node: a MATMUL IntrinsicCall node.
        :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

        '''
        # pylint: disable=too-many-locals
        assignment = node.parent
        matrix = node.arguments[0]
        vector = node.arguments[1]
        result = node.parent.lhs
        result_symbol = result.symbol

        # Create new i and j loop iterators.
        symbol_table = node.scope.symbol_table
        i_loop_sym = symbol_table.new_symbol("i", symbol_type=DataSymbol,
                                             datatype=INTEGER_TYPE)
        j_loop_sym = symbol_table.new_symbol("j", symbol_type=DataSymbol,
                                             datatype=INTEGER_TYPE)

        # Create "result(i)"
        result_dims = [Reference(i_loop_sym)]
        if len(result.children) > 1:
            # Add any additional dimensions (in case of an array slice)
            for child in result.children[1:]:
                result_dims.append(child.copy())
        result_ref = ArrayReference.create(result_symbol, result_dims)
        # Create "vector(j)"
        vector_dims = [Reference(j_loop_sym)]
        if len(vector.children) > 1:
            # Add any additional dimensions (in case of an array slice)
            for child in vector.children[1:]:
                vector_dims.append(child.copy())
        vector_array_reference = ArrayReference.create(
            vector.symbol, vector_dims)
        _, ref_non_full_ranges, order = _get_full_range_split(matrix)
        # Create "matrix(i,j)"
        matrix_array_reference = _create_matrix_ref(matrix.symbol,
                                                    [i_loop_sym, j_loop_sym],
                                                    ref_non_full_ranges,
                                                    order)
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
        first_pos = _get_first_full_range_idx(vector)
        lower_bound, upper_bound, step = _get_array_bound(vector, first_pos)
        jloop = Loop.create(j_loop_sym, lower_bound, upper_bound, step,
                            [assign])
        # Create "result(i) = 0.0"
        assign = Assignment.create(result_ref.copy(),
                                   Literal("0.0", REAL_TYPE))
        # Create i loop and add assignment and j loop as children
        first_pos = _get_first_full_range_idx(matrix)
        lower_bound, upper_bound, step = _get_array_bound(matrix, first_pos)
        iloop = Loop.create(i_loop_sym, lower_bound, upper_bound, step,
                            [assign, jloop])
        # Replace the existing assignment with the new loop.
        assignment.replace_with(iloop)

    @staticmethod
    def _apply_matrix_matrix(node):
        '''
        Apply the transformation for the case of a matrix-matrix
        multiplication.

        :param node: a MATMUL IntrinsicCall.
        :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

        '''
        # pylint: disable=too-many-locals
        assignment = node.parent
        matrix1 = node.arguments[0]
        matrix2 = node.arguments[1]
        result = node.parent.lhs

        # Create new i, j and ii loop iterators.
        symbol_table = node.scope.symbol_table
        i_loop_sym = symbol_table.new_symbol("i", symbol_type=DataSymbol,
                                             datatype=INTEGER_TYPE)
        j_loop_sym = symbol_table.new_symbol("j", symbol_type=DataSymbol,
                                             datatype=INTEGER_TYPE)
        ii_loop_sym = symbol_table.new_symbol("ii", symbol_type=DataSymbol,
                                              datatype=INTEGER_TYPE)
        # Create "result(i,j)"
        _, res_non_full_ranges, order = _get_full_range_split(result)
        result_ref = _create_matrix_ref(result.symbol,
                                        [i_loop_sym, j_loop_sym],
                                        res_non_full_ranges,
                                        order)
        # Create "matrix2(ii,j)"
        _, m2_non_full_ranges, order = _get_full_range_split(matrix2)
        m2_array_reference = _create_matrix_ref(matrix2.symbol,
                                                [ii_loop_sym, j_loop_sym],
                                                m2_non_full_ranges,
                                                order)
        # Create "matrix1(i,ii)"
        _, m1_non_full_ranges, order = _get_full_range_split(matrix1)
        m1_array_reference = _create_matrix_ref(matrix1.symbol,
                                                [i_loop_sym, ii_loop_sym],
                                                m1_non_full_ranges,
                                                order)
        # Create "matrix1(i,ii) * matrix2(ii,j)"
        multiply = BinaryOperation.create(
            BinaryOperation.Operator.MUL, m1_array_reference,
            m2_array_reference)
        # Create "result(i,j) + matrix1(i,ii) * matrix2(ii,j)"
        rhs = BinaryOperation.create(
            BinaryOperation.Operator.ADD, result_ref, multiply)
        # Create "result(i,j) = result(i,j) + matrix1(i,ii) * matrix2(ii,j)"
        assign = Assignment.create(result_ref.copy(), rhs)
        # Create ii loop and add the above code as a child
        # Work out the bounds
        pos_last = _get_first_full_range_idx(matrix1, reverse=True)
        lower_bound, upper_bound, step = _get_array_bound(matrix1, pos_last)
        # Must be the same as _get_array_bound(matrix2, pos_first)
        iiloop = Loop.create(ii_loop_sym, lower_bound, upper_bound, step,
                             [assign])
        # Create "result(i,j) = 0.0"
        assign = Assignment.create(result_ref.copy(),
                                   Literal("0.0", REAL_TYPE))
        # Create i loop and add assignment and ii loop as children.
        pos_first = _get_first_full_range_idx(matrix1)
        lower_bound, upper_bound, step = _get_array_bound(matrix1, pos_first)
        iloop = Loop.create(i_loop_sym, lower_bound, upper_bound, step,
                            [assign, iiloop])
        # Create j loop and add i loop as child.
        pos_last = _get_first_full_range_idx(matrix2, reverse=True)
        lower_bound, upper_bound, step = _get_array_bound(matrix2, pos_last)
        jloop = Loop.create(j_loop_sym, lower_bound, upper_bound, step,
                            [iloop])
        # Replace the original assignment with the new loop.
        assignment.replace_with(jloop)
