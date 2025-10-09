# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2025, Science and Technology Facilities Council.
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
# Modified: A. B. G. Chalk, STFC Daresbury Lab

'''Module providing a transformation from a PSyIR MATMUL operator to
PSyIR code. This could be useful if the MATMUL operator is not
supported by the back-end or if the performance in the inline code is
better than the intrinsic. MATMUL supports both matrix multiply and
matrix vector multiply. This transformation supports both with the
restriction that the first matrix must be of at least rank 2.

'''
import warnings

from psyclone.psyir.nodes import (
    BinaryOperation, Assignment, Reference,
    Loop, Literal, ArrayReference, Range, IntrinsicCall)
from psyclone.psyir.symbols import (
    DataSymbol, INTEGER_TYPE, REAL_TYPE, TypedSymbol, UnsupportedType)
from psyclone.psyir.transformations.intrinsics.intrinsic2code_trans import (
    Intrinsic2CodeTrans)
from psyclone.utils import transformation_documentation_wrapper


def _create_array_ref(array_symbol, loop_idx_symbols, other_dims,
                      loop_idx_order, other_dims_order):
    '''
    Utility function to create a reference to an array element being
    accessed using one or more loop indices followed by zero or more
    other index expressions.

    :param array_symbol: the symbol representing the array.
    :type array_symbol: :py:class:`psyclone.psyir.symbols.DataSymbol`
    :param loop_idx_symbols: loop indices which will index into the array.
    :type loop_idx_symbols: List[:py:class:`psyclone.psyir.symbols.DataSymbol`]
    :param other_dims: index expressions for any other dimensions that are
                       not being looped over. (If there are none then this
                       must be an empty list.)
    :type other_dims: List[:py:class:`psyclone.psyir.nodes.ExpressionNode`]
    :param List[int] loop_idx_order: list of indices in the original array
                                     where the loop_idx_symbols are.
    :param List[int] other_dims_order: list of indices in the original array
                                       where the other_dims are.

    :returns: the new reference to an array element.
    :rtype: :py:class:`psyclone.psyir.nodes.ArrayReference`

    '''
    # If there are no other dims, then we simply create
    # an array with the loop_idx_symbols in order.
    if len(other_dims) == 0:
        indices = [Reference(sym) for sym in loop_idx_symbols]
    else:
        n_indices = len(loop_idx_symbols) + len(other_dims)
        indices = [-1]*n_indices
        for it, order_idx in enumerate(loop_idx_order):
            indices[order_idx] = Reference(loop_idx_symbols[it])
        # Fill the remaining indices with the other dims
        for it, order_idx in enumerate(other_dims_order):
            indices[order_idx] = other_dims[it].copy()
    return ArrayReference.create(array_symbol, indices)


@transformation_documentation_wrapper
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

    def _get_full_range_split(self, array):
        '''A utility function that returns the number of full ranges
           and the list of indices which are not full ranges.

        :param array: the reference that we are interested in.
        :type array: :py:class:`psyir.nodes.Reference`
        :returns: tuple with list of full range indices,
                  the list of non full range indices, and
                  list of non full range nodes for this array.
        :rtype: (List[int], List[int], List[psyclone.psyir.nodes.DataNode])

       :raises TransformationError: if any Range nodes are not full, or
            if there are more than two full range nodes.

        '''
        from psyclone.psyir.transformations import TransformationError
        non_full_ranges = []
        full_range_order = []
        non_full_range_order = []
        for it, idx in enumerate(array.children):
            if isinstance(idx, Range):
                if array.is_full_range(it):
                    full_range_order.append(it)
                else:
                    raise TransformationError(
                        f"To use {self.name} on matmul, each Range index "
                        f"of the array '{array.debug_string()}' "
                        f"must be a full range but found "
                        f"non full range at position {it}.")
            else:
                non_full_ranges.append(idx)
                non_full_range_order.append(it)

        # Error raising if we go above 2 full ranges
        n_full_ranges = len(full_range_order)
        if n_full_ranges > 2:
            raise TransformationError(
                f"To use {self.name} on matmul, no more than "
                f"two indices of the array '{array.debug_string()}' "
                f"must be full ranges but found {n_full_ranges}.")
        return (full_range_order, non_full_range_order, non_full_ranges)

    def _validate_array_full_ranges(self, array, permitted_full_ranges):
        '''A utility function that checks if the number of full ranges
           of an array is allowed.

        :param array: the reference that we are interested in.
        :type array: :py:class:`psyir.nodes.Reference`
        :param List[int] permitted_full_ranges: the permitted number
                                                of full ranges for array

       :raises TransformationError: if the number of full ranges
                                    is not permitted.

        '''
        from psyclone.psyir.transformations import TransformationError
        # Make sure the array has as many full ranges as needed
        if (len(array.symbol.shape) in permitted_full_ranges
           and not array.children):
            # If the array only has 1 or 2 dimensions and all of its
            # data is used in the matrix multiply then the reference does
            # not need to supply any dimension information.
            pass
        else:
            # There should be one index per dimension. This is enforced
            # by the array create method so is not tested here.
            full_range_order, _, _ = self._get_full_range_split(array)
            n_full_ranges = len(full_range_order)
            if n_full_ranges not in permitted_full_ranges:
                # Formatting error message nicely.
                if len(permitted_full_ranges) == 1:
                    permit_str = f"{permitted_full_ranges[0]}"
                else:
                    permit_str = f"either {permitted_full_ranges[0]} " \
                                 f"or {permitted_full_ranges[1]}"
                raise TransformationError(
                    f"To use {self.name} on matmul, {permit_str} "
                    f"indices of the array '{array.debug_string()}' "
                    f"must be full ranges but found {n_full_ranges}.")

    def validate(self, node, options=None, **kwargs):
        '''Perform checks to ensure that it is valid to apply the
        Matmul2CodeTran transformation to the supplied node.

        :param node: the node that is being checked.
        :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`
        :param options: options for the transformation.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the node argument is not the
            expected type.
        :raises TransformationError: if the parent of the MATMUL
            operation is not an assignment.
        :raises TransformationError: if the matmul arguments are not in
            the required form.
        :raises TransformationError: if sub-sections of an array are present
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
        for var in [matrix1, matrix2, result]:
            if (not isinstance(var.symbol, TypedSymbol) or
                    isinstance(var.symbol.datatype, UnsupportedType)):
                raise TransformationError(
                    f"Must have full type information for result and operands "
                    f"of MATMUL IntrinsicCall but found '{result.symbol}', "
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
        self._validate_array_full_ranges(matrix1, [2])

        if len(matrix2.symbol.shape) > 2 and not matrix2.children:
            # If matrix2 has no children then it is a reference. If
            # it is a reference then the number of dimensions must be
            # 1 or 2.
            raise TransformationError(
                f"Expected 2nd child of a MATMUL IntrinsicCall to have 1 "
                f"or 2 dimensions, but found '{len(matrix2.symbol.shape)}'.")
        self._validate_array_full_ranges(matrix2, [1, 2])

        # Make sure the result has as many full ranges as needed
        self._validate_array_full_ranges(result, [1, 2])

        # Make sure the result is not one of the MATMUL operands
        if result.symbol in (matrix1.symbol, matrix2.symbol):
            raise TransformationError(
                f"'{result.symbol.name}' is the result location and one of "
                f"the MATMUL operators. This is not supported.")

    def apply(self, node, options=None, **kwargs):
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
        # TODO #2668 options dict is now deprecated
        if options:
            warnings.warn(self._deprecation_warning, DeprecationWarning, 2)

        self.validate(node, options, **kwargs)

        arg2 = node.arguments[1]
        full_range_order, _, _ = self._get_full_range_split(arg2)
        n_full_ranges = len(full_range_order)
        if (n_full_ranges > 1 or
                not arg2.children and len(arg2.symbol.shape) == 2):
            self._apply_matrix_matrix(node)
        else:
            self._apply_matrix_vector(node)

    def _apply_matrix_vector(self, node):
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

        # Create new i and j loop iterators.
        symbol_table = node.scope.symbol_table
        i_loop_sym = symbol_table.new_symbol("i", symbol_type=DataSymbol,
                                             datatype=INTEGER_TYPE)
        j_loop_sym = symbol_table.new_symbol("j", symbol_type=DataSymbol,
                                             datatype=INTEGER_TYPE)

        # Create "result(i)"
        r_fr_order, r_nfr_order, r_nfr = self._get_full_range_split(result)
        result_ref = _create_array_ref(result.symbol,
                                       [i_loop_sym],
                                       r_nfr,
                                       r_fr_order,
                                       r_nfr_order)
        # Create "vector(j)"
        v_fr_order, v_nfr_order, v_nfr = self._get_full_range_split(vector)
        vector_array_reference = _create_array_ref(vector.symbol,
                                                   [j_loop_sym],
                                                   v_nfr,
                                                   v_fr_order,
                                                   v_nfr_order)
        m_fr_order, m_nfr_order, m_nfr = self._get_full_range_split(matrix)
        # Create "matrix(i,j)"
        matrix_array_reference = _create_array_ref(matrix.symbol,
                                                   [i_loop_sym, j_loop_sym],
                                                   m_nfr,
                                                   m_fr_order,
                                                   m_nfr_order)
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
        if len(v_fr_order) == 0:
            # If no full ranges, then the vector was specified
            # without them i.e.: only has one dimension.
            first_pos = 0
        else:
            first_pos = v_fr_order[0]
        lower_bound, upper_bound = vector.symbol.get_bounds(first_pos)
        jloop = Loop.create(j_loop_sym, lower_bound, upper_bound,
                            Literal("1", INTEGER_TYPE), [assign])
        # Create "result(i) = 0.0"
        assign = Assignment.create(result_ref.copy(),
                                   Literal("0.0", REAL_TYPE))
        # Create i loop and add assignment and j loop as children
        if len(m_fr_order) == 0:
            # If no full ranges, then matrix was specified
            # without them i.e.: only has two dimensions.
            first_pos = 0
        else:
            first_pos = m_fr_order[0]
        lower_bound, upper_bound = matrix.symbol.get_bounds(first_pos)
        iloop = Loop.create(i_loop_sym, lower_bound, upper_bound,
                            Literal("1", INTEGER_TYPE), [assign, jloop])
        # Replace the existing assignment with the new loop.
        assignment.replace_with(iloop)

    def _apply_matrix_matrix(self, node):
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
        r_fr_order, r_nfr_order, r_nfr = self._get_full_range_split(result)
        result_ref = _create_array_ref(result.symbol,
                                       [i_loop_sym, j_loop_sym],
                                       r_nfr,
                                       r_fr_order,
                                       r_nfr_order)
        # Create "matrix2(ii,j)"
        m2_fr_order, m2_nfr_order, m2_nfr = self._get_full_range_split(matrix2)
        m2_array_reference = _create_array_ref(matrix2.symbol,
                                               [ii_loop_sym, j_loop_sym],
                                               m2_nfr,
                                               m2_fr_order,
                                               m2_nfr_order)
        # Create "matrix1(i,ii)"
        m1_fr_order, m1_nfr_order, m1_nfr = self._get_full_range_split(matrix1)
        m1_array_reference = _create_array_ref(matrix1.symbol,
                                               [i_loop_sym, ii_loop_sym],
                                               m1_nfr,
                                               m1_fr_order,
                                               m1_nfr_order)
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
        if len(m1_fr_order) == 0:
            # If no full ranges, then matrix was specified
            # without them i.e.: only has two dimensions.
            pos_last = 1
        else:
            pos_last = m1_fr_order[-1]
        lower_bound, upper_bound = matrix1.symbol.get_bounds(pos_last)
        iiloop = Loop.create(ii_loop_sym, lower_bound, upper_bound,
                             Literal("1", INTEGER_TYPE), [assign])
        # Create "result(i,j) = 0.0"
        assign = Assignment.create(result_ref.copy(),
                                   Literal("0.0", REAL_TYPE))
        # Create i loop and add assignment and ii loop as children.
        if len(m1_fr_order) == 0:
            # If no full ranges, then matrix was specified
            # without them i.e.: only has two dimensions.
            pos_first = 0
        else:
            pos_first = m1_fr_order[0]
        lower_bound, upper_bound = matrix1.symbol.get_bounds(pos_first)
        iloop = Loop.create(i_loop_sym, lower_bound, upper_bound,
                            Literal("1", INTEGER_TYPE), [assign, iiloop])
        # Create j loop and add i loop as child.
        if len(m2_fr_order) == 0:
            # If no full ranges, then matrix was specified
            # without them i.e.: only has two dimensions.
            pos_last = 1
        else:
            pos_last = m2_fr_order[-1]
        lower_bound, upper_bound = matrix2.symbol.get_bounds(pos_last)
        jloop = Loop.create(j_loop_sym, lower_bound, upper_bound,
                            Literal("1", INTEGER_TYPE), [iloop])
        # Replace the original assignment with the new loop.
        assignment.replace_with(jloop)


# For AutoAPI auto-documentation generation.
__all__ = ["Matmul2CodeTrans"]
