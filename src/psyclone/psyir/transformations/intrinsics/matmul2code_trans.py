# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council
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

'''Module providing a transformation from a PSyIR MATMUL operator to
PSyIR code. This could be useful if the MATMUL operator is not
supported by the back-end or if the performance in the inline code is
better than the intrinsic. MATMUL supports both matrix multiply and
matrix vector multiply. At the moment this transformation is limited
to matrix vector multiply.

'''
from __future__ import absolute_import
from psyclone.psyir.nodes import BinaryOperation, Assignment, Reference, \
    Loop, Literal, Array, Range
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
    my_dim = array.symbol.shape[index]
    if isinstance(my_dim, int):
        lower_bound = Literal("1", INTEGER_TYPE)
        upper_bound = Literal(str(my_dim), INTEGER_TYPE)
    elif isinstance(my_dim, DataSymbol):
        lower_bound = Literal("1", INTEGER_TYPE)
        upper_bound = Reference(my_dim)
    elif my_dim in [ArrayType.Extent.DEFERRED, ArrayType.Extent.ATTRIBUTE]:
        lower_bound = BinaryOperation.create(
            BinaryOperation.Operator.LBOUND, Reference(array.symbol),
            Literal(str(index), INTEGER_TYPE))
        upper_bound = BinaryOperation.create(
            BinaryOperation.Operator.UBOUND, Reference(array.symbol),
            Literal(str(index), INTEGER_TYPE))
    else:
        # Added import here to avoid circular dependencies.
        from psyclone.psyir.transformations import TransformationError
        raise TransformationError(
            "Unsupported index type '{0}' found for dimension {1} of array "
            "'{2}'.".format(type(my_dim).__name__, index+1, array.name))
    step = Literal("1", INTEGER_TYPE)
    return (lower_bound, upper_bound, step)


class Matmul2CodeTrans(Operator2CodeTrans):
    '''Provides a transformation from a PSyIR MATMUL Operator node to
    equivalent code in a PSyIR tree. Validity checks are also
    performed. Currently only the matrix vector version of MATMUL is
    supported.

    If the dimensions of ``R``, ``A``, and ``B`` are ``R(N)``,
    ``A(N,M)``, ``B(M)``, The transformation replaces:

    .. code-block:: fortran

        R=MATMUL(A,B)

    with the following code:

    .. code-block:: fortran

        do i=1,N
            R(i) = 0.0
                do j=1,M
                    R(i) = R(i) + A(i,j) * B(j)

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

        '''
        # pylint: disable=too-many-branches

        # Import here to avoid circular dependencies.
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
                "Expected children of a MATMUL BinaryOperation to be "
                "references, but found '{0}', '{1}'."
                "".format(type(matrix).__name__,
                          type(vector).__name__))

        # The children of matvec should be References to arrays
        if not (matrix.symbol.shape or vector.symbol.shape):
            raise TransformationError(
                "Expected children of a MATMUL BinaryOperation to be "
                "references to arrays, but found '{0}', '{1}'."
                "".format(type(matrix.symbol).__name__,
                          type(vector.symbol).__name__))

        # The first child (the matrix) should be declared as an array
        # with at least 2 dimensions
        if len(matrix.symbol.shape) < 2:
            raise TransformationError(
                "Expected 1st child of a MATMUL BinaryOperation to be "
                "a matrix with at least 2 dimensions, but found '{0}'."
                "".format(len(matrix.symbol.shape)))
        if len(matrix.symbol.shape) > 2 and not matrix.children:
            # If the matrix has no children then it is a reference. If
            # it is a reference then the number of arguments must be
            # 2.
            raise TransformationError(
                "Expected 1st child of a MATMUL BinaryOperation to have 2 "
                "dimensions, but found '{0}'."
                "".format(len(matrix.symbol.shape)))
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
                    "To use matmul2code_trans on matmul, indices 0 and 1 of "
                    "the 1st (matrix) argument '{0}' must be full ranges."
                    "".format(matrix.name))

            if len(matrix.children) > 2:
                # The 3rd index and onwards must not be ranges.
                for (count, index) in enumerate(matrix.children[2:]):
                    if isinstance(index, Range):
                        raise NotImplementedError(
                            "To use matmul2code_trans on matmul, only the "
                            "first two indices of the 1st (matrix) argument "
                            "are permitted to be Ranges but found {0} at "
                            "index {1}.".format(type(index).__name__, count+2))

        if len(vector.symbol.shape) > 1 and not vector.children:
            # If the vector has no children then it is a reference. If
            # it is a reference then the number of arguments must be
            # 1.
            raise TransformationError(
                "Expected 2nd child of a MATMUL BinaryOperation to have 1 "
                "dimension, but found '{0}'."
                "".format(len(vector.symbol.shape)))
        if len(vector.symbol.shape) == 1 and not vector.children:
            # If the vector only has 1 dimension and all of its data is
            # used in the matrix vector multiply then the reference does
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
                    "To use matmul2code_trans on matmul, index 0 of the 2nd "
                    "(vector) argument '{0}' must be a full range."
                    "".format(matrix.name))
            if len(vector.children) > 1:
                # The 2nd index and onwards must not be ranges.
                for (count, index) in enumerate(vector.children[1:]):
                    if isinstance(index, Range):
                        raise NotImplementedError(
                            "To use matmul2code_trans on matmul, only the "
                            "first index of the 2nd (vector) argument is "
                            "permitted to be a Range but found {0} at index "
                            "{1}.".format(type(index).__name__, count+1))

    def apply(self, node, options=None):
        '''Apply the MATMUL intrinsic conversion transformation to the
        specified node. This node must be a MATMUL
        BinaryOperation. Currently only the matrix vector version of
        MATMUL is supported. The arguments are permitted to have
        additional dimensions (i.e. more than 2 for the matrix and
        more than 1 for the vector) but the matrix can only have two
        indices which are ranges and these must be the first two
        indices and the vector can only have one index that is a range
        and this must be the first index. Further, the ranges must be
        for the full index space for that dimension (i.e. array
        subsections are not supported). If the transformation is
        successful then an assignment which includes a MATMUL
        BinaryOperation node is converted to equivalent inline code.

        :param node: a MATMUL Binary-Operation node.
        :type node: :py:class:`psyclone.psyGen.BinaryOperation`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        '''
        # pylint: disable=too-many-locals
        self.validate(node)

        assignment = node.parent
        matrix = node.children[0]
        vector = node.children[1]
        result = node.parent.lhs
        result_symbol = result.symbol

        # Create new i and j loop iterators.
        # Note that this approach risks declaring the same symbol
        # names as a parent symbol table until issue #630 is
        # addressed.
        symbol_table = node.find_symbol_table()
        i_loop_name = symbol_table.new_symbol_name("i")
        i_loop_symbol = DataSymbol(i_loop_name, INTEGER_TYPE)
        symbol_table.add(i_loop_symbol)
        j_loop_name = symbol_table.new_symbol_name("j")
        j_loop_symbol = DataSymbol(j_loop_name, INTEGER_TYPE)
        symbol_table.add(j_loop_symbol)

        # Create "result(i)"
        result_dims = [Reference(i_loop_symbol)]
        if len(result.children) > 1:
            # Add any additional dimensions (in case of an array slice)
            result_dims.extend(result.children[1:])
        result = Array.create(result_symbol, result_dims)
        # Create "vector(j)"
        vector_dims = [Reference(j_loop_symbol)]
        if len(vector.children) > 1:
            # Add any additional dimensions (in case of an array slice)
            vector_dims.extend(vector.children[1:])
        vector_array_reference = Array.create(
            vector.symbol, vector_dims)
        # Create "matrix(i,j)"
        array_dims = [Reference(i_loop_symbol), Reference(j_loop_symbol)]
        if len(matrix.children) > 2:
            # Add any additional dimensions (in case of an array slice)
            array_dims.extend(matrix.children[2:])
        matrix_array_reference = Array.create(matrix.symbol, array_dims)
        # Create "matrix(i,j) * vector(j)"
        multiply = BinaryOperation.create(
            BinaryOperation.Operator.MUL, matrix_array_reference,
            vector_array_reference)
        # Create "result(i) + matrix(i,j) * vector(j)"
        rhs = BinaryOperation.create(
            BinaryOperation.Operator.ADD, result, multiply)
        # Create "result(i) = result(i) + matrix(i,j) * vector(j)"
        assign = Assignment.create(result, rhs)
        # Create j loop and add the above code as a child
        # Work out the bounds
        lower_bound, upper_bound, step = _get_array_bound(vector, 0)
        jloop = Loop.create(j_loop_name, lower_bound, upper_bound, step,
                            [assign])
        # Create "result(i) = 0.0"
        assign = Assignment.create(result, Literal("0.0", REAL_TYPE))
        # Create i loop and add assigment and j loop as children
        lower_bound, upper_bound, step = _get_array_bound(matrix, 0)
        iloop = Loop.create(
            i_loop_name, lower_bound, upper_bound, step, [assign, jloop])
        # Add the new code to the PSyIR
        iloop.parent = assignment.parent
        assignment.parent.children.insert(assignment.position, iloop)
        # remove the original matmul
        assignment.parent.children.remove(assignment)
