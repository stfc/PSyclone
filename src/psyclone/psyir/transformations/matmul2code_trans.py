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

'''Module providing a NEMO-API-specific transformation from a PSyIR
MATMUL operator to PSyIR code. This could be useful if the
MATMULoperator is not supported by the back-end or if the performance
in the inline code is better than the intrinsic. MATMUL supports both
matrix multiply and matrix vector multiply. At the moment this
transformation is limited to matrix vector multiply.

'''
from __future__ import absolute_import
from psyclone.psyir.nodes import BinaryOperation, Assignment, Reference, \
    Loop, Literal, Array, Range
from psyclone.psyir.symbols import DataType, DataSymbol
from psyclone.psyGen import Transformation
from psyclone.errors import GenerationError


class Matmul2CodeTrans(Transformation):
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
            do j=1,M
               R(i) += A(i,j) * B(j)

    '''
    def __str__(self):
        return ("Convert the PSyIR MATMUL intrinsic to equivalent PSyIR "
                "code.")

    @property
    def name(self):
        '''
        :returns: the name of the transformation as a string.
        :rtype:str

        '''
        return "Matmul2CodeTrans"

    def validate(self, node):
        '''Perform checks to ensure that it is valid to apply the
        Matmul2CodeTran transformation to the supplied node.

        :param node: the node that is being checked.
        :type node: :py:class:`psyclone.psyGen.Operation`

        :raises GenerationError: if the node argument is not the \
            expected type.
        :raises GenerationError: if the parent of the MATMUL \
            operation is not an assignment.
        :raises GenerationError: if the matmul arguments are not in \
            the required form.

        '''
        # Check the supplied argument is a matvec node
        if not isinstance(node, BinaryOperation):
            raise GenerationError(
                "The supplied node should be a BinaryOperation but "
                "found '{0}'.".format(type(node).__name__))
        if node.operator != BinaryOperation.Operator.MATMUL:
            raise GenerationError(
                "The supplied node should be a MATMUL BinaryOperation but "
                "found '{0}'.".format(node.operator))
        # Check the matmul is the only code on the rhs of an assignment
        # i.e. ... = matmul(a,b)
        if not isinstance(node.parent, Assignment):
            raise GenerationError(
                "Matmul2CodeTrans only supports the transformation of a "
                "MATMUL operation when it is the sole operation on the rhs "
                "of an assignment.")

        matrix = node.children[0]
        vector = node.children[1]
        # The children of matvec should be References
        if not (isinstance(matrix, Reference) and
                isinstance(vector, Reference)):
            raise GenerationError(
                "Expected children of a MATMUL BinaryOperation to be "
                "references, but found '{0}', '{1}'."
                "".format(type(matrix).__name__,
                          type(vector).__name__))

        # The children of matvec should be References to arrays
        if not (matrix.symbol.shape or vector.symbol.shape):
            raise GenerationError(
                "Expected children of a MATMUL BinaryOperation to be "
                "references to arrays, but found '{0}', '{1}'."
                "".format(type(matrix.symbol).__name__,
                          type(vector.symbol).__name__))

        # The first child (the matrix) should be declared as an array
        # with at least 2 dimensions
        if len(matrix.symbol.shape) < 2:
            raise GenerationError(
                "Expected 1st child of a MATMUL BinaryOperation to be "
                "a matrix with at least 2 dimensions, but found '{0}'."
                "".format(len(matrix.symbol.shape)))
        if len(matrix.symbol.shape) == 2 and not matrix.children:
            # If the matrix only has 2 dimensions and all of its data is
            # used in the matrix vector multiply then the reference does
            # not need to supply any dimension information.
            pass
        else:
            # There should be one index per dimension. This is enforced
            # by the array create method so is not tested here.

            # The first two indices should be ranges. This
            # transformation is currently limited to Ranges which
            # specify the full extent of the dimension.
            if not (matrix.is_full_range(0) and matrix.is_full_range(1)):
                raise GenerationError(
                    "To use matmul2code_trans on matmul, indices 0 and 1 of "
                    "the 1st (matrix) argument '{0}' must be full ranges."
                    "".format(matrix.name))

            if len(matrix.children) > 2:
                # The 3rd index and onwards must not be ranges.
                for (count, index) in enumerate(matrix.children[2:]):
                    if isinstance(index, Range):
                        raise GenerationError(
                            "To use matmul2code_trans on matmul, indices 2 "
                            "onwards of the first (matrix) argument should "
                            "not be Ranges but found {0} at index {1}."
                            "".format(type(index).__name__, count+2))

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
                raise GenerationError(
                    "To use matmul2code_trans on matmul, index 0 of the 2nd "
                    "(vector) argument '{0}' must be a full range."
                    "".format(matrix.name))
            if len(vector.children) > 1:
                # The 2nd index and onwards must be references.
                for (count, index) in enumerate(vector.children[1:]):
                    if isinstance(index, Range):
                        raise GenerationError(
                            "To use matmul2code_trans on matmul, indices 1 "
                            "onwards of the second (vector) argument should "
                            "not be Ranges but found {0} at index {1}."
                            "".format(type(index).__name__, count+1))

    def apply(self, node, options=None):
        '''Apply the MATMUL intrinsic conversion transformation to the
        specified node. This node must be a MATMUL
        BinaryOperation. Currently only the matrix vector version of
        MATMUL is supported. If the transformation is successful then
        an assignment which includes a MATMUL BinaryOperation node

        .. code-block:: fortran

            R = MATMUL(A,B)

        is converted to the following equivalent inline code:

        .. code-block:: fortran

            do i=1,N
                R(i) = 0.0
                    do j=1,M
                        R(i) = R(i) + A(i,j) * B(j)

        :param node: a MATMUL Binary-Operation node.
        :type node: :py:class:`psyclone.psyGen.BinaryOperation`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        '''
        self.validate(node)

        assignment = node.parent
        matrix = node.children[0]
        vector = node.children[1]
        result = node.parent.lhs
        result_symbol = result.symbol

        # Create new i and j loop iterators
        symbol_table = node.find_symbol_table()
        i_loop_name = symbol_table.new_symbol_name("i")
        i_loop_symbol = DataSymbol(i_loop_name, DataType.INTEGER)
        symbol_table.add(i_loop_symbol)
        j_loop_name = symbol_table.new_symbol_name("j")
        j_loop_symbol = DataSymbol(j_loop_name, DataType.INTEGER)
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
        jloop = Loop.create(
            j_loop_name, Literal("1", DataType.INTEGER),
            Reference(vector.symbol), Literal("1", DataType.INTEGER), [assign])
        # Create "result(i) = 0.0"
        assign = Assignment.create(result, Literal("0.0", DataType.REAL))
        # Create i loop and add assigment and j loop as children
        iloop = Loop.create(
            i_loop_name, Literal("1", DataType.INTEGER),
            Reference(matrix.symbol), Literal("1", DataType.INTEGER),
            [assign, jloop])
        # Add the new code to the PSyIR
        iloop.parent = assignment.parent
        assignment.parent.children.insert(assignment.position, iloop)
        # remove the original matmul
        assignment.parent.children.remove(assignment)
