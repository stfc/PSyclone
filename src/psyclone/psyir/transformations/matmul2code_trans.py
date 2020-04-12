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
from psyclone.psyir.nodes import BinaryOperation, Assignment, Reference, \
    Loop, Literal, Array
from psyclone.psyir.symbols import DataType, DataSymbol
from psyclone.psyGen import Transformation
from psyclone.errors import GenerationError


class Matmul2CodeTrans(Transformation):
    '''Provides a transformation from a PSyIR MATMUL Operator node to
    equivalent code in a PSyIR tree. Validity checks are also
    performed. Currently only the vector LHS version of MATMUL is
    supported.

    If the dimensions of R, A, and B are R(N), A(N,M), B(M),
    The transformation replaces `R=MATMUL(A,B)` with the following code:

    ```loop i=1,N
           loop j=1,M
             R(i) += A(i,j) * B(j)```

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
        # i.e. ... = matvec(a,b)
        if not isinstance(node.parent, Assignment):
            raise GenerationError(
                "Matmul2CodeTrans only supports the transformation of a "
                "MATMUL operation when it is the sole operation on the rhs "
                "of an assignment.")

        matrix = node.children[0]
        vector = node.children[1]
        # The children of matvec should be arrays
        if not matrix.symbol.shape or not vector.symbol.shape:
            raise GenerationError(
                "Expected children of a MATMUL BInaryOperation to be "
                "arrays, but found '{0}', '{1}'."
                "".format(type(matrix.symbol).__name__,
                          type(vector.symbol).__name__))

        # The first child (the matrix) should be declared as an array
        # with at least 2 dimensions
        if len(matrix.symbol.shape) < 2:
            raise GenerationError(
                "Expected 1st child of a MATMUL BInaryOperation to be "
                "a matrix with at least 2 dimensions, but found '{0}'."
                "".format(len(matrix.symbol.shape)))
        if len(matrix.symbol.shape) == 2 and not matrix.children:
            # If the matrix only has 2 dimensions and all of its data is
            # used in the matrix vector multiply then the reference does
            # not need to supply any dimension information.
            pass
        else:
            # There should be one index per dimension.
            if len(matrix.symbol.shape) != len(matrix.children):
                raise GenerationError(
                    "There should be an index per dimension for array '{0}' "
                    "but there are {1} dimensions and {2} indices."
                    "".format(matrix.name, len(matrix.symbol.shape),
                              len(matrix.children)))
            # The first two indices should be ranges. This
            # transformation is currently limited to Ranges which
            # specify the full extent of the dimension.
            if not (matrix.is_full_range(0) and matrix.is_full_range(1)):
                raise GenerationError(
                    "Indices 0 and 1 of matrix '{0}' must be full ranges."
                    "".format(matrix.name))

            if len(matrix.children) > 2:
                # The 3rd index and onwards must be references.
                for (count, index) in enumerate(matrix.children[2:]):
                    if not isinstance(index, Reference):
                        raise GenerationError(
                            "Index {0} should be a reference but found {1}."
                            "".format(count+3, type(index).__name__))

        array = node.children[1]
        if isinstance(array, Array) and not len(array.children) == 1:
            raise GenerationError(
                "Matmul2CodeTrans only supports the matrix vector form of "
                "matmul which requires the 2nd argument to be a "
                "one-dimensional array, but found {0} dimensions."
                "".format(len(array.children)))

        if len(vector.symbol.shape) == 1 and not vector.children:
            # If the vector only has 1 dimension and all of its data is
            # used in the matrix vector multiply then the reference does
            # not need to supply any dimension information.
            pass
        else:
            # There should be one index per dimension.
            if len(vector.symbol.shape) != len(vector.children):
                raise GenerationError(
                    "There should be an index per dimension for vector '{0}' "
                    "but there are {1} dimensions and {2} indices."
                    "".format(vector.name, len(vector.symbol.shape),
                              len(vector.children)))
            # The first index should be a range. This
            # transformation is currently limited to Ranges which
            # specify the full extent of the dimension.
            if not vector.is_full_range(0):
                raise GenerationError(
                    "Index 0 of vector '{0}' must be a full range."
                    "".format(matrix.name))
            if len(vector.children) > 1:
                # The 2nd index and onwards must be references.
                for (count, index) in enumerate(vector.children[1:]):
                    if not isinstance(index, Reference):
                        raise GenerationError(
                            "Index {0} should be a reference but found {1}."
                            "".format(count+2, type(index).__name__))

    def apply(self, node, options=None):
        '''Apply the MATMUL intrinsic conversion transformation to the
        specified node. This node must be a MATMUL
        BinaryOperation. Currently only the vector LHS version of
        MATMUL is supported.  BinaryOperation is converted to the
        following equivalent inline code:

        R=MATMUL(A,B)
        to:

        ```loop i=1,N
             R(i) = 0.0
             loop j=1,M
               R(i) = R(i) + A(i,j) * B(j)```

        :param node: a MATMUL Binary-Operation node.
        :type node: :py:class:`psyclone.psyGen.BinaryOperation`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        '''
        self.validate(node)

        assignment = node.parent

        matrix = node.children[0]
        matrix_bound = matrix.symbol.shape[0]

        vector = node.children[1]
        vector_bound = vector.symbol.shape[0]

        result_symbol = node.parent.lhs.symbol

        # Create new i and j loop iterators
        symbol_table = node.find_symbol_table()
        i_loop_name = symbol_table.new_symbol_name("i")
        i_loop_symbol = DataSymbol(i_loop_name, DataType.INTEGER)
        symbol_table.add(i_loop_symbol)
        j_loop_name = symbol_table.new_symbol_name("j")
        j_loop_symbol = DataSymbol(j_loop_name, DataType.INTEGER)
        symbol_table.add(j_loop_symbol)

        # Create "result(i)"
        result = Array.create(result_symbol, [Reference(i_loop_symbol)])
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
            Reference(vector_bound), Literal("1", DataType.INTEGER), [assign])
        # Create "result(i) = 0.0"
        assign = Assignment.create(result, Literal("0.0", DataType.REAL))
        # Create i loop and add assigment and j loop as children
        iloop = Loop.create(
            i_loop_name, Literal("1", DataType.INTEGER),
            Reference(matrix_bound), Literal("1", DataType.INTEGER),
            [assign, jloop])
        # Add the new code to the PSyIR
        iloop.parent = assignment.parent
        assignment.parent.children.insert(assignment.position, iloop)
        # remove the original matmul
        assignment.parent.children.remove(assignment)
