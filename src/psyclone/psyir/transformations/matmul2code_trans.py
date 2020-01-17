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
from psyclone.undoredo import Memento
from psyclone.psyGen import BinaryOperation, NaryOperation, Assignment, \
        Reference, IfBlock, Loop, Literal, Array
from psyclone.psyir.symbols import DataType, DataSymbol
from psyclone.psyGen import Transformation

class Matmul2CodeTrans(Transformation):
    '''Provides a transformation from a PSyIR MATMUL Operator node to
    equivalent code in a PSyIR tree. Validity checks are also
    performed. Currently only the vector LHS version of MATMUL is
    supported.

    If the dimensions of R, A, and B are R(M), A(M,L), B(L), 
    The transformation replaces `R=MATMUL(A,B)` with the following code:
    
    ```loop i=1,N
           loop j=1,M
             R(i) += A(i,j) * B(k)```

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
        pass

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
               R(i) = R(i) + A(i,j) * B(k)```

        :param node: a MATMUL Binary-Operation node.
        :type node: :py:class:`psyclone.psyGen.BinaryOperation`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :returns: 2-tuple of new schedule and memento of transform.
        :rtype: (:py:class:`psyclone.nemo.NemoInvokeSchedule`, \
                 :py:class:`psyclone.undoredo.Memento`)

        '''
        self.validate(node)

        schedule = node.root
        memento = Memento(schedule, self, node)

        oper_parent = node.parent
        assignment = node.ancestor(Assignment)

        matrix = node.children[0]
        vector = node.children[1]
        # Find nearest ancestor symbol table (#issue xxx will address this)
        current = node
        while current and not hasattr(current, "symbol_table"):
            current=current.parent
        symbol_table = current.symbol_table
        # find vectors symbol
        vector_symbol=symbol_table.lookup(vector.name)
        # find the array bounds of the vector
        vector_bound = vector_symbol.shape[0].name

        # find matrix's symbol
        matrix_symbol = symbol_table.lookup(matrix.name)
        matrix_bound = matrix_symbol.shape[0].name
        # note shape[1] should be the same as vector_bound

        result_name = node.parent.lhs.name
        # Warning: name can be unicode which causes Array.create to fail.
        result = Array.create(str(result_name), [Reference("i")])
        # Warning: name can be unicode which causes Array.create to fail.
        vector_array_reference = Array.create(str(vector.name), [Reference("j")])
        # Third dimension is a hack as we don't yet support ":" so the first 2 dims are in a code block. So it should be children[2]!!
        matrix_array_reference = Array.create(str(matrix.name), [Reference("i"), Reference("j"), matrix.children[1]])
        multiply = BinaryOperation.create(BinaryOperation.Operator.MUL, matrix_array_reference, vector_array_reference)
        rhs = BinaryOperation.create(BinaryOperation.Operator.ADD, result, multiply)
        assign = Assignment.create(result, rhs)
        jloop = Loop.create("j", Literal("1", DataType.INTEGER),
                            Reference(vector_bound), Literal("1", DataType.INTEGER),
                            [assign])
        assign = Assignment.create(result, Literal("0.0", DataType.REAL))
        iloop = Loop.create("i", Literal("1", DataType.INTEGER),
                            Reference(matrix_bound), Literal("1", DataType.INTEGER),
                            [assign, jloop])
        iloop.parent = node.parent.parent
        node.parent.parent.children.insert(node.parent.position, iloop)
        from psyclone.psyir.backend.fortran import FortranWriter
        fortran_writer = FortranWriter()
        code = fortran_writer(schedule)
        print (code)
        exit(1)

        # Create a temporary result variable. There is an assumption
        # here that the MIN Operator returns a PSyIR real type. This
        # might not be what is wanted (e.g. the args might PSyIR
        # integers), or there may be errors (arguments are of
        # different types) but this can't be checked as we don't have
        # access to a symbol table (see #500) and don't have the
        # appropriate methods to query nodes (see #658).
        res_var = symbol_table.new_symbol_name("res_min")
        symbol_table.add(DataSymbol(res_var, DataType.REAL))
        # Create a temporary variable. Again there is an
        # assumption here about the datatype - please see previous
        # comment (associated issues #500 and #658).
        tmp_var = symbol_table.new_symbol_name("tmp_min")
        symbol_table.add(DataSymbol(tmp_var, DataType.REAL))

        # Replace operation with a temporary (res_var).
        oper_parent.children[node.position] = Reference(res_var,
                                                        parent=oper_parent)

        # res_var=A
        lhs = Reference(res_var)
        new_assignment = Assignment.create(lhs, node.children[0])
        new_assignment.parent = assignment.parent
        assignment.parent.children.insert(assignment.position, new_assignment)

        # For each of the remaining min arguments (B,C...)
        for expression in node.children[1:]:

            # tmp_var=(B or C or ...)
            lhs = Reference(tmp_var)
            new_assignment = Assignment.create(lhs, expression)
            new_assignment.parent = assignment.parent
            assignment.parent.children.insert(assignment.position,
                                              new_assignment)

            # if_condition: tmp_var<res_var
            lhs = Reference(tmp_var)
            rhs = Reference(res_var)
            if_condition = BinaryOperation.create(BinaryOperation.Operator.LT,
                                                  lhs, rhs)

            # then_body: res_var=tmp_var
            lhs = Reference(res_var)
            rhs = Reference(tmp_var)
            then_body = [Assignment.create(lhs, rhs)]

            # if [if_condition] then [then_body]
            if_stmt = IfBlock.create(if_condition, then_body)
            if_stmt.parent = assignment.parent
            assignment.parent.children.insert(assignment.position, if_stmt)

        return schedule, memento
