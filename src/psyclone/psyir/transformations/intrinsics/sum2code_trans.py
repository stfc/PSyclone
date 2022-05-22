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
class Sum2CodeTrans(Transformation):
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
        super(Abs2CodeTrans, self).__init__()
        self._operator_name = "ABS"
        self._classes = (UnaryOperation,)
        self._operators = (UnaryOperation.Operator.ABS,)

    def validate(self, node):
        ''' xxx '''
        # assignment
        # rhs is sum and nothing else

    def apply(self, node, options=None):
        '''Apply the SUM intrinsic conversion transformation to the specified
        node. This node must be a SUM Operation which is converted to equivalent inline code.
        '''
        self.validate(node)

        # TODO get args 1) array, 2) dimension 3) mask

        # TODO determine dimension of array statically else raise exception
        
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

        # Create two temporary variables.  There is an assumption here
        # that the ABS Operator returns a PSyIR real type. This might
        # not be what is wanted (e.g. the args might PSyIR integers),
        # or there may be errors (arguments are of different types)
        # but this can't be checked as we don't have the appropriate
        # methods to query nodes (see #658).
        symbol_res_var = symbol_table.new_symbol(
            "res_abs", symbol_type=DataSymbol, datatype=REAL_TYPE)
        symbol_tmp_var = symbol_table.new_symbol(
            "tmp_abs", symbol_type=DataSymbol, datatype=REAL_TYPE)

        # Replace operation with a temporary (res_X).
        node.replace_with(Reference(symbol_res_var))

        # tmp_var=X
        lhs = Reference(symbol_tmp_var)
        rhs = node.children[0].detach()
        new_assignment = Assignment.create(lhs, rhs)
        assignment.parent.children.insert(assignment.position, new_assignment)

        # if condition: tmp_var>0.0
        lhs = Reference(symbol_tmp_var)
        rhs = Literal("0.0", REAL_TYPE)
        if_condition = BinaryOperation.create(BinaryOperation.Operator.GT,
                                              lhs, rhs)

        # then_body: res_var=tmp_var
        lhs = Reference(symbol_res_var)
        rhs = Reference(symbol_tmp_var)
        then_body = [Assignment.create(lhs, rhs)]

        # else_body: res_var=-1.0*tmp_var
        lhs = Reference(symbol_res_var)
        lhs_child = Reference(symbol_tmp_var)
        rhs_child = Literal("-1.0", REAL_TYPE)
        rhs = BinaryOperation.create(BinaryOperation.Operator.MUL, lhs_child,
                                     rhs_child)
        else_body = [Assignment.create(lhs, rhs)]

        # if [if_condition] then [then_body] else [else_body]
        if_stmt = IfBlock.create(if_condition, then_body, else_body)
        assignment.parent.children.insert(assignment.position, if_stmt)
