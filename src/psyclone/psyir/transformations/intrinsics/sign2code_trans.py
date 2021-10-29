# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council
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
# Modified: A. R. Porter, STFC Daresbury Laboratory

'''Module providing a transformation from a PSyIR SIGN operator to
PSyIR code. This could be useful if the SIGN operator is not supported
by the back-end or if the performance of the inline code is better
than the intrinsic.

'''
from __future__ import absolute_import
from psyclone.psyir.transformations.intrinsics.operator2code_trans import \
    Operator2CodeTrans
from psyclone.psyir.transformations import Abs2CodeTrans
from psyclone.psyir.nodes import UnaryOperation, BinaryOperation, Assignment, \
    Reference, Literal, IfBlock
from psyclone.psyir.symbols import DataSymbol, REAL_TYPE


class Sign2CodeTrans(Operator2CodeTrans):
    '''Provides a transformation from a PSyIR SIGN Operator node to
    equivalent code in a PSyIR tree. Validity checks are also
    performed.

    The transformation replaces

    .. code-block:: python

        R = SIGN(A, B)

    with the following logic:

    .. code-block:: python

        R = ABS(A)
        if B < 0.0:
            R = R*-1.0

    i.e. the value of ``A`` with the sign of ``B``

    '''
    def __init__(self):
        super(Sign2CodeTrans, self).__init__()
        self._operator_name = "SIGN"
        self._classes = (BinaryOperation,)
        self._operators = (BinaryOperation.Operator.SIGN,)

    def apply(self, node, options=None):
        '''Apply the SIGN intrinsic conversion transformation to the specified
        node. This node must be a SIGN BinaryOperation. The SIGN
        BinaryOperation is converted to equivalent inline code. This
        is implemented as a PSyIR transform from:

        .. code-block:: python

            R = ... SIGN(A, B) ...

        to:

        .. code-block:: python

            tmp_abs = A
            if tmp_abs < 0.0:
                res_abs = tmp_abs*-1.0
            else:
                res_abs = tmp_abs
            res_sign = res_abs
            tmp_sign = B
            if tmp_sign < 0.0:
                res_sign = res_sign*-1.0
            R = ... res_sign ...

        where ``A`` and ``B`` could be arbitrarily complex PSyIR
        expressions, ``...`` could be arbitrary PSyIR code and where
        ``ABS`` has been replaced with inline code by the NemoAbsTrans
        transformation.

        This transformation requires the operation node to be a
        descendent of an assignment and will raise an exception if
        this is not the case.

        :param node: a SIGN BinaryOperation node.
        :type node: :py:class:`psyclone.psyir.nodes.BinaryOperation`
        :param symbol_table: the symbol table.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        '''
        # pylint: disable=too-many-locals
        self.validate(node)

        symbol_table = node.scope.symbol_table
        assignment = node.ancestor(Assignment)

        # Create two temporary variables.  There is an assumption here
        # that the SIGN Operator returns a PSyIR real type. This might
        # not be what is wanted (e.g. the args might PSyIR integers),
        # or there may be errors (arguments are of different types)
        # but this can't be checked as we don't have the appropriate
        # methods to query nodes (see #658).
        res_var_symbol = symbol_table.new_symbol(
            "res_sign", symbol_type=DataSymbol, datatype=REAL_TYPE)
        tmp_var_symbol = symbol_table.new_symbol(
            "tmp_sign", symbol_type=DataSymbol, datatype=REAL_TYPE)

        # Replace operator with a temporary (res_var).
        node.replace_with(Reference(res_var_symbol))

        # Extract the operand nodes
        op1, op2 = node.pop_all_children()

        # res_var=ABS(A)
        lhs = Reference(res_var_symbol)
        rhs = UnaryOperation.create(UnaryOperation.Operator.ABS, op1)
        new_assignment = Assignment.create(lhs, rhs)
        assignment.parent.children.insert(assignment.position, new_assignment)

        # Replace the ABS intrinsic with inline code.
        abs_trans = Abs2CodeTrans()
        abs_trans.apply(rhs, symbol_table)

        # tmp_var=B
        lhs = Reference(tmp_var_symbol)
        new_assignment = Assignment.create(lhs, op2)
        assignment.parent.children.insert(assignment.position, new_assignment)

        # if_condition: tmp_var<0.0
        lhs = Reference(tmp_var_symbol)
        rhs = Literal("0.0", REAL_TYPE)
        if_condition = BinaryOperation.create(BinaryOperation.Operator.LT,
                                              lhs, rhs)

        # then_body: res_var=res_var*-1.0
        lhs = Reference(res_var_symbol)
        lhs_child = Reference(res_var_symbol)
        rhs_child = Literal("-1.0", REAL_TYPE)
        rhs = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                     lhs_child, rhs_child)
        then_body = [Assignment.create(lhs, rhs)]

        # if [if_condition] then [then_body]
        if_stmt = IfBlock.create(if_condition, then_body)
        assignment.parent.children.insert(assignment.position, if_stmt)
