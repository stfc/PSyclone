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

'''Module providing a NEMO-api-specific transformation from a PSyIR
ABS operator to PSyIR code. This could be useful if the ABS operator
is not supported by the back-end or if the performance in the inline
code is better than the intrinsic.

'''
from psyclone.undoredo import Memento
from psyclone.psyir.transformations.nemo_operator_trans import \
    NemoOperatorTrans
from psyclone.psyGen import UnaryOperation, BinaryOperation, Assignment, \
    Reference, Literal, IfBlock
from psyclone.psyir.symbols import DataType, DataSymbol


class NemoAbsTrans(NemoOperatorTrans):
    '''Provides a NEMO-api-specific transformation from a PSyIR ABS
    Operator node to equivalent code in a PSyIR tree. Validity checks
    are also performed.

    The transformation replaces `R=ABS(X)` with the following logic:

    `IF (X<0.0) R=X*-1.0 ELSE R=X`

    '''
    def __init__(self):
        super(NemoAbsTrans, self).__init__()
        self._operator_name = "ABS"
        self._class = UnaryOperation
        self._operator = UnaryOperation.Operator.ABS

    def apply(self, node, symbol_table, options=None):
        '''Apply the ABS intrinsic conversion transformation to the specified
        node. This node must be an ABS UnaryOperation. The ABS
        UnaryOperation is converted to the following equivalent inline code:

        R=ABS(X) => IF (X<0.0) R=X*-1.0 ELSE R=X

        In the PSyIR this is implemented as a transform from:

        R= ... ABS(X) ...

        to:

        tmp_abs=X
        IF (tmp_abs<0.0) res_abs=tmp_abs*-1.0 ELSE res_abs=tmp_abs
        R= ... res_abs ...

        where X could be an arbitrarily complex expression.

        A symbol table is required as the NEMO api does not currently
        contain a symbol table and one is required in order to create
        temporary variables whose names do not clash with existing
        code. This non-standard argument is also the reason why this
        transformation is currently limited to the NEMO api.

        This transformation requires the operation node to be a
        descendent of an assignment and will raise an exception if
        this is not the case.

        :param node: an ABS UnaryOperation node.
        :type node: :py:class:`psyclone.psyGen.UnaryOperation`
        :param symbol_table: the symbol table.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :returns: 2-tuple of new schedule and memento of transform.
        :rtype: (:py:class:`psyclone.nemo.NemoInvokeSchedule`, \
                 :py:class:`psyclone.undoredo.Memento`)

        '''
        self.validate(node, symbol_table)

        schedule = node.root
        memento = Memento(schedule, self, [node, symbol_table])

        oper_parent = node.parent
        assignment = node.ancestor(Assignment)
        # Create two temporary variables.
        res_var = symbol_table.new_symbol_name("res_abs")
        symbol_table.add(DataSymbol(res_var, DataType.REAL))
        tmp_var = symbol_table.new_symbol_name("tmp_abs")
        symbol_table.add(DataSymbol(tmp_var, DataType.REAL))

        # Replace operation with a temporary (res_X).
        oper_parent.children[node.position] = Reference(res_var,
                                                        parent=oper_parent)

        # tmp_var=X
        lhs = Reference(tmp_var)
        rhs = node.children[0]
        new_assignment = Assignment.create(lhs, rhs)
        new_assignment.parent = assignment.parent
        assignment.parent.children.insert(assignment.position, new_assignment)

        # if condition: tmp_var>0.0
        lhs = Reference(tmp_var)
        rhs = Literal("0.0", DataType.REAL)
        if_condition = BinaryOperation.create(BinaryOperation.Operator.GT,
                                              lhs, rhs)

        # then_body: res_var=tmp_var
        lhs = Reference(res_var)
        rhs = Reference(tmp_var)
        then_body = [Assignment.create(lhs, rhs)]

        # else_body: res_var=-1.0*tmp_var
        lhs = Reference(res_var)
        lhs_child = Reference(tmp_var)
        rhs_child = Literal("-1.0", DataType.REAL)
        rhs = BinaryOperation.create(BinaryOperation.Operator.MUL, lhs_child,
                                     rhs_child)
        else_body = [Assignment.create(lhs, rhs)]

        # if [if_condition] then [then_body] else [else_body]
        if_stmt = IfBlock.create(if_condition, then_body, else_body)
        if_stmt.parent = assignment.parent
        assignment.parent.children.insert(assignment.position, if_stmt)

        return schedule, memento
