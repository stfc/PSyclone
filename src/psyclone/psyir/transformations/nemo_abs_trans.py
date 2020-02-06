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

The implementation is NEMO-specific as NEMO code generation does not
currently create a symbol table, see issue #500. Once this has been
implemented the transformation can be modified to work for all APIs.

'''
from __future__ import absolute_import
from psyclone.undoredo import Memento
from psyclone.psyir.transformations.nemo_operator_trans import \
    NemoOperatorTrans
from psyclone.psyir.nodes import UnaryOperation, BinaryOperation, Assignment, \
    Reference, Literal, IfBlock
from psyclone.psyir.symbols import DataType, DataSymbol


class NemoAbsTrans(NemoOperatorTrans):
    '''Provides a NEMO-api-specific transformation from a PSyIR ABS
    Operator node to equivalent code in a PSyIR tree. Validity checks
    are also performed.

    The transformation replaces

    .. code-block:: python

        R = ABS(X)

    with the following logic:

    .. code-block:: python

        IF X < 0.0:
            R = X*-1.0
        ELSE:
            R = X

    '''
    def __init__(self):
        super(NemoAbsTrans, self).__init__()
        self._operator_name = "ABS"
        self._classes = (UnaryOperation,)
        self._operators = (UnaryOperation.Operator.ABS,)

    def apply(self, node, symbol_table, options=None):
        '''Apply the ABS intrinsic conversion transformation to the specified
        node. This node must be an ABS UnaryOperation. The ABS
        UnaryOperation is converted to equivalent inline code. This is
        implemented as a PSyIR transform from:

        .. code-block:: python

            R = ... ABS(X) ...

        to:

        .. code-block:: python

            tmp_abs = X
            if tmp_abs < 0.0:
                res_abs = tmp_abs*-1.0
            else:
                res_abs = tmp_abs
            R = ... res_abs ...

        where ``X`` could be an arbitrarily complex PSyIR expression
        and ``...`` could be arbitrary PSyIR code.

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
        # Create two temporary variables.  There is an assumption here
        # that the ABS Operator returns a PSyIR real type. This might
        # not be what is wanted (e.g. the args might PSyIR integers),
        # or there may be errors (arguments are of different types)
        # but this can't be checked as we don't have access to a
        # symbol table (see #500) and don't have the appropriate
        # methods to query nodes (see #658).
        res_var = symbol_table.new_symbol_name("res_abs")
        symbol_res_var = DataSymbol(res_var, DataType.REAL)
        symbol_table.add(symbol_res_var)
        tmp_var = symbol_table.new_symbol_name("tmp_abs")
        symbol_tmp_var = DataSymbol(tmp_var, DataType.REAL)
        symbol_table.add(symbol_tmp_var)

        # Replace operation with a temporary (res_X).
        oper_parent.children[node.position] = Reference(symbol_res_var,
                                                        parent=oper_parent)

        # tmp_var=X
        lhs = Reference(symbol_tmp_var)
        rhs = node.children[0]
        new_assignment = Assignment.create(lhs, rhs)
        new_assignment.parent = assignment.parent
        assignment.parent.children.insert(assignment.position, new_assignment)

        # if condition: tmp_var>0.0
        lhs = Reference(symbol_tmp_var)
        rhs = Literal("0.0", DataType.REAL)
        if_condition = BinaryOperation.create(BinaryOperation.Operator.GT,
                                              lhs, rhs)

        # then_body: res_var=tmp_var
        lhs = Reference(symbol_res_var)
        rhs = Reference(symbol_tmp_var)
        then_body = [Assignment.create(lhs, rhs)]

        # else_body: res_var=-1.0*tmp_var
        lhs = Reference(symbol_res_var)
        lhs_child = Reference(symbol_tmp_var)
        rhs_child = Literal("-1.0", DataType.REAL)
        rhs = BinaryOperation.create(BinaryOperation.Operator.MUL, lhs_child,
                                     rhs_child)
        else_body = [Assignment.create(lhs, rhs)]

        # if [if_condition] then [then_body] else [else_body]
        if_stmt = IfBlock.create(if_condition, then_body, else_body)
        if_stmt.parent = assignment.parent
        assignment.parent.children.insert(assignment.position, if_stmt)

        return schedule, memento
