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
# Author: R. W. Ford, STFC Daresbury Laboratory
# Modified: A. R. Porter, STFC Daresbury Laboratory

'''Module providing a transformation from a PSyIR MIN operator to
PSyIR code. This could be useful if the MIN operator is not supported
by the back-end or if the performance in the inline code is better
than the intrinsic.

'''
from __future__ import absolute_import
from psyclone.psyir.transformations.intrinsics.operator2code_trans import \
        Operator2CodeTrans
from psyclone.psyir.nodes import BinaryOperation, NaryOperation, Assignment, \
        Reference, IfBlock
from psyclone.psyir.symbols import DataSymbol, REAL_TYPE


class Min2CodeTrans(Operator2CodeTrans):
    '''Provides a transformation from a PSyIR MIN Operator node to
    equivalent code in a PSyIR tree. Validity checks are also
    performed.

    The transformation replaces

    .. code-block:: python

        R = MIN(A, B, C ...)

    with the following logic:

    .. code-block:: python

        R = A
        if B < R:
            R = B
        if C < R:
            R = C
        ...

    '''
    def __init__(self):
        super(Min2CodeTrans, self).__init__()
        self._operator_name = "MIN"
        self._classes = (BinaryOperation, NaryOperation)
        self._operators = (BinaryOperation.Operator.MIN,
                           NaryOperation.Operator.MIN)

    def apply(self, node, options=None):
        '''Apply the MIN intrinsic conversion transformation to the specified
        node. This node must be an MIN NaryOperation. The MIN
        NaryOperation is converted to equivalent inline code.  This is
        implemented as a PSyIR transform from:

        .. code-block:: python

            R = ... MIN(A, B, C ...) ...

        to:

        .. code-block:: python

            res_min = A
            tmp_min = B
            IF tmp_min < res_min:
                res_min = tmp_min
            tmp_min = C
            IF tmp_min < res_min:
                res_min = tmp_min
            ...
            R = ... res_min ...

        where ``A``, ``B``, ``C`` ... could be arbitrarily complex PSyIR
        expressions and the ``...`` before and after ``MIN(A, B, C
        ...)`` can be arbitrary PSyIR code.

        This transformation requires the operation node to be a
        descendent of an assignment and will raise an exception if
        this is not the case.

        :param node: a MIN Binary- or Nary-Operation node.
        :type node: :py:class:`psyclone.psyGen.BinaryOperation` or \
        :py:class:`psyclone.psyGen.NaryOperation`
        :param symbol_table: the symbol table.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        '''
        # pylint: disable=too-many-locals
        self.validate(node)

        schedule = node.root
        symbol_table = schedule.symbol_table

        oper_parent = node.parent
        assignment = node.ancestor(Assignment)

        # Create a temporary result variable. There is an assumption
        # here that the MIN Operator returns a PSyIR real type. This
        # might not be what is wanted (e.g. the args might PSyIR
        # integers), or there may be errors (arguments are of
        # different types) but this can't be checked as we don't have
        # appropriate methods to query nodes (see #658).
        res_var = symbol_table.new_symbol_name("res_min")
        res_var_symbol = DataSymbol(res_var, REAL_TYPE)
        symbol_table.add(res_var_symbol)
        # Create a temporary variable. Again there is an
        # assumption here about the datatype - please see previous
        # comment (associated issue #658).
        tmp_var = symbol_table.new_symbol_name("tmp_min")
        tmp_var_symbol = DataSymbol(tmp_var, REAL_TYPE)
        symbol_table.add(tmp_var_symbol)

        # Replace operation with a temporary (res_var).
        oper_parent.children[node.position] = Reference(res_var_symbol,
                                                        parent=oper_parent)

        # res_var=A
        lhs = Reference(res_var_symbol)
        new_assignment = Assignment.create(lhs, node.children[0])
        new_assignment.parent = assignment.parent
        assignment.parent.children.insert(assignment.position, new_assignment)

        # For each of the remaining min arguments (B,C...)
        for expression in node.children[1:]:

            # tmp_var=(B or C or ...)
            lhs = Reference(tmp_var_symbol)
            new_assignment = Assignment.create(lhs, expression)
            new_assignment.parent = assignment.parent
            assignment.parent.children.insert(assignment.position,
                                              new_assignment)

            # if_condition: tmp_var<res_var
            lhs = Reference(tmp_var_symbol)
            rhs = Reference(res_var_symbol)
            if_condition = BinaryOperation.create(BinaryOperation.Operator.LT,
                                                  lhs, rhs)

            # then_body: res_var=tmp_var
            lhs = Reference(res_var_symbol)
            rhs = Reference(tmp_var_symbol)
            then_body = [Assignment.create(lhs, rhs)]

            # if [if_condition] then [then_body]
            if_stmt = IfBlock.create(if_condition, then_body)
            if_stmt.parent = assignment.parent
            assignment.parent.children.insert(assignment.position, if_stmt)
