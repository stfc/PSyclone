# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council
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
# Authors: R. W. Ford and N. Nobre, STFC Daresbury Lab

'''Module containing a class that provides functionality to transform
a PSyIR MIN or MAX operator to PSyIR code. This could be useful if the
operator is not supported by the back-end or if the performance of the
inline code is better than the intrinsic. This utility transformation
should not be called directly by the user, rather it provides
functionality that can be specialised by MIN and MAX-specific
transformations.

'''
from __future__ import absolute_import

from psyclone.psyir.nodes import BinaryOperation, NaryOperation, Assignment, \
        Reference, IfBlock
from psyclone.psyir.symbols import DataSymbol, REAL_TYPE
from psyclone.psyir.transformations.intrinsics.operator2code_trans import \
        Operator2CodeTrans


class MinOrMax2CodeTrans(Operator2CodeTrans):
    '''Provides a utility transformation from a PSyIR MIN or MAX Operator
    node to equivalent code in a PSyIR tree. Validity checks are also
    performed (by the parent class). This utility transformation is
    not designed to be called directly by the user, rather it should
    be specialised to provide MIN or MAX transformations.

    The transformation replaces

    .. code-block:: python

        R = [MIN or MAX](A, B, C ...)

    with the following logic:

    .. code-block:: python

        R = A
        if B [< or >] R:
            R = B
        if C [< or >] R:
            R = C
        ...

    '''
    def __init__(self):
        super(MinOrMax2CodeTrans, self).__init__()
        self._classes = (BinaryOperation, NaryOperation)
        self._compare_operator = None

    def apply(self, node, options=None):
        '''Apply this utility transformation to the specified node. This node
        must be a MIN or MAX BinaryOperation or NaryOperation. The
        operation is converted to equivalent inline code.  This is
        implemented as a PSyIR transform from:

        .. code-block:: python

            R = ... [MIN or MAX](A, B, C ...) ...

        to:

        .. code-block:: python

            res = A
            tmp = B
            IF tmp [< or >] res:
                res = tmp
            tmp = C
            IF tmp [< or >] res:
                res = tmp
            ...
            R = ... res ...

        where ``A``, ``B``, ``C`` ... could be arbitrarily complex
        PSyIR expressions and the ``...`` before and after ``[MIN or
        MAX](A, B, C ...)`` can be arbitrary PSyIR code.

        This transformation requires the operation node to be a
        descendent of an assignment and will raise an exception if
        this is not the case.

        :param node: a MIN or MAX Binary- or Nary-Operation node.
        :type node: :py:class:`psyclone.psyir.nodes.BinaryOperation` or \
            :py:class:`psyclone.psyir.nodes.NaryOperation`
        :param options: a dictionary with options for transformations.
        :type options: dict of str:values or None

        '''
        # pylint: disable=too-many-locals
        self.validate(node)

        symbol_table = node.scope.symbol_table
        assignment = node.ancestor(Assignment)

        # Create a temporary result variable. There is an assumption
        # here that the Operator returns a PSyIR real type. This
        # might not be what is wanted (e.g. the args might PSyIR
        # integers), or there may be errors (arguments are of
        # different types) but this can't be checked as we don't have
        # appropriate methods to query nodes (see #658).
        res_var_symbol = symbol_table.new_symbol(
            f"res_{self._operator_name.lower()}",
            symbol_type=DataSymbol, datatype=REAL_TYPE)
        # Create a temporary variable. Again there is an
        # assumption here about the datatype - please see previous
        # comment (associated issue #658).
        tmp_var_symbol = symbol_table.new_symbol(
            f"tmp_{self._operator_name.lower()}",
            symbol_type=DataSymbol, datatype=REAL_TYPE)

        # Replace operation with a temporary (res_var).
        node.replace_with(Reference(res_var_symbol))

        # res_var=A (child[0] of node)
        lhs = Reference(res_var_symbol)
        new_assignment = Assignment.create(lhs, node.children[0].detach())
        assignment.parent.children.insert(assignment.position, new_assignment)

        # For each of the remaining arguments (B,C...)
        for expression in node.pop_all_children():

            # tmp_var=(B or C or ...)
            lhs = Reference(tmp_var_symbol)
            new_assignment = Assignment.create(lhs, expression)
            assignment.parent.children.insert(assignment.position,
                                              new_assignment)

            # if_condition: tmp_var [< or >] res_var
            lhs = Reference(tmp_var_symbol)
            rhs = Reference(res_var_symbol)
            if_condition = BinaryOperation.create(
                self._compare_operator, lhs, rhs)

            # then_body: res_var=tmp_var
            lhs = Reference(res_var_symbol)
            rhs = Reference(tmp_var_symbol)
            then_body = [Assignment.create(lhs, rhs)]

            # if [if_condition] then [then_body]
            if_stmt = IfBlock.create(if_condition, then_body)
            assignment.parent.children.insert(assignment.position, if_stmt)
