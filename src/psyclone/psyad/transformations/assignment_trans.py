# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''This module contains a transformation that replaces a PSyIR
assignment node with its adjoint form.

'''
from __future__ import absolute_import

from psyclone.psyir.nodes import BinaryOperation, Assignment, Reference, \
    Literal, Node, Operation, UnaryOperation
from psyclone.psyir.symbols import DataSymbol, REAL_TYPE
from psyclone.psyir.transformations import TransformationError

from psyclone.psyad.transformations import AdjointTransformation, \
    TangentLinearError


class AssignmentTrans(AdjointTransformation):
    ''' xxx '''

    def apply(self, node, options=None):
        '''Apply the Assignment transformation to the specified node. The node
        must be a valid tangent linear assignment . The assignment is
        replaced with its adjoint version.

        :param node: an Assignment node.
        :type node: :py:class:`psyclone.psyir.nodes.Assignment`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        '''
        self.validate(node)

        parent = node.parent

        # Split the RHS of the assignment into <term> +- <term> +- ...
        rhs_terms, rhs_operators = self._split_nodes(
            node.rhs, [BinaryOperation.Operator.ADD,
                       BinaryOperation.Operator.SUB])

        #print ("****")
        #for rhs_term in rhs_terms:
        #    print (self._writer(rhs_term))
        #print ("****")
        #for op in rhs_operators:
        #    print (op)
        #print ("****")

        pure_inc = False
        deferred_inc = []
        # For each term
        for idx, rhs_term in enumerate(rhs_terms):
            # Split the term into <active_variable> */ <expr>.
            active_var, operator, expr = self._split_active_var(rhs_term)

            #if active_var is None:
            #    print ("ACTIVE_VAR is NONE")
            #else:
            #    print (self._writer(active_var))
            #print (operator)
            #if expr is None:
            #    print ("EXPR is NONE")
            #else:
            #    print (type(expr).__name__)
            #    print (self._writer(expr))

            if not active_var:
                # This is an expression without an active
                # variable. There is therefore nothing to output.
                continue

            if active_var.name.lower() == node.lhs.name.lower():
                # This is an increment. We need to defer any output as
                # other terms must be completed before the LHS TL
                # active variable is modified. There may be multiple
                # increments on the rhs. The solution is the following:
                # A = A + xA - A/y => A = A * (1 + x - 1/y). At this
                # point we gather all the RHS terms (1, x, 1/y) and
                # save them.

                # TODO WORK WITH NEGATIVE VALUES. ONLY WORKS WITH + AT THE MOMENT
                if expr:
                    if operator == BinaryOperation.Operator.DIV:
                        # DIV so store 1.0/expr
                        tmp = BinaryOperation.create(operator, Literal("1.0", REAL_TYPE), expr.copy())
                        deferred_inc.append(tmp)
                    else:
                        # MUL so store expr
                        deferred_inc.append(expr.copy())
                else:
                    pure_inc = True
                    # A = A so store 1.0
                    deferred_inc.append(Literal("1.0", REAL_TYPE))
                continue

            # Adjoint is active_var = active_var +- lhs_active_var */ expr
            if expr:
                tmp = BinaryOperation.create(operator, node.lhs.copy(), expr.copy())
            else:
                tmp = node.lhs.copy()
            if idx == 0:
                rhs_operator = BinaryOperation.Operator.ADD
            else:
                rhs_operator = rhs_operators[idx-1]
            rhs = BinaryOperation.create(rhs_operator, active_var.copy(), tmp)
            assignment = Assignment.create(active_var.copy(), rhs)
            node.parent.children.insert(node.position, assignment)

        if deferred_inc:
            if pure_inc and len(deferred_inc) == 1:
                # No need to output anything as it is A = A
                pass
            else:
                # Output the deferred increment assignment.
                add = None
                for expr in deferred_inc:
                    if not add:
                        add = expr.copy()
                    else:
                        add = BinaryOperation.create(BinaryOperation.Operator.ADD, add, expr.copy())
                rhs = BinaryOperation.create(BinaryOperation.Operator.MUL, node.lhs.copy(), add)
                assignment = Assignment.create(node.lhs.copy(), rhs)
                node.parent.children.insert(node.position, assignment)
        else:
            # The assignment is not an increment. The LHS active
            # variable needs to be zero'ed.
            assignment = Assignment.create(node.lhs.copy(), Literal("0.0", REAL_TYPE))
            node.parent.children.insert(node.position, assignment)

        # Remove the original node
        node.detach()

    def validate(self, node, options=None):
        '''Perform various checks to ensure that it is valid to apply the
        AssignmentTrans transformation to the supplied PSyIR Node.

        :param node: the node that is being checked.
        :type node: :py:class:`psyclone.psyir.nodes.Assignment`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the node argument is not an \
            Assignment.
        :raises TangentLinearError: if the assignment does not conform \
            to the required tangent linear structure.

        '''
        # Check node argument is an assignment node
        if not isinstance(node, Assignment):
            raise TransformationError(
                "Node argument in assignment transformation should be a PSyIR "
                "Assignment, but found '{0}'.".format(type(node).__name__))

        # If there are no active variables then return
        assignment_active_vars = [
            var.name.lower() for var in node.walk(Reference)
            if var.name.lower() in self._active_variables]
        if not assignment_active_vars:
            # No active variables in this assigment so the assignment
            # remains unchanged.
            return

        # The lhs of the assignment node should be an active variable
        if not node.lhs.name.lower() in self._active_variables:
            # There are active vars on RHS but not on LHS
            raise TangentLinearError(
                "Assignment node '{0}' has the following active variables on "
                "its RHS '{1}' but its LHS '{2}' is not an active variable."
                "".format(self._writer(node), assignment_active_vars,
                          node.lhs.name))

        # Split the RHS of the assignment into <expr> +- <expr> +- <expr>
        rhs_terms, rhs_operators = self._split_nodes(
            node.rhs, [BinaryOperation.Operator.ADD,
                       BinaryOperation.Operator.SUB])

        # Check each expression term. It must be in the form
        # A */ <expr> where A is an active variable.
        for rhs_term in rhs_terms:

            active_vars = [
                ref for ref in rhs_term.walk(Reference) if ref.name.lower()
                in self._active_variables]

            if not active_vars:
                # This term must contain an active variable
                raise TangentLinearError(
                    "Each term on the RHS of the assigment '{0}' must have an "
                    "active variable but '{1}' does not."
                    "".format(self._writer(node), self._writer(rhs_term)))

            if len(active_vars) > 1:
                # This term can only contain one active variable
                raise TangentLinearError(
                    "Each term on the RHS of the assigment '{0}' must not "
                    "have more than one active variable but '{1}' has {2}."
                    "".format(self._writer(node), self._writer(rhs_term),
                              len(active_vars)))

            if isinstance(rhs_term, Reference) and rhs_term.name.lower() \
               in self._active_variables:
                # This term is a single active variable and is therefore valid
                continue

            # Split the term into <expr> */ <expr> */ <expr>
            expr_terms, expr_operators = self._split_nodes(
                rhs_term, [BinaryOperation.Operator.MUL,
                           BinaryOperation.Operator.DIV])

            # One of expression terms must be an active variable
            found = False
            for index, expr_term in enumerate(expr_terms):
                if (isinstance(expr_term, Reference) and
                        expr_term.name.lower() in self._active_variables):
                    found = True
                    break
            if not found:
                raise TangentLinearError(
                    "Each term on the RHS of the assignment '{0}' must be an "
                    "active variable multiplied or divided by an expression, "
                    "but found '{1}'.".format(
                        self._writer(node), self._writer(rhs_term)))

            # The active variable must not be a divisor
            if expr_operators[index-1] == BinaryOperation.Operator.DIV:
                raise TangentLinearError(
                    "A term on the RHS of the assignment '{0}' with a "
                    "division must not have the active variable as a divisor "
                    "but found '{1}'.".format(
                        self._writer(node), self._writer(rhs_term)))
                
            # All terms must be Reference or operator
            for tmp_node in rhs_term.walk(Node):
                if not isinstance(tmp_node, (Reference, Operation)):
                    raise TangentLinearError(
                        "A term on the RHS of the assignment '{0}' contains "
                        "an unsupported node type '{1}' ({2}) in '{3}'."
                        "".format(
                            self._writer(node), type(tmp_node).__name__,
                            self._writer(tmp_node), self._writer(rhs_term)))

    def _split_nodes(self, node, binary_operator_list):
        '''Utility to split an expression into a series of sub-expressions
        separated by one of the binary operators specified in binary_operator_list.

        :param node: xxx
        :type node: xxx
        :param binary_operator_list: xxx
        :type binary_operator_list: list of xxx

        :returns: xxx
        :rtype: xxx

        '''
        if (isinstance(node, BinaryOperation)) and \
           (node.operator in binary_operator_list):
            lhs_node_list, lhs_op_list = self._split_nodes(
                node.children[0], binary_operator_list)
            rhs_node_list, rhs_op_list = self._split_nodes(
                node.children[1], binary_operator_list)
            return(lhs_node_list + rhs_node_list, lhs_op_list + [node.operator] + rhs_op_list)
        else:
            return ([node], [])

    def _split_active_var(self, term):
        '''Utility to split the term into an active variable, an expression
        and an operator that acts on the two. As validation has been
        performed we can assume that there is at most one active
        variable in the term and that it will be a sub-term i.e. when
        the term is split into sub-terms separated by * or / it will
        be one of the sub-terms.

        :param term: xxx
        :type term: yyy

        :returns: tuple containing the active variable or None if
            there is no active variable, the operator that combines
            the active variable with an expression or None if there is
            no operator and the expression or None if there is no
            expression.

        :rtype: (x or NoneType, y or NoneType, z or NoneType)

        '''
        # Split term into <sub-term> */ <sub-term> */ <sub-term> */ ...
        sub_term_exprs, sub_term_operators = self._split_nodes(
            term, [BinaryOperation.Operator.MUL, BinaryOperation.Operator.DIV])

        # Default to no information
        term_active_var = None
        term_operator = None
        term_expr = None

        # For each sub-term
        for idx, sub_term_expr in enumerate(sub_term_exprs):
            #print ("++++++++++")
            #print (self._writer(sub_term_expr))
            #print (type(sub_term_expr))
            #print (isinstance(sub_term_expr, UnaryOperation))
            #print (sub_term_expr.operator in [UnaryOperation.Operator.MINUS, UnaryOperation.Operator.PLUS])
            #print ("++++++++++")
            negate_active_var = False
            # Sort out any unary + or - associated with an active variable
            if isinstance(sub_term_expr, UnaryOperation) and sub_term_expr.operator in [UnaryOperation.Operator.MINUS, UnaryOperation.Operator.PLUS] and isinstance(sub_term_expr.children[0], Reference) and sub_term_expr.children[0].symbol in self._active_variables:
                if sub_term_expr.operator == UnaryOperation.Operator.MINUS:
                    #print ("NEGATE!")
                    negate_active_var = True
                # Remove unary operator
                sub_term_expr = sub_term_expr.children[0]
            if isinstance(sub_term_expr, Reference) and sub_term_expr.symbol in self._active_variables:
                # Active variable
                term_active_var = sub_term_expr.copy()
                if idx == 0:
                    if sub_term_operators:
                        term_operator = sub_term_operators[idx]
                else:
                    term_operator = sub_term_operators[idx-1]
            else:
                if idx == 0 or term_expr is None:
                    term_expr = sub_term_expr
                else:
                    term_expr = BinaryOperation.create(sub_term_operators[idx-1], term_expr.copy(), sub_term_expr.copy())
            if negate_active_var:
                if term_expr:
                    term_expr = BinaryOperator.create(BinaryOperator.Operator.MUL, Literal("-1.0", REAL_TYPE), term_exr)
                else:
                    term_expr = Literal("-1.0", REAL_TYPE)
                    term_operator = BinaryOperation.Operator.MUL
                          
        return (term_active_var, term_operator, term_expr)

    def __str__(self):
        return "Convert a PSyIR Assignment to its adjoint form"

    @property
    def name(self):
        '''
        :returns: the name of the transformation as a string.
        :rtype: str

        '''
        return type(self).__name__


__all__ = ["AssignmentTrans"]
