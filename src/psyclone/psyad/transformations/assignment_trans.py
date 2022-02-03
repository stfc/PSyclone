# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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
# Modified by J. Henrichs, Bureau of Meteorology

'''This module contains a transformation that replaces a PSyIR
assignment node with its adjoint form.

'''
from __future__ import absolute_import

from psyclone.core import SymbolicMaths
from psyclone.psyir.nodes import BinaryOperation, Assignment, Reference, \
    Literal, UnaryOperation
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.symbols import REAL_TYPE
from psyclone.psyir.transformations import TransformationError

from psyclone.psyad.transformations import TangentLinearError
from psyclone.psyad.transformations.adjoint_trans import AdjointTransformation

# pylint: disable=too-many-locals
# pylint: disable=too-many-branches


class AssignmentTrans(AdjointTransformation):
    '''Implements a transformation to translate a Tangent-Linear
    assignment to its Adjoint form.

    '''
    def apply(self, node, options=None):
        '''Apply the Assignment transformation to the specified node. The node
        must be a valid tangent-linear assignment. The assignment is
        replaced with its adjoint version.

        :param node: an Assignment node.
        :type node: :py:class:`psyclone.psyir.nodes.Assignment`
        :param options: a dictionary with options for transformations.
        :type options: dict of string:values or None

        '''
        self.validate(node)

        # Split the RHS of the assignment into <term> +- <term> +- ...
        rhs_terms = self._split_nodes(
            node.rhs, [BinaryOperation.Operator.ADD,
                       BinaryOperation.Operator.SUB])

        deferred_inc = []
        sym_maths = SymbolicMaths.get()
        # For each term
        for rhs_term in rhs_terms:

            # Find the active var in rhs_term if one exists (we may
            # find 0.0), storing it in 'active_var' and if so replace
            # it with lhs_active_var storing the modified term in
            # 'new_rhs_term'. Also determine whether this is an
            # increment, storing the result in 'increment'.
            increment = False
            active_var = None
            new_rhs_term = rhs_term.copy()
            for ref in new_rhs_term.walk(Reference):
                if ref.symbol not in self._active_variables:
                    continue
                active_var = ref
                # Identify whether this reference on the RHS matches the
                # one on the LHS - if so we have an increment.
                if node.is_array_range and isinstance(ref, ArrayMixin):
                    # TODO #1537 - we can't just do `sym_maths.equal` if we
                    # have an array range because the SymbolicMaths class does
                    # not currently support them.
                    # Since we have already checked (in validate) that any
                    # references to the same symbol on the RHS have the same
                    # range, this is an increment if the symbols match.
                    if node.lhs.symbol is ref.symbol:
                        increment = True
                else:
                    if sym_maths.equal(ref, node.lhs):
                        increment = True
                if ref.parent:
                    ref.replace_with(node.lhs.copy())
                else:
                    new_rhs_term = node.lhs.copy()
                break

            # Work out whether the binary operation for this term is a
            # '+' or a '-' and return it in 'rhs_operator'.
            rhs_operator = BinaryOperation.Operator.ADD
            previous = rhs_term
            candidate = rhs_term.parent
            while not isinstance(candidate, Assignment):
                if (isinstance(candidate, BinaryOperation) and
                    candidate.operator == BinaryOperation.Operator.SUB and
                        candidate.children[1] is previous):
                    # Rules: + + -> +; - - -> +; + - -> -; - + -> -
                    # If the higher level op is + then there is no
                    # change to the existing op. If it is - then
                    # we flip the op i.e. - => + and + => -.
                    if rhs_operator == BinaryOperation.Operator.SUB:
                        rhs_operator = BinaryOperation.Operator.ADD
                    else:
                        rhs_operator = BinaryOperation.Operator.SUB
                previous = candidate
                candidate = candidate.parent

            if not active_var:
                # This is an expression without an active variable
                # (which must be 0.0, otherwise validation will have
                # rejected it). There is therefore nothing to output.
                continue

            if increment:
                # The output of an increment needs to be deferred as
                # other terms must be completed before the LHS TL
                # active variable is modified. Save the rhs term
                # and its associated operator.
                deferred_inc.append((new_rhs_term, rhs_operator))
            else:
                # Output the adjoint for this term
                rhs = BinaryOperation.create(
                    rhs_operator, active_var.copy(), new_rhs_term)
                assignment = Assignment.create(active_var.copy(), rhs)
                node.parent.children.insert(node.position, assignment)

        if (len(deferred_inc) == 1 and
                isinstance(deferred_inc[0][0], Reference)):
            # No need to output anything as the adjoint is A = A.
            pass
        elif deferred_inc:
            # Output the adjoint for all increment terms in a single line.
            rhs, _ = deferred_inc.pop(0)
            for term, operator in deferred_inc:
                rhs = BinaryOperation.create(operator, rhs, term)
            assignment = Assignment.create(node.lhs.copy(), rhs)
            node.parent.children.insert(node.position, assignment)
        else:
            # The assignment is not an increment. The LHS active
            # variable needs to be zero'ed.
            assignment = Assignment.create(
                node.lhs.copy(), Literal("0.0", REAL_TYPE))
            node.parent.children.insert(node.position, assignment)

        # Remove the original node
        node.detach()

    def _array_ranges_match(self, assign, active_variable):
        '''
        If the supplied assignment is to an array range and the supplied
        active variable is the entity being assigned to then this routine
        checks that the array ranges of the LHS and the supplied reference
        match. If they do not then an exception is raised.

        :param assign: the assignment that we are checking.
        :type assign: :py:class:`psyclone.psyir.nodes.Assignment`
        :param active_variable: an active variable that appears on the \
            LHS and RHS of the supplied assignment.
        :type active_variable: :py:class:`psyclone.psyir.nodes.Reference`

        :raises TangentLinearError: if the supplied assignment is to a \
            symbol with an array range but the same symbol occurs on the \
            RHS without an array range.
        :raises NotImplementedError: if the array ranges on the LHS and \
            RHS of the assignment for the supplied variable do not match.

        '''
        # This check only needs to proceed if the assignment is to an array
        # range and the supplied active variable is the one being assigned to.
        if not (assign.is_array_range and active_variable.symbol is
                assign.lhs.symbol):
            return

        if not isinstance(active_variable, ArrayMixin):
            raise TangentLinearError(
                f"Assignment is to an array range but found a "
                f"reference to the LHS variable "
                f"'{assign.lhs.symbol.name}' without array notation"
                f" on the RHS: '{self._writer(assign)}'")

        sym_maths = SymbolicMaths.get()

        for pos, idx in enumerate(active_variable.indices):
            lhs_idx = assign.lhs.indices[pos]
            # TODO #1537. This is a workaround until the SymbolicMaths
            # class supports the comparison of array ranges.
            # pylint: disable=unidiomatic-typecheck
            if not (type(idx) == type(lhs_idx) and
                    sym_maths.equal(idx.start,
                                    lhs_idx.start) and
                    sym_maths.equal(idx.stop,
                                    lhs_idx.stop) and
                    sym_maths.equal(idx.step,
                                    lhs_idx.step)):
                raise NotImplementedError(
                    f"Different sections of the same active array "
                    f"'{assign.lhs.symbol.name}' are "
                    f"accessed on the LHS and RHS of an assignment: "
                    f"'{self._writer(assign)}'. This is not supported.")

    def validate(self, node, options=None):
        '''Perform various checks to ensure that it is valid to apply the
        AssignmentTrans transformation to the supplied PSyIR Node.

        :param node: the node that is being checked.
        :type node: :py:class:`psyclone.psyir.nodes.Assignment`
        :param options: a dictionary with options for transformations.
        :type options: dict of string:values or None

        :raises TransformationError: if the node argument is not an \
            Assignment.
        :raises TangentLinearError: if the assignment does not conform \
            to the required tangent-linear structure.

        '''
        # Check node argument is an assignment node
        if not isinstance(node, Assignment):
            raise TransformationError(
                "Node argument in assignment transformation should be a PSyIR "
                "Assignment, but found '{0}'.".format(type(node).__name__))
        assign = node

        # If there are no active variables then return
        assignment_active_var_names = [
            var.name for var in assign.walk(Reference)
            if var.symbol in self._active_variables]
        if not assignment_active_var_names:
            # No active variables in this assigment so the assignment
            # remains unchanged.
            return

        # The lhs of the assignment node should be an active variable
        if assign.lhs.symbol not in self._active_variables:
            # There are active vars on RHS but not on LHS
            raise TangentLinearError(
                "Assignment node '{0}' has the following active variables on "
                "its RHS '{1}' but its LHS '{2}' is not an active variable."
                "".format(self._writer(assign), assignment_active_var_names,
                          assign.lhs.name))

        # Split the RHS of the assignment into <expr> +- <expr> +- <expr>
        rhs_terms = self._split_nodes(
            assign.rhs, [BinaryOperation.Operator.ADD,
                         BinaryOperation.Operator.SUB])

        # Check for the special case where RHS=0.0. This is really a
        # representation of multiplying an active variable by zero but
        # this is obviously not visible in the code. Use 'float' to
        # normalise different representations of 0.
        if (len(rhs_terms) == 1 and isinstance(rhs_terms[0], Literal) and
                float(rhs_terms[0].value) == 0.0):
            return

        # Check each expression term. It must be in the form
        # A */ <expr> where A is an active variable.
        for rhs_term in rhs_terms:

            # When searching for references to an active variable we must
            # take care to exclude those cases where they are present as
            # arguments to the L/UBOUND intrinsics (as they will be when
            # array notation is used).
            active_vars = []
            lu_bound_ops = [BinaryOperation.Operator.LBOUND,
                            BinaryOperation.Operator.UBOUND]
            for ref in rhs_term.walk(Reference):
                if (ref.symbol in self._active_variables and
                        not (isinstance(ref.parent, BinaryOperation) and
                             ref.parent.operator in lu_bound_ops)):
                    active_vars.append(ref)

            if not active_vars:
                # This term must contain an active variable
                raise TangentLinearError(
                    "Each non-zero term on the RHS of the assigment '{0}' "
                    "must have an active variable but '{1}' does not."
                    "".format(self._writer(assign), self._writer(rhs_term)))

            if len(active_vars) > 1:
                # This term can only contain one active variable
                raise TangentLinearError(
                    "Each term on the RHS of the assigment '{0}' must not "
                    "have more than one active variable but '{1}' has {2}."
                    "".format(self._writer(assign), self._writer(rhs_term),
                              len(active_vars)))

            if (isinstance(rhs_term, Reference) and rhs_term.symbol
                    in self._active_variables):
                self._array_ranges_match(assign, rhs_term)
                # This term consists of a single active variable (with
                # a multiplier of unity) and is therefore valid.
                continue

            # Split the term into <expr> */ <expr> */ <expr>
            expr_terms = self._split_nodes(
                rhs_term, [BinaryOperation.Operator.MUL,
                           BinaryOperation.Operator.DIV])

            # One of the expression terms must be an active variable
            # or an active variable with a preceding + or -.
            for expr_term in expr_terms:
                check_term = expr_term
                if (isinstance(expr_term, UnaryOperation) and
                    expr_term.operator in [UnaryOperation.Operator.PLUS,
                                           UnaryOperation.Operator.MINUS]):
                    check_term = expr_term.children[0]
                if (isinstance(check_term, Reference) and
                        check_term.symbol in self._active_variables):
                    active_variable = check_term
                    break
            else:
                raise TangentLinearError(
                    "Each term on the RHS of the assignment '{0}' must "
                    "be linear with respect to the active variable, but "
                    "found '{1}'.".format(
                        self._writer(assign), self._writer(rhs_term)))

            # The term must be a product of an active variable with an
            # inactive expression. Check that the active variable does
            # not appear in a denominator.
            candidate = active_variable
            parent = candidate.parent
            while not isinstance(parent, Assignment):
                # Starting with the active variable reference, look up
                # the tree for an ancestor divide operation until
                # reaching the assignment node.
                if (isinstance(parent, BinaryOperation) and
                    parent.operator == BinaryOperation.Operator.DIV and
                        parent.children[1] is candidate):
                    # Found a divide and the active variable is on its RHS
                    raise TangentLinearError(
                        "In tangent-linear code an active variable cannot "
                        "appear as a denominator but '{0}' was found in "
                        "'{1}'.".format(
                            self._writer(rhs_term), self._writer(assign)))
                # Continue up the PSyIR tree
                candidate = parent
                parent = candidate.parent

            # If the LHS of the assignment is an array range then we only
            # support accesses of the same variable on the RHS if they have
            # the same range.
            self._array_ranges_match(assign, active_variable)

    @staticmethod
    def _split_nodes(node, binary_operator_list):
        '''Utility to split an expression into a series of sub-expressions
        separated by one of the binary operators specified in
        binary_operator_list.

        :param node: the node containing the expression to split.
        :type node: :py:class:`psyclone.psyir.nodes.DataNode`
        :param binary_operator_list: list of binary operators.
        :type binary_operator_list: list of
            :py:class:`psyclone.psyir.nodes.BinaryOperations.Operator`

        :returns: a list of sub-expressions.
        :rtype: list of :py:class:`psyclone.psyir.nodes.DataNode`

        '''
        if (isinstance(node, BinaryOperation)) and \
           (node.operator in binary_operator_list):
            lhs_node_list = AssignmentTrans._split_nodes(
                node.children[0], binary_operator_list)
            rhs_node_list = AssignmentTrans._split_nodes(
                node.children[1], binary_operator_list)
            return lhs_node_list + rhs_node_list
        return [node]

    def __str__(self):
        return "Convert a tangent-linear PSyIR Assignment to its adjoint form"

    @property
    def name(self):
        '''
        :returns: the name of the transformation as a string.
        :rtype: str

        '''
        return type(self).__name__


# =============================================================================
# Documentation utils: The list of module members that we wish AutoAPI to
# generate documentation for (see https://psyclone-ref.readthedocs.io).
__all__ = ["AssignmentTrans"]
