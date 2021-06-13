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
    Literal
from psyclone.psyir.symbols import DataSymbol, REAL_TYPE
from psyclone.psyir.transformations import TransformationError

from psyclone.psyad.transformations import AdjointTransformation, \
    TangentLinearError


class AssignmentTrans(AdjointTransformation):
    ''' xxx '''

    def _process(self, rhs_node, lhs_node):
        if (isinstance(rhs_node, BinaryOperation)) and \
           (rhs_node.operator == BinaryOperation.Operator.ADD):
            new_node_list = self._process(
                rhs_node.children[0], lhs_node)
            new_node_list.extend(
                self._process(rhs_node.children[1], lhs_node))
            return new_node_list
        else:
            if isinstance(rhs_node, Literal):
                # There is no active variable
                return []
            elif isinstance(rhs_node, BinaryOperation) and \
                 (rhs_node.operator in (BinaryOperation.Operator.MUL, BinaryOperation.Operator.DIV)):
                # Find the active variable and remove it from the constant
                constant = rhs_node.copy()
                # To ensure constant has a parent to support replacement.
                for node in constant.walk(Reference):
                    if node.symbol in self._active_variables:
                        active_var = node.copy()
                        mult = node.parent
                        if mult.children[0] is node:
                            keep = mult.children[1]
                        else:
                            keep = mult.children[0]
                        keep.detach()
                        if mult.parent:
                            mult.replace_with(keep)
                        else:
                            constant = keep
                        break
            elif isinstance(rhs_node, Reference):
                constant = None
                active_var = rhs_node.copy()
            else:
                print ("UNSUPPORTED NODE TYPE FOUND")
                print (type(rhs_node).__name__)
                exit(1)
            new_lhs = active_var.copy()
            if constant and rhs_node.operator == BinaryOperation.Operator.MUL:
                # output in the form x*A
                new_rhs_part = BinaryOperation.create(
                    rhs_node.operator, constant, lhs_node.copy())
            elif constant and rhs_node.operator == BinaryOperation.Operator.DIV:
                # output in the form A/x
                new_rhs_part = BinaryOperation.create(
                    rhs_node.operator, lhs_node.copy(), constant)
            else:
                new_rhs_part = lhs_node.copy()
            if active_var.name == lhs_node.name:
                if constant:
                    # Adjoint is lhs_node = constant * lhs_node
                    new_assignment = Assignment.create(new_lhs, new_rhs_part)
                else:
                    return []
            else:
                # Adjoint is rhs_node = rhs_node + constant * lhs_node
                new_rhs = BinaryOperation.create(
                    BinaryOperation.Operator.ADD, active_var.copy(), new_rhs_part)
                new_assignment = Assignment.create(new_lhs, new_rhs)
            return [new_assignment]


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

        # Create a list of adjoint assignments from the rhs terms
        adjoint_assignment_list = self._process(node.rhs, node.lhs)

        # Deal with the adjoint wrt the lhs
        increment_node = False
        for reference in node.rhs.walk(Reference):
            if node.lhs.name == reference.name:
                increment_node = True
                break
        if not increment_node:
            # lhs is not an increment so set x=0
            new_rhs = Literal("0.0", REAL_TYPE)
            adjoint_assignment_list.insert(
                0, Assignment.create(node.lhs.copy(), new_rhs))
        # replace original node with new nodes
        for adjoint_assignment in reversed(adjoint_assignment_list):
            node.parent.children.insert(node.position, adjoint_assignment)
        node.detach()

    def validate(self, node, options=None):
        '''Perform various checks to ensure that it is valid to apply the
        AssignmentTran transformation to the supplied PSyIR Node.

        :param node: the node that is being checked.
        :type node: :py:class:`psyclone.psyir.nodes.Assignment`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the node argument is not an \
            Assignment.

        '''
        if not isinstance(node, Assignment):
            raise TransformationError(
                "Node argument in assignment transformation should be a PSyIR "
                "Assignment, but found '{0}'.".format(type(node).__name__))
        assignment_active_vars = [
            var.name for var in node.walk(Reference) if var.name.lower()
            in self._active_variables]
        if not assignment_active_vars:
            # No active variables in this assigment so the assignment
            # remains unchanged.
            return
        # Check that the assignment node has a valid tangent linear form
        # i.e. DELTA_A [=|+=] X * DELTA_B + Y * DELTA_C + ...
        if not node.lhs.name.lower() in self._active_variables:
            # 1: Active vars on RHS but not active on LHS
            raise TangentLinearError(
                "Assignment node has the following active variables on its "
                "RHS '{0}' but its LHS '{1}' is not an active variable."
                "".format(assignment_active_vars, node.lhs.name))
        
        # 2) RHS term without active variable
        # 3) RHS term with multiple active variables
        # 4) RHS term with invalid form e.g. 
        # No functions, no intrinsics

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
