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
from psyclone.psyad.transformations import AdjointTransformation
from psyclone.psyir.nodes import BinaryOperation, Assignment, Reference, \
    Literal
from psyclone.psyir.symbols import DataSymbol, REAL_TYPE


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
            # Adjoint is rhs_node = rhs_node + constant * lhs_node
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
            new_rhs = BinaryOperation.create(
                BinaryOperation.Operator.ADD, active_var.copy(), new_rhs_part)
            new_assignment = Assignment.create(new_lhs, new_rhs)
            return [new_assignment]


    def apply(self, node, options=None):
        ''' xxx '''
        self.validate(node)

        parent = node.parent

        # Create a list of adjoint assignments from the rhs terms
        adjoint_assignment_list = self._process(node.rhs, node.lhs)

        # Deal with the adjoint wrt the lhs
        increment_node = None
        for reference in node.rhs.walk(Reference):
            if node.lhs.name == reference.name:
                increment_node = node
        if increment_node:
            # lhs is an increment so x=ax
            new_rhs = node.lhs.copy()
        else:
            # lhs is not an increment so x=0
            new_rhs = Literal("0.0", REAL_TYPE)
        adjoint_assignment_list.insert(0, Assignment.create(node.lhs.copy(), new_rhs))

        # replace original node with new nodes
        for adjoint_assignment in reversed(adjoint_assignment_list):
            node.parent.children.insert(node.position, adjoint_assignment)
        node.detach()

    def validate(self, node, optione=None):
        ''' xxx '''
        # Must be assignment
        # Must have parent
        # Check that assignment is in the form DELTA_A [=|+=] X *
        # DELTA_B + Y * DELTA_C + ...
        pass

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
