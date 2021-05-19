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
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import BinaryOperation, Assignment, Reference, \
    Literal
from psyclone.psyir.symbols import REAL_TYPE

# make abstract
class AdjointTransformation(Transformation):
    ''' xxx '''

    def __init__(self, active_variables):
        super(AdjointTransformation, self).__init__()
        self._active_variables = active_variables


class AssignmentTrans(AdjointTransformation):
    ''' xxx '''

    @staticmethod
    def _process(rhs_node, lhs_node):
        if (isinstance(rhs_node, BinaryOperation)) and \
           (rhs_node.operator == BinaryOperation.Operator.ADD):
            new_node_list = AssignmentTrans._process(
                rhs_node.children[0], lhs_node)
            new_node_list.extend(
                AssignmentTrans._process(rhs_node.children[1], lhs_node))
            return new_node_list
        else:
            # Adjoint is rhs_node = rhs_node + constant * lhs_node
            # *** Assuming no constant for the moment ***
            new_lhs = rhs_node.copy()
            new_rhs = BinaryOperation.create(
                BinaryOperation.Operator.ADD, rhs_node.copy(), lhs_node.copy())
            new_assignment = Assignment.create(new_lhs, new_rhs)
            return [new_assignment]

    def apply(self, node, options=None):
        ''' xxx '''
        self.validate(node)

        parent = node.parent

        # Create a list of adjoint assignments from the rhs terms
        adjoint_assignment_list = AssignmentTrans._process(node.rhs, node.lhs)

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
