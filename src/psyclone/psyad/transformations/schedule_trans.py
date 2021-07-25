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

'''This module contains a transformation that replaces the contents of
a tangent-linear PSyIR schedule with its adjoint form.

'''
from __future__ import absolute_import

from psyclone.psyir.nodes import BinaryOperation, Assignment, Reference, \
    Literal, UnaryOperation, Schedule
from psyclone.psyir.symbols import REAL_TYPE
from psyclone.psyir.transformations import TransformationError

from psyclone.psyad.transformations import AdjointTransformation, \
    TangentLinearError, AssignmentTrans

# pylint: disable=too-many-locals
# pylint: disable=too-many-branches


class ScheduleTrans(AdjointTransformation):
    '''Implements a transformation to translate the contents of a
    Tangent-Linear PSyIR schedule with its adjoint form.

    '''
    def apply(self, node, options=None):
        '''Apply the ScheduleTrans transformation to the specified node. The
        node must be a valid tangent-linear schedule. The contents of
        the schedule are replaced with their adjoint version.

        :param node: a Schedule node.
        :type node: :py:class:`psyclone.psyir.nodes.Schedule`
        :param options: a dictionary with options for transformations.
        :type options: dict of string:values or None

        '''
        self.validate(node)
        
        orig_schedule = node
        assign_trans = AssignmentTrans(self._active_variables)

        nodes = orig_schedule.children[:]
        # Detach nodes from a copy of the original list, otherwise
        # we are modifying a list as we iterate over it which can
        # cause incorrect behaviour.
        for node in nodes:
            node.detach()

        # split active and inactive assignments.
        active_nodes = []
        for node in nodes:
            if not assign_trans.active(node):
                # Add inactive assignments back into the schedule.
                orig_schedule.children.append(node)
            else:
                # Store active asignments for further processing.
                active_nodes.append(node)

        # Reverse active assignments.
        active_nodes.reverse()

        # Add active assignments back into the schedule and transform
        # them.
        for node in active_nodes:
            orig_schedule.children.append(node)
            assign_trans.apply(node)

    def validate(self, node, options=None):
        '''Perform various checks to ensure that it is valid to apply the
        ScheduleTrans transformation to the supplied PSyIR Node.

        :param node: the node that is being checked.
        :type node: :py:class:`psyclone.psyir.nodes.Schedule`
        :param options: a dictionary with options for transformations.
        :type options: dict of string:values or None

        :raises TransformationError: if the node argument is not a \
            Schedule.
        :raises TransformationError: if at least one of the nodes \
            within the schedule is not an assignment node.

        '''
        # Check node argument is a schedule node.
        if not isinstance(node, Schedule):
            raise TransformationError(
                "Node argument in schedule transformation should be a PSyIR "
                "Schedule, but found '{0}'.".format(type(node).__name__))

        # Check all nodes in the schedule are assignments.
        for child in node.children:
            if not isinstance(child, Assignment):
                raise TransformationError(
                    "This transformation is currently limited to assignments "
                    "within a schedule but found '{0}'"
                    "".format(self._writer(child)))

    def __str__(self):
        return "Convert a tangent-linear PSyIR Schedule to its adjoint form"

    @property
    def name(self):
        return self.__class__.__name__
    


# =============================================================================
# Documentation utils: The list of module members that we wish AutoAPI to
# generate documentation for (see https://psyclone-ref.readthedocs.io).
__all__ = ["ScheduleTrans"]
