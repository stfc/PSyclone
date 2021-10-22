# -----------------------------------------------------------------------------
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

'''A PSyIR visitor for PSyAD : the PSyclone Adjoint support. Applies
transformations to tangent-linear PSyIR to return its PSyIR adjoint.

'''
from __future__ import print_function
import logging

from psyclone.psyad.transformations import AssignmentTrans
from psyclone.psyad.utils import node_is_passive
from psyclone.psyir.backend.visitor import PSyIRVisitor, VisitorError
from psyclone.psyir.nodes import Schedule, Node


class AdjointVisitor(PSyIRVisitor):
    '''An Adjoint Visitor that translates the PSyIR of a tangent-linear
    code into its adjoint form.

    :param active_variable_names: a list of the active variables.
    :type active_variable_names: list of str

    :raises ValueError: if no active variables are supplied.

    '''
    def __init__(self, active_variable_names):
        super(AdjointVisitor, self).__init__()
        if not active_variable_names:
            raise ValueError(
                "There should be at least one active variable supplied to "
                "an AdjointVisitor.")
        self._active_variable_names = active_variable_names
        self._active_variables = None
        self._logger = logging.getLogger(__name__)

    def container_node(self, node):
        '''This method is called if the visitor finds a Container node. A copy
        of the container is returned (as this does not change when
        converting from tangent linear to adjoint) containing
        processed descendants.

        :param node: a Container PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Container`

        :returns: a new PSyIR tree containing the adjoint of this node \
            and its descendant nodes.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        self._logger.debug("Copying Container")
        return self._copy_and_process(node)

    def schedule_node(self, node):
        '''This method is called if the visitor finds a Schedule node. A copy
        of the schedule is returned, as this does not change when
        converting from tangent linear to adjoint and its children are
        re-ordered and sorted dependending on whether they are active
        or passive nodes.

        As a schedule contains variable scoping information, i.e. a
        symbol table, the symbols representing the active variable
        names supplied to the visitor are found and added to an
        internal list so they are available when processing any
        descendants.

        :param node: a Schedule PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Schedule`

        :returns: a new PSyIR tree containing the adjoint equivalent \
            of this node and its descendants.
        :rtype: :py:class:`psyclone.psyir.nodes.Schedule`

        '''
        self._logger.debug("Transforming Schedule")

        # A schedule has a scope so determine and store active variables
        symbol_table = node.scope.symbol_table
        self._active_variables = []
        for variable_name in self._active_variable_names:
            self._active_variables.append(symbol_table.lookup(variable_name))

        # We only need to copy this node. Issue #1440 will address
        # this.
        node_copy = node.copy()
        node_copy.children = []

        # Split active and passive nodes.
        self._logger.debug("Adding passive code into new schedule")
        active_nodes = []
        for child in node.children:
            if node_is_passive(child, self._active_variables):
                # Add passive nodes back into the schedule as they do
                # not change.
                node_copy.children.append(child.copy())
            else:
                # Store active nodes for further processing.
                active_nodes.append(child)

        # Reverse active nodes.
        self._logger.debug("Reversing order of active code")
        active_nodes.reverse()

        # Process active nodes.
        self._logger.debug(
            "Processing active code and adding results into new schedule")
        for child in active_nodes:
            result = self._visit(child)
            # The else clause below can not be exercised until nodes
            # in a schedule other than assignment are supported, see
            # issue #1457. Therefore, for the moment simply assume
            # that the result is a list.
            node_copy.children.extend(result)
            # if isinstance(result, list):
            #     node_copy.children.extend(result)
            # else:
            #     node_copy.children.append(result)

        return node_copy

    def assignment_node(self, node):
        '''This method is called if the visitor finds an Assignment node. The
        adjoint of this tangent-linear assignment is returned via the
        AssignmentTrans transformation. As the adjoint of a single
        tangent-linear assignment can consist of multiple assignments,
        a list of nodes is returned.

        :param node: an Assignment PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Assignment`

        :returns: a list of PSyIR nodes containing the adjoint \
            of this node.
        :rtype: list of :py:class:`psyclone.psyir.nodes.Node`

        :raises VisitorError: if the schedule_node method has not been \
            called previously.

        '''
        self._logger.debug("Transforming active assignment")
        if self._active_variables is None:
            raise VisitorError(
                "An assignment node should not be visited before a schedule, "
                "as the latter sets up the active variables.")
        assign_trans = AssignmentTrans(self._active_variables)
        new_node = node.copy()
        # Temporary parent schedule required by the transformation.
        dummy_schedule = Schedule()
        dummy_schedule.children.append(new_node)
        assign_trans.apply(new_node)
        return dummy_schedule.pop_all_children()

    def _copy_and_process(self, node):
        '''Utility function to return a copy the current node containing the
        result of processing all descendants.

        :param node: a PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Node`

        :returns: a new PSyIR tree containing a copy of this node \
            which contains the result of processing all of its \
            descendants.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        # We only need to copy this node. Issue #1440 will address
        # this.
        node_copy = node.copy()
        node_copy.children = []
        for child in node.children:
            result = self._visit(child)
            # Result may be a node or a list of nodes
            if isinstance(result, Node):
                result = [result]
            node_copy.children.extend(result)
        return node_copy


# =============================================================================
# Documentation utils: The list of module members that we wish AutoAPI to
# generate documentation for (see https://psyclone-ref.readthedocs.io).
__all__ = ["AdjointVisitor"]
