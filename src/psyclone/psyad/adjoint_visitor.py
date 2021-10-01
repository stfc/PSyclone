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

'''The implementation of PSyAD : the PSyclone Adjoint support. A PSyIR
visitor that supports the transformation of tangent-linear code to its
adjoint.

'''
from __future__ import print_function
import logging

from psyclone.psyad.transformations import AssignmentTrans, TangentLinearError
from psyclone.psyir.backend.visitor import PSyIRVisitor, VisitorError
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Schedule, Reference, UnaryOperation, \
    BinaryOperation, Literal, Assignment, Node
from psyclone.psyir.symbols import REAL_TYPE, INTEGER_TYPE


class AdjointVisitor(PSyIRVisitor):
    '''An Adjoint Visitor that translates a PSyIR tree representation of a
    tangent linear code to its adjoint.

    :param active_variables_names: a list of the active variables.
    :type active_variables_names: list of str

    :raises TypeError: if no active variables are supplied.

    '''
    def __init__(self, active_variable_names):
        super(AdjointVisitor, self).__init__()
        if not active_variable_names:
            raise TypeError("There should be at least one active variable supplied.")
        self._active_variable_names = active_variable_names
        self._active_variables = None
        self._logger = logging.getLogger(__name__)

    def container_node(self, node):
        '''This method is called if the visitor finds a Container node. A copy
        of the container is returned, as this does not change when
        converting from tangent linear to adjoint, which contains
        processed siblings.

        :param node: a Container PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Container`

        :returns: a new PSyIR tree containing the adjoint equivalent \
            of this node and its sibling nodes.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        self._logger.debug("Copying Container")
        return self._copy_and_process(node)

    def schedule_node(self, node):
        '''This method is called if the visitor finds a Schedule node. A copy
        of the container is returned, as this does not change when
        converting from tangent linear to adjoint and its children are
        re-ordered and sorted dependending on whether they are active
        or passive nodes.

        As a schedule contains variable scoping information. i.e. a
        symbol table, the symbols representing the active variable
        strings supplied to the visitor are found and added to an
        internal list so they are available when processing any
        siblings.

        :param node: a Schedule PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Schedule`

        :returns: a new PSyIR tree containing the adjoint equivalent \
            of this node and its sibling nodes.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        self._logger.debug("Transforming Schedule")
        if not (len(node.children) == 1 and isinstance(node.children[0], Assignment)):
                raise TangentLinearError(
                    "Support is currently limited to code with a single "
                    "assignment statement.")
        # A schedule has a scope so determine and store active variables
        symbol_table = node.scope.symbol_table
        self._active_variables = []
        for variable_name in self._active_variable_names:
            self._active_variables.append(symbol_table.lookup(variable_name))
        # At the moment support is limited to a single Assignment node
        # so it is possible to make direct use of the
        # _copy_and_process() method.
        return self._copy_and_process(node)

    def assignment_node(self, node):
        '''This method is called if the visitor finds an Assignment node. The
        adjoint equivalent of this tangent linear assignment is
        returned via the AssignmentTrans transformation. As a single
        tangent linear assignment can result in multiple adjoint
        assignments, a list of nodes is returned.

        :param node: a Schedule PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Schedule`

        :returns: a list of PSyIR nodes containing the adjoint \
            equivalent of this node.
        :rtype: list of :py:class:`psyclone.psyir.nodes.Node`

        :raises VisitorError: if no active variable symbols are \
            found. This should not be the case as these are set up in \
            a Schedule node and an Assignment node should be a sibling \
            of a Schedule node.

        '''
        self._logger.debug("Transforming active assignment")
        if not self._active_variables:
            raise VisitorError(
                "An assignment node should not be called without a schedule "
                "being called beforehand as the latter sets up the active "
                "variables.")
        assign_trans = AssignmentTrans(self._active_variables)
        new_node = node.copy()
        # Temporary parent schedule required by the transformation.
        dummy_schedule = Schedule()
        dummy_schedule.children.append(new_node)
        assign_trans.apply(new_node)
        return dummy_schedule.pop_all_children()

    def _copy_and_process(self, node):
        '''Utility function to return a copy the current node containing the
        result of processing all siblings.

        :param node: a PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Node`

        :returns: a new PSyIR tree containing a copy of this node \
            containing the result of processing all of its sibling nodes.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        node_copy = node.copy()
        node_copy.children = []
        for child in node.children:
            result = self._visit(child)
            # Result may be a node or a list of nodes
            if isinstance(result, Node):
                result = [result]
            node_copy.children.extend(result)
        return node_copy
