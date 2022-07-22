# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# Author: J. Henrichs, Bureau of Meteorology


'''This module contains the base class for adding code that traces array
accesses at runtime."
'''

from psyclone.psyGen import BuiltIn, Kern
from psyclone.psyir.nodes import (Literal, Loop, TracingNode, Directive,
                                  Reference, Schedule, OMPParallelDirective,
                                  ACCParallelDirective)
from psyclone.psyir.transformations.psy_data_trans import PSyDataTrans
from psyclone.psyir.transformations.transformation_error \
    import TransformationError


class TracingTrans(PSyDataTrans):
    '''This transformation inserts a TracingNode into the PSyIR of a
    schedule. At code creation time this node will use the PSyData API
    to create code that will trace variable accesses to arrays.

    After applying the transformation the Nodes marked for tracing are
    children of the TracingNode.
    Nodes to verify can be individual constructs within an Invoke (e.g.
    Loops containing a Kernel or BuiltIn call) or entire Invokes.

    :param node_class: The class of Node which will be inserted \
        into the tree (defaults to TracingNode), but can be any \
        derived class.
    :type node_class: :py:class:`psyclone.psyir.nodes.TracingNode` or \
        derived class

    '''
    # The types of node that this transformation can enclose
    valid_node_types = (Loop, Kern, BuiltIn, Directive, Literal, Reference)

    def __init__(self, node_class=TracingNode):
        super().__init__(node_class=node_class)

    def validate(self, nodes, options=None):
        '''Performs validation checks specific to tracing-based
        transformations.

        :param nodes: the list of Node(s) to trace.
        :type nodes: list[:py:class:`psyclone.psyir.nodes.Node]`
        :param options: a dictionary with options for transformations.
        :type options: dict[str] = (str or NoneType)

        :raises TransformationError: if transformation is applied to a \
                                     Kernel or a BuiltIn call without its \
                                     parent Loop.
        :raises TransformationError: if transformation is applied to a Loop \
                                     without its parent Directive when \
                                     optimisations are applied.
        :raises TransformationError: if transformation is applied to an \
                                     orphaned Directive without its parent \
                                     Directive.

        '''

        node_list = self.get_node_list(nodes)

        # Check TracingTrans specific constraints.
        # Check constraints not covered by valid_node_types for
        # individual Nodes in node_list.
        for node in node_list:

            # Check that a TracingNode is not inserted between a Loop
            # and its parent Directive when optimisations are applied, as
            # this may result in including the end Directive but not the
            # beginning.
            if isinstance(node, Loop) and isinstance(node.parent, Schedule) \
               and isinstance(node.parent.parent, Directive):
                raise TransformationError(
                    f"Error in {self.name}: Application to a Loop without its "
                    f"parent Directive is not allowed.")

            # Check that the TracingNode is not inserted within a
            # thread parallel region when optimisations are applied.
            if node.ancestor((OMPParallelDirective, ACCParallelDirective)):
                raise TransformationError(
                    f"Error in {self.name}: Application to Nodes enclosed "
                    f"within a thread-parallel region is not allowed.")

        # Performs validation checks specific to PSyData-based
        # transformations.
        super().validate(node_list, options)


# ============================================================================
# For automatic documentation creation:
__all__ = ["TracingTrans"]
