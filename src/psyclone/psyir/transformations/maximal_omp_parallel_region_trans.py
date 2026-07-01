# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2026, Science and Technology Facilities Council.
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
# Authors A. B. G. Chalk, STFC Daresbury Lab

'''This module contains the MaximalOMPParallelRegionTrans.'''

from typing import Union

from psyclone.psyir.nodes import (
        Assignment,
        OMPTaskwaitDirective,
        OMPBarrierDirective,
        OMPSerialDirective,
        OMPTaskloopDirective,
        OMPDoDirective,
        OMPLoopDirective,
        OMPTaskDirective,
        DynamicOMPTaskDirective,
        Node,
        Reference,
        Schedule
)
from psyclone.psyir.transformations.maximal_region_trans import (
        MaximalRegionTrans
)
from psyclone.psyir.transformations.omp_parallel_trans import OMPParallelTrans
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class MaximalOMPParallelRegionTrans(MaximalRegionTrans):
    '''Applies OpenMP Parallel directives around the largest possible sections
    of the input.

    At current, this will never place OpenMP parallel sections around
    Assignments that are outside of another OpenMP directive. See #3157 and
    the discussion on #3205 for more detail.'''
    # The type of parallel transformation to be applied to the input region.
    _transformation = OMPParallelTrans
    _SUB_TRANSFORMATIONS = [OMPParallelTrans]
    # Tuple of statement nodes allowed inside the _transformation
    # Note that Assignments may also be allowed, but they are a special
    # case not included in this selection.
    _allowed_contiguous_statements = (
        OMPTaskwaitDirective,
        OMPBarrierDirective,
        OMPSerialDirective,
        OMPTaskloopDirective,
        OMPDoDirective,
        OMPLoopDirective,
        OMPTaskDirective,
        DynamicOMPTaskDirective,
    )
    _required_nodes = (
        OMPSerialDirective,
        OMPTaskloopDirective,
        OMPDoDirective,
        OMPLoopDirective,
        OMPTaskDirective,
        DynamicOMPTaskDirective,
    )

    def _node_allowed(self, node: Node, current_block: list[Node]) -> bool:
        '''Returns whether the provided node is allowed in the _transformation.
        
        FIXME document why this differs.

        :param node: the candidate node to be in the transformation region.
        :param current_block: The current block that node would be added into.

        :returns: whether the node is allowed to be in the transformed region.
        '''
        if isinstance(node, self._allowed_contiguous_statements):
            return True

        if not isinstance(node, Assignment):
            return False

        # Assignments are a special case.
        # If the lhs is an array or a non-local symbol then it is not allowed.
        if node.lhs.is_array or not node.lhs.symbol.is_automatic:
            return False

        # If the lhs symbol appears on the rhs then its read first so we
        # dissallow it for now. Note that in theory this could be allowed
        # if its previously been accessed in this region and is private
        # but we have no ability to determine this in PSyclone at this stage.
        lhs_sym = node.lhs.symbol
        for reference in node.rhs.walk(Reference):
            if reference.symbol.name == lhs_sym.name:
                return False

        # We know we have an assignment where the lhs is a local scalar
        # and does not appear on the right hand side. We need to check all
        # the following read accesses appear in the current_block.
        accesses = node.lhs.next_accesses()
        # Find the abs_position of the final node in the current_block.
        last_element = current_block[-1]
        # For reviewer - this could be implemented using
        # following_node.abs_position-1, but if there is no following_node
        # we'd still need to return to this (or I guess some recursive
        # loop over children[-1]. Let me know if you have a preference
        # w.r.t performance as this is probably the worst option but
        # easy to read.
        start_position = node.lhs.abs_position
        end_position = last_element.walk(Node)[-1].abs_position

        for access in accesses:
            # We can ignore writes as this overwrites any data from
            # this assignment.
            if isinstance(access, Reference) and access.is_write:
                continue
            pos = access.abs_position
            # If we have a read access outside of the region then
            # we can't add this assignment to the region.
            if pos < start_position or pos > end_position:
                return False
        # Privatisation of variables is handled when applying the
        # transformation.
        return True

    def _apply_transformation(self, block: list[Node],
                              **kwargs):
        '''
        Applies the transformation defined by self._transformation to the
        block supplied, with the kwargs provided.

        The default implementation does nothing else, however the
        functionality is provided so subclasses can perform specific
        operations (or provide additional specific options) as required for
        their transformations.

        :param block: The block to apply the transformations to.
        '''
        # FIXME privatisation.
        # If we have any assignments directly in the block then we need to
        # do privatisation
        self._transformation.apply(block, **kwargs)

    def apply(self, nodes: Union[Node, Schedule, list[Node]], **kwargs):
        '''Applies the transformation to the nodes provided.

        :param nodes: can be a single node, a schedule or a list of nodes.
        '''
        super().apply(nodes, **kwargs)
