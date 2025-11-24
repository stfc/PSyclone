# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
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

'''This module contains the MaximalParallelRoutineTrans.'''

import abc
from typing import Union, List

from psyclone.psyir.nodes import (
    Node,
    Schedule,
    Loop,
    IfBlock,
    WhileLoop,
)
from psyclone.psyir.transformations.region_trans import RegionTrans
from psyclone.psyir.transformations.transformation_error import \
    TransformationError


class MaximalParallelRegionTrans(RegionTrans, metaclass=abc.ABCMeta):
    '''Abstract transformation containing the functionality to add
    the largest allowed parallel regions to the provided code segment.

    Subclasses should override the _parallel_transformation and _allowed_nodes
    members to control the functionality.

    The _parallel_transformation should be a transformation class to apply to
    the computed parallel regions.

    The _allowed_nodes is a tuple of Node classes that are allowed as
    statements in the parallel region. Note that upon finding a Loop or
    IfBlock, the node's children will be checked to determine whether its safe
    to contain the Loop or IfBlock in the parallel section.'''

    # The type of parallel transformation to be applied to the input region.
    _parallel_transformation = None
    # Tuple of statement nodes allowed inside the _parallel_transformation
    _allowed_nodes = ()
    # Tuple of nodes that there must be at least one of inside the block
    # to be parallelised, else the block can be ignored (e.g. a block of
    # only barriers doesn't need to be parallelised).
    _required_nodes = ()

    def _can_be_in_parallel_region(self, node: Node) -> bool:
        '''Returns whether the provided node can be included in an
        OpenMP parallel region. Most OpenMP directives can be included,
        and loops and if statements are recursed into to check if their
        children can be.

        Other statements are currently not added to an OpenMP parallel region
        by this transformation.

        :param node: the candidate Node to be placed into a parallel region.

        :returns: whether it is safe to add the node to the parallel region.
        '''

        if isinstance(node, self._allowed_nodes):
            return True

        if isinstance(node, Loop):
            # Recurse through the loop body.
            for child in node.loop_body:
                if not self._can_be_in_parallel_region(child):
                    break
            else:
                return True
            return False

        if isinstance(node, IfBlock):
            # Recurse through the if_body and else_body
            allowed = True
            for child in node.if_body:
                allowed = (allowed and self._can_be_in_parallel_region(child))
            if node.else_body and allowed:
                for child in node.else_body:
                    allowed = (allowed and
                               self._can_be_in_parallel_region(child))
            return allowed

        # All other node types we default to False.
        return False

    def validate(self, nodes: Union[Node, Schedule, List[Node]], **kwargs):
        '''Validates whether this transformation can be applied to the
        nodes provided.

        :param nodes: can be a single node, a schedule or a list of nodes.

        :raises TransformationError: if the nodes provided don't all have the
                                     same parent and aren't consecutive.
        '''

        self.validate_options(**kwargs)
        node_list = self.get_node_list(nodes)

        node_parent = node_list[0].parent
        prev_position = node_list[0].position
        for child in node_list[1:]:
            if child.parent is not node_parent:
                raise TransformationError(
                    f"Error in {self.name} transformation: supplied nodes "
                    f"are not children of the same parent.")
            if prev_position+1 != child.position:
                raise TransformationError(
                    f"Children are not consecutive children of one parent: "
                    f"child '{child.debug_string().rstrip()}' has position "
                    f"{child.position}, but previous child had position "
                    f"{prev_position}.")
            prev_position = child.position

    def apply(self, nodes: Union[Node, Schedule, List[Node]], **kwargs):
        '''Applies the transformation to the nodes provided.

        :param nodes: can be a single node, a schedule or a list of nodes.
        '''
        node_list = self.get_node_list(nodes)

        # Call validate.
        self.validate(nodes, **kwargs)

        par_trans = self._parallel_transformation()

        # Find the largest sections we can surround with parallel regions.
        current_block = []
        for child in node_list:
            # If the child can be added to a parallel region then add it
            # to the current block of nodes.
            if self._can_be_in_parallel_region(child):
                current_block.append(child)
            else:
                # Otherwise, if the current_block contains any children,
                # add them to a parallel region if we should and reset
                # the current_block.
                if current_block:
                    for node in current_block:
                        if node.walk(self._required_nodes,
                                     stop_type=self._required_nodes):
                            par_trans.apply(current_block)
                            break
                    current_block = []
                # Need to recurse on some node types
                if isinstance(child, IfBlock):
                    self.apply(child.if_body)
                    if child.else_body:
                        self.apply(child.else_body)
                if isinstance(child, Loop):
                    self.apply(child.loop_body)
                if isinstance(child, WhileLoop):
                    self.apply(child.loop_body)
        # If any nodes are left in the current block at the end of the
        # node_list, then add them to a parallel region
        if current_block:
            for node in current_block:
                if node.walk(self._required_nodes,
                             stop_type=self._required_nodes):
                    par_trans.apply(current_block)
                    break
