# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025-2026, Science and Technology Facilities Council.
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

'''This module contains the MaximalRegionTrans.'''

import abc
from typing import Union

from psyclone.psyir.nodes import (
    Node,
    Schedule,
    Loop,
    IfBlock,
    WhileLoop,
)
from psyclone.psyGen import Transformation
from psyclone.psyir.transformations.region_trans import RegionTrans
from psyclone.psyir.transformations.transformation_error import \
    TransformationError
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class MaximalRegionTrans(RegionTrans, metaclass=abc.ABCMeta):
    '''Abstract transformation containing the functionality to add
    the largest allowed transformation to the provided code segment.

    Subclasses should override the _transformation and
    _allowed_contiguous_nodes members to control the functionality.

    The _transformation should be a transformation class to apply to
    the computed set of regions.

    The _allowed_contiguous_nodes is a tuple of Node classes that are allowed
    as statements in the transformed region. Note that upon finding a Loop or
    IfBlock, the node's children will be checked to determine whether its safe
    to contain the Loop or IfBlock in the transformed section.'''

    #: The type of transformation to be applied to the input region.
    _transformation = None
    #: Tuple of top-level statement nodes allowed inside the _transformation.
    #: Loops and IfBlocks are always recursed into if they're not part of this
    #: tuple, and their children will be checked to see which sections can
    #: have the transformation applied.
    _allowed_contiguous_nodes = ()
    #: Tuple of nodes that there must be at least one of inside the block
    #: to be transformed, else the block can be ignored (e.g. a block of
    #: only barriers doesn't need to be transformed). Defaults to any Node.
    _required_nodes = (Node)

    def _node_allowed(self, node: Node) -> bool:
        '''Returns whether the provided node is allowed in the _transformation.

        The default implementation checks whether the node is an instance
        of the _allowed_contiguous_nodes tuple, but subclasses may override
        this with additional functionality (e.g. to check if a function is
        pure).

        :param node: the candidate node to be in the transformation region.
        :returns: whether the node is allowed to be in the transformed region.
        '''
        return isinstance(node, self._allowed_contiguous_nodes)

    def _satisfies_minimum_region_rules(self, region: list[Node]) -> bool:
        '''Returns whether the provided node list satisfies the requirements
        to create a region for the _transformation.

        The default implementation checks whether a _required_node is present
        in the region, but subclasses may override this with additional
        functionality.

        :param region: The candidate region to have the transformation
            applied.
        :returns: whether the provided node list should have the
            _transformation applied.
        '''
        for node in region:
            if node.walk(self._required_nodes,
                         stop_type=self._required_nodes):
                return True
        return False

    def _can_be_in_region(self, node: Node) -> bool:
        '''Returns whether the provided node can be included in a
        region. Loops and if statements are recursed into to check if their
        children can be.

        :param node: the candidate Node to be placed into a transformed
            region.

        :returns: whether it is safe to add the node to a transformed region.
        '''
        if self._node_allowed(node):
            return True

        if isinstance(node, (Loop, WhileLoop)):
            # Check that all contents of the loop body can be part
            # of the region.
            for child in node.loop_body:
                if not self._can_be_in_region(child):
                    break
            else:
                return True
            return False

        if isinstance(node, IfBlock):
            # Check that all contents of each branch body can be part
            # of the region.
            allowed = True
            for child in node.if_body:
                allowed = (allowed and self._can_be_in_region(child))
            if node.else_body and allowed:
                for child in node.else_body:
                    allowed = (allowed and
                               self._can_be_in_region(child))
            return allowed

        # All other node types we default to False.
        return False

    def _compute_transformable_sections(
            self, node_list: list[Node],
            trans: Transformation,
    ) -> list[list[Node]]:
        '''
        Computes the sections of the input node_list to apply the
        transformation to.

        :param node_list: The node_list passed into this Transformation.
        :param trans: The transformation applied to the regions found.
        :returns: The list of node_lists to apply this class'
            _transformation class to.
        '''
        # Find the largest sections we can surround with the transformation.
        all_blocks = []
        current_block = []
        for child in node_list:
            # If the child can be added to a transformed region then add it
            # to the current block of nodes.
            if self._can_be_in_region(child):
                # Check that validation still succeeds if we add this child
                # to the current block.
                try:
                    trans.validate(current_block + [child])
                    current_block.append(child)
                except TransformationError:
                    # If validation now fails, then don't add this to the
                    # current block and add the block to the allowed blocks
                    # if allowed.
                    if current_block:
                        if self._satisfies_minimum_region_rules(current_block):
                            all_blocks.append(current_block)
                        current_block = []
            else:
                # Otherwise, if the current_block contains any children,
                # add them to the list of regions to be transformed and reset
                # the current_block.
                if current_block:
                    if self._satisfies_minimum_region_rules(current_block):
                        all_blocks.append(current_block)
                    current_block = []
                # Need to recurse on some node types
                if isinstance(child, IfBlock):
                    if_blocks = self._compute_transformable_sections(
                            child.if_body, trans
                    )
                    all_blocks.extend(if_blocks)
                    if child.else_body:
                        else_blocks = self._compute_transformable_sections(
                            child.else_body, trans
                        )
                        all_blocks.extend(else_blocks)
                if isinstance(child, (Loop, WhileLoop)):
                    loop_blocks = self._compute_transformable_sections(
                        child.loop_body, trans
                    )
                    all_blocks.extend(loop_blocks)
        # If any nodes are left in the current block at the end of the
        # node_list, then add them to a transformed region
        if current_block:
            if self._satisfies_minimum_region_rules(current_block):
                all_blocks.append(current_block)

        return all_blocks

    def validate(self, nodes: Union[Node, Schedule, list[Node]], **kwargs):
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

    def apply(self, nodes: Union[Node, Schedule, list[Node]], **kwargs):
        '''Applies the transformation to the nodes provided.

        :param nodes: can be a single node, a schedule or a list of nodes.
        '''
        node_list = self.get_node_list(nodes)

        # Call validate.
        self.validate(nodes, **kwargs)

        par_trans = self._transformation()

        all_blocks = self._compute_transformable_sections(node_list, par_trans)

        # Apply the transformation to all of the blocks found.
        for block in all_blocks:
            par_trans.apply(block)
