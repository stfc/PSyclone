# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024-2025, Science and Technology Facilities Council.
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
# Author: A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------
"""This module contains the DefinitionUseChain class"""

import sys

from fparser.two.Fortran2003 import (
    Cycle_Stmt,
    Exit_Stmt,
    Goto_Stmt,
)

from psyclone.psyir.nodes import (
    Assignment,
    Call,
    CodeBlock,
    IfBlock,
    IntrinsicCall,
    Loop,
    Node,
    Reference,
    RegionDirective,
    Return,
    Routine,
    Schedule,
    Statement,
    WhileLoop,
    PSyDataNode,
)


class DefinitionUseChain:
    """The DefinitionUseChain class is used to find nodes in a tree
    that have data dependencies on the provided reference.

    :param reference: The Reference for which the dependencies will be
                      computed.
    :type reference: :py:class:`psyclone.psyir.nodes.Reference`
    :param control_flow_region: Optional region to search for data
                                dependencies. Default is the parent Routine or
                                the root of the tree's children if no ancestor
                                Routine exists.
    :type control_flow_region: Optional[List[
                               :py:class:`psyclone.psyir.nodes.Node`]]
    :param int start_point: Optional argument to define a start point for the
                            dependency search.
    :param int stop_point: Optional argument to define a stop point for the
                           dependency search.

    :raises TypeError: If one of the arguments is the wrong type.

    """

    def __init__(
        self,
        reference,
        control_flow_region=None,
        start_point=None,
        stop_point=None,
    ):
        if not isinstance(reference, Reference):
            raise TypeError(
                f"The 'reference' argument passed into a DefinitionUseChain "
                f"must be a Reference but found "
                f"'{type(reference).__name__}'."
            )
        self._reference = reference
        # Store the absolute position for later.
        self._reference_abs_pos = reference.abs_position
        # To enable loops to work correctly we can set the start/stop point
        # and not just use base it on the reference's absolute position
        if start_point and not isinstance(start_point, int):
            raise TypeError(
                f"The start_point passed into a "
                f"DefinitionUseChain must be an int but found "
                f"'{type(start_point).__name__}'."
            )
        if stop_point and not isinstance(stop_point, int):
            raise TypeError(
                f"The stop_point passed into a "
                f"DefinitionUseChain must be an int but found "
                f"'{type(stop_point).__name__}'."
            )
        self._start_point = start_point
        self._stop_point = stop_point
        if control_flow_region is None:
            self._scope = [reference.ancestor(Routine)]
            if self._scope[0] is None:
                self._scope = reference.root.children[:]
        else:
            # We need a list of regions for control flow.
            if not isinstance(control_flow_region, list):
                raise TypeError(
                    f"The control_flow_region passed into a "
                    f"DefinitionUseChain must be a list but found "
                    f"'{type(control_flow_region).__name__}'."
                )
            if not all(isinstance(x, Node) for x in control_flow_region):
                raise TypeError(
                    f"Each element of the control_flow_region passed into a "
                    f"DefinitionUseChain must be a Node but found a non-Node "
                    f"element. Full input is '{str(control_flow_region)}'."
                )
            self._scope = control_flow_region

        # The uses, defsout and killed sets as defined for each basic block.
        self._uses = []
        self._defsout = []
        self._killed = []

        # The output map, mapping between nodes and the reach of that node.
        self._reaches = []

    @property
    def uses(self):
        """
        :returns: the list of nodes using the value that the referenced symbol
                  has before it is reassigned.
        :rtype: List[:py:class:`psyclone.psyir.nodes.Node`]
        """
        return self._uses

    @property
    def defsout(self):
        """
        :returns: the list of nodes that reach the end of the block without
                  being killed, and therefore can have dependencies outside
                  of this block.
        :rtype: List[:py:class:`psyclone.psyir.nodes.Node`]
        """
        return self._defsout

    @property
    def killed(self):
        """
        :returns: the list of nodes that represent the last use of an assigned
                  variable. Calling next_access on any of these nodes will find
                  a write that reassigns it's value.
        :rtype: List[:py:class:`psyclone.psyir.nodes.Node`]
        """
        return self._killed

    @property
    def is_basic_block(self):
        """
        :returns: whether the scope of this DefinitionUseChain is a basic
                  block, i.e. whether it contains any control flow nodes.
        :rtype: bool
        """
        # A basic block is a scope without any control flow nodes inside.
        # In PSyclone, possible control flow nodes are IfBlock, Loop
        # and WhileLoop, along with RegionDirectives.
        for node in self._scope:
            if node.has_descendant(
                    (IfBlock, Loop, WhileLoop, RegionDirective)):
                return False
        return True

    def find_forward_accesses(self):
        """
        Find all the forward accesses for the reference defined in this
        DefinitionUseChain.
        Forward accesses are all of the References or Calls that read
        or write to the symbol of the reference up to the point that a
        write to the symbol is guaranteed to occur.
        PSyclone assumes all control flow may not be taken, so writes
        that occur inside control flow do not end the forward access
        chain.

        :returns: the forward accesses of the reference given to this
                  DefinitionUseChain
        :rtype: list[:py:class:`psyclone.psyir.nodes.Node`]
        """
        # Compute the abs position caches as we'll use these a lot.
        # The compute_cached_abs_position will only do this if needed
        # so we don't need to check here.
        self._reference.compute_cached_abs_positions()

        # Setup the start and stop positions
        save_start_position = self._start_point
        save_stop_position = self._stop_point
        # If there is no set start point, then we look for all
        # accesses after the Reference.
        if self._start_point is None:
            self._start_point = self._reference_abs_pos
        # If there is no set stop point, then any Reference after
        # the start point can potentially be a forward access.
        if self._stop_point is None:
            self._stop_point = sys.maxsize
        if not self.is_basic_block:
            # If this isn't a basic block, then we find all of the basic
            # blocks.
            control_flow_nodes, basic_blocks = self._find_basic_blocks(
                self._scope
            )
            chains = []
            # If this is the top level access, we need to check if the
            # reference has an ancestor loop. If it does, we find the
            # highest ancestor Loop in the tree and add a
            # DefinitionUseChain block at the start to search for things
            # before the Reference that can also be looped back to.
            # We should probably have this be any top level time this is
            # called but thats hard to otherwise track.
            if (
                isinstance(self._scope[0], Routine)
                or self._scope[0] is self._reference.root
            ):
                # Check if there is an ancestor Loop/WhileLoop.
                ancestor = self._reference.ancestor((Loop, WhileLoop))
                while ancestor is not None:
                    # Create a basic block for the ancestor Loop.
                    body = ancestor.loop_body.children[:]
                    control_flow_nodes.insert(0, ancestor)
                    # Find the stop point - this needs to be the node after
                    # the ancestor statement.
                    sub_stop_point = (
                        self._reference.ancestor(Statement)
                        .walk(Node)[-1]
                        .abs_position
                        + 1
                    )
                    # If we have a basic block with no children then skip it,
                    # e.g. for an if block with no code before the else
                    # statement.
                    if len(body) > 0:
                        # We make a copy of the reference to have a detached
                        # node to avoid handling the special cases based on
                        # the parents of the reference.
                        chain = DefinitionUseChain(
                            self._reference.copy(),
                            body,
                            start_point=ancestor.abs_position,
                            stop_point=sub_stop_point,
                        )
                        chains.insert(0, chain)
                    # If its a while loop, create a basic block for the while
                    # condition.
                    if isinstance(ancestor, WhileLoop):
                        control_flow_nodes.insert(0, None)
                        sub_stop_point = ancestor.loop_body.abs_position
                        chain = DefinitionUseChain(
                            self._reference.copy(),
                            [ancestor.condition],
                            start_point=ancestor.abs_position,
                            stop_point=sub_stop_point,
                        )
                        chains.insert(0, chain)
                    ancestor = ancestor.ancestor((Loop, WhileLoop))

                # Check if there is an ancestor Assignment.
                ancestor = self._reference.ancestor(Assignment)
                if ancestor is not None:
                    # If the reference is the lhs then we can ignore the RHS.
                    if ancestor.lhs is self._reference:
                        # Find the last node in the assignment
                        last_node = ancestor.walk(Node)[-1]
                        # Modify the start_point to only include the node after
                        # this assignment.
                        self._start_point = last_node.abs_position
                    else:
                        # Add the lhs as a potential basic block with
                        # different start and stop positions.
                        chain = DefinitionUseChain(
                            self._reference,
                            [ancestor.lhs],
                            start_point=ancestor.lhs.abs_position - 1,
                            stop_point=ancestor.lhs.abs_position + 1,
                        )
                        control_flow_nodes.append(None)
                        chains.append(chain)
                        # N.B. For now this assumes that for an expression
                        # b = a * a, that next_access to the first Reference
                        # to a should not return the second Reference to a.
            # Now add all the other standardly handled basic_blocks to the
            # list of chains.
            for block in basic_blocks:
                # If we have a basic block with no children then skip it,
                # e.g. for an if block with no code before the else
                # statement.
                if len(block) == 0:
                    continue
                chain = DefinitionUseChain(
                    self._reference,
                    block,
                    start_point=self._start_point,
                    stop_point=self._stop_point,
                )
                chains.append(chain)
            for i, chain in enumerate(chains):
                # Compute the defsout, killed and reaches for the block.
                chain.find_forward_accesses()
                cfn = control_flow_nodes[i]

                if cfn is None:
                    # We're outside a control flow region, updating the reaches
                    # here is to find all the reached nodes.
                    for ref in chain._reaches:
                        # Add unique references to reaches. Since we're not
                        # in a control flow region, we can't have added
                        # these references into the reaches array yet so
                        # they're guaranteed to be unique.
                        self._reaches.append(ref)
                    # If we have a defsout in the chain then we can stop as we
                    # will never get past the write as its not conditional.
                    if len(chain.defsout) > 0:
                        # Reset the start and stop points before returning
                        # the result.
                        self._start_point = save_start_position
                        self._stop_point = save_stop_position
                        return self._reaches
                else:
                    # We assume that the control flow here could be 'not
                    # taken', i.e. that this doesn't kill the chain.
                    # TODO #2760: In theory we could analyse loop structures
                    # or if block structures to see if we're guaranteed to
                    # write to the symbol.
                    # If the control flow node is a Loop we have to check
                    # if the variable is the same symbol as the _reference.
                    if isinstance(cfn, Loop):
                        cfn_abs_pos = cfn.abs_position
                        if (
                            cfn.variable == self._reference.symbol
                            and cfn_abs_pos >= self._start_point
                            and cfn_abs_pos < self._stop_point
                        ):
                            # The loop variable is always written to and so
                            # we're done if its reached.
                            self._reaches.append(cfn)
                            self._start_point = save_start_position
                            self._stop_point = save_stop_position
                            return self._reaches

                    for ref in chain._reaches:
                        # We will only ever reach a reference once, so
                        # we don't need to check uniqueness.
                        self._reaches.append(ref)
        else:
            # Check if there is an ancestor Assignment.
            ancestor = self._reference.ancestor(Assignment)
            if ancestor is not None:
                # If we get here to check the start part of a loop we need
                # to handle this differently.
                if self._start_point != self._reference_abs_pos:
                    pass
                # If the reference is the lhs then we can ignore the RHS.
                if ancestor.lhs is self._reference:
                    # Find the last node in the assignment
                    last_node = ancestor.walk(Node)[-1]
                    # Modify the start_point to only include the node after
                    # this assignment.
                    self._start_point = last_node.abs_position
                elif ancestor.lhs is self._scope[0] and len(self._scope) == 1:
                    # If the ancestor LHS is the scope of this chain then we
                    # do nothing.
                    pass
                else:
                    # Add the lhs as a potential basic block with different
                    # start and stop positions.
                    chain = DefinitionUseChain(
                        self._reference,
                        [ancestor.lhs],
                        start_point=ancestor.lhs.abs_position - 1,
                        stop_point=ancestor.lhs.abs_position + 1,
                    )
                    # Find any forward_accesses in the lhs.
                    chain.find_forward_accesses()
                    for ref in chain._reaches:
                        self._reaches.append(ref)
                    # If we have a defsout in the chain then we can stop as we
                    # will never get past the write as its not conditional.
                    if len(chain.defsout) > 0:
                        # Reset the start and stop points before returning
                        # the result.
                        self._start_point = save_start_position
                        self._stop_point = save_stop_position
                        return self._reaches
            # We can compute the rest of the accesses
            self._compute_forward_uses(self._scope)
            for ref in self._uses:
                self._reaches.append(ref)
            # If this block doesn't kill any accesses, then we add
            # the defsout into the reaches array.
            if len(self.killed) == 0:
                for ref in self._defsout:
                    self._reaches.append(ref)
            else:
                # If this block killed any accesses, then the first element
                # of the killed writes is the access access that we're
                # dependent with.
                self._reaches.append(self.killed[0])

        # Reset the start and stop points before returning the result.
        self._start_point = save_start_position
        self._stop_point = save_stop_position
        return self._reaches

    def _compute_forward_uses(self, basic_block_list):
        """
        Compute the forward uses for self._reference for the
        basic_block_list provided. This function will not work
        correctly if there is control flow inside the
        basic_block_list provided.
        Reads to the reference that occur before a write will
        be added to the self._uses array, the final write will
        be provided as self._defsout and all previous writes
        will be inside self._killed.

        :param basic_block_list: The list of nodes that make up the basic
                                 block to find the forward uses in.
        :type basic_block_list: list[:py:class:`psyclone.psyir.nodes.Node`]

        :raises NotImplementedError: If a GOTO statement is found in the code
                                     region.
        """
        sig, _ = self._reference.get_signature_and_indices()
        # For a basic block we will only ever have one defsout
        defs_out = None
        for region in basic_block_list:
            for reference in region.walk((Reference, Call, CodeBlock, Return)):
                # Store the position instead of computing it twice.
                abs_pos = reference.abs_position
                if abs_pos <= self._start_point or abs_pos >= self._stop_point:
                    continue
                if isinstance(reference, Return):
                    # When we find a return statement any following statements
                    # can be ignored so we can return.
                    if defs_out is not None:
                        self._defsout.append(defs_out)
                    return
                # If its parent is an inquiry function then its neither
                # a read nor write if its the first argument.
                if (isinstance(reference.parent, IntrinsicCall) and
                        reference.parent.is_inquiry and
                        reference.parent.arguments[0] is reference):
                    continue
                if isinstance(reference, CodeBlock):
                    # CodeBlocks only find symbols, so we can only do as good
                    # as checking the symbol - this means we can get false
                    # positives for structure accesses inside CodeBlocks.
                    if isinstance(reference._fp2_nodes[0], Goto_Stmt):
                        raise NotImplementedError(
                            "DefinitionUseChains can't handle code containing"
                            " GOTO statements."
                        )
                    # If we find an Exit or Cycle statement, we can't
                    # reach further in this code region so we can return.
                    if isinstance(
                        reference._fp2_nodes[0], (Exit_Stmt, Cycle_Stmt)
                    ):
                        if defs_out is not None:
                            self._defsout.append(defs_out)
                        return
                    if (
                        self._reference.symbol.name
                        in reference.get_symbol_names()
                    ):
                        # Assume the worst for a CodeBlock and we count them
                        # as killed and defsout and uses.
                        if defs_out is not None:
                            self._killed.append(defs_out)
                        defs_out = reference
                        continue
                elif isinstance(reference, Call):
                    # If its a local variable we can ignore it as we'll catch
                    # the Reference later if its passed into the Call.
                    if self._reference.symbol.is_automatic:
                        continue
                    if isinstance(reference, IntrinsicCall):
                        # IntrinsicCall can only do stuff to arguments, these
                        # will be caught by Reference walk already.
                        # Note that this assumes two symbols are not
                        # aliases of each other.
                        continue
                    if reference.is_pure:
                        # Pure subroutines only touch their arguments, so we'll
                        # catch the arguments that are passed into the call
                        # later as References.
                        continue
                    # For now just assume calls are bad if we have a non-local
                    # variable and we treat them as though they were a write.
                    if defs_out is not None:
                        self._killed.append(defs_out)
                    defs_out = reference
                    continue
                elif reference.get_signature_and_indices()[0] == sig:
                    # Work out if its read only or not.
                    assign = reference.ancestor(Assignment)
                    if assign is not None:
                        if assign.lhs is reference:
                            # This is a write to the reference, so kill the
                            # previous defs_out and set this to be the
                            # defs_out.
                            if defs_out is not None:
                                self._killed.append(defs_out)
                            defs_out = reference
                        elif (
                            assign.lhs is defs_out
                            and len(self._killed) == 0
                            and assign.lhs.get_signature_and_indices()[0]
                            == sig
                            and assign.lhs is not self._reference
                        ):
                            # reference is on the rhs of an assignment such as
                            # a = a + 1. Since the PSyIR tree walk accesses
                            # the lhs of an assignment before the rhs of an
                            # assignment we need to not ignore these accesses.
                            self._uses.append(reference)
                        else:
                            # Read only, so if we've not yet set written to
                            # this variable this is a use. NB. We need to
                            # check the if the write is the LHS of the parent
                            # assignment and if so check if we killed any
                            # previous assignments.
                            if defs_out is None:
                                self._uses.append(reference)
                    elif reference.ancestor(Call):
                        # Otherwise we assume read/write access for now.
                        if defs_out is not None:
                            self._killed.append(defs_out)
                        defs_out = reference
                    else:
                        # Reference outside an Assignment - read only
                        # This could be References inside a While loop
                        # condition for example.
                        if defs_out is None:
                            self._uses.append(reference)
        if defs_out is not None:
            self._defsout.append(defs_out)

    def _find_basic_blocks(self, nodelist):
        """
        Compute the blocks inside the provided list of nodes.
        Each block is a set of nodes inside a control flow region, and
        may contain more control flow (which will be handled recusively later).
        Each block also has the control flow node stored that contains the
        block, e.g. for a Loop, the block consisting of loop.body will have
        contain the associated Loop at the same index in the control_flow_nodes
        return value.

        :returns: (control_flow_nodes, basic_blocks). control_flow_nodes
                  contains the list of control_flow_nodes corresponding to
                  the lists of nodes contained in the basic_block list.
        :rtype: tuple(list[:py:class:`psyclone.psyir.nodes.Node],
                      list[list[:py:class:`psyclone.psyir.nodes.Node]])

        """
        current_block = []
        # Keep track of the basic blocks.
        basic_blocks = []
        # Keep track of the control flow node that corresponds to a basic block
        # if one exists.
        control_flow_nodes = []
        # If we have the top level routine then the nodelist needs to be the
        # Routine's children.
        if len(nodelist) == 1 and isinstance(nodelist[0], Routine):
            nodelist = nodelist[0].children[:]
        # We expand Schedules if they're in the nodelist.
        new_nodelist = []
        for node in nodelist:
            if isinstance(node, Schedule):
                new_nodelist.extend(node.children[:])
            else:
                new_nodelist.append(node)
        nodelist = new_nodelist
        for node in nodelist:
            if isinstance(node, Loop):
                # Add any current block to the list of blocks.
                if len(current_block) > 0:
                    basic_blocks.append(current_block)
                    control_flow_nodes.append(None)
                    current_block = []
                # The start/stop/step expr are non-conditional (but also
                # read only).
                control_flow_nodes.append(None)
                current_block.append(node.start_expr)
                current_block.append(node.stop_expr)
                current_block.append(node.step_expr)
                basic_blocks.append(current_block)
                current_block = []
                # The loop body is a conditional.
                control_flow_nodes.append(node)
                basic_blocks.append(node.loop_body.children[:])
            elif isinstance(node, WhileLoop):
                if len(current_block) > 0:
                    basic_blocks.append(current_block)
                    control_flow_nodes.append(None)
                    current_block = []
                # The current block is a list of the parts of a Whileloop.
                # The while loop condition is non-conditional.
                control_flow_nodes.append(None)
                basic_blocks.append([node.condition])
                # the loop body is conditional.
                control_flow_nodes.append(node)
                basic_blocks.append(node.loop_body.children[:])
            elif isinstance(node, IfBlock):
                # Add any current block to the list of blocks.
                if len(current_block) > 0:
                    basic_blocks.append(current_block)
                    control_flow_nodes.append(None)
                    current_block = []
                # We add a basic block for each of the parts of an IfBlock.
                # No control for the condition - we always check that.
                control_flow_nodes.append(None)
                basic_blocks.append([node.condition])
                # If it is inside a loop, the condition can loop back to itself
                # or the other branch in the IfBlock
                if node.ancestor((Loop, WhileLoop)):
                    control_flow_nodes.append(node)
                    basic_blocks.append(node.if_body.children[:])
                    if node.else_body:
                        control_flow_nodes.append(node)
                        basic_blocks.append(node.else_body.children[:])
                    continue
                # Check if the node is in the else_body
                in_else_body = False
                if node.else_body:
                    refs = node.else_body.walk(Reference)
                    for ref in refs:
                        if ref is self._reference:
                            # If its in the else_body we don't add the if_body
                            in_else_body = True
                            break
                if not in_else_body:
                    control_flow_nodes.append(node)
                    basic_blocks.append(node.if_body.children[:])
                # Check if the node is in the if_body
                in_if_body = False
                refs = node.if_body.walk(Reference)
                for ref in refs:
                    if ref is self._reference:
                        in_if_body = True
                        break
                if node.else_body and not in_if_body:
                    control_flow_nodes.append(node)
                    basic_blocks.append(node.else_body.children[:])
            elif isinstance(node, RegionDirective):
                # Add any current block to the list of blocks.
                if len(current_block) > 0:
                    basic_blocks.append(current_block)
                    control_flow_nodes.append(None)
                    current_block = []
                # TODO #2751 if directives are optional we should be more
                # careful, i.e. if we add support for the if clause.
                # We add a basic block for each of the parts of the
                # RegionDirective. We don't need to do anything with the
                # control flow storing for now.
                # This assumes that data in clauses is inquiry for now.
                control_flow_nodes.append(None)
                basic_blocks.append([node.dir_body])
            elif isinstance(node, PSyDataNode):
                # Add any current block to the list of blocks.
                if len(current_block) > 0:
                    basic_blocks.append(current_block)
                    control_flow_nodes.append(None)
                    current_block = []
                control_flow_nodes.append(None)
                basic_blocks.append([node.psy_data_body])
            else:
                # This is a basic node, add it to the current block
                current_block.append(node)
        if len(current_block) > 0:
            basic_blocks.append(current_block)
            control_flow_nodes.append(None)
        return control_flow_nodes, basic_blocks

    def _compute_backward_uses(self, basic_block_list):
        """
        Compute the backward uses for self._reference for the
        basic_block_list provided. This function will not work
        correctly if there is control flow inside the
        basic_block_list provided.
        The basic_block_list will be reversed to find the backward
        accesses.
        Reads to the reference that occur before a write will
        be added to the self._uses array, the earliest write will
        be provided as self._defsout and all previous writes
        will be inside self._killed.

        :param basic_block_list: The list of nodes that make up the basic
                                 block to find the forward uses in.
        :type basic_block_list: list[:py:class:`psyclone.psyir.nodes.Node`]

        :raises NotImplementedError: If a GOTO statement is found in the code
                                     region.
        """
        sig, _ = self._reference.get_signature_and_indices()
        # For a basic block we will only ever have one defsout
        defs_out = None
        # Working backwards so reverse the basic_block_list
        basic_block_list.reverse()
        stop_position = self._stop_point
        for region in basic_block_list:
            region_list = region.walk((Reference, Call, CodeBlock, Return))
            # If the region contains any Return, Exit or Cycle statements then
            # we modify the stop position to only look at statements that
            # occur before this statement.
            # This doesn't work correctly if the Reference that
            # is having its backwards dependencies analysed occurs after
            # one of these such statements in a basic block, however
            # since they're unreachable maybe we don't care?
            for reference in region_list:
                if isinstance(reference, Return):
                    stop_position = min(reference.abs_position, stop_position)
                if isinstance(reference, CodeBlock):
                    if isinstance(
                        reference._fp2_nodes[0], (Exit_Stmt, Cycle_Stmt)
                    ):
                        stop_position = min(
                            reference.abs_position, stop_position
                        )
        for region in basic_block_list:
            region_list = region.walk((Reference, Call, CodeBlock, Return))
            # Reverse the list
            region_list.reverse()
            for reference in region_list:
                # Store the position instead of computing it twice.
                abs_pos = reference.abs_position
                if abs_pos < self._start_point or abs_pos >= stop_position:
                    continue
                # If its parent is an inquiry function then its neither
                # a read nor write if its the first argument.
                if (isinstance(reference.parent, IntrinsicCall) and
                        reference.parent.is_inquiry and
                        reference.parent.arguments[0] is reference):
                    continue
                if isinstance(reference, CodeBlock):
                    # CodeBlocks only find symbols, so we can only do as good
                    # as checking the symbol - this means we can get false
                    # positives for structure accesses inside CodeBlocks.
                    if isinstance(reference._fp2_nodes[0], Goto_Stmt):
                        raise NotImplementedError(
                            "DefinitionUseChains can't handle code containing"
                            " GOTO statements."
                        )
                    if (
                        self._reference.symbol.name
                        in reference.get_symbol_names()
                    ):
                        # Assume the worst for a CodeBlock and we count them
                        # as killed and defsout and uses.
                        if defs_out is not None:
                            self._killed.append(defs_out)
                        defs_out = reference
                        continue
                elif isinstance(reference, Call):
                    # If its a local variable we can ignore it as we'll catch
                    # the Reference later if its passed into the Call.
                    if self._reference.symbol.is_automatic:
                        continue
                    if isinstance(reference, IntrinsicCall):
                        # IntrinsicCall can only do stuff to arguments, these
                        # will be caught by Reference walk already.
                        # Note that this assumes two symbols are not
                        # aliases of each other.
                        continue
                    if reference.is_pure:
                        # Pure subroutines only touch their arguments, so we'll
                        # catch the arguments that are passed into the call
                        # later as References.
                        continue
                    # For now just assume calls are bad if we have a non-local
                    # variable and we treat them as though they were a write.
                    if defs_out is not None:
                        self._killed.append(defs_out)
                    defs_out = reference
                    continue
                elif reference.get_signature_and_indices()[0] == sig:
                    # Work out if its read only or not.
                    assign = reference.ancestor(Assignment)
                    # RHS reads occur "before" LHS writes, so if we
                    # hit the LHS or an assignment then we won't have
                    # a dependency to the value used from the LHS.
                    if assign is not None:
                        if assign.lhs is reference:
                            # Check if the RHS contains the self._reference.
                            # Can't use in since equality is not what we want
                            # here.
                            found = False
                            for ref in assign.rhs.walk(Reference):
                                if (
                                    ref is self._reference
                                    and self._stop_point == ref.abs_position
                                ):
                                    found = True
                            # If the RHS contains the self._reference, then
                            # this LHS is "after" so we skip it
                            if found:
                                continue
                            # This is a write to the reference, so kill the
                            # previous defs_out and set this to be the
                            # defs_out.
                            if defs_out is not None:
                                self._killed.append(defs_out)
                            defs_out = reference
                        elif (
                            assign.lhs.get_signature_and_indices()[0] == sig
                            and assign.lhs is not self._reference
                        ):
                            # Reference is on the rhs of an assignment such as
                            # a = a + 1. Since we're looping through the tree
                            # walk in reverse, we find the a on the RHS of the
                            # statement before the a on the LHS. Since the LHS
                            # of the statement is a write to this symbol, the
                            # RHS needs to not be a dependency when working
                            # backwards.
                            continue
                        else:
                            # Read only, so if we've not yet set written to
                            # this variable this is a use. NB. We need to
                            # check the if the write is the LHS of the parent
                            # assignment and if so check if we killed any
                            # previous assignments.
                            if defs_out is None:
                                self._uses.append(reference)
                    elif reference.ancestor(Call):
                        # Otherwise we assume read/write access for now.
                        if defs_out is not None:
                            self._killed.append(defs_out)
                        defs_out = reference
                    else:
                        # Reference outside an Assignment - read only
                        # This could be References inside a While loop
                        # condition for example.
                        if defs_out is None:
                            self._uses.append(reference)
        if defs_out is not None:
            self._defsout.append(defs_out)

    def find_backward_accesses(self):
        """
        Find all the backward accesses for the reference defined in this
        DefinitionUseChain.
        Backward accesses are all of the prior References or Calls that read
        or write to the symbol of the reference up to the point that a
        write to the symbol is guaranteed to occur.
        DUC assumes that any control flow might not be taken, so writes
        that occur inside control flow do not end the backward access
        chain.

        :returns: the backward accesses of the reference given to this
                  DefinitionUseChain
        :rtype: list[:py:class:`psyclone.psyir.nodes.Node`]
        """
        # Compute the abs position caches as we'll use these a lot.
        # The compute_cached_abs_position will only do this if needed
        # so we don't need to check here.
        self._reference.compute_cached_abs_positions()

        # Setup the start and stop positions
        save_start_position = self._start_point
        save_stop_position = self._stop_point
        # If there is no set start point, then we look for all
        # accesses after the Reference.
        if self._stop_point is None:
            self._stop_point = self._reference_abs_pos
        # If there is no set stop point, then any Reference after
        # the start point can potentially be a forward access.
        if self._start_point is None:
            self._start_point = self._scope[0].abs_position
        if not self.is_basic_block:
            # If this isn't a basic block, then we find all of the basic
            # blocks.
            control_flow_nodes, basic_blocks = self._find_basic_blocks(
                self._scope
            )
            chains = []
            # Now add all the other standardly handled basic_blocks to the
            # list of chains.
            for block in basic_blocks:
                # If we have a basic block with no children then skip it,
                # e.g. for an if block with no code before the else
                # statement.
                if len(block) == 0:
                    continue
                chain = DefinitionUseChain(
                    self._reference,
                    block,
                    start_point=self._start_point,
                    stop_point=self._stop_point,
                )
                chains.append(chain)
            # If this is the top level access, we need to check if the
            # reference has an ancestor loop. If it does, we find the
            # highest ancestor Loop in the tree and add a
            # DefinitionUseChain block at the start to search for things
            # before the Reference that can also be looped back to.
            # We should probably have this be any top level time this is
            # called but thats hard to otherwise track.
            if (
                isinstance(self._scope[0], Routine)
                or self._scope[0] is self._reference.root
            ):
                # Check if there is an ancestor Loop/WhileLoop.
                ancestor = self._reference.ancestor((Loop, WhileLoop))
                while ancestor is not None:
                    # Create a basic block for the ancestor Loop.
                    body = ancestor.loop_body.children[:]
                    # Find the stop point - this needs to be the last node
                    # in the ancestor loop
                    sub_stop_point = ancestor.walk(Node)[-1].abs_position + 1
                    # We make a copy of the reference to have a detached
                    # node to avoid handling the special cases based on
                    # the parents of the reference.
                    if self._reference.ancestor(Assignment) is not None:
                        sub_start_point = self._reference.ancestor(
                            Assignment
                        ).abs_position
                    else:
                        sub_start_point = self._reference.abs_position
                    # If we have a basic block with no children then skip it,
                    # e.g. for an if block with no code before the else
                    # statement.
                    if len(body) > 0:
                        chain = DefinitionUseChain(
                            self._reference.copy(),
                            body,
                            start_point=sub_start_point,
                            stop_point=sub_stop_point,
                        )
                        chains.append(chain)
                        control_flow_nodes.append(ancestor)
                    # If its a while loop, create a basic block for the while
                    # condition.
                    if isinstance(ancestor, WhileLoop):
                        control_flow_nodes.append(None)
                        sub_stop_point = ancestor.loop_body.abs_position
                        chain = DefinitionUseChain(
                            self._reference.copy(),
                            [ancestor.condition],
                            start_point=ancestor.abs_position,
                            stop_point=sub_stop_point,
                        )
                        chains.append(chain)
                    ancestor = ancestor.ancestor((Loop, WhileLoop))

                # Check if there is an ancestor Assignment.
                ancestor = self._reference.ancestor(Assignment)
                if ancestor is not None:
                    # If the reference is not the lhs then we can ignore
                    # the RHS.
                    if ancestor.lhs is self._reference:
                        end = ancestor.walk(Node)[-1]
                        # Add the rhs as a potential basic block with
                        # different start and stop positions.
                        chain = DefinitionUseChain(
                            self._reference.copy(),
                            ancestor.rhs.children[:],
                            start_point=ancestor.rhs.abs_position,
                            stop_point=end.abs_position,
                        )
                        control_flow_nodes.append(None)
                        chains.append(chain)
                        # N.B. For now this assumes that for an expression
                        # b = a * a, that next_access to the first Reference
                        # to a should not return the second Reference to a.

            # For backwards we want to reverse the order.
            chains.reverse()
            control_flow_nodes.reverse()
            for i, chain in enumerate(chains):
                # Compute the defsout, killed and reaches for the block.
                chain.find_backward_accesses()
                cfn = control_flow_nodes[i]

                if cfn is None:
                    # We're outside a control flow region, updating the reaches
                    # here is to find all the reached nodes.
                    for ref in chain._reaches:
                        # Add unique references to reaches. Since we're not
                        # in a control flow region, we can't have added
                        # these references into the reaches array yet so
                        # they're guaranteed to be unique.
                        found = False
                        for ref2 in self._reaches:
                            if ref is ref2:
                                found = True
                                break
                        if not found:
                            self._reaches.append(ref)
                    # If we have a defsout in the chain then we can stop as we
                    # will never get past the write as its not conditional.
                    if len(chain.defsout) > 0:
                        # Reset the start and stop points before returning
                        # the result.
                        self._start_point = save_start_position
                        self._stop_point = save_stop_position
                        return self._reaches
                else:
                    # We assume that the control flow here could be 'not
                    # taken', i.e. that this doesn't kill the chain.
                    # TODO #2760: In theory we could analyse loop structures
                    # or if block structures to see if we're guaranteed to
                    # write to the symbol.
                    # If the control flow node is a Loop we have to check
                    # if the variable is the same symbol as the _reference.
                    if isinstance(cfn, Loop):
                        cfn_abs_pos = cfn.abs_position
                        if (
                            cfn.variable == self._reference.symbol
                            and cfn_abs_pos >= self._start_point
                            and cfn_abs_pos < self._stop_point
                        ):
                            # The loop variable is always written to and so
                            # we're done if its reached.
                            self._reaches.append(cfn)
                            self._start_point = save_start_position
                            self._stop_point = save_stop_position
                            return self._reaches

                    for ref in chain._reaches:
                        found = False
                        for ref2 in self._reaches:
                            if ref is ref2:
                                found = True
                                break
                        if not found:
                            self._reaches.append(ref)
        else:
            # Check if there is an ancestor Assignment.
            ancestor = self._reference.ancestor(Assignment)
            if ancestor is not None:
                # If we get here to check the start part of a loop we need
                # to handle this differently.
                # If the reference is the lhs then we can ignore the RHS.
                if ancestor.lhs is not self._reference:
                    pass
                elif ancestor.rhs is self._scope[0] and len(self._scope) == 1:
                    # If the ancestor RHS is the scope of this chain then we
                    # do nothing.
                    pass
                else:
                    # Add the rhs as a potential basic block with different
                    # start and stop positions.
                    chain = DefinitionUseChain(
                        self._reference,
                        [ancestor.rhs],
                        start_point=ancestor.rhs.abs_position,
                        stop_point=sys.maxsize,
                    )
                    # Find any backward_accesses in the rhs.
                    chain.find_backward_accesses()
                    for ref in chain._reaches:
                        self._reaches.append(ref)

            # We can compute the rest of the accesses
            self._compute_backward_uses(self._scope)
            for ref in self._uses:
                self._reaches.append(ref)
            # If this block doesn't kill any accesses, then we add
            # the defsout into the reaches array.
            if len(self.killed) == 0:
                for ref in self._defsout:
                    self._reaches.append(ref)
            else:
                # If this block killed any accesses, then the first element
                # of the killed writes is the access access that we're
                # dependent with.
                self._reaches.append(self.killed[0])

        # Reset the start and stop points before returning the result.
        self._start_point = save_start_position
        self._stop_point = save_stop_position
        return self._reaches
