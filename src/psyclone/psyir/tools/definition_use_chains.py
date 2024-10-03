# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024-2024, Science and Technology Facilities Council.
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
# TODO Info

import sys

from psyclone.psyir.nodes import (
    Assignment,
    Call,
    CodeBlock,
    IfBlock,
    IntrinsicCall,
    Loop,
    Node,
    Reference,
    Routine,
    Schedule,
    Statement,
    WhileLoop,
)
from psyclone.psyir.symbols import AutomaticInterface


class DefinitionUseChain:
    """TODO"""

    def __init__(
        self,
        reference,
        control_flow_region=None,
        is_local=None,
        start_point=None,
        stop_point=None,
    ):
        # FIXME We should check if this is a reference probably.
        self._reference = reference
        # Store the absolute position for later.
        self._reference_abs_pos = reference.abs_position
        # To enable loops to work correctly we can set the start/stop point
        # and not just use base it on the reference's absolute position
        self._start_point = start_point
        self._stop_point = stop_point
        if control_flow_region is None:
            self._scope = [reference.ancestor(Routine)]
            if self._scope is None:
                self._scope = reference.root.children[:]
        else:
            # We need a list of regions for control flow.
            if not isinstance(control_flow_region, list):
                raise TypeError(
                    f"The control_flow_region passed into a "
                    f"DefinitionUseChain must be a list but found "
                    f"'{type(control_flow_region).__name__}'."
                )
            self._scope = control_flow_region
        if is_local is None:
            # TODO Work out if symbol is local to the scope or
            # globally accessible. Anything that isn't a local
            # variable in a Routine has to be assumed as globally
            # accessible for now.
            self._is_local = isinstance(
                reference.symbol.interface, AutomaticInterface
            )
        else:
            self._is_local = is_local

        # The uses, defsout and killed sets as defined for each basic block.
        self._uses = []
        self._defsout = []
        self._killed = []

        # The output map, mapping between nodes and the reach of that node.
        self._reaches = []
        self._writes = []

    @property
    def uses(self):
        """
        :returns: the list of nodes used by this DefinitionUseChain.
        :rtype: List[:py:class:`psyclone.psyir.nodes.Node`]
        """
        return self._uses

    @property
    def defsout(self):
        """
        :returns: the list of output nodes computed by this DefinitionUseChain.
        :rtype: List[:py:class:`psyclone.psyir.nodes.Node`]
        """
        return self._defsout

    @property
    def killed(self):
        """
        :returns: the list of killed output nodes computed by this
                  DefinitionUseChain.
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
        # This is a basic block if there is no control flow inside this scope
        # then it is a basic block.
        # In PSyclone, possible control flow nodes are IfBlock, Loop
        # and WhileLoop.
        for node in self._scope:
            c_f_nodes = node.walk((IfBlock, Loop, WhileLoop))
            if len(c_f_nodes) > 0:
                return False
        return True

    def find_forward_accesses(self):
        """
        Find all the forward accesses for the reference defined in this
        DefinitionUseChain.
        Forward accesses are all of the References (or Calls) that read
        or write to the symbol of the reference up to the point that a
        write to the symbol is guaranteed to occur.
        PSyclone only assumes a write to the symbol is guaranteed to occur
        if it occurs outside of a control flow region.

        :returns: the forward accesses of the reference given to this
                  DefinitionUseChain
        :rtype: list[:py:class:`psyclone.psyir.nodes.Node`]
        """
        # FIXME If all defsout is in control flow we should add a None into
        # the defsout array.
        # Find the position of the Reference's highest-level parent in
        # the Routine.
        routine = self._reference.ancestor(Routine)
        if routine is None:
            # Handle subtrees without a routine
            routine = self._reference.root
        node = self._reference
        while node.depth > routine.depth + 1:
            node = node.parent

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
            # If this isn't a basic block, the we find all the basic blocks.
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
                if ancestor is not None:
                    next_ancestor = ancestor.ancestor((Loop, WhileLoop))
                    while next_ancestor is not None:
                        ancestor = next_ancestor
                        next_ancestor = ancestor.ancestor((Loop, WhileLoop))
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
                    # We make a copy of the reference to have a detached
                    # node to avoid handling the special cases based on
                    # the parents of the reference.
                    chain = DefinitionUseChain(
                        self._reference.copy(),
                        body,
                        self._is_local,
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
                            self._is_local,
                            start_point=ancestor.abs_position,
                            stop_point=sub_stop_point,
                        )
                        chains.insert(0, chain)

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
                            self._is_local,
                            start_point=ancestor.lhs.abs_position - 1,
                            stop_point=ancestor.lhs.abs_position + 1,
                        )
                        control_flow_nodes.append(None)
                        chains.append(chain)
                        # TODO For now I'm assuming the for an expression
                        # b = a * a, that next_access to the first Reference
                        # to a should not return the second Reference to a.
            # Now add all the other standardly handled basic_blocks to the
            # list of chains.
            for block in basic_blocks:
                chain = DefinitionUseChain(
                    self._reference,
                    block,
                    self._is_local,
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
                        # Add unique references to reaches. Can't just check
                        # with "in" as unique references can be equal if
                        # they're basic references to the same symbol.
                        found = False
                        for ref2 in self._reaches:
                            if ref2 is ref:
                                found = True
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
                    # We assume that the control flow here is "optional", i.e.
                    # that this doesn't kill the chain.
                    # In theory we could analyse loop structures or if block
                    # structures to see if we're guaranteed to write to the
                    # symbol.
                    # If the control flow node is a Loop we have to check
                    # if the variable is the same symbol as the _reference.
                    if isinstance(cfn, Loop):
                        if cfn.variable == self._reference.symbol:
                            # The loop variable is always written to and so
                            # we're done if its reached.
                            self._reaches.append(cfn)
                            self._start_point = save_start_position
                            self._stop_point = save_stop_position
                            return self._reaches

                    for ref in chain._reaches:
                        # Add unique references to reaches. Can't just check
                        # with "in" as unique references can be equal if
                        # they're basic references to the same symbol.
                        found = False
                        for ref2 in self._reaches:
                            if ref2 is ref:
                                found = True
                        if not found:
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
                        self._is_local,
                        start_point=ancestor.lhs.abs_position - 1,
                        stop_point=ancestor.lhs.abs_position + 1,
                    )
                    # Find any forward_accesses in the lhs.
                    chain.find_forward_accesses()
                    for ref in chain._reaches:
                        # Add unique references to reaches. Can't just check
                        # with "in" as unique references can be equal if
                        # they're basic references to the same symbol.
                        found = False
                        for ref2 in self._reaches:
                            if ref2 is ref:
                                found = True
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
            # We can compute the rest of the accesses
            self._compute_forward_uses(self._scope)
            for ref in self._uses:
                # Add unique references to reaches. Can't just check with
                # in as unique references can be equal if they're basic
                # references to the same symbol.
                found = False
                for ref2 in self._reaches:
                    if ref2 is ref:
                        found = True
                if not found:
                    self._reaches.append(ref)
            # If this block doesn't kill any accesses, then we add
            # the defsout into the reaches array.
            if len(self.killed) == 0:
                for ref in self._defsout:
                    found = False
                    for ref2 in self._reaches:
                        if ref2 is ref:
                            found = True
                    if not found:
                        self._reaches.append(ref)
            else:
                # TODO I think we should only add the first killed reference?
                # Not sure, I think we probably should but we need to use
                # some complex control flow before I'm sure.
                for ref in self.killed:
                    found = False
                    for ref2 in self._reaches:
                        if ref2 is ref:
                            found = True
                    if not found:
                        self._reaches.append(ref)

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
        FIXME should we check and raise an exception?
        Reads to the reference that occur before a write will
        be added to the self._uses array, the final write will
        be provided as self._defsout and all previous writes
        will be inside self._killed.

        :param basic_block_list: The list of nodes that make up the basic
                                 block to find the forward uses in.
        :type basic_block_list: list[:py:class:`psyclone.psyir.nodes.Node`]
        """
        sig, _ = self._reference.get_signature_and_indices()
        # For a basic block we will only ever have one defsout
        defs_out = None
        for region in basic_block_list:
            for reference in region.walk((Reference, Call, CodeBlock)):
                # Store the position instead of computing it twice.
                abs_pos = reference.abs_position
                if abs_pos <= self._start_point or abs_pos >= self._stop_point:
                    continue
                if isinstance(reference, CodeBlock):
                    # CodeBlocks only find symbols, so we can only do as good
                    # as checking the symbol - this means we can get false
                    # positives for structure accesses inside CodeBlocks.
                    if self._reference.symbol in reference.get_symbol_names():
                        # Assume the worst for a CodeBlock and we count them
                        # as killed and defsout and uses.
                        if defs_out is None:
                            self._uses.append(reference)
                        if defs_out is not None:
                            self._killed.append(defs_out)
                        defs_out = reference
                elif isinstance(reference, Call):
                    # If its a local variable we can ignore it as we'll catch
                    # the Reference later if its passed into the Call.
                    if self._is_local:
                        continue
                    if isinstance(reference, IntrinsicCall):
                        # IntrinsicCall can only do stuff to arguments, these
                        # will be caught by Reference walk already.
                        # Note that this assumption two symbols are not
                        # aliases of each other.
                        continue
                    # For now just assume calls are bad if we have a non-local
                    # variable and we count them as killed and defsout
                    # and uses.
                    if defs_out is None:
                        self._uses.append(reference)
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
                        # It has a Call ancestor so assume read/write access
                        # for now.
                        # We can do better for IntrinsicCalls realistically.
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
        # We want to expand Schedules if they're in the nodelist.
        # TODO I don't like this, can we instead expand this when placing it
        # in? At the moment if we do this it breaks the is_basic_block check
        # as we end up with a list inside a list.
        # Maybe this is ok now?
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
                control_flow_nodes.append(node)
                basic_blocks.append(node.if_body.children[:])
                # Check if the node is in the if_body
                in_if_body = False
                # FIXME We could potentially optimise this loop if its
                # expensive
                refs = node.if_body.walk(Reference)
                for ref in refs:
                    if ref is self._reference:
                        in_if_body = True
                        break
                if node.else_body and not in_if_body:
                    control_flow_nodes.append(node)
                    basic_blocks.append(node.else_body.children[:])
            else:
                # This is a basic node, add it to the current block
                current_block.append(node)
        if len(current_block) > 0:
            basic_blocks.append(current_block)
            control_flow_nodes.append(None)
        return control_flow_nodes, basic_blocks
