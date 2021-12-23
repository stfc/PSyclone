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
# Author: A. R. Porter, STFC Daresbury Lab

'''
This module provides the ACCUpdateTrans transformation that ensures that
data is kept up-to-date on the host.
'''

from __future__ import absolute_import
from psyclone.configuration import Config
from psyclone.psyir.nodes import (Call, IfBlock, Loop, Schedule, Operation,
                                  BinaryOperation, ACCKernelsDirective,
                                  ACCParallelDirective, ACCUpdateDirective,
                                  ACCEnterDataDirective)
from psyclone.psyir.tools import DependencyTools
from psyclone.psyGen import Transformation
#, TransformationError


class ACCUpdateTrans(Transformation):
    '''
    Examines the supplied Schedule and adds "acc update" directives
    for any data accessed outside of a kernels or parallel region.

    '''
    def __init__(self):
        # Perform some set-up required by the recursive routine.
        self._dep_tools = DependencyTools()
        # We treat a Call as being a (potential) ACC region because we
        # don't know what happens within it.
        self._acc_region_nodes = (ACCParallelDirective, ACCKernelsDirective,
                                  Call)
        loop_type_mapping = Config.get().api_conf("nemo") \
                                        .get_loop_type_mapping()
        self._loop_vars = loop_type_mapping.keys()

    def validate(self, sched, options=None):
        ''' '''
        if not isinstance(sched, Schedule):
            raise TransformationError()
        super().validate(sched, options)

    @staticmethod
    def get_forward_dependence(taskloop, root):
        '''
        Returns the next forward dependence for a taskloop using the
        dependence-analysis functionality provided by
        psyclone.psyir.tools.dependency_tools.
        Forward dependencies can be of the following types:
        Loop
        OMPDoDirective
        OMPTaskloopDirective
        OMPTaskwaitDirective (If in same OMPSingle and that single has
        nowait=False)
        OMPSingleDirective (If ancestor OMPSingle has nowait=False)

        Loop, OMPDoDirective, OMPTaskloopDirective types are returned when
        a following directive is found which has a RaW, WaR or WaW dependency
        to taskloop.

        An OMPTaskwaitDirective type is returned when a following directive
        is found inside the same parent OMPSingleDirective which has no
        nowait clause applied.

        An OMPSingleDirective type is returned when the first dependency is
        within a different OMPSerialDirective, and the ancestor of taskloop
        is an OMPSingleDirective with no nowait clause.

        The forward dependency is never a child of taskloop, and must have
        abs_position > taskloop.abs_position

        :param taskloop: the taskloop node for which to find the \
                         forward_dependence.
        :type taskloop: :py:class:`psyclone.psyir.nodes.OMPTaskloopDirective`
        :param root: the tree in which to search for the forward_dependence.
        :type root: :py:class:`psyclone.psyir.nodes.OMPParallelDirective`

        :returns: the forward_dependence of taskloop.
        :rtype: :py:class:`psyclone.f2pygen.Node`

        '''
        # Check supplied the correct type for root
        if not isinstance(root, OMPParallelDirective):
            raise TransformationError("Expected the root of the tree in which "
                                      "to search for a forward dependence to "
                                      "be an instance of OMPParallelDirective,"
                                      " but was supplied an instance of '{0}'"
                                      .format(type(root).__name__))
        # We only look for specific types
        node_list = root.walk((Loop, OMPDoDirective, OMPTaskloopDirective,
                               OMPTaskwaitDirective))
        # Find the taskloop's variable access info. We need to skip over the
        # Loop variable writes from the Loop, so we skip the Loop children.
        taskloop_vars = VariablesAccessInfo()
        for child in taskloop.walk(nodes.Node):
            if child is not taskloop and not isinstance(child,
                                                        (Schedule, Loop)):
                taskloop_vars.merge(VariablesAccessInfo(child))
        taskloop_signatures = taskloop_vars.all_signatures
        # Find our parent serial region if it has a barrier
        parent_single = taskloop.ancestor(OMPSingleDirective)
        # Cache the parent single for use in later if statements
        cached_parent_single = parent_single
        # If the parent single region has a nowait clause it can never
        # act as the dependency for this taskloop, so we set it to None.
        # The same behaviour occurs implicitly if the parent
        # OMPSerialDirective is an OMPMasterDirective. Without a blocking
        # parent region we can never guarantee synchronicity if dependencies
        # are outside of the parent region.
        if parent_single is not None and parent_single.nowait:
            parent_single = None
        # Find our parent parallel region
        parent_parallel = taskloop.ancestor(OMPParallelDirective)
        # Raise an error if there is no parent_parallel region
        if parent_parallel is None:
            fwriter = FortranWriter()
            raise InternalError(
                    LazyString(lambda: "No parent parallel directive was "
                                       "found for the taskloop region: {0}"
                               .format(fwriter(taskloop).rstrip("\n"))))

        for node in node_list:
            if node.abs_position <= taskloop.abs_position:
                continue
            # Ignore any children of the taskloop directive
            anc = node.ancestor(OMPTaskloopDirective)
            if anc is taskloop:
                continue
            node_vars = None
            if (isinstance(node, OMPTaskwaitDirective) and
                    (cached_parent_single is
                     node.ancestor(OMPSingleDirective)) and
                    cached_parent_single is not None and
                    cached_parent_single.nowait is False):
                # If we find a taskwait barrier inside the same
                # OMPSingleDirective and that OMPSingleDirective has a no
                # nowait clause, then it acts as a barrier for dependencies
                # as well, so we return it
                return node
            if not isinstance(node, OMPTaskwaitDirective):
                # For all our other node types we calculate their own
                # variable accesses
                node_vars = VariablesAccessInfo()
                for child in node.walk(nodes.Node):
                    if child is not node and not isinstance(child,
                                                            (Schedule, Loop)):
                        refs = VariablesAccessInfo(child)
                        if refs is not None:
                            node_vars.merge(refs)
            node_signatures = node_vars.all_signatures
            # Once we have the node's variable accesses, check for collisions
            for sig1 in taskloop_signatures:
                # If this signature is not in the node signatures then continue
                if sig1 not in node_signatures:
                    continue
                access1 = taskloop_vars[sig1]
                access2 = node_vars[sig1]
                # If both are only read we can ignore this signature
                # Otherwise, one of them writes so return this node as
                # we have a WaW, WaR or RaW dependency
                if access1.is_written() or access2.is_written():
                    # If we have a different parent serial node, and
                    # parent_single is not None then our parent_single is our
                    # dependency, otherwise this node is the dependency
                    if (taskloop.ancestor(OMPSerialDirective) is not
                            node.ancestor(OMPSerialDirective) and
                            parent_single is not None):
                        return parent_single
                    return node
        # If we found no dependencies, then return that!
        return None

    def apply(self, sched, options=None):
        ''' '''
        self.validate(sched, options)
        self.add_updates(sched)
        return

        # Find all the regions that access (or may access) data on the GPU
        acc_regions = sched.walk(self._acc_region_nodes)

        # Loop over the ACC regions
        for task_region in acc_regions:
            create_endtaskwait = False
            endwaits = []
            # Find all of the taskloops
            taskloops = task_region.walk(OMPTaskloopDirective)
            # Get the positions of all of the taskloops
            taskloop_positions = [-1] * len(taskloops)
            # Get the forward_dependence position of all of the taskloops
            dependence_position = [None] * len(taskloops)
            dependence_node = [None] * len(taskloops)
            for i, taskloop in enumerate(taskloops):
                taskloop_positions[i] = taskloop.abs_position
                # Only set forward_dep for taskloops with nogroup set
                forward_dep = None
                if taskloop.nogroup:
                    forward_dep = OMPTaskwaitTrans.get_forward_dependence(
                            taskloop, node)
                    # If the forward_dependence is one of our parents then we
                    # should ignore it
                    if (forward_dep is not None and
                            forward_dep.abs_position < taskloop.abs_position):
                        # If we're in a blocking single region and the
                        # dependency for any of its tasks points to its
                        # parent single region, then its "real" next dependency
                        # is outside of the single region. To ensure we
                        # synchronize before this dependency, we must have a
                        # task synchronization construct before the spawning
                        # thread leaves the single region. It is possible that
                        # this dependency could be handled by another
                        # intermediary taskwait, however I can't work out a
                        # good way to detect that now (since there is no
                        # "end of single region" abs_position to map to).
                        # I could store a list of all the taskloops that
                        # require this final taskwait, and walk from each one
                        # to see if a taskwait has been placed to satisfy their
                        # dependency, but that does not seem elegant somehow.
                        if forward_dep is task_region:
                            endwaits.append(taskloop)
                            create_endtaskwait = True
                        continue
                if forward_dep is None:
                    continue
                # Check if the taskloop and its forward dependence are in the
                # same serial region
                if (taskloops[i].ancestor(OMPSerialDirective) is
                        forward_dep.ancestor(OMPSerialDirective)):
                    # We're in the same OMPSerialDirective so store the
                    # position of the forward dependency
                    dependence_position[i] = forward_dep.abs_position
                    dependence_node[i] = forward_dep
            # Forward dependency positions are now computed for this region.
            dependence_position, dependence_node = \
                OMPTaskwaitTrans._eliminate_unneeded_dependencies(
                            taskloop_positions, dependence_position,
                            dependence_node)
            # dependence_position now contains only the required dependencies
            # to satisfy the full superset of dependencies. We can loop over
            # these by index, and if dependence_position[i] is not None then
            # go to its forward dependency, and insert a TaskwaitDirective
            # immediately before.
            for i, dep_pos in enumerate(dependence_position):
                if dep_pos is not None:
                    forward_dep = dependence_node[i]
                    fdep_parent = forward_dep.parent
                    # Find the position of the forward_dep in its parent's
                    # children list
                    loc = fdep_parent.children.index(forward_dep)
                    # We've found the position, so we now insert an
                    # OMPTaskwaitDirective in that location instead
                    fdep_parent.addchild(OMPTaskwaitDirective(), loc)
            if create_endtaskwait:
                # For each taskloop that needed a taskwait at the end
                for taskloop in endwaits:
                    taskloop_pos = taskloop.abs_position
                    # Find all the taskwaits in the task_region
                    node_list = task_region.walk(OMPTaskwaitDirective)
                    # If every taskwait appears before this taskloop then
                    # add a taskwait at the end of the serial region and
                    # stop.
                    if all(node1.abs_position < taskloop_pos
                            for node1 in node_list):
                        task_region.dir_body.addchild(OMPTaskwaitDirective())
                        break

    def add_updates(self, sched):
        '''
        Recursively identify those statements that are not being executed
        on the GPU and add any required OpenACC update directives.

        :param sched: the schedule of statements to process.
        :type sched: :py:class:`psyclone.psyir.nodes.Schedule`

        '''
        # We must walk through the Schedule and find those nodes that are
        # not within a kernels or parallel region.
        node_list = []
        acc_node_list = []
        for child in sched.children[:]:
            if isinstance(child, ACCEnterDataDirective):
                continue
            #if isinstance(child, Call):
                # We have to handle a Call separately as, provided its
                # arguments do not involve expressions, it does not actually
                # read from them. However, they may
                # have been written to on the GPU and therefore, if they are
                # accessed within this schedule on the CPU they will need to
                # be pulled back.
            #    pass
            if not child.walk(self._acc_region_nodes):
                node_list.append(child)
            else:
                if node_list:
                    self._add_update_directives(sched, node_list)
                    node_list = []
                if isinstance(child, Call):
                    ops = child.walk(Operation)
                    if any(op.operator not in [BinaryOperation.Operator.LBOUND,
                                               BinaryOperation.Operator.UBOUND]
                           for op in ops):
                        raise NotImplementedError("ARPDBG4")
                    pass
                if isinstance(child, (IfBlock, Loop)):
                    # TODO: Update node_list with nodes from IfBlock condition
                    # or Loop bounds/step
                    if isinstance(child, IfBlock):
                        self._add_update_directives(sched, [child.condition])
                        # Recurse down
                        self.add_updates(child.if_body)
                        if child.else_body:
                            self.add_updates(child.else_body)
        # We've reached the end of the list of children - are there any
        # last nodes that represent computation on the CPU?
        if node_list:
            self._add_update_directives(sched, node_list)

    def _add_update_directives(self, sched, node_list):
        '''
        Adds the required update directives before and after the nodes in
        the supplied list.

        :param sched: the schedule which contains the nodes in node_list.
        :type sched: :py:class:`psyclone.psyir.nodes.Schedule`
        :param node_list: the PSyIR nodes representing code executed on the \
                          CPU rather than the GPU.
        :type node_list: list of :py:class:`psyclone.psyir.nodes.Node`

        '''
        inputs, outputs = self._dep_tools.get_in_out_parameters(node_list)
        parent = sched
        # Copy any data that is read by this region to the host if it is
        # on the GPU.
        if inputs:
            first_child = node_list[0]
            while first_child not in parent.children:
                first_child = first_child.parent
            for sig in inputs:
                if sig.is_structure:
                    raise NotImplementedError("ARPDBG2")
                if sig.var_name in self._loop_vars:
                    continue
                sym = sched.symbol_table.lookup(sig.var_name)
                update_dir = ACCUpdateDirective(sym, "host")
                parent.addchild(update_dir,
                                parent.children.index(first_child))
        # Copy any data that is written by this region back to the GPU.
        if outputs:
            last_child = node_list[-1]
            while last_child not in parent.children:
                last_child = last_child.parent
            for sig in outputs:
                if sig.is_structure:
                    raise NotImplementedError("ARPDBG3")
                sym = sched.symbol_table.lookup(sig.var_name)
                if sig.var_name in self._loop_vars:
                    continue
                update_dir = ACCUpdateDirective(sym, "device")
                parent.addchild(update_dir,
                                parent.children.index(last_child)+1)
