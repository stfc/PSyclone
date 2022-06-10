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
# Authors A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------
''' This module contains the implementation of the various Otter
transformations.'''

from psyclone.core import VariablesAccessInfo
from psyclone.errors import LazyString
from psyclone.psyGen import Transformation
from psyclone.psyir.transformations.chunk_loop_trans import ChunkLoopTrans
from psyclone.psyir.transformations.loop_trans import LoopTrans
from psyclone.psyir.transformations.region_trans import RegionTrans
from psyclone.psyir import nodes
from psyclone.psyir.nodes import Loop, Schedule, \
    OtterTraceSetupNode, OtterParallelNode, OtterTaskNode, \
    OtterTaskSingleNode, OtterLoopNode, OtterLoopIterationNode, \
    OtterSynchroniseChildrenNode, OtterSynchroniseDescendantTasksNode, \
    OtterTraceNode
from psyclone.psyir.transformations.transformation_error import \
        TransformationError

class OtterTraceSetupTrans(RegionTrans):
    '''
    TODO
    '''
    def __str__(self):
        rval = ("Adds a Otter Trace setup node to a region of code "
                "to enable Otter instrumentation of PSyclone")
        return rval

    def validate(self, node_list, options=None):
        return True

    def apply(self, nodes, options=None):
        '''TODO'''
        node_list = self.get_node_list(nodes)
        self.validate(node_list, options)

        # Get useful references
        parent = node_list[0].parent
        position = node_list[0].position

        otter_trace_node = OtterTraceSetupNode()
        parent.children.insert(position, otter_trace_node)
        for child in node_list:
            child.detach()
            otter_trace_node.children[0].addchild(child)


class OtterParallelTrans(RegionTrans):
    '''
    TODO
    '''
    def __str__(self):
        rval = "Adds a Otter Parallel node to a region of code"
        return rval

    def apply(self, nodes, options=None):
        '''TODO'''
        node_list = self.get_node_list(nodes)
        self.validate(node_list, options)

        # Get useful references
        parent = node_list[0].parent
        position = node_list[0].position

        otter_parallel_node = OtterParallelNode()
        parent.children.insert(position, otter_parallel_node)
        for child in node_list:
            child.detach()
            otter_parallel_node.children[0].addchild(child)


class OtterTaskloopTrans(LoopTrans):
    '''
    TODO
    '''
    def __str__(self):
        rval = "Adds a set of Otter Tasking nodes to a Loop by chunking."
        return rval

    def apply(self, node, options=None):
        '''TODO'''
        self.validate(node, options)

        # Chunk the Loop
        ctrans = ChunkLoopTrans()
        ctrans.apply(node)

        # Get the info about the loop and its position
        parent = node.parent
        position = node.position

        task_node = OtterTaskNode()
        parent.children.insert(position, task_node)
        node.detach()
        task_node.children[0].addchild(node)


class OtterTaskSingleTrans(RegionTrans):
    '''
    TODO
    '''
    def __str__(self):
        rval = "Adds a Otter TaskSingle node to a region of code"
        return rval

    def apply(self, nodes, options=None):
        '''TODO'''
        node_list = self.get_node_list(nodes)
        self.validate(node_list, options)

        # Get useful references
        parent = node_list[0].parent
        position = node_list[0].position

        otter_tasksingle_node = OtterTaskSingleNode()
        parent.children.insert(position, otter_tasksingle_node)
        for child in node_list:
            child.detach()
            otter_tasksingle_node.children[0].addchild(child)

class OtterLoopTrans(LoopTrans):
    '''
    TODO
    '''
    def __str__(self):
        rval = "Adds an Otter Loop node to a Loop"
        return rval

    def apply(self, node, options=None):
        '''
        TODO
        '''
        self.validate(node, options)

        parent = node.parent
        position = node.position

        otter_loop_node = OtterLoopNode()
        parent.children.insert(position, otter_loop_node)
        node.detach()
        otter_loop_node.children[0].addchild(node)
    
        otter_loopit_node = OtterLoopIterationNode()
        for child in node.children[3].children:
            child.detach()
            otter_loopit_node.children[0].addchild(child)
        node.children[3].addchild(otter_loopit_node)

class OtterSynchroniseChildrenTrans(Transformation):
    '''
    TODO
    '''
    def __str__(self):
        rval = ("Adds an Otter Synchronise Children Transformation after the"
                " supplied node.")
        return rval

    def apply(self, node, options=None):
        '''
        TODO
        '''
        self.validate(node, options)

        parent = node.parent
        position = node.position
    
        otter_sync_node = OtterSynchroniseChildrenNode()
        parent.children.insert(position+1, otter_sync_node)

class OtterSynchroniseRegionTrans(RegionTrans):
    '''
    TODO
    '''
    def __str__(self):
        rval =  ("Adds 'Otter Synchronise Children' calls to a region of code "
                "to satisfy data dependencies between 'Otter Task' calls")
        return rval

    @staticmethod
    def get_forward_dependence(taskloop, root):
        # root is OtterParallelNode
        # taskloop is a Loop containing OtterTaskNode
        # Check supplied the correct type for root
        if not isinstance(root, OtterParallelNode):
            raise TransformationError(f"Expected the root of the tree in which"
                                      f" to look for a forward dependence to "
                                      f"be an instance of OtterParallelNode"
                                      f", but was supplied an instance of "
                                      f"'{type(root).__name__}'")
        # We only look for specific types
        node_list = root.walk((Loop, OtterLoopNode,
                               OtterSynchroniseChildrenNode))
        # Find the taskloop's variable access info. We need to skip over the
        # Loop variable writes from the Loop, so we skip the Loop children.
        taskloop_vars = VariablesAccessInfo()
        for child in taskloop.walk(nodes.Node):
            if child is not taskloop and not isinstance(child,
                                                        (Schedule, Loop)):
                taskloop_vars.merge(VariablesAccessInfo(child))
        taskloop_signatures = taskloop_vars.all_signatures
        # Find our parent parallel region
        parent_parallel = taskloop.ancestor(OtterParallelNode)
        # Raise an error if there is no parent_parallel region
        if parent_parallel is None:
            fwriter = FortranWriter()
            # Since "Backslashes may not appear inside the expression
            # portions of f-strings" via PEP 498, use chr(10) for '\n'
            raise InternalError(
                    LazyString(lambda: f"No parent otter parallel node was "
                                       f"found for the taskloop region: "
                                       f"{fwriter(taskloop).rstrip(chr(10))}"))

        for node in node_list:
            if node.abs_position <= taskloop.abs_position:
                continue
            # Ignore any children of the taskloop directive
            anc = node.ancestor(Loop)
            if anc is taskloop:
                continue
            node_vars = None
            if (isinstance(node, OtterSynchroniseChildrenNode)):
                # If we find a synchronise children node, then it acts as a 
                # barrier for dependencies as well, so we return it
                return node
            if not isinstance(node, OtterSynchroniseChildrenNode):
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
                    return node
        # If we found no dependencies, then return that!
        return None

    @staticmethod
    def _eliminate_unneeded_dependencies(taskloop_positions,
                                         dependence_positions,
                                         dependence_nodes):
        '''
        Eliminates unneeded dependencies from a set of taskloop positions,
        dependence_positions and dependence_nodes for a region of code.

        :param taskloop_positions: positions of the taskloops.
        :type taskloop_positions: list of int
        :param dependence_positions: positions of the taskloops' dependencies.
        :type dependence_positions: list of int
        :param dependence_nodes: the nodes respresenting the forward \
                                 dependency of each taskloop node.
        :type dependence_nodes: list of :py:class:`psyclone.psyir.nodes.Node`

        :returns: the updated dependence_positions and dependence_nodes arrays
        :rval: 2-tuple of (list of int, list of
               :py:class:`psyclone.psyir.nodes.Node`)

        '''
        # We loop over each dependence and perform the same operation:
        # For each dependence we loop over all other
        # taskloops whose position is after this taskloop, but before
        # this taskloop's forward dependency. If that dependence is
        # fulfilled by this dependence (i.e.
        # dependence_position[this] < dependence_position[that]) then
        # we remove that dependence. If this dependence is fulfilled
        # by that dependence (i.e. dependence_position[that] <
        # dependence_position[this]) then we remove this dependence.
        # This assumes that taskloop_positions is in ascending order,
        # which I believe to be true by construction.
        for i, dep_pos in enumerate(dependence_positions):
            # Corresponding taskloop has no dependency or was satisfied
            # by another dependency being resolved.
            if dep_pos is None:
                continue
            # Grab the position of the next dependence
            next_dependence = dep_pos
            # Loop over the other taskloops
            for j in range(i+1, len(dependence_positions)):
                # If this taskloop happens after the next_dependence
                # then we can just stop.
                if taskloop_positions[j] >= next_dependence:
                    break
                # If the jth taskloop has no dependency then continue
                if dependence_positions[j] is None:
                    continue
                # Check if next_dependence will satisfy the jth
                # taskloops dependency
                if next_dependence <= dependence_positions[j]:
                    dependence_positions[j] = None
                    dependence_nodes[j] = None
                    continue
                # Check if the jth taskloop's dependence will satisfy
                # the next_dependence
                if dependence_positions[j] < next_dependence:
                    dependence_positions[i] = None
                    dependence_nodes[i] = None
                    # If it does then we can move to the next taskloop
                    break
        return dependence_positions, dependence_nodes

    def apply(self, node, options=None):
        '''
        Apply an OMPTaskwait Transformation to the supplied node
        (which must be an OMPParallelDirective). In the generated code this
        corresponds to adding zero or more OMPTaskwaitDirectives as
        appropriate:

        .. code-block:: fortran

          !$OMP PARALLEL
            ...
            !$OMP TASKWAIT
            ...
            !$OMP TASKWAIT
            ...
          !$OMP END PARALLEL

        :param node: the node to which to apply the transformation.
        :type node: :py:class:`psyclone.psyir.nodes.OMPParallelDirective`
        :param options: a dictionary with options for transformations\
                        and validation.
        :type options: dict of string:values or None
        :param bool options["fail_on_no_taskloop"]:
                indicating whether this should throw an error if no \
                OMPTaskloop nodes are found in this tree. This can be \
                safely disabled as if there are no Taskloop nodes the \
                result of this transformation is valid OpenMP code. Default \
                is True

        '''
        self.validate(node, options=options)

        # Find all of the taskloops
        taskloops = []
        for loop in node.walk(Loop):
            if loop.walk(OtterTaskNode):
                taskloops.append(loop)
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
                forward_dep = \
                        OtterSynchroniseRegionTrans.get_forward_dependence(
                        taskloop, node)
                # If the forward_dependence is one of our parents then we
                # should ignore it
                if (forward_dep is not None and
                        forward_dep.abs_position < taskloop.abs_position):
                    continue
            if forward_dep is None:
                continue
            dependence_position[i] = forward_dep.abs_position
            dependence_node[i] = forward_dep
        # Forward dependency positions are now computed for this region.
        dependence_position, dependence_node = \
            OtterSynchroniseRegionTrans._eliminate_unneeded_dependencies(
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
                loc = forward_dep.position
                # We've found the position, so we now insert an
                # OMPTaskwaitDirective in that location instead
                fdep_parent.addchild(OtterSynchroniseChildrenNode(), loc)

class OtterSynchroniseDescendantsTrans(RegionTrans):
    '''
    TODO
    '''
    def __str__(self):
        rval = ("Adds an Otter Synchronise Descendents node around the"
                " supplied nodes.")
        return rval

    def apply(self, nodes, options=None):
        '''TODO'''
        node_list = self.get_node_list(nodes)
        self.validate(node_list, options)

        parent = node_list[0].parent
        position = node_list[0].position

        otter_syncdecs_node = OtterSynchroniseDescendantTasksNode()
        parent.children.insert(position, otter_syncdecs_node)
        for child in node_list:
            child.detach()
            otter_syncdecs_node.children[0].addchild(child)


class OtterTraceStartEndTrans(RegionTrans):
    '''
    TODO
    '''
    def __str__(self):
        rval = ("Adds an Otter Trace Start and End node around the supplied "
                "nodes.")
        return rval

    def apply(self, nodes, options=None):
        '''TODO'''
        node_list = self.get_node_list(nodes)
        self.validate(node_list, options)

        parent = node_list[0].parent
        position = node_list[0].position

        otter_trace_node = OtterTraceNode()
        parent.children.insert(position, otter_trace_node)
        for child in node_list:
            child.detach()
            otter_trace_node.children[0].addchild(child)
