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

from psyclone.psyGen import Transformation
from psyclone.psyir.transformations.chunk_loop_trans import ChunkLoopTrans
from psyclone.psyir.transformations.loop_trans import LoopTrans
from psyclone.psyir.transformations.region_trans import RegionTrans
from psyclone.psyir.nodes import Loop, Schedule, \
    OtterTraceSetupNode, OtterParallelNode, OtterTaskNode, \
    OtterTaskSingleNode, OtterLoopNode, OtterLoopIterationNode, \
    OtterSynchroniseChildrenNode, OtterSynchroniseDescendantTasksNode, \
    OtterTraceNode

class OtterTraceSetupTrans(RegionTrans):
    '''
    TODO
    '''
    def __str__(self):
        rval = ("Adds a Otter Trace setup node to a region of code"
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
        rval = "Adds a set of Otter Tasking nodes to a Loop"
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

class OtterSynchroniseDescendantsTrans(RegionTrans):
    '''
    TODO
    '''
    def ___str__(self):
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
    def __str(self):
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
