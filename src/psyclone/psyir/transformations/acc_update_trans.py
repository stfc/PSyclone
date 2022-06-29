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
from fparser.two import Fortran2003
from psyclone.core import Signature
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import (Call, CodeBlock, IfBlock, Loop, Routine,
                                  Schedule,
                                  ACCEnterDataDirective, ACCUpdateDirective,
                                  ACCKernelsDirective, ACCParallelDirective)
from psyclone.psyir.tools import DependencyTools


class ACCUpdateTrans(Transformation):
    '''
    Examines the supplied Schedule and adds "acc update" directives
    for any data accessed outside of a kernels or parallel region.

    '''
    def __init__(self):
        # Perform some set-up required by the recursive routine.
        self._dep_tools = DependencyTools()
        self._acc_regions = (ACCParallelDirective, ACCKernelsDirective)
        self._breakpoints = (Call, CodeBlock)

        super().__init__()

    def validate(self, node, options=None):
        '''
        Checks that it is valid to apply this transformation to the supplied
        schedule.

        :param node: the Schedule that is to be transformed.
        :type node: :py:class:`psyclone.psyir.nodes.Schedule`
        :param options: any options to this transformation.
        :type options: dict

        :raises TransformationError: if the supplied node is not a Schedule.
        :raises TransformationError: if the supplied node is already within \
            an OpenACC region.
        :raises TransformationError: if the supplied Schedule contains any \
            CodeBlocks and the 'allow-codeblocks' option is False.
        '''
        # transformations module file needs moving into the psyir
        # hierarchy.
        # pylint: disable=import-outside-toplevel
        from psyclone.transformations import TransformationError

        if not isinstance(node, Schedule):
            raise TransformationError(f"Expected a Schedule but got a node of "
                                      f"type '{type(node).__name__}'")

        if node.ancestor(self._acc_regions):
            raise TransformationError(
                "Cannot apply the ACCUpdateTrans to nodes that are already "
                "within an OpenACC compute region.")

        super().validate(node, options)

    def apply(self, node, options=None):
        '''
        Applies this transformation to the supplied Schedule. Identifies any
        regions of code outside of ACC regions and adds the necessary ACC
        update directives to ensure that the host and device copies of any
        variables are kept up-to-date.

        :param node: the Schedule that is to be transformed.
        :type node: :py:class:`psyclone.psyir.nodes.Schedule`
        :param options: any options to this transformation.
        :type options: dict

        '''
        self.validate(node, options)

        routine = node.ancestor(Routine, include_self=True)
        self._routine_name = routine.name if routine else ""

        # Call the routine that recursively adds updates to all Schedules
        # within the supplied Schedule.
        self._add_updates_to_schedule(node)

    def _add_updates_to_schedule(self, sched):
        '''
        Recursively identify those statements that are not being executed
        on the GPU and add any required OpenACC update directives.

        :param sched: the schedule of statements to process.
        :type sched: :py:class:`psyclone.psyir.nodes.Schedule`

        '''
        # We must walk through the Schedule and find those nodes representing
        # contiguous regions of code that are not executed on the GPU. Any
        # Call and CodeBlock nodes are taken as boundaries of such regions
        # because it may be that their bodies are executed on the GPU.
        node_list = []
        for child in sched.children[:]:
            if isinstance(child, ACCEnterDataDirective):
                continue
            elif not child.walk(self._acc_regions + self._breakpoints):
                node_list.append(child)
            else:
                self._add_update_directives(sched, node_list)
                node_list.clear()
                if isinstance(child, self._breakpoints):
                    self._add_update_directives(sched, [child])
                elif isinstance(child, IfBlock):
                    # Add any update statements that are required due to
                    # (read) accesses within the condition of the If.
                    self._add_update_directives(sched, [child.condition])
                    # Recurse down into the if body
                    self._add_updates_to_schedule(child.if_body)
                    if child.else_body:
                        self._add_updates_to_schedule(child.else_body)
                elif isinstance(child, Loop):
                    # A loop on the CPU with code that may be on the GPU.
                    # The loop start, stop and step values could all
                    # potentially have been previously written to on the GPU.
                    self._add_update_directives(sched, [child.start_expr,
                                                        child.stop_expr,
                                                        child.step_expr])
                    # Recurse down into the loop body
                    self._add_updates_to_schedule(child.loop_body)

        # We've reached the end of the list of children - are there any
        # last nodes that represent computation on the CPU?
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
        # pylint: disable=import-outside-toplevel
        from psyclone.nemo import NemoInvokeSchedule
        if sched.ancestor(NemoInvokeSchedule, include_self=True):
            from psyclone.nemo import NemoACCUpdateDirective as \
                ACCUpdateDirective

        if not node_list:
            return

        inputs, outputs = self._dep_tools.get_in_out_parameters(node_list)

        # Workaround for CodeBlocks
        # TODO We are probably relying on undefined behaviour here
        for node in node_list:
            if isinstance(node, CodeBlock):
                for symbol_name in node.get_symbol_names():
                    inputs.append(Signature(symbol_name))
                    outputs.append(Signature(symbol_name))

        # Copy any data that is read by this region to the host (resp. device)
        # if it is on the device (resp. host).
        for idx, inouts in enumerate((inputs, outputs)):
            if not inouts:
                continue
            if idx == 0:   # inputs
                node_index = 0
                node_offset = 0
                direction = "host"
            elif idx == 1: # outputs
                node_index = -1
                node_offset = 1
                direction = "device"
            # Since the supplied nodes may be the children of an IfBlock or
            # Loop, we have to search up to find the ancestor that *is* a
            # child of the Schedule we are working on.
            child = node_list[node_index]
            while child not in sched.children:
                child = child.parent
            
            update_pos = sched.children.index(child) + node_offset
            sig_list = set(inouts)

            # Avoid repeating variables in a neighbouring update directive
            if update_pos < len(sched.children):
                preceding_node = sched.children[update_pos - 1]
                if isinstance(preceding_node, ACCUpdateDirective):
                    sig_list.difference_update(preceding_node.sig_list)

            if sig_list:
                update_dir = ACCUpdateDirective(sig_list, direction)
                sched.addchild(update_dir, update_pos)
