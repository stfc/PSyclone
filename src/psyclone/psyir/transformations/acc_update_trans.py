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
# Author: A. R. Porter and N. Nobre, STFC Daresbury Lab

'''
This module provides the ACCUpdateTrans transformation that ensures that
data is kept up-to-date on the host.
'''

from psyclone.core import Signature
from psyclone.psyGen import InvokeSchedule, Transformation
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
        # Assume CodeBlocks do not call routines executed on the GPU
        self._brk_nodes = (Call, )

        super().__init__()

    def validate(self, node, options=None):
        '''
        Checks that it is valid to apply this transformation to the supplied
        schedule.

        :param node: the Schedule that is to be transformed.
        :type node: :py:class:`psyclone.psyir.nodes.Schedule`
        :param options: any options to this transformation.
        :type options: Optional[Dict[str, str]]

        :raises TransformationError: if the supplied node is not a Schedule.
        :raises TransformationError: if the supplied node is already within \
            an OpenACC region.
        '''
        # transformations module file needs moving into the psyir hierarchy.
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
        :type options: Optional[Dict[str, str]]

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
        # Call nodes are taken as boundaries of such regions
        # because it may be that their bodies are executed on the GPU.
        node_list = []
        for child in sched.children[:]:
            if isinstance(child, ACCEnterDataDirective):
                continue
            elif not child.walk(self._acc_regions + self._brk_nodes):
                node_list.append(child)
            else:
                self._add_update_directives(node_list)
                node_list.clear()
                if isinstance(child, self._brk_nodes):
                    # Conservatively add an update host statement just before
                    # the Call node for fear of temporary operands.
                    self._add_update_directives([child])
                elif isinstance(child, IfBlock):
                    # Add any update statements that are required due to
                    # (read) accesses within the condition of the If.
                    self._add_update_directives([child.condition])
                    # Recurse down into the if body
                    self._add_updates_to_schedule(child.if_body)
                    if child.else_body:
                        self._add_updates_to_schedule(child.else_body)
                elif isinstance(child, Loop):
                    # A loop on the CPU with code that may be on the GPU.
                    # The loop start, stop and step values could all
                    # potentially have been previously written to on the GPU.
                    self._add_update_directives([child.start_expr,
                                                 child.stop_expr,
                                                 child.step_expr])
                    # Recurse down into the loop body
                    self._add_updates_to_schedule(child.loop_body)

        # We've reached the end of the list of children - are there any
        # last nodes that represent computation on the CPU?
        self._add_update_directives(node_list)

    def _add_update_directives(self, node_list):
        '''
        Adds the required update directives before and after the nodes in
        the supplied list.

        :param sched: the schedule which contains the nodes in node_list.
        :type sched: :py:class:`psyclone.psyir.nodes.Schedule`
        :param node_list: the PSyIR nodes representing code executed on the \
                          CPU rather than the GPU.
        :type node_list: list of :py:class:`psyclone.psyir.nodes.Node`

        '''
        if not node_list:
            return

        inputs, outputs = self._dep_tools.get_in_out_parameters(node_list)
        inputs, outputs = set(inputs), set(outputs)

        # Workaround for lack of precise access descriptions, e.g. in the
        # presence of array slices we currently overapproximate dependencies.
        inputs.update(outputs)

        # Workaround for CodeBlocks
        # TODO We are probably relying on undefined behaviour here.
        for node in node_list:
            if isinstance(node, CodeBlock):
                for symbol_name in node.get_symbol_names():
                    inputs.add(Signature(symbol_name))
                    outputs.add(Signature(symbol_name))

        # Copy any data that is read by this region to the host (resp. device)
        # if it is on the device (resp. host).
        IN, OUT = 0, 1
        for idx, cpu_sig in enumerate((inputs, outputs)):
            if not cpu_sig:
                continue
            if idx == IN:     # inputs
                node_index = 0
                node_offset = 0
                direction = "host"
                tentative_update_pos = "beg"
            elif idx == OUT:  # outputs
                node_index = -1
                node_offset = 1
                direction = "device"
                tentative_update_pos = "end"

            # Since the supplied nodes may be the children of an IfBlock or
            # Loop, we have to search up to find the ancestor that *is* a
            # child of a Schedule.
            child = node_list[node_index]
            sched = child.parent
            while not isinstance(sched, Schedule):
                child, sched = child.parent, sched.parent

            while cpu_sig:
                update_pos = sched.children.index(child) + node_offset

                if idx == IN:
                    beg, end = None, update_pos
                elif idx == OUT:
                    beg, end = update_pos, None

                text_dep_stmts = sched.children[beg:end]
                text_sig, text_sync = set(), set()

                loop_dep_stmts = sched.children[end:beg]
                loop_sig, loop_sync = set(), set()

                for dep_stmts, sig, sync in [
                     (text_dep_stmts, text_sig, text_sync),
                     (loop_dep_stmts, loop_sig, loop_sync)]:
                    for stmt in dep_stmts:
                        for acc in stmt.walk(self._acc_regions):
                            # Workaround for lack of precise access
                            # descriptions, used trivial condition as reminder.
                            if idx == IN or idx == OUT:
                                sig.update(acc.out_kernel_references)
                            if idx == OUT:
                                sig.update(acc.in_kernel_references)
                    # If there are data dependencies or there is a statement
                    # (e.g a call) that requires synchronisation.
                    sync.update(cpu_sig.intersection(sig))
                    if any(stmt.walk(self._brk_nodes) for stmt in dep_stmts):
                        sync.update(cpu_sig)

                loop_sync.difference_update(text_sync)

                # If textual synchronisation is required, those variables need
                # to be updated straight away.
                self._place_update(sched, update_pos, text_sync, direction)
                cpu_sig.difference_update(text_sync)

                update_pos = tentative_update_pos

                # If within loop body, we must cover loop-carried dependencies.
                # If within if stmt, update device directives can only be moved
                # out if an update host directive for the same variable - which
                # always exists as all outputs are inputs - has already been
                # moved out as well. This happens iff there's no textually
                # preceding kernel within the if stmt which writes that
                # variable which would appear in loop_sync as part of the
                # inputs of all textually preceding kernels.
                if isinstance(sched.parent, Loop) or \
                   isinstance(sched.parent, IfBlock) and idx == OUT:
                    self._place_update(sched, update_pos, loop_sync, direction)
                    cpu_sig.difference_update(loop_sync)

                # This schedule is the body of a routine and, at least until we
                # can do interprocedural analysis, this is the end of the road.
                if isinstance(sched, InvokeSchedule):
                    self._place_update(sched, update_pos, cpu_sig, direction)
                    break

                while True:
                    child, sched = child.parent, sched.parent
                    if isinstance(sched, Schedule):
                        break

    def _place_update(self, sched, update_pos, cpu_sig, direction):
        '''
        Places, avoiding redundancy where possible, an update directive in the
        provided schedule, at the requested position, for the requested
        variables and direction.

        :param sched: the schedule in which to add the directive.
        :type sched: :py:class:`psyclone.psyir.nodes.Schedule`
        :param int update_pos: where to place the directive in the schedule.
        :param cpu_sig: the access signature(s) that need to be synchronised \
                        with the accelerator.
        :type cpu_sig: Set[:py:class:`psyclone.core.Signature`]
        :param str direction: the direction of the synchronisation.

        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.nemo import NemoInvokeSchedule
        if sched.ancestor(NemoInvokeSchedule, include_self=True):
            from psyclone.nemo import NemoACCUpdateDirective as \
                ACCUpdateDirective

        cpu_sig = cpu_sig.copy()

        if update_pos == "beg":
            update_pos = 0
        elif update_pos == "end":
            update_pos = len(sched.children)

        for update_offset in (-1, 0):
            try:
                neighbour_node = sched.children[update_pos + update_offset]
                if isinstance(neighbour_node, ACCUpdateDirective):
                    # Merge neighbour update directives in the same direction.
                    if neighbour_node.direction == direction:
                        neighbour_node.sig_set.update(cpu_sig)
                        cpu_sig.clear()
                    # Avoid updating the host just after updating the device.
                    else:
                        cpu_sig.difference_update(neighbour_node.sig_set)
            except IndexError:
                pass

        if cpu_sig:
            update_dir = ACCUpdateDirective(cpu_sig, direction)
            sched.addchild(update_dir, update_pos)
