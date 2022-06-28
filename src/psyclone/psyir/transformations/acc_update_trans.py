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
from psyclone.psyGen import InvokeSchedule, Transformation
from psyclone.psyir.nodes import (Call, IfBlock, Loop, Routine, Schedule,
                                  ACCEnterDataDirective,
                                  ACCKernelsDirective, ACCParallelDirective)
from psyclone.psyir.nodes.acc_directives import _sig_tools
from psyclone.psyir.tools import DependencyTools


class ACCUpdateTrans(Transformation):
    '''
    Examines the supplied Schedule and adds "acc update" directives
    for any data accessed outside of a kernels or parallel region.

    '''
    def __init__(self):
        # Perform some set-up required by the recursive routine.
        self._dep_tools = DependencyTools()
        self._acc_nodes = (ACCParallelDirective, ACCKernelsDirective)

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
        '''
        # transformations module file needs moving into the psyir
        # hierarchy.
        # pylint: disable=import-outside-toplevel
        from psyclone.transformations import TransformationError

        if not isinstance(node, Schedule):
            raise TransformationError(f"Expected a Schedule but got a node of "
                                      f"type '{type(node).__name__}'")

        if node.ancestor((ACCParallelDirective, ACCKernelsDirective)):
            raise TransformationError(
                "Cannot apply the ACCUpdateTrans to nodes that are already "
                "within an OpenACC region.")

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
        # contiguous regions of code that are executed on the CPU.
        cpu_stmts = []
        for stmt in sched.children[:]:
            if isinstance(stmt, ACCEnterDataDirective):
                continue
            elif not stmt.walk(self._acc_nodes):
                cpu_stmts.append(stmt)
            else:
                # After a sequence of pure CPU statements, update the GPU
                if cpu_stmts:
                    self._add_update_directives_gpu(sched, cpu_stmts)
                    cpu_stmts = []

                if isinstance(stmt, self._acc_nodes):
                    # If a compute construct is found, update the CPU
                    self._add_update_directives_cpu(sched, stmt)
                else:
                    # Recurse down the schedule tree
                    if isinstance(stmt, IfBlock):
                        self._add_updates_to_schedule(stmt.if_body)
                        if stmt.else_body:
                            self._add_updates_to_schedule(stmt.else_body)
                    elif isinstance(stmt, Loop):
                        self._add_updates_to_schedule(stmt.loop_body)

        # We've reached the end of the list of children - are there any
        # last nodes that represent computation on the CPU?
        if cpu_stmts:
            self._add_update_directives_gpu(sched, cpu_stmts)

        # At the end of a routine, we exit cleanly by updating the CPU
        if isinstance(sched, InvokeSchedule):
            self._add_update_directives_cpu(sched, None)

    def _add_update_directives_cpu(self, sched, direc):
        '''
        Adds the required update directive after the supplied compute
        construct.

        :param sched: the schedule which contains the node in direc.
        :type sched: :py:class:`psyclone.psyir.nodes.Schedule`
        :param stmts: the PSyIR node representing code executed on the GPU.
        :type node_list: list of :py:class:`psyclone.psyir.nodes.Node`

        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.nemo import NemoInvokeSchedule
        if sched.ancestor(NemoInvokeSchedule, include_self=True):
            from psyclone.nemo import NemoACCUpdateDirective as \
                ACCUpdateDirective

        acc_outputs = set([kref for kref in direc.out_kernel_references])
        
        # Capture the inputs of the dependent CPU statements unless we find a
        # call statement, in which case we assume all GPU outputs need updating 
        dependent_stmts = self._dependent_stmts(direc)
        nacc_stmts = self._non_acc_stmts(dependent_stmts)
        if all(stmt.walk(Call) is None for stmt in nacc_stmts):
            cpu_inputs, _ = DependencyTools().get_in_out_parameters(nacc_stmts)
            acc_outputs.intersection_update(cpu_inputs)

        # Add the directive just after the compute construct
        update_dir = ACCUpdateDirective(acc_outputs, 'host')
        sched.addchild(update_dir, sched.children.index(direc)+1)

    def _add_update_directives_gpu(self, sched, stmts):
        '''
        Adds the required update directive after the statements in the supplied
        list.

        :param sched: the schedule which contains the nodes in stmts.
        :type sched: :py:class:`psyclone.psyir.nodes.Schedule`
        :param stmts: the PSyIR nodes representing code executed on the \
                      CPU rather than the GPU.
        :type node_list: list of :py:class:`psyclone.psyir.nodes.Node`

        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.nemo import NemoInvokeSchedule
        if sched.ancestor(NemoInvokeSchedule, include_self=True):
            from psyclone.nemo import NemoACCUpdateDirective as \
                ACCUpdateDirective

        dependent_stmts = self._dependent_stmts(stmts[-1])
        acc_directives = [cdir for stmt in dependent_stmts 
                               for cdir in stmt.walk(self._acc_nodes)]
        acc_inputs = set([kref for cdir in acc_directives
                               for kref in cdir.in_kernel_references])

        # Capture the outputs of the CPU statements unless we find a call
        # statement, in which case we assume all GPU inputs need updating 
        if all(stmt.walk(Call) is None for stmt in stmts):
            _, cpu_outputs = DependencyTools().get_in_out_parameters(stmts)
            acc_inputs.intersection_update(cpu_outputs)

        # Add the directive just after the last CPU statement
        update_dir = ACCUpdateDirective(acc_inputs, 'device')
        sched.addchild(update_dir, sched.children.index(stmts[-1])+1)

    def _dependent_stmts(self, stmt):
        outermost_loop = None
        while not isinstance(stmt.parent, InvokeSchedule):
            if isinstance(stmt, Loop):
                outermost_loop = stmt
            stmt = stmt.parent

        index = stmt.parent.children.index(stmt)
        dependent_stmts = stmt.parent.children[index+1:]

        if outermost_loop:
            dependent_stmts += outermost_loop.loop_body

        return dependent_stmts

    def _non_acc_stmts(self, stmts):
        non_acc_stmts = []
        for stmt in stmts:
            if not stmt.walk(self._acc_nodes):
                non_acc_stmts.append(stmt)
            else:
                non_acc_stmts.extend(self._non_acc_stmts(stmt.children))
        
        return non_acc_stmts

