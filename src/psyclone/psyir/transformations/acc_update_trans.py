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
from psyclone.configuration import Config
from psyclone.psyir.nodes import (Call, IfBlock, Loop, Schedule, Operation,
                                  BinaryOperation, ACCKernelsDirective,
                                  ACCParallelDirective, ACCUpdateDirective,
                                  ACCEnterDataDirective, CodeBlock, Routine)
from psyclone.psyir.tools import DependencyTools
from psyclone.psyGen import Transformation


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
                                  Call, CodeBlock)
        loop_type_mapping = Config.get().api_conf("nemo") \
                                        .get_loop_type_mapping()
        self._loop_vars = loop_type_mapping.keys()
        # Those fparser2 nodes that may occur inside a CodeBlock but represent
        # accesses that do not require any data synchronisation between CPU
        # and GPU.
        self._fp_ignore_nodes = (Fortran2003.Allocate_Stmt,
                                 Fortran2003.Deallocate_Stmt,
                                 Fortran2003.Inquire_Stmt)

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

        if node.ancestor((ACCParallelDirective, ACCKernelsDirective)):
            raise TransformationError(
                "Cannot apply the ACCUpdateTrans to nodes that are already "
                "within an OpenACC region.")

        if not options or not options.get("allow-codeblocks", False):
            if node.walk(CodeBlock):
                raise TransformationError(
                    "Supplied Schedule contains one or more CodeBlocks which "
                    "prevents the data analysis required for adding Update "
                    "directives.")

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

        if options and options.get("allow-codeblocks", False):
            excluded_nodes = tuple(list(self._acc_region_nodes) + [CodeBlock])
        else:
            excluded_nodes = self._acc_region_nodes

        routine = node.ancestor(Routine, include_self=True)
        if routine:
            self._routine_name = routine.name
        else:
            self._routine_name = ""

        # Call the routine that recursively adds updates to all Schedules
        # within the supplied Schedule.
        self._add_updates_to_schedule(node, excluded_nodes)

    def _add_updates_to_schedule(self, sched, excluded_nodes):
        '''
        Recursively identify those statements that are not being executed
        on the GPU and add any required OpenACC update directives.

        :param sched: the schedule of statements to process.
        :type sched: :py:class:`psyclone.psyir.nodes.Schedule`

        '''
        # We must walk through the Schedule and find those nodes representing
        # contiguous regions of code that are not executed on the GPU. Any
        # Call nodes are taken as boundaries of such regions because it may
        # be that the body of the call is executed on the GPU.
        node_list = []
        for child in sched.children[:]:
            if isinstance(child, ACCEnterDataDirective):
                continue
            if not child.walk(excluded_nodes):
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
                        ptree = Fortran2003.Stop_Stmt(
                            f"STOP 'PSyclone: {self._routine_name}: manually "
                            f"add ACC update statements for the temporaries "
                            f"required by the following Call'")
                        sched.addchild(CodeBlock(
                            [ptree], CodeBlock.Structure.STATEMENT),
                                       index=child.position)
                if isinstance(child, IfBlock):
                    # Add any update statements that are required due to
                    # (read) accesses within the condition of the If.
                    self._add_update_directives(sched, [child.condition])
                    # Recurse down
                    self._add_updates_to_schedule(child.if_body,
                                                  excluded_nodes)
                    if child.else_body:
                        self._add_updates_to_schedule(child.else_body,
                                                      excluded_nodes)
                if isinstance(child, Loop):
                    # We have a loop that is on the CPU but contains code
                    # that could be on the GPU.
                    # The loop start, stop and step values could all
                    # potentially have been written to on the GPU.
                    self._add_update_directives(sched, [child.start_expr,
                                                        child.stop_expr,
                                                        child.step_expr])
                    # Recurse down into the loop body
                    self._add_updates_to_schedule(child.loop_body,
                                                  excluded_nodes)
                if isinstance(child, CodeBlock):
                    # All is not lost if we encounter a CodeBlock as we may
                    # be able to infer that it doesn't require any data
                    # transfers.
                    fp_nodes = child.get_ast_nodes
                    if all(isinstance(fp_node, self._fp_ignore_nodes)
                           for fp_node in fp_nodes):
                        continue
                    # We don't currently handle CodeBlocks so we inject code
                    # to abort the execution.
                    ptree = Fortran2003.Stop_Stmt(
                        f"STOP 'PSyclone: {self._routine_name}: manually add "
                        f"ACC update statements (if required) for the "
                        f"following CodeBlock...'")
                    sched.addchild(CodeBlock([ptree],
                                             CodeBlock.Structure.STATEMENT),
                                   index=child.position)
                    # If this is not the last child in the Schedule then we
                    # can add a comment to show where the CodeBlock ends.
                    if child is not sched.children[-1]:
                        sibling = sched.children[child.position+1]
                        sibling.preceding_comment = "...CodeBlock ends here."

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
            # Since the supplied nodes may be the children of an IfBlock or
            # Loop, we have to search up to find the ancestor that *is* a
            # child of the Schedule we are working on.
            first_child = node_list[0]
            while first_child not in parent.children:
                first_child = first_child.parent
            sig_list = []
            for sig in inputs:
                if sig.var_name in self._loop_vars:
                    continue
                sig_list.append(sig)
            if sig_list:
                update_dir = ACCUpdateDirective(sig_list, "host")
                parent.addchild(update_dir, parent.children.index(first_child))
        # Copy any data that is written by this region and that is on the GPU
        # back to the GPU.
        if outputs:
            # Since the supplied nodes may be the children of an IfBlock or
            # Loop, we have to search up to find the ancestor that *is* a
            # child of the Schedule we are working on.
            last_child = node_list[-1]
            while last_child not in parent.children:
                last_child = last_child.parent
            sig_list = []
            for sig in outputs:
                if sig.var_name in self._loop_vars:
                    continue
                sig_list.append(sig)
            if sig_list:
                update_dir = ACCUpdateDirective(sig_list, "device")
                parent.addchild(update_dir,
                                parent.children.index(last_child)+1)
