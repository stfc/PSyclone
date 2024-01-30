# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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
# Modified: R. W. Ford, STFC Daresbury Lab
# Modified: J. Henrichs, Bureau of Meteorology

'''
This module provides the ACCUpdateTrans transformation that, on programs that
use accelerator devices without managed memory support, ensures that data is
kept up-to-date on the host for the execution of host code and that data
is returned to the device in time for the execution of compute regions.
'''

from psyclone.core import Signature
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import (Call, CodeBlock, IfBlock, Loop, Routine,
                                  Schedule, ACCEnterDataDirective,
                                  ACCKernelsDirective, ACCParallelDirective,
                                  Node, IntrinsicCall)
from psyclone.psyir.tools import DependencyTools
from psyclone.psyir.transformations import TransformationError

# We distinguish between two access modes, IN (read) and OUT (write).
IN, OUT = 0, 1


class ACCUpdateTrans(Transformation):
    '''
    Examines the supplied Schedule and adds OpenACC "update" directives
    for any data accessed outside of a kernels or parallel region.
    For example:

    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Routine
    >>> from psyclone.psyir.transformations import ACCUpdateTrans
    >>>
    >>> code = """
    ... subroutine run_it()
    ...   real :: a(10)
    ...   a(:) = 7.0
    ... end subroutine run_it"""
    >>>
    >>> psyir = FortranReader().psyir_from_source(code)
    >>> routine = psyir.walk(Routine)[0]
    >>>
    >>> # Add update directives
    >>> uptrans = ACCUpdateTrans()
    >>> uptrans.apply(routine)
    >>>
    >>> # Uncomment to see a text view of the new routine schedule
    >>> # print(routine.view())
    >>> print(FortranWriter()(routine))
    subroutine run_it()
      real, dimension(10) :: a
    <BLANKLINE>
      !$acc update if_present host(a)
      a(:) = 7.0
      !$acc update if_present device(a)
    <BLANKLINE>
    end subroutine run_it
    <BLANKLINE>

    '''
    # Tuple of OpenACC directives we ignore when traversing a schedule.
    _ACC_IGNORE = (ACCEnterDataDirective, )
    # Tuple of OpenACC compute directives delimiting possible device execution.
    _ACC_COMPUTE = (ACCParallelDirective, ACCKernelsDirective)

    def validate(self, node, options=None):
        '''
        Checks that it is valid to apply this transformation to the supplied
        schedule.

        :param node: the Schedule that is to be transformed.
        :type node: :py:class:`psyclone.psyir.nodes.Schedule`
        :param options: any options to this transformation.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the supplied node is not a Schedule.
        :raises TransformationError: if the supplied node is within \
            an OpenACC region.
        '''
        if not isinstance(node, Schedule):
            raise TransformationError(f"Expected a Schedule but got a node of "
                                      f"type '{type(node).__name__}'")

        if node.ancestor(self._ACC_COMPUTE):
            raise TransformationError(
                "Cannot apply the ACCUpdateTrans to nodes that are within "
                "an OpenACC compute region.")

        super().validate(node, options)

    def apply(self, node, options=None):
        '''
        Applies this transformation to the supplied Schedule. Identifies any
        regions of code outside of OpenACC regions and adds the necessary
        OpenACC update directives to ensure that the host and device copies of
        any variables are kept up-to-date.

        :param node: the Schedule that is to be transformed.
        :type node: :py:class:`psyclone.psyir.nodes.Schedule`
        :param options: any options to this transformation.
        :type options: Optional[Dict[str, Any]]

        '''
        self.validate(node, options)

        # Call the routine that recursively adds updates to all Schedules
        # within the supplied Schedule.
        self._add_updates_to_schedule(node)

    @staticmethod
    def _may_compute(node):
        '''This method returns True if the supplied node is a Call node. This
        is because a Call node may call a routine whose body (or part
        of its body) executes on the device. For consistency with
        earlier implementations it also returns True if the supplied
        node is an allocate or deallocate intrinsic.

        :param node: a PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Node`

        :returns: True if the supplied node is a Call node or an \
            allocate or deallocate intrinsic.
        :rtype: bool

        '''
        # pylint: disable=unidiomatic-typecheck
        if type(node) is Call or (
                isinstance(node, IntrinsicCall) and node.routine.name in
                ["ALLOCATE", "DEALLOCATE"]):
            return True
        return False

    def _add_updates_to_schedule(self, sched):
        '''
        Recursively identify those statements that are not being executed
        on the device and add any required OpenACC update directives.

        :param sched: the schedule of statements to process.
        :type sched: :py:class:`psyclone.psyir.nodes.Schedule`

        '''
        # We must walk through the Schedule and find those nodes
        # representing contiguous regions of code that are not
        # executed on the device. Any Call nodes are taken as
        # boundaries of such regions because it may be that (part of)
        # their bodies are executed on the device. Allocate and
        # deallocate intrinsics are also taken as boundaries of such
        # regions.
        node_list = []
        for child in sched[:]:
            if isinstance(child, self._ACC_IGNORE):
                continue
            found = False
            for node in child.walk(Node):
                if isinstance(node, self._ACC_COMPUTE) or \
                   self._may_compute(node):
                    found = True
                    break
            if not found:
                node_list.append(child)
            else:
                self._add_update_directives(node_list)
                node_list.clear()
                if self._may_compute(child):
                    # Conservatively add an update host statement just
                    # before the node.
                    self._add_update_directives([child])
                elif isinstance(child, IfBlock):
                    # Add any update host statements that are required due to
                    # (read) accesses within the condition of the If.
                    self._add_update_directives([child.condition])
                    # Recurse down into the if body
                    self._add_updates_to_schedule(child.if_body)
                    if child.else_body:
                        self._add_updates_to_schedule(child.else_body)
                elif isinstance(child, Loop):
                    # A loop on the host with code that may be on the device.
                    # The loop start, stop and step values could all
                    # potentially have been written to on the device.
                    self._add_update_directives([child.start_expr,
                                                 child.stop_expr,
                                                 child.step_expr])
                    # Recurse down into the loop body
                    self._add_updates_to_schedule(child.loop_body)

        # We've reached the end of the list of children - are there any
        # last nodes that represent computation on the host?
        self._add_update_directives(node_list)

    # pylint: disable=too-many-locals
    # pylint: disable=too-many-branches
    def _add_update_directives(self, node_list):
        # pylint: disable=too-many-locals, too-many-branches
        '''
        Adds the required OpenACC update directives before and after the nodes
        in the supplied list.

        :param node_list: the PSyIR nodes representing code executed on the \
                          host rather than the device.
        :type node_list: List[:py:class:`psyclone.psyir.nodes.Node`]

        '''
        if not node_list:
            return

        # TODO #1872: the lack of precise array access descriptions might
        # unnecessarily increase the data transfer volume.
        read_write_info = DependencyTools().get_in_out_parameters(node_list)
        inputs = set(read_write_info.signatures_read)
        outputs = set(read_write_info.signatures_written)

        # TODO #1872: as a workaround for the lack of precise array access
        # descriptions, we currently overapproximate dependencies by adding any
        # variables being written to the input set (in addition to the output
        # set). The aim is to avoid situations where the host only writes an
        # array slice and the respective update device directive would then
        # incorrectly overwrite the entire array on the device, including the
        # data outside the slice. If or when this is no longer needed, the step
        # of the algorithm below dealing with if stmts will need updating.
        inputs.update(outputs)

        # TODO #1732: for CodeBlock nodes, until we have semantic information
        # on the statements involved, we conservatively assume every referenced
        # variable is both an input and an output.
        # TODO #1872: CodeBlock.get_symbol_names might include function names
        # mistaken for array names. However, as these do not correspond to any
        # arrays, the compiler should be able to ignore them in the directive.
        for node in node_list:
            if isinstance(node, CodeBlock):
                for symbol_name in node.get_symbol_names():
                    inputs.add(Signature(symbol_name))
                    outputs.add(Signature(symbol_name))

        # Copy any data that is accessed by this host region to the host
        # (resp. device) if it is on the device (resp. host).
        # In this function, IN and OUT are used to determine the access mode of
        # data references on the host, as per the value of mode.
        for mode, host_sig in enumerate((inputs, outputs)):
            if mode == IN:
                node_index, node_offset = 0, 0
            elif mode == OUT:
                node_index, node_offset = -1, 1

            child = node_list[node_index]
            sched = child.parent
            # Since the supplied nodes may be the children of an IfBlock or a
            # Loop, we get the deepest ancestor that is a child of a Schedule.
            while not isinstance(sched, Schedule):
                child, sched = child.parent, sched.parent

            # While there are variables from this host region which have not
            # been placed in an update directive, we check if they need to be
            # placed at the current schedule, in which case they are, or
            # if we can bring them up to the next deepest ancestor schedule.
            # The main goal is to avoid placing update directives inside loop
            # statements, but the algorithm also tries to minimise the total
            # number of update directives by promoting the coalescence of
            # neighbouring directives.
            while host_sig:
                # Most conservative position for a new update directive at this
                # schedule, i.e. that closest to the host region. This is used
                # as a boundary to compute textual dependencies, i.e. between
                # a pair of statements not within a loop or the same iteration
                # of a loop, and loop-carried dependencies, i.e. between a pair
                # of statements on different iterations of a loop.
                # It is also the position for the update directive used by
                # those variables with textual dependencies.
                # Perhaps, eventually, specially if we adopt the async clause,
                # it may be beneficial to textually move update host directives
                # earlier within the schedule as much as legally possible to
                # leverage overlapping communication and computation.
                update_pos = sched.children.index(child) + node_offset

                if mode == IN:
                    beg, end = None, update_pos
                elif mode == OUT:
                    beg, end = update_pos, None

                # The inability to place variables in update directives in
                # outer schedules is determined by their involvement in textual
                # or loop-carried dependencies in the current schedule. First,
                # we find all statements which may incur data dependencies.
                text_dep_stmts = sched[beg:end]
                loop_dep_stmts = sched[end:beg]
                # Second, we find which of the variables in the host region
                # require synchronisation at the current schedule, i.e. those
                # that are used in the compute regions within those statements.
                text_sync = self._sig_to_sync(text_dep_stmts, host_sig, mode)
                loop_sync = self._sig_to_sync(loop_dep_stmts, host_sig, mode)

                # Those variables that require both textual and, if within a
                # loop, also loop-carried synchronisation, need only be updated
                # once and can be dropped from the latter set.
                loop_sync.difference_update(text_sync)

                # If textual synchronisation is required, those variables need
                # to be updated straight away. Indeed, for update device
                # directives, but not quite for update host directives, where
                # placing the directive earlier and adding the async clause
                # might perform better.
                self._place_update(sched, update_pos, text_sync, mode)
                host_sig.difference_update(text_sync)

                # All the remaining variables that need synchronisation at this
                # schedule can be placed in an update directive at the edge
                # of the schedule, promoting coalescence with other directives.
                update_pos = None

                # If within loop body, we must cover loop-carried dependencies.
                # If within if stmt, update device directives can only be moved
                # out, i.e. executed unconditionally, if an update host
                # directive for the same variable - which always exists as all
                # outputs are inputs - has already been moved out as well. This
                # happens if and only if there's no textually preceding kernel
                # within the if stmt which writes that variable. Since that
                # variable would appear in loop_sync as part of the outputs of
                # any textually preceding kernels, we may simply place an
                # update device directive inside the if statement for those
                # variables in loop_sync. loop_sync also includes the outputs
                # of this host region which are inputs of any textually
                # preceding kernels but, even though these might not need to
                # be updated here, i.e. conditionally, they can safely be.
                if isinstance(sched.parent, Loop) or \
                   isinstance(sched.parent, IfBlock) and mode == OUT:
                    self._place_update(sched, update_pos, loop_sync, mode)
                    host_sig.difference_update(loop_sync)

                # This schedule is the body of a routine and, at least until we
                # can do interprocedural analysis, this is the end of the road.
                if isinstance(sched, Routine):
                    self._place_update(sched, update_pos, host_sig, mode)
                    break

                # Move up the code tree to the next deepest ancestor schedule.
                child, sched = child.parent, sched.parent
                while not isinstance(sched, Schedule):
                    child, sched = child.parent, sched.parent

    def _sig_to_sync(self, dep_stmts, host_sig, mode):
        '''
        Retrieves, amongst the host region signatures in host_sig, those that
        require synchronisation because of accesses in the compute regions
        within the statements in dep_stmts.

        :param dep_stmts: list of statements which may include compute regions.
        :type dep_stmts: List[:py:class:`psyclone.psyir.nodes.Statement`]
        :param host_sig: access signature(s) that need to be synchronised \
                         with the accelerator device.
        :type host_sig: Set[:py:class:`psyclone.core.Signature`]
        :param int mode: the access mode from the point of view of the host,
                         either IN (read) or OUT (write).

        '''
        # If there is a statement (e.g. a call) among the dependent
        # statements that may launch device kernels, we conservatively
        # assume a dependency for all variables in the host region
        # regardless of access mode. For the performance of the Python
        # code there is an assumption here that _may_compute only
        # returns True for Call nodes.
        for stmt in dep_stmts:
            if any(self._may_compute(call) for call in stmt.walk(Call)):
                return host_sig.copy()

        # Set of all signatures in compute kernels that may require syncing.
        kern_sig = set()

        for stmt in dep_stmts:
            for acc in stmt.walk(self._ACC_COMPUTE):
                # Kernel outputs can be both input and output dependencies.
                # The latter is since we must guarantee no kernel write is
                # overwritten by an earlier host write whose update device
                # directive could appear later.
                kern_sig.update(acc.signatures[OUT])
                if mode == OUT:
                    kern_sig.update(acc.signatures[IN])

        return host_sig.intersection(kern_sig)

    def _place_update(self, sched, update_pos, host_sig, mode):
        '''
        Places, avoiding redundancy where possible, an update directive in the
        provided schedule, at the requested position, for the requested
        variables and access mode.

        :param sched: the schedule in which to add the directive.
        :type sched: :py:class:`psyclone.psyir.nodes.Schedule`
        :param int update_pos: where to place the directive in the schedule.
        :param host_sig: the access signature(s) that need to be synchronised \
                         with the accelerator device.
        :type host_sig: Set[:py:class:`psyclone.core.Signature`]
        :param int mode: the access mode from the point of view of the host,
                         either IN (read) or OUT (write).

        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.nemo import NemoInvokeSchedule
        if sched.ancestor(NemoInvokeSchedule, include_self=True):
            from psyclone.nemo import NemoACCUpdateDirective as \
                ACCUpdateDirective
        else:
            from psyclone.psyir.nodes import ACCUpdateDirective

        # Avoid rewriting the set of signatures on the caller.
        host_sig = host_sig.copy()

        # Specify the data movement direction with the right clause depending
        # on the access mode. When the update directive can be placed at the
        # edge of the schedule, the position of the directive, update_pos, is
        # undefined until this point.
        if mode == IN:
            direction = "host"
            if update_pos is None:
                update_pos = 0
        elif mode == OUT:
            direction = "device"
            if update_pos is None:
                update_pos = len(sched.children)

        # Check preceding and succeeding schedule positions for existing update
        # directives, coalescing with existing directives if possible.
        # If at the beginning or at the end of the schedule, we may go out of
        # bounds, thus the try statement.
        for update_offset in (-1, 0):
            try:
                neighbour_node = sched[update_pos + update_offset]
                if isinstance(neighbour_node, ACCUpdateDirective):
                    # Merge neighbour update directives in the same direction.
                    if neighbour_node.direction == direction:
                        neighbour_node.sig_set.update(host_sig)
                        host_sig.clear()
                    # Avoid updating the host just after updating the device.
                    else:
                        host_sig.difference_update(neighbour_node.sig_set)
            except IndexError:
                pass

        # Add the update directive to the schedule.
        if host_sig:
            update_dir = ACCUpdateDirective(host_sig, direction)
            sched.addchild(update_dir, update_pos)
