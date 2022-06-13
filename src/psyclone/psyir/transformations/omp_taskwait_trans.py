# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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
# Modified: R. W. Ford and N. Nobre, STFC Daresbury Lab

''' This module provides the OMPTaskwaitTrans transformation that can be
applied to an OMPParallelDirective to satisfy any task-based dependencies
created by OpenMP Taskloops.'''
from __future__ import absolute_import, print_function

from psyclone.core import VariablesAccessInfo
from psyclone.errors import LazyString, InternalError
from psyclone.psyGen import Transformation
from psyclone.psyir import nodes
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import Loop, Schedule, \
    OMPDoDirective, OMPTaskloopDirective, OMPSerialDirective, \
    OMPTaskwaitDirective, OMPSingleDirective, OMPParallelDirective
from psyclone.psyir.transformations.transformation_error import \
        TransformationError


class OMPTaskwaitTrans(Transformation):
    '''
    Adds zero or more OpenMP Taskwait directives to an OMP parallel region.
    This transformation will add directives to satisfy dependencies between
    Taskloop directives without an associated taskgroup (i.e. no nogroup
    clause). It also tries to minimise the number added to maximise available
    parallelism.

    For example:

    >>> from pysclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "gocean1.0"
    >>> filename = "nemolite2d_alg.f90"
    >>> ast, invokeInfo = parse(filename, api=api, invoke_name="invoke")
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.transformations import OMPParallelTrans, OMPSingleTrans
    >>> from psyclone.transformations import OMPTaskloopTrans
    >>> from psyclone.psyir.transformations import OMPTaskwaitTrans
    >>> singletrans = OMPSingleTrans()
    >>> paralleltrans = OMPParallelTrans()
    >>> tasklooptrans = OMPTaskloopTrans()
    >>> taskwaittrans = OMPTaskwaitTrans()
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> print(schedule.view())
    >>>
    >>> # Apply the OpenMP Taskloop transformation to *every* loop
    >>> # in the schedule.
    >>> # This ignores loop dependencies. These are handled by the
    >>> # taskwait transformation.
    >>> for child in schedule.children:
    >>>     tasklooptrans.apply(child, nogroup = true)
    >>> # Enclose all of these loops within a single OpenMP
    >>> # SINGLE region
    >>> singletrans.apply(schedule.children)
    >>> # Enclose all of these loops within a single OpenMP
    >>> # PARALLEL region
    >>> paralleltrans.apply(schedule.children)
    >>> taskwaittrans.apply(schedule.children)
    >>> print(schedule.view())

    '''
    def __str__(self):
        rval = ("Adds 'OpenMP TASKWAIT' directives to an OpenMP parallel "
                "region to satisfy 'OpenMP TASKLOOP' dependencies")
        return rval

    def validate(self, node, options=None):
        '''
        Validity checks for input arguments.

        :param node: the OMPParallelDirective node to validate.
        :type node: :py:class:`psyclone.psyir.nodes.OMPParallelDirective`
        :param options: a dictionary with options for transformations.
        :type options: dict of string:values or None
        :param bool options["fail_on_no_taskloop"]:
                indicating whether this should throw an error if no \
                OMPTaskloop nodes are found in this tree. This can be \
                safely disabled as if there are no Taskloop nodes the \
                result of this transformation is valid OpenMP code. Default \
                is True.

        :raises TransformationError: If the supplied node is not an \
                                     OMPParallelDirective
        :raises TransformationError: If there are no OMPTaskloopDirective \
                                     nodes in this tree.
        :raises TransformationError: If taskloop dependencies can't be \
                                     satisfied due to dependencies across \
                                     barrierless OpenMP Serial Regions.
        '''
        fail_on_no_taskloop = True
        if options is not None:
            fail_on_no_taskloop = options.get("fail_on_no_taskloop", True)
        # Check the supplied node is an OMPParallelDirective
        if not isinstance(node, nodes.OMPParallelDirective):
            raise TransformationError(f"OMPTaskwaitTrans was supplied a "
                                      f"'{node.__class__.__name__}'"
                                      f" node, but expected an "
                                      f"OMPParallelDirective")

        # Find all the taskloops
        taskloops = node.walk(OMPTaskloopDirective)

        # Walk the tree to find any OMPTaskloopTrans
        if fail_on_no_taskloop and taskloops == []:
            raise TransformationError("OMPTaskwaitTrans was supplied an "
                                      "OMPParallelDirective that does not "
                                      "contain any OMPTaskloopDirectives")

        # Check that all of the dependencies are satisfiable
        for taskloop in taskloops:
            # Find the first RaW or WaR dependency for this taskloop.
            forward_dep = OMPTaskwaitTrans.get_forward_dependence(taskloop,
                                                                  node)
            if forward_dep is None:
                continue
            # Check if the taskloop and its forward dependence are in the
            # same serial region
            if (taskloop.ancestor(OMPSerialDirective) is not
                    forward_dep.ancestor(OMPSerialDirective)):
                # They're not in the same serial region. Check if our
                # taskloop is in a waiting Single Directive
                ancestor = taskloop.ancestor(OMPSerialDirective)
                valid = isinstance(ancestor, nodes.OMPSingleDirective)
                if valid:
                    valid = not ancestor.nowait
                # If not valid, then we're in a Master Directive or a
                # Single Directive with nowait. In this case we can't
                # safely guarantee our forward dependency so throw an error
                if not valid:
                    fwr = FortranWriter()
                    # pylint: disable=cell-var-from-loop
                    # Since "Backslashes may not appear inside the expression
                    # portions of f-strings" via PEP 498, use chr(10) for '\n'
                    raise TransformationError(LazyString(
                                lambda: f"Couldn't satisfy the dependencies "
                                        f"due to taskloop dependencies across "
                                        f"barrierless OMP serial regions. "
                                        f"Dependency is from\n"
                                        f"{fwr(taskloop).rstrip(chr(10))}"
                                        f"\nto\n"
                                        f"{fwr(forward_dep).rstrip(chr(10))}"))

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
            raise TransformationError(f"Expected the root of the tree in which"
                                      f" to look for a forward dependence to "
                                      f"be an instance of OMPParallelDirective"
                                      f", but was supplied an instance of "
                                      f"'{type(root).__name__}'")
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
            # Since "Backslashes may not appear inside the expression
            # portions of f-strings" via PEP 498, use chr(10) for '\n'
            raise InternalError(
                    LazyString(lambda: f"No parent parallel directive was "
                                       f"found for the taskloop region: "
                                       f"{fwriter(taskloop).rstrip(chr(10))}"))

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

        # Find all the OpenMP Single & Master regions
        task_regions = node.walk(OMPSerialDirective)

        # Loop over the task regions
        for task_region in task_regions:
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
                    loc = forward_dep.position
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
