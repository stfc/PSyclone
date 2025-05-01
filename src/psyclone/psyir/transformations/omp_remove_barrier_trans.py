# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025, Science and Technology Facilities Council.
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
# Authors: A. B. G. Chalk, STFC Daresbury Lab
'''Contains the OMPRemoveBarrierTrans.'''

# TODO #2837: Once we leave python 3.8 we can use list instead of List for
# type hints.
from typing import List

from psyclone.psyir.nodes import (
    Directive,
    Routine, OMPDoDirective,
    OMPBarrierDirective, OMPTaskwaitDirective,
    OMPTargetDirective,
    IfBlock, Node, Loop
)
from psyclone.psyir.transformations.transformation_error import \
    TransformationError
from psyclone.psyir.transformations.region_trans import RegionTrans
from psyclone.psyir.transformations.async_trans_mixin import \
    AsyncTransMixin


# FIXME Does this need to be RegionTrans?
class OMPRemoveBarrierTrans(RegionTrans, AsyncTransMixin):
    '''
    Attempts to remove OMPTaskwaitDirective or
    OMPBarrierDirective nodes from a supplied region as long as
    any dependencies continue to be satisfied. The goal is to reduce
    the number of synchronisation points to enable better asynchronous
    execution of loops or kernels.

    For example:
    ...
    '''
    # Inherites from AsyncTransMixin as it needs some of the helper functions
    def __str__(self) -> str:
        return ("Removes OMPTaskwaitDirective or OMPBarrierDirective nodes "
                "from the supplied region to reduce synchronicity without "
                "invalidating dependencies.")

    def validate(self, node: Routine) -> None:
        '''
        Validity check for the input arguments.

        :param node: the routine to try to remove barriers from.
        :raises TypeError: if the supplied input isn't a Routine.
        '''
        if not isinstance(node, Routine):
            raise TypeError(f"OMPRemoveBarrierTrans expects a Routine input "
                            f"but found '{type(node).__name__}'.")

    def _find_dependencies(self, directives: List[Directive]) -> List[Node]:
        '''
        TODO
        '''
        dependencies = []
        for directive in directives:
            dependency = self._find_next_dependency(
                    directive.dir_body, directive
            )
            # Want to explicily check if the dependency is False
            # as that means we have a nowait with an unsatisfiable
            # dependency.
            if dependency is False:
                raise TransformationError(
                    "Found a nowait directive with an unsatisfiable "
                    "dependency. PSyclone cannot remove barriers from the "
                    "provided Routine."
                )
            dependencies.append(dependency)
        return dependencies

    @staticmethod
    def _reduce_barrier_set(required_barriers: List[Node],
                            depending_barriers: List[List[Node]]) -> None:
        # Now we have some initial required barriers (ideally) we can reduce
        # the lists of barriers for each dependency covered by multiple
        # barriers.
        for i, barriers in enumerate(depending_barriers):
            # Look through all the barrier sets which have more than one
            # satisfying barrier
            if len(barriers) > 1:
                # For each barrier in this barrier set, if its in the
                # required_barrier list, then we can replace this barrier
                # set with a required_barrier
                for bar in barriers:
                    # Check if this barrier is in the required_barrier
                    required = False
                    for req in required_barriers:
                        if req is bar:
                            required = True
                            break
                    # If its a required barrier, then we can replace the
                    # list of dependencies with just the required barrier for
                    # now.
                    if required:
                        bars = [bar]
                        depending_barriers[i] = bars
                        break

    @staticmethod
    def _get_max_barrier_dependency(
            depending_barriers: List[List[Node]]) -> int:
        max_size = 1
        for barriers in depending_barriers:
            if len(barriers) > max_size:
                max_size = len(barriers)
        return max_size

    def _eliminate_barriers(self, node: Routine, directives: List[Directive],
                            barrier_type: type) -> None:
        '''
        TODO
        '''
        # For each of the directives find the next dependency.
        next_dependencies = self._find_dependencies(directives)
        dependency_pos = []
        for dep in next_dependencies:
            if dep is True:
                dependency_pos.append(0)
            else:
                dependency_pos.append(dep.abs_position)
        # dependency_pos = [dep.abs_position for dep in next_dependencies]

        # Get the abs_positions and depths of each of the directives.
        abs_positions = [node.abs_position for node in directives]

        # Find all the barriers
        all_barriers = node.walk(barrier_type)
        barrier_positions = [bar.abs_position for bar in all_barriers]

        # For each directive find all the barriers that satisfy its dependency
        # A barrier satisfies its dependency if:
        # 1. Its abs_position is > the directive and < the next_dependency
        #    position
        #    TODO What if the dependency is before it in the tree?
        # 2. All of the barrier's if statement ancestors are also ancestors of
        #    the directive or the dependency, and they're contained in the
        #    same schedule of
        #    the if statement, i.e. both are in the if schedule or the else
        #    schedule.
        depending_barriers = []
        for i, directive in enumerate(directives):
            found_barriers = []
            # If next_dependencies[i] is True, there is no dependency so we
            # don't need to do anything
            if next_dependencies[i] is True:
                depending_barriers.append(found_barriers)
                continue
            # Loop through the barriers.
            for j, barrier in enumerate(all_barriers):
                # If the dependency is after the nowait directive then
                # we have the easy strategy.
                if dependency_pos[i] > abs_positions[i]:
                    # If the barrier appears before the directive then skip
                    # it.
                    if barrier_positions[j] < abs_positions[i]:
                        continue
                    # If the barrier appears after the dependency then we are
                    # done with this directive.
                    if barrier_positions[j] > dependency_pos[i]:
                        break
                else:
                    # Otherwise the dependency is before the directive, so
                    # they're contained in a loop.
                    # If the barrier is after the dependency but before the
                    # directive we can skip it.
                    if (barrier_positions[j] > dependency_pos[i] and
                            barrier_positions[j] < abs_positions[i]):
                        continue
                    # If the barrier is not contained in any of the ancestor
                    # loops of both then we can ignore it.
                    loop_ancestor = directives[i].ancestor(Loop)
                    barrier_in_ancestor_loop = False
                    while loop_ancestor:
                        if not next_dependencies[i].is_descendent_of(
                                loop_ancestor):
                            break
                        if barrier.is_descendent_of(loop_ancestor):
                            barrier_in_ancestor_loop = True
                            break
                        loop_ancestor = loop_ancestor.ancestor(Loop)
                    if not barrier_in_ancestor_loop:
                        continue

                # The barrier appears between the node and its dependency.
                # Recurse up from the barrier and find all the if statement
                # ancestors.
                barrier_ancestor_if = barrier.ancestor(IfBlock)
                while barrier_ancestor_if is not None:
                    # Check if there's an elseblock, and if the barrier
                    # is inside the else body if so.
                    if (barrier_ancestor_if.else_body and
                            barrier.is_descendent_of(
                                barrier_ancestor_if.else_body)):
                        # Check that either the dependency or the directive
                        # are in the same else body
                        if (not (directive.is_descendent_of(
                                    barrier_ancestor_if.else_body) or
                                 next_dependencies[i].is_descendent_of(
                                     barrier_ancestor_if.else_body)
                                 )):
                            # Neither are in the same else block so exit early
                            break
                    elif (barrier.is_descendent_of(
                            barrier_ancestor_if.if_body)):
                        # Check that either the dependency or the directive
                        # are in the same else body
                        if (not (directive.is_descendent_of(
                                    barrier_ancestor_if.if_body) or
                                 next_dependencies[i].is_descendent_of(
                                     barrier_ancestor_if.if_body)
                                 )):
                            # Neither are in the same else block so exit early
                            break
                    # This if statement contains both the barrier and either
                    # the directive or the dependency so we recurse upwards.
                    barrier_ancestor_if = barrier_ancestor_if.ancestor(
                            IfBlock
                    )

                # If we make it to barrier_ancestor_if is None then all of its
                # ifblock ancestors have either the directive or the
                # dependency in the same conditional, meaning this is a
                # dependency satisfying barrier.
                if barrier_ancestor_if is None:
                    found_barriers.append(barrier)
            # We found all the barriers that satisfy this directives
            # dependency so we can store them.
            if len(found_barriers) == 0:
                raise TransformationError(
                    "Found a nowait with no barrier satisfying its "
                    "dependency which is unsupported behaviour for "
                    "OMPRemoveBarrierTrans"
                )
            depending_barriers.append(found_barriers)

        # We now have a list of every barrier that satisfies the dependency
        # of each of the nowait directives in this Routine.
        # First we have to keep all the barriers that are the only barrier
        # satisfying a dependency.
        # NB. All barrier nodes are == but not `is` according to our
        # implementation (they represent the same code). This makes
        # checking for duplication more code as we can't use `in`.
        required_barriers = []
        for barriers in depending_barriers:
            if len(barriers) == 1:
                # Add the barrier as required if its not already.
                bar = barriers[0]
                add = True
                for req in required_barriers:
                    if req is bar:
                        add = False
                        break
                if add:
                    required_barriers.append(bar)

        # Now we have some required barriers, we can replace the
        # depending_barriers of any set of multi barriers with a required
        # barrier if a required barrier is included in the set.
        OMPRemoveBarrierTrans._reduce_barrier_set(
                required_barriers, depending_barriers
        )

        # Create a list of all barriers not yet in the required_barriers
        # set.
        potential_removes = []
        for barrier in all_barriers:
            required = False
            for req in required_barriers:
                if req is barrier:
                    required = True
                    break
            if not required:
                potential_removes.append(barrier)

        # Time to loop until we satisfy all dependencies with one barrier
        # each.
        while (OMPRemoveBarrierTrans._get_max_barrier_dependency(
                depending_barriers) > 1):
            # The chosen strategy here is to find which of the remaining
            # barriers can satisfy the most possible dependency sets, add
            # that barrier to the set of required barriers and then update
            # the dependency sets. This process is repeated until all
            # barrier sets are satisfied by a required barrier (i.e. have a
            # size of 1).
            # NB. We haven't proven this is an optimal solution in terms of
            # solution quality or computation time - if either causes problems
            # we can re-evaluate.
            dependencies_satisfied = [0] * len(potential_removes)
            for i, bar in enumerate(potential_removes):
                # Count how many of the dependencies can be solved by this
                # barrier
                solves = 0
                for dep_list in depending_barriers:
                    for req in dep_list:
                        if bar is req:
                            solves = solves + 1
                            break
                dependencies_satisfied[i] = solves
            # Find the max value and the first index with that number of
            # satisfied dependencies:
            maxval = max(dependencies_satisfied)
            max_index = dependencies_satisfied.index(maxval)

            # Add the barrier found to the list of required_barriers, and
            # remove it from the potential_removes.
            required_barriers.append(potential_removes.pop(max_index))

            # Reduce the barrier set with the new required_barrier.
            OMPRemoveBarrierTrans._reduce_barrier_set(
                    required_barriers, depending_barriers
            )

            # This process repeats now until we have a set of required
            # barriers that satisfies all barriers.

        # The final barrier in a routine shouldn't be removed as this ensure
        # we have synchronicity between routines.
        final_barrier = node.walk(barrier_type)[-1]
        # Otherwise at this point all the potential removes should be safe to
        # remove.
        for barrier in potential_removes:
            if barrier is not final_barrier:
                barrier.detach()

    def apply(self, node: Routine) -> None:
        '''
        TODO

        :param node: the routine to try to remove barriers from.
        '''
        self.validate(node)

        # Find all the OMPDoDirectives with nowait.
        cpu_directives = [x for x in node.walk(OMPDoDirective) if x.nowait]

        # Find all the OMPTargetDirectives with nowait.
        gpu_directives = [x for x in node.walk(OMPTargetDirective) if x.nowait]

        # Eliminate OMPBarrierDirectives for the cpu_directives
        if len(cpu_directives) > 0:
            self._eliminate_barriers(node, cpu_directives, OMPBarrierDirective)
        # Eliminate OMPTaskwaitDirectives for the gpu_directives
        if len(gpu_directives) > 0:
            self._eliminate_barriers(node, gpu_directives,
                                     OMPTaskwaitDirective)
