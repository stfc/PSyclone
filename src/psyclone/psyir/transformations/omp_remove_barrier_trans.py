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
    IfBlock, Node
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
                            f"but found {type(node).__str__}")

        # FIXME Do we need to consider code with gotos and disallow them or
        # does something else do this? I suspect not...

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

    def _reduce_barrier_set(self, required_barriers: List[Node],
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

    def _get_max_barrier_dependency(
            self, depending_barriers: List[List[Node]]) -> int:
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
                # If the barrier appears before the directive then skip it.
                if barrier_positions[j] < abs_positions[i]:
                    continue
                # If the barrier appears after the dependency then we are
                # done with this directive.
                if barrier_positions[j] > dependency_pos[i]:
                    break

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
        self._reduce_barrier_set(required_barriers, depending_barriers)

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
        while self._get_max_barrier_dependency(depending_barriers) > 1:
            # NEED A GOOD STRATEGY HERE
            # MY INITIAL IDEA WOULD BE TO FIND THE "POTENTIAL REMOVES" BARRIER
            # THAT CAN SATISFY THE MOST POSSIBLE BARRIER SETS, MARK THAT AS
            # REQUIRED AND REDUCE BARRIER SET, AND REPEAT AS MANY TIMES AS
            # NEEDED.
            # THIS COULD BE A LONG PROCESS SO I DON'T LOVE IT BUT NEED TO
            # RESEARCH IF THERE'S A BETTER SOLUTION. NOT SURE WHAT THIS IS
            # CALLED? IS IT A KNOWN GRAPH PROBLEM (OR EQUIVALENT TO?). FEELS
            # LIKE IT SHOULD BE.

            # ALSO WON'T PROVE THAT THAT APPROACH WOULD RESULT IN MINIMAL SET
            # OF BARRIERS FOR NOW, BUT PROBABLY IS THE IDEAL AIM.

            # We've eliminated all required dependencies by this point, whats
            # the worst case from here? Think worst case would be eliminating
            # only 2 per step of the loop, which results in n log n? Since
            # we'd need log n steps and each step requires to loop through
            # all n barriers. A better solution would need to loop through all
            # n barriers per step and eliminate more barriers per step -
            # not straightforward though.

            # Seems like graph algorithm somehow - edge elimination of DAG?
            # without reducing paths between nodes or something. Doesn't quite
            # work becuase we have 2 "node" types in that context maybe
            assert False

        # At this point all the potential removes should be safe to remove.
        for barrier in potential_removes:
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
