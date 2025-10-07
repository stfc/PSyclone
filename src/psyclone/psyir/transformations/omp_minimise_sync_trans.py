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

'''Contains the OMPMinimiseSyncTrans.'''

# TODO #2837: Once we leave python 3.8 we can use list instead of List for
# type hints.
from typing import List, Union

from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import (
    Directive,
    IfBlock, Loop, Node,
    OMPDoDirective,
    OMPBarrierDirective, OMPTaskwaitDirective,
    OMPTargetDirective, Routine, WhileLoop,
    OMPParallelDirective
)
from psyclone.psyir.transformations.transformation_error import \
    TransformationError
from psyclone.psyir.transformations.async_trans_mixin import \
    AsyncTransMixin


def _eliminate_final_parallel_barrier(
        parallel_directive: OMPParallelDirective
) -> None:
    '''
    Removes the final OMPBarrierDirective from the end of the
    provided parallel_directive if present.

    :param parallel_directive: The OMPParallelDirective whose final child
                               is to be removed if its a barrier.
    '''
    if isinstance(parallel_directive.dir_body.children[-1],
                  OMPBarrierDirective):
        parallel_directive.dir_body.children[-1].detach()


# Inherits from AsyncTransMixin as it needs some of the helper functions
class OMPMinimiseSyncTrans(Transformation, AsyncTransMixin):
    '''
    Attempts to remove OMPTaskwaitDirective or
    OMPBarrierDirective nodes from a supplied region as long as
    any dependencies continue to be satisfied. The goal is to reduce
    the number of synchronisation points to enable better asynchronous
    execution of loops or kernels.

    For example:

    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.nodes import Loop
    >>> from psyclone.psyir.transformations import OMPLoopTrans
    >>> from psyclone.psyir.transformations import OMPMinimiseSyncTrans
    >>> from psyclone.transformations import OMPParallelTrans
    >>>
    >>> psyir = FortranReader().psyir_from_source("""
    ...     subroutine test
    ...         integer, dimension(100) :: a,b
    ...         integer :: i
    ...
    ...         do i = 1, 100
    ...           a(i) = i
    ...         end do
    ...
    ...         do i = 1, 100
    ...           b(i) = i
    ...         end do
    ...
    ...         do i = 1, 100
    ...           b(i) = b(i) + 1
    ...         end do
    ...
    ...         do i = 1, 100
    ...           a(i) = a(i) + 1
    ...         end do
    ...     end subroutine
    ...     """)
    >>> omplooptrans1 = OMPLoopTrans()
    >>> for loop in psyir.walk(Loop):
    ...     omplooptrans1.apply(loop, nowait=True)
    >>> partrans = OMPParallelTrans()
    >>> partrans.apply(psyir.children[0].children[:])
    >>> rbartrans = OMPMinimiseSyncTrans()
    >>> rbartrans.apply(psyir.children[0])
    >>> print(FortranWriter()(psyir))
    subroutine test()
      integer, dimension(100) :: a
      integer, dimension(100) :: b
      integer :: i
    <BLANKLINE>
      !$omp parallel default(shared), private(i)
      !$omp do schedule(auto)
      do i = 1, 100, 1
        a(i) = i
      enddo
      !$omp end do nowait
      !$omp do schedule(auto)
      do i = 1, 100, 1
        b(i) = i
      enddo
      !$omp end do nowait
      !$omp barrier
      !$omp do schedule(auto)
      do i = 1, 100, 1
        b(i) = b(i) + 1
      enddo
      !$omp end do nowait
      !$omp do schedule(auto)
      do i = 1, 100, 1
        a(i) = a(i) + 1
      enddo
      !$omp end do nowait
      !$omp barrier
      !$omp end parallel
    <BLANKLINE>
    end subroutine test
    <BLANKLINE>

    '''
    def __str__(self) -> str:
        '''Returns the string representation of this OMPMinimiseSyncTrans
        object.'''
        return ("Removes OMPTaskwaitDirective or OMPBarrierDirective nodes "
                "from the supplied region to reduce synchronicity without "
                "invalidating dependencies.")

    def validate(self, node: Routine, **kwargs) -> None:
        '''
        Validity check for the input arguments.

        :param node: the routine to try to remove barriers from.

        :raises TypeError: if the supplied input isn't a Routine.

        '''
        super().validate(node, kwargs)
        if not isinstance(node, Routine):
            raise TypeError(f"OMPMinimiseSyncTrans expects a Routine input "
                            f"but found '{type(node).__name__}'.")

    def _eliminate_adjacent_barriers(self, routine: Routine,
                                     bar_type: type) -> None:
        '''
        Removes excess adjacent bar_type barriers from the input routine, i.e:

        >>> !$omp taskwait
        >>> !$omp taskwait

        will be simplified to just:

        >>> !$omp taskwait

        :param routine: the routine to remove duplicate barriers from.
        :param bar_type: the barrier type to remove.
        '''

        # Find all the barriers in the routine.
        barriers = routine.walk(bar_type)

        # Loop through all the barriers other than the first.
        # We loop backwards as we're removing things from the tree as we go
        # through the list, so i-1 wouldn't work if we loop forwards.
        for i, barrier in reversed(list(enumerate(barriers))):
            if barrier.immediately_follows(barriers[i-1]):
                barrier.detach()

    def _find_dependencies(self, directives: List[Directive]) \
            -> List[Union[Node, bool]]:
        '''
        Finds the next dependencies for each of the directives provided.

        :param directives: The list of directives to find the dependencies
                           for.

        :returns: A list of nodes (or True if a directive has no dependency)
                  corresponding to the dependencies for each of the input
                  directives.

        :raises TransformationError: If one of the input directives has a
                                     dependency that PSyclone can't satisfy.

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
        '''
        Reduces the depending_barriers set according to the list of
        required_barriers, i.e. if a required_barrier is present in one of
        the lists in the depending_barriers, all of the other elements in that
        depending_barrier list are removed.

        :param required_barriers: A list of barriers that are treated as
                                  required to satisfy dependencies.
        :param depending_barriers: A list of lists of barrier sets that each
                                   could satisfy the dependency between a
                                   directive and its dependency.
        '''
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
                for barrier in barriers:
                    # Check if this barrier is in the required_barrier
                    required = False
                    for req in required_barriers:
                        if req is barrier:
                            required = True
                            break
                    # If its a required barrier, then we can replace the
                    # list of dependencies with just the required barrier for
                    # now.
                    if required:
                        bars = [barrier]
                        depending_barriers[i] = bars
                        break

    @staticmethod
    def _get_max_barrier_dependency(
            depending_barriers: List[List[Node]]) -> int:
        '''
        Returns the maximum size of a sublist in the depending_barriers
        input.

        :param depending_barriers: The input list of lists for which this
                                   function is to find the largest sublist
                                   size.

        :returns: the maximum size of a sublist in the depending_barriers
                  input.
        '''
        return len(max(depending_barriers, key=len))

    def _eliminate_barriers(self, node: Routine, directives: List[Directive],
                            barrier_type: type) -> None:
        '''
        Eliminates barriers of the barrier_type in the input Routine node that
        satsfies all the dependencies for the input directives.

        :param node: The routine to remove barriers from.
        :param directives: The list of directives whose dependencies must
                           still be satisfied at the end of barrier removal.
        :param barrier_type: The type of barrier that satisfies the
                             dependencies of the directives.

        :raises TransformationError: If a directive is found to have no
                                     barriers that satisfy its dependencies.
        '''
        # For each of the directives find the next dependency.
        next_dependencies = self._find_dependencies(directives)
        # If dep is True then there is no real next_dependency, so
        # 0 is used a substitute.
        dependency_pos = [[dep.abs_position for dep in next_depends]
                          if isinstance(next_depends, list) else
                          [-1] for next_depends in next_dependencies]

        # Get the abs_positions and depths of each of the directives.
        abs_positions = [node.abs_position for node in directives]

        # Find all the barriers
        all_barriers = node.walk(barrier_type)
        barrier_positions = [barrier.abs_position for barrier in all_barriers]

        # For each directive find all the barriers that satisfy its dependency
        # A barrier satisfies its dependency if:
        # 1. Its abs_position is > the directive and < the next_dependency
        #    position (or something slightly more complex if the dependency is
        #    before it in the tree.
        # 2. All of the barrier's if statement ancestors are also ancestors of
        #    the directive or the dependency, and they're contained in the
        #    same schedule of
        #    the if statement, i.e. both are in the if schedule or the else
        #    schedule.
        depending_barriers = []
        for i, directive in enumerate(directives):
            # If next_dependencies[i] is True, there is no dependency so we
            # don't need to do anything
            if next_dependencies[i] is True:
                depending_barriers.append([])
                continue

            # For each of the dependencies of the directive.
            for k, next_depend in enumerate(next_dependencies[i]):
                found_barriers = []

                # Loop through the barriers.
                for j, barrier in enumerate(all_barriers):
                    # If the dependency is after the nowait directive then
                    # we have the easy strategy.
                    if dependency_pos[i][k] > abs_positions[i]:
                        # If the barrier appears before the directive then skip
                        # it.
                        if barrier_positions[j] < abs_positions[i]:
                            continue
                        # If the barrier appears after the dependency then we
                        # are done with this directive.
                        if barrier_positions[j] > dependency_pos[i][k]:
                            break
                    else:
                        # Otherwise the dependency is before the directive, so
                        # they're contained in a loop.
                        # If the barrier is after the dependency but before the
                        # directive we can skip it.
                        if (barrier_positions[j] > dependency_pos[i][k] and
                                barrier_positions[j] < abs_positions[i]):
                            continue
                        # If the directive and the dependency share an
                        # ancestor Loop, then the barrier must also
                        # be a child of that loop.
                        loop_ancestor = directive.ancestor(
                                (Loop, WhileLoop), shared_with=next_depend
                        )
                        if (loop_ancestor and
                                not barrier.is_descendant_of(loop_ancestor)):
                            continue

                    # The barrier appears between the node and its dependency.
                    # Recurse up from the barrier and find all the if statement
                    # ancestors.
                    barrier_ancestor_if = barrier.ancestor(IfBlock)
                    while barrier_ancestor_if is not None:
                        # Check if there's an elseblock, and if the barrier
                        # is inside the else body if so.
                        if (barrier_ancestor_if.else_body and
                                barrier.is_descendant_of(
                                    barrier_ancestor_if.else_body)):
                            # Check that either the dependency or the directive
                            # are in the same else body
                            if (not (directive.is_descendant_of(
                                        barrier_ancestor_if.else_body) or
                                     next_depend.is_descendant_of(
                                         barrier_ancestor_if.else_body)
                                     )):
                                # Neither are in the same else block so exit
                                # early
                                break
                        elif (barrier.is_descendant_of(
                                barrier_ancestor_if.if_body)):
                            # Check that either the dependency or the directive
                            # are in the same else body
                            if (not (directive.is_descendant_of(
                                        barrier_ancestor_if.if_body) or
                                     next_depend.is_descendant_of(
                                         barrier_ancestor_if.if_body)
                                     )):
                                # Neither are in the same else block so exit
                                # early
                                break
                        # This if statement contains both the barrier and
                        # either the directive or the dependency so we recurse
                        # upwards.
                        barrier_ancestor_if = barrier_ancestor_if.ancestor(
                                IfBlock
                        )
                    # If we make it to barrier_ancestor_if is None then all of
                    # its IfBlock ancestors have either the directive or the
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
                        "OMPMinimiseSyncTrans."
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
                barrier = barriers[0]
                add = True
                for req in required_barriers:
                    if req is barrier:
                        add = False
                        break
                if add:
                    required_barriers.append(barrier)

        # Now we have some required barriers, we can replace the
        # depending_barriers of any set of multi barriers with a required
        # barrier if a required barrier is included in the set.
        OMPMinimiseSyncTrans._reduce_barrier_set(
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
        while (OMPMinimiseSyncTrans._get_max_barrier_dependency(
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
            for i, barrier in enumerate(potential_removes):
                # Count how many of the dependencies can be solved by this
                # barrier
                solves = 0
                for dep_list in depending_barriers:
                    for req in dep_list:
                        if barrier is req:
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
            OMPMinimiseSyncTrans._reduce_barrier_set(
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

    def apply(self, node: Routine, **kwargs) -> None:
        '''
        Applies the transformation, which eliminates unneccessary
        barriers whilst satisfying all of the dependencies.

        :param node: the routine to try to remove barriers from.
        '''
        self.validate(node, **kwargs)
        super().apply(node, **kwargs)

        # Find all the OMPDoDirectives with nowait.
        cpu_directives = [x for x in node.walk(OMPDoDirective) if x.nowait]

        # Find all the OMPTargetDirectives with nowait.
        gpu_directives = [x for x in node.walk(OMPTargetDirective) if x.nowait]

        # Eliminate OMPBarrierDirectives for the cpu_directives
        if len(cpu_directives) > 0:
            self._eliminate_adjacent_barriers(node, OMPBarrierDirective)
            self._eliminate_barriers(node, cpu_directives, OMPBarrierDirective)
            # We can also remove the final child from each parallel directive
            # if its a OMPBarrierDirective as they are uneccessary.
            for parallel in node.walk(OMPParallelDirective):
                _eliminate_final_parallel_barrier(parallel)
        # Eliminate OMPTaskwaitDirectives for the gpu_directives
        if len(gpu_directives) > 0:
            self._eliminate_adjacent_barriers(node, OMPTaskwaitDirective)
            self._eliminate_barriers(node, gpu_directives,
                                     OMPTaskwaitDirective)
