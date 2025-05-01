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
# ----------------------------------------------------------------------------
# Author: A. B. G. Chalk, STFC Daresbury Lab

''' Tests for the OMPRemoveBarrierTrans transformation. '''

import pytest
from psyclone.psyir.nodes import (
        Loop, Routine, OMPBarrierDirective,
        OMPTaskwaitDirective, OMPDoDirective,
        OMPTargetDirective)
from psyclone.psyir.transformations import (
        OMPLoopTrans, OMPRemoveBarrierTrans,
        OMPTargetTrans, TransformationError
)
from psyclone.transformations import OMPParallelTrans


def test_omp_remove_barrier_trans_str():
    '''Tests the __str__ member of the OMPRemoveBarrierTrans.'''
    instance = OMPRemoveBarrierTrans()
    assert (str(instance) == "Removes OMPTaskwaitDirective or "
            "OMPBarrierDirective nodes from the supplied region to reduce "
            "synchronicity without invalidating dependencies.")


def test_omp_remove_barrier_validate():
    '''Tests the validate member of the OMPRemoveBarrierTrans.'''
    with pytest.raises(TypeError) as excinfo:
        OMPRemoveBarrierTrans().validate("a")

    assert ("OMPRemoveBarrierTrans expects a Routine input but found 'str'."
           in str(excinfo.value))


def test_omp_remove_barrier_find_dependencies(fortran_reader):
    '''Test the _find_dependencies routine of the OMPRemoveBarrierTrans.'''

    code = """
    subroutine test
        integer, dimension(100) :: a
        integer :: i, j

        do i = 1, 250
          do j = 1, 100
            a(j) = i * j
          end do
        end do
    end subroutine
    """

    psyir = fortran_reader.psyir_from_source(code)
    loops = psyir.walk(Loop)
    # Add a nowait to a loop that can't really have one by doing it explicitly
    # as PSyclone won't let us.
    otrans = OMPTargetTrans()
    otrans.apply(loops[1])
    tar_dir = psyir.children[0].children[0].loop_body.children[0]
    tar_dir.nowait = True

    # If we pass this directive into _find_dependencies it should error as it
    # is its own dependency.
    with pytest.raises(TransformationError) as excinfo:
        OMPRemoveBarrierTrans()._find_dependencies([tar_dir])
    assert ("Found a nowait directive with an unsatisfiable dependency. "
            "PSyclone cannot remove barriers from the provided Routine."
            in str(excinfo.value))

    # Test we get dependencies as expected otherwise.
    code = """
    subroutine test
        integer, dimension(100) :: a,b
        integer :: i

        do i = 1, 100
          a(i) = i
        end do

        do i = 1, 100
          b(i) = i
        end do

        do i = 1, 100
          b(i) = b(i) + 1
        end do

        do i = 1, 100
          a(i) = a(i) + 1
        end do
    end subroutine
    """

    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    otrans = OMPLoopTrans()
    for loop in psyir.walk(Loop):
        otrans.apply(loop, nowait=True)

    do_dirs = psyir.walk(OMPDoDirective)
    deps = OMPRemoveBarrierTrans()._find_dependencies(do_dirs)

    assert len(deps) == 4
    # Next dependency for the first directive is the lhs of the assignment
    # in the last loop
    assert deps[0] is routine.children[5].dir_body[0].loop_body[0].lhs
    # Next dependency for the second directive is the lhs of the assignment
    # in the 3rd loop
    assert deps[1] is routine.children[3].dir_body[0].loop_body[0].lhs
    # The final two directives have no dependency in this scope.
    assert deps[2] is True
    assert deps[3] is True


def test_omp_remove_barrier_reduce_barrier_set():
    '''Test the _reduce_barrier_set method of OMPRemoveBarrierTrans.'''
    # Create some barriers
    barriers = []
    for i in range(5):
        barriers.append(OMPBarrierDirective())

    # Create a barrier set {0}, {1,2}, {2,3}, {3,4}
    barrier_set = [[barriers[0]],
                   [barriers[1], barriers[2]],
                   [barriers[2], barriers[3]],
                   [barriers[3], barriers[4]]]

    # Set require barriers to barriers 0 and 2
    required_barriers = [barriers[0], barriers[2]]
    # Once we reduce the barrier set we should have a new set of:
    # {0}, {2}, {2}, {3,4}
    OMPRemoveBarrierTrans._reduce_barrier_set(required_barriers,
                                              barrier_set)
    assert len(barrier_set[0]) == 1
    assert barrier_set[0][0] is barriers[0]
    assert len(barrier_set[1]) == 1
    assert barrier_set[1][0] is barriers[2]
    assert len(barrier_set[2]) == 1
    assert barrier_set[2][0] is barriers[2]
    assert len(barrier_set[3]) == 2
    assert barrier_set[3][0] is barriers[3]
    assert barrier_set[3][1] is barriers[4]


def test_omp_remove_barrier_get_max_barrier_dependency():
    '''Test the _get_max_barrier_dependency method of OMPRemoveBarrierTrans.'''
    # Technically doesn't care if it has barrier inputs, so this test is
    # just going to use lists of lists of ints.

    barriers = [[0], [1, 2], [1, 2], [3, 4, 5]]

    assert OMPRemoveBarrierTrans._get_max_barrier_dependency(barriers) == 3

######################################
# Beginning of Functionality Testing #
######################################
# This tests _eliminate_barrier through apply, since the apply functionality
# is mostly encapsulates the eliminate_barrier usage.


def test_basic_barrier_removal(fortran_reader, fortran_writer):
    ''' Tests the basic barrier removal idea - 4 loops A, B, C, D.
    A => D and B=> C are the dependencies - we only need a barrier between
    B and C to satisfy both dependencies.'''
    code = """
    subroutine test
        integer, dimension(100) :: a,b
        integer :: i

        do i = 1, 100
          a(i) = i
        end do

        do i = 1, 100
          b(i) = i
        end do

        do i = 1, 100
          b(i) = b(i) + 1
        end do

        do i = 1, 100
          a(i) = a(i) + 1
        end do
    end subroutine
    """

    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    otrans = OMPLoopTrans()
    for loop in psyir.walk(Loop):
        otrans.apply(loop, nowait=True)

    paralleltrans = OMPParallelTrans()
    paralleltrans.apply(routine.children[:])

    # The initial implementation would give 3 barriers (since we add one
    # at the end for safety which is unneccessary but maximises safety).
    assert len(psyir.walk(OMPBarrierDirective)) == 3

    rtrans = OMPRemoveBarrierTrans()

    rtrans.apply(routine)
    assert len(psyir.walk(OMPBarrierDirective)) == 2
    correct = """  !$omp do schedule(auto)
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
  !$omp end parallel"""
    out = fortran_writer(psyir)
    assert correct in out

    # Test the same for parallelisation with target.
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    targettrans = OMPTargetTrans()
    for loop in psyir.walk(Loop):
        targettrans.apply(loop, options={"nowait": True})

    assert len(psyir.walk(OMPTaskwaitDirective)) == 3
    rtrans = OMPRemoveBarrierTrans()

    rtrans.apply(routine)
    assert len(psyir.walk(OMPTaskwaitDirective)) == 2
    correct = """  !$omp target nowait
  do i = 1, 100, 1
    a(i) = i
  enddo
  !$omp end target
  !$omp target nowait
  do i = 1, 100, 1
    b(i) = i
  enddo
  !$omp end target
  !$omp taskwait
  !$omp target nowait
  do i = 1, 100, 1
    b(i) = b(i) + 1
  enddo
  !$omp end target
  !$omp target nowait
  do i = 1, 100, 1
    a(i) = a(i) + 1
  enddo
  !$omp end target
  !$omp taskwait"""
    out = fortran_writer(psyir)
    assert correct in out


def test_dependency_before_directive(fortran_reader, fortran_writer):
    ''' Test what happens if a directives dependency appears before it in the
    PSyIR tree - i.e. its inside a loop with multiple directives contained.'''
    code = """
    subroutine test
    integer, dimension(100) :: a, b
    integer :: i, j

    do i = 1, 10
        do j = 1, 100
            b(j) = j
        end do
        do j = 1, 100
            b(j) = b(j) + 1
        end do
        do j = 1, 100
            a(j) = j
        end do
        do j = 1, 100
            a(j) = a(j) + i
        end do
    end do
    end subroutine"""

    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    targettrans = OMPTargetTrans()
    for loop in psyir.walk(Loop)[1:]:
        targettrans.apply(loop, options={"nowait": True})

    # We have if we imagine we have loops BCDA b->c,c->b, d->a and a->d
    # depdendencies. The a->d barrier is covered by b->c, and the c->b barrier
    # is covered by the d->a barrier.
    assert len(psyir.walk(OMPTaskwaitDirective)) == 5

    rtrans = OMPRemoveBarrierTrans()

    rtrans.apply(routine)
    assert len(psyir.walk(OMPTaskwaitDirective)) == 3
    correct = """  do i = 1, 10, 1
    !$omp target nowait
    do j = 1, 100, 1
      b(j) = j
    enddo
    !$omp end target
    !$omp taskwait
    !$omp target nowait
    do j = 1, 100, 1
      b(j) = b(j) + 1
    enddo
    !$omp end target
    !$omp target nowait
    do j = 1, 100, 1
      a(j) = j
    enddo
    !$omp end target
    !$omp taskwait
    !$omp target nowait
    do j = 1, 100, 1
      a(j) = a(j) + i
    enddo
    !$omp end target
  enddo
  !$omp taskwait"""
    out = fortran_writer(psyir)
    assert correct in out


def test_barrier_in_if_statement_is_ignored(fortran_reader):
    code = """
    subroutine test
        integer, dimension(100) :: a,b
        integer :: i

        do i = 1, 100
          a(i) = i
        end do

        if( i < 100) then
            do i = 1, 100
              b(i) = i
            end do

            do i = 1, 100
              b(i) = b(i) + 1
            end do
        end if

        do i = 1, 100
          a(i) = a(i) + 1
        end do
    end subroutine
    """
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    targettrans = OMPTargetTrans()
    for loop in psyir.walk(Loop):
        targettrans.apply(loop, options={"nowait": True})

    # We have 3 barriers initially
    assert len(psyir.walk(OMPTaskwaitDirective)) == 3

    # We can't remove the a->d barrier here as the if statement containing the
    # barrier between them prevents it.
    rtrans = OMPRemoveBarrierTrans()
    rtrans.apply(routine)
    assert len(psyir.walk(OMPTaskwaitDirective)) == 3
    # The transformation doesn't modify the code so we don't check the output
    # for this test.


def test_barrier_in_else_is_ignored(fortran_reader):
    ''' Test that barriers in an else statement are also ignored
    for when searching for barriers that satisfy dependencies.'''
    code = """
    subroutine test
        integer, dimension(100) :: a,b
        integer :: i

        do i = 1, 100
          a(i) = i
        end do

        if( i < 100) then
            i = 1
        else
            do i = 1, 100
              b(i) = i
            end do

            do i = 1, 100
              b(i) = b(i) + 1
            end do
        end if

        do i = 1, 100
          a(i) = a(i) + 1
        end do
    end subroutine
    """
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    targettrans = OMPTargetTrans()
    for loop in psyir.walk(Loop):
        targettrans.apply(loop, options={"nowait": True})

    # We have 3 barriers initially
    assert len(psyir.walk(OMPTaskwaitDirective)) == 3

    # We can't remove the a->d barrier here as the if statement containing the
    # barrier between them prevents it.
    rtrans = OMPRemoveBarrierTrans()
    rtrans.apply(routine)
    assert len(psyir.walk(OMPTaskwaitDirective)) == 3
    # The transformation doesn't modify the code so we don't check the output
    # for this test.


def test_multiple_nowaits_covered_by_same_barrier_initially(fortran_reader):
    '''This test covers the case where we have one barrier that satisfies
    multiple dependencies as this barrier is only added to the list of 
    required_barriers once.'''
    code = """
    subroutine test
        integer, dimension(100) :: a,b
        integer :: i

        do i = 1, 100
          a(i) = i
        end do

        do i = 1, 100
          b(i) = i
        end do

        do i = 1, 100
          a(i) = a(i) + b(i)
        end do
    end subroutine
    """
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    targettrans = OMPTargetTrans()
    for loop in psyir.walk(Loop):
        targettrans.apply(loop, options={"nowait": True})

    # This is unlikely to occur in normal code, but if we apply the
    # transformation twice to this routine it would occur.
    rtrans = OMPRemoveBarrierTrans()
    rtrans.apply(routine)
    assert len(routine.walk(OMPTaskwaitDirective)) == 2

    # Now we have one barrier covering both dependencies from loops 1 & 2 to
    # loop 3, so it only gets added once and we should end up with 2
    # barriers still
    rtrans = OMPRemoveBarrierTrans()
    rtrans.apply(routine)
    assert len(routine.walk(OMPTaskwaitDirective)) == 2


# FIXME Cover loop

# Cover the "no barrier found" failure.
def test_no_barrier_from_nowait(fortran_reader):
    '''Test that the correct TransformationError is thrown if a nowait
    directive is not found to have a barrier satisfying its dependencies.'''
    code = """
    subroutine test
    integer, dimension(100) :: a
    integer :: j

    do j = 1, 100
        a(j) = j
    end do
    do j = 1, 100
        a(j) = a(j) + i
    end do
    end subroutine"""

    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    targettrans = OMPTargetTrans()
    for loop in psyir.walk(Loop):
        targettrans.apply(loop)

    # Add the nowait manually to the first OMPTargetDirective
    psyir.walk(OMPTargetDirective)[0].nowait = True

    rtrans = OMPRemoveBarrierTrans()
    with pytest.raises(TransformationError) as excinfo:
        rtrans.apply(routine)
    assert ("Found a nowait with no barrier satisfying its dependency which "
            "is unsupported behaviour for OMPRemoveBarrierTrans." in
            str(excinfo.value))


# Remaining things to test:
# The while loop needs covering, not sure if it is yet or if we need to make a
# manual example to test that.
