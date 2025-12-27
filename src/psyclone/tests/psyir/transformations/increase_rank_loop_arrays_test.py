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
# Author: S. Siso, STFC Daresbury Lab

''' Module containing tests for the IncreaseRankLoopArraysTrans class. '''

import pytest
from psyclone.psyir.nodes import Loop
from psyclone.psyir.transformations import IncreaseRankLoopArraysTrans
from psyclone.tests.utilities import Compile
from psyclone.transformations import TransformationError


def test_trans_name_and_str():
    ''' Check that the name and str method works as expected. '''
    trans = IncreaseRankLoopArraysTrans()
    assert str(trans) == (
        "Increases the Rank of the supplied arrays by the iteration space of "
        "the given loop, and update all references to those arrays.")


def test_irla_validate(fortran_reader):
    ''' Check that the validate method works as expected. '''
    trans = IncreaseRankLoopArraysTrans()

    psyir = fortran_reader.psyir_from_source("""
        program test
            real, dimension(10) :: a
            integer, dimension(10) :: map
            integer :: i = 3
            integer :: variable
            ! Being used inside a non-elemental function or as array index
            ! makes altering 'a' dangerous here
            variable = sum(map(int(a + 3)))
            a(i) = 3
            do i = 1, 10
               a(i) = 3
            end do
            associate(x => a(i))
            end associate
        end program
    """)

    routine = psyir.children[0]
    with pytest.raises(TransformationError) as err:
        trans.apply(routine.children[0])
    assert ("The target of the IncreaseRankLoopArraysTrans transformation "
            "should be a Loop, but found 'Assignment'." in str(err.value))

    with pytest.raises(TransformationError) as err:
        trans.apply(routine.children[2])
    assert ("IncreaseRankLoopArraysTrans has a mandatory 'arrays' option "
            "that is required to specify which arrays are to have their "
            "rank increased." in str(err.value))

    with pytest.raises(TransformationError) as err:
        trans.apply(routine.children[2], arrays=['i'])
    assert ("IncreaseRankLoopArraysTrans provided 'arrays' must be local "
            "array symbols, but 'i: " in str(err.value))

    with pytest.raises(TransformationError) as err:
        trans.apply(routine.children[2], arrays=['a'])
    assert ("IncreaseRankLoopArraysTrans does not support arrays that are "
            "referenced inside a Codeblock, but 'a' is inside one."
            in str(err.value))

    routine.children[3].detach()
    with pytest.raises(TransformationError) as err:
        trans.apply(routine.children[2], arrays=['a'])
    assert ("IncreaseRankLoopArraysTrans does not support arrays that are "
            "referenced outside the given loop in a non-trivial expression "
            "but 'a' is used outside the loop." in str(err.value))

    with pytest.raises(TransformationError) as err:
        trans.apply(routine.children[2], arrays=['non_existant'])
    assert ("IncreaseRankLoopArraysTrans provided array 'non_existant' does "
            "not existin this scope." in str(err.value))

    with pytest.raises(TransformationError) as err:
        trans.apply(routine.children[2].detach(), arrays=['a'])
    assert ("The target Loop of the IncreaseRankLoopArraysTrans transformation"
            " must be inside a Routine." in str(err.value))


def test_irla_validate_bounds(fortran_reader):
    ''' Check that the validate method checks if the loop bounds are static
    when possible. '''
    trans = IncreaseRankLoopArraysTrans()
    psyir = fortran_reader.psyir_from_source("""
        program test
            use other
            real, dimension(10) :: a
            integer, parameter :: constant = 3
            integer :: i, j
            variable = 3

            ! This loop has variable bounds
            do j = 1, variable
                do i = 1, 10
                   a(i) = 3
                end do
            enddo

            ! These could be static
            do j = size(variable), constant + imported
                do i = 1, 10
                   a(i) = 3
                end do
            enddo

        end program
    """)
    routine = psyir.children[0]
    with pytest.raises(TransformationError) as err:
        trans.apply(routine.walk(Loop)[0], arrays=['a'])
    assert ("IncreaseRankLoopArraysTrans can only be applied to loops with "
            "static loop bound expressions, but it has been attempted "
            "in a loop with the variable 'variable' which is assigned to: "
            "'variable = 3'." in str(err.value))

    # This is fine, it can safely add a new rank to keep each a(i) inside the
    # second loop independent (note that each a(i) in the first loop will still
    # be overwritted as in the original code)
    trans.validate(routine.walk(Loop)[2], arrays=['a'])


def test_irla_validate_multiple_loops_using_array(fortran_reader):
    ''' Check that the validation does not permit multiple loops that use the
    same array in anything other than writing array assignments.'''
    trans = IncreaseRankLoopArraysTrans()
    psyir = fortran_reader.psyir_from_source("""
        program test
            use other
            real, dimension(10) :: a
            integer :: i, j

            do j = 1, 100
                do i = 1, 10
                   call myfunc(a)
                end do
            enddo

            do j = 1, 100
                do i = 1, 10
                   a(i) = a(i) + 3
                end do
            enddo

        end program
    """)
    routine = psyir.children[0]
    # None of the loops pass validation, because the other one also uses 'a'
    # in somthing other than the lhs of an array assignment.
    for loop in routine.walk(Loop, stop_type=Loop):
        with pytest.raises(TransformationError) as err:
            trans.apply(loop, arrays=['a'])
        assert ("IncreaseRankLoopArraysTrans does not support arrays that are "
                "referenced outside the given loop in a non-trivial expression"
                " but 'a' is used outside the loop." in str(err.value))


def test_irla_apply(fortran_reader, fortran_writer):
    ''' Check that the array rank is increased by the bounds of the loop. '''
    trans = IncreaseRankLoopArraysTrans()

    psyir = fortran_reader.psyir_from_source("""
     program test
         integer :: N=10, M=10
         integer :: i, j
         real, dimension(N) :: ztmp
         do i = -5, M+3
             do j = 1, N
                 ztmp(j) = 1
             end do
             do j = 1, N
                 ztmp(j) = ztmp(j) + 1
             end do
         end do
     end program
    """)

    trans.apply(psyir.walk(Loop)[0], arrays=['ztmp'])
    code = fortran_writer(psyir)

    # The declaration and references have been updated
    assert "real, dimension(n,-5:m + 3) :: ztmp" in code
    assert "ztmp(j,i) = 1" in code
    assert "ztmp(j,i) = ztmp(j,i) + 1" in code


def test_irla_apply_accesses_outside_loop(
        fortran_reader, fortran_writer, tmpdir):
    ''' Check that the accesses outside the loop are also populate the whole
    array, this will imply duplicated computations for each value '''
    psyir = fortran_reader.psyir_from_source("""
     program test
         integer, parameter :: N=10, M=10
         integer :: i, j
         real, dimension(N) :: ztmp
         ! Implicit loops
         ztmp = 1
         ! Range loop
         ztmp(3:4) = 2
         ! Explit loop
         do j = 1, 3
             ztmp(j) = 3
         enddo

         do i = 1, 10
             do j = 1, N
                 ztmp(j) = ztmp(j) + 4
             end do
             ztmp(:) = 5
         end do
     end program
    """)
    trans = IncreaseRankLoopArraysTrans()
    trans.apply(psyir.walk(Loop)[1], arrays=['ztmp'])
    code = fortran_writer(psyir)
    # Check the ztmp accesses outside the target loop
    assert "ztmp = 1" in code  # This already indexes the whole array
    assert "ztmp(3:4,:) = 2" in code
    assert "ztmp(j,:) = 3" in code
    # And the two inside the loop
    assert "ztmp(j,i) = ztmp(j,i) + 4" in code
    assert "ztmp(:,i) = 5" in code
    assert Compile(tmpdir).string_compiles(code)
