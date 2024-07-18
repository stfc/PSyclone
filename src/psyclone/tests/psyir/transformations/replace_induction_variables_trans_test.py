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
# ----------------------------------------------------------------------------
# Author: J. Henrichs, Bureau of Meteorology
# Modified: S. Siso, STFC Daresbury Labs

'''This module tests the ReplaceInductionVariablesTrans transformation.
'''

import pytest

from psyclone.psyir.nodes import Literal
from psyclone.psyir.symbols import INTEGER_TYPE
from psyclone.psyir.transformations import (ReplaceInductionVariablesTrans,
                                            TransformationError)


# ----------------------------------------------------------------------------
def test_riv_general():
    '''Test general functionality of the transformation. '''

    riv = ReplaceInductionVariablesTrans()

    assert str(riv) == "Replaces all induction variables in a loop."
    assert riv.name == "ReplaceInductionVariablesTrans"


# ----------------------------------------------------------------------------
def test_riv_errors():
    '''Test errors that should be thrown. '''

    riv = ReplaceInductionVariablesTrans()
    lit = Literal("1", INTEGER_TYPE)
    with pytest.raises(TransformationError) as err:
        riv.apply(lit)

    assert ("Error in ReplaceInductionVariablesTrans transformation. The "
            "supplied node argument should be a PSyIR Loop, but found "
            "'Literal'" in str(err.value))


# ----------------------------------------------------------------------------
def test_riv_working(fortran_reader, fortran_writer):
    '''Tests if loop-invariant assignments are replaced as expected.'''
    source = '''program test
                use mymod
                type(my_type):: t1, t2, t3, t4

                integer i, invariant, ic1, ic2, ic3, ic4, ic5, ic6
                real, dimension(10) :: a
                invariant = 1
                do i = 1, 10
                    ic1 = 12                ! Constant
                    ic2 = invariant         ! Invariant variable
                    ic3 = i+1               ! Dependent on loop variable
                    ic4 = i + 1 + invariant ! Loop variable plus invariant
                    ic5 = i*i+3*sin(i)      ! Complicated expression
                    ic6 = ic4 + ic5         ! Multi-step replacement
                    t1%a = 13
                    a(ic1) = 1+(ic1+1)*ic1
                    a(ic2) = 2+(ic2+1)*ic2
                    a(ic3) = 3+(ic3+1)*ic3
                    a(ic4) = 4+(ic4+1)*ic4
                    a(ic5) = 5+(ic5+1)*ic5
                    a(ic6) = 6+(ic6+1)*ic6
                    a(t1%a) = 7+(t1%a+1)*t1%a
                end do
                end program test'''
    psyir = fortran_reader.psyir_from_source(source)
    # The first child is the assignment to 'invariant'
    loop = psyir.children[0].children[1]

    riv = ReplaceInductionVariablesTrans()
    riv.apply(loop)
    out_all = fortran_writer(psyir)
    out_loop = fortran_writer(loop)

    assert "a(12) = 1 + (12 + 1) * 12" in out_loop
    # Make sure the assignment to ic1 has been added outside of the loop:
    assert "ic1 = 12" in out_all
    assert "ic1 = 12" not in out_loop

    assert "a(invariant) = 2 + (invariant + 1) * invariant" in out_loop
    # Make sure the assignment to ic2 has been added outside of the loop:
    assert "ic2 = invariant" in out_all
    assert "ic2 = invariant" not in out_loop

    assert "a(i + 1) = 3 + (i + 1 + 1) * (i + 1)" in out_loop
    assert "ic3 = i - 1 + 1" in out_all

    assert ("a(i + 1 + invariant) = 4 + (i + 1 + invariant + 1) * "
            "(i + 1 + invariant)" in out_loop)
    assert "ic4 = i - 1 + 1 + invariant" in out_all

    assert ("a(i * i + 3 * SIN(i)) = 5 + (i * i + 3 * SIN(i) + 1) * "
            "(i * i + 3 * SIN(i))" in out_loop)
    assert "ic5 = (i - 1) * (i - 1) + 3 * SIN(i - 1)" in out_all
    assert ("a(i + 1 + invariant + (i * i + 3 * SIN(i))) = 6 + "
            "(i + 1 + invariant + (i * i + 3 * SIN(i)) + 1) * "
            "(i + 1 + invariant + (i * i + 3 * SIN(i)))" in out_loop)
    assert ("ic6 = i - 1 + 1 + invariant + ((i - 1) * (i - 1) + "
            "3 * SIN(i - 1))" in out_all)
    assert "a(13) = 7 + (13 + 1) * 13" in out_loop
    # Make sure the assignment to t1%a has been added outside of the loop:
    assert "t1%a = 13" in out_all
    assert "t1%a = 13" not in out_loop


# ----------------------------------------------------------------------------
def test_riv_not_movable(fortran_reader, fortran_writer):
    '''Tests assignments that cannot be moved.
    '''
    source = '''program test
                integer i, ic1, ic2, ic3, ic4, ic5, ic6
                integer :: some_func
                real, dimension(10) :: a
                do i = 1, 10
                    ic1 = i+a(i)             ! a is written to below
                    ic3 = ic2                ! Read ic2 before write
                    ic2 = 5                  ! write to ic2 is not first use
                    ic4 = i-1                ! written twice
                    if (i.eq.3) then         ! Nothing inside another statement
                        ic5 = 1
                    endif
                    ic6 = i + some_func()    ! Function might depend on #calls
                    a(ic1) = 1+(ic1+1)*ic1
                    a(ic2) = 2+(ic2+1)*ic2
                    a(ic3) = 3+(ic3+1)*ic3
                    ic4 = i + 1              ! written twice
                    a(ic4) = 4+(ic4+1)*ic4
                    a(ic5) = 5+(ic5+1)*ic5
                    a(ic6) = 6+(ic6+1)*ic6
                end do
                end program test'''
    psyir = fortran_reader.psyir_from_source(source)
    loop = psyir.children[0].children[0]

    # None of the statements can be moved, so the output
    # before and after the transformation should be identical:
    out_before = fortran_writer(loop)
    riv = ReplaceInductionVariablesTrans()
    riv.apply(loop)
    out_after = fortran_writer(loop)
    assert out_before == out_after


# ----------------------------------------------------------------------------
def test_riv_other_step_size(fortran_reader, fortran_writer):
    '''Tests loops with a step size different from one, which affects
    the value to be assigned to replaced induction variables.
    '''
    source = '''program test
                integer i, ic1, ic2
                real, dimension(10) :: a
                do i = 1, 10, 5
                    ic1 = i+1
                    ic2 = 3
                    a(ic1) = 1+(ic1+1)*ic1
                end do
                end program test'''
    psyir = fortran_reader.psyir_from_source(source)
    loop = psyir.children[0].children[0]

    riv = ReplaceInductionVariablesTrans()
    riv.apply(loop)
    out = fortran_writer(psyir)
    assert "a(i + 1) = 1 + (i + 1 + 1) * (i + 1)" in out
    assert "ic1 = i - 5 + 1" in out
    assert "ic2 = 3" in out


# ----------------------------------------------------------------------------
@pytest.mark.parametrize("array_expr", ["ic(i)", "ic(2)", "t(i)%b", "t(1)%b",
                                        "myt%a(i)%b", "myt%a%b(i)%c",
                                        "myt%a(i)", "myt%a%b(i)"])
def test_riv_no_arrays(array_expr, fortran_reader, fortran_writer):
    '''Tests that no arrays are accepted as induction varibles.'''
    source = f'''program test
                use mymod
                integer i, invariant
                integer, dimension(10) :: ic
                type(my_type), dimension(10) :: t
                type(my_type):: myt
                real, dimension(10) :: a
                invariant = 1
                do i = 1, 10
                    {array_expr} = 12
                    a({array_expr}) = 1
                end do
                end program test'''
    psyir = fortran_reader.psyir_from_source(source)
    # The first child is the assignment to 'invariant'
    loop = psyir.children[0].children[1]

    # None of the statements can be moved, so the output
    # before and after the transformation should be identical:
    out_before = fortran_writer(loop)
    riv = ReplaceInductionVariablesTrans()
    riv.apply(loop)
    out_after = fortran_writer(loop)
    assert out_before == out_after


# ----------------------------------------------------------------------------
def test_riv_impure_function_calls(fortran_reader, fortran_writer):
    '''Tests that induction variables that use a function call are
    not replaced unless we can guarantee that the call is pure (since the
    function might return a different value on each call).
    '''
    source = '''program test
                integer i, ic1, ic2
                real, dimension(10) :: a
                do i = 1, 10, 5
                    ic1 = SIN(i)                  ! Pure function call
                    ic2 = GET_COMMAND_ARGUMENT(i) ! Impure function call
                    a(ic1) = 1+(ic1+1)*ic1
                    a(ic2) = 1+(ic2+1)*ic2
                end do
                end program test'''

    psyir = fortran_reader.psyir_from_source(source)
    loop = psyir.children[0].children[0]
    riv = ReplaceInductionVariablesTrans()
    riv.apply(loop)
    out = fortran_writer(loop)

    # ic1 has been replaced
    assert "a(SIN(i)) = 1 + (SIN(i) + 1) * SIN(i)" in out
    # ic2 has NOT been replaced
    assert "a(ic2) = 1 + (ic2 + 1) * ic2" in out
