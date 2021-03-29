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
# ----------------------------------------------------------------------------
# Author: J. Henrichs, Bureau of Meteorology
# Modifies R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic, Met Office

'''This module tests the loop fusion transformation.
'''

from __future__ import absolute_import, print_function

import pytest

from fparser.common.readfortran import FortranStringReader

from psyclone.domain.nemo.transformations import NemoLoopFuseTrans
from psyclone.psyGen import PSyFactory
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import Literal, Loop, Schedule, Return
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE
from psyclone.psyir.transformations import LoopFuseTrans, TransformationError


# ----------------------------------------------------------------------------
def test_fusetrans_error_incomplete():
    ''' Check that we reject attempts to fuse loops which are incomplete. '''
    sch = Schedule()
    loop1 = Loop(variable=DataSymbol("i", INTEGER_TYPE), parent=sch)
    loop2 = Loop(variable=DataSymbol("j", INTEGER_TYPE), parent=sch)
    sch.addchild(loop1)
    sch.addchild(loop2)

    fuse = LoopFuseTrans()

    # Check first loop
    with pytest.raises(TransformationError) as err:
        fuse.validate(loop1, loop2)
    assert ("Error in LoopFuseTrans transformation. The target loop must have "
            "four children but found: []" in str(err.value))

    loop1.addchild(Literal("start", INTEGER_TYPE, parent=loop1))
    loop1.addchild(Literal("stop", INTEGER_TYPE, parent=loop1))
    loop1.addchild(Literal("step", INTEGER_TYPE, parent=loop1))
    loop1.addchild(Schedule(parent=loop1))
    loop1.loop_body.addchild(Return(parent=loop1.loop_body))

    # Check second loop
    with pytest.raises(TransformationError) as err:
        fuse.validate(loop1, loop2)
    assert ("Error in LoopFuseTrans transformation. The target loop must have "
            "four children but found: []" in str(err.value))

    loop2.addchild(Literal("start", INTEGER_TYPE, parent=loop2))
    loop2.addchild(Literal("stop", INTEGER_TYPE, parent=loop2))
    loop2.addchild(Literal("step", INTEGER_TYPE, parent=loop2))
    loop2.addchild(Schedule(parent=loop2))
    loop2.loop_body.addchild(Return(parent=loop2.loop_body))

    # Validation should now pass
    fuse.validate(loop1, loop2)


# ----------------------------------------------------------------------------
def test_fusetrans_error_not_same_parent():
    ''' Check that we reject attempts to fuse loops which don't share the
    same parent '''

    sch1 = Schedule()
    sch2 = Schedule()
    loop1 = Loop(variable=DataSymbol("i", INTEGER_TYPE), parent=sch1)
    loop2 = Loop(variable=DataSymbol("j", INTEGER_TYPE), parent=sch2)
    sch1.addchild(loop1)
    sch2.addchild(loop2)

    loop1.addchild(Literal("1", INTEGER_TYPE, parent=loop1))  # start
    loop1.addchild(Literal("10", INTEGER_TYPE, parent=loop1))  # stop
    loop1.addchild(Literal("1", INTEGER_TYPE, parent=loop1))  # step
    loop1.addchild(Schedule(parent=loop1))  # loop body

    loop2.addchild(Literal("1", INTEGER_TYPE, parent=loop2))  # start
    loop2.addchild(Literal("10", INTEGER_TYPE, parent=loop2))  # stop
    loop2.addchild(Literal("1", INTEGER_TYPE, parent=loop2))  # step
    loop2.addchild(Schedule(parent=loop2))  # loop body

    fuse = LoopFuseTrans()

    # Try to fuse loops with different parents
    with pytest.raises(TransformationError) as err:
        fuse.validate(loop1, loop2)
    assert ("Error in LoopFuseTrans transformation. Loops do not have the "
            "same parent" in str(err.value))


# ----------------------------------------------------------------------------
def fuse_loops(parser=None, fortran_code=None):
    '''Helper function that fuses the first two nodes in the given
    Fortran code, and returns the fused Fortran code as string.

    :param string fortran_code: the Fortran code to loop fuse.

    :returns: String representation of fused code.
    :rtype: str

    :raises TransformationError: if the loop fuse transformation cannot
        be applied.
    '''
    reader = FortranStringReader(fortran_code)
    ast = parser(reader)
    psy = PSyFactory("nemo").create(ast)
    schedule = psy.invokes.get("sub").schedule

    loop1 = schedule.children[0]
    loop2 = schedule.children[1]
    fuse = NemoLoopFuseTrans()
    fuse.apply(loop1, loop2)
    writer = FortranWriter()
    return writer(schedule), schedule


# ----------------------------------------------------------------------------
def test_fuse_ok(parser):
    '''This tests verifies that the valid loop fuse statements work
    as expected.
    '''

    code = '''subroutine sub()
              integer :: ji, jj, n
              integer, dimension(10,10) :: s, t
              do jj=1, n
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              do jj=1, n
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              end subroutine sub'''
    out, schedule = fuse_loops(parser, code)
    expected = """do jj = 1, n, 1
  do ji = 1, 10, 1
    s(ji,jj) = t(ji,jj) + 1
  enddo
  do ji = 1, 10, 1
    s(ji,jj) = t(ji,jj) + 1
  enddo
enddo"""
    assert expected in out

    # Then fuse the inner ji loops
    fuse = NemoLoopFuseTrans()
    fuse.apply(schedule[0].loop_body[0],
               schedule[0].loop_body[1])
    writer = FortranWriter()

    out = writer(schedule)

    expected = """do jj = 1, n, 1
  do ji = 1, 10, 1
    s(ji,jj) = t(ji,jj) + 1
    s(ji,jj) = t(ji,jj) + 1
  enddo
enddo"""
    assert expected in out

    # Test more complex loop boundaries. Note that
    # we might actually consider simplifying these
    # expressions
    code = '''subroutine sub()
              integer :: ji, jj, n
              integer, dimension(10,10) :: s, t
              do jj=2-1, n+1-1
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              do jj=2-1, n+1-1
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              end subroutine sub'''
    out, _ = fuse_loops(parser, code)
    expected = """do jj = 2 - 1, n + 1 - 1, 1
  do ji = 1, 10, 1
    s(ji,jj) = t(ji,jj) + 1
  enddo
  do ji = 1, 10, 1
    s(ji,jj) = t(ji,jj) + 1
  enddo
enddo"""
    assert expected in out


# ----------------------------------------------------------------------------
def test_fuse_incorrect_bounds_step(parser):
    '''
    Test that loop boundaries and step size must be identical.
    '''

    # Lower loop boundary
    code = '''subroutine sub()
              integer :: ji, jj, n
              integer, dimension(10,10) :: s, t
              do jj=1, n
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              do jj=2, n
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              end subroutine sub'''
    with pytest.raises(TransformationError) as err:
        fuse_loops(parser, code)
    assert "Lower loop bounds must be identical, but are" in str(err.value)

    # Upper loop boundary
    code = '''subroutine sub()
              integer :: ji, jj, n
              integer, dimension(10,10) :: s, t
              do jj=1, n
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              do jj=1, n+1
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              end subroutine sub'''
    with pytest.raises(TransformationError) as err:
        fuse_loops(parser, code)
    assert "Upper loop bounds must be identical, but are" in str(err.value)

    # Test step size
    code = '''subroutine sub()
              integer :: ji, jj, n
              integer, dimension(10,10) :: s, t
              do jj=1, n, 2
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              do jj=1, n
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              end subroutine sub'''
    with pytest.raises(TransformationError) as err:
        fuse_loops(parser, code)
    assert "Step size in loops must be identical, but are" in str(err.value)

    # Test step size - make sure it defaults to 1
    code = '''subroutine sub()
              integer :: ji, jj, n
              integer, dimension(10,10) :: s, t
              do jj=1, n, 1
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              do jj=1, n, 1
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              end subroutine sub'''
    fuse_loops(parser, code)


# ----------------------------------------------------------------------------
def test_fuse_different_loop_vars(parser):
    '''
    Test that loop variables are identicla
    '''
    code = '''subroutine sub()
              integer :: ji, jj, n
              integer, dimension(10,10) :: s, t
              do jj=1, n
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              do ji=1, n
                 do jj=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              end subroutine sub'''
    with pytest.raises(TransformationError) as err:
        fuse_loops(parser, code)
    assert "Loop variables must be the same, but are 'jj' and 'ji'" \
        in str(err.value)


# ----------------------------------------------------------------------------
@pytest.mark.xfail(reason="Needs evaluation of constant expressions")
def test_fuse_correct_bounds(parser):
    '''
    Test that loop boundaries must be identical.
    '''
    # TODO: This test needs evaluation
    # of constant expressions in PSyclone
    code = '''subroutine sub()
              integer :: ji, jj, n
              integer, dimension(10,10) :: s, t
              do jj=2-1, n
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              do jj=2, n+1-1
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              end subroutine sub'''
    fuse_loops(parser, code)


# ----------------------------------------------------------------------------
def test_fuse_dimension_change(parser):
    '''Test that inconsistent use of dimemsions are detected, e.g.:
    loop1:  a(i,j)
    loop2:  a(j,i)
    when at least one operation is a write
    '''

    # The first example can be merged, since 't' is read-only,
    # so it doesn't matter that it is accessed differently
    code = '''subroutine sub()
              integer :: ji, jj, n
              integer, dimension(10,10) :: s, t
              do jj=1, n+1
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              do jj=1, n+1
                 do ji=1, 10
                    s(ji, jj)=s(ji, jj) + t(jj, jj) + t(ji, ji)
                 enddo
              enddo
              end subroutine sub'''

    fuse_loops(parser, code)

    # This cannot be fused, since 's' is written in the
    # first iteration and read in the second.
    code = '''subroutine sub()
              integer :: ji, jj, n
              integer, dimension(10,10) :: s, t, u
              do jj=1, n+1
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              do jj=1, n+1
                 do ji=1, 10
                    u(ji, jj)=s(jj, ji)+1
                 enddo
              enddo
              end subroutine sub'''

    with pytest.raises(TransformationError) as err:
        fuse_loops(parser, code)
    assert "Variable 's' is using loop variable jj in index 1 and 0." \
        in str(err.value)

    # This cannot be fused, since 's' is read in the
    # first iteration and written in the second with
    # different indices.
    code = '''subroutine sub()
              integer :: ji, jj, n
              integer, dimension(10,10) :: s, t, u
              do jj=1, n+1
                 do ji=1, 10
                    u(ji, jj)=s(jj, ji)+1
                 enddo
              enddo
              do jj=1, n+1
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              end subroutine sub'''

    with pytest.raises(TransformationError) as err:
        fuse_loops(parser, code)
    assert "Variable 's' is using loop variable jj in index 0 and 1." \
        in str(err.value)


# ----------------------------------------------------------------------------
def test_fuse_independent_array(parser):
    '''Test that using arrays which are not dependent on the loop variable
    are handled correctly. Example:
    do j  ... a(1) = b(j) * c(j)
    do j ...  d(j) = a(1)
    '''

    # The first example can be merged, since 't' is read-only,
    # so it doesn't matter that it is accessed differently
    code = '''subroutine sub()
              integer :: ji, jj, n
              integer, dimension(10,10) :: s, t
              do jj=1, n
                 do ji=1, 10
                    s(1, 1)=t(ji, jj)+1
                 enddo
              enddo
              do jj=1, n
                 do ji=1, 10
                    t(ji, jj) = s(1, 1) + t(ji, jj)
                 enddo
              enddo
              end subroutine sub'''

    with pytest.raises(TransformationError) as err:
        fuse_loops(parser, code)
    assert "Variable 's' does not depend on loop variable 'jj', but is " \
           "read and written" in str(err.value)


# ----------------------------------------------------------------------------
def test_fuse_scalars(parser):
    '''Test that using arrays which are not dependent on the loop variable
    are handled correctly. Example:
    do j  ... a(1) = b(j) * c(j)
    do j ...  d(j) = a(1)
    '''

    # First test: read/read of scalar variable
    code = '''subroutine sub()
              integer :: ji, jj, n
              real, dimension(10,10) :: s, t
              real                   :: a
              do jj=1, n
                 do ji=1, 10
                    s(ji, jj) = t(ji, jj) + a
                 enddo
              enddo
              do jj=1, n
                 do ji=1, 10
                    t(ji, jj) = t(ji, jj) - a
                 enddo
              enddo
              end subroutine sub'''
    fuse_loops(parser, code)

    # Second test: read/write of scalar variable
    code = '''subroutine sub()
              integer :: ji, jj, n
              real, dimension(10,10) :: s, t
              real                   :: a
              do jj=1, n
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+a
                 enddo
              enddo
              do jj=1, n
                 do ji=1, 10
                    a = t(ji, jj) - 2
                    s(ji, jj)=t(ji, jj)+a
                 enddo
              enddo
              end subroutine sub'''

    with pytest.raises(TransformationError) as err:
        fuse_loops(parser, code)
    assert "Scalar variable 'a' is written in one loop, but only read in " \
           "other loop." in str(err.value)

    # Third test: write/read of scalar variable
    code = '''subroutine sub()
              integer :: ji, jj, n
              real, dimension(10,10) :: s, t
              real                   :: b
              do jj=1, n
                 do ji=1, 10
                    b = t(ji, jj) - 2
                    s(ji, jj )=t(ji, jj)+b
                 enddo
              enddo
              do jj=1, n
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+b
                 enddo
              enddo
              end subroutine sub'''

    with pytest.raises(TransformationError) as err:
        fuse_loops(parser, code)
    assert "Scalar variable 'b' is written in one loop, but only read in " \
           "other loop." in str(err.value)

    # Fourth test: write/write of scalar variable - this is ok
    code = '''subroutine sub()
              integer :: ji, jj, n
              real, dimension(10,10) :: s, t
              real                   :: b
              do jj=1, n
                 do ji=1, 10
                    b = t(ji, jj) - 2
                    s(ji, jj )=t(ji, jj)+b
                 enddo
              enddo
              do jj=1, n
                 do ji=1, 10
                    b = sqrt(t(ji, jj))
                    s(ji, jj)=t(ji, jj)+b
                 enddo
              enddo
              end subroutine sub'''
    fuse_loops(parser, code)


@pytest.mark.xfail(reason="Variable usage does not handle conditional - #641")
def test_fuse_scalars_incorrect(parser):
    '''This example incorrectly allows loop fusion due to known
    restrictions of the variable access detection. If t(1,1)
    would be < 0, the second loop would use ``t(10,10)-2`` as value
    for b (last value assigned to ``b`` in first loop). But after
    fusing the loop, b would be ``t(1,1)-2``
    '''
    code = '''subroutine sub()
              integer :: ji, jj, n
              real, dimension(10,10) :: s, t
              real                   :: b
              do jj=1, 10
                 do ji=1, 10
                    b = t(ji, jj) - 2
                    s(ji, jj )=t(ji, jj)+b
                 enddo
              enddo
              do jj=1, 10
                 do ji=1, 10
                    if (t(ji,jj) > 0) then
                        b = sqrt(t(ji, jj))
                    endif
                    s(ji, jj)=t(ji, jj)+b
                 enddo
              enddo
              end subroutine sub'''
    with pytest.raises(TransformationError) as err:
        fuse_loops(parser, code)
    assert "Scalar variable 'b' might not be written in one loop" \
        in str(err.value)
