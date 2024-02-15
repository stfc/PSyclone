# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
# Modified by R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified by I. Kavcic, Met Office
# Modified by A. B. G. Chalk, STFC Daresbury Lab

'''This module tests the loop fusion transformation.
'''

from __future__ import absolute_import, print_function

import pytest

from psyclone.psyir.nodes import Literal, Loop, Schedule, Return
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE
from psyclone.psyir.transformations import LoopFuseTrans, TransformationError
from psyclone.tests.utilities import Compile, get_invoke


# ----------------------------------------------------------------------------
def test_fusetrans_error_incomplete():
    ''' Check that we reject attempts to fuse loops which are incomplete. '''
    sch = Schedule()
    loop1 = Loop(variable=DataSymbol("i", INTEGER_TYPE))
    loop2 = Loop(variable=DataSymbol("j", INTEGER_TYPE))
    sch.addchild(loop1)
    sch.addchild(loop2)

    fuse = LoopFuseTrans()

    # Check first loop
    with pytest.raises(TransformationError) as err:
        fuse.validate(loop1, loop2)
    assert ("Error in LoopFuseTrans transformation. The target loop must have "
            "four children but found: []" in str(err.value))

    loop1.addchild(Literal("1", INTEGER_TYPE))
    loop1.addchild(Literal("3", INTEGER_TYPE))
    loop1.addchild(Literal("1", INTEGER_TYPE))
    loop1.addchild(Schedule())
    loop1.loop_body.addchild(Return())

    # Check second loop
    with pytest.raises(TransformationError) as err:
        fuse.validate(loop1, loop2)
    assert ("Error in LoopFuseTrans transformation. The target loop must have "
            "four children but found: []" in str(err.value))

    loop2.addchild(Literal("1", INTEGER_TYPE))
    loop2.addchild(Literal("3", INTEGER_TYPE))
    loop2.addchild(Literal("1", INTEGER_TYPE))
    loop2.addchild(Schedule())
    loop2.loop_body.addchild(Return())

    # Validation should now pass
    fuse.validate(loop1, loop2)


# ----------------------------------------------------------------------------
def test_fusetrans_error_not_same_parent():
    ''' Check that we reject attempts to fuse loops which don't share the
    same parent '''

    loop1 = Loop.create(DataSymbol("i", INTEGER_TYPE),
                        Literal("1", INTEGER_TYPE),
                        Literal("10", INTEGER_TYPE),
                        Literal("1", INTEGER_TYPE), [Return()])
    sch1 = Schedule()
    sch1.addchild(loop1)

    sch2 = Schedule()
    loop2 = Loop.create(DataSymbol("j", INTEGER_TYPE),
                        Literal("1", INTEGER_TYPE),
                        Literal("10", INTEGER_TYPE),
                        Literal("1", INTEGER_TYPE), [Return()])

    sch2.addchild(loop2)

    fuse = LoopFuseTrans()

    # Try to fuse loops with different parents
    with pytest.raises(TransformationError) as err:
        fuse.validate(loop1, loop2)
    assert ("Error in LoopFuseTrans transformation. Loops do not have the "
            "same parent" in str(err.value))


# ----------------------------------------------------------------------------
def fuse_loops(fortran_code, fortran_reader, fortran_writer):
    '''Helper function that fuses the first two nodes in the given
    Fortran code, and returns the fused Fortran code as string.
    If an error is detected by the used LoopFuseTrans transformation,
    it will raise a TransformationError.

    :param str fortran_code: the Fortran code to loop fuse.
    :param fortran_reader: the PSyIR Fortran frontend.
    :type fortran_reader: :py:class:`psyclone.psyir.frontend.fortran`
    :param fortran_writer: the PSyIR Fortran backend.
    :type fortran_writer: :py:class:`psyclone.psyir.backend.fortran`

    :returns: a 2-tuple of the fused Fortran code, and the PSyIR \
        representation of the supplied Fortran code.
    :rtype: 2-tuple of (str, :py:class:`psyclone.psyir.nodes.Container` or \
                             :py:class:`psyclone.psyir.nodes.Routine`)

    '''
    psyir = fortran_reader.psyir_from_source(fortran_code)
    fuse = LoopFuseTrans()
    # Raise the language-level PSyIR to NEMO PSyIR
    loop1 = psyir.children[0].children[0]
    loop2 = psyir.children[0].children[1]
    fuse.apply(loop1, loop2)

    return fortran_writer(psyir), psyir


# ----------------------------------------------------------------------------
def test_fuse_ok(tmpdir, fortran_reader, fortran_writer):
    '''This tests verifies that loop fusion can be successfully applied to
    conformant loops.

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
    out, psyir = fuse_loops(code, fortran_reader, fortran_writer)

    expected = """  do jj = 1, n, 1
    do ji = 1, 10, 1
      s(ji,jj) = t(ji,jj) + 1
    enddo
    do ji = 1, 10, 1
      s(ji,jj) = t(ji,jj) + 1
    enddo
  enddo"""
    assert expected in out
    assert Compile(tmpdir).string_compiles(out)

    # Then fuse the inner ji loops
    fuse = LoopFuseTrans()
    fuse.apply(psyir.children[0][0].loop_body[0],
               psyir.children[0][0].loop_body[1])

    out = fortran_writer(psyir)
    expected = """
  do jj = 1, n, 1
    do ji = 1, 10, 1
      s(ji,jj) = t(ji,jj) + 1
      s(ji,jj) = t(ji,jj) + 1
    enddo
  enddo"""
    assert expected in out
    assert Compile(tmpdir).string_compiles(out)

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
    out, _ = fuse_loops(code, fortran_reader, fortran_writer)
    expected = """  do jj = 2 - 1, n + 1 - 1, 1
    do ji = 1, 10, 1
      s(ji,jj) = t(ji,jj) + 1
    enddo
    do ji = 1, 10, 1
      s(ji,jj) = t(ji,jj) + 1
    enddo
  enddo"""
    assert expected in out
    assert Compile(tmpdir).string_compiles(out)


# ----------------------------------------------------------------------------
def test_fuse_incorrect_bounds_step(tmpdir, fortran_reader, fortran_writer):
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
        fuse_loops(code, fortran_reader, fortran_writer)
    assert "Loops do not have the same iteration space" in str(err.value)

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
        fuse_loops(code, fortran_reader, fortran_writer)
    assert "Loops do not have the same iteration space" in str(err.value)

    # Test step size:
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
        fuse_loops(code, fortran_reader, fortran_writer)
    assert "Loops do not have the same iteration space" in str(err.value)

    # Test step size - make sure it defaults to 1
    code = '''subroutine sub()
              integer :: ji, jj, n
              integer, dimension(10,10) :: s, t
              do jj=1, n, 1
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
    out, _ = fuse_loops(code, fortran_reader, fortran_writer)
    assert Compile(tmpdir).string_compiles(out)


# ----------------------------------------------------------------------------
def test_fuse_correct_bounds(tmpdir, fortran_reader, fortran_writer):
    '''
    Test that loop boundaries must be identical.
    '''
    code = '''subroutine sub()
              integer :: ji, jj, n
              integer, dimension(10,10) :: s, t
              do jj=2, n
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
    out, _ = fuse_loops(code, fortran_reader, fortran_writer)
    assert Compile(tmpdir).string_compiles(out)


# ----------------------------------------------------------------------------
def test_fuse_dimension_change(tmpdir, fortran_reader, fortran_writer):
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

    out, _ = fuse_loops(code, fortran_reader, fortran_writer)
    correct = """
  do jj = 1, n + 1, 1
    do ji = 1, 10, 1
      s(ji,jj) = t(ji,jj) + 1
    enddo
    do ji = 1, 10, 1
      s(ji,jj) = s(ji,jj) + t(jj,jj) + t(ji,ji)
    enddo
  enddo"""
    assert correct in out
    assert Compile(tmpdir).string_compiles(out)

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
        fuse_loops(code, fortran_reader, fortran_writer)
    assert ("Variable 's' is written to and the "
            "loop variable 'jj' is used in different index locations: "
            "s(ji,jj) and s(jj,ji)."
            in str(err.value))

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
        fuse_loops(code, fortran_reader, fortran_writer)
    assert ("Variable 's' is written to and the loop variable 'jj' is "
            "used in different index locations: s(jj,ji) and s(ji,jj)."
            in str(err.value))

    # Same test using a structure type:
    code = '''subroutine sub()
              use my_module
              integer :: ji, jj, n
              type(my_type) :: s, t, u
              do jj=1, n+1
                 do ji=1, 10
                    u%comp1(ji)%comp2(jj)=s%comp1(jj)%comp2(ji)+1
                 enddo
              enddo
              do jj=1, n+1
                 do ji=1, 10
                    s%comp1(ji)%comp2(jj)=t%comp1(ji)%comp2(jj)+1
                 enddo
              enddo
              end subroutine sub'''

    with pytest.raises(TransformationError) as err:
        fuse_loops(code, fortran_reader, fortran_writer)
    assert ("Variable 's' is written to and the loop variable 'jj' is used "
            "in different index locations: s%comp1(jj)%comp2(ji) and "
            "s%comp1(ji)%comp2(jj)."
            in str(err.value))


# ----------------------------------------------------------------------------
def test_fuse_independent_array(fortran_reader, fortran_writer):
    '''Test that using arrays which are not dependent on the loop variable
    are handled correctly. Example:
    do j  ... a(1) = b(j) * c(j)
    do j ...  d(j) = a(1)
    '''

    # The first example can be merged, since 's' does not
    # depend on the loop variable, and it is written and read.
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
        fuse_loops(code, fortran_reader, fortran_writer)
    assert ("Variable 's' does not depend on loop variable 'jj', but is "
            "read and written" in str(err.value))


# ----------------------------------------------------------------------------
def test_fuse_scalars(tmpdir, fortran_reader, fortran_writer):
    '''Test that using scalars work as expected in all combinations of
    being read/written in both loops.
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
    out, _ = fuse_loops(code, fortran_reader, fortran_writer)
    assert Compile(tmpdir).string_compiles(out)

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
        fuse_loops(code, fortran_reader, fortran_writer)
    assert ("Scalar variable 'a' is written in one loop, but only read in "
            "the other loop." in str(err.value))

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
        fuse_loops(code, fortran_reader, fortran_writer)
    assert "Scalar variable 'b' is written in one loop, but only read in " \
           "the other loop." in str(err.value)

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
    out, _ = fuse_loops(code, fortran_reader, fortran_writer)
    assert Compile(tmpdir).string_compiles(out)


@pytest.mark.xfail(reason="Variable usage does not handle conditional - #641")
def test_fuse_scalars_incorrect(fortran_reader, fortran_writer):
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
        fuse_loops(code, fortran_reader, fortran_writer)
    assert ("Scalar variable 'b' might not be written in one loop"
            in str(err.value))


# ----------------------------------------------------------------------------
def test_fuse_no_symbol(fortran_reader, fortran_writer):
    '''Tests what happens if a variable name is not in the symbol table,
    e.g. because of a wildcard import. It also checks if a name is defined
    in an outer module.
    '''
    # Case 1: assume that the array 't' is imported from mymod. In
    # this case the loop validation will find a Symbol (not a DataSymbol),
    # and cannot test if this variable is an array. It should fall back
    # to use the variable accesses information (which includes indices),
    # knowing this way that this is an array.
    code = '''subroutine sub()
              use mymod
              integer :: ji, jj, n
              integer, dimension(10,10) :: s
              do jj=1, n
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              do jj=1, n
                 do ji=1, 10
                    t(ji, jj) = s(ji, jj) + t(ji, jj)
                 enddo
              enddo
              end subroutine sub'''
    out, psyir = fuse_loops(code, fortran_reader, fortran_writer)
    assert """
  do jj = 1, n, 1
    do ji = 1, 10, 1
      s(ji,jj) = t(ji,jj) + 1
    enddo
    do ji = 1, 10, 1
      t(ji,jj) = s(ji,jj) + t(ji,jj)
    enddo
  enddo""" in out

    fuse = LoopFuseTrans()
    # Case 2: Symbol 't' is defined in outer module:
    code = '''
    module mymod
        integer, dimension(10, 10) :: t
    contains
        subroutine sub()
            integer :: ji, jj, n
            integer, dimension(10,10) :: s
            do jj=1, n
               do ji=1, 10
                  s(ji, jj)=t(ji, jj)+1
               enddo
            enddo
            do jj=1, n
               do ji=1, 10
                  t(ji, jj) = s(ji, jj) + t(ji, jj)
               enddo
            enddo
        end subroutine sub
    end module mymod'''
    psyir = fortran_reader.psyir_from_source(code)
    # First child is now the subroutine, which has
    # two children which are the two loops:
    loop1 = psyir.children[0].children[0][0]
    loop2 = psyir.children[0].children[0][1]
    fuse.apply(loop1, loop2)

    out = fortran_writer(psyir)
    assert """
    do jj = 1, n, 1
      do ji = 1, 10, 1
        s(ji,jj) = t(ji,jj) + 1
      enddo
      do ji = 1, 10, 1
        t(ji,jj) = s(ji,jj) + t(ji,jj)
      enddo
    enddo""" in out


def test_loop_fuse_different_iterates_over(fortran_reader):
    ''' Test that an appropriate error is raised when we attempt to
    fuse two loops that have differing values of ITERATES_OVER '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0, dist_mem=False)
    schedule = invoke.schedule
    fuse = LoopFuseTrans()

    # TODO 1731: For PSyLoops it currently only compares the iterates_over
    # attribute, but this could be just a computed property so comparing the
    # generic loop bounds would be enough. Otherwise this should be moved
    # into a PSyLoopFuseTrans specialization.
    with pytest.raises(TransformationError) as err:
        fuse.apply(schedule.children[0], schedule.children[1])
    assert "Loops do not have the same iteration space" in str(err.value)

    # Generic loops compare the loop bounds
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
    psyir = fortran_reader.psyir_from_source(code)
    loop1 = psyir.children[0].children[0]
    loop2 = psyir.children[0].children[1]
    with pytest.raises(TransformationError) as err:
        fuse.apply(loop1, loop2)
    assert ("Error in LoopFuseTrans transformation. Loops do not have "
            "the same iteration space" in str(err.value))

    # But symbolic differences are handled properly
    code = '''subroutine sub()
              integer :: ji, jj, n
              integer, dimension(10,10) :: s, t
              do jj=1, n
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              do jj=3-2, 1+n+n-n-1, (-1)*(-1)
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              end subroutine sub'''
    psyir = fortran_reader.psyir_from_source(code)
    loop1 = psyir.children[0].children[0]
    loop2 = psyir.children[0].children[1]
    fuse.apply(loop1, loop2)


def test_loop_fuse_different_variables(fortran_reader, fortran_writer):
    '''Test that fusing loops with different variables is possible, and
    renaming works appropriately.'''
    code = '''subroutine sub()
    integer :: ji, jj, n, jk
    integer, dimension(10, 10) :: s, t
    do jj = 1, n
      do ji = 1, 10
        s(ji, jj) = t(ji, jj) + 1
      end do
      do jk = 1, 10
        s(jk, jj) = t(jk, jj) + 1
      end do
    end do
    end subroutine sub'''
    psyir = fortran_reader.psyir_from_source(code)
    loops = psyir.children[0].walk(Loop)
    fuse = LoopFuseTrans()
    fuse.apply(loops[1], loops[2])
    out = fortran_writer(psyir)
    correct = '''subroutine sub()
  integer :: ji
  integer :: jj
  integer :: n
  integer :: jk
  integer, dimension(10,10) :: s
  integer, dimension(10,10) :: t

  do jj = 1, n, 1
    do ji = 1, 10, 1
      s(ji,jj) = t(ji,jj) + 1
      s(ji,jj) = t(ji,jj) + 1
    enddo
  enddo

end subroutine sub'''
    assert correct in out


def test_loop_fuse_different_variables_with_access(fortran_reader):
    '''Test that fusing loops with different variables is disallowed when
    either loop uses the other loops variable for any reason.'''
    code = '''subroutine sub()
    integer :: ji, jj, n, jk
    integer, dimension(10, 10) :: s, t
    do jj = 1, n
      do ji = 1, 10
        s(ji, jj) = t(ji, jj) + 1
      end do
      do jk = 1, 10
        ji = jk
        s(jk, jj) = t(jk, jj) + ji
      end do
    end do
    end subroutine sub'''
    psyir = fortran_reader.psyir_from_source(code)
    loops = psyir.children[0].walk(Loop)
    fuse = LoopFuseTrans()
    with pytest.raises(TransformationError) as excinfo:
        fuse.apply(loops[1], loops[2])
    assert ("Error in LoopFuseTrans transformation. Second loop contains "
            "accesses to the first loop's variable: ji." in str(excinfo.value))

    code = '''subroutine sub()
    integer :: ji, jj, n, jk
    integer, dimension(10, 10) :: s, t
    do jj = 1, n
      do ji = 1, 10
        jk = ji
        s(ji, jj) = t(ji, jj) + jk
      end do
      do jk = 1, 10
        s(jk, jj) = t(jk, jj) + ji
      end do
    end do
    end subroutine sub'''
    psyir = fortran_reader.psyir_from_source(code)
    loops = psyir.children[0].walk(Loop)
    fuse = LoopFuseTrans()
    with pytest.raises(TransformationError) as excinfo:
        fuse.apply(loops[1], loops[2])
    assert ("Error in LoopFuseTrans transformation. First loop contains "
            "accesses to the second loop's variable: jk."
            in str(excinfo.value))
