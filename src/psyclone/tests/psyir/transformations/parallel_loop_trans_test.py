# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2025, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab

''' pytest tests for the parallel_loop_trans module. '''

import pytest

from psyclone.psyir.nodes import Loop, OMPParallelDoDirective
from psyclone.psyir.transformations import (
    ParallelLoopTrans, TransformationError)
from psyclone.psyir.tools import DependencyTools, DTCode
from psyclone.tests.utilities import get_invoke


class ParaTrans(ParallelLoopTrans):
    '''
    Concrete implementation of virtual ParallelLoopTrans class to permit
    testing of methods of that class.

    '''
    def _directive(self, children, collapse=None):
        '''
        Creates an OMP Parallel Do directive for the purposes of testing.
        '''
        return OMPParallelDoDirective(children=children, collapse=collapse)


CODE = '''
subroutine my_sub()
  integer ji, jj
  real :: var1(10,10), sum
  sum = 0.0
  var1 = 1.0
  do ji = 1, 10
    do jj = 1, 10
      sum = sum + var1(ji, jj)
    end do
  end do
end subroutine my_sub'''


def test_paralooptrans_validate_force(fortran_reader):
    '''
    Test that the 'force' option allows the validate check to succeed even
    when the dependency analysis finds a possible loop-carried dependency.

    '''
    psyir = fortran_reader.psyir_from_source(CODE)
    loop = psyir.walk(Loop)[0]
    trans = ParaTrans()
    with pytest.raises(TransformationError) as err:
        trans.validate(loop)
    assert "Loop cannot be parallelised because:" in str(err.value)
    # Set the 'force' option to True - no exception should be raised.
    trans.validate(loop, {"force": True})


def test_paralooptrans_validate_pure_calls(fortran_reader):
    ''' Test that the validation checks for calls that are not guaranteed
    to be pure unless the 'force' option is provided.
    '''
    psyir = fortran_reader.psyir_from_source('''
        subroutine my_sub()
          use other, only: my_sub2
          integer :: i
          real :: var(10) = 1

          do i = LBOUND(var), UBOUND(var)  ! Inquiry calls are pure
            var(i) = max(i, 3)             ! max is pure
            call my_sub2(i)                ! purity is unknown
          end do
        end subroutine my_sub''')

    loop = psyir.walk(Loop)[0]
    trans = ParaTrans()
    # Check that we reject non-integer collapse arguments
    with pytest.raises(TransformationError) as err:
        trans.validate(loop, {"verbose": True})
    assert ("Loop cannot be parallelised because it cannot guarantee that "
            "the following calls are pure: ['my_sub2']" in str(err.value))

    # Check that forcing the transformation or setting it to "pure" let the
    # validation pass
    trans.validate(loop.copy(), {"force": True})
    loop.scope.symbol_table.lookup("my_sub2").is_pure = True
    trans.validate(loop)


def test_paralooptrans_validate_loop_inside_pure(fortran_reader):
    ''' Test that the validation checks that we don't attempt to parallelise
    a loop inside a pure (or elemental) routine
    '''
    psyir = fortran_reader.psyir_from_source('''
    program test_prog
        real, dimension(10) :: var = 1
        integer :: j
        do j = LBOUND(var), UBOUND(var)
          var(j) = var(j) + 1
        end do
    end program
    module test
        contains
        elemental function my_func(a)
          real, intent(in) :: a(10)
          real, intent(out) :: my_func(10)
          integer :: i

          do i = LBOUND(a), UBOUND(a)
            my_func(i) = a(i) + 1
          end do
        end function

        pure subroutine my_sub(a,b)
          real, intent(in) :: a(10)
          real, intent(out) :: b(10)
          integer :: i

          do i = LBOUND(a), UBOUND(a)
            b(i) = a(i) + 1
          end do
        end subroutine
    end module
    ''')

    trans = ParaTrans()
    loops = psyir.walk(Loop)

    # The first one succeeds as it is not inside a pure function
    trans.validate(loops[0], {"verbose": True})

    for loop in loops[1:]:
        # Check that we reject parallelisng inside a pure routine
        with pytest.raises(TransformationError) as err:
            trans.validate(loop, {"verbose": True})
        assert ("Loops inside a pure (or elemental) routine cannot be "
                "parallelised, but attempted to parallelise loop inside '"
                in str(err.value))


def test_paralooptrans_validate_ignore_dependencies_for(fortran_reader,
                                                        fortran_writer):
    '''
    Test that the 'ignore_dependencies_for' option allows the validate check to
    succeed even when the dependency analysis finds a possible loop-carried
    dependency, but the user guarantees that it's a false dependency. It also
    checks that the appopriate comments are added when dependencies are found.

    '''
    psyir = fortran_reader.psyir_from_source(CODE)
    loop = psyir.walk(Loop)[0]
    trans = ParaTrans()
    with pytest.raises(TransformationError) as err:
        trans.validate(loop, options={"verbose": True})
    assert ("Loop cannot be parallelised because:\nWarning: Variable 'sum' is "
            "read first, which indicates a reduction. Variable: 'sum'."
            in str(err.value))
    # With the verbose option, the dependency will be reported in a comment
    assert ("PSyclone: Loop cannot be parallelised because:"
            in loop.preceding_comment)

    # Test that the inner loop does not log again the same error message
    with pytest.raises(TransformationError) as err:
        trans.validate(loop.loop_body[0], options={"verbose": True})
    assert ("PSyclone: Loop cannot be parallelised because the dependency"
            not in loop.loop_body[0].preceding_comment)

    # But if it's not already in an ancestor, it adds the comment there
    loop.preceding_comment = ""
    with pytest.raises(TransformationError) as err:
        trans.validate(loop.loop_body[0], options={"verbose": True})
    assert ("PSyclone: Loop cannot be parallelised because:"
            in loop.loop_body[0].preceding_comment)

    # Now use the 'ignore_dependencies_for' option
    with pytest.raises(TypeError) as err:
        trans.validate(loop, {"ignore_dependencies_for": "sum"})
    assert ("The 'ignore_dependencies_for' option must be an Iterable object "
            "containing str representing the symbols to ignore, but"
            " got 'sum'.") in str(err.value)
    # Set the ignore_dependencies_for option to ignore "sum"
    trans.validate(loop, {"ignore_dependencies_for": ["sum"]})


def test_paralooptrans_apply_collapse(fortran_reader, fortran_writer):
    ''' Test the 'collapse' option with valid bool and integer values. '''
    trans = ParaTrans()
    psyir = fortran_reader.psyir_from_source('''
        subroutine my_sub()
          integer :: i, j, k
          real :: var(10,10,10) = 1

          do i = 1, 10
            do j = 1, 10
              do k = 1, 10
                var(i,j,k) = var(i, j, k) + 1
              end do
            end do
          end do
        end subroutine my_sub''')

    # When 'collapse' is an integer, it won't collapse more than the requested
    # number of loops. But it won't fail if it can't
    test_loop = psyir.copy().walk(Loop, stop_type=Loop)[0]
    trans.apply(test_loop, {"collapse": 2})
    assert ("!$omp parallel do collapse(2)"
            in fortran_writer(test_loop.parent.parent))

    test_loop = psyir.copy().walk(Loop, stop_type=Loop)[0]
    trans.apply(test_loop, {"collapse": 4})
    assert ("!$omp parallel do collapse(3)"
            in fortran_writer(test_loop.parent.parent))

    # When collapse is a bool, it will collapse all possible loops when True
    test_loop = psyir.copy().walk(Loop, stop_type=Loop)[0]
    trans.apply(test_loop, {"collapse": True})
    assert ("!$omp parallel do collapse(3)"
            in fortran_writer(test_loop.parent.parent))


def test_paralooptrans_collapse_options(fortran_reader, fortran_writer):
    '''
    Test the 'collapse' option, also in combination with the 'force' and
    'ignore_dependencies_for' options.
    '''
    psyir = fortran_reader.psyir_from_source('''
        subroutine my_sub()
          integer :: i, j, k
          real :: var(10,10,10) = 1
          integer, dimension(10) :: map

          do i = 1, 10  ! This loop is iteration independent
            do j = 1, 10  ! This loop has a loop-carried dependency in var
              do k = 1, j  ! This loop bound depends on previous indices
                var(i,j,k) = var(i, map(j), k)
              end do
            end do
          end do
        end subroutine my_sub''')
    loop = psyir.walk(Loop)[0]
    trans = ParaTrans()
    # The validation does not see any problem because the outer loop can be
    # parallel
    trans.validate(loop)

    # By default it stops at collapse 1, because loop 2 has a dependency on,
    # var. With verbose it adds the stopping reason as a comment.
    test_loop = psyir.copy().walk(Loop, stop_type=Loop)[0]
    trans.apply(test_loop, {"collapse": True, "verbose": True})
    assert '''\
!$omp parallel do collapse(1) default(shared), private(i,j,k)
do i = 1, 10, 1
  ! Error: The write access to 'var(i,j,k)' and the read access to 'var(i,\
map(j),k)' are dependent and cannot be parallelised. Variable: 'var'. \
Consider using the "ignore_dependencies_for" transformation option if this \
is a false dependency.
  do j = 1, 10, 1
    do k = 1, j, 1
      var(i,j,k) = var(i,map(j),k)
    enddo
  enddo
enddo
''' in fortran_writer(test_loop.parent.parent)

    # Check that the collapse logic can uses the "ignore_dependencies_for" and
    # "force" logic to skip dependencies
    test_loop = psyir.copy().walk(Loop, stop_type=Loop)[0]
    trans.apply(test_loop, {"collapse": True, "verbose": True,
                            "ignore_dependencies_for": ["var"]})
    assert '''\
!$omp parallel do collapse(2) default(shared), private(i,j,k)
do i = 1, 10, 1
  do j = 1, 10, 1
    ! Loop cannot be collapsed because one of the bounds depends on the \
previous iteration variable 'j'
    do k = 1, j, 1
      var(i,j,k) = var(i,map(j),k)
    enddo
  enddo
enddo
''' in fortran_writer(test_loop.parent.parent)

    test_loop = psyir.copy().walk(Loop, stop_type=Loop)[0]
    trans.apply(test_loop, {"collapse": True, "verbose": True, "force": True})
    assert '''\
!$omp parallel do collapse(2) default(shared), private(i,j,k)
do i = 1, 10, 1
  do j = 1, 10, 1
    ! Loop cannot be collapsed because one of the bounds depends on the \
previous iteration variable 'j'
    do k = 1, j, 1
      var(i,j,k) = var(i,map(j),k)
    enddo
  enddo
enddo
''' in fortran_writer(test_loop.parent.parent)

    # Also it won't collapse if the loop inside is not perfectly nested,
    # regardless of the force option.
    psyir = fortran_reader.psyir_from_source('''
        subroutine my_sub()
          integer :: i, j, k
          real :: var(10,10,10) = 1
          integer, dimension(10) :: map

          do i = 1, 10  ! This loop is iteration independent
            do j = 1, 10  ! This loop has a loop-carried dependency in var1
              do k = 1, 10
                var(i,j,k) = var(i, map(j), k)
              end do
              var(i,j,5) = 0
            end do
          end do
        end subroutine my_sub''')
    loop = psyir.walk(Loop)[0]
    trans = ParaTrans()
    test_loop = psyir.copy().walk(Loop, stop_type=Loop)[0]
    trans.apply(test_loop, {"collapse": True, "verbose": True, "force": True})
    assert '''\
!$omp parallel do collapse(2) default(shared), private(i,j,k)
do i = 1, 10, 1
  do j = 1, 10, 1
    ! Loop cannot be collapsed because it has siblings
    do k = 1, 10, 1
      var(i,j,k) = var(i,map(j),k)
    enddo
    var(i,j,5) = 0
  enddo
enddo
''' in fortran_writer(test_loop.parent.parent)


def test_paralooptrans_validate_sequential(fortran_reader):
    '''
    Test that the 'sequential' option allows the validate check to succeed even
    when the dependency analysis finds a possible loop-carried dependency.

    '''
    psyir = fortran_reader.psyir_from_source(CODE)
    loop = psyir.walk(Loop)[0]
    trans = ParaTrans()
    with pytest.raises(TransformationError) as err:
        trans.validate(loop)
    assert "Loop cannot be parallelised because" in str(err.value)
    # Set the 'sequential' option to True - no exception should be raised.
    trans.validate(loop, {"sequential": True})


def test_paralooptrans_validate_collapse(fortran_reader):
    '''
    Test the various validation checks on the 'collapse' option.

    '''
    psyir = fortran_reader.psyir_from_source(CODE)
    loop = psyir.walk(Loop)[0]
    trans = ParaTrans()
    # Check that we reject non-integer collapse arguments
    with pytest.raises(TypeError) as err:
        trans.validate(loop, {"collapse": loop})
    assert ("The 'collapse' argument must be an integer or a bool but got an"
            " object of type" in str(err.value))


def test_paralooptrans_validate_colours(monkeypatch):
    '''
    Test that we raise an error if the user attempts to apply the
    transformation to a loop over colours (since any such
    loop must be sequential). If the user explicitly requests a 'sequential'
    loop transformation (e.g. for "acc loop seq") then that should be
    permitted.

    '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", "gocean",
                           name="invoke_0", dist_mem=False)
    schedule = invoke.schedule
    child = schedule.walk(Loop)[0]
    trans = ParaTrans()
    # Monkeypatch the loop to make it appear that it is over colours.
    monkeypatch.setattr(child, "_loop_type", "colours")
    with pytest.raises(TransformationError) as err:
        trans.validate(child)
    assert ("The target loop is over colours and must be computed serially"
            in str(err.value))
    # However, if we are requesting a sequential loop then all is fine.
    trans.validate(child, options={"sequential": True})


def test_paralooptrans_validate_ignore_written_once(fortran_reader):
    '''
    Test that validate() ignores a warning from the dependency analysis
    about a variable that is written to once.

    '''
    code = '''
subroutine my_sub()
  integer ji, jj
  real :: var1(10,10), sum
  sum = 0.0
  var1 = 1.0
  do ji = 1, 10
    do jj = 1, 10
      var1(ji, jj) = 1.0
      sum = var1(ji, jj)
    end do
  end do
end subroutine my_sub'''
    psyir = fortran_reader.psyir_from_source(code)
    loop = psyir.walk(Loop)[0]
    trans = ParaTrans()
    # Check that the dependency tools will raise the expected warning.
    dep_tools = DependencyTools()
    dep_tools.can_loop_be_parallelised(loop)
    for message in dep_tools.get_all_messages():
        if message.code == DTCode.WARN_SCALAR_WRITTEN_ONCE:
            break
    else:
        assert False, "Dependency tools didn't generate expected message"
    # Check that this warning is ignored by the validate() method.
    trans.validate(loop)


def test_paralooptrans_validate_all_vars(fortran_reader):
    '''
    Test that validate() checks the accesses of *all* variables in a loop. We
    use a case where the first warning the dependence analysis generates is
    one that is ignored (scalar-written-once) so that we must look at other
    messages to see that the loop is not safe.

    '''
    code = '''
subroutine my_sub(ztmp3)
  real, intent(inout) :: ztmp3(:,:)
  real :: zcol1, zcol2, zval1, zval2
  integer :: ipivot, ji_sd, ji1_sd, jj_sd, ninco
  ztmp3 = 0._wp

  DO ji_sd = 1, ninco

     zval1 = ABS(ztmp3(ji_sd,ji_sd))

     ipivot = ji_sd
     DO jj_sd = ji_sd, ninco
        zval2 = ABS(ztmp3(ji_sd,jj_sd))
        IF( zval2 >= zval1 )THEN
           ipivot = jj_sd
           zval1  = zval2
        ENDIF
     END DO

     DO ji1_sd = 1, ninco
        zcol1                = ztmp3(ji1_sd,ji_sd)
        zcol2                = ztmp3(ji1_sd,ipivot)
        ztmp3(ji1_sd,ji_sd)  = zcol2
        ztmp3(ji1_sd,ipivot) = zcol1
     END DO

  END DO
end subroutine my_sub
   '''
    psyir = fortran_reader.psyir_from_source(code)
    loop = psyir.walk(Loop)[1]
    trans = ParaTrans()
    with pytest.raises(TransformationError) as err:
        trans.validate(loop)
    err_text = str(err.value)
    assert ("Variable 'zval1' is read first, which indicates a reduction"
            in err_text)


def test_paralooptrans_apply_calls_validate(fortran_reader, monkeypatch):
    '''
    Check that the apply() method calls the validate() method.

    '''
    psyir = fortran_reader.psyir_from_source(CODE)
    loop = psyir.walk(Loop)[0]
    trans = ParaTrans()

    # Monkeypatch the validate() method so that it raises a unique error.
    def fake(_1, _2, options):
        raise TransformationError("just a test")
    monkeypatch.setattr(ParaTrans, "validate", fake)
    with pytest.raises(TransformationError) as err:
        trans.apply(loop)
    assert "just a test" in str(err.value)


def test_paralooptrans_apply(fortran_reader):
    '''
    Check that the apply() method works as expected, including passing
    `options` down to validate().

    '''
    psyir = fortran_reader.psyir_from_source(CODE)
    loop = psyir.walk(Loop)[0]
    trans = ParaTrans()
    trans.apply(loop, {"force": True})
    assert isinstance(loop.parent.parent, OMPParallelDoDirective)


def test_paralooptrans_with_array_privatisation(fortran_reader,
                                                fortran_writer):
    '''
    Check that the 'privatise_arrays' transformation option allows to ignore
    write-write dependencies by setting the associated variable as 'private'
    '''
    psyir = fortran_reader.psyir_from_source('''
        subroutine my_sub()
          integer ji, jj
          real :: var1(10,10)
          real :: ztmp(10)
          real :: ztmp2(10)
          var1 = 1.0
          ztmp2 = 1.0

          do ji = 1, 10
            do jj = 1, 10
              if (jj == 4) then
                ztmp2(jj) = 4
              end if  ! the rest get the value from before the loop
              ztmp(jj) = var1(ji, jj) + ztmp2(jj)
            end do
            do jj = 1, 10
              var1(ji, jj) = ztmp(jj) * 2
            end do
          end do
        end subroutine my_sub''')

    loop = psyir.walk(Loop)[0]
    trans = ParaTrans()

    # By default this can not be parallelised because 'ztmp' is shared
    with pytest.raises(TransformationError) as err:
        trans.apply(loop)
    assert "ztmp(jj)\' causes a write-write race condition." in str(err.value)
    assert "ztmp2(jj)\' causes a write-write race condition." in str(err.value)

    # Now enable array privatisation
    trans.apply(loop, {"privatise_arrays": True})
    assert ("!$omp parallel do default(shared), private(ji,jj,ztmp), "
            "firstprivate(ztmp2)" in fortran_writer(psyir))

    # If the array is accessed after the loop, or is a not an automatic
    # interface, or is not a plain array, the privatisation will fail
    psyir = fortran_reader.psyir_from_source('''
        subroutine my_sub()
          use other, only: mystruct
          integer ji, jj
          real :: ztmp(10)  ! This one is fine
          real :: ztmp_after(10)
          real, save :: ztmp_nonlocal(10)
          var1 = 1.0
          ztmp = 3.0

          do ji = 1, 10
            do jj = 1, 10
              ztmp(jj) = 3
              ztmp_nonlocal(jj) = 3
              ztmp_after(jj) = 3
              mystruct%array(jj) = 3
            end do
          end do
          call something(ztmp_after)
        end subroutine my_sub''')

    loop = psyir.walk(Loop)[0]
    with pytest.raises(TransformationError) as err:
        trans.apply(loop, {"privatise_arrays": True})
    # with and updated error messages
    assert "ztmp(jj)\' causes a write-write race " not in str(err.value)
    assert "ztmp_after(jj)\' causes a write-write race " not in str(err.value)
    assert ("ztmp_nonlocal(jj)\' causes a write-write race "
            not in str(err.value))
    assert ("The write-write dependency in 'ztmp_after' cannot be solved by "
            "array privatisation because it is not a plain local array or it "
            "is used after the loop" in str(err.value))
    assert ("The write-write dependency in 'ztmp_nonlocal' cannot be solved "
            "by array privatisation because it is not a plain local array or "
            "it is used after the loop" in str(err.value))
    assert ("The write-write dependency in 'mystruct%array' cannot be solved "
            "by array privatisation because it is not a plain local array or "
            "it is used after the loop" in str(err.value))

    # The privatise_arrays only accepts bools
    with pytest.raises(TypeError) as err:
        trans.apply(loop, {"privatise_arrays": 3})
    assert ("The 'privatise_arrays' option must be a bool but got an object "
            "of type int" in str(err.value))
