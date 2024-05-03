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
        return OMPParallelDoDirective(children=children)


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
    assert "Dependency analysis failed with the following" in str(err.value)
    # Set the 'force' option to True - no exception should be raised.
    trans.validate(loop, {"force": True})


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
    assert "Dependency analysis failed with the following" in str(err.value)
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
    with pytest.raises(TransformationError) as err:
        trans.validate(loop, {"collapse": loop})
    assert ("The 'collapse' argument must be an integer but got an object "
            "of type" in str(err.value))

    # Check that we reject invalid depths
    with pytest.raises(TransformationError) as err:
        trans.validate(loop, {"collapse": 1})
    assert ("It only makes sense to collapse 2 or more loops but got a "
            "value of 1" in str(err.value))

    # Check that we reject attempts to collapse more loops than we have
    with pytest.raises(TransformationError) as err:
        trans.validate(loop, {"collapse": 3})
    assert ("Cannot apply COLLAPSE(3) clause to a loop nest containing "
            "only 2 loops" in str(err.value))


def test_paralooptrans_validate_colours(monkeypatch):
    '''
    Test that we raise an error if the user attempts to apply the
    transformation to a loop over colours (since any such
    loop must be sequential). If the user explicitly requests a 'sequential'
    loop transformation (e.g. for "acc loop seq") then that should be
    permitted.

    '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", "gocean1.0",
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
    # Should have two messages (the first being the one that is ignored by
    # validate).
    assert "'ipivot' is only written once" in err_text
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
