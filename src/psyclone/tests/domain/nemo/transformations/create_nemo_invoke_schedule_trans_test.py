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
# -----------------------------------------------------------------------------
# Author A. R. Porter, STFC Daresbury Lab
# Modified by R. W. Ford, STFC Daresbury Lab

'''
Module providing pytest tests of the CreateNemoInvokeScheduleTrans
transformation.

'''
from __future__ import absolute_import
import pytest

from psyclone.psyir.nodes import Return, Routine, Loop
from psyclone.psyir.symbols import ScalarType
from psyclone.transformations import TransformationError
from psyclone.domain.nemo.transformations import CreateNemoInvokeScheduleTrans
from psyclone.nemo import NemoInvokeSchedule


def test_construct_create_invokesched_trans():
    ''' Check that we can construct the Transformation object. '''
    trans = CreateNemoInvokeScheduleTrans()
    assert isinstance(trans, CreateNemoInvokeScheduleTrans)
    assert trans.name == "CreateNemoInvokeScheduleTrans"


def test_create_invokesched_validate():
    ''' Check that the validate() method works as expected. '''
    trans = CreateNemoInvokeScheduleTrans()
    with pytest.raises(TransformationError) as err:
        trans.validate(Return())
    assert ("supplied node should be a PSyIR Routine but found 'Return'" in
            str(err.value))


def test_basic_invokesched_trans(fortran_reader):
    ''' Check that a basic routine can be transformed correctly. '''
    code = '''subroutine basic_loop()
  integer, parameter :: jpi=16, jpj=16
  integer :: ji, jj
  real :: a(jpi, jpj), fconst
  do jj = 1, jpj
    do ji = 1, jpi
      a(ji) = fconst
    end do
  end do
end subroutine basic_loop
'''
    psyir = fortran_reader.psyir_from_source(code)
    subroutine = psyir.children[0]
    trans = CreateNemoInvokeScheduleTrans()
    first_loop = subroutine[0]
    routines = psyir.walk(Routine)
    assert routines[0] is psyir.children[0]
    # Apply the transformation to the Routine
    trans.apply(routines[0])
    sched = psyir.children[0]
    assert isinstance(sched, NemoInvokeSchedule)
    assert sched.name == "basic_loop"
    assert sched[0] is first_loop
    # Check that its return symbol is still None
    assert sched.return_symbol is None


def test_multi_invoke_schedules(fortran_reader):
    ''' Test that the transformation works successfully when the target
    routine is one of two in a module. '''
    code = '''module my_mod
contains
subroutine init()
  write (*,*) "init called"
end subroutine init
subroutine basic_loop()
  integer, parameter :: jpi=16, jpj=16
  integer :: ji, jj
  real :: a(jpi, jpj), fconst
  do jj = 1, jpj
    do ji = 1, jpi
      a(ji) = fconst
    end do
  end do
end subroutine basic_loop
end module my_mod
'''
    psyir = fortran_reader.psyir_from_source(code)
    module = psyir.children[0]
    trans = CreateNemoInvokeScheduleTrans()
    routines = psyir.walk(Routine)
    loops = psyir.walk(Loop)
    trans.apply(routines[1])
    assert isinstance(module.children[1], NemoInvokeSchedule)
    # Check body has not changed
    assert module.children[1][0] is loops[0]


def test_invoke_function(fortran_reader):
    ''' Test the transformation when the target routine is a function. '''
    code = '''module my_mod
contains
real function calc(x) result(val)
  real :: x
  val = x
end function calc
end module my_mod
'''
    psyir = fortran_reader.psyir_from_source(code)
    trans = CreateNemoInvokeScheduleTrans()
    routine = psyir.walk(Routine)[0]
    assert routine.return_symbol.name == "val"
    trans.apply(routine)
    sched = psyir.walk(NemoInvokeSchedule)[0]
    # Check that the new NemoInvokeSchedule still has a return symbol
    assert sched.return_symbol.name == "val"
    assert isinstance(sched.return_symbol.datatype, ScalarType)
    assert sched.return_symbol.datatype.intrinsic == ScalarType.Intrinsic.REAL
