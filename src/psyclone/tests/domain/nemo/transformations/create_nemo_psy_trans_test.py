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
# Author: A. R. Porter, STFC Daresbury Lab
# Modified by R. W. Ford, STFC Daresbury Lab

'''Module containing tests for the CreateNemoPSyTrans transformation.'''

import pytest
from psyclone.psyir.nodes import Assignment, CodeBlock
from psyclone.transformations import Transformation, TransformationError
from psyclone.domain.nemo.transformations import CreateNemoPSyTrans
from psyclone.nemo import NemoInvokeSchedule, NemoLoop


@pytest.fixture(scope="session", name="psy_trans")
def psy_trans_fixture():
    ''' pytest fixture that creates an instance of the Transformation that
    this module is testing. '''
    return CreateNemoPSyTrans()


def test_construction(psy_trans):
    ''' Check that we can construct the transformation object. '''
    assert isinstance(psy_trans, Transformation)
    assert psy_trans.name == "CreateNemoPSyTrans"


def test_create_psy_validation(psy_trans):
    ''' Check the validate() method of the transformation. '''
    with pytest.raises(TransformationError) as err:
        psy_trans.validate(1)
    assert ("Error in CreateNemoPSyTrans transformation. The supplied node "
            "should be a PSyIR Node but found 'int'" in str(err.value))


def test_no_matching_psyir(psy_trans, fortran_reader):
    ''' Check that the transformation has no effect if no suitable nodes
    are found in the supplied PSyIR. '''
    code = '''subroutine basic()
  write(*,*) "Hello world"
end subroutine basic
'''
    psyir = fortran_reader.psyir_from_source(code)
    subroutine = psyir.children[0]
    cblock = subroutine[0]
    psy_trans.apply(subroutine[0])
    # Transformation should have had no effect
    assert subroutine[0] is cblock


def test_basic_psy(psy_trans, fortran_reader):
    ''' Check that the transformation correctly generates NEMO PSyIR for
    a simple case. '''
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
    psy_trans.apply(psyir)
    sched = psyir.children[0]
    assert isinstance(sched, NemoInvokeSchedule)
    assert isinstance(sched[0], NemoLoop)
    assert isinstance(sched[0].loop_body[0], NemoLoop)
    assert isinstance(sched[0].loop_body[0].loop_body[0], Assignment)


def test_module_psy(psy_trans, fortran_reader):
    ''' Check that the transformation works as expected when the source code
    contains a module with more than one routine. '''
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
    psy_trans.apply(psyir)
    invokes = psyir.walk(NemoInvokeSchedule)
    assert len(invokes) == 2
    assert invokes[0].name == "init"
    assert isinstance(invokes[0][0], CodeBlock)
    assert invokes[1].name == "basic_loop"
    assert isinstance(invokes[1][0], NemoLoop)
