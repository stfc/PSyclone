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
# -----------------------------------------------------------------------------
# Author A. R. Porter, STFC Daresbury Lab

'''
Module providing pytest tests of the CreateNemoInvokeScheduleTrans
transformation.

'''
from __future__ import absolute_import
import pytest

from fparser.common.readfortran import FortranStringReader
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import Return, Routine, Assignment
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


def test_basic_invokesched_trans(parser):
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
    trans = CreateNemoInvokeScheduleTrans()
    fp2reader = Fparser2Reader()
    reader = FortranStringReader(code)
    prog = parser(reader)
    psyir = fp2reader.generate_psyir(prog)
    routines = psyir.walk(Routine)
    # Apply the transformation to the Routine
    sched = trans.apply(routines[0])
    sched.view()
    assert 0
