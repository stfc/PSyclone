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

'''
Module providing pytest tests of the CreateNemoLoopTrans transformation.
'''

from __future__ import absolute_import
import pytest

from psyclone.psyir.nodes import Return, Loop, Assignment
from psyclone.transformations import TransformationError
from psyclone.domain.nemo.transformations import CreateNemoLoopTrans
from psyclone.nemo import NemoLoop


def test_construct_create_loop_trans():
    ''' Check that we can construct the Transformation object. '''
    trans = CreateNemoLoopTrans()
    assert isinstance(trans, CreateNemoLoopTrans)
    assert trans.name == "CreateNemoLoopTrans"


def test_create_loop_validate():
    ''' Check that the validate() method works as expected. '''
    trans = CreateNemoLoopTrans()
    with pytest.raises(TransformationError) as err:
        trans.validate(Return())
    assert ("supplied node should be a PSyIR Loop but found 'Return'" in
            str(err.value))


def test_basic_loop_trans(fortran_reader):
    ''' Check that a basic loop can be transformed correctly. '''
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
    trans = CreateNemoLoopTrans()
    loops = psyir.walk(Loop)
    # Apply the transformation to the outermost loop
    trans.apply(loops[0])
    outer_loop = psyir.walk(NemoLoop)[0]
    assert outer_loop.loop_type == "lat"
    # Check that the new loop is in the Schedule
    loops = psyir.walk(Loop)
    assert loops[0] is outer_loop
    # Apply the transformation to the inner loop
    trans.apply(loops[1])
    inner_loop = psyir.walk(NemoLoop)[1]
    assert isinstance(inner_loop, NemoLoop)
    assert inner_loop.loop_type == "lon"
    assert isinstance(inner_loop.loop_body[0], Assignment)
