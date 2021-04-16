# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council.
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

'''Module containing tests for the NemoLoopTrans transformation.'''

from __future__ import absolute_import

import pytest
from fparser.common.readfortran import FortranStringReader
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.transformations import Transformation, TransformationError
from psyclone.domain.nemo.transformations import NemoPSyTrans


def test_construction():
    ''' Check that we can construct the transformation object. '''
    trans = NemoPSyTrans()
    assert isinstance(trans, Transformation)
    assert trans.name == "NemoPSyTrans"


def test_basic_psy(parser):
    ''' Check that the transformation correctly generates NEMO PSyIR for
    a simple case. '''
    code = '''subroutine basic_loop()
  integer, parameter :: jpi=16
  integer :: ji
  real :: a(jpi), fconst
  do ji = 1, jpi
     a(ji) = fconst
  end do
end subroutine basic_loop
'''
    fp2reader = Fparser2Reader()
    reader = FortranStringReader(code)
    prog = parser(reader)
    psyir = fp2reader.generate_psyir(prog)
    psyir.view()
    trans = NemoPSyTrans()
    sched = trans.apply(psyir)
    sched.view()
    assert 0
