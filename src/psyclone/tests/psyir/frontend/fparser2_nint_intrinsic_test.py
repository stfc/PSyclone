# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Laboratory
# Modified: S. Siso, STFC Daresbury Laboratory

''' Module containing pytest tests for the handling of the NINT intrinsic
in the PSyIR. '''


from fparser.common.readfortran import FortranStringReader
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import (Assignment, IntrinsicCall,
                                  BinaryOperation, Routine)

TEST_CODE = '''
 PROGRAM my_test
  INTEGER :: irgb
  REAL :: zchl, zekb(10, 10)

  irgb = NINT(41 + 20. * LOG10(zchl) + 1.E-15)
  irgb = irgb + NINT(zekb(1,1) + 4.5 + zchl)

END PROGRAM my_test
'''


def test_nint(parser):
    ''' Basic test that the NINT intrinsic is recognised and represented
    in the PSyIR.

    '''
    processor = Fparser2Reader()
    reader = FortranStringReader(TEST_CODE)
    ptree = parser(reader)
    psyir = processor.generate_psyir(ptree)
    sched = psyir.walk(Routine)[0]
    assert isinstance(sched[0], Assignment)
    assert isinstance(sched[0].rhs, IntrinsicCall)
    assert sched[0].rhs.intrinsic == IntrinsicCall.Intrinsic.NINT
    assert isinstance(sched[0].rhs.children[0], BinaryOperation)
    assert isinstance(sched[1], Assignment)
    assert isinstance(sched[1].rhs, BinaryOperation)
    assert isinstance(sched[1].rhs.children[1], IntrinsicCall)
    assert sched[1].rhs.children[1].intrinsic == IntrinsicCall.Intrinsic.NINT
