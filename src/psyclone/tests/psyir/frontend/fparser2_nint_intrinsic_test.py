# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

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
    assert isinstance(sched[0].rhs.arguments[0], BinaryOperation)
    assert isinstance(sched[1], Assignment)
    assert isinstance(sched[1].rhs, BinaryOperation)
    assert isinstance(sched[1].rhs.children[1], IntrinsicCall)
    assert sched[1].rhs.children[1].intrinsic == IntrinsicCall.Intrinsic.NINT
