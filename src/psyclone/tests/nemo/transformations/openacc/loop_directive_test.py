# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''Module containing py.test tests for the transformation of the PSy
   representation of NEMO code using the OpenACC loop directive.

'''

from __future__ import print_function, absolute_import
import pytest
from fparser.common.readfortran import FortranStringReader
from psyclone.psyGen import PSyFactory, TransInfo
from psyclone.transformations import TransformationError


# The PSyclone API under test
API = "nemo"


def test_explicit_loop(parser):
    ''' Check that we can apply the transformation to an explicit loop. '''
    reader = FortranStringReader("program do_loop\n"
                                 "real :: sto_tmp(jpj), sto_tmp2(jpj)\n"
                                 "do ji = 1,jpj\n"
                                 "  sto_tmp(ji) = 1.0d0\n"
                                 "end do\n"
                                 "do ji = 1,jpj\n"
                                 "  sto_tmp2(ji) = 1.0d0\n"
                                 "end do\n"
                                 "end program do_loop\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = TransInfo().get_trans_name('ACCLoopTrans')
    schedule, _ = acc_trans.apply(schedule.children[0])
    schedule, _ = acc_trans.apply(schedule.children[1], independent=False)
    code = str(psy.gen)
    assert ("PROGRAM do_loop\n"
            "  REAL :: sto_tmp(jpj), sto_tmp2(jpj)\n"
            "  !$ACC LOOP INDEPENDENT\n"
            "  DO ji = 1, jpj\n"
            "    sto_tmp(ji) = 1.0D0\n"
            "  END DO\n"
            "  !$ACC LOOP\n"
            "  DO ji = 1, jpj\n"
            "    sto_tmp2(ji) = 1.0D0\n"
            "  END DO\n"
            "END PROGRAM do_loop" in code)


SINGLE_LOOP = ("program do_loop\n"
               "real(kind=wp) :: sto_tmp(jpj)\n"
               "do ji = 1,jpj\n"
               "  sto_tmp(ji) = 1.0d0\n"
               "end do\n"
               "end program do_loop\n")

DOUBLE_LOOP = ("program do_loop\n"
               "real(kind=wp) :: sto_tmp(jpi, jpj)\n"
               "do jj = 1,jpj\n"
               "  do ji = 1,jpi\n"
               "    sto_tmp(ji, jj) = 1.0d0\n"
               "  end do\n"
               "end do\n"
               "end program do_loop\n")


def test_seq_loop(parser):
    ''' Check that we can apply the transformation with the 'sequential'
    clause. '''
    reader = FortranStringReader(SINGLE_LOOP)
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = TransInfo().get_trans_name('ACCLoopTrans')
    schedule, _ = acc_trans.apply(schedule.children[0], sequential=True)
    code = str(psy.gen)
    assert ("  REAL(KIND = wp) :: sto_tmp(jpj)\n"
            "  !$ACC LOOP SEQ\n"
            "  DO ji = 1, jpj\n" in code)


def test_collapse(parser):
    ''' Check that we can apply the loop transformation with the 'collapse'
    clause. '''
    reader = FortranStringReader(DOUBLE_LOOP)
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = TransInfo().get_trans_name('ACCLoopTrans')
    schedule, _ = acc_trans.apply(schedule.children[0], collapse=2)
    code = str(psy.gen)
    assert ("  REAL(KIND = wp) :: sto_tmp(jpi, jpj)\n"
            "  !$ACC LOOP INDEPENDENT COLLAPSE(2)\n"
            "  DO jj = 1, jpj\n"
            "    DO ji = 1, jpi\n" in code)


def test_collapse_err(parser):
    ''' Check that attempting to apply the loop transformation with a
    'collapse' depth creater than the number of nested loops raises an
    error. '''
    reader = FortranStringReader(DOUBLE_LOOP)
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = TransInfo().get_trans_name('ACCLoopTrans')
    with pytest.raises(TransformationError) as err:
        _, _ = acc_trans.apply(schedule.children[0], collapse=3)
    assert ("Cannot apply COLLAPSE(3) clause to a loop nest containing "
            "only 2 loops" in str(err))
