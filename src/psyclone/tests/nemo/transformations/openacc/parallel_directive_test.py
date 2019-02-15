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
   representation of NEMO code using the OpenACC parallel directive.

'''

from __future__ import print_function, absolute_import
from fparser.common.readfortran import FortranStringReader
from psyclone.psyGen import PSyFactory, TransInfo


# The PSyclone API under test
API = "nemo"


SINGLE_LOOP = ("program do_loop\n"
               "real(kind=wp) :: sto_tmp(jpj)\n"
               "do ji = 1,jpj\n"
               "  sto_tmp(ji) = 1.0d0\n"
               "end do\n"
               "end program do_loop\n")


def test_parallel_single_loop(parser):
    ''' Check that we can apply the transformation to a single, explicit
    loop. '''
    reader = FortranStringReader(SINGLE_LOOP)
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = TransInfo().get_trans_name('ACCParallelTrans')
    schedule, _ = acc_trans.apply(schedule.children[0:1])
    code = str(psy.gen)
    assert ("PROGRAM do_loop\n"
            "  REAL(KIND = wp) :: sto_tmp(jpj)\n"
            "  !$ACC PARALLEL\n"
            "  DO ji = 1, jpj\n"
            "    sto_tmp(ji) = 1.0D0\n"
            "  END DO\n"
            "  !$ACC END PARALLEL\n"
            "END PROGRAM do_loop" in code)


def test_parallel_two_loops(parser):
    ''' Check that we can enclose two loops within a parallel region. '''
    reader = FortranStringReader("program do_loop\n"
                                 "real :: sto_tmp(jpi), sto_tmp2(jpi)\n"
                                 "do ji = 1,jpi\n"
                                 "  sto_tmp(ji) = 1.0d0\n"
                                 "end do\n"
                                 "do ji = 1,jpi\n"
                                 "  sto_tmp2(ji) = 1.0d0\n"
                                 "end do\n"
                                 "end program do_loop\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = TransInfo().get_trans_name('ACCParallelTrans')
    schedule, _ = acc_trans.apply(schedule.children[0:2])
    code = str(psy.gen)
    assert ("PROGRAM do_loop\n"
            "  REAL :: sto_tmp(jpi), sto_tmp2(jpi)\n"
            "  !$ACC PARALLEL\n"
            "  DO ji = 1, jpi\n"
            "    sto_tmp(ji) = 1.0D0\n"
            "  END DO\n"
            "  DO ji = 1, jpi\n"
            "    sto_tmp2(ji) = 1.0D0\n"
            "  END DO\n"
            "  !$ACC END PARALLEL\n"
            "END PROGRAM do_loop" in code)


def test_parallel_if_block(parser):
    ''' Check that we can enclose an IF-block within a parallel region. '''
    reader = FortranStringReader("program do_loop\n"
                                 "real :: sto_tmp(jpi), sto_tmp2(jpi)\n"
                                 "if(init)then\n"
                                 "  do ji = 1,jpi\n"
                                 "    sto_tmp(ji) = 1.0d0\n"
                                 "  end do\n"
                                 "else\n"
                                 "  do ji = 1,jpi\n"
                                 "    sto_tmp2(ji) = 1.0d0\n"
                                 "  end do\n"
                                 "end if\n"
                                 "end program do_loop\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = TransInfo().get_trans_name('ACCParallelTrans')
    schedule, _ = acc_trans.apply(schedule.children[0:1])
    schedule.view()
    code = str(psy.gen)
    print(code)
    assert ("  !$ACC PARALLEL\n"
            "  IF (init) THEN\n"
            "    DO ji = 1, jpi\n" in code)
    assert ("    END DO\n"
            "  END IF\n"
            "  !$ACC END PARALLEL\n" in code)
