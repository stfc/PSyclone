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
# Author: A. R. Porter, STFC Daresbury Lab

'''Module containing py.test tests for the transformation of the PSy
   representation of NEMO code to insert profiling calls.

'''

from __future__ import absolute_import, print_function
from fparser.common.readfortran import FortranStringReader
from psyclone.psyGen import PSyFactory, TransInfo


# The PSyclone API under test
API = "nemo"

def test_profile_single_loop(parser):
    ''' Check that the correct code is added to the generated Fortran
    when profiling a single loop nest. '''
    reader = FortranStringReader("program do_loop\n"
                                 "use kind_mod, only: wp\n"
                                 "real :: sto_tmp(jpj), sto_tmp2(jpj)\n"
                                 "do ji = 1,jpj\n"
                                 "  sto_tmp(ji) = 1.0d0\n"
                                 "end do\n"
                                 "end program do_loop\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = TransInfo().get_trans_name('ProfileRegionTrans')
    schedule, _ = acc_trans.apply(schedule.children[0])
    code = str(psy.gen)
    assert (
        "  USE profile_mod, ONLY: ProfileData, ProfileStart, ProfileEnd\n"
        "  USE kind_mod, ONLY: wp\n" in code)
    assert (
        "  REAL :: sto_tmp(jpj), sto_tmp2(jpj)\n"
        "  TYPE(ProfileData), SAVE :: psy_profile0\n" in code)
    assert (
        "  CALL ProfileStart('do_loop', 'region_0', psy_profile0)\n"
        "  DO ji = 1, jpj\n"
        "    sto_tmp(ji) = 1.0D0\n"
        "  END DO\n"
        "  CALL ProfileEnd(psy_profile0)\n" in code)


def test_profile_two_loops(parser):
    ''' Check that the correct code is added to the generated Fortran
    when profiling two, separate loop nests. '''
    reader = FortranStringReader("program do_loop\n"
                                 "use kind_mod, only: wp\n"
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
    acc_trans = TransInfo().get_trans_name('ProfileRegionTrans')
    # Create two separate profiling regions
    schedule, _ = acc_trans.apply(schedule.children[1])
    schedule, _ = acc_trans.apply(schedule.children[0])
    code = str(psy.gen)
    assert (
        "  USE profile_mod, ONLY: ProfileData, ProfileStart, ProfileEnd\n"
        "  USE kind_mod, ONLY: wp\n" in code)
    assert code.count("USE profile_mod") == 1
    assert (
        "  REAL :: sto_tmp(jpj), sto_tmp2(jpj)\n"
        "  TYPE(ProfileData), SAVE :: psy_profile0\n"
        "  TYPE(ProfileData), SAVE :: psy_profile1\n" in code)
    assert (
        "  CALL ProfileStart('do_loop', 'region_0', psy_profile0)\n"
        "  DO ji = 1, jpj\n"
        "    sto_tmp(ji) = 1.0D0\n"
        "  END DO\n"
        "  CALL ProfileEnd(psy_profile0)\n" in code)
    assert (
        "  CALL ProfileStart('do_loop', 'region_1', psy_profile1)\n"
        "  DO ji = 1, jpj\n"
        "    sto_tmp2(ji) = 1.0D0\n"
        "  END DO\n"
        "  CALL ProfileEnd(psy_profile1)\n" in code)


def test_profile_codeblock(parser):
    ''' Check that we can put profiling calls around a region containing
    a CodeBlock. '''
    reader = FortranStringReader("subroutine cb_test()\n"
                                 "use kind_mod, only: wp\n"
                                 "real :: sto_tmp2(jpj)\n"
                                 "do ji = 1,jpj\n"
                                 "  write(*,*) sto_tmp2(ji)\n"
                                 "end do\n"
                                 "end subroutine cb_test\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = TransInfo().get_trans_name('ProfileRegionTrans')
    # Create two separate profiling regions
    schedule, _ = acc_trans.apply(schedule.children[0])
    schedule.view()
    code = str(psy.gen)
    print(code)
    assert (
        "  CALL ProfileStart('cb_test', 'region_0', psy_profile0)\n"
        "  DO ji = 1, jpj\n"
        "    WRITE(*, FMT = *) sto_tmp2(ji)\n"
        "  END DO\n"
        "  CALL ProfileEnd(psy_profile0)\n" in code)

