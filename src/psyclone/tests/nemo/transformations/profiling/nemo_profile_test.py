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
import pytest
from fparser.common.readfortran import FortranStringReader
from psyclone.psyGen import PSyFactory, TransInfo
from psyclone.transformations import TransformationError, ProfileRegionTrans

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
    ptrans = ProfileRegionTrans()
    schedule, _ = ptrans.apply(schedule.children[0])
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
    ptrans = ProfileRegionTrans()
    # Create two separate profiling regions
    schedule, _ = ptrans.apply(schedule.children[1])
    schedule, _ = ptrans.apply(schedule.children[0])
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
    ptrans = ProfileRegionTrans()
    schedule, _ = ptrans.apply(schedule.children[0])
    code = str(psy.gen)
    assert (
        "  CALL ProfileStart('cb_test', 'region_0', psy_profile0)\n"
        "  DO ji = 1, jpj\n"
        "    WRITE(*, FMT = *) sto_tmp2(ji)\n"
        "  END DO\n"
        "  CALL ProfileEnd(psy_profile0)\n" in code)


def test_profile_inside_if(parser):
    ''' Check that we can put a profiling region inside an If block. '''
    reader = FortranStringReader("subroutine inside_if_test()\n"
                                 "use kind_mod, only: wp\n"
                                 "real :: sto_tmp2(jpj)\n"
                                 "logical :: do_this = .true.\n"
                                 "if(do_this)then\n"
                                 "  do ji = 1,jpj\n"
                                 "    write(*,*) sto_tmp2(ji)\n"
                                 "  end do\n"
                                 "endif\n"
                                 "end subroutine inside_if_test\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    ptrans = ProfileRegionTrans()
    schedule, _ = ptrans.apply(schedule.children[0].if_body[0])
    gen_code = str(psy.gen)
    assert ("  IF (do_this) THEN\n"
            "    CALL ProfileStart(" in gen_code)
    assert ("    CALL ProfileEnd(psy_profile0)\n"
            "  END IF\n" in gen_code)


def test_profile_single_line_if(parser):
    ''' Test that we can put a single-line if statement inside a
    profiling region. '''
    reader = FortranStringReader("subroutine one_line_if_test()\n"
                                 "use kind_mod, only: wp\n"
                                 "real :: sto_tmp2(jpj)\n"
                                 "logical :: do_this = .true.\n"
                                 "if(do_this) write(*,*) sto_tmp2(ji)\n"
                                 "end subroutine one_line_if_test\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    ptrans = TransInfo().get_trans_name('ProfileRegionTrans')
    # Check that we refuse to attempt to split the body of the If from
    # its parent (as it is a one-line statement). This limitation will
    # be removed once we use the PSyIR Fortran backend in the NEMO API
    # (as opposed to manipulating the fparser2 parse tree).
    # TODO #435
    with pytest.raises(TransformationError) as err:
        schedule, _ = ptrans.apply(schedule[0].if_body)
    assert "child of a single-line if" in str(err.value)
    # But we should be able to put the whole If statement in a profiling
    # region...
    schedule, _ = ptrans.apply(schedule[0])
    gen_code = str(psy.gen)
    assert (
        "  CALL ProfileStart('one_line_if_test', 'region_0', psy_profile0)\n"
        "  IF (do_this) WRITE(*, FMT = *) sto_tmp2(ji)\n"
        "  CALL ProfileEnd(psy_profile0)\n" in gen_code)


def test_profiling_case(parser):
    ''' Check that we can put profiling around a case statement. This is
    harder than might be expected because of the complexity of mapping back
    from the PSyIR (where SELECT CASE blocks are represented as If blocks)
    to the fparser2 parse tree. '''
    code = (
        "subroutine my_test()\n"
        "   integer :: ji, ii, je_2\n"
        "   p_fld_crs(:,:) = 0._wp\n"
        "   SELECT CASE ( cd_op )\n"
        "     CASE ( 'VOL' )\n"
        "         ALLOCATE( zsurfmsk(jpi,jpj) )\n"
        "         zsurfmsk(:,:) =  p_e12(:,:) * p_e3(:,:,1) * p_mask(:,:,1)\n"
        "         IF( nldj_crs == 1 .AND. mje_crs(2) < 2 ) THEN\n"
        "            IF( mje_crs(2) - mjs_crs(2) == 1 ) THEN\n"
        "               je_2 = mje_crs(2)\n"
        "               DO ji = nistr, niend, nn_factx\n"
        "                  zflcrs =  p_fld(ji  ,je_2) * zsurfmsk(ji  ,je_2)\n"
        "                  p_fld_crs(ii,2) = zflcrs\n"
        "               ENDDO\n"
        "            ENDIF\n"
        "         ELSE\n"
        "            je_2 = mjs_crs(2)\n"
        "         ENDIF\n"
        "         DO jj  = njstr, njend, nn_facty\n"
        "            DO ji = nistr, niend, nn_factx\n"
        "               ii  = ( ji - mis_crs(2) ) * rfactx_r + 2\n"
        "            ENDDO\n"
        "         ENDDO\n"
        "         DEALLOCATE( zsurfmsk )\n"
        "     CASE ( 'SUM' )\n"
        "        ALLOCATE( zsurfmsk(jpi,jpj) )\n"
        "        IF( PRESENT( p_e3 ) ) THEN\n"
        "           zsurfmsk(:,:) = p_e12(:,:) * p_e3(:,:,1) * p_mask(:,:,1)\n"
        "        ELSE\n"
        "           zsurfmsk(:,:) = p_e12(:,:) * p_mask(:,:,1)\n"
        "        ENDIF\n"
        "        DEALLOCATE(zsurfmsk)\n"
        "   END SELECT\n"
        "end subroutine my_test\n")
    reader = FortranStringReader(code)
    prog = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(prog)
    sched = psy.invokes.invoke_list[0].schedule
    ptrans = ProfileRegionTrans()
    # Innermost if-body
    ptrans.apply(sched.children[1].if_body[2].if_body[0].if_body.children)
    # Body of second CASE
    ptrans.apply(sched.children[1].else_body.children)
    # Whole routine
    ptrans.apply(sched.children)
    sched.view()
    code = str(psy.gen)
    print(code)
    assert (
        "  TYPE(ProfileData), SAVE :: psy_profile1\n"
        "  TYPE(ProfileData), SAVE :: psy_profile2\n"
        "  TYPE(ProfileData), SAVE :: psy_profile0\n"
        "  CALL ProfileStart('my_test', 'region_0', psy_profile0)\n"
        "  p_fld_crs(:, :) = 0._wp\n" in code)
    assert ("    CALL ProfileEnd(psy_profile2)\n"
            "  END SELECT\n"
            "  CALL ProfileEnd(psy_profile0)\n"
            "END SUBROUTINE my_test\n" in code)
    assert 0
