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
from psyclone.psyGen import PSyFactory
from psyclone.transformations import TransformationError, ProfileRegionTrans
from fparser.common.readfortran import FortranStringReader

# The transformation that most of these tests use
PTRANS = ProfileRegionTrans()


def get_nemo_schedule(parser, code):
    ''' Utility to construct the PSyIR of the supplied Fortran code.

    :param parser: the Fortran parser to use.
    :type parser: :py:class:`fparser.two.Fortran2003.Program`
    :param str code: the Fortran code to process.

    :returns: 2-tuple of the top-level PSy object and the Schedule of \
              the first Invoke (routine) in `code`.
    :rtype: (:py:class:`psyclone.nemo.NemoPSy`, \
             :py:class:`psyclone.nemo.NemoInvokeSchedule`)

    '''
    reader = FortranStringReader(code)
    code = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    return psy, schedule


def test_profile_single_loop(parser):
    ''' Check that the correct code is added to the generated Fortran
    when profiling a single loop nest. '''
    psy, schedule = get_nemo_schedule(parser,
                                      "program do_loop\n"
                                      "use kind_mod, only: wp\n"
                                      "real :: sto_tmp(jpj), sto_tmp2(jpj)\n"
                                      "do ji = 1,jpj\n"
                                      "  sto_tmp(ji) = 1.0d0\n"
                                      "end do\n"
                                      "end program do_loop\n")
    schedule, _ = PTRANS.apply(schedule.children[0])
    code = str(psy.gen)
    assert (
        "  USE profile_mod, ONLY: ProfileData, ProfileStart, ProfileEnd\n"
        "  USE kind_mod, ONLY: wp\n" in code)
    assert (
        "  REAL :: sto_tmp(jpj), sto_tmp2(jpj)\n"
        "  TYPE(ProfileData), SAVE :: psy_profile0\n" in code)
    assert (
        "  CALL ProfileStart('do_loop', 'r0', psy_profile0)\n"
        "  DO ji = 1, jpj\n"
        "    sto_tmp(ji) = 1.0D0\n"
        "  END DO\n"
        "  CALL ProfileEnd(psy_profile0)\n" in code)


def test_profile_two_loops(parser):
    ''' Check that the correct code is added to the generated Fortran
    when profiling two, separate loop nests. '''
    psy, schedule = get_nemo_schedule(parser,
                                      "program do_loop\n"
                                      "use kind_mod, only: wp\n"
                                      "real :: sto_tmp(jpj), sto_tmp2(jpj)\n"
                                      "do ji = 1,jpj\n"
                                      "  sto_tmp(ji) = 1.0d0\n"
                                      "end do\n"
                                      "do ji = 1,jpj\n"
                                      "  sto_tmp2(ji) = 1.0d0\n"
                                      "end do\n"
                                      "end program do_loop\n")
    # Create two separate profiling regions
    schedule, _ = PTRANS.apply(schedule.children[1])
    schedule, _ = PTRANS.apply(schedule.children[0])
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
        "  CALL ProfileStart('do_loop', 'r0', psy_profile0)\n"
        "  DO ji = 1, jpj\n"
        "    sto_tmp(ji) = 1.0D0\n"
        "  END DO\n"
        "  CALL ProfileEnd(psy_profile0)\n" in code)
    assert (
        "  CALL ProfileStart('do_loop', 'r1', psy_profile1)\n"
        "  DO ji = 1, jpj\n"
        "    sto_tmp2(ji) = 1.0D0\n"
        "  END DO\n"
        "  CALL ProfileEnd(psy_profile1)\n" in code)


def test_profile_codeblock(parser):
    ''' Check that we can put profiling calls around a region containing
    a CodeBlock. '''
    psy, schedule = get_nemo_schedule(parser,
                                      "subroutine cb_test()\n"
                                      "use kind_mod, only: wp\n"
                                      "real :: sto_tmp2(jpj)\n"
                                      "do ji = 1,jpj\n"
                                      "  write(*,*) sto_tmp2(ji)\n"
                                      "end do\n"
                                      "end subroutine cb_test\n")
    schedule, _ = PTRANS.apply(schedule.children[0])
    code = str(psy.gen)
    assert (
        "  CALL ProfileStart('cb_test', 'r0', psy_profile0)\n"
        "  DO ji = 1, jpj\n"
        "    WRITE(*, FMT = *) sto_tmp2(ji)\n"
        "  END DO\n"
        "  CALL ProfileEnd(psy_profile0)\n" in code)


def test_profile_inside_if1(parser):
    ''' Check that we can put a profiling region inside an If block when
    we pass the transformation the first (and only) child of the Schedule
    of the If. '''
    psy, schedule = get_nemo_schedule(parser,
                                      "subroutine inside_if_test()\n"
                                      "use kind_mod, only: wp\n"
                                      "real :: sto_tmp2(jpj)\n"
                                      "logical :: do_this = .true.\n"
                                      "if(do_this)then\n"
                                      "  do ji = 1,jpj\n"
                                      "    write(*,*) sto_tmp2(ji)\n"
                                      "  end do\n"
                                      "endif\n"
                                      "end subroutine inside_if_test\n")
    schedule, _ = PTRANS.apply(schedule.children[0].if_body[0])
    gen_code = str(psy.gen)
    assert ("  IF (do_this) THEN\n"
            "    CALL ProfileStart(" in gen_code)
    assert ("    CALL ProfileEnd(psy_profile0)\n"
            "  END IF\n" in gen_code)


def test_profile_inside_if2(parser):
    ''' Check that we can put a profiling region inside an If block
    when we pass the transformation the Schedule rather than its child. '''
    psy, schedule = get_nemo_schedule(parser,
                                      "subroutine inside_if_test()\n"
                                      "use kind_mod, only: wp\n"
                                      "real :: sto_tmp2(jpj)\n"
                                      "logical :: do_this = .true.\n"
                                      "if(do_this)then\n"
                                      "  do ji = 1,jpj\n"
                                      "    write(*,*) sto_tmp2(ji)\n"
                                      "  end do\n"
                                      "endif\n"
                                      "end subroutine inside_if_test\n")
    schedule, _ = PTRANS.apply(schedule.children[0].if_body)
    gen_code = str(psy.gen)
    assert ("  IF (do_this) THEN\n"
            "    CALL ProfileStart(" in gen_code)
    assert ("    CALL ProfileEnd(psy_profile0)\n"
            "  END IF\n" in gen_code)


def test_profile_single_line_if(parser):
    ''' Test that we can put a single-line if statement inside a
    profiling region. '''
    psy, schedule = get_nemo_schedule(parser,
                                      "subroutine one_line_if_test()\n"
                                      "use kind_mod, only: wp\n"
                                      "real :: sto_tmp2(jpj)\n"
                                      "logical :: do_this = .true.\n"
                                      "if(do_this) write(*,*) sto_tmp2(ji)\n"
                                      "end subroutine one_line_if_test\n")
    # Check that we refuse to attempt to split the body of the If from
    # its parent (as it is a one-line statement). This limitation will
    # be removed once we use the PSyIR Fortran backend in the NEMO API
    # (as opposed to manipulating the fparser2 parse tree).
    # TODO #435
    with pytest.raises(TransformationError) as err:
        schedule, _ = PTRANS.apply(schedule[0].if_body)
    assert "single-line if statement" in str(err.value)
    # But we should be able to put the whole If statement in a profiling
    # region...
    schedule, _ = PTRANS.apply(schedule[0])
    gen_code = str(psy.gen)
    assert (
        "  CALL ProfileStart('one_line_if_test', 'r0', psy_profile0)\n"
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
    psy, sched = get_nemo_schedule(parser, code)
    # Innermost if-body
    PTRANS.apply(sched[1].if_body[2].if_body[0].if_body.children)
    # Body of first CASE
    PTRANS.apply(sched[1].if_body[:])
    # Body of second CASE
    PTRANS.apply(sched[1].else_body.children)
    # Whole routine
    PTRANS.apply(sched.children)
    code = str(psy.gen)
    assert (
        "  TYPE(ProfileData), SAVE :: psy_profile2\n"
        "  TYPE(ProfileData), SAVE :: psy_profile1\n"
        "  TYPE(ProfileData), SAVE :: psy_profile3\n"
        "  TYPE(ProfileData), SAVE :: psy_profile0\n"
        "  CALL ProfileStart('my_test', 'r0', psy_profile0)\n"
        "  p_fld_crs(:, :) = 0._wp\n" in code)
    assert ("      IF (mje_crs(2) - mjs_crs(2) == 1) THEN\n"
            "        CALL ProfileStart('my_test', 'r2', psy_profile2)\n"
            in code)
    assert ("        END DO\n"
            "        CALL ProfileEnd(psy_profile2)\n"
            "      END IF\n" in code)
    assert ("  CASE ('VOL')\n"
            "    CALL ProfileStart('my_test', 'r1', psy_profile1)\n" in code)
    assert ("    CALL ProfileEnd(psy_profile1)\n"
            "  CASE ('SUM')\n" in code)
    assert ("  CASE ('SUM')\n"
            "    CALL ProfileStart('my_test', 'r3', psy_profile3)\n" in code)
    assert ("    CALL ProfileEnd(psy_profile3)\n"
            "  END SELECT\n"
            "  CALL ProfileEnd(psy_profile0)\n"
            "END SUBROUTINE my_test" in code)


def test_profiling_case_loop(parser):
    ''' Check that we can put profiling around a CASE and a subsequent
    loop. '''
    code = ("subroutine my_test()\n"
            "  integer :: igrd\n"
            "  select case(igrd)\n"
            "     case(1)\n"
            "        pmask => tmask(:,:,:)\n"
            "        bdypmask => bdytmask(:,:)\n"
            "     case default ;   CALL ctl_stop('unrecognised value')\n"
            "  end select\n"
            "  do ib = 1, idx%nblenrim(igrd)\n"
            "     ii = idx%nbi(ib,igrd)\n"
            "  end do\n"
            "end subroutine\n")
    psy, sched = get_nemo_schedule(parser, code)
    PTRANS.apply(sched.children)
    code = str(psy.gen)
    assert ("  CALL ProfileStart('my_test', 'r0', psy_profile0)\n"
            "  SELECT CASE (igrd)\n" in code)
    assert ("CALL ProfileEnd(psy_profile0)\n"
            "END SUBROUTINE" in code)


def test_profiling_no_spec_part(parser, monkeypatch):
    ''' Check that attempting to add profiling to a routine that has no
    Specification_Part (i.e. no declarations) raises the expected errors
    in the NEMO API (this restriction will be lifted by #435). '''
    psy, sched = get_nemo_schedule(parser,
                                   "subroutine no_decs()\n"
                                   "  write(*,*) 'This is just a test'\n"
                                   "end subroutine no_decs\n")
    with pytest.raises(TransformationError) as err:
        PTRANS.apply(sched.children)
    assert ("only be added to routines which contain existing variable "
            "declarations" in str(err.value))
    assert "'no_decs' does not have any" in str(err.value)

    # Monkeypatch the validate method so that we can check that we raise the
    # expected error at code-generation time too.
    monkeypatch.setattr(PTRANS, "_validate", lambda nodes: None)
    PTRANS.apply(sched.children)

    with pytest.raises(NotImplementedError) as err:
        _ = psy.gen
    assert ("Addition of profiling regions to routines without any "
            "existing declarations is not supported" in str(err.value))


def test_profiling_missing_end(parser):
    ''' Check that we raise the expected error if we are unable to find
    the end of the profiled code section in the parse tree. '''
    from psyclone.psyGen import Loop, InternalError
    psy, schedule = get_nemo_schedule(parser,
                                      "program do_loop\n"
                                      "real :: sto_tmp(jpj)\n"
                                      "do ji = 1,jpj\n"
                                      "  sto_tmp(ji) = 1.0d0\n"
                                      "end do\n"
                                      "end program do_loop\n")
    schedule, _ = PTRANS.apply(schedule.children[0])
    # Manually break the _ast_end property by making it point to the root
    # of the whole parse tree
    loops = schedule.walk(Loop)
    loops[0]._ast_end = psy._ast
    with pytest.raises(InternalError) as err:
        _ = psy.gen
    assert "Failed to find the location of 'PROGRAM do_loop" in str(err.value)


def test_profiling_mod_use_clash(parser):
    ''' Check that we abort cleanly if we encounter a 'use' of a module that
    clashes with the one we would 'use' for the profiling API. '''
    psy, schedule = get_nemo_schedule(parser,
                                      "program the_clash\n"
                                      "  use profile_mod, only: some_var\n"
                                      "  my_array(:,:) = 0.0\n"
                                      "end program the_clash\n")
    PTRANS.apply(schedule.children[0])
    schedule.view()
    with pytest.raises(NotImplementedError) as err:
        _ = psy.gen
    assert ("Cannot add profiling to 'the_clash' because it already 'uses' "
            "a module named 'profile_mod'" in str(err.value))


def test_profiling_mod_name_clash(parser):
    ''' Check that we abort cleanly if we encounter code that has a name
    clash with the name of the profiling API module. '''
    psy, schedule = get_nemo_schedule(parser,
                                      "program profile_mod\n"
                                      "  real :: my_array(3,3)\n"
                                      "  my_array(:,:) = 0.0\n"
                                      "end program profile_mod\n")
    PTRANS.apply(schedule.children[0])
    with pytest.raises(NotImplementedError) as err:
        _ = psy.gen
    assert ("Cannot add profiling to 'profile_mod' because it already "
            "contains a symbol that clashes with the name of the PSyclone "
            "profiling " in str(err.value))


def test_profiling_symbol_clash(parser):
    ''' Check that we abort cleanly if we encounter code that has a name
    clash with any of the symbols we 'use' from profile_mode. '''
    from psyclone.profiler import ProfileNode
    for var_name in ProfileNode.profiling_symbols:
        psy, schedule = get_nemo_schedule(
            parser,
            "program my_test\n"
            "  real :: my_array(3,3)\n"
            "  integer :: {0}\n"
            "  my_array(:,:) = 0.0\n"
            "end program my_test\n".format(var_name))
        PTRANS.apply(schedule.children[0])
        with pytest.raises(NotImplementedError) as err:
            _ = psy.gen
        assert ("Cannot add profiling to 'my_test' because it already "
                "contains a symbol that clashes with one of those ('{0}')"
                " that must be".format(var_name) in str(err.value))


def test_profiling_var_clash(parser):
    ''' Check that we abort cleanly if we encounter code that has a potential
    name clash with the variables we will introduce for each profiling
    region. '''
    from psyclone.profiler import ProfileNode
    psy, schedule = get_nemo_schedule(
        parser,
        "program my_test\n"
        "  real :: my_array(3,3)\n"
        "  integer :: {0}\n"
        "  my_array(:,:) = 0.0\n"
        "end program my_test\n".format(ProfileNode.profiling_var))
    PTRANS.apply(schedule.children[0])
    with pytest.raises(NotImplementedError) as err:
        _ = psy.gen
    assert ("Cannot add profiling to 'my_test' because it already contains "
            "symbols that potentially clash with the variables we will "
            in str(err.value))
