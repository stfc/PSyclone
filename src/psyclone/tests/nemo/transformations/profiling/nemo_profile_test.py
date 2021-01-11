# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council.
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
# Modified by: R. W. Ford, STFC Daresbury Lab

'''Module containing py.test tests for the transformation of the PSy
   representation of NEMO code to insert profiling calls.

'''

from __future__ import absolute_import, print_function
import re
import pytest
from fparser.common.readfortran import FortranStringReader
from psyclone.errors import InternalError
from psyclone.configuration import Config
from psyclone.psyGen import PSyFactory
from psyclone.psyir.nodes import PSyDataNode, Loop, ProfileNode
from psyclone.psyir.transformations import ProfileTrans, TransformationError
from psyclone.transformations import OMPParallelLoopTrans, ACCKernelsTrans
from psyclone.profiler import Profiler


# The transformation that most of these tests use
PTRANS = ProfileTrans()


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use the nemo API, and that we clean
    up the config file at the end of the tests.'''

    Config.get().api = "nemo"
    yield()
    # At the end of all tests make sure that we wipe the Config object
    # so we get a fresh/default one for any further test (and not a
    # left-over one from a test here).
    Config._instance = None


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
                                      "integer :: ji\n"
                                      "integer, parameter :: jpj=2\n"
                                      "real :: sto_tmp(jpj), sto_tmp2(jpj)\n"
                                      "do ji = 1,jpj\n"
                                      "  sto_tmp(ji) = 1.0d0\n"
                                      "end do\n"
                                      "end program do_loop\n")
    schedule, _ = PTRANS.apply(schedule.children[0])
    code = str(psy.gen)
    assert (
        "  USE profile_psy_data_mod, ONLY: profile_PSyDataType\n"
        "  USE kind_mod, ONLY: wp\n" in code)
    assert (
        "  REAL :: sto_tmp(jpj), sto_tmp2(jpj)\n"
        "  TYPE(profile_PSyDataType), TARGET, SAVE :: profile_psy_data0\n"
        in code)
    assert (
        "  CALL profile_psy_data0 % PreStart('do_loop', 'r0', 0, 0)\n"
        "  DO ji = 1, jpj\n"
        "    sto_tmp(ji) = 1.0D0\n"
        "  END DO\n"
        "  CALL profile_psy_data0 % PostEnd\n" in code)


def test_profile_single_loop_named(parser):
    '''Check that the correct code is added to the generated Fortran when
    profiling a single loop nest with the profile being named by the
    user.

    '''
    psy, schedule = get_nemo_schedule(parser,
                                      "program do_loop\n"
                                      "integer :: ji\n"
                                      "integer, parameter :: jpj=34\n"
                                      "real :: sto_tmp(jpj), sto_tmp2(jpj)\n"
                                      "do ji = 1,jpj\n"
                                      "  sto_tmp(ji) = 1.0d0\n"
                                      "end do\n"
                                      "end program do_loop\n")
    options = {"region_name": ("my_routine", "my_region")}
    schedule, _ = PTRANS.apply(schedule.children[0], options=options)
    code = str(psy.gen)
    assert ("CALL profile_psy_data0 % PreStart('my_routine', 'my_region', "
            "0, 0)" in code)


def test_profile_two_loops(parser):
    ''' Check that the correct code is added to the generated Fortran
    when profiling two, separate loop nests. '''
    psy, schedule = get_nemo_schedule(parser,
                                      "program do_loop\n"
                                      "use kind_mod, only: wp\n"
                                      "integer :: ji\n"
                                      "integer, parameter :: jpj=8\n"
                                      "real :: sto_tmp(jpj), sto_tmp2(jpj)\n"
                                      "do ji = 1,jpj\n"
                                      "  sto_tmp(ji) = 1.0d0\n"
                                      "end do\n"
                                      "do ji = 1,jpj\n"
                                      "  sto_tmp2(ji) = 1.0d0\n"
                                      "end do\n"
                                      "end program do_loop\n")
    # Create two separate profiling regions
    PTRANS.apply(schedule[1])
    PTRANS.apply(schedule[0])
    code = str(psy.gen)
    assert (
        "  USE profile_psy_data_mod, ONLY: profile_PSyDataType\n"
        "  USE kind_mod, ONLY: wp\n" in code)
    assert code.count("USE profile_psy_data_mod") == 1
    assert (
        "  REAL :: sto_tmp(jpj), sto_tmp2(jpj)\n"
        "  TYPE(profile_PSyDataType), TARGET, SAVE :: profile_psy_data0\n"
        "  TYPE(profile_PSyDataType), TARGET, SAVE :: profile_psy_data1\n"
        in code)
    assert (
        "  CALL profile_psy_data0 % PreStart('do_loop', 'r0', 0, 0)\n"
        "  DO ji = 1, jpj\n"
        "    sto_tmp(ji) = 1.0D0\n"
        "  END DO\n"
        "  CALL profile_psy_data0 % PostEnd\n" in code)
    assert (
        "  CALL profile_psy_data1 % PreStart('do_loop', 'r1', 0, 0)\n"
        "  DO ji = 1, jpj\n"
        "    sto_tmp2(ji) = 1.0D0\n"
        "  END DO\n"
        "  CALL profile_psy_data1 % PostEnd\n" in code)


def test_profile_codeblock(parser):
    ''' Check that we can put profiling calls around a region containing
    a CodeBlock. '''
    psy, schedule = get_nemo_schedule(parser,
                                      "subroutine cb_test()\n"
                                      "use kind_mod, only: wp\n"
                                      "integer :: ji\n"
                                      "integer, parameter :: jpj=128\n"
                                      "real :: sto_tmp2(jpj)\n"
                                      "do ji = 1,jpj\n"
                                      "  write(*,*) sto_tmp2(ji)\n"
                                      "end do\n"
                                      "end subroutine cb_test\n")
    schedule, _ = PTRANS.apply(schedule.children[0])
    code = str(psy.gen)
    assert (
        "  CALL profile_psy_data0 % PreStart('cb_test', 'r0', 0, 0)\n"
        "  DO ji = 1, jpj\n"
        "    WRITE(*, FMT = *) sto_tmp2(ji)\n"
        "  END DO\n"
        "  CALL profile_psy_data0 % PostEnd\n" in code)


def test_profile_inside_if1(parser):
    ''' Check that we can put a profiling region inside an If block when
    we pass the transformation the first (and only) child of the Schedule
    of the If. '''
    psy, schedule = get_nemo_schedule(
        parser,
        "subroutine inside_if_test()\n"
        "use kind_mod, only: wp\n"
        "integer :: ji\n"
        "integer, parameter :: jpj=3\n"
        "real :: sto_tmp2(jpj)\n"
        "logical, parameter :: do_this = .true.\n"
        "if(do_this)then\n"
        "  do ji = 1,jpj\n"
        "    write(*,*) sto_tmp2(ji)\n"
        "  end do\n"
        "endif\n"
        "end subroutine inside_if_test\n")
    schedule, _ = PTRANS.apply(schedule.children[0].if_body[0])
    gen_code = str(psy.gen)
    assert ("  IF (do_this) THEN\n"
            "    CALL profile_psy_data0 % PreStart(" in gen_code)
    assert ("    CALL profile_psy_data0 % PostEnd\n"
            "  END IF\n" in gen_code)


def test_profile_inside_if2(parser):
    ''' Check that we can put a profiling region inside an If block
    when we pass the transformation the Schedule rather than its child. '''
    psy, schedule = get_nemo_schedule(
        parser,
        "subroutine inside_if_test()\n"
        "use kind_mod, only: wp\n"
        "integer :: ji\n"
        "integer, parameter :: jp=256\n"
        "real :: sto_tmp2(jpj)\n"
        "logical, parameter :: do_this = .true.\n"
        "if(do_this)then\n"
        "  do ji = 1,jpj\n"
        "    write(*,*) sto_tmp2(ji)\n"
        "  end do\n"
        "endif\n"
        "end subroutine inside_if_test\n")
    schedule, _ = PTRANS.apply(schedule.children[0].if_body)
    gen_code = str(psy.gen)
    assert ("  IF (do_this) THEN\n"
            "    CALL profile_psy_data0 % PreStart(" in gen_code)
    assert ("    CALL profile_psy_data0 % PostEnd\n"
            "  END IF\n" in gen_code)


def test_profile_single_line_if(parser):
    ''' Test that we can put a single-line if statement inside a
    profiling region. '''
    psy, schedule = get_nemo_schedule(
        parser,
        "subroutine one_line_if_test()\n"
        "use kind_mod, only: wp\n"
        "integer :: ji\n"
        "integer, parameter :: jpj=32\n"
        "real :: sto_tmp2(jpj)\n"
        "logical, parameter :: do_this = .true.\n"
        "if(do_this) write(*,*) sto_tmp2(ji)\n"
        "end subroutine one_line_if_test\n")
    # Check that we refuse to attempt to split the body of the If from
    # its parent (as it is a one-line statement). This limitation will
    # be removed once we use the PSyIR Fortran backend in the NEMO API
    # (as opposed to manipulating the fparser2 parse tree).
    # TODO #435
    with pytest.raises(TransformationError) as err:
        PTRANS.apply(schedule[0].if_body)
    assert "single-line if statement" in str(err.value)
    # But we should be able to put the whole If statement in a profiling
    # region...
    PTRANS.apply(schedule[0])
    gen_code = str(psy.gen)
    assert (
        "  CALL profile_psy_data0 % PreStart('one_line_if_test', 'r0', 0, 0)\n"
        "  IF (do_this) WRITE(*, FMT = *) sto_tmp2(ji)\n"
        "  CALL profile_psy_data0 % PostEnd\n" in gen_code)


def test_profiling_case(parser):
    ''' Check that we can put profiling around a case statement. This is
    harder than might be expected because of the complexity of mapping back
    from the PSyIR (where SELECT CASE blocks are represented as If blocks)
    to the fparser2 parse tree. '''
    code = (
        "subroutine my_test()\n"
        "   integer :: ji, jj, ii, je_2, jpi, jpj, nldj_crs\n"
        "   integer :: nistr, niend, njstr, njend, nn_factx, nn_facty\n"
        "   integer, dimension(:) :: mje_crs, mjs_crs, mis_crs\n"
        "   real, dimension(:,:) :: p_fld_crs, p_e12, p_fld, zsurfmsk\n"
        "   real, dimension(:,:,:) :: p_mask, p_e3\n"
        "   real :: zflcrs, rfactx_r\n"
        "   character(len=3) :: cd_op\n"
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
        "  TYPE(profile_PSyDataType), TARGET, SAVE :: profile_psy_data2\n"
        "  TYPE(profile_PSyDataType), TARGET, SAVE :: profile_psy_data1\n"
        "  TYPE(profile_PSyDataType), TARGET, SAVE :: profile_psy_data3\n"
        "  TYPE(profile_PSyDataType), TARGET, SAVE :: profile_psy_data0\n"
        "  CALL profile_psy_data0 % PreStart('my_test', 'r0', 0, 0)\n"
        "  p_fld_crs(:, :) = 0._wp\n" in code)
    assert ("      IF (mje_crs(2) - mjs_crs(2) == 1) THEN\n"
            "        CALL profile_psy_data2 % PreStart('my_test', 'r2', 0, "
            "0)\n"
            in code)
    assert ("        END DO\n"
            "        CALL profile_psy_data2 % PostEnd\n"
            "      END IF\n" in code)
    assert ("  CASE ('VOL')\n"
            "    CALL profile_psy_data1 % PreStart('my_test', 'r1', 0, 0)\n"
            in code)
    assert ("    CALL profile_psy_data1 % PostEnd\n"
            "  CASE ('SUM')\n" in code)
    assert ("  CASE ('SUM')\n"
            "    CALL profile_psy_data3 % PreStart('my_test', 'r3', 0, 0)\n"
            in code)
    assert ("    CALL profile_psy_data3 % PostEnd\n"
            "  END SELECT\n"
            "  CALL profile_psy_data0 % PostEnd\n"
            "END SUBROUTINE my_test" in code)


def test_profiling_case_loop(parser):
    ''' Check that we can put profiling around a CASE and a subsequent
    loop. '''
    code = ("subroutine my_test()\n"
            "  integer :: igrd, ib, ii\n"
            "  real, dimension(:,:,:) :: pmask, tmask\n"
            "  real, dimension(:,:) :: bdypmask, bdytmask\n"
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
    assert ("  CALL profile_psy_data0 % PreStart('my_test', 'r0', 0, 0)\n"
            "  SELECT CASE (igrd)\n" in code)
    assert ("CALL profile_psy_data0 % PostEnd\n"
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
    monkeypatch.setattr(PTRANS, "validate", lambda nodes, options: None)
    PTRANS.apply(sched.children)

    with pytest.raises(NotImplementedError) as err:
        _ = psy.gen
    assert ("Addition of PSyData regions to routines without any "
            "existing declarations is not supported" in str(err.value))


def test_profiling_missing_end(parser):
    ''' Check that we raise the expected error if we are unable to find
    the end of the profiled code section in the parse tree. '''
    psy, schedule = get_nemo_schedule(parser,
                                      "program do_loop\n"
                                      "integer :: ji\n"
                                      "integer, parameter :: jpj=32\n"
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
    assert ("nodes of the PSyData region in the fparser2 parse tree do not "
            "have the same parent" in str(err.value))


def test_profiling_mod_use_clash(parser):
    ''' Check that we abort cleanly if we encounter a 'use' of a module that
    clashes with the one we would 'use' for the profiling API. '''
    psy, schedule = get_nemo_schedule(parser,
                                      "program the_clash\n"
                                      "  use profile_psy_data_mod, only: "
                                      "some_var\n"
                                      "  real :: my_array(20,10)\n"
                                      "  my_array(:,:) = 0.0\n"
                                      "end program the_clash\n")
    PTRANS.apply(schedule.children[0])
    schedule.view()
    with pytest.raises(NotImplementedError) as err:
        _ = psy.gen
    assert ("Cannot add PSyData calls to 'the_clash' because it already "
            "'uses' a module named 'profile_psy_data_mod'" in str(err.value))


def test_profiling_mod_name_clash(parser):
    ''' Check that we abort cleanly if we encounter code that has a name
    clash with the name of the profiling API module. '''
    psy, schedule = get_nemo_schedule(parser,
                                      "program psy_data_mod\n"
                                      "  real :: my_array(3,3)\n"
                                      "  my_array(:,:) = 0.0\n"
                                      "end program psy_data_mod\n")
    PTRANS.apply(schedule.children[0])
    with pytest.raises(NotImplementedError) as err:
        _ = psy.gen
    assert ("Cannot add PSyData calls to 'psy_data_mod' because it already "
            "contains a symbol that clashes with the name of the PSyclone "
            "PSyData module" in str(err.value))


def test_profiling_symbol_clash(parser):
    ''' Check that we abort cleanly if we encounter code that has a name
    clash with any of the symbols we 'use' from profile_mode. '''
    for var_name in PSyDataNode.symbols:
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
        assert ("Cannot add PSyData calls to 'my_test' because it already "
                "contains a symbol that clashes with one of those ('{0}')"
                " that must be".format(var_name) in str(err.value))


def test_profiling_var_clash(parser):
    ''' Check that we abort cleanly if we encounter code that has a potential
    name clash with the variables we will introduce for each profiling
    region. '''
    psy, schedule = get_nemo_schedule(
        parser,
        "program my_test\n"
        "  real :: my_array(3,3)\n"
        "  integer :: profile_psy_data\n"
        "  my_array(:,:) = 0.0\n"
        "end program my_test\n")
    PTRANS.apply(schedule.children[0])
    with pytest.raises(NotImplementedError) as err:
        _ = psy.gen
    assert ("Cannot add PSyData calls to 'my_test' because it already "
            "contains symbols that potentially clash with the variables "
            "we will " in str(err.value))


def test_only_profile():
    '''Test that the update function in PSyDataNode aborts if the node is not
    a ProfileNode.
    '''

    class DummyNode(PSyDataNode):
        '''Dummy class for testing.'''

    node = DummyNode()
    with pytest.raises(InternalError) as err:
        node.update()
    # Python 2 and 3 have slightly different messages to describe the type
    correct_re = "PSyData.update is only supported for a ProfileNode, not " \
        "for a node of type .*DummyNode'>."
    assert re.search(correct_re, str(err.value))


def test_no_return_in_profiling(parser):
    ''' Check that the transformation refuses to include a Return node within
    a profiled region. '''
    _, schedule = get_nemo_schedule(
        parser,
        "function my_test()\n"
        "  integer :: my_test\n"
        "  real :: my_array(3,3)\n"
        "  my_array(:,:) = 0.0\n"
        "  my_test = 1\n"
        "  return\n"
        "end function my_test\n")
    with pytest.raises(TransformationError) as err:
        PTRANS.apply(schedule.children)
    assert ("Nodes of type 'Return' cannot be enclosed by a ProfileTrans "
            "transformation" in str(err.value))


def test_profile_nemo_auto_kernels(parser):
    '''Check that valid kernels are instrumented in the NEMO API
    and that loops that do not contain kernels are not. '''
    Profiler.set_options([Profiler.KERNELS])
    psy, schedule = get_nemo_schedule(parser,
                                      "program do_loop\n"
                                      "integer :: ji\n"
                                      "integer, parameter :: jpj=32\n"
                                      "real :: sto_tmp(jpj)\n"
                                      "do ji = 1,jpj\n"
                                      "  sto_tmp(ji) = 1.0d0\n"
                                      "end do\n"
                                      "do ji = 1,jpj\n"
                                      "  write(*,*) sto_tmp(ji)\n"
                                      "end do\n"
                                      "end program do_loop\n")
    Profiler.add_profile_nodes(schedule, Loop)
    pnodes = schedule.walk(ProfileNode)
    # The second loop will contain a CodeBlock and therefore is not a kernel
    assert len(pnodes) == 1
    code = str(psy.gen).lower()
    # Check that it's the first loop that's had profiling added
    assert ("  type(profile_psydatatype), target, save :: profile_psy_data0\n"
            "  call profile_psy_data0 % prestart('do_loop', 'r0', 0, 0)\n"
            "  do ji = 1, jpj" in code)


def test_profile_nemo_loop_nests(parser):
    ''' Check that the automatic kernel-level profiling handles a
    tightly-nested loop containing a valid kernel. '''
    Profiler.set_options([Profiler.KERNELS])
    psy, schedule = get_nemo_schedule(parser,
                                      "program do_loop\n"
                                      "integer :: ji, jj\n"
                                      "integer, parameter :: jpi=4, jpj=8\n"
                                      "real :: sto_tmp(jpi,jpj)\n"
                                      "do jj = 1, jpj\n"
                                      "  do ji = 1,jpi\n"
                                      "    sto_tmp(ji,jj) = 1.0d0\n"
                                      "  end do\n"
                                      "end do\n"
                                      "end program do_loop\n")
    Profiler.add_profile_nodes(schedule, Loop)
    code = str(psy.gen).lower()
    # Check that it's the outer loop that's had profiling added
    assert ("  type(profile_psydatatype), target, save :: profile_psy_data0\n"
            "  call profile_psy_data0 % prestart('do_loop', 'r0', 0, 0)\n"
            "  do jj = 1, jpj" in code)


def test_profile_nemo_openmp(parser):
    ''' Check that the automatic kernel-level profiling handles a
    tightly-nested loop that has been parallelised using OpenMP. '''
    omptrans = OMPParallelLoopTrans()
    Profiler.set_options([Profiler.KERNELS])
    psy, schedule = get_nemo_schedule(parser,
                                      "program do_loop\n"
                                      "integer, parameter :: jpi=5, jpj=5\n"
                                      "integer :: ji, jj\n"
                                      "real :: sto_tmp(jpi,jpj)\n"
                                      "do jj = 1, jpj\n"
                                      "  do ji = 1,jpi\n"
                                      "    sto_tmp(ji,jj) = 1.0d0\n"
                                      "  end do\n"
                                      "end do\n"
                                      "end program do_loop\n")
    omptrans.apply(schedule[0])
    Profiler.add_profile_nodes(schedule, Loop)
    code = str(psy.gen).lower()
    assert ("  type(profile_psydatatype), target, save :: profile_psy_data0\n"
            "  call profile_psy_data0 % prestart('do_loop', 'r0', 0, 0)\n"
            "  !$omp parallel do default(shared), private(ji,jj), "
            "schedule(static)\n"
            "  do jj = 1, jpj" in code)


def test_profile_nemo_no_acc_kernels(parser):
    ''' Check that the automatic kernel-level profiling does not add any
    calls for the case of two kernels within an OpenACC kernels region.
    No calls are added because the PSyData routines would have to have been
    compiled for execution on the GPU. '''
    acctrans = ACCKernelsTrans()
    Profiler.set_options([Profiler.KERNELS])
    psy, schedule = get_nemo_schedule(parser,
                                      "program do_loop\n"
                                      "integer, parameter :: jpi=5, jpj=5\n"
                                      "integer :: ji, jj\n"
                                      "real :: sto_tmp(jpi,jpj)\n"
                                      "do jj = 1, jpj\n"
                                      "  do ji = 1,jpi\n"
                                      "    sto_tmp(ji,jj) = 1.0d0\n"
                                      "  end do\n"
                                      "end do\n"
                                      "do ji = 1, jpi\n"
                                      "  sto_tmp(ji,1) = 0.0d0\n"
                                      "end do\n"
                                      "end program do_loop\n")
    acctrans.apply(schedule.children)
    Profiler.add_profile_nodes(schedule, Loop)
    code = str(psy.gen).lower()
    assert "profile_psy" not in code


def test_profile_nemo_loop_imperfect_nest(parser):
    ''' Check that the automatic kernel-level profiling handles a
    tightly-nested loop within an imperfectly-nested loop. '''
    Profiler.set_options([Profiler.KERNELS])
    psy, schedule = get_nemo_schedule(parser,
                                      "program do_loop\n"
                                      "integer :: ji, jj\n"
                                      "integer, parameter :: jpi=4, jpj=5\n"
                                      "integer :: npt, jt\n"
                                      "logical :: ln_use_this\n"
                                      "real :: sto_tmp(jpi,jpj)\n"
                                      "if(ln_use_this)then\n"
                                      "  do jt = 1, npt\n"
                                      "    do jj = 1, jpj\n"
                                      "      do ji = 1,jpi\n"
                                      "        sto_tmp(ji,jj) = 1.0d0\n"
                                      "      end do\n"
                                      "    end do\n"
                                      "    do ji = 1, jpi\n"
                                      "      sto_tmp(ji,1) = 0.0d0\n"
                                      "    end do\n"
                                      "  end do\n"
                                      "end if\n"
                                      "end program do_loop\n")
    Profiler.add_profile_nodes(schedule, Loop)
    pnodes = schedule.walk(ProfileNode)
    assert len(pnodes) == 2
    tloop = schedule[0].if_body[0]
    assert isinstance(tloop.loop_body[0], ProfileNode)
    assert isinstance(tloop.loop_body[1], ProfileNode)
    code = str(psy.gen).lower()
    assert ("        end do\n"
            "      end do\n"
            "      call profile_psy_data0 % postend\n"
            "      call profile_psy_data1 % prestart('do_loop', 'r1', 0, 0)\n"
            "      do ji = 1, jpi" in code)
