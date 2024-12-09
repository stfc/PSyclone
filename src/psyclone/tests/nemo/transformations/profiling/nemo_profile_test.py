# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2024, Science and Technology Facilities Council.
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
# Modified by: S. Siso, STFC Daresbury Lab

'''Module containing py.test tests for the transformation of the PSy
   representation of NEMO code to insert profiling calls.

'''

import pytest
from psyclone.psyir.transformations import ProfileTrans, TransformationError


# The transformation that most of these tests use
PTRANS = ProfileTrans()


def test_profile_single_loop(fortran_reader, fortran_writer):
    ''' Check that the correct code is added to the generated Fortran
    when profiling a single loop nest. '''
    psyir = fortran_reader.psyir_from_source(
              "program do_loop\n"
              "use kind_mod, only: wp\n"
              "integer :: ji\n"
              "integer, parameter :: jpj=2\n"
              "real :: sto_tmp(jpj), sto_tmp2(jpj)\n"
              "do ji = 1,jpj\n"
              "  sto_tmp(ji) = 1.0d0\n"
              "end do\n"
              "end program do_loop\n")
    PTRANS.apply(psyir.children[0].children[0])
    code = fortran_writer(psyir).lower()
    assert (
        "  use kind_mod, only : wp\n"
        "  use profile_psy_data_mod, only : profile_psydatatype\n" in code)
    assert (
        "  real, dimension(jpj) :: sto_tmp\n"
        "  real, dimension(jpj) :: sto_tmp2\n"
        "  type(profile_psydatatype), save, target :: profile_psy_data\n"
        in code)
    assert (
        "  call profile_psy_data % prestart(\"do_loop\", \"r0\", 0, 0)\n"
        "  do ji = 1, jpj, 1\n"
        "    sto_tmp(ji) = 1.0d0\n"
        "  enddo\n"
        "  call profile_psy_data % postend\n" in code)


def test_profile_single_loop_named(fortran_reader, fortran_writer):
    '''Check that the correct code is added to the generated Fortran when
    profiling a single loop nest with the profile being named by the
    user.

    '''
    psyir = fortran_reader.psyir_from_source(
              "program do_loop\n"
              "integer :: ji\n"
              "integer, parameter :: jpj=34\n"
              "real :: sto_tmp(jpj), sto_tmp2(jpj)\n"
              "do ji = 1,jpj\n"
              "  sto_tmp(ji) = 1.0d0\n"
              "end do\n"
              "end program do_loop\n")
    options = {"region_name": ("my_routine", "my_region")}
    PTRANS.apply(psyir.children[0].children[0], options=options)
    code = fortran_writer(psyir).lower()
    assert ("call profile_psy_data % prestart(\"my_routine\", \"my_region\", "
            "0, 0)" in code)


def test_profile_two_loops(fortran_reader, fortran_writer):
    ''' Check that the correct code is added to the generated Fortran
    when profiling two, separate loop nests. '''
    psyir = fortran_reader.psyir_from_source(
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
    schedule = psyir.children[0]
    # Create two separate profiling regions
    PTRANS.apply(schedule[1])
    PTRANS.apply(schedule[0])
    code = fortran_writer(psyir).lower()
    assert (
        "  use kind_mod, only : wp\n"
        "  use profile_psy_data_mod, only : profile_psydatatype\n"
        in code)
    assert code.count("use profile_psy_data_mod") == 1
    assert (
        "  real, dimension(jpj) :: sto_tmp\n"
        "  real, dimension(jpj) :: sto_tmp2\n"
        "  type(profile_psydatatype), save, target :: profile_psy_data\n"
        "  type(profile_psydatatype), save, target :: profile_psy_data_1\n"
        in code)
    assert (
        "  call profile_psy_data_1 % prestart(\"do_loop\", \"r0\", 0, 0)\n"
        "  do ji = 1, jpj, 1\n"
        "    sto_tmp(ji) = 1.0d0\n"
        "  enddo\n"
        "  call profile_psy_data_1 % postend\n" in code)
    assert (
        "  call profile_psy_data % prestart(\"do_loop\", \"r1\", 0, 0)\n"
        "  do ji = 1, jpj, 1\n"
        "    sto_tmp2(ji) = 1.0d0\n"
        "  enddo\n"
        "  call profile_psy_data % postend\n" in code)


def test_profile_codeblock(fortran_reader, fortran_writer):
    ''' Check that we can put profiling calls around a region containing
    a CodeBlock. '''
    psyir = fortran_reader.psyir_from_source(
              "subroutine cb_test()\n"
              "use kind_mod, only: wp\n"
              "integer :: ji\n"
              "integer, parameter :: jpj=128\n"
              "real :: sto_tmp2(jpj)\n"
              "do ji = 1,jpj\n"
              "  write(*,*) sto_tmp2(ji)\n"
              "end do\n"
              "end subroutine cb_test\n")
    schedule = psyir.children[0]
    PTRANS.apply(schedule.children[0])
    code = fortran_writer(psyir).lower()
    assert (
        "  call profile_psy_data % prestart(\"cb_test\", \"r0\", 0, 0)\n"
        "  do ji = 1, jpj, 1\n"
        "    ! psyclone codeblock (unsupported code) reason:\n"
        "    !  - unsupported statement: write_stmt\n"
        "    write(*, *) sto_tmp2(ji)\n"
        "  enddo\n"
        "  call profile_psy_data % postend\n" in code)


def test_profile_inside_if1(fortran_reader, fortran_writer):
    ''' Check that we can put a profiling region inside an If block when
    we pass the transformation the first (and only) child of the Schedule
    of the If. '''
    psyir = fortran_reader.psyir_from_source(
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
    schedule = psyir.children[0]
    PTRANS.apply(schedule.children[0].if_body[0])
    gen_code = fortran_writer(psyir).lower()
    assert ("  if (do_this) then\n"
            "    call profile_psy_data % prestart(" in gen_code)
    assert ("    call profile_psy_data % postend\n"
            "  end if\n" in gen_code)


def test_profile_inside_if2(fortran_reader, fortran_writer):
    ''' Check that we can put a profiling region inside an If block
    when we pass the transformation the Schedule rather than its child. '''
    psyir = fortran_reader.psyir_from_source(
        "subroutine inside_if_test()\n"
        "use kind_mod, only: wp\n"
        "integer :: ji\n"
        "integer, parameter :: jpj=256\n"
        "real :: sto_tmp2(jpj)\n"
        "logical, parameter :: do_this = .true.\n"
        "if(do_this)then\n"
        "  do ji = 1,jpj\n"
        "    write(*,*) sto_tmp2(ji)\n"
        "  end do\n"
        "endif\n"
        "end subroutine inside_if_test\n")
    schedule = psyir.children[0]
    PTRANS.apply(schedule.children[0].if_body)
    gen_code = fortran_writer(psyir).lower()
    assert ("  if (do_this) then\n"
            "    call profile_psy_data % prestart(" in gen_code)
    assert ("    call profile_psy_data % postend\n"
            "  end if\n" in gen_code)


def test_profile_single_line_if(fortran_reader, fortran_writer):
    ''' Test that we can put the body of a single-line if statement inside a
    profiling region. '''
    psyir = fortran_reader.psyir_from_source(
        "subroutine one_line_if_test()\n"
        "use kind_mod, only: wp\n"
        "integer :: ji\n"
        "integer, parameter :: jpj=32\n"
        "real :: sto_tmp2(jpj)\n"
        "logical, parameter :: do_this = .true.\n"
        "if(do_this) write(*,*) sto_tmp2(ji)\n"
        "end subroutine one_line_if_test\n")
    schedule = psyir.children[0]
    PTRANS.apply(schedule[0].if_body)
    gen_code = fortran_writer(psyir).lower()
    assert (
        "  if (do_this) then\n"
        "    call profile_psy_data % prestart(\"one_line_if_test\", \"r0\", 0,"
        " 0)\n"
        "    ! psyclone codeblock (unsupported code) reason:\n"
        "    !  - unsupported statement: write_stmt\n"
        "    write(*, *) sto_tmp2(ji)\n"
        "    call profile_psy_data % postend\n"
        "  end if\n" in gen_code)


def test_profiling_case(fortran_reader, fortran_writer):
    ''' Check that we can put profiling around a case statement. This is
    harder than might be expected because of the complexity of mapping back
    from the PSyIR (where SELECT CASE blocks are represented as If blocks)
    to the fparser2 parse tree. '''
    psyir = fortran_reader.psyir_from_source(
        "subroutine my_test()\n"
        "   integer :: ji, jj, ii, je_2, jpi, jpj, nldj_crs\n"
        "   integer :: nistr, niend, njstr, njend, nn_factx, nn_facty\n"
        "   integer, dimension(:) :: mje_crs, mjs_crs, mis_crs\n"
        "   real, dimension(:,:) :: p_fld_crs, p_e12, p_fld, zsurfmsk\n"
        "   real, dimension(:,:,:) :: p_mask, p_e3\n"
        "   real :: zflcrs, rfactx_r\n"
        "   character(len=3) :: cd_op\n"
        "   p_fld_crs(:,:) = 0.0\n"
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
    sched = psyir.children[0]
    # Innermost if-body
    PTRANS.apply(sched[1].if_body[2].if_body[0].if_body.children)
    # Body of first CASE
    PTRANS.apply(sched[1].if_body[:])
    # Body of second CASE
    PTRANS.apply(sched[1].else_body.children)
    # Whole routine
    PTRANS.apply(sched.children)
    code = fortran_writer(psyir).lower()
    assert (
        "  type(profile_psydatatype), save, target :: profile_psy_data\n"
        "  type(profile_psydatatype), save, target :: profile_psy_data_1\n"
        "  type(profile_psydatatype), save, target :: profile_psy_data_2\n"
        "  type(profile_psydatatype), save, target :: profile_psy_data_3\n"
        "\n"
        "  call profile_psy_data_3 % prestart(\"my_test\", \"r0\", 0, 0)\n"
        "  p_fld_crs(:,:) = 0.0\n" in code)
    assert ("      if (mje_crs(2) - mjs_crs(2) == 1) then\n"
            "        call profile_psy_data % prestart(\"my_test\", \"r2\", 0, "
            "0)\n"
            in code)
    assert ("        enddo\n"
            "        call profile_psy_data % postend\n"
            "      end if\n" in code)
    assert ("  if (cd_op == 'vol') then\n"
            "    call profile_psy_data_1 % prestart(\"my_test\", \"r1\", 0, 0)"
            "\n" in code)
    assert ("    call profile_psy_data_1 % postend\n"
            "  else\n"
            "    call profile_psy_data_2 % prestart(\"my_test\", \"r3\", 0, 0)"
            "\n" in code)
    assert ("    call profile_psy_data_2 % postend\n"
            "  end if\n"
            "  call profile_psy_data_3 % postend\n"
            "\n"
            "end subroutine my_test" in code)


def test_profiling_case_loop(fortran_reader, fortran_writer):
    ''' Check that we can put profiling around a CASE and a subsequent
    loop. '''
    psyir = fortran_reader.psyir_from_source(
                "subroutine my_test()\n"
                "  use my_mod, only: a_type, ctl_stop\n"
                "  integer :: igrd, ib, ii\n"
                "  type(a_type) :: idx\n"
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
    sched = psyir.children[0]
    PTRANS.apply(sched.children)
    code = fortran_writer(psyir).lower()
    assert ("  call profile_psy_data % prestart(\"my_test\", \"r0\", 0, 0)\n"
            "  if (igrd == 1) then\n" in code)
    assert ("call profile_psy_data % postend\n\n"
            "end subroutine" in code)


def test_profiling_mod_use_clash(fortran_reader):
    ''' Check that we abort cleanly if we encounter a 'use' of a module that
    clashes with the one we would 'use' for the profiling API. '''
    psyir = fortran_reader.psyir_from_source(
                "program the_clash\n"
                "  use profile_psy_data_mod, only: "
                "some_var\n"
                "  real :: my_array(20,10)\n"
                "  my_array(:,:) = 0.0\n"
                "end program the_clash\n")
    schedule = psyir.children[0]
    with pytest.raises(TransformationError) as err:
        PTRANS.apply(schedule.children[0])
    assert ("Cannot add PSyData calls because there is already a symbol "
            "named 'profile_psy_data_mod' which clashes" in str(err.value))


def test_profiling_mod_name_clash(fortran_reader):
    ''' Check that we abort cleanly if we encounter code that has a name
    clash with the name of the profiling API module. '''
    psyir = fortran_reader.psyir_from_source(
                "program profile_psy_data_mod\n"
                "  real :: my_array(3,3)\n"
                "  my_array(:,:) = 0.0\n"
                "end program profile_psy_data_mod\n")
    schedule = psyir.children[0]
    with pytest.raises(TransformationError) as err:
        PTRANS.apply(schedule.children[0])
    assert ("Cannot add PSyData calls because there is already a symbol "
            "named 'profile_psy_data_mod' which clashes " in str(err.value))


def test_profiling_symbol_clash(fortran_reader):
    ''' Check that we abort cleanly if we encounter code that has a name
    clash with any of the symbols we 'use' from profile_mode. '''
    for sym in ["PSyDataType", "psy_data_mod"]:
        psyir = fortran_reader.psyir_from_source(
            f"program my_test\n"
            f"  real :: my_array(3,3)\n"
            f"  integer :: profile_{sym}\n"
            f"  my_array(:,:) = 0.0\n"
            f"end program my_test\n")
        schedule = psyir.children[0]
        with pytest.raises(TransformationError) as err:
            PTRANS.apply(schedule.children[0])
        assert (f"Cannot add PSyData calls because there is already a symbol "
                f"named 'profile_{sym}' which clashes with one of those used "
                f"by the PSyclone PSyData API."
                in str(err.value))


def test_profiling_var_clash(fortran_reader, fortran_writer):
    ''' Check that we generate the expected code if we encounter code that has
    a potential name clash with the variables we will introduce for each
    profiling region. '''
    psyir = fortran_reader.psyir_from_source(
        "program my_test\n"
        "  real :: my_array(3,3)\n"
        "  integer :: profile_psy_data\n"
        "  my_array(:,:) = 0.0\n"
        "end program my_test\n")
    schedule = psyir.children[0]
    PTRANS.apply(schedule.children[0])
    code = fortran_writer(psyir).lower()
    assert ("  integer :: profile_psy_data\n"
            "  type(profile_psydatatype), save, target :: profile_psy_data_1"
            in code)


def test_no_return_in_profiling(fortran_reader):
    ''' Check that the transformation refuses to include a Return node within
    a profiled region. '''
    psyir = fortran_reader.psyir_from_source(
        "subroutine my_test()\n"
        "  integer :: my_test\n"
        "  real :: my_array(3,3)\n"
        "  my_array(:,:) = 0.0\n"
        "  my_test = 1\n"
        "  return\n"
        "end subroutine my_test\n")
    schedule = psyir.children[0]
    with pytest.raises(TransformationError) as err:
        PTRANS.apply(schedule.children)
    assert ("cannot be enclosed by a ProfileTrans "
            "transformation" in str(err.value))
