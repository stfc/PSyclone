# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2019, Science and Technology Facilities Council.
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
   representation of NEMO code using the OpenACC data directive.

'''

from __future__ import print_function, absolute_import
import os
import pytest
from fparser.common.readfortran import FortranStringReader
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory, TransInfo, InternalError
from psyclone.transformations import TransformationError


# Constants
API = "nemo"
# Location of the Fortran files associated with these tests
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "../../test_files")

# Test code with explicit NEMO-style do loop
EXPLICIT_DO = ("program explicit_do\n"
               "  REAL, DIMENSION(jpi, jpj, jpk) :: umask\n"
               "  DO jk = 1, jpk\n"
               "     DO jj = 1, jpj\n"
               "        DO ji = 1, jpi\n"
               "           umask(ji,jj,jk) = ji*jj*jk/r\n"
               "        END DO\n"
               "     END DO\n"
               "  END DO\n"
               "end program explicit_do\n")


def test_explicit(parser):
    '''
    Check code generation for enclosing a single explicit loop containing a
    kernel inside a data region.

    '''
    reader = FortranStringReader(EXPLICIT_DO)
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.get('explicit_do').schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    schedule, _ = acc_trans.apply(schedule.children)
    gen_code = str(psy.gen)

    assert ("  REAL, DIMENSION(jpi, jpj, jpk) :: umask\n"
            "  !$ACC DATA COPYOUT(umask)\n"
            "  DO jk = 1, jpk") in gen_code

    assert ("  END DO\n"
            "  !$ACC END DATA\n"
            "END PROGRAM explicit_do") in gen_code


def test_data_no_gen_code():
    ''' Check that the ACCDataDirective.gen_code() method raises the
    expected InternalError as it should not be called. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "explicit_do.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    schedule, _ = acc_trans.apply(schedule.children[0:2])
    with pytest.raises(InternalError) as err:
        schedule.children[0].gen_code(schedule)
    assert ("ACCDataDirective.gen_code should not have "
            "been called" in str(err))


def test_add_region_invalid_data_move():
    ''' Check that _add_region() raises the expected error if an invalid
    value for data_movement is supplied. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "explicit_do.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.get('explicit_do').schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    schedule, _ = acc_trans.apply(schedule.children)
    datadir = schedule.children[0]
    with pytest.raises(InternalError) as err:
        datadir._add_region("DATA", "END DATA", data_movement="invalid")
    assert ("optional data_movement argument must be one of ['present', "
            "'analyse'] but got 'invalid'" in str(err))


def test_add_region(parser):
    ''' Check that add_region works as expected. '''
    from fparser.two import Fortran2003
    reader = FortranStringReader(EXPLICIT_DO)
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.get('explicit_do').schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    schedule, _ = acc_trans.apply(schedule.children)
    datadir = schedule.children[0]
    datadir._add_region("data", "end data")
    assert isinstance(datadir._ast, Fortran2003.Comment)
    assert str(datadir._ast).lower() == "!$acc data"
    assert isinstance(datadir._ast_end, Fortran2003.Comment)
    assert str(datadir._ast_end).lower() == "!$acc end data"


def test_add_region_comment_err(parser):
    ''' Check that _add_region rejects begin and end strings that contain
    comments that are not directives. '''
    reader = FortranStringReader(EXPLICIT_DO)
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.get('explicit_do').schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    schedule, _ = acc_trans.apply(schedule.children)
    datadir = schedule.children[0]
    with pytest.raises(InternalError) as err:
        datadir._add_region("!data", "!end data")
    assert ("start_text must be a plain label without directive or comment "
            "characters but got: '!data'" in str(err))
    with pytest.raises(InternalError) as err:
        datadir._add_region("data", "!end data")
    assert ("end_text must be a plain label without directive or comment "
            "characters but got: '!end data'" in str(err))


def test_data_view(parser, capsys):
    ''' Check that the ACCDataDirective.view() method works as expected. '''
    reader = FortranStringReader(EXPLICIT_DO)
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.get('explicit_do').schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    schedule, _ = acc_trans.apply(schedule.children)
    schedule.view()
    output, _ = capsys.readouterr()
    assert "[ACC DATA]" in output
    assert schedule.children[0].dag_name == "ACC_data_1"


def test_explicit_directive(parser):
    '''Check code generation for a single explicit loop containing a
    kernel with a pre-existing (openacc kernels) directive.

    '''
    reader = FortranStringReader(EXPLICIT_DO)
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.get('explicit_do').schedule
    acc_trans = TransInfo().get_trans_name('ACCKernelsTrans')
    schedule, _ = acc_trans.apply(schedule.children, default_present=True)
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    schedule, _ = acc_trans.apply(schedule.children)
    gen_code = str(psy.gen)

    assert ("  REAL, DIMENSION(jpi, jpj, jpk) :: umask\n"
            "  !$ACC DATA COPYOUT(umask)\n"
            "  !$ACC KERNELS DEFAULT(PRESENT)\n"
            "  DO jk = 1, jpk") in gen_code

    assert ("  END DO\n"
            "  !$ACC END KERNELS\n"
            "  !$ACC END DATA\n"
            "END PROGRAM explicit_do") in gen_code


def test_array_syntax():
    '''Check code generation for a mixture of loops and code blocks.'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "array_syntax.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.get('tra_ldf_iso').schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    # We do not permit arbitrary code blocks to be included in data
    # regions so just put two of the loops into regions.
    schedule, _ = acc_trans.apply([schedule.children[0]])
    schedule, _ = acc_trans.apply([schedule.children[-1]])
    gen_code = str(psy.gen)

    assert ("  REAL(KIND = wp), DIMENSION(jpi, jpj, jpk) :: zdit, zdjt, "
            "zftu, zftv, ztfw\n"
            "  !$ACC DATA COPYOUT(zftv)\n"
            "  zftv(:, :, :) = 0.0D0" in gen_code)

    assert ("  !$ACC DATA COPYOUT(tmask)\n"
            "  tmask(:, :) = jpi\n"
            "  !$ACC END DATA\n"
            "END SUBROUTINE tra_ldf_iso" in gen_code)


def test_multi_data():
    '''Check code generation with multiple data directives.'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "imperfect_nest.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.get('imperfect_nest').schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    schedule, _ = acc_trans.apply(schedule.children[0].children[0:2])
    schedule, _ = acc_trans.apply(schedule.children[0].children[1:3])
    gen_code = str(psy.gen)

    assert ("  DO jk = 1, jpkm1\n"
            "    !$ACC DATA COPYIN(ptb,wmask) "
            "COPYOUT(zdk1t,zdkt)\n"
            "    DO jj = 1, jpj, 1") in gen_code

    assert ("    END IF\n"
            "    !$ACC END DATA\n"
            "    !$ACC DATA COPYIN(e2_e1u,e2u,e3t_n,e3u_n,pahu,r1_e1e2t,"
            "umask,uslp,wmask,zdit,zdk1t,zdkt,zftv) COPYOUT(zftu) COPY(pta)\n"
            "    DO jj = 1, jpjm1") in gen_code

    assert ("    END DO\n"
            "    !$ACC END DATA\n"
            "  END DO") in gen_code


def test_replicated_loop(parser):
    '''Check code generation with two loops that have the same
    structure.

    '''
    reader = FortranStringReader("subroutine replicate()\n"
                                 "   INTEGER :: dummy\n"
                                 "   zwx(:,:) = 0.e0\n"
                                 "   zwx(:,:) = 0.e0\n"
                                 "END subroutine replicate\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.get('replicate').schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    schedule, _ = acc_trans.apply(schedule.children[0:1])
    schedule, _ = acc_trans.apply(schedule.children[1:2])
    gen_code = str(psy.gen)

    assert ("  !$ACC DATA COPYOUT(zwx)\n"
            "  zwx(:, :) = 0.E0\n"
            "  !$ACC END DATA\n"
            "  !$ACC DATA COPYOUT(zwx)\n"
            "  zwx(:, :) = 0.E0\n"
            "  !$ACC END DATA" in gen_code)


def test_data_ref():
    '''Check code generation with an array accessed via a derived type.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "data_ref.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.get('data_ref').schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    schedule, _ = acc_trans.apply(schedule.children)
    gen_code = str(psy.gen)
    assert "!$ACC DATA COPYIN(a) COPYOUT(prof,prof%npind)" in gen_code


def test_no_data_ref_read(parser):
    ''' Check that we reject code that reads from a derived type. This
    limitation will be addressed in #309. '''
    reader = FortranStringReader("program dtype_read\n"
                                 "real(kind=wp) :: sto_tmp(5)\n"
                                 "do ji = 1,jpj\n"
                                 "sto_tmp(ji) = fld%data(ji) + 1._wp\n"
                                 "end do\n"
                                 "end program dtype_read\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    schedule, _ = acc_trans.apply(schedule.children)
    with pytest.raises(NotImplementedError) as err:
        _ = str(psy.gen)
    assert ("derived-type references on the RHS of assignments are not yet "
            "supported" in str(err))


def test_array_section():
    '''Check code generation with a arrays accessed via an array section.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "array_section.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.get('array_section').schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    schedule, _ = acc_trans.apply(schedule.children)
    gen_code = str(psy.gen)
    assert "!$ACC DATA COPYIN(b,c) COPYOUT(a)" in gen_code


def test_kind_parameter(parser):
    ''' Check that we don't attempt to put kind parameters into the list
    of variables to copyin/out. '''
    reader = FortranStringReader("program kind_param\n"
                                 "real(kind=wp) :: sto_tmp(5)\n"
                                 "do ji = 1,jpj\n"
                                 "sto_tmp(ji) = 0._wp\n"
                                 "end do\n"
                                 "end program kind_param\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    schedule, _ = acc_trans.apply(schedule.children[0:1])
    gen_code = str(psy.gen)

    assert "copyin(wp)" not in gen_code.lower()


def test_fn_call(parser):
    ''' Check that we don't attempt to put function names into the list
    of variables we copyin/out. '''
    reader = FortranStringReader("program fn_call\n"
                                 "real(kind=wp) :: sto_tmp(5)\n"
                                 "do ji = 1,jpj\n"
                                 "sto_tmp(ji) = my_func()\n"
                                 "end do\n"
                                 "end program fn_call\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    schedule, _ = acc_trans.apply(schedule.children[0:1])
    gen_code = str(psy.gen)
    assert "copyin(my_func)" not in gen_code.lower()


def test_no_copyin_intrinsics(parser):
    ''' Check that we don't generate a copyin/out for Fortran instrinsic
    functions (i.e. we don't mistake them for array accesses). '''
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    for intrinsic in ["cos(ji)", "sin(ji)", "tan(ji)", "atan(ji)",
                      "mod(ji, 5)"]:
        reader = FortranStringReader(
            "program call_intrinsic\n"
            "real(kind=wp) :: sto_tmp(5)\n"
            "do ji = 1,jpj\n"
            "sto_tmp(ji) = {0}\n"
            "end do\n"
            "end program call_intrinsic\n".format(intrinsic))
        code = parser(reader)
        psy = PSyFactory(API, distributed_memory=False).create(code)
        schedule = psy.invokes.invoke_list[0].schedule
        schedule, _ = acc_trans.apply(schedule.children[0:1])
        gen_code = str(psy.gen)
        idx = intrinsic.index("(")
        assert "copyin({0})".format(intrinsic[0:idx]) not in gen_code.lower()


def test_no_code_blocks(parser):
    ''' Check that we refuse to include CodeBlocks (i.e. code that we
    don't recognise) within a data region. '''
    reader = FortranStringReader("program write_out\n"
                                 "real(kind=wp) :: sto_tmp(5)\n"
                                 "do ji = 1,jpj\n"
                                 "read(*,*) sto_tmp(ji)\n"
                                 "end do\n"
                                 "do ji = 1,jpj\n"
                                 "write(*,*) sto_tmp(ji)\n"
                                 "end do\n"
                                 "end program write_out\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    with pytest.raises(TransformationError) as err:
        _, _ = acc_trans.apply(schedule.children[0:1])
    assert "CodeBlock'>' cannot be enclosed by a ACCDataTrans" in str(err)
    with pytest.raises(TransformationError) as err:
        _, _ = acc_trans.apply(schedule.children[1:2])
    assert "CodeBlock'>' cannot be enclosed by a ACCDataTrans" in str(err)


def test_kernels_in_data_region(parser):
    ''' Check that directives end up in the correct locations when enclosing
    a kernels region inside a data region. '''
    reader = FortranStringReader("program one_loop\n"
                                 "real(kind=wp) :: sto_tmp(5)\n"
                                 "do ji = 1,jpj\n"
                                 "  sto_tmp(ji) = 0.0\n"
                                 "end do\n"
                                 "end program one_loop\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_dtrans = TransInfo().get_trans_name('ACCDataTrans')
    acc_ktrans = TransInfo().get_trans_name('ACCKernelsTrans')
    schedule, _ = acc_ktrans.apply(schedule.children[:], default_present=True)
    schedule, _ = acc_dtrans.apply(schedule.children[:])
    new_code = str(psy.gen)
    assert ("  !$ACC DATA COPYOUT(sto_tmp)\n"
            "  !$ACC KERNELS DEFAULT(PRESENT)\n"
            "  DO ji = 1, jpj\n" in new_code)
    assert ("  END DO\n"
            "  !$ACC END KERNELS\n"
            "  !$ACC END DATA\n"
            "END PROGRAM one_loop" in new_code)


def test_no_enter_data(parser):
    ''' Check that we refuse to allow a data region to be created in a
    Schedule that has already had an Enter Data node added to it. '''
    reader = FortranStringReader(EXPLICIT_DO)
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.get('explicit_do').schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    # We don't yet support ACCEnterDataTrans for the NEMO API (Issue 310)
    # so manually insert a GOACCEnterDataDirective in the Schedule.
    from psyclone.gocean1p0 import GOACCEnterDataDirective
    directive = GOACCEnterDataDirective(parent=schedule, children=[])
    schedule.children.insert(0, directive)
    with pytest.raises(TransformationError) as err:
        _, _ = acc_trans.apply(schedule.children)
    assert ("Cannot add an OpenACC data region to a schedule that already "
            "contains an 'enter data' directive" in str(err))


def test_array_access_in_ifblock(parser):
    ''' Check that we generate the necessary copyin clause when a data region
    contains an IF clause with an array access. '''
    code = ("program ifclause\n"
            "  real(kind=wp) :: zmask(8,8), zdta(8,8)\n"
            "  integer :: ji, jj\n"
            "  zmask(:,:) = 1.0\n"
            "  do jj = 1, 8\n"
            "    do ji = 1, 8\n"
            "      if(zmask(ji,jj) < 0.0)then\n"
            "        zdta(ji,jj) = 0.0\n"
            "      end if\n"
            "    end do\n"
            "  end do\n"
            "end program ifclause\n")
    reader = FortranStringReader(code)
    ptree = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(ptree)
    schedule = psy.invokes.get('ifclause').schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    # Put the second loop nest inside a data region
    acc_trans.apply(schedule.children[1:])
    gen_code = str(psy.gen).lower()
    assert " copyin(zmask)" in gen_code


@pytest.mark.xfail(reason="Fail to identify array accesses in loop bounds, "
                   "issue #309")
def test_array_access_loop_bounds(parser):
    ''' Check that we raise the expected error if our code that identifies
    read and write accesses misses an array access. '''
    code = ("program do_bound\n"
            "  real(kind=wp) :: trim_width(8), zdta(8,8)\n"
            "  integer :: ji, jj\n"
            "  do jj = 1, trim_width(dom)\n"
            "    do ji = 1, 8\n"
            "      zdta(ji,jj) = 0.0\n"
            "    end do\n"
            "  end do\n"
            "end program do_bound\n")
    reader = FortranStringReader(code)
    ptree = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(ptree)
    schedule = psy.invokes.get('do_bound').schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    # Put the second loop nest inside a data region
    acc_trans.apply(schedule.children)
    gen_code = str(psy.gen).lower()
    assert "copyin(trim_width)" in gen_code


def test_missed_array_case(parser):
    ''' Check that we raise the expected InternalError if our internal
    sanity check spots that we've missed an array access. This test
    should be eliminated as part of #309. '''
    code = ("program do_bound\n"
            "  real(kind=wp) :: trim_width(8), zdta(8,8)\n"
            "  integer :: ji, jj\n"
            "  do jj = 1, trim_width(dom)\n"
            "    do ji = 1, 8\n"
            "      select case(ice_mask(ji,jj))\n"
            "      case(0)\n"
            "        zdta(ji,jj) = 1.0\n"
            "      case(1)\n"
            "        zdta(ji,jj) = 0.0\n"
            "      end select\n"
            "    end do\n"
            "  end do\n"
            "end program do_bound\n")
    reader = FortranStringReader(code)
    ptree = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(ptree)
    schedule = psy.invokes.get('do_bound').schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    # Put the second loop nest inside a data region
    acc_trans.apply(schedule.children)
    with pytest.raises(InternalError) as err:
        _ = str(psy.gen)
    assert ("Array 'ice_mask' present in source code ('ice_mask(ji, jj)') "
            "but not identified" in str(err))
