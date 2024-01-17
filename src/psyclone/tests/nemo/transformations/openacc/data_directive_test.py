# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2024, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified by J. Henrichs, Bureau of Meteorology

'''Module containing py.test tests for the transformation of the PSy
   representation of NEMO code using the OpenACC data directive.

'''

import os
import pytest

from fparser.common.readfortran import FortranStringReader
from psyclone.errors import InternalError
from psyclone.psyGen import PSyFactory, TransInfo
from psyclone.psyir.nodes import ACCDataDirective, Schedule, Routine
from psyclone.psyir.transformations import TransformationError
from psyclone.tests.utilities import get_invoke, Compile


# Constants
API = "nemo"
# Location of the Fortran files associated with these tests
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "../../test_files")

# Test code with explicit NEMO-style do loop
EXPLICIT_DO = ("program explicit_do\n"
               "  REAL :: r\n"
               "  INTEGER :: ji, jj, jk\n"
               "  INTEGER, PARAMETER :: jpi=3, jpj=5, jpk=7\n"
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
    acc_trans.apply(schedule.children)
    gen_code = str(psy.gen).lower()

    assert ("  real, dimension(jpi,jpj,jpk) :: umask\n"
            "\n"
            "  !$acc data copyout(umask)\n"
            "  do jk = 1, jpk") in gen_code

    assert ("  enddo\n"
            "  !$acc end data\n"
            "\n"
            "end program explicit_do") in gen_code


def test_data_single_node(parser):
    ''' Check that the ACCDataTrans works if passed a single node rather
    than a list. '''
    reader = FortranStringReader(EXPLICIT_DO)
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.get('explicit_do').schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    acc_trans.apply(schedule[0])
    assert isinstance(schedule[0], ACCDataDirective)


def test_data_no_gen_code():
    ''' Check that the ACCDataDirective.gen_code() method raises the
    expected InternalError as it should not be called. '''
    _, invoke_info = get_invoke("explicit_do.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    acc_trans.apply(schedule.children[0:2])
    with pytest.raises(InternalError) as err:
        schedule.children[0].gen_code(schedule)
    assert ("ACCDataDirective.gen_code should not have "
            "been called" in str(err.value))


def test_explicit_directive(parser):
    '''Check code generation for a single explicit loop containing a
    kernel with a pre-existing (openacc kernels) directive.

    '''
    reader = FortranStringReader(EXPLICIT_DO)
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.get('explicit_do').schedule
    acc_trans = TransInfo().get_trans_name('ACCKernelsTrans')
    acc_trans.apply(schedule.children, {"default_present": True})
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    acc_trans.apply(schedule.children)
    gen_code = str(psy.gen).lower()

    assert ("  real, dimension(jpi,jpj,jpk) :: umask\n"
            "\n"
            "  !$acc data copyout(umask)\n"
            "  !$acc kernels default(present)\n"
            "  do jk = 1, jpk, 1") in gen_code

    assert ("  enddo\n"
            "  !$acc end kernels\n"
            "  !$acc end data\n"
            "\n"
            "end program explicit_do") in gen_code


def test_array_syntax():
    '''Check code generation for a mixture of loops and code blocks.'''
    psy, invoke_info = get_invoke("array_syntax.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    # We do not permit arbitrary code blocks to be included in data
    # regions so just put two of the loops into regions.
    acc_trans.apply([schedule.children[0]])
    acc_trans.apply([schedule.children[-1]])
    gen_code = str(psy.gen).lower()

    assert ("  real(kind=wp), dimension(jpi,jpj,jpk) :: ztfw\n"
            "\n"
            "  !$acc data copyout(zftv)\n"
            "  zftv(:,:,:) = 0.0d0" in gen_code)

    assert ("  !$acc data copyout(tmask)\n"
            "  tmask(:,:) = jpi\n"
            "  !$acc end data\n"
            "\n"
            "end subroutine tra_ldf_iso" in gen_code)


def test_multi_data():
    '''Check code generation with multiple data directives.'''
    psy, invoke_info = get_invoke("imperfect_nest.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    acc_trans.apply(schedule.children[0].loop_body[0:2])
    acc_trans.apply(schedule.children[0].loop_body[1:3])
    gen_code = str(psy.gen)

    assert ("  do jk = 1, jpkm1, 1\n"
            "    !$acc data copyin(ptb,wmask), copyout(zdk1t,zdkt)\n"
            "    do jj = 1, jpj, 1") in gen_code

    assert ("    end if\n"
            "    !$acc end data\n"
            "    !$acc data copyin(e2_e1u,e2u,e3t_n,e3u_n,pahu,r1_e1e2t,"
            "umask,uslp,wmask,zdit,zdk1t,zdkt,zftv), copyout(zftu), "
            "copy(pta)\n"
            "    do jj = 1, jpjm1, 1") in gen_code

    assert ("    enddo\n"
            "    !$acc end data\n"
            "  enddo") in gen_code


def test_replicated_loop(parser, tmpdir):
    '''Check code generation with two loops that have the same
    structure.

    '''
    reader = FortranStringReader("subroutine replicate()\n"
                                 "   INTEGER :: dummy\n"
                                 "   REAL :: zwx(10,10)\n"
                                 "   zwx(:,:) = 0.e0\n"
                                 "   zwx(:,:) = 0.e0\n"
                                 "END subroutine replicate\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.get('replicate').schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    acc_trans.apply(schedule.children[0:1])
    acc_trans.apply(schedule.children[1:2])
    gen_code = str(psy.gen)

    assert ("  !$acc data copyout(zwx)\n"
            "  zwx(:,:) = 0.e0\n"
            "  !$acc end data\n"
            "  !$acc data copyout(zwx)\n"
            "  zwx(:,:) = 0.e0\n"
            "  !$acc end data" in gen_code)
    assert Compile(tmpdir).string_compiles(gen_code)


def test_data_ref(parser):
    '''Check code generation with an array accessed via a derived type.

    '''
    reader = FortranStringReader('''subroutine data_ref()
  use some_mod, only: prof_type
  INTEGER, parameter :: n=16
  INTEGER :: ji
  real :: a(n), fconst
  type(prof_type) :: prof
  do ji = 1, n
     prof%npind(ji) = 2.0*a(ji) + fconst
  end do
END subroutine data_ref
''')
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    acc_trans.apply(schedule.children)
    gen_code = str(psy.gen)
    assert "!$acc data copyin(a), copyout(prof,prof%npind)" in gen_code


def test_data_ref_read(parser):
    ''' Check support for reading from derived types. '''
    reader = FortranStringReader("program dtype_read\n"
                                 "use field_mod, only: fld_type, wp\n"
                                 "real(kind=wp) :: sto_tmp(5)\n"
                                 "integer :: ji\n"
                                 "integer, parameter :: jpj = 10\n"
                                 "type(fld_type) :: fld\n"
                                 "do ji = 1,jpj\n"
                                 "sto_tmp(ji) = fld%data(ji) + 1._wp\n"
                                 "end do\n"
                                 "end program dtype_read\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    acc_trans.apply(schedule.children)
    gen_code = str(psy.gen)
    assert "copyin(fld,fld%data)" in gen_code


def test_multi_array_derived_type(fortran_reader, fortran_writer):
    '''
    Check that we generate the correct clause if the derived-type contains
    more than one array access but only one is iterated over.
    '''
    psyir = fortran_reader.psyir_from_source(
        "program dtype_read\n"
        "use field_mod, only: fld_type\n"
        "integer, parameter :: jpj = 10\n"
        "type(fld_type), dimension(5) :: small_holding\n"
        "real, dimension(jpj) :: sto_tmp\n"
        "integer :: ji\n"
        "do ji = 1,jpj\n"
        "  sto_tmp(ji) = small_holding(2)%data(ji) + 1.0\n"
        "end do\n"
        "end program dtype_read\n")
    schedule = psyir.walk(Schedule)[0]
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    acc_trans.apply(schedule.children)
    gen_code = fortran_writer(psyir)
    assert ("!$acc data copyin(small_holding,small_holding(2)%data), "
            "copyout(sto_tmp)" in gen_code)


def test_multi_array_derived_type_error(fortran_reader):
    '''
    Check that we raise the expected error if the derived-type contains
    more than one array access and they are both iterated over.
    '''
    psyir = fortran_reader.psyir_from_source(
        "program dtype_read\n"
        "use field_mod, only: fld_type\n"
        "integer, parameter :: jpj = 10\n"
        "type(fld_type), dimension(5) :: small_holding\n"
        "real, dimension(jpj) :: sto_tmp\n"
        "integer :: ji, jf\n"
        "sto_tmp(:) = 0.0\n"
        "do jf = 1, 5\n"
        "do ji = 1,jpj\n"
        "  sto_tmp(ji) = sto_tmp(ji) + small_holding(3)%grid(jf)%data(ji)\n"
        "end do\n"
        "end do\n"
        "end program dtype_read\n")
    schedule = psyir.walk(Schedule)[0]
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    with pytest.raises(TransformationError) as err:
        acc_trans.apply(schedule.children)
    assert ("Data region contains a structure access 'small_holding(3)%"
            "grid(jf)%data(ji)' where component 'grid' is an array and is "
            "iterated over (variable 'jf'). Deep copying of data for "
            "structures is only supported where the deepest component is the "
            "one being iterated over." in str(err.value))


def test_array_section():
    '''Check code generation with a arrays accessed via an array section.

    '''
    psy, invoke_info = get_invoke("array_section.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    acc_trans.apply(schedule.children)
    gen_code = str(psy.gen).lower()
    assert "!$acc data copyin(b,c), copyout(a)" in gen_code


def test_kind_parameter(parser):
    ''' Check that we don't attempt to put kind parameters into the list
    of variables to copyin/out. '''
    reader = FortranStringReader("program kind_param\n"
                                 "use kind_params_mod\n"
                                 "integer :: ji, jpj\n"
                                 "real(kind=wp) :: sto_tmp(5)\n"
                                 "do ji = 1,jpj\n"
                                 "sto_tmp(ji) = 0._wp\n"
                                 "end do\n"
                                 "end program kind_param\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    acc_trans.apply(schedule.children[0:1])
    gen_code = str(psy.gen)

    assert "copyin(wp)" not in gen_code.lower()


def test_no_copyin_intrinsics(fortran_reader, fortran_writer):
    ''' Check that we don't generate a copyin/out for Fortran intrinsic
    functions (i.e. we don't mistake them for array accesses). '''
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    for intrinsic in ["cos(ji)", "sin(ji)", "tan(ji)", "atan(ji)",
                      "mod(ji, 5)"]:
        code = (f"program call_intrinsic\n"
                f"use kind_params_mod\n"
                f"integer :: ji, jpj\n"
                f"real(kind=wp) :: sto_tmp(5)\n"
                f"do ji = 1,jpj\n"
                f"sto_tmp(ji) = {intrinsic}\n"
                f"end do\n"
                f"end program call_intrinsic\n")
        psy = fortran_reader.psyir_from_source(code)
        schedule = psy.walk(Routine)[0]
        acc_trans.apply(schedule.children[0:1])
        gen_code = fortran_writer(psy)
        idx = intrinsic.index("(")
        assert f"copyin({intrinsic[0:idx]})" not in gen_code.lower()


def test_no_code_blocks(parser):
    ''' Check that we refuse to include CodeBlocks (i.e. code that we
    don't recognise) within a data region. '''
    reader = FortranStringReader("program write_out\n"
                                 "integer, parameter :: wp = kind(1.0)\n"
                                 "integer :: ji, jpj\n"
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
        acc_trans.apply(schedule.children[0:1])
    assert ("'CodeBlock' cannot be enclosed by a ACCDataTrans"
            in str(err.value))
    with pytest.raises(TransformationError) as err:
        acc_trans.apply(schedule.children[1:2])
    assert ("'CodeBlock' cannot be enclosed by a ACCDataTrans"
            in str(err.value))


def test_kernels_in_data_region(parser):
    ''' Check that directives end up in the correct locations when enclosing
    a kernels region inside a data region. '''
    reader = FortranStringReader("program one_loop\n"
                                 "use kind_params_mod\n"
                                 "integer :: ji, jpj\n"
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
    acc_ktrans.apply(schedule.children[:], {"default_present": True})
    acc_dtrans.apply(schedule.children[:])
    new_code = str(psy.gen).lower()
    assert ("  !$acc data copyout(sto_tmp)\n"
            "  !$acc kernels default(present)\n"
            "  do ji = 1, jpj, 1\n" in new_code)
    assert ("  enddo\n"
            "  !$acc end kernels\n"
            "  !$acc end data\n"
            "\n"
            "end program one_loop" in new_code)


def test_no_enter_data(parser):
    ''' Check that we refuse to allow a data region to be created in a
    Schedule that has already had an Enter Data node added to it. '''
    reader = FortranStringReader(EXPLICIT_DO)
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.get('explicit_do').schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    enter_data_trans = TransInfo().get_trans_name('ACCEnterDataTrans')
    enter_data_trans.apply(schedule)
    with pytest.raises(TransformationError) as err:
        acc_trans.apply(schedule.children)
    assert ("Cannot add an OpenACC data region to a schedule that already "
            "contains an 'enter data' directive" in str(err.value))


def test_array_access_in_ifblock(parser):
    ''' Check that we generate the necessary copyin clause when a data region
    contains an IF clause with an array access. '''
    code = ("program ifclause\n"
            "  use kind_params_mod\n"
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


def test_array_access_loop_bounds(parser):
    ''' Check that the correct data-movement statement is generated when
    the region contains a loop that has an array-access as one of its
    bounds. '''
    code = ("program do_bound\n"
            "  use kind_params_mod\n"
            "  real(kind=wp) :: trim_width(8), zdta(8,8)\n"
            "  integer :: ji, jj, dom\n"
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
