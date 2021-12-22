# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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

'''Module containing py.test tests for the transformation of the PSy
   representation of NEMO code using the OpenACC enterdata directive.

'''

from __future__ import print_function, absolute_import

import os
import pytest

from fparser.common.readfortran import FortranStringReader
from psyclone.psyGen import PSyFactory
from psyclone.psyir.transformations import TransformationError
from psyclone.transformations import ACCEnterDataTrans, ACCKernelsTrans
from psyclone.psyir.transformations import ACCUpdateTrans
from psyclone.tests.utilities import Compile, get_invoke


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
    acc_kernels = ACCKernelsTrans()
    acc_kernels.apply(schedule.children)
    acc_trans = ACCEnterDataTrans()
    acc_trans.apply(schedule)
    gen_code = str(psy.gen).lower()
    assert ("  real, dimension(jpi,jpj,jpk) :: umask\n"
            "\n"
            "  !$acc enter data copyin(jpi,jpj,jpk,r,umask)\n"
            "  !$acc kernels\n"
            "  do jk = 1, jpk") in gen_code

    assert ("  enddo\n"
            "  !$acc end kernels\n"
            "\n"
            "end program explicit_do") in gen_code


def test_simple_missed_region(parser):
    ''' Check code generation when there is a simple section of host code
    between two kernels regions. '''
    code = '''
SUBROUTINE tra_ldf_iso()
  USE some_mod, only: dia_ptr_hst
  INTEGER, PARAMETER :: jpi=2, jpj=2, jpk=2
  INTEGER :: jn
  LOGICAL :: l_ptr
  INTEGER, DIMENSION(jpi,jpj) :: tmask
  REAL, DIMENSION(jpi,jpj,jpk) ::   zdit, zdjt, zftu, zftv, ztfw
  zftv(:,:,:) = 0.0d0
  IF( l_ptr )THEN
    CALL dia_ptr_hst( jn, 'ldf', -zftv(:,:,:)  )
    zftv(:,:,:) = 1.0d0
  END IF
  CALL dia_ptr_hst( jn, 'ldf', -zftv(:,:,:)  )
  zftu(:,:,1) = 1.0d0
  tmask(:,:) = jpi
end SUBROUTINE tra_ldf_iso
'''
    reader = FortranStringReader(code)
    ast = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(ast)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = ACCEnterDataTrans()
    acc_kernels = ACCKernelsTrans()
    acc_update = ACCUpdateTrans()
    # We do not permit arbitrary code blocks to be included in data
    # regions so just put two of the loops into regions.
    acc_kernels.apply(schedule[0])
    acc_kernels.apply(schedule[3:5])
    acc_trans.apply(schedule)
    acc_update.apply(schedule)
    gen_code = str(psy.gen).lower()
    assert ("  real, dimension(jpi,jpj,jpk) :: ztfw\n"
            "\n"
            "  !$acc enter data copyin(zftv,jpi,tmask,zftu)\n"
            "  !$acc kernels\n"
            "  zftv(:,:,:) = 0.0d0\n"
            "  !$acc end kernels\n"
            "  !$acc update if(acc_is_present(jn)) host(jn)\n"
            "  !$acc update if(acc_is_present(l_ptr)) host(l_ptr)\n"
            "  !$acc update if(acc_is_present(zftv)) host(zftv)\n"
            "  if (l_ptr) then\n" in gen_code)

    assert ("  !$acc kernels\n"
            "  zftu(:,:,1) = 1.0d0\n"
            "  tmask(:,:) = jpi\n"
            "  !$acc end kernels\n"
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
            "    !$acc data copyin(ptb,wmask) copyout(zdk1t,zdkt)\n"
            "    do jj = 1, jpj, 1") in gen_code

    assert ("    end if\n"
            "    !$acc end data\n"
            "    !$acc data copyin(e2_e1u,e2u,e3t_n,e3u_n,pahu,r1_e1e2t,"
            "umask,uslp,wmask,zdit,zdk1t,zdkt,zftv) copyout(zftu) copy(pta)\n"
            "    do jj = 1, jpjm1, 1") in gen_code

    assert ("    enddo\n"
            "    !$acc end data\n"
            "  enddo") in gen_code


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
    assert "!$acc data copyin(a) copyout(prof,prof%npind)" in gen_code


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


def test_array_section():
    '''Check code generation with a arrays accessed via an array section.

    '''
    psy, invoke_info = get_invoke("array_section.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    acc_trans.apply(schedule.children)
    gen_code = str(psy.gen).lower()
    assert "!$acc data copyin(b,c) copyout(a)" in gen_code


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


def test_no_copyin_intrinsics(parser):
    ''' Check that we don't generate a copyin/out for Fortran instrinsic
    functions (i.e. we don't mistake them for array accesses). '''
    acc_trans = TransInfo().get_trans_name('ACCDataTrans')
    for intrinsic in ["cos(ji)", "sin(ji)", "tan(ji)", "atan(ji)",
                      "mod(ji, 5)"]:
        reader = FortranStringReader(
            "program call_intrinsic\n"
            "use kind_params_mod\n"
            "integer :: ji, jpj\n"
            "real(kind=wp) :: sto_tmp(5)\n"
            "do ji = 1,jpj\n"
            "sto_tmp(ji) = {0}\n"
            "end do\n"
            "end program call_intrinsic\n".format(intrinsic))
        code = parser(reader)
        psy = PSyFactory(API, distributed_memory=False).create(code)
        schedule = psy.invokes.invoke_list[0].schedule
        acc_trans.apply(schedule.children[0:1])
        gen_code = str(psy.gen)
        idx = intrinsic.index("(")
        assert "copyin({0})".format(intrinsic[0:idx]) not in gen_code.lower()


def test_no_code_blocks(parser):
    ''' Check that we refuse to include CodeBlocks (i.e. code that we
    don't recognise) within a data region. '''
    reader = FortranStringReader("program write_out\n"
                                 " integer :: ji, jpj\n"
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
    # We don't yet support ACCEnterDataTrans for the NEMO API (Issue 310)
    # so manually insert a GOACCEnterDataDirective in the Schedule.
    directive = GOACCEnterDataDirective(children=[])
    schedule.children.insert(0, directive)
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
    ''' Check that we raise the expected error if our code that identifies
    read and write accesses misses an array access. '''
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
