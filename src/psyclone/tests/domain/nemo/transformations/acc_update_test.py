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

import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003

from psyclone.psyGen import PSyFactory
from psyclone.psyir.nodes import CodeBlock
from psyclone.psyir.transformations import TransformationError, ACCUpdateTrans
from psyclone.transformations import ACCEnterDataTrans, ACCKernelsTrans
from psyclone.tests.utilities import Compile

# Constants
API = "nemo"


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
    CALL dia_ptr_hst( jn, 'ldf', zftv(:,:,:)  )
    zftv(:,:,:) = 1.0d0
  END IF
  CALL dia_ptr_hst( jn, 'ldf', zftv(:,:,:)  )
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
            "  !$acc enter data copyin(jpi,tmask,zftu,zftv)\n"
            "  !$acc kernels\n"
            "  zftv(:,:,:) = 0.0d0\n"
            "  !$acc end kernels\n"
            "  !$acc update if_present host(l_ptr)\n"
            "  if (l_ptr) then\n" in gen_code)
    assert ("    zftv(:,:,:) = 1.0d0\n"
            "    !$acc update if_present device(zftv)\n" in gen_code)
    assert ("  !$acc kernels\n"
            "  zftu(:,:,1) = 1.0d0\n"
            "  tmask(:,:) = jpi\n"
            "  !$acc end kernels\n"
            "\n"
            "end subroutine tra_ldf_iso" in gen_code)


def test_nested_acc_in_if(parser):
    ''' Test that the necessary update directives are added when we have
    a kernels region within an IfBlock. '''
    code = '''
SUBROUTINE tra_ldf_iso()
  USE some_mod, only: dia_ptr_hst
  INTEGER, PARAMETER :: jpi=2, jpj=2, jpk=2
  INTEGER :: jn
  LOGICAL :: l_ptr
  INTEGER, DIMENSION(jpi,jpj) :: tmask
  REAL, DIMENSION(jpi,jpj,jpk) :: zdit, zdjt, zftu, zftv, zftw
  zftv(:,:,:) = 0.0d0
  IF( l_ptr )THEN
    CALL dia_ptr_hst( jn, 'ldf', zftv(:,:,:)  )
    zftv(:,:,:) = 1.0d0
    zftw(:,:,:) = -1.0d0
  END IF
  CALL dia_ptr_hst( jn, 'ldf', zftv(:,:,:)  )
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
    acc_kernels.apply(schedule[0])
    acc_kernels.apply(schedule[1].if_body[2])
    acc_kernels.apply(schedule[3])
    acc_trans.apply(schedule)
    acc_update.apply(schedule)
    gen_code = str(psy.gen).lower()
    assert ("  !$acc end kernels\n"
            "  !$acc update if_present host(l_ptr)\n"
            "  if (l_ptr) then\n"
            ) in gen_code
    assert ("    zftv(:,:,:) = 1.0d0\n"
            "    !$acc update if_present device(zftv)\n"
            ) in gen_code
    assert ("  tmask(:,:) = jpi\n"
            "  !$acc update if_present device(tmask)\n"
            ) in gen_code


def test_call_accesses(parser):
    ''' Check that a call does not result in any update directives. '''
    code = '''
SUBROUTINE tra_ldf_iso()
  USE some_mod, only: dia_ptr_hst
  INTEGER, PARAMETER :: jpi=2, jpj=2, jpk=2
  INTEGER :: jn=2
  REAL :: checksum
  REAL, DIMENSION(jpi,jpj,jpk) :: zdit, zdjt, zftv, zftw
  zftv(:,:,:) = 0.0d0
  CALL dia_ptr_hst( jn, 'ldf', zftv(:,:,:)  )
  checksum = SUM(zftv)
  !write(*,*) checksum
end SUBROUTINE tra_ldf_iso
'''
    reader = FortranStringReader(code)
    ast = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(ast)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_update = ACCUpdateTrans()
    acc_update.apply(schedule)
    gen_code = str(psy.gen).lower()
    assert ("  zftv(:,:,:) = 0.0d0\n"
            "  !$acc update if_present device(zftv)\n"
            "  call dia_ptr_hst(jn, 'ldf', zftv(:,:,:))\n"
            # TODO we should be able to infer that the following update is
            # not required because if it *is* updated in the call above then
            # the GPU copy should already be up-to-date.
            "  !$acc update if_present host(zftv)\n"
            "  checksum = sum(zftv)\n" in gen_code)


def test_call_within_if(parser):
    ''' Check that a Call within an If results in the expected update
    directive when one of its arguments is subsequently accessed on
    the CPU. '''
    code = '''
SUBROUTINE tra_ldf_iso()
  USE some_mod, only: dia_ptr_hst
  INTEGER, PARAMETER :: jpi=2, jpj=2, jpk=2
  INTEGER :: jn=2
  REAL :: checksum
  REAL, DIMENSION(jpi,jpj,jpk) :: zdit, zdjt, zftv, zftw
  zftv(:,:,:) = 0.0d0
  if(jn == 1)then
    CALL dia_ptr_hst( jn, 'ldf', zftv(:,:,:)  )
  end if
  checksum = SUM(zftv)
  !write(*,*) checksum
end SUBROUTINE tra_ldf_iso
'''
    reader = FortranStringReader(code)
    ast = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(ast)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_update = ACCUpdateTrans()
    acc_update.apply(schedule)
    gen_code = str(psy.gen).lower()
    print(gen_code)
    assert ("  zftv(:,:,:) = 0.0d0\n"
            "  !$acc update if_present device(zftv)\n" in gen_code)
    # TODO again we should be able to spot that this update is not required
    # as it is only separated from the previous region by a call().
    assert ("  end if\n"
            "  !$acc update if_present host(zftv)\n" in gen_code)


def test_loop_on_cpu(parser):
    ''' Test the application of ACCUpdateTrans to CPU code containing a
    loop which itself contains a Call. '''
    code = '''
SUBROUTINE tra_ldf_iso()
  USE some_mod, only: dia_ptr_hst
  INTEGER, PARAMETER :: jpi=2, jpj=2, jpk=2, start=2, step=2
  INTEGER :: jn, ji
  LOGICAL :: l_ptr
  INTEGER, DIMENSION(jpi,jpj) :: tmask
  REAL, DIMENSION(jpi,jpj,jpk) :: zdit, zdjt, zftu, zftv, zftw
  zftv(:,:,:) = 0.0d0
  DO ji = start, jpi, step
    CALL dia_ptr_hst( ji, 'ldf', zftv(ji,:,:)  )
    zftv(ji,:,:) = 1.0d0
    zftw(ji,:,:) = -1.0d0
  END DO
  zftu(:,:,1) = 1.0d0
  tmask(:,:) = jpi
end SUBROUTINE tra_ldf_iso
'''
    reader = FortranStringReader(code)
    ast = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(ast)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_update = ACCUpdateTrans()
    acc_update.apply(schedule)
    gen_code = str(psy.gen).lower()
    # Loop variable should not be copied to device
    assert "device(ji)" not in gen_code
    # Currently jpi is copied to and fro. We need a list of variables that
    # are written just once and are read-only thereafter. Such variables will
    # be written on the CPU (e.g. after reading from namelist).
    assert ("  zftv(:,:,:) = 0.0d0\n"
            "  !$acc update if_present device(zftv)\n"
            "  !$acc update if_present host(jpi,start,step)\n"
            "  do ji = start, jpi, step\n" in gen_code)
    # TODO All of these variables are actually local to the subroutine so
    # should not be copied back.
    assert ("  zftu(:,:,1) = 1.0d0\n"
            "  tmask(:,:) = jpi\n"
            "  !$acc update if_present device(tmask,zftu)\n"
            in gen_code)


def test_codeblock_stop(parser):
    ''' Test that we insert a STOP if we encounter a CodeBlock that we can't
    reason about. '''
    code = '''
subroutine lbc_update()
  use oce, only: tmask, jpi, jpj, jpk

  open(unit=32, file="some_forcing.dat")
  read(32, *) jpi, jpj, jpk
  allocate(tmask(jpi, jpj))
  read(32, *) tmask

end subroutine lbc_update
'''
    reader = FortranStringReader(code)
    ast = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(ast)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_update = ACCUpdateTrans()
    acc_update.apply(schedule, options={"allow-codeblocks": True})
    assert isinstance(schedule[0], CodeBlock)
    assert isinstance(schedule[1], CodeBlock)
    gen_code = str(psy.gen).lower()
    assert ("  stop 'psyclone: lbc_update: manually add acc update statements "
            "(if required) for the following codeblock...'\n"
            "  open" in gen_code)


def test_codeblock_no_access(parser):
    ''' Check that we permit a CodeBlock if we are confident it doesn't modify
    access data. '''
    code = '''
subroutine lbc_update()
  use oce, only: tmask, jpi, jpj, jpk

  open(unit=32, file="some_forcing.dat")
  write(32, *) "Hello"

end subroutine lbc_update
'''
    reader = FortranStringReader(code)
    ast = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(ast)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_update = ACCUpdateTrans()
    acc_update.apply(schedule, options={"allow-codeblocks": True})
    assert len(schedule.children) == 1
    assert isinstance(schedule[0], CodeBlock)
    assert isinstance(schedule[0].get_ast_nodes[0], Fortran2003.Open_Stmt)
