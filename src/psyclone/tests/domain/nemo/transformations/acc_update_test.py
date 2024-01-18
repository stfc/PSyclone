# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab

'''Module containing py.test tests for the transformation of the PSy
   representation of NEMO code using OpenACC update directives.

'''

import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003

from psyclone.psyGen import PSyFactory
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import (
    ACCKernelsDirective, CodeBlock, Routine, Schedule, Call, IntrinsicCall,
    Reference, Node)
from psyclone.psyir.symbols import RoutineSymbol, DataSymbol, REAL_TYPE
from psyclone.psyir.transformations import TransformationError, ACCUpdateTrans
from psyclone.transformations import ACCEnterDataTrans, ACCKernelsTrans

# Constants
API = "nemo"


def test_validate():
    ''' Check that the validate() method works as expected. '''
    trans = ACCUpdateTrans()

    with pytest.raises(TransformationError) as err:
        trans.validate(ACCKernelsDirective())
    assert ("Expected a Schedule but got a node of type 'ACCKernelsDirective'"
            in str(err.value))

    with pytest.raises(TransformationError) as err:
        trans.validate(Schedule(parent=ACCKernelsDirective()))
    assert ("Cannot apply the ACCUpdateTrans to nodes that are "
            "within an OpenACC compute region." in str(err.value))


def test_may_compute():
    ''' Check that the _may_compute method works as expected. '''
    node = Call.create(RoutineSymbol("hello"), [])
    assert ACCUpdateTrans._may_compute(node)
    node = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.ALLOCATE,
        [Reference(DataSymbol("x", REAL_TYPE))])
    assert ACCUpdateTrans._may_compute(node)
    node = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.DEALLOCATE,
        [Reference(DataSymbol("x", REAL_TYPE))])
    assert ACCUpdateTrans._may_compute(node)
    assert not ACCUpdateTrans._may_compute(Node())
    node = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.SUM,
        [Reference(DataSymbol("x", REAL_TYPE))])
    assert not ACCUpdateTrans._may_compute(node)


def test_simple_missed_region(parser, fortran_writer):
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
    # We do not permit arbitrary code blocks to be included in data regions so
    # just put three of the (implicit) loops into kernels.
    acc_kernels.apply(schedule[0])
    acc_kernels.apply(schedule[3:5])
    acc_trans.apply(schedule)
    acc_update.apply(schedule)
    code = fortran_writer(schedule)
    assert ("  real, dimension(jpi,jpj,jpk) :: ztfw\n"
            "\n"
            "  !$acc update if_present host(jn,l_ptr)\n"
            "  !$acc enter data copyin(jpi,tmask,zftu,zftv)\n"
            "  !$acc kernels\n"
            "  zftv(:,:,:) = 0.0d0\n"
            "  !$acc end kernels\n"
            "  !$acc update if_present host(zftv)\n"
            "  if (l_ptr) then\n" in code)
    assert ("    call dia_ptr_hst(jn, 'ldf', zftv(:,:,:))\n"
            "    !$acc update if_present host(zftv)\n"
            "    zftv(:,:,:) = 1.0d0\n"
            "    !$acc update if_present device(jn,zftv)\n" in code)
    assert ("  !$acc update if_present host(jn,zftv)\n"
            "  call" in code)


def test_nested_acc_in_if(parser, fortran_writer):
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
  ELSE
    CALL dia_ptr_hst( jn, 'ldf', zftv(:,:,:)  )
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
    code = fortran_writer(schedule)
    assert ("  !$acc update if_present host(jn,l_ptr)\n"
            "  !$acc enter data"
            ) in code
    assert ("  !$acc update if_present host(zftv)\n"
            "  if"
            ) in code
    assert ("    !$acc update if_present host(zftv)\n"
            "    zftv(:,:,:) = 1.0d0\n"
            ) in code
    assert ("    !$acc update if_present device(jn,zftv)\n"
            "  else\n"
            ) in code
    assert ("  !$acc update if_present host(jn,zftv)\n"
            "  call"
            ) in code
    assert ("  !$acc update if_present host(jpi,tmask)\n"
            "  tmask(:,:) = jpi\n"
            "  !$acc update if_present device(jn,tmask,zftv)\n"
            ) in code


def test_call_accesses(fortran_writer):
    ''' Check that a call results in extra update directives, bar relocation,
    and that there are no redundant consecutive update directives. '''
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
end SUBROUTINE tra_ldf_iso
'''
    psyir = FortranReader().psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    acc_update = ACCUpdateTrans()
    acc_update.apply(routine)
    code = fortran_writer(routine)
    assert ("  !$acc update if_present host(jn,zftv)\n"
            "  zftv(:,:,:) = 0.0d0\n"
            "  !$acc update if_present device(zftv)\n"
            "  call dia_ptr_hst(jn, 'ldf', zftv(:,:,:))\n"
            "  !$acc update if_present host(checksum,zftv)\n"
            "  checksum = SUM(zftv)\n"
            "  !$acc update if_present device(checksum,jn,zftv)\n" in code)


def test_call_within_if(parser, fortran_writer):
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
end SUBROUTINE tra_ldf_iso
'''
    reader = FortranStringReader(code)
    ast = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(ast)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_update = ACCUpdateTrans()
    acc_update.apply(schedule)
    code = fortran_writer(schedule)
    assert ("  !$acc update if_present host(jn,zftv)\n"
            "  zftv(:,:,:) = 0.0d0\n"
            "  !$acc update if_present device(zftv)\n" in code)
    assert ("  end if\n"
            "  !$acc update if_present host(checksum,zftv)\n"
            "  checksum = SUM(zftv)\n"
            "  !$acc update if_present device(checksum)\n" in code)


def test_loop_on_cpu(parser, fortran_writer):
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
    code = fortran_writer(schedule)
    # Loop variable should not be copied to device
    assert "device(ji)" not in code
    # TODO #1872: Currently jpi is copied to and from. We need a list of
    # variables that are written just once and are read-only thereafter. Such
    # variables will be written on the CPU (e.g. after reading from namelist).
    assert ("  !$acc update if_present host(jpi,start,step,zftv)\n"
            "  zftv(:,:,:) = 0.0d0\n"
            "  !$acc update if_present device(zftv)\n"
            "  do ji = start, jpi, step\n") in code
    assert ("    !$acc update if_present host(zftv)\n"
            "    call") in code
    assert ("    !$acc update if_present host(zftv,zftw)\n"
            "    zftv(ji,:,:) = 1.0d0\n"
            "    zftw(ji,:,:) = -1.0d0\n"
            "    !$acc update if_present device(zftv,zftw)\n") in code
    # TODO #1872: All of these variables are actually local to the subroutine
    # so should not be copied back to the device.
    assert ("  !$acc update if_present host(jpi,tmask,zftu)\n"
            "  zftu(:,:,1) = 1.0d0\n"
            "  tmask(:,:) = jpi\n"
            "  !$acc update if_present device(tmask,zftu)\n"
            in code)


def test_codeblock(parser, fortran_writer):
    ''' Test that we can reason about CodeBlocks.'''
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
    acc_update.apply(schedule)
    assert isinstance(schedule[1], CodeBlock)
    code = fortran_writer(schedule)
    assert ('''  !$acc update if_present host(jpi,jpj,jpk,tmask)\n'''
            '''  OPEN(UNIT = 32, FILE = "some_forcing.dat")''' in code)
    assert ("  READ(32, *) tmask\n"
            "  !$acc update if_present device(tmask)" in code)


def test_codeblock_no_access(parser):
    ''' Check that we also permit a CodeBlock if it doesn't access data. '''
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
    acc_update.apply(schedule)
    assert len(schedule.children) == 1
    assert isinstance(schedule[0], CodeBlock)
    assert isinstance(schedule[0].get_ast_nodes[0], Fortran2003.Open_Stmt)


def test_loop_host_overwriting(parser, fortran_writer):
    ''' Test the placement of update directives when a loop contains a host
    statement writing the same variable as a host statement outside the loop.
    The host statememnt inside the loop is placed before a kernel writing a
    variable said statement uses as input. '''
    code = '''
SUBROUTINE tra_ldf_iso()
  INTEGER, PARAMETER :: jpi=2, jpj=2, jpk=2, start=2, step=2
  INTEGER :: ji
  REAL, DIMENSION(jpi,jpj,jpk) :: zftv, zftw
  zftv(:,:,:) = 0.0d0
  DO ji = start, jpi, step
    zftv(ji,:,:) = zftw(ji,:,:)
    zftw(ji,:,:) = -1.0d0
  END DO
end SUBROUTINE tra_ldf_iso
'''
    reader = FortranStringReader(code)
    ast = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(ast)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_enter = ACCEnterDataTrans()
    acc_update = ACCUpdateTrans()
    acc_kernels = ACCKernelsTrans()
    acc_kernels.apply(schedule[1].loop_body[1])
    acc_enter.apply(schedule)
    acc_update.apply(schedule)
    code = fortran_writer(schedule)
    # Loop variable should not be copied to device
    assert "device(ji)" not in code
    assert ("  !$acc update if_present host(jpi,start,step,zftv)\n"
            "  zftv(:,:,:) = 0.0d0\n"
            "  !$acc enter data copyin(zftw)\n"
            "  do ji = start, jpi, step\n"
            "    !$acc update if_present host(zftw)\n"
            "    zftv(ji,:,:) = zftw(ji,:,:)\n"
            "    !$acc kernels\n"
            "    zftw(ji,:,:) = -1.0d0\n"
            "    !$acc end kernels\n"
            "  enddo\n"
            "  !$acc update if_present device(zftv)\n" in code)


def test_if_host_overwriting(parser, fortran_writer):
    ''' Test the placement of update host directives when an IfBlock contains
     a host statement writing the same variable as a previous host statement
     outside the IfBlock with no depedent device kernel in between them. '''
    code = '''
SUBROUTINE tra_ldf_iso()
  INTEGER, PARAMETER :: jpi=2, jpj=2, jpk=2
  LOGICAL :: l_ptr
  REAL, DIMENSION(jpi,jpj,jpk) :: zftv, zftw
  zftv(:,:,:) = 0.0d0
  IF( l_ptr )THEN
    zftw(:,:,:) = -1.0d0
    zftv(1,:,:) = 1.0d0
  END IF
end SUBROUTINE tra_ldf_iso
'''
    reader = FortranStringReader(code)
    ast = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(ast)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_update = ACCUpdateTrans()
    acc_kernels = ACCKernelsTrans()
    acc_kernels.apply(schedule[1].if_body[0])
    acc_update.apply(schedule)
    code = fortran_writer(schedule)
    assert ("  !$acc update if_present host(l_ptr,zftv)\n"
            "  zftv(:,:,:) = 0.0d0\n"
            "  if (l_ptr) then\n"
            "    !$acc kernels\n"
            "    zftw(:,:,:) = -1.0d0\n"
            "    !$acc end kernels\n"
            "    zftv(1,:,:) = 1.0d0\n"
            "  end if\n"
            "  !$acc update if_present device(zftv)") in code


def test_if_update_device(parser, fortran_writer):
    ''' Test the placement of update device directives when an IfBlock contains
     a host statement writing the same variable as a preceding kernel. '''
    code = '''
SUBROUTINE tra_ldf_iso()
  INTEGER, PARAMETER :: jpi=2, jpj=2, jpk=2
  LOGICAL :: l_ptr
  REAL, DIMENSION(jpi,jpj,jpk) :: zftv
  IF( l_ptr )THEN
    zftv(:,:,:) = 0.0d0
    zftv(1,:,:) = 1.0d0
  END IF
end SUBROUTINE tra_ldf_iso
'''
    reader = FortranStringReader(code)
    ast = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(ast)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_update = ACCUpdateTrans()
    acc_kernels = ACCKernelsTrans()
    acc_kernels.apply(schedule[0].if_body[0])
    acc_update.apply(schedule)
    code = fortran_writer(schedule)
    assert ("  if (l_ptr) then\n"
            "    !$acc kernels\n"
            "    zftv(:,:,:) = 0.0d0\n"
            "    !$acc end kernels\n"
            "    !$acc update if_present host(zftv)\n"
            "    zftv(1,:,:) = 1.0d0\n"
            "    !$acc update if_present device(zftv)\n"
            "  end if\n") in code
