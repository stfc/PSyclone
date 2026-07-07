# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, Science and Technology Facilities Council.
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
# Author: A. B. G. Chalk, STFC Daresbury Lab

'''Contains the tests for the OMPCPURoutineTrans metatransformation.'''

from psyclone.psyir.transformations.metatransformations import (
        OMPCPURoutineTrans
)


def test_ompcpuroutinetrans_validate():
    '''
    Test the OMPCPURoutineTrans validate works correctly when
    provided an options dict.
    '''
    # If we provide an options dict we shouldn't get a failure due
    # to the new code in psyGen, since its not included in
    # inherited valid kwargs.
    OMPCPURoutineTrans().validate(None, options={})


def test_ompcpuroutinetrans_apply_without_nowait(fortran_reader,
                                                 fortran_writer):
    '''Test the behaviour of the OMPCPURoutineTrans works correctly
    when nowait isn't supplied.'''

    code = """subroutine x
    integer :: i
    integer, dimension(100) :: a, b
    do i = 1, 100
        a(i) = i
    end do

    do i = 1, 100
        b(i) = a(i) * 4 + i
    end do
    end subroutine x"""

    psyir = fortran_reader.psyir_from_source(code)

    trans = OMPCPURoutineTrans()

    trans.apply(psyir.children[0])

    out = fortran_writer(psyir)
    correct = """!$omp parallel default(shared) private(i)
  !$omp do schedule(auto)
  do i = 1, 100, 1
    a(i) = i
  enddo
  !$omp end do
  !$omp do schedule(auto)
  do i = 1, 100, 1
    b(i) = a(i) * 4 + i
  enddo
  !$omp end do
  !$omp end parallel"""
    assert correct in out


def test_ompcpuroutinetrans_apply_with_nowait(fortran_reader,
                                              fortran_writer):
    '''Test the behaviour of the OMPCPURoutineTrans works correctly
    when nowait is supplied.'''

    code = """subroutine x
    integer :: i
    integer, dimension(100) :: a, b
    do i = 1, 100
        a(i) = i
    end do

    do i = 1, 100
        b(i) = 4 + i
    end do
    end subroutine x"""

    psyir = fortran_reader.psyir_from_source(code)

    trans = OMPCPURoutineTrans()

    trans.apply(psyir.children[0], nowait=True)

    out = fortran_writer(psyir)
    correct = """!$omp parallel default(shared) private(i)
  !$omp do schedule(auto)
  do i = 1, 100, 1
    a(i) = i
  enddo
  !$omp end do nowait
  !$omp do schedule(auto)
  do i = 1, 100, 1
    b(i) = 4 + i
  enddo
  !$omp end do nowait
  !$omp end parallel"""
    assert correct in out
