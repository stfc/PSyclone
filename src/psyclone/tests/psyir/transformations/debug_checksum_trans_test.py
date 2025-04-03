# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council.
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
# Author A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the tests for the DebugChecksumTrans.'''

from psyclone.psyir.transformations import DebugChecksumTrans


def test_checksum(fortran_reader, fortran_writer):
    ''' Test the behaviour of the debug_checksum_trans'''

    code = """
    subroutine test
        integer, dimension(1:100) :: a, b, c
        logical, dimension(1:100) :: f
        integer :: i
        integer :: d

        do i = 1, 100
            a(i) = c(i) + d
            b(i) = 2 * i
            f(i) = .true.
        enddo
    end subroutine
    """

    psyir = fortran_reader.psyir_from_source(code)

    DebugChecksumTrans().apply(psyir.children[0].children[0])

    correct = """subroutine test()
  integer, dimension(100) :: a
  integer, dimension(100) :: b
  integer, dimension(100) :: c
  logical, dimension(100) :: f
  integer :: i
  integer :: d
  integer :: PSYCLONE_INTERNAL_line_

  do i = 1, 100, 1
    a(i) = c(i) + d
    b(i) = 2 * i
    f(i) = .true.
  enddo
  PSYCLONE_INTERNAL_line_ = __LINE__
  PRINT *, "checksums from test at line:", PSYCLONE_INTERNAL_line_ + 1
  PRINT *, "b checksum", SUM(b)
  PRINT *, "a checksum", SUM(a)

end subroutine test
"""
    out = fortran_writer(psyir)
    assert out == correct

    # Check the checksums are in the right place if there's no hierarchy of
    # nodes.
    code = """
    subroutine test
        integer, dimension(1:100) :: a, b
        a(:) = 1
        b(:) = 2
    end subroutine
    """
    psyir = fortran_reader.psyir_from_source(code)

    DebugChecksumTrans().apply(psyir.children[0].children[:])

    correct = """subroutine test()
  integer, dimension(100) :: a
  integer, dimension(100) :: b
  integer :: PSYCLONE_INTERNAL_line_

  a(:) = 1
  b(:) = 2
  PSYCLONE_INTERNAL_line_ = __LINE__
  PRINT *, "checksums from test at line:", PSYCLONE_INTERNAL_line_ + 1
  PRINT *, "b checksum", SUM(b)
  PRINT *, "a checksum", SUM(a)

end subroutine test
"""
    out = fortran_writer(psyir)
    assert correct == out
