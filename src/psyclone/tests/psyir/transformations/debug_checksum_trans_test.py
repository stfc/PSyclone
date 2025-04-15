# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025, Science and Technology Facilities Council.
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

from psyclone.psyir.nodes import Routine
from psyclone.psyir.transformations import DebugChecksumTrans
from psyclone.tests.utilities import Compile


def test_checksum(fortran_reader, fortran_writer, tmpdir):
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

    out = fortran_writer(psyir)
    correct = "integer :: PSYCLONE_INTERNAL_line_"
    assert correct in out
    correct = """  enddo
  PSYCLONE_INTERNAL_line_ = __LINE__
  ! PSyclone DebugChecksumTrans-generated checksums
  PRINT *, "PSyclone checksums from test at line:", PSYCLONE_INTERNAL_line_ + 1
  PRINT *, "b checksum", SUM(b(:))
  PRINT *, "a checksum", SUM(a(:))"""
    assert correct in out
    assert Compile(tmpdir).string_compiles(out)

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
    out = fortran_writer(psyir)

    correct = "integer :: PSYCLONE_INTERNAL_line_"
    assert correct in out
    correct = """  b(:) = 2
  PSYCLONE_INTERNAL_line_ = __LINE__
  ! PSyclone DebugChecksumTrans-generated checksums
  PRINT *, "PSyclone checksums from test at line:", PSYCLONE_INTERNAL_line_ + 1
  PRINT *, "b checksum", SUM(b(:))
  PRINT *, "a checksum", SUM(a(:))

end subroutine test
"""
    assert correct in out
    assert Compile(tmpdir).string_compiles(out)

    # Check non-checksum types are excluded.
    code = """
    module my_mod
        type :: superval
            integer :: j(100)
        end type
        type :: vals
          integer :: i(100)
          real :: x(100)
          character :: b(100)
          type(superval) :: s
        end type
        contains
        subroutine test_sub
            type(vals) :: values
            type(vals) :: values2(2)
            character, dimension(100) :: char_array
            logical, dimension(100) :: logicals
            values%i(:) = 1
            values%x(:) = 2.0
            values%s%j(100) = 1
            values2(1)%i(:) = 1
            values%b(:) = "a"
            logicals(:) = .true.
            char_array(:) = "b"
        end subroutine
    end module
    """
    psyir = fortran_reader.psyir_from_source(code)
    DebugChecksumTrans().apply(psyir.walk(Routine)[0].children[:])
    out = fortran_writer(psyir)
    correct = """char_array(:) = 'b'
    PSYCLONE_INTERNAL_line_ = __LINE__
    ! PSyclone DebugChecksumTrans-generated checksums
    PRINT *, "PSyclone checksums from test_sub at line:", \
PSYCLONE_INTERNAL_line_ + 1
    PRINT *, "values%s%j checksum", SUM(values % s % j(1 : 100))
    PRINT *, "values%x checksum", SUM(values % x(1 : 100))
    PRINT *, "values%i checksum", SUM(values % i(1 : 100))"""
    assert correct in out

    # Check Unknown types are exlcuded
    code = """subroutine test()
    use my_mod

    something(:) = 1
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    DebugChecksumTrans().apply(psyir.walk(Routine)[0].children[:])
    out = fortran_writer(psyir)
    assert "! PSyclone DebugChecksumTrans-generated checksums" not in out

    # Check we place checksums in the right place when not all statements
    # are included
    code = """subroutine test()
    integer :: a(100), b(100)
    integer :: i

    do i = 1, 100
       a(i) = i
    end do
    b(:) = 0
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    DebugChecksumTrans().apply(psyir.walk(Routine)[0].children[0])
    out = fortran_writer(psyir)
    correct = """enddo
  PSYCLONE_INTERNAL_line_ = __LINE__
  ! PSyclone DebugChecksumTrans-generated checksums
  PRINT *, "PSyclone checksums from test at line:", PSYCLONE_INTERNAL_line_ + 1
  PRINT *, "a checksum", SUM(a(:))
  b(:) = 0"""
    assert correct in out
