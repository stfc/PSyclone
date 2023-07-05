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
# Author A. B. G. Chalk, STFC Daresbury Laboratory

''' Module containing pytest tests for the handling of some select case
construction for the Fparser->PSyIR frontend.'''

from __future__ import absolute_import

from psyclone.psyir.nodes import CodeBlock


def test_logical_literal_case(fortran_reader, fortran_writer):
    '''Test that a select case statement comparing to logical literals
    results in if statements with .eqv. checks.'''
    code = '''subroutine test_subroutine(a)
    logical :: a

    SELECT CASE(A)
    CASE(.FALSE.)
        print *, "Not hello"
    CASE(.TRUE.)
        print *, "hello"
    END SELECT
  end subroutine'''

    psyir = fortran_reader.psyir_from_source(code)
    pass_through = fortran_writer(psyir)
    assert "a == .true." not in pass_through
    assert "a .EQV. .true." in pass_through

    # Test with an unknown comparison to a literal.
    code = '''subroutine test_subroutine()
    use mymod, only: a

    SELECT CASE(A)
    CASE(.FALSE.)
        print *, "Not hello"
    CASE(.TRUE.)
        print *, "hello"
    END SELECT
  end subroutine'''

    psyir = fortran_reader.psyir_from_source(code)
    pass_through = fortran_writer(psyir)
    assert "a == .true." not in pass_through
    assert "a .EQV. .true." in pass_through


def test_logical_reference_case(fortran_reader, fortran_writer):
    '''Test that a select case statement comparing to logical references
    results in if statements with .eqv. checks.'''
    code = '''subroutine test_subroutine(a, b, c)
    logical :: a, b, c

    SELECT CASE(A)
    CASE(b)
        print *, "Not hello"
    CASE(c)
        print *, "hello"
    END SELECT
  end subroutine'''

    psyir = fortran_reader.psyir_from_source(code)
    pass_through = fortran_writer(psyir)
    assert "a == b" not in pass_through
    assert "a .EQV. b" in pass_through
    assert "a .EQV. c" in pass_through


def test_nonlogical_literal_case(fortran_reader, fortran_writer):
    '''Test that a select case statement comparing to non-logical literals
    results in if statements with == checks.'''
    code = '''subroutine test_subroutine(a)
    integer :: a

    SELECT CASE(A)
    CASE(1)
        print *, "Not hello"
    CASE(2)
        print *, "hello"
    END SELECT
  end subroutine'''

    psyir = fortran_reader.psyir_from_source(code)
    pass_through = fortran_writer(psyir)
    assert "a == 1" in pass_through
    assert "a == 2" in pass_through


def test_nonlogical_reference_case(fortran_reader, fortran_writer):
    '''Test that a select case statement comparing to non-logical references
    results in if statements with == checks.'''
    code = '''subroutine test_subroutine(a, b, c)
    integer :: a, b, c

    SELECT CASE(A)
    CASE(b)
        print *, "Not hello"
    CASE(c)
        print *, "hello"
    END SELECT
  end subroutine'''

    psyir = fortran_reader.psyir_from_source(code)
    pass_through = fortran_writer(psyir)
    assert "a == b" in pass_through
    assert "a == c" in pass_through


def test_expression_case(fortran_reader):
    '''Test that a select case statement comparing two expressions
    results in a code block.'''
    code = '''subroutine test_subroutine(a, b, c)
    integer :: a, b, c

    SELECT CASE(a*a)
    CASE(b-c)
        print *, "Not hello"
    CASE(c-b)
        print *, "hello"
    END SELECT
  end subroutine'''

    psyir = fortran_reader.psyir_from_source(code)
    assert isinstance(psyir.children[0].children[0], CodeBlock)


def test_unknown_types_case(fortran_reader):
    '''Test that a seclect case statement comapring two unknown types
    results in a code block.'''
    code = '''subroutine test_subroutine()
    use my_mod, only : a, b, c

    SELECT CASE(a)
    CASE(b)
        print *, "Not hello"
    CASE(c)
        print *, "hello"
    END SELECT
  end subroutine'''

    psyir = fortran_reader.psyir_from_source(code)
    assert isinstance(psyir.children[0].children[0], CodeBlock)
