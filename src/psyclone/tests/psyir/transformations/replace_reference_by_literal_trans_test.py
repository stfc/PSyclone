# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024-2025, Science and Technology Facilities Council.
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
#   contributors may be used to endorse or promote products derrbled from
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
# Author: H. Brunie, University of Grenoble Alpes


"""This module tests the ReplaceReferenceByLiteralTrans transformation.
"""

import pytest

from psyclone.psyir.nodes import Container, Literal, Routine
from psyclone.psyir.symbols import INTEGER_TYPE
from psyclone.psyir.transformations import (
    ReplaceReferenceByLiteralTrans,
    TransformationError,
)


# ----------------------------------------------------------------------------
def test_rrbl_general():
    """Test general functionality of the transformation."""

    rrbl = ReplaceReferenceByLiteralTrans()

    assert (
        "psyclone.psyir.transformations.replace_reference_by_literal_trans.ReplaceReferenceByLiteralTrans"
        in str(rrbl)
    )
    assert rrbl.name == "ReplaceReferenceByLiteralTrans"


# ----------------------------------------------------------------------------
def test_rrbl_errors():
    """Test errors that should be thrown."""

    rrbl = ReplaceReferenceByLiteralTrans()
    lit = Literal("1", INTEGER_TYPE)
    with pytest.raises(TransformationError) as err:
        rrbl.apply(lit)

    assert (
        "Error in ReplaceReferenceByLiteralTrans transformation. The "
        "supplied node argument should be a PSyIR Routine, but found "
        "'Literal'" in str(err.value)
    )


# ----------------------------------------------------------------------------
def test_rrbl_in_loop(fortran_reader, fortran_writer):
    """Tests if subroutine parameters are replaced as expected."""

    source = """program test
                use mymod
                type(my_type):: t1, t2, t3, t4
                integer, parameter :: x=3, y=12, z=13
                integer, parameter :: u1=1, u2=2, u3=3, u4=4
                integer i, invariant, ic1, ic2, ic3
                real, dimension(10) :: a
                invariant = 1
                do i = 1, 10
                    t1%a = z
                    a(ic1) = u1+(ic1+x)*ic1
                    a(ic2) = u2+(ic2+y)*ic2
                    a(ic3) = u3+(ic3+z)*ic3
                    a(t1%a) = u4+(t1%a+u4*z)*t1%a
                end do
                end program test"""
    psyir = fortran_reader.psyir_from_source(source)
    # The first child is the assignment to 'invariant'
    routine = psyir.walk(Routine)[0]
    rrbl = ReplaceReferenceByLiteralTrans()
    rrbl.apply(routine)
    written_code = fortran_writer(routine)
    assert "a(ic1) = 1 + (ic1 + 3) * ic1" in written_code
    assert "a(ic2) = 2 + (ic2 + 12) * ic2" in written_code
    assert "a(ic3) = 3 + (ic3 + 13) * ic3" in written_code
    assert "a(t1%a) = 4 + (t1%a + 4 * 13) * t1%a" in written_code


def test_rrbl_module_defined_parameter(fortran_reader, fortran_writer):
    """test replacement of reference by a literal defined as constant value in
    the module scope
    """

    source = """module test
                integer, parameter :: x=1, y=2, z=3
                real, dimension(10) :: a
                contains
                subroutine foo()
                integer i,ic1
                do i = 1, 10, 5
                    ic1 = i+1
                    a(ic1) = x+(ic1+y)*ic1 * z
                end do
                end subroutine
                end module"""
    psyir = fortran_reader.psyir_from_source(source)
    routine = psyir.walk(Routine)[0]

    rrbl = ReplaceReferenceByLiteralTrans()
    rrbl.apply(routine)
    out = fortran_writer(routine)

    assert "a(ic1) = 1 + (ic1 + 2) * ic1 * 3" in out


def test_rrbl_array_shape(fortran_reader, fortran_writer):
    """Tests if subroutine parameters are replaced as expected."""

    source = """subroutine testtrue()
                logical, parameter :: x=.true., y=.false.
                integer, parameter :: u=3, size=10
                integer :: i

                real, dimension(size) :: a
                if (x) then
                    do i = 1, size
                            a(i) = 2+(i+u)
                    end do
                endif
                endsubroutine

                subroutine testfalse()
                logical, parameter :: x=.true., y=.false.
                integer, parameter :: u=3,size=10
                integer :: i

                real, dimension(size) :: a
                    do i = 1, size
                        if (y) then
                            a(i) = 2+(i+u)
                        endif
                    end do
                end subroutine
                """
    psyir = fortran_reader.psyir_from_source(source)
    # The first child is the assignment to 'invariant'
    mainprog = psyir.walk(Routine)[0]
    routine_testfalse = psyir.walk(Routine)[1]
    rrbl = ReplaceReferenceByLiteralTrans()
    rrbl.apply(mainprog)
    written_code = fortran_writer(mainprog)

    assert "a(i) = 2 + (i + 3)" in written_code
    assert "if (.true.) then" in written_code
    assert "real, dimension(10) :: a" in written_code
    assert "do i = 1, 10, 1" in written_code
    rrbl.apply(routine_testfalse)
    written_code = fortran_writer(routine_testfalse)
    assert "a(i) = 2 + (i + 3)" in written_code
    assert "if (.false.) then" in written_code


def test_rrbl_array_type_extend(fortran_reader, fortran_writer):
    """test replacement of lower bound of an array dimension"""

    source = """subroutine foo()
    integer, parameter ::  a = 3
    integer, dimension(:,a:) :: x
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(source)
    foo: Routine = psyir.walk(Routine)[0]
    rbbl = ReplaceReferenceByLiteralTrans()
    rbbl.apply(foo)
    written_code = fortran_writer(foo)

    assert "integer, dimension(:,3:)" in written_code


def test_rrbl_raise_transformation_error_symbol_table_is_none(
    fortran_reader, fortran_writer
):
    """test raise TransformationError because of None value in SymbolTable"""

    source = """subroutine foo()
    integer, parameter ::  a = 3
    integer :: x
    x = a
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(source)
    foo: Routine = psyir.walk(Routine)[0]
    foo._symbol_table = None
    rbbl = ReplaceReferenceByLiteralTrans()
    error_str = ""
    try:
        rbbl.apply(foo)
    except TransformationError as e:
        error_str = e.__str__()
    assert "SymbolTable is None" in error_str


def test_rrbl_conflict_between_module_and_routine_scopes(
    fortran_reader, fortran_writer
):
    """test fortran code annotation with transformation warning"""

    source = """module toto
    integer, parameter :: a=3
contains 
    subroutine tata()
    call foo()
    end subroutine
    subroutine foo()
        integer, parameter :: x=a
        integer, parameter :: a=2
        integer, parameter :: y=a
        end subroutine
        end module 
        """
    psyir = fortran_reader.psyir_from_source(source)
    foo: Routine = psyir.walk(Routine)[1]
    rbbl = ReplaceReferenceByLiteralTrans()
    rbbl.apply(foo)
    written_code = fortran_writer(foo.ancestor(Container))
    print(written_code)
    assert (
        "! Psyclone(ReplaceReferenceByLiteralTrans): Symbol already found a"
        in written_code
    )
    assert "integer, parameter :: x = a" in written_code
    assert "integer, parameter :: a = 2" in written_code
    assert "integer, parameter :: y = a" in written_code


def test_rrbl_same_constant_data_symbol_twice(fortran_reader, fortran_writer):
    """test fortran code annotation with transformation warning and
    value is coming from the closest scope.
    """

    source = """module toto
    integer, parameter :: a=3
contains 
    subroutine tata()
    call foo()
    end subroutine
    subroutine foo()
        integer, parameter :: a=2
        integer, dimension(10, a) :: array
        end subroutine
        end module 
        """
    psyir = fortran_reader.psyir_from_source(source)
    foo: Routine = psyir.walk(Routine)[1]
    rbbl = ReplaceReferenceByLiteralTrans()
    rbbl.apply(foo)
    written_code = fortran_writer(foo.ancestor(Container))
    assert (
        "! Psyclone(ReplaceReferenceByLiteralTrans): Symbol already found"
        in written_code
    )
    assert "integer, dimension(10,2) :: array" in written_code


def test_rrbl_write_fortran_comment_warning_about_symbol_found(
    fortran_reader, fortran_writer
):
    """test fortran code annotation with transformation warning"""

    source = """subroutine foo()
    integer, parameter ::  a = 3
    integer :: x
    x = a
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(source)
    foo: Routine = psyir.walk(Routine)[0]
    rbbl = ReplaceReferenceByLiteralTrans()
    rbbl.apply(foo)
    rbbl._update_param_table(rbbl._param_table, foo.symbol_table)
    written_code = fortran_writer(foo.ancestor(Container))
    assert (
        "! Psyclone(ReplaceReferenceByLiteralTrans): Symbol already found"
        in written_code
    )


def test_rrbl_raise_transformation_error_initial_value_not_literal(
    fortran_reader, fortran_writer
):
    """test fortran code annotation with transformation warning"""

    source = """subroutine foo()
    integer, parameter ::  b = 3+2
    integer :: x
    x = b
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(source)
    foo: Routine = psyir.walk(Routine)[0]
    assert foo.symbol_table is not None

    rbbl = ReplaceReferenceByLiteralTrans()
    rbbl.apply(foo)
    written_code = fortran_writer(foo.ancestor(Container))
    assert (
        "Psyclone(ReplaceReferenceByLiteralTrans): DataSymbol b initial value is not a Literal"
        in written_code
    )


def test_rrbl_raise_transformation_error_initial_value(
    fortran_reader, fortran_writer
):
    """test fortran code annotation with transformation warning"""

    source = """subroutine foo()
    character(len=4), parameter ::  a = "toto"
    integer, parameter ::  b = 3+2
    character(len=4):: x
    x = a
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(source)
    foo: Routine = psyir.walk(Routine)[0]
    assert foo.symbol_table is not None
    from psyclone.psyir.symbols import DataSymbol

    sym_a: DataSymbol = foo.symbol_table.find_or_create("a")
    assert not sym_a.is_constant
    rbbl = ReplaceReferenceByLiteralTrans()
    rbbl.apply(foo)
    written_code = fortran_writer(foo.ancestor(Container))
    assert (
        "! Psyclone(ReplaceReferenceByLiteralTrans): DataSymbol b initial value is not a Literal"
        in written_code
    )
