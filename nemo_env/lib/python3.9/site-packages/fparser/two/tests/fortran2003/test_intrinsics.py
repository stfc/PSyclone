# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2023, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
#
# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------

"""Test that Fortran 2003 intrinsic functions are parsed correctly."""

import pytest
from fparser.two.Fortran2003 import (
    Program,
    Intrinsic_Function_Reference,
    Intrinsic_Name,
)
from fparser.two.symbol_table import SYMBOL_TABLES
from fparser.two.utils import (
    FortranSyntaxError,
    NoMatchError,
    InternalSyntaxError,
    walk,
)
from fparser.api import get_reader

# class program


@pytest.mark.usefixtures("f2003_create")
def test_intrinsic_recognised():
    """Test that an intrinsic is picked up when used in a program."""

    reader = get_reader("subroutine sub()\na = sin(b)\nend subroutine sub\n")
    ast = Program(reader)
    assert walk(ast, Intrinsic_Function_Reference)


@pytest.mark.usefixtures("f2003_create")
def test_intrinsic_error():
    """Test that Program raises the expected exception when there is an
    intrinsic syntax error.

    """
    reader = get_reader("subroutine sub()\na = sin(b,c)\nend subroutine sub\n")
    with pytest.raises(FortranSyntaxError) as excinfo:
        _ = Program(reader)
    assert (
        "at line 2\n" ">>>a = sin(b,c)\n" "Intrinsic 'SIN' expects 1 arg(s) but found 2"
    ) in str(excinfo.value)


# class intrinsic_name


@pytest.mark.usefixtures("f2003_create")
def test_intrinsic_name_generic():
    """Test that class Intrinsic_Name correctly matches a generic name."""
    result = Intrinsic_Name("COS")
    assert isinstance(result, Intrinsic_Name)
    assert str(result) == "COS"


@pytest.mark.usefixtures("f2003_create")
def test_intrinsic_name_specific():
    """Test that class Intrinsic_Name correctly matches a specific name."""
    result = Intrinsic_Name("CCOS")
    assert isinstance(result, Intrinsic_Name)
    assert str(result) == "CCOS"


@pytest.mark.usefixtures("f2003_create")
def test_intrinsic_name_invalid():
    """Test that class Intrinsic_Name raises the expected exception if an
    invalid intrinsic name is provided.

    """
    with pytest.raises(NoMatchError):
        _ = Intrinsic_Name("NOT_AN_INTRINSIC")


@pytest.mark.usefixtures("f2003_create")
def test_intrinsic_name_case_insensitive():
    """Test that class Intrinsic_Name is a case insensitive match which
    returns the name in upper case.

    """
    result = Intrinsic_Name("CcoS")
    assert isinstance(result, Intrinsic_Name)
    assert str(result) == "CCOS"


# class intrinsic_function_reference


@pytest.mark.usefixtures("f2003_create")
def test_intrinsic_function_reference_generic():
    """Test that class Intrinsic_Function_Reference correctly matches a
    generic intrinsic with a valid number of arguments. We test both
    with and without the existance of a symbol table.

    """
    result = Intrinsic_Function_Reference("SIN(A)")
    assert isinstance(result, Intrinsic_Function_Reference)
    assert str(result) == "SIN(A)"
    # Repeat when there is a scoping region.
    SYMBOL_TABLES.enter_scope("test_scope")
    result = Intrinsic_Function_Reference("SIN(A)")
    assert isinstance(result, Intrinsic_Function_Reference)
    assert str(result) == "SIN(A)"
    table = SYMBOL_TABLES.current_scope
    assert "sin" not in table._data_symbols
    SYMBOL_TABLES.exit_scope()


@pytest.mark.usefixtures("f2003_create")
def test_intrinsic_function_reference():
    """Test that class Intrinsic_Function_Reference correctly matches a
    specific intrinsic with a valid number of arguments.

    """
    result = Intrinsic_Function_Reference("DSIN(A)")
    assert isinstance(result, Intrinsic_Function_Reference)
    assert str(result) == "DSIN(A)"


@pytest.mark.usefixtures("f2003_create")
def test_intrinsic_function_nomatch():
    """Test that class Intrinsic_Function_Reference raises the expected
    exception if there is no match.

    """
    with pytest.raises(NoMatchError):
        _ = Intrinsic_Function_Reference("NO_MATCH(A)")


@pytest.mark.usefixtures("f2003_create")
def test_intrinsic_function_reference_multi_args():
    """Test that class Intrinsic_Function_Reference correctly matches a
    generic intrinsic which accepts more than one argument (two in
    this case).

    """
    result = Intrinsic_Function_Reference("MATMUL(A,B)")
    assert isinstance(result, Intrinsic_Function_Reference)
    assert str(result) == "MATMUL(A, B)"


@pytest.mark.usefixtures("f2003_create")
def test_intrinsic_function_reference_zero_args():
    """Test that class Intrinsic_Function_Reference correctly matches a
    generic intrinsic which accepts zero arguments.

    """
    result = Intrinsic_Function_Reference("COMMAND_ARGUMENT_COUNT()")
    assert isinstance(result, Intrinsic_Function_Reference)
    assert str(result) == "COMMAND_ARGUMENT_COUNT()"


@pytest.mark.usefixtures("f2003_create")
def test_intrinsic_function_reference_range_args():
    """Test that class Intrinsic_Function_Reference correctly matches a
    generic intrinsic which accepts a range of number of arguments.

    """
    for args in ["", "A", "A, B", "A, B, C"]:
        result = Intrinsic_Function_Reference(f"SYSTEM_CLOCK({args})")
        assert isinstance(result, Intrinsic_Function_Reference)
        assert str(result) == f"SYSTEM_CLOCK({args})"


@pytest.mark.usefixtures("f2003_create")
def test_intrinsic_function_reference_unlimited_args():
    """Test that class Intrinsic_Function_Reference correctly matches a
    generic intrinsic which accepts an unlimitednumber of arguments.

    """
    for args in ["A, B", "A, B, C", "A, B, C, D"]:
        result = Intrinsic_Function_Reference(f"MAX({args})")
        assert isinstance(result, Intrinsic_Function_Reference)
        assert str(result) == f"MAX({args})"


@pytest.mark.usefixtures("f2003_create")
def test_intrinsic_function_reference_error1():
    """Test that class Intrinsic_Function_Reference raises the expected
    exception when the valid min and max args are equal (2 in this case)
    and the wrong number of arguments is supplied.

    """
    with pytest.raises(InternalSyntaxError) as excinfo:
        _ = Intrinsic_Function_Reference("MATMUL(A)")
    assert "Intrinsic 'MATMUL' expects 2 arg(s) but found 1." "" in str(excinfo.value)

    with pytest.raises(InternalSyntaxError) as excinfo:
        _ = Intrinsic_Function_Reference("MATMUL(A,B,C)")
    assert "Intrinsic 'MATMUL' expects 2 arg(s) but found 3." "" in str(excinfo.value)


@pytest.mark.usefixtures("f2003_create")
def test_intrinsic_function_reference_error2():
    """Test that class Intrinsic_Function_Reference raises the expected
    exception when the valid min args is less than the valid max args
    and the wrong number of arguments is supplied.

    """
    with pytest.raises(InternalSyntaxError) as excinfo:
        _ = Intrinsic_Function_Reference("PRODUCT()")
    assert "Intrinsic 'PRODUCT' expects between 1 and 3 args but found 0." "" in str(
        excinfo.value
    )

    with pytest.raises(InternalSyntaxError) as excinfo:
        _ = Intrinsic_Function_Reference("PRODUCT(A,B,C,D)")
    assert "Intrinsic 'PRODUCT' expects between 1 and 3 args but found 4." "" in str(
        excinfo.value
    )


@pytest.mark.usefixtures("f2003_create")
def test_intrinsic_function_reference_error3():
    """Test that class Intrinsic_Function_Reference raises the expected
    exception when the number of arguments is unlimited.

    """
    with pytest.raises(InternalSyntaxError) as excinfo:
        _ = Intrinsic_Function_Reference("MIN(A)")
    assert "Intrinsic 'MIN' expects at least 2 args but found 1." "" in str(
        excinfo.value
    )


@pytest.mark.usefixtures("f2003_create")
def test_intrinsic_inside_intrinsic():
    """Test that when an intrinsic is within another instrinsic then both
    are recognised as intrinsics.

    """
    reader = get_reader("subroutine sub()\na = sin(cos(b))\nend subroutine sub\n")
    ast = Program(reader)
    rep = repr(ast).replace("u'", "'")
    assert "Intrinsic_Name('SIN')" in rep
    assert "Intrinsic_Name('COS')" in rep


def test_locally_shadowed_intrinsic(f2003_parser):
    """Check that a locally-defined symbol that shadows (overwrites) a
    Fortran intrinsic is correctly identified."""
    tree = f2003_parser(
        get_reader(
            """\
module my_mod
  use some_mod
  real :: dot_product(2,2)
contains
  subroutine my_sub()
    real :: result
    result = dot_product(1,1)
  end subroutine my_sub
end module my_mod
    """
        )
    )
    tables = SYMBOL_TABLES
    # We should not have an intrinsic-function reference in the parse tree
    assert not walk(tree, Intrinsic_Function_Reference)
    table = tables.lookup("my_mod")
    sym = table.children[0].lookup("dot_product")
    assert sym.primitive_type == "real"


def test_shadowed_intrinsic_named_import(f2003_parser):
    """Check that an imported symbol that shadows (overwrites) a
    Fortran intrinsic is correctly identified."""
    tree = f2003_parser(
        get_reader(
            """\
module my_mod
  use some_mod, only: dot_product
contains
  subroutine my_sub()
    real :: result
    result = dot_product(1,1)
  end subroutine my_sub
end module my_mod
    """
        )
    )
    tables = SYMBOL_TABLES
    # We should not have an intrinsic-function reference in the parse tree
    assert not walk(tree, Intrinsic_Function_Reference)
    table = tables.lookup("my_mod")
    sym = table.children[0].lookup("dot_product")
    assert sym.primitive_type == "unknown"


@pytest.mark.parametrize("use_stmts", [("use some_mod", ""), ("", "use some_mod")])
def test_shadowed_intrinsic_import(f2003_parser, use_stmts):
    """Check that an imported symbol that shadows (overwrites) a Fortran
    intrinsic is not identified as an intrinsic if it has the wrong
    number of 'arguments'.

    """
    tree = f2003_parser(
        get_reader(
            f"""\
module my_mod
  {use_stmts[0]}\n
contains
  subroutine my_sub()
    {use_stmts[1]}\n
    real :: result
    ! Too many args
    result = dot_product(1,1,1)
    ! Too few args for an intrinsic that has no max arg. count
    result = max()
    ! Wrong number of args for an intrinsic with a min and max arg. count that are
    ! not equal.
    result = aint(1, 2, 3)

    contains

    function tricky(a) result(b)
      real, intent(in) :: a
      real :: b
      ! Another reference to dot_product for which we need to find the
      ! wildcard import in the top-level module.
      b = 2 * a + dot_product(a,1,2)
    end function tricky
  end subroutine my_sub
end module my_mod
    """
        )
    )
    # We should not have an intrinsic-function reference in the parse tree
    assert not walk(tree, Intrinsic_Function_Reference)


def test_shadowed_intrinsic_error(f2003_parser):
    """Check that an imported symbol that shadows (overwrites) a
    Fortran intrinsic is not identified as an intrinsic if it has the wrong
    types of argument. At the moment we are unable to check the types of
    arguments (TODO #201) and so this test x-fails."""
    tree = f2003_parser(
        get_reader(
            """\
module my_mod
  use some_mod
contains
  subroutine my_sub()
    real :: result
    result = dot_product(1,1)
  end subroutine my_sub
end module my_mod
    """
        )
    )
    # We should not have an intrinsic-function reference in the parse tree
    if walk(tree, Intrinsic_Function_Reference):
        pytest.xfail("TODO #201: incorrect match of Intrinsic_Function_Reference")
