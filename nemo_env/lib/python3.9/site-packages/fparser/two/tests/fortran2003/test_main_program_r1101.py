# Copyright (c) 2018-2021 Science and Technology Facilities Council.

# All rights reserved.

# Modifications made as part of the fparser project are distributed
# under the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:

# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.

# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.

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

"""Test Fortran 2003 rule R1101 : This file tests the support for a
main program with a program statement. It does not test the case where
there is no program statement. That situation is covered by the
Main_Program0 class. Tests for Main_Program0 are currently in
test_fortran2003.py.

As this rule is about ordering of other rules we only need to test the
different combinations. We don't need to test the content of
individual rules (e.g we do not need to worry about "end", "end
program", "end program a" as these should be covered by the tests for
End_Program_Unit

"""

import pytest
from fparser.two.utils import FortranSyntaxError, NoMatchError
from fparser.api import get_reader
from fparser.two.Fortran2003 import Main_Program
from fparser.two.symbol_table import SYMBOL_TABLES


def test_valid(f2003_create):
    """
    Test that valid code is parsed correctly and associated symbol tables
    created.

    """
    # basic
    obj = Main_Program(get_reader("program a\nend"))
    assert isinstance(obj, Main_Program)
    assert str(obj) == "PROGRAM a\nEND"
    assert repr(obj) == (
        "Main_Program(Program_Stmt('PROGRAM', Name('a')), "
        "End_Program_Stmt(None, None))"
    )

    # name matching
    obj = Main_Program(get_reader("program a\nend program a"))
    assert isinstance(obj, Main_Program)
    assert str(obj) == "PROGRAM a\nEND PROGRAM a"
    assert repr(obj) == (
        "Main_Program(Program_Stmt('PROGRAM', Name('a')), "
        "End_Program_Stmt('PROGRAM', Name('a')))"
    )

    # mixed case name matching
    obj = Main_Program(get_reader("program a\nend program A"))
    assert isinstance(obj, Main_Program)
    assert str(obj) == "PROGRAM a\nEND PROGRAM A"

    # specification-part
    obj = Main_Program(get_reader("program a\ninteger i\nend program a"))
    assert str(obj) == "PROGRAM a\n  INTEGER :: i\nEND PROGRAM a"
    table = SYMBOL_TABLES.lookup("a")
    assert table.lookup("i")
    # Clear existing symbol tables before next part of this test
    SYMBOL_TABLES.clear()

    # execution-part
    obj = Main_Program(get_reader("program a\ni=10\nend program a"))
    assert str(obj) == "PROGRAM a\n  i = 10\nEND PROGRAM a"

    # internal-subprogram-part
    obj = Main_Program(
        get_reader("program a\ncontains\nsubroutine foo\n" "end\nend program a")
    )
    assert str(obj) == (
        "PROGRAM a\n  CONTAINS\n  SUBROUTINE foo\n" "  END\nEND PROGRAM a"
    )

    # specification-part + execution-part
    obj = Main_Program(get_reader("program a\ninteger i\ni=10\nend program a"))
    assert str(obj) == "PROGRAM a\n  INTEGER :: i\n  i = 10\nEND PROGRAM a"
    # Clear existing symbol tables before next part of this test
    SYMBOL_TABLES.clear()

    # execution-part + internal-subprogram-part
    obj = Main_Program(
        get_reader("program a\ni=10\ncontains\nsubroutine foo\n" "end\nend program a")
    )
    assert str(obj) == (
        "PROGRAM a\n  i = 10\n  CONTAINS\n  SUBROUTINE foo\n" "  END\nEND PROGRAM a"
    )
    # Clear existing symbol tables before next part of this test
    SYMBOL_TABLES.clear()

    # specification-part + execution-part + internal-subprogram-part
    obj = Main_Program(
        get_reader(
            "program a\ninteger i\ni=10\ncontains\n"
            "subroutine foo\nend\nend program a"
        )
    )
    assert str(obj) == (
        "PROGRAM a\n  INTEGER :: i\n  i = 10\n  CONTAINS\n  "
        "SUBROUTINE foo\n  END\nEND PROGRAM a"
    )
    table = SYMBOL_TABLES.lookup("a")
    assert table.lookup("i")
    assert table.children[0].name == "foo"


def test_invalid1(f2003_create):
    """Test that exceptions are raised for invalid code"""
    # no end
    with pytest.raises(NoMatchError) as excinfo:
        _ = Main_Program(get_reader("program a\n"))
    assert "at line 1\n>>>program a" in str(excinfo.value)
    # Check that we have no symbol table
    assert "a" not in SYMBOL_TABLES._symbol_tables

    # no start
    with pytest.raises(NoMatchError) as excinfo:
        _ = Main_Program(get_reader("end program a\n"))
    assert "at line 1\n>>>end program a" in str(excinfo.value)
    # Check that we have no symbol table
    assert "a" not in SYMBOL_TABLES._symbol_tables

    # name mismatch
    with pytest.raises(FortranSyntaxError) as excinfo:
        _ = Main_Program(get_reader("program a\nend program b"))
    assert "at line 2\n>>>end program b\nExpecting name 'a'" in str(excinfo.value)
    # Check that we have no symbol table
    assert "a" not in SYMBOL_TABLES._symbol_tables


def test_invalid2(f2003_create):
    """Test that specification-part after execution-part produces an
    error.

    """
    with pytest.raises(NoMatchError) as excinfo:
        _ = Main_Program(get_reader("program a\ni=10\ninteger i\n" "end program a"))
    assert "at line 3\n>>>integer i\n" in str(excinfo.value)
    assert "a" not in SYMBOL_TABLES._symbol_tables


def test_invalid3(f2003_create):
    """Test that execution-part after internal-subprogram-part produces an
    error.

    """
    with pytest.raises(NoMatchError) as excinfo:
        _ = Main_Program(
            get_reader(
                "program a\ncontains\nsubroutine foo\n" "end\ni=10\nend program a"
            )
        )
    assert "at line 5\n>>>i=10\n" in str(excinfo.value)
    assert "a" not in SYMBOL_TABLES._symbol_tables
