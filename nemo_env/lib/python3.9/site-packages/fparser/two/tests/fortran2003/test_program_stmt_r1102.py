# Copyright (c) 2018-2021 Science and Technology Facilities Council

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

"""Test Fortran 2003 rule R1102 : This file tests the support for the
program statement.

"""

import pytest
from fparser.api import get_reader
from fparser.two.utils import NoMatchError
from fparser.two.symbol_table import SYMBOL_TABLES
from fparser.two.Fortran2003 import Program_Stmt, Program, Name


@pytest.mark.usefixtures("f2003_create")
def test_valid():
    """Test that valid code is parsed correctly."""

    obj = Program_Stmt("program a")
    assert isinstance(obj, Program_Stmt)
    assert str(obj) == "PROGRAM a"
    assert repr(obj) == "Program_Stmt('PROGRAM', Name('a'))"
    # Check that the parent of the Name is correctly set
    assert obj.items[1].parent is obj


@pytest.mark.usefixtures("f2003_create")
def test_invalid():
    """Test that exceptions are raised for invalid code."""

    for string in [
        "",
        "  ",
        "prog",
        "program",
        "programa",
        "a program",
        "a program a",
        "program a a",
    ]:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Program_Stmt(string)
        assert "Program_Stmt: '{0}'".format(string) in str(excinfo.value)


@pytest.mark.usefixtures("f2003_create")
def test_prog_symbol_table():
    """Check that an associated symbol table is created when parsing a
    program unit."""
    reader = get_reader("program my_prog\n" "end program my_prog\n")
    prog = Program(reader)
    assert "my_prog" in SYMBOL_TABLES._symbol_tables


def test_get_name():
    """Test we can get the name of the program"""
    obj = Program_Stmt("program foo")
    assert obj.get_name() == Name("foo")


def test_get_start_name():
    """Test we can get the name of the function as a string"""

    obj = Program_Stmt("program foo")
    assert obj.get_start_name() == "foo"
