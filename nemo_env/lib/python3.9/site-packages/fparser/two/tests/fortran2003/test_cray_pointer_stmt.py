# Copyright (c) 2019-2024 Science and Technology Facilities Council.

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

"""Test Fortran 2003 Cray-pointers: This file tests the support for the
Cray-pointer statement.

"""

import pytest
from fparser.api import get_reader
from fparser.two import utils
from fparser.two.Fortran2003 import Cray_Pointer_Stmt
from fparser.two.utils import NoMatchError


def test_cray_pointer_stmt(f2003_create):
    """Check that a basic Cray-pointer statement is parsed
    correctly. Input separately as a string and as a reader object

    """

    def check_use(reader):
        """Internal helper function to avoid code replication."""
        ast = Cray_Pointer_Stmt(reader)
        assert "POINTER(a, b)" in str(ast)
        assert (
            repr(ast).replace("u", "")
            == "Cray_Pointer_Stmt('POINTER', Cray_Pointer_Decl_List(',', "
            "(Cray_Pointer_Decl(Name('a'), Name('b')),)))"
        )

    line = "pointer (a, b)"
    check_use(line)
    reader = get_reader(line)
    check_use(reader)


def test_spaces(f2003_create):
    """Check that spaces are allowed."""
    line = "  pointer  ( a , b )  "
    ast = Cray_Pointer_Stmt(line)
    assert "POINTER(a, b)" in str(ast)


def test_case(f2003_create):
    """Check that different case is allowed."""
    line = "PoInTeR (a, b)"
    ast = Cray_Pointer_Stmt(line)
    assert "POINTER(a, b)" in str(ast)


def test_list(f2003_create):
    """Check that a list of Cray-pointers is supported."""
    line = "pointer (a, b), (c, d(1:n)), (e, f)"
    ast = Cray_Pointer_Stmt(line)
    assert "POINTER(a, b), (c, d(1 : n)), (e, f)" in str(ast)


def test_errors(f2003_create):
    """Check that syntax errors produce a NoMatchError exception."""
    for line in [
        "",
        "  ",
        "ponter (a, b)",
        "pointer",
        "pointer a, b" "pointer (a, b) (a, b)",
    ]:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Cray_Pointer_Stmt(line)
        assert "Cray_Pointer_Stmt: '{0}'".format(line) in str(excinfo.value)


def test_invalid_cray_pointer(f2003_create, monkeypatch):
    """Test that the cray-pointer extension to the standard raises an
    exception if it is not named as a valid extension.

    """
    monkeypatch.setattr(utils, "_EXTENSIONS", [])
    myinput = "pointer (mypointer, mypointee)"
    with pytest.raises(NoMatchError) as excinfo:
        _ = Cray_Pointer_Stmt(myinput)
    assert "Cray_Pointer_Stmt: '{0}'".format(myinput) in str(excinfo.value)


def test_valid_cray_pointer(f2003_create, monkeypatch):
    """Test that the cray-pointer extension to the standard produces the
    expected output if it is named as a valid extension.

    """
    monkeypatch.setattr(utils, "_EXTENSIONS", ["cray-pointer"])
    myinput = "pointer(mypointer, mypointee)"
    result = Cray_Pointer_Stmt(myinput)
    assert str(result).lower() == myinput
