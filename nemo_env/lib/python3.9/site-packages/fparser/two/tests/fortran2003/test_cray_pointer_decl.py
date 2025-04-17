# Copyright (c) 2019-2022 Science and Technology Facilities Council

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

"""Test Fortran 2003 Cray-pointers: This file tests the support for a
Cray-pointer declaration.

"""

import pytest
from fparser.two.Fortran2003 import Cray_Pointer_Decl
from fparser.two.utils import NoMatchError, InternalError


def test_cray_pointer_decl(f2003_create):
    """Check that Cray-pointer declarations are parsed correctly."""
    for myinput in ["(a, b)", "  ( a , b )  "]:
        ast = Cray_Pointer_Decl(myinput)
        assert "(a, b)" in str(ast)
        assert repr(ast) == ("Cray_Pointer_Decl(Name('a'), Name('b'))")


def test_pointee_decl(f2003_create):
    """Check that a Cray-pointer declaration containing a pointee
    declaration is parsed correctly (for explicit and assumed shape
    arrays).

    """
    for myinput in [
        "(a, b(n))",
        "(a, b(0 : n))",
        "(a, b(n, m))",
        "(a, b(5, *))",
        "(a, b(*))",
        "(a, b(0 : 1, 2 : *))",
    ]:
        ast = Cray_Pointer_Decl(myinput)
        assert myinput in str(ast)


def test_errors(f2003_create):
    """Check that syntax errors produce a NoMatchError exception."""
    for myinput in [
        None,
        "",
        "  ",
        "a, b)",
        "(a, b",
        "()",
        "(a)",
        "(a b)",
        "(1, a)",
        "(a, 1)",
        "(a, b(2)",
        "(a, b2))",
        "(a, b())",
    ]:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Cray_Pointer_Decl(myinput)
        assert "Cray_Pointer_Decl: '{0}'".format(myinput) in str(excinfo.value)


def test_internal_error1(f2003_create, monkeypatch):
    """Check that an internal error is raised if the length of the Items
    list is not 2 as the str() method assumes that it is.

    """
    myinput = "(mypointer, mypointee)"
    ast = Cray_Pointer_Decl(myinput)
    monkeypatch.setattr(ast, "items", [None])
    with pytest.raises(InternalError) as excinfo:
        str(ast)
    assert "should be of size 2 but found '1'" in str(excinfo.value)


def test_internal_error2(f2003_create, monkeypatch):
    """Check that an internal error is raised if the pointer name (entry 0
    of Items) is empty or None as the str() method assumes that it is
    a string with content.

    """
    myinput = "(mypointer, mypointee)"
    ast = Cray_Pointer_Decl(myinput)
    for change in [None, ""]:
        monkeypatch.setattr(ast, "items", (change, "mypointee"))
        with pytest.raises(InternalError) as excinfo:
            str(ast)
        assert ("'Items' entry 0 should be a pointer name but it is empty") in str(
            excinfo.value
        )


def test_internal_error3(f2003_create, monkeypatch):
    """Check that an internal error is raised if the pointee name (entry 1
    of Items) is empty or None as the str() method assumes that it is
    a string with content.

    """
    myinput = "(mypointer, mypointee)"
    ast = Cray_Pointer_Decl(myinput)
    for change in [None, ""]:
        monkeypatch.setattr(ast, "items", ("mypointer", change))
        with pytest.raises(InternalError) as excinfo:
            str(ast)
        assert (
            "'Items' entry 1 should be a pointee name or pointee "
            "declaration but it is empty"
        ) in str(excinfo.value)
