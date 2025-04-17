# Copyright (c) 2018 Science and Technology Facilities Council

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

"""Test Fortran 2003 rule R430 : This file tests the support for the
Derived Type Statement e.g.

type, private, abstract :: my_type(b,c)

"""

import pytest
from fparser.two.Fortran2003 import Derived_Type_Stmt
from fparser.two.utils import NoMatchError, InternalError


def test_valid(f2003_create):
    """Check that valid input is parsed correctly."""

    # simple minimal type statement
    ast = Derived_Type_Stmt("type a")
    assert "TYPE :: a" in str(ast)
    assert repr(ast) == "Derived_Type_Stmt(None, Type_Name('a'), None)"

    # simple minimal type statement with spaces
    ast = Derived_Type_Stmt("  type  a  ")
    assert "TYPE :: a" in str(ast)

    # simple minimal type statement with no spaces
    ast = Derived_Type_Stmt("typea")
    assert "TYPE :: a" in str(ast)

    # simple minimal type statement with "::"
    ast = Derived_Type_Stmt("type :: a")
    assert "TYPE :: a" in str(ast)

    # type statement with attribute spec list
    ast = Derived_Type_Stmt("type, private, abstract :: a")
    assert "TYPE, PRIVATE, ABSTRACT :: a" in str(ast)
    assert repr(ast) == (
        "Derived_Type_Stmt(Type_Attr_Spec_List(',', "
        "(Access_Spec('PRIVATE'), "
        "Type_Attr_Spec('ABSTRACT', None))), "
        "Type_Name('a'), None)"
    )

    # type statement with type parameter name list
    ast = Derived_Type_Stmt("type, private, abstract :: a(b,c)")
    assert "TYPE, PRIVATE, ABSTRACT :: a(b, c)" in str(ast)
    assert repr(ast) == (
        "Derived_Type_Stmt(Type_Attr_Spec_List(',', "
        "(Access_Spec('PRIVATE'), "
        "Type_Attr_Spec('ABSTRACT', None))), "
        "Type_Name('a'), Type_Param_Name_List(',', "
        "(Name('b'), Name('c'))))"
    )

    # type statement with type parameter name list spaces
    ast = Derived_Type_Stmt("  type , private , abstract :: a ( b , c )  ")
    assert "TYPE, PRIVATE, ABSTRACT :: a(b, c)" in str(ast)

    # type statement with type parameter name list no spaces
    ast = Derived_Type_Stmt("type,private,abstract::a(b,c)")
    assert "TYPE, PRIVATE, ABSTRACT :: a(b, c)" in str(ast)

    # type statement with type parameter name list no colons
    ast = Derived_Type_Stmt("type a(b,c)")
    assert "TYPE :: a(b, c)" in str(ast)


def test_errors(f2003_create):
    """Check that invalid input does not match."""

    for value in [
        "",
        "  ",
        "typ",
        "type",
        "type ::",
        "type, ::",
        "type x ::",
        "type a b",
        "type a()",
        "type a(  )",
        "type a(b) c",
    ]:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Derived_Type_Stmt(value)
        assert "Derived_Type_Stmt: '{0}'".format(value) in str(excinfo.value)


def test_tostr_1(f2003_create, monkeypatch):
    """Check that Derived_Type_Stmt.tostr() raises an exception if there
    is an invalid number of items.

    """

    ast = Derived_Type_Stmt("type a")
    monkeypatch.setattr(ast, "items", ["A"])
    with pytest.raises(InternalError) as excinfo:
        str(ast)
    assert "should be of size 3 but found '1'" in str(excinfo.value)


def test_tostr_2(f2003_create, monkeypatch):
    """Check that Derived_Type_Stmt.tostr() raises an exception if the
    content of items[1] is invalid.

    """

    ast = Derived_Type_Stmt("type a")
    monkeypatch.setattr(ast, "items", [None, None, None])
    with pytest.raises(InternalError) as excinfo:
        str(ast)
    assert (
        "'items[1]' should be a Name instance containing the "
        "derived type name but it is empty"
    ) in str(excinfo.value)


def test_get_start_name(f2003_create):
    """Check that the appropriate name is returned from
    get_start_name()

    """
    ast = Derived_Type_Stmt("type a")
    assert ast.get_start_name() == "a"
