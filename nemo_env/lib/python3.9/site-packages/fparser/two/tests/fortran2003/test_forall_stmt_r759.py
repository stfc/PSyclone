# Copyright (c) 2019 Science and Technology Facilities Council

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

"""Test Fortran 2003 rule R759 : This file tests the support for the
forall statement.

"""

import pytest
from fparser.api import get_reader
from fparser.two.Fortran2003 import Forall_Stmt
from fparser.two.utils import NoMatchError, InternalError


def test_forall_stmt(f2003_create):
    """Check that a basic forall statement is parsed correctly. Input
    separately as a string and as a reader object.

    """

    def check_forall(reader):
        """Internal helper function to avoid code replication."""
        ast = Forall_Stmt(reader)
        assert "FORALL (i = 1 : 2, j = 1 : 2) a(i, j) = i + j" in str(ast)

    for line in [
        "forall (i=1:2,j=1:2) a(i,j)=i+j",
        "FoRaLl (i=1:2,j=1:2) a(i,j)=i+j",
        "  forall  (  i=1:2,j=1:2  )   a(i,j)=i+j  ",
    ]:
        check_forall(line)
        reader = get_reader(line)
        check_forall(reader)


def test_forall_stmt_brackets(f2003_create):
    """Check that a basic forall statement with brackets in the triplet
    list is parsed correctly. Input separately as a string and as a
    reader object.

    """

    def check_forall(reader):
        """Internal helper function to avoid code replication."""
        ast = Forall_Stmt(reader)
        assert "FORALL (i = b(k) : 2, j = 1 : c(k)) a(i, j) = 0.0" in str(ast)

        line = "forall (i=b(k):2,j=1:c(k)) a(i,j)=0.0"
        check_forall(line)
        reader = get_reader(line)
        check_forall(reader)


def test_syntaxerror(f2003_create):
    """Test that NoMatchError is raised for various syntax errors."""
    for line in [
        "",
        "   ",
        "forall",
        "for all (i=1,2,j=1:2) a(i,j)=i+j",
        "forl (i=1,2,j=1:2) a(i,j)=i+j",
        "forall i=1,2,j=1:2) a(i,j)=i+j",
        "forall (i=1,2,j=1:2 a(i,j)=i+j",
        "forall i=1,2,j=1:2) a=0.0",
        "forall (i=1,2,j=1:2 a=0.0",
        "forall x (i=1,2,j=1:2) a(i,j)=i+j",
        "forall (i=1,2,j=1:2) x a(i,j)=i+j",
        "forall (i=1,2,j=1:2)",
        "forall a(i,j)=i+j",
        "forall () a(i,j)=i+j",
    ]:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Forall_Stmt(line)
        assert "Forall_Stmt: '{0}'".format(line) in str(excinfo.value)


def test_internal_error1(f2003_create, monkeypatch):
    """Check that an internal error is raised if the length of the Items
    list is not 2 as the str() method assumes that it is.

    """
    line = "forall (i=1:2,j=1:2) a(i,j)=0.0"
    ast = Forall_Stmt(line)
    monkeypatch.setattr(ast, "items", [None, None, None])
    with pytest.raises(InternalError) as excinfo:
        str(ast)
    assert (
        "Class Forall_Stmt method tostr() has '3' items, but " "expecting 2."
    ) in str(excinfo.value)


def test_internal_error2(f2003_create):
    """Check that an internal error is raised if entry 0 of Items is empty
    or None as the str() method assumes that it is a string with
    content.

    """
    line = "forall (i=1:2,j=1:2) a(i,j)=0.0"
    ast = Forall_Stmt(line)
    for content in [None, ""]:
        ast.items = (content, ast.items[1])
        with pytest.raises(InternalError) as excinfo:
            str(ast)
        assert ("'Items' entry 0 should be a valid " "Forall_Header") in str(
            excinfo.value
        )


def test_internal_error3(f2003_create):
    """Check that an internal error is raised if entry 1 of Items is empty
    or None as the str() method assumes that it is a string with
    content.

    """
    line = "forall (i=1:2,j=1:2) a(i,j)=0.0"
    ast = Forall_Stmt(line)
    for content in [None, ""]:
        ast.items = (ast.items[0], content)
        with pytest.raises(InternalError) as excinfo:
            str(ast)
        assert ("'Items' entry 1 should be a valid " "Forall_Assignment_Stmt") in str(
            excinfo.value
        )
