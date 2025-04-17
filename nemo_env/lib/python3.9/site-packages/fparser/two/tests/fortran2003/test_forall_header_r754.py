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

"""Test Fortran 2003 rule R754 : This file tests the support for the
forall-header statement.

"""

import pytest
from fparser.two.Fortran2003 import Forall_Header
from fparser.two.utils import NoMatchError, InternalError


def test_triplet_list(f2003_create):
    """Test that one or more triplets is parsed correctly."""
    for my_input in [
        "(i=1:n)",
        "  (  i  =  1  :  n  )  ",
        "(I=1:N)",
        "(i=1:n,j=1:n)",
        "(i=1:n:m,j=1:n)",
        "(i=1:n,j=1:n:m)",
        "(i=1:n:m,j=1:n:m)",
        "(i=1:n:m,j=1:n:m,k=1:n:m)",
        "(i=a(p,p):n:m,j=1:b(p,p):m,k=1:n:c(p,p))",
    ]:
        ast = Forall_Header(my_input)
        assert my_input.replace(" ", "") in str(ast).replace(" ", "")


def test_mask_expr(f2003_create):
    """Test that an optional mask expression after a triplet is parsed
    correctly.

    """
    for my_input in [
        "(i=1:n,i<4)",
        "  (  i  =  1  :  n  ,  i  <  4  )  ",
        "(I=1:N,I<4)",
        "(i=1:n,j=1:n,i/=j)",
        "(i=1:n:m,j=1:n,i/=j)",
        "(i=1:n,j=1:n:m,i/=j)",
        "(i=1:n:m,j=1:n:m,i/=j)",
        "(i=1:n:m,j=1:n:m,k=1:n:m,i/=k)",
        "(i=1:n:m,j=1:n:m,k=1:n:m,a(i,j,k)==0.0)",
    ]:
        ast = Forall_Header(my_input)
        assert my_input.replace(" ", "") in str(ast).replace(" ", "")


def test_parse_error(f2003_create):
    """Test that NoMatchError is raised for various syntax errors."""
    for my_input in ["", "  " "(", "()", "i=1:n)", "(i=1:n", "(error)"]:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Forall_Header(my_input)
        assert "Forall_Header: '{0}'".format(my_input) in str(excinfo.value)


def test_internal_error1(f2003_create, monkeypatch):
    """Check that an internal error is raised if the length of the Items
    list is not 2 as the str() method assumes that it is.

    """
    my_input = "(i=1:2)"
    ast = Forall_Header(my_input)
    monkeypatch.setattr(ast, "items", [None, None, None])
    with pytest.raises(InternalError) as excinfo:
        str(ast)
    assert "should be of size 2 but found '3'" in str(excinfo.value)


def test_use_internal_error2(f2003_create, monkeypatch):
    """Check that an internal error is raised if the triplet lisy (entry 0
    of Items) is empty or None as the str() method assumes that it
    returns a string with content.

    """
    my_input = "(i=1:2)"
    ast = Forall_Header(my_input)
    for content in [None, ""]:
        monkeypatch.setattr(ast, "items", [content, None])
        with pytest.raises(InternalError) as excinfo:
            str(ast)
        assert (
            "'items[0]' should be a Forall_Triplet_Spec_List instance "
            "but it is empty"
        ) in str(excinfo.value)
