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

"""Tests for a Fortran 2003 data edit descriptor that only allows
descriptors that conform to (the first clause in) constraint C1002.

"""

import pytest
from fparser.two.Fortran2003 import Data_Edit_Desc_C1002
from fparser.two.utils import NoMatchError, InternalError


def test_wd(f2003_create):
    """Check that valid w.d format specifications are parsed
    correctly. Also include an example with spaces and multiple
    digits with spaces.

    """
    for descriptor in [
        "F",
        "E",
        "EN",
        "ES",
        "G",
        "D",
        "f",
        "e",
        "en",
        "En",
        "eN",
        "es",
        "g",
        "d",
    ]:
        for my_input in [
            "{0}2.3".format(descriptor),
            " {0} 2 . 3 ".format(descriptor),
            " {0} 2 2 . 3 3 ".format(descriptor),
        ]:
            ast = Data_Edit_Desc_C1002(my_input)
            assert str(ast) == my_input.upper().replace(" ", "")


def test_wde(f2003_create):
    """Check that valid w.dEe format specifications are parsed
    correctly. Also include an example with spaces and multiple
    digits with spaces.

    """
    for descriptor in ["E", "EN", "ES", "G", "e", "en", "En", "eN", "es", "g"]:
        for my_input in [
            "{0}2.3E4".format(descriptor),
            " {0} 2 . 3 E 4 ".format(descriptor),
            " {0} 2 2 . 3 3 E 4 4 ".format(descriptor),
        ]:
            ast = Data_Edit_Desc_C1002(my_input)
            assert str(ast) == my_input.upper().replace(" ", "")


def test_errors(f2003_create):
    """Check that format specifications not conforming to the C1002 1st
    clause, raise an NoMatchError exception.

    """
    for my_input in [
        None,
        "",
        "  ",
        "I2",
        "F2.3E4",
        "D2.3E4",
        "F",
        "D",
        "ES",
        "F2",
        "F2.",
        "E2E3",
        "E2.E3",
        "E2.3E",
        "E2.3E4E",
    ]:
        with pytest.raises(NoMatchError):
            _ = Data_Edit_Desc_C1002(my_input)


def test_internal_error1(f2003_create, monkeypatch):
    """Check that an internal error is raised if the length of the Items
    list is not 4 as the str() method assumes that it is.

    """
    my_input = "E1.2E3"
    ast = Data_Edit_Desc_C1002(my_input)
    monkeypatch.setattr(ast, "items", [None, None, None])
    with pytest.raises(InternalError) as excinfo:
        str(ast)
    assert "has '3' items, but expecting 4." in str(excinfo.value)


def test_internal_error2(f2003_create, monkeypatch):
    """Check that an internal error is raised if the descriptor name
    (entry 0 of items) is empty or None as the str() method assumes
    that it is a string with content.

    """
    my_input = "E1.2E3"
    ast = Data_Edit_Desc_C1002(my_input)
    for content in [None, ""]:
        monkeypatch.setattr(ast, "items", [content] + list(ast.items[1:]))
        with pytest.raises(InternalError) as excinfo:
            str(ast)
        assert "should be a descriptor name but is empty or None" in str(excinfo.value)


def test_internal_error3(f2003_create, monkeypatch):
    """Check that an internal error is raised if the w value
    (entry 1 of items) is empty or None as the str() method assumes
    that it is a string with content.

    """
    my_input = "E1.2E3"
    ast = Data_Edit_Desc_C1002(my_input)
    for content in [None, ""]:
        monkeypatch.setattr(
            ast, "items", [ast.items[0], content, ast.items[2], ast.items[3]]
        )
        with pytest.raises(InternalError) as excinfo:
            str(ast)
        assert "should be the w value but is empty or None" in str(excinfo.value)


def test_internal_error4(f2003_create, monkeypatch):
    """Check that an internal error is raised if the m value (entry 2 of
    items) is empty or None as the str() method assumes that it is a
    string with content.

    """
    my_input = "E1.2E3"
    ast = Data_Edit_Desc_C1002(my_input)
    for content in [None, ""]:
        monkeypatch.setattr(
            ast, "items", [ast.items[0], ast.items[1], content, ast.items[3]]
        )
        with pytest.raises(InternalError) as excinfo:
            str(ast)
        assert "should be the m value but is empty or None" in str(excinfo.value)


def test_internal_error5(f2003_create, monkeypatch):
    """Check that an internal error is raised if the e value (entry 3 of
    items) has content when the descriptor is F or D as it should be
    None.

    """
    for my_input in ["F1.2", "D1.2"]:
        ast = Data_Edit_Desc_C1002(my_input)
        monkeypatch.setattr(ast, "items", list(ast.items[0:3]) + ["3"])
        with pytest.raises(InternalError) as excinfo:
            str(ast)
        assert (
            "has an exponent value '3' but this is not allowed for 'F' "
            "and 'D' descriptors" in str(excinfo.value)
        )


def test_internal_error6(f2003_create, monkeypatch):
    """Check that an internal error is raised if the descriptor name
    has an unexpected value.

    """
    my_input = "E1.2E3"
    ast = Data_Edit_Desc_C1002(my_input)
    monkeypatch.setattr(ast, "items", ["INVALID"] + list(ast.items[1:]))
    with pytest.raises(InternalError) as excinfo:
        str(ast)
    assert "Unexpected descriptor name 'INVALID'" in str(excinfo.value)
