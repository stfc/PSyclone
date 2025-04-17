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

"""Tests for a Fortran 2003 R1011 control edit descriptor."""

import pytest
from fparser.two import utils
from fparser.two.Fortran2003 import Control_Edit_Desc
from fparser.two.utils import NoMatchError, InternalError


def test_descriptors_match(f2003_create):
    """Check that valid control edit descriptors are parsed correctly when
    they are dealt with by the match method. These are '/', ':', 'P'
    and '$'. '$' is dealt with in separate tests as it is an
    extension. We test with and without spaces.

    """
    for my_input in [
        "/",
        " / ",
        "2/",
        " 2 / ",
        ":",
        " : ",
        "2P",
        " 2 P ",
        "2p",
        " 2 p ",
    ]:
        ast = Control_Edit_Desc(my_input)
        assert str(ast) == my_input.upper().replace(" ", "")


def test_descriptors_subclass(f2003_create):
    """Check that valid control edit descriptors are parsed correctly when
    they are passed onto subclasses. In this case we just test a
    single example for each subclass to see if valid values are passed
    on, as the subclass tests check all the options.

    """
    for my_input in ["T2", "SS", "BN", "RU", "DC"]:
        ast = Control_Edit_Desc(my_input)
        assert str(ast) == my_input.upper()


def test_dollar_valid(f2003_create, monkeypatch):
    """Check that valid $ format specifications are parsed correctly if
    the dollar-descriptor extension is specified. Also include an
    example with spaces.

    """
    monkeypatch.setattr(utils, "EXTENSIONS", ["dollar-descriptor"])
    for my_input in ["$", " $ "]:
        ast = Control_Edit_Desc(my_input)
        assert str(ast) == my_input.upper().replace(" ", "")


def test_dollar_invalid(f2003_create, monkeypatch):
    """Check that valid '$' format specifications raise a NoMatchError if
    the 'dollar-format' extension is not in the EXTENSIONS list.

    """
    monkeypatch.setattr(utils, "_EXTENSIONS", [])
    for my_input in ["$", " $ "]:
        with pytest.raises(NoMatchError):
            _ = Control_Edit_Desc(my_input)


def test_invalid_format_errors(f2003_create):
    """Check that invalid format for the match method raises a
    NoMatchError exception.

    """
    for my_input in [
        None,
        "",
        "  ",
        "//",
        "a /",
        "/ a",
        "::",
        "a :",
        ": a",
        "pp",
        "a p",
        "p a",
    ]:
        with pytest.raises(NoMatchError):
            _ = Control_Edit_Desc(my_input)


def test_internal_error1(f2003_create, monkeypatch):
    """Check that an internal error is raised if the length of the Items
    list is not 2 as the str() method assumes that it is.

    """
    my_input = "3P"
    ast = Control_Edit_Desc(my_input)
    monkeypatch.setattr(ast, "items", [None, None, None])
    with pytest.raises(InternalError) as excinfo:
        str(ast)
    assert "has '3' items, but expecting 2." in str(excinfo.value)


def test_internal_error2(f2003_create, monkeypatch):
    """Check that an internal error is raised if the descriptor name
    (first entry of items) is empty or None as the str() method assumes
    that it is a string with content.

    """
    my_input = "3P"
    ast = Control_Edit_Desc(my_input)
    for content in [None, ""]:
        monkeypatch.setattr(ast, "items", [ast.items[0], content])
        with pytest.raises(InternalError) as excinfo:
            str(ast)
        assert "should be an edit descriptor name but is empty or None" in str(
            excinfo.value
        )
