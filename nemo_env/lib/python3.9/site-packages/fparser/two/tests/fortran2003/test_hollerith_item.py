# Copyright (c) 2019-2024 Science and Technology Facilities Council

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

"""Test Fortran 2003 : This file tests the support for a Hollerith
string. Hollerith strings take the form `nHx`, where `n` is an integer
and `x` is a sequence of characters of length `n`.

Note, the Hollerith format was deprecated in Fortran77 and removed in
Fortran95. However, Fortran compilers still support it.

"""

import pytest
from fparser.two.Fortran2003 import Hollerith_Item
from fparser.two.utils import NoMatchError, InternalError


def test_hollerith(f2003_create, monkeypatch):
    """Check that a valid hollerith string is parsed correctly."""
    from fparser.two import utils

    monkeypatch.setattr(utils, "EXTENSIONS", ["hollerith"])
    for myinput in ["2Hab", "  2Hab", "1h ", "7h1234567", " 1 1 H01234567890"]:
        ast = Hollerith_Item(myinput)
        expected = myinput.upper()
        # Remove any spaces before the H (but not after).
        lhs, rhs = expected.split("H")
        expected = lhs.replace(" ", "") + "H" + rhs
        assert str(ast).upper() == expected


def test_repr(f2003_create, monkeypatch):
    """Check that the repr output of a hollerith string gives the expected
    result.

    """
    from fparser.two import utils

    monkeypatch.setattr(utils, "EXTENSIONS", ["hollerith"])
    myinput = "2Hab"
    ast = Hollerith_Item(myinput)
    assert repr(ast) == "Hollerith_Item('ab')"


def test_syntaxerror(f2003_create, monkeypatch):
    """test that an exception is raised if the supplied format is
    invalid.

    """
    from fparser.two import utils

    monkeypatch.setattr(utils, "EXTENSIONS", ["hollerith"])
    for myinput in [None, "", "  ", "0H", "1H", "2Hx" "2Hxxx", "H20", "xH"]:
        with pytest.raises(NoMatchError):
            _ = Hollerith_Item(myinput)


def test_internal_error1(f2003_create, monkeypatch):
    """Check that an internal error is raised if the length of the Items
    list is not 1 as the str() method assumes that it is.

    """
    from fparser.two import utils

    monkeypatch.setattr(utils, "EXTENSIONS", ["hollerith"])
    myinput = "2Hab"
    ast = Hollerith_Item(myinput)
    monkeypatch.setattr(ast, "items", [None, None])
    with pytest.raises(InternalError) as excinfo:
        str(ast)
    assert "should be of length 1 but found '2'" in str(excinfo.value)


def test_internal_error2(f2003_create, monkeypatch):
    """Check that an internal error is raised if the string value (entry 0
    of Items) is empty or None as the str() method assumes that it is
    a string with content.

    """
    from fparser.two import utils

    monkeypatch.setattr(utils, "EXTENSIONS", ["hollerith"])
    myinput = "2hab"
    ast = Hollerith_Item(myinput)
    for content in [None, ""]:
        monkeypatch.setattr(ast, "items", [content])
        with pytest.raises(InternalError) as excinfo:
            str(ast)
        assert ("entry 0 should be a valid Hollerith string but it is empty") in str(
            excinfo.value
        )


def test_invalid_hollerith(f2003_create, monkeypatch):
    """Test that the hollerith extension to the standard raises an
    exception if it is not named as a valid extension.

    """
    from fparser.two import utils

    monkeypatch.setattr(utils, "_EXTENSIONS", [])
    myinput = "2Hab"
    with pytest.raises(NoMatchError) as excinfo:
        _ = Hollerith_Item(myinput)
    assert "Hollerith_Item: '{0}'".format(myinput) in str(excinfo.value)
