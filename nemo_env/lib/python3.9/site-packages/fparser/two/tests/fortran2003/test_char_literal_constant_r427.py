# Copyright (c) 2018-2019 Science and Technology Facilities Council.

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

"""Test Fortran 2003 rule R427 : This file tests the support for a
character literal constant.

"""

import pytest
from fparser.two.Fortran2003 import Char_Literal_Constant
from fparser.two.utils import NoMatchError, InternalError


def test_match_valid():
    """Test that valid input is parsed correctly"""

    # simple, single quotes
    obj = Char_Literal_Constant("'DO'")
    assert isinstance(obj, Char_Literal_Constant), repr(obj)
    assert str(obj) == "'DO'"
    assert repr(obj).replace("u", "") == "Char_Literal_Constant(\"'DO'\", None)"

    # simple, double quotes
    obj = Char_Literal_Constant('"DO"')
    assert isinstance(obj, Char_Literal_Constant), repr(obj)
    assert str(obj) == '"DO"'

    # single quotes inside single quotes (two means one)
    obj = Char_Literal_Constant("'DON''T'")
    assert isinstance(obj, Char_Literal_Constant), repr(obj)
    assert str(obj) == "'DON''T'"

    # double quotes inside double quotes (two means one)
    obj = Char_Literal_Constant('"""busy"""')
    assert isinstance(obj, Char_Literal_Constant), repr(obj)
    assert str(obj) == '"""busy"""'

    # single quotes, spaces
    obj = Char_Literal_Constant("  '  D  O  '  ")
    assert isinstance(obj, Char_Literal_Constant), repr(obj)
    assert str(obj) == "'  D  O  '"
    assert repr(obj).replace('u"', '"') == "Char_Literal_Constant(\"'  D  O  '\", None)"

    # Single quotes, empty string
    obj = Char_Literal_Constant("''")
    assert isinstance(obj, Char_Literal_Constant), repr(obj)
    assert str(obj) == "''"

    # Double quotes, empty string
    obj = Char_Literal_Constant('""')
    assert isinstance(obj, Char_Literal_Constant), repr(obj)
    assert str(obj) == '""'

    # include a kind parameter (which says what character set to
    # expect)
    obj = Char_Literal_Constant('KP_"DO"')
    assert isinstance(obj, Char_Literal_Constant), repr(obj)
    assert str(obj) == 'KP_"DO"'
    assert repr(obj).replace("u'", "'") == "Char_Literal_Constant('\"DO\"', 'KP')"

    # include a kind parameter with spaces
    obj = Char_Literal_Constant('  KP  _  "  D  O  "  ')
    assert isinstance(obj, Char_Literal_Constant), repr(obj)
    assert str(obj) == 'KP_"  D  O  "'
    assert repr(obj).replace("u'", "'") == "Char_Literal_Constant('\"  D  O  \"', 'KP')"

    # additional characters
    obj = Char_Literal_Constant("'()!$%^&*_+=-01~@#;:/?.>,<|'")
    assert isinstance(obj, Char_Literal_Constant), repr(obj)
    assert str(obj) == "'()!$%^&*_+=-01~@#;:/?.>,<|'"


def test_match_invalid():
    """Test that invalid input raises an exception"""

    # test various invalid options
    for example in [
        None,
        "",
        "  ",
        "A",
        "'A",
        "A'",
        '"A',
        'A"',
        "A'A'",
        "A 'A'",
        "'A'A",
        "'A' A",
        "_'A'",
        "$_'A'",
        "A A_'A'",
        "A_'A'A",
        "A_'A' A",
    ]:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Char_Literal_Constant(example)
        assert "Char_Literal_Constant: '{0}'".format(example) in str(excinfo.value)


def test_tostr_invalid1(monkeypatch):
    """Test that an invalid number of items raises an exception"""

    # test internal error in tostr() when the items list is not the
    # expected size
    obj = Char_Literal_Constant("'A'")
    monkeypatch.setattr(obj, "items", ["A"])
    with pytest.raises(InternalError) as excinfo:
        _ = str(obj)
    assert "tostr() has '1' items, but expecting 2" in str(excinfo.value)


def test_tostr_invalid2(monkeypatch):
    """Test that an empty items value raises an exception"""

    # test internal error in tostr() when the items list index 0 has
    # no content
    obj = Char_Literal_Constant("'A'")
    monkeypatch.setattr(obj, "items", [None, None])
    with pytest.raises(InternalError) as excinfo:
        _ = str(obj)
    assert "'Items' entry 0 should not be empty" in str(excinfo.value)


def test_tostr_non_ascii():
    """Check that the tostr() method works when the character string
    contains non-ascii characters."""
    obj = Char_Literal_Constant("'for e1=1\xb0'")
    out_str = str(obj)
    assert "for e1=1" in out_str
    # With a kind specifier...
    obj = Char_Literal_Constant("ckind_'for e1=1\xb0'")
    out_str = str(obj)
    assert "ckind_'for e1=" in out_str
