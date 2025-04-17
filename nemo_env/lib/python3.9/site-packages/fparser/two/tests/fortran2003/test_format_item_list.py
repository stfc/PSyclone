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

"""Test Fortran 2003 : This file tests the support for a format item
list. List classes are usually generated, and therefore are not
directly tested, but in this case we need to make a specific list
class due to the complexity of supporting Hollerith format items.

Note, the hollerith format was deprecated in Fortran77 and removed in
Fortran95. However, Fortran compilers still support it.

"""

import pytest
from fparser.two.Fortran2003 import Format_Item_List
from fparser.two.utils import NoMatchError
from fparser.two import utils


def test_non_hollerith(f2003_create):
    """Check that some basic format specifications are parsed
    correctly. Individual specifications are tested by the associated
    classes.

    """
    myinput = "E2.2, 'HELLO'"
    ast = Format_Item_List(myinput)
    assert myinput in str(ast)
    assert repr(ast).replace("u", "") == (
        "Format_Item_List(',', (Format_Item(None, Data_Edit_Desc_C1002('E', "
        "Digit_String('2', None), Int_Literal_Constant('2', None), None)), "
        "Char_Literal_Constant(\"'HELLO'\", None)))"
    )


def test_mixed_hollerith_1(f2003_create, monkeypatch):
    """Check that a hollerith item is parsed correctly. We do this as a
    hollerith item can contain commas so we can't use a simple
    format-item-list class to split items.

    """
    from fparser.two import utils

    monkeypatch.setattr(utils, "EXTENSIONS", ["hollerith"])
    myinput = "2Hab, 'HELLO'"
    ast = Format_Item_List(myinput)
    assert myinput in str(ast)
    assert repr(ast).replace("u", "") == (
        "Format_Item_List(',', (Hollerith_Item('ab'), "
        "Char_Literal_Constant(\"'HELLO'\", None)))"
    )


def test_mixed_hollerith_2(f2003_create, monkeypatch):
    """Check that a hollerith item is parsed correctly. We do this as a
    hollerith item can contain commas so we can't use a simple
    format-item-list class to split items.

    """
    from fparser.two import utils

    monkeypatch.setattr(utils, "EXTENSIONS", ["hollerith"])
    myinput = "'HELLO', 2hab"
    ast = Format_Item_List(myinput)
    assert myinput.lower() in str(ast).lower()
    assert repr(ast).replace("u", "") == (
        "Format_Item_List(',', (Char_Literal_Constant(\"'HELLO'\", None), "
        "Hollerith_Item('ab')))"
    )


def test_hollerith_only(f2003_create, monkeypatch):
    """Check that a hollerith item is parsed correctly. We do this as a
    hollerith item can contain commas so we can't use a simple
    format-item-list class to split items.

    """
    from fparser.two import utils

    monkeypatch.setattr(utils, "EXTENSIONS", ["hollerith"])
    myinput = "3Habc,2hab"
    ast = Format_Item_List(myinput)
    assert str(ast) == "3Habc, 2Hab"
    assert repr(ast) == (
        "Format_Item_List(',', (Hollerith_Item('abc'), " "Hollerith_Item('ab')))"
    )


def test_hollerith_only_spaces(f2003_create, monkeypatch):
    """Check that a hollerith item is parsed correctly in the presence of
    additional spaces. We do this as a hollerith item must have the
    correct number of characters so any spaces that are not required
    must be removed.

    """
    from fparser.two import utils

    monkeypatch.setattr(utils, "EXTENSIONS", ["hollerith"])
    myinput = "  3Habc  ,  2hab  "
    ast = Format_Item_List(myinput)
    assert str(ast) == "3Habc, 2Hab"
    assert repr(ast) == (
        "Format_Item_List(',', (Hollerith_Item('abc'), " "Hollerith_Item('ab')))"
    )


def test_hollerith_omitted_comma_before(f2003_create, monkeypatch):
    """Check that a hollerith item is parsed correctly when preceeded by a
    slash or colon without a separating comma.

    """

    monkeypatch.setattr(utils, "EXTENSIONS", ["hollerith"])
    for item in ["/", ":"]:
        myinput = f"{item}3Habc"
        ast = Format_Item_List(myinput)
        assert str(ast) == f"{item}, 3Habc"
        assert repr(ast) == (
            f"Format_Item_List(',', (Control_Edit_Desc(None, '{item}'), "
            "Hollerith_Item('abc')))"
        )


def test_hollerith_omitted_comma_after(f2003_create, monkeypatch):
    """Check that a hollerith item is parsed correctly when followed by a
    slash or colon without a separating comma.

    """

    monkeypatch.setattr(utils, "EXTENSIONS", ["hollerith"])
    for item in ["/", ":"]:
        myinput = f"3Habc{item}"
        ast = Format_Item_List(myinput)
        assert str(ast) == f"3Habc, {item}"
        assert repr(ast) == (
            "Format_Item_List(',', (Hollerith_Item('abc'), "
            f"Control_Edit_Desc(None, '{item}')))"
        )


def test_hollerith_trailing_space(f2003_create, monkeypatch):
    """Check that a hollerith item is parsed correctly at the end of a list
    when it contains a trailing space.

    """

    monkeypatch.setattr(utils, "EXTENSIONS", ["hollerith"])
    myinput = "4Habc "
    ast = Format_Item_List(myinput)
    assert str(ast) == "4Habc "
    assert repr(ast) == "Format_Item_List(',', (Hollerith_Item('abc '),))"


def test_errors(f2003_create, monkeypatch):
    """test some list errors. Individual item errors will be picked up by
    the subclasses.

    """
    from fparser.two import utils

    monkeypatch.setattr(utils, "EXTENSIONS", ["hollerith"])
    for myinput in [
        None,
        "",
        "  ",
        "E2.2  2Hab",
        "E2.2, E2.2 E2.2",
        "2Hab,2Ha,2Hab",
        "2Hab,2Hab x,2Hab",
        "2Hab,2Hab,2Ha",
        "2Hab,2Hab,2Hab x",
    ]:
        with pytest.raises(NoMatchError):
            _ = Format_Item_List(myinput)
