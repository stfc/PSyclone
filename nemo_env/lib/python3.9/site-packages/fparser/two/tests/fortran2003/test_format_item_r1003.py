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

"""Test Fortran 2003 R1003: This file tests the support for a format item.

Note, the hollerith format was deprecated in Fortran77 and removed in
Fortran95. However, Fortran compilers still support it.

"""

import pytest
from fparser.two.Fortran2003 import Format_Item
from fparser.two.utils import NoMatchError, InternalError
from fparser.two import utils


def test_data_edit_descriptor(f2003_create):
    """Check that basic format specifications are parsed correctly for a
    data edit descriptor. The description is tested in more detail by
    the associated class.

    """
    # No R
    for my_input in ["F2.2", " F2.2 "]:
        ast = Format_Item(my_input)
        assert my_input.replace(" ", "") in str(ast)
        assert repr(ast) == (
            "Format_Item(None, Data_Edit_Desc_C1002('F',"
            " Digit_String('2', None), Int_Literal_Constant("
            "'2', None), None))"
        )
    # R
    for my_input in ["2F2.2", " 2 F2.2 "]:
        ast = Format_Item(my_input)
        assert my_input.replace(" ", "") in str(ast)
        assert repr(ast) == (
            "Format_Item(Digit_String('2', None), Data_Edit"
            "_Desc_C1002('F', Digit_String('2', None), "
            "Int_Literal_Constant('2', None), None))"
        )
    # Multi-R
    for my_input in ["22F2.2", " 2 2 F2.2 "]:
        ast = Format_Item(my_input)
        assert my_input.replace(" ", "") in str(ast)
        assert repr(ast) == (
            "Format_Item(Digit_String('22', None), Data_Edit"
            "_Desc_C1002('F', Digit_String('2', None), "
            "Int_Literal_Constant('2', None), None))"
        )


def test_control_edit_descriptor(f2003_create):
    """Check that basic format specifications are parsed correctly for a
    control edit descriptor. The description is tested in more detail by
    the associated class.

    """
    for my_input in ["2P", " 2P "]:
        ast = Format_Item(my_input)
        assert my_input.strip() in str(ast)
        assert repr(ast) == (
            "Control_Edit_Desc(Signed_Int_Literal_Constant(" "'2', None), 'P')"
        )


def test_char_edit_descriptor(f2003_create):
    """Check that basic format specifications are parsed correctly for a
    char string edit descriptor. The description is tested in more
    detail by the associated class.

    """
    for my_input in ["'hello'", " 'hello' "]:
        ast = Format_Item(my_input)
        assert my_input.strip() in str(ast)
        assert repr(ast).replace("u", "") == "Char_Literal_Constant(\"'hello'\", None)"


def test_format_list_descriptor(f2003_create):
    """Check that basic format specifications are parsed correctly for a
    format item list descriptor. The description is tested in more
    detail by the associated class.

    """
    # No R
    for my_input in ["(F2.2)", " (F2.2) ", " ( F2.2 ) "]:
        ast = Format_Item(my_input)
        assert my_input.replace(" ", "") in str(ast)
        assert repr(ast) == (
            "Format_Item(None, Format_Item_List(',', ("
            "Format_Item(None, Data_Edit_Desc_C1002('F', "
            "Digit_String('2', None), Int_Literal_Constant("
            "'2', None), None)),)))"
        )
    # R
    for my_input in ["2(F2.2)", " 2 (F2.2) ", " 2 ( F2.2 ) "]:
        ast = Format_Item(my_input)
        assert my_input.replace(" ", "") in str(ast)
        assert repr(ast) == (
            "Format_Item(Digit_String('2', None), Format_"
            "Item_List(',', (Format_Item(None, Data_Edit_"
            "Desc_C1002('F', Digit_String('2', None), Int_"
            "Literal_Constant('2', None), None)),)))"
        )


def test_format_list_descriptor_trailing_space(f2003_create, monkeypatch):
    """Check that format item list descriptors preserve trailing space."""

    monkeypatch.setattr(utils, "EXTENSIONS", ["hollerith"])
    myinput = "(4Habc )"
    ast = Format_Item(myinput)
    assert str(ast) == myinput
    assert repr(ast) == (
        "Format_Item(None, Format_Item_List(',', (Hollerith_Item('abc '),)))"
    )


def test_hollerith_item(f2003_create, monkeypatch):
    """Check that a hollerith item is parsed correctly. The description is
    tested in more detail by the associated class.

    """
    from fparser.two import utils

    monkeypatch.setattr(utils, "EXTENSIONS", ["hollerith"])
    for my_input in ["2H12", " 2H12 "]:
        ast = Format_Item(my_input)
        assert my_input.strip() in str(ast)
        assert repr(ast) == ("Hollerith_Item('12')")


def test_errors(f2003_create):
    """test some list errors. Individual item errors will be picked up by
    the subclasses.

    """
    for my_input in [None, "", "  ", "2", "2  ", "(", ")"]:
        with pytest.raises(NoMatchError):
            _ = Format_Item(my_input)


def test_internal_errors1(f2003_create, monkeypatch):
    """Check that an internal error is raised if the length of the Items
    list is not 2 as the str() method assumes that it is.

    """
    line = "2F2.2"
    ast = Format_Item(line)
    monkeypatch.setattr(ast, "items", [None, None, None])
    with pytest.raises(InternalError) as excinfo:
        str(ast)
    assert "should be of length 2 but found '3'" in str(excinfo.value)


def test_internal_errors2(f2003_create, monkeypatch):
    """Check that an internal error is raised if the descriptor item
    (first entry of items) is empty or None as the str() method
    assumes that it has content.

    """

    ast = Format_Item("F2.2")
    monkeypatch.setattr(ast, "items", [ast.items[0], None])
    with pytest.raises(InternalError) as excinfo:
        str(ast)
    assert (
        "items list second entry should be a valid descriptor but it "
        "is empty or None"
    ) in str(excinfo.value)
