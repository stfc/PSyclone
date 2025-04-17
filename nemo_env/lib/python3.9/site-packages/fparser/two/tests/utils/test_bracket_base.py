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

"""File containing unit tests for the BracketBase baseclass in
utils.py"""

import pytest
from fparser.two.utils import BracketBase, InternalError
from fparser.two.Fortran2003 import Name, Hollerith_Item


def test_brackets():
    """Test the bracketsbase match method with no content within the
    brackets.

    """
    for lhs, rhs in [
        ("(", ")"),
        (" ( ", " ) "),
        ("[", "]"),
        ("A", "A"),
        ("([", "])"),
        ("{{[(", ")]}}"),
        ("  {  {  [  (  ", " ) ] } } "),
    ]:
        brackets = lhs + rhs
        input_text = brackets.replace(" ", "")
        result = BracketBase.match(brackets, None, input_text, require_cls=False)
        assert str(result) == "('{0}', None, '{1}')".format(
            lhs.replace(" ", ""), rhs.replace(" ", "")
        )


def test_input_too_short():
    """Test the bracketsbase match method returns None when the input
    string is shorter than the brackets.

    """
    brackets = "(())"
    input_text = "(H)"
    result = BracketBase.match(brackets, None, input_text, require_cls=False)
    assert result is None


def test_cls():
    """Test the bracketsbase match method with content within the brackets
    for both require_cls is False and True. require_cls indicates
    whether content should be expected within the brackets (and the
    match failing if not), or whether it is optional. The actual
    contents are passed on to the specified class ('Name' in this
    case) to match (or not), hence the name require_cls.

    """
    for require in [False, True]:
        for lhs, rhs in [
            ("(", ")"),
            (" ( ", " ) "),
            ("[", "]"),
            ("A", "A"),
            ("([", "])"),
            ("{{[(", ")]}}"),
        ]:
            brackets = lhs + rhs
            input_text = lhs + "hello" + rhs
            result = BracketBase.match(brackets, Name, input_text, require_cls=require)
            assert str(result) == "('{0}', Name('hello'), '{1}')".format(
                lhs.replace(" ", ""), rhs.replace(" ", "")
            )


@pytest.mark.parametrize("require", [False, True])
@pytest.mark.parametrize(
    "lhs, rhs",
    [
        ("(", ")"),
        (" ( ", " ) "),
        ("[", "]"),
        ("A", "A"),
        ("([", "])"),
        ("{{[(", ")]}}"),
    ],
)
def test_trailing_whitespace(require, lhs, rhs):
    """Test that the BracketBase match method passes any trailing whitespace
    within the brackets to the specified class.
    """
    brackets = lhs + rhs
    input_text = lhs + "4habc " + rhs
    result = BracketBase.match(
        brackets, Hollerith_Item, input_text, require_cls=require
    )
    assert str(result) == "('{0}', Hollerith_Item('abc '), '{1}')".format(
        lhs.replace(" ", ""), rhs.replace(" ", "")
    )


def test_brackets_error1():
    """Test the bracketbase match method returns None if the brackets are
    invalid."""

    for brackets in [None, "", "  ", "(", ")", "[[]"]:
        input_text = "()"
        result = BracketBase.match(brackets, None, input_text, require_cls=False)
        assert result is None


def test_brackets_error2():
    """Test the bracketbase match method returns the brackets if class contents
    are expected but there are none.

    """
    result = BracketBase.match("()", None, "()", require_cls=True)
    assert result is None


def test_brackets_error3():
    """Test the bracketbase match method returns None if class contents
    are expected (as the default is require_cls=True) but there are
    none.

    """
    result = BracketBase.match("()", None, "()")
    assert result is None


def test_tostr(monkeypatch):
    """It is not possible to instantiate BracketBase directly so we create
    a class that uses BracketBase (Format_Specification) and then test
    the tostr() method from it. This test checks that an internal
    error is raised if the size of the internal items list is
    incorrect.

    """
    from fparser.two.Fortran2003 import Format_Specification

    ast = Format_Specification("()")
    monkeypatch.setattr(ast, "items", [None])
    with pytest.raises(InternalError) as excinfo:
        _ = str(ast)
    assert "Class BracketBase method tostr()" in str(excinfo.value)
    assert "tostr() has '1' items, but expecting 3" in str(excinfo.value)


def test_tostr_invalid2(monkeypatch):
    """It is not possible to instantiate BracketBase directly so we create
    a class that uses BracketBase (Format_Specification) and then test
    the tostr() method from it. This test checks that an internal
    error is raised if entry 0 of the internal items list is empty or
    None.

    """
    from fparser.two.Fortran2003 import Format_Specification

    ast = Format_Specification("()")
    monkeypatch.setattr(ast, "items", [None, ast.items[1], ast.items[2]])
    with pytest.raises(InternalError) as excinfo:
        _ = str(ast)
    assert "Class BracketBase method tostr()" in str(excinfo.value)
    assert (
        "'Items' entry 0 should be a string containing the left hand "
        "bracket but it is empty or None"
    ) in str(excinfo.value)


def test_tostr_invalid3(monkeypatch):
    """It is not possible to instantiate BracketBase directly so we create
    a class that uses BracketBase (Format_Specification) and then test
    the tostr() method from it. This test checks that an internal
    error is raised if entry 2 of the internal items list is empty or
    None.

    """
    from fparser.two.Fortran2003 import Format_Specification

    ast = Format_Specification("()")
    monkeypatch.setattr(ast, "items", [ast.items[0], ast.items[1], None])
    with pytest.raises(InternalError) as excinfo:
        _ = str(ast)
    assert "Class BracketBase method tostr()" in str(excinfo.value)
    assert (
        "'Items' entry 2 should be a string containing the right hand "
        "bracket but it is empty or None"
    ) in str(excinfo.value)
