# Copyright (c) 2021 Science and Technology Facilities Council

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

"""File containing unit tests for the BinaryOpBase baseclass in
utils.py

"""

import pytest
from fparser.two.utils import BinaryOpBase, NoMatchError
from fparser.two.Fortran2003 import (
    Name,
    Real_Literal_Constant,
    Int_Literal_Constant,
    Complex_Literal_Constant,
    Level_4_Expr,
)
import fparser.two.pattern_tools as fparser_patterns
from fparser.two.parser import ParserFactory

# This is required to setup the fortran2003 classes (when matching
# with Complex_Literal_Constant)
_ = ParserFactory().create(std="f2003")


@pytest.mark.parametrize("pattern", ["%", fparser_patterns.percent_op])
@pytest.mark.parametrize("right", [True, False])
def test_binaryopbase_pattern_nomatch(pattern, right):
    """Test the BinaryOpBase match method returns None if the pattern is
    of type 'str' or 'Pattern' and is not found in the string. Check
    with the optional 'right' argument set to True and False.

    """
    string = "ab"
    result = BinaryOpBase.match(None, pattern, None, string, right=right)
    assert result is None


@pytest.mark.parametrize("string", ["%b", "a%"])
def test_binaryopbase_empty_nomatch(string):
    """Test the BinaryOpBase match method returns None if the lhs or the
    rhs of the string is empty.

    """
    pattern = "%"
    result = BinaryOpBase.match(None, pattern, None, string)
    assert result is None


def test_binaryopbase_exclude_nomatch():
    """Test the BinaryOpBase match method returns None if the matching
    pattern is excluded.

    """
    pattern = "%"
    string = "a%b"
    result = BinaryOpBase.match(
        None, pattern, None, string, exclude_op_pattern=fparser_patterns.percent_op
    )
    assert result is None


@pytest.mark.parametrize(
    "right,string,expected",
    [
        (True, "1a%1b", "1b"),
        (False, "1a%1b", "1a"),
        (True, "1a%b", "1a"),
        (False, "a%1b", "1b"),
    ],
)
def test_binaryopbase_right_nomatch(right, string, expected):
    """Test the BinaryOpBase match method checks whether classes match
    using the rhs string first and the lhs string second if the
    'right' optional argument is true and vice versa if the 'right'
    optional argument is false.

    """
    lhs = Name
    rhs = Name
    pattern = "%"
    with pytest.raises(NoMatchError) as info:
        _ = BinaryOpBase.match(lhs, pattern, rhs, string, right=right)
    assert "Name: '{0}'".format(expected) in str(info.value)


@pytest.mark.parametrize("pattern", ["%", fparser_patterns.percent_op.named()])
@pytest.mark.parametrize("string", ["a%b", "  a  %  b  "])
def test_binaryopbase_match(pattern, string):
    """Test the BinaryOpBase match method returns the expected results if
    there is a match for a pattern of type str and Pattern. Also check
    that spaces do not affect matching and that they are removed in
    the results.

    """
    lhs = Name
    rhs = Name
    result = BinaryOpBase.match(lhs, pattern, rhs, string)
    assert len(result) == 3
    assert isinstance(result[0], Name)
    assert result[0].string == "a"
    assert isinstance(result[1], str)
    assert result[1] == "%"
    assert isinstance(result[2], Name)
    assert result[2].string == "b"


def test_binaryopbase_addition():
    """Test that an addition involving a numerical constant with an
    exponent containing '+' is parsed correctly.

    """
    string = "a+3.0e+10"
    pattern = fparser_patterns.add_op.named()
    lhs = Name
    rhs = Real_Literal_Constant

    result = BinaryOpBase.match(lhs, pattern, rhs, string)

    assert len(result) == 3
    assert isinstance(result[0], Name)
    assert result[0].string == "a"
    assert isinstance(result[1], str)
    assert result[1] == "+"
    assert isinstance(result[2], Real_Literal_Constant)
    assert result[2].string == "3.0e+10"


@pytest.mark.parametrize(
    "pattern,expected", [("a", "a"), (fparser_patterns.letter.named(), "A")]
)
def test_upper(pattern, expected):
    """Test that the case of the matched pattern is unchanged if the match
    is a string but it is made upper case if it is a Pattern. The
    upper-casing does not seem to serve any purpose so should probably
    be removed, see issue #282.

    """
    string = "1a1"
    lhs = Int_Literal_Constant
    rhs = Int_Literal_Constant

    result = BinaryOpBase.match(lhs, pattern, rhs, string)
    assert result[1] == expected


def test_spaces():
    """Tests that BinaryOpBase matches with spaces in a pattern but
    removes the spaces in the output when there is a match. There
    seems to be no reason to do this so it should probably be removed,
    see issue #282.

    """
    pattern = " a "
    lhs = Int_Literal_Constant
    rhs = Int_Literal_Constant

    string = "1 a1"
    result = BinaryOpBase.match(lhs, pattern, rhs, string)
    assert result is None

    string = "1 a 1"
    result = BinaryOpBase.match(lhs, pattern, rhs, string)
    assert str(result[0]) == "1"
    assert result[1] == "a"
    assert str(result[2]) == "1"

    pattern = fparser_patterns.rel_op.named()
    string = "1 . eq . 1"
    result = BinaryOpBase.match(lhs, pattern, rhs, string)
    assert str(result[0]) == "1"
    assert result[1] == ".EQ."
    assert str(result[2]) == "1"


def test_repmap():
    """Test that repmap is used correctly to remove appropriate patterns
    that might falsely match and that the values are returned
    appropriately if there is a match.

    """
    string = "(a,a)a(a,a)"
    lhs = Complex_Literal_Constant
    rhs = Complex_Literal_Constant
    pattern = "a"
    result = BinaryOpBase.match(lhs, pattern, rhs, string)
    assert str(result[0]) == "(a, a)"
    assert result[1] == "a"
    assert str(result[2]) == "(a, a)"


def test_tostr():
    """Test that the BinaryOpBase tostr method returns the expected
    output. It is not possible to instantiate BinaryOpBase directly as
    the number of arguments expected in the constructor differs from
    the number required in the match (so an error is
    raised). Therefore we use an existing subclass - Level_4_Expr - to
    test.

    """
    string = "x . eq . y"
    obj = Level_4_Expr(string)
    assert isinstance(obj, Level_4_Expr)
    assert obj.tostr() == "x .EQ. y"
