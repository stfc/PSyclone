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

"""Test Fortran 2003 rule R728 : This file tests the support for a
Fortran numeric expression.

"""

import pytest
from fparser.two.Fortran2003 import (
    Numeric_Expr,
    Int_Literal_Constant,
    Real_Literal_Constant,
    Complex_Literal_Constant,
    Level_2_Expr,
)
from fparser.two.utils import NoMatchError


@pytest.mark.usefixtures("f2003_create")
def test_simple_case_int():
    """Test that a simple integer expression gives the expected result."""
    result = Numeric_Expr("1")
    assert isinstance(result, Int_Literal_Constant)
    assert str(result) == "1"
    assert repr(result) == "Int_Literal_Constant('1', None)"


@pytest.mark.usefixtures("f2003_create")
def test_simple_case_real():
    """Test that a simple real expression gives the expected result."""
    result = Numeric_Expr("1.0")
    assert isinstance(result, Real_Literal_Constant)
    assert str(result) == "1.0"
    assert repr(result) == "Real_Literal_Constant('1.0', None)"


@pytest.mark.usefixtures("f2003_create")
def test_simple_case_complex():
    """Test that a simple complex expression gives the expected result."""
    result = Numeric_Expr("(1.0,-2.0)")
    assert isinstance(result, Complex_Literal_Constant)
    assert str(result) == "(1.0, -2.0)"
    assert repr(result) == (
        "Complex_Literal_Constant(Signed_Real_Literal_Constant('1.0', None), "
        "Signed_Real_Literal_Constant('-2.0', None))"
    )


@pytest.mark.usefixtures("f2003_create")
def test_complicated_case_int():
    """Test that a more complicated integer expression gives the expected
    result.

    """
    result = Numeric_Expr("a*2+array(b)-w")
    assert isinstance(result, Level_2_Expr)
    assert str(result) == ("a * 2 + array(b) - w")
    assert repr(result).replace("u'", "'") == (
        "Level_2_Expr(Level_2_Expr(Add_Operand(Name('a'), '*', "
        "Int_Literal_Constant('2', None)), '+', Part_Ref(Name('array'), "
        "Section_Subscript_List(',', (Name('b'),)))), '-', Name('w'))"
    )


@pytest.mark.usefixtures("f2003_create")
def test_complicated_case_real():
    """Test that a more complicated real expression gives the expected
    result.

    """
    result = Numeric_Expr("a*2.0+array(b)/w")
    assert isinstance(result, Level_2_Expr)
    assert str(result) == ("a * 2.0 + array(b) / w")
    assert repr(result).replace("u'", "'") == (
        "Level_2_Expr(Add_Operand(Name('a'), '*', Real_Literal_Constant("
        "'2.0', None)), '+', Add_Operand(Part_Ref(Name('array'), "
        "Section_Subscript_List(',', (Name('b'),))), '/', Name('w')))"
    )


@pytest.mark.usefixtures("f2003_create")
def test_complicated_case_complex():
    """Test that a more complicated complex expression gives the expected
    result.

    """
    result = Numeric_Expr("a*(2.0,3.0)+array(b)/w")
    assert isinstance(result, Level_2_Expr)
    assert str(result) == ("a * (2.0, 3.0) + array(b) / w")
    assert repr(result).replace("u'", "'") == (
        "Level_2_Expr(Add_Operand(Name('a'), '*', Complex_Literal_Constant("
        "Signed_Real_Literal_Constant('2.0', None), "
        "Signed_Real_Literal_Constant('3.0', None))), '+', Add_Operand("
        "Part_Ref(Name('array'), Section_Subscript_List(',', (Name('b'),))), "
        "'/', Name('w')))"
    )


@pytest.mark.parametrize(
    "string", [".true.", "b'1010'", "o'7070'", "h'f0f0'", "'hello'"]
)
@pytest.mark.usefixtures("f2003_create")
def test_c709(string):
    """Check that invalid literal constants do not match. Note, there are
    many other cases that are not currently checked in fparser.

    In theory -1.0 should become a Signed_Real_Literal_Constant and
    then fail to match. However the "-" is treated as a unary
    expression so this never happens.

    """
    with pytest.raises(NoMatchError):
        _ = Numeric_Expr(string)
