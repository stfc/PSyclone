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

"""Test Fortran 2003 rule R705 : This file tests support for the
Add_Operand class.

"""

import pytest
from fparser.two.utils import NoMatchError
from fparser.two.Fortran2003 import Add_Operand, Level_2_Expr
from fparser.two.parser import ParserFactory

# This is required to setup the fortran2003 classes (when matching
# with Add_Operand)
_ = ParserFactory().create(std="f2003")


@pytest.mark.parametrize(
    "string,str_repr",
    [
        ("a ** b", "Mult_Operand(Name('a'), '**', Name('b'))"),
        (".DEFINED. a", "Level_1_Expr('.DEFINED.', Name('a'))"),
        ("1.0", "Real_Literal_Constant('1.0', None)"),
        (
            "(a + b)",
            "Parenthesis('(', Level_2_Expr(Name('a'), '+', " "Name('b')), ')')",
        ),
    ],
)
def test_mult(string, str_repr):
    """Test for a successful match with a valid mult-operand string"""
    result = Add_Operand(string)
    assert str(result) == string
    assert repr(result) == str_repr


def test_mult_fail():
    """Test for a failure to match an invalid multi-operand string"""
    with pytest.raises(NoMatchError) as info:
        Add_Operand("a .and. b")
    assert "Add_Operand: 'a .and. b'" in str(info.value)


@pytest.mark.parametrize(
    "string,str_repr",
    [
        ("a * b", "Add_Operand(Name('a'), '*', Name('b'))"),
        (
            "a * 1.0E-3",
            "Add_Operand(Name('a'), '*', " "Real_Literal_Constant('1.0E-3', None))",
        ),
        ("a / b", "Add_Operand(Name('a'), '/', Name('b'))"),
        (
            "a * b * c",
            "Add_Operand(Add_Operand(Name('a'), '*', Name('b')), " "'*', Name('c'))",
        ),
        (
            "a * b / c",
            "Add_Operand(Add_Operand(Name('a'), '*', Name('b')), " "'/', Name('c'))",
        ),
    ],
)
def test_add_operand(string, str_repr):
    """Test for a successful match with a valid add_operand"""
    result = Add_Operand(string)
    assert str(result) == string
    assert repr(result) == str_repr


@pytest.mark.parametrize("string", ["a + b", "_ * b", "a * _", "a * b + c"])
def test_add_operand_fail(string):
    """Test for a failure to match an invalid add-operand string"""
    with pytest.raises(NoMatchError) as info:
        Add_Operand(string)
    assert "Add_Operand: '{0}'".format(string) in str(info.value)
