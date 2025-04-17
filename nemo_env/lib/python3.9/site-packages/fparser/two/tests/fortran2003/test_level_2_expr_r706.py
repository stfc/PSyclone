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

"""Test Fortran 2003 rule R706 : This file tests support for the
Level_2_Expr class.

"""

import pytest
from fparser.two.Fortran2003 import Level_2_Expr
from fparser.two.parser import ParserFactory

# This is required to setup the fortran2003 classes (when matching
# with Level_2_Expr directly)
_ = ParserFactory().create(std="f2003")


@pytest.mark.parametrize(
    "string,str_repr",
    [
        (
            "a + c ** b",
            "Level_2_Expr(Name('a'), '+', Mult_Operand(Name('c'), '**'," " Name('b')))",
        ),
        (
            "a + c * b",
            "Level_2_Expr(Name('a'), '+', Add_Operand(Name('c'), '*'," " Name('b')))",
        ),
        (
            "a + 1.0E-10 * b",
            "Level_2_Expr(Name('a'), '+', Add_Operand("
            "Real_Literal_Constant('1.0E-10', None), '*', Name('b')))",
        ),
        (
            "a + b * 1.0E-10",
            "Level_2_Expr(Name('a'), '+', Add_Operand("
            "Name('b'), '*', Real_Literal_Constant('1.0E-10', None)))",
        ),
        (
            "a + 1.0d+10 * b",
            "Level_2_Expr(Name('a'), '+', Add_Operand("
            "Real_Literal_Constant('1.0D+10', None), '*', Name('b')))",
        ),
        (
            "- .0E-10 * b - a",
            "Level_2_Expr(Level_2_Unary_Expr('-', Add_Operand("
            "Real_Literal_Constant('.0E-10', None), '*', Name('b'))), '-', "
            "Name('a'))",
        ),
    ],
)
def test_level2_exprn(string, str_repr):
    """Test for a successful match with a valid level-2 expression."""
    result = Level_2_Expr(string)
    assert str(result).lower() == string.lower()
    assert repr(result) == str_repr
