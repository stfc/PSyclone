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

"""Test Fortran 2003 rule R727 : This file tests the support for a
Fortran integer expression.

"""

import pytest
from fparser.two.Fortran2003 import Int_Expr, Int_Literal_Constant, Level_2_Expr
from fparser.two.utils import NoMatchError


@pytest.mark.usefixtures("f2003_create")
def test_simple_case():
    """Test that a simple expression gives the expected result."""
    result = Int_Expr("1")
    assert isinstance(result, Int_Literal_Constant)
    assert str(result) == "1"
    assert repr(result) == "Int_Literal_Constant('1', None)"


@pytest.mark.usefixtures("f2003_create")
def test_complicated_case():
    """Test that a more complicated expression gives the expected
    result.

    """
    result = Int_Expr("a*2+array(b)-w")
    assert isinstance(result, Level_2_Expr)
    assert str(result) == ("a * 2 + array(b) - w")
    assert repr(result).replace("u'", "'") == (
        "Level_2_Expr(Level_2_Expr(Add_Operand(Name('a'), '*', "
        "Int_Literal_Constant('2', None)), '+', Part_Ref(Name('array'), "
        "Section_Subscript_List(',', (Name('b'),)))), '-', Name('w'))"
    )


@pytest.mark.parametrize(
    "string", [".true.", "b'1010'", "o'7070'", "h'f0f0'", "1.0", "(1.0,1.0)", "'hello'"]
)
@pytest.mark.usefixtures("f2003_create")
def test_c708(string):
    """Check that invalid literal constants do not match. Note, there are
    many other cases that are not currently checked in fparser.

    In theory -1.0 should become a Signed_Real_Literal_Constant and
    then fail to match. However the "-" is treated as a unary
    expression so this never happens.

    """
    with pytest.raises(NoMatchError):
        _ = Int_Expr(string)
