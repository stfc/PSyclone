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

"""Test Fortran 2003 rule R724 : This file tests the support for a
Fortran logical expression.

"""

import pytest
from fparser.two.Fortran2003 import (
    Logical_Expr,
    Logical_Literal_Constant,
    Equiv_Operand,
)
from fparser.two.utils import NoMatchError


@pytest.mark.usefixtures("f2003_create")
def test_simple_case():
    """Test that a simple expression gives the expected result."""
    result = Logical_Expr(".true.")
    assert isinstance(result, Logical_Literal_Constant)
    assert str(result) == ".TRUE."
    assert repr(result) == "Logical_Literal_Constant('.TRUE.', None)"


@pytest.mark.usefixtures("f2003_create", "fake_symbol_table")
def test_complicated_case():
    """Test that a more complicated expression gives the expected
    result.

    """
    result = Logical_Expr("(f0 .lt. f1) .and. abs(x1-x0) .gt. abs(x2) .or.  .not. root")
    assert isinstance(result, Equiv_Operand)
    assert str(result) == (
        "(f0 .LT. f1) .AND. ABS(x1 - x0) .GT. ABS(x2) " ".OR. .NOT. root"
    )
    assert repr(result).replace("u'", "'") == (
        "Equiv_Operand(Or_Operand(Parenthesis('(', Level_4_Expr(Name('f0'), "
        "'.LT.', Name('f1')), ')'), '.AND.', Level_4_Expr(Intrinsic_Function"
        "_Reference(Intrinsic_Name('ABS'), Actual_Arg_Spec_List(',', (Level_"
        "2_Expr(Name('x1'), '-', Name('x0')),))), '.GT.', Intrinsic_Function"
        "_Reference(Intrinsic_Name('ABS'), Actual_Arg_Spec_List(',', (Name('"
        "x2'),))))), '.OR.', And_Operand('.NOT.', Name('root')))"
    )


@pytest.mark.parametrize(
    "string", ["1", "b'1010'", "o'7070'", "h'f0f0'", "1.0", "(1.0,1.0)", "'hello'"]
)
@pytest.mark.usefixtures("f2003_create")
def test_c705(string):
    """Check that invalid literal constants do not match. Note, there are
    many other cases that are not currently checked in fparser.

    In theory -1 should become a signed_int_literal_constant and then
    fail to match. Similarly -1.0 should become a
    Signed_Real_Literal_Constant and then fail to match. However the
    "-" is treated as a unary expression so this never happens.

    """
    with pytest.raises(NoMatchError):
        _ = Logical_Expr(string)
