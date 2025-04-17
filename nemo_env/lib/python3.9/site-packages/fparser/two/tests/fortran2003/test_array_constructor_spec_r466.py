# Copyright (c) 2020-2021 Science and Technology Facilities Council.
#
# All rights reserved.
#
# Modifications made as part of the fparser project are distributed
# under the following license:
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
#
# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.
#
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

"""Test Fortran 2003 rule R466: this file tests the support for the
various forms of array constructor specifier.

Note that the tostr method is not yet tested.

"""

import pytest
from fparser.two.utils import NoMatchError
from fparser.two import Fortran2003


@pytest.mark.usefixtures("f2003_create")
def test_zero_size_array_constructor():
    """Test that we can parse a valid, zero-size array constructor."""
    fcode = "integer ::"
    ast = Fortran2003.Ac_Spec(fcode)
    assert isinstance(ast, Fortran2003.Ac_Spec)
    assert isinstance(ast.children[0], Fortran2003.Intrinsic_Type_Spec)


def test_int_literals_array_constructor():
    """Test when a simple list of integer literals is provided as the
    content of the constructor."""
    fcode = "1, 2, 3"
    ast = Fortran2003.Ac_Spec(fcode)
    assert isinstance(ast, Fortran2003.Ac_Value_List)


@pytest.mark.usefixtures("fake_symbol_table")
def test_expr_list_array_constructor():
    """Test when the provided content consists of expressions."""
    fcode = "ACOS(-1.0), SIN(1.0), 1.0+3.0"
    ast = Fortran2003.Ac_Spec(fcode)
    assert isinstance(ast, Fortran2003.Ac_Value_List)


@pytest.mark.usefixtures("f2003_create")
def test_array_spec_char_len():
    """Test with a specifier that specifies a length type parameter."""
    fcode = "CHARACTER(LEN=7) :: 'Takata', 'Tanaka', 'Hayashi'"
    ast = Fortran2003.Ac_Spec(fcode)
    assert isinstance(ast, Fortran2003.Ac_Spec)
    assert isinstance(ast.children[0], Fortran2003.Intrinsic_Type_Spec)
    assert "CHARACTER(LEN = 7) :: 'Takata', 'Tanaka', 'Hayashi'" in str(ast)


@pytest.mark.usefixtures("f2003_create")
def test_array_spec_no_match():
    """Check that incorrect content is not matched."""
    fcode = "call hello()"
    with pytest.raises(NoMatchError):
        Fortran2003.Ac_Spec(fcode)
