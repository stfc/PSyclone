# Copyright (c) 2020 Science and Technology Facilities Council.
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

"""Test Fortran 2003 rule R465: this file tests that both (/.../) and
[...] are supported when specifying an array constructor.

"""

import pytest
from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("left, right", [("(/", "/)"), ("[", "]")])
def test_brackets_array_constructor(left, right):
    """Test parsing of array constructor specified with both valid types of
    bracket."""
    fcode = "array = {0} 1, 2, 3{1}".format(left, right)
    reader = FortranStringReader(fcode)
    ast = Fortran2003.Assignment_Stmt(reader)
    assert isinstance(ast, Fortran2003.Assignment_Stmt)
    assert isinstance(ast.children[2], Fortran2003.Array_Constructor)
    assert isinstance(ast.children[2].children[1], Fortran2003.Ac_Value_List)
    assert "array = {0}1, 2, 3{1}".format(left, right) in str(ast)
    # Invalid content between valid brackets
    fcode = "array = {0}call hello(){1}".format(left, right)
    reader = FortranStringReader(fcode)
    ast = Fortran2003.Assignment_Stmt(reader)
    assert ast is None


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("example", ["[1,2/)", "[1,2[]", "(/1,3]", "(/1,3)"])
def test_array_constructor_no_match(example):
    """Check that incorrect constructors are not matched."""
    fcode = "array = " + example
    reader = FortranStringReader(fcode)
    ast = Fortran2003.Assignment_Stmt(reader)
    assert ast is None
