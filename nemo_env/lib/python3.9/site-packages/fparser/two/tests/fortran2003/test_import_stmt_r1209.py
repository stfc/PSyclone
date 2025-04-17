# Copyright (c) 2020 Science and Technology Facilities Council.

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

"""Test Fortran 2003 rule R1209 : This file tests the support for an
import statement.

"""

import pytest
from fparser.two.Fortran2003 import Import_Stmt
from fparser.two.utils import NoMatchError


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize(
    "example,result",
    [
        ("IMPORT", "IMPORT"),
        ("iMpOrT", "IMPORT"),
        ("  IMPORT  ", "IMPORT"),
        ("IMPORT name1", "IMPORT :: name1"),
        ("IMPORT name1, name2", "IMPORT :: name1, name2"),
        ("IMPORT :: name1", "IMPORT :: name1"),
        ("IMPORT :: name1, name2", "IMPORT :: name1, name2"),
        ("IMPORT::name1,name2", "IMPORT :: name1, name2"),
        ("  IMPORT  ::  name1  ,  name2  ", "IMPORT :: name1, name2"),
    ],
)
def test_match_valid(example, result):
    """Test that valid input is parsed correctly"""

    obj = Import_Stmt(example)
    assert isinstance(obj, Import_Stmt), repr(obj)
    assert str(obj) == result


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize(
    "example", ["", "  ", "IMPOR", "IMPORT : name1", "IMPORT ::", "IMPORTname1"]
)
def test_match_invalid(example):
    """Test that invalid input raises an exception"""

    with pytest.raises(NoMatchError) as excinfo:
        _ = Import_Stmt(example)
    assert "Import_Stmt: '{0}'".format(example) in str(excinfo.value)
