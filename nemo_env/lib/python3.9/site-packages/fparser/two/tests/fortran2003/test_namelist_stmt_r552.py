# Copyright (c) 2023 Science and Technology Facilities Council.

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

"""Test Fortran 2003 rule R552 : This file tests the support for the
NAMELIST statement.

"""
import pytest

from fparser.two.Fortran2003 import Namelist_Stmt
from fparser.two.utils import NoMatchError


@pytest.mark.parametrize(
    "code",
    [
        "",
        "namelost",
        "namelist",
        "namelist x",
        "namelist x/",
        "namelist /x",
        "namelist /x/ y /",
    ],
)
def test_match_errors(code):
    """Test that the match method returns None when the supplied code is
    invalid. Also check that this results in a NoMatchError for an
    instance of the class.

    """
    assert not Namelist_Stmt.match(code)
    with pytest.raises(NoMatchError):
        _ = Namelist_Stmt(code)


def test_simple():
    """Test that a namelist with a single name and list is matched and
    that the tostr() method outputs the resultant code as
    expected. Also check that the namelist keyword matching is case
    insensitive and that leading and trailing spaces are supported.

    """
    result = Namelist_Stmt(" NamelisT /x/ a ")
    assert isinstance(result, Namelist_Stmt)
    assert result.tostr() == "NAMELIST /x/ a"


def test_multi():
    """Test that multiple names and lists are matched, with and without a
    comma separator and that the tostr() method outputs the resultant
    code as expected.

    """
    result = Namelist_Stmt("namelist /x/ a, /y/ b,c /z/ d")
    assert isinstance(result, Namelist_Stmt)
    assert result.tostr() == "NAMELIST /x/ a, /y/ b, c, /z/ d"
