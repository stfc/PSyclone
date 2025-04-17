# Copyright (c) 2023 Science and Technology Facilities Council

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

"""Test Fortran 2008 rule R818

    loop-control is [ , ] do-variable = scalar-int-expr , scalar-int-expr
                       [ , scalar-int-expr ]
                    or [ , ] WHILE ( scalar-logical-expr )
                    or [ , ] CONCURRENT forall-header

    Extends the Fortran2003 rule R830 with the additional CONCURRENT clause.

"""
import pytest
from fparser.two.Fortran2008 import Loop_Control
from fparser.two.utils import NoMatchError


def test_f2003_match():
    """Test that there is a match if the string contains a F2003 form of
    the rule and that the F2003 tostr() is as expected.

    """
    result = Loop_Control("while (.true.)")
    assert isinstance(result, Loop_Control)
    assert str(result) == "WHILE (.TRUE.)"


def test_start_space():
    """Test that there is a match if the string contains white space at
    the start and end and that the tostr() output is as expected.

    """
    result = Loop_Control(" concurrent (i=1:10) ")
    assert isinstance(result, Loop_Control)
    assert str(result) == "CONCURRENT (i = 1 : 10)"


def test_delim():
    """Test that there is a match if the string contains an options
    delimiter at the start and that the tostr() output is as expected.

    """
    result = Loop_Control(" , concurrent (i=1:10)")
    assert isinstance(result, Loop_Control)
    assert str(result) == ", CONCURRENT (i = 1 : 10)"


@pytest.mark.parametrize(
    "string",
    [
        "",
        ": concurrent (i=1:10)",
        "concurren (i=1:10)",
        "concurrent",
        "concurrent invalid",
    ],
)
def test_invalid(string):
    """Test that there is no match for various invalid input strings."""
    with pytest.raises(NoMatchError):
        _ = Loop_Control(string)
