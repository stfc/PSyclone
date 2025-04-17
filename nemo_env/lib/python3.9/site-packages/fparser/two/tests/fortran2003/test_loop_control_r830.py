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

"""Test Fortran 2003 rule R830 : This file tests the support for the
loop-control rule.

"""
import pytest
from fparser.two.Fortran2003 import Loop_Control
from fparser.two.utils import NoMatchError


def test_start_end_space_while():
    """Test that there is a match if the string contains white space at
    the start and end and that the tostr() output is as expected. Also
    test matching to the while form of this rule.

    """
    result = Loop_Control(" while (.true.) ")
    assert isinstance(result, Loop_Control)
    assert str(result) == "WHILE (.TRUE.)"


def test_delim():
    """Test that there is a match if the string contains an optional
    delimiter at the start and that the tostr() output is as expected.

    """
    result = Loop_Control(" , while (.true.) ")
    assert isinstance(result, Loop_Control)
    assert str(result) == ", WHILE (.TRUE.)"


def test_repmap():
    """Test matching when the while logical expresssion contains
    brackets. This tests the use of the string_replace_map() function.

    """
    result = Loop_Control(" , while (((a .or. b) .and. (c .or. d))) ")
    assert isinstance(result, Loop_Control)
    assert str(result) == ", WHILE (((a .OR. b) .AND. (c .OR. d)))"


def test_counter():
    """Test matching to the counter form of this rule and that the tostr()
    output is as expected.

    """
    # Lower-bound and upper-bound only
    result = Loop_Control("idx = start,stop")
    assert isinstance(result, Loop_Control)
    assert str(result) == "idx = start, stop"
    # Lower-bound, upper-bound and step
    result = Loop_Control("idx = start,stop,step")
    assert isinstance(result, Loop_Control)
    assert str(result) == "idx = start, stop, step"
    # Bounds are integer expressions
    result = Loop_Control("idx = ((s+2)-q),(p*m)/4,a+b+c")
    assert isinstance(result, Loop_Control)
    assert str(result) == "idx = ((s + 2) - q), (p * m) / 4, a + b + c"


@pytest.mark.parametrize(
    "string",
    [
        "",
        " ",
        ": while(.true.)",
        "whil (.true.)",
        "while .true.",
        "while ()",
        "while( )",
        "while (.true ",
        "while ())",
        "while('text')",
        " == ",
        " = ",
        "idx=",
        "=1,2",
        "idx=1",
        "idx=1,2,3,4",
        "1=1,2",
        "idx=1,.false.",
    ],
)
def test_invalid(string):
    """Test that there is no match for various invalid input strings."""
    with pytest.raises(NoMatchError):
        _ = Loop_Control(string)
