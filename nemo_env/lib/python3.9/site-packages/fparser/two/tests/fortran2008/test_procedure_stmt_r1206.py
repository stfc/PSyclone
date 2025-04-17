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

"""Test Fortran 2008 rule R1206

    procedure-stmt is [ MODULE ] PROCEDURE [ :: ] procedure-name-list

"""

import pytest
from fparser.two.Fortran2008 import Procedure_Stmt
from fparser.two.utils import NoMatchError


def test_start_space():
    """Test that there is a match if the string contains white space at
    the start and that the tostr() output is as expected.

    """
    result = Procedure_Stmt(" procedure dummy")
    assert isinstance(result, Procedure_Stmt)
    assert str(result) == "PROCEDURE dummy"


def test_module():
    """Test that there is a match if the string contains the optional
    MODULE keyword and that the tostr() output is as expected.

    """
    result = Procedure_Stmt(" module procedure dummy")
    assert isinstance(result, Procedure_Stmt)
    assert str(result) == "MODULE PROCEDURE dummy"


def test_colons():
    """Test that there is a match if the string contains optional :: after
    the procedure keyword and that the tostr() output is as expected.

    """
    result = Procedure_Stmt(" module procedure :: dummy")
    assert isinstance(result, Procedure_Stmt)
    assert str(result) == "MODULE PROCEDURE :: dummy"


@pytest.mark.parametrize(
    "string",
    [
        "procedur dummy",
        "modul procedure dummy",
        "procedure : dummy",
        "procedure ",
        "procedure :: ",
    ],
)
def test_invalid(string):
    """Test that there is no match for various invalid input strings."""
    with pytest.raises(NoMatchError):
        _ = Procedure_Stmt(string)
