# Copyright (c) 2019 Science and Technology Facilities Council

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

"""Test Fortran 2003 rule R612 : This file tests support for the
Data_Ref class.

"""

import pytest
from fparser.two.utils import NoMatchError
from fparser.two.Fortran2003 import Data_Ref, Part_Ref, Name


def test_valid_sequence(f2003_create):
    """Test that a data_ref object is returned when a valid sequence is
    supplied.

    """
    for string in ["a%b", " a % b ", "a%b%c", "A%B%C"]:
        result = Data_Ref(string)
        assert str(result) == str(result).strip()
        assert isinstance(result, Data_Ref)


def test_single_entry(f2003_create):
    """Test that a data_ref object is not returned when the sequence is
    valid but contains a single entry.

    """
    for string in ["a", " a ", "A"]:
        result = Data_Ref(string)
        assert str(result) == str(result).strip()
        assert isinstance(result, Name)

    result = Data_Ref("a(2)")
    assert str(result) == str(result).strip()
    assert isinstance(result, Part_Ref)


def test_invalid(f2003_create):
    """Test that there is no match when the input is invalid."""

    for string in ["", "  ", "1", "%", "a b"]:
        with pytest.raises(NoMatchError):
            assert Data_Ref(string) is None
