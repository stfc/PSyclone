# Copyright (c) 2018 Science and Technology Facilities Council

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

"""Test Fortran 2003 rule R304 : This file tests support for the Name
class.

"""

import pytest
from fparser.two.utils import NoMatchError
from fparser.two.Fortran2003 import Name

# Test the name class R304


def test_valid_names():
    """Test the appropriate set of valid names. Note, '$' is not valid in
    the standard but is supported by some compilers so we allow it
    here.

    """
    for name in ["a", "abcde", "a_", "a1", "  a  ", "a$"]:
        result = Name(name)
        assert str(result) == name.strip()


def test_invalid_names():
    """Test invalid names. There are too many to try all invalid
    characters so just do a subset.

    """
    for name in [
        "1",
        "_",
        "1a",
        "_a",
        "a a",
        "",
        " ",
        "a!",
        "a@",
        "a#",
        "a%",
        "a^",
        "a&",
        "a*",
        "a(",
        "a)",
    ]:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Name(name)
        assert "Name: '{0}'".format(name) in str(excinfo.value)
