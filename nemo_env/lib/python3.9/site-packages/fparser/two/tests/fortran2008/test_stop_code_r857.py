# Copyright (c) 2024 Science and Technology Facilities Council

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

"""Test Fortran 2008 rule R856

    error-stop-stmt is ERROR STOP [ stop-code ]
"""

import pytest

from fparser.api import get_reader
from fparser.two.utils import NoMatchError, walk
from fparser.two import Fortran2003, utils
from fparser.two.Fortran2008 import Stop_Code


@pytest.mark.usefixtures("f2008_create")
@pytest.mark.parametrize("string", ["1", "- 1", '"abc"', "'abc'", "'abc' // 'def'"])
def test_simple_stop_code(string):
    """Test that error-stop matches the expected valid values."""
    result = Stop_Code(string)
    assert str(result) == string


@pytest.mark.usefixtures("f2008_create")
@pytest.mark.parametrize("string", ["call sub()", "do i", "1, 2, 3"])
def test_simple_stop_code_errors(string):
    """Test that invalid stop codes are handled."""
    with pytest.raises(NoMatchError) as err:
        Stop_Code(string)
    assert f"Stop_Code: '{string}'" in str(err.value)


@pytest.mark.parametrize("string", ["1", "12345"])
def test_stop_stmt_2003_stop_code(f2008_parser, string):
    """Test that 'stop' parsing works in real code, and returns a 2003
    StopCode. This is the case if the stop code is between one and
    five digits only:
    """
    code = f"""
    subroutine dummy()
    stop {string}
    end subroutine dummy
    """
    tree = f2008_parser(get_reader(code))
    stop_code = walk(tree, Fortran2003.Stop_Code)[0]
    assert str(stop_code) == string


@pytest.mark.parametrize("string", ["1234567", "12 .AND. 34"])
def test_stop_stmt_2008(f2008_parser, string, monkeypatch):
    """Test that stop parsing works in real code when using F2008
    only (i.e. not F2003) statements. Note that '12 .and. 34' is a
    level-5-expr, and as such would not be accepted by the F2003
    "extended-stop-args" extension in fparser.
    """
    monkeypatch.setattr(utils, "_EXTENSIONS", [])
    code = f"""
    subroutine dummy()
    stop {string}
    end subroutine dummy
    """
    tree = f2008_parser(get_reader(code))
    stop_stmt = walk(tree, Fortran2003.Stop_Stmt)[0]
    assert str(stop_stmt.children[1]) == string
