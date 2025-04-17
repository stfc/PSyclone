# Copyright (c) 2022 Science and Technology Facilities Council

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
from fparser.two.utils import NoMatchError, walk
from fparser.two.Fortran2008 import Error_Stop_Stmt
from fparser.api import get_reader


@pytest.mark.usefixtures("f2008_create")
@pytest.mark.parametrize("string", ["ERROR STOP", "error stop"])
def test_simple(string):
    """Test that error-stop-stmt without stop-code matches."""
    result = Error_Stop_Stmt(string)
    assert str(result) == "ERROR STOP"


@pytest.mark.usefixtures("f2008_create")
@pytest.mark.parametrize("stop_code", ["1", "A", "err_code"])
def test_stop_code(stop_code):
    """Test that error-stop-stmt with stop-code matches."""
    result = Error_Stop_Stmt("ERROR STOP {}".format(stop_code))
    assert str(result) == "ERROR STOP {}".format(stop_code)


@pytest.mark.usefixtures("f2008_create")
def test_error1():
    """Test error-stop-stmt is not matched for wrong syntax."""
    with pytest.raises(NoMatchError) as excinfo:
        _ = Error_Stop_Stmt("ERROR () STOP")
    assert "Error_Stop_Stmt: 'ERROR () STOP'" in str(excinfo.value)


def test_functional1(f2008_parser):
    """Test error-stop-stmt is matched in a subroutine."""
    tree = f2008_parser(
        get_reader(
            """\
subroutine my_abort
error stop
end subroutine my_abort
    """
        )
    )
    assert walk(tree, Error_Stop_Stmt)
    assert "ERROR STOP" in str(tree)


def test_functional2(f2008_parser):
    """Test error-stop-stmt is matched in a if-stmt."""
    tree = f2008_parser(
        get_reader(
            """\
subroutine my_abort(err_code)
implicit none
integer, intent(in) :: err_code
if (err_code /= 0) error stop err_code
end subroutine my_abort
    """
        )
    )
    assert walk(tree, Error_Stop_Stmt)
    assert "ERROR STOP err_code" in str(tree)
