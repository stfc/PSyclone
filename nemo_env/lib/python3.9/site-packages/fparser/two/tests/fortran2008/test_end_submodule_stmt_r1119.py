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

"""Test Fortran 2008 rule R1119.

    end-submodule-stmt is END [ SUBMODULE [ submodule-name ] ]

"""
import pytest
from fparser.two.utils import NoMatchError
from fparser.two.Fortran2008 import End_Submodule_Stmt


def test_simple_1(f2008_create):
    """Test the parsing of a minimal end-submodule statement."""
    result = End_Submodule_Stmt("end")
    assert str(result) == "END"


def test_simple_2(f2008_create):
    """Test the parsing of an end-submodule statement which includes the
    submodule keyword.

    """
    result = End_Submodule_Stmt("end submodule")
    assert str(result) == "END SUBMODULE"


def test_simple_3(f2008_create):
    """Test the parsing of an end-submodule statement which includes the
    submodule name

    """
    result = End_Submodule_Stmt("end submodule name")
    assert str(result) == "END SUBMODULE name"


def test_simple_error1(f2008_create):
    """Test an end-submodule statement with a syntax error raises an
    exception.

    """
    with pytest.raises(NoMatchError) as excinfo:
        dummy_ = End_Submodule_Stmt("edn")
    assert "End_Submodule_Stmt: 'edn'" in str(excinfo.value)


def test_simple_error2(f2008_create):
    """Test an end-submodule statement with a syntax error raises an
    exception.

    """
    with pytest.raises(NoMatchError) as excinfo:
        dummy_ = End_Submodule_Stmt("end submod")
    assert "End_Submodule_Stmt: 'end submod'" in str(excinfo.value)


def test_simple_error3(f2008_create):
    """Test an end-submodule statement with additional content after the
    match raises an exception.

    """
    with pytest.raises(NoMatchError) as excinfo:
        dummy_ = End_Submodule_Stmt("end submodule name :")
    assert "End_Submodule_Stmt: 'end submodule name :'" in str(excinfo.value)
