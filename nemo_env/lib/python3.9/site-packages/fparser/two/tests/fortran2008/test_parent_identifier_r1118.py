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

"""Test Fortran 2008 rule R1118 (C1113).

   parent-identifier is ancestor-module-name [ : parent-submodule-name ]

   C1113 The ancestor-module-name shall be the name of a nonintrinsic
   module; the parent-submodule name shall be the name of a
   descendant of that module.

   It is not possible to test C1113 with fparser as the names may be
   in different files.

"""
import pytest
from fparser.two.utils import NoMatchError
from fparser.two.Fortran2008 import Parent_Identifier


def test_simple_1(f2008_create):
    """Test the parsing of a minimal parent-identifier statement."""
    result = Parent_Identifier("modulename")
    assert str(result) == "modulename"


def test_simple_2(f2008_create):
    """Test the parsing of a minimal parent-identifier statement."""
    result = Parent_Identifier("modulename : submodulename")
    assert str(result) == "modulename:submodulename"


def test_simple_error1(f2008_create):
    """Test a parent_identifier statement with a syntax error raises an
    exception.

    """
    with pytest.raises(NoMatchError) as excinfo:
        dummy_ = Parent_Identifier("modulename ; submodulename")
    assert "Parent_Identifier: 'modulename ; submodulename'" in str(excinfo.value)


def test_simple_error2(f2008_create):
    """Test a parent_identifier statement with a syntax error raises an
    exception.

    """
    with pytest.raises(NoMatchError) as excinfo:
        dummy_ = Parent_Identifier("modulename :")
    assert "Parent_Identifier: 'modulename :'" in str(excinfo.value)


def test_simple_error3(f2008_create):
    """Test a parent_identifier statement with a syntax error raises an
    exception.

    """
    with pytest.raises(NoMatchError) as excinfo:
        dummy_ = Parent_Identifier(": submodulename")
    assert "Parent_Identifier: ': submodulename'" in str(excinfo.value)


def test_simple_error4(f2008_create):
    """Test a parent_identifier statement with a syntax error raises an
    exception.

    """
    with pytest.raises(NoMatchError) as excinfo:
        dummy_ = Parent_Identifier("modulename : submodulename : anothername")
    assert "Parent_Identifier: 'modulename : submodulename : anothername'" in str(
        excinfo.value
    )


def test_simple_error5(f2008_create):
    """Test a parent-identifier statement with additional content after
    the match raises an exception.

    """
    with pytest.raises(NoMatchError) as excinfo:
        dummy_ = Parent_Identifier("modulename : submodulename :")
    assert "Parent_Identifier: 'modulename : submodulename :'" in str(excinfo.value)
