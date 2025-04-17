# Copyright (c) 2018-2024 Science and Technology Facilities Council

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

"""Test Fortran 2003 rule R1013 : This file tests support for the
Position_Edit_Desc class.

"""

import pytest

import fparser.two.utils as utils
from fparser.two.utils import NoMatchError, InternalError
from fparser.two.Fortran2003 import Position_Edit_Desc


def test_invalid_descriptor():
    """Test invalid options raise an exception."""

    for descriptor in [
        None,
        "",
        "  ",
        "1T",
        "XT",
        "T",
        "TL",
        "TR",
        "XT1",
        "XX",
        "X X",
        "1XX",
    ]:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Position_Edit_Desc(descriptor)
        assert "Position_Edit_Desc: '{0}'".format(descriptor) in str(excinfo.value)


def test_valid_t_descriptor(f2003_create):
    """Test valid nT, nTL and nTR inputs provide the expected output"""

    for name in ["T", "TL", "TR"]:
        for descriptor in [
            "{0}1".format(name),
            "  {0}  1  ".format(name),
            "{0}999".format(name),
            "{0}999".format(name.lower()),
        ]:
            result = Position_Edit_Desc(descriptor)
            assert str(result) == "".join(descriptor.split()).upper()


def test_valid_x_descriptor(f2003_create):
    """Test valid nX inputs provide the expected output"""

    for descriptor in ["1X", "  1  X  ", "999X", "999x"]:
        result = Position_Edit_Desc(descriptor)
        assert str(result) == "".join(descriptor.split()).upper()


def test_invalid_x_descriptor1(f2003_create, monkeypatch):
    """Test that the X extension to the standard raises an exception if it
    is not named as a valid extension.

    """
    monkeypatch.setattr(utils, "_EXTENSIONS", [])
    for descriptor in ["X", "  X  ", "x"]:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Position_Edit_Desc(descriptor)
        assert "Position_Edit_Desc: '{0}'".format(descriptor) in str(excinfo.value)


def test_valid_x_descriptor2(f2003_create, monkeypatch):
    """Test that the X extension to the standard produces the expected
    output if it is named as a valid extension.

    """
    monkeypatch.setattr(utils, "EXTENSIONS", ["x-format"])
    for descriptor in ["X", "  X  ", "x"]:
        result = Position_Edit_Desc(descriptor)
        assert str(result) == "X"


def test_tostr_invalid_1(f2003_create, monkeypatch):
    """Test that invalid input (the items list is not the expected size)
    raises an exception in the tostr method

    """

    obj = Position_Edit_Desc("1X")
    monkeypatch.setattr(obj, "items", [])
    with pytest.raises(InternalError) as excinfo:
        _ = str(obj)
    assert "tostr() has '0' items, but expecting 2" in str(excinfo.value)


def test_tostr_invalid_2(f2003_create, monkeypatch):
    """Test that invalid input (items[1] is empty) raises an exception in
    the tostr method"""

    obj = Position_Edit_Desc("1X")
    monkeypatch.setattr(obj, "items", ["1", ""])
    with pytest.raises(InternalError) as excinfo:
        _ = str(obj)
    assert (
        "items[1] in Class Position_Edit_Desc method tostr() is empty or None"
    ) in str(excinfo.value)
