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

"""Test Fortran 2003 constraint C1002 : This file tests the support
for a format specification. The standard C1002 tests are performed via
test_format_specification_r1002.py as the constraints are associated
with R1002. This file picks up any tests that need to act directly on
this class.

"""

import pytest
from fparser.two.Fortran2003 import Format_Item_C1002
from fparser.two.utils import InternalError, NoMatchError


def test_data_edit_descriptor_error(f2003_create):
    """Check that None is returned if the descriptor following a P edit
    descriptor is not of the expected type. What is expected is a
    Format_Item instance containing a Data_Edit_Descriptor as its
    second item. This test checks that we return None if the second
    item is not a Data_Edit_Descriptor.

    We do this by trying to match with a format-item-list as this is
    the only other thing that returns a Format_Item instance. However,
    it does not contain a Data_Edit_Descriptor as its second item so
    it triggers the appropriate line of code.

    """

    my_input = "2P ('hello')"
    with pytest.raises(NoMatchError) as excinfo:
        _ = Format_Item_C1002(my_input)
    assert "Format_Item_C1002: '2P ('hello')'" in str(excinfo.value)


def test_internal_errors1(f2003_create, monkeypatch):
    """Check that an internal error is raised if the length of the Items
    list is not 2 as the str() method assumes that it is.

    """
    line = "2P F2.2"
    ast = Format_Item_C1002(line)
    monkeypatch.setattr(ast, "items", [None, None, None])
    with pytest.raises(InternalError) as excinfo:
        str(ast)
    assert "should be length 2 but found '3'" in str(excinfo.value)


def test_internal_error2(f2003_create, monkeypatch):
    """Check that an internal error is raised if entry 0 of items is empty
    or None as the str() method assumes that it has content.

    """
    line = "2P F2.2"
    ast = Format_Item_C1002(line)
    monkeypatch.setattr(ast, "items", [None, ast.items[1]])
    with pytest.raises(InternalError) as excinfo:
        str(ast)
    assert (
        "items entry 0 should contain a format items object but it " "is empty or None"
    ) in str(excinfo.value)


def test_internal_error3(f2003_create, monkeypatch):
    """Check that an internal error is raised if entry 1 of items is empty
    or None as the str() method assumes that it has content.

    """
    line = "2P F2.2"
    ast = Format_Item_C1002(line)
    monkeypatch.setattr(ast, "items", [ast.items[0], None])
    with pytest.raises(InternalError) as excinfo:
        str(ast)
    assert (
        "items entry 1 should contain a format items object but it " "is empty or None"
    ) in str(excinfo.value)
