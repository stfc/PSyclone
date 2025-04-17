# Copyright (c) 2019-2020 Science and Technology Facilities Council.

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

"""Test Fortran 2003 rule R438: this module contains pytest tests for the
   support for the components of a derived type. Note that only condition
   C449 is tested for here. The other conditions (C436-C452) are untested,
   this is the subject of #232.
"""

import pytest
from fparser.api import get_reader
from fparser.two.Fortran2003 import Component_Part
from fparser.two.utils import NoMatchError


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize(
    "var_type", ["integer", "logical", "character(len=1)", "real*8", "real(r_def)"]
)
def test_data_component_part(var_type):
    """Test that various data component declarations are
    recognised. R440."""
    code = var_type + " :: iflag"
    reader = get_reader(code, isfree=True, isstrict=True)
    obj = Component_Part(reader)
    assert "iflag" in str(obj)


@pytest.mark.usefixtures("f2003_create")
def test_invalid_data_component_part():
    """Check that we don't get a match for an invalid component decln."""
    code = "log :: iflag"
    reader = get_reader(code)
    with pytest.raises(NoMatchError):
        _ = Component_Part(reader)


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize(
    "interface, attributes",
    [("", "pointer, nopass"), ("real_func", "pointer"), ("real_func", "pointer, pass")],
)
def test_proc_component_part(interface, attributes):
    """Test that various procedure component declarations are
    recognised. R445."""
    code = "procedure({0}), {1} :: my_proc".format(interface, attributes)
    reader = get_reader(code)
    obj = Component_Part(reader)
    assert obj is not None


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize(
    "invalid_code",
    [
        "procure(), nopass :: my_proc",
        "procedure, nopass :: my_proc",
        "procedure), nopass :: my_proc",
        "procedure(, nopass :: my_proc",
        "procedure() nopass :: my_proc",
        "procedure(), nopass my_proc",
    ],
)
def test_invalid_proc_component(invalid_code):
    """Check that we don't get a match for an invalid procedure
    declaration."""
    reader = get_reader(invalid_code)
    with pytest.raises(NoMatchError):
        Component_Part(reader)


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("attributes", ["nopass", "pass"])
def test_proc_component_pointer(attributes):
    """R445, C449: POINTER shall appear in each
    proc-component-attr-spec-list."""
    code = "procedure(), {0} :: my_proc".format(attributes)
    reader = get_reader(code)
    with pytest.raises(NoMatchError):
        _ = Component_Part(reader)
