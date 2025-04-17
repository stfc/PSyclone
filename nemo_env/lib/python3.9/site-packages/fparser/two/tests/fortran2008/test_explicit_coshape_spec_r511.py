# Copyright (c) 2020 Science and Technology Facilities Council

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

"""Test Fortran 2008 rule R511

    explicit-coshape-spec is [ coshape-spec-list , ] [ lower-cobound : ] *

"""

import pytest
from fparser.two.Fortran2008 import (
    Explicit_Coshape_Spec,
    Coshape_Spec,
    Coshape_Spec_List,
)
from fparser.two import Fortran2003


@pytest.mark.usefixtures("f2008_create")
@pytest.mark.parametrize(
    "attr, types",
    [
        ("*", (type(None), type(None))),
        ("5 :*", (type(None), Fortran2003.Int_Literal_Constant)),
        ("1:  3  ,  a: *", (Coshape_Spec_List, Fortran2003.Name)),
        (" 1 ,  2,3:*", (Coshape_Spec_List, Fortran2003.Int_Literal_Constant)),
        ("1:2,3,*", (Coshape_Spec_List, type(None))),
    ],
)
def test_explicit_coshape_spec(attr, types):
    """Test that explicit_coshape_spec are parsed correctly."""
    obj = Explicit_Coshape_Spec(attr)
    assert isinstance(obj, Explicit_Coshape_Spec), repr(obj)
    assert isinstance(obj.items[0], types[0])
    assert isinstance(obj.items[1], types[1])
    ref = attr.replace(" ", "").replace(":", " : ").replace(",", ", ")
    assert str(obj) == ref


@pytest.mark.usefixtures("f2008_create")
@pytest.mark.parametrize("attr", ["1:3", ":", ":b", "a:", ":*", "", "1:,*", "1,,*"])
def test_invalid_explicit_coshape_spec(attr):
    """Test that invalid explicit_coshape_spec raise exception."""
    with pytest.raises(Fortran2003.NoMatchError):
        _ = Explicit_Coshape_Spec(attr)


@pytest.mark.usefixtures("f2008_create")
@pytest.mark.parametrize("attr", ["a", "10", "1:3", " 5 : 123 ", "2:   b   "])
def test_coshape_spec(attr):
    """Test that coshape_spec are parsed correctly."""
    obj = Coshape_Spec(attr)
    assert isinstance(obj, Coshape_Spec), repr(obj)
    assert str(obj) == attr.replace(" ", "").replace(":", " : ")


@pytest.mark.usefixtures("f2008_create")
@pytest.mark.parametrize("attr", [":", "a:", ":b", "*", "", "1::3"])
def test_invalid_coshape_spec(attr):
    """Test that invalid coshape_spec raise exception."""
    with pytest.raises(Fortran2003.NoMatchError):
        _ = Coshape_Spec(attr)
