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

"""Test Fortran 2008 rule R509

    coarray-spec is deferred-coshape-spec-list
                    or explicit-coshape-spec-list

"""

import pytest
from fparser.two.Fortran2008 import (
    Coarray_Spec,
    Deferred_Coshape_Spec_List,
    Explicit_Coshape_Spec,
)
from fparser.two import Fortran2003


@pytest.mark.usefixtures("f2008_create")
@pytest.mark.parametrize(
    "attr, _type",
    [
        ("*", Explicit_Coshape_Spec),
        ("5 :*", Explicit_Coshape_Spec),
        ("1:  3  ,  a: *", Explicit_Coshape_Spec),
        (" 1 ,  2,3:*", Explicit_Coshape_Spec),
        ("1:2,3,*", Explicit_Coshape_Spec),
        (":", Deferred_Coshape_Spec_List),
    ],
)
def test_coarray_spec(attr, _type):
    """Test that coarray_spec are parsed correctly."""
    obj = Coarray_Spec(attr)
    assert isinstance(obj, _type), repr(obj)
    ref = attr.replace(" ", "").replace(":", " : ").replace(",", ", ").strip()
    assert str(obj) == ref


@pytest.mark.usefixtures("f2008_create")
@pytest.mark.parametrize(
    "attr", ["1:3", "", ":b", "a:", ":*", "", "1:,*", "1,,*", "::", "5"]
)
def test_invalid_coarray_spec(attr):
    """Test that invalid coarray_spec raise exception."""
    with pytest.raises(Fortran2003.NoMatchError):
        _ = Coarray_Spec(attr)
