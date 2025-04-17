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

"""Test Fortran 2003 rule R1227 : prefix."""
import pytest

import fparser.two.Fortran2003 as f2003
from fparser.two.utils import NoMatchError


def test_prefix(f2003_create):
    """Test that valid Prefix strings are matched successfully."""
    # single space
    result = f2003.Prefix("impure elemental type(my_type) module")
    assert result.tostr() == "IMPURE ELEMENTAL TYPE(my_type) MODULE"
    assert (
        result.torepr()
        == "Prefix(' ', (Prefix_Spec('IMPURE'), Prefix_Spec('ELEMENTAL'), "
        "Declaration_Type_Spec('TYPE', Type_Name('my_type')), "
        "Prefix_Spec('MODULE')))"
    )

    # multiple spaces
    result = f2003.Prefix("  impure  elemental  type  (  my_type  )  module  ")
    assert result.tostr() == "IMPURE ELEMENTAL TYPE(my_type) MODULE"
    assert (
        result.torepr()
        == "Prefix(' ', (Prefix_Spec('IMPURE'), Prefix_Spec('ELEMENTAL'), "
        "Declaration_Type_Spec('TYPE', Type_Name('my_type')), "
        "Prefix_Spec('MODULE')))"
    )


def test_single_prefix_spec(f2003_create):
    """Test that a single prefix-spec is returned as a Prefix containing a
    Prefix_Spec.

    """
    result = f2003.Prefix("impure")
    assert result.tostr() == "IMPURE"
    assert result.torepr() == "Prefix(' ', (Prefix_Spec('IMPURE'),))"


def test_single_decl_spec(f2003_create):
    """Test that a single prefix-spec is returned as a Prefix containing a
    declaration-type-spec.

    """
    result = f2003.Prefix("type(my_type)")
    assert result.tostr() == "TYPE(my_type)"
    assert (
        result.torepr() == "Prefix(' ', (Declaration_Type_Spec('TYPE', "
        "Type_Name('my_type')),))"
    )


def test_decl_spec_lhs(f2003_create):
    """Test that a Prefix containing a declaration-type-spec and
    prefix-spec keywords (e.g. IMPURE) to its left, produce the
    expected output.

    """
    result = f2003.Prefix("impure recursive type(my_type)")
    assert result.tostr() == "IMPURE RECURSIVE TYPE(my_type)"
    assert (
        result.torepr()
        == "Prefix(' ', (Prefix_Spec('IMPURE'), Prefix_Spec('RECURSIVE'), "
        "Declaration_Type_Spec('TYPE', Type_Name('my_type'))))"
    )


def test_decl_spec_rhs(f2003_create):
    """Test that a Prefix containing a declaration-type-spec and
    prefix-spec keywords (e.g. IMPURE) to its right, produce the
    expected output.

    """
    result = f2003.Prefix("type(my_type) impure recursive")
    assert result.tostr() == "TYPE(my_type) IMPURE RECURSIVE"
    assert (
        result.torepr() == "Prefix(' ', (Declaration_Type_Spec('TYPE', "
        "Type_Name('my_type')), Prefix_Spec('IMPURE'), "
        "Prefix_Spec('RECURSIVE')))"
    )


def test_no_decl_spec(f2003_create):
    """Test that a Prefix without a declaration-type-spec produces the
    expected output.

    """
    result = f2003.Prefix("module impure")
    assert result.tostr() == "MODULE IMPURE"
    assert (
        result.torepr() == "Prefix(' ', (Prefix_Spec('MODULE'), Prefix_Spec('IMPURE')))"
    )


@pytest.mark.parametrize("string", ["invalid", "pure impure purile", "", " "])
def test_prefix_nomatch(f2003_create, string):
    """Test that invalid Prefix strings raise a NoMatchError exception."""
    with pytest.raises(NoMatchError):
        _ = f2003.Prefix(string)


def test_prefix_c1240(f2003_create):
    """Test that an exception is raised if constraint c1240 is broken (at
    most one of each prefix-spec).

    """
    with pytest.raises(NoMatchError):
        _ = f2003.Prefix("module module")

    with pytest.raises(NoMatchError):
        _ = f2003.Prefix("type(my_type) type(my_type2)")

    with pytest.raises(NoMatchError):
        _ = f2003.Prefix("module type(my_type2) module")


def test_prefix_c1241(f2003_create):
    """Test that an exception is raised if constraint c1241 is broken
    (elemental and recursive are used together).

    """
    with pytest.raises(NoMatchError):
        _ = f2003.Prefix("recursive elemental")
