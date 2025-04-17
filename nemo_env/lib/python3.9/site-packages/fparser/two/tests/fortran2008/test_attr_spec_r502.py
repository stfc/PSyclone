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

"""Test Fortran 2008 rule R502.

    attr-spec is access-spec
                 or ALLOCATABLE
                 or ASYNCHRONOUS
                 or CODIMENSION lbracket coarray-spec rbracket
                 or CONTIGUOUS
                 or DIMENSION ( component-array-spec )
                 or EXTERNAL
                 or INTENT ( intent-spec )
                 or INTRINSIC
                 or language-binding-spec
                 or OPTIONAL
                 or PARAMETER
                 or POINTER
                 or PROTECTED
                 or SAVE
                 or TARGET
                 or VALUE
                 or VOLATILE
"""

import pytest
from fparser.two.Fortran2008 import (
    Attr_Spec,
    Codimension_Attr_Spec,
    Deferred_Coshape_Spec,
    Coshape_Spec,
)
from fparser.two import Fortran2003
from fparser.api import get_reader


@pytest.mark.usefixtures("f2008_create")
@pytest.mark.parametrize(
    "mixed_case_ref",
    [
        "ALlOCaTABLE",
        "ASYNChroNOUS",
        "CONTIGUOUs",
        "eXTERNAL",
        "INtRINsIC",
        "OPTIOnAL",
        "PARAmeTER",
        "PoInTeR",
        "pROTECTEd",
        "SAvE",
        "TargeT",
        "VALUE",
        "VOLATILE",
    ],
)
def test_attr_spec(mixed_case_ref):
    """Test the parsing of common attributes provided as mixed case, upper
    case and lower case strings."""
    for manip in [str, str.upper, str.lower]:
        obj = Attr_Spec(manip(mixed_case_ref))
        assert isinstance(obj, Attr_Spec), repr(obj)
        assert str(obj) == mixed_case_ref.upper()


@pytest.mark.usefixtures("f2008_create")
@pytest.mark.parametrize(
    "attr", ["", "CONTIGUOS", " SAVE", "TARGET ", "VA LUE", "POINNTER", " DIMENSION(:)"]
)
def test_invalid_attr_spec(attr):
    """Test that invalid or misspelled attributes raise exceptions."""
    with pytest.raises(Fortran2003.NoMatchError):
        _ = Attr_Spec(attr)


@pytest.mark.usefixtures("f2008_create")
@pytest.mark.parametrize(
    "attr, ref",
    [
        ("DIMENSION   (:)", "DIMENSION(:)"),
        ("dimenSION(  : )", "DIMENSION(:)"),
        ("DIMENSION (5)", "DIMENSION(5)"),
        ("DIMENSION (2:)", "DIMENSION(2 :)"),
        ("dimension (1:5, 2, 3:4)", "DIMENSION(1 : 5, 2, 3 : 4)"),
    ],
)
def test_attr_spec_dimension(attr, ref):
    """Test the parsing of dimension attribute in attr_spec."""
    obj = Attr_Spec(attr)
    assert isinstance(obj, Fortran2003.Dimension_Attr_Spec), repr(obj)
    assert str(obj) == ref


@pytest.mark.usefixtures("f2008_create")
@pytest.mark.parametrize(
    "attr, ref",
    [
        ("codimension[*]", "CODIMENSION [*]"),
        ("CODIMENSION   [:]", "CODIMENSION [:]"),
        ("COdimenSION[  : ]", "CODIMENSION [:]"),
        ("coDIMENSIon [ *]", "CODIMENSION [*]"),
        ("CoDimension [1:*]", "CODIMENSION [1 : *]"),
        (" CODIMENSION [1:5, *]   ", "CODIMENSION [1 : 5, *]"),
        ("codimension [1:5, 2, 3:*]", "CODIMENSION [1 : 5, 2, 3 : *]"),
    ],
)
def test_attr_spec_codimension(attr, ref):
    """Test the parsing of codimension attribute in attr_spec."""
    obj = Attr_Spec(attr)
    assert isinstance(obj, Codimension_Attr_Spec), repr(obj)
    assert str(obj) == ref


@pytest.mark.usefixtures("f2008_create")
@pytest.mark.parametrize(
    "attr",
    [
        "codimension[1:5]",
        "codimension[3:]",
        "codimension[:5]",
        "codimension[,,]",
        "codimension[1, 5, 3 *]",
        "co dimension[*]",
        "coodimension[*]",
        "codimension[]",
        "codimensions[*]",
        "codimension",
        "codimension[[*]",
        "codimension[[:]]",
        "codimension[][]",
    ],
)
def test_invalid_attr_spec_codimension(attr):
    """Test that invalid codimension attribute in attr_spec raise exception."""
    with pytest.raises(Fortran2003.NoMatchError):
        _ = Attr_Spec(attr)


@pytest.mark.usefixtures("f2008_create")
@pytest.mark.parametrize(
    "attr, ref",
    [
        ("codimension[*]", "CODIMENSION [*]"),
        ("CODIMENSION   [:]", "CODIMENSION [:]"),
        ("COdimenSION[  : ]", "CODIMENSION [:]"),
        ("coDIMENSIon [ *]", "CODIMENSION [*]"),
        (" CODIMENSION [1:5, *]   ", "CODIMENSION [1 : 5, *]"),
        ("codimension [1:5, 2, 3:*]", "CODIMENSION [1 : 5, 2, 3 : *]"),
    ],
)
def test_codimension_attr_spec(attr, ref):
    """Test the parsing of codimension attributes."""
    obj = Codimension_Attr_Spec(attr)
    assert isinstance(obj, Codimension_Attr_Spec), repr(obj)
    assert str(obj) == ref


@pytest.mark.usefixtures("f2008_create")
@pytest.mark.parametrize(
    "attr",
    [
        "codimension[1:5]",
        "codimension[3:]",
        "codimension[:5]",
        "codimension[,,]",
        "codimension[1, 5, 3 *]",
        "co dimension[*]",
        "coodimension[*]",
        "codimension[]",
        "codimensions[*]",
        "codimension",
        "codimension[[*]",
        "codimension[[:]]",
        "codimension[][]",
    ],
)
def test_invalid_codimension_attr_spec(attr):
    """Test that invalid codimension attributes raise exception."""
    with pytest.raises(Fortran2003.NoMatchError):
        _ = Codimension_Attr_Spec(attr)


def test_attr_spec_list_parser(f2008_parser):
    """Test that Attr_Spec_List is generated correctly and used by a parser."""
    code = """
SUBROUTINE FOO(i)
  INTEGER, CONTIGUOUS, INTENT(IN) :: i(10)
  INTEGER :: j
END SUBROUTINE
    """.strip()
    reader = get_reader(code)
    result = f2008_parser(reader)
    assert str(result) == code
