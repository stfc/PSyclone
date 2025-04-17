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

"""Test Fortran 2008 rule R437.

    component-attr-spec = access-spec
                          or ALLOCATABLE
                          or CODIMENSION lbracket coarray-spec rbracket
                          or CONTIGUOUS
                          or DIMENSION ( component-array-spec )
                          or POINTER
"""

import pytest
from fparser.two.Fortran2003 import (
    Access_Spec,
    Dimension_Component_Attr_Spec,
    NoMatchError,
)
from fparser.two.Fortran2008 import Component_Attr_Spec, Codimension_Attr_Spec
from fparser.api import get_reader


@pytest.mark.usefixtures("f2008_create")
@pytest.mark.parametrize("mixed_case_ref", ["ALlOCaTABLE", "CONTIGUOUs", "PoInTeR"])
def test_component_attr_spec(mixed_case_ref):
    """Test the parsing of component attributes provided as mixed case, upper
    case and lower case strings."""
    for manip in [str, str.upper, str.lower]:
        obj = Component_Attr_Spec(manip(mixed_case_ref))
        assert isinstance(obj, Component_Attr_Spec), repr(obj)
        assert str(obj) == mixed_case_ref.upper()


@pytest.mark.usefixtures("f2008_create")
@pytest.mark.parametrize(
    "attr",
    ["", " ALLOCATABLE", "POINTER ", "POIN TER", "CONTIGOUS", "ALOCATABLE", "FOO"],
)
def test_invalid_component_attr_spec(attr):
    """Test that invalid or misspelled component attributes raise exception."""
    with pytest.raises(NoMatchError):
        _ = Component_Attr_Spec(attr)


@pytest.mark.usefixtures("f2008_create")
@pytest.mark.parametrize(
    "attr, ref",
    [
        ("DIMENSION   (:)", "DIMENSION(:)"),
        ("dimenSION(  : )", "DIMENSION(:)"),
        ("DIMENSION (5)", "DIMENSION(5)"),
        ("dimension (1:5, 2, 3:4)", "DIMENSION(1 : 5, 2, 3 : 4)"),
    ],
)
def test_component_attr_spec_dimension(attr, ref):
    """Test the parsing of dimension attribute in component_attr_spec."""
    obj = Component_Attr_Spec(attr)
    assert isinstance(obj, Dimension_Component_Attr_Spec), repr(obj)
    assert str(obj) == ref


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
def test_component_attr_spec_codimensions(attr, ref):
    """Test the parsing of codimension attribute in component_attr_spec."""
    obj = Component_Attr_Spec(attr)
    assert isinstance(obj, Codimension_Attr_Spec), repr(obj)
    assert str(obj) == ref


@pytest.mark.usefixtures("f2008_create")
def test_private_attr():
    """Tests the parsing of private attribute."""
    obj = Component_Attr_Spec("private")
    assert isinstance(obj, Access_Spec), repr(obj)
    assert str(obj) == "PRIVATE"


def test_component_attr_spec_list_parser(f2008_parser):
    """Test that Component_Attr_Spec_List is generated correctly
    and used by the F2008 parser."""
    code = """
MODULE FOO
  TYPE :: GRID_TYPE
    REAL, ALLOCATABLE, CODIMENSION [:, :, :] :: GRID(:, :, :)
    REAL :: BAR
  END TYPE GRID_TYPE
END MODULE FOO
    """.strip()
    reader = get_reader(code)
    result = f2008_parser(reader)
    assert str(result) == code
