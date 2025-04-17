# Copyright (c) 2020-2021 Science and Technology Facilities Council.
#
# All rights reserved.
#
# Modifications made as part of the fparser project are distributed
# under the following license:
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
#
# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.
#
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

"""
Module containing py.test tests for the Io_Control_Spec class and
the list variant - r913.

"""

import pytest
from fparser.two import Fortran2003
from fparser.two.parser import ParserFactory

# this is required to setup the fortran2003 classes
_ = ParserFactory().create(std="f2003")

# Tests for the io_control_spec class.


def test_io_control_spec():
    """Test that we can construct an io-control-spec and that its str
    and repr properties are correct."""
    tcls = Fortran2003.Io_Control_Spec
    obj = tcls("end=123")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "END = 123"
    assert repr(obj) == "Io_Control_Spec('END', Label('123'))"


@pytest.mark.parametrize("string", ["6", "end 123"])
def test_io_ctrl_spec_named_only(string):
    """Check that io-control-spec only matches named entities."""
    tcls = Fortran2003.Io_Control_Spec
    with pytest.raises(Fortran2003.NoMatchError) as err:
        _ = tcls(string)
    assert "Io_Control_Spec: '{0}'".format(string) in str(err.value)


@pytest.mark.parametrize(
    "lhs, rhs",
    [
        ("unit", "num"),
        ("unit", "6"),
        ("fmt", "fmt_str"),
        ("fmt", "*"),
        ("fmt", "'(I3)'"),
        ("nml", "nml_name"),
        ("advance", "yes"),
        ("advance", "no"),
        ("asynchronous", "yes"),
        ("asynchronous", "no"),
        ("blank", "null"),
        ("blank", "zero"),
        ("decimal", "comma"),
        ("decimal", "point"),
        ("delim", "apostrophe"),
        ("end", "10"),
        ("eor", "10"),
        ("err", "10"),
        ("id", "var"),
        ("iomsg", "msg_var"),
        ("iostat", "var"),
        ("pad", "yes"),
        ("pad", "no"),
        ("pos", "posn"),
        ("pos", "10"),
        ("rec", "1"),
        ("round", "up"),
        ("round", "down"),
        ("round", "zero"),
        ("round", "nearest"),
        ("round", "compatible"),
        ("round", "processor_defined"),
        ("sign", "plus"),
        ("sign", "suppress"),
        ("sign", "processor_defined"),
        ("size", "var"),
    ],
)
def test_io_ctrl_spec_named_entities(lhs, rhs):
    """Check that all valid names are matched correctly."""
    obj = Fortran2003.Io_Control_Spec("=".join([lhs, rhs]))
    assert isinstance(obj, Fortran2003.Io_Control_Spec)
    assert obj.children[0] == lhs.upper()
    obj = Fortran2003.Io_Control_Spec("=".join([lhs.upper(), rhs]))
    assert isinstance(obj, Fortran2003.Io_Control_Spec)


@pytest.mark.parametrize(
    "lhs, rhs", [("asize", "var"), ("unit", "1.0"), ("size", "10")]
)
def test_io_ctrl_spec_invalid_values(lhs, rhs):
    """Check that io_ctrl_spec does not match if a keyword is unrecognised
    or the value is invalid."""
    with pytest.raises(Fortran2003.NoMatchError):
        _ = Fortran2003.Io_Control_Spec("=".join([lhs, rhs]))


# Tests for the io_control_spec_list class.


def test_io_control_spec_list():
    """Test that we correctly parse and then generate various
    forms of IO-control specification lists (R913-list)."""
    tcls = Fortran2003.Io_Control_Spec_List
    obj = tcls("23, end=123")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "23, END = 123"
    assert (
        repr(obj) == "Io_Control_Spec_List(',', (Io_Control_Spec("
        "None, Int_Literal_Constant('23', None)), Io_Control_Spec('END', "
        "Label('123'))))"
    )

    obj = tcls("123")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "123"

    obj = tcls("123,*")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "123, *"
    assert repr(obj) == (
        "Io_Control_Spec_List(',', (Io_Control_Spec("
        "None, Int_Literal_Constant('123', None)), "
        "Io_Control_Spec(None, Format('*'))))"
    )

    obj = tcls("123,fmt=a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "123, FMT = a"
    assert repr(obj) == (
        "Io_Control_Spec_List(',', "
        "(Io_Control_Spec(None, "
        "Int_Literal_Constant('123', None)), "
        "Io_Control_Spec('FMT', Name('a'))))"
    )

    obj = tcls("123,nml=a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "123, NML = a"
    assert repr(obj) == (
        "Io_Control_Spec_List(',', "
        "(Io_Control_Spec(None, "
        "Int_Literal_Constant('123', None)), "
        "Io_Control_Spec('NML', Name('a'))))"
    )

    obj = tcls('123, "(I3)"')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == '123, "(I3)"'

    obj = tcls("123,a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "123, a"

    # Unit named and not the first argument
    obj = tcls("fmt=b, unit=123")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "FMT = b, UNIT = 123"


def test_io_control_spec_list_3_or_more():
    """Test the matching when there are more than 2 entries in list - 3rd
    and subsequent entries must be named.
    """
    tcls = Fortran2003.Io_Control_Spec_List
    with pytest.raises(Fortran2003.NoMatchError) as err:
        _ = tcls("123,a,b")
    assert "Io_Control_Spec_List: '123,a,b'" in str(err.value)
    with pytest.raises(Fortran2003.NoMatchError) as err:
        _ = tcls("123,a,err=10,var")
    assert "Io_Control_Spec_List: '123,a,err=10,var'" in str(err.value)

    obj = tcls("123,a,err=10")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "123, a, ERR = 10"

    obj = tcls("123,a,err=10,end=12")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "123, a, ERR = 10, END = 12"

    obj = tcls("unit=123,err=10,end=12")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "UNIT = 123, ERR = 10, END = 12"


def test_io_control_spec_list_invalid_first_entry():
    """Test that the special handling for the first item in the list
    rejects an invalid, unnamed entry."""
    tcls = Fortran2003.Io_Control_Spec_List
    with pytest.raises(Fortran2003.NoMatchError) as err:
        tcls("'name'")
    assert "Io_Control_Spec_List: ''name''" in str(err.value)
    with pytest.raises(Fortran2003.NoMatchError) as err:
        tcls("1.0")
    assert "Io_Control_Spec_List: '1.0'" in str(err.value)


def test_io_control_spec_list_invalid_2nd_entry():
    """Test that the special handling for the first two unnamed items in
    the list rejects an invalid, unnamed second entry."""
    tcls = Fortran2003.Io_Control_Spec_List
    # The second entry must be either a format specifier or a namelist-group-
    # name. We therefore supply a real literal as that doesn't match either.
    with pytest.raises(Fortran2003.NoMatchError) as err:
        tcls("unit_no, 1.0")
    assert "Io_Control_Spec_List: 'unit_no, 1.0'" in str(err.value)


def test_io_spec_list_c910():
    """Check that various constraints are applied in the matching. Currently
    only C910, C916-918 are implemented (#267)."""
    tcls = Fortran2003.Io_Control_Spec_List
    # C910 - must have a unit number
    with pytest.raises(Fortran2003.NoMatchError) as err:
        tcls('fmt="some-fmt", end=10')
    assert "Io_Control_Spec_List: 'fmt=\"some-fmt\", end=10'" in str(err.value)
    # Again but when list contains only one entry
    with pytest.raises(Fortran2003.NoMatchError) as err:
        tcls('fmt="some-fmt"')
    assert "Io_Control_Spec_List: 'fmt=\"some-fmt\"'" in str(err.value)


def test_io_spec_list_c916():
    """C916 - cannot have both a namelist and a format."""
    tcls = Fortran2003.Io_Control_Spec_List
    with pytest.raises(Fortran2003.NoMatchError) as err:
        tcls("123,nml=a,fmt=b")
    assert "Io_Control_Spec_List: '123,nml=a,fmt=b'" in str(err.value)
    # Check when we have an un-named second entry
    with pytest.raises(Fortran2003.NoMatchError) as err:
        tcls('123, "(I3)", fmt=var')
    assert "Io_Control_Spec_List: '123, \"(I3)\", fmt=var'" in str(err.value)
    with pytest.raises(Fortran2003.NoMatchError) as err:
        tcls("123, nml_grp_name, fmt=var")
    assert "Io_Control_Spec_List: '123, nml_grp_name, fmt=var'" in str(err.value)


def test_io_spec_list_c917():
    """C917, if format is second item and is un-named then first item must be
    un-named unit number."""
    tcls = Fortran2003.Io_Control_Spec_List
    with pytest.raises(Fortran2003.NoMatchError) as err:
        tcls("unit=10, \"('a format')\"")
    assert "Io_Control_Spec_List: 'unit=10" in str(err.value)


def test_io_spec_list_c918():
    """C918, if namelist group name is second item and is un-named then first
    item must be un-named unit number."""
    tcls = Fortran2003.Io_Control_Spec_List
    with pytest.raises(Fortran2003.NoMatchError) as err:
        tcls("unit=10, nml_grp_name")
    assert "Io_Control_Spec_List: 'unit=10" in str(err.value)
