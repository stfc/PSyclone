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

"""Test Fortran 2003 rule R603 : This file tests support for the
Designator class.

Three of the rules in Designator class are currently not called as the
text matches to earlier rules. The associated rule constraints and
specification text (if implemented, which they are not - see issue
#201) would fix this problem.

For example, here is note 6.6 from the F2003 specification which helps
clarify for derived types:

The syntax rules are structured such that a data-ref that ends in a
component name without a following subscript list is a structure
component, even when other component names in the dataref are followed
by a subscript list. A data-ref that ends in a component name with a
following subscript list is either an array element or an array
section. A data-ref of nonzero rank that ends with a substring-range
is an array section. A data-ref of zero rank that ends with a
substring-range is a substring.

"""
import pytest
from fparser.two.Fortran2003 import (
    Designator,
    Name,
    Part_Ref,
    Array_Section,
    Program,
    Data_Ref,
    NoMatchError,
    Section_Subscript_List,
    Array_Element,
)
from fparser.api import get_reader


@pytest.mark.usefixtures("f2003_create")
def test_object_name():
    """Test that a Name object is returned when a valid object-name name
    is supplied

    """
    result = Designator("fred")
    assert str(result) == "fred"
    assert isinstance(result, Name)


@pytest.mark.usefixtures("f2003_create")
def test_array_element():
    """Test that the array-element rule is matched when the Designator
    rule is supplied with an array-element.

    # Note that at this time this example would also match rules
    # array-section and structure-component (although it would give
    # the same result so would not matter). It matches the expected
    # rule (array-element) as this rule occurs before array-section
    # and structure-component in the Designator class subclass_names
    # list so is matched first.

    """
    # A Part_Ref is returned (as array-element -> data-ref -> part-ref
    # and a data-ref is not returned if there is only one part-ref)
    result = Designator("a(7,b(i)+c)")
    assert str(result) == "a(7, b(i) + c)"
    assert isinstance(result, Part_Ref)

    # A Data_Ref containing a Part_Ref is returned (as array-element
    # -> data-ref -> part-ref and a data-ref is returned is there is
    # more than one part-ref)
    result = Designator("x%a(1)")
    assert str(result) == "x % a(1)"
    assert isinstance(result, Data_Ref)
    assert isinstance(result.items[0], Name)  # x
    assert isinstance(result.items[1], Part_Ref)  # a(1)


@pytest.mark.xfail(reason="issue #201 Array section is parsed as an array " "element.")
@pytest.mark.usefixtures("f2003_create")
def test_array_section():
    """Test that the array-section rule is matched when the Designator
    rule is supplied with an array-section.

    """
    result = Designator("a(1:2)")
    assert str(result) == "a(1 : 2)"
    assert isinstance(result, Part_Ref)
    assert isinstance(result.items[0], Name)  # a
    assert isinstance(result.items[1], Section_Subscript_List)  # 1:2

    # This example should not match Array_Element as, if it does, it
    # will follow the wrong rule hierarchy (as array-element occurs
    # before array-section in the Designator class
    # subclass_names). This would be avoided if the appropriate
    # constraints were checked, but they are not. However, this does
    # not really matter in this example as we end up with the same
    # parse tree hierarchy in any case (as both rule hierarchies end
    # up with a match to part_ref).
    with pytest.raises(NoMatchError):
        _ = Array_Element("a(1:2)")


@pytest.mark.usefixtures("f2003_create")
def test_array_section2():
    """Test that an Array_Section object is returned when a valid
    array-section (followed by a substring-range) is supplied.

    """
    result = Designator("a(1:2:3)(:)")
    assert str(result) == "a(1 : 2 : 3)(:)"
    assert isinstance(result, Array_Section)


@pytest.mark.xfail(
    reason="issue #201 Structure-component is parsed as an " "array element."
)
@pytest.mark.usefixtures("f2003_create")
def test_structure_component():
    """Test that the structure-component rule is matched when the Designator
    rule is supplied with a structure component.

    """
    result = Designator("parent%scalar_field")
    assert str(result) == "parent % scalar_field"
    assert isinstance(result, Data_Ref)

    # This example should not match Array_Element or Array_Section as,
    # if it does, it will follow the wrong rule hierarchy (as
    # array-element and array-section occur before structure-component
    # in the Designator class subclass_names). This would be avoided
    # if the appropriate constraints were checked, but they are
    # not. However, this does not really matter in this example as we
    # end up with the same parse tree hierarchy in any case (as the
    # rule hierarchies end up with a match to part_ref).
    with pytest.raises(NoMatchError):
        _ = Array_Element("parent%scalar_field")
    with pytest.raises(NoMatchError):
        _ = Array_Section("parent%scalar_field")


@pytest.mark.xfail(reason="issue #201 Substring is parsed as an array " "element.")
@pytest.mark.usefixtures("f2003_create")
def test_substring():
    """Test that an Substring object is returned when a valid substring is
    supplied. This example provides the context (i.e. 'a' is a
    character string) as otherwise it is not possible for the parser
    to distinguish between a substring and an array section and it
    defaults to the latter.

    """
    reader = get_reader(
        "program test\n"
        "character(len=10) :: a\n"
        "a(1:3)='hey'\n"
        "end program test\n"
    )
    result = Program(reader)
    assert "a(1 : 3) = 'hey'" in str(result)
    assert isinstance(result, Program)
    assert "Substring" in repr(result)
