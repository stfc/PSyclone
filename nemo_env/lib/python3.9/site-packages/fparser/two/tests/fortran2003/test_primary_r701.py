# Copyright (c) 2019-2021 Science and Technology Facilities Council

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

"""Test Fortran 2003 rule R701 : primary type.

Sub-rules:
    C701 (R701) The type-param-name shall be the name of a type parameter.
    C702 (R701) The designator shall not be a whole assumed-size array

Neither C701 nor C702 can be tested here, as they require context of the
type defined outside of Primary.

"""
import sys

import pytest

import fparser.two.Fortran2003 as f2003
from fparser.two.utils import NoMatchError


def assert_subclass_parse(source, base_type, actual_type=None, expected_str=None):
    """Assert that the given source matches the given ``base_type``
    and optionally the specific type that it should produce.

    :param source: The Fortran source to be parsed.
    :type source: str or :py:class:`FortranReaderBase`
    :param base_type: the base type from which a match is expected to be found
    :type base_type: :py:class:`fortran.two.Fortran2003.Base` subclass
    :param actual_type: The actual type matched by the parser.
    :type actual_type: :py:class:`fortran.two.Fortran2003.Base` subclass
    :param str expected_str: The expected ``str(result)`` of the parsed result

    """
    obj = f2003.Primary(source)
    # Note: Check that the type exists in the possible types, rather than
    # checking the type is an instance of one of the possible types (and
    # all of its ancestor types). This is a much more targetted test as a
    # result.
    assert type(obj) in possible_subclasses(base_type)

    if actual_type:
        assert isinstance(obj, actual_type)
    else:
        assert isinstance(obj, base_type)

    if expected_str:
        assert expected_str == str(obj)


def possible_subclasses(node_type, _seen=None):
    """Given a type (e.g. Fortran2003.Primary), return all of the
    subtypes that could have been matched after parsing.

    NOTE: This is not a general implementation. It is useful for testing
    in some limited situations. Please refer to the Base.__new__ for the
    actual logic for identifying possible_subclasses.

    :param node_type: The root node from which to find all subclasses.
    :type node_type: :py:class:`fortran.two.Fortran2003.Base` subclass
    :param _seen: Private list of seen subclasses, designed to support \
                  recursive calls to this function.
    :type _seen: None or list

    """
    seen = _seen or []
    subclasses = getattr(node_type, "subclass_names", [])
    if node_type not in seen:
        seen.append(node_type)

    for subclass_name in subclasses:
        module = sys.modules[node_type.__module__]
        subclass = getattr(module, subclass_name, None)
        if subclass is not None and subclass not in seen:
            seen.append(subclass)
            possible_subclasses(subclass, seen)
    return seen


@pytest.mark.usefixtures("f2003_create", "fake_symbol_table")
def test_intrinsic_function():
    """Test that an intrinsic function is matched by Primary."""
    assert_subclass_parse(
        "sin(x)",
        f2003.Intrinsic_Function_Reference,
        actual_type=f2003.Intrinsic_Function_Reference,
        expected_str="SIN(x)",
    )


@pytest.mark.usefixtures("f2003_create")
def test_constant():
    """Test that Constant types are matched by Primary."""
    assert_subclass_parse(
        "1.2e-03",
        f2003.Constant,
        actual_type=f2003.Real_Literal_Constant,
        expected_str="1.2E-03",
    )


@pytest.mark.usefixtures("f2003_create")
def test_designator():
    """Test that Designator types are matched by Primary."""
    assert_subclass_parse(
        "array(1:5)",
        f2003.Designator,
        actual_type=f2003.Part_Ref,
        expected_str="array(1 : 5)",
    )


@pytest.mark.usefixtures("f2003_create")
def test_array_constructor():
    """Test that Array Constructor types are matched by Primary."""
    assert_subclass_parse(
        "[ 1.2, 2.3e + 2,    -5.1 e-3 ]",
        f2003.Array_Constructor,
        actual_type=f2003.Array_Constructor,
        expected_str="[1.2, 2.3E+2, - 5.1E-3]",
    )


@pytest.mark.usefixtures("f2003_create")
def test_structure_constructor_1():
    """Test that Structure Constructor types are matched by Primary."""
    assert_subclass_parse(
        'PERSON ( 12,   "Jones" )',
        f2003.Structure_Constructor,
        actual_type=f2003.Structure_Constructor,
        expected_str='PERSON(12, "Jones")',
    )


@pytest.mark.xfail(reason="Requires more parse context (#190)")
@pytest.mark.usefixtures("f2003_create")
def test_structure_constructor_2():
    """Test that Structure Constructor types are matched by Primary.

    This test currently fails as PERSON( 12, 24 ) is matched as an
    array access (Designator). In general, the only way to tell the
    difference is to know whether PERSON is the name of a structure or
    an array. However, we don't keep this information at the moment
    and even when we do in the future, we still might
    not know the information before link time as declarations may be
    in different modules (see issue #201).

    """
    # This test incorrectly Matches with Designator. It would
    # correctly match with Structure_Constructor but Designator is
    # checked first from the Primary class.
    assert_subclass_parse(
        "PERSON ( 12, 24 )",
        f2003.Structure_Constructor,
        actual_type=f2003.Structure_Constructor,
        expected_str="PERSON(12, 24)",
    )


@pytest.mark.xfail(reason="Requires more parse context (#190)")
@pytest.mark.usefixtures("f2003_create")
def test_function_reference():
    """This test demonstrates the inability to distinguish
    Structure_Constructor from Function_Reference without more parse context
    than is currently being provided.
    """
    assert_subclass_parse(
        'a_function(1.2, some_kwarg="hello")', f2003.Function_Reference
    )


@pytest.mark.xfail(reason="Requires more parse context (#190)")
def test_type_param_inquiry():
    """This test demonstrates the inability to distinguish Designator from
    Type_Param_Inquiry without more parse context than is currently being
    provided.
    """
    assert_subclass_parse("X % KIND", f2003.Type_Param_Inquiry)


def test_type_param_name():
    """Test that Type_Param_Name types are matched by Primary."""
    assert_subclass_parse(
        "INTEGER", f2003.Type_Param_Name, actual_type=f2003.Name, expected_str="INTEGER"
    )


@pytest.mark.parametrize(
    "string",
    [
        "(a)",
        "(a + b)",
        "(a + 1)",
        "((a))",
        '("a" + "c")',
        '("a" + ")")',
        "(')' + \")\")",
    ],
)
@pytest.mark.usefixtures("f2003_create")
def test_parenthesis(string):
    """Test that Parenthesis types are matched by the Primary. As
    fparser2 implements this match as a separate class called
    `Parenthesis`, also check this class directly.

    """
    assert_subclass_parse(string, f2003.Parenthesis, expected_str=string)

    result = f2003.Parenthesis(string)
    assert isinstance(result, f2003.Parenthesis)
    assert str(result) == string


@pytest.mark.parametrize("string", ["(a+b)*(c+d)", "()"])
@pytest.mark.parametrize("cls", [f2003.Primary, f2003.Parenthesis])
@pytest.mark.usefixtures("f2003_create")
def test_parenthesis_no_match(string, cls):
    """Test that invalid Parenthesis input is not matched by Primary or
    Parenthesis classes.

    """
    with pytest.raises(NoMatchError) as error:
        _ = cls(string)
    assert "{0}: '{1}'".format(cls.__name__, string) in str(error.value)


@pytest.mark.usefixtures("f2003_create")
def test_no_match():
    """Test that a NoMatchError is raised if we provide code
    that isn't allowed as a Primary type (e.g. a comment).
    """
    with pytest.raises(NoMatchError):
        _ = f2003.Primary("! A comment")


@pytest.mark.xfail(reason="Requires more parse context (#190)")
@pytest.mark.usefixtures("f2003_create")
def test_c701_no_assumed_size_array():
    """Test C701 (R701) The type-param-name shall be the name of a type.
    This test cannot be passed without more parse context of things like
    defined types.
    """
    context = f2003.Type_Declaration_Stmt("INTEGER :: not_a_type")
    with pytest.raises(NoMatchError):
        f2003.Primary("not_a_type")  # context)


@pytest.mark.xfail(reason="Requires more parse context (#190)")
@pytest.mark.usefixtures("f2003_create")
def test_c702_no_assumed_size_array():
    """Test C702 (R701) The designator shall not be a whole assumed-size array.
    This test cannot be passed without more parse context of things like
    defined types.
    """
    context = f2003.Type_Declaration_Stmt("integer(*) :: assumed_size_array")
    with pytest.raises(NoMatchError):
        f2003.Primary("assumed_size_array")  # context)
