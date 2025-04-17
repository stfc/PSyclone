# Copyright (c) 2018-2021 Science and Technology Facilities Council.

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

"""Test Fortran 2003 rule R404 : This file tests the support for the
kind selector.

"""

import pytest
from fparser.two.utils import InternalError
from fparser.api import get_reader
from fparser.two.Fortran2003 import Kind_Selector, Program

# match() 'kind=' is present


def test_kind(f2003_create):
    """Test that a kind selector with 'kind=' present can be parsed
    successfully.

    """
    reader = get_reader("(KIND=some_kind)")
    ast = Kind_Selector(reader)
    assert "(KIND = some_kind)" in str(ast)


def test_kind_case(f2003_create):
    """Test that a kind selector with 'kind=' present with mixed case can
    be parsed successfully.

    """
    reader = get_reader("(KiNd=some_kind)")
    ast = Kind_Selector(reader)
    assert "(KIND = some_kind)" in str(ast)


def test_kind_spaces(f2003_create):
    """Test that a kind selector with 'kind=' present and spaces can be
    parsed successfully.

    """
    reader = get_reader("  (  kind  =  some_kind  )  ")
    ast = Kind_Selector(reader)
    assert "(KIND = some_kind)" in str(ast)


# match() 'kind=' is not present


def test_nokind(f2003_create):
    """Test that a kind selector with 'kind=' not present can be parsed
    successfully. Note, the parser automatically adds-in kind for the
    output.

    """
    reader = get_reader("(some_kind)")
    ast = Kind_Selector(reader)
    assert "(KIND = some_kind)" in str(ast)


def test_nokind_spaces(f2003_create):
    """Test that a kind selector with 'kind=' not present and spaces can
    be parsed successfully. Note, the parser automatically adds-in
    kind for the output.

    """
    reader = get_reader("  (  some_kind  )  ")
    ast = Kind_Selector(reader)
    assert "(KIND = some_kind)" in str(ast)


def test_nokind_short(f2003_create):
    """Test that a kind selector with 'kind=' not present and a name less
    than 6 characters can be parsed successfully. This is important as
    there is a pattern match for 'kind=' so we need to test we don't
    accidentally access outside of the string. Note, the parser
    automatically adds-in kind for the output.

    """
    reader = get_reader("(a)")
    ast = Kind_Selector(reader)
    assert "(KIND = a)" in str(ast)


@pytest.mark.usefixtures("fake_symbol_table")
def test_nokind_kind_function(f2003_create):
    """Test that a kind selector with 'kind=' not present and a kind
    function can be parsed successfully. This is a test as there is a
    risk that the pattern match will think the kind function is the
    start of 'kind='. Note, the parser automatically adds-in kind for
    the output.

    """
    reader = get_reader("(kind(kind_id))")
    ast = Kind_Selector(reader)
    assert "(KIND = KIND(kind_id))" in str(ast)


# match() *n extension


def test_size_extension(f2003_create):
    """Test that a kind selector with the '*number' format e.g. integer*4
    can be parsed successfully. This is an extension to the standard
    but is well used and is supported in mainstream compilers so is
    also supported here. Note, we can't use a reader as it will not
    read '*2' so we pass the string directly.

    """
    ast = Kind_Selector("*2")
    assert "*2" in str(ast)


def test_size_extension_spaces(f2003_create):
    """Test that a kind selector with the '*number' format e.g. integer*4
    can be parsed successfully if there are spaces between the * and
    the number. This is an extension to the standard but is well used
    and is supported in mainstream compilers so is also supported
    here. Note, we can't use a reader as it will not read '*2' so we
    pass the string directly.

    """
    ast = Kind_Selector("  *  2  ")
    assert "*2" in str(ast)


# match() Syntax errors


def test_error_left_bracket(f2003_create):
    """Test that None is returned if the left bracket is missing."""
    reader = get_reader("KIND = some_kind)")
    ast = Kind_Selector(reader)
    assert not ast


def test_error_right_bracket(f2003_create):
    """Test that None is returned if the right bracket is missing."""
    reader = get_reader("(KIND = some_kind")
    ast = Kind_Selector(reader)
    assert not ast


def test_error_kind_name(f2003_create):
    """Test that None is returned if the name 'kind' is incorrect."""
    reader = get_reader("(KINE = some_kind")
    ast = Kind_Selector(reader)
    assert not ast


def test_error_kind_equals(f2003_create):
    """Test that None is returned if the '=' after 'kind' is not there."""
    reader = get_reader("(KIND some_kind")
    ast = Kind_Selector(reader)
    assert not ast


# match() Internal errors


def test_error_star_only(f2003_create):
    """Test that an InternalError is raised if there are less than two
    characters in the string as valid input must have at least two.

    """
    with pytest.raises(InternalError) as excinfo:
        _ = Kind_Selector("*")
    assert "too short to be valid" in str(excinfo.value)


def test_error_empty(f2003_create):
    """Test that an InternalError is raised if the input is empty."""
    with pytest.raises(InternalError) as excinfo:
        _ = Kind_Selector("")
    assert "too short to be valid" in str(excinfo.value)


def test_error_none(f2003_create):
    """Test that an InternalError is raised if the input is None."""
    with pytest.raises(InternalError) as excinfo:
        _ = Kind_Selector(None)
    assert "method match() is None" in str(excinfo.value)


# tostr() Errors


def test_tostr_error_nitems(f2003_create, monkeypatch):
    """Test that the appropriate exception is raised if the number of
    elements in the self.items list is >3 or <2.

    """
    ast = Kind_Selector("*8")
    monkeypatch.setattr(ast, "items", ["*"])
    with pytest.raises(InternalError) as excinfo:
        _ = str(ast)
    assert "tostr() has '1' items, but expecting 2 or 3" in str(excinfo.value)


# misc


@pytest.mark.xfail(reason="The x gets lost when there is a space in " "front of it.")
def test_to_be_moved(f2003_create):
    """Show that an example with a space before a kind specification
    fails. This is not the fault of this class but I'm including it
    here for the moment until the reason for the error is fixed.

    """
    reader = get_reader(
        """\
      subroutine test()
        integer( x) y
      end subroutine
      """
    )
    ast = Program(reader)
    assert ("SUBROUTINE test\n" "  integer( x) y\n" "END SUBROUTINE") in str(ast)
