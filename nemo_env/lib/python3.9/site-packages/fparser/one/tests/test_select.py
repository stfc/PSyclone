# Modified work Copyright (c) 2017-2018 Science and Technology
# Facilities Council
# Original work Copyright (c) 1999-2008 Pearu Peterson
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

# --------------------------------------------------------------------

# The original software (in the f2py project) was distributed under
# the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:

#   a. Redistributions of source code must retain the above copyright notice,
#      this list of conditions and the following disclaimer.
#   b. Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in the
#      documentation and/or other materials provided with the distribution.
#   c. Neither the name of the F2PY project nor the names of its
#      contributors may be used to endorse or promote products derived from
#      this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR
# ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.

"""
Test fparser support for parsing select-case and select-type blocks

"""

import pytest
import fparser.common.sourceinfo


# We need to monkeypatch the logger used by fparser because it grabs
# stdout before the pytest framework can intercept it. In python < 3
# you can't make a lambda out of 'print' because print is not a
# function (you cannot do "x = print y" for instance). Therefore we
# have to create our own function that simply wraps print and returns
# a value.
def print_wrapper(arg):
    """A wrapper that allows us to call print as a function. Used for
    monkeypatching logging calls."""
    print(arg)
    return None


def test_case():
    """Basic tests for parsing of individual case statements"""
    from fparser.one.tests.test_parser import parse
    from fparser.one.block_statements import Case

    assert parse(Case, "case (1)") == "CASE ( 1 )"
    assert parse(Case, "case (1:)") == "CASE ( 1 : )"
    assert parse(Case, "case (:1)") == "CASE ( : 1 )"
    assert parse(Case, "case (1:2)") == "CASE ( 1 : 2 )"
    assert parse(Case, "case (a(1,2))") == "CASE ( a(1,2) )"
    assert parse(Case, 'case ("ab")') == 'CASE ( "ab" )'
    assert parse(Case, "case default") == "CASE DEFAULT"
    assert parse(Case, "case (1:2 ,3:4)") == "CASE ( 1 : 2, 3 : 4 )"
    assert parse(Case, "case (a(1,:):)") == "CASE ( a(1,:) : )"
    assert parse(Case, "case default") == "CASE DEFAULT"


def test_case_internal_error(monkeypatch, capsys):
    """Check that expected errors are raised when invalid case
    statements are encountered"""
    from fparser.one.block_statements import Case
    from fparser.common.readfortran import FortranStringReader

    reader = FortranStringReader("CASE (yes)")
    reader.set_format(fparser.common.sourceinfo.FortranFormat(True, False))
    item = next(reader)
    stmt = Case(item, item)
    # Monkeypatch our valid Case object so that get_line() now
    # returns something invalid. We have to do it this way
    # because if we started with this text then we wouldn't get
    # past the match() method
    monkeypatch.setattr(stmt.item, "get_line", lambda: "case invalid")
    # Monkeypatch the Case object so that a call to self.warning
    # (which normally results in a call to the logger) gets replaced
    # with a call to our print_wrapper() function
    monkeypatch.setattr(stmt, "warning", print_wrapper)
    stmt.process_item()
    output, _ = capsys.readouterr()
    print(output)
    assert "Internal error when parsing CASE statement" in output


def test_class_internal_error(monkeypatch, capsys):
    """Check that expected errors are raised when invalid CLASS
    statements are encountered"""
    from fparser.one.block_statements import ClassIs
    from fparser.common.readfortran import FortranStringReader

    reader = FortranStringReader("CLASS IS (yes)")
    reader.set_format(fparser.common.sourceinfo.FortranFormat(True, False))
    item = next(reader)
    stmt = ClassIs(item, item)
    # Monkeypatch our valid Case object so that get_line() now
    # returns something invalid. We have to do it this way
    # because if we started with this text then we wouldn't get
    # past the match() method
    monkeypatch.setattr(stmt.item, "get_line", lambda: "class invalid")
    # Monkeypatch the Case object so that a call to self.warning
    # (which normally results in a call to the logger) gets replaced
    # with a call to our print_wrapper() function
    monkeypatch.setattr(stmt, "warning", print_wrapper)
    stmt.process_item()
    output, _ = capsys.readouterr()
    assert "Internal error when parsing CLASS statement" in output


def test_select_case():
    """Test that fparser correctly recognises select case"""
    from fparser import api

    source_str = """
    subroutine foo
    integer :: iflag = 1
    real    :: aval = 0.0
    select case(iflag)
    case(1)
      aval = 1.0
    case default
      aval = 0.0
    end select
    ! Same again but with more white space
    select   case(iflag)
    case ( 1 )
      aval = 1.0
    case  default
      aval = 0.0
    end select
    end subroutine foo
    """
    tree = api.parse(source_str, isfree=True, isstrict=False, ignore_comments=True)
    assert tree
    select_list = []
    for statement in tree.content[0].content:
        if isinstance(statement, fparser.one.block_statements.SelectCase):
            select_list.append(statement)
    assert len(select_list) == 2
    for statement in select_list:
        assert isinstance(statement, fparser.one.block_statements.SelectCase)
        assert isinstance(statement.content[0], fparser.one.statements.Case)
        assert isinstance(statement.content[2], fparser.one.statements.Case)
        assert isinstance(statement.content[3], fparser.one.statements.Assignment)
    gen = str(tree)
    print(gen)
    assert "SELECT CASE ( iflag )" in gen


@pytest.mark.xfail(reason="fparser does not work with named select statements")
def test_named_select_case():
    """Test that fparser correctly recognises a named select case"""
    from fparser import api

    source_str = """
    subroutine foo
    integer :: iflag = 1
    real    :: aval = 0.0
    incase: select case(iflag)
    case(1) incase
      aval = 1.0
    case default   incase
      aval = 0.0
    end select  incase
    end subroutine foo
    """
    tree = api.parse(source_str, isfree=True, isstrict=False)
    assert tree
    select_list = []
    for statement in tree.content[0].content:
        if isinstance(statement, fparser.one.block_statements.SelectCase):
            select_list.append(statement)
    assert len(select_list) == 2
    for statement in select_list:
        assert isinstance(statement, fparser.one.block_statements.SelectCase)
        assert isinstance(statement.content[0], fparser.one.statements.Case)
        assert isinstance(statement.content[2], fparser.one.statements.Case)
        assert isinstance(statement.content[3], fparser.one.statements.Assignment)
    gen = str(tree)
    print(gen)
    assert "incase: SELECT CASE ( iflag )" in gen


def test_select_case_brackets():
    """Test that fparser correctly parses a select case involving
    parentheses"""
    from fparser import api

    source_str = """
    subroutine foo
    integer :: iflag(2) = 1
    real    :: aval = 0.0
    select case(iflag(1))
    case(1)
      aval = 1.0
    case default
      aval = 0.0
    end select
    end subroutine foo
    """
    tree = api.parse(source_str, isfree=True, isstrict=False)
    assert tree
    statement = None  # Keep pylint happy
    for statement in tree.content[0].content:
        if isinstance(statement, fparser.one.block_statements.SelectCase):
            break
    assert isinstance(statement, fparser.one.block_statements.SelectCase)
    assert isinstance(statement.content[0], fparser.one.statements.Case)
    assert isinstance(statement.content[2], fparser.one.statements.Case)
    assert isinstance(statement.content[3], fparser.one.statements.Assignment)
    gen = str(tree)
    print(gen)
    assert "SELECT CASE ( iflag(1) )" in gen


def test_select_type():
    """Test that fparser correctly recognises select type"""
    from fparser import api

    source_str = """
    subroutine foo(an_object)
    class(*) :: an_object
    real    :: aval = 0.0
    select type(an_object)
    type is (some_type)
      aval = 1.0
    class is (some_class)
      aval = 0.0
    class default
      aval = -1.0
    end select
    ! Same again but with more white space
    select   type ( an_object )
    type  is  ( some_type(i), some_other_type )
      aval = 1.0
    class  is  ( some_class(1), some_other_class )
      aval = 0.0
    class   default
      aval = -1.0
    end select
    end subroutine foo
    """
    tree = api.parse(source_str, isfree=True, isstrict=False)
    assert tree
    select_list = []
    for statement in tree.content[0].content:
        if isinstance(statement, fparser.one.block_statements.SelectType):
            select_list.append(statement)
    assert len(select_list) == 2
    for statement in select_list:
        assert isinstance(statement, fparser.one.block_statements.SelectType)
        assert isinstance(statement.content[0], fparser.one.statements.TypeIs)
        assert isinstance(statement.content[2], fparser.one.statements.ClassIs)
        assert isinstance(statement.content[3], fparser.one.statements.Assignment)
        assert isinstance(statement.content[4], fparser.one.statements.ClassIs)
    gen = str(tree)
    print(gen)
    assert "SELECT TYPE ( an_object )" in gen
    assert "TYPE IS ( some_type )" in gen
    assert "TYPE IS ( some_type(i), some_other_type )" in gen
    assert "CLASS IS ( some_class )" in gen
    assert "CLASS DEFAULT" in gen
    assert "CLASS IS ( some_class(1), some_other_class )" in gen


def test_type_is_process_item(monkeypatch, capsys):
    """Test error condition raised in TypeIs.process_item() method"""
    from fparser import api

    source_str = """
    subroutine foo(an_object)
    class(*) :: an_object
    real    :: aval = 0.0
    select type(an_object)
    type is (some_type)
      aval = 1.0
    class is (some_class)
      aval = 0.0
    end select
    end subroutine foo
    """
    tree = api.parse(source_str, isfree=True, isstrict=False)
    assert tree
    statement = None  # Keeps pylint happy
    for statement in tree.content[0].content:
        if isinstance(statement, fparser.one.block_statements.SelectType):
            break
    assert isinstance(statement, fparser.one.block_statements.SelectType)
    assert isinstance(statement.content[0], fparser.one.statements.TypeIs)
    typeis = statement.content[0]
    typeis.parent.name = "not_a_name"
    monkeypatch.setattr(typeis.item, "get_line", lambda: "type is (blah): wrong_name")
    # Monkeypatch the typeis object so that a call to self.warning
    # (which normally results in a call to the logger) gets replaced
    # with a call to our print_wrapper() function
    monkeypatch.setattr(typeis, "warning", print_wrapper)
    typeis.process_item()
    output, _ = capsys.readouterr()
    print(output)
    assert "expected type-is-construct-name 'not_a_name' but got " in output


def test_type_is_to_fortran():
    """Test error condition raised in TypeIs.to_fortran() method"""
    from fparser import api
    from fparser.common.utils import ParseError

    source_str = """
    subroutine foo(an_object)
    class(*) :: an_object
    real    :: aval = 0.0
    select type(an_object)
    type is (some_type)
      aval = 1.0
    class is (some_class)
      aval = 0.0
    end select
    end subroutine foo
    """
    tree = api.parse(source_str, isfree=True, isstrict=False)
    assert tree
    statement = None  # Keeps pylint happy
    for statement in tree.content[0].content:
        if isinstance(statement, fparser.one.block_statements.SelectType):
            break
    assert isinstance(statement, fparser.one.block_statements.SelectType)
    assert isinstance(statement.content[0], fparser.one.statements.TypeIs)
    typeis = statement.content[0]
    typeis.name = "some_name"
    fort = typeis.tofortran()
    assert "TYPE IS ( some_type ) some_name" in fort
    # Now break the internal state and check for expected exception
    typeis.items = None
    with pytest.raises(ParseError) as excinfo:
        _ = typeis.tofortran()
    assert "TYPE IS construct must have arguments" in str(excinfo.value)


def test_class_is_process_item(monkeypatch, capsys):
    """Test error condition raised in ClassIs.process_item() method"""
    from fparser import api

    source_str = """
    subroutine foo(an_object)
    class(*) :: an_object
    real    :: aval = 0.0
    select type(an_object)
    class is (some_class)
      aval = 0.0
    class default
      aval = -1.0
    end select
    end subroutine foo
    """
    tree = api.parse(source_str, isfree=True, isstrict=False)
    assert tree
    statement = None  # Keeps pylint happy
    for statement in tree.content[0].content:
        if isinstance(statement, fparser.one.block_statements.SelectType):
            break
    assert isinstance(statement, fparser.one.block_statements.SelectType)
    assert isinstance(statement.content[0], fparser.one.statements.ClassIs)
    clsis = statement.content[0]
    clsis.parent.name = "not_a_name"
    monkeypatch.setattr(clsis.item, "get_line", lambda: "class is (blah): wrong_name")
    # Monkeypatch the typeis object so that a call to self.warning
    # (which normally results in a call to the logger) gets replaced
    # with a call to our print_wrapper() function
    monkeypatch.setattr(clsis, "warning", print_wrapper)
    clsis.process_item()
    output, _ = capsys.readouterr()
    print(output)
    assert "expected class-construct-name 'not_a_name' but got " in output


def test_class_is_to_fortran():
    """Test ClassIs.to_fortran() method"""
    from fparser import api

    source_str = """
    subroutine foo(an_object)
    class(*) :: an_object
    real    :: aval = 0.0
    select type(an_object)
    class is (some_class)
      aval = 0.0
    end select
    end subroutine foo
    """
    tree = api.parse(source_str, isfree=True, isstrict=False)
    assert tree
    statement = None  # Keeps pylint happy
    for statement in tree.content[0].content:
        if isinstance(statement, fparser.one.block_statements.SelectType):
            break
    assert isinstance(statement, fparser.one.block_statements.SelectType)
    assert isinstance(statement.content[0], fparser.one.statements.ClassIs)
    clsis = statement.content[0]
    clsis.name = "some_name"
    fort = clsis.tofortran()
    assert "CLASS IS ( some_class ) some_name" in fort
