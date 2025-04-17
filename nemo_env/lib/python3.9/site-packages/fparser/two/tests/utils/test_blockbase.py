# Copyright (c) 2019-2021, Science and Technology Facilities Council

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

""" File containing unit tests for the BlockBase baseclass in utils.py """

import pytest

from fparser.api import get_reader
from fparser.two.symbol_table import SYMBOL_TABLES
from fparser.two.utils import BlockBase
import fparser.two.Fortran2003 as F2003

# TODO #179: full testing of this class. We currently only test the
# comment and include support.


def test_include(f2003_create):
    """Test the BlockBase match method supports include statements and
    comments before the class that indicates the start of the block
    (the start class).

    """
    # Use the Main Program rule R1101 as an example
    startcls = F2003.Program_Stmt
    subclasses = [
        F2003.Specification_Part,
        F2003.Execution_Part,
        F2003.Internal_Subprogram_Part,
    ]
    endcls = F2003.End_Program_Stmt
    reader = get_reader(
        (
            "include '1'\n"
            "! comment1\n"
            "program test\n"
            "include '2'\n"
            "! comment2\n"
            "integer :: i\n"
            "include '3'\n"
            "! comment3\n"
            "i=1\n"
            "include '4'\n"
            "! comment4\n"
            "contains\n"
            "include '5'\n"
            "! comment5\n"
            "end program test\n"
            "! I should be ignored"
            "include 'so should I'"
        ),
        ignore_comments=False,
    )
    result = BlockBase.match(startcls, subclasses, endcls, reader)
    assert (
        "([Include_Stmt(Include_Filename('1')), Comment('! comment1'), "
        "Program_Stmt('PROGRAM', Name('test')), Specification_Part("
        "Implicit_Part(Include_Stmt(Include_Filename('2')), "
        "Comment('! comment2')), Type_Declaration_Stmt(Intrinsic_Type_Spec("
        "'INTEGER', None), None, Entity_Decl_List(',', (Entity_Decl(Name("
        "'i'), None, None, None),))), Implicit_Part(Include_Stmt("
        "Include_Filename('3')), Comment('! comment3'))), Execution_Part("
        "Assignment_Stmt(Name('i'), '=', Int_Literal_Constant('1', None)), "
        "Include_Stmt(Include_Filename('4')), Comment('! comment4')), "
        "Internal_Subprogram_Part(Contains_Stmt('CONTAINS'), Include_Stmt("
        "Include_Filename('5')), Comment('! comment5')), End_Program_Stmt("
        "'PROGRAM', Name('test'))],)" in str(result).replace("u'", "'")
    )
    assert "should" not in str(result)


@pytest.mark.parametrize("strict_order", [True, False])
def test_strict_order_invalid_code(f2003_create, strict_order):
    """Check that the strict_order flag toggles the parse behaviour as
    expected.
    """
    subclasses = [F2003.Specification_Part, F2003.Execution_Part]
    reader = get_reader(
        """
        program main
            i = 2
            integer :: i
        end program main
        """
    )

    expected = remove_indentation(
        """([
      Program_Stmt('PROGRAM', Name('main')),
      Execution_Part(
          Assignment_Stmt(Name('i'), '=',
          Int_Literal_Constant('2', None))),
      Specification_Part(
          Type_Declaration_Stmt(
              Intrinsic_Type_Spec('INTEGER', None),
              None,
              Entity_Decl_List(
                  ',',
                  (Entity_Decl(Name('i'), None, None, None),)))),
      End_Program_Stmt('PROGRAM', Name('main'))
     ],)
    """
    )

    result = BlockBase.match(
        F2003.Program_Stmt,
        subclasses,
        F2003.End_Program_Stmt,
        reader,
        strict_order=strict_order,
    )

    if strict_order:
        assert result is None
    else:
        assert str(result) == expected


def test_strict_order_valid_code(f2003_create):
    """Tests that the strict_order keyword allows repeated types."""
    subclasses = [F2003.Specification_Part, F2003.Execution_Part]
    reader = get_reader(
        """
        program main
            integer :: i
            real    :: rho
            i = 2
            rho = i * 3.14
        end program main
        """
    )

    expected = remove_indentation(
        """([
      Program_Stmt('PROGRAM', Name('main')),
      Specification_Part(
          Type_Declaration_Stmt(
              Intrinsic_Type_Spec('INTEGER', None), None,
              Entity_Decl_List(
                  ',',
                  (Entity_Decl(Name('i'), None, None, None),))),
          Type_Declaration_Stmt(
              Intrinsic_Type_Spec('REAL', None), None,
              Entity_Decl_List(
                  ',',
                  (Entity_Decl(Name('rho'), None, None, None),)))),
      Execution_Part(
          Assignment_Stmt(Name('i'), '=', Int_Literal_Constant('2', None)),
          Assignment_Stmt(
              Name('rho'), '=', Add_Operand(Name('i'), '*',
              Real_Literal_Constant('3.14', None)))),
      End_Program_Stmt('PROGRAM', Name('main'))
     ],)
    """
    )
    result = BlockBase.match(
        F2003.Program_Stmt,
        subclasses,
        F2003.End_Program_Stmt,
        reader,
        strict_order=True,
    )

    assert str(result) == expected


def test_label_do_construct_hook_match(f2003_create):
    """Check that specifying the enable_do_label_construct_hook to the match
    method allows multiple loops sharing the same label to be matched.
    """
    subclasses = [F2003.Assignment_Stmt, F2003.Continue_Stmt]
    code = """
        do 100 i = 1,10
          j = 10
          do 100 j = 1,10
            k = 5
        100 continue
        """
    reader = get_reader(code)
    result = BlockBase.match(
        F2003.Label_Do_Stmt,
        subclasses,
        None,
        reader,
        match_labels=True,
        enable_do_label_construct_hook=True,
    )
    assert isinstance(result[0][0], F2003.Label_Do_Stmt)
    assert isinstance(result[0][2], F2003.Label_Do_Stmt)
    # Without the `enable_do_label_construct_hook` set, we only match
    # the first loop.
    reader = get_reader(code)
    result = BlockBase.match(
        F2003.Label_Do_Stmt, subclasses, None, reader, match_labels=True
    )
    assert len(result[0]) == 2


def test_label_do_nomatch(f2003_create):
    """
    Check that the match() method returns None for an invalid do
    statement.
    """
    subclasses = [F2003.Assignment_Stmt, F2003.Continue_Stmt]
    reader = get_reader(
        """
        do 100 i-10
          j = 10
        100 continue
        """
    )
    result = BlockBase.match(
        F2003.Label_Do_Stmt,
        subclasses,
        None,
        reader,
        match_labels=True,
        enable_do_label_construct_hook=True,
    )
    assert result is None


def remove_indentation(string):
    """
    A utility function that removes indented multiline strings of reprs.

    :param str string: the string to dedent.
    :returns: a dedented version of the string.
    :rtype: str

    """
    # Note, unlike textwrap.dedent this function removes the leading
    # whitespace on each line in a context sensitive way.
    block_indent = 0
    result = ""
    for line in string.split("\n"):
        this_indent = len(line) - len(line.lstrip())
        line = line.lstrip()
        if this_indent > block_indent or not result.endswith(","):
            result += line
        else:
            result += " " + line
        block_indent = this_indent
    return result.strip()


@pytest.mark.usefixtures("f2003_create")
def test_syntax_error_nested_symbol_table():
    """
    Test that a syntax error within a nested scoping region is handled
    correctly. We use some code that has a mis-spelt attribute on a
    declaration to trigger a syntax error.

    """
    reader = get_reader(
        """
module my_mod
contains
FUNCTION dot_v_mod_2d( )
  REAL ::  dot_v_mod_2d
  REAL, DIMENSION(:,:), POINTER, CONTIOUS :: z_msk_i
  dot_v_mod_2d = 0.0_wp
END FUNCTION dot_v_mod_2d
end module my_mod
"""
    )
    result = F2003.Module.match(reader)
    # There should be no match and, as a result, there should be no
    # symbol-table entries.
    assert result is None
    assert SYMBOL_TABLES._symbol_tables == {}
