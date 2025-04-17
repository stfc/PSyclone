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

"""Test C99 Preprocessor directives: This file tests the preprocessor
directives defined in the C99 standard. Whilst this is not part of any
Fortran standard, most compilers support preprocessing of Fortran files
and support the majority of directives from the C-Standard. Since this
is actively used in user codes there are cases where users might like to
keep the directives in the Fortran parse tree and output it again.

"""
# Author: Balthasar Reuter <balthasar.reuter@ecmwf.int>
# Based on previous work by Martin Schlipf (https://github.com/martin-schlipf)
# First version created: Jan 2020

import pytest
from fparser.two.C99Preprocessor import (
    Cpp_If_Stmt,
    Cpp_Elif_Stmt,
    Cpp_Else_Stmt,
    Cpp_Endif_Stmt,
    Cpp_Include_Stmt,
    Cpp_Macro_Stmt,
    Cpp_Macro_Identifier,
    Cpp_Macro_Identifier_List,
    Cpp_Undef_Stmt,
    Cpp_Line_Stmt,
    Cpp_Error_Stmt,
    Cpp_Warning_Stmt,
    Cpp_Null_Stmt,
    Cpp_Pp_Tokens,
)
from fparser.two.utils import NoMatchError
from fparser.api import get_reader


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("line", ["ABC", "A>5", "!defined(ABC)"])
def test_pp_tokens(line):
    """Test that the generic Cpp_Pp_Tokens class parses correctly"""
    result = Cpp_Pp_Tokens(line)
    assert str(result) == line


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("line", [None, "", "    "])
def test_invalid_pp_tokens(line):
    """Test that the invalid Cpp_Pp_Tokens raise exception"""
    with pytest.raises(NoMatchError) as excinfo:
        _ = Cpp_Pp_Tokens(line)
    assert "Cpp_Pp_Tokens: '{0}'".format(line) in str(excinfo.value)


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize(
    "line",
    [
        "#if CONSTANT",
        "#ifdef MACRO",
        "#ifndef _MACRO",
        "#if defined(__MACRO__)",
        "#if defined __MACRO__",
        "#if !defined(__MACRO__)",
        "#if !defined MACRO",
    ],
)
def test_if_stmt(line):
    """Test that various forms of #if, #ifdef, #ifndef are recognized."""
    result = Cpp_If_Stmt(line)
    assert str(result) == line


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize(
    "line, ref",
    [
        ("  #  if    CONSTANT  ", "#if CONSTANT"),
        ("  #  ifdef  MACRO  ", "#ifdef MACRO"),
        ("  #  ifndef  _MACRO  ", "#ifndef _MACRO"),
        ("# if  defined(__MACRO__)    ", "#if defined(__MACRO__)"),
        ("# if  defined __MACRO__    ", "#if defined __MACRO__"),
        ("# if    !defined(__MACRO__)    ", "#if !defined(__MACRO__)"),
        ("# if  !defined __MACRO__    ", "#if !defined __MACRO__"),
    ],
)
def test_if_stmt_with_whitespaces(line, ref):
    """Test that various forms of #if, #ifdef, #ifndef are recognized when
    whitespaces are added."""
    result = Cpp_If_Stmt(line)
    assert str(result) == ref


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize(
    "line", [None, "", " ", "#ifdfe", "#if", "#ifdef", "#ifdef two macros"]
)
def test_incorrect_if_stmt(line):
    """Test that incorrectly formed #if statements raise exception."""
    with pytest.raises(NoMatchError) as excinfo:
        _ = Cpp_If_Stmt(line)
    assert "Cpp_If_Stmt: '{0}'".format(line) in str(excinfo.value)


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("line", ["#elif CONDITION", "  #  elif   CONDITION "])
def test_elif_stmt(line):
    """Test that #elif is correctly recognized."""
    ref = "#elif CONDITION"
    result = Cpp_Elif_Stmt(line)
    assert str(result) == ref


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("line", [None, "", " ", "#elfi", "#elif"])
def test_incorrect_elif_stmt(line):
    """Test that incorrectly formed #elif statements raise exception."""
    with pytest.raises(NoMatchError) as excinfo:
        _ = Cpp_Elif_Stmt(line)
    assert "Cpp_Elif_Stmt: '{0}'".format(line) in str(excinfo.value)


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("line", ["#else", "  # else  "])
def test_else_stmt(line):
    """Test that #else is correctly recognized"""
    result = Cpp_Else_Stmt(line)
    assert str(result) == line


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("line", [None, "", " ", "#esle", "#elseA", "#Aelse"])
def test_incorrect_else_stmt(line):
    """Test that incorrectly formed #else statements raise exception."""
    with pytest.raises(NoMatchError) as excinfo:
        _ = Cpp_Else_Stmt(line)
    assert "Cpp_Else_Stmt: '{0}'".format(line) in str(excinfo.value)


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("line", ["#endif", "  #  endif  "])
def test_endif_stmt(line):
    """Test that #endif is correctly recognized."""
    result = Cpp_Endif_Stmt(line)
    assert str(result) == line


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("line", [None, "", " ", "#ednif", "#endifA", "#Aendif"])
def test_incorrect_endif_stmt(line):
    """Test that incorrectly formed #endif statements raise exception."""
    with pytest.raises(NoMatchError) as excinfo:
        _ = Cpp_Endif_Stmt(line)
    assert "Cpp_Endif_Stmt: '{0}'".format(line) in str(excinfo.value)


def test_parse_define_outside_subroutine(f2003_parser):
    """Test parsing of #define outside of a subroutine."""
    code = "#define MACRO\nSUBROUTINE FOO\n  CALL sub\nEND SUBROUTINE FOO"
    reader = get_reader(code)
    result = f2003_parser(reader)
    assert str(result) == code


@pytest.mark.parametrize(
    "lines",
    [
        ["SUBROUTINE FOO", "  #ifdef __BAR__", "  #endif", "END SUBROUTINE FOO"],
        [
            "SUBROUTINE FOO(BAR)",
            "  INTEGER, INTENT(INOUT) :: BAR",
            "  CALL FOOBAR(BAR)",
            "  #define MACRO_NAME",
            "END SUBROUTINE FOO",
        ],
        [
            "SUBROUTINE FOO(BAZ)",
            "  INTEGER, INTENT(INOUT) :: BAZ",
            "  #define MACRO_NAME",
            "  CALL FOOBAZ(BAR)",
            "END SUBROUTINE FOO",
        ],
        [
            "SUBROUTINE FOO(BAZ2)",
            "  #define MACR",
            "  INTEGER, INTENT(IN) :: BAZ2",
            "  CALL FOOBAZ2(BAR)",
            "END SUBROUTINE FOO",
        ],
        [
            "SUBROUTINE FOO(BAR2)",
            "  INTEGER, INTENT(IN) :: BAR2",
            "  #ifdef MACRO",
            "  CALL FOO2(BAR)",
            "  #else",
            '  CALL ABORT("error msg")',
            "  #endif",
            "  BAR2 = BAR2 + 1",
            "END SUBROUTINE FOO",
        ],
        [
            "SUBROUTINE FOO(BAR3)",
            "  #ifdef MACRO",
            "  INTEGER, INTENT(IN) :: BAR3",
            "  CALL FOOBAR3(BAR)",
            "  #endif",
            "END SUBROUTINE FOO",
        ],
        [
            "#define MACRO",
            "SUBROUTINE BAR",
            "  #if defined(MACRO)",
            "  CALL sub1",
            "  #elif FOO",
            "  CALL sub2",
            "  #else",
            "  CALL sub3",
            "  #endif",
            "END SUBROUTINE BAR",
        ],
    ],
)
def test_directive_at_different_places(f2003_parser, lines):
    """Test that directives are parsed correctly irrespective of where they
    appear in the code."""
    code = "\n".join(line.strip() for line in lines)
    ref = "\n".join(lines)
    reader = get_reader(code)
    result = f2003_parser(reader)
    assert str(result) == ref


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize(
    "line", ['#include "filename.inc"', '   #   include  "filename.inc"  ']
)
def test_include_stmt_quotes(line):
    """Test that #include is recognized"""
    ref = '#include "filename.inc"'
    result = Cpp_Include_Stmt(line)
    assert str(result) == ref


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize(
    "line", ["#include <filename.inc>", "   #   include  <filename.inc>  "]
)
def test_include_stmt_bracket(line):
    """Test that #include is recognized"""
    ref = '#include "filename.inc"'
    result = Cpp_Include_Stmt(line)
    assert str(result) == ref


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize(
    "line",
    [
        None,
        "",
        "  ",
        "#includ",
        '#includ "x"',
        "#include",
        '#include ""',
        "#include 'x'",
        '#include "x',
        '#include x"',
        "#include x",
        '#include x"x"',
        '#include "x"x',
        'x #include "x"',
        '#includex "x"',
        "#include 'abc'",
        '#include " a.inc"',
        '#include "      "',
    ],
)
def test_incorrect_include_stmt(line):
    """Test that incorrectly formed #include statements raise exception"""
    with pytest.raises(NoMatchError) as excinfo:
        _ = Cpp_Include_Stmt(line)
    assert "Cpp_Include_Stmt: '{0}'".format(line) in str(excinfo.value)


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize(
    "line",
    [
        "#define MACRO value",
        "#define _MACRO",
        "#define MACRO(x) call func(x)",
        "#define MACRO (x, y)",
        "#define MACRO(x, y) (x) + (y)",
        "#define MACRO(a, b, c, d) (a) * (b) + (c) * (d)",
        "#define eprintf(...) fprintf (stderr, __VA_ARGS__)",
        "#define report(tst, ...) ((tst)?puts(#tst):printf(__VA_ARGS__))",
        "#define hash_hash # ## #",
        "#define TABSIZE 100",
        "#define r(x,y) x ## y",
        "#define MACRO(a, bbb, c_d) (a) * (bbb + c_d)",
        "#define MACRO x",
        "#define MACRO( a,b2_ ,   c) (a )*    (   b2_   + c  )",
        "#define omp_get_num_threads() 1",
        "#define MACRO(a2aa, b, c)",
    ],
)
def test_macro_stmt(line):
    """Test that #define is recognized"""
    result = Cpp_Macro_Stmt(line)
    assert str(result) == line


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize(
    "line, ref",
    [
        ("  #  define   MACRO   value  ", "#define MACRO value"),
        ("   #  define  _MACRO  ", "#define _MACRO"),
    ],
)
def test_macro_stmt_with_whitespace(line, ref):
    """Test that #define is recognized when white space are added."""
    result = Cpp_Macro_Stmt(line)
    assert str(result) == ref


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize(
    "line",
    [
        None,
        "",
        " ",
        "#def",
        "#defnie",
        "#definex",
        "#define 2a" "#define fail(...,test) test",
        "#define",
        "#define fail(...,...)",
    ],
)
def test_incorrect_macro_stmt(line):
    """Test that incorrectly formed #define statements raise exception"""
    with pytest.raises(NoMatchError) as excinfo:
        _ = Cpp_Macro_Stmt(line)
    assert "Cpp_Macro_Stmt: '{0}'".format(line) in str(excinfo.value)


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize(
    "name", ["MACRO", "MACRO1", "MACRO_", "MACRO_NAME", "macro", "_", "_12"]
)
def test_macro_identifier(name):
    """Test that all allowed names can be parsed"""
    result = Cpp_Macro_Identifier(name)
    assert str(result) == name


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("name", ["12MACRO", "+MACRO1", "//MACRO_", "MAC RO", ""])
def test_invalid_macro_identifier(name):
    """Test that invalid names raise exceptions"""
    with pytest.raises(NoMatchError) as excinfo:
        _ = Cpp_Macro_Identifier(name)
    assert "Cpp_Macro_Identifier: '{0}'".format(name) in str(excinfo.value)


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("line", ["(a, b, ...)", "(...)", "(a)", "(a, b)"])
def test_macro_identifier_list(line):
    """Test that correct lists are parsed"""
    result = Cpp_Macro_Identifier_List(line)
    assert str(result) == line


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("line", [None, "(a, ..., b)", "(..., ...)", "(..)", "(....)"])
def test_invalid_macro_identifier_list(line):
    """Test that invalid lists raise exceptions"""
    with pytest.raises(NoMatchError) as excinfo:
        _ = Cpp_Macro_Identifier_List(line)
    msg = "Cpp_Macro_Identifier_List: '{0}'".format(line)
    assert msg in str(excinfo.value)


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("line", ["#undef _MACRO", "   #  undef  _MACRO  "])
def test_undef_stmt(line):
    """Test that #undef is recognized"""
    ref = "#undef _MACRO"
    result = Cpp_Undef_Stmt(line)
    assert str(result) == ref


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("line", [None, "", " ", "#undef", "#unfed", "#undefx"])
def test_incorrect_undef_stmt(line):
    """Test that incorrectly formed #undef statements raise exception"""
    with pytest.raises(NoMatchError) as excinfo:
        _ = Cpp_Undef_Stmt(line)
    assert "Cpp_Undef_Stmt: '{0}'".format(line) in str(excinfo.value)


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("line", ["#line CONFIG", "  #  line   CONFIG  "])
def test_line_statement(line):
    """Test that #line is recognized"""
    ref = "#line CONFIG"
    result = Cpp_Line_Stmt(line)
    assert str(result) == ref


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("line", [None, "", " ", "#line", "#linex", "#lien"])
def test_incorrect_line_stmt(line):
    """Test that incorrectly formed #line statements raise exception"""
    with pytest.raises(NoMatchError) as excinfo:
        _ = Cpp_Line_Stmt(line)
    assert "Cpp_Line_Stmt: '{0}'".format(line) in str(excinfo.value)


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("line", ["#error MSG", "  #  error  MSG  "])
def test_error_statement_with_msg(line):
    """Test that #error is recognized"""
    # error with message
    ref = "#error MSG"
    result = Cpp_Error_Stmt(line)
    assert str(result) == ref


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("line", ["#error", "  #   error"])
def test_error_statement_without_msg(line):
    """Test that #error is recognized"""
    # error without message
    ref = "#error"
    result = Cpp_Error_Stmt(line)
    assert str(result) == ref


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("line", [None, "", " ", "#erorr", "#errorx"])
def test_incorrect_error_stmt(line):
    """Test that incorrectly formed #error statements raise exception"""
    with pytest.raises(NoMatchError) as excinfo:
        _ = Cpp_Error_Stmt(line)
    assert "Cpp_Error_Stmt: '{0}'".format(line) in str(excinfo.value)


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("line", ["#warning MSG", "  #  warning  MSG  "])
def test_warning_statement_with_msg(line):
    """Test that #warning is recognized (not actually part of C99)
    with a message."""
    ref = "#warning MSG"
    result = Cpp_Warning_Stmt(line)
    assert str(result) == ref


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("line", ["#warning ", "  #  warning"])
def test_warning_statement_without_msg(line):
    """Test that #warning is recognized (not actually part of C99)
    without a message."""
    ref = "#warning"
    result = Cpp_Warning_Stmt(line)
    assert str(result) == ref


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("line", [None, "", " ", "#wrning", "#warningx"])
def test_incorrect_warning_stmt(line):
    """Test that incorrectly formed #warning statements raise exception"""
    with pytest.raises(NoMatchError) as excinfo:
        _ = Cpp_Warning_Stmt(line)
    assert "Cpp_Warning_Stmt: '{0}'".format(line) in str(excinfo.value)


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("line", ["#", "   #  ", "#   ", "   #"])
def test_null_stmt(line):
    """Test that null directives are recognized"""
    ref = "#"
    result = Cpp_Null_Stmt(line)
    assert str(result) == ref


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("line", [None, "", " ", "#a", "##", "###", "  # #", "# a"])
def test_incorrect_null_stmt(line):
    """Test that anything that is not a single hash sign is not recognized"""
    with pytest.raises(NoMatchError) as excinfo:
        _ = Cpp_Null_Stmt(line)
    assert "Cpp_Null_Stmt: '{0}'".format(line) in str(excinfo.value)
