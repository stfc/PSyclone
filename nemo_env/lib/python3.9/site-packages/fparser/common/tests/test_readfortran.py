# -*- coding: utf-8 -*-
##############################################################################
# Copyright (c) 2017-2024 Science and Technology Facilities Council.
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
##############################################################################
# Modified M. Hambley and P. Elson, Met Office
# Modified R. W. Ford and A. R. Porter, STFC Daresbury Lab
# Modified by J. Henrichs, Bureau of Meteorology
##############################################################################
"""
Test battery associated with fparser.common.readfortran package.
"""

import io
import os.path
import tempfile
import pytest

from fparser.common.readfortran import (
    FortranFileReader,
    FortranStringReader,
    FortranReaderBase,
    FortranReaderError,
    Line,
    extract_label,
    extract_construct_name,
    CppDirective,
    Comment,
)
from fparser.common.sourceinfo import FortranFormat


@pytest.fixture(scope="module", name="f2py_enabled", params=[True, False])
def f2py_enabled_fixture(request):
    """Fixture that returns whether or not to enable reader support for
    f2py directives."""
    return request.param


def test_empty_line_err():
    """Check that we raise the expected error if we try and create
    an empty Line"""
    with pytest.raises(FortranReaderError) as err:
        _ = Line("   ", 1, "", "a_name", None)
    assert "Got empty line: '   '" in str(err.value)


def test_line_map():
    """Check the application and reversal of tokenisation of a Line for
    strings, expressions in parentheses and literal constants with an
    exponent.

    """
    my_code = (
        "program test\n"
        " real :: var\n"
        " var = 1.0e-3\n"
        " var = var * (var + 1.0d-4)\n"
        " write(*,*) 'var = ', var\n"
        "end program test\n"
    )
    reader = FortranStringReader(my_code, ignore_comments=True)
    for _ in range(3):
        line = reader.next()
    assert line.get_line() == "var = F2PY_REAL_CONSTANT_1_"
    assert line.get_line(apply_map=True) == "var = 1.0e-3"
    line = reader.next()
    assert line.get_line() == "var = var * (F2PY_EXPR_TUPLE_1)"
    assert line.get_line(apply_map=True) == "var = var * (var + 1.0d-4)"
    line = reader.next()
    assert line.get_line() == (
        "write(F2PY_EXPR_TUPLE_1) " "'_F2PY_STRING_CONSTANT_1_', var"
    )
    assert line.get_line(apply_map=True) == "write(*,*) 'var = ', var"


def test_fortranreaderbase_logging(log, monkeypatch):
    """
    Tests the logging functionality of the FortranReaderBase class.

    """

    class FailFile:
        """
        A "file-like" object which returns a line of Fortran source followed
        by raising a StopIteration exception.
        """

        _stuff = ["x=1"]

        def __next__(self):
            """
            :returns: the next line of source.
            :rtype: str
            """
            return self._stuff.pop()

    monkeypatch.setattr(
        "fparser.common.readfortran.FortranReaderBase.id",
        lambda x: "foo",
        raising=False,
    )
    mode = FortranFormat(True, False)
    unit_under_test = FortranReaderBase(FailFile(), mode, True)
    assert str(unit_under_test.next()) == "line #1'x=1'"
    with pytest.raises(StopIteration):
        unit_under_test.next()

    assert log.messages["info"] == []
    assert log.messages["warning"] == []
    assert log.messages["error"] == []
    result = log.messages["critical"][0].split("\n")[1]
    assert result == "    1:x=1 <== while processing line"
    assert log.messages["critical"][1] == "STOPPED READING"
    expected = "Traceback\n"
    assert log.messages["debug"][0][: len(expected)] == expected


def test_include_not_found():
    """Tests that FortranReaderBase.next() provides the include line when
    the included file is not found.

    """
    code = "include 'nonexistant.f90'"
    unit_under_test = FortranStringReader(code)
    line = unit_under_test.next()
    assert str(line.line) == code


def test_base_next_good_include(log):
    """
    Tests that FortranReaderBase.next() causes a message to be logged when a
    file is included.
    """
    code = "include 'modfile.f95'\nx=2"
    include_directories = [os.path.dirname(__file__)]
    unit_under_test = FortranStringReader(
        code, include_dirs=include_directories, ignore_comments=False
    )
    line = unit_under_test.next()
    assert str(line)[:19] == "Comment('! Modified"  # First line of inclusion
    assert log.messages["debug"] == []
    assert log.messages["error"] == []
    assert log.messages["warning"] == []
    assert log.messages["critical"] == []
    expected = (
        "    1:include 'modfile.f95' " + "<== including file '{path}/modfile.f95'"
    )
    result = log.messages["info"][0].split("\n")[1]
    assert result == expected.format(path=include_directories[0])


def test_fortranreaderbase_info(log):
    """
    Tests that FortranReaderBase.info() causes a message to be logged.
    """
    unit_under_test = FortranStringReader("x=3")
    thing = unit_under_test.get_source_item()
    unit_under_test.info("Mighty Whirlitzer", thing)
    assert log.messages["debug"] == []
    assert log.messages["error"] == []
    assert log.messages["warning"] == []
    assert log.messages["critical"] == []
    expected = "    1:x=3 <== Mighty Whirlitzer"
    result = log.messages["info"][0].split("\n")[1]
    assert result == expected


def test_fortranreaderbase_error(log):
    """
    Tests that FortranReaderBase.error() causes a message to be logged.
    """
    unit_under_test = FortranStringReader("x=2")
    thing = unit_under_test.get_source_item()
    with pytest.raises(SystemExit):
        unit_under_test.error("Thundering Chalmer", thing)
    assert log.messages["debug"] == []
    assert log.messages["info"] == []
    assert log.messages["warning"] == []
    assert log.messages["critical"] == []
    expected = "    1:x=2 <== Thundering Chalmer"
    result = log.messages["error"][0].split("\n")[1]
    assert result == expected


def test_fortranreaderbase_warning(log):
    """
    Tests that FortranReaderBase.warning() causes a message to be logged.
    """
    unit_under_test = FortranStringReader("x=1")
    thing = unit_under_test.get_source_item()
    unit_under_test.warning("Flatulent Hermit", thing)
    assert log.messages["debug"] == []
    assert log.messages["info"] == []
    assert log.messages["error"] == []
    assert log.messages["critical"] == []
    expected = "    1:x=1 <== Flatulent Hermit"
    result = log.messages["warning"][0].split("\n")[1]
    assert result == expected


def test_base_handle_multilines(log):
    """
    Tests that FortranReaderBase.get_source_item() logs the correct messages
    when there are quote discrepancies.
    """
    code = 'character(8) :: test = \'foo"""bar'
    log.reset()
    unit_under_test = FortranStringReader(code)
    mode = FortranFormat(True, True)
    unit_under_test.set_format(mode)  # Force strict free format
    unit_under_test.get_source_item()
    assert log.messages["debug"] == []
    assert log.messages["info"] == []
    assert log.messages["error"] == []
    assert log.messages["critical"] == []
    expected = 'multiline prefix contains odd number of "\'" characters'
    result = log.messages["warning"][0].split("<==")[1].lstrip()
    assert result == expected

    code = 'goo """boo\n doo""" soo \'foo'
    log.reset()
    unit_under_test = FortranStringReader(code)
    mode = FortranFormat(True, True)
    unit_under_test.set_format(mode)  # Force strict free format
    unit_under_test.get_source_item()
    assert log.messages["debug"] == []
    assert log.messages["info"] == []
    assert log.messages["error"] == []
    assert log.messages["critical"] == []
    expected = 'following character continuation: "\'", expected None.'
    result = log.messages["warning"][0].split("<==")[1].lstrip()
    assert result == expected


def test_base_fixed_nonlabel(log):
    """
    Tests that FortranReaderBase.get_source_item() logs the correct messages
    when there is an unexpected character in the initial 6 columns.
    """
    # Checks that a bad character in the first column causes an event to be
    # logged.
    code = "w    integer :: i"
    log.reset()
    unit_under_test = FortranStringReader(code)
    mode = FortranFormat(False, True)
    unit_under_test.set_format(mode)  # Force fixed format
    unit_under_test.get_source_item()
    assert log.messages["debug"] == []
    assert log.messages["info"] == []
    assert log.messages["error"] == []
    assert log.messages["critical"] == []
    result = log.messages["warning"][0].split("<==")[1].lstrip()
    expected = (
        "non-space/digit char 'w' found in column 1 of fixed "
        + "Fortran code, interpreting line as comment line"
    )
    assert result == expected

    # Checks a bad character in columns 2-6
    for i in range(1, 5):
        code = " " * i + "w" + " " * (5 - i) + "integer :: i"
        log.reset()
        unit_under_test = FortranStringReader(code)
        mode = FortranFormat(False, True)
        unit_under_test.set_format(mode)  # Force strict fixed format
        unit_under_test.get_source_item()
        assert log.messages["debug"] == []
        assert log.messages["info"] == []
        assert log.messages["error"] == []
        assert log.messages["critical"] == []
        result = log.messages["warning"][0].split("<==")[1].lstrip()
        expected = (
            "non-space/digit char 'w' found in column {col} " + "of fixed Fortran code"
        )
        assert result == expected.format(col=i + 1)

    # Checks for a bad character, not in the first column, with "sloppy" mode
    # engaged.
    code = " w   integer :: i"
    log.reset()
    unit_under_test = FortranStringReader(code)
    mode = FortranFormat(False, False)
    unit_under_test.set_format(mode)  # Force sloppy fixed format
    unit_under_test.get_source_item()
    assert log.messages["debug"] == []
    assert log.messages["info"] == []
    assert log.messages["error"] == []
    assert log.messages["critical"] == []
    expected = (
        "non-space/digit char 'w' found in column 2 "
        + "of fixed Fortran code, switching to free format mode"
    )
    result = log.messages["warning"][0].split("<==")[1].lstrip()
    assert result == expected


def test_base_fixed_continuation(log):
    """
    Tests that FortranReaderBase.get_source_item() logs the correct messages
    when there are quote mismatches across a continuation in fixed format.
    """
    code = '     character(4) :: cheese = "a & !\n     & b'
    log.reset()
    unit_under_test = FortranStringReader(code)
    mode = FortranFormat(False, False)
    unit_under_test.set_format(mode)  # Force sloppy fixed format
    unit_under_test.get_source_item()
    assert log.messages["debug"] == []
    assert log.messages["info"] == []
    assert log.messages["error"] == []
    assert log.messages["critical"] == []
    expected = "following character continuation: '\"', expected None."
    result = log.messages["warning"][0].split("<==")[1].lstrip()
    assert result == expected

    code = "     x=1 &\n     +1 &\n     -2"
    log.reset()
    unit_under_test = FortranStringReader(code)
    mode = FortranFormat(False, False)
    unit_under_test.set_format(mode)  # Force sloppy fixed format
    unit_under_test.get_source_item()
    assert log.messages["debug"] == []
    assert log.messages["info"] == []
    assert log.messages["error"] == []
    assert log.messages["critical"] == []
    expected = (
        "free format line continuation character `&' detected "
        + "in fix format code\n    2:     +1 &\n    3:     -2"
    )
    result = log.messages["warning"][0].split("<==")[1].lstrip()
    assert result == expected


def test_base_free_continuation(log):
    """
    Tests that FortranReaderBase.get_source_item() logs the correct messages
    when there are quote mismatches across a continuation in free format.
    """
    code = 'character(4) :: "boo & que'
    log.reset()
    unit_under_test = FortranStringReader(code)
    mode = FortranFormat(True, False)
    unit_under_test.set_format(mode)  # Force sloppy free format
    unit_under_test.get_source_item()
    assert log.messages["debug"] == []
    assert log.messages["info"] == []
    assert log.messages["warning"] == []
    assert log.messages["critical"] == []
    expected = "following character continuation: '\"', expected None."
    result = log.messages["error"][0].split("<==")[1].lstrip()
    assert result == expected


def check_include_works(
    fortran_filename, fortran_code, include_info, expected, tmpdir, ignore_comments=True
):
    """Utility function used by a number of tests to check that include
    files work as expected.

    :param str fortran_filename: the name of the fortran file that is \
    going to be created in the 'tmpdir' directory.
    :param str fortran_code: the fortran code to put in the fortran \
    file specified by 'fortran_filename'.
    :param include_info: a list of 2-tuples each with an include \
    filename as a string followed by include code as a string.
    :type include_info: List[str]
    :param str expected: the expected output after parsing the code.
    :param str tmpdir: the temporary directory in which to create and \
    process the Fortran files.
    :param bool ignore_comments: whether to ignore (skip) comments in \
    the Fortran code or not. Defaults to ignore them.

    """

    try:
        oldpwd = tmpdir.chdir()
        cwd = str(tmpdir)

        # Create the program
        with open(os.path.join(cwd, fortran_filename), "w") as cfile:
            cfile.write(fortran_code)
        for include_filename in include_info.keys():
            with open(os.path.join(cwd, include_filename), "w") as cfile:
                cfile.write(include_info[include_filename])
        reader = FortranFileReader(fortran_filename, ignore_comments=ignore_comments)
        for orig_line in expected.split("\n"):
            new_line = reader.next().line
            assert new_line == orig_line
        with pytest.raises(StopIteration):
            reader.next()
    finally:
        oldpwd.chdir()


FORTRAN_CODE = (
    "program test\n"
    "  ! prog comment 1\n"
    "  print *, 'Hello'\n"
    "  ! prog comment 2\n"
    "end program"
)

EXPECTED_CODE = "program test\n" "print *, 'Hello'\n" "end program"


def test_include1(tmpdir):
    """Test that FortranReaderBase can parse an include file when the
    original program consists only of an include.

    """
    fortran_filename = "prog.f90"
    include_filename = "prog.inc"
    fortran_code = "include '{0}'".format(include_filename)
    include_code = EXPECTED_CODE
    include_info = {include_filename: include_code}
    check_include_works(
        fortran_filename, fortran_code, include_info, EXPECTED_CODE, tmpdir
    )


def test_include2(tmpdir):
    """Test that FortranReaderBase can parse an include file when the
    original and include files both have multiple lines.

    """
    fortran_filename = "prog.f90"
    include_filename = "my-include.h"
    fortran_code = (
        "module include_test\n"
        "  include '{0}'\n"
        "end module include_test".format(include_filename)
    )
    include_code = (
        "interface mpi_sizeof\n"
        "subroutine simple()\n"
        "end subroutine simple\n"
        "end interface mpi_sizeof"
    )
    split_code = fortran_code.split("\n")
    expected = split_code[0] + "\n" + include_code + "\n" + split_code[2]
    include_info = {include_filename: include_code}
    check_include_works(fortran_filename, fortran_code, include_info, expected, tmpdir)


def test_include3(tmpdir):
    """Test that FortranReaderBase can parse an include file when the
    original program is invalid without the include.

    """
    fortran_filename = "prog.f90"
    include_filename = "prog.inc"
    fortran_code = "program test\n" "include '{0}'".format(include_filename)
    include_code = "print *, 'Hello'\n" "end program"
    include_info = {include_filename: include_code}
    check_include_works(
        fortran_filename, fortran_code, include_info, EXPECTED_CODE, tmpdir
    )


def test_include4(tmpdir):
    """Test that FortranReaderBase can parse input containing multiple
    include files.

    """
    fortran_filename = "prog.f90"
    include_filename1 = "prog.inc1"
    include_filename2 = "prog.inc2"
    fortran_code = (
        "program test\n"
        "include '{0}'\n"
        "include '{1}'".format(include_filename1, include_filename2)
    )
    include_code1 = "print *, 'Hello'\n"
    include_code2 = "end program"
    expected = fortran_code.split("\n")[0] + "\n" + include_code1 + include_code2
    include_info = {include_filename1: include_code1, include_filename2: include_code2}
    check_include_works(fortran_filename, fortran_code, include_info, expected, tmpdir)


def test_include5(tmpdir):
    """Test that FortranReaderBase can parse nested include files."""
    fortran_filename = "prog.f90"
    include_filename1 = "prog.inc1"
    include_filename2 = "prog.inc2"
    fortran_code = "program test\n" "include '{0}'".format(include_filename1)
    include_code1 = "print *, 'Hello'\n" "include '{0}'".format(include_filename2)
    include_code2 = "end program"
    include_info = {include_filename1: include_code1, include_filename2: include_code2}
    check_include_works(
        fortran_filename, fortran_code, include_info, EXPECTED_CODE, tmpdir
    )


def test_include6(tmpdir, ignore_comments):
    """Check that FortranReaderBase can parse an include file correctly
    when it contains comments. Test with and without comments being
    ignored.

    """
    fortran_filename = "prog.f90"
    include_filename = "prog.inc"
    fortran_code = (
        "program test\n"
        "  ! prog comment 1\n"
        "  include '{0}' ! this is an include\n"
        "  ! prog comment 2\n"
        "end program".format(include_filename)
    )
    include_code = "! include comment 1\n" "print *, 'Hello'\n" "! include comment 2"
    include_info = {include_filename: include_code}
    if ignore_comments:
        expected = EXPECTED_CODE
    else:
        expected = (
            "program test\n"
            "! prog comment 1\n"
            "! include comment 1\n"
            "print *, 'Hello'\n"
            "! include comment 2\n"
            "! this is an include\n"
            "! prog comment 2\n"
            "end program"
        )
    check_include_works(
        fortran_filename,
        fortran_code,
        include_info,
        expected,
        tmpdir,
        ignore_comments=ignore_comments,
    )


def test_multi_stmt_line1():
    """Check that simple statements separated by ; on a single line are
    correctly split into multiple lines by FortranReaderBase

    """
    code = "do i=1,10;b=20 ; c=30"
    reader = FortranStringReader(code)
    mode = FortranFormat(True, False)
    reader.set_format(mode)
    line1 = reader.next()
    assert isinstance(line1, Line)
    assert line1.line == "do i=1,10"
    assert line1.span == (1, 1)
    assert line1.label is None
    assert line1.name is None
    assert line1.reader is reader
    line2 = reader.next()
    assert isinstance(line2, Line)
    assert line2.line == "b=20"
    assert line2.span is line1.span
    assert line2.label is None
    assert line2.name is None
    assert line2.reader is reader
    line3 = reader.next()
    assert isinstance(line3, Line)
    assert line3.line == "c=30"
    assert line3.span is line1.span
    assert line3.label is None
    assert line3.name is None
    assert line3.reader is reader


def test_multi_stmt_line2():
    """Check that format statements separated by ; on a single line are
    correctly split into multiple lines by FortranReaderBase

    """
    code = "10 format(a); 20 format(b)"
    reader = FortranStringReader(code)
    mode = FortranFormat(True, False)
    reader.set_format(mode)
    line1 = reader.next()
    assert isinstance(line1, Line)
    assert line1.line == "format(a)"
    assert line1.span == (1, 1)
    assert line1.label == 10
    assert line1.name is None
    assert line1.reader is reader
    line2 = reader.next()
    assert line2.line == "format(b)"
    assert line2.span is line1.span
    assert line2.label == 20
    assert line2.name is None
    assert line2.reader is reader


def test_multi_stmt_line3():
    """Check that named do loops separated by ; on a single line are correctly
    split into multiple lines by FortranReaderBase

    """
    code = "name:do i=1,10;name2 : do j=1,10"
    reader = FortranStringReader(code)
    mode = FortranFormat(True, False)
    reader.set_format(mode)
    line1 = reader.next()
    assert isinstance(line1, Line)
    assert line1.line == "do i=1,10"
    assert line1.span == (1, 1)
    assert line1.label is None
    assert line1.name == "name"
    assert line1.reader is reader
    line2 = reader.next()
    assert line2.line == "do j=1,10"
    assert line2.span is line1.span
    assert line2.label is None
    assert line2.name == "name2"
    assert line2.reader is reader


def test_get_item(ignore_comments):
    """Check the get_item() function works as expected. Test with and
    without comments being ignored.

    """
    if ignore_comments:
        expected = EXPECTED_CODE
    else:
        expected = (
            "program test\n"
            "! prog comment 1\n"
            "print *, 'Hello'\n"
            "! prog comment 2\n"
            "end program"
        )
    reader = FortranStringReader(FORTRAN_CODE, ignore_comments=ignore_comments)
    for expected_line in expected.split("\n"):
        output_line = reader.get_item()
        assert expected_line in output_line.line
    assert not reader.get_item()


def test_put_item(ignore_comments):
    """Check that when a line is consumed it can be pushed back so it can
    be consumed again. Test with and without comments being
    ignored.

    """
    reader = FortranStringReader(FORTRAN_CODE, ignore_comments=ignore_comments)
    while True:
        orig_line = reader.get_item()
        if not orig_line:
            break
        reader.put_item(orig_line)
        fifo_line = reader.get_item()
        assert fifo_line == orig_line


def test_put_item_include(ignore_comments, tmpdir):
    """Check that when a line that has been included via an include
    statement is consumed it can be pushed back so it can be consumed
    again. Test with and without ignoring comments.

    """
    _ = tmpdir.chdir()
    cwd = str(tmpdir)
    include_code = """
    var1 = 1
    var2 = 2
    var3 = 3
"""
    with open(os.path.join(cwd, "my_include.h"), "w") as cfile:
        cfile.write(include_code)
    code = """
program my_prog
  integer :: var1, var2, var3
  include 'my_include.h'
end program my_prog
"""
    reader = FortranStringReader(code, ignore_comments=ignore_comments)
    lines = []
    while True:
        lines.append(reader.get_item())
        # Try immediately putting the line back and then requesting it again.
        # This checks that the correct FIFO buffer is being used.
        reader.put_item(lines[-1])
        assert reader.get_item().line == lines[-1].line
        if "var3 =" in lines[-1].line:
            # Stop reading while we're still in the INCLUDE file so that we
            # have a stack of readers when calling `put_item` below.
            break
    # Put all the lines back in the same order that we saw them.
    for line in reversed(lines):
        reader.put_item(line)
    # Check that the reader returns all those lines in the same order that
    # we saw them originally.
    for line in lines:
        assert reader.get_item().line == line.line


def test_multi_put_item(ignore_comments):
    """Check that multiple lines can be pushed back and will be returned
    correctly in the specified order (actually the reverse of the
    original). Test with and without ignoring comments.

    """
    reader = FortranStringReader(FORTRAN_CODE, ignore_comments=ignore_comments)
    orig_lines = []
    while True:
        orig_line = reader.get_item()
        if not orig_line:
            break
        # Make sure our original lines are kept in reverse order.
        orig_lines.insert(0, orig_line)

    # Put back original lines in reverse order as that is what we
    # would expect when processing and rolling back.
    for line in orig_lines:
        reader.put_item(line)

    # Lines should now be returned in the correct order (so compare in
    # reverse order with the original line list)
    while True:
        filo_line = reader.get_item()
        if not filo_line:
            break
        assert filo_line == orig_lines.pop(-1)
    assert not orig_lines


# Issue 177: get_item(ignore_comments) - how does ignore_comments affect
# processing?

# Issue 178: Why is there a next() as well as a get_item()? How do they
# (and put_item()) interact?


##############################################################################

FULL_FREE_SOURCE = """

!> Unicode comment: ❤ ✓ ☂ ♞ ☯

program test

  implicit none

  character, parameter :: nature = 'free format'

end program test
"""

FULL_FREE_EXPECTED = [
    "!> Unicode comment: ❤ ✓ ☂ ♞ ☯",
    "program test",
    "  implicit none",
    "  character, parameter :: nature = 'free format'",
    "end program test",
]


##############################################################################


def test_filename_reader():
    """
    Tests that a Fortran source file can be read given its filename.
    """
    handle, filename = tempfile.mkstemp(suffix=".f90", text=True)
    os.close(handle)
    try:
        with io.open(filename, mode="w", encoding="UTF-8") as source_file:
            source_file.write(FULL_FREE_SOURCE)

        unit_under_test = FortranFileReader(filename)
        expected = FortranFormat(True, False)
        assert unit_under_test.format == expected
        for expected in FULL_FREE_EXPECTED:
            found = unit_under_test.get_single_line(ignore_empty=True)
            assert found == expected
    except Exception:
        os.unlink(filename)
        raise


##############################################################################


def test_file_reader():
    """
    Tests that a Fortran source file can be read given a file object of it.
    """
    handle, filename = tempfile.mkstemp(suffix=".f90", text=True)
    os.close(handle)
    try:
        with io.open(filename, mode="w", encoding="UTF-8") as source_file:
            source_file.write(FULL_FREE_SOURCE)

        with io.open(filename, mode="r", encoding="UTF-8") as source_file:
            unit_under_test = FortranFileReader(source_file)

            expected = FortranFormat(True, False)
            assert unit_under_test.format == expected
            for expected in FULL_FREE_EXPECTED:
                assert unit_under_test.get_single_line(ignore_empty=True) == expected
    except Exception:
        os.unlink(filename)
        raise


def test_none_in_fifo(log):
    """Check that a None entry in the reader FIFO buffer is handled
    correctly."""
    log.reset()
    handle, filename = tempfile.mkstemp(suffix=".f90", text=True)
    os.close(handle)

    with io.open(filename, mode="w", encoding="UTF-8") as source_file:
        source_file.write(FULL_FREE_SOURCE)

    with io.open(filename, mode="r", encoding="UTF-8") as source_file:
        unit_under_test = FortranFileReader(source_file)
        while True:
            try:
                _ = unit_under_test.next(ignore_comments=False)
            except StopIteration:
                break
        # Erroneously push a None to the FIFO buffer
        unit_under_test.put_item(None)
        # Attempt to read the next item
        with pytest.raises(StopIteration):
            _ = unit_under_test.next(ignore_comments=False)
        # Check that nothing has been logged
        for log_level in ["debug", "info", "warning", "error", "critical"]:
            assert log.messages[log_level] == []


def test_bad_file_reader():
    """
    Tests that the file reader can spot when it is given something to read
    which is neither file nor filename.
    """
    with pytest.raises(ValueError) as ex:
        _ = FortranFileReader(42)
    expected = "FortranFileReader is used with a filename or file-like object."
    assert expected in str(ex.value)


##############################################################################


def test_string_reader():
    """
    Tests that Fortran source can be read from a string.
    """
    unit_under_test = FortranStringReader(FULL_FREE_SOURCE)
    expected = FortranFormat(True, False)
    assert unit_under_test.format == expected
    for expected in FULL_FREE_EXPECTED:
        assert unit_under_test.get_single_line(ignore_empty=True) == expected


@pytest.mark.parametrize("reader_cls", [FortranStringReader, FortranFileReader])
def test_reader_ignore_encoding(reader_cls, tmp_path):
    """
    Tests that the Fortran{String,File}Reader can be configured to take
    notice of Python-style encoding information.
    """
    source = "! -*- f77 -*-\n" + FULL_FREE_SOURCE
    if reader_cls is FortranFileReader:
        sfile = tmp_path / "my_test.f90"
        sfile.write_text(source)
        # File location with full path.
        rinput = str(sfile.absolute())
    else:
        rinput = source
    reader = reader_cls(rinput)
    # By default the encoding information is ignored so the format should be
    # free format, not strict.
    assert reader.format == FortranFormat(True, False)
    # Check that explicitly setting ignore_encoding=True gives
    # the same behaviour.
    reader1 = reader_cls(rinput, ignore_encoding=True)
    assert reader1.format == FortranFormat(True, False)
    # Now test when the reader takes notice of the encoding information.
    reader2 = reader_cls(rinput, ignore_encoding=False)
    # Should be fixed format, strict.
    assert reader2.format == FortranFormat(False, True)


def test_inherited_f77():
    """
    A grab bag of functional tests inherited from readfortran.py.
    """
    string_f77 = """c -*- f77 -*-
c12346 comment
      subroutine foo
      call foo
     'bar
a    'g
      abc=2
      call you ! hi
      end
     '"""
    expected = [
        "Comment('c -*- f77 -*-',(1, 1))",
        "Comment('c12346 comment',(2, 2))",
        "line #3'subroutine foo'",
        "line #4'call foobar'",
        'Comment("a    \'g",(6, 6))',
        "line #7'abc=2'",
        "line #8'call you ! hi'",
        "line #9'end'",
    ]

    # Reading from buffer
    reader = FortranStringReader(
        string_f77, ignore_comments=False, ignore_encoding=False
    )
    assert reader.format.mode == "f77", repr(reader.format.mode)
    stack = expected[:]
    for item in reader:
        assert str(item) == stack.pop(0)

    # Reading from file
    handle, filename = tempfile.mkstemp(suffix=".f", text=True)
    os.close(handle)
    with open(filename, "w") as fortran_file:
        print(string_f77, file=fortran_file)

    reader = FortranFileReader(filename, ignore_comments=False, ignore_encoding=False)
    stack = expected[:]
    for item in reader:
        assert str(item) == stack.pop(0)


def test_pyf():
    """
    Tests inherited from implementation.
    """
    string_pyf = """! -*- pyf -*-
python module foo
  interface
  beginml '''1st line
  2nd line
  end line'''endml='tere!fake comment'!should be a comment
  a = 2
  'charc\"onstant' ''' single line mline '''a='hi!fake comment'!should be a comment
  a=\\\\\\\\\\'''not a multiline'''
  !blah='''never ending multiline
  b=3! hey, fake line continuation:&
  c=4& !line cont
  &45
  thisis_label_2 : c = 3
   xxif_isotropic_2 :     if ( string_upper_compare ( o%opt_aniso, 'ISOTROPIC' ) ) then
   g=3
   endif
  end interface
  if ( pc_get_lun() .ne. 6) &

    write ( pc_get_lun(), '( &
    & /, a, /, " p=", i4, " stopping c_flag=", a, &
    & /, " print unit=", i8)') &
    trim(title), pcpsx_i_pel(), trim(c_flag), pc_get_lun()
end python module foo
! end of file
"""
    expected = [
        "Comment('! -*- pyf -*-',(1, 1))",
        "line #2'python module foo'",
        "line #3'interface'",
        "MultiLine('  beginml ',"
        + "['1st line', '  2nd line', '  end line'],"
        + "\"endml='tere!fake comment'\",(4, 6))",
        "Comment('!should be a comment',(6, 6))",
        "line #7'a = 2'",
        "MultiLine('  \\'charc\"onstant\\' ',"
        + "[' single line mline '],"
        + "\"a='hi!fake comment'\",(8, 8))",
        "Comment('!should be a comment',(8, 8))",
        "line #9\"a=\\\\\\\\\\\\\\\\\\\\'''not a multiline'''\"",
        "Comment(\"!blah='''never ending multiline\",(10, 10))",
        "line #11'b=3'",
        "Comment('! hey, fake line continuation:&',(11, 11))",
        "line #12'c=445'",
        "Comment('!line cont',(12, 12))",
        "line #14thisis_label_2: 'c = 3'",
        "line #15xxif_isotropic_2: "
        + '"if ( string_upper_compare ( o%opt_aniso,'
        + " 'ISOTROPIC' ) ) then\"",
        "line #16'g=3'",
        "line #17'endif'",
        "line #18'end interface'",
        "line #19'if ( pc_get_lun() .ne. 6)"
        + '     write ( pc_get_lun(), \\\'(  /, a, /, " p=", i4,'
        + ' " stopping c_flag=", a,  /, " print unit=", i8)\\\')'
        + "     trim(title), pcpsx_i_pel(), trim(c_flag),"
        + " pc_get_lun()'",
        "line #25'end python module foo'",
        "Comment('! end of file',(26, 26))",
    ]

    reader = FortranStringReader(
        string_pyf, ignore_comments=False, ignore_encoding=False
    )
    assert reader.format.mode == "pyf", repr(reader.format.mode)
    for item in reader:
        assert str(item) == expected.pop(0)


def test_fix90():
    """
    Tests inherited from implementation.
    """
    string_fix90 = """c -*- fix -*-
      subroutine foo
cComment
 1234 a = 3 !inline comment
      b = 3
!
     !4!line cont. with comment symbol
     &5
!   KDMO
      write (obj%print_lun, *) ' KDMO : '
      write (obj%print_lun, *) '  COORD = ',coord, '  BIN_WID = ',             &
       obj%bin_wid,'  VEL_DMO = ', obj%vel_dmo
      end subroutine foo
      subroutine

     & foo
      end
"""
    expected = [
        "Comment('c -*- fix -*-',(1, 1))",
        "line #2'subroutine foo'",
        "Comment('cComment',(3, 3))",
        "line #4 1234 'a = 3'",
        "Comment('!inline comment',(4, 4))",
        "line #5'b = 345'",
        "Comment('!',(6, 6))",
        "Comment('!line cont. with comment symbol',(7, 7))",
        "Comment('!   KDMO',(9, 9))",
        "line #10\"write (obj%print_lun, *) ' KDMO : '\"",
        "line #11\"write (obj%print_lun, *) '  COORD = ',coord,"
        + " '  BIN_WID = ',             &\"",
        "line #12\"obj%bin_wid,'  VEL_DMO = ', obj%vel_dmo\"",
        "line #13'end subroutine foo'",
        "line #14'subroutine foo'",
        "Comment('',(15, 15))",
        "line #17'end'",
    ]
    reader = FortranStringReader(
        string_fix90, ignore_comments=False, ignore_encoding=False
    )
    assert reader.format.mode == "fix", repr(reader.format.mode)
    for item in reader:
        assert str(item) == expected.pop(0)


def test_f2py_directive_fixf90(f2py_enabled):
    """Test the handling of the f2py directive in fixed-format f90."""
    string_fix90 = """c -*- fix -*-
      subroutine foo
      a = 3!f2py.14 ! pi!
!f2py a = 0.0
      end subroutine foo"""
    reader = FortranStringReader(string_fix90, ignore_comments=False)
    reader.set_format(FortranFormat(False, False, f2py_enabled))
    expected = ["Comment('c -*- fix -*-',(1, 1))", "line #2'subroutine foo'"]
    if f2py_enabled:
        expected.extend(
            ["line #3'a = 3.14'", "Comment('! pi!',(3, 3))", "line #4'a = 0.0'"]
        )
    else:
        expected.extend(
            [
                "line #3'a = 3'",
                "Comment('!f2py.14 ! pi!',(3, 3))",
                "Comment('!f2py a = 0.0',(4, 4))",
            ]
        )
    expected.append("line #5'end subroutine foo'")
    for item in reader:
        assert str(item) == expected.pop(0)


def test_f2py_freef90(f2py_enabled):
    """Test the handling of f2py directives in free-format f90, in both
    in-line and full-line comments."""
    lines = [
        "subroutine foo",
        "   a = 3!f2py.14 ! pi!",
        "!f2py a = 0.0",
        "end subroutine foo",
    ]
    reader = FortranStringReader("\n".join(lines), ignore_comments=False)
    reader.set_format(FortranFormat(True, False, f2py_enabled))
    expected = ["line #1'subroutine foo'"]
    if f2py_enabled:
        expected.extend(
            ["line #2'a = 3.14'", "Comment('! pi!',(2, 2))", "line #3'a = 0.0'"]
        )
    else:
        expected.extend(
            [
                "line #2'a = 3'",
                "Comment('!f2py.14 ! pi!',(2, 2))",
                "Comment('!f2py a = 0.0',(3, 3))",
            ]
        )
    expected.append("line #4'end subroutine foo'")
    for item in reader:
        assert str(item) == expected.pop(0)


@pytest.mark.xfail(reason="Issue #270: f2py directives not working in F77 " "code.")
def test_f2py_directive_f77(f2py_enabled):
    """Test the handling of the f2py directive in fixed-format f77."""
    string_f77 = """c -*- f77 -*-
      subroutine foo
cf2py call me ! hey
      end"""
    expected = ["Comment('c -*- f77 -*-',(1, 1))", "line #2'subroutine foo'"]
    if f2py_enabled:
        expected.append("line #3'call me ! hey'")
    else:
        expected.append("Comment('cf2py call me ! hey',(3, 3))")
    expected.append("line #4'end'")

    # Reading from buffer
    reader = FortranStringReader(string_f77, ignore_comments=False)
    for item in reader:
        assert str(item) == expected.pop(0)


def test_utf_char_in_code(log):
    """Check that we cope with Fortran code that contains UTF characters. This
    is not valid Fortran but most compilers cope with it."""
    log.reset()
    fort_file = os.path.join(os.path.dirname(__file__), "utf_in_code.f90")
    reader = FortranFileReader(fort_file, ignore_comments=True)
    out_line = reader.get_item()
    while out_line:
        out_line = reader.get_item()
    assert log.messages["critical"] == []


def test_extract_label():
    """Test the extract label function in readfortran.py."""
    text_input = "no label"
    label, text_result = extract_label(text_input)
    assert label is None
    assert text_result is text_input

    text_input = " 80stuff"
    label, text_result = extract_label(text_input)
    assert label is None
    assert text_result is text_input

    text_input = " 80 stuff"
    label, text_result = extract_label(text_input)
    assert label == 80
    assert text_result == "stuff"


def test_extract_construct_name():
    """Test the extract construct name function in readfortran.py."""
    text_input = "no construct name"
    name, text_result = extract_construct_name(text_input)
    assert name is None
    assert text_result is text_input

    text_input = "name:stuff"
    name, text_result = extract_construct_name(text_input)
    assert name == "name"
    assert text_result == "stuff"

    text_input = " name : stuff"
    name, text_result = extract_construct_name(text_input)
    assert name == "name"
    assert text_result == "stuff"


@pytest.mark.parametrize(
    "input_text, ref",
    [
        ('#include "abc"', True),
        (' #include "abc"', True),
        (' #  include "abc"', True),
        ("#define ABC 1", True),
        ("#ifdef ABC", True),
        ("#if !defined(ABC)", True),
        ("abc #define", False),
        ('"#"', False),
        ("! #", False),
    ],
)
def test_handle_cpp_directive(input_text, ref):
    """Test the function that detects cpp directives in readfortran.py."""
    reader = FortranStringReader(input_text)
    output_text, result = reader.handle_cpp_directive(input_text)
    assert result == ref
    assert output_text is input_text


def test_reader_cpp_directives():
    """Test that CPP directives are read correctly in readfortran.py."""
    input_text = [
        "program test",
        "#define ABC 123",
        "character :: c = '#'",
        "#ifdef ABC",
        "! Some comment that should be ignored",
        "integer :: a",
        "#endif",
        "#if !defined(ABC)",
        "integer :: b",
        "#endif",
        "end program test",
    ]
    ref_text = "\n".join(input_text[:4] + input_text[5:]).strip()
    reader = FortranStringReader("\n".join(input_text))
    lines = list(reader)

    pp_lines = [1, 3, 5, 6, 8]
    for i, line in enumerate(lines):
        if i in pp_lines:
            assert isinstance(line, CppDirective)
        else:
            assert isinstance(line, Line)

    assert "\n".join(item.line for item in lines) == ref_text


def test_multiline_cpp_directives():
    """Test that multiline CPP directives are read correctly."""
    input_text = [
        "program test",
        "#define ABC \\ ",
        "  123 ",
        "integer a",
        "end program test",
    ]
    ref_text = "program test\n#define ABC   123\ninteger a\nend program test"
    reader = FortranStringReader("\n".join(input_text))
    lines = list(reader)
    assert len(lines) == 4
    assert isinstance(lines[1], CppDirective)
    assert lines[1].span == (2, 3)
    assert "\n".join(item.line for item in lines) == ref_text


# Tests for the get_source_item method in class FortranReaderBase :
# Comments


def test_single_comment():
    """Test that a free format single line comment is output as expected"""
    input_text = "! a comment\n"

    reader = FortranStringReader(input_text, ignore_comments=False)
    lines = list(reader)
    assert len(lines) == 1
    assert isinstance(lines[0], Comment)
    assert lines[0].span == (1, 1)
    assert lines[0].line + "\n" == input_text

    reader = FortranStringReader(input_text, ignore_comments=True)
    lines = list(reader)
    assert len(lines) == 0


def test_many_comments():
    """Test that a large consecutive number of single line comments can be
    successfully processed. Earlier versions of the reader used
    recursion for multiple consecutive free format single line
    comments which resulted in a recursion error in this case.

    """
    number_of_comments = 1000
    input_text = "program hello\n"
    for index in range(number_of_comments):
        input_text += "! comment{0}\n".format(index + 1)
    input_text += "end program hello\n"

    reader = FortranStringReader(input_text, ignore_comments=False)
    lines = list(reader)
    assert len(lines) == number_of_comments + 2
    for index in range(1, number_of_comments):
        assert isinstance(lines[index], Comment)
        assert lines[index].span == (index + 1, index + 1)
        assert lines[index].line + "\n" == "! comment{0}\n".format(index)

    reader = FortranStringReader(input_text, ignore_comments=True)
    lines = list(reader)
    assert len(lines) == 2


@pytest.mark.parametrize("inline_comment", [' "', " '", " 'andy' '"])
def test_quotes_in_inline_comments(inline_comment):
    """Test that an in-line comment containing a quotation mark is
    read successfully."""
    input_text = f"""\
    character(*) :: a='hello' &!{inline_comment}
    &        b
"""
    reader = FortranStringReader(input_text, ignore_comments=False)
    lines = list(reader)
    assert len(lines) == 2
    assert isinstance(lines[1], Comment)
    assert lines[1].line.endswith(inline_comment)


def test_comments_within_continuation():
    """Test that comments and multi-line statements are processed
    correctly.

    """
    input_text = (
        "  ! Comment1\n"
        "  real :: a &\n"
        "  ! Comment2\n"
        "          ,b\n"
        "  ! Comment3\n"
    )

    reader = FortranStringReader(input_text, ignore_comments=False)
    lines = list(reader)
    assert len(lines) == 4

    assert isinstance(lines[0], Comment)
    assert lines[0].span == (1, 1)
    assert lines[0].line == "! Comment1"

    assert lines[1].span == (2, 4)
    assert lines[1].line == "real :: a           ,b"

    assert isinstance(lines[2], Comment)
    assert lines[2].span == (3, 3)
    assert lines[2].line == "! Comment2"

    assert isinstance(lines[3], Comment)
    assert lines[3].span == (5, 5)
    assert lines[3].line == "! Comment3"

    reader = FortranStringReader(input_text, ignore_comments=True)
    lines = list(reader)
    assert len(lines) == 1
    assert lines[0].span == (2, 4)
    assert lines[0].line == "real :: a           ,b"


# Tests for the get_source_item method in class FortranReaderBase :
# Blank lines


@pytest.mark.parametrize("input_text", ["\n", "    \n"])
def test_single_blank_line(input_text):
    """Test that a single blank line with or without white space is output
    as an empty Comment object.

    """
    reader = FortranStringReader(input_text, ignore_comments=False)
    lines = list(reader)
    assert len(lines) == 1
    assert isinstance(lines[0], Comment)
    assert lines[0].span == (1, 1)
    assert lines[0].line == ""

    reader = FortranStringReader(input_text, ignore_comments=True)
    lines = list(reader)
    assert len(lines) == 0


def test_multiple_blank_lines():
    """Test that multiple blank lines with or without white space are
    output as an empty Comment objects.

    """
    input_text = "   \n\nprogram test\n  \n\nend program test\n  \n\n"
    reader = FortranStringReader(input_text, ignore_comments=False)
    lines = list(reader)
    assert len(lines) == 8
    for index in [0, 1, 3, 4, 6, 7]:
        assert isinstance(lines[index], Comment)
        assert lines[index].span == (index + 1, index + 1)
        assert lines[index].line == ""
    assert isinstance(lines[2], Line)
    assert lines[2].span == (3, 3)
    assert lines[2].line == "program test"
    assert isinstance(lines[5], Line)
    assert lines[5].span == (6, 6)
    assert lines[5].line == "end program test"

    reader = FortranStringReader(input_text, ignore_comments=True)
    lines = list(reader)
    assert len(lines) == 2
    assert isinstance(lines[0], Line)
    assert lines[0].span == (3, 3)
    assert lines[0].line == "program test"
    assert isinstance(lines[1], Line)
    assert lines[1].span == (6, 6)
    assert lines[1].line == "end program test"


def test_blank_lines_within_continuation():
    """Test that blank lines and multi-line statements are processed
    correctly. Note, empty lines within a multi-line statement are
    removed.

    """
    input_text = "  \n  real :: a &\n  \n\n          ,b\n  \n  real :: c\n"

    reader = FortranStringReader(input_text, ignore_comments=False)
    lines = list(reader)
    assert len(lines) == 4

    assert isinstance(lines[0], Comment)
    assert lines[0].span == (1, 1)
    assert lines[0].line == ""
    assert isinstance(lines[1], Line)
    assert lines[1].span == (2, 5)
    assert lines[1].line == "real :: a           ,b"
    assert isinstance(lines[2], Comment)
    assert lines[2].span == (6, 6)
    assert lines[2].line == ""
    assert isinstance(lines[3], Line)
    assert lines[3].span == (7, 7)
    assert lines[3].line == "real :: c"

    reader = FortranStringReader(input_text, ignore_comments=True)
    lines = list(reader)
    assert len(lines) == 2
    assert isinstance(lines[0], Line)
    assert lines[0].span == (2, 5)
    assert lines[0].line == "real :: a           ,b"
    assert isinstance(lines[1], Line)
    assert lines[1].span == (7, 7)
    assert lines[1].line == "real :: c"


def test_conditional_include_omp_conditional_liness_fixed_format_single_line():
    """Test handling of conditional OMP sentinels in a single line
    with source code in fixed format."""

    for sentinel in ["!$", "c$", "C$", "*$"]:
        input_text = f"{sentinel}    bla"

        # 1. By default (not ignoring comments), the line is just a comment:
        reader = FortranStringReader(input_text, ignore_comments=False)
        comment = reader.next()
        assert isinstance(comment, Comment)
        assert comment.comment == input_text

        # 2. And if comments are ignored, nothing should be returned and a
        #    StopIteration exception will be raised.
        reader = FortranStringReader(input_text, ignore_comments=True)
        with pytest.raises(StopIteration):
            comment = reader.next()

        # 3. When omp-sentinels are accepted, we should get a line,
        # not a comment:
        reader = FortranStringReader(
            input_text, ignore_comments=False, include_omp_conditional_lines=True
        )
        line = reader.next()
        assert isinstance(line, Line)
        assert line.line == "bla"

        # 4. If omp-sentinels are accepted, and comments ignored,
        #    we should still get the line (with the sentinel removed):
        reader = FortranStringReader(
            input_text, ignore_comments=True, include_omp_conditional_lines=True
        )
        line = reader.next()
        assert isinstance(line, Line)
        assert line.line == "bla"

        # 5. Make sure that a real omp directive stays a comment:
        input_text = f"{sentinel}omp something"
        reader = FortranStringReader(
            input_text, ignore_comments=False, include_omp_conditional_lines=True
        )
        line = reader.next()
        # This is not a conditional sentinel, so it must be returned
        # as a comment line:
        assert isinstance(line, Comment)
        assert line.line == input_text

    # 6. Test some corner cases (all of which are not valid sentinels):
    for sentinel in ["!!$", "! $", " !$", " ! $"]:
        input_text = f"{sentinel}    bla"
        reader = FortranStringReader(
            input_text, ignore_comments=False, include_omp_conditional_lines=True
        )
        # Enforce fixed format, otherwise fparser will silently switch
        # to free format and suddenly interpret comments differently
        reader.set_format(FortranFormat(False, False))
        comment = reader.next()
        assert isinstance(comment, Comment)
        assert comment.comment == input_text


def test_conditional_include_omp_conditional_liness_free_format_single_line():
    """Test handling of conditional OMP sentinels in a single line
    with source code in free format."""

    # 1. By default, a omp sentinel will be returned as a comment
    input_text = "  !$ bla"
    reader = FortranStringReader(input_text, ignore_comments=False)
    comment = reader.next()
    reader.set_format(FortranFormat(True, True))
    assert isinstance(comment, Comment)
    assert comment.comment == input_text.strip()

    # 2. And if comments are ignored, nothing should be returned and a
    #    StopIteration exception will be raised.
    reader = FortranStringReader(input_text, ignore_comments=True)
    reader.set_format(FortranFormat(True, True))
    with pytest.raises(StopIteration):
        comment = reader.next()

    # 3. When omp-sentinels are accepted, we should get a line,
    # not a comment:
    reader = FortranStringReader(
        input_text, ignore_comments=False, include_omp_conditional_lines=True
    )
    reader.set_format(FortranFormat(True, True))
    line = reader.next()
    assert isinstance(line, Line)
    assert line.line == "bla"

    # 4. If omp-sentinels are accepted, and comments ignored,
    #    we should still get the line (with the sentinel removed):
    reader = FortranStringReader(
        input_text, ignore_comments=True, include_omp_conditional_lines=True
    )
    line = reader.next()
    assert isinstance(line, Line)
    assert line.line == "bla"

    # 5. Make sure that a real omp directive stays a comment:
    input_text = "  !$omp something"
    reader = FortranStringReader(
        input_text, ignore_comments=False, include_omp_conditional_lines=True
    )
    reader.set_format(FortranFormat(True, True))
    line = reader.next()
    # This is not a conditional sentinel, so it must be returned
    # as a comment line:
    assert isinstance(line, Comment)
    assert line.line == input_text.strip()

    # 6. Test some corner cases (all of which are not valid sentinels):
    for sentinel in ["!!$", "! $", " ! $"]:
        input_text = f"{sentinel}    bla"
        reader = FortranStringReader(
            input_text, ignore_comments=False, include_omp_conditional_lines=True
        )
        reader.set_format(FortranFormat(True, True))
        comment = reader.next()
        assert isinstance(comment, Comment)
        # Since fparser will remove leading white spaces, we need to
        # compare with the input text after removing its white spaces:
        assert comment.comment == input_text.strip()


def test_conditional_include_omp_conditional_liness_fixed_format_multiple():
    """Test handling of conditional OMP sentinels with continuation lines
    with source code in fixed format."""

    input_text = "!$     bla\n!$   &bla"
    reader = FortranStringReader(input_text, ignore_comments=True)
    with pytest.raises(StopIteration):
        reader.next()
    reader = FortranStringReader(input_text, ignore_comments=False)
    # Without handling of sentinels, this should return
    # two comment lines:
    comment = reader.next()
    assert isinstance(comment, Comment)
    assert comment.comment == "!$     bla"
    comment = reader.next()
    assert isinstance(comment, Comment)
    assert comment.comment == "!$   &bla"

    # Now enable handling of sentinels, which will result
    # in returning only one line with both concatenated.
    input_text = "!$     bla\n!$   &bla"
    reader = FortranStringReader(
        input_text, ignore_comments=False, include_omp_conditional_lines=True
    )
    line = reader.next()
    assert isinstance(line, Line)
    assert line.line == "blabla"

    # Ignoring comments must not change the behaviour:
    reader = FortranStringReader(
        input_text, ignore_comments=True, include_omp_conditional_lines=True
    )
    line = reader.next()
    assert isinstance(line, Line)
    assert line.line == "blabla"

    # Add invalid sentinels in continuation lines:
    input_text = "!$     bla\n! $   &bla"
    reader = FortranStringReader(
        input_text, ignore_comments=True, include_omp_conditional_lines=True
    )
    line = reader.next()
    assert line.line == "bla"
    # The second line is just a comment line, so it must be ignored:
    with pytest.raises(StopIteration):
        reader.next()

    reader = FortranStringReader(
        input_text, ignore_comments=False, include_omp_conditional_lines=True
    )
    line = reader.next()
    assert line.line == "bla"
    line = reader.next()
    assert line.line == "! $   &bla"


def test_conditional_include_omp_conditional_liness_free_format_multiple():
    """Test handling of conditional OMP sentinels with continuation lines
    with source code in free format."""

    # Test with the optional & in the continuation line:
    # ---------------------------------------------------
    input_text = "!$     bla   &\n!$&     bla"
    reader = FortranStringReader(input_text, ignore_comments=False)
    # Make sure to enforce free format
    reader.set_format(FortranFormat(True, True))
    comment = reader.next()

    assert comment.comment == "!$     bla   &"
    comment = reader.next()
    assert comment.comment == "!$&     bla"

    input_text = "!$     bla   &\n!$&     bla"
    reader = FortranStringReader(
        input_text, ignore_comments=False, include_omp_conditional_lines=True
    )
    # Make sure to enforce free format
    reader.set_format(FortranFormat(True, True))
    line = reader.next()
    assert line.line == "bla        bla"

    # Test without the optional & in the continuation line:
    # -----------------------------------------------------
    input_text = "!$     bla   &\n!$      bla"
    reader = FortranStringReader(input_text, ignore_comments=False)
    # Make sure to enforce free format
    reader.set_format(FortranFormat(True, True))
    comment = reader.next()

    assert comment.comment == "!$     bla   &"
    comment = reader.next()
    assert comment.comment == "!$      bla"

    reader = FortranStringReader(input_text, ignore_comments=True)
    # Make sure to enforce free format
    reader.set_format(FortranFormat(True, True))
    with pytest.raises(StopIteration):
        comment = reader.next()

    input_text = "!$     bla   &\n!$     bla"
    reader = FortranStringReader(
        input_text, ignore_comments=False, include_omp_conditional_lines=True
    )
    # Make sure to enforce free format
    reader.set_format(FortranFormat(True, True))
    line = reader.next()
    # 8 spaces in input_text, plus two for replacing the !$
    assert line.line == "bla          bla"
