# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab
# Modified by A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module tests the line_limit module using pytest. '''

# imports
import os
import pytest
from psyclone.line_length import FortLineLength, find_break_point
from psyclone.generator import generate
from psyclone.errors import InternalError

# functions


def test_openmp_directive():
    ''' Tests that we successfully break an OpenMP directive line
    on a space '''
    input_file = "  !$OMP PARALLEL LOOP\n"
    expected_output = "  !$OMP PARALLEL  &\n!$omp& LOOP\n"
    fll = FortLineLength(line_length=len(input_file)-3)
    output_file = fll.process(input_file)
    assert output_file == expected_output


def test_acc_directive():
    ''' Tests that we deal with an OpenACC directive appropriately
    when its needs to be line wrapped '''
    input_file = "  !$ACC kernels loop gang(32), vector(16)\n"
    expected_output = "  !$ACC kernels loop gang(32),  &\n!$acc& vector(16)\n"
    fll = FortLineLength(line_length=len(input_file)-5)
    output_file = fll.process(input_file)
    assert output_file == expected_output


def test_comment():
    ''' Tests that a long comment line wrapped as expected '''
    input_file = " ! this is a comment"
    expected_output = " ! this is a \n!& comment"
    fll = FortLineLength(line_length=len(input_file)-5)
    output_file = fll.process(input_file)
    assert output_file == expected_output


def test_unchanged():
    ''' Tests that a file whose lines are shorter than the specified
    line length is unchanged by the FortLineLength class '''
    input_file = (
        "    INTEGER stuff\n"
        "    REAL stuff\n"
        "    TYPE stuff\n"
        "    CALL stuff\n"
        "    SUBROUTINE stuff\n"
        "    USE stuff\n"
        "    !$OMP stuff\n"
        "    !$ACC stuff\n"
        "    ! stuff\n"
        "    stuff\n")
    fll = FortLineLength(line_length=25)
    output_file = fll.process(input_file)
    print("("+input_file+")")
    print("("+output_file+")")
    assert input_file == output_file, "input should remain unchanged"


INPUT_FILE = (
    "    INTEGER    stuff blah blah blah\n"
    "    REAL       stuff blah blah blah\n"
    "    TYPE       stuff blah blah blah\n"
    "    CALL       stuff blah blah blah\n"
    "    SUBROUTINE stuff blah blah blah\n"
    "    USE        stuff blah blah blah\n"
    "    !$OMP      stuff blah blah blah\n"
    "    !$ACC      stuff blah blah blah\n"
    "    ! stuff     blah blah blah blah\n"
    "    stuff\n")

EXPECTED_OUTPUT = (
    "    INTEGER    stuff blah &\n"
    "&blah blah\n"
    "    REAL       stuff blah &\n"
    "&blah blah\n"
    "    TYPE       stuff blah &\n"
    "&blah blah\n"
    "    CALL       stuff blah &\n"
    "&blah blah\n"
    "    SUBROUTINE stuff blah &\n"
    "&blah blah\n"
    "    USE        stuff blah &\n"
    "&blah blah\n"
    "    !$OMP      stuff blah  &\n"
    "!$omp& blah blah\n"
    "    !$ACC      stuff blah  &\n"
    "!$acc& blah blah\n"
    "    ! stuff     blah blah \n"
    "!& blah blah\n"
    "    stuff\n")


def test_wrapped():
    ''' Tests that a file whose lines are longer than the specified
    line length is wrapped appropriately by the FortLineLength class '''
    fll = FortLineLength(line_length=30)
    output_file = fll.process(INPUT_FILE)
    print("("+EXPECTED_OUTPUT+")")
    print("("+output_file+")")
    assert output_file == EXPECTED_OUTPUT, "output and expected output differ "


def test_wrapped_lower():
    ''' Tests that a lower case file whose lines are longer than the
    specified line length is wrapped appropriately by the
    FortLineLength class'''
    fll = FortLineLength(line_length=30)
    output_file = fll.process(INPUT_FILE.lower())
    print("("+EXPECTED_OUTPUT.lower()+")")
    print("("+output_file+")")
    assert output_file == EXPECTED_OUTPUT.lower(), \
        "output and expected output differ "


def test_fail_to_wrap():
    ''' Tests that we raise an error if we can't find anywhere to wrap
    the line'''
    input_file = "!$OMPPARALLELDO"
    with pytest.raises(Exception) as excinfo:
        fll = FortLineLength(line_length=len(input_file)-1)
        _ = fll.process(input_file)
    assert 'No suitable break point found' in str(excinfo.value)


def test_multiple_lines_statements():
    ''' test that multiple lines works as expected for statements
    (INTEGER, REAL, TYPE, CALL, SUBROUTINE and USE) '''
    input_file = (
        "INTEGER blahdeblah, blahdeblah, blahdeblah, blahdeblah\n")
    expected_output = (
        "INTEGER &\n&blahdeblah, &\n&blahdeblah, &\n&blahdeblah,"
        " &\n&blahdeblah\n")
    fll = FortLineLength(line_length=18)
    output_file = fll.process(input_file)
    assert output_file == expected_output


def test_multiple_lines_omp():
    ''' test that multiple lines works as expected for OpenMP directives '''
    input_file = (
        "!$omp blahdeblah, blahdeblah, blahdeblah, blahdeblah\n")
    expected_output = (
        "!$omp blahdeblah,  &\n!$omp& blahdeblah,  &\n!$omp& blahdeblah,"
        "  &\n!$omp& blahdeblah\n")
    fll = FortLineLength(line_length=24)
    output_file = fll.process(input_file)
    assert output_file == expected_output


def test_multiple_lines_acc():
    ''' test that multiple lines works as expected for OpenACC directives '''
    input_file = (
        "!$acc blahdeblah, blahdeblah, blahdeblah, blahdeblah\n")
    expected_output = (
        "!$acc blahdeblah,  &\n!$acc& blahdeblah,  &\n!$acc& blahdeblah,"
        "  &\n!$acc& blahdeblah\n")
    fll = FortLineLength(line_length=24)
    output_file = fll.process(input_file)
    assert output_file == expected_output


def test_multiple_lines_comment():
    ''' test that multiple lines works as expected for comments '''
    input_file = (
        "! blahdeblah, blahdeblah, blahdeblah, blahdeblah\n")
    expected_output = (
        "! blahdeblah, \n!& blahdeblah, \n!& blahdeblah, \n!& blahdeblah\n")
    fll = FortLineLength(line_length=18)
    output_file = fll.process(input_file)
    assert output_file == expected_output


def test_exception_line_too_long():
    ''' Test that output lines are not longer than the maximum
    specified'''
    input_file = (
        "INTEGER stuffynostrils, blahdeblah,blahdeblah blahdeblah\n"
        "!$omp stuffynostrils,(blahdeblah)blahdeblah=(blahdeblah)\n"
        "!$acc stuffynostrils,(blahdeblah)blahdeblah=(blahdeblah)\n"
        "!     stuffynostrils,blahdeblah.blahdeblah blahdeblah).\n")
    fll = FortLineLength(line_length=24)
    output_file = fll.process(input_file)
    for line in output_file.split('\n'):
        assert len(line) <= 24, \
            "Error, output line is longer than the maximum allowed"


def test_break_types_multi_line():
    ''' Test the different supported line breaks.'''
    input_file = (
        "INTEGER stuffynostrils, blahdeblah,blahdeblah blahdeblah\n"
        "!$omp stuffynostrils,(blahdeblah)blahdeblah=(blahdeblah)\n"
        "!$acc stuffynostrils,(blahdeblah)blahdeblah=(blahdeblah)\n"
        "!     stuffynostrils,blahdeblah.blahdeblah blahdeblah).\n")
    expected_output = (
        "INTEGER stuffynostrils,&\n& blahdeblah,&\n&blahdeblah"
        " blahdeblah\n"
        "!$omp  &\n!$omp& stuffynostrils, &\n!$omp& (blahdeblah) &\n!$omp&"
        " blahdeblah= &\n!$omp& (blahdeblah)\n"
        "!$acc  &\n!$acc& stuffynostrils, &\n!$acc& (blahdeblah) &\n!$acc&"
        " blahdeblah= &\n!$acc& (blahdeblah)\n"
        "!     \n!& stuffynostrils,\n!& blahdeblah.\n!& blahdeblah \n!&"
        " blahdeblah).\n")

    fll = FortLineLength(line_length=24)
    output_file = fll.process(input_file)
    print("("+output_file+")")
    print(expected_output)
    assert output_file == expected_output


def test_edge_conditions_statements():
    ''' Test that we get correct behaviour using statements (INTEGER,
    REAL, TYPE, CALL, SUBROUTINE and USE) when the input line equals
    the max line length, or multiples thereof and lengths one larger
    and one smaller. This is to make sure we don't have issues like
    ending up with a continuation but no following line.'''
    input_string = (
        "INTEGER INTEG\n"
        "INTEGER INTEGE\n"
        "INTEGER INTEGER\n")
    expected_output = (
        "INTEGER INTEG\n"
        "INTEGER INTEGE\n"
        "INTEGER &\n&INTEGER\n")
    fll = FortLineLength(line_length=len("INTEGER INTEGE"))
    output_string = fll.process(input_string)
    print(output_string)
    print(expected_output)
    assert output_string == expected_output

    input_string = (
        "INTEGER INTEGER INTEG\n"
        "INTEGER INTEGER INTEGE\n"
        "INTEGER INTEGER INTEGER\n")
    expected_output = (
        "INTEGER &\n&INTEGER INTEG\n"
        "INTEGER &\n&INTEGER INTEGE\n"
        "INTEGER &\n&INTEGER &\n&INTEGER\n")
    fll = FortLineLength(line_length=len("INTEGER INTEGER"))
    output_string = fll.process(input_string)
    print(output_string)
    print(expected_output)
    assert output_string == expected_output


def test_edge_conditions_omp():
    '''Test that we get correct behaviour using OpenMP directives when
    the input line equals the max line length, or multiples thereof
    and lengths one larger and one smaller. This is to make sure we
    don't have issues like ending up with a continuation but no
    following line. '''
    input_string = (
        "!$OMP  OPENMP OPEN\n"
        "!$OMP  OPENMP OPENM\n"
        "!$OMP  OPENMP OPENMP\n"
        "!$OMP  OPENMP OPENMP OPEN\n"
        "!$OMP  OPENMP OPENMP OPENM\n"
        "!$OMP  OPENMP OPENMP OPENMP\n")
    expected_output = (
        "!$OMP  OPENMP OPEN\n"
        "!$OMP  OPENMP OPENM\n"
        "!$OMP  OPENMP  &\n!$omp& OPENMP\n"
        "!$OMP  OPENMP  &\n!$omp& OPENMP OPEN\n"
        "!$OMP  OPENMP  &\n!$omp& OPENMP OPENM\n"
        "!$OMP  OPENMP  &\n!$omp& OPENMP  &\n!$omp& OPENMP\n")
    fll = FortLineLength(line_length=len("!$OMP  OPENMP OPENM"))
    output_string = fll.process(input_string)
    assert output_string == expected_output


def test_edge_conditions_acc():
    '''Test that we get correct behaviour using OpenACC directives when
    the input line equals the max line length, or multiples thereof
    and lengths one larger and one smaller. This is to make sure we
    don't have issues like ending up with a continuation but no
    following line. '''
    input_string = (
        "!$ACC  OPENACC OPENA\n"
        "!$ACC  OPENACC OPENAC\n"
        "!$ACC  OPENACC OPENACC\n"
        "!$ACC  OPENACC OPENACC OPENA\n"
        "!$ACC  OPENACC OPENACC OPENAC\n"
        "!$ACC  OPENACC OPENACC OPENACC\n")
    expected_output = (
        "!$ACC  OPENACC OPENA\n"
        "!$ACC  OPENACC OPENAC\n"
        "!$ACC  OPENACC  &\n!$acc& OPENACC\n"
        "!$ACC  OPENACC  &\n!$acc& OPENACC OPENA\n"
        "!$ACC  OPENACC  &\n!$acc& OPENACC OPENAC\n"
        "!$ACC  OPENACC  &\n!$acc& OPENACC  &\n!$acc& OPENACC\n")
    fll = FortLineLength(line_length=len("!$ACC  OPENACC OPENAC"))
    output_string = fll.process(input_string)
    assert output_string == expected_output


def test_edge_conditions_comments():
    '''Test that we get correct behaviour with comments when the input
    line equals the max line length, or multiples thereof and lengths
    one larger and one smaller. This is to make sure we don't have
    issues like ending up with a continuation but no following line.'''
    input_string = (
        "!  COMMENT COMME\n"
        "!  COMMENT COMMEN\n"
        "!  COMMENT COMMENT\n"
        "!  COMMENT COMMENT COMME\n"
        "!  COMMENT COMMENT COMMEN\n"
        "!  COMMENT COMMENT COMMENT\n")
    expected_output = (
        "!  COMMENT COMME\n"
        "!  COMMENT COMMEN\n"
        "!  COMMENT \n!& COMMENT\n"
        "!  COMMENT \n!& COMMENT COMME\n"
        "!  COMMENT \n!& COMMENT COMMEN\n"
        "!  COMMENT \n!& COMMENT \n!& COMMENT\n")
    fll = FortLineLength(line_length=len("!  COMMENT COMMEN"))
    output_string = fll.process(input_string)
    assert output_string == expected_output


def test_long_lines_allocate(fortran_reader, fortran_writer):
    '''Test the we get the correct behaviour and never break the
    line before the first statement.'''
    code = '''subroutine test()
    use external_module, only: some_type
    integer, dimension(:,:,:,:), allocatable :: pressure_prsc
    IF (.NOT. ALLOCATED(pressure_prsc)) &
      ALLOCATE(pressure_prsc        ( some_type%longstringname1, &
                                      some_type%longstringname2, &
                                      some_type%longstringname3, &
                                      some_type%longstringname4 ))
    end subroutine test
    '''

    psyir = fortran_reader.psyir_from_source(code)
    output = fortran_writer(psyir)
    line_length = FortLineLength()
    correct = '''  if (.NOT.ALLOCATED(pressure_prsc)) then
    ALLOCATE(pressure_prsc(1:some_type%longstringname1,1:some_type%\
longstringname2,1:some_type%longstringname3,&
&1:some_type%longstringname4))'''
    out = line_length.process(output)
    assert correct in out


def test_long_lines_indentation():
    '''Test that if we have too much initial indentation that we still
    can output a result.'''
    long_string = '''                                                     \
                                                                          \
    allocate(pressure_prsc(some_type%longstringname1, \
some_type%longstringname2))'''
    line_length = FortLineLength()
    out = line_length.process(long_string)
    correct = '''allocate(pressure_prsc(some_type%longstringname1, \
some_type%longstringname2))'''
    assert correct == out

    long_string = '''                                                     \
                                                                          \
    allocate(pressure_prsc(some_type%longstringname1, \
some_type%longstringname2, some_type%longstringname3, \
some_type%longerstringname4))'''
    out = line_length.process(long_string)
    correct = '''allocate(pressure_prsc(some_type%longstringname1, \
some_type%longstringname2, some_type%longstringname3, &
&some_type%longerstringname4))'''
    assert correct == out


def test_long_lines_true():
    ''' Tests that the long_lines method returns true with fortran
    input which has at least one line longer than the specified
    maximum'''
    input_string = (
        "! line1\n"
        "! " + "line2"*6 + "\n"
        "! line3\n")
    fll = FortLineLength(line_length=30)
    assert fll.long_lines(input_string), \
        "long_lines_true test should return True"


def test_long_lines_false():
    ''' Tests that the long_lines method returns false with fortran
    input which has all lines shorter than the specified maximum'''
    input_string = (
        "! line1\n"
        "! " + "line2"*5 + "\n" +
        "! line3\n")
    fll = FortLineLength(line_length=30)
    assert not fll.long_lines(input_string), \
        "long_lines_false test should return False"


def test_length():
    ''' Tests that the length method returns the expected value '''
    input_length = 20
    fll = FortLineLength(line_length=input_length)
    output_length = fll.length
    assert output_length == input_length, \
        "test_length expecting length method to be the same as the length" +\
        "provided on input"


def test_long_line_continuator():
    '''Tests that an input algorithm file with long lines of a type not
       recognised by FortLineLength (assignments in this case), which
       already have continuators to make the code conform to the line
       length limit, does not cause an error.
    '''
    alg, _ = generate(os.path.join(os.path.dirname(os.path.
                                                   abspath(__file__)),
                                   "test_files", "dynamo0p3",
                                   "13.2_alg_long_line_continuator.f90"),
                      api="dynamo0.3")
    input_string = str(alg)
    fll = FortLineLength()
    _ = fll.process(input_string)


@pytest.mark.parametrize("line,max_index,key_list,index", [
    ("allocate(x, y, z)", 17, [",", ")"], 14),
    ("allocate(x, y, z)", 17, ["y,", ")"], 14),
    ("allocate(x, y, z)", 17, ["allocate", ","], 14),
    ("x = z + c * y - x + 3", 22, ["x"],  17),
    ("x = max(35, 16) - 19 + 3", 21, ["+", "-"], 17)
    ])
def test_find_break_point(line, max_index, key_list, index):
    '''Tests the find_break_point routine correctly gives correct
    line break point.'''
    assert find_break_point(line, max_index, key_list) == index


def test_find_break_point_exception():
    '''Test the fail case of find_break_point raises the exception
    and expected error message.'''
    line = "allocate(x, y, z)"
    key_list = ["+"]
    max_index = 17

    with pytest.raises(InternalError) as excinfo:
        find_break_point(line, max_index, key_list)

    assert ("Error in find_break_point. No suitable break point found for line"
            " 'allocate(x, y, z)' and keys '['+']'" in str(excinfo.value))
