# -----------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -----------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

''' This module tests the line_limit module using pytest. '''

# imports
from __future__ import absolute_import
import os
import pytest
from psyclone.line_length import FortLineLength
from psyclone.generator import generate

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
    print "("+input_file+")"
    print "("+output_file+")"
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
    print "("+EXPECTED_OUTPUT+")"
    print "("+output_file+")"
    assert output_file == EXPECTED_OUTPUT, "output and expected output differ "


def test_wrapped_lower():
    ''' Tests that a lower case file whose lines are longer than the
    specified line length is wrapped appropriately by the
    FortLineLength class'''
    fll = FortLineLength(line_length=30)
    output_file = fll.process(INPUT_FILE.lower())
    print "("+EXPECTED_OUTPUT.lower()+")"
    print "("+output_file+")"
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
    print "("+output_file+")"
    print expected_output
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
    print output_string
    print expected_output
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
    print output_string
    print expected_output
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


def test_long_lines_true():
    ''' Tests that the long_lines method returns true with fortran
    input which has at least one line longer than the specified
    maximum'''
    input_string = (
        "! line1\n"
        "! " + "line2"*6 + "\n"
        "! line3\n")
    fll = FortLineLength(line_length=30)
    assert fll.long_lines(input_string),\
        "long_lines_true test should return True"


def test_long_lines_false():
    ''' Tests that the long_lines method returns false with fortran
    input which has all lines shorter than the specified maximum'''
    input_string = (
        "! line1\n"
        "! " + "line2"*5 + "\n" +
        "! line3\n")
    fll = FortLineLength(line_length=30)
    assert not fll.long_lines(input_string),\
        "long_lines_false test should return False"


def test_length():
    ''' Tests that the length method returns the expected value '''
    input_length = 20
    fll = FortLineLength(line_length=input_length)
    output_length = fll.length
    assert output_length == input_length,\
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
