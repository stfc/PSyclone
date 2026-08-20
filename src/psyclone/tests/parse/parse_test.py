# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' A module to perform pytest unit and functional tests on the parse
function. '''


import os
import pytest
from psyclone.parse.algorithm import parse, ParseError

TEST_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..",
                         "test_files", "lfric")


def test_continuators_kernel():
    '''Tests that an input kernel file with long lines that already has
       continuators to make the code conform to the line length limit
       does not cause an error. '''
    _, _ = parse(os.path.join(TEST_PATH, "1.1.0_single_invoke_xyoz_qr.f90"),
                 api="lfric", line_length=True)


def test_continuators_algorithm():
    '''Tests that an input algorithm file with long lines that already has
       continuators to make the code conform to the line length limit
       does not cause an error. '''
    _, _ = parse(os.path.join(TEST_PATH, "13.2_alg_long_line_continuator.f90"),
                 api="lfric", line_length=True)


def test_get_builtin_map_wrong_api():
    ''' Check that we raise an appropriate error if we call
    get_builtin_defs() with an invalid API '''
    import psyclone.parse.algorithm as pparse
    with pytest.raises(ParseError) as excinfo:
        _ = pparse.get_builtin_map('invalid_api')
    assert "check_api: Unsupported API 'invalid_api'" in str(excinfo.value)


def test_builtin_with_use():
    ''' Check that we raise an error if we encounter a use statement for
    a built-in operation '''
    with pytest.raises(ParseError) as excinfo:
        _, _ = parse(
            os.path.join(TEST_PATH, "15.12.2_builtin_with_use.f90"),
            api="lfric")
    assert ("A built-in cannot be named in a use statement but "
            "'setval_c' is used from module 'fake_builtin_mod' in "
            in str(excinfo.value))


def test_too_many_names_invoke():
    ''' Test that we raise the expected error when the invoke contains
    more than one name=xxx argument. '''
    with pytest.raises(ParseError) as err:
        _, _ = parse(
            os.path.join(TEST_PATH, "1.0.2_many_named_invoke.f90"),
            api="lfric")
    assert "An invoke must contain one or zero " in str(err.value)
    assert "1.0.2_many_named_invoke.f90" in str(err.value)


def test_wrong_named_invoke():
    ''' Test that we raise the expected error when the invoke contains
    a named argument where the argument is not called 'name' '''
    with pytest.raises(ParseError) as err:
        _, _ = parse(
            os.path.join(TEST_PATH, "1.0.3_wrong_named_arg_invoke.f90"),
            api="lfric")
    assert ("Expected named identifier to be 'name' but found "
            "'not_a_name'" in str(err.value))


def test_wrong_type_named_invoke():
    ''' Test that we raise the expected error when the invoke contains
    a named argument but its value is not a string '''
    with pytest.raises(ParseError) as err:
        _, _ = parse(
            os.path.join(TEST_PATH, "1.0.4_wrong_type_named_arg_invoke.f90"),
            api="lfric")
    assert ("The (optional) name of an invoke must be specified as a "
            "string" in str(err.value))
    assert "1.0.4_wrong_type_named_arg_invoke.f90" in str(err.value)


def test_invalid_named_invoke():
    ''' Test that we raise the expected error when the invoke contains
    a named argument but its value is not a valid Fortran name '''
    with pytest.raises(ParseError) as err:
        _, _ = parse(
            os.path.join(TEST_PATH, "1.0.6_invoke_name_invalid_chars.f90"),
            api="lfric")
    assert ("the (optional) name of an invoke must be a string containing a "
            "valid Fortran name (with no whitespace) but "
            "got 'jack(1)' " in str(err.value))
    assert "1.0.6_invoke_name_invalid_chars.f90" in str(err.value)


def test_duplicate_named_invoke():
    ''' Test that we raise the expected error when an algorithm file
    contains two invokes that are given the same name '''
    with pytest.raises(ParseError) as err:
        _, _ = parse(os.path.join(
            TEST_PATH, "3.3_multi_functions_multi_invokes_name_clash.f90"),
                     api="lfric")
    assert ("Found multiple named invoke()'s with the same label ('jack') "
            "when parsing " in str(err.value))
    assert "3.3_multi_functions_multi_invokes_name_clash.f90" in str(err.value)


def test_duplicate_named_invoke_case():
    ''' Test that we raise the expected error when an algorithm file
    contains two invokes that are given the same name but with different
    case. '''
    with pytest.raises(ParseError) as err:
        _, _ = parse(os.path.join(
            TEST_PATH, "3.4_multi_invoke_name_clash_case_insensitive.f90"),
                     api="lfric")
    assert ("Found multiple named invoke()'s with the same label ('jack') "
            "when parsing " in str(err.value))
    assert "3.4_multi_invoke_name_clash_case_insensitive.f90" in str(err.value)
