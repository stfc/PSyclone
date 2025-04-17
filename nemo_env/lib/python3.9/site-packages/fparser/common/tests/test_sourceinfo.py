# -*- coding: utf-8 -*-
##############################################################################
# Copyright (c) 2017-2023 Science and Technology Facilities Council.
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
# Modified M.Hambley, UK Met Office
##############################################################################
"""
Test battery associated with fparser.sourceinfo package.
"""

import os
import tempfile
import pytest

from fparser.common.sourceinfo import (
    FortranFormat,
    get_source_info_str,
    get_source_info,
)


##############################################################################


def test_format_constructor_faults():
    """
    Tests that the constructor correctly rejects attempts to create an object
    with "None" arguments.
    """
    with pytest.raises(Exception):
        _unit_under_test = FortranFormat(True, None)

    with pytest.raises(Exception):
        _unit_under_test = FortranFormat(None, True)


##############################################################################
@pytest.fixture(
    scope="module",
    params=[
        (False, False, "fix", "Non-strict fixed format"),
        (False, True, "f77", "Strict fixed format"),
        (True, False, "free", "Non-strict free format"),
        (True, True, "pyf", "Strict free format"),
    ],
)
def pretty(request):
    """
    Returns all possible permutations of flags and their corresponding
    mode and descriptive strings.
    """
    return request.param


##############################################################################


def test_fortranformat_constructor(pretty):
    # pylint: disable=redefined-outer-name
    """
    Tests the constructor correctly sets up the object.
    """
    unit_under_test = FortranFormat(pretty[0], pretty[1])
    assert str(unit_under_test) == pretty[3]
    assert unit_under_test.is_free == pretty[0]
    assert unit_under_test.is_fixed == (not pretty[0])
    assert unit_under_test.is_strict == pretty[1]
    assert unit_under_test.is_f77 == (not pretty[0] and pretty[1])
    assert unit_under_test.is_fix == (not pretty[0] and not pretty[1])
    assert unit_under_test.is_pyf == (pretty[0] and pretty[1])
    assert unit_under_test.mode == pretty[2]
    assert not unit_under_test.f2py_enabled


@pytest.mark.parametrize(
    "permutations", [(False, False), (False, True), (True, False), (True, True)]
)
def test_fortranformat_equality(permutations, pretty):
    # pylint: disable=redefined-outer-name
    """
    Tests that the equality operator works as expected.
    """
    expected = (permutations[0] == pretty[0]) and (permutations[1] == pretty[1])
    unit_under_test = FortranFormat(permutations[0], permutations[1])
    candidate = FortranFormat(pretty[0], pretty[1])
    assert (unit_under_test == candidate) == expected


##############################################################################


def test_fortranformat_invalid():
    """
    Tests that the equality operator understands that it can't compare apples
    and oranges.
    """
    unit_under_test = FortranFormat(True, False)
    with pytest.raises(NotImplementedError):
        if unit_under_test == "oranges":
            raise Exception("That shouldn't have happened")


##############################################################################


@pytest.fixture(
    scope="module",
    params=[
        ("free", True, False),
        ("f77", False, True),
        ("fix", False, False),
        ("pyf", True, True),
    ],
)
def mode(request):
    """
    Returns all possible mode strings and their corresponding flags.
    """
    return request.param


##############################################################################


def test_fortranformat_from_mode(mode):
    # pylint: disable=redefined-outer-name
    """
    Tests that the object is correctly created by the from_mode function.
    """
    unit_under_test = FortranFormat.from_mode(mode[0])
    assert unit_under_test.mode == mode[0]
    assert unit_under_test.is_free == mode[1]
    assert unit_under_test.is_fixed == (not mode[1])
    assert unit_under_test.is_strict == mode[2]
    assert unit_under_test.is_f77 == (not mode[1] and mode[2])
    assert unit_under_test.is_fix == (not mode[1] and not mode[2])
    assert unit_under_test.is_pyf == (mode[1] and mode[2])
    assert str(unit_under_test.mode) == mode[0]
    assert not unit_under_test.f2py_enabled


def test_format_from_mode_bad():
    """
    Tests that an exception is thrown for an unrecognised mode string.
    """
    with pytest.raises(NotImplementedError):
        _unit_under_test = FortranFormat.from_mode("cheese")


##############################################################################
# Setting up a pytest fixture in this way is a mechanism for creating
# parameterised tests.
#
# Normally when a test includes a fixture in its argument list the
# corresponding function will be called and the result passed in to the test.
#
# When used like this the test will be called once for each value the fixture
# function returns. So in this case any test including "header" in its
# argument list will be run once with "header" equal to "! -*- fortran -*-",
# then with "header" equal to "! -*- f77 -*-" and so on.
#
# If a test includes multiple parameter fixtures it will be called for every
# permutation thus afforded.
#
@pytest.fixture(
    scope="module",
    params=[
        (None, FortranFormat(True, True)),
        ("! -*- fortran -*-", FortranFormat(False, True)),
        ("! -*- f77 -*-", FortranFormat(False, True)),
        ("! -*- f90 -*-", FortranFormat(True, False)),
        ("! -*- f03 -*-", FortranFormat(True, False)),
        ("! -*- f08 -*-", FortranFormat(True, False)),
        ("! -*- fix -*-", FortranFormat(False, False)),
        ("! -*- pyf -*-", FortranFormat(True, True)),
    ],
    name="header",
)
def header_fixture(request):
    """
    Returns parameters for header tests.
    """
    return request.param


##############################################################################

_FIXED_SOURCE = """      program main
      end program main
"""

_FREE_SOURCE = """program main
end program main
"""

_FIXED_WITH_CONTINUE = """      program main
          implicit none
          integer :: foo, &
                     bar
      end program main
"""

_FIXED_WITH_COMMENTS = """!     The program
      program main
c         Enforce explicit variable declaration
          implicit none
*         variables
          integer :: foo
      end program main
"""

# Tabs are not actually in the Fortran character set but fparser has handled
# them in the past even if it shouldn't have. We have to continue handling
# them for the time being until we work out why they were supported.
#
_INITIAL_TAB = "\tprogram main\n"
_MIDDLE_TAB = " \tprogram main\n"


# Another parameterised test fixture. See "header" above.
#
@pytest.fixture(
    scope="module",
    params=[
        (None, FortranFormat(False, False)),
        (_FIXED_SOURCE, FortranFormat(False, False)),
        (_FREE_SOURCE, FortranFormat(True, False)),
        (_FIXED_WITH_CONTINUE, FortranFormat(True, False)),
        (_FIXED_WITH_COMMENTS, FortranFormat(False, False)),
        (_INITIAL_TAB, FortranFormat(False, False)),
        (_MIDDLE_TAB, FortranFormat(True, False)),
    ],
)
def content(request):
    """
    Returns parameters for content tests.
    """
    return request.param


##############################################################################


def test_get_source_info_str(header, content):
    # pylint: disable=redefined-outer-name
    """
    Tests that source format is correctly identified when read from a string.
    """
    full_source = ""
    if header[0] is not None:
        full_source += header[0] + "\n"
    if content[0] is not None:
        full_source += content[0]

    source_info = get_source_info_str(full_source, ignore_encoding=False)
    if header[0]:
        assert source_info == header[1]
    else:  # No header
        assert source_info == content[1]


##############################################################################


# Another parameterised test fixture. See "header" above.
#
@pytest.fixture(
    scope="module",
    params=[
        (".f", None),
        (".f90", None),
        (".pyf", FortranFormat(True, True)),
        (".guff", None),
    ],
)
def extension(request):
    """
    Returns parameters for extension tests.
    """
    return request.param


##############################################################################


def test_get_source_info_filename(extension, header, content):
    # pylint: disable=redefined-outer-name
    """
    Tests that source format is correctly identified when read from a file.
    """
    full_source = ""
    if header[0] is not None:
        full_source += header[0] + "\n"
    if content[0] is not None:
        full_source += content[0]

    source_file, filename = tempfile.mkstemp(suffix=extension[0], text=True)
    os.close(source_file)

    with open(filename, "w") as source_file:
        print(full_source, file=source_file)

    try:
        source_info = get_source_info(filename, ignore_encoding=False)
        if extension[1] is not None:
            assert source_info == extension[1]
        elif header[0] is not None:
            assert source_info == header[1]
        else:  # No header
            assert source_info == content[1]
    except Exception as ex:
        os.remove(filename)
        raise ex


##############################################################################


def test_get_source_info_file(extension, header, content):
    # pylint: disable=redefined-outer-name
    """
    Tests that source format is correctly identified when read from a file.
    """
    full_source = ""
    if header[0] is not None:
        full_source += header[0] + "\n"
    if content[0] is not None:
        full_source += content[0]

    with tempfile.TemporaryFile(mode="w+", suffix=extension[0]) as source_file:
        print(full_source, file=source_file)
        source_file.seek(0)

        source_info = get_source_info(source_file, ignore_encoding=False)
        if header[0] is not None:
            assert source_info == header[1]
        else:  # No header
            assert source_info == content[1]


def test_get_source_info_utf8():
    """
    Tests that Fortran code containing a unicode character can be read
    by the get_source_info method.

    """
    encoding = dict(encoding="UTF-8")
    with tempfile.NamedTemporaryFile(mode="w", **encoding) as tmp_file:
        content = '  ! A fortran comment with a unicode character "\u2014"'
        tmp_file.write(content)
        tmp_file.flush()

        source_info = get_source_info(tmp_file.name)
    assert source_info is not None


##############################################################################


def test_get_source_info_wrong():
    """
    Tests that get_source_info throws an exception if passed the wrong type
    of argument.
    """
    with pytest.raises(ValueError):
        _source_info = get_source_info(42)  # Obviously wrong

    with pytest.raises(ValueError):
        _source_info = get_source_info(["one"])  # Less obviously wrong


##############################################################################
