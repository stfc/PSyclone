# This Python file uses the following encoding: utf-8
# Copyright (c) 2019-2022 Science and Technology Facilities Council.

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

""" pytest module for the Fortran2003 Case Construct - R808.
    Does not test all aspects of R808, in particular the conditions C803-7
    are not checked - #232. """

import pytest
from fparser.api import get_reader
from fparser.common.readfortran import FortranStringReader
from fparser.two.Fortran2003 import Case_Construct
from fparser.two.utils import FortranSyntaxError


@pytest.mark.usefixtures("f2003_create")
def test_case_construct():
    """Basic test that we parse a Case Construct successfully."""
    tcls = Case_Construct
    obj = tcls(
        get_reader(
            """\
select case (n)
case (:-1)
  signum = -1
case (0)
  signum = 0
case (1:)
  signum = 1
case default
  signum = -2
end select
"""
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert (
        str(obj) == "SELECT CASE (n)\nCASE (: - 1)\n  signum = - 1\nCASE (0)\n"
        "  signum = 0\nCASE (1 :)\n  signum = 1\nCASE DEFAULT\n"
        "  signum = - 2\nEND SELECT"
    )


def test_case_construct_name(f2003_create):
    """Basic test that we parse a Case Construct successfully."""
    tcls = Case_Construct
    obj = tcls(
        get_reader(
            """\
    name: select case (n)
    case (:-1) name
        signum = -1
    case (0) name
        signum = 0
    case (1:) name
        signum = 1
    case default name
        signum = -2
    end select name
    """
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert (
        str(obj)
        == "name:SELECT CASE (n)\nCASE (: - 1) name\n  signum = - 1\nCASE (0) name\n"
        "  signum = 0\nCASE (1 :) name\n  signum = 1\nCASE DEFAULT name\n"
        "  signum = - 2\nEND SELECT name"
    )


@pytest.mark.usefixtures("f2003_create")
def test_tofortran_non_ascii():
    """Check that the tofortran() method works when the character string
    contains non-ascii characters."""
    code = (
        "SELECT CASE(iflag)\n"
        "CASE(  30  ) ! This is a comment\n"
        "  IF(lwp) WRITE(*,*) ' for e1=1\xb0'\n"
        "END SELECT\n"
    )
    reader = FortranStringReader(code, ignore_comments=False)
    obj = Case_Construct(reader)
    out_str = str(obj)
    assert "for e1=1" in out_str


def test_case_construct_wrong_name(f2003_create, fake_symbol_table):
    """Check that named 'case' block has correct matching start/end name"""
    with pytest.raises(FortranSyntaxError) as exc_info:
        Case_Construct(
            get_reader(
                """\
            name: select case (n)
            case (:-1)
                a = 1
            end select wrong"""
            )
        )
    assert exc_info.value.args[0].endswith("Expecting name 'name', got 'wrong'")


def test_case_construct_missing_start_name(f2003_create, fake_symbol_table):
    """Check that named 'case' block has correct matching start/end name"""
    with pytest.raises(FortranSyntaxError) as exc_info:
        Case_Construct(
            get_reader(
                """\
            select case(n)
            case (:-1)
                a = 1
            end select name"""
            )
        )
    assert exc_info.value.args[0].endswith(
        "Name 'name' has no corresponding starting name"
    )


def test_case_construct_missing_end_name(f2003_create, fake_symbol_table):
    """Check that named 'case' block has correct matching start/end name"""
    with pytest.raises(FortranSyntaxError) as exc_info:
        Case_Construct(
            get_reader(
                """\
            name: select case(n)
            case (:-1)
                a = 1
            end select"""
            )
        )
    assert exc_info.value.args[0].endswith("Expecting name 'name' but none given")


def test_case_construct_case_wrong_name(f2003_create, fake_symbol_table):
    """Check that named 'case' block has correct matching start/end name"""
    with pytest.raises(FortranSyntaxError) as exc_info:
        Case_Construct(
            get_reader(
                """\
            name: select case(n)
            case (:-1) wrong
                a = 1
            end select name"""
            )
        )
    assert exc_info.value.args[0].endswith("Expecting name 'name', got 'wrong'")
