# This Python file uses the following encoding: utf-8
# Copyright (c) 2020-2022 Science and Technology Facilities Council.

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

""" pytest module for the Fortran2003 Where Construct - R744. Note that
    this requires re-implementing and extending to fully cover the
    language specification (#232). """

import pytest
from fparser.api import get_reader
from fparser.common.readfortran import FortranStringReader
from fparser.two.Fortran2003 import Where_Construct
from fparser.two.utils import FortranSyntaxError


@pytest.mark.usefixtures("f2003_create")
def test_where_construct():
    """Tests for the WHERE construct, R744."""
    tcls = Where_Construct
    obj = tcls(
        get_reader(
            """\
    where (pressure <= 1.0)
    pressure = pressure + inc_pressure
    temp = temp - 5.0
    elsewhere
    raining = .true.
    end where
"""
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert (
        str(obj) == "WHERE (pressure <= 1.0)\n  "
        "pressure = pressure + inc_pressure\n  "
        "temp = temp - 5.0\n"
        "ELSEWHERE\n  raining = .TRUE.\nEND WHERE"
    )

    obj = tcls(
        get_reader(
            """\
    where (cond1)
    else    where (cond2)
    end where
"""
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "WHERE (cond1)\nELSEWHERE(cond2)\nEND WHERE"

    obj = tcls(
        get_reader(
            """\
    n:where (cond1)
    elsewhere (cond2) n
    else   where n
    end where n
"""
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert (
        str(obj) == "n:WHERE (cond1)\nELSEWHERE(cond2) n\n" "ELSEWHERE n\nEND WHERE n"
    )

    obj = tcls(
        get_reader(
            """\
    n:where (cond1)
    else where (cond2) n
    else where n
    end where n
"""
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert (
        str(obj) == "n:WHERE (cond1)\nELSEWHERE(cond2) n\nELSEWHERE n\n" "END WHERE n"
    )

    obj = tcls(
        get_reader(
            """\
    n:where (me(:)=="hello")
    else where (me(:)=="goodbye") n
    else where n
    end where n
"""
        )
    )
    assert (
        str(obj) == 'n:WHERE (me(:) == "hello")\nELSEWHERE(me(:) == "goodbye") n\n'
        "ELSEWHERE n\n"
        "END WHERE n"
    )


@pytest.mark.usefixtures("f2003_create")
def test_where_tofortran_non_ascii():
    """Check that the tofortran() method works when a WHERE construct
    contains a character string with non-ascii characters."""
    code = "WHERE(iflag)\n" "  msg_list = ' for e1=1\xb0'\n" "END WHERE\n"
    reader = FortranStringReader(code, ignore_comments=False)
    obj = Where_Construct(reader)
    out_str = str(obj)
    assert "for e1=1" in out_str


def test_where_construct_wrong_name(f2003_create, fake_symbol_table):
    """Check named 'where' construct has correct start/end name"""
    with pytest.raises(FortranSyntaxError) as exc_info:
        Where_Construct(
            get_reader(
                """\
            name: where (expr)
                a = 1
            end where wrong"""
            )
        )
    assert exc_info.value.args[0].endswith("Expecting name 'name', got 'wrong'")


def test_where_construct_missing_start_name(f2003_create, fake_symbol_table):
    """Check named 'where' construct has correct start/end name"""
    with pytest.raises(FortranSyntaxError) as exc_info:
        Where_Construct(
            get_reader(
                """\
            where (expr)
                a = 1
            end where name"""
            )
        )
    assert exc_info.value.args[0].endswith(
        "Name 'name' has no corresponding starting name"
    )


def test_where_construct_missing_end_name(f2003_create, fake_symbol_table):
    """Check named 'where' construct has correct start/end name"""
    with pytest.raises(FortranSyntaxError) as exc_info:
        Where_Construct(
            get_reader(
                """\
            name: where (expr)
                a = 1
            end where"""
            )
        )
    assert exc_info.value.args[0].endswith("Expecting name 'name' but none given")


def test_where_construct_else_wrong_end_name(f2003_create, fake_symbol_table):
    """Check named 'where' construct has correct start/end name"""
    with pytest.raises(FortranSyntaxError) as exc_info:
        Where_Construct(
            get_reader(
                """\
            name: where (expr)
                a = 1
            elsewhere
                a = 2
            end where wrong"""
            )
        )
    assert exc_info.value.args[0].endswith("Expecting name 'name', got 'wrong'")


def test_where_construct_else_wrong_name(f2003_create, fake_symbol_table):
    """Check named 'where' construct has correct start/end name"""
    with pytest.raises(FortranSyntaxError) as exc_info:
        Where_Construct(
            get_reader(
                """\
            name: where (expr)
                a = 1
            elsewhere wrong
                a = 2
            end where name"""
            )
        )
    assert exc_info.value.args[0].endswith("Expecting name 'name', got 'wrong'")


def test_where_construct_else_where_wrong_name(f2003_create, fake_symbol_table):
    """Check named 'where' construct has correct start/end name"""
    with pytest.raises(FortranSyntaxError) as exc_info:
        Where_Construct(
            get_reader(
                """\
            name: where (expr)
                a = 1
            elsewhere (other_expr) wrong
                a = 2
            end where name"""
            )
        )
    assert exc_info.value.args[0].endswith("Expecting name 'name', got 'wrong'")
