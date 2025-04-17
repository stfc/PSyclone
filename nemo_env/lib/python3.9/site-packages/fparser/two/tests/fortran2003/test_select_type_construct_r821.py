# Modified work Copyright (c) 2022 Science and Technology
# Facilities Council.
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


import pytest
from fparser.api import get_reader
from fparser.two.Fortran2003 import Select_Type_Construct
from fparser.two.utils import FortranSyntaxError


def test_select_type_construct():
    """Check 'select type' construct is parsed correctly"""
    tcls = Select_Type_Construct
    tree = tcls(
        get_reader(
            """\
            n:SELECT TYPE ( A => P_OR_C )
            CLASS IS ( POINT ) n
            PRINT *, A%X, A%Y ! This block gets executed
            TYPE IS ( POINT_3D ) n
            PRINT *, A%X, A%Y, A%Z
            END SELECT n
            """,
            ignore_comments=False,
        )
    )
    assert (
        str(tree) == "n:SELECT TYPE(A=>P_OR_C)\n"
        "  CLASS IS (POINT) n\n"
        "  PRINT *, A % X, A % Y\n"
        "  ! This block gets executed\n"
        "  TYPE IS (POINT_3D) n\n"
        "  PRINT *, A % X, A % Y, A % Z\n"
        "END SELECT n"
    )


def test_select_type_construct_wrong_name(f2003_create, fake_symbol_table):
    """Check named 'select type' construct has correct start/end names"""
    with pytest.raises(FortranSyntaxError) as exc_info:
        Select_Type_Construct(
            get_reader(
                """\
            name: select type (n)
            type is (point)
                a = 1
            end select wrong"""
            )
        )
    assert exc_info.value.args[0].endswith("Expecting name 'name', got 'wrong'")


def test_select_type_construct_missing_start_name(f2003_create, fake_symbol_table):
    """Check named 'select type' construct has correct start/end names"""
    with pytest.raises(FortranSyntaxError) as exc_info:
        Select_Type_Construct(
            get_reader(
                """\
            select type(n)
            type is (point)
                a = 1
            end select name"""
            )
        )
    assert exc_info.value.args[0].endswith(
        "Name 'name' has no corresponding starting name"
    )


def test_select_type_construct_missing_end_name(f2003_create, fake_symbol_table):
    """Check named 'select type' construct has correct start/end names"""
    with pytest.raises(FortranSyntaxError) as exc_info:
        Select_Type_Construct(
            get_reader(
                """\
            name: select type(n)
            type is (point)
                a = 1
            end select"""
            )
        )
    assert exc_info.value.args[0].endswith("Expecting name 'name' but none given")


def test_select_type_construct_select_type_wrong_name(f2003_create, fake_symbol_table):
    """Check named 'select type' construct has correct start/end names"""
    with pytest.raises(FortranSyntaxError) as exc_info:
        Select_Type_Construct(
            get_reader(
                """\
            name: select type(n)
            type is (point) wrong
                a = 1
            end select name"""
            )
        )
    assert exc_info.value.args[0].endswith("Expecting name 'name', got 'wrong'")
