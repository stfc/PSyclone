# Copyright (c) 2021 Science and Technology Facilities Council.

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

"""Test Fortran 2003 rule R1232 : the majority of the tests for this
are still in test_fortran2003.py and need to be moved here TODO #306.

"""

import pytest
from fparser.api import get_reader
from fparser.two.Fortran2003 import Subroutine_Subprogram, Subroutine_Stmt, Name
from fparser.two.symbol_table import SYMBOL_TABLES


def test_sub_stmt_new_symbol_table(f2003_create):
    """
    Test that valid code is parsed correctly and an associated symbol table
    created.

    """
    obj = Subroutine_Subprogram(get_reader("subroutine a\nend subroutine"))
    assert isinstance(obj, Subroutine_Subprogram)
    assert str(obj) == "SUBROUTINE a\nEND SUBROUTINE"
    assert repr(obj) == (
        "Subroutine_Subprogram(Subroutine_Stmt(None, "
        "Name('a'), None, None), End_Subroutine_Stmt('"
        "SUBROUTINE', None))"
    )
    assert "a" in SYMBOL_TABLES._symbol_tables


def test_subroutine_get_name():
    """Test we can get the name of the subroutine"""
    obj = Subroutine_Stmt("subroutine foo")
    assert obj.get_name() == Name("foo")
