# Copyright (c) 2022 Science and Technology Facilities Council.

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

"""
pytest tests for Fortran2003 rule R501 - Type Declaration Statement.

TODO #318 - these tests need extending to fully cover the
Fortran2003.Type_Declaration_Stmtclass. They will also need to be
extended as part of #259 as that will add support for the various
constraints that apply to R501.

"""

import pytest
from fparser.two import Fortran2003
from fparser.two.symbol_table import SYMBOL_TABLES


@pytest.mark.usefixtures("f2003_create")
@pytest.mark.parametrize("table_name", ["", "test_mod"])
def test_type_declaration_stmt(table_name):
    """Various tests for the type declaration statement (R501). We test both
    with and without an existing scoping region."""
    if table_name:
        SYMBOL_TABLES.enter_scope(table_name)
    table = SYMBOL_TABLES.current_scope

    tcls = Fortran2003.Type_Declaration_Stmt
    obj = tcls("integer a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "INTEGER :: a"
    assert (
        repr(obj) == "Type_Declaration_Stmt(Intrinsic_Type_Spec('INTEGER', "
        "None), None, Entity_Decl_List(',', (Entity_Decl(Name('a'), None, "
        "None, None),)))"
    )
    if table:
        assert "a" in table._data_symbols

    obj = tcls("integer ,dimension(2):: b*3")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "INTEGER, DIMENSION(2) :: b*3"
    if table:
        assert "b" in table._data_symbols

    obj = tcls("real c")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "REAL :: c"
    assert (
        repr(obj) == "Type_Declaration_Stmt(Intrinsic_Type_Spec('REAL', None), None, "
        "Entity_Decl_List(',', (Entity_Decl(Name('c'), None, None, "
        "None),)))"
    )
    if table:
        assert "c" in table._data_symbols

    obj = tcls("REAL D( LDA, * ), E( LDB, * )")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "REAL :: D(LDA, *), E(LDB, *)"
    if table:
        assert "d" in table._data_symbols
        assert "e" in table._data_symbols

    obj = tcls("DOUBLE PRECISION   ALPHA, BETA")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "DOUBLE PRECISION :: ALPHA, BETA"
    if table:
        assert "alpha" in table._data_symbols
        assert "beta" in table._data_symbols

    obj = tcls("logical,parameter:: T=.true.")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "LOGICAL, PARAMETER :: T = .TRUE."
    if table:
        assert "t" in table._data_symbols

    obj = tcls("character(n),private:: x(n)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "CHARACTER(LEN = n), PRIVATE :: x(n)"
    if table:
        assert "x" in table._data_symbols

    obj = tcls("character(lenmax),private:: y(n)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "CHARACTER(LEN = lenmax), PRIVATE :: y(n)"
    if table:
        assert "y" in table._data_symbols
