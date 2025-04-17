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

"""Test Fortran 2008 rule R501.

    type-declaration-stmt is declaration-type-spec [ [ , attr-spec ] ... :: ]
                             entity-decl-list

"""

import pytest
from fparser.two.Fortran2003 import Intrinsic_Function_Reference
from fparser.two.Fortran2008 import Type_Declaration_Stmt
from fparser.two.symbol_table import SYMBOL_TABLES
from fparser.two.utils import walk
from fparser.api import get_reader


@pytest.mark.usefixtures("fake_symbol_table")
def test_type_declaration_stmt():  # R501
    """
    Tests copied from Fortran 2003.

    """
    tcls = Type_Declaration_Stmt
    obj = tcls("integer a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "INTEGER :: a"
    assert (
        repr(obj) == "Type_Declaration_Stmt(Intrinsic_Type_Spec('INTEGER', "
        "None), None, Entity_Decl_List(',', (Entity_Decl(Name('a'), None, "
        "None, None),)))"
    )

    obj = tcls("integer ,dimension(2):: b*3")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "INTEGER, DIMENSION(2) :: b*3"

    obj = tcls("real c")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "REAL :: c"
    assert (
        repr(obj) == "Type_Declaration_Stmt(Intrinsic_Type_Spec('REAL', None), None, "
        "Entity_Decl_List(',', (Entity_Decl(Name('c'), None, None, "
        "None),)))"
    )

    obj = tcls("REAL D( LDA, * ), E( LDB, * )")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "REAL :: D(LDA, *), E(LDB, *)"

    obj = tcls("DOUBLE PRECISION   ALPHA, BETA")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "DOUBLE PRECISION :: ALPHA, BETA"

    obj = tcls("logical,parameter:: T=.true.")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "LOGICAL, PARAMETER :: T = .TRUE."

    obj = tcls("character(n),private:: x(n)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "CHARACTER(LEN = n), PRIVATE :: x(n)"

    obj = tcls("character(lenmax),private:: y(n)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "CHARACTER(LEN = lenmax), PRIVATE :: y(n)"


def test_shadowed_intrinsic(f2008_parser):
    """Check that a locally-defined symbol that shadows (overwrites) a
    Fortran intrinsic is correctly identified."""
    tree = f2008_parser(
        get_reader(
            """\
module my_mod
  use some_mod
  real :: dot_product(2,2)
contains
  subroutine my_sub()
    real :: result
    result = dot_product(1,1)
  end subroutine my_sub
end module my_mod
    """
        )
    )
    tables = SYMBOL_TABLES
    # We should not have an intrinsic-function reference in the parse tree
    assert not walk(tree, Intrinsic_Function_Reference)
    table = tables.lookup("my_mod")
    sym = table.children[0].lookup("dot_product")
    assert sym.primitive_type == "real"
