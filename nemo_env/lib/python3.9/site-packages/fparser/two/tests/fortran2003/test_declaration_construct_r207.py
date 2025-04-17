# Copyright (c) 2019-2021 Science and Technology Facilities Council.

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

"""Test Fortran 2003 rule R207 : This file tests support for the
Declaration_Construct class.

As this class just uses Base to match with a choice of subclasses we
test that an instance of each subclass can be succesfully
parsed. Detailed checking of the subclass rules are performed by the
subclass tests.

"""

import pytest
from fparser.two.Fortran2003 import Declaration_Construct
from fparser.api import get_reader


def test_derived_type_def(f2003_create):
    """Test a derived type definition statement is supported by the
    declaration construct class.

    """
    code = "TYPE :: my_type\n" "END TYPE my_type"
    reader = get_reader(code)
    result = Declaration_Construct(reader)
    assert str(result) == code
    assert "Derived_Type_Def" in repr(result)


def test_entry_stmt(f2003_create):
    """Test an entry statement is supported by the declaration construct
    class.

    """
    code = "ENTRY my_function()"
    reader = get_reader(code)
    result = Declaration_Construct(reader)
    assert str(result) == code
    assert "Entry_Stmt" in repr(result)


def test_enum_def(f2003_create):
    """Test an enum definition is supported by the declaration construct
    class.

    """
    code = "ENUM, BIND(C)\n" "  ENUMERATOR :: a = 1\n" "END ENUM"
    reader = get_reader(code)
    result = Declaration_Construct(reader)
    assert str(result) == code
    assert "Enum_Def" in repr(result)


def test_format_statement(f2003_create):
    """Test a format statement is supported by the declaration construct
    class.

    """
    code = "FORMAT('(x)')"
    reader = get_reader(code)
    result = Declaration_Construct(reader)
    assert str(result) == code
    assert "Format_Stmt" in repr(result)


def test_interface_block(f2003_create):
    """Test an interface block statement is supported by the declaration
    construct class.

    """
    code = "INTERFACE\n" "END INTERFACE"
    reader = get_reader(code)
    result = Declaration_Construct(reader)
    assert str(result) == code
    assert "Interface_Block" in repr(result)


def test_parameter_stmt(f2003_create):
    """Test a parameter statement is supported by the declaration
    construct class.

    """
    code = "PARAMETER(A = 2)"
    reader = get_reader(code)
    result = Declaration_Construct(reader)
    assert str(result) == code
    assert "Parameter_Stmt" in repr(result)


def test_procedure_declaration_stmt(f2003_create):
    """Test a procedure declaration statement is supported by the
    declaration construct class.

    """
    code = "PROCEDURE(REAL) FUNC"
    reader = get_reader(code)
    result = Declaration_Construct(reader)
    assert str(result) == code
    assert "Procedure_Declaration_Stmt" in repr(result)


def test_specification_stmt(f2003_create):
    """Test a specification statement is supported by the declaration
    construct class. An access statement is a specification statement,
    so check for this.

    """
    code = "PUBLIC :: A"
    reader = get_reader(code)
    result = Declaration_Construct(reader)
    assert str(result) == code
    assert "Access_Stmt" in repr(result)


@pytest.mark.usefixtures("fake_symbol_table")
def test_type_declaration_stmt(f2003_create):
    """Test a type declaration statement is supported by the declaration
    construct class.

    """
    code = "INTEGER :: X"
    reader = get_reader(code)
    result = Declaration_Construct(reader)
    assert str(result) == code
    assert "Type_Declaration_Stmt" in repr(result)


@pytest.mark.xfail(reason="#202. Statement Function support removed.")
def test_stmt_function_stmt(f2003_create):
    """Test a statement function statement is supported by the declaration
    construct class.

    """
    code = "C(F) = 5.0*(F - 32.0)/9.0"
    reader = get_reader(code)
    result = Declaration_Construct(reader)
    assert str(result) == code
    assert "Stmt_Function_Stmt" in repr(result)
