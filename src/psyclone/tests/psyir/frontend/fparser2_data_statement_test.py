# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2024-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Tests Fortran data statements in the fparser2 PSyIR front-end. Data
statements are represented as an UnsupportedFortranType, but any variable
in a data statement should get a StaticInterface. The tests
here cover a few variations of the data statement, in case that at
some stage we need more information.
'''

import pytest
from psyclone.psyir.nodes import Routine
from psyclone.psyir.symbols import (DataSymbol, StaticInterface,
                                    UnresolvedType, UnsupportedFortranType)


def test_simple_data_statement(fortran_reader):
    ''' Test that data statements are correctly captured. '''

    psyir = fortran_reader.psyir_from_source('''
        subroutine test()
            implicit none
            integer :: a, b, c
            data a/1/, b/2/, c/3/
        end subroutine test
      ''')

    routine = psyir.walk(Routine)[0]
    symtab = routine.symbol_table
    assert isinstance(symtab.lookup("a").interface, StaticInterface)
    assert isinstance(symtab.lookup("b").interface, StaticInterface)
    assert isinstance(symtab.lookup("c").interface, StaticInterface)

    # There is a data_stmt in the symbol table:
    data = symtab.lookup("_PSYCLONE_INTERNAL_DATA_STMT")
    assert isinstance(data.datatype, UnsupportedFortranType)
    assert data.datatype.declaration == "DATA a / 1 /, b / 2 /, c / 3 /"

    # Test with a single data statement
    psyir = fortran_reader.psyir_from_source('''
        subroutine test()
            implicit none
            integer :: a
            data a/1/
        end subroutine test
      ''')

    routine = psyir.walk(Routine)[0]
    symtab = routine.symbol_table

    # There is a data_stmt in the symbol table:
    data = symtab.lookup("_PSYCLONE_INTERNAL_DATA_STMT")
    assert isinstance(data.datatype, UnsupportedFortranType)
    assert data.datatype.declaration == "DATA a / 1 /"


def test_multiple_data_statement(fortran_reader):
    ''' Test that multiple data statements are correctly captured. '''

    # Test with multiple initialisations
    psyir = fortran_reader.psyir_from_source('''
        subroutine test()
            implicit none
            integer :: a, b, c(5), i
            data a/1/, b/2/
            data (c(i), i=1,5) / 5, 4, 3, 2, 1/
        end subroutine test
      ''')

    routine = psyir.walk(Routine)[0]
    symtab = routine.symbol_table
    assert isinstance(symtab.lookup("a").interface, StaticInterface)
    assert isinstance(symtab.lookup("b").interface, StaticInterface)
    assert isinstance(symtab.lookup("c").interface, StaticInterface)

    # There is a data_stmt in the symbol table:
    data = symtab.lookup("_PSYCLONE_INTERNAL_DATA_STMT")
    assert isinstance(data.datatype, UnsupportedFortranType)
    assert data.datatype.declaration == "DATA a / 1 /, b / 2 /"


@pytest.mark.usefixtures("f2008_parser")
def test_data_statement_implicit_loop(fortran_reader):
    ''' Test that a data statement with an implicit loop is correctly
    captured. '''

    # Test with a single data statement with an implicit loop
    psyir = fortran_reader.psyir_from_source('''
        subroutine test()
            implicit none
            integer :: c(5), i
            data (c(i), i=1,5) / 5, 4, 3, 2, 1/
        end subroutine test
      ''')
    routine = psyir.walk(Routine)[0]
    symtab = routine.symbol_table

    assert isinstance(symtab.lookup("c").interface, StaticInterface)
    # There is a data_stmt in the symbol table:
    data = symtab.lookup("_PSYCLONE_INTERNAL_DATA_STMT")
    assert isinstance(data.datatype, UnsupportedFortranType)
    assert (data.datatype.declaration ==
            "DATA (c(i), i = 1, 5) / 5, 4, 3, 2, 1 /")


def test_data_statement_nested_implicit_loop(fortran_reader):
    ''' Test that a data statement with an implicit loop is correctly
    captured. '''

    # Test with a single data statement with an implicit loop
    psyir = fortran_reader.psyir_from_source('''
        subroutine test()
            implicit none
            integer :: c(5, 5), i, j
            data ((c(i,j), i=j+1, 5), j=1, 5) / 10 * 1.0 /
        end subroutine test
      ''')
    routine = psyir.walk(Routine)[0]
    symtab = routine.symbol_table

    assert isinstance(symtab.lookup("c").interface, StaticInterface)
    # There is a data_stmt in the symbol table:
    data = symtab.lookup("_PSYCLONE_INTERNAL_DATA_STMT")
    assert isinstance(data.datatype, UnsupportedFortranType)
    assert (data.datatype.declaration ==
            "DATA ((c(i, j), i = j + 1, 5), j = 1, 5) / 10 * 1.0 /")


def test_data_statement_derived_type(fortran_reader):
    ''' Test that a data statement with a derived type work
    as expected. '''

    # Test a derived type, initialised with a derived type:
    psyir = fortran_reader.psyir_from_source('''
        subroutine test()
            implicit none
            type :: mytype
               integer :: a, b
            end type mytype
            type(mytype) :: t
            data t / mytype(1, 2) /
        end subroutine test
      ''')
    routine = psyir.walk(Routine)[0]
    symtab = routine.symbol_table

    assert isinstance(symtab.lookup("t").interface, StaticInterface)
    # There is a data_stmt in the symbol table:
    data = symtab.lookup("_PSYCLONE_INTERNAL_DATA_STMT")
    assert isinstance(data.datatype, UnsupportedFortranType)
    assert data.datatype.declaration == "DATA t / mytype(1, 2) /"

    # Test a derived type initialised component-wise
    psyir = fortran_reader.psyir_from_source('''
        subroutine test()
            implicit none
            type :: mytype
               integer :: a, b
            end type mytype
            type(mytype) :: t
            data t%a, t%b / 1, 2 /
        end subroutine test
      ''')
    routine = psyir.walk(Routine)[0]
    symtab = routine.symbol_table

    assert isinstance(symtab.lookup("t").interface, StaticInterface)
    # There is a data_stmt in the symbol table:
    data = symtab.lookup("_PSYCLONE_INTERNAL_DATA_STMT")
    assert isinstance(data.datatype, UnsupportedFortranType)
    assert data.datatype.declaration == "DATA t % a, t % b / 1, 2 /"


def test_data_statement_undeclared_var(fortran_reader):
    ''' Test that a data statement for a variable that is otherwise
    not declared works as expected. '''

    psyir = fortran_reader.psyir_from_source('''
        subroutine test()
            data a / 1 /
        end subroutine test
      ''')
    routine = psyir.walk(Routine)[0]
    symtab = routine.symbol_table

    # The code should create an UnresolvedType with a static interface:
    sym_a = symtab.lookup("a")
    assert isinstance(sym_a, DataSymbol)
    assert isinstance(sym_a.datatype, UnresolvedType)
    assert isinstance(sym_a.interface, StaticInterface)

    # Check that the data statement is included as well:
    data = symtab.lookup("_PSYCLONE_INTERNAL_DATA_STMT")
    assert isinstance(data.datatype, UnsupportedFortranType)
    assert data.datatype.declaration == "DATA a / 1 /"
