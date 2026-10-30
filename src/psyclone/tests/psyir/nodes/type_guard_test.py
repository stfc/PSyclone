# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Module containing pytest tests for the TypeGuard node. '''

import pytest

from psyclone.errors import GenerationError, InternalError
from psyclone.psyir.nodes import Schedule, TypeGuard
from psyclone.psyir.symbols import (DataTypeSymbol, SymbolTable,
                                    UnresolvedType, UnresolvedInterface)


def test_typeguard_init():
    '''Tests the constructor and getters/setters of a TypeGuard.'''
    typg = TypeGuard(TypeGuard.TypeGuardType.CLASS_IS, None)

    assert typg.comparison_type is None
    assert typg.type_guard_type == TypeGuard.TypeGuardType.CLASS_IS

    with pytest.raises(
        TypeError, match="A TypeGuard needs a DataTypeSymbol, DataType "
                         "or None as the comparison type but '123' was "
                         "provided"):
        typg.comparison_type = 123

    with pytest.raises(
        TypeError, match="A TypeGuard needs a TypeGuardType as the "
                         "type_guard_type but '123' was provided."):
        typg.type_guard_type = 123

    with pytest.raises(
        InternalError, match="TypeGuard malformed or incomplete. It should "
                             "have 1 child but found 0."):
        _ = typg.body


def test_typeguard_create():
    '''Tests the create method of the TypeGuard class.'''

    with pytest.raises(
        GenerationError, match="children argument in create method of "
                               "TypeGuard class should be a list but found "
                               "'int'."):
        _ = TypeGuard.create(None, None, 123)

    typg = TypeGuard.create(TypeGuard.TypeGuardType.CLASS_DEFAULT,
                            None, [])

    assert typg.comparison_type is None
    assert typg.type_guard_type == TypeGuard.TypeGuardType.CLASS_DEFAULT
    assert isinstance(typg.children[0], Schedule)
    assert len(typg.children[0].children) == 0


def test_typeguard_str():
    ''' Tests the __str__ and begin_str methods of the TypeGuard class.'''

    typg = TypeGuard.create(TypeGuard.TypeGuardType.CLASS_DEFAULT,
                            None, [])

    correct = """TypeGuard[class default][]
Schedule:
End Schedule
End TypeGuard"""
    assert correct == str(typg)

    assert typg.begin_str() == "class default"


def test_typeguard_reference_accesses(fortran_reader):
    ''' Tests the reference accesses method of the TypeGuard class.'''
    code = """subroutine test(a)
    class(*), allocatable :: a
    integer :: i, j

    select type(a)
        type is(integer)
            i = 1
            j = i + 1
    end select
    end subroutine"""

    psyir = fortran_reader.psyir_from_source(code)

    typeguard = psyir.walk(TypeGuard)[0]
    i_sig, _ = (typeguard.children[0].children[0].lhs.
                get_signature_and_indices())
    j_sig, _ = (typeguard.children[0].children[1].lhs.
                get_signature_and_indices())

    ref_accesses = typeguard.reference_accesses()
    assert len(ref_accesses) == 2
    assert ref_accesses[i_sig].is_read()
    assert ref_accesses[i_sig].is_written()
    assert not ref_accesses[j_sig].is_read()
    assert ref_accesses[j_sig].is_written()


@pytest.mark.parametrize("table", [None, SymbolTable()])
def test_type_guard_replace_symbols_using(fortran_reader, table):
    '''Test the replace_symbols_using() method of TypeGuard.'''

    code = """subroutine test(a)
    use some_mod, only: test_type
    class(*), allocatable :: a

    select type(a)
        type is(test_type)
            i = 1
            j = i + 1
    end select
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    type_guard = psyir.walk(TypeGuard)[0]
    assert type_guard.comparison_type.name == "test_type"
    # Create a replacement symbol.
    new_tt = DataTypeSymbol("test_type", datatype=UnresolvedType(),
                            interface=UnresolvedInterface())
    assert type_guard.comparison_type is not new_tt
    if table is not None:
        # Test nothing bad happens if no replacement is in the table.
        type_guard.replace_symbols_using(table)
        assert type_guard.comparison_type is not new_tt
        table.add(new_tt)
        type_guard.replace_symbols_using(table)
    else:
        type_guard.replace_symbols_using(new_tt)
    # Comparison Type should have been updated.
    assert type_guard.comparison_type is new_tt


def test_type_guard_eq(fortran_reader):
    '''Test the equality functionality of TypeGuard.'''
    code = """subroutine test(a)

    use some_mod, only: test_type, tt2
    class(*), allocatable :: a

    select type(a)
        type is(test_type)
        type is(test_type)
        type is(tt2)
        class is(test_type)
        class default
    end select
    end subroutine"""

    psyir = fortran_reader.psyir_from_source(code)
    type_guards = psyir.walk(TypeGuard)

    # Only the first two type guards should be equal.
    assert type_guards[0] == type_guards[1]
    assert type_guards[0] != type_guards[2]
    assert type_guards[0] != type_guards[3]
    assert type_guards[0] != type_guards[4]
