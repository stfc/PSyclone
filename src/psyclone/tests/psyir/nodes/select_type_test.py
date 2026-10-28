# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Module containing pytest tests for the SelectType node. '''

import pytest

from psyclone.core import AccessType
from psyclone.errors import GenerationError
from psyclone.psyir.nodes import SelectType, Reference
from psyclone.psyir.symbols import DataSymbol, ScalarType


def test_select_type_create():
    '''Tests the create method of the SelectType class.'''
    with pytest.raises(
        GenerationError, match="istypes argument in create method of "
                               "SelectType class should be a list but found "
                               "'int'."):
        _ = SelectType.create(None, 123)

    sym = DataSymbol("a", ScalarType.integer_type())
    ref = Reference(sym)
    selt = SelectType.create(ref, [])

    assert len(selt.children) == 1
    assert selt.children[0] is ref


def test_select_type_str(fortran_reader):
    '''Tests the __str__ method of the SelectType class.'''

    code = """subroutine test(a)
    class(*), allocatable :: a
    integer :: i, j

    select type(a)
        type is(integer)
            i = 1
    end select
    end subroutine"""

    psyir = fortran_reader.psyir_from_source(code)
    selt = psyir.walk(SelectType)[0]
    correct = """SelectType[Reference[name:'a']]
TypeGuard[type is][Scalar<INTEGER, UNDEFINED>]
Schedule:
Assignment[]
Reference[name:'i']Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
End Schedule
End TypeGuard
End SelectType"""
    assert correct == str(selt)


def test_select_type_reference_accesses(fortran_reader):
    '''Tests the reference_accesses method of the SelectType class.'''
    code = """subroutine test(a)
    class(*), allocatable :: a
    integer :: i, j

    select type(a)
        type is(integer)
            i = 1
        type is(real)
            j = i + 1
    end select
    end subroutine"""

    psyir = fortran_reader.psyir_from_source(code)

    selt = psyir.walk(SelectType)[0]
    refs = psyir.walk(Reference)
    a_sig, _ = refs[0].get_signature_and_indices()
    i_sig, _ = refs[1].get_signature_and_indices()
    j_sig, _ = refs[2].get_signature_and_indices()
    ref_accesses = selt.reference_accesses()
    assert len(ref_accesses) == 3
    assert ref_accesses[a_sig][0].access_type == AccessType.INQUIRY
    assert ref_accesses[i_sig].is_read()
    assert ref_accesses[i_sig].is_written()
    assert not ref_accesses[j_sig].is_read()
    assert ref_accesses[j_sig].is_written()
