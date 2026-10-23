# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Performs pytest tests on PSyIR Fortran Backend for CommonBlocks '''

import pytest

from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.nodes import Routine
from psyclone.psyir.symbols import (
    CommonBlockInterface, DataSymbol, ScalarType)
from psyclone.tests.utilities import Compile


def test_fw_common_blocks(fortran_reader, fortran_writer, tmpdir):
    '''Test that declarations with common blocks are maintained in the
    generated Fortran. Note that some declarations are purposely out of order
    but the common block symbol sequence should remain the same.

    '''
    # Generate PSyIR from Fortran code.
    code = (
        "module test\n"
        "  contains\n"
        "  subroutine sub()\n"
        "    integer :: c, b, a\n"
        "    real :: d, e, f\n"
        "    common /name1/ a, b\n"
        "    common /name1/ c /name2/ d\n"
        "    common e, f\n"
        "  end subroutine sub\n"
        "end module test\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]

    assert routine.symbol_table.lookup("a").is_commonblock  # Sanity check
    assert routine.symbol_table.lookup("d").is_commonblock  # Sanity check
    assert routine.symbol_table.lookup("e").is_commonblock  # Sanity check

    code = fortran_writer(routine)
    assert code == (
        "subroutine sub()\n"
        "  integer :: c\n"
        "  integer :: b\n"
        "  integer :: a\n"
        "  real :: d\n"
        "  real :: e\n"
        "  real :: f\n"
        "  common /name1/ a, b, c\n"
        "  common /name2/ d\n"
        "  common // e, f\n\n\n"
        "end subroutine sub\n")
    assert Compile(tmpdir).string_compiles(fortran_writer(psyir))


def test_fw_common_block_duplicate_positions(fortran_writer):
    '''Test that the backend is case-insensitibe to common block names and
    duplicate positions within one common block are rejected.'''
    routine = Routine.create("sub")
    routine.symbol_table.add(DataSymbol(
        "var1", ScalarType.integer_type(),
        interface=CommonBlockInterface("somegroup", 0)))
    routine.symbol_table.add(DataSymbol(
        "var2", ScalarType.integer_type(),
        interface=CommonBlockInterface("SOMEGROUP", 0)))

    with pytest.raises(VisitorError) as err:
        _ = fortran_writer(routine)

    assert ("Common block 'somegroup' has Symbols with duplicate positions: "
            "[0, 0]." in str(err.value))
