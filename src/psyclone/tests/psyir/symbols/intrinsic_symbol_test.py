# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Perform py.test tests on the psygen.psyir.symbols.intrinsic_symbol file '''

from psyclone.psyir.nodes import IntrinsicCall
from psyclone.psyir.symbols import IntrinsicSymbol


def test_intrinsicsymbol_copy(fortran_reader):
    '''Test the copy function on the IntrinsicSymbol class.
    '''
    # Create an IntrinsicCall
    code = """subroutine x
    integer :: a
    a = INT(1.0)
    end subroutine x"""
    psyir = fortran_reader.psyir_from_source(code)
    intrinsic = psyir.walk(IntrinsicCall)[0]
    assert isinstance(intrinsic.routine.symbol, IntrinsicSymbol)
    isym = intrinsic.routine.symbol
    copy = isym.copy()
    assert copy is not isym
    assert isym.name == copy.name
    assert isym.intrinsic == copy.intrinsic
    assert isym.datatype == copy.datatype
    assert isym.visibility == copy.visibility
    assert isym.interface == copy.interface
    assert isym.is_pure == copy.is_pure
    assert isym.is_elemental == copy.is_elemental
