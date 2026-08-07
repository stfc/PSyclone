# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Performs pytest tests on PSyIR Fortran Backend for CommonBlocks '''

from psyclone.psyir.nodes import Routine
from psyclone.tests.utilities import Compile


def test_fw_common_blocks(fortran_reader, fortran_writer, tmpdir):
    '''Test that declarations with common blocks are maintained in the
    generated Fortran.

    '''
    # Generate PSyIR from Fortran code.
    code = (
        "module test\n"
        "  contains\n"
        "  subroutine sub()\n"
        "    integer :: a, b, c\n"
        "    real :: d, e, f\n"
        "    common /name1/ a, b\n"
        "    common /name1/ c /name2/ d\n"
        "    common e, f\n"
        "  end subroutine sub\n"
        "end module test\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]

    assert routine.symbol_table.lookup("a").is_commonblock  # Sanity check

    code = fortran_writer(routine)
    assert code == (
        "subroutine sub()\n"
        "  integer :: a\n"
        "  integer :: b\n"
        "  integer :: c\n"
        "  real :: d\n"
        "  real :: e\n"
        "  real :: f\n"
        "  COMMON /name1/ a, b\n"
        "  COMMON /name1/ c /name2/ d\n"
        "  COMMON // e, f\n\n\n"
        "end subroutine sub\n")
    assert Compile(tmpdir).string_compiles(fortran_writer(psyir))
