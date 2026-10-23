# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the tests for the OpenMP Critical
transformation.'''

from psyclone.psyir.nodes import OMPCriticalDirective, Routine
from psyclone.psyir.transformations import OMPCriticalTrans


def test_omp_critical_apply(fortran_reader, fortran_writer):
    '''Test the apply function of the OMPCriticalTrans.'''
    ctrans = OMPCriticalTrans()

    code = """subroutine x
    integer :: i, j

    i = 2
    j = 3
    end subroutine x"""

    psyir = fortran_reader.psyir_from_source(code)

    routine = psyir.walk(Routine)[0]

    ctrans.apply(routine.children[:])
    assert isinstance(routine.children[0], OMPCriticalDirective)

    output = fortran_writer(psyir)
    correct = """subroutine x()
  integer :: i
  integer :: j

  !$omp critical
  i = 2
  j = 3
  !$omp end critical

end subroutine x"""
    assert correct in output
