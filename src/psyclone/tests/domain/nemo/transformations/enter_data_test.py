# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing py.test tests for the transformation of the PSy
   representation of NEMO code using the OpenACC enterdata directive.

'''

import os

from psyclone.psyir.transformations import ACCKernelsTrans
from psyclone.transformations import ACCEnterDataTrans


# Location of the Fortran files associated with these tests
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "../../test_files")

# Test code with explicit NEMO-style do loop
EXPLICIT_DO = ("program explicit_do\n"
               "  REAL :: r\n"
               "  INTEGER :: ji, jj, jk\n"
               "  INTEGER, PARAMETER :: jpi=3, jpj=5, jpk=7\n"
               "  REAL, DIMENSION(jpi, jpj, jpk) :: umask\n"
               "  DO jk = 1, jpk\n"
               "     DO jj = 1, jpj\n"
               "        DO ji = 1, jpi\n"
               "           umask(ji,jj,jk) = ji*jj*jk/r\n"
               "        END DO\n"
               "     END DO\n"
               "  END DO\n"
               "end program explicit_do\n")


def test_apply_to_explicit_loop(fortran_reader, fortran_writer):
    '''
    Check code generation for enclosing a single explicit loop containing a
    kernel inside a data region.

    '''
    psyir = fortran_reader.psyir_from_source(EXPLICIT_DO)
    schedule = psyir.children[0]
    acc_kernels = ACCKernelsTrans()
    acc_kernels.apply(schedule.children)
    acc_trans = ACCEnterDataTrans()
    acc_trans.apply(schedule)
    code = fortran_writer(schedule)
    # Check the enter data directive captures all variables read and written in
    # the loop.
    assert ("  real, dimension(jpi,jpj,jpk) :: umask\n"
            "\n"
            "  !$acc enter data copyin(ji,jj,jk,jpi,jpj,jpk,r,umask)\n"
            "  !$acc kernels\n"
            "  do jk = 1, jpk") in code

    assert ("  enddo\n"
            "  !$acc end kernels\n"
            "\n"
            "end program explicit_do") in code
