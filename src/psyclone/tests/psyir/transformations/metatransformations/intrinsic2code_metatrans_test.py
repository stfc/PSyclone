# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module contains the tests for the Intrinsic2CodeMetaTrans
metatransformation.'''

import pytest
from psyclone.psyir.nodes import IntrinsicCall
from psyclone.psyir.transformations import Intrinsic2CodeMetaTrans


def test_intrinsic2code_trans_validate(fortran_reader):
    '''
    Tests the validate method of the Intrinsic2CodeMetaTrans
    metatransformation.
    '''
    with pytest.raises(TypeError) as err:
        Intrinsic2CodeMetaTrans().validate(123)
    assert ("Input node to Intrinsic2CodeMetaTrans must be an IntrinsicCall "
            "but received 'int'." in str(err.value))


@pytest.mark.parametrize("code, expected", [
    ("j = MAXVAL(i)",
     """  reduction_var = -HUGE(reduction_var)
  do idx = LBOUND(i, dim=1), UBOUND(i, dim=1), 1
    reduction_var = MAX(reduction_var, i(idx))
  enddo
  j = reduction_var"""),
    ("j = MINVAL(i)",
     """  reduction_var = HUGE(reduction_var)
  do idx = LBOUND(i, dim=1), UBOUND(i, dim=1), 1
    reduction_var = MIN(reduction_var, i(idx))
  enddo
  j = reduction_var"""),
    ("j = PRODUCT(i)",
     """  reduction_var = 1
  do idx = LBOUND(i, dim=1), UBOUND(i, dim=1), 1
    reduction_var = reduction_var * i(idx)
  enddo
  j = reduction_var"""),
    ("j = SUM(i)",
     """reduction_var = 0
  do idx = LBOUND(i, dim=1), UBOUND(i, dim=1), 1
    reduction_var = reduction_var + i(idx)
  enddo
  j = reduction_var"""),
    ("j = UBOUND(i)", "j = UBOUND(i)"),
    ])
def test_intrinsic2code_trans_apply(fortran_reader, fortran_writer,
                                    code, expected):
    '''Test the apply function of the Intrinsic2CodeMetaTrans
    metatransformation.
    '''
    code = f"""subroutine test
  integer, dimension(:) :: i
  integer :: j

  {code}

  end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    intrinsic = psyir.walk(IntrinsicCall)[0]
    Intrinsic2CodeMetaTrans().apply(intrinsic)

    out = fortran_writer(psyir)
    print(out)
    correct = f"{expected}"
    assert correct in out
