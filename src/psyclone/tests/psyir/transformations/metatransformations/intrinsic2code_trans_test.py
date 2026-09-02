# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module contains the tests for the Intrinsic2CodeTrans
metatransformation.'''

import pytest
import logging
from psyclone.psyir.nodes import IntrinsicCall
from psyclone.psyir.transformations import Intrinsic2CodeTrans


LOGGER_NAME = ("psyclone.psyir.transformations.metatransformations."
               "intrinsic2code_trans")


def test_intrinsic2code_trans_validate(fortran_reader):
    '''
    Tests the validate method of the Intrinsic2CodeTrans
    metatransformation.
    '''
    with pytest.raises(TypeError) as err:
        Intrinsic2CodeTrans().validate(123)
    assert ("Input node to Intrinsic2CodeTrans must be an IntrinsicCall "
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
    '''Test the apply function of the Intrinsic2CodeTrans
    metatransformation.
    '''
    code = f"""subroutine test
  integer, dimension(:) :: i
  integer :: j

  {code}

  end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    intrinsic = psyir.walk(IntrinsicCall)[0]
    Intrinsic2CodeTrans().apply(intrinsic)

    out = fortran_writer(psyir)
    correct = f"{expected}"
    assert correct in out


def test_intrinsic2code_trans_apply_logging(fortran_reader, caplog):
    '''Test the apply function of the Intrinsic2CodeTrans metatransformation
    logs a message when an unsupported Intrinsic is found.'''
    code = """subroutine test
    real :: j

    j = log10(j)
    end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    intrinsic = psyir.walk(IntrinsicCall)[0]
    with caplog.at_level(logging.DEBUG, logger=LOGGER_NAME):
        Intrinsic2CodeTrans().apply(intrinsic)
    assert ("Input node was intrinsic of type 'LOG10' which is not "
            "transformed by Intrinsic2CodeTrans. Supported intrinsics are "
            in caplog.text)
