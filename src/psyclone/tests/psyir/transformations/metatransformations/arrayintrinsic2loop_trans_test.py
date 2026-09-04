# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module contains the tests for the ArrayIntrinsic2LoopTrans
metatransformation.'''

import logging
import pytest
from psyclone.psyir.nodes import IntrinsicCall
from psyclone.psyir.transformations import (
    ArrayIntrinsic2LoopTrans, Maxval2LoopTrans, Minval2LoopTrans,
    Product2LoopTrans, Sum2LoopTrans
)

LOGGER_NAME = ("psyclone.psyir.transformations.metatransformations."
               "intrinsic2code_trans")


@pytest.mark.parametrize("code, transformation", [
    ("j = MAXVAL(i)", Maxval2LoopTrans),
    ("j = MINVAL(i)", Minval2LoopTrans),
    ("j = PRODUCT(i)", Product2LoopTrans),
    ("j = SUM(i)", Sum2LoopTrans),
    ])
def test_arrayintrinsic2loop_trans_apply(fortran_reader, monkeypatch,
                                         code, transformation):
    '''Test the apply function of the ArrayIntrinsic2LoopTrans
    metatransformation.
    '''
    code = f"""subroutine test
  integer, dimension(:) :: i
  integer :: j

  {code}

  end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)

    def error_func(self, *args, **kwargs):
        '''
        Monkeypatched apply function for the used transformation.
        '''
        raise TypeError("Test function usage.")
    monkeypatch.setattr(transformation, "apply", error_func)
    intrinsic = psyir.walk(IntrinsicCall)[0]
    with pytest.raises(TypeError) as err:
        ArrayIntrinsic2LoopTrans().apply(intrinsic)
    assert ("Test function usage." in str(err.value))


def test_arrayintrinsic2loop_trans_apply_logging(fortran_reader, caplog):
    '''
    Test the apply function of the ArrayIntrinsic2LoopTrans
    metatransformation logs a message when an unsupported Intrinsic is found.
    '''
    code = """subroutine test
    real :: j

    j = matmul(j, j)
    end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    intrinsic = psyir.walk(IntrinsicCall)[0]
    with caplog.at_level(logging.DEBUG, logger=LOGGER_NAME):
        ArrayIntrinsic2LoopTrans().apply(intrinsic)
    assert ("Input node was intrinsic of type 'MATMUL' which is not "
            "transformed by ArrayIntrinsic2LoopTrans. Supported intrinsics "
            "are " in caplog.text)
