# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing tests for the translation of PSyIR to LFRic
Algorithm PSyIR.

'''
from psyclone.psyir.nodes import Call
from psyclone.domain.lfric.algorithm.psyir import (
    LFRicAlgorithmInvokeCall, LFRicKernelFunctor, LFRicBuiltinFunctor)
from psyclone.domain.lfric.transformations import (
    LFRicAlgTrans, RaisePSyIR2LFRicAlgTrans)


def test_init():
    '''Check that an LFRicAlgTrans instance can be created correctly, has
    the expected defaults, deals with any __init__ arguments and its
    name method returns the expected value.

    '''
    alg_trans = LFRicAlgTrans()
    assert alg_trans.name == "LFRicAlgTrans"
    assert isinstance(alg_trans, LFRicAlgTrans)
    assert isinstance(alg_trans._invoke_trans, RaisePSyIR2LFRicAlgTrans)


def test_apply(fortran_reader):
    '''Test that the apply method behaves as expected.

    '''
    code = (
        "module alg_mod\n"
        "  use kern_mod\n"
        "  use field_mod, only : field\n"
        "  type(field) :: field1\n"
        "  contains\n"
        "  subroutine alg1()\n"
        "    call invoke(kern(1.0))\n"
        "    call not_invoke(kern(1.0))\n"
        "    call invoke(setval_c(field1,1.0))\n"
        "  end subroutine alg1\n"
        "  subroutine alg2()\n"
        "    call invoke(kern(1.0))\n"
        "  end subroutine alg2\n"
        "end module alg_mod\n")

    psyir = fortran_reader.psyir_from_source(code)
    alg_trans = LFRicAlgTrans()
    assert len(psyir.walk(Call)) == 8
    assert len(psyir.walk(LFRicAlgorithmInvokeCall)) == 0
    assert len(psyir.walk(LFRicKernelFunctor)) == 0
    assert len(psyir.walk(LFRicBuiltinFunctor)) == 0

    alg_trans.apply(psyir)

    # The 3 invokes + the not_invoke + the kern inside not_invoke
    assert len(psyir.walk(Call)) == 5
    assert len(psyir.walk(LFRicAlgorithmInvokeCall)) == 3
    assert len(psyir.walk(LFRicKernelFunctor)) == 2
    assert len(psyir.walk(LFRicBuiltinFunctor)) == 1
