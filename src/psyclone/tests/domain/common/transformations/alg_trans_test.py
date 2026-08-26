# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing tests for the translation of PSyIR to PSyclone
Algorithm PSyIR.

'''
import pytest

from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.nodes import Call

from psyclone.domain.common.algorithm import AlgorithmInvokeCall, \
    KernelFunctor
from psyclone.domain.common.transformations import (RaisePSyIR2AlgTrans,
                                                    AlgTrans)


def test_init():
    '''Check that an InvokeCallTrans instance can be created correctly,
    has the expected defaults, deals with any __init__ arguments and
    its name method returns the expected value.

    '''
    alg_trans = AlgTrans()
    assert alg_trans.name == "AlgTrans"
    assert isinstance(alg_trans, AlgTrans)
    assert isinstance(alg_trans._invoke_trans, RaisePSyIR2AlgTrans)


def test_validate_node_error(fortran_reader):
    '''Test that the validate method raises the expected exception if an
    invalid node argument is provided, or the node is not the root
    node. Also test that the apply method calls the validate method.

    '''
    code = (
        "module alg_mod\n"
        "  contains\n"
        "  subroutine alg()\n"
        "    use kern_mod\n"
        "    call invoke(kern(1.0))\n"
        "  end subroutine alg\n"
        "end module alg_mod\n")

    psyir = fortran_reader.psyir_from_source(code)

    alg_trans = AlgTrans()
    with pytest.raises(TransformationError) as info:
        alg_trans.validate("hello")
    assert ("The supplied call argument should be a Routine or Container "
            "node but found 'str'." in str(info.value))

    with pytest.raises(TransformationError) as info:
        alg_trans.apply("hello")
    assert ("The supplied call argument should be a Routine or Container "
            "node but found 'str'." in str(info.value))

    with pytest.raises(TransformationError) as info:
        alg_trans.validate(psyir.children[0])
    assert ("The supplied node should be the root of a PSyIR tree but this "
            "node has a parent." in str(info.value))

    alg_trans.validate(psyir)


def test_apply(fortran_reader):
    '''Test that the apply method behaves as expected.

    '''
    code = (
        "module alg_mod\n"
        "  use kern_mod\n"
        "  contains\n"
        "  subroutine alg1()\n"
        "    call invoke(kern(1.0))\n"
        "    call not_invoke(kern(1.0))\n"
        "    call invoke(kern(1.0))\n"
        "  end subroutine alg1\n"
        "  subroutine alg2()\n"
        "    call invoke(kern(1.0))\n"
        "  end subroutine alg2\n"
        "end module alg_mod\n")

    psyir = fortran_reader.psyir_from_source(code)
    alg_trans = AlgTrans()
    assert len(psyir.walk(Call)) == 8
    assert len(psyir.walk(AlgorithmInvokeCall)) == 0
    assert len(psyir.walk(KernelFunctor)) == 0

    alg_trans.apply(psyir)

    # The 3 invokes + the not_invoke + the kern inside not_invoke
    assert len(psyir.walk(Call)) == 5
    assert len(psyir.walk(AlgorithmInvokeCall)) == 3
    assert len(psyir.walk(KernelFunctor)) == 3

    for idx, invoke_call in enumerate(psyir.walk(AlgorithmInvokeCall)):
        assert invoke_call._index == idx
