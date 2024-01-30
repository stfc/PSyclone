# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Author R. W. Ford, STFC Daresbury Lab
# Modified by S. Siso, STFC Daresbury Lab

'''Module containing tests for the translation of PSyIR to PSyclone
Algorithm PSyIR.

'''
from __future__ import absolute_import
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
    assert len(psyir.walk(Call)) == 4
    assert len(psyir.walk(AlgorithmInvokeCall)) == 0
    assert len(psyir.walk(KernelFunctor)) == 0

    alg_trans.apply(psyir)

    assert len(psyir.walk(Call)) == 4
    assert len(psyir.walk(AlgorithmInvokeCall)) == 3
    assert len(psyir.walk(KernelFunctor)) == 3

    for idx, invoke_call in enumerate(psyir.walk(AlgorithmInvokeCall)):
        assert invoke_call._index == idx
