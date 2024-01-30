# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
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
# Author: R. W. Ford, STFC Daresbury Lab

'''Module containing pytest unit tests for the
GOceanAlgInvoke2PSyCallTrans transformation.

'''
import pytest

from psyclone.domain.common.algorithm import (
    AlgorithmInvokeCall, KernelFunctor)
from psyclone.psyir.nodes import Reference, Literal, ArrayReference
from psyclone.psyir.symbols import RoutineSymbol, DataTypeSymbol, \
    REAL_TYPE, Symbol, DataSymbol, ArrayType, INTEGER_TYPE
from psyclone.domain.gocean.transformations import GOceanAlgInvoke2PSyCallTrans


def test_get_arguments(monkeypatch):
    '''Test the GOceanAlgInvoke2PSyCallTrans get_arguments method.'''

    args_in = [Reference(Symbol("arg0")),
               Literal("1.0", REAL_TYPE),
               ArrayReference.create(DataSymbol(
                   "arg2", ArrayType(
                       REAL_TYPE, [10])), [Literal("1", INTEGER_TYPE)]),
               Reference(Symbol("arg3"))]
    kernel_functor1 = KernelFunctor.create(
        DataTypeSymbol("test1", REAL_TYPE), args_in)

    args_in = [Reference(Symbol("arg3"))]
    kernel_functor2 = KernelFunctor.create(
        DataTypeSymbol("test2", REAL_TYPE), args_in)

    routine = RoutineSymbol("hello")
    index = 0
    invoke = AlgorithmInvokeCall.create(routine, [
        kernel_functor1, kernel_functor2], index)

    trans = GOceanAlgInvoke2PSyCallTrans()
    args_out = trans.get_arguments(invoke)
    assert len(args_out) == 3
    assert args_out[0].name == "arg0"
    assert args_out[1].name == "arg2"
    assert args_out[2].name == "arg3"

    # Check for exception
    monkeypatch.setattr(invoke.children[1], "_children", [None])
    with pytest.raises(TypeError) as info:
        _ = trans.get_arguments(invoke)
    assert ("Expected Algorithm-layer kernel arguments to be a Literal, "
            "Reference or CodeBlock, but found 'NoneType'."
            in str(info.value))
