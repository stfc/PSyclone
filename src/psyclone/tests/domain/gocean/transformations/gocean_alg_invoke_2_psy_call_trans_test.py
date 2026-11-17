# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing pytest unit tests for the
GOceanAlgInvoke2PSyCallTrans transformation.

'''
import pytest

from psyclone.domain.common.algorithm import (
    AlgorithmInvokeCall, KernelFunctor)
from psyclone.psyir.nodes import Reference, Literal, ArrayReference
from psyclone.psyir.symbols import RoutineSymbol, DataTypeSymbol, \
    ScalarType, Symbol, DataSymbol, ArrayType
from psyclone.domain.gocean.transformations import GOceanAlgInvoke2PSyCallTrans


def test_get_arguments(monkeypatch):
    '''Test the GOceanAlgInvoke2PSyCallTrans get_arguments method.'''

    args_in = [Reference(Symbol("arg0")),
               Literal("1.0", ScalarType.real_type()),
               ArrayReference.create(DataSymbol(
                   "arg2", ArrayType(
                       ScalarType.real_type(), [10])),
                       [Literal("1", ScalarType.integer_type())]),
               Reference(Symbol("arg3"))]
    kernel_functor1 = KernelFunctor.create(
        DataTypeSymbol("test1", ScalarType.real_type()), args_in)

    args_in = [Reference(Symbol("arg3"))]
    kernel_functor2 = KernelFunctor.create(
        DataTypeSymbol("test2", ScalarType.real_type()), args_in)

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
