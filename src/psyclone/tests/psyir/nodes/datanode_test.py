# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Performs pytest tests on the PSyIR DataNode.

'''
import pytest

from psyclone.psyir.nodes import (
    DataNode, Reference, BinaryOperation, Loop, Call, ArrayReference, Range)
from psyclone.psyir.symbols import (
    DataSymbol, UnresolvedType, RoutineSymbol, ScalarType)


def test_datanode_datatype():
    '''
    Test that the base implementation of datatype returns UnresolvedType,
    unless it is in a place where we can infer the type from its context.

    '''
    dnode = DataNode()
    assert isinstance(dnode.datatype, UnresolvedType)

    # If it is a Loop we can infer the type of the bounds
    loop = Loop.create(
                    DataSymbol("a", ScalarType.integer_single_type()),
                    Call.create(RoutineSymbol("get_start")),
                    Call.create(RoutineSymbol("get_stop")),
                    Call.create(RoutineSymbol("get_step")),
                    []
    )

    assert loop.start_expr.datatype == ScalarType.integer_type()
    assert loop.stop_expr.datatype == ScalarType.integer_type()
    assert loop.step_expr.datatype == ScalarType.integer_type()

    # If it is a Range we can infer the type of the bounds
    ref1 = Reference(DataSymbol("i", UnresolvedType()))
    ref2 = Reference(DataSymbol("lb", UnresolvedType()))
    ref3 = Reference(DataSymbol("ub", UnresolvedType()))
    arrayref = ArrayReference.create(
        DataSymbol("a", UnresolvedType()),
        indices=[ref1, Range.create(ref2, ref3, None)]
    )

    # We cannot infer the type of an index because it could be an
    # integer, or an array of integers.
    assert arrayref.indices[0].datatype == UnresolvedType()
    # But the Range bounds can only be integers
    assert (arrayref.indices[1].children[0].datatype ==
            ScalarType.integer_type())
    assert (arrayref.indices[1].children[1].datatype ==
            ScalarType.integer_type())


def test_datanode_is_character():
    '''Test that character expressions are marked correctly.
    '''
    reference = Reference(DataSymbol("char", ScalarType.character_type()))
    assert reference.is_character()

    reference = Reference(DataSymbol("int", ScalarType.integer_single_type()))
    reference2 = Reference(
        DataSymbol("int2", ScalarType.integer_single_type()))
    bop = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                 reference, reference2)
    assert not reference.is_character()
    assert not reference2.is_character()
    assert not bop.is_character()

    reference = Reference(DataSymbol("real", ScalarType.real_type()))
    assert not reference.is_character()

    reference = Reference(DataSymbol("unknown", UnresolvedType()))
    with pytest.raises(ValueError) as excinfo:
        _ = reference.is_character()
    assert ("is_character could not resolve whether the expression 'unknown'"
            " operates on characters." in str(excinfo.value))
    reference = Reference(DataSymbol("unknown", UnresolvedType()))
    assert not reference.is_character(unknown_as=False)
    assert reference.is_character(unknown_as=True)
