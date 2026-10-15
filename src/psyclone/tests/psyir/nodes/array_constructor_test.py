# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Performs py.test tests on the ArrayConstructor PSyIR node. '''

import pytest
from psyclone.errors import GenerationError
from psyclone.psyir.nodes import (
    ArrayConstructor, Literal, BinaryOperation, Assignment,
    Reference, IfBlock)
from psyclone.core import AccessType, Signature
from psyclone.psyir.symbols import ScalarType, DataSymbol
from psyclone.utils import colored


def test_array_construction_valid():
    '''Test array construction and addition of children.'''
    expr1 = Literal("1", ScalarType.integer_type())
    expr2 = Literal("2", ScalarType.integer_type())
    array_cons = ArrayConstructor.create([expr1, expr2])
    expr3 = Literal("3", ScalarType.integer_type())
    expr4 = Literal("4", ScalarType.integer_type())
    expr5 = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                   expr3, expr4)
    array_cons.children.append(expr5)
    assert isinstance(array_cons.children[0], Literal)
    assert isinstance(array_cons.children[1], Literal)
    assert isinstance(array_cons.children[2], BinaryOperation)


def test_array_construction_invalid():
    '''Test invalid array construction.'''
    # Construct an IfBlock
    if_condition = Literal('true', ScalarType.boolean_type())
    if_body = [Assignment.create(
        Reference(DataSymbol("tmp", ScalarType.real_single_type())),
        Literal("10", ScalarType.integer_type()))]
    ifblock = IfBlock.create(if_condition, if_body)
    # Check that IfBlock cannot be an element of an ArrayConstructor
    with pytest.raises(GenerationError) as err:
        ArrayConstructor.create([ifblock])
    assert ("Generation Error: Item 'IfBlock' can't be child 0 of "
            "'ArrayConstructor'. The valid format is: '[DataNode]*"
            in str(err.value))


def test_array_construction_reference_accesses():
    '''Test the reference_accesses() method of an array constructor'''
    ref = Reference(DataSymbol("tmp", ScalarType.integer_type()))
    arr = ArrayConstructor.create([ref])
    accs = arr.reference_accesses()
    access_seq = accs[Signature("tmp")]
    assert len(access_seq) == 1
    assert access_seq[0].access_type is AccessType.READ


def test_array_constructor_node_str():
    ''' Check the node_str method of the ArrayConstructor class.'''
    lit = Literal("1", ScalarType.integer_single_type())
    array_cons = ArrayConstructor.create([lit])
    coloured_array_cons = colored("ArrayConstructor", ArrayConstructor._colour)
    assert f"{coloured_array_cons}[]" == array_cons.node_str()
