# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''A module to perform pytest tests on the code in the
utils.py file within the psyad directory.

'''
import pytest

from psyclone.psyad.utils import node_is_active, node_is_passive, negate_expr
from psyclone.psyir.nodes import Literal, UnaryOperation, Reference
from psyclone.psyir.symbols import ScalarType, DataSymbol


# node_is_active and node_is_passive functions
def test_active_passive(fortran_reader):
    '''Test that the node_is_active function returns True if an active
    variable exists in the node or its descendants and False if
    not. Also test that the node_is_passive function returns the
    opposite results. Test for active/passive variables provided as a
    list of symbols or as a list of variables names.

    '''
    code = (
        "program test\n"
        "real :: a, b, c\n"
        "a = b\n"
        "end program test\n")
    tl_psyir = fortran_reader.psyir_from_source(code)
    symbol_table = tl_psyir.children[0].symbol_table
    symbol_a = symbol_table.lookup("a")
    symbol_b = symbol_table.lookup("b")
    symbol_c = symbol_table.lookup("c")
    assignment = tl_psyir.children[0][0]

    assert node_is_active(assignment, [symbol_a])
    assert node_is_active(assignment, ["a"])
    assert not node_is_passive(assignment, [symbol_a])
    assert not node_is_passive(assignment, ["a"])
    assert node_is_active(assignment, [symbol_b])
    assert node_is_active(assignment, ["b"])
    assert not node_is_passive(assignment, [symbol_b])
    assert not node_is_passive(assignment, ["b"])
    assert node_is_active(assignment, [symbol_a, symbol_b])
    assert node_is_active(assignment, ["a", "b"])
    assert not node_is_passive(assignment, [symbol_a, symbol_b])
    assert not node_is_passive(assignment, ["a", "b"])
    assert node_is_active(assignment, [symbol_a, symbol_b, symbol_c])
    assert node_is_active(assignment, ["a", "b", "c"])
    assert not node_is_passive(assignment, [symbol_a, symbol_b, symbol_c])
    assert not node_is_passive(assignment, ["a", "b", "c"])

    assert node_is_passive(assignment, [])
    assert node_is_passive(assignment, [])
    assert not node_is_active(assignment, [])
    assert not node_is_active(assignment, [])
    assert node_is_passive(assignment, [symbol_c])
    assert node_is_passive(assignment, ["c"])
    assert not node_is_active(assignment, [symbol_c])
    assert not node_is_active(assignment, ["c"])


def test_active_error():
    '''Test that the node_is_active function raises the expected
    exceptions if the arguments are invalid.

    '''
    with pytest.raises(TypeError) as info:
        node_is_active(None, None)
    assert ("The node argument to the node_is_active() method should be a "
            "PSyIR Node, but found NoneType" in str(info.value))
    node = Reference(DataSymbol("a", ScalarType.integer_type()))
    with pytest.raises(TypeError) as info:
        node_is_active(node, None)
    assert ("The active_variables argument to the node_is_active() method "
            "should be a list, but found NoneType." in str(info.value))
    with pytest.raises(ValueError) as info:
        node_is_active(node, [None])
    assert ("Expected the active_variables argument to the node_is_active() "
            "method to be a list containing either solely PSyIR DataSymbols "
            "or solely strings, but found ['NoneType']." in str(info.value))
    with pytest.raises(ValueError) as info:
        node_is_active(node, [DataSymbol("a", ScalarType.integer_type()), "a"])
    assert ("Expected the active_variables argument to the node_is_active() "
            "method to be a list containing either solely PSyIR DataSymbols "
            "or solely strings, but found ['DataSymbol', 'str']."
            in str(info.value))


def test_negate_expr(fortran_writer):
    '''Test that the negate_expr function negates a PSyIR expression by
    multiplying it by minus one.

    '''
    # positive literal value
    literal = Literal("1", ScalarType.integer_type())
    result = negate_expr(literal)
    assert fortran_writer(result) == "-1"
    # negative literal value
    literal = Literal("-1", ScalarType.integer_type())
    result = negate_expr(literal)
    assert fortran_writer(result) == "1"
    # unary minus
    minus = UnaryOperation.create(
        UnaryOperation.Operator.MINUS, Literal("1", ScalarType.integer_type()))
    result = negate_expr(minus)
    assert isinstance(result, Literal)
    assert fortran_writer(result) == "1"
    # unary plus
    minus = UnaryOperation.create(
        UnaryOperation.Operator.PLUS, Literal("1", ScalarType.integer_type()))
    result = negate_expr(minus)
    # assert isinstance(result, Literal)
    assert fortran_writer(result) == "-1 * (+1)"
    # expression
    expr = Reference(DataSymbol("a", ScalarType.integer_type()))
    result = negate_expr(expr)
    assert fortran_writer(result) == "-1 * a"
