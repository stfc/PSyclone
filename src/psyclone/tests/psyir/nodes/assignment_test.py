# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2022, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' Performs py.test tests on the Assignment PSyIR node. '''

import pytest
from psyclone.errors import InternalError, GenerationError
from psyclone.f2pygen import ModuleGen
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import Assignment, Reference, Literal, \
    ArrayReference, Range, BinaryOperation, StructureReference, \
    ArrayOfStructuresReference, UnaryOperation, IntrinsicCall
from psyclone.psyir.nodes.node import colored
from psyclone.psyir.symbols import DataSymbol, REAL_SINGLE_TYPE, Symbol, \
    INTEGER_SINGLE_TYPE, REAL_TYPE, ArrayType, INTEGER_TYPE, StructureType, \
    DataTypeSymbol
from psyclone.tests.utilities import check_links


def test_assignment_node_str():
    ''' Check the node_str method of the Assignment class.'''
    assignment = Assignment()
    coloredtext = colored("Assignment", Assignment._colour)
    assert coloredtext+"[]" in assignment.node_str()


def test_assignment_can_be_printed():
    '''Test that an Assignment instance can always be printed (i.e. is
    initialised fully)'''
    assignment = Assignment()
    assert "Assignment[]\n" in str(assignment)


def test_assignment_semantic_navigation():
    '''Test that the Assignment navigation properties reference the expected
    children'''
    assignment = Assignment()

    # lhs should fail if first child is not present
    with pytest.raises(InternalError) as err:
        _ = assignment.lhs
    assert "' malformed or incomplete. It needs at least 1 child to have " \
        "a lhs." in str(err.value)

    ref = Reference(DataSymbol("a", REAL_SINGLE_TYPE), parent=assignment)
    assignment.addchild(ref)

    # rhs should fail if second child is not present
    with pytest.raises(InternalError) as err:
        _ = assignment.rhs
    assert " malformed or incomplete. It needs at least 2 children to have " \
        "a rhs." in str(err.value)

    lit = Literal("1", INTEGER_SINGLE_TYPE, assignment)
    assignment.addchild(lit)
    assert assignment.lhs is assignment._children[0]
    assert assignment.rhs is assignment._children[1]


def test_assignment_create():
    '''Test that the create method in the Assignment class correctly
    creates an Assignment instance.

    '''
    lhs = Reference(DataSymbol("tmp", REAL_SINGLE_TYPE))
    rhs = Literal("0.0", REAL_SINGLE_TYPE)
    assignment = Assignment.create(lhs, rhs)
    check_links(assignment, [lhs, rhs])
    result = FortranWriter().assignment_node(assignment)
    assert result == "tmp = 0.0\n"


def test_assignment_children_validation():
    '''Test that children added to assignment are validated. Assignment
    accepts just 2 children and both are DataNodes.

    '''
    # Create method with lhs not a Node.
    with pytest.raises(GenerationError) as excinfo:
        _ = Assignment.create("invalid", Literal("0.0", REAL_SINGLE_TYPE))
    assert ("Item 'str' can't be child 0 of 'Assignment'. The valid format "
            "is: 'DataNode, DataNode'.") in str(excinfo.value)

    # Create method with rhs not a Node.
    with pytest.raises(GenerationError) as excinfo:
        _ = Assignment.create(Reference(DataSymbol("tmp", REAL_SINGLE_TYPE)),
                              "invalid")
    assert ("Item 'str' can't be child 1 of 'Assignment'. The valid format "
            "is: 'DataNode, DataNode'.") in str(excinfo.value)

    # Direct assignment of a 3rd children
    assignment = Assignment.create(
        Reference(DataSymbol("tmp", REAL_SINGLE_TYPE)),
        Literal("0.0", REAL_SINGLE_TYPE))
    with pytest.raises(GenerationError) as excinfo:
        assignment.children.append(Literal("0.0", REAL_SINGLE_TYPE))
    assert ("Item 'Literal' can't be child 2 of 'Assignment'. The valid format"
            " is: 'DataNode, DataNode'.") in str(excinfo.value)


def test_is_array_range():
    '''test that the is_array_range method behaves as expected, returning
    true if the LHS of the assignment is an array range access.

    '''
    one = Literal("1.0", REAL_TYPE)
    int_one = Literal("1", INTEGER_TYPE)
    int_ten = Literal("10", INTEGER_TYPE)

    # lhs is an array reference with a range
    array_type = ArrayType(REAL_TYPE, [10, 10])
    symbol = DataSymbol("x", array_type)
    x_range = Range.create(int_one, int_ten.copy(), int_one.copy())
    array_ref = ArrayReference.create(symbol, [x_range, int_one.copy()])
    assignment = Assignment.create(array_ref, one.copy())
    assert assignment.is_array_range is True

    # Check when lhs consists of various forms of structure access
    grid_type = StructureType.create([
        ("dx", REAL_SINGLE_TYPE, Symbol.Visibility.PUBLIC),
        ("dy", REAL_SINGLE_TYPE, Symbol.Visibility.PUBLIC)])
    grid_type_symbol = DataTypeSymbol("grid_type", grid_type)
    # Create the definition of the 'field_type', contains array of grid_types
    field_type_def = StructureType.create(
        [("data", ArrayType(REAL_SINGLE_TYPE, [10]), Symbol.Visibility.PUBLIC),
         ("sub_meshes", ArrayType(grid_type_symbol, [3]),
          Symbol.Visibility.PUBLIC)])
    field_type_symbol = DataTypeSymbol("field_type", field_type_def)
    field_symbol = DataSymbol("wind", field_type_symbol)

    # Array reference to component of derived type using a range
    lbound = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND,
        StructureReference.create(field_symbol, ["data"]), int_one.copy())
    ubound = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND,
        StructureReference.create(field_symbol, ["data"]), int_one.copy())
    my_range = Range.create(lbound, ubound)

    data_ref = StructureReference.create(field_symbol, [("data", [my_range])])
    assign = Assignment.create(data_ref, one.copy())
    assert assign.is_array_range is True

    # Access to slice of 'sub_meshes': wind%sub_meshes(1:3)%dx = 1.0
    sub_range = Range.create(int_one.copy(), Literal("3", INTEGER_TYPE))
    dx_ref = StructureReference.create(field_symbol, [("sub_meshes",
                                                       [sub_range]), "dx"])
    sub_assign = Assignment.create(dx_ref, one.copy())
    assert sub_assign.is_array_range is True

    # Create an array of these derived types and assign to a slice:
    # chi(1:10)%data(1) = 1.0
    field_bundle_symbol = DataSymbol("chi", ArrayType(field_type_symbol, [3]))
    fld_range = Range.create(int_one.copy(), Literal("10", INTEGER_TYPE))
    fld_ref = ArrayOfStructuresReference.create(field_bundle_symbol,
                                                [fld_range],
                                                [("data", [int_one.copy()])])
    fld_assign = Assignment.create(fld_ref, one.copy())
    assert fld_assign.is_array_range is True

    # When the slice has two operator ancestors, none of which are a reduction
    # e.g y(1, INT(ABS(map(:, 1)))) = 1.0
    int_array_type = ArrayType(INTEGER_SINGLE_TYPE, [10, 10])
    map_sym = DataSymbol("map", int_array_type)
    lbound1 = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND,
        Reference(map_sym), int_one.copy())
    ubound1 = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND,
        Reference(map_sym), int_one.copy())
    my_range1 = Range.create(lbound1, ubound1)
    abs_op = UnaryOperation.create(UnaryOperation.Operator.ABS,
                                   ArrayReference.create(map_sym,
                                                         [my_range1,
                                                          int_one.copy()]))
    int_op = UnaryOperation.create(UnaryOperation.Operator.INT, abs_op)
    assignment = Assignment.create(
        ArrayReference.create(symbol, [int_one.copy(), int_op]),
        one.copy())
    assert assignment.is_array_range is True


def test_array_range_with_reduction():
    ''' Test that we correctly identify an array range when it is the result
        of a reduction from an array, e.g x(1, INT(SUM(map(:, :), 1))) = 1.0

    '''
    one = Literal("1.0", REAL_TYPE)
    int_one = Literal("1", INTEGER_TYPE)
    int_two = Literal("2", INTEGER_TYPE)
    int_array_type = ArrayType(INTEGER_SINGLE_TYPE, [10, 10])
    map_sym = DataSymbol("map", int_array_type)
    array_type = ArrayType(REAL_TYPE, [10, 10])
    symbol = DataSymbol("x", array_type)
    lbound1 = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND,
        Reference(map_sym), int_one.copy())
    ubound1 = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND,
        Reference(map_sym), int_one.copy())
    my_range1 = Range.create(lbound1, ubound1)
    lbound2 = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND,
        Reference(map_sym), int_two.copy())
    ubound2 = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND,
        Reference(map_sym), int_two.copy())
    my_range2 = Range.create(lbound2, ubound2)
    bsum_op = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.SUM,
        [ArrayReference.create(map_sym, [my_range1, my_range2]),
         ("dim", int_one.copy())])
    int_op2 = UnaryOperation.create(UnaryOperation.Operator.INT, bsum_op)
    assignment = Assignment.create(
        ArrayReference.create(symbol,
                              [int_one.copy(), int_op2]),
        one.copy())
    if not assignment.is_array_range:
        # is_array_range should return True
        pytest.xfail(reason="#658 needs typing of PSyIR expressions")


def test_is_not_array_range():
    ''' Test that is_array_range correctly rejects things that aren't
    an assignment to an array range.

    '''
    int_one = Literal("1", INTEGER_SINGLE_TYPE)
    one = Literal("1.0", REAL_TYPE)
    var = DataSymbol("x", REAL_TYPE)
    reference = Reference(var)

    # lhs is not an array
    assignment = Assignment.create(reference, one)
    assert assignment.is_array_range is False

    # lhs is an array reference but has no range
    array_type = ArrayType(REAL_TYPE, [10, 10])
    symbol = DataSymbol("y", array_type)
    array_ref = Reference(symbol)
    assignment = Assignment.create(array_ref, one.copy())
    assert assignment.is_array_range is False

    # lhs is an array reference but the single index value is obtained
    # using an array range, y(1, SUM(map(:), 1)) = 1.0
    int_array_type = ArrayType(INTEGER_SINGLE_TYPE, [10])
    map_sym = DataSymbol("map", int_array_type)
    start = BinaryOperation.create(BinaryOperation.Operator.LBOUND,
                                   Reference(map_sym), int_one.copy())
    stop = BinaryOperation.create(BinaryOperation.Operator.UBOUND,
                                  Reference(map_sym), int_one.copy())
    my_range = Range.create(start, stop)
    sum_op = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.SUM,
        [ArrayReference.create(map_sym, [my_range]), ("dim", int_one.copy())])
    assignment = Assignment.create(
        ArrayReference.create(symbol, [int_one.copy(), sum_op]),
        one.copy())
    assert assignment.is_array_range is False

    # When the slice has two operator ancestors, one of which is a reduction
    # e.g y(1, SUM(ABS(map(:)), 1)) = 1.0
    abs_op = UnaryOperation.create(UnaryOperation.Operator.ABS,
                                   ArrayReference.create(map_sym,
                                                         [my_range.copy()]))
    sum_op2 = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.SUM, [abs_op, ("dim", int_one.copy())])
    assignment = Assignment.create(
        ArrayReference.create(symbol, [int_one.copy(), sum_op2]),
        one.copy())
    assert assignment.is_array_range is False

    # lhs is a scalar member of a structure
    grid_type = StructureType.create([
        ("dx", REAL_SINGLE_TYPE, Symbol.Visibility.PUBLIC),
        ("dy", REAL_SINGLE_TYPE, Symbol.Visibility.PUBLIC)])
    grid_type_symbol = DataTypeSymbol("grid_type", grid_type)
    grid_sym = DataSymbol("grid", grid_type_symbol)
    assignment = Assignment.create(StructureReference.create(grid_sym, ["dx"]),
                                   one.copy())
    assert assignment.is_array_range is False


def test_assignment_gen_code():
    '''Test that the gen_code method in the Assignment class produces the
    expected Fortran code.

    TODO #1648: This is just needed for coverage of the gen_code, that in turn
    is needed because another test (profiling_node tests) uses it. But gen_code
    is deprecated and this test should be removed when the gen_code is not used
    in any other test.

    '''
    lhs = Reference(DataSymbol("tmp", REAL_SINGLE_TYPE))
    rhs = Literal("0.0", REAL_SINGLE_TYPE)
    assignment = Assignment.create(lhs, rhs)
    check_links(assignment, [lhs, rhs])
    module = ModuleGen("test")
    assignment.gen_code(module)
    code = str(module.root)
    assert "tmp = 0.0\n" in code
