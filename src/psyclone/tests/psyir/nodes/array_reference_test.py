# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2021, Science and Technology Facilities Council.
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

''' Performs py.test tests of the ArrayReference PSyIR node. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.nodes.node import colored
from psyclone.psyir.nodes import Reference, ArrayReference, Assignment, \
    Literal, BinaryOperation, Range, KernelSchedule
from psyclone.psyir.symbols import DataSymbol, ArrayType, \
    REAL_SINGLE_TYPE, INTEGER_SINGLE_TYPE, REAL_TYPE, INTEGER_TYPE
from psyclone.errors import GenerationError, InternalError
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import check_links


def test_array_node_str():
    ''' Check the node_str method of the ArrayReference class.'''
    kschedule = KernelSchedule("kname")
    array_type = ArrayType(INTEGER_SINGLE_TYPE, [ArrayType.Extent.ATTRIBUTE])
    symbol = DataSymbol("aname", array_type)
    kschedule.symbol_table.add(symbol)
    assignment = Assignment(parent=kschedule)
    array = ArrayReference(symbol, parent=assignment)
    coloredtext = colored("ArrayReference", ArrayReference._colour)
    assert coloredtext+"[name:'aname']" in array.node_str()


def test_array_can_be_printed():
    '''Test that an ArrayReference instance can always be printed (i.e. is
    initialised fully)'''
    kschedule = KernelSchedule("kname")
    symbol = DataSymbol("aname", INTEGER_SINGLE_TYPE)
    kschedule.symbol_table.add(symbol)
    assignment = Assignment(parent=kschedule)
    array = ArrayReference(symbol, assignment)
    assert "ArrayReference[name:'aname']\n" in str(array)


def test_array_create():
    '''Test that the create method in the ArrayReference class correctly
    creates an ArrayReference instance.

    '''
    array_type = ArrayType(REAL_SINGLE_TYPE, [10, 10, 10])
    symbol_temp = DataSymbol("temp", array_type)
    symbol_i = DataSymbol("i", INTEGER_SINGLE_TYPE)
    symbol_j = DataSymbol("j", INTEGER_SINGLE_TYPE)
    children = [Reference(symbol_i), Reference(symbol_j),
                Literal("1", INTEGER_SINGLE_TYPE)]
    array = ArrayReference.create(symbol_temp, children)
    check_links(array, children)
    result = FortranWriter().arrayreference_node(array)
    assert result == "temp(i,j,1)"


def test_array_create_invalid1():
    '''Test that the create method in the ArrayReference class raises an
    exception if the provided symbol is not an array.

    '''
    symbol_i = DataSymbol("i", INTEGER_SINGLE_TYPE)
    symbol_j = DataSymbol("j", INTEGER_SINGLE_TYPE)
    symbol_temp = DataSymbol("temp", REAL_SINGLE_TYPE)
    children = [Reference(symbol_i), Reference(symbol_j),
                Literal("1", INTEGER_SINGLE_TYPE)]
    with pytest.raises(GenerationError) as excinfo:
        _ = ArrayReference.create(symbol_temp, children)
    assert ("expecting the symbol to be an array, not a scalar."
            in str(excinfo.value))


def test_array_create_invalid2():
    '''Test that the create method in the ArrayReference class raises an
    exception if the number of dimension in the provided symbol is different
    to the number of indices provided to the create method.

    '''
    array_type = ArrayType(REAL_SINGLE_TYPE, [10])
    symbol_temp = DataSymbol("temp", array_type)
    symbol_i = DataSymbol("i", INTEGER_SINGLE_TYPE)
    symbol_j = DataSymbol("j", INTEGER_SINGLE_TYPE)
    children = [Reference(symbol_i), Reference(symbol_j),
                Literal("1", INTEGER_SINGLE_TYPE)]
    with pytest.raises(GenerationError) as excinfo:
        _ = ArrayReference.create(symbol_temp, children)
    assert ("the symbol should have the same number of dimensions as indices "
            "(provided in the 'indices' argument). Expecting '3' but found "
            "'1'." in str(excinfo.value))


def test_array_create_invalid3():
    '''Test that the create method in an ArrayReference class raises the
    expected exception if the provided input is invalid.

    '''
    # symbol argument is not a DataSymbol
    with pytest.raises(GenerationError) as excinfo:
        _ = ArrayReference.create([], [])
    assert ("symbol argument in create method of ArrayReference class should "
            "be a DataSymbol but found 'list'."
            in str(excinfo.value))

    # children not a list
    with pytest.raises(GenerationError) as excinfo:
        _ = ArrayReference.create(DataSymbol("temp", REAL_SINGLE_TYPE),
                                  "invalid")
    assert ("indices argument in create method of ArrayReference class should"
            " be a list but found 'str'." in str(excinfo.value))


def test_array_children_validation():
    '''Test that children added to Array are validated. Array accepts
    DataNodes and Range children.'''
    array_type = ArrayType(REAL_SINGLE_TYPE, shape=[5, 5])
    array = ArrayReference(DataSymbol("rname", array_type))
    datanode1 = Literal("1", INTEGER_SINGLE_TYPE)
    erange = Range()
    assignment = Assignment()

    # Invalid child
    with pytest.raises(GenerationError) as excinfo:
        array.addchild(assignment)
    assert ("Item 'Assignment' can't be child 0 of 'ArrayReference'. The valid"
            " format is: '[DataNode | Range]+'." in str(excinfo.value))

    # Valid children
    array.addchild(datanode1)
    array.addchild(erange)


def test_array_validate_index():
    '''Test that the validate_index utility function behaves as expected.'''
    array = ArrayReference.create(DataSymbol("test",
                                             ArrayType(REAL_TYPE, [10])),
                                  [Literal("1", INTEGER_TYPE)])
    with pytest.raises(TypeError) as info:
        array._validate_index("hello")
    assert ("The index argument should be an integer but found 'str'."
            in str(info.value))

    with pytest.raises(ValueError) as info:
        array._validate_index(1)
    assert ("In ArrayReference 'test' the specified index '1' must be less "
            "than the number of dimensions '1'." in str(info.value))

    array._validate_index(0)
    array._validate_index(-1)


def test_array_is_lower_bound():
    '''Test that the is_lower_bound method in the Array Node works as
    expected.

    '''
    one = Literal("1", INTEGER_TYPE)
    array = ArrayReference.create(DataSymbol("test",
                                             ArrayType(REAL_TYPE, [10])),
                                  [one])
    with pytest.raises(TypeError) as info:
        array.is_lower_bound("hello")
    assert ("The index argument should be an integer but found 'str'."
            in str(info.value))

    # not a range node at index 0
    assert not array.is_lower_bound(0)

    # range node does not have a binary operator for its start value
    array.children[0] = Range.create(one.copy(), one.copy(), one.copy())
    assert not array.is_lower_bound(0)

    # range node lbound references a different array
    array2 = ArrayReference.create(DataSymbol("test2",
                                              ArrayType(REAL_TYPE, [10])),
                                   [one.copy()])
    operator = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND, array2, one.copy())
    array.children[0] = Range.create(operator, one.copy(), one.copy())
    assert not array.is_lower_bound(0)

    # range node lbound references a different index
    operator = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND, array.copy(),
        Literal("2", INTEGER_TYPE))
    array.children[0] = Range.create(operator, one.copy(), one.copy())
    assert not array.is_lower_bound(0)

    # all is well
    operator = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND, array.copy(), one.copy())
    array.children[0] = Range.create(operator, one.copy(), one.copy())
    assert array.is_lower_bound(0)


def test_array_is_upper_bound():
    '''Test that the is_upper_bound method in the Array Node works as
    expected.

    '''
    one = Literal("1", INTEGER_TYPE)
    array = ArrayReference.create(DataSymbol("test",
                                             ArrayType(REAL_TYPE, [10])),
                                  [one])
    with pytest.raises(TypeError) as info:
        array.is_upper_bound("hello")
    assert ("The index argument should be an integer but found 'str'."
            in str(info.value))

    # not a range node at index 0
    assert not array.is_upper_bound(0)

    # range node does not have a binary operator for its stop value
    array.children[0] = Range.create(one.copy(), one.copy(), one.copy())
    assert not array.is_upper_bound(0)

    # range node ubound references a different array
    array2 = ArrayReference.create(DataSymbol("test2",
                                              ArrayType(REAL_TYPE, [10])),
                                   [one.copy()])
    operator = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, array2, one.copy())
    array.children[0] = Range.create(one.copy(), operator, one.copy())
    assert not array.is_upper_bound(0)

    # range node ubound references a different index
    operator = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, array.copy(),
        Literal("2", INTEGER_TYPE))
    array.children[0] = Range.create(one.copy(), operator, one.copy())
    assert not array.is_upper_bound(0)

    # all is well
    operator = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, array.copy(), one.copy())
    array.children[0] = Range.create(one.copy(), operator, one.copy())
    assert array.is_upper_bound(0)


def test_array_is_full_range():
    '''Test that the is_full_range method in the Array Node works as
    expected. '''
    # pylint: disable=too-many-statements
    zero = Literal("0", INTEGER_SINGLE_TYPE)
    one = Literal("1", INTEGER_SINGLE_TYPE)
    array_type = ArrayType(REAL_SINGLE_TYPE, [10])
    symbol = DataSymbol("my_array", array_type)
    reference = Reference(symbol)
    lbound = BinaryOperation.create(BinaryOperation.Operator.LBOUND,
                                    reference, one)
    ubound = BinaryOperation.create(BinaryOperation.Operator.UBOUND,
                                    reference.copy(), one.copy())
    symbol_error = DataSymbol("another_array", array_type)
    reference_error = Reference(symbol_error)

    # Index out of bounds
    array_reference = ArrayReference.create(symbol, [one.copy()])
    with pytest.raises(ValueError) as excinfo:
        array_reference.is_full_range(1)
    assert ("In ArrayReference 'my_array' the specified index '1' must be "
            "less than the number of dimensions '1'." in str(excinfo.value))

    # Array dimension is not a Range
    assert not array_reference.is_full_range(0)

    # Check LBOUND
    # Array dimension range lower bound is not a binary operation
    my_range = Range.create(one.copy(), one.copy(), one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range lower bound is not an LBOUND binary operation
    my_range = Range.create(ubound, one.copy(), one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range lower bound is an LBOUND binary operation
    # with the first value not being a reference
    lbound_error = BinaryOperation.create(BinaryOperation.Operator.LBOUND,
                                          zero.copy(), zero.copy())
    my_range = Range.create(lbound_error, one.copy(), one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range lower bound is an LBOUND binary operation
    # with the first value being a reference to a different symbol
    lbound_error = BinaryOperation.create(BinaryOperation.Operator.LBOUND,
                                          reference_error, zero.copy())
    my_range = Range.create(lbound_error, one.copy(), one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range lower bound is an LBOUND binary operation
    # with the second value not being a literal.
    lbound_error = BinaryOperation.create(BinaryOperation.Operator.LBOUND,
                                          reference.copy(), reference.copy())
    my_range = Range.create(lbound_error, one.copy(), one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range lower bound is an LBOUND binary operation
    # with the second value not being an integer literal.
    lbound_error = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND, reference.copy(),
        Literal("1.0", REAL_SINGLE_TYPE))
    my_range = Range.create(lbound_error, one.copy(), one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range lower bound is an LBOUND binary operation
    # with the second value being an integer literal with the wrong
    # value (should be 0 as this dimension index is 0).
    lbound_error = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND, reference.copy(), one.copy())
    my_range = Range.create(lbound_error, one.copy(), one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Check UBOUND
    # Array dimension range upper bound is not a binary operation
    my_range = Range.create(lbound, one.copy(), one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range upper bound is not a UBOUND binary operation
    my_range = Range.create(lbound.copy(), lbound.copy(), one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range upper bound is a UBOUND binary operation
    # with the first value not being a reference
    ubound_error = BinaryOperation.create(BinaryOperation.Operator.UBOUND,
                                          zero.copy(), zero.copy())
    my_range = Range.create(lbound.copy(), ubound_error, one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range upper bound is a UBOUND binary operation
    # with the first value being a reference to a different symbol
    ubound_error = BinaryOperation.create(BinaryOperation.Operator.UBOUND,
                                          reference_error.copy(), zero.copy())
    my_range = Range.create(lbound.copy(), ubound_error, one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range upper bound is a UBOUND binary operation
    # with the second value not being a literal.
    ubound_error = BinaryOperation.create(BinaryOperation.Operator.UBOUND,
                                          reference.copy(), reference.copy())
    my_range = Range.create(lbound.copy(), ubound_error, one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range upper bound is a UBOUND binary operation
    # with the second value not being an integer literal.
    ubound_error = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, reference.copy(),
        Literal("1.0", REAL_SINGLE_TYPE))
    my_range = Range.create(lbound.copy(), ubound_error, one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range upper bound is a UBOUND binary operation
    # with the second value being an integer literal with the wrong
    # value (should be 1 as this dimension is 1).
    ubound_error = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, reference.copy(), zero.copy())
    my_range = Range.create(lbound.copy(), ubound_error, one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Check Step
    # Array dimension range step is not a literal.
    my_range = Range.create(lbound.copy(), ubound.copy(), lbound.copy())
    array_reference = ArrayReference.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range step is not an integer literal.
    my_range = Range.create(lbound.copy(), ubound.copy(), one.copy())
    # We have to change this to a non-integer manually as the create
    # function only accepts integer literals for the step argument.
    my_range.children[2] = Literal("1.0", REAL_SINGLE_TYPE)
    array_reference = ArrayReference.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range step is is an integer literal with the
    # wrong value (not 1).
    my_range = Range.create(lbound.copy(), ubound.copy(), zero.copy())
    array_reference = ArrayReference.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # All is as it should be.
    # The full range is covered so return true.
    my_range = Range.create(lbound.copy(), ubound.copy(), one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])
    assert array_reference.is_full_range(0)


def test_array_indices():
    ''' Tests for the indices property (provided by the ArrayMixin class). '''
    one = Literal("1", INTEGER_TYPE)
    array = ArrayReference.create(DataSymbol("test",
                                             ArrayType(REAL_TYPE, [10])),
                                  [one])
    assert array.indices == [one]
    # Add an invalid child
    array._children = [one.copy(), "hello"]
    with pytest.raises(InternalError) as err:
        _ = array.indices
    assert ("ArrayReference malformed or incomplete: child 1 must by a "
            "psyir.nodes.DataNode or Range representing an array-index "
            "expression but found 'str'" in str(err.value))
    # Remove the children altogether
    array._children = []
    with pytest.raises(InternalError) as err:
        _ = array.indices
    assert ("ArrayReference malformed or incomplete: must have one or more "
            "children representing array-index expressions but found none"
            in str(err.value))
