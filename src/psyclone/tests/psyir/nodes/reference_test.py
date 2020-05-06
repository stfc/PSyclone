# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council.
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

''' Performs py.test tests on the Reference PSyIR node. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.nodes import Reference, Array, Assignment, Literal, \
    BinaryOperation, Range
from psyclone.psyir.symbols import DataSymbol, ArrayType, \
    REAL_SINGLE_TYPE, INTEGER_SINGLE_TYPE
from psyclone.psyGen import GenerationError, KernelSchedule
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import check_links


def test_reference_bad_init():
    '''Check that the __init__ method of the Reference class raises the
    expected exception if the symbol argument is not of the right
    type.

    '''
    with pytest.raises(TypeError) as excinfo:
        _ = Reference("hello")
    assert ("In Reference initialisation expecting a symbol but found 'str'."
            in str(excinfo.value))


def test_reference_node_str():
    ''' Check the node_str method of the Reference class.'''
    from psyclone.psyir.nodes.node import colored, SCHEDULE_COLOUR_MAP
    kschedule = KernelSchedule("kname")
    symbol = DataSymbol("rname", INTEGER_SINGLE_TYPE)
    kschedule.symbol_table.add(symbol)
    assignment = Assignment(parent=kschedule)
    ref = Reference(symbol, assignment)
    coloredtext = colored("Reference", SCHEDULE_COLOUR_MAP["Reference"])
    assert coloredtext+"[name:'rname']" in ref.node_str()


def test_reference_can_be_printed():
    '''Test that a Reference instance can always be printed (i.e. is
    initialised fully)'''
    kschedule = KernelSchedule("kname")
    symbol = DataSymbol("rname", INTEGER_SINGLE_TYPE)
    kschedule.symbol_table.add(symbol)
    assignment = Assignment(parent=kschedule)
    ref = Reference(symbol, assignment)
    assert "Reference[name:'rname']" in str(ref)


def test_reference_optional_parent():
    '''Test that the parent attribute is None if the optional parent
    argument is not supplied.

    '''
    ref = Reference(DataSymbol("rname", REAL_SINGLE_TYPE))
    assert ref.parent is None


# Test Array class


def test_array_node_str():
    ''' Check the node_str method of the Array class.'''
    from psyclone.psyir.nodes.node import colored, SCHEDULE_COLOUR_MAP
    kschedule = KernelSchedule("kname")
    array_type = ArrayType(INTEGER_SINGLE_TYPE, [ArrayType.Extent.ATTRIBUTE])
    symbol = DataSymbol("aname", array_type)
    kschedule.symbol_table.add(symbol)
    assignment = Assignment(parent=kschedule)
    array = Array(symbol, parent=assignment)
    coloredtext = colored("ArrayReference", SCHEDULE_COLOUR_MAP["Reference"])
    assert coloredtext+"[name:'aname']" in array.node_str()


def test_array_can_be_printed():
    '''Test that an Array instance can always be printed (i.e. is
    initialised fully)'''
    kschedule = KernelSchedule("kname")
    symbol = DataSymbol("aname", INTEGER_SINGLE_TYPE)
    kschedule.symbol_table.add(symbol)
    assignment = Assignment(parent=kschedule)
    array = Array(symbol, assignment)
    assert "ArrayReference[name:'aname']\n" in str(array)


def test_array_create():
    '''Test that the create method in the Array class correctly
    creates an Array instance.

    '''
    array_type = ArrayType(REAL_SINGLE_TYPE, [10, 10, 10])
    symbol_temp = DataSymbol("temp", array_type)
    symbol_i = DataSymbol("i", INTEGER_SINGLE_TYPE)
    symbol_j = DataSymbol("j", INTEGER_SINGLE_TYPE)
    children = [Reference(symbol_i), Reference(symbol_j),
                Literal("1", INTEGER_SINGLE_TYPE)]
    array = Array.create(symbol_temp, children)
    check_links(array, children)
    result = FortranWriter().array_node(array)
    assert result == "temp(i,j,1)"


def test_array_create_invalid1():
    '''Test that the create method in the Array class raises an exception
    if the provided symbol is not an array.

    '''
    symbol_i = DataSymbol("i", INTEGER_SINGLE_TYPE)
    symbol_j = DataSymbol("j", INTEGER_SINGLE_TYPE)
    symbol_temp = DataSymbol("temp", REAL_SINGLE_TYPE)
    children = [Reference(symbol_i), Reference(symbol_j),
                Literal("1", INTEGER_SINGLE_TYPE)]
    with pytest.raises(GenerationError) as excinfo:
        _ = Array.create(symbol_temp, children)
    assert ("expecting the symbol to be an array, not a scalar."
            in str(excinfo.value))


def test_array_create_invalid2():
    '''Test that the create method in the Array class raises an exception
    if the number of dimension in the provided symbol is different to
    the number of indices provided to the create method.

    '''
    array_type = ArrayType(REAL_SINGLE_TYPE, [10])
    symbol_temp = DataSymbol("temp", array_type)
    symbol_i = DataSymbol("i", INTEGER_SINGLE_TYPE)
    symbol_j = DataSymbol("j", INTEGER_SINGLE_TYPE)
    children = [Reference(symbol_i), Reference(symbol_j),
                Literal("1", INTEGER_SINGLE_TYPE)]
    with pytest.raises(GenerationError) as excinfo:
        _ = Array.create(symbol_temp, children)
    assert ("the symbol should have the same number of dimensions as indices "
            "(provided in the 'children' argument). Expecting '3' but found "
            "'1'." in str(excinfo.value))


def test_array_create_invalid3():
    '''Test that the create method in an Array class raises the expected
    exception if the provided input is invalid.

    '''
    # symbol argument is not a DataSymbol
    with pytest.raises(GenerationError) as excinfo:
        _ = Array.create([], [])
    assert ("symbol argument in create method of Array class should "
            "be a DataSymbol but found 'list'."
            in str(excinfo.value))

    # children not a list
    with pytest.raises(GenerationError) as excinfo:
        _ = Array.create(DataSymbol("temp", REAL_SINGLE_TYPE), "invalid")
    assert ("children argument in create method of Array class should "
            "be a list but found 'str'." in str(excinfo.value))

    # contents of children list are not Node
    with pytest.raises(GenerationError) as excinfo:
        _ = Array.create(DataSymbol("temp", REAL_SINGLE_TYPE),
                         [Reference(DataSymbol("i", INTEGER_SINGLE_TYPE)),
                          "invalid"])
    assert (
        "child of children argument in create method of Array class "
        "should be a PSyIR Node but found 'str'." in str(excinfo.value))


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
                                    reference, one)
    symbol_error = DataSymbol("another_array", array_type)
    reference_error = Reference(symbol_error)

    # Index out of bounds
    array_reference = Array.create(symbol, [one])
    with pytest.raises(ValueError) as excinfo:
        array_reference.is_full_range(1)
    assert ("In Array 'my_array' the specified index '1' must be less than "
            "the number of dimensions '1'." in str(excinfo.value))

    # Array dimension is not a Range
    assert not array_reference.is_full_range(0)

    # Check LBOUND
    # Array dimension range lower bound is not a binary operation
    my_range = Range.create(one, one, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range lower bound is not an LBOUND binary operation
    my_range = Range.create(ubound, one, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range lower bound is an LBOUND binary operation
    # with the first value not being a reference
    lbound_error = BinaryOperation.create(BinaryOperation.Operator.LBOUND,
                                          zero, zero)
    my_range = Range.create(lbound_error, one, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range lower bound is an LBOUND binary operation
    # with the first value being a reference to a different symbol
    lbound_error = BinaryOperation.create(BinaryOperation.Operator.LBOUND,
                                          reference_error, zero)
    my_range = Range.create(lbound_error, one, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range lower bound is an LBOUND binary operation
    # with the second value not being a literal.
    lbound_error = BinaryOperation.create(BinaryOperation.Operator.LBOUND,
                                          reference, reference)
    my_range = Range.create(lbound_error, one, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range lower bound is an LBOUND binary operation
    # with the second value not being an integer literal.
    lbound_error = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND, reference,
        Literal("1.0", REAL_SINGLE_TYPE))
    my_range = Range.create(lbound_error, one, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range lower bound is an LBOUND binary operation
    # with the second value being an integer literal with the wrong
    # value (should be 0 as this dimension index is 0).
    lbound_error = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND, reference, one)
    my_range = Range.create(lbound_error, one, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Check UBOUND
    # Array dimension range upper bound is not a binary operation
    my_range = Range.create(lbound, one, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range upper bound is not a UBOUND binary operation
    my_range = Range.create(lbound, lbound, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range upper bound is a UBOUND binary operation
    # with the first value not being a reference
    ubound_error = BinaryOperation.create(BinaryOperation.Operator.UBOUND,
                                          zero, zero)
    my_range = Range.create(lbound, ubound_error, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range upper bound is a UBOUND binary operation
    # with the first value being a reference to a different symbol
    ubound_error = BinaryOperation.create(BinaryOperation.Operator.UBOUND,
                                          reference_error, zero)
    my_range = Range.create(lbound, ubound_error, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range upper bound is a UBOUND binary operation
    # with the second value not being a literal.
    ubound_error = BinaryOperation.create(BinaryOperation.Operator.UBOUND,
                                          reference, reference)
    my_range = Range.create(lbound, ubound_error, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range upper bound is a UBOUND binary operation
    # with the second value not being an integer literal.
    ubound_error = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, reference,
        Literal("1.0", REAL_SINGLE_TYPE))
    my_range = Range.create(lbound, ubound_error, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range upper bound is a UBOUND binary operation
    # with the second value being an integer literal with the wrong
    # value (should be 1 as this dimension is 1).
    ubound_error = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, reference, zero)
    my_range = Range.create(lbound, ubound_error, one)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Check Step
    # Array dimension range step is not a literal.
    my_range = Range.create(lbound, ubound, lbound)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range step is not an integer literal.
    my_range = Range.create(lbound, ubound, one)
    # We have to change this to a non-integer manually as the create
    # function only accepts integer literals for the step argument.
    my_range.children[2] = Literal("1.0", REAL_SINGLE_TYPE)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # Array dimension range step is is an integer literal with the
    # wrong value (not 1).
    my_range = Range.create(lbound, ubound, zero)
    array_reference = Array.create(symbol, [my_range])
    assert not array_reference.is_full_range(0)

    # All is as it should be.
    # The full range is covered so return true.
    my_range = Range.create(lbound, ubound, one)
    array_reference = Array.create(symbol, [my_range])
    assert array_reference.is_full_range(0)
