# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2022, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains pytest tests for the ArrayMember class. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir import symbols, nodes
from psyclone.errors import GenerationError, InternalError


def test_am_constructor():
    ''' Test that we can construct an ArrayMember. '''
    amr = nodes.ArrayMember("sub_mesh")
    assert len(amr.children) == 0
    assert amr.name == "sub_mesh"


def test_am_create():
    ''' Test the create method of ArrayMember. '''
    amem = nodes.ArrayMember.create("subdomains",
                                    [nodes.Literal("1", symbols.INTEGER_TYPE),
                                     nodes.Literal("2", symbols.INTEGER_TYPE)])
    assert isinstance(amem, nodes.ArrayMember)
    assert len(amem.children) == 2
    assert isinstance(amem.indices[1], nodes.Literal)
    assert amem.indices[1].parent is amem

    with pytest.raises(GenerationError) as err:
        nodes.ArrayMember.create("subdomains",
                                 nodes.Literal("1", symbols.INTEGER_TYPE))
    assert ("indices argument in create method of ArrayMember class should be "
            "a list but found 'Literal'" in str(err.value))


def test_am_validate_child():
    ''' Test the _validate_child method of ArrayMember. '''
    idx = nodes.Literal("3", symbols.INTEGER_TYPE)
    amr = nodes.ArrayMember("sub_mesh")
    with pytest.raises(GenerationError) as err:
        amr.addchild("wrong")
    assert "'str' can't be child 0 of 'ArrayMember'" in str(err.value)
    amr.addchild(idx)
    assert amr.children[0] is idx


def test_am_is_lower_upper_bound():
    ''' Test the is_lower/upper_bound methods of ArrayMember. '''
    one = nodes.Literal("1", symbols.INTEGER_TYPE)
    two = nodes.Literal("2", symbols.INTEGER_TYPE)
    amem1 = nodes.ArrayMember.create(
        "subdomains",
        [one.copy(), nodes.Literal("2", symbols.INTEGER_TYPE)])
    assert amem1.is_lower_bound(0) is False
    assert amem1.is_upper_bound(0) is False
    grid_type = symbols.DataTypeSymbol("grid_type", symbols.DeferredType())
    sym = symbols.DataSymbol("grid_var", grid_type)
    ref = nodes.StructureReference.create(sym, ["data"])
    # Start and stop for the range are binary operators but not the right ones
    start = nodes.operation.BinaryOperation.create(
        nodes.operation.BinaryOperation.Operator.UBOUND,
        ref.copy(), one.copy())
    stop = nodes.operation.BinaryOperation.create(
        nodes.operation.BinaryOperation.Operator.LBOUND,
        ref.copy(), one.copy())
    my_range = nodes.Range.create(start, stop)
    sref = nodes.StructureReference.create(sym, [("data", [my_range])])
    amem2 = sref.walk(nodes.ArrayMember)[0]
    assert amem2.is_lower_bound(0) is False
    assert amem2.is_upper_bound(0) is False
    # Correct binary operators but wrong types of operand
    start = nodes.operation.BinaryOperation.create(
        nodes.operation.BinaryOperation.Operator.LBOUND,
        one.copy(), one.copy())
    stop = nodes.operation.BinaryOperation.create(
        nodes.operation.BinaryOperation.Operator.UBOUND,
        one.copy(), one.copy())
    my_range = nodes.Range.create(start, stop)
    sref = nodes.StructureReference.create(sym, [("data", [my_range])])
    amem2 = sref.walk(nodes.ArrayMember)[0]
    assert amem2.is_lower_bound(0) is False
    assert amem2.is_upper_bound(0) is False
    # Correct start and stop arguments to Range
    start = nodes.operation.BinaryOperation.create(
        nodes.operation.BinaryOperation.Operator.LBOUND,
        ref.copy(), one.copy())
    stop = nodes.operation.BinaryOperation.create(
        nodes.operation.BinaryOperation.Operator.UBOUND,
        ref.copy(), one.copy())
    my_range = nodes.Range.create(start, stop)
    sref = nodes.StructureReference.create(sym, [("data", [my_range])])
    amem2 = sref.walk(nodes.ArrayMember)[0]
    assert amem2.is_lower_bound(0) is True
    assert amem2.is_upper_bound(0) is True
    # Range in a dimension other than the first
    start = nodes.operation.BinaryOperation.create(
        nodes.operation.BinaryOperation.Operator.LBOUND,
        ref.copy(), two.copy())
    stop = nodes.operation.BinaryOperation.create(
        nodes.operation.BinaryOperation.Operator.UBOUND,
        ref.copy(), two.copy())
    my_range = nodes.Range.create(start, stop)
    sref = nodes.StructureReference.create(sym, [("data",
                                                  [one.copy(), my_range])])
    amem2 = sref.walk(nodes.ArrayMember)[0]
    assert amem2.is_lower_bound(0) is False
    assert amem2.is_upper_bound(0) is False
    assert amem2.is_lower_bound(1) is True
    assert amem2.is_upper_bound(1) is True


def test_rank_of_subsection():
    '''Test the rank_of_subsection method.

    '''
    int_one = nodes.Literal("1", symbols.INTEGER_TYPE)

    # Structure reference containing no array access
    symbol = symbols.DataSymbol("field", symbols.DeferredType())
    assert nodes.StructureReference.create(
            symbol, ["first", "second"]).rank_of_subsection() == 0

    # Structure reference with ranges in more than one part reference.
    sref1 = nodes.StructureReference.create(symbol, ["first"])
    lbound1 = nodes.BinaryOperation.create(
        nodes.BinaryOperation.Operator.LBOUND, sref1, int_one.copy())
    ubound1 = nodes.BinaryOperation.create(
        nodes.BinaryOperation.Operator.UBOUND, sref1.copy(), int_one.copy())
    range1 = nodes.Range.create(lbound1, ubound1)
    sref = nodes.StructureReference.create(symbol, [("first", [range1]),
                                                    "second"])
    lbound2 = nodes.BinaryOperation.create(
        nodes.BinaryOperation.Operator.LBOUND, sref, int_one.copy())
    ubound2 = nodes.BinaryOperation.create(
        nodes.BinaryOperation.Operator.UBOUND, sref.copy(), int_one.copy())
    range2 = nodes.Range.create(lbound2, ubound2)
    with pytest.raises(ValueError) as err:
        nodes.StructureReference.create(
            symbol, [("first", [range1.copy()]),
                     ("second", [range2])]).rank_of_subsection()
    assert ("create() may contain a Range but both 'first' and 'second' have "
            "Ranges." in str(err.value))
    # Repeat but this time for an ArrayOfStructuresReference.
    lbound3 = nodes.BinaryOperation.create(
        nodes.BinaryOperation.Operator.LBOUND,
        nodes.Reference(symbol), int_one.copy())
    ubound3 = nodes.BinaryOperation.create(
        nodes.BinaryOperation.Operator.UBOUND,
        nodes.Reference(symbol), int_one.copy())
    range3 = nodes.Range.create(lbound3, ubound3)
    asref = nodes.ArrayOfStructuresReference.create(
            symbol, [range3.copy()],
            ["first", ("second", [range2.copy()])])
    with pytest.raises(NotImplementedError) as err:
        asref.rank_of_subsection()
    assert ("Only subsections that involve the full extent of a given array "
            "dimension are supported but index 0 of 'second' contains a "
            "Range that is not for the full extent: second" in str(err.value))

    # An array with no subsection has rank of zero
    array_type = symbols.ArrayType(symbols.REAL_TYPE, [10])
    symbol = symbols.DataSymbol("a", array_type)
    array = nodes.ArrayReference.create(symbol, [int_one.copy()])
    assert array.rank_of_subsection() == 0

    # If array syntax notation is found, it must be for all elements
    # in that dimension
    array_type = symbols.ArrayType(symbols.REAL_TYPE, [10, 10, 10])
    symbol = symbols.DataSymbol("a", array_type)
    lbound_op1 = nodes.BinaryOperation.create(
        nodes.BinaryOperation.Operator.LBOUND,
        nodes.Reference(symbol), nodes.Literal("1", symbols.INTEGER_TYPE))
    ubound_op1 = nodes.BinaryOperation.create(
        nodes.BinaryOperation.Operator.UBOUND,
        nodes.Reference(symbol), nodes.Literal("1", symbols.INTEGER_TYPE))
    lbound_op3 = nodes.BinaryOperation.create(
        nodes.BinaryOperation.Operator.LBOUND,
        nodes.Reference(symbol), nodes.Literal("3", symbols.INTEGER_TYPE))
    ubound_op3 = nodes.BinaryOperation.create(
        nodes.BinaryOperation.Operator.UBOUND,
        nodes.Reference(symbol), nodes.Literal("3", symbols.INTEGER_TYPE))

    range1 = nodes.Range.create(lbound_op1, ubound_op1)
    range2 = nodes.Range.create(lbound_op3, ubound_op3)
    one = nodes.Literal("1", symbols.INTEGER_TYPE)
    array = nodes.ArrayReference.create(symbol, [range1, one.copy(), range2])
    # Two array dimensions use array notation.
    assert array.rank_of_subsection() == 2

    # Make one of the array notation dimensions differ from what is required.
    range2 = nodes.Range.create(lbound_op3.copy(), one.copy())
    array = nodes.ArrayReference.create(symbol, [range1.copy(), one.copy(),
                                                 range2.copy()])
    with pytest.raises(NotImplementedError) as excinfo:
        array.rank_of_subsection()
    assert ("Only subsections that involve the full extent of a given array "
            "dimension are supported but index 2 of 'a' contains a Range "
            "that is not for the full extent: a(:,1,:1)" in str(excinfo.value))


def test_am_same_array():
    ''' Test the is_same_array method of ArrayMember. '''
    one = nodes.Literal("1", symbols.INTEGER_TYPE)
    two = nodes.Literal("2", symbols.INTEGER_TYPE)
    amem1 = nodes.ArrayMember.create(
        "subdomains",
        [one.copy(), nodes.Literal("2", symbols.INTEGER_TYPE)])
    # Check when the ArrayMember has no parent Reference
    result = amem1.is_same_array(
        nodes.Reference(symbols.DataSymbol("fake",
                                           symbols.INTEGER_TYPE)))
    assert result is False
    grid_type = symbols.DataTypeSymbol("grid_type", symbols.DeferredType())
    sym1 = symbols.DataSymbol("grid_var", grid_type)
    sym2 = symbols.DataSymbol("grid_var2", grid_type)
    ref1 = nodes.StructureReference.create(sym1, ["data"])
    # Reference is to a different symbol
    ref2 = nodes.StructureReference.create(sym2, [("data", [one.copy()])])
    assert ref2.member.is_same_array(ref1) is False
    # Reference is to a different member of the same symbol
    ref2 = nodes.StructureReference.create(sym1, [("xvals", [one.copy()])])
    assert ref2.member.is_same_array(ref1) is False
    ref1 = nodes.StructureReference.create(sym1, [("data", [one.copy()]),
                                                  ("xobs", [one.copy()])])
    ref2 = nodes.StructureReference.create(sym1, [("data", [one.copy()])])
    assert ref1.member.is_same_array(ref2) is True
    assert ref2.member.is_same_array(ref1) is False
    ref2 = nodes.StructureReference.create(sym1, [("data", [one.copy()]),
                                                  ("yobs", [one.copy()])])
    amem = ref2.member.member  # "yobs"
    assert amem.is_same_array(ref1) is False
    # The same 'signature' (a%b%c) but where b is an array access in one
    # case. This may not be possible in Fortran but we need to exercise
    # all conditions.
    ref1 = nodes.StructureReference.create(sym1, [("a", [one.copy()]),
                                                  "b", "c"])
    ref2 = nodes.StructureReference.create(sym1, [("a", [one.copy()]),
                                                  ("b", [one.copy()]),
                                                  ("c", [one.copy()])])
    amem = ref2.walk(nodes.ArrayMember)[0]
    assert amem.is_same_array(ref1) is False
    # Same 'signature' but with one array access having more dimensions.
    ref1 = nodes.StructureReference.create(sym1, [("a", [one.copy()]),
                                                  ("b", [one.copy()]),
                                                  ("c", [one.copy()])])
    ref2 = nodes.StructureReference.create(sym1, [("a", [one.copy()]),
                                                  ("b", [one.copy(),
                                                         one.copy()]),
                                                  ("c", [one.copy()])])
    amem = ref2.walk(nodes.ArrayMember)[0]
    assert amem.is_same_array(ref1) is False
    # Same 'signature' but with one array access having a different index.
    ref1 = nodes.StructureReference.create(sym1, [("a", [one.copy()]),
                                                  ("b", [one.copy(),
                                                         one.copy()]),
                                                  ("c", [one.copy()])])
    ref2 = nodes.StructureReference.create(sym1, [("a", [one.copy()]),
                                                  ("b", [one.copy(),
                                                         two.copy()]),
                                                  ("c", [one.copy()])])
    amem = ref2.walk(nodes.ArrayMember)[0]
    assert amem.is_same_array(ref1) is False
    # Reference to an element of the same array
    ref1 = nodes.StructureReference.create(sym1, ["data"])
    ref2 = nodes.StructureReference.create(sym1, [("data", [one.copy()])])
    assert ref2.member.is_same_array(ref1) is True
    # Reference to an ArrayOfStructures
    array_sym = symbols.DataSymbol("grids",
                                   symbols.ArrayType(grid_type, [two.copy()]))
    ref1 = nodes.ArrayOfStructuresReference.create(array_sym, [one.copy()],
                                                   ["data"])
    assert ref1.is_same_array(nodes.Reference(array_sym))
    # member being compared is not at the bottom of a derived-type access
    ref1 = nodes.StructureReference.create(sym1, [("a", [one.copy()]),
                                                  ("b", [one.copy()])])
    ref2 = nodes.StructureReference.create(sym1, [("a", [one.copy()]),
                                                  ("b", [one.copy()]),
                                                  ("c", [one.copy()])])
    amem = ref2.member.member
    assert amem.name == "b"
    assert amem.is_same_array(ref1)
