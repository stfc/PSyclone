# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
# Authors: A. R. Porter, N. Nobre and S. Siso, STFC Daresbury Lab
#          J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains pytest tests for the ArrayOfStructuresReference
    class. '''

import pytest

from psyclone.tests.utilities import check_links
from psyclone.psyir import symbols, nodes


@pytest.fixture(name="component_symbol")
def make_component_symbol():
    '''
    Creates a Symbol of type "grid_type" equivalent to the Fortran:

      type :: grid_type
        integer :: nx
        type(region_type) :: region
      end type

    and

      type :: region_type
        integer :: startx
      end type

    :returns: symbol named "grid" of type "grid_type".
    :rtype: :py:class:`psyclone.psyir.symbols.DataSymbol`

    '''
    region_type = symbols.StructureType.create([
        ("startx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC,
         None)])
    region_type_symbol = symbols.DataTypeSymbol("region_type", region_type)
    grid_type = symbols.StructureType.create([
        ("nx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC, None),
        ("region", region_type_symbol, symbols.Symbol.Visibility.PUBLIC,
         None)])
    grid_type_symbol = symbols.DataTypeSymbol("grid_type", grid_type)
    grid_array_type = symbols.ArrayType(grid_type_symbol, [5])
    ssym = symbols.DataSymbol("grid", grid_array_type)
    return ssym


def test_asr_create(component_symbol):
    ''' Check the create method. '''
    int_one = nodes.Literal("1", symbols.INTEGER_TYPE)
    # Reference to scalar member of structure in array of structures
    asref = nodes.ArrayOfStructuresReference.create(
        component_symbol, [int_one], ["nx"])
    assert isinstance(asref.children[0], nodes.Member)
    assert isinstance(asref.children[1], nodes.Literal)
    check_links(asref, asref.children)
    # Reference to member of structure member of structure in array of
    # structures
    asref = nodes.ArrayOfStructuresReference.create(
        component_symbol, [int_one.copy()], ["region", "startx"])
    assert isinstance(asref.children[0], nodes.StructureMember)
    assert isinstance(asref.children[0].children[0], nodes.Member)
    # Reference to range of structures
    lbound = nodes.IntrinsicCall.create(
        nodes.IntrinsicCall.Intrinsic.LBOUND,
        [nodes.Reference(component_symbol), ("dim", int_one.copy())])
    ubound = nodes.IntrinsicCall.create(
        nodes.IntrinsicCall.Intrinsic.UBOUND,
        [nodes.Reference(component_symbol), ("dim", int_one.copy())])
    my_range = nodes.Range.create(lbound, ubound)
    asref = nodes.ArrayOfStructuresReference.create(component_symbol,
                                                    [my_range], ["nx"])
    assert isinstance(asref.children[0], nodes.Member)
    assert isinstance(asref.children[1], nodes.Range)
    check_links(asref, asref.children)
    check_links(asref.children[1], asref.children[1].children)

    # Test to enforce a type:
    lbound = nodes.IntrinsicCall.create(
        nodes.IntrinsicCall.Intrinsic.LBOUND,
        [nodes.Reference(component_symbol), ("dim", int_one.copy())])
    ubound = nodes.IntrinsicCall.create(
        nodes.IntrinsicCall.Intrinsic.UBOUND,
        [nodes.Reference(component_symbol), ("dim", int_one.copy())])
    my_range = nodes.Range.create(lbound, ubound)
    datatype = symbols.INTEGER8_TYPE
    asref = nodes.ArrayOfStructuresReference.\
        create(component_symbol, [my_range], ["nx"],
               overwrite_datatype=datatype)
    assert asref.datatype is datatype

    # Reference to a symbol of UnresolvedType
    ssym = symbols.DataSymbol("grid", symbols.UnresolvedType())
    asref = nodes.ArrayOfStructuresReference.create(
        ssym, [int_one.copy()], ["region", "startx"])
    assert isinstance(asref.symbol.datatype, symbols.UnresolvedType)
    assert isinstance(asref.children[0], nodes.StructureMember)
    assert isinstance(asref.children[0].children[0], nodes.Member)


def test_asr_create_errors(component_symbol):
    ''' Test the validation checks within the create method. Most validation
    is done within the StructureReference class so there's not much to check
    here. '''
    with pytest.raises(TypeError) as err:
        _ = nodes.ArrayOfStructuresReference.create(1, [], [])
    assert ("'symbol' argument to ArrayOfStructuresReference.create() must "
            "be a DataSymbol but found 'int'" in str(err.value))
    scalar_symbol = symbols.DataSymbol("scalar", symbols.INTEGER_TYPE)
    with pytest.raises(TypeError) as err:
        _ = nodes.ArrayOfStructuresReference.create(scalar_symbol, [], [])
    assert ("ArrayType, UnresolvedType or UnsupportedType but symbol 'scalar' "
            "has type 'Scalar" in str(err.value))
    # Missing children (for array-index expressions)
    with pytest.raises(TypeError) as err:
        _ = nodes.ArrayOfStructuresReference.create(component_symbol,
                                                    False, ["hello"])
    assert ("must be a list containing at least one array-index expression "
            "but this is missing for symbol 'grid'" in str(err.value))
    # Missing member(s)
    with pytest.raises(ValueError) as err:
        _ = nodes.ArrayOfStructuresReference.create(
            component_symbol, [nodes.Literal("1", symbols.INTEGER_TYPE)], [])
    assert ("'members' that are being accessed but got an empty list for "
            "symbol 'grid'" in str(err.value))


def test_ast_str():
    ''' Test that the __str__ method of the StructureReference class works OK
    when we have an ArrayOfStructuresReference. '''
    grid_type = symbols.StructureType.create([
        ("nx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC,
         None)])
    grid_type_symbol = symbols.DataTypeSymbol("grid_type", grid_type)
    grid_array_type = symbols.ArrayType(grid_type_symbol, [5])
    ssym = symbols.DataSymbol("grid", grid_array_type)
    asref = nodes.ArrayOfStructuresReference.create(
        ssym, [nodes.Literal("2", symbols.INTEGER_TYPE)], ["nx"])
    assert (str(asref) == "ArrayOfStructuresReference[name:'grid']\n"
            "Member[name:'nx']\n"
            "Literal[value:'2', Scalar<INTEGER, UNDEFINED>]")


def test_ast_is_array():
    ''' Test that an ArrayOfStructuresReference is marked as being an array.
    '''
    grid_type = symbols.StructureType.create([
        ("nx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC,
         None)])
    grid_type_symbol = symbols.DataTypeSymbol("grid_type", grid_type)
    grid_array_type = symbols.ArrayType(grid_type_symbol, [5])
    ssym = symbols.DataSymbol("grid", grid_array_type)
    asref = nodes.ArrayOfStructuresReference.create(
        ssym, [nodes.Literal("2", symbols.INTEGER_TYPE)], ["nx"])
    assert asref.is_array


def test_asr_datatype():
    '''Test that the datatype property works correctly for
    ArrayOfStructuresReference. (The actual implementation is in
    StructureReference.)'''
    one = nodes.Literal("1", symbols.INTEGER_TYPE)
    two = nodes.Literal("2", symbols.INTEGER_TYPE)

    ndofs = symbols.DataSymbol("ndofs", symbols.INTEGER_TYPE)
    atype = symbols.ArrayType(symbols.REAL_TYPE,
                              [nodes.Reference(ndofs), nodes.Reference(ndofs)])
    grid_type = symbols.StructureType.create([
        ("nx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC, None),
        ("data", atype, symbols.Symbol.Visibility.PUBLIC, None)])
    grid_type_symbol = symbols.DataTypeSymbol("grid_type", grid_type)
    grid_array_type = symbols.ArrayType(grid_type_symbol, [5])
    ssym = symbols.DataSymbol("grid", grid_array_type)
    # Reference to a single member of the array of structures and to the "nx"
    # member of it.
    asref = nodes.ArrayOfStructuresReference.create(
        ssym, [two.copy()], ["nx"])
    assert asref.datatype == symbols.INTEGER_TYPE
    # Reference to a range of members of the array of structures and to the
    # "nx" member of each.
    my_range = nodes.Range.create(two.copy(),
                                  nodes.Literal("3", symbols.INTEGER_TYPE))
    asref2 = nodes.ArrayOfStructuresReference.create(
        ssym, [my_range], ["nx"])
    assert isinstance(asref2.datatype, symbols.ArrayType)
    assert asref2.datatype.intrinsic == symbols.ScalarType.Intrinsic.INTEGER
    assert len(asref2.datatype.shape) == 1
    assert asref2.datatype.shape[0].lower == one
    assert isinstance(asref2.datatype.shape[0].upper, nodes.BinaryOperation)
    # Reference to a single member of the array of structures and to the "data"
    # member of it which is itself an array.
    asref3 = nodes.ArrayOfStructuresReference.create(
        ssym, [one.copy()], ["data"])
    assert isinstance(asref3.datatype, symbols.ArrayType)
    assert len(asref3.datatype.shape) == 2
