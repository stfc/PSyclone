# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council.
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

''' This module contains pytest tests for the ArrayOfStructuresReference
    class. '''

from __future__ import absolute_import
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
        ("startx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC)])
    region_type_symbol = symbols.TypeSymbol("region_type", region_type)
    grid_type = symbols.StructureType.create([
        ("nx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC),
        ("region", region_type_symbol, symbols.Symbol.Visibility.PUBLIC)])
    grid_type_symbol = symbols.TypeSymbol("grid_type", grid_type)
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
    lbound = nodes.BinaryOperation.create(
        nodes.BinaryOperation.Operator.LBOUND,
        nodes.Reference(component_symbol), int_one.copy())
    ubound = nodes.BinaryOperation.create(
        nodes.BinaryOperation.Operator.UBOUND,
        nodes.Reference(component_symbol), int_one.copy())
    my_range = nodes.Range.create(lbound, ubound)
    asref = nodes.ArrayOfStructuresReference.create(component_symbol,
                                                    [my_range], ["nx"])
    assert isinstance(asref.children[0], nodes.Member)
    assert isinstance(asref.children[1], nodes.Range)
    check_links(asref, asref.children)
    check_links(asref.children[1], asref.children[1].children)
    # Reference to a symbol of DeferredType
    ssym = symbols.DataSymbol("grid", symbols.DeferredType())
    asref = nodes.ArrayOfStructuresReference.create(
        ssym, [int_one.copy()], ["region", "startx"])
    assert isinstance(asref.symbol.datatype, symbols.DeferredType)
    assert isinstance(asref.children[0], nodes.StructureMember)
    assert isinstance(asref.children[0].children[0], nodes.Member)


def test_asr_create_errors(component_symbol):
    ''' Test the validation checks within the create method. Most validation
    is done within the StructureReference class so there's not much to check
    here. '''
    with pytest.raises(TypeError) as err:
        _ = nodes.ArrayOfStructuresReference.create(1, [], [])
    assert ("'symbol' argument to ArrayOfStructuresReference.create() should "
            "be a DataSymbol but found 'int'" in str(err.value))
    scalar_symbol = symbols.DataSymbol("scalar", symbols.INTEGER_TYPE)
    with pytest.raises(TypeError) as err:
        _ = nodes.ArrayOfStructuresReference.create(scalar_symbol, [], [])
    assert ("ArrayType, DeferredType or UnknownType but symbol 'scalar' has "
            "type 'Scalar" in str(err.value))
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
        ("nx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC)])
    grid_type_symbol = symbols.TypeSymbol("grid_type", grid_type)
    grid_array_type = symbols.ArrayType(grid_type_symbol, [5])
    ssym = symbols.DataSymbol("grid", grid_array_type)
    asref = nodes.ArrayOfStructuresReference.create(
        ssym, [nodes.Literal("2", symbols.INTEGER_TYPE)], ["nx"])
    assert (str(asref) == "ArrayOfStructuresReference[name:'grid']\n"
            "Member[name:'nx']\n"
            "Literal[value:'2', Scalar<INTEGER, UNDEFINED>]")
