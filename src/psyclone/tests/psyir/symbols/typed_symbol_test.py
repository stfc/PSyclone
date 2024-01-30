# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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

''' Perform pytest tests on the psyclone.psyir.symbols.typed_symbol file. '''

import pytest

from psyclone.psyir.symbols import TypedSymbol, ContainerSymbol, DataSymbol, \
    ImportInterface, UnresolvedInterface, ScalarType, ArrayType, \
    REAL_SINGLE_TYPE, REAL_DOUBLE_TYPE, REAL4_TYPE, REAL8_TYPE, \
    INTEGER_SINGLE_TYPE, INTEGER_DOUBLE_TYPE, INTEGER4_TYPE, \
    BOOLEAN_TYPE, CHARACTER_TYPE, UnresolvedType, Symbol, DataTypeSymbol
from psyclone.psyir.nodes import Literal, Reference


class TSymbol(TypedSymbol):
    '''
    Concrete sub-class of TypedSymbol for testing.

    '''
    def __str__(self):
        return "TSymbol"


def test_typed_symbol_abstract():
    ''' Check that TypedSymbol is abstract. '''
    # pylint: disable=abstract-class-instantiated
    with pytest.raises(TypeError) as err:
        TypedSymbol('a', REAL_SINGLE_TYPE)
    assert "instantiate abstract class TypedSymbol" in str(err.value)


def test_typed_symbol_initialisation():
    '''Test that a TypedSymbol-subclass instance can be created when valid
    arguments are given, otherwise raise relevant exceptions.'''

    # Test with valid arguments
    assert isinstance(TSymbol('a', REAL_SINGLE_TYPE), TypedSymbol)
    assert isinstance(TSymbol('a', REAL_DOUBLE_TYPE), TypedSymbol)
    assert isinstance(TSymbol('a', REAL4_TYPE), TypedSymbol)
    kind = DataSymbol('r_def', INTEGER_SINGLE_TYPE)
    real_kind_type = ScalarType(ScalarType.Intrinsic.REAL, kind)
    assert isinstance(TSymbol('a', real_kind_type), TypedSymbol)
    assert isinstance(TSymbol('a', INTEGER_SINGLE_TYPE), TypedSymbol)
    assert isinstance(TSymbol('a', INTEGER_DOUBLE_TYPE), TypedSymbol)
    assert isinstance(TSymbol('a', INTEGER4_TYPE), TypedSymbol)

    assert isinstance(TSymbol('a', CHARACTER_TYPE), TypedSymbol)
    assert isinstance(TSymbol('a', BOOLEAN_TYPE), TypedSymbol)
    array_type = ArrayType(REAL_SINGLE_TYPE, [ArrayType.Extent.ATTRIBUTE])
    assert isinstance(TSymbol('a', array_type), TypedSymbol)

    array_type = ArrayType(REAL_SINGLE_TYPE, [3])
    assert isinstance(TSymbol('a', array_type), TypedSymbol)
    array_type = ArrayType(REAL_SINGLE_TYPE, [3, 6])
    assert isinstance(TSymbol('a', array_type), TypedSymbol)
    assert isinstance(TSymbol('a', REAL_SINGLE_TYPE), TypedSymbol)
    assert isinstance(TSymbol('a', REAL8_TYPE), TypedSymbol)
    dim = DataSymbol('dim', INTEGER_SINGLE_TYPE,
                     interface=UnresolvedInterface())
    array_type = ArrayType(REAL_SINGLE_TYPE, [Reference(dim)])
    assert isinstance(TSymbol('a', array_type), TypedSymbol)
    array_type = ArrayType(REAL_SINGLE_TYPE,
                           [ArrayType.Extent.ATTRIBUTE,
                            ArrayType.Extent.ATTRIBUTE])
    assert isinstance(TSymbol('a', array_type), TypedSymbol)
    assert isinstance(TSymbol('field', DataTypeSymbol("field_type",
                                                      UnresolvedType())),
                      TypedSymbol)


def test_typed_symbol_init_errors():
    ''' Test that the TypedSymbol constructor raises appropriate errors if
    supplied with invalid arguments. '''

    with pytest.raises(TypeError) as error:
        TSymbol('a', 'invalidtype')
    assert ("datatype of a TSymbol must be specified using either a "
            "DataType or a DataTypeSymbol but got: 'str'" in str(error.value))

    with pytest.raises(TypeError) as error:
        TSymbol('a', 3)
    assert ("datatype of a TSymbol must be specified using either a "
            "DataType or a DataTypeSymbol but got:" in str(error.value))


def test_typedsymbol_specialise_and_process_arguments():
    ''' Tests that a TypedSymbol created from a specialisation instead of
    the constructor deals with the arguments as expected.'''

    # Try to make a TSymbol without a datatype
    sym1 = Symbol("symbol1")
    with pytest.raises(AttributeError) as error:
        sym1.specialise(TSymbol)
    assert "Missing mandatory 'datatype' attribute" in str(error.value)

    # Include a datatype
    sym2 = Symbol("symbol2")
    sym2.specialise(TSymbol, datatype=REAL_SINGLE_TYPE)
    assert sym2.datatype is REAL_SINGLE_TYPE


def test_typed_symbol_scalar_array():
    '''Test that the TypedSymbol property is_scalar returns True if the
    TypedSymbol is a scalar and False if not and that the TypedSymbol property
    is_array returns True if the TypedSymbol is an array and False if not.

    '''
    sym1 = DataSymbol("s1", INTEGER_SINGLE_TYPE,
                      interface=UnresolvedInterface())
    array_type = ArrayType(REAL_SINGLE_TYPE, [2, Reference(sym1)])
    sym2 = TSymbol("s2", array_type)
    assert sym1.is_scalar
    assert not sym1.is_array
    assert not sym2.is_scalar
    assert sym2.is_array


def test_typed_symbol_copy():
    '''Test that the TypedSymbol copy method produces a faithful separate copy
    of the original symbol.

    '''
    array_type = ArrayType(REAL_SINGLE_TYPE, [1, 2])
    symbol = TSymbol("myname", array_type)
    new_symbol = symbol.copy()

    # Check the new symbol has the same properties as the original
    assert symbol.name == new_symbol.name
    assert symbol.datatype == new_symbol.datatype
    assert symbol.shape == new_symbol.shape

    # Change the properties of the new symbol and check the original
    # is not affected.
    new_symbol._name = "new"
    new_symbol.datatype = ArrayType(ScalarType(ScalarType.Intrinsic.INTEGER,
                                               ScalarType.Precision.DOUBLE),
                                    [3, 4])

    assert symbol.name == "myname"
    assert symbol.datatype.intrinsic == ScalarType.Intrinsic.REAL
    assert symbol.datatype.precision == ScalarType.Precision.SINGLE
    assert len(symbol.shape) == 2
    assert isinstance(symbol.shape[0].lower, Literal)
    assert isinstance(symbol.shape[0].upper, Literal)
    assert symbol.shape[0].lower.value == "1"
    assert symbol.shape[0].upper.value == "1"
    assert (symbol.shape[0].upper.datatype.intrinsic ==
            ScalarType.Intrinsic.INTEGER)
    assert (symbol.shape[0].upper.datatype.precision ==
            ScalarType.Precision.UNDEFINED)
    assert isinstance(symbol.shape[1].lower, Literal)
    assert isinstance(symbol.shape[1].upper, Literal)
    assert symbol.shape[1].lower.value == "1"
    assert symbol.shape[1].upper.value == "2"
    assert (symbol.shape[1].upper.datatype.intrinsic ==
            ScalarType.Intrinsic.INTEGER)
    assert (symbol.shape[1].upper.datatype.precision ==
            ScalarType.Precision.UNDEFINED)


def test_typed_symbol_copy_properties():
    ''' Check that the copy_properties() method works as expected. '''
    array_type = ArrayType(REAL_SINGLE_TYPE, [1, 2])
    symbol = TSymbol("myname", array_type)
    new_sym = TSymbol("new_name", INTEGER_SINGLE_TYPE)
    new_sym.copy_properties(symbol)
    # Name should be unchanged
    assert new_sym.name == "new_name"
    assert new_sym.datatype == array_type
    with pytest.raises(TypeError) as err:
        new_sym.copy_properties(INTEGER_SINGLE_TYPE)
    assert ("Argument should be of type 'TypedSymbol' but found 'ScalarType'"
            in str(err.value))


def test_typed_symbol_resolve_type(monkeypatch):
    ''' Test the TypedSymbol resolve_type method '''
    symbola = TSymbol('a', INTEGER_SINGLE_TYPE)
    new_sym = symbola.resolve_type()
    # For a TypedSymbol (unlike a Symbol), resolve_type should always
    # return the object on which it was called.
    assert new_sym is symbola
    module = ContainerSymbol("dummy_module")
    symbolb = TSymbol('b', visibility=Symbol.Visibility.PRIVATE,
                      datatype=UnresolvedType(),
                      interface=ImportInterface(module))
    # Monkeypatch the get_external_symbol() method so that it just returns
    # a new DataSymbol
    monkeypatch.setattr(symbolb, "get_external_symbol",
                        lambda: TSymbol("b", INTEGER_SINGLE_TYPE))
    new_sym = symbolb.resolve_type()
    assert new_sym is symbolb
    assert new_sym.datatype == INTEGER_SINGLE_TYPE
    assert new_sym.visibility == Symbol.Visibility.PRIVATE
    assert isinstance(new_sym.interface, ImportInterface)


def test_typed_symbol_shape():
    ''' Test that shape returns [] if the symbol is a scalar.'''
    data_symbol = TSymbol("a", REAL4_TYPE)
    assert data_symbol.shape == []
