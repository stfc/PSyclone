# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2022, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' Perform py.test tests on the psygen.psyir.symbols.datasymbols file '''

from __future__ import absolute_import
import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003

from psyclone.psyir.symbols import DataSymbol, ContainerSymbol, \
    LocalInterface, ImportInterface, ArgumentInterface, UnresolvedInterface, \
    ScalarType, ArrayType, REAL_SINGLE_TYPE, REAL_DOUBLE_TYPE, REAL4_TYPE, \
    REAL8_TYPE, INTEGER_SINGLE_TYPE, INTEGER_DOUBLE_TYPE, INTEGER4_TYPE, \
    BOOLEAN_TYPE, CHARACTER_TYPE, DeferredType, Symbol, DataTypeSymbol
from psyclone.psyir.nodes import (Literal, Reference, BinaryOperation, Return,
                                  CodeBlock)


def test_datasymbol_initialisation():
    '''Test that a DataSymbol instance can be created when valid arguments are
    given, otherwise raise relevant exceptions.'''

    # Test with valid arguments
    assert isinstance(DataSymbol('a', REAL_SINGLE_TYPE), DataSymbol)
    assert isinstance(DataSymbol('a', REAL_DOUBLE_TYPE), DataSymbol)
    assert isinstance(DataSymbol('a', REAL4_TYPE), DataSymbol)
    kind = DataSymbol('r_def', INTEGER_SINGLE_TYPE)
    real_kind_type = ScalarType(ScalarType.Intrinsic.REAL, kind)
    assert isinstance(DataSymbol('a', real_kind_type),
                      DataSymbol)
    # real constants are not currently supported
    assert isinstance(DataSymbol('a', INTEGER_SINGLE_TYPE), DataSymbol)
    assert isinstance(DataSymbol('a', INTEGER_DOUBLE_TYPE, constant_value=0),
                      DataSymbol)
    assert isinstance(DataSymbol('a', INTEGER4_TYPE),
                      DataSymbol)

    assert isinstance(DataSymbol('a', CHARACTER_TYPE), DataSymbol)
    assert isinstance(DataSymbol('a', CHARACTER_TYPE,
                                 constant_value="hello"), DataSymbol)
    assert isinstance(DataSymbol('a', BOOLEAN_TYPE), DataSymbol)
    assert isinstance(DataSymbol('a', BOOLEAN_TYPE,
                                 constant_value=False),
                      DataSymbol)
    array_type = ArrayType(REAL_SINGLE_TYPE, [ArrayType.Extent.ATTRIBUTE])
    assert isinstance(DataSymbol('a', array_type), DataSymbol)

    array_type = ArrayType(REAL_SINGLE_TYPE, [3])
    assert isinstance(DataSymbol('a', array_type), DataSymbol)
    array_type = ArrayType(REAL_SINGLE_TYPE, [3, ArrayType.Extent.ATTRIBUTE])
    assert isinstance(DataSymbol('a', array_type), DataSymbol)
    assert isinstance(DataSymbol('a', REAL_SINGLE_TYPE), DataSymbol)
    assert isinstance(DataSymbol('a', REAL8_TYPE), DataSymbol)
    dim = DataSymbol('dim', INTEGER_SINGLE_TYPE,
                     interface=UnresolvedInterface())
    array_type = ArrayType(REAL_SINGLE_TYPE, [Reference(dim)])
    assert isinstance(DataSymbol('a', array_type), DataSymbol)
    array_type = ArrayType(REAL_SINGLE_TYPE,
                           [3, Reference(dim), ArrayType.Extent.ATTRIBUTE])
    assert isinstance(DataSymbol('a', array_type), DataSymbol)
    assert isinstance(
        DataSymbol('a', REAL_SINGLE_TYPE,
                   interface=ArgumentInterface(
                       ArgumentInterface.Access.READWRITE)), DataSymbol)
    assert isinstance(
        DataSymbol('a', REAL_SINGLE_TYPE,
                   visibility=Symbol.Visibility.PRIVATE), DataSymbol)
    assert isinstance(DataSymbol('field', DataTypeSymbol("field_type",
                                                         DeferredType())),
                      DataSymbol)


def test_datasymbol_specialise_and_process_arguments():
    ''' Tests that a DataSymbol created from a specialisation instead of
    the constructor deals with the arguments as expected.'''

    # Try to make a DataSymbol without a datatype
    sym1 = Symbol("symbol1")
    with pytest.raises(AttributeError) as error:
        sym1.specialise(DataSymbol)
    assert "Missing mandatory 'datatype' attribute" in str(error.value)

    # Include a datatype
    sym2 = Symbol("symbol2")
    sym2.specialise(DataSymbol, datatype=REAL_SINGLE_TYPE)
    assert sym2.datatype is REAL_SINGLE_TYPE
    assert sym2.constant_value is None

    # Include a constant_value
    sym3 = Symbol("symbol3")
    sym3.specialise(DataSymbol, datatype=REAL_SINGLE_TYPE,
                    constant_value=3.14)
    assert sym3.datatype is REAL_SINGLE_TYPE
    assert isinstance(sym3.constant_value, Literal)
    assert sym3.constant_value.value == '3.14'

    # Include a constant_value of the wrong type
    sym4 = Symbol("symbol4")
    with pytest.raises(ValueError) as error:
        sym4.specialise(DataSymbol, datatype=INTEGER_SINGLE_TYPE,
                        constant_value=3.14)
    assert ("This DataSymbol instance datatype is 'Scalar<INTEGER, SINGLE>' "
            "meaning the constant value should be"
            in str(error.value))


def test_datasymbol_can_be_printed():
    '''Test that a DataSymbol instance can always be printed. (i.e. is
    initialised fully.)'''
    symbol = DataSymbol("sname", REAL_SINGLE_TYPE)
    assert "sname: DataSymbol<Scalar<REAL, SINGLE>, Local>" in str(symbol)

    sym1 = DataSymbol("s1", INTEGER_SINGLE_TYPE,
                      interface=UnresolvedInterface())
    assert "s1: DataSymbol<Scalar<INTEGER, SINGLE>, Unresolved>" in str(sym1)

    array_type = ArrayType(REAL_SINGLE_TYPE,
                           [ArrayType.Extent.ATTRIBUTE, 2, Reference(sym1)])
    sym2 = DataSymbol("s2", array_type)
    assert ("s2: DataSymbol<Array<Scalar<REAL, SINGLE>, shape=['ATTRIBUTE', "
            "2, Reference[name:'s1']]>, Local>" in str(sym2))

    my_mod = ContainerSymbol("my_mod")
    sym3 = DataSymbol("s3", REAL_SINGLE_TYPE,
                      interface=ImportInterface(my_mod))
    assert ("s3: DataSymbol<Scalar<REAL, SINGLE>, Import(container='my_mod')>"
            in str(sym3))

    sym3 = DataSymbol("s3", INTEGER_SINGLE_TYPE, constant_value=12)
    assert ("s3: DataSymbol<Scalar<INTEGER, SINGLE>, Local, "
            "constant_value=Literal"
            "[value:'12', Scalar<INTEGER, SINGLE>]>" in str(sym3))

    sym4 = DataSymbol("s4", INTEGER_SINGLE_TYPE,
                      interface=UnresolvedInterface())
    assert "s4: DataSymbol<Scalar<INTEGER, SINGLE>, Unresolved>" in str(sym4)


def test_datasymbol_constant_value_setter():
    '''Test that a DataSymbol constant value can be set if given a new valid
    constant value.'''

    # Test with valid constant values
    sym = DataSymbol('a', INTEGER_SINGLE_TYPE, constant_value=7)
    assert sym.constant_value.value == "7"
    sym.constant_value = 9
    assert sym.constant_value.value == "9"

    sym = DataSymbol('a', REAL_SINGLE_TYPE, constant_value=3.1415)
    assert sym.constant_value.value == "3.1415"
    sym.constant_value = 1.0
    assert sym.constant_value.value == "1.0"

    sym = DataSymbol('a', BOOLEAN_TYPE, constant_value=True)
    assert sym.constant_value.value == "true"
    sym.constant_value = False
    assert sym.constant_value.value == "false"

    # Test with valid constant expressions
    lhs = Literal('2', INTEGER_SINGLE_TYPE)
    rhs = Reference(DataSymbol('constval', INTEGER_SINGLE_TYPE))
    ct_expr = BinaryOperation.create(BinaryOperation.Operator.ADD, lhs, rhs)
    sym = DataSymbol('a', INTEGER_SINGLE_TYPE, constant_value=ct_expr)
    assert isinstance(sym.constant_value, BinaryOperation)
    assert sym.constant_value is ct_expr

    # Test setting it back to non-constant
    sym.constant_value = None
    assert sym.constant_value is None


def test_datasymbol_constant_value_setter_invalid():
    '''Test that a DataSymbol constant value setter raises the appropriate
    error if an invalid value and/or datatype are given.'''

    # Test with invalid constant values
    sym = DataSymbol('a', DeferredType())
    with pytest.raises(ValueError) as error:
        sym.constant_value = 1.0
    assert ("Error setting constant value for symbol 'a'. A DataSymbol with "
            "a constant value must be a scalar or an array but found "
            "'DeferredType'." in str(error.value))

    # Test with invalid constant expressions
    ct_expr = Return()
    with pytest.raises(ValueError) as error:
        _ = DataSymbol('a', INTEGER_SINGLE_TYPE, constant_value=ct_expr)
    assert ("Error setting constant value for symbol 'a'. PSyIR static "
            "expressions can only contain PSyIR Literal, Operation, Reference "
            "or CodeBlock nodes but found:" in str(error.value))

    with pytest.raises(ValueError) as error:
        DataSymbol('a', INTEGER_SINGLE_TYPE, interface=ArgumentInterface(),
                   constant_value=9)
    assert ("Error setting constant value for symbol 'a'. A DataSymbol with "
            "an ArgumentInterface can not have a constant value."
            in str(error.value))

    with pytest.raises(ValueError) as error:
        DataSymbol('a', INTEGER_SINGLE_TYPE, constant_value=9.81)
    assert ("Error setting constant value for symbol 'a'. This DataSymbol "
            "instance datatype is 'Scalar<INTEGER, SINGLE>' meaning "
            "the constant value should be") in str(error.value)
    assert "'int'>' but found " in str(error.value)
    assert "'float'>'." in str(error.value)

    with pytest.raises(ValueError) as error:
        DataSymbol('a', CHARACTER_TYPE, constant_value=42)
    assert ("Error setting constant value for symbol 'a'. This DataSymbol "
            "instance datatype is 'Scalar<CHARACTER, UNDEFINED>' meaning "
            "the constant value should be") in str(error.value)
    assert "'str'>' but found " in str(error.value)
    assert "'int'>'." in str(error.value)

    with pytest.raises(ValueError) as error:
        DataSymbol('a', BOOLEAN_TYPE, constant_value="hello")
    assert ("Error setting constant value for symbol 'a'. This DataSymbol "
            "instance datatype is 'Scalar<BOOLEAN, UNDEFINED>' meaning "
            "the constant value should be") in str(error.value)
    assert "'bool'>' but found " in str(error.value)
    assert "'str'>'." in str(error.value)


def test_datasymbol_is_constant():
    '''Test that the DataSymbol is_constant property returns True if a
    constant value is set and False if it is not.

    '''
    sym = DataSymbol('a', INTEGER_SINGLE_TYPE)
    assert not sym.is_constant
    sym.constant_value = 9
    assert sym.is_constant


@pytest.mark.usefixtures("parser")
def test_datasymbol_is_constant_codeblock():
    ''' Test that a DataSymbol can have a CodeBlock as its constant value. '''
    sym = DataSymbol('a', INTEGER_SINGLE_TYPE)
    reader = FortranStringReader(
        "INTEGER, PARAMETER :: a=SELECTED_REAL_KIND(6,37)")
    fparser2spec = Fortran2003.Specification_Part(reader).children[0]
    # We want the first child of the Initialization node in the parse tree as
    # the basis for our CodeBlock
    inits = Fortran2003.walk(fparser2spec, Fortran2003.Initialization)
    cblock = CodeBlock([inits[0].children[1]], CodeBlock.Structure.EXPRESSION)
    assert not sym.is_constant
    sym.constant_value = cblock
    assert sym.is_constant


def test_datasymbol_scalar_array():
    '''Test that the DataSymbol property is_scalar returns True if the
    DataSymbol is a scalar and False if not and that the DataSymbol property
    is_array returns True if the DataSymbol is an array and False if not.

    '''
    sym1 = DataSymbol("s1", INTEGER_SINGLE_TYPE,
                      interface=UnresolvedInterface())
    array_type = ArrayType(REAL_SINGLE_TYPE,
                           [ArrayType.Extent.ATTRIBUTE, 2, Reference(sym1)])
    sym2 = DataSymbol("s2", array_type)
    assert sym1.is_scalar
    assert not sym1.is_array
    assert not sym2.is_scalar
    assert sym2.is_array


def test_datasymbol_copy():
    '''Test that the DataSymbol copy method produces a faithful separate copy
    of the original symbol.

    '''
    array_type = ArrayType(REAL_SINGLE_TYPE, [1, 2])
    symbol = DataSymbol("myname", array_type, constant_value=None,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.READWRITE))
    new_symbol = symbol.copy()

    # Check the new symbol has the same properties as the original
    assert symbol.name == new_symbol.name
    assert symbol.datatype == new_symbol.datatype
    assert symbol.shape == new_symbol.shape
    assert symbol.constant_value == new_symbol.constant_value
    assert symbol.interface == new_symbol.interface

    # Change the properties of the new symbol and check the original
    # is not affected. Can't check constant_value yet as we have a
    # shape value
    new_symbol._name = "new"
    new_symbol.datatype = ArrayType(ScalarType(ScalarType.Intrinsic.INTEGER,
                                               ScalarType.Precision.DOUBLE),
                                    [3, 4])
    new_symbol._interface = LocalInterface()

    assert symbol.name == "myname"
    assert symbol.datatype.intrinsic == ScalarType.Intrinsic.REAL
    assert symbol.datatype.precision == ScalarType.Precision.SINGLE
    assert len(symbol.shape) == 2
    assert isinstance(symbol.shape[0], ArrayType.ArrayBounds)
    assert isinstance(symbol.shape[0].lower, Literal)
    assert isinstance(symbol.shape[0].upper, Literal)
    assert symbol.shape[0].lower.value == "1"
    assert symbol.shape[0].upper.value == "1"
    assert (symbol.shape[0].upper.datatype.intrinsic ==
            ScalarType.Intrinsic.INTEGER)
    assert (symbol.shape[0].upper.datatype.precision ==
            ScalarType.Precision.UNDEFINED)
    assert isinstance(symbol.shape[1], ArrayType.ArrayBounds)
    assert isinstance(symbol.shape[1].lower, Literal)
    assert isinstance(symbol.shape[1].upper, Literal)
    assert symbol.shape[1].lower.value == "1"
    assert symbol.shape[1].upper.value == "2"
    assert (symbol.shape[1].upper.datatype.intrinsic ==
            ScalarType.Intrinsic.INTEGER)
    assert (symbol.shape[1].upper.datatype.precision ==
            ScalarType.Precision.UNDEFINED)
    assert not symbol.constant_value

    # Now check constant_value
    new_symbol.constant_value = 3

    assert isinstance(symbol.shape[0].upper, Literal)
    assert symbol.shape[0].upper.value == "1"
    assert (symbol.shape[0].upper.datatype.intrinsic ==
            ScalarType.Intrinsic.INTEGER)
    assert (symbol.shape[0].upper.datatype.precision ==
            ScalarType.Precision.UNDEFINED)
    assert isinstance(symbol.shape[1].upper, Literal)
    assert symbol.shape[1].upper.value == "2"
    assert (symbol.shape[1].upper.datatype.intrinsic ==
            ScalarType.Intrinsic.INTEGER)
    assert (symbol.shape[1].upper.datatype.precision ==
            ScalarType.Precision.UNDEFINED)
    assert not symbol.constant_value


def test_datasymbol_copy_properties():
    '''Test that the DataSymbol copy_properties method works as expected.'''
    array_type = ArrayType(REAL_SINGLE_TYPE, [1, 2])
    symbol = DataSymbol("myname", array_type, constant_value=None,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.READWRITE))

    # Check an exception is raised if an incorrect argument is passed in
    with pytest.raises(TypeError) as excinfo:
        symbol.copy_properties(None)
    assert ("Argument should be of type 'DataSymbol' but found 'NoneType'."
            "") in str(excinfo.value)

    new_symbol = DataSymbol("other_name", INTEGER_SINGLE_TYPE,
                            constant_value=7)

    symbol.copy_properties(new_symbol)

    assert symbol.name == "myname"
    assert symbol.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert symbol.datatype.precision == ScalarType.Precision.SINGLE
    assert symbol.is_local
    assert isinstance(symbol.constant_value, Literal)
    assert symbol.constant_value.value == "7"
    assert (symbol.constant_value.datatype.intrinsic ==
            symbol.datatype.intrinsic)
    assert (symbol.constant_value.datatype.precision ==
            symbol.datatype.precision)


def test_datasymbol_resolve_deferred(monkeypatch):
    ''' Test the datasymbol resolve_deferred method '''
    symbola = DataSymbol('a', INTEGER_SINGLE_TYPE)
    new_sym = symbola.resolve_deferred()
    # For a DataSymbol (unlike a Symbol), resolve_deferred should always
    # return the object on which it was called.
    assert new_sym is symbola
    module = ContainerSymbol("dummy_module")
    symbolb = DataSymbol('b', visibility=Symbol.Visibility.PRIVATE,
                         datatype=DeferredType(),
                         interface=ImportInterface(module))
    # Monkeypatch the get_external_symbol() method so that it just returns
    # a new DataSymbol
    monkeypatch.setattr(symbolb, "get_external_symbol",
                        lambda: DataSymbol("b", INTEGER_SINGLE_TYPE))
    new_sym = symbolb.resolve_deferred()
    assert new_sym is symbolb
    assert new_sym.datatype == INTEGER_SINGLE_TYPE
    assert new_sym.visibility == Symbol.Visibility.PRIVATE
    assert isinstance(new_sym.interface, ImportInterface)


def test_datasymbol_shape():
    ''' Test that shape returns [] if the symbol is a scalar.'''
    data_symbol = DataSymbol("a", REAL4_TYPE)
    assert data_symbol.shape == []


def test_datasymbol_str():
    '''Test that the DataSymbol __str__ method returns the expected string'''
    data_symbol = DataSymbol("a", INTEGER4_TYPE, constant_value=3)
    assert (data_symbol.__str__() ==
            "a: DataSymbol<Scalar<INTEGER, 4>, Local, constant_value="
            "Literal[value:'3', Scalar<INTEGER, 4>]>")
