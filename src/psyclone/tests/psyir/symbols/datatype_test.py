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
# Authors: R. W. Ford, A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Perform py.test tests on the psygen.psyir.symbols.datatype module '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.symbols import DataType, DeferredType, ScalarType, \
    ArrayType, UnknownFortranType, DataSymbol, StructureType, \
    INTEGER_TYPE, REAL_TYPE, Symbol, TypeSymbol, SymbolTable
from psyclone.psyir.nodes import Literal, BinaryOperation, Reference, \
    Container, KernelSchedule
from psyclone.errors import InternalError


# Abstract DataType class

def test_datatype():
    '''Test that the DataType class can't be created.'''
    # pylint: disable=abstract-class-instantiated
    with pytest.raises(TypeError) as excinfo:
        _ = DataType()
    assert ("Can't instantiate abstract class DataType with abstract methods "
            "__str__" in str(excinfo.value))


# DeferredType class

def test_deferredtype():
    '''Test that the DeferredType class can be created successfully.'''
    assert isinstance(DeferredType(), DeferredType)


def test_deferredtype_str():
    '''Test that the DeferredType class str method works as expected.'''
    data_type = DeferredType()
    assert str(data_type) == "DeferredType"


# ScalarType class

@pytest.mark.parametrize("precision", [ScalarType.Precision.SINGLE,
                                       ScalarType.Precision.DOUBLE,
                                       ScalarType.Precision.UNDEFINED])
@pytest.mark.parametrize("intrinsic", [ScalarType.Intrinsic.INTEGER,
                                       ScalarType.Intrinsic.REAL,
                                       ScalarType.Intrinsic.BOOLEAN,
                                       ScalarType.Intrinsic.CHARACTER])
def test_scalartype_enum_precision(intrinsic, precision):
    '''Test that the ScalarType class can be created successfully for all
    supported ScalarType intrinsics and all suported enumerated precisions.

    '''
    scalar_type = ScalarType(intrinsic, precision)
    assert isinstance(scalar_type, ScalarType)
    assert scalar_type.intrinsic == intrinsic
    assert scalar_type.precision == precision


@pytest.mark.parametrize("precision", [1, 8, 16])
@pytest.mark.parametrize("intrinsic", [ScalarType.Intrinsic.INTEGER,
                                       ScalarType.Intrinsic.REAL,
                                       ScalarType.Intrinsic.BOOLEAN,
                                       ScalarType.Intrinsic.CHARACTER])
def test_scalartype_int_precision(intrinsic, precision):
    '''Test that the ScalarType class can be created successfully for all
    supported ScalarType intrinsics and a set of valid integer precisions.

    '''
    scalar_type = ScalarType(intrinsic, precision)
    assert isinstance(scalar_type, ScalarType)
    assert scalar_type.intrinsic == intrinsic
    assert scalar_type.precision == precision


@pytest.mark.parametrize("intrinsic", [ScalarType.Intrinsic.INTEGER,
                                       ScalarType.Intrinsic.REAL,
                                       ScalarType.Intrinsic.BOOLEAN,
                                       ScalarType.Intrinsic.CHARACTER])
def test_scalartype_datasymbol_precision(intrinsic):
    '''Test that the ScalarType class can be created successfully for all
    supported ScalarType intrinsics and the precision specified by another
    symbol.

    '''
    # Create an r_def precision symbol with a constant value of 8
    data_type = ScalarType(ScalarType.Intrinsic.INTEGER,
                           ScalarType.Precision.UNDEFINED)
    precision_symbol = DataSymbol("r_def", data_type, constant_value=8)
    # Set the precision of our ScalarType to be the precision symbol
    scalar_type = ScalarType(intrinsic, precision_symbol)
    assert isinstance(scalar_type, ScalarType)
    assert scalar_type.intrinsic == intrinsic
    assert scalar_type.precision is precision_symbol


def test_scalartype_invalid_intrinsic_type():
    '''Test that the ScalarType class raises an exception when an invalid
    intrinsic type is provided.

    '''
    with pytest.raises(TypeError) as excinfo:
        _ = ScalarType(None, None)
    assert ("ScalarType expected 'intrinsic' argument to be of type "
            "ScalarType.Intrinsic but found 'NoneType'." in str(excinfo.value))


def test_scalartype_invalid_precision_type():
    '''Test that the ScalarType class raises an exception when an invalid
    precision type is provided.

    '''
    with pytest.raises(TypeError) as excinfo:
        _ = ScalarType(ScalarType.Intrinsic.INTEGER, None)
    assert ("ScalarType expected 'precision' argument to be of type int, "
            "ScalarType.Precision or DataSymbol, but found 'NoneType'."
            in str(excinfo.value))


def test_scalartype_invalid_precision_int_value():
    '''Test that the ScalarType class raises an exception when an invalid
    integer precision value is provided.

    '''
    with pytest.raises(ValueError) as excinfo:
        _ = ScalarType(ScalarType.Intrinsic.INTEGER, 0)
    assert ("The precision of a DataSymbol when specified as an integer "
            "number of bytes must be > 0 but found '0'."
            in str(excinfo.value))


def test_scalartype_invalid_precision_datasymbol():
    '''Test that the ScalarType class raises an exception when an invalid
    precision symbol is provided (it must be a scalar integer or
    deferred).

    '''
    # Create an r_def precision symbol with a constant value of 8
    data_type = ScalarType(ScalarType.Intrinsic.REAL, 4)
    precision_symbol = DataSymbol("r_def", data_type)
    with pytest.raises(ValueError) as excinfo:
        _ = ScalarType(ScalarType.Intrinsic.REAL, precision_symbol)
    assert ("A DataSymbol representing the precision of another DataSymbol "
            "must be of either 'deferred' or scalar, integer type but got: "
            "r_def: <Scalar<REAL, 4>, Local>"
            in str(excinfo.value))


def test_scalartype_str():
    '''Test that the ScalarType class str method works as expected.'''
    data_type = ScalarType(ScalarType.Intrinsic.BOOLEAN,
                           ScalarType.Precision.UNDEFINED)
    assert str(data_type) == "Scalar<BOOLEAN, UNDEFINED>"


def test_scalartype_immutable():
    '''Test that the scalartype attributes can't be modified'''
    data_type = ScalarType(ScalarType.Intrinsic.REAL, 4)
    with pytest.raises(AttributeError):
        data_type.intrinsic = ScalarType.Intrinsic.INTEGER
    with pytest.raises(AttributeError):
        data_type.precision = 8


# ArrayType class
def test_arraytype():
    '''Test that the ArrayType class __init__ works as expected. Test the
    different dimension datatypes that are supported.'''
    scalar_type = ScalarType(ScalarType.Intrinsic.INTEGER, 4)
    data_symbol = DataSymbol("var", scalar_type, constant_value=30)
    one = Literal("1", scalar_type)
    var_plus_1 = BinaryOperation.create(
        BinaryOperation.Operator.ADD, Reference(data_symbol), one)
    literal = Literal("20", scalar_type)
    array_type = ArrayType(
        scalar_type, [10, literal, var_plus_1, Reference(data_symbol),
                      ArrayType.Extent.DEFERRED, ArrayType.Extent.ATTRIBUTE])
    assert isinstance(array_type, ArrayType)
    assert len(array_type.shape) == 6
    # Provided as an int but stored as a Literal
    shape0 = array_type.shape[0]
    assert isinstance(shape0, Literal)
    assert shape0.value == "10"
    assert shape0.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert shape0.datatype.precision == ScalarType.Precision.UNDEFINED
    # Provided and stored as a Literal (DataNode)
    assert array_type.shape[1] is literal
    # Provided and stored as an Operator (DataNode)
    assert array_type.shape[2] is var_plus_1
    # Provided and stored as a Reference to a DataSymbol
    assert isinstance(array_type.shape[3], Reference)
    assert array_type.shape[3].symbol is data_symbol
    # Provided and stored as a deferred extent
    assert array_type.shape[4] == ArrayType.Extent.DEFERRED
    # Provided as an attribute extent
    assert array_type.shape[5] == ArrayType.Extent.ATTRIBUTE


def test_arraytype_invalid_datatype():
    '''Test that the ArrayType class raises an exception when the datatype
    argument is the wrong type.

    '''
    with pytest.raises(TypeError) as excinfo:
        _ = ArrayType(None, None)
    assert ("ArrayType expected 'datatype' argument to be of type DataType "
            "or TypeSymbol but found 'NoneType'." in str(excinfo.value))


def test_arraytype_typesymbol_only():
    ''' Test that we currently refuse to make an ArrayType with an intrinsic
    type of StructureType. (This limitation is the subject of #1031.) '''
    with pytest.raises(NotImplementedError) as err:
        _ = ArrayType(StructureType.create(
            [("nx", INTEGER_TYPE, Symbol.Visibility.PUBLIC)]),
                      [5])
    assert ("When creating an array of structures, the type of those "
            "structures must be supplied as a TypeSymbol but got a "
            "StructureType instead." in str(err.value))


def test_arraytype_typesymbol():
    ''' Test that we can correctly create an ArrayType when the type of the
    elements is specified as a TypeSymbol. '''
    tsym = TypeSymbol("my_type", DeferredType())
    atype = ArrayType(tsym, [5])
    assert isinstance(atype, ArrayType)
    assert len(atype.shape) == 1
    assert atype.intrinsic is tsym
    assert atype.precision is None


def test_arraytype_invalid_shape():
    '''Test that the ArrayType class raises an exception when the shape
    argument is the wrong type.

    '''
    scalar_type = ScalarType(ScalarType.Intrinsic.REAL, 4)
    with pytest.raises(TypeError) as excinfo:
        _ = ArrayType(scalar_type, None)
    assert ("ArrayType 'shape' must be of type list but "
            "found 'NoneType'." in str(excinfo.value))


def test_arraytype_invalid_shape_dimension_1():
    '''Test that the ArrayType class raises an exception when one of the
    dimensions of the shape list argument is a datasymbol but is not a
    scalar integer.

    '''
    scalar_type = ScalarType(ScalarType.Intrinsic.REAL, 4)
    symbol = DataSymbol("fred", scalar_type, constant_value=3.0)
    with pytest.raises(TypeError) as excinfo:
        _ = ArrayType(scalar_type, [Reference(symbol)])
    assert (
        "If a datasymbol is used as a dimension declaration then it should "
        "be a scalar integer or an unknown type, but 'fred' is a "
        "'Scalar<REAL, 4>'." in str(excinfo.value))


def test_arraytype_invalid_shape_dimension_2():
    '''Test that the ArrayType class raises an exception when one of the
    dimensions of the shape list argument is not a datasymbol, datanode,
    integer or ArrayType.Extent type.

    '''
    scalar_type = ScalarType(ScalarType.Intrinsic.REAL, 4)
    with pytest.raises(TypeError) as excinfo:
        _ = ArrayType(scalar_type, [None])
    assert ("DataSymbol shape list elements can only be 'int', "
            "ArrayType.Extent or 'DataNode' but found 'NoneType'."
            in str(excinfo.value))


@pytest.mark.xfail(reason="issue #1089. Support for this check needs to be"
                   "implemented")
def test_arraytype_invalid_shape_dimension_3():
    '''Test that the ArrayType class raises an exception when one of the
    dimensions of the shape list argument is a DataNode that contains
    a local datasymbol that does not have a constant value (as this
    will not be initialised).

    '''
    scalar_type = ScalarType(ScalarType.Intrinsic.INTEGER, 4)
    data_symbol = DataSymbol("var", scalar_type)
    one = Literal("1", scalar_type)
    var_plus_1 = BinaryOperation.create(
        BinaryOperation.Operator.ADD, Reference(data_symbol), one)
    with pytest.raises(TypeError) as info:
        _ = ArrayType(scalar_type, [var_plus_1])
    assert ("If a local datasymbol is used as part of a dimension "
            "declaration then it should be a constant, but 'var' is "
            "not." in str(info.value))


def test_arraytype_shape_dim_from_parent_scope():
    ''' Check that the shape checking in the ArrayType class permits the
    use of a reference to a symbol in a parent scope. '''
    cont = Container("test_mod")
    dim_sym = cont.symbol_table.new_symbol("dim1", symbol_type=DataSymbol,
                                           datatype=INTEGER_TYPE)
    kernel1 = KernelSchedule.create("mod_1", SymbolTable(), [])
    cont.addchild(kernel1)
    asym = kernel1.symbol_table.new_symbol(
        "array1", symbol_type=DataSymbol,
        datatype=ArrayType(INTEGER_TYPE, [Reference(dim_sym)]))
    assert isinstance(asym, DataSymbol)


def test_arraytype_str():
    '''Test that the ArrayType class str method works as expected.'''
    scalar_type = ScalarType(ScalarType.Intrinsic.INTEGER,
                             ScalarType.Precision.UNDEFINED)
    data_symbol = DataSymbol("var", scalar_type, constant_value=20)
    data_type = ArrayType(scalar_type, [10, Reference(data_symbol),
                                        ArrayType.Extent.DEFERRED,
                                        ArrayType.Extent.ATTRIBUTE])
    assert (str(data_type) == "Array<Scalar<INTEGER, UNDEFINED>,"
            " shape=[Literal[value:'10', Scalar<INTEGER, UNDEFINED>], "
            "Reference[name:'var'], 'DEFERRED', 'ATTRIBUTE']>")


def test_arraytype_str_invalid():
    '''Test that the ArrayType class str method raises an exception if an
    unsupported dimension type is found.

    '''
    scalar_type = ScalarType(ScalarType.Intrinsic.INTEGER, 4)
    array_type = ArrayType(scalar_type, [10])
    # Make one of the array dimensions an unsupported type
    array_type._shape = [None]
    with pytest.raises(InternalError) as excinfo:
        _ = str(array_type)
    assert ("PSyclone internal error: ArrayType shape list elements can only "
            "be 'DataNode', or 'ArrayType.Extent', but found 'NoneType'."
            in str(excinfo.value))


def test_arraytype_immutable():
    '''Test that the scalartype attributes can't be modified'''
    scalar_type = ScalarType(ScalarType.Intrinsic.REAL, 4)
    data_type = ArrayType(scalar_type, [10, 10])
    with pytest.raises(AttributeError):
        data_type.intrinsic = ScalarType.Intrinsic.INTEGER
    with pytest.raises(AttributeError):
        data_type.precision = 8
    with pytest.raises(AttributeError):
        data_type.shape = []


def test_unknown_fortran_type():
    ''' Check the constructor and 'declaration' property of the
    UnknownFortranType class. '''
    with pytest.raises(TypeError) as err:
        UnknownFortranType(1)
    assert ("constructor expects the original variable declaration as a "
            "string but got an argument of type 'int'" in str(err.value))
    decl = "type(some_type) :: var"
    utype = UnknownFortranType(decl)
    assert str(utype) == "UnknownFortranType('" + decl + "')"
    assert utype.declaration == decl


# StructureType tests

def test_structure_type():
    ''' Check the StructureType constructor and that we can add components. '''
    stype = StructureType()
    assert str(stype) == "StructureType<>"
    assert not stype.components
    stype.add("flag", INTEGER_TYPE, Symbol.Visibility.PUBLIC)
    flag = stype.lookup("flag")
    assert isinstance(flag, StructureType.ComponentType)
    with pytest.raises(TypeError) as err:
        stype.add(1, "hello", "hello")
    assert ("name of a component of a StructureType must be a 'str' but got "
            "'int'" in str(err.value))
    with pytest.raises(TypeError) as err:
        stype.add("hello", "hello", "hello")
    assert ("type of a component of a StructureType must be a 'DataType' "
            "or 'TypeSymbol' but got 'str'" in str(err.value))
    with pytest.raises(TypeError) as err:
        stype.add("hello", INTEGER_TYPE, "hello")
    assert ("visibility of a component of a StructureType must be an instance "
            "of 'Symbol.Visibility' but got 'str'" in str(err.value))
    with pytest.raises(KeyError):
        stype.lookup("missing")
    # Cannot have a recursive type definition
    with pytest.raises(TypeError) as err:
        stype.add("hello", stype, Symbol.Visibility.PUBLIC)
    assert ("attempting to add component 'hello' - a StructureType definition "
            "cannot be recursive" in str(err.value))


def test_create_structuretype():
    ''' Test the create() method of StructureType. '''
    # One member will have its type defined by a TypeSymbol
    tsymbol = TypeSymbol("my_type", DeferredType())
    stype = StructureType.create([
        ("fred", INTEGER_TYPE, Symbol.Visibility.PUBLIC),
        ("george", REAL_TYPE, Symbol.Visibility.PRIVATE),
        ("barry", tsymbol, Symbol.Visibility.PUBLIC)])
    assert len(stype.components) == 3
    george = stype.lookup("george")
    assert isinstance(george, StructureType.ComponentType)
    assert george.name == "george"
    assert george.datatype == REAL_TYPE
    assert george.visibility == Symbol.Visibility.PRIVATE
    barry = stype.lookup("barry")
    assert isinstance(barry, StructureType.ComponentType)
    assert barry.datatype is tsymbol
    assert barry.visibility == Symbol.Visibility.PUBLIC
    with pytest.raises(TypeError) as err:
        StructureType.create([
            ("fred", INTEGER_TYPE, Symbol.Visibility.PUBLIC),
            ("george", Symbol.Visibility.PRIVATE)])
    assert ("Each component must be specified using a 3-tuple of (name, "
            "type, visibility) but found a tuple with 2 members: ("
            "'george', " in str(err.value))
