# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2025, Science and Technology Facilities Council.
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
# Modified: S. Siso, STFC Daresbury Lab
# Modified: by J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' Perform py.test tests on the psyclone.psyir.symbols.datatype module. '''

import pytest
from psyclone.errors import InternalError
from psyclone.psyir.nodes import (
    BinaryOperation, Container, KernelSchedule,
    Literal, Reference, Routine)
from psyclone.psyir.symbols import (
    ArrayType, DataType, UnresolvedType, ScalarType, UnsupportedFortranType,
    DataSymbol, StructureType, NoType, INTEGER_TYPE, REAL_TYPE, Symbol,
    DataTypeSymbol, SymbolTable)


# Abstract DataType class

def test_datatype():
    '''Test that the DataType class can't be created.'''
    # pylint: disable=abstract-class-instantiated
    with pytest.raises(TypeError) as excinfo:
        _ = DataType()
    msg = str(excinfo.value)
    # Python >= 3.9 spots that 'method' should be singular. Prior to this it
    # was plural. Python >= 3.12 tweaks the error message yet again to mention
    # the lack of an implementation and to quote the method name.
    # We split the check to accomodate for this.
    assert "Can't instantiate abstract class DataType with" in msg
    assert "abstract method" in msg
    assert "__str__" in msg


# UnresolvedType class

def test_unresolvedtype():
    '''Test that the UnresolvedType class can be created successfully.'''
    assert isinstance(UnresolvedType(), UnresolvedType)


def test_unresolvedtype_str():
    '''Test that the UnresolvedType class str method works as expected.'''
    data_type = UnresolvedType()
    assert str(data_type) == "UnresolvedType"


def test_unresolvedtype_is_allocatable():
    '''Test that the UnresolvedType class' is_allocatable property works.'''
    data_type = UnresolvedType()
    assert data_type.is_allocatable is None


def test_unresolvedtype_eq():
    '''Test the equality operator of UnresolvedType.'''
    data_type1 = UnresolvedType()
    assert data_type1 == UnresolvedType()
    assert data_type1 != NoType()


def test_unresolvedtype_copy():
    '''Test the copy() method of UnresolvedType (which is just inherited
    from DataType).'''
    dtype1 = UnresolvedType()
    dtype2 = dtype1.copy()
    assert isinstance(dtype2, UnresolvedType)
    assert dtype2 is not dtype1


# NoType class

def test_notype():
    ''' Check that the NoType class can be instantiated successfully and
    that its str method works as expected. '''
    data_type = NoType()
    assert isinstance(data_type, NoType)
    assert str(data_type) == "NoType"


def test_notype_is_allocatable():
    ''' Check that the NoType class is not allocatable'''
    data_type = NoType()
    assert isinstance(data_type, NoType)
    assert data_type.is_allocatable is False


def test_notype_eq():
    '''Test the equality operator of NoType.'''
    notype1 = NoType()
    assert notype1 == NoType()
    assert notype1 != UnresolvedType()
    assert notype1 != ScalarType(ScalarType.Intrinsic.INTEGER,
                                 ScalarType.Precision.SINGLE)


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
    supported ScalarType intrinsics and all supported enumerated precisions.
    Also test that two such types are equal, and that is_allocatable
    works as expected.

    '''
    scalar_type = ScalarType(intrinsic, precision)
    assert isinstance(scalar_type, ScalarType)
    assert scalar_type.intrinsic == intrinsic
    assert scalar_type.precision == precision
    scalar_type2 = ScalarType(intrinsic, precision)
    assert scalar_type == scalar_type2
    assert scalar_type.is_allocatable is False


@pytest.mark.parametrize("precision", [1, 8, 16])
@pytest.mark.parametrize("intrinsic", [ScalarType.Intrinsic.INTEGER,
                                       ScalarType.Intrinsic.REAL,
                                       ScalarType.Intrinsic.BOOLEAN,
                                       ScalarType.Intrinsic.CHARACTER])
def test_scalartype_int_precision(intrinsic, precision):
    '''Test that the ScalarType class can be created successfully for all
    supported ScalarType intrinsics and a set of valid integer precisions.
    Also test that two such types are equal.

    '''
    scalar_type = ScalarType(intrinsic, precision)
    assert isinstance(scalar_type, ScalarType)
    assert scalar_type.intrinsic == intrinsic
    assert scalar_type.precision == precision
    scalar_type2 = ScalarType(intrinsic, precision)
    assert scalar_type == scalar_type2


@pytest.mark.parametrize("intrinsic", [ScalarType.Intrinsic.INTEGER,
                                       ScalarType.Intrinsic.REAL,
                                       ScalarType.Intrinsic.BOOLEAN,
                                       ScalarType.Intrinsic.CHARACTER])
def test_scalartype_datasymbol_precision(intrinsic):
    '''Test that the ScalarType class can be created successfully for all
    supported ScalarType intrinsics and the precision specified by another
    symbol.  Also test that two such types are equal.

    '''
    # Create an r_def precision symbol with a constant value of 8
    data_type = ScalarType(ScalarType.Intrinsic.INTEGER,
                           ScalarType.Precision.UNDEFINED)
    precision_symbol = DataSymbol("r_def", data_type, is_constant=True,
                                  initial_value=8)
    # Set the precision of our ScalarType to be the precision symbol
    scalar_type = ScalarType(intrinsic, precision_symbol)
    assert isinstance(scalar_type, ScalarType)
    assert scalar_type.intrinsic == intrinsic
    assert scalar_type.precision is precision_symbol
    scalar_type2 = ScalarType(intrinsic, precision_symbol)
    assert scalar_type == scalar_type2


def test_scalartype_not_equal():
    '''
    Check that ScalarType instances with different precision or intrinsic type
    are recognised as being different. Also check that an ArrayType is !=
    to a ScalarType.

    '''
    intrinsic = ScalarType.Intrinsic.INTEGER
    data_type = ScalarType(ScalarType.Intrinsic.INTEGER,
                           ScalarType.Precision.UNDEFINED)
    precision_symbol = DataSymbol("r_def", data_type, is_constant=True,
                                  initial_value=8)
    # Set the precision of our ScalarType to be the precision symbol
    scalar_type = ScalarType(intrinsic, precision_symbol)
    # Same precision symbol but different intrinsic type
    scalar_type2 = ScalarType(ScalarType.Intrinsic.REAL, precision_symbol)
    assert scalar_type2 != scalar_type
    # Same intrinsic type but different precision specified as an integer
    scalar_type3 = ScalarType(intrinsic, 8)
    assert scalar_type3 != scalar_type
    # Same intrinsic type but different precision
    scalar_type4 = ScalarType(intrinsic, ScalarType.Precision.SINGLE)
    assert scalar_type4 != scalar_type
    # A ScalarType is not equal to an ArrayType
    atype = ArrayType(scalar_type4, [10])
    assert scalar_type4 != atype


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
    Unresolved).

    '''
    # Create an r_def precision symbol with a constant value of 8
    data_type = ScalarType(ScalarType.Intrinsic.REAL, 4)
    precision_symbol = DataSymbol("r_def", data_type)
    with pytest.raises(ValueError) as excinfo:
        _ = ScalarType(ScalarType.Intrinsic.REAL, precision_symbol)
    assert ("A DataSymbol representing the precision of another DataSymbol "
            "must be of either 'unresolved' or scalar, integer type but got: "
            "r_def: DataSymbol<Scalar<REAL, 4>, Automatic>"
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


def test_scalartype_replace_symbols():
    '''Test that the replace_symbols_using method updates any symbol referred
    to by a ScalarType.

    '''
    stype = ScalarType(ScalarType.Intrinsic.BOOLEAN,
                       ScalarType.Precision.UNDEFINED)
    table = SymbolTable()
    stype.replace_symbols_using(table)
    # No Symbol so there should be no change to the object.
    assert stype == ScalarType(ScalarType.Intrinsic.BOOLEAN,
                               ScalarType.Precision.UNDEFINED)
    rdef = DataSymbol("rdef", INTEGER_TYPE)
    stype2 = ScalarType(ScalarType.Intrinsic.INTEGER,
                        rdef)
    # Symbol with name 'rdef' is not in the supplied table so no change.
    stype2.replace_symbols_using(table)
    assert stype2.precision is rdef
    # Add a symbol with that name to the table and repeat.
    rdef2 = DataSymbol("rdef", INTEGER_TYPE)
    table.add(rdef2)
    stype2.replace_symbols_using(table)
    # Precision symbol should have been updated.
    assert stype2.precision is rdef2


# ArrayType class

def test_arraytype_extent():
    '''Test the ArrayType.Extent class. This is just an enum with a
    copy() method. '''
    xtent = ArrayType.Extent.ATTRIBUTE
    ytent = xtent.copy()
    assert isinstance(ytent, ArrayType.Extent)


def test_arraytype():
    '''Test that the ArrayType class __init__ works as expected. Test the
    different dimension datatypes that are supported.'''
    scalar_type = ScalarType(ScalarType.Intrinsic.INTEGER, 4)
    data_symbol = DataSymbol("var", scalar_type, is_constant=True,
                             initial_value=30)
    one = Literal("1", scalar_type)
    var_plus_1 = BinaryOperation.create(
        BinaryOperation.Operator.ADD, Reference(data_symbol), one)
    literal = Literal("20", scalar_type)
    array_type = ArrayType(
        scalar_type, [10, literal, var_plus_1, Reference(data_symbol),
                      (0, 10), (-1, var_plus_1.copy()),
                      (var_plus_1.copy(), var_plus_1.copy())])
    assert isinstance(array_type, ArrayType)
    assert len(array_type.shape) == 7
    # Provided as an int but stored as a Literal and given an explicit lower
    # bound of 1.
    shape0 = array_type.shape[0]
    assert isinstance(shape0, ArrayType.ArrayBounds)
    assert shape0.lower.value == "1"
    assert shape0.upper.value == "10"
    assert shape0.upper.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert shape0.upper.datatype.precision == ScalarType.Precision.UNDEFINED
    # TODO #1857: the datatype property might be affected.
    assert array_type.datatype == scalar_type
    # Provided and stored as a Literal (DataNode)
    assert array_type.shape[1].upper == literal
    # Provided and stored as an Operator (DataNode)
    assert array_type.shape[2].upper == var_plus_1
    # Provided and stored as a Reference to a DataSymbol
    assert isinstance(array_type.shape[3].upper, Reference)
    assert array_type.shape[3].upper.symbol is data_symbol
    assert isinstance(array_type.shape[4], ArrayType.ArrayBounds)
    assert array_type.shape[4].lower.value == "0"
    assert array_type.shape[4].upper.value == "10"
    # Provided as integer lower and PSyIR upper bound
    assert isinstance(array_type.shape[5], ArrayType.ArrayBounds)
    assert array_type.shape[5].lower.value == "-1"
    assert isinstance(array_type.shape[5].upper, BinaryOperation)
    # Provided as PSyIR lower and upper bounds
    assert isinstance(array_type.shape[6], ArrayType.ArrayBounds)
    assert isinstance(array_type.shape[6].lower, BinaryOperation)
    assert isinstance(array_type.shape[6].upper, BinaryOperation)
    # Provided and stored as a deferred extent
    array_type = ArrayType(
        scalar_type, [ArrayType.Extent.DEFERRED,
                      ArrayType.Extent.DEFERRED])
    assert array_type.shape[1] == ArrayType.Extent.DEFERRED
    assert array_type.is_allocatable
    # Provided as an attribute extent
    array_type = ArrayType(
        scalar_type, [ArrayType.Extent.ATTRIBUTE,
                      (2, ArrayType.Extent.ATTRIBUTE)])
    assert array_type.shape[1].upper == ArrayType.Extent.ATTRIBUTE
    assert array_type.is_allocatable is False


def test_arraytype_invalid_datatype():
    '''Test that the ArrayType class raises an exception when the datatype
    argument is the wrong type.

    '''
    with pytest.raises(TypeError) as excinfo:
        _ = ArrayType(None, None)
    assert ("ArrayType expected 'datatype' argument to be of type DataType "
            "or DataTypeSymbol but found 'NoneType'." in str(excinfo.value))


def test_arraytype_datatypesymbol_only():
    ''' Test that we currently refuse to make an ArrayType with an intrinsic
    type of StructureType. (This limitation is the subject of #1031.) '''
    with pytest.raises(NotImplementedError) as err:
        _ = ArrayType(StructureType.create(
            [("nx", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None)]),
                      [5])
    assert ("When creating an array of structures, the type of those "
            "structures must be supplied as a DataTypeSymbol but got a "
            "StructureType instead." in str(err.value))


def test_arraytype_datatypesymbol():
    ''' Test that we can correctly create an ArrayType when the type of the
    elements is specified as a DataTypeSymbol. '''
    tsym = DataTypeSymbol("my_type", UnresolvedType())
    atype = ArrayType(tsym, [5])
    assert isinstance(atype, ArrayType)
    assert atype.datatype == tsym
    assert len(atype.shape) == 1
    assert atype.intrinsic is tsym
    assert atype.precision is None


def test_arraytype_unsupportedtype():
    '''Test that we can construct an ArrayType when the type of the elements
    is an UnsupportedType.'''
    utype = UnsupportedFortranType("integer, pointer :: var")
    atype = ArrayType(utype, [8])
    assert isinstance(atype, ArrayType)
    assert atype.datatype is utype
    assert atype.precision is None
    assert utype.declaration == "integer, pointer :: var"
    # Since no partial datatype is provided, these return None
    assert utype.partial_datatype is None
    assert utype.intrinsic is None
    # Test the allocatable flag
    assert utype.is_allocatable is None


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
    symbol = DataSymbol("fred", scalar_type, is_constant=True,
                        initial_value=3.0)
    with pytest.raises(TypeError) as excinfo:
        _ = ArrayType(scalar_type, [Reference(symbol)])
    assert (
        "If a DataSymbol is referenced in a dimension declaration then it "
        "should be an integer or of UnsupportedType or UnresolvedType, but "
        "'fred' is a 'Scalar<REAL, 4>'." in str(excinfo.value))


def test_arraytype_invalid_shape_dimension_2():
    '''Test that the ArrayType class raises an exception when one of the
    dimensions of the shape list argument is not a datasymbol, datanode,
    integer, tuple or ArrayType.Extent type.

    '''
    scalar_type = ScalarType(ScalarType.Intrinsic.REAL, 4)
    with pytest.raises(TypeError) as excinfo:
        _ = ArrayType(scalar_type, [None])
    assert ("ArrayType shape-list elements can only be 'int', "
            "ArrayType.Extent, 'DataNode' or a 2-tuple thereof but found "
            "'NoneType'." in str(excinfo.value))


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


def test_arraytype_invalid_shape_bounds():
    ''' Check that the ArrayType class raises the expected exception when
    one of the dimensions of the shape list is a tuple that does not contain
    either an int or a DataNode or is not a scalar. Also test when an invalid
    lower bound is specified.'''
    scalar_type = ScalarType(ScalarType.Intrinsic.REAL, 4)
    with pytest.raises(TypeError) as excinfo:
        _ = ArrayType(scalar_type, [(1, 4, 1)])
    assert ("An ArrayType shape-list element specifying lower and upper bounds"
            " must be a 2-tuple but '(1, 4, 1)' has 3 entries" in
            str(excinfo.value))
    with pytest.raises(TypeError) as excinfo:
        _ = ArrayType(scalar_type, [(1, None)])
    assert ("ArrayType shape-list elements can only be 'int', ArrayType."
            "Extent, 'DataNode' or a 2-tuple thereof but found 'NoneType'." in
            str(excinfo.value))
    with pytest.raises(TypeError) as excinfo:
        _ = ArrayType(scalar_type, [(None, 1)])
    assert ("ArrayType shape-list elements can only be 'int', ArrayType."
            "Extent, 'DataNode' or a 2-tuple thereof but found 'NoneType'" in
            str(excinfo.value))
    with pytest.raises(TypeError) as excinfo:
        _ = ArrayType(scalar_type, [10, (None, 1)])
    assert ("ArrayType shape-list elements can only be 'int', ArrayType."
            "Extent, 'DataNode' or a 2-tuple thereof but found 'NoneType'" in
            str(excinfo.value))
    with pytest.raises(TypeError) as err:
        _ = ArrayType(scalar_type, [(ArrayType.Extent.ATTRIBUTE, 15)])
    assert ("If present, the lower bound in an ArrayType 'shape' must "
            "represent a value but found ArrayType.Extent" in str(err.value))
    scalar_type = ScalarType(ScalarType.Intrinsic.REAL, 4)
    symbol = DataSymbol("fred", scalar_type, initial_value=3.0)
    with pytest.raises(TypeError) as excinfo:
        _ = ArrayType(scalar_type, [(1, Reference(symbol))])
    assert (
        "If a DataSymbol is referenced in a dimension declaration then it "
        "should be an integer or of UnsupportedType or UnresolvedType, but "
        "'fred' is a 'Scalar<REAL, 4>'." in str(excinfo.value))
    array_type = ArrayType(INTEGER_TYPE, [10])
    symbol = DataSymbol("jim", array_type)
    with pytest.raises(TypeError) as excinfo:
        _ = ArrayType(scalar_type, [(1, Reference(symbol))])
    assert ("If a DataSymbol is referenced in a dimension declaration then it "
            "should be a scalar but 'Reference[name:'jim']' is not." in
            str(excinfo.value))
    # If one dimension is DEFERRED then all must be.
    with pytest.raises(TypeError) as err:
        _ = ArrayType(scalar_type, [ArrayType.Extent.DEFERRED, 5])
    assert ("A declaration of an allocatable array must have the extent of "
            "every dimension as 'DEFERRED' but found shape: [<Extent.DEFERRED:"
            " 1>, 5]" in str(err.value))
    # An assumed-shape array must have ATTRIBUTE in every dimension.
    with pytest.raises(TypeError) as err:
        _ = ArrayType(scalar_type, [ArrayType.Extent.ATTRIBUTE, 5])
    assert ("An assumed-shape array must have every dimension unspecified "
            "(either as 'ATTRIBUTE' or with the upper bound as 'ATTRIBUTE') "
            "but found shape: [<Extent.ATTRIBUTE: 2>, 5]" in str(err.value))


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
    data_symbol = DataSymbol("var", scalar_type, is_constant=True,
                             initial_value=20)
    data_type = ArrayType(scalar_type, [10, Reference(data_symbol),
                                        (2, Reference(data_symbol)),
                                        (Reference(data_symbol), 10)])
    assert (str(data_type) == "Array<Scalar<INTEGER, UNDEFINED>,"
            " shape=[10, Reference[name:'var'], 2:Reference[name:'var'], "
            "Reference[name:'var']:10]>")
    data_type = ArrayType(scalar_type, [ArrayType.Extent.DEFERRED])
    assert (str(data_type) == "Array<Scalar<INTEGER, UNDEFINED>, "
            "shape=['DEFERRED']>")


def test_arraytype_str_invalid():
    '''Test that the ArrayType class str method raises an exception if an
    unsupported dimension type is found.

    '''
    scalar_type = ScalarType(ScalarType.Intrinsic.INTEGER, 4)
    array_type = ArrayType(scalar_type, [10])
    # Make one of the array dimensions an unsupported type
    array_type._shape = [Literal("10", INTEGER_TYPE)]
    with pytest.raises(InternalError) as excinfo:
        _ = str(array_type)
    assert ("Once constructed, every member of an ArrayType shape-list should "
            "either be an ArrayBounds object or an instance of ArrayType."
            "Extent but found 'Literal'" in str(excinfo.value))


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


def test_arraytype_eq():
    '''Test the equality operator for ArrayType.'''
    scalar_type = ScalarType(ScalarType.Intrinsic.REAL, 4)
    data_type1 = ArrayType(scalar_type, [10, 10])
    assert data_type1 == ArrayType(scalar_type, [10, 10])
    assert data_type1 != scalar_type
    assert data_type1 == ArrayType(scalar_type, [10,
                                                 Literal("10", INTEGER_TYPE)])
    # Same type but different shape.
    assert data_type1 != ArrayType(scalar_type, [10])
    assert data_type1 != ArrayType(scalar_type, [10, 10, 5])
    assert data_type1 != ArrayType(scalar_type, [10, 5])
    assert data_type1 != ArrayType(scalar_type, [10, 5])
    sym = DataSymbol("nx", INTEGER_TYPE)
    assert data_type1 != ArrayType(scalar_type, [10, Reference(sym)])
    # Same shape but different type.
    dscalar_type = ScalarType(ScalarType.Intrinsic.REAL, 8)
    assert data_type1 != ArrayType(dscalar_type, [10, 10])
    iscalar_type = ScalarType(ScalarType.Intrinsic.INTEGER, 4)
    assert data_type1 != ArrayType(iscalar_type, [10, 10])


def test_arraytype_copy():
    '''Test the copy() method of ArrayType.'''
    sym1 = DataSymbol("alimit", INTEGER_TYPE)
    atype = ArrayType(INTEGER_TYPE, [Reference(sym1),
                                     (Reference(sym1), Reference(sym1))])
    acopy = atype.copy()
    assert acopy == atype
    assert acopy is not atype
    # The Reference defining the upper bound should have been copied.
    assert acopy.shape[0].upper is not atype.shape[0].upper
    assert acopy.shape[1].lower is not atype.shape[1].lower
    # But the new Reference should still refer to the same Symbol.
    assert acopy.shape[0].upper.symbol is atype.shape[0].upper.symbol
    assert acopy.shape[1].lower.symbol is atype.shape[1].lower.symbol
    # When shape doesn't have set bounds.
    btype = ArrayType(INTEGER_TYPE, [ArrayType.Extent.ATTRIBUTE])
    bcopy = btype.copy()
    assert bcopy == btype
    assert bcopy is not btype


def test_arraytype_replace_symbols_using():
    '''Test that the replace_symbols_using method updates any symbol referred
    to by an ArrayType.

    '''
    table = SymbolTable()
    sym1 = DataSymbol("alimit", INTEGER_TYPE)
    atype = ArrayType(INTEGER_TYPE, [Reference(sym1),
                                     (Reference(sym1), Reference(sym1))])
    atype.replace_symbols_using(table)
    assert atype.shape[0].upper.symbol is sym1
    assert atype.shape[1].lower.symbol is sym1
    assert atype.shape[1].upper.symbol is sym1

    sym1_new = DataSymbol("alimit", INTEGER_TYPE)
    table.add(sym1_new)
    atype.replace_symbols_using(table)
    assert atype.shape[0].upper.symbol is sym1_new
    assert atype.shape[1].lower.symbol is sym1_new
    assert atype.shape[1].upper.symbol is sym1_new

    # Test when the intrinsic type of the array is given by a DataTypeSymbol.
    typesym = DataTypeSymbol("grid", UnresolvedType())
    btype = ArrayType(typesym, [Reference(sym1)])
    btype.replace_symbols_using(table)
    assert btype.shape[0].upper.symbol is sym1_new
    newtypesim = DataTypeSymbol("grid", UnresolvedType())
    table.add(newtypesim)
    btype.replace_symbols_using(table)
    assert btype.intrinsic is newtypesim

    # Test when the precision of the intrinsic type of the array is given
    # by a symbol.
    rdef = DataSymbol("rdef", INTEGER_TYPE)
    ctype = ArrayType(ScalarType(ScalarType.Intrinsic.REAL, rdef),
                      [Reference(sym1)])
    ctype.replace_symbols_using(table)
    assert ctype.precision is rdef
    assert ctype.shape[0].upper.symbol is sym1_new
    newrdef = DataSymbol("rdef", INTEGER_TYPE)
    table.add(newrdef)
    ctype.replace_symbols_using(table)
    assert ctype.precision is newrdef

    # Check that having an array dimension of unknown size is OK.
    dtype = ArrayType(INTEGER_TYPE, [ArrayType.Extent.DEFERRED])
    dtype.replace_symbols_using(table)
    assert dtype == ArrayType(INTEGER_TYPE, [ArrayType.Extent.DEFERRED])

    idef = DataSymbol("idef", INTEGER_TYPE)
    etype = ArrayType(ScalarType(ScalarType.Intrinsic.REAL, rdef),
                      [Literal("10", ScalarType(ScalarType.Intrinsic.INTEGER,
                                                idef))])
    etype.replace_symbols_using(table)
    assert etype.shape[0].upper.datatype.precision is idef
    newidef = DataSymbol("idef", INTEGER_TYPE)
    table.add(newidef)
    etype.replace_symbols_using(table)
    assert etype.shape[0].upper.datatype.precision is newidef


# UnsupportedFortranType tests

def test_unsupported_fortran_type():
    ''' Check the constructor and 'declaration' property of the
    UnsupportedFortranType class. '''
    with pytest.raises(TypeError) as err:
        UnsupportedFortranType(1)
    assert ("constructor expects the original variable declaration as a "
            "string but got an argument of type 'int'" in str(err.value))
    decl = "type(some_type) :: var"
    utype = UnsupportedFortranType(decl)
    assert utype._type_text == ""
    assert utype._partial_datatype is None
    assert str(utype) == f"UnsupportedFortranType('{decl}')"
    assert utype._declaration == decl


def test_unsupported_fortran_type_optional_arg():
    '''Check the optional 'partial_datatype' argument of the
    UnsupportedFortranType class works as expected. Also check the getter
    method and the string methods work as expected when
    partial_datatype information is supplied.

    '''
    decl = "type(some_type) :: var"
    with pytest.raises(TypeError) as err:
        _ = UnsupportedFortranType(decl, partial_datatype="invalid")
    assert ("partial_datatype argument in UnsupportedFortranType "
            "initialisation should be a DataType, DataTypeSymbol, or "
            "NoneType, but found 'str'." in str(err.value))
    utype = UnsupportedFortranType(decl, partial_datatype=None)
    assert utype._partial_datatype is None
    assert utype.partial_datatype is None

    utype = UnsupportedFortranType(
        decl, partial_datatype=DataTypeSymbol("some_type", UnresolvedType()))
    assert isinstance(utype._partial_datatype, DataTypeSymbol)
    assert isinstance(utype.partial_datatype, DataTypeSymbol)
    assert utype.partial_datatype.name == "some_type"
    assert str(utype) == f"UnsupportedFortranType('{decl}')"


def test_unsupported_fortran_type_text():
    '''
    Check that the 'type_text' property returns the expected string and
    that the result is cached.
    '''
    decl = "type(some_type) :: var"
    utype = UnsupportedFortranType(decl)
    text = utype.type_text
    assert text == "TYPE(some_type)"
    # Calling it a second time should just return the previously cached
    # result.
    assert utype.type_text is text
    # Test for a SAVE for a common block.
    decl2 = "save :: /a_common_fault/"
    utype2 = UnsupportedFortranType(decl2)
    assert utype2.type_text == "SAVE"
    # Test for a Common block.
    decl3 = "common /name1/ a, b, c"
    utype3 = UnsupportedFortranType(decl3)
    assert utype3.type_text == "COMMON"


def test_unsupported_fortran_type_text_error():
    '''
    Check that the expected error is raised if the 'type_text' is requested
    for something that is not a straightforward declaration.
    '''
    decl = "10 format('4I4')"
    utype = UnsupportedFortranType(decl)
    with pytest.raises(NotImplementedError) as err:
        utype.type_text
    assert ("Cannot extract the declaration part from UnsupportedFortranType "
            "'10 format('4I4')'. Only Declaration_Construct, "
            "Type_Declaration_Stmt, Save_Stmt and Common_Stmt are supported "
            "but got 'Implicit_Part' from the parser." in str(err.value))
    # Valid Fortran but not a declaration.
    utype = UnsupportedFortranType("call fn(a)")
    with pytest.raises(NotImplementedError) as err:
        utype.type_text
    assert ("Cannot extract the declaration part from UnsupportedFortranType "
            "'call fn(a)' because parsing (attempting to match a "
            "Fortran2003.Specification_Part) failed." in str(err.value))
    # Invalid Fortran.
    utype = UnsupportedFortranType("not valid fortran")
    with pytest.raises(NotImplementedError) as err:
        utype.type_text
    assert ("Cannot extract the declaration part from UnsupportedFortranType "
            "'not valid fortran' because parsing (attempting to match a "
            "Fortran2003.Specification_Part) failed." in str(err.value))


def test_unsupported_fortran_type_eq():
    '''Test the equality operator for UnsupportedFortranType.'''
    decl = "type(some_type) :: var"
    utype = UnsupportedFortranType(decl)
    assert utype == UnsupportedFortranType(decl)
    assert utype != NoType()
    # Type is the same even if the variable name is different.
    assert utype == UnsupportedFortranType("type(some_type) :: var1")
    assert utype != UnsupportedFortranType("type(other_type) :: var")
    # A common block is the same type as another common block.
    assert (UnsupportedFortranType("common /how_common/ a, b, cc") ==
            UnsupportedFortranType("common /common_land/ a, b, cc"))
    # A SAVE statement is the same type as another SAVE statement.
    assert (UnsupportedFortranType("save :: /how_common/") ==
            UnsupportedFortranType("save :: blue_blood"))
    # Just sanity check that the type of a SAVE != that of a common.
    assert (UnsupportedFortranType("common /how_common/ a, b, cc") !=
            UnsupportedFortranType("save :: blue_blood"))


def test_unsupported_fortran_type_is_allocatable(fortran_reader):
    '''Test the is_allocatable() method of UnsupportedFortranType.'''
    code = '''
    subroutine test
      use some_mod, only: some_type, start, stop
      integer, parameter :: nelem = 4
      type(some_type), pointer :: var(nelem), var2(start:stop)
      type(some_type), target, allocatable :: var_alloc(:)
    end subroutine
    '''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    vsym = routine.symbol_table.lookup("var")
    # Make sure we do indeed test the UnsupportedFortranType
    assert isinstance(vsym.datatype, UnsupportedFortranType)
    assert vsym.datatype.is_allocatable is False

    # Now test an allocatable array in an UnsupportedFortranType:
    vsym_alloc = routine.symbol_table.lookup("var_alloc")
    # Make sure we do indeed test the UnsupportedFortranType
    assert isinstance(vsym_alloc.datatype, UnsupportedFortranType)
    assert vsym_alloc.datatype.is_allocatable

    # Check the behaviour if partial_datatype is None
    unsup_type = UnsupportedFortranType("some_declaration",
                                        partial_datatype=None)
    assert unsup_type.is_allocatable is None


def test_unsupported_fortran_type_copy(fortran_reader):
    '''Test the copy() method of UnsupportedFortranType.'''
    code = '''
    subroutine test
      use some_mod, only: some_type, start, stop
      integer, parameter :: nelem = 4
      type(some_type), pointer :: var(nelem), var2(start:stop)
    end subroutine
    '''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    vsym = routine.symbol_table.lookup("var")
    vtype = vsym.datatype
    assert isinstance(vtype, UnsupportedFortranType)
    cpytype = vtype.copy()
    assert isinstance(cpytype, UnsupportedFortranType)
    assert isinstance(cpytype.partial_datatype, ArrayType)
    # Reference to 'nelem' should be a copy.
    assert (cpytype.partial_datatype.shape[0] is not
            vtype.partial_datatype.shape[0])
    assert (cpytype.partial_datatype.shape[0].lower is not
            vtype.partial_datatype.shape[0].lower)
    assert (cpytype.partial_datatype.shape[0].upper is not
            vtype.partial_datatype.shape[0].upper)
    # But the symbol referred to should be the same.
    assert (cpytype.partial_datatype.shape[0].upper.symbol is
            vtype.partial_datatype.shape[0].upper.symbol)
    # The intrinsic type of the partial type should also be the same Symbol
    # in both cases.
    stype = routine.symbol_table.lookup("some_type")
    assert vtype.intrinsic is stype
    assert cpytype.intrinsic is stype
    # Repeat check when array lower bound is also a Reference.
    var2 = routine.symbol_table.lookup("var2")
    v2type = var2.datatype
    v2copy = v2type.copy()
    assert (v2type.partial_datatype.shape[0].lower is not
            v2copy.partial_datatype.shape[0].lower)
    assert (v2type.partial_datatype.shape[0].lower.symbol is
            v2copy.partial_datatype.shape[0].lower.symbol)


def test_unsupported_fortran_type_replace_symbols():
    '''Test that replace_symbols_using() correctly updates Symbols in
    the _partial_datatype.

    '''
    decl = "type(some_type), dimension(nelem) :: var"
    stype = DataTypeSymbol("some_type", UnresolvedType())
    nelem = DataSymbol("nelem", INTEGER_TYPE)
    ptype = ArrayType(stype, [Reference(nelem)])
    utype = UnsupportedFortranType(decl, partial_datatype=ptype)
    table = SymbolTable()
    utype.replace_symbols_using(table)
    assert utype.partial_datatype.shape[0].upper.symbol is nelem
    newnelem = nelem.copy()
    table.add(newnelem)
    utype.replace_symbols_using(table)
    assert utype.partial_datatype.shape[0].upper.symbol is newnelem
    wp = DataSymbol("wp", INTEGER_TYPE)
    ptype2 = ScalarType(ScalarType.Intrinsic.REAL, wp)
    decl2 = "real(kind=wp), pointer :: var"
    stype2 = UnsupportedFortranType(decl2, partial_datatype=ptype2)
    stype2.replace_symbols_using(table)
    assert stype2.partial_datatype.precision is wp
    newp = wp.copy()
    table.add(newp)
    stype2.replace_symbols_using(table)
    assert stype2.partial_datatype.precision is newp


# StructureType tests

def test_structure_type():
    ''' Check the StructureType constructor and that we can add components. '''
    stype = StructureType()
    assert str(stype) == "StructureType<>"
    assert not stype.components
    stype.add("flaG", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None)
    # Lookup is not case sensitive
    flag = stype.lookup("flag")
    # But we retain information on the original capitalisation
    assert flag.name == "flaG"
    assert not flag.initial_value
    assert isinstance(flag, StructureType.ComponentType)
    stype.add("flag2", INTEGER_TYPE, Symbol.Visibility.PUBLIC,
              Literal("1", INTEGER_TYPE))
    flag2 = stype.lookup("flag2")
    assert isinstance(flag2, StructureType.ComponentType)
    assert flag2.initial_value.value == "1"
    with pytest.raises(TypeError) as err:
        stype.add(1, "hello", "hello", None)
    assert ("name of a component of a StructureType must be a 'str' but got "
            "'int'" in str(err.value))
    with pytest.raises(TypeError) as err:
        stype.add("hello", "hello", "hello", None)
    assert ("type of a component of a StructureType must be a 'DataType' "
            "or 'DataTypeSymbol' but got 'str'" in str(err.value))
    with pytest.raises(TypeError) as err:
        stype.add("hello", INTEGER_TYPE, "hello", None)
    assert ("visibility of a component of a StructureType must be an instance "
            "of 'Symbol.Visibility' but got 'str'" in str(err.value))
    with pytest.raises(TypeError) as err:
        stype.add("hello", INTEGER_TYPE, Symbol.Visibility.PUBLIC, "Hello")
    assert ("The initial value of a component of a StructureType must be "
            "None or an instance of 'DataNode', but got 'str'."
            in str(err.value))
    with pytest.raises(TypeError) as err:
        stype.add("hello", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None,
                  preceding_comment=None)
    assert ("The preceding_comment of a component of a StructureType "
            "must be a 'str' but got 'NoneType'" in str(err.value))
    with pytest.raises(TypeError) as err:
        stype.add("hello", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None,
                  inline_comment=None)
    assert ("The inline_comment of a component of a StructureType "
            "must be a 'str' but got 'NoneType'" in str(err.value))

    with pytest.raises(KeyError):
        stype.lookup("missing")
    # Cannot have a recursive type definition
    with pytest.raises(TypeError) as err:
        stype.add("hello", stype, Symbol.Visibility.PUBLIC, None)
    assert ("attempting to add component 'hello' - a StructureType definition "
            "cannot be recursive" in str(err.value))
    assert stype.is_allocatable is False


def test_create_structuretype():
    ''' Test the create() method of StructureType. '''
    # One member will have its type defined by a DataTypeSymbol
    tsymbol = DataTypeSymbol("my_type", UnresolvedType())
    stype = StructureType.create([
        ("fred", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
        ("george", REAL_TYPE, Symbol.Visibility.PRIVATE,
         Literal("1.0", REAL_TYPE)),
        ("barry", tsymbol, Symbol.Visibility.PUBLIC, None)])
    assert len(stype.components) == 3
    george = stype.lookup("george")
    assert isinstance(george, StructureType.ComponentType)
    assert george.name == "george"
    assert george.datatype == REAL_TYPE
    assert george.visibility == Symbol.Visibility.PRIVATE
    assert george.initial_value.value == "1.0"
    barry = stype.lookup("barry")
    assert isinstance(barry, StructureType.ComponentType)
    assert barry.datatype is tsymbol
    assert barry.visibility == Symbol.Visibility.PUBLIC
    assert not barry.initial_value
    with pytest.raises(TypeError) as err:
        StructureType.create([
            ("fred", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
            ("george", Symbol.Visibility.PRIVATE)])
    assert ("Each component must be specified using a 4 to 6-tuple of (name, "
            "type, visibility, initial_value, preceding_comment, "
            "inline_comment) but found a tuple with 2 members: ('george', "
            in str(err.value))


def test_structuretype_eq():
    '''Test the equality operator of StructureType.'''
    stype = StructureType.create([
        ("nancy", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
        ("peggy", REAL_TYPE, Symbol.Visibility.PRIVATE,
         Literal("1.0", REAL_TYPE))])
    assert stype == StructureType.create([
        ("nancy", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
        ("peggy", REAL_TYPE, Symbol.Visibility.PRIVATE,
         Literal("1.0", REAL_TYPE))])
    # Something that is not a StructureType
    assert stype != NoType()
    # Component with a different name.
    assert stype != StructureType.create([
        ("nancy", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
        ("roger", REAL_TYPE, Symbol.Visibility.PRIVATE,
         Literal("1.0", REAL_TYPE))])
    # Component with a different type.
    assert stype != StructureType.create([
        ("nancy", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
        ("peggy", INTEGER_TYPE, Symbol.Visibility.PRIVATE,
         Literal("1.0", REAL_TYPE))])
    # Component with a different visibility.
    assert stype != StructureType.create([
        ("nancy", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
        ("peggy", REAL_TYPE, Symbol.Visibility.PUBLIC,
         Literal("1.0", REAL_TYPE))])
    # Component wth a different initialisation
    assert stype != StructureType.create([
        ("nancy", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
        ("peggy", REAL_TYPE, Symbol.Visibility.PRIVATE, None)])
    # Different number of components.
    assert stype != StructureType.create([
        ("nancy", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
        ("peggy", REAL_TYPE, Symbol.Visibility.PRIVATE,
         Literal("1.0", REAL_TYPE)),
        ("roger", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None)])


def test_structuretype_replace_symbols():
    '''Test that replace_symbols_using() correctly updates any Symbols referred
    to within a StructureType.

    '''
    tsymbol = DataTypeSymbol("my_type", UnresolvedType())
    stype = StructureType.create([
        ("fred", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
        ("george", REAL_TYPE, Symbol.Visibility.PRIVATE,
         Literal("1.0", REAL_TYPE)),
        ("barry", tsymbol, Symbol.Visibility.PUBLIC, None)])
    table = SymbolTable()
    assert stype.components["barry"].datatype is tsymbol
    stype.replace_symbols_using(table)
    assert stype.components["barry"].datatype is tsymbol
    newtsymbol = DataTypeSymbol("my_type", UnresolvedType())
    table.add(newtsymbol)
    stype.replace_symbols_using(table)
    assert stype.components["barry"].datatype is newtsymbol


def test_structuretype_componenttype_eq():
    '''Test that the equality operator of StructureType.ComponentType does
    not take the preceding_comment and inline_comment into account.
    '''
    comp1 = StructureType.ComponentType("fred", INTEGER_TYPE,
                                        Symbol.Visibility.PUBLIC, None)
    comp2 = StructureType.ComponentType("fred", INTEGER_TYPE,
                                        Symbol.Visibility.PUBLIC, None)
    assert comp1 == comp2

    comp1 = StructureType.ComponentType("fred", INTEGER_TYPE,
                                        Symbol.Visibility.PUBLIC, None)
    object.__setattr__(comp1, "_preceding_comment", "A comment")
    comp2 = StructureType.ComponentType("fred", INTEGER_TYPE,
                                        Symbol.Visibility.PUBLIC, None)
    object.__setattr__(comp2, "_preceding_comment", "Another comment")
    assert comp1 == comp2

    comp1 = StructureType.ComponentType("fred", INTEGER_TYPE,
                                        Symbol.Visibility.PUBLIC, None)
    object.__setattr__(comp1, "_inline_comment", "A comment")
    comp2 = StructureType.ComponentType("fred", INTEGER_TYPE,
                                        Symbol.Visibility.PUBLIC, None)
    object.__setattr__(comp2, "_inline_comment", "Another comment")
    assert comp1 == comp2

    comp1 = StructureType.ComponentType("fred", INTEGER_TYPE,
                                        Symbol.Visibility.PUBLIC, None)
    comp2 = StructureType.ComponentType("george", INTEGER_TYPE,
                                        Symbol.Visibility.PUBLIC, None)
    assert comp1 != comp2


def test_structuretype___copy__():
    '''Test the __copy__ method of StructureType.'''
    stype = StructureType.create([
        ("nancy", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
        ("peggy", REAL_TYPE, Symbol.Visibility.PRIVATE,
         Literal("1.0", REAL_TYPE))])
    copied = stype.__copy__()
    assert copied == stype
    assert copied is not stype
    # The components should be the same objects
    assert copied.components["nancy"] == stype.components["nancy"]
    assert copied.components["peggy"] == stype.components["peggy"]
