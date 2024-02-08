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
# Author R. W. Ford, STFC Daresbury Lab
# Modified S. Siso, STFC Daresbury Lab
# Modified J. Henrichs, Bureau of Meteorology
# Modified by O. Brunt, Met Office

'''Test that the LFRic-specific PSyIR classes are created and declared
correctly'''

import pytest

from psyclone.configuration import Config
from psyclone.domain.lfric import LFRicConstants, LFRicTypes
from psyclone.errors import InternalError
from psyclone.psyir.symbols import ContainerSymbol, DataSymbol, \
    ImportInterface, ScalarType, AutomaticInterface, ArgumentInterface, \
    ArrayType, Symbol
from psyclone.psyir.nodes import Reference, Literal


# pylint complains about all isinstance tests involving LFRicTypes,
# since it doesn't know what the actual return type is. So disable
# it here globally for this file:
# pylint: disable=isinstance-second-argument-not-valid-type


def test_singleton(monkeypatch):
    '''Tests that the singleton implementation works as expected.

    '''
    # First check that on first time creation the class is properly
    # initialised:
    monkeypatch.setattr(LFRicTypes, "_name_to_class", None)
    assert LFRicTypes._name_to_class is None
    LFRicTypes("I_DEF")
    assert LFRicTypes._name_to_class is not None

    # Now replace the internal dictionary
    monkeypatch.setattr(LFRicTypes, "_name_to_class",
                        {"new": "really_new"})
    # Make sure we get the new dictionary:
    assert LFRicTypes("new") == "really_new"

    # Check error handling:
    monkeypatch.setattr(LFRicTypes, "_name_to_class", None)
    with pytest.raises(InternalError) as err:
        LFRicTypes("does_not_exist")
    assert ("Unknown LFRic type 'does_not_exist'. Valid values are "
            in str(err.value))


def test_constants_mod():
    '''Test the generated module symbol and its argument symbols are
    created correctly.

    '''
    api_config = Config.get().api_conf("dynamo0.3")
    module = LFRicTypes("constants_mod")
    assert isinstance(module, ContainerSymbol)
    symbol_list = list(api_config.precision_map.keys())
    for symbol_name in symbol_list:
        symbol = LFRicTypes(symbol_name.upper())
        # pylint: disable=no-member
        assert isinstance(symbol, DataSymbol)
        assert isinstance(symbol.interface, ImportInterface)
        assert symbol.interface.container_symbol is module


# Generic scalars
@pytest.mark.parametrize("type_name, symbol_name, intrinsic, precision_name", [
    ("LFRicIntegerScalarDataType", "LFRicIntegerScalarDataSymbol",
     ScalarType.Intrinsic.INTEGER, "I_DEF"),
    ("LFRicRealScalarDataType", "LFRicRealScalarDataSymbol",
     ScalarType.Intrinsic.REAL, "R_DEF"),
    ("LFRicLogicalScalarDataType", "LFRicLogicalScalarDataSymbol",
     ScalarType.Intrinsic.BOOLEAN, "L_DEF")])
def test_generic_scalars(type_name, symbol_name, intrinsic,
                         precision_name):
    '''Test the generated generic scalar datatypes and symbols are created
    correctly.

    '''
    data_type = LFRicTypes(type_name)
    symbol = LFRicTypes(symbol_name)
    precision = LFRicTypes(precision_name)
    # datatype
    lfric_datatype = data_type()
    assert lfric_datatype.intrinsic == intrinsic
    assert lfric_datatype.precision is precision
    # precision can be set explicitly
    lfric_datatype = data_type(precision=4)
    assert lfric_datatype.precision == 4
    # symbol
    lfric_symbol = symbol("symbol")
    assert lfric_symbol.name == "symbol"
    assert isinstance(lfric_symbol.interface, AutomaticInterface)
    assert isinstance(lfric_symbol.datatype, data_type)
    lfric_symbol = symbol(
        "symbol", interface=ArgumentInterface(ArgumentInterface.Access.READ))
    assert isinstance(lfric_symbol.interface, ArgumentInterface)
    assert lfric_symbol.interface.access == ArgumentInterface.Access.READ
    # precision can be set explicitly
    lfric_symbol = symbol("symbol", precision=4)
    assert lfric_symbol.datatype.precision == 4


# Scalar literals
def test_scalar_literals():
    '''Test the scalar literals are defined correctly.'''
    # LFRicDimension class
    lfric_dim_class = LFRicTypes("LFRicDimension")
    assert isinstance(LFRicTypes("LFRicDimension")("1"),
                      lfric_dim_class)
    assert isinstance(LFRicTypes("LFRicDimension")("3"),
                      lfric_dim_class)
    with pytest.raises(ValueError) as info:
        LFRicTypes("LFRicDimension")("4")
    assert ("An LFRic dimension object must be '1', '2' or '3', but found '4'."
            in str(info.value))
    # LFRIC_SCALAR_DIMENSION instance
    assert isinstance(LFRicTypes("LFRIC_SCALAR_DIMENSION"), lfric_dim_class)
    # pylint: disable=no-member
    assert LFRicTypes("LFRIC_SCALAR_DIMENSION").value == "1"
    # LFRIC_VECTOR_DIMENSION instance
    assert isinstance(LFRicTypes("LFRIC_VECTOR_DIMENSION"),
                      lfric_dim_class)
    assert LFRicTypes("LFRIC_VECTOR_DIMENSION").value == "3"


# Specific scalar datatypes
@pytest.mark.parametrize("data_type_name, generic_type_name", [
    ("CellPositionDataType", "LFRicIntegerScalarDataType"),
    ("MeshHeightDataType", "LFRicIntegerScalarDataType"),
    ("NumberOfCellsDataType", "LFRicIntegerScalarDataType"),
    ("NumberOfDofsDataType", "LFRicIntegerScalarDataType"),
    ("NumberOfUniqueDofsDataType", "LFRicIntegerScalarDataType"),
    ("NumberOfFacesDataType", "LFRicIntegerScalarDataType"),
    ("NumberOfEdgesDataType", "LFRicIntegerScalarDataType"),
    ("NumberOfQrPointsInXyDataType", "LFRicIntegerScalarDataType"),
    ("NumberOfQrPointsInZDataType", "LFRicIntegerScalarDataType"),
    ("NumberOfQrPointsInFacesDataType", "LFRicIntegerScalarDataType"),
    ("NumberOfQrPointsInEdgesDataType", "LFRicIntegerScalarDataType")])
def test_specific_scalar_types(data_type_name, generic_type_name):
    '''Test the generated specific scalar datatypes are created correctly.

    '''
    lfric_datatype = LFRicTypes(data_type_name)()
    assert isinstance(lfric_datatype, LFRicTypes(generic_type_name))


def test_specific_scalar_types_init_args():
    '''Test the generated specific scalar data types are created correctly
    and work with positional and keyword arguments.

    '''
    type_class = LFRicTypes("NumberOfDofsDataSymbol")
    positional_init = type_class("name", "fs")
    assert positional_init.fs == "fs"
    keyword_init = type_class("name", fs="fs")
    assert keyword_init.fs == "fs"


# Specific scalar symbols
@pytest.mark.parametrize("symbol_name, generic_symbol_name, attribute_map", [
    ("CellPositionDataSymbol", "LFRicIntegerScalarDataSymbol", {}),
    ("MeshHeightDataSymbol", "LFRicIntegerScalarDataSymbol", {}),
    ("NumberOfCellsDataSymbol", "LFRicIntegerScalarDataSymbol", {}),
    ("NumberOfDofsDataSymbol", "LFRicIntegerScalarDataSymbol", {"fs": "w3"}),
    ("NumberOfUniqueDofsDataSymbol", "LFRicIntegerScalarDataSymbol",
     {"fs": "w2"}),
    ("NumberOfFacesDataSymbol", "LFRicIntegerScalarDataSymbol", {}),
    ("NumberOfEdgesDataSymbol", "LFRicIntegerScalarDataSymbol", {}),
    ("NumberOfQrPointsInXyDataSymbol", "LFRicIntegerScalarDataSymbol", {}),
    ("NumberOfQrPointsInZDataSymbol", "LFRicIntegerScalarDataSymbol", {}),
    ("NumberOfQrPointsInFacesDataSymbol", "LFRicIntegerScalarDataSymbol", {}),
    ("NumberOfQrPointsInEdgesDataSymbol", "LFRicIntegerScalarDataSymbol", {})])
def test_specific_scalar_symbols(symbol_name, generic_symbol_name,
                                 attribute_map):
    '''Test the generated specific scalar symbols are
    created correctly.

    '''
    symbol = LFRicTypes(symbol_name)
    generic_symbol = LFRicTypes(generic_symbol_name)
    args = ["symbol"] + list(attribute_map.values())
    lfric_symbol = symbol(*args)
    assert isinstance(lfric_symbol, generic_symbol)
    assert lfric_symbol.name == "symbol"
    assert isinstance(lfric_symbol.interface, AutomaticInterface)
    for attribute in attribute_map:
        assert getattr(lfric_symbol, attribute) == attribute_map[attribute]
    lfric_symbol = symbol(
        *args, interface=ArgumentInterface(ArgumentInterface.Access.READ))
    assert isinstance(lfric_symbol.interface, ArgumentInterface)
    assert lfric_symbol.interface.access == ArgumentInterface.Access.READ


# Specific scalar datatypes
@pytest.mark.parametrize(
    "data_type_name, symbol_name, scalar_type_name, dims_args,"
    "attribute_map",
    [("RealFieldDataType", "RealFieldDataSymbol",
      "LFRicRealScalarDataType",
      [("NumberOfUniqueDofsDataSymbol", "ndofs", "w0")], {"fs": "w0"}),
     ("IntegerFieldDataType", "IntegerFieldDataSymbol",
      "LFRicIntegerScalarDataType",
      [("NumberOfUniqueDofsDataSymbol", "ndofs", "w1")], {"fs": "w1"}),
     ("LogicalFieldDataType", "LogicalFieldDataSymbol",
      "LFRicLogicalScalarDataType",
      [("NumberOfUniqueDofsDataSymbol", "ndofs", "w2")], {"fs": "w2"}),
     ("OperatorDataType", "OperatorDataSymbol",
      "LFRicRealScalarDataType",
      [("NumberOfDofsDataSymbol", "ndofs", "w3"),
       ("NumberOfDofsDataSymbol", "ndofs", "w3"),
       ("NumberOfCellsDataSymbol", "ncells")],
     {"fs_from": "w3", "fs_to": "w3"}),
     ("DofMapDataType", "DofMapDataSymbol",
      "LFRicIntegerScalarDataType",
      [("NumberOfDofsDataSymbol", "ndofs", "w3")], {"fs": "w3"}),
     ("BasisFunctionQrXyozDataType", "BasisFunctionQrXyozDataSymbol",
      "LFRicRealScalarDataType",
      [1,
       ("NumberOfDofsDataSymbol", "ndofs", "w0"),
       ("NumberOfQrPointsInXyDataSymbol", "qr_xy"),
       ("NumberOfQrPointsInZDataSymbol", "qr_z")], {"fs": "w0"}),
     ("BasisFunctionQrFaceDataType", "BasisFunctionQrFaceDataSymbol",
      "LFRicRealScalarDataType",
      [3,
       ("NumberOfDofsDataSymbol", "ndofs", "w1"),
       ("NumberOfQrPointsInFacesDataSymbol", "qr"),
       ("NumberOfFacesDataSymbol", "nfaces")], {"fs": "w1"}),
     ("BasisFunctionQrEdgeDataType", "BasisFunctionQrEdgeDataSymbol",
      "LFRicRealScalarDataType",
      [1,
       ("NumberOfDofsDataSymbol", "ndofs", "w2"),
       ("NumberOfQrPointsInEdgesDataSymbol", "qr"),
       ("NumberOfEdgesDataSymbol", "nedges")], {"fs": "w2"}),
     ("DiffBasisFunctionQrXyozDataType", "DiffBasisFunctionQrXyozDataSymbol",
      "LFRicRealScalarDataType",
      [3,
       ("NumberOfDofsDataSymbol", "ndofs", "wtheta"),
       ("NumberOfQrPointsInXyDataSymbol", "qr_xy"),
       ("NumberOfQrPointsInZDataSymbol", "qr_z")], {"fs": "wtheta"}),
     ("DiffBasisFunctionQrFaceDataType", "DiffBasisFunctionQrFaceDataSymbol",
      "LFRicRealScalarDataType",
      [3,
       ("NumberOfDofsDataSymbol", "ndofs", "w1"),
       ("NumberOfQrPointsInFacesDataSymbol", "qr"),
       ("NumberOfFacesDataSymbol", "nfaces")], {"fs": "w1"}),
     ("DiffBasisFunctionQrEdgeDataType", "DiffBasisFunctionQrEdgeDataSymbol",
      "LFRicRealScalarDataType",
      [1,
       ("NumberOfDofsDataSymbol", "ndofs", "w2v"),
       ("NumberOfQrPointsInEdgesDataSymbol", "qr"),
       ("NumberOfEdgesDataSymbol", "nedges")], {"fs": "w2v"}),
     ("QrWeightsInXyDataType", "QrWeightsInXyDataSymbol",
      "LFRicRealScalarDataType",
      [("NumberOfQrPointsInXyDataSymbol", "qr_xy")], {}),
     ("QrWeightsInZDataType", "QrWeightsInZDataSymbol",
      "LFRicRealScalarDataType",
      [("NumberOfQrPointsInZDataSymbol", "qr_z")], {}),
     ("QrWeightsInFacesDataType", "QrWeightsInFacesDataSymbol",
      "LFRicRealScalarDataType",
      [("NumberOfQrPointsInFacesDataSymbol", "qr")], {}),
     ("QrWeightsInEdgesDataType", "QrWeightsInEdgesDataSymbol",
      "LFRicRealScalarDataType",
      [("NumberOfQrPointsInEdgesDataSymbol", "qr")], {})])
def test_arrays(data_type_name, symbol_name, scalar_type_name,
                dims_args, attribute_map):
    '''Test the generated array datatypes and datasymbols are created
    correctly. This includes field datatypes and symbols which are
    kept as a separate list in psyir.py

    '''
    # pylint: disable=too-many-locals
    dims = []
    # Each dimension arg is either an integer number, or a tuple consisting of
    # an LFRic data type, followed by additional constructor arguments. Use
    # this to create the required list of dimensions:
    for i in dims_args:
        if isinstance(i, int):
            dims.append(i)
        else:
            # Tage the additional constructor arguments
            args = i[1:]
            interface = ArgumentInterface(ArgumentInterface.Access.READ)
            ref = Reference(LFRicTypes(i[0])(*args,
                                             interface=interface))
            dims.append(ref)

    # Datatype creation
    data_type = LFRicTypes(data_type_name)
    scalar_type = LFRicTypes(scalar_type_name)
    lfric_datatype = data_type(dims)
    assert isinstance(lfric_datatype, ArrayType)
    assert isinstance(lfric_datatype._datatype, scalar_type)
    for idx, dim in enumerate(lfric_datatype.shape):
        if isinstance(dim.upper, Literal):
            assert dim.upper.value == str(dims[idx])
        elif isinstance(dim.upper, Reference):
            assert dim.upper == dims[idx]
            assert dim.upper.symbol == dims[idx].symbol
        else:
            assert False, "unexpected type of dimension found"
    # Wrong number of dims
    with pytest.raises(TypeError) as info:
        _ = data_type([])
    assert (f"'{type(lfric_datatype).__name__}' expected the number of "
            f"supplied dimensions to be {len(dims)} but found 0." in
            str(info.value))
    # Datasymbol creation
    args = list(attribute_map.values())
    symbol = LFRicTypes(symbol_name)
    lfric_symbol = symbol("symbol", dims, *args)
    assert isinstance(lfric_symbol, DataSymbol)
    assert lfric_symbol.name == "symbol"
    assert isinstance(lfric_symbol.interface, AutomaticInterface)
    assert isinstance(lfric_symbol.datatype, data_type)
    lfric_symbol = symbol(
        "symbol", dims, *args,
        interface=ArgumentInterface(ArgumentInterface.Access.READ))
    assert isinstance(lfric_symbol.interface, ArgumentInterface)
    assert lfric_symbol.interface.access == ArgumentInterface.Access.READ


def test_arrays_data_symbol_init_args():
    '''Test the generated array datasymbols are created correctly.
    and accept positional and keyword arguments in the constructor.

    '''
    symbol_class = LFRicTypes("DiffBasisFunctionQrEdgeDataSymbol")
    lfric_symbol = symbol_class("symbol", [1, 2, 3, 4], "fs1")
    assert lfric_symbol.fs == "fs1"
    lfric_symbol = symbol_class("symbol", dims=[1, 2, 3, 4], fs="fs2")
    assert lfric_symbol.fs == "fs2"


# Vector field-data data-symbols
@pytest.mark.parametrize(
    "symbol, parent_symbol, space, visibility",
    [("RealVectorFieldDataSymbol", "RealFieldDataSymbol",
      "w0", None),
     ("IntegerVectorFieldDataSymbol", "IntegerFieldDataSymbol",
      "w1", Symbol.Visibility.PUBLIC),
     ("LogicalVectorFieldDataSymbol", "LogicalFieldDataSymbol",
      "w2", Symbol.Visibility.PRIVATE)])
def test_vector_fields(symbol, parent_symbol, space, visibility):
    '''Test the generated vector field datasymbols are created
    correctly. These are straight subclasses of the equivalent field
    datasymbols.

    '''

    kwargs = {"interface": ArgumentInterface(ArgumentInterface.Access.READ)}
    if visibility is not None:
        kwargs["visibility"] = visibility
    ref = Reference(LFRicTypes("NumberOfUniqueDofsDataSymbol")(
                    "ndofs", space, **kwargs))
    args = [space]
    lfric_symbol = LFRicTypes(symbol)("symbol", [ref], *args)
    assert isinstance(lfric_symbol, LFRicTypes(parent_symbol))
    assert lfric_symbol.name == "symbol"
    assert lfric_symbol.fs == space
    if visibility is None:
        assert ref.symbol.visibility == lfric_symbol.DEFAULT_VISIBILITY
    else:
        assert ref.symbol.visibility == visibility
