# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2023, Science and Technology Facilities Council.
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

'''Test that the LFRic-specific PSyIR classes are created and declared
correctly'''

import pytest

from psyclone.domain.lfric import psyir as lfric_psyir, LFRicTypes
from psyclone.psyir.symbols import ContainerSymbol, DataSymbol, \
    ImportInterface, ScalarType, LocalInterface, ArgumentInterface, \
    ArrayType, Symbol
from psyclone.psyir.nodes import Reference, Literal


# Modules and their arguments
@pytest.mark.parametrize("module, symbol_list",
                         [(lfric_psyir.CONSTANTS_MOD,
                           [lfric_psyir.I_DEF, lfric_psyir.R_DEF,
                            lfric_psyir.L_DEF])])
def test_constants_mod(module, symbol_list):
    '''Test the generated module symbol and its argument symbols are
    created correctly.

    '''
    assert isinstance(module, ContainerSymbol)
    for symbol in symbol_list:
        assert isinstance(symbol, DataSymbol)
        assert isinstance(symbol.interface, ImportInterface)
        assert symbol.interface.container_symbol is module


# Generic scalars
@pytest.mark.parametrize("data_type, symbol, intrinsic, precision", [
    (lfric_psyir.LfricIntegerScalarDataType,
     lfric_psyir.LfricIntegerScalarDataSymbol,
     ScalarType.Intrinsic.INTEGER, lfric_psyir.I_DEF),
    (lfric_psyir.LfricRealScalarDataType,
     lfric_psyir.LfricRealScalarDataSymbol,
     ScalarType.Intrinsic.REAL, lfric_psyir.R_DEF),
    (lfric_psyir.LfricLogicalScalarDataType,
     lfric_psyir.LfricLogicalScalarDataSymbol,
     ScalarType.Intrinsic.BOOLEAN, lfric_psyir.L_DEF)])
def test_generic_scalars(data_type, symbol, intrinsic, precision):
    '''Test the generated generic scalar datatypes and symbols are created
    correctly.

    '''
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
    assert isinstance(lfric_symbol.interface, LocalInterface)
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
    # LfricDimension class
    assert isinstance(lfric_psyir.LfricDimension("1"),
                      lfric_psyir.LfricDimension)
    assert isinstance(lfric_psyir.LfricDimension("3"),
                      lfric_psyir.LfricDimension)
    with pytest.raises(ValueError) as info:
        lfric_psyir.LfricDimension("2")
    assert ("An LFRic dimension object must be '1' or '3', but found '2'."
            in str(info.value))
    # LFRIC_SCALAR_DIMENSION instance
    assert isinstance(lfric_psyir.LFRIC_SCALAR_DIMENSION,
                      lfric_psyir.LfricDimension)
    assert lfric_psyir.LFRIC_SCALAR_DIMENSION.value == "1"
    # LFRIC_VECTOR_DIMENSION instance
    assert isinstance(lfric_psyir.LFRIC_VECTOR_DIMENSION,
                      lfric_psyir.LfricDimension)
    assert lfric_psyir.LFRIC_VECTOR_DIMENSION.value == "3"


# Specific scalar datatypes
@pytest.mark.parametrize("data_type, generic_type", [
    (lfric_psyir.CellPositionDataType,
     lfric_psyir.LfricIntegerScalarDataType),
    (lfric_psyir.MeshHeightDataType,
     lfric_psyir.LfricIntegerScalarDataType),
    (lfric_psyir.NumberOfCellsDataType,
     lfric_psyir.LfricIntegerScalarDataType),
    (lfric_psyir.NumberOfDofsDataType,
     lfric_psyir.LfricIntegerScalarDataType),
    (lfric_psyir.NumberOfUniqueDofsDataType,
     lfric_psyir.LfricIntegerScalarDataType),
    (lfric_psyir.NumberOfFacesDataType,
     lfric_psyir.LfricIntegerScalarDataType),
    (lfric_psyir.NumberOfEdgesDataType,
     lfric_psyir.LfricIntegerScalarDataType),
    (lfric_psyir.NumberOfQrPointsInXyDataType,
     lfric_psyir.LfricIntegerScalarDataType),
    (lfric_psyir.NumberOfQrPointsInZDataType,
     lfric_psyir.LfricIntegerScalarDataType),
    (lfric_psyir.NumberOfQrPointsInFacesDataType,
     lfric_psyir.LfricIntegerScalarDataType),
    (lfric_psyir.NumberOfQrPointsInEdgesDataType,
     lfric_psyir.LfricIntegerScalarDataType)])
def test_specific_scalar_types(data_type, generic_type):
    '''Test the generated specific scalar datatypes are created correctly.

    '''
    lfric_datatype = data_type()
    assert isinstance(lfric_datatype, generic_type)


# Specific scalar symbols
@pytest.mark.parametrize("symbol, generic_symbol, attribute_map", [
    (lfric_psyir.CellPositionDataSymbol,
     lfric_psyir.LfricIntegerScalarDataSymbol, {}),
    (lfric_psyir.MeshHeightDataSymbol,
     lfric_psyir.LfricIntegerScalarDataSymbol, {}),
    (lfric_psyir.NumberOfCellsDataSymbol,
     lfric_psyir.LfricIntegerScalarDataSymbol, {}),
    (lfric_psyir.NumberOfDofsDataSymbol,
     lfric_psyir.LfricIntegerScalarDataSymbol, {"fs": "w3"}),
    (lfric_psyir.NumberOfUniqueDofsDataSymbol,
     lfric_psyir.LfricIntegerScalarDataSymbol, {"fs": "w2"}),
    (lfric_psyir.NumberOfFacesDataSymbol,
     lfric_psyir.LfricIntegerScalarDataSymbol, {}),
    (lfric_psyir.NumberOfEdgesDataSymbol,
     lfric_psyir.LfricIntegerScalarDataSymbol, {}),
    (lfric_psyir.NumberOfQrPointsInXyDataSymbol,
     lfric_psyir.LfricIntegerScalarDataSymbol, {}),
    (lfric_psyir.NumberOfQrPointsInZDataSymbol,
     lfric_psyir.LfricIntegerScalarDataSymbol, {}),
    (lfric_psyir.NumberOfQrPointsInFacesDataSymbol,
     lfric_psyir.LfricIntegerScalarDataSymbol, {}),
    (lfric_psyir.NumberOfQrPointsInEdgesDataSymbol,
     lfric_psyir.LfricIntegerScalarDataSymbol, {})])
def test_specific_scalar_symbols(symbol, generic_symbol, attribute_map):
    '''Test the generated specific scalar symbols are
    created correctly.

    '''
    args = ["symbol"] + list(attribute_map.values())
    lfric_symbol = symbol(*args)
    assert isinstance(lfric_symbol, generic_symbol)
    assert lfric_symbol.name == "symbol"
    assert isinstance(lfric_symbol.interface, LocalInterface)
    for attribute in attribute_map:
        assert getattr(lfric_symbol, attribute) == attribute_map[attribute]
    lfric_symbol = symbol(
        *args, interface=ArgumentInterface(ArgumentInterface.Access.READ))
    assert isinstance(lfric_symbol.interface, ArgumentInterface)
    assert lfric_symbol.interface.access == ArgumentInterface.Access.READ


# Specific scalar datatypes
@pytest.mark.parametrize(
    "data_type_name, symbol_name, scalar_type, dims, attribute_map",
    [("RealFieldDataDataType", "RealFieldDataDataSymbol",
      lfric_psyir.LfricRealScalarDataType,
      [Reference(
          lfric_psyir.NumberOfUniqueDofsDataSymbol(
              "ndofs", "w0",
              interface=ArgumentInterface(ArgumentInterface.Access.READ)))],
      {"fs": "w0"}),
     ("IntegerFieldDataDataType", "IntegerFieldDataDataSymbol",
      lfric_psyir.LfricIntegerScalarDataType,
      [Reference(
          lfric_psyir.NumberOfUniqueDofsDataSymbol(
              "ndofs", "w1",
              interface=ArgumentInterface(ArgumentInterface.Access.READ)))],
      {"fs": "w1"}),
     ("LogicalFieldDataDataType", "LogicalFieldDataDataSymbol",
      lfric_psyir.LfricLogicalScalarDataType,
      [Reference(
          lfric_psyir.NumberOfUniqueDofsDataSymbol(
              "ndofs", "w2",
              interface=ArgumentInterface(ArgumentInterface.Access.READ)))],
      {"fs": "w2"}),
     ("OperatorDataType", "OperatorDataSymbol",
      lfric_psyir.LfricRealScalarDataType,
      [Reference(
          lfric_psyir.NumberOfDofsDataSymbol(
              "ndofs", "w3",
              interface=ArgumentInterface(ArgumentInterface.Access.READ))),
       Reference(
           lfric_psyir.NumberOfDofsDataSymbol(
               "ndofs", "w3",
               interface=ArgumentInterface(ArgumentInterface.Access.READ))),
       Reference(lfric_psyir.NumberOfCellsDataSymbol(
           "ncells",
           interface=ArgumentInterface(ArgumentInterface.Access.READ)))],
     {"fs_from": "w3", "fs_to": "w3"}),
     ("DofMapDataType", "DofMapDataSymbol",
      lfric_psyir.LfricIntegerScalarDataType,
      [Reference(
          lfric_psyir.NumberOfDofsDataSymbol(
              "ndofs", "w3",
              interface=ArgumentInterface(ArgumentInterface.Access.READ)))],
      {"fs": "w3"}),
     ("BasisFunctionQrXyozDataType", "BasisFunctionQrXyozDataSymbol",
      lfric_psyir.LfricRealScalarDataType,
      [1, Reference(
          lfric_psyir.NumberOfDofsDataSymbol(
              "ndofs", "w0",
              interface=ArgumentInterface(ArgumentInterface.Access.READ))),
       Reference(
           lfric_psyir.NumberOfQrPointsInXyDataSymbol(
               "qr_xy",
               interface=ArgumentInterface(ArgumentInterface.Access.READ))),
       Reference(
           lfric_psyir.NumberOfQrPointsInZDataSymbol(
               "qr_z",
               interface=ArgumentInterface(ArgumentInterface.Access.READ)))],
      {"fs": "w0"}),
     ("BasisFunctionQrFaceDataType", "BasisFunctionQrFaceDataSymbol",
      lfric_psyir.LfricRealScalarDataType,
      [3, Reference(
          lfric_psyir.NumberOfDofsDataSymbol(
              "ndofs", "w1",
              interface=ArgumentInterface(ArgumentInterface.Access.READ))),
       Reference(
           lfric_psyir.NumberOfQrPointsInFacesDataSymbol(
               "qr",
               interface=ArgumentInterface(ArgumentInterface.Access.READ))),
       Reference(
           lfric_psyir.NumberOfFacesDataSymbol(
               "nfaces",
               interface=ArgumentInterface(ArgumentInterface.Access.READ)))],
      {"fs": "w1"}),
     ("BasisFunctionQrEdgeDataType", "BasisFunctionQrEdgeDataSymbol",
      lfric_psyir.LfricRealScalarDataType,
      [1, Reference(
          lfric_psyir.NumberOfDofsDataSymbol(
              "ndofs", "w2",
              interface=ArgumentInterface(ArgumentInterface.Access.READ))),
       Reference(lfric_psyir.NumberOfQrPointsInEdgesDataSymbol(
           "qr",
           interface=ArgumentInterface(ArgumentInterface.Access.READ))),
       Reference(
           lfric_psyir.NumberOfEdgesDataSymbol(
               "nedges",
               interface=ArgumentInterface(ArgumentInterface.Access.READ)))],
      {"fs": "w2"}),
     ("DiffBasisFunctionQrXyozDataType", "DiffBasisFunctionQrXyozDataSymbol",
      lfric_psyir.LfricRealScalarDataType,
      [3, Reference(
          lfric_psyir.NumberOfDofsDataSymbol(
              "ndofs", "wtheta",
              interface=ArgumentInterface(ArgumentInterface.Access.READ))),
       Reference(
           lfric_psyir.NumberOfQrPointsInXyDataSymbol(
               "qr_xy",
               interface=ArgumentInterface(ArgumentInterface.Access.READ))),
       Reference(
           lfric_psyir.NumberOfQrPointsInZDataSymbol(
               "qr_z",
               interface=ArgumentInterface(ArgumentInterface.Access.READ)))],
      {"fs": "wtheta"}),
     ("DiffBasisFunctionQrFaceDataType", "DiffBasisFunctionQrFaceDataSymbol",
      lfric_psyir.LfricRealScalarDataType,
      [3, Reference(
          lfric_psyir.NumberOfDofsDataSymbol(
              "ndofs", "w1",
              interface=ArgumentInterface(ArgumentInterface.Access.READ))),
       Reference(lfric_psyir.NumberOfQrPointsInFacesDataSymbol(
           "qr",
           interface=ArgumentInterface(ArgumentInterface.Access.READ))),
       Reference(
           lfric_psyir.NumberOfFacesDataSymbol(
               "nfaces",
               interface=ArgumentInterface(ArgumentInterface.Access.READ)))],
      {"fs": "w1"}),
     ("DiffBasisFunctionQrEdgeDataType", "DiffBasisFunctionQrEdgeDataSymbol",
      lfric_psyir.LfricRealScalarDataType,
      [1, Reference(
          lfric_psyir.NumberOfDofsDataSymbol(
              "ndofs", "w2v",
              interface=ArgumentInterface(ArgumentInterface.Access.READ))),
       Reference(
           lfric_psyir.NumberOfQrPointsInEdgesDataSymbol(
               "qr",
               interface=ArgumentInterface(ArgumentInterface.Access.READ))),
       Reference(
           lfric_psyir.NumberOfEdgesDataSymbol(
               "nedges",
               interface=ArgumentInterface(ArgumentInterface.Access.READ)))],
      {"fs": "w2v"}),
     ("QrWeightsInXyDataType", "QrWeightsInXyDataSymbol",
      lfric_psyir.LfricRealScalarDataType,
      [Reference(
          lfric_psyir.NumberOfQrPointsInXyDataSymbol(
              "qr_xy",
              interface=ArgumentInterface(ArgumentInterface.Access.READ)))],
      {}),
     ("QrWeightsInZDataType", "QrWeightsInZDataSymbol",
      lfric_psyir.LfricRealScalarDataType,
      [Reference(
          lfric_psyir.NumberOfQrPointsInZDataSymbol(
              "qr_z",
              interface=ArgumentInterface(ArgumentInterface.Access.READ)))],
      {}),
     ("QrWeightsInFacesDataType", "QrWeightsInFacesDataSymbol",
      lfric_psyir.LfricRealScalarDataType,
      [Reference(
          lfric_psyir.NumberOfQrPointsInFacesDataSymbol(
              "qr",
              interface=ArgumentInterface(ArgumentInterface.Access.READ)))],
      {}),
     ("QrWeightsInEdgesDataType", "QrWeightsInEdgesDataSymbol",
      lfric_psyir.LfricRealScalarDataType,
      [Reference(
          lfric_psyir.NumberOfQrPointsInEdgesDataSymbol(
              "qr",
              visibility=Symbol.Visibility.PRIVATE,
              interface=ArgumentInterface(ArgumentInterface.Access.READ)))],
      {})])
def test_arrays(data_type_name, symbol_name, scalar_type, dims, attribute_map):
    '''Test the generated array datatypes and datasymbols are created
    correctly. This includes field datatypes and symbols which are
    kept as a separate list in psyir.py

    '''
    # Datatype creation
    lfric_types = LFRicTypes.get()
    data_type = lfric_types(data_type_name)
    lfric_datatype = data_type(dims)
    assert isinstance(lfric_datatype, ArrayType)
    assert isinstance(lfric_datatype._datatype, scalar_type)
    for idx, dim in enumerate(lfric_datatype.shape):
        if isinstance(dim.upper, Literal):
            assert dim.upper.value == str(dims[idx])
        elif isinstance(dim.upper, Reference):
            assert dim.upper is dims[idx]
            assert dim.upper.symbol is dims[idx].symbol
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
    print("CREATING symbol", symbol_name)
    symbol = lfric_types(symbol_name)
    lfric_symbol = symbol("symbol", dims, *args)
    assert isinstance(lfric_symbol, DataSymbol)
    assert lfric_symbol.name == "symbol"
    assert isinstance(lfric_symbol.interface, LocalInterface)
    assert isinstance(lfric_symbol.datatype, data_type)
    lfric_symbol = symbol(
        "symbol", dims, *args,
        interface=ArgumentInterface(ArgumentInterface.Access.READ))
    assert isinstance(lfric_symbol.interface, ArgumentInterface)
    assert lfric_symbol.interface.access == ArgumentInterface.Access.READ


# Vector field-data data-symbols
@pytest.mark.parametrize(
    "symbol, parent_symbol, dims, attribute_map",
    [("RealVectorFieldDataDataSymbol",
      "RealFieldDataDataSymbol",
      [Reference(
          lfric_psyir.NumberOfUniqueDofsDataSymbol(
              "ndofs", "w0",
              interface=ArgumentInterface(ArgumentInterface.Access.READ)))],
      {"fs": "w0"}),
     ("IntegerVectorFieldDataDataSymbol",
      "IntegerFieldDataDataSymbol",
      [Reference(
          lfric_psyir.NumberOfUniqueDofsDataSymbol(
              "ndofs", "w1",
              visibility=Symbol.Visibility.PUBLIC,
              interface=ArgumentInterface(ArgumentInterface.Access.READ)))],
      {"fs": "w1"}),
     ("LogicalVectorFieldDataDataSymbol",
      "LogicalFieldDataDataSymbol",
      [Reference(
          lfric_psyir.NumberOfUniqueDofsDataSymbol(
              "ndofs", "w2",
              visibility=Symbol.Visibility.PRIVATE,
              interface=ArgumentInterface(ArgumentInterface.Access.READ)))],
      {"fs": "w2"})])
def test_vector_fields(symbol, parent_symbol, dims, attribute_map):
    '''Test the generated vector field datasymbols are created
    correctly. These are straight subclasses of the equivalent field
    datasymbols.

    '''
    lfric_types = LFRicTypes.get()

    args = list(attribute_map.values())
    lfric_symbol = lfric_types(symbol)("symbol", dims, *args)
    assert isinstance(lfric_symbol, lfric_types(parent_symbol))
    assert lfric_symbol.name == "symbol"
