# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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
# Author R. W. Ford STFC Daresbury Lab

'''Test that the LFRic-specific PSyIR classes are created and declared
correctly'''

# pylint: disable=no-name-in-module
from __future__ import absolute_import
import pytest
from psyclone.domain.lfric.psyir import \
    CONSTANTS_MOD, I_DEF, R_DEF, L_DEF, \
    \
    LfricRealScalarDataType, LfricRealScalarDataSymbol, \
    LfricIntegerScalarDataType, LfricIntegerScalarDataSymbol, \
    LfricLogicalScalarDataType, LfricLogicalScalarDataSymbol, \
    \
    CellPositionDataType, CellPositionDataSymbol, \
    MeshHeightDataType, MeshHeightDataSymbol, \
    NumberOfCellsDataType, NumberOfCellsDataSymbol, \
    NumberOfDofsDataType, NumberOfDofsDataSymbol, \
    NumberOfUniqueDofsDataType, NumberOfUniqueDofsDataSymbol, \
    NumberOfFacesDataType, NumberOfFacesDataSymbol, \
    NumberOfEdgesDataType, NumberOfEdgesDataSymbol, \
    NumberOfQrPointsInHorizontalDataType, \
    NumberOfQrPointsInHorizontalDataSymbol, \
    NumberOfQrPointsInVerticalDataType, NumberOfQrPointsInVerticalDataSymbol, \
    NumberOfQrPointsDataType, NumberOfQrPointsDataSymbol, \
    \
    RealFieldDataDataType, RealFieldDataDataSymbol, \
    IntegerFieldDataDataType, IntegerFieldDataDataSymbol, \
    LogicalFieldDataDataType, LogicalFieldDataDataSymbol, \
    \
    OperatorDataType, OperatorDataSymbol, \
    DofMapDataType, DofMapDataSymbol, \
    BasisFunctionQrXyozDataType, BasisFunctionQrXyozDataSymbol, \
    BasisFunctionQrFaceDataType, BasisFunctionQrFaceDataSymbol, \
    BasisFunctionQrEdgeDataType, BasisFunctionQrEdgeDataSymbol, \
    DiffBasisFunctionQrXyozDataType, DiffBasisFunctionQrXyozDataSymbol, \
    DiffBasisFunctionQrFaceDataType, DiffBasisFunctionQrFaceDataSymbol, \
    DiffBasisFunctionQrEdgeDataType, DiffBasisFunctionQrEdgeDataSymbol, \
    QrWeightsInHorizontalDataType, QrWeightsInHorizontalDataSymbol, \
    QrWeightsInVerticalDataType, QrWeightsInVerticalDataSymbol, \
    QrWeightsDataType, QrWeightsDataSymbol, \
    \
    RealVectorFieldDataDataSymbol, IntegerVectorFieldDataDataSymbol, \
    LogicalVectorFieldDataDataSymbol
from psyclone.psyir.symbols import ContainerSymbol, DataSymbol, \
    GlobalInterface, ScalarType, LocalInterface, ArgumentInterface, \
    ArrayType


# Modules and their arguments
@pytest.mark.parametrize("module, symbol_list",
                         [(CONSTANTS_MOD, [I_DEF, R_DEF, L_DEF])])
def test_constants_mod(module, symbol_list):
    '''Test the generated module symbol and its argument symbols are
    created correctly.

    '''
    assert module
    assert isinstance(module, ContainerSymbol)
    for symbol in symbol_list:
        assert symbol
        assert isinstance(symbol, DataSymbol)
        assert isinstance(symbol.interface, GlobalInterface)
        assert symbol.interface.container_symbol is module


# Generic scalars
@pytest.mark.parametrize("data_type, symbol, intrinsic, precision", [
    (LfricIntegerScalarDataType, LfricIntegerScalarDataSymbol,
     ScalarType.Intrinsic.INTEGER, I_DEF),
    (LfricRealScalarDataType, LfricRealScalarDataSymbol,
     ScalarType.Intrinsic.REAL, R_DEF),
    (LfricLogicalScalarDataType, LfricLogicalScalarDataSymbol,
     ScalarType.Intrinsic.BOOLEAN, L_DEF)])
def test_generic_scalars(data_type, symbol, intrinsic, precision):
    '''Test the generated generic scalar datatypes and symbols are created
    correctly.

    '''
    # datatype
    lfric_datatype = data_type()
    assert lfric_datatype.intrinsic == intrinsic
    assert lfric_datatype.precision is precision
    # symbol
    lfric_symbol = symbol("symbol")
    assert lfric_symbol.name == "symbol"
    assert isinstance(lfric_symbol.interface, LocalInterface)
    assert isinstance(lfric_symbol.datatype, data_type)
    lfric_symbol = symbol(
        "symbol", interface=ArgumentInterface(ArgumentInterface.Access.READ))
    assert isinstance(lfric_symbol.interface, ArgumentInterface)
    assert lfric_symbol.interface.access == ArgumentInterface.Access.READ


# Specific scalar datatypes
@pytest.mark.parametrize("data_type, generic_type", [
    (CellPositionDataType, LfricIntegerScalarDataType),
    (MeshHeightDataType, LfricIntegerScalarDataType),
    (NumberOfCellsDataType, LfricIntegerScalarDataType),
    (NumberOfDofsDataType, LfricIntegerScalarDataType),
    (NumberOfUniqueDofsDataType, LfricIntegerScalarDataType),
    (NumberOfFacesDataType, LfricIntegerScalarDataType),
    (NumberOfEdgesDataType, LfricIntegerScalarDataType),
    (NumberOfQrPointsInHorizontalDataType, LfricIntegerScalarDataType),
    (NumberOfQrPointsInVerticalDataType, LfricIntegerScalarDataType),
    (NumberOfQrPointsDataType, LfricIntegerScalarDataType)])
def test_specific_scalar_types(data_type, generic_type):
    '''Test the generated specific scalar datatypes are created correctly.

    '''
    lfric_datatype = data_type()
    assert isinstance(lfric_datatype, generic_type)


# Specific scalar symbols
@pytest.mark.parametrize("symbol, generic_symbol, attribute_map", [
    (CellPositionDataSymbol, LfricIntegerScalarDataSymbol, {}),
    (MeshHeightDataSymbol, LfricIntegerScalarDataSymbol, {}),
    (NumberOfCellsDataSymbol, LfricIntegerScalarDataSymbol, {}),
    (NumberOfDofsDataSymbol, LfricIntegerScalarDataSymbol, {"fs": "w3"}),
    (NumberOfUniqueDofsDataSymbol, LfricIntegerScalarDataSymbol, {"fs": "w2"}),
    (NumberOfFacesDataSymbol, LfricIntegerScalarDataSymbol, {}),
    (NumberOfEdgesDataSymbol, LfricIntegerScalarDataSymbol, {}),
    (NumberOfQrPointsInHorizontalDataSymbol, LfricIntegerScalarDataSymbol, {}),
    (NumberOfQrPointsInVerticalDataSymbol, LfricIntegerScalarDataSymbol, {}),
    (NumberOfQrPointsDataSymbol, LfricIntegerScalarDataSymbol, {})])
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
    "data_type, symbol, scalar_type, dims, attribute_map",
    [(RealFieldDataDataType, RealFieldDataDataSymbol, LfricRealScalarDataType,
      [NumberOfUniqueDofsDataSymbol("ndofs", "w0")], {"fs": "w0"}),
     (IntegerFieldDataDataType, IntegerFieldDataDataSymbol,
      LfricIntegerScalarDataType,
      [NumberOfUniqueDofsDataSymbol("ndofs", "w1")], {"fs": "w1"}),
     (LogicalFieldDataDataType, LogicalFieldDataDataSymbol,
      LfricLogicalScalarDataType,
      [NumberOfUniqueDofsDataSymbol("ndofs", "w2")], {"fs": "w2"}),
     (OperatorDataType, OperatorDataSymbol, LfricRealScalarDataType,
      [NumberOfDofsDataSymbol("ndofs", "w3"),
       NumberOfDofsDataSymbol("ndofs", "w3"),
       NumberOfCellsDataSymbol("ncells")], {"fs_from": "w3", "fs_to": "w3"}),
     (DofMapDataType, DofMapDataSymbol, LfricIntegerScalarDataType,
      [NumberOfDofsDataSymbol("ndofs", "w3")], {"fs": "w0"}),
     (BasisFunctionQrXyozDataType, BasisFunctionQrXyozDataSymbol,
      LfricRealScalarDataType,
      [1, NumberOfDofsDataSymbol("ndofs", "w3"),
       NumberOfQrPointsInHorizontalDataSymbol("qr_h"),
       NumberOfQrPointsInVerticalDataSymbol("qr_v")], {"fs": "w0"}),
     (BasisFunctionQrFaceDataType, BasisFunctionQrFaceDataSymbol,
      LfricRealScalarDataType,
      [3, NumberOfDofsDataSymbol("ndofs", "w3"),
       NumberOfQrPointsDataSymbol("qr"),
       NumberOfFacesDataSymbol("nfaces")], {"fs": "w0"}),
     (BasisFunctionQrEdgeDataType, BasisFunctionQrEdgeDataSymbol,
      LfricRealScalarDataType,
      [1, NumberOfDofsDataSymbol("ndofs", "w3"),
       NumberOfQrPointsDataSymbol("qr"),
       NumberOfEdgesDataSymbol("nedges")], {"fs": "w0"}),
     (DiffBasisFunctionQrXyozDataType, DiffBasisFunctionQrXyozDataSymbol,
      LfricRealScalarDataType,
      [3, NumberOfDofsDataSymbol("ndofs", "w3"),
       NumberOfQrPointsInHorizontalDataSymbol("qr_h"),
       NumberOfQrPointsInVerticalDataSymbol("qr_v")], {"fs": "w0"}),
     (DiffBasisFunctionQrFaceDataType, DiffBasisFunctionQrFaceDataSymbol,
      LfricRealScalarDataType,
      [3, NumberOfDofsDataSymbol("ndofs", "w3"),
       NumberOfQrPointsDataSymbol("qr"),
       NumberOfFacesDataSymbol("nfaces")], {"fs": "w0"}),
     (DiffBasisFunctionQrEdgeDataType, DiffBasisFunctionQrEdgeDataSymbol,
      LfricRealScalarDataType,
      [1, NumberOfDofsDataSymbol("ndofs", "w3"),
       NumberOfQrPointsDataSymbol("qr"),
       NumberOfEdgesDataSymbol("nedges")], {"fs": "w0"}),
     (QrWeightsInHorizontalDataType, QrWeightsInHorizontalDataSymbol,
      LfricRealScalarDataType,
      [NumberOfQrPointsInHorizontalDataSymbol("qr_h")], {}),
     (QrWeightsInVerticalDataType, QrWeightsInVerticalDataSymbol,
      LfricRealScalarDataType,
      [NumberOfQrPointsInVerticalDataSymbol("qr_v")], {}),
     (QrWeightsDataType, QrWeightsDataSymbol, LfricRealScalarDataType,
      [NumberOfQrPointsDataSymbol("qr")], {})])
def test_arrays(data_type, symbol, scalar_type, dims, attribute_map):
    '''Test the generated array datatypes and datasymbols are created
    correctly. This includes field datatypes and symbols which are
    kept as a separate list in psyir.py

    '''
    # Datatype creation
    lfric_datatype = data_type(dims)
    assert isinstance(lfric_datatype, ArrayType)
    assert isinstance(lfric_datatype._datatype, scalar_type)
    assert lfric_datatype.shape is dims
    # Wrong number of dims
    with pytest.raises(TypeError) as info:
        _ = data_type([])
    assert ("{0} expected the number of supplied dimensions to be {1} but "
            "found 0.".format(type(lfric_datatype).__name__, len(dims))
            in str(info.value))
    # Datasymbol creation
    args = list(attribute_map.values())
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
    [(RealVectorFieldDataDataSymbol, RealFieldDataDataSymbol,
      [NumberOfUniqueDofsDataSymbol("ndofs", "w0")], {"fs": "w0"}),
     (IntegerVectorFieldDataDataSymbol, IntegerFieldDataDataSymbol,
      [NumberOfUniqueDofsDataSymbol("ndofs", "w1")], {"fs": "w1"}),
     (LogicalVectorFieldDataDataSymbol, LogicalFieldDataDataSymbol,
      [NumberOfUniqueDofsDataSymbol("ndofs", "w2")], {"fs": "w2"})])
def test_vector_fields(symbol, parent_symbol, dims, attribute_map):
    '''Test the generated vector field datasymbols are created
    correctly. These are straight subclasses of the equivalent field
    datasymbols.

    '''
    args = list(attribute_map.values())
    lfric_symbol = symbol("symbol", dims, *args)
    assert isinstance(lfric_symbol, parent_symbol)
    assert lfric_symbol.name == "symbol"
