# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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
# Author: R. W. Ford, STFC Daresbury Lab

'''This module tests the FormalKernelArgsFromMetadata class.'''

from collections import OrderedDict

import pytest

from psyclone.domain import lfric
from psyclone.errors import InternalError
from psyclone.psyir import symbols


def call_method(method_name, *args, metadata=None):
    '''Utility function that initialises the FormalKernelArgsFromMetadata
    class with optional metadata and a symbol table, then calls the
    class method specified in argument 'method_name' with the the
    arguments specified in argument *args and returns the class.

    :param str method_name: the name of the method to test.
    :param metadata: optional metadata required by some methods.
    :type metadata: Optional[ \
        :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`]

    :returns: a FormalKernelArgsFromMetadata class after the supplied \
        class method has been called.
    :rtype: :py:class:`psyclone.domain.lfric.FormalKernelArgsFromMetadata`

    '''
    cls = lfric.FormalKernelArgsFromMetadata
    cls._info = symbols.SymbolTable()
    cls._metadata = metadata
    getattr(cls, method_name)(*args)
    return cls


def check_single_symbol(
        method_name, datasymbol_name, symbol_name, *args, metadata=None):
    '''Utility function that calls the method in argument 'method_name'
    with the arguments stored in argument '*args' and checks that as a
    result a symbol with name 'symbol_name' of type 'datasymbol_name'
    is created. This function tests methods where a single symbol is
    created.

    :param str method_name: the name of the method to test.
    :param str datasymbol_name: the name of the expected symbol type \
        that is created.
    :param str symbol_name: the expected name of the created symbol.
    :param metadata: optional metadata required by some methods.
    :type metadata: Optional[ \
        :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`]

    :returns: xxx

    '''
    cls = call_method(method_name, *args, metadata=metadata)
    lfric_class = lfric.LFRicTypes(datasymbol_name)
    symbol = cls._info.lookup(symbol_name)
    # pylint gets confused here
    # pylint: disable=isinstance-second-argument-not-valid-type
    assert isinstance(symbol, lfric_class)
    # pylint: enable=isinstance-second-argument-not-valid-type
    assert len(cls._info._argument_list) == 1
    assert cls._info._argument_list[0] is symbol
    return cls


def check_symbols(cls, symbol_dict):
    ''' xxx '''
    for symbol_name, lfric_class in symbol_dict.items():
        assert isinstance(cls._info.lookup(symbol_name), lfric_class)


def check_arg_symbols(cls, symbol_dict):
    ''' xxx '''
    check_symbols(cls, symbol_dict)
    assert len(cls._info._argument_list) == len(symbol_dict)
    for idx, symbol_name in enumerate(symbol_dict.keys()):
        assert cls._info._argument_list[idx].name == symbol_name


# TODO test _initialise


def test_cell_position():
    ''' Test _cell_position method. '''
    check_single_symbol("_cell_position", "CellPositionDataSymbol", "cell")


def test_mesh_height():
    ''' Test _mesh_height method. '''
    check_single_symbol("_mesh_height", "MeshHeightDataSymbol", "nlayers")


def test_mesh_ncell2d_no_halos():
    ''' Test _mesh_ncell2d_no_halos method. '''
    check_single_symbol(
        "_mesh_ncell2d_no_halos", "LFRicIntegerScalarDataSymbol",
        "ncell_2d_no_halos")


def test_mesh_ncell2d():
    ''' Test _mesh_ncell2d method. '''
    symbol_name = "ncell_2d"
    cls = check_single_symbol(
        "_mesh_ncell2d", "LFRicIntegerScalarDataSymbol", symbol_name)
    # Check that the symbol remains unchanged if it has already been declared.
    symbol = cls._info.lookup_with_tag(symbol_name)
    symbol_id = id(symbol)
    # Reset the argument list as '_mesh_ncell2d' adds the symbol to
    # the symbol table and to the argument list whereas if it had
    # already been declared by another method it will have only been
    # added to the symbol table.
    cls._info._argument_list = []
    cls._mesh_ncell2d()
    symbol = cls._info.lookup_with_tag(symbol_name)
    assert symbol_id == id(symbol)
    assert len(cls._info._argument_list) == 1
    assert cls._info._argument_list[0] is symbol


def test_cell_map():
    ''' Test _cell_map method. '''
    cls = call_method("_cell_map")
    lfric_class = lfric.LFRicTypes("LFRicIntegerScalarDataSymbol")
    # Symbols added to the symbol table and to the argument list.
    check_arg_symbols(cls, OrderedDict(
        [("cell_map", symbols.DataSymbol), ("ncell_f_per_c_x", lfric_class),
         ("ncell_f_per_c_y", lfric_class), ("ncell_f", lfric_class)]))


def test_scalar():
    ''' Test _scalar method. '''
    # At least one field arg is required for the metadata to be valid
    # even though we only want to test the scalar metadata.
    field_meta_arg = lfric.kernel.FieldArgMetadata("GH_REAL", "GH_WRITE", "W3")
    scalar_meta_arg = lfric.kernel.ScalarArgMetadata("GH_REAL", "GH_READ")
    metadata = lfric.kernel.LFRicKernelMetadata(
        operates_on="cell_column", meta_args=[field_meta_arg, scalar_meta_arg])
    metadata.validate()
    check_single_symbol(
        "_scalar", "LFRicRealScalarDataSymbol", "rscalar_2", scalar_meta_arg,
        metadata=metadata)


def test_field():
    ''' Test _field method. '''
    field_meta_arg = lfric.kernel.FieldArgMetadata("GH_REAL", "GH_WRITE", "W3")
    metadata = lfric.kernel.LFRicKernelMetadata(
        operates_on="cell_column", meta_args=[field_meta_arg])
    metadata.validate()
    cls = check_single_symbol(
        "_field", "RealFieldDataSymbol", "rfield_1", field_meta_arg,
        metadata=metadata)
    lfric_class = lfric.LFRicTypes("NumberOfUniqueDofsDataSymbol")
    # Symbols added to the symbol table but not to the argument list.
    check_symbols(cls, {"undf_w3": lfric_class})


def test_field_vector():
    ''' Test _field_vector method. '''
    field_meta_arg = lfric.kernel.FieldVectorArgMetadata(
        "GH_REAL", "GH_WRITE", "W3", "3")
    metadata = lfric.kernel.LFRicKernelMetadata(
        operates_on="cell_column", meta_args=[field_meta_arg])
    metadata.validate()
    cls = call_method("_field_vector", field_meta_arg, metadata=metadata)
    lfric_class = lfric.LFRicTypes("RealFieldDataSymbol")
    # Symbols added to the symbol table and to the argument list.
    check_arg_symbols(cls, OrderedDict(
        [("rfield_1_v1", lfric_class), ("rfield_1_v2", lfric_class),
         ("rfield_1_v3", lfric_class)]))
    lfric_class = lfric.LFRicTypes("NumberOfUniqueDofsDataSymbol")
    # Symbols added to the symbol table but not to the argument list.
    check_symbols(cls, {"undf_w3": lfric_class})


def test_operator():
    ''' Test _operator method. '''
    operator_meta_arg = lfric.kernel.OperatorArgMetadata(
        "GH_REAL", "GH_WRITE", "W3", "W2")
    metadata = lfric.kernel.LFRicKernelMetadata(
        operates_on="cell_column", meta_args=[operator_meta_arg])
    metadata.validate()
    cls = call_method("_operator", operator_meta_arg, metadata=metadata)
    lfric_int_class = lfric.LFRicTypes("LFRicIntegerScalarDataSymbol")
    lfric_op_class = lfric.LFRicTypes("OperatorDataSymbol")
    # Symbols added to the symbol table and to the argument list.
    check_arg_symbols(cls, OrderedDict(
        [("op_1_ncell_3d", lfric_int_class), ("op_1", lfric_op_class)]))
    lfric_dofs_class = lfric.LFRicTypes("NumberOfDofsDataSymbol")
    # Symbols added to the symbol table but not to the argument list.
    check_symbols(
        cls, {"ndf_w3": lfric_dofs_class, "ndf_w2": lfric_dofs_class})


def check_common_cma_symbols(fs1, fs2):
    ''' xxx '''
    operator_meta_arg = lfric.kernel.ColumnwiseOperatorArgMetadata(
        "GH_REAL", "GH_WRITE", fs1, fs2)
    metadata = lfric.kernel.LFRicKernelMetadata(
        operates_on="cell_column", meta_args=[operator_meta_arg])
    metadata.validate()
    cls = call_method("_cma_operator", operator_meta_arg, metadata=metadata)
    return cls


def test_cma_operator():
    ''' Test _cma_operator method. '''
    # to/from function spaces differ so there is an additional
    # argument.
    cls = check_common_cma_symbols("W3", "W2")
    lfric_op_class = lfric.LFRicTypes("OperatorDataSymbol")
    lfric_int_class = lfric.LFRicTypes("LFRicIntegerScalarDataSymbol")
    check_arg_symbols(cls, OrderedDict(
        [("cma_op_1", lfric_op_class),
         ("nrow_cma_op_1", lfric_int_class),
         ("ncol_cma_op_1", lfric_int_class),
         ("bandwidth_cma_op_1", lfric_int_class),
         ("alpha_cma_op_1", lfric_int_class),
         ("beta_cma_op_1", lfric_int_class),
         ("gamma_m_cma_op_1", lfric_int_class),
         ("gamma_p_cma_op_1", lfric_int_class)]))
    check_symbols(cls, {"ncell_2d": lfric_int_class})

    # to/from function spaces are the same so there is no additional
    # argument.
    cls = check_common_cma_symbols("W3", "W3")
    with pytest.raises(KeyError):
        cls._info.lookup("ncol_cma_op_1")
    check_arg_symbols(cls, OrderedDict(
        [("cma_op_1", lfric_op_class),
         ("nrow_cma_op_1", lfric_int_class),
         ("bandwidth_cma_op_1", lfric_int_class),
         ("alpha_cma_op_1", lfric_int_class),
         ("beta_cma_op_1", lfric_int_class),
         ("gamma_m_cma_op_1", lfric_int_class),
         ("gamma_p_cma_op_1", lfric_int_class)]))
    check_symbols(cls, {"ncell_2d": lfric_int_class})


# pylint: disable=too-many-statements
def test_ref_element_properties(monkeypatch):
    ''' Test _ref_element_properties method. '''
    lfric_qr_xy_class = lfric.LFRicTypes("NumberOfQrPointsInXyDataSymbol")
    lfric_qr_z_class = lfric.LFRicTypes("NumberOfQrPointsInZDataSymbol")
    lfric_qr_faces_class = lfric.LFRicTypes(
        "NumberOfQrPointsInFacesDataSymbol")

    # Horizontal
    meta_ref_element = [
        lfric.kernel.MetaRefElementArgMetadata("normals_to_horizontal_faces")]
    cls = call_method("_ref_element_properties", meta_ref_element)
    check_arg_symbols(cls, OrderedDict([
        ("nfaces_re_h", lfric_qr_xy_class),
        ("normals_to_horizontal_faces", symbols.DataSymbol)]))
    symbol = cls._info.lookup("normals_to_horizontal_faces")
    assert symbol.is_array
    assert len(symbol.datatype.shape) == 2
    assert symbol.datatype.shape[0].upper.value == "3"
    assert symbol.datatype.shape[1].upper.symbol.name == "nfaces_re_h"

    # Vertical
    meta_ref_element = [
        lfric.kernel.MetaRefElementArgMetadata("normals_to_vertical_faces")]
    cls = call_method("_ref_element_properties", meta_ref_element)
    check_arg_symbols(cls, OrderedDict([
        ("nfaces_re_v", lfric_qr_z_class),
        ("normals_to_vertical_faces", symbols.DataSymbol)]))
    symbol = cls._info.lookup("normals_to_vertical_faces")
    assert symbol.is_array
    assert len(symbol.datatype.shape) == 2
    assert symbol.datatype.shape[0].upper.value == "3"
    assert symbol.datatype.shape[1].upper.symbol.name == "nfaces_re_v"

    # General
    meta_ref_element = [
        lfric.kernel.MetaRefElementArgMetadata("normals_to_faces")]
    cls = call_method("_ref_element_properties", meta_ref_element)
    check_arg_symbols(cls, OrderedDict([
        ("nfaces_re", lfric_qr_faces_class),
        ("normals_to_faces", symbols.DataSymbol)]))
    symbol = cls._info.lookup("normals_to_faces")
    assert symbol.is_array
    assert len(symbol.datatype.shape) == 2
    assert symbol.datatype.shape[0].upper.value == "3"
    assert symbol.datatype.shape[1].upper.symbol.name == "nfaces_re"

    # All
    meta_ref_element = [
        lfric.kernel.MetaRefElementArgMetadata("normals_to_horizontal_faces"),
        lfric.kernel.MetaRefElementArgMetadata("normals_to_vertical_faces"),
        lfric.kernel.MetaRefElementArgMetadata("normals_to_faces")]
    cls = call_method("_ref_element_properties", meta_ref_element)
    check_arg_symbols(cls, OrderedDict([
        ("nfaces_re_h", lfric_qr_xy_class),
        ("nfaces_re_v", lfric_qr_z_class),
        ("nfaces_re", lfric_qr_faces_class),
        ("normals_to_horizontal_faces", symbols.DataSymbol),
        ("normals_to_vertical_faces", symbols.DataSymbol),
        ("normals_to_faces", symbols.DataSymbol)]))
    symbol = cls._info.lookup("normals_to_horizontal_faces")
    assert symbol.is_array
    assert len(symbol.datatype.shape) == 2
    assert symbol.datatype.shape[0].upper.value == "3"
    assert symbol.datatype.shape[1].upper.symbol.name == "nfaces_re_h"
    symbol = cls._info.lookup("normals_to_vertical_faces")
    assert symbol.is_array
    assert len(symbol.datatype.shape) == 2
    assert symbol.datatype.shape[0].upper.value == "3"
    assert symbol.datatype.shape[1].upper.symbol.name == "nfaces_re_v"
    symbol = cls._info.lookup("normals_to_faces")
    assert symbol.is_array
    assert len(symbol.datatype.shape) == 2
    assert symbol.datatype.shape[0].upper.value == "3"
    assert symbol.datatype.shape[1].upper.symbol.name == "nfaces_re"

    # Exception
    meta_ref_element = [
        lfric.kernel.MetaRefElementArgMetadata("normals_to_faces")]
    monkeypatch.setattr(meta_ref_element[0], "_reference_element", "invalid")
    with pytest.raises(InternalError) as info:
        _ = call_method("_ref_element_properties", meta_ref_element)
    assert ("Unsupported reference element property 'invalid' found."
            in str(info.value))


def test_mesh_properties(monkeypatch):
    ''' Test _mesh_properties method. '''
    # nfaces_re_h already passed
    field_meta_arg = lfric.kernel.FieldArgMetadata("GH_REAL", "GH_WRITE", "W3")
    meta_ref_element = lfric.kernel.MetaRefElementArgMetadata(
        "normals_to_horizontal_faces")
    meta_mesh_arg = lfric.kernel.MetaMeshArgMetadata("adjacent_face")
    metadata = lfric.kernel.LFRicKernelMetadata(
        operates_on="cell_column", meta_args=[field_meta_arg],
        meta_ref_element=[meta_ref_element], meta_mesh=[meta_mesh_arg])
    metadata.validate()
    cls = call_method("_mesh_properties", [meta_mesh_arg], metadata=metadata)
    check_arg_symbols(cls, OrderedDict([
        ("adjacent_face", symbols.DataSymbol)]))
    symbol = cls._info.lookup("adjacent_face")
    assert symbol.is_array
    assert len(symbol.datatype.shape) == 1
    assert symbol.datatype.shape[0].upper.symbol.name == "nfaces_re_h"

    # nfaces_re_h not yet passed
    metadata = lfric.kernel.LFRicKernelMetadata(
        operates_on="cell_column", meta_args=[field_meta_arg],
        meta_mesh=[meta_mesh_arg])
    metadata.validate()
    cls = call_method("_mesh_properties", [meta_mesh_arg], metadata=metadata)
    lfric_qr_xy_class = lfric.LFRicTypes("NumberOfQrPointsInXyDataSymbol")
    check_arg_symbols(cls, OrderedDict([
        ("nfaces_re_h", lfric_qr_xy_class),
        ("adjacent_face", symbols.DataSymbol)]))
    symbol = cls._info.lookup("adjacent_face")
    assert symbol.is_array
    assert len(symbol.datatype.shape) == 1
    assert symbol.datatype.shape[0].upper.symbol.name == "nfaces_re_h"

    # Exception
    monkeypatch.setattr(meta_mesh_arg, "_mesh", "invalid")
    with pytest.raises(InternalError) as info:
        _ = call_method("_mesh_properties", [meta_mesh_arg], metadata=metadata)
    assert ("Unexpected mesh property 'invalid' found. Expected "
            "'adjacent_face'." in str(info.value))
