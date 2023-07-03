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
    class method specified in argument 'method_name' with the
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
        method_name, datasymbol_name, symbol_name, *args, metadata=None, check_unchanged=False):
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
    *** check unchanged ***

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
    if check_unchanged:
        # Check that the symbol remains unchanged if it has already been declared.
        symbol_id = id(symbol)
        # Reset the argument list as this method will have added it
        # both to the symbol table and to the argument list whereas if
        # it had already been declared by another method it will have
        # only been added to the symbol table.
        cls._info._argument_list = []
        getattr(cls, method_name)(*args)
        symbol = cls._info.lookup_with_tag(symbol_name)
        assert symbol_id == id(symbol)
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
        "_mesh_ncell2d", "LFRicIntegerScalarDataSymbol", symbol_name,
        check_unchanged=True)


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


def test_fs_common():
    ''' Test _fs_common method. '''
    function_space = "w3"
    symbol_name = lfric.FormalKernelArgsFromMetadata._ndf_name(function_space)
    cls = check_single_symbol(
        "_fs_common", "NumberOfDofsDataSymbol", symbol_name, function_space,
        check_unchanged=True)


def test_fs_compulsory_field():
    ''' Test _fs_compulsory_field method. '''
    function_space = "w3"
    cls = call_method("_fs_compulsory_field", function_space)
    check_fs_compulsory_field(cls, function_space)


def check_fs_compulsory_field(cls, function_space):
    ''' xxx '''
    undf_name = lfric.FormalKernelArgsFromMetadata._undf_name(function_space)
    dofmap_name = lfric.FormalKernelArgsFromMetadata._dofmap_name(function_space)
    # Check that undf and dofmap symbols are added to the symbol table
    # and to the argument list.
    undf_class = lfric.LFRicTypes("NumberOfUniqueDofsDataSymbol")
    check_arg_symbols(cls, OrderedDict(
        [(undf_name, undf_class), (dofmap_name, symbols.DataSymbol)]))
    # Check that dofmap is an array with the expected extent.
    dofmap_symbol = cls._info.lookup(dofmap_name)
    assert dofmap_symbol.is_array
    assert len(dofmap_symbol.datatype.shape) == 1
    ndf_name = lfric.FormalKernelArgsFromMetadata._ndf_name(function_space)
    assert dofmap_symbol.datatype.shape[0].upper.symbol.name == ndf_name
    # Check that the method works if undf has already been added to
    # the symbol table.
    # Remove dofmap from the symbol table and remove both symbols from
    # the argument list. This should leave the symbol table containing
    # just undf.
    cls._info._argument_list = []
    norm_name = cls._info._normalize(dofmap_name)
    cls._info._symbols.pop(norm_name)
    # remove dofmap tag if there is one
    for tag, tagged_symbol in list(cls._info._tags.items()):
        if dofmap_symbol is tagged_symbol:
            del cls._info._tags[tag]

    # Call the method again and check that the undf symbol does not
    # change and that a dofmap symbol is added to the symbol table.
    undf_symbol = cls._info.lookup(undf_name)
    cls._fs_compulsory_field(function_space)
    check_arg_symbols(cls, OrderedDict(
        [(undf_name, undf_class), (dofmap_name, symbols.DataSymbol)]))
    assert cls._info.lookup(undf_name) is undf_symbol


def test_fs_intergrid():
    ''' Test _fs_intergrid method. '''
    # gh_fine
    function_space = "w3"
    intergrid_meta_arg = lfric.kernel.InterGridArgMetadata(
        "GH_REAL", "GH_WRITE", function_space, "GH_FINE")
    cls = call_method("_fs_intergrid", intergrid_meta_arg)
    ndf_name = lfric.FormalKernelArgsFromMetadata._ndf_name(function_space)
    undf_name = lfric.FormalKernelArgsFromMetadata._undf_name(function_space)
    fullmap_name = lfric.FormalKernelArgsFromMetadata._fullmap_name(function_space)
    ndf_class = lfric.LFRicTypes("NumberOfDofsDataSymbol")
    undf_class = lfric.LFRicTypes("NumberOfUniqueDofsDataSymbol")
    check_arg_symbols(cls, OrderedDict(
        [(ndf_name, ndf_class), (undf_name, undf_class),
         (fullmap_name, symbols.DataSymbol)]))
    # Check that fullmap is an array with the expected extent.
    fullmap_symbol = cls._info.lookup(fullmap_name)
    assert fullmap_symbol.is_array
    assert len(fullmap_symbol.datatype.shape) == 2
    ndf_name = lfric.FormalKernelArgsFromMetadata._ndf_name(function_space)
    assert fullmap_symbol.datatype.shape[0].upper.symbol.name == ndf_name
    assert fullmap_symbol.datatype.shape[1].upper.symbol.name == "ncell_f"

    # if undf and ndf are already declared
    undf_symbol = cls._info.lookup(undf_name)
    ndf_symbol = cls._info.lookup(ndf_name)
    from psyclone.psyir.symbols import SymbolTable
    symbol_table = SymbolTable()
    symbol_table.add(undf_symbol, tag=undf_name)
    symbol_table.add(ndf_symbol, tag=ndf_name)
    cls._info=symbol_table
    cls._fs_intergrid(intergrid_meta_arg)
    check_arg_symbols(cls, OrderedDict(
        [(ndf_name, ndf_class), (undf_name, undf_class),
         (fullmap_name, symbols.DataSymbol)]))
    assert cls._info.lookup(undf_name) is undf_symbol
    assert cls._info.lookup(ndf_name) is ndf_symbol

    # gh_coarse
    intergrid_meta_arg = lfric.kernel.InterGridArgMetadata(
        "GH_REAL", "GH_WRITE", function_space, "GH_COARSE")
    cls = call_method("_fs_intergrid", intergrid_meta_arg)
    check_fs_compulsory_field(cls, function_space)


def test_basis_or_diff_basis_dimension():
    ''' TODO '''
    pass


def test_basis_dimension():
    '''Test the _basis_dimension utility method. Test with one example of
    each option (returning 1, returning 3, or raising an
    exception. Also test that function_space values can be lower or upper
    case.

    '''
    cls = lfric.FormalKernelArgsFromMetadata
    assert cls._basis_dimension("W0") == 1
    assert cls._basis_dimension("w1") == 3
    with pytest.raises(ValueError) as info:
        cls._basis_dimension("invalid")
    assert ("Unexpected function space value 'invalid' found in "
            "basis_dimension. Expected one of ['w0', 'w2trace', 'w2htrace', "
            "'w2vtrace', 'w3', 'wtheta', 'wchi', 'w1', 'w2', 'w2h', 'w2v', "
            "'w2broken', 'any_w2']." in str(info.value))


def test_diff_basis_dimension():
    '''Test the _diff_basis_dimension utility method. Test with one
    example of each case (returning 1, returning 3, or raising an
    exception. Also test that function_space values can be upper or
    lower case.

    '''
    cls = lfric.FormalKernelArgsFromMetadata
    assert cls._diff_basis_dimension("W2") == 1
    assert cls._diff_basis_dimension("w0") == 3
    with pytest.raises(ValueError) as info:
        cls._diff_basis_dimension("invalid")
    assert ("Unexpected function space value 'invalid' found in "
            "diff_basis_dimension. Expected one of  ['w2', 'w2h', 'w2v', "
            "'w2broken', 'any_w2', 'w0', 'w1', 'w2trace', 'w2htrace', "
            "'w2vtrace', 'w3', 'wtheta', 'wchi']." in str(info.value))


def test_basis_or_diff_basis():
    ''' TODO '''
    # gh_quadrature_*
    # name = "<name>_"<function_space>_<quadrature_arg_name>.
    # gh_quadrature_xyoz
    # (dimension, number_of_dofs, np_xy, np_z).
    # gh_quadrature_face
    # (dimension, number_of_dofs, np_xyz, nfaces)
    # gh_quadrature_edge
    # (dimension, number_of_dofs, np_xyz, nedges)
    # gh_evaluator
    # name = <name>"_"<function_space>"_on_"<target_function_space>.
    # (dimension, number_of_dofs, ndf_<target_function_space>)


def test_basis():
    ''' Test _basis method. '''
    function_space = "w3"
    field_meta_arg = lfric.kernel.FieldArgMetadata("GH_REAL", "GH_WRITE", "W3")
    meta_funcs_arg = lfric.kernel.MetaFuncsArgMetadata(function_space, basis_function=True)
    metadata = lfric.kernel.LFRicKernelMetadata(
        operates_on="cell_column", meta_args=[field_meta_arg],
        meta_funcs=[meta_funcs_arg], shapes=["gh_quadrature_xyoz"])
    metadata.validate()
    cls = call_method("_basis", function_space, metadata=metadata)
    check_arg_symbols(cls, OrderedDict(
        [("basis_w3_qr_xyoz", symbols.DataSymbol)]))
    # Check that basis is an array with the expected extent.
    basis_symbol = cls._info.lookup("basis_w3_qr_xyoz")
    assert basis_symbol.is_array
    assert len(basis_symbol.datatype.shape) == 4
    ndf_name = lfric.FormalKernelArgsFromMetadata._ndf_name(function_space)
    assert basis_symbol.datatype.shape[0].upper.value == "1"
    assert basis_symbol.datatype.shape[1].upper.symbol.name == ndf_name
    assert basis_symbol.datatype.shape[2].upper.symbol.name == "np_xy"
    assert basis_symbol.datatype.shape[3].upper.symbol.name == "np_z"


def test_diff_basis():
    ''' Test _diff_basis method. '''
    function_space = "w3"
    field_meta_arg = lfric.kernel.FieldArgMetadata("GH_REAL", "GH_WRITE", "W3")
    meta_funcs_arg = lfric.kernel.MetaFuncsArgMetadata(function_space, diff_basis_function=True)
    metadata = lfric.kernel.LFRicKernelMetadata(
        operates_on="cell_column", meta_args=[field_meta_arg],
        meta_funcs=[meta_funcs_arg], shapes=["gh_quadrature_xyoz"])
    metadata.validate()
    cls = call_method("_diff_basis", function_space, metadata=metadata)
    check_arg_symbols(cls, OrderedDict(
        [("diff_basis_w3_qr_xyoz", symbols.DataSymbol)]))
    # Check that diff basis is an array with the expected extent.
    diff_basis_symbol = cls._info.lookup("diff_basis_w3_qr_xyoz")
    assert diff_basis_symbol.is_array
    assert len(diff_basis_symbol.datatype.shape) == 4
    ndf_name = lfric.FormalKernelArgsFromMetadata._ndf_name(function_space)
    assert diff_basis_symbol.datatype.shape[0].upper.value == "3"
    assert diff_basis_symbol.datatype.shape[1].upper.symbol.name == ndf_name
    assert diff_basis_symbol.datatype.shape[2].upper.symbol.name == "np_xy"
    assert diff_basis_symbol.datatype.shape[3].upper.symbol.name == "np_z"
