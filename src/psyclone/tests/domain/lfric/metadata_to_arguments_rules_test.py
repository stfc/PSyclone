# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
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

'''This module contains tests for the metadata_to_arguments_rules
module which contains the MetadataToArgumentsRules class.

'''
import pytest

from psyclone.domain.lfric import MetadataToArgumentsRules
from psyclone.domain.lfric.kernel import (
    ColumnwiseOperatorArgMetadata, FieldArgMetadata, FieldVectorArgMetadata,
    InterGridArgMetadata, LFRicKernelMetadata, MetaMeshArgMetadata,
    MetaFuncsArgMetadata, OperatorArgMetadata, MetaRefElementArgMetadata,
    ScalarArgMetadata)
from psyclone.errors import InternalError


def check_called(monkeypatch, function, method_name, metadata):
    '''Utility method to check that the method (supplied in the
    method_name argument) is called by monkeypatching it with the
    supplied function argument and testing for a resultant exception.

    :param monkeypatch: a monkeypatch object.
    :type monkeypatch: :py:class:`_pytest.monkeypatch.MonkeyPatch`
    :param function: the function that will be used to monkeypatch.
    :type function: :py:class:`function`
    :param str method_name: the name of the method to be monkeypatched.
    :param metadata: the metadata to pass to the _generate method.
    :type metadata: \
        :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

    '''
    monkeypatch.setattr(MetadataToArgumentsRules, method_name, function)
    monkeypatch.setattr(MetadataToArgumentsRules, "_metadata", metadata)
    with pytest.raises(Exception) as info:
        _ = MetadataToArgumentsRules._generate()
    assert method_name in str(info.value)


def test_bc_kern_regex():
    '''
    Test the regular expression used to identify the boundary-condition kernel
    and its transformed forms.

    TODO #487 - this test should be removed once metadata is used to identify
    the boundary-condition kernel.

    '''
    cls = MetadataToArgumentsRules
    assert cls.bc_kern_regex.match("enforce_bc_code")
    assert cls.bc_kern_regex.match("enforce_BC_code")
    assert cls.bc_kern_regex.match("enforce_BC_1099_code")
    assert not cls.bc_kern_regex.match("other_bc_code")
    assert not cls.bc_kern_regex.match("enforce_bc_a1_code")
    assert not cls.bc_kern_regex.match("enforce_bc_1a_code")


def test_mapping(monkeypatch):
    '''Test the MetadataToArgumentRules class mapping method works as
    expected.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()
    cls = MetadataToArgumentsRules
    # Check that _initialise is called and _metadata is updated and
    # _info is returned.
    result = cls.mapping(metadata, info="hello")
    assert result == "hello"
    assert cls._metadata is metadata
    # Check that _generate is called.
    monkeypatch.setattr(cls, "_generate", lambda: 1/0)
    with pytest.raises(ZeroDivisionError):
        _ = cls.mapping(metadata)


def test_initialise():
    '''Test the MetadataToArgumentsRules class _initialise method.'''
    cls = MetadataToArgumentsRules
    assert cls._info is None
    cls._initialise("hello")
    assert cls._info == "hello"


def test_generate_cell_position(monkeypatch):
    '''Test the MetadataToArgumentsRules class _generate method calls the
    _cell_position method if an operator is supplied.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0"),
        FieldArgMetadata("GH_REAL", "GH_READ", "W1"),
        OperatorArgMetadata("GH_REAL", "GH_READ", "W1", "W0")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()

    def _cell_position():
        raise Exception("_cell_position")
    check_called(monkeypatch, _cell_position, "_cell_position", metadata)


def test_generate_mesh_height(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _mesh_height method.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0"),
        FieldArgMetadata("GH_REAL", "GH_READ", "W0")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()

    def _mesh_height():
        raise Exception("_mesh_height")
    check_called(monkeypatch, _mesh_height, "_mesh_height", metadata)


def test_generate_mesh_ncell2d_no_halos(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _mesh_ncell2d_no_halos method if the operates_on metadata
    specifies 'domain'.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_WRITE", "W3"),
        FieldArgMetadata("GH_REAL", "GH_READ", "W3")]
    metadata = LFRicKernelMetadata(operates_on="domain", meta_args=meta_args)
    metadata.validate()

    def _mesh_ncell2d_no_halos():
        raise Exception("_mesh_ncell2d_no_halos")
    check_called(monkeypatch, _mesh_ncell2d_no_halos,
                 "_mesh_ncell2d_no_halos", metadata)


def test_generate_mesh_ncell2d(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _mesh_ncell2d method if the metadata contains a columnwise
    operator argument. Uses a cma-matrix-matrix kernel.

    '''
    meta_args = [
        ColumnwiseOperatorArgMetadata("GH_REAL", "GH_READWRITE", "W0", "W1")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()

    def _mesh_ncell2d():
        raise Exception("_mesh_ncell2d")
    check_called(monkeypatch, _mesh_ncell2d, "_mesh_ncell2d", metadata)


def test_generate_cell_map(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _cell_map method if the metadata is an inter-grid kernel.

    '''
    meta_args = [
        InterGridArgMetadata("GH_REAL", "GH_READ", "W0", "GH_FINE"),
        InterGridArgMetadata("GH_REAL", "GH_INC", "W1", "GH_COARSE")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()

    def _cell_map():
        raise Exception("_cell_map")
    check_called(monkeypatch, _cell_map, "_cell_map", metadata)


def test_generate_scalar(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _scalar method if one of the metadata meta_args is a scalar.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0"),
        ScalarArgMetadata("GH_REAL", "GH_READ")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()

    def _scalar(_):
        raise Exception("_scalar")
    check_called(monkeypatch, _scalar, "_scalar", metadata)


def test_generate_field(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _field method if one of the metadata meta_args is a field.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()

    def _field(_):
        raise Exception("_field")
    check_called(monkeypatch, _field, "_field", metadata)


def test_generate_field_vector(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _field_vector method if one of the metadata meta_args is a field
    vector.

    '''
    meta_args = [
        FieldVectorArgMetadata("GH_REAL", "GH_INC", "W0", "3")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()

    def _field_vector(_):
        raise Exception("_field_vector")
    check_called(monkeypatch, _field_vector, "_field_vector", metadata)


def test_generate_operator(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _operator method if one of the metadata meta_args is an operator.

    '''
    meta_args = [OperatorArgMetadata("GH_REAL", "GH_READWRITE", "W0", "W1")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()

    def _operator(_):
        raise Exception("_operator")
    check_called(monkeypatch, _operator, "_operator", metadata)


def test_generate_cma_operator(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _cma_operator method if one of the metadata meta_args is a
    cma operator.

    '''
    meta_args = [
        ColumnwiseOperatorArgMetadata("GH_REAL", "GH_READWRITE", "W0", "W1")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()

    def _cma_operator(_):
        raise Exception("_cma_operator")
    check_called(monkeypatch, _cma_operator, "_cma_operator", metadata)


def test_generate_ref_element_properties(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _ref_element_properties method if reference element properties are
    specified in the metadata.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0")]
    meta_ref_element = [
        MetaRefElementArgMetadata("normals_to_horizontal_faces")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args,
        meta_ref_element=meta_ref_element)
    metadata.validate()

    def _ref_element_properties(_):
        raise Exception("_ref_element_properties")
    check_called(monkeypatch, _ref_element_properties,
                 "_ref_element_properties", metadata)


def test_generate_mesh_properties(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _mesh_properties method if mesh properties are specified in the
    metadata.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0")]
    meta_mesh = [MetaMeshArgMetadata("adjacent_face")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args, meta_mesh=meta_mesh)
    metadata.validate()

    def _mesh_properties(_):
        raise Exception("_mesh_properties")
    check_called(monkeypatch, _mesh_properties, "_mesh_properties", metadata)


def test_generate_fs_common(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _fs_common method.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()

    def _fs_common(_):
        raise Exception("_fs_common")
    check_called(monkeypatch, _fs_common, "_fs_common", metadata)


def test_generate_fs_compulsory_field(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _fs_compulsory_field method.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()

    def _fs_compulsory_field(_):
        raise Exception("_fs_compulsory_field")
    check_called(monkeypatch, _fs_compulsory_field,
                 "_fs_compulsory_field", metadata)


def test_generate_fs_intergrid(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _fs_integrid method if the metadata specifies an intergrid kernel.

    '''
    meta_args = [
        InterGridArgMetadata("GH_REAL", "GH_READ", "W0", "GH_FINE"),
        InterGridArgMetadata("GH_REAL", "GH_INC", "W1", "GH_COARSE")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()

    def _fs_intergrid(_):
        raise Exception("_fs_intergrid")
    check_called(monkeypatch, _fs_intergrid, "_fs_intergrid", metadata)


def test_generate_basis(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _basis method if a basis function is specified in the metadata.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0")]
    meta_funcs = [
        MetaFuncsArgMetadata("W0", basis_function=True)]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args,
        meta_funcs=meta_funcs)
    metadata.validate()

    def _basis(_):
        raise Exception("_basis")
    check_called(monkeypatch, _basis, "_basis", metadata)


def test_generate_diff_basis(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _diff_basis method if a differential basis function is specified
    in the metadata.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0")]
    meta_funcs = [
        MetaFuncsArgMetadata("W0", diff_basis_function=True)]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args,
        meta_funcs=meta_funcs)
    metadata.validate()

    def _diff_basis(_):
        raise Exception("_diff_basis")
    check_called(monkeypatch, _diff_basis, "_diff_basis", metadata)


def test_generate_quad_rule(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _quad_rule method if both a basis function or a differential basis
    function and quadrature shape information are specified in the
    metadata.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0")]
    meta_funcs = [
        MetaFuncsArgMetadata("W0", basis_function=True)]
    shapes = ["gh_quadrature_xyoz"]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args,
        meta_funcs=meta_funcs, shapes=shapes)
    metadata.validate()

    def _quad_rule(_):
        raise Exception("_quad_rule")
    check_called(monkeypatch, _quad_rule, "_quad_rule", metadata)


def test_generate_field_bcs_kernel(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _field_bcs_kernel method if the supplied kernel name is
    enforce_bc_code.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "ANY_SPACE_1")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args,
        procedure_name="enforce_bc_code")
    metadata.validate()

    def _field_bcs_kernel():
        raise Exception("_field_bcs_kernel")
    check_called(monkeypatch, _field_bcs_kernel, "_field_bcs_kernel", metadata)


def test_generate_operator_bcs_kernel(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _operator_bcs_kernel method if the supplied kernel name is
    enforce_operator_bc_code.

    '''
    meta_args = [
        OperatorArgMetadata("GH_REAL", "GH_READWRITE", "W0", "W1")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args,
        procedure_name="enforce_operator_bc_code")
    metadata.validate()

    def _operator_bcs_kernel():
        raise Exception("_operator_bcs_kernel")
    check_called(monkeypatch, _operator_bcs_kernel,
                 "_operator_bcs_kernel", metadata)


def test_generate_stencil_cross2d_extent(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _stencil_cross2d_extent method if the supplied field metadata
    contains a cross2d stencil.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0", stencil="cross2d")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()

    def _stencil_cross2d_extent(_):
        raise Exception("_stencil_cross2d_extent")
    check_called(monkeypatch, _stencil_cross2d_extent,
                 "_stencil_cross2d_extent", metadata)


def test_generate_stencil_cross2d_max_extent(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _stencil_stencil2d_max_extent method if the supplied field
    metadata contains a cross2d stencil.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0", stencil="cross2d")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()

    def _stencil_cross2d_max_extent(_):
        raise Exception("_stencil_cross2d_max_extent")
    check_called(monkeypatch, _stencil_cross2d_max_extent,
                 "_stencil_cross2d_max_extent", metadata)


def test_generate_stencil_extent(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _stencil_extent method if the supplied field metadata
    contains an xory1d stencil.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0", stencil="xory1d")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()

    def _stencil_extent(_):
        raise Exception("_stencil_extent")
    check_called(monkeypatch, _stencil_extent, "_stencil_extent", metadata)


def test_generate_stencil_xory1d_direction(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _stencil_xory1d_direction method if the supplied field metadata
    contains an xory1d stencil.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0", stencil="xory1d")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()

    def _stencil_xory1d_direction(_):
        raise Exception("_stencil_xory1d_direction")
    check_called(monkeypatch, _stencil_xory1d_direction,
                 "_stencil_xory1d_direction", metadata)


def test_generate_stencil_cross2d(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _stencil_cross2d method if the supplied field metadata contains a
    cross2d stencil.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0", stencil="cross2d")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()

    def _stencil_cross2d(_):
        raise Exception("_stencil_cross2d")
    check_called(monkeypatch, _stencil_cross2d, "_stencil_cross2d", metadata)


def test_generate_stencil(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _stencil method if the supplied field metadata contains an xory1d
    stencil.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0", stencil="xory1d")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()

    def _stencil(_):
        raise Exception("_stencil")
    check_called(monkeypatch, _stencil, "_stencil", metadata)


def test_generate_banded_dofmap(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _banded_dofmap method if the kernel type is cma-assembly.

    '''
    meta_args = [
        ColumnwiseOperatorArgMetadata("GH_REAL", "GH_READWRITE", "W0", "W1"),
        OperatorArgMetadata("GH_REAL", "GH_READ", "W0", "W1")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()
    assert metadata._get_kernel_type() == "cma-assembly"

    def _banded_dofmap(_):
        raise Exception("_banded_dofmap")
    check_called(monkeypatch, _banded_dofmap, "_banded_dofmap", metadata)


def test_generate_indirection_dofmap(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method calls the
    _indirection_dofmap method if the kernel type is cma-apply.

    '''
    meta_args = [
        ColumnwiseOperatorArgMetadata("GH_REAL", "GH_READ", "W0", "W1"),
        FieldArgMetadata("GH_REAL", "GH_READ", "W1"),
        FieldArgMetadata("GH_REAL", "GH_INC", "W0")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()
    assert metadata._get_kernel_type() == "cma-apply"

    def _indirection_dofmap(_):
        raise Exception("_indirection_dofmap")
    check_called(monkeypatch, _indirection_dofmap,
                 "_indirection_dofmap", metadata)


def test_generate_exception(monkeypatch):
    '''Test the MetadataToArgumentRules class _generate method raises the
    expected exception if an an unexpected meta_arg type is found.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()
    monkeypatch.setattr(metadata, "_validate_general_purpose_kernel",
                        lambda: None)
    monkeypatch.setattr(metadata._meta_args, "_meta_args_args", [None])
    monkeypatch.setattr(MetadataToArgumentsRules, "_metadata", metadata)
    with pytest.raises(InternalError) as info:
        MetadataToArgumentsRules._generate()
    assert ("PSyclone internal error: Unexpected meta_arg type 'NoneType' "
            "found." in str(info.value))
