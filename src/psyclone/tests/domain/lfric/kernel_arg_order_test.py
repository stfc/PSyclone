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

'''This module contains tests for the kernel_arg_order module which
contains the kernel_arg_order class.

'''
import pytest

from psyclone.domain.lfric import KernelArgOrder
from psyclone.domain.lfric.kernel import (
    ColumnwiseOperatorArgMetadata, FieldArgMetadata, FieldVectorArgMetadata,
    InterGridArgMetadata, LFRicKernelMetadata, MetaMeshArgMetadata,
    MetaFuncsArgMetadata, OperatorArgMetadata, MetaRefElementArgMetadata,
    ScalarArgMetadata)
from psyclone.errors import InternalError


def test_general_kernel():
    '''Test the kernel_arg_order class works as expected with simple
    general kernel metadata. This tests the mesh_height, field,
    fs_common and fs_compulsory_field methods, as well as the
    class initialisation and running of the _generate method.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0"),
        FieldArgMetadata("GH_REAL", "GH_READ", "W0")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()
    kernel_arg_order = KernelArgOrder(metadata)
    assert len(kernel_arg_order.meta_arg_index_from_actual_index) == 2
    assert kernel_arg_order.meta_arg_index_from_actual_index[1] == 0
    assert kernel_arg_order.meta_arg_index_from_actual_index[2] == 1
    assert kernel_arg_order.arg_info == [
        'nlayers', 'rfield_1', 'rfield_2', 'ndf_w0', 'undf_w0', 'map_w0']


def test_cell_position():
    '''Test the kernel_arg_order class calls the cell_position method if
    an operator is supplied by checking the general kernel
    metadata. This also tests the operator method.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0"),
        FieldArgMetadata("GH_REAL", "GH_READ", "W1"),
        OperatorArgMetadata("GH_REAL", "GH_READ", "W1", "W0")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()
    kernel_arg_order = KernelArgOrder(metadata)
    assert len(kernel_arg_order.meta_arg_index_from_actual_index) == 3
    assert kernel_arg_order.meta_arg_index_from_actual_index[2] == 0
    assert kernel_arg_order.meta_arg_index_from_actual_index[3] == 1
    assert kernel_arg_order.meta_arg_index_from_actual_index[5] == 2
    assert kernel_arg_order.arg_info == [
        'cell', 'nlayers', 'rfield_1', 'rfield_2', 'op_3_ncell_3d', 'op_3',
        'ndf_w0', 'undf_w0', 'map_w0', 'ndf_w1', 'undf_w1', 'map_w1']


def test_mesh_ncell2d_no_halos():
    '''Test the kernel_arg_order class calls the mesh_ncell2d_no_halos
    method, if the operates_on metadata specifies 'domain', by
    checking domain kernel metadata.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_WRITE", "W3"),
        FieldArgMetadata("GH_REAL", "GH_READ", "W3")]
    metadata = LFRicKernelMetadata(operates_on="domain", meta_args=meta_args)
    metadata.validate()
    kernel_arg_order = KernelArgOrder(metadata)
    assert len(kernel_arg_order.meta_arg_index_from_actual_index) == 2
    assert kernel_arg_order.meta_arg_index_from_actual_index[2] == 0
    assert kernel_arg_order.meta_arg_index_from_actual_index[3] == 1
    assert kernel_arg_order.arg_info == [
        'nlayers', 'ncell_2d_no_halos', 'rfield_1', 'rfield_2', 'ndf_w3',
        'undf_w3', 'map_w3']


def test_mesh_ncell2d():
    '''Test the kernel_arg_order class calls the mesh_ncell2d method if
    the metadata contains a columnwise operator argument. Uses a
    cma-matrix-matrix kernel. Also tests (some of) the cma_operator
    method.

    '''
    meta_args = [
        ColumnwiseOperatorArgMetadata("GH_REAL", "GH_READWRITE", "W0", "W1")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()
    kernel_arg_order = KernelArgOrder(metadata)
    assert len(kernel_arg_order.meta_arg_index_from_actual_index) == 1
    assert kernel_arg_order.meta_arg_index_from_actual_index[2] == 0
    assert kernel_arg_order.arg_info == [
        'cell', 'ncell_2d', 'cma_op_1', 'nrow_cma_op_1', 'ncol_cma_op_1',
        'alpha_cma_op_1', 'beta_cma_op_1', 'gamma_m_cma_op_1',
        'gamma_p_cma_op_1']


def test_cell_map():
    '''Test the kernel_arg_order class calls the cell_map method if the
    metadata is an inter-grid kernel. Also tests the fs_intergrid method.

    '''
    meta_args = [
        InterGridArgMetadata("GH_REAL", "GH_READ", "W0", "GH_FINE"),
        InterGridArgMetadata("GH_REAL", "GH_INC", "W1", "GH_COARSE")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()
    kernel_arg_order = KernelArgOrder(metadata)
    assert len(kernel_arg_order.meta_arg_index_from_actual_index) == 2
    assert kernel_arg_order.meta_arg_index_from_actual_index[5] == 0
    assert kernel_arg_order.meta_arg_index_from_actual_index[6] == 1
    assert kernel_arg_order.arg_info == [
        'nlayers', 'cell_map', 'ncell_f_per_c_x', 'ncell_f_per_c_y',
        'ncell_f', 'rfield_1', 'rfield_2', 'ndf_w0', 'undf_w0', 'full_map_w0',
        'undf_w1', 'map_w1']


def test_scalar():
    '''Test the kernel_arg_order class calls the scalar method if one of
    the metadata meta_args is a scalar. Test with the different
    supported datatypes as they have different default names.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0"),
        ScalarArgMetadata("GH_REAL", "GH_READ"),
        ScalarArgMetadata("GH_INTEGER", "GH_READ"),
        ScalarArgMetadata("GH_LOGICAL", "GH_READ")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()
    kernel_arg_order = KernelArgOrder(metadata)
    assert len(kernel_arg_order.meta_arg_index_from_actual_index) == 4
    assert kernel_arg_order.meta_arg_index_from_actual_index[1] == 0
    assert kernel_arg_order.meta_arg_index_from_actual_index[2] == 1
    assert kernel_arg_order.meta_arg_index_from_actual_index[3] == 2
    assert kernel_arg_order.meta_arg_index_from_actual_index[4] == 3
    assert kernel_arg_order.arg_info == [
        'nlayers', 'rfield_1', 'rscalar_2', 'iscalar_3', 'lscalar_4',
        'ndf_w0', 'undf_w0', 'map_w0']


def test_field():
    '''Test the kernel_arg_order class field with the different supported
    datatypes as they have different default names. Previous tests
    have already tested that this method is called (for field with
    real data).

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0"),
        FieldArgMetadata("GH_INTEGER", "GH_READ", "W0")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()
    kernel_arg_order = KernelArgOrder(metadata)
    assert len(kernel_arg_order.meta_arg_index_from_actual_index) == 2
    assert kernel_arg_order.meta_arg_index_from_actual_index[1] == 0
    assert kernel_arg_order.meta_arg_index_from_actual_index[2] == 1
    assert kernel_arg_order.arg_info == [
        'nlayers', 'rfield_1', 'ifield_2', 'ndf_w0', 'undf_w0', 'map_w0']


def test_field_vector():
    '''Test the kernel_arg_order class calls the field_vector method if one of
    the metadata meta_args is a field_vector. Test with the different
    supported datatypes as they have different default names.

    '''
    meta_args = [
        FieldVectorArgMetadata("GH_REAL", "GH_INC", "W0", "3"),
        FieldVectorArgMetadata("GH_INTEGER", "GH_READ", "W0", "2")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()
    kernel_arg_order = KernelArgOrder(metadata)
    assert len(kernel_arg_order.meta_arg_index_from_actual_index) == 5
    assert kernel_arg_order.meta_arg_index_from_actual_index[1] == 0
    assert kernel_arg_order.meta_arg_index_from_actual_index[2] == 0
    assert kernel_arg_order.meta_arg_index_from_actual_index[3] == 0
    assert kernel_arg_order.meta_arg_index_from_actual_index[4] == 1
    assert kernel_arg_order.meta_arg_index_from_actual_index[5] == 1
    assert kernel_arg_order.arg_info == [
        'nlayers', 'rfield_1_v1', 'rfield_1_v2', 'rfield_1_v3', 'ifield_2_v1',
        'ifield_2_v2', 'ndf_w0', 'undf_w0', 'map_w0']


# The kernel_arg_order class operator method has already been tested
# by test_cell_position().


def test_cma_operator():
    '''Test the kernel_arg_order class mesh_ncell2d method. Most of this
    method has already been tested in test_mesh_ncell2d(). This test
    checks the case where the two function spaces for the cma operator
    are the same. A cma-matrix-matrix kernel is used here.

    '''
    meta_args = [
        ColumnwiseOperatorArgMetadata("GH_REAL", "GH_READWRITE", "W0", "W0")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()
    kernel_arg_order = KernelArgOrder(metadata)
    assert len(kernel_arg_order.meta_arg_index_from_actual_index) == 1
    assert kernel_arg_order.meta_arg_index_from_actual_index[2] == 0
    assert kernel_arg_order.arg_info == [
        'cell', 'ncell_2d', 'cma_op_1', 'nrow_cma_op_1', 'alpha_cma_op_1',
        'beta_cma_op_1', 'gamma_m_cma_op_1', 'gamma_p_cma_op_1']


def test_ref_element_properties():
    '''Test the kernel_arg_order class calls the ref_element_properties
    method if reference element properties are specified in the
    metadata. Test for all supported property names.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0")]
    meta_ref_element = [
        MetaRefElementArgMetadata("normals_to_horizontal_faces"),
        MetaRefElementArgMetadata("normals_to_vertical_faces"),
        MetaRefElementArgMetadata("normals_to_faces"),
        MetaRefElementArgMetadata("outward_normals_to_vertical_faces"),
        MetaRefElementArgMetadata("outward_normals_to_horizontal_faces"),
        MetaRefElementArgMetadata("outward_normals_to_faces")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args,
        meta_ref_element=meta_ref_element)
    metadata.validate()
    kernel_arg_order = KernelArgOrder(metadata)
    assert len(kernel_arg_order.meta_arg_index_from_actual_index) == 1
    assert kernel_arg_order.meta_arg_index_from_actual_index[1] == 0
    assert kernel_arg_order.arg_info == [
        'nlayers', 'rfield_1', 'ndf_w0', 'undf_w0', 'map_w0', 'nfaces_re_h',
        'nfaces_re_v', 'nfaces_re', 'normals_to_horizontal_faces',
        'normals_to_vertical_faces', 'normals_to_faces',
        'outward_normals_to_vertical_faces',
        'outward_normals_to_horizontal_faces', 'outward_normals_to_faces']


def test_mesh_properties_no_ref_el():
    '''Test the kernel_arg_order class calls the mesh_properties method if
    mesh properties are specified in the metadata. Test for all
    supported property names and with no reference element specified.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0")]
    meta_mesh = [MetaMeshArgMetadata("adjacent_face")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args, meta_mesh=meta_mesh)
    metadata.validate()
    kernel_arg_order = KernelArgOrder(metadata)
    assert len(kernel_arg_order.meta_arg_index_from_actual_index) == 1
    assert kernel_arg_order.meta_arg_index_from_actual_index[1] == 0
    assert kernel_arg_order.arg_info == [
        'nlayers', 'rfield_1', 'ndf_w0', 'undf_w0', 'map_w0', 'nfaces_re_h',
        'adjacent_face']


def test_mesh_properties_ref_el_horiz():
    '''Test the kernel_arg_order class calls the mesh_properties method if
    mesh properties are specified in the metadata. Test for all
    supported property names and with a horizontal reference element
    specified ('nfaces_re_h' should only be output once).

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0")]
    meta_mesh = [MetaMeshArgMetadata("adjacent_face")]
    meta_ref_element = [
        MetaRefElementArgMetadata("normals_to_horizontal_faces")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args,
        meta_mesh=meta_mesh, meta_ref_element=meta_ref_element)
    metadata.validate()
    kernel_arg_order = KernelArgOrder(metadata)
    assert len(kernel_arg_order.meta_arg_index_from_actual_index) == 1
    assert kernel_arg_order.meta_arg_index_from_actual_index[1] == 0
    assert kernel_arg_order.arg_info == [
        'nlayers', 'rfield_1', 'ndf_w0', 'undf_w0', 'map_w0', 'nfaces_re_h',
        'normals_to_horizontal_faces', 'adjacent_face']


def test_mesh_properties_ref_el_not_horiz():
    '''Test the kernel_arg_order class calls the mesh_properties method if
    mesh properties are specified in the metadata. Test for all
    supported property names and with a non-horiztonal reference element
    specified ('nfaces_re_h' should be output due to mesh property).

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0")]
    meta_mesh = [MetaMeshArgMetadata("adjacent_face")]
    meta_ref_element = [
        MetaRefElementArgMetadata("normals_to_faces")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args,
        meta_mesh=meta_mesh, meta_ref_element=meta_ref_element)
    metadata.validate()
    kernel_arg_order = KernelArgOrder(metadata)
    assert len(kernel_arg_order.meta_arg_index_from_actual_index) == 1
    assert kernel_arg_order.meta_arg_index_from_actual_index[1] == 0
    assert kernel_arg_order.arg_info == [
        'nlayers', 'rfield_1', 'ndf_w0', 'undf_w0', 'map_w0', 'nfaces_re',
        'normals_to_faces', 'nfaces_re_h', 'adjacent_face']


def test_mesh_properties_ref_el_invalid(monkeypatch):
    '''Test the kernel_arg_order class calls the mesh_properties method if
    mesh properties are specified in the metadata. Test that the
    expected exception is raised if an unexpected property name is
    found.

    '''
    # First create a valid instance of KernelArgOrder
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0")]
    meta_mesh = [MetaMeshArgMetadata("adjacent_face")]
    meta_ref_element = [
        MetaRefElementArgMetadata("normals_to_faces")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args,
        meta_mesh=meta_mesh, meta_ref_element=meta_ref_element)
    metadata.validate()
    kernel_arg_order = KernelArgOrder(metadata)

    # Now cause exception.
    mesh_property = [MetaMeshArgMetadata("adjacent_face")]
    monkeypatch.setattr(mesh_property[0], "_mesh", "invalid")
    with pytest.raises(InternalError) as info:
        kernel_arg_order.mesh_properties(mesh_property)
    assert ("Unexpected mesh property 'invalid' found. Expected "
            "'adjacent_face'." in str(info.value))


# The kernel_arg_order class fs_common and fs_compulsory_field methods
# have already been tested by test_general_kernel().
# The kernel_arg_order class fs_intergrid method has already been
# tested by test_cell_map().


@pytest.mark.parametrize("name", ["basis", "diff_basis"])
def test_basis_or_diff_basis_no_shape(monkeypatch, name):
    '''Test the kernel_arg_order class _basis_or_diff_basis utility method
    if a basis or differential basis function is specified in the
    metadata but no shape information is specified in the
    metadata.

    '''
    # First create a valid instance of KernelArgOrder
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0")]
    meta_funcs = [
        MetaFuncsArgMetadata("W0", basis_function=True,
                             diff_basis_function=True)]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args,
        meta_funcs=meta_funcs)
    metadata.validate()
    kernel_arg_order = KernelArgOrder(metadata)

    # Now test the method
    monkeypatch.setattr(kernel_arg_order, "_arg_index", 0)
    monkeypatch.setattr(kernel_arg_order, "arg_info", [])
    kernel_arg_order._basis_or_diff_basis(name, "w0")
    assert kernel_arg_order._arg_index == 0
    assert not kernel_arg_order.arg_info


@pytest.mark.parametrize("name", ["basis", "diff_basis"])
def test_basis_or_diff_basis_quad(monkeypatch, name):
    '''Test the kernel_arg_order class _basis_or_diff_basis utility method
    if a basis or differential basis function is specified in the
    metadata and shape information is also specified in the
    metadata. Tests the case where the shape information is a type of
    quadrature.

    '''
    # First create a valid instance of KernelArgOrder
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0")]
    meta_funcs = [
        MetaFuncsArgMetadata("W0", basis_function=True,
                             diff_basis_function=True)]
    shapes = ["gh_quadrature_xyoz", "gh_quadrature_face", "gh_quadrature_edge"]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args,
        meta_funcs=meta_funcs, shapes=shapes)
    metadata.validate()
    kernel_arg_order = KernelArgOrder(metadata)

    # Now test the method
    monkeypatch.setattr(kernel_arg_order, "_arg_index", 0)
    monkeypatch.setattr(kernel_arg_order, "arg_info", [])
    kernel_arg_order._basis_or_diff_basis(name, "w0")
    assert kernel_arg_order._arg_index == 3
    assert kernel_arg_order.arg_info == [
        f'{name}_w0_qr_xyoz', f'{name}_w0_qr_face', f'{name}_w0_qr_edge']


@pytest.mark.parametrize("name", ["basis", "diff_basis"])
def test_basis_or_diff_basis_evaluator_targets(monkeypatch, name):
    '''Test the kernel_arg_order class _basis_or_diff_basis utility method
    if a basis or differential basis function is specified in the
    metadata and shape information is also specified in the
    metadata. Tests the case where the shape information is an
    evaluator and gh_evaluator_targets metadata is provided.

    '''
    # First create a valid instance of KernelArgOrder
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0"),
        FieldArgMetadata("GH_REAL", "GH_READ", "W1")]
    meta_funcs = [
        MetaFuncsArgMetadata("W0", basis_function=True)]
    shapes = ["gh_evaluator"]
    evaluator_targets = ["w1"]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args,
        meta_funcs=meta_funcs, shapes=shapes,
        evaluator_targets=evaluator_targets)
    metadata.validate()
    kernel_arg_order = KernelArgOrder(metadata)

    # Now test the method
    monkeypatch.setattr(kernel_arg_order, "_arg_index", 0)
    monkeypatch.setattr(kernel_arg_order, "arg_info", [])
    kernel_arg_order._basis_or_diff_basis(name, "w0")
    assert kernel_arg_order._arg_index == 1
    assert kernel_arg_order.arg_info == [f'{name}_w0_to_w1']


@pytest.mark.parametrize("name", ["basis", "diff_basis"])
def test_basis_or_diff_basis_evaluator_no_targets(monkeypatch, name):
    '''Test the kernel_arg_order class _basis_or_diff_basis utility method
    if a basis or differential basis function is specified in the
    metadata and shape information is also specified in the
    metadata. Tests the case where the shape information is an
    evaluator and gh_evaluator_targets metadata is not provided (so
    the targets are inferred from the modified fields).

    '''
    # First create a valid instance of KernelArgOrder
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0"),
        FieldArgMetadata("GH_REAL", "GH_INC", "W0"),
        FieldArgMetadata("GH_REAL", "GH_READ", "W1"),
        FieldArgMetadata("GH_REAL", "GH_READWRITE", "W3")]
    meta_funcs = [
        MetaFuncsArgMetadata("W1", basis_function=True)]
    shapes = ["gh_evaluator"]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args,
        meta_funcs=meta_funcs, shapes=shapes)
    metadata.validate()
    kernel_arg_order = KernelArgOrder(metadata)

    # Now test the method
    monkeypatch.setattr(kernel_arg_order, "_arg_index", 0)
    monkeypatch.setattr(kernel_arg_order, "arg_info", [])
    kernel_arg_order._basis_or_diff_basis(name, "w1")
    assert kernel_arg_order._arg_index == 2
    assert kernel_arg_order.arg_info == [
        f'{name}_w1_to_w0', f'{name}_w1_to_w3']


@pytest.mark.parametrize("name", ["basis", "diff_basis"])
def test_basis_or_diff_basis_evaluator_error(monkeypatch, name):
    '''Test the kernel_arg_order class _basis_or_diff_basis utility method
    if a basis or differential basis function is specified in the
    metadata and shape information is also specified in the
    metadata. Tests the case where the shape information is not
    recognised so causes an exception.

    '''
    # First create a valid instance of KernelArgOrder
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0"),
        FieldArgMetadata("GH_REAL", "GH_READ", "W1")]
    meta_funcs = [
        MetaFuncsArgMetadata(
            "W1", basis_function=True, diff_basis_function=True)]
    shapes = ["gh_evaluator"]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args,
        meta_funcs=meta_funcs, shapes=shapes)
    metadata.validate()
    kernel_arg_order = KernelArgOrder(metadata)

    # Now cause exception.
    monkeypatch.setattr(metadata._shapes, "_shapes", ["invalid"])
    with pytest.raises(InternalError) as info:
        if name == "basis":
            kernel_arg_order.basis("w1")
        else:
            kernel_arg_order.diff_basis("w1")
    assert ("Unexpected shape metadata. Found 'invalid' but expected one of "
            "['gh_quadrature_xyoz', 'gh_quadrature_face', "
            "'gh_quadrature_edge', 'gh_evaluator']." in str(info.value))


def test_basis_and_diff_basis():
    '''Test the kernel_arg_order class basis and diff_basis methods are
    called if a basis function and a differential basis function are
    specified in the metadata and shape information is also specified
    in the metadata. Also tests the quad_rule method.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0")]
    meta_funcs = [
        MetaFuncsArgMetadata(
            "W0", basis_function=True, diff_basis_function=True)]
    shapes = ["gh_quadrature_xyoz", "gh_quadrature_face", "gh_quadrature_edge"]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args,
        meta_funcs=meta_funcs, shapes=shapes)
    metadata.validate()
    kernel_arg_order = KernelArgOrder(metadata)
    assert len(kernel_arg_order.meta_arg_index_from_actual_index) == 1
    assert kernel_arg_order.meta_arg_index_from_actual_index[1] == 0
    assert kernel_arg_order.arg_info == [
        'nlayers', 'rfield_1', 'ndf_w0', 'undf_w0', 'map_w0',
        'basis_w0_qr_xyoz', 'basis_w0_qr_face', 'basis_w0_qr_edge',
        'diff_basis_w0_qr_xyoz', 'diff_basis_w0_qr_face',
        'diff_basis_w0_qr_edge', 'npxy_xyoz', 'np_z_xyoz',
        'weights_xz_xyoz', 'weights_z_xyoz', 'nfaces_face',
        'np_xyz_face', 'weights_xyz_face', 'nedges_edge',
        'np_xyz_edge', 'weights_xyz_edge']


def test_quad_rule_error():
    '''Test the kernel_arg_order class quad_rule method is called if a
    basis function or a differential basis function is specified in
    the metadata and quadrature shape information is also specified in
    the metadata. This method has already been part tested by
    test_basis_and_diff_basis(). This test checks that the expected
    exception is raised if unexpected shape metadata is found.

    '''
    # First create a valid instance of KernelArgOrder
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0")]
    meta_funcs = [
        MetaFuncsArgMetadata("W0", basis_function=True)]
    shapes = ["gh_quadrature_xyoz"]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args,
        meta_funcs=meta_funcs, shapes=shapes)
    metadata.validate()
    kernel_arg_order = KernelArgOrder(metadata)

    # Now cause exception.
    with pytest.raises(InternalError) as info:
        kernel_arg_order.quad_rule(["invalid"])
    assert ("Unexpected shape metadata. Found 'invalid' but expected one of "
            "['gh_quadrature_xyoz', 'gh_quadrature_face', "
            "'gh_quadrature_edge']." in str(info.value))


def test_field_bcs_kernel():
    '''Test the kernel_arg_order class calls the field_bcs_kernel method
    if the supplied kernel name is enforce_bc_code. Test for valid
    metadata.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "ANY_SPACE_1")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args,
        procedure_name="enforce_bc_code")
    metadata.validate()
    kernel_arg_order = KernelArgOrder(metadata)
    assert len(kernel_arg_order.meta_arg_index_from_actual_index) == 1
    assert kernel_arg_order.meta_arg_index_from_actual_index[1] == 0
    assert kernel_arg_order.arg_info == [
        'nlayers', 'rfield_1', 'ndf_any_space_1', 'undf_any_space_1',
        'map_any_space_1', 'boundary_dofs_rfield_1']


def test_field_bcs_kernel_error1():
    '''Test the kernel_arg_order class calls the field_bcs_kernel method
    if the supplied kernel name is enforce_bc_code. Test for an
    exception as there is more than one argument specified in the
    metadata.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "ANY_SPACE_1"),
        FieldArgMetadata("GH_REAL", "GH_READ", "ANY_SPACE_1")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args,
        procedure_name="enforce_bc_code")
    metadata.validate()
    with pytest.raises(InternalError) as info:
        _ = KernelArgOrder(metadata)
    assert ("An enforce_bc_code kernel should have a single argument but "
            "found '2'." in str(info.value))


def test_field_bcs_kernel_error2():
    '''Test the kernel_arg_order class calls the field_bcs_kernel method
    if the supplied kernel name is enforce_bc_code. Test for an
    exception as there is one argument but it is not a field.

    '''
    meta_args = [
        FieldVectorArgMetadata("GH_REAL", "GH_INC", "ANY_SPACE_1", "2")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args,
        procedure_name="enforce_bc_code")
    metadata.validate()
    with pytest.raises(InternalError) as info:
        _ = KernelArgOrder(metadata)
    assert ("An enforce_bc_code kernel should have a single field argument "
            "but found 'FieldVectorArgMetadata'." in str(info.value))


def test_field_bcs_kernel_error3():
    '''Test the kernel_arg_order class calls the field_bcs_kernel method
    if the supplied kernel name is enforce_bc_code. Test for an
    exception as there is one field argument but it is not on the
    any_space_1 function space.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args,
        procedure_name="enforce_bc_code")
    metadata.validate()
    with pytest.raises(InternalError) as info:
        _ = KernelArgOrder(metadata)
    assert ("An enforce_bc_code kernel should have a single field argument "
            "on the 'any_space_1' function space, but found 'w0'."
            in str(info.value))


def test_operator_bcs_kernel():
    '''Test the kernel_arg_order class calls the field_bcs_kernel method
    if the supplied kernel name is enforce_operator_bc_code. Test for
    valid metadata.

    '''
    meta_args = [
        OperatorArgMetadata("GH_REAL", "GH_READWRITE", "W0", "W1")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args,
        procedure_name="enforce_operator_bc_code")
    metadata.validate()
    kernel_arg_order = KernelArgOrder(metadata)
    assert len(kernel_arg_order.meta_arg_index_from_actual_index) == 1
    assert kernel_arg_order.meta_arg_index_from_actual_index[3] == 0
    assert kernel_arg_order.arg_info == [
        'cell', 'nlayers', 'op_1_ncell_3d', 'op_1', 'ndf_w0', 'ndf_w1',
        'boundary_dofs_op_1']


def test_operator_bcs_kernel_error1():
    '''Test the kernel_arg_order class calls the operator_bcs_kernel
    method if the supplied kernel name is
    enforce_operator_bc_code. Test for an exception as there is more
    than one argument specified in the metadata.

    '''
    meta_args = [
        OperatorArgMetadata("GH_REAL", "GH_READWRITE", "w0", "w1"),
        FieldArgMetadata("GH_REAL", "GH_READ", "w1")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args,
        procedure_name="enforce_operator_bc_code")
    metadata.validate()
    with pytest.raises(InternalError) as info:
        _ = KernelArgOrder(metadata)
    assert ("An enforce_operator_bc_code kernel should have a single "
            "argument but found '2'." in str(info.value))


def test_operator_bcs_kernel_error2():
    '''Test the kernel_arg_order class calls the operator_bcs_kernel method
    if the supplied kernel name is enforce_operator_bc_code. Test for an
    exception as there is one argument but it is not a field.

    '''
    meta_args = [
        FieldVectorArgMetadata("GH_REAL", "GH_READWRITE", "ANY_SPACE_1", "2")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args,
        procedure_name="enforce_operator_bc_code")
    metadata.validate()
    with pytest.raises(InternalError) as info:
        _ = KernelArgOrder(metadata)
    assert ("An enforce_operator_bc_code kernel should have a single lma "
            "operator argument but found 'FieldVectorArgMetadata'."
            in str(info.value))


def test_stencil_2d_unknown_extent():
    '''Test the kernel_arg_order class calls the stencil_2d_unknown_extent
    method if the supplied field metadata contains a cross2d
    stencil. Also tests stencil_2d_max_extent and stencil_2d methods.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0", stencil="cross2d")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()
    kernel_arg_order = KernelArgOrder(metadata)
    assert len(kernel_arg_order.meta_arg_index_from_actual_index) == 1
    assert kernel_arg_order.meta_arg_index_from_actual_index[1] == 0
    assert kernel_arg_order.arg_info == [
        'nlayers', 'rfield_1', 'rfield_1_stencil_size',
        'rfield_1_max_branch_length', 'rfield_1_stencil_dofmap',
        'ndf_w0', 'undf_w0', 'map_w0']


# kernel_arg_order class stencil_2d_max_extent has already been tested
# by test_stencil_2d_unknown_extent().


def test_stencil_unknown_extent():
    '''Test the kernel_arg_order class calls the stencil_unknown_extent
    method if the supplied field metadata contains an xory1d
    stencil. Also tests stencil_unknown_direction and stencil methods.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0", stencil="xory1d")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()
    kernel_arg_order = KernelArgOrder(metadata)
    assert len(kernel_arg_order.meta_arg_index_from_actual_index) == 1
    assert kernel_arg_order.meta_arg_index_from_actual_index[1] == 0
    assert kernel_arg_order.arg_info == [
        'nlayers', 'rfield_1', 'rfield_1_stencil_size', 'rfield_1_direction',
        'rfield_1_stencil_dofmap', 'ndf_w0', 'undf_w0', 'map_w0']


# kernel_arg_order class stencil_unknown_direction has already been tested
# by test_stencil_unknown_extent().
# kernel_arg_order class stencil_2d has already been tested
# by test_stencil_2d_unknown_extent().
# kernel_arg_order class stencil has already been tested
# by test_stencil_unknown_extent().


def test_banded_dofmap():
    '''Test that the KernelArgOrder class banded_dofmap is called if the
    kernel type is cma-assmembly.

    '''
    meta_args = [
        ColumnwiseOperatorArgMetadata("GH_REAL", "GH_READWRITE", "W0", "W1"),
        OperatorArgMetadata("GH_REAL", "GH_READ", "W0", "W1")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()
    assert metadata._get_kernel_type() == "cma-assembly"
    kernel_arg_order = KernelArgOrder(metadata)
    assert len(kernel_arg_order.meta_arg_index_from_actual_index) == 2
    assert kernel_arg_order.meta_arg_index_from_actual_index[3] == 0
    assert kernel_arg_order.meta_arg_index_from_actual_index[11] == 1
    assert kernel_arg_order.arg_info == [
        'cell', 'nlayers', 'ncell_2d', 'cma_op_1', 'nrow_cma_op_1',
        'ncol_cma_op_1', 'alpha_cma_op_1', 'beta_cma_op_1',
        'gamma_m_cma_op_1', 'gamma_p_cma_op_1', 'op_2_ncell_3d', 'op_2',
        'ndf_w0', 'cbanded_map_w0_cma_op_1', 'ndf_w1',
        'cbanded_map_w1_cma_op_1']


def test_indirection_dofmap():
    '''Test that the KernelArgOrder class indirection_dofmap is called if the
    kernel type is cma-apply.

    '''
    meta_args = [
        ColumnwiseOperatorArgMetadata("GH_REAL", "GH_READ", "W0", "W1"),
        FieldArgMetadata("GH_REAL", "GH_READ", "W1"),
        FieldArgMetadata("GH_REAL", "GH_INC", "W0")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()
    assert metadata._get_kernel_type() == "cma-apply"
    kernel_arg_order = KernelArgOrder(metadata)
    assert len(kernel_arg_order.meta_arg_index_from_actual_index) == 3
    assert kernel_arg_order.meta_arg_index_from_actual_index[2] == 0
    assert kernel_arg_order.meta_arg_index_from_actual_index[9] == 1
    assert kernel_arg_order.meta_arg_index_from_actual_index[10] == 2
    assert kernel_arg_order.arg_info == [
        'cell', 'ncell_2d', 'cma_op_1', 'nrow_cma_op_1', 'ncol_cma_op_1',
        'alpha_cma_op_1', 'beta_cma_op_1', 'gamma_m_cma_op_1',
        'gamma_p_cma_op_1', 'rfield_2', 'rfield_3', 'ndf_w0', 'undf_w0',
        'map_w0', 'cma_indirection_map_w0_cma_op_1', 'ndf_w1', 'undf_w1',
        'map_w1', 'cma_indirection_map_w1_cma_op_1']


def test_indirection_dofmap_single():
    '''Test that the KernelArgOrder class indirection_dofmap is called if
    the kernel type is cma-apply. Tests the case where the CMA
    function spaces are the same so only one indirection dofmap is
    required.

    '''
    meta_args = [
        ColumnwiseOperatorArgMetadata("GH_REAL", "GH_READ", "W0", "W0"),
        FieldArgMetadata("GH_REAL", "GH_READ", "W0"),
        FieldArgMetadata("GH_REAL", "GH_INC", "W0")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()
    assert metadata._get_kernel_type() == "cma-apply"
    kernel_arg_order = KernelArgOrder(metadata)
    assert len(kernel_arg_order.meta_arg_index_from_actual_index) == 3
    assert kernel_arg_order.meta_arg_index_from_actual_index[2] == 0
    assert kernel_arg_order.meta_arg_index_from_actual_index[8] == 1
    assert kernel_arg_order.meta_arg_index_from_actual_index[9] == 2
    assert kernel_arg_order.arg_info == [
        'cell', 'ncell_2d', 'cma_op_1', 'nrow_cma_op_1', 'alpha_cma_op_1',
        'beta_cma_op_1', 'gamma_m_cma_op_1', 'gamma_p_cma_op_1', 'rfield_2',
        'rfield_3', 'ndf_w0', 'undf_w0', 'map_w0',
        'cma_indirection_map_w0_cma_op_1']


def test_generate_error(monkeypatch):
    '''Most of the _generate function has been tested by the earlier tests
    in this file. This test covers the exception that _generate raises
    if there is invalid input.

    '''
    # First create a valid instance of KernelArgOrder
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0", stencil="xory1d")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()
    kernel_arg_order = KernelArgOrder(metadata)

    # Now cause exception.
    monkeypatch.setattr(metadata._meta_args, "_meta_args_args", [None])
    monkeypatch.setattr(
        metadata, "_get_kernel_type", lambda: "general-purpose")
    with pytest.raises(InternalError) as info:
        kernel_arg_order._generate(metadata)
    assert "Unexpected meta_arg type 'NoneType' found." in str(info.value)


# TODO tests for meta_arg name functions.
# TODO Check names needing FUNCTION SPACE MANGLED NAMES i.e. any_space_x
