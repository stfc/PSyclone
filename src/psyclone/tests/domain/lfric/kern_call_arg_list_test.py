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
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab
# Modified I. Kavcic, Met Office
# Modified J. Henrichs, Bureau of Meteorology

''' This module tests the LFric classes based on ArgOrdering.'''

import os

import pytest

from psyclone.core import Signature, VariablesAccessInfo
from psyclone.domain.lfric import KernCallArgList
from psyclone.errors import GenerationError, InternalError
from psyclone.dynamo0p3 import DynKern
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir.nodes import Literal, Reference
from psyclone.tests.utilities import get_base_path, get_invoke
from psyclone.transformations import Dynamo0p3ColourTrans

TEST_API = "dynamo0.3"


def check_psyir_results(create_arg_list, fortran_writer):
    '''Helper function to check if the PSyIR representation of the arguments
     is identical to the old style textual representation. It checks that each
     member of the psyir_arglist is a Reference, and that the textural
     representation matches the textual presentation (which was already
     verified).

     '''
    # Check the PSyIR representation
    result = []
    for node in create_arg_list.psyir_arglist:
        assert isinstance(node, Reference)
        result.append(fortran_writer(node))

    assert result == create_arg_list._arglist


def test_field_prolong(dist_mem, fortran_writer):
    ''' Check that we generate correct psy-layer code for an invoke
    containing a kernel that performs a prolongation operation '''

    full_path = os.path.join(get_base_path(TEST_API),
                             "22.0_intergrid_prolong.f90")

    _, invoke_info = parse(full_path, api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule.kernels()[0]

    create_arg_list = KernCallArgList(kernel)
    create_arg_list.generate()
    assert create_arg_list._arglist == [
        'nlayers', 'cell_map_field2(:,:,cell)', 'ncpc_field1_field2_x',
        'ncpc_field1_field2_y', 'ncell_field1', 'field1_proxy%data',
        'field2_proxy%data', 'ndf_w1', 'undf_w1', 'map_w1', 'undf_w2',
        'map_w2(:,cell)']

    check_psyir_results(create_arg_list, fortran_writer)


def test_kerncallarglist_face_xyoz(dist_mem, fortran_writer):
    ''' Check the handling of basis, diff_basis, and face and xyoz
    quadrature.'''

    psy, _ = get_invoke("1.1.9_single_invoke_2qr_shapes_int_field.f90",
                        TEST_API, dist_mem=dist_mem, idx=0)

    schedule = psy.invokes.invoke_list[0].schedule
    create_arg_list = KernCallArgList(schedule.kernels()[0])

    create_arg_list.generate()
    assert create_arg_list._arglist == [
        'nlayers', 'f1_proxy%data', 'f2_proxy(1)%data', 'f2_proxy(2)%data',
        'f2_proxy(3)%data', 'f3_proxy%data', 'istp', 'ndf_w2', 'undf_w2',
        'map_w2(:,cell)', 'basis_w2_qr_xyoz', 'basis_w2_qr_face', 'ndf_wchi',
        'undf_wchi', 'map_wchi(:,cell)', 'diff_basis_wchi_qr_xyoz',
        'diff_basis_wchi_qr_face', 'ndf_adspc1_f3', 'undf_adspc1_f3',
        'map_adspc1_f3(:,cell)', 'basis_adspc1_f3_qr_xyoz',
        'basis_adspc1_f3_qr_face', 'diff_basis_adspc1_f3_qr_xyoz',
        'diff_basis_adspc1_f3_qr_face', 'np_xy_qr_xyoz', 'np_z_qr_xyoz',
        'weights_xy_qr_xyoz', 'weights_z_qr_xyoz', 'nfaces_qr_face',
        'np_xyz_qr_face', 'weights_xyz_qr_face']

    check_psyir_results(create_arg_list, fortran_writer)


def test_kerncallarglist_face_edge(dist_mem, fortran_writer):
    ''' Check the handling of basis, diff_basis, and quad_rule, including
    face and edge quadrature
    '''

    psy, _ = get_invoke("1.1.7_face_and_edge_qr.f90",
                        TEST_API, dist_mem=dist_mem, idx=0)

    schedule = psy.invokes.invoke_list[0].schedule
    create_arg_list = KernCallArgList(schedule.kernels()[0])

    create_arg_list.generate()
    assert create_arg_list._arglist == [
        'nlayers', 'f1_proxy%data', 'f2_proxy%data', 'm1_proxy%data',
        'm2_proxy%data', 'ndf_w1', 'undf_w1', 'map_w1(:,cell)',
        'basis_w1_qr_face', 'basis_w1_qr_edge', 'ndf_w2', 'undf_w2',
        'map_w2(:,cell)', 'diff_basis_w2_qr_face', 'diff_basis_w2_qr_edge',
        'ndf_w3', 'undf_w3', 'map_w3(:,cell)', 'basis_w3_qr_face',
        'basis_w3_qr_edge', 'diff_basis_w3_qr_face', 'diff_basis_w3_qr_edge',
        'nfaces_qr_face', 'np_xyz_qr_face', 'weights_xyz_qr_face',
        'nedges_qr_edge', 'np_xyz_qr_edge', 'weights_xyz_qr_edge']

    check_psyir_results(create_arg_list, fortran_writer)

    # Test that invalid kernel arguments raise the expected error:
    create_arg_list._kern._qr_rules["gh_quadrature_face"] = \
        DynKern.QRRule("invalid", "invalid", ["invalid"])
    with pytest.raises(InternalError) as err:
        create_arg_list.generate()
    assert "Found invalid kernel argument 'invalid'" in str(err.value)


def test_kerncallarglist_colouring(dist_mem, fortran_writer):
    ''' Check the handling of coluring.
    '''

    psy, _ = get_invoke("4.8_multikernel_invokes.f90",
                        TEST_API, dist_mem=dist_mem, idx=0)

    schedule = psy.invokes.invoke_list[0].schedule
    ctrans = Dynamo0p3ColourTrans()
    indx = 0
    if dist_mem:
        # Skip the halo exchange nodes when dist_mem is enabled
        indx = 4
    ctrans.apply(schedule.children[indx])

    create_arg_list = KernCallArgList(schedule.kernels()[0])
    create_arg_list.generate()
    assert create_arg_list._arglist == [
        'nlayers', 'rdt', 'h_proxy%data', 'f_proxy%data', 'c_proxy%data',
        'd_proxy%data', 'ndf_w1', 'undf_w1', 'map_w1(:,cmap(colour,cell))',
        'ndf_w2', 'undf_w2', 'map_w2(:,cmap(colour,cell))', 'ndf_w3',
        'undf_w3', 'map_w3(:,cmap(colour,cell))']

    check_psyir_results(create_arg_list, fortran_writer)


def test_kerncallarglist_mesh_properties(fortran_writer):
    ''' Check the handling of mesh properties.
    '''

    psy, _ = get_invoke("24.1_mesh_prop_invoke.f90",
                        TEST_API, dist_mem=False, idx=0)

    schedule = psy.invokes.invoke_list[0].schedule
    ctrans = Dynamo0p3ColourTrans()
    ctrans.apply(schedule.children[0])

    create_arg_list = KernCallArgList(schedule.kernels()[0])
    var_info = VariablesAccessInfo()
    create_arg_list.generate(var_accesses=var_info)
    assert str(var_info) == ("a: READ, adjacent_face: READ, cell: READ, "
                             "cmap: READ, colour: READ, f1: READ+WRITE, "
                             "map_w1: READ, ndf_w1: READ, nfaces_re_h: "
                             "READ, nlayers: READ, undf_w1: READ")
    # Tests that multiple reads are reported as expected:
    assert str(var_info[Signature("cell")]) == "cell:READ(0),READ(0)"
    assert str(var_info[Signature("colour")]) == "colour:READ(0),READ(0)"
    assert str(var_info[Signature("cmap")]) == "cmap:READ(0),READ(0)"
    assert str(var_info[Signature("adjacent_face")]) == "adjacent_face:READ(0)"

    assert create_arg_list._arglist == [
        'nlayers', 'a', 'f1_proxy%data', 'ndf_w1', 'undf_w1',
        'map_w1(:,cmap(colour,cell))', 'nfaces_re_h',
        'adjacent_face(:,cmap(colour,cell))']

    check_psyir_results(create_arg_list, fortran_writer)


def test_kerncallarglist_evaluator(fortran_writer):
    ''' Check the handling of evaluators.
    '''

    psy, _ = get_invoke("6.1_eval_invoke.f90", TEST_API,
                        dist_mem=False, idx=0)

    schedule = psy.invokes.invoke_list[0].schedule
    ctrans = Dynamo0p3ColourTrans()
    ctrans.apply(schedule.children[0])

    create_arg_list = KernCallArgList(schedule.kernels()[0])
    create_arg_list.generate()
    assert create_arg_list._arglist == [
        'nlayers', 'f0_proxy%data', 'f1_proxy%data', 'ndf_w0', 'undf_w0',
        'map_w0(:,cmap(colour,cell))', 'basis_w0_on_w0', 'ndf_w1', 'undf_w1',
        'map_w1(:,cmap(colour,cell))', 'diff_basis_w1_on_w0']

    check_psyir_results(create_arg_list, fortran_writer)


def test_kerncallarglist_stencil(fortran_writer):
    ''' Check the handling of stencils.
    '''

    psy, _ = get_invoke("19.7_multiple_stencils.f90", TEST_API,
                        dist_mem=False, idx=0)

    schedule = psy.invokes.invoke_list[0].schedule
    ctrans = Dynamo0p3ColourTrans()
    ctrans.apply(schedule.children[0])

    create_arg_list = KernCallArgList(schedule.kernels()[0])
    create_arg_list.generate()

    assert create_arg_list._arglist == [
        'nlayers', 'f1_proxy%data', 'f2_proxy%data',
        'f2_stencil_size(cmap(colour,cell))',
        'f2_stencil_dofmap(:,:,cmap(colour,cell))', 'f3_proxy%data',
        'f3_stencil_size(cmap(colour,cell))', 'f3_direction',
        'f3_stencil_dofmap(:,:,cmap(colour,cell))', 'f4_proxy%data',
        'f4_stencil_size(cmap(colour,cell))',
        'f4_stencil_dofmap(:,:,cmap(colour,cell))', 'ndf_w1', 'undf_w1',
        'map_w1(:,cmap(colour,cell))', 'ndf_w2', 'undf_w2',
        'map_w2(:,cmap(colour,cell))', 'ndf_w3', 'undf_w3',
        'map_w3(:,cmap(colour,cell))']

    assert create_arg_list.nlayers_positions == [1]
    assert create_arg_list.ndf_positions == [
        KernCallArgList.NdfInfo(13, "w1"),
        KernCallArgList.NdfInfo(16, "w2"),
        KernCallArgList.NdfInfo(19, "w3")]

    check_psyir_results(create_arg_list, fortran_writer)


def test_kerncallarglist_cross2d_stencil(fortran_writer):
    ''' Check the handling of cross-2d stencils.
    '''

    psy, _ = get_invoke("19.26_single_stencil_cross2d.f90", TEST_API,
                        dist_mem=False, idx=0)

    schedule = psy.invokes.invoke_list[0].schedule
    create_arg_list = KernCallArgList(schedule.kernels()[0])
    create_arg_list.generate()

    assert create_arg_list._arglist == [
        'nlayers', 'f1_proxy%data', 'f2_proxy%data',
        'f2_stencil_size(:,cell)', 'f2_max_branch_length',
        'f2_stencil_dofmap(:,:,:,cell)', 'f3_proxy%data', 'f4_proxy%data',
        'ndf_w1', 'undf_w1', 'map_w1(:,cell)', 'ndf_w2', 'undf_w2',
        'map_w2(:,cell)', 'ndf_w3', 'undf_w3', 'map_w3(:,cell)'
    ]
    check_psyir_results(create_arg_list, fortran_writer)


def test_kerncallarglist_bcs(fortran_writer, monkeypatch):
    ''' Check the handling of bc_kernel
    '''

    psy, _ = get_invoke("12.2_enforce_bc_kernel.f90", TEST_API,
                        dist_mem=False, idx=0)

    schedule = psy.invokes.invoke_list[0].schedule
    create_arg_list = KernCallArgList(schedule.kernels()[0])
    create_arg_list.generate()
    assert create_arg_list._arglist == [
        'nlayers', 'a_proxy%data', 'ndf_aspc1_a', 'undf_aspc1_a',
        'map_aspc1_a(:,cell)', 'boundary_dofs_a']

    check_psyir_results(create_arg_list, fortran_writer)

    loop = schedule.children[0]
    call = loop.loop_body[0]
    arg = call.arguments.args[0]

    # Monkeypatch the argument object so that it thinks it is an
    # operator rather than a field
    monkeypatch.setattr(arg, "_argument_type", value="gh_operator")
    with pytest.raises(GenerationError) as err:
        create_arg_list.generate()
    assert ("Expected an argument of ['gh_field'] type from which to look-up "
            "boundary dofs for kernel enforce_bc_code but got 'gh_operator'"
            in str(err.value))


def test_kerncallarglist_bcs_operator(fortran_writer):
    ''' Check the handling of bc_kernel operators.
    '''

    psy, _ = get_invoke("12.4_enforce_op_bc_kernel.f90", TEST_API,
                        dist_mem=False, idx=0)

    schedule = psy.invokes.invoke_list[0].schedule
    create_arg_list = KernCallArgList(schedule.kernels()[0])
    create_arg_list.generate()
    assert create_arg_list._arglist == [
        'cell', 'nlayers', 'op_a_proxy%ncell_3d', 'op_a_proxy%local_stencil',
        'ndf_aspc1_op_a', 'ndf_aspc2_op_a', 'boundary_dofs_op_a']

    check_psyir_results(create_arg_list, fortran_writer)


def test_kerncallarglist_scalar_literal(fortran_writer):
    ''' Check the handling of a scalar literal.
    '''

    psy, _ = get_invoke("int_real_literal_scalar.f90", TEST_API,
                        dist_mem=False, idx=0)

    schedule = psy.invokes.invoke_list[0].schedule
    create_arg_list = KernCallArgList(schedule.kernels()[0])
    create_arg_list.generate()
    assert create_arg_list._arglist == [
        'nlayers', 'f1_proxy%data', 'f2_proxy%data', 'm1_proxy%data',
        '1.0_r_def', 'm2_proxy%data', '2_i_def', 'ndf_w1', 'undf_w1',
        'map_w1(:,cell)', 'basis_w1_qr', 'ndf_w2', 'undf_w2', 'map_w2(:,cell)',
        'diff_basis_w2_qr', 'ndf_w3', 'undf_w3', 'map_w3(:,cell)',
        'basis_w3_qr', 'diff_basis_w3_qr', 'np_xy_qr', 'np_z_qr',
        'weights_xy_qr', 'weights_z_qr']

    # There is a difference between the string version and the PSyIR
    # representation: the latter has the precision appended to the constant
    # (i.e. `2_i_def` vs `2` and `1.0_r_def` vs `1.0`).
    result = []
    for node in create_arg_list.psyir_arglist:
        assert isinstance(node, (Literal, Reference))
        result.append(fortran_writer(node))
    assert result == [
        'nlayers', 'f1_proxy%data', 'f2_proxy%data', 'm1_proxy%data',
        '1.0_r_def', 'm2_proxy%data', '2_i_def', 'ndf_w1', 'undf_w1',
        'map_w1(:,cell)', 'basis_w1_qr', 'ndf_w2', 'undf_w2',
        'map_w2(:,cell)', 'diff_basis_w2_qr', 'ndf_w3', 'undf_w3',
        'map_w3(:,cell)', 'basis_w3_qr', 'diff_basis_w3_qr', 'np_xy_qr',
        'np_z_qr', 'weights_xy_qr', 'weights_z_qr']

    assert create_arg_list.nqp_positions == [{'horizontal': 21,
                                              'vertical': 22}]
