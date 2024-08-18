# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
# Modified I. Kavcic and L. Turner, Met Office
# Modified J. Henrichs, Bureau of Meteorology

''' This module tests the LFric KernCallArg class.'''

import os
import re
import pytest

from psyclone.core import Signature, VariablesAccessInfo
from psyclone.domain.lfric import (KernCallArgList, LFRicSymbolTable,
                                   LFRicTypes, LFRicKern)
from psyclone.errors import GenerationError, InternalError
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir.nodes import Literal, Loop, Reference, UnaryOperation
from psyclone.psyir.symbols import (
    ArrayType, ScalarType, UnsupportedFortranType)
from psyclone.tests.utilities import get_base_path, get_invoke
from psyclone.transformations import Dynamo0p3ColourTrans

TEST_API = "dynamo0.3"


def check_psyir_results(create_arg_list, fortran_writer, valid_classes=None):
    '''Helper function to check if the PSyIR representation of the arguments
    is identical to the old style textual representation. It checks that each
    member of the psyir_arglist is a Reference, and that the textual
    representation matches the textual presentation (which was already
    verified).

    :param create_arg_list: a KernCallArgList instance.
    :type create_arg_list: :py:class:`psyclone.domain.lfric.KernCallArgList`
    :param fortran_writer: a FortranWriter instance.
    :type fortran_writer:
        :py:class:`psyclone.psyir.backend.fortran.FortranWriter`
    :param valid_classes: a tuple of classes that are expected in the PSyIR
        argument list. Defaults to `(Reference)`.
    :type valid_classes: Tuple[:py:class:`psyclone.psyir.nodes.node`]

    '''
    if not valid_classes:
        valid_classes = Reference

    # Check the PSyIR representation
    result = []
    for node in create_arg_list.psyir_arglist:
        assert isinstance(node, valid_classes)
        out = fortran_writer(node)
        # We're comparing old (textual) and new (PSyIR) approaches here and
        # only the new, PSyIR approach supports the addition of array-slice
        # notation (e.g. 'array(:)'). Therefore, we remove it before comparing.
        result.append(re.sub(r"[(]\s*:(,\s*:)*\s*[)]$", "", out))

    assert result == create_arg_list._arglist


def test_cellmap_intergrid(dist_mem, fortran_writer):
    ''' Check the handling of cell_map and fs_intergrid.'''

    full_path = os.path.join(get_base_path(TEST_API),
                             "22.0_intergrid_prolong.f90")

    _, invoke_info = parse(full_path, api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule.kernels()[0]

    create_arg_list = KernCallArgList(kernel)
    vai = VariablesAccessInfo()
    create_arg_list.generate(vai)

    # Verify that no array expression turns up in the access information
    assert "cell_map_field2(:,:,cell)" not in str(vai)
    assert Signature("cell_map_field2") in vai

    assert create_arg_list._arglist == [
        'nlayers', 'cell_map_field2(:,:,cell)', 'ncpc_field1_field2_x',
        'ncpc_field1_field2_y', 'ncell_field1', 'field1_data',
        'field2_data', 'ndf_w1', 'undf_w1', 'map_w1', 'undf_w2',
        'map_w2(:,cell)']

    check_psyir_results(create_arg_list, fortran_writer)
    arg = create_arg_list.psyir_arglist[5]
    assert isinstance(arg.datatype, UnsupportedFortranType)
    assert isinstance(arg.datatype.partial_datatype, ArrayType)
    assert len(arg.datatype.partial_datatype.shape) == 1


def test_kerncallarglist_face_xyoz(dist_mem, fortran_writer):
    ''' Check the handling of basis, diff_basis, and face and xyoz
    quadrature.'''

    psy, _ = get_invoke("1.1.9_single_invoke_2qr_shapes_int_field.f90",
                        TEST_API, dist_mem=dist_mem, idx=0)

    schedule = psy.invokes.invoke_list[0].schedule
    create_arg_list = KernCallArgList(schedule.kernels()[0])

    create_arg_list.generate()
    assert create_arg_list._arglist == [
        'nlayers', 'f1_data', 'f2_1_data', 'f2_2_data',
        'f2_3_data', 'f3_data', 'istp', 'ndf_w2', 'undf_w2',
        'map_w2(:,cell)', 'basis_w2_qr_xyoz', 'basis_w2_qr_face', 'ndf_wchi',
        'undf_wchi', 'map_wchi(:,cell)', 'diff_basis_wchi_qr_xyoz',
        'diff_basis_wchi_qr_face', 'ndf_adspc1_f3', 'undf_adspc1_f3',
        'map_adspc1_f3(:,cell)', 'basis_adspc1_f3_qr_xyoz',
        'basis_adspc1_f3_qr_face', 'diff_basis_adspc1_f3_qr_xyoz',
        'diff_basis_adspc1_f3_qr_face', 'np_xy_qr_xyoz', 'np_z_qr_xyoz',
        'weights_xy_qr_xyoz', 'weights_z_qr_xyoz', 'nfaces_qr_face',
        'np_xyz_qr_face', 'weights_xyz_qr_face']

    check_psyir_results(create_arg_list, fortran_writer)

    # Check that the right datatype is set:
    # The kernel in question accepts integer fields.
    array_1d = ArrayType(LFRicTypes("LFRicIntegerScalarDataType")(),
                         [ArrayType.Extent.DEFERRED])
    assert isinstance(create_arg_list.psyir_arglist[2].datatype,
                      UnsupportedFortranType)
    assert (create_arg_list.psyir_arglist[2].datatype.partial_datatype ==
            array_1d)
    array_4d = ArrayType(LFRicTypes("LFRicRealScalarDataType")(),
                         [ArrayType.Extent.DEFERRED]*4)
    assert create_arg_list.psyir_arglist[15].datatype == array_4d
    assert create_arg_list.psyir_arglist[16].datatype == array_4d


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
        'nlayers', 'f1_data', 'f2_data', 'm1_data',
        'm2_data', 'ndf_w1', 'undf_w1', 'map_w1(:,cell)',
        'basis_w1_qr_face', 'basis_w1_qr_edge', 'ndf_w2', 'undf_w2',
        'map_w2(:,cell)', 'diff_basis_w2_qr_face', 'diff_basis_w2_qr_edge',
        'ndf_w3', 'undf_w3', 'map_w3(:,cell)', 'basis_w3_qr_face',
        'basis_w3_qr_edge', 'diff_basis_w3_qr_face', 'diff_basis_w3_qr_edge',
        'nfaces_qr_face', 'np_xyz_qr_face', 'weights_xyz_qr_face',
        'nedges_qr_edge', 'np_xyz_qr_edge', 'weights_xyz_qr_edge']

    check_psyir_results(create_arg_list, fortran_writer)

    # Test that invalid kernel arguments raise the expected error:
    create_arg_list._kern._qr_rules["gh_quadrature_face"] = \
        LFRicKern.QRRule("invalid", "invalid", ["invalid"])
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
    loops = schedule.walk(Loop)
    ctrans.apply(loops[0])

    create_arg_list = KernCallArgList(schedule.kernels()[0])
    create_arg_list.generate()
    assert create_arg_list._arglist == [
        'nlayers', 'rdt', 'h_data', 'f_data', 'c_data',
        'd_data', 'ndf_w1', 'undf_w1', 'map_w1(:,cmap(colour,cell))',
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
                             "cmap: READ, colour: READ, f1_data: READ+WRITE, "
                             "map_w1: READ, ndf_w1: READ, nfaces_re_h: "
                             "READ, nlayers: READ, undf_w1: READ")
    # Tests that multiple reads are reported as expected:
    assert str(var_info[Signature("cell")]) == "cell:READ(0),READ(0)"
    assert str(var_info[Signature("colour")]) == "colour:READ(0),READ(0)"
    assert str(var_info[Signature("cmap")]) == "cmap:READ(0),READ(0)"
    assert str(var_info[Signature("adjacent_face")]) == "adjacent_face:READ(0)"

    assert create_arg_list._arglist == [
        'nlayers', 'a', 'f1_data', 'ndf_w1', 'undf_w1',
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
        'nlayers', 'f0_data', 'cmap_data', 'ndf_w0', 'undf_w0',
        'map_w0(:,cmap_1(colour,cell))', 'basis_w0_on_w0', 'ndf_w1', 'undf_w1',
        'map_w1(:,cmap_1(colour,cell))', 'diff_basis_w1_on_w0']

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
        'nlayers', 'f1_data', 'f2_data',
        'f2_stencil_size(cmap(colour,cell))',
        'f2_stencil_dofmap(:,:,cmap(colour,cell))', 'f3_data',
        'f3_stencil_size(cmap(colour,cell))', 'f3_direction',
        'f3_stencil_dofmap(:,:,cmap(colour,cell))', 'f4_data',
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
        'nlayers', 'f1_data', 'f2_data',
        'f2_stencil_size(:,cell)', 'f2_max_branch_length',
        'f2_stencil_dofmap(:,:,:,cell)', 'f3_data', 'f4_data',
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
        'nlayers', 'a_data', 'ndf_aspc1_a', 'undf_aspc1_a',
        'map_aspc1_a(:,cell)', 'boundary_dofs_a']

    check_psyir_results(create_arg_list, fortran_writer)

    loop = schedule.children[0]
    call = loop.loop_body[0]
    arg = call.arguments.args[0]

    # Monkeypatch the argument object so that it thinks it is an
    # operator rather than a field
    monkeypatch.setattr(arg, "_argument_type", value="gh_operator")
    # Add a tag to the symbol table to allow us to get through to the error
    # (since the argument is/was a field, its tagged name is now wrong).
    schedule.symbol_table.find_or_create_tag("a:local_stencil")
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
    access_info = VariablesAccessInfo()
    create_arg_list.generate(access_info)
    assert create_arg_list._arglist == [
        'cell', 'nlayers', 'op_a_proxy%ncell_3d', 'op_a_local_stencil',
        'ndf_aspc1_op_a', 'ndf_aspc2_op_a', 'boundary_dofs_op_a']

    check_psyir_results(create_arg_list, fortran_writer)
    assert (create_arg_list.psyir_arglist[2].datatype ==
            LFRicTypes("LFRicIntegerScalarDataType")())
    arg = create_arg_list.psyir_arglist[3]
    assert isinstance(arg.datatype, UnsupportedFortranType)
    assert arg.datatype.partial_datatype.precision.name == "r_def"
    assert len(arg.datatype.partial_datatype.shape) == 3

    # Also check that the structure access is correctly converted
    # into a 2-component signature:
    sig = Signature(("op_a_proxy", "ncell_3d"))
    assert str(access_info[sig]) == "op_a_proxy%ncell_3d:READ(0)"
    assert (str(access_info[Signature("op_a_local_stencil")]) ==
            "op_a_local_stencil:READWRITE(0)")


def test_kerncallarglist_mixed_precision():
    ''' Check the handling of mixed precision. This kernel has five invokes:
    The first using 'r_def', the second 'r_solver', the third 'r_tran', the
    fourth 'r_bl' and the fifth 'r_phys'.
    '''

    psy, _ = get_invoke("26.8_mixed_precision_args.f90", TEST_API,
                        dist_mem=False, idx=0)

    schedule = psy.invokes.invoke_list[0].schedule
    create_arg_list = KernCallArgList(schedule.kernels()[0])
    create_arg_list.generate()
    # TODO #744: Depending on the implementation of #744, we can replace
    # the test for name with a test for the actual precision, e.g.:
    # assert create_arg_list.psyir_arglist[3].datatype.precision == psyir.R_DEF

    # Scalar:
    assert create_arg_list.psyir_arglist[2].datatype.precision.name == "r_def"
    # Field.
    assert isinstance(create_arg_list.psyir_arglist[3].datatype,
                      UnsupportedFortranType)
    assert isinstance(
        create_arg_list.psyir_arglist[3].datatype.partial_datatype, ArrayType)
    # operator: ncell_3d:
    assert create_arg_list.psyir_arglist[4].datatype.precision.name == "i_def"
    # operator: local_stencil
    arg5 = create_arg_list.psyir_arglist[5]
    assert isinstance(arg5.datatype, UnsupportedFortranType)
    assert isinstance(arg5.datatype.partial_datatype, ArrayType)
    assert len(arg5.datatype.partial_datatype.shape) == 3

    create_arg_list = KernCallArgList(schedule.kernels()[1])
    create_arg_list.generate()
    assert (create_arg_list.psyir_arglist[2].datatype.precision.name ==
            "r_solver")
    assert isinstance(create_arg_list.psyir_arglist[3].datatype,
                      UnsupportedFortranType)
    assert isinstance(
        create_arg_list.psyir_arglist[3].datatype.partial_datatype, ArrayType)
    assert create_arg_list.psyir_arglist[4].datatype.precision.name == "i_def"
    arg5 = create_arg_list.psyir_arglist[5]
    assert isinstance(arg5.datatype, UnsupportedFortranType)
    assert isinstance(arg5.datatype.partial_datatype, ArrayType)

    create_arg_list = KernCallArgList(schedule.kernels()[2])
    create_arg_list.generate()
    assert create_arg_list.psyir_arglist[2].datatype.precision.name == "r_tran"
    assert isinstance(create_arg_list.psyir_arglist[3].datatype,
                      UnsupportedFortranType)
    assert isinstance(
        create_arg_list.psyir_arglist[3].datatype.partial_datatype,
        ArrayType)
    assert create_arg_list.psyir_arglist[4].datatype.precision.name == "i_def"
    arg5 = create_arg_list.psyir_arglist[5]
    assert isinstance(arg5.datatype, UnsupportedFortranType)
    assert isinstance(arg5.datatype.partial_datatype, ArrayType)

    create_arg_list = KernCallArgList(schedule.kernels()[3])
    create_arg_list.generate()
    assert create_arg_list.psyir_arglist[2].datatype.precision.name == "r_bl"
    assert isinstance(
        create_arg_list.psyir_arglist[3].datatype.partial_datatype,
        ArrayType)
    arg5 = create_arg_list.psyir_arglist[5]
    assert isinstance(arg5.datatype, UnsupportedFortranType)
    assert isinstance(arg5.datatype.partial_datatype, ArrayType)

    create_arg_list = KernCallArgList(schedule.kernels()[4])
    create_arg_list.generate()
    assert create_arg_list.psyir_arglist[2].datatype.precision.name == "r_phys"
    assert isinstance(create_arg_list.psyir_arglist[3].datatype,
                      UnsupportedFortranType)
    assert isinstance(
        create_arg_list.psyir_arglist[3].datatype.partial_datatype,
        ArrayType)
    arg5 = create_arg_list.psyir_arglist[5]
    assert isinstance(arg5.datatype, UnsupportedFortranType)
    assert isinstance(arg5.datatype.partial_datatype, ArrayType)


def test_kerncallarglist_scalar_literal(fortran_writer):
    ''' Check the handling of a scalar literal.
    '''

    psy, _ = get_invoke("int_real_literal_scalar.f90", TEST_API,
                        dist_mem=False, idx=0)

    schedule = psy.invokes.invoke_list[0].schedule
    vai = VariablesAccessInfo()
    create_arg_list = KernCallArgList(schedule.kernels()[0])
    create_arg_list.generate(vai)

    # Verify that a constant is not returned in the access info list
    assert "1.0" not in str(vai)

    assert create_arg_list._arglist == [
        'nlayers', 'f1_data', 'f2_data', 'm1_data',
        '1.0_r_def', 'm2_data', '2_i_def', 'ndf_w1', 'undf_w1',
        'map_w1(:,cell)', 'basis_w1_qr', 'ndf_w2', 'undf_w2', 'map_w2(:,cell)',
        'diff_basis_w2_qr', 'ndf_w3', 'undf_w3', 'map_w3(:,cell)',
        'basis_w3_qr', 'diff_basis_w3_qr', 'np_xy_qr', 'np_z_qr',
        'weights_xy_qr', 'weights_z_qr']

    check_psyir_results(create_arg_list, fortran_writer, (Literal, Reference))

    assert create_arg_list.nqp_positions == [{'horizontal': 21,
                                              'vertical': 22}]

    # Check the handling of logical types. The third argument
    # should be a scalar
    args = schedule.kernels()[0].arguments.args
    assert args[3].is_scalar

    # Make sure a negative number is accepted:
    args[3]._name = "-2.0_r_def"
    create_arg_list.scalar(args[3])
    lit = create_arg_list.psyir_arglist[-1]
    # In Fortran a negative number as above is an UnaryOperation
    assert isinstance(lit, UnaryOperation)

    # Make sure a negative number with a space works
    args[3]._name = "- 2.0_r_def"
    create_arg_list.scalar(args[3])
    lit = create_arg_list.psyir_arglist[-1]
    # In Fortran a negative number as above is an UnaryOperation
    assert isinstance(lit, UnaryOperation)

    # Modify the third argument to be a logical
    args[3]._intrinsic_type = "logical"
    args[3]._name = ".true."
    create_arg_list.scalar(args[3])
    lit = create_arg_list.psyir_arglist[-1]
    assert isinstance(lit, Literal)
    assert lit.datatype.intrinsic == ScalarType.Intrinsic.BOOLEAN

    # Now set the intrinsic type to be invalid and check that the
    # underlying PSyIR creation catches it.
    args[3]._name = "invalid"
    args[3]._intrinsic_type = "invalid"
    with pytest.raises(InternalError) as err:
        create_arg_list.scalar(args[3])
    assert ("Expected argument 'invalid' to kernel 'testkern_qr_code' "
            "to be a literal but the created PSyIR contains one or more "
            "References." in str(err.value))


def test_indirect_dofmap(fortran_writer):
    '''Test the indirect dofmap functionality.
    '''
    psy, _ = get_invoke("20.1.2_cma_apply_disc.f90", TEST_API,
                        dist_mem=False, idx=0)

    schedule = psy.invokes.invoke_list[0].schedule
    create_arg_list = KernCallArgList(schedule.kernels()[0])
    create_arg_list.generate()
    assert (create_arg_list._arglist == [
        'cell', 'ncell_2d', 'field_a_data', 'field_b_data',
        'cma_op1_cma_matrix', 'cma_op1_nrow', 'cma_op1_ncol',
        'cma_op1_bandwidth', 'cma_op1_alpha', 'cma_op1_beta',
        'cma_op1_gamma_m', 'cma_op1_gamma_p',
        'ndf_adspc1_field_a', 'undf_adspc1_field_a',
        'map_adspc1_field_a(:,cell)', 'cma_indirection_map_adspc1_field_a',
        'ndf_aspc1_field_b', 'undf_aspc1_field_b', 'map_aspc1_field_b(:,cell)',
        'cma_indirection_map_aspc1_field_b'])

    check_psyir_results(create_arg_list, fortran_writer)

    psyir_args = create_arg_list.psyir_arglist
    sym_tab = schedule.symbol_table
    for ref in psyir_args:
        assert ref.symbol.name in sym_tab

    for i in [0, 1, 5, 6, 7, 8, 9, 10, 11, 12, 13, 16, 17]:
        assert (psyir_args[i].symbol.datatype ==
                LFRicTypes("LFRicIntegerScalarDataType")())

    # Create a dummy LFRic symbol table to simplify creating
    # standard LFRic types:
    dummy_sym_tab = LFRicSymbolTable()
    # Test all 1D real arrays:
    for i in [2, 3]:
        # The datatype of a field reference is of UnsupportedFortranType
        # because the PSyIR doesn't support pointers. However, its
        # 'partial_datatype' is the type of the member accessed, i.e. it's
        # the 1D real array.
        assert isinstance(psyir_args[i].datatype, UnsupportedFortranType)
        assert isinstance(psyir_args[i].datatype.partial_datatype,
                          ArrayType)
        assert (psyir_args[i].datatype.partial_datatype.intrinsic ==
                ScalarType.Intrinsic.REAL)

    # Test all 3D real arrays:
    assert isinstance(psyir_args[4].datatype, UnsupportedFortranType)
    assert (psyir_args[4].datatype.partial_datatype.intrinsic ==
            ScalarType.Intrinsic.REAL)
    assert len(psyir_args[4].datatype.partial_datatype.shape) == 3

    # Test all 1D integer arrays:
    int_1d = dummy_sym_tab.find_or_create_array("doesnt_matter1dint", 1,
                                                ScalarType.Intrinsic.INTEGER)
    for i in [15, 19]:
        assert psyir_args[i].datatype == int_1d.datatype

    # Test all 2D integer arrays:
    int_2d = dummy_sym_tab.find_or_create_array("doesnt_matter2dint", 2,
                                                ScalarType.Intrinsic.INTEGER)
    for i in [14, 18]:
        assert psyir_args[i].symbol.datatype == int_2d.datatype


def test_ref_element_handling(fortran_writer):
    '''Test the handling of the reference element.
    '''
    psy, _ = get_invoke("23.5_ref_elem_mixed_prec.f90", TEST_API,
                        dist_mem=False, idx=0)

    schedule = psy.invokes.invoke_list[0].schedule
    create_arg_list = KernCallArgList(schedule.kernels()[0])
    vai = VariablesAccessInfo()
    create_arg_list.generate(vai)

    assert (create_arg_list._arglist == [
        'nlayers', 'f1_data', 'ndf_w1', 'undf_w1', 'map_w1(:,cell)',
        'nfaces_re_h', 'nfaces_re_v', 'normals_to_horiz_faces',
        'normals_to_vert_faces'])

    assert ("cell: READ, f1_data: READ+WRITE, map_w1: READ, ndf_w1: READ, "
            "nfaces_re_h: READ, nfaces_re_v: READ, nlayers: READ, "
            "normals_to_horiz_faces: READ, normals_to_vert_faces: READ, "
            "undf_w1: READ" == str(vai))

    check_psyir_results(create_arg_list, fortran_writer)

    psyir_args = create_arg_list.psyir_arglist
    sym_tab = schedule.symbol_table
    for ref in psyir_args:
        assert ref.symbol.name in sym_tab

    for i in [0, 2, 3, 5, 6]:
        assert (psyir_args[i].symbol.datatype ==
                LFRicTypes("LFRicIntegerScalarDataType")())

    # Test the 1d real array, which is of type r_solver
    # The datatype of a field  reference is the type of the member
    # accessed, i.e. it's the 1D real array.
    arg = psyir_args[1]
    assert isinstance(arg.datatype, UnsupportedFortranType)
    assert isinstance(arg.datatype.partial_datatype,
                      ArrayType)
    assert len(arg.datatype.partial_datatype.shape) == 1
    assert arg.datatype.partial_datatype.intrinsic == ScalarType.Intrinsic.REAL
    assert arg.datatype.partial_datatype.precision.name == "r_solver"
    # TODO #2022: it would be convenient if find_or_create_array could
    # create an r_solver based array, then the above tests would just (
    # once #744 is sorted out) be:
    # assert psyir_args[i].datatype == r_solver_1d.datatype

    # Create a dummy LFRic symbol table to simplify creating
    # standard LFRic types:
    dummy_sym_tab = LFRicSymbolTable()
    # Test all 2D integer arrays:
    int_2d = dummy_sym_tab.find_or_create_array("doesnt_matter2dint", 2,
                                                ScalarType.Intrinsic.INTEGER)
    for i in [4]:
        assert psyir_args[i].symbol.datatype == int_2d.datatype

    int_arr_2d = dummy_sym_tab.find_or_create_array("doesnt_matter2dreal", 2,
                                                    ScalarType.Intrinsic.REAL)
    for i in [7, 8]:
        assert psyir_args[i].symbol.datatype == int_arr_2d.datatype
