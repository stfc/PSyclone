# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2021, Science and Technology Facilities Council
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic, Met Office
# Modified by J. Henrichs, Bureau of Meteorology

''' This module contains tests for the multi-grid part of the Dynamo 0.3 API
    using pytest. '''

from __future__ import absolute_import, print_function
# Since this is a file containing tests which often have to get in and
# change the internal state of objects we disable pylint's warning
# about such accesses

import os
import pytest
import fparser

from fparser import api as fpapi
from psyclone.configuration import Config
from psyclone.domain.lfric import LFRicConstants
from psyclone.dynamo0p3 import DynHaloExchange, DynKernMetadata, HaloReadAccess
from psyclone.errors import GenerationError, InternalError
from psyclone.gen_kernel_stub import generate
from psyclone.parse.algorithm import parse
from psyclone.parse.utils import ParseError
from psyclone.psyGen import PSyFactory
from psyclone.psyir.nodes import Node
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.transformations import check_intergrid, Dynamo0p3ColourTrans, \
        DynamoOMPParallelLoopTrans, TransformationError

# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")

API = "dynamo0.3"

RESTRICT_MDATA = '''
module restrict_mod
type, public, extends(kernel_type) :: restrict_kernel_type
   private
   type(arg_type) :: meta_args(2) = (/                                        &
       arg_type(GH_FIELD, GH_REAL, GH_INC,  ANY_SPACE_1, mesh_arg=GH_COARSE), &
       arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_SPACE_2, mesh_arg=GH_FINE  )  &
       /)
  integer :: operates_on = cell_column
contains
  procedure, nopass :: restrict_kernel_code
end type restrict_kernel_type
contains
  subroutine restrict_kernel_code()
  end subroutine restrict_kernel_code
end module restrict_mod
'''


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use dynamo0.3 as API.'''
    Config.get().api = "dynamo0.3"
    yield()
    Config._instance = None


def test_check_intergrid():
    ''' Test that the check_intergrid utility does not raise an error if the
    supplied node has no children. '''
    tnode = Node()
    check_intergrid(tnode)


def test_invalid_mesh_type():
    ''' Check that we raise an error if an unrecognised name is supplied
    for the mesh associated with a field argument '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = RESTRICT_MDATA.replace("GH_COARSE", "GH_RUBBISH", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "restrict_kernel_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("mesh_arg must be one of ['gh_coarse', "
            "'gh_fine'] but got gh_rubbish" in str(excinfo.value))


def test_invalid_mesh_specifier():
    ''' Check that we raise an error if "mesh_arg" is mis-spelt '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = RESTRICT_MDATA.replace("mesh_arg=GH_COARSE",
                                  "mesh_ar=GH_COARSE", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "restrict_kernel_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("mesh_ar=gh_coarse is not a valid mesh identifier" in
            str(excinfo.value))


def test_all_args_same_mesh_error():
    ''' Check that we reject a kernel if all arguments are specified
    as being on the same mesh (coarse or fine). '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    # Both on fine mesh
    code = RESTRICT_MDATA.replace("GH_COARSE", "GH_FINE", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "restrict_kernel_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    const = LFRicConstants()
    assert ("Inter-grid kernels in the Dynamo 0.3 API must have at least "
            "one field argument on each of the mesh types ({0}). However, "
            "kernel restrict_kernel_type has arguments only on ['gh_fine']".
            format(const.VALID_MESH_TYPES) in str(excinfo.value))
    # Both on coarse mesh
    code = RESTRICT_MDATA.replace("GH_FINE", "GH_COARSE", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("Inter-grid kernels in the Dynamo 0.3 API must have at least "
            "one field argument on each of the mesh types ({0}). However, "
            "kernel restrict_kernel_type has arguments only on ['gh_coarse']".
            format(const.VALID_MESH_TYPES) in str(excinfo.value))


def test_all_fields_have_mesh():
    ''' Check that we reject an inter-grid kernel if any of its field
    arguments are missing a mesh specifier '''
    # Add a field argument that is missing a mesh_arg specifier
    code = RESTRICT_MDATA.replace(
        "       arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_SPACE_2, "
        "mesh_arg=GH_FINE  )  &",
        "       arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_SPACE_2, "
        "mesh_arg=GH_FINE  ), &\n"
        "       arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_SPACE_2) &\n", 1)
    code = code.replace("(2)", "(3)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "restrict_kernel_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("Inter-grid kernels in the Dynamo 0.3 API must specify which "
            "mesh each field argument "
            "is on but kernel restrict_kernel_type has at least one field "
            "argument for which mesh_arg is missing." in str(excinfo.value))


def test_args_same_space_error():
    ''' Check that we reject a kernel if arguments on different meshes
    are specified as being on the same function space '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = RESTRICT_MDATA.replace("ANY_SPACE", "ANY_DISCONTINUOUS_SPACE")
    code = code.replace("_2", "_1")
    code = code.replace("GH_INC", "GH_READWRITE")
    ast = fpapi.parse(code, ignore_comments=False)
    name = "restrict_kernel_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("inter-grid kernels must be on different function spaces if "
            "they are on different meshes. However kernel "
            "restrict_kernel_type has a field on function space(s)"
            " ['any_discontinuous_space_1'] on each of the mesh types"
            " ['gh_coarse', 'gh_fine']." in str(excinfo.value))


def test_only_field_args():
    ''' Check that we reject an inter-grid kernel if it has any arguments
    that are not fields '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    # Add a scalar argument to the kernel
    code = RESTRICT_MDATA.replace(
        "       arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_SPACE_2, "
        "mesh_arg=GH_FINE  )  &",
        "       arg_type(GH_FIELD, GH_REAL, GH_READ, ANY_SPACE_2, "
        "mesh_arg=GH_FINE  ),  &\n"
        "       arg_type(GH_SCALAR, GH_REAL, GH_READ) &", 1)
    code = code.replace("(2)", "(3)", 1)

    ast = fpapi.parse(code, ignore_comments=False)
    name = "restrict_kernel_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("Inter-grid kernels in the Dynamo 0.3 API are only permitted to "
            "have field arguments but kernel restrict_kernel_type also has "
            "arguments of type ['gh_scalar']" in str(excinfo.value))


def test_field_vector():
    ''' Check that we accept an inter-grid kernel with field-vector
    arguments '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    # Change both of the arguments to be vectors
    code = RESTRICT_MDATA.replace("GH_FIELD,", "GH_FIELD*2,", 2)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "restrict_kernel_type"
    dkm = DynKernMetadata(ast, name=name)
    for arg in dkm.arg_descriptors:
        assert arg.vector_size == 2
    # Change only one of the arguments to be a vector
    code = RESTRICT_MDATA.replace("GH_FIELD,", "GH_FIELD*3,", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    dkm = DynKernMetadata(ast, name=name)
    assert dkm.arg_descriptors[0].vector_size == 3
    assert dkm.arg_descriptors[1].vector_size == 1


def test_two_grid_types(monkeypatch):
    ''' Check that PSyclone raises an error if the number of grid types
    supported for inter-grid kernels is not two. '''
    # Change LFRicConstants.VALID_MESH_TYPES so that it contains
    # three values
    monkeypatch.setattr(
        target=LFRicConstants, name="VALID_MESH_TYPES",
        value=["gh_coarse", "gh_fine", "gh_medium"])
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(RESTRICT_MDATA, ignore_comments=False)
    name = "restrict_kernel_type"
    with pytest.raises(InternalError) as err:
        _ = DynKernMetadata(ast, name=name)
    assert ("The implementation of inter-grid support in the LFRic "
            "API assumes there are exactly two mesh types but "
            "LFRicConstants.VALID_MESH_TYPES contains 3: "
            "['gh_coarse', 'gh_fine', 'gh_medium']" in str(err.value))


def test_field_prolong(tmpdir):
    ''' Check that we generate correct psy-layer code for an invoke
    containing a kernel that performs a prolongation operation '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "22.0_intergrid_prolong.f90"),
                           api=API)
    for distmem in [False, True]:
        psy = PSyFactory(API, distributed_memory=distmem).create(invoke_info)
        gen_code = str(psy.gen)

        assert LFRicBuild(tmpdir).code_compiles(psy)

        expected = (
            "      USE prolong_test_kernel_mod, "
            "ONLY: prolong_test_kernel_code\n"
            "      USE mesh_map_mod, ONLY: mesh_map_type\n"
            "      USE mesh_mod, ONLY: mesh_type\n"
            "      TYPE(field_type), intent(in) :: field1, field2\n"
            "      INTEGER(KIND=i_def) cell\n")
        assert expected in gen_code

        expected = (
            "      INTEGER(KIND=i_def) ncell_field1, ncpc_field1_field2_x, "
            "ncpc_field1_field2_y\n"
            "      INTEGER(KIND=i_def), pointer :: "
            "cell_map_field2(:,:,:) => null()\n"
            "      TYPE(mesh_map_type), pointer :: "
            "mmap_field1_field2 => null()\n"
            "      TYPE(mesh_type), pointer :: mesh_field2 => null()\n"
            "      TYPE(mesh_type), pointer :: mesh_field1 => null()\n")
        assert expected in gen_code

        expected = (
            "      ! Look-up mesh objects and loop limits for inter-grid "
            "kernels\n"
            "      !\n"
            "      mesh_field1 => field1_proxy%vspace%get_mesh()\n"
            "      mesh_field2 => field2_proxy%vspace%get_mesh()\n"
            "      mmap_field1_field2 => mesh_field2%get_mesh_map"
            "(mesh_field1)\n"
            "      cell_map_field2 => mmap_field1_field2%"
            "get_whole_cell_map()\n")
        if distmem:
            expected += (
                "      ncell_field1 = mesh_field1%get_last_halo_cell("
                "depth=2)\n")
        else:
            expected += \
                "      ncell_field1 = field1_proxy%vspace%get_ncell()\n"
        expected += (
            "      ncpc_field1_field2_x = mmap_field1_field2%"
            "get_ntarget_cells_per_source_x()\n"
            "      ncpc_field1_field2_y = mmap_field1_field2%"
            "get_ntarget_cells_per_source_y()\n")
        assert expected in gen_code

        if distmem:
            # We are writing to a continuous field on the fine mesh, we
            # only need to halo swap to depth one on the coarse.
            expected = (
                "      IF (field2_proxy%is_dirty(depth=1)) THEN\n"
                "        CALL field2_proxy%halo_exchange(depth=1)\n"
                "      END IF\n"
                "      !\n"
                "      DO cell=1,mesh_field2%get_last_halo_cell(1)\n")
            assert expected in gen_code
        else:
            assert "DO cell=1,field2_proxy%vspace%get_ncell()\n" in gen_code

        expected = (
            "        CALL prolong_test_kernel_code(nlayers, "
            "cell_map_field2(:,:,cell), ncpc_field1_field2_x, "
            "ncpc_field1_field2_y, ncell_field1, field1_proxy%data, "
            "field2_proxy%data, ndf_w1, undf_w1, map_w1, undf_w2, "
            "map_w2(:,cell))\n"
            "      END DO\n")
        assert expected in gen_code

        if distmem:
            set_dirty = "      CALL field1_proxy%set_dirty()\n"
            assert set_dirty in gen_code


def test_field_restrict(tmpdir, dist_mem, monkeypatch, annexed):
    ''' Test that we generate correct code for an invoke containing a
    single restriction operation (read from fine, write to
    coarse). Check when annexed is False and True as we produce a
    different number of halo exchanges.

    '''

    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)

    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "22.1_intergrid_restrict.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    output = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    defs = (
        "      USE restrict_test_kernel_mod, "
        "ONLY: restrict_test_kernel_code\n"
        "      USE mesh_map_mod, ONLY: mesh_map_type\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      TYPE(field_type), intent(in) :: field1, field2\n")
    assert defs in output

    defs2 = (
        "      INTEGER(KIND=i_def) nlayers\n"
        "      TYPE(field_proxy_type) field1_proxy, field2_proxy\n"
        "      INTEGER(KIND=i_def), pointer :: "
        "map_aspc1_field1(:,:) => null(), map_aspc2_field2(:,:) => null()\n"
        "      INTEGER(KIND=i_def) ndf_aspc1_field1, undf_aspc1_field1, "
        "ndf_aspc2_field2, undf_aspc2_field2\n"
        "      INTEGER(KIND=i_def) ncell_field2, ncpc_field2_field1_x, "
        "ncpc_field2_field1_y\n"
        "      INTEGER(KIND=i_def), pointer :: "
        "cell_map_field1(:,:,:) => null()\n"
        "      TYPE(mesh_map_type), pointer :: mmap_field2_field1 => "
        "null()\n"
        "      TYPE(mesh_type), pointer :: mesh_field2 => null()\n"
        "      TYPE(mesh_type), pointer :: mesh_field1 => null()\n")
    assert defs2 in output

    inits = (
        "      !\n"
        "      ! Look-up mesh objects and loop limits for inter-grid kernels\n"
        "      !\n"
        "      mesh_field2 => field2_proxy%vspace%get_mesh()\n"
        "      mesh_field1 => field1_proxy%vspace%get_mesh()\n"
        "      mmap_field2_field1 => mesh_field1%get_mesh_map(mesh_field2)\n"
        "      cell_map_field1 => mmap_field2_field1%get_whole_cell_map()\n")
    if dist_mem:
        inits += ("      ncell_field2 = mesh_field2%"
                  "get_last_halo_cell(depth=2)\n")
    else:
        inits += "      ncell_field2 = field2_proxy%vspace%get_ncell()\n"
    inits += (
        "      ncpc_field2_field1_x = mmap_field2_field1%"
        "get_ntarget_cells_per_source_x()\n"
        "      ncpc_field2_field1_y = mmap_field2_field1%"
        "get_ntarget_cells_per_source_y()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_aspc1_field1 => field1_proxy%vspace%get_whole_dofmap()\n"
        "      map_aspc2_field2 => field2_proxy%vspace%get_whole_dofmap()\n")
    assert inits in output

    if dist_mem:
        # We write out to the L1 halo on the coarse mesh which means
        # we require up-to-date values out to the L2 halo on the fine.
        # Since we are incrementing the coarse field we also need
        # up-to-date values for it in the L1 halo.
        if not annexed:
            halo_exchs = (
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      IF (field1_proxy%is_dirty(depth=1)) THEN\n"
                "        CALL field1_proxy%halo_exchange(depth=1)\n"
                "      END IF\n"
                "      !\n"
                "      IF (field2_proxy%is_dirty(depth=2)) THEN\n"
                "        CALL field2_proxy%halo_exchange(depth=2)\n"
                "      END IF\n"
                "      !\n"
                "      DO cell=1,mesh_field1%get_last_halo_cell(1)\n")
        else:
            halo_exchs = (
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      IF (field2_proxy%is_dirty(depth=2)) THEN\n"
                "        CALL field2_proxy%halo_exchange(depth=2)\n"
                "      END IF\n"
                "      !\n"
                "      DO cell=1,mesh_field1%get_last_halo_cell(1)\n")
        assert halo_exchs in output

    # We pass the whole dofmap for the fine mesh (we are reading from).
    # This is associated with the second kernel argument.
    kern_call = (
        "        !\n"
        "        CALL restrict_test_kernel_code(nlayers, "
        "cell_map_field1(:,:,cell), ncpc_field2_field1_x, "
        "ncpc_field2_field1_y, ncell_field2, "
        "field1_proxy%data, field2_proxy%data, undf_aspc1_field1, "
        "map_aspc1_field1(:,cell), ndf_aspc2_field2, undf_aspc2_field2, "
        "map_aspc2_field2)\n"
        "      END DO\n"
        "      !\n")
    assert kern_call in output

    if dist_mem:
        set_dirty = "      CALL field1_proxy%set_dirty()\n"
        assert set_dirty in output


def test_restrict_prolong_chain(tmpdir, dist_mem):
    ''' Test when we have a single invoke containing a chain of
    restrictions and prolongations.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "22.2_intergrid_3levels.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    output = str(psy.gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)
    expected = (
        "      ! Look-up mesh objects and loop limits for inter-grid "
        "kernels\n"
        "      !\n"
        "      mesh_fld_f => fld_f_proxy%vspace%get_mesh()\n"
        "      mesh_fld_m => fld_m_proxy%vspace%get_mesh()\n"
        "      mmap_fld_f_fld_m => mesh_fld_m%get_mesh_map(mesh_fld_f)\n"
        "      cell_map_fld_m => mmap_fld_f_fld_m%get_whole_cell_map()\n")

    assert expected in output

    if dist_mem:
        expected = (
            "      ncell_fld_f = mesh_fld_f%get_last_halo_cell(depth=2)\n"
            "      ncpc_fld_f_fld_m_x = mmap_fld_f_fld_m%"
            "get_ntarget_cells_per_source_x()\n"
            "      ncpc_fld_f_fld_m_y = mmap_fld_f_fld_m%"
            "get_ntarget_cells_per_source_y()\n"
            "      mesh_fld_c => fld_c_proxy%vspace%get_mesh()\n"
            "      mmap_fld_m_fld_c => mesh_fld_c%get_mesh_map(mesh_fld_m)\n"
            "      cell_map_fld_c => mmap_fld_m_fld_c%get_whole_cell_map()\n"
            "      ncell_fld_m = mesh_fld_m%get_last_halo_cell(depth=2)\n"
            "      ncpc_fld_m_fld_c_x = mmap_fld_m_fld_c%"
            "get_ntarget_cells_per_source_x()\n"
            "      ncpc_fld_m_fld_c_y = mmap_fld_m_fld_c%"
            "get_ntarget_cells_per_source_y()\n")
    else:
        expected = (
            "      ncell_fld_f = fld_f_proxy%vspace%get_ncell()\n"
            "      ncpc_fld_f_fld_m_x = mmap_fld_f_fld_m%"
            "get_ntarget_cells_per_source_x()\n"
            "      ncpc_fld_f_fld_m_y = mmap_fld_f_fld_m%"
            "get_ntarget_cells_per_source_y()\n"
            "      mesh_fld_c => fld_c_proxy%vspace%get_mesh()\n"
            "      mmap_fld_m_fld_c => mesh_fld_c%get_mesh_map(mesh_fld_m)\n"
            "      cell_map_fld_c => mmap_fld_m_fld_c%get_whole_cell_map()\n"
            "      ncell_fld_m = fld_m_proxy%vspace%get_ncell()\n"
            "      ncpc_fld_m_fld_c_x = mmap_fld_m_fld_c%get_ntarget_cells_"
            "per_source_x()\n"
            "      ncpc_fld_m_fld_c_y = mmap_fld_m_fld_c%get_ntarget_cells_"
            "per_source_y()\n")
    assert expected in output

    # Check that we haven't got duplicated output
    assert output.count("mesh_fld_m => fld_m_proxy%vspace%get_mesh") == 1
    assert output.count("ncell_fld_m = ") == 1
    assert output.count("ncell_fld_f = ") == 1

    if dist_mem:
        # Have two potential halo exchanges before 1st prolong because
        # of continuous "read"er and "inc" writer
        expected = (
            "      IF (fld_m_proxy%is_dirty(depth=1)) THEN\n"
            "        CALL fld_m_proxy%halo_exchange(depth=1)\n"
            "      END IF\n"
            "      !\n"
            "      IF (fld_c_proxy%is_dirty(depth=1)) THEN\n"
            "        CALL fld_c_proxy%halo_exchange(depth=1)\n"
            "      END IF\n"
            "      !\n"
            "      DO cell=1,mesh_fld_c%get_last_halo_cell(1)\n")
        assert expected in output
        # Since we loop into L1 halo of the coarse mesh, the L1 halo
        # of the fine(r) mesh will now be clean. Therefore, no halo
        # swap before the next prolongation required for fld_m
        expected = (
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL fld_m_proxy%set_dirty()\n"
            "      CALL fld_m_proxy%set_clean(1)\n"
            "      !\n"
            "      IF (fld_f_proxy%is_dirty(depth=1)) THEN\n"
            "        CALL fld_f_proxy%halo_exchange(depth=1)\n"
            "      END IF\n"
            "      !\n"
            "      DO cell=1,mesh_fld_m%get_last_halo_cell(1)\n")
        assert expected in output
        # Again the L1 halo for fld_f will now be clean but for restriction
        # we need the L2 halo to be clean. There's a set_clean(1) for
        # fld_f because the above loop over the coarser fld_m will go
        # into the L2 halo of fld_f. However, it is a continuous field
        # so only the L1 halo will actually be clean.
        expected = ("      CALL fld_f_proxy%set_dirty()\n"
                    "      CALL fld_f_proxy%set_clean(1)\n"
                    "      !\n"
                    "      CALL fld_f_proxy%halo_exchange(depth=2)\n"
                    "      !\n"
                    "      DO cell=1,mesh_fld_m%get_last_halo_cell(1)\n"
                    "        !\n"
                    "        CALL restrict_test_kernel_code")
        assert expected in output
        # For the final restriction we need the L2 halo of fld_m to be
        # clean. There's no set_clean() call on fld_m because it is
        # only updated out to the L1 halo and it is a continuous field
        # so the shared dofs in the L1 halo will still be dirty.
        expected = ("      CALL fld_m_proxy%set_dirty()\n"
                    "      !\n"
                    "      CALL fld_m_proxy%halo_exchange(depth=2)\n"
                    "      !\n"
                    "      DO cell=1,mesh_fld_c%get_last_halo_cell(1)\n"
                    "        !\n"
                    "        CALL restrict_test_kernel_code")
        assert expected in output
    else:
        expected = (
            "      DO cell=1,fld_c_proxy%vspace%get_ncell()\n"
            "        !\n"
            "        CALL prolong_test_kernel_code(nlayers, cell_map_fld_c"
            "(:,:,cell), ncpc_fld_m_fld_c_x, ncpc_fld_m_fld_c_y, ncell_fld_m, "
            "fld_m_proxy%data, fld_c_proxy%data, ndf_w1, undf_w1, map_w1, "
            "undf_w2, map_w2(:,cell))\n"
            "      END DO\n"
            "      DO cell=1,fld_m_proxy%vspace%get_ncell()\n"
            "        !\n"
            "        CALL prolong_test_kernel_code(nlayers, cell_map_fld_m"
            "(:,:,cell), ncpc_fld_f_fld_m_x, ncpc_fld_f_fld_m_y, ncell_fld_f, "
            "fld_f_proxy%data, fld_m_proxy%data, ndf_w1, undf_w1, map_w1, "
            "undf_w2, map_w2(:,cell))\n"
            "      END DO\n"
            "      DO cell=1,fld_m_proxy%vspace%get_ncell()\n"
            "        !\n"
            "        CALL restrict_test_kernel_code(nlayers, cell_map_fld_m"
            "(:,:,cell), ncpc_fld_f_fld_m_x, ncpc_fld_f_fld_m_y, ncell_fld_f, "
            "fld_m_proxy%data, fld_f_proxy%data, undf_aspc1_fld_m, "
            "map_aspc1_fld_m(:,cell), ndf_aspc2_fld_f, undf_aspc2_fld_f, "
            "map_aspc2_fld_f)\n"
            "      END DO\n"
            "      DO cell=1,fld_c_proxy%vspace%get_ncell()\n"
            "        !\n"
            "        CALL restrict_test_kernel_code(nlayers, cell_map_fld_c"
            "(:,:,cell), ncpc_fld_m_fld_c_x, ncpc_fld_m_fld_c_y, ncell_fld_m, "
            "fld_c_proxy%data, fld_m_proxy%data, undf_aspc1_fld_c, "
            "map_aspc1_fld_c(:,cell), ndf_aspc2_fld_m, undf_aspc2_fld_m, "
            "map_aspc2_fld_m)\n")
        assert expected in output


def test_fine_halo_read():
    ''' Check that the halo exchange has double the depth if it is
    for a field on the fine mesh with a read dependence '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "22.2_intergrid_3levels.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    hexch = schedule.children[5]
    assert isinstance(hexch, DynHaloExchange)
    assert hexch._compute_halo_depth() == '2'
    call = schedule.children[6]
    field = call.args[1]
    hra = HaloReadAccess(field)
    assert hra._var_depth is None
    # Change the internal state of the HaloReadAccess to mimic the case
    # where the field in question has a stencil access with a variable depth
    hra._var_depth = "my_depth"
    hra._compute_from_field(field)
    assert hra._var_depth == "2*my_depth"


def test_prolong_with_gp_error():
    ''' Check that we reject an invoke that contains both
    an inter-grid and a general-purpose kernel '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "22.3_intergrid_plus_general.f90"),
                           api=API)
    with pytest.raises(GenerationError) as err:
        _ = PSyFactory(API).create(invoke_info)
    assert ("no other kernel types but kernels 'testkern_w2_only_code' in "
            "invoke 'invoke_0' are not inter-grid" in str(err.value))


def test_prolong_vector(tmpdir):
    ''' Check that we generate correct code when an inter-grid kernel
    takes a field vector as argument '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "22.4_intergrid_prolong_vec.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)
    output = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert "TYPE(field_type), intent(in) :: field1(3)" in output
    assert "TYPE(field_proxy_type) field1_proxy(3)" in output
    # Make sure we always index into the field arrays
    assert " field1%" not in output
    assert " field2%" not in output
    assert ("ncpc_field1_field2_x, ncpc_field1_field2_y, ncell_field1, "
            "field1_proxy(1)%data, field1_proxy(2)%data, field1_proxy(3)%data,"
            " field2_proxy(1)%data, field2_proxy(2)%data, "
            "field2_proxy(3)%data, ndf_w1" in output)
    for idx in [1, 2, 3]:
        assert (
            "      IF (field2_proxy({0})%is_dirty(depth=1)) THEN\n"
            "        CALL field2_proxy({0})%halo_exchange(depth=1)\n"
            "      END IF\n".format(idx) in output)
        assert ("field1_proxy({0}) = field1({0})%get_proxy()".format(idx) in
                output)
        assert "CALL field1_proxy({0})%set_dirty()".format(idx) in output
        assert "CALL field1_proxy({0})%set_clean(1)".format(idx) in output


def test_no_stub_gen():
    ''' Check that the kernel-stub generator refuses to attempt to create
    a kernel stub if the meta-data contains mesh information. '''
    with pytest.raises(NotImplementedError) as excinfo:
        generate(os.path.join(BASE_PATH, "prolong_test_kernel_mod.f90"),
                 api="dynamo0.3")
    assert ('prolong_test_kernel_code is an inter-grid kernel and stub '
            'generation is not yet supported for inter-grid kernels'
            in str(excinfo.value))


def test_restrict_prolong_chain_anyd(tmpdir):
    ''' Test that we generate correct code for an invoke containing a
    chain of discontinuous restrictions and continuous prolongations.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "22.2.1_intergrid_3levels_anyd.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    output = str(psy.gen)
    # Check maps for any_discontinuous_space
    expected = (
        "      map_adspc1_fld_m => fld_m_proxy%vspace%get_whole_dofmap()\n"
        "      map_adspc2_fld_f => fld_f_proxy%vspace%get_whole_dofmap()\n"
        "      map_adspc1_fld_c => fld_c_proxy%vspace%get_whole_dofmap()\n"
        "      map_adspc2_fld_m => fld_m_proxy%vspace%get_whole_dofmap()\n")
    assert expected in output
    # Check ndf and undf initialisations the second restrict kernel
    # (fld_m to fld_c)
    expected = (
        "      ! Initialise number of DoFs for adspc1_fld_c\n"
        "      !\n"
        "      ndf_adspc1_fld_c = fld_c_proxy%vspace%get_ndf()\n"
        "      undf_adspc1_fld_c = fld_c_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for adspc2_fld_m\n"
        "      !\n"
        "      ndf_adspc2_fld_m = fld_m_proxy%vspace%get_ndf()\n"
        "      undf_adspc2_fld_m = fld_m_proxy%vspace%get_undf()\n")
    assert expected in output
    # Check an example of restrict loop and all upper loop bounds
    expected = (
        "      ! Call kernels and communication routines\n"
        "      !\n"
        "      DO cell=1,mesh_fld_m%get_last_edge_cell()\n"
        "        !\n"
        "        CALL restrict_kernel_code(nlayers, cell_map_fld_m(:,:,cell), "
        "ncpc_fld_f_fld_m_x, ncpc_fld_f_fld_m_y, ncell_fld_f, "
        "fld_m_proxy%data, fld_f_proxy%data, undf_adspc1_fld_m, "
        "map_adspc1_fld_m(:,cell), ndf_adspc2_fld_f, "
        "undf_adspc2_fld_f, map_adspc2_fld_f)\n"
        "      END DO\n")
    assert expected in output
    assert "DO cell=1,mesh_fld_c%get_last_edge_cell()" in output
    assert "DO cell=1,mesh_fld_c%get_last_halo_cell(1)" in output
    assert "DO cell=1,mesh_fld_m%get_last_halo_cell(1)" in output
    # Check compilation
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Now do some transformations
    otrans = DynamoOMPParallelLoopTrans()
    ctrans = Dynamo0p3ColourTrans()
    # Apply OMP to the first restrict kernel
    _, _ = otrans.apply(schedule.children[0])
    # Apply colouring and OMP to the first prolong kernel
    _, _ = ctrans.apply(schedule.children[4])
    _, _ = otrans.apply(schedule.children[4].loop_body[0])
    output = str(psy.gen)
    expected = (
        "      !$omp parallel do default(shared), private(cell), "
        "schedule(static)\n"
        "      DO cell=1,mesh_fld_m%get_last_edge_cell()\n"
        "        !\n"
        "        CALL restrict_kernel_code")
    assert expected in output
    expected = (
        "      DO colour=1,ncolour_fld_m\n"
        "        !$omp parallel do default(shared), private(cell), "
        "schedule(static)\n"
        "        DO cell=1,mesh_fld_c%get_last_halo_cell_per_colour"
        "(colour,1)\n"
        "          !\n"
        "          CALL prolong_test_kernel_code")
    assert expected in output
    # Try to apply colouring to the second restrict kernel
    with pytest.raises(TransformationError) as excinfo:
        _, _ = ctrans.apply(schedule.children[1])
    assert ("Loops iterating over a discontinuous function space "
            "are not currently supported." in str(excinfo.value))
