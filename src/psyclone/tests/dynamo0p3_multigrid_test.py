# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017, Science and Technology Facilities Council
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

''' This module contains tests for the multi-grid part of the Dynamo 0.3 API
    using pytest. '''

from __future__ import absolute_import
# Since this is a file containing tests which often have to get in and
# change the internal state of objects we disable pylint's warning
# about such accesses
# pylint: disable=protected-access

import os
import pytest
import fparser
import utils
from fparser import api as fpapi
from psyclone.dynamo0p3 import DynKernMetadata
from psyclone.parse import ParseError, parse
from psyclone.psyGen import PSyFactory

# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")

API = "dynamo0.3"

RESTRICT_MDATA = '''
module restrict_mod
type, public, extends(kernel_type) :: restrict_kernel_type
   private
   type(arg_type) :: meta_args(2) = (/                                 &
       arg_type(GH_FIELD, GH_INC, ANY_SPACE_1, mesh_arg=GH_COARSE),    &
       arg_type(GH_FIELD, GH_READ,  ANY_SPACE_2, mesh_arg=GH_FINE   )  &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass :: restrict_kernel_code
end type restrict_kernel_type
contains
  subroutine restrict_kernel_code()
  end subroutine restrict_kernel_code
end module restrict_mod
'''


def test_invalid_mesh_type():
    ''' Check that we raise an error if an unrecognised name is supplied
    for the mesh associated with a field argument '''
    fparser.logging.disable('CRITICAL')
    code = RESTRICT_MDATA.replace("GH_COARSE", "GH_RUBBISH", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "restrict_kernel_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    print str(excinfo)
    assert ("mesh_arg must be one of [\\'gh_coarse\\', "
            "\\'gh_fine\\'] but got gh_rubbish" in str(excinfo))


def test_invalid_mesh_specifier():
    ''' Check that we raise an error if "mesh_arg" is mis-spelt '''
    fparser.logging.disable('CRITICAL')
    code = RESTRICT_MDATA.replace("mesh_arg=GH_COARSE",
                                  "mesh_ar=GH_COARSE", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "restrict_kernel_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    print str(excinfo)
    assert ("mesh_ar=gh_coarse is not a valid mesh identifier" in
            str(excinfo))


def test_all_args_same_mesh_error():
    ''' Check that we reject a kernel if all arguments are specified
    as being on the same mesh (coarse or fine) '''
    fparser.logging.disable('CRITICAL')
    # Both on fine mesh
    code = RESTRICT_MDATA.replace("GH_COARSE", "GH_FINE", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "restrict_kernel_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("Inter-grid kernels in the Dynamo 0.3 API must have at least "
            "one field argument on each of the mesh types (['gh_coarse', "
            "'gh_fine']). However, kernel restrict_kernel_type has arguments "
            "only on ['gh_fine']" in str(excinfo))
    # Both on coarse mesh
    code = RESTRICT_MDATA.replace("GH_FINE", "GH_COARSE", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("Inter-grid kernels in the Dynamo 0.3 API must have at least "
            "one field argument on each of the mesh types (['gh_coarse', "
            "'gh_fine']). However, kernel restrict_kernel_type has arguments "
            "only on ['gh_coarse']" in str(excinfo))


def test_all_fields_have_mesh():
    ''' Check that we reject an inter-grid kernel if any of its field
    arguments are missing a mesh specifier '''
    # Add a field argument that is missing a mesh_arg specifier
    code = RESTRICT_MDATA.replace(
        "       arg_type(GH_FIELD, GH_READ,  ANY_SPACE_2, "
        "mesh_arg=GH_FINE   )  &",
        "       arg_type(GH_FIELD, GH_READ,  ANY_SPACE_2, "
        "mesh_arg=GH_FINE   ), &\n"
        "       arg_type(GH_FIELD, GH_READ,  ANY_SPACE_2) &\n", 1)
    code = code.replace("(2)", "(3)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "restrict_kernel_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("Inter-grid kernels in the Dynamo 0.3 API must specify which "
            "mesh each field argument "
            "is on but kernel restrict_kernel_type has at least one field "
            "argument for which mesh_arg is missing." in str(excinfo))


def test_args_same_space_error():
    ''' Check that we reject a kernel if arguments on different meshes
    are specified as being on the same function space '''
    fparser.logging.disable('CRITICAL')
    code = RESTRICT_MDATA.replace("ANY_SPACE_2", "ANY_SPACE_1", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "restrict_kernel_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("inter-grid kernels must be on different function spaces if they "
            "are on different meshes. However kernel restrict_kernel_type "
            "has a field on function space(s) ['any_space_1'] on each of the "
            "mesh types ['gh_coarse', 'gh_fine']." in str(excinfo))


def test_only_field_args():
    ''' Check that we reject an inter-grid kernel if it has any arguments
    that are not fields '''
    fparser.logging.disable('CRITICAL')
    # Add a scalar argument to the kernel
    code = RESTRICT_MDATA.replace(
        "       arg_type(GH_FIELD, GH_READ,  ANY_SPACE_2, "
        "mesh_arg=GH_FINE   )  &",
        "       arg_type(GH_FIELD, GH_READ,  ANY_SPACE_2, "
        "mesh_arg=GH_FINE   ), &\n"
        "       arg_type(GH_REAL, GH_READ) &", 1)
    code = code.replace("(2)", "(3)", 1)
    print code
    ast = fpapi.parse(code, ignore_comments=False)
    name = "restrict_kernel_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("Inter-grid kernels in the Dynamo 0.3 API are only permitted to "
            "have field arguments but kernel restrict_kernel_type also has "
            "arguments of type ['gh_real']" in str(excinfo))


def test_field_vector():
    ''' Check that we accept an inter-grid kernel with field-vector
    arguments '''
    fparser.logging.disable('CRITICAL')
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


def test_field_prolong(tmpdir, f90, f90flags):
    ''' Check that we generate correct psy-layer code for an invoke
    containing a kernel that performs a prolongation operation '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "22.0_intergrid_prolong.f90"),
                           api=API)
    for distmem in [True, False]:
        psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
        gen_code = str(psy.gen)
        print gen_code

        if utils.TEST_COMPILE:
            assert utils.code_compiles(API, psy, tmpdir, f90, f90flags)

        expected = (
            "    USE prolong_kernel_mod, ONLY: prolong_kernel_code\n"
            "    TYPE(field_type), intent(inout) :: field1\n"
            "    TYPE(field_type), intent(in) :: field2\n"
            "    INTEGER cell\n"
            # We only require ndf for the fine field (on W1), not the coarse
            # field
            "    INTEGER ndf_w1, undf_w1, undf_w2\n"
            "    INTEGER nlayers\n"
            "    TYPE(field_proxy_type) field1_proxy, field2_proxy\n")
        assert expected in gen_code

        expected = (
            "    type(mesh_map_type), pointer :: mesh_map => null()\n"
            "    type(mesh_type), pointer     :: mesh => null(), mesh_f=>null()\n"
            "    integer :: ncell_f, ncell_c, nc2f, cell\n"
            "    integer, pointer :: cell_map(:,:)\n")
        assert expected in gen_code

        expected = (
            "    ! dof maps\n"
            "    integer, pointer :: dofmap_f(:,:) => null(), dofmap_c(:,:) => null()\n"
            "    integer :: nlayers, ndf, undf_f, undf_c\n"
            "   \n" 
            "\n"
            "    ! get the proxies \n"
            "    fc_fp = fc%get_proxy()\n"
            "    ff_fp = ff%get_proxy()\n"
            "\n"
            "    mesh => fc%get_mesh()\n"
            "    mesh_f => ff%get_mesh()\n"
            "    mesh_map => mesh%get_mesh_map(mesh_f)\n"
            "    nlayers = ff_fp%vspace%get_nlayers()\n"
            "    ! get the horinzontal loop counter from the fine mesh\n"
            "    ncell_f = mesh_f%get_last_halo_cell(depth=2)\n"
            "    ncell_c = mesh%get_last_halo_cell(depth=1)\n"
            "    ! get the ratio\n"
            "    nc2f = mesh_map%get_ntarget_cells_per_source_cell()\n"
            "    ! check this looks sane\n"
            "    write(*,*) \"meshes:\",ncell_f, ncell_c, nc2f\n"
            "    nlayers = ff_fp%vspace%get_nlayers()\n"
            "\n"
            "    dofmap_f => ff_fp%vspace%get_whole_dofmap()\n"
            "    dofmap_c => fc_fp%vspace%get_whole_dofmap()\n"
            "    ndf = ff_fp%vspace%get_ndf()\n"
            "    undf_f = ff_fp%vspace%get_undf()\n"
            "    undf_c = fc_fp%vspace%get_undf()\n"
            "\n"
            "    ! halo exchange to depth two on the fine \n"
            "    if (ff_fp%is_dirty(depth=2)) then\n"
            "       call ff_fp%halo_exchange(depth=2)\n"
            "    end if\n"
            "\n"
            "    ! halo exchange to depth one on the coarse\n"
            "    ! to last halo cell(1)\n"
            "    if (fc_fp%is_dirty(depth=1)) then\n"
            "       call fc_fp%halo_exchange(depth=1)\n"
            "    end if\n"
            "\n"
            "    cell_map => mesh_map%get_whole_cell_map()\n"
            "\n"
            "    do cell = 1, ncell_c\n"
            "       call prolong_kernel_code(nlayers, cell_map(:,cell), nc2f, dofmap_f, &\n"
            "            ncell_f,dofmap_c(:,cell),ndf, undf_c, undf_f, fc_fp%data, ff_fp%data)\n"
            "    end do \n"
    "\n"
            "    call ff_fp%set_dirty()\n")
        assert expected in gen_code


def test_field_restrict(tmpdir, f90, f90flags):
    ''' Test that we generate correct code for an invoke containing a
    single restriction operation '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "22.1_intergrid_restrict.f90"),
                           api=API)
    for distmem in [True, False]:
        psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
        output = str(psy.gen)
        print output

        if utils.TEST_COMPILE:
            assert utils.code_compiles(API, psy, tmpdir, f90, f90flags)

        defs = (
            "    use restrict_kernel_mod, only : restrict_kernel_code\n"
            "    type(field_type), intent(inout) :: fc, ff\n"
            "    type(field_proxy_type) :: fc_fp, ff_fp\n"
            "\n"
            "    integer(kind=i_def) :: ndofs_c, ndofs_f\n"
            "    integer(kind=i_def) :: ncell_ratio, ncell_f, ncell_c\n"
            "    type(mesh_map_type), pointer :: mesh_map => null()\n"
            "    type(mesh_type), pointer     :: mesh => null()\n"
            "    type(mesh_type), pointer     :: mesh_f => null()\n"
            "\n"
            "    integer(kind=i_def), pointer :: dofmap_f(:,:) => null(), dofmap_c(:,:) => null()\n"
            "    integer(kind=i_def) :: cell\n"
            "    integer(kind=i_def) :: nlayers, ndf, undf_f, undf_c\n"
            "\n"
            "    integer(kind=i_def), pointer :: cell_map(:,:) => null()\n")
        assert defs in output

        inits = (
            "    ! get the field proxies\n"
            "    fc_fp = fc%get_proxy()\n"
            "    ff_fp = ff%get_proxy()\n"
            "\n"
            "    ! get the mesh from the first field argument, this is the\n"
            "    ! field which is being written too. See Kernel Metadata\n"
            "    mesh => fc%get_mesh() \n"
            "    ! get the map from the 1st to the 2nd , i.e. coarse to fine\n"
            "    mesh_f => ff%get_mesh()\n"
            "    mesh_map => mesh%get_mesh_map( mesh_f )\n"
            "    ! could get nlayers from either, but lets be consistent, get it from\n"
            "    ! the field we are writing too.\n"
            "    nlayers = fc_fp%vspace%get_nlayers()\n"
            "    !    ncell_f = mesh_map%get_nsource_cells()\n"
            "    ! there are multiple routes to this information which we don't want, so \n"
            "    ! always get the number of cells from the mesh, not the map\n"
            "    ncell_f = mesh_f%get_last_halo_cell(depth=2)\n"
            "    ncell_c = mesh%get_last_halo_cell(depth=1)\n"
            "    ncell_ratio = mesh_map%get_ncell_map_ratio()\n"
            "\n"
            "    dofmap_f => ff_fp%vspace%get_whole_dofmap()\n"
            "    dofmap_c => fc_fp%vspace%get_whole_dofmap()\n"
            "    ndf = ff_fp%vspace%get_ndf()\n"
            "    undf_f = ff_fp%vspace%get_undf()\n"
            "    undf_c = fc_fp%vspace%get_undf()\n")
        assert inits in output

        halo_exchs = (
            "    ! halo exchange to depth two on the fine as we are reading from\n"
            "    if (ff_fp%is_dirty(depth=2)) then\n"
            "       call ff_fp%halo_exchange(depth=2)\n"
            "    end if\n"
            "\n"
            "    ! halo exchange to depth one because we are incrementing and looping \n"
            "    ! to last halo cell(1)\n"
            "    if (fc_fp%is_dirty(depth=1)) then\n"
            "       call fc_fp%halo_exchange(depth=1)\n"
            "    end if\n"
            "\n"
            "    cell_map => mesh_map%get_whole_cell_map()\n")
        assert halo_exchs in output

        kern_call = (
            "    ! if we are using OpenMP, using the coarse grid colouring is still correct\n"
            "    do cell = 1, ncell_c\n"
            "\n"
            "       ! pass the whole dofmap for the fine (we are reading from)\n"
            "       call restrict_kernel_code(nlayers, cell_map(:,cell), ncell_ratio, &\n"
            "            dofmap_f, ncell_f, dofmap_c(:,cell), ndf, undf_f, undf_c, fc_fp%data, ff_fp%data)\n"
            "    end do \n"
            "    \n"
            "    call fc_fp%set_dirty()\n")
        assert kern_call in output
