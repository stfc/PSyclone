# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab


''' This module contains pytest tests for LFRic kernels which operate on
    halo cells (OPERATES_ON = HALO_CELL_COLUMN or OWNED_AND_HALO_CELL_COLUMN).
'''

import os
import pytest
from fparser import api as fpapi
from psyclone.domain.lfric import LFRicKern, LFRicKernMetadata
from psyclone.parse.algorithm import parse
from psyclone.parse.utils import ParseError
from psyclone.psyGen import PSyFactory
from psyclone.psyir.nodes import Loop
from psyclone.tests.lfric_build import LFRicBuild

BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "dynamo0p3")
TEST_API = "lfric"


@pytest.fixture(name="operates_on",
                params=["halo_cell_column", "owned_and_halo_cell_column"])
def operates_on_fixture(request):
    yield request.param


def test_halo_cell_kernel(operates_on):
    ''' Check that we can successfully parse metadata that specifies a
    kernel with operates_on = HALO_CELL_COLUMN. '''
    ast = fpapi.parse(f'''
module testkern_halo_mod
  type, extends(kernel_type) :: testkern_halo_type
     type(arg_type), meta_args(4) =                             &
          (/ arg_type(gh_scalar, gh_real,    gh_read),          &
             arg_type(gh_field,  gh_real,    gh_readwrite, w3), &
             arg_type(gh_field,  gh_real,    gh_read,      w2), &
             arg_type(gh_scalar, gh_integer, gh_read)           &
           /)
     integer :: operates_on = {operates_on}
   contains
     procedure, nopass :: code => testkern_halo_code
  end type testkern_halo_type
contains
  subroutine testkern_halo_code(nlevels, halo_depth, a, b, c, d)
    integer, intent(in) :: nlevels, halo_depth
  end subroutine testkern_halo_code
end module testkern_halo_mod
''', ignore_comments=False)
    dkm = LFRicKernMetadata(ast, name="testkern_halo_type")
    assert dkm.iterates_over == operates_on


def test_stencil_halo_kernel(operates_on):
    ''' Check that we accept a halo kernel if it has an argument with a
    stencil access. '''
    ast = fpapi.parse(f'''module testkern_domain_mod
  type, extends(kernel_type) :: testkern_domain_type
     type(arg_type), meta_args(3) =                                         &
          (/ arg_type(gh_scalar, gh_real, gh_read),                         &
             arg_type(gh_field,  gh_real, gh_readwrite, w3),                &
             arg_type(gh_field,  gh_real, gh_read,      w3, stencil(cross)) &
           /)
     integer :: operates_on = {operates_on}
   contains
     procedure, nopass :: code => testkern_domain_code
  end type testkern_domain_type
contains
  subroutine testkern_domain_code(a, b, c, d)
  end subroutine testkern_domain_code
end module testkern_domain_mod
''', ignore_comments=False)
    mdata = LFRicKernMetadata(ast, name="testkern_domain_type")
    assert mdata.iterates_over == operates_on


def test_psy_gen_halo_kernel(dist_mem, tmpdir, fortran_writer):
    ''' Check the generation of the PSy layer for an invoke consisting of a
    single kernel with operates_on=halo_cell_column. '''
    _, info = parse(os.path.join(BASE_PATH, "1.4_into_halos_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
    gen_code = str(psy.gen).lower()

    # A halo kernel needs to look up the last halo column in the mesh.
    # Therefore we require a mesh object.
    if dist_mem:
        assert "type(mesh_type), pointer :: mesh => null()" in gen_code
        assert "mesh => f1_proxy%vspace%get_mesh()" in gen_code
        assert ("loop0_stop = mesh%get_last_halo_cell(halo_depth)"
                in gen_code)

        assert ("      do cell = loop0_start, loop0_stop, 1\n"
                "        call testkern_halo_only_code(nlayers, halo_depth, a, "
                "f1_data, f2_data, m1_data, m2_data, undf_w1, map_w1(:,cell), "
                "undf_w2, map_w2(:,cell), undf_w3, map_w3(:,cell))"
                in gen_code)
    else:
        # No distributed memory so no halo region.
        assert "loop0_stop == f1_proxy%vspace%get_ncell()\n" in gen_code

        assert ("      do cell = loop0_start, loop0_stop, 1\n"
                "        call testkern_halo_only_code(nlayers, 0, a, "
                "f1_data, f2_data, m1_data, m2_data, undf_w1, map_w1(:,cell), "
                "undf_w2, map_w2(:,cell), undf_w3, map_w3(:,cell))"
                in gen_code)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Also test that the FortranWriter handles halo kernels as expected.
    # ATM we have a `lower_to_language_level method` for LFRicLoop which
    # removes the loop node for a domain kernel entirely and only leaves the
    # body. So we can't call the FortranWriter directly, since it will first
    # lower the tree, which removes the domain kernel.
    # In order to test the actual writer atm, we have to call the
    # `loop_node` directly. But in order for this to work, we need to
    # lower the actual kernel call. Once #1731 is fixed, the temporary
    # `lower_to_language_level` method in LFRicLoop can (likely) be removed,
    # and then we can just call `fortran_writer(schedule)` here.
    schedule = psy.invokes.invoke_list[0].schedule
    # Lower the LFRicKern:
    for kern in schedule.walk(LFRicKern):
        kern.lower_to_language_level()
    # Now call the loop handling method directly.
    out = fortran_writer.loop_node(schedule.walk(Loop)[0])
    assert ("call testkern_halo_only_code(nlayers, halo_depth, a, "
            "f1_data, f2_data, m1_data, m2_data, undf_w1, map_w1(:,cell), "
            "undf_w2, map_w2(:,cell), undf_w3, map_w3(:,cell))" in out)


def test_psy_gen_domain_two_kernel(dist_mem, tmpdir):
    ''' Check the generation of the PSy layer for an invoke consisting of a
    kernel with operates_on=domain and another with
    operates_on=halo_cell_column.
    '''
    _, info = parse(os.path.join(BASE_PATH, "missing_domain.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
    gen_code = str(psy.gen).lower()

    assert "mesh => f2_proxy%vspace%get_mesh()" in gen_code
    assert "integer(kind=i_def) ncell_2d_no_halos" in gen_code

    expected = (
        "      end do\n")
    if dist_mem:
        expected += (
            "      !\n"
            "      ! set halos dirty/clean for fields modified in the above "
            "loop\n"
            "      !\n"
            "      call f2_proxy%set_dirty()\n"
            "      !\n")
    expected += (
        "      call testkern_domain_code(nlayers, ncell_2d_no_halos, b, "
        "f1_data, ndf_w3, undf_w3, map_w3)\n")
    assert expected in gen_code
    if dist_mem:
        assert ("      ! set halos dirty/clean for fields modified in the "
                "above kernel\n"
                "      !\n"
                "      call f1_proxy%set_dirty()\n" in gen_code)

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_psy_gen_domain_multi_kernel(dist_mem, tmpdir):
    ''' Check the generation of the PSy layer for an invoke consisting of
    several kernels, two with operates_on=domain and another with
    operates_on=cell_column.
    '''
    _, info = parse(os.path.join(BASE_PATH, "also_missing__domain.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
    gen_code = str(psy.gen).lower()

    # Check that we only have one last-edge-cell assignment
    assert gen_code.count("ncell_2d_no_halos = mesh%get_last_edge_cell()") == 1

    expected = ("      !\n"
                "      call testkern_domain_code(nlayers, ncell_2d_no_halos, "
                "b, f1_data, ndf_w3, undf_w3, map_w3)\n")
    if dist_mem:
        assert "loop1_stop = mesh%get_last_halo_cell(1)\n" in gen_code
        expected += ("      !\n"
                     "      ! set halos dirty/clean for fields modified in "
                     "the above kernel\n"
                     "      !\n"
                     "      call f1_proxy%set_dirty()\n"
                     "      !\n"
                     "      if (f2_proxy%is_dirty(depth=1)) then\n"
                     "        call f2_proxy%halo_exchange(depth=1)\n"
                     "      end if\n"
                     "      if (f3_proxy%is_dirty(depth=1)) then\n"
                     "        call f3_proxy%halo_exchange(depth=1)\n"
                     "      end if\n"
                     "      if (f4_proxy%is_dirty(depth=1)) then\n"
                     "        call f4_proxy%halo_exchange(depth=1)\n"
                     "      end if\n"
                     "      call f1_proxy%halo_exchange(depth=1)\n")
    else:
        assert "loop1_stop = f2_proxy%vspace%get_ncell()\n" in gen_code
    expected += "      do cell = loop1_start, loop1_stop, 1\n"
    assert expected in gen_code

    expected = (
        "      end do\n")
    if dist_mem:
        expected += (
            "      !\n"
            "      ! set halos dirty/clean for fields modified in the above "
            "loop\n"
            "      !\n"
            "      call f1_proxy%set_dirty()\n"
            "      !\n")
    expected += (
        "      call testkern_domain_code(nlayers, ncell_2d_no_halos, c, "
        "f1_data, ndf_w3, undf_w3, map_w3)\n")
    assert expected in gen_code
    if dist_mem:
        assert ("      ! set halos dirty/clean for fields modified in the "
                "above kernel\n"
                "      !\n"
                "      call f5_proxy%set_dirty()\n"
                "      !\n"
                "      !\n"
                "    end subroutine invoke_0" in gen_code)

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_cell_halo_plus_cma_kernels(dist_mem, tmpdir):
    '''
    Check that we look-up and use the number of columns with and without halos
    when an invoke contains both a domain and a CMA kernel.
    '''
    _, info = parse(os.path.join(BASE_PATH, "missing_cma.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
    gen_code = str(psy.gen).lower()

    assert "type(mesh_type), pointer :: mesh => null()" in gen_code
    assert "integer(kind=i_def) ncell_2d" in gen_code
    assert "integer(kind=i_def) ncell_2d_no_halos" in gen_code
    assert "mesh => f1_proxy%vspace%get_mesh()" in gen_code
    assert "ncell_2d = mesh%get_ncells_2d()" in gen_code
    assert "ncell_2d_no_halos = mesh%get_last_edge_cell()" in gen_code
    assert ("call testkern_domain_code(nlayers, ncell_2d_no_halos, b, "
            "f1_data, ndf_w3, undf_w3, map_w3)" in gen_code)
    assert ("call columnwise_op_asm_kernel_code(cell, nlayers, ncell_2d, "
            "lma_op1_proxy%ncell_3d," in gen_code)

    assert LFRicBuild(tmpdir).code_compiles(psy)
