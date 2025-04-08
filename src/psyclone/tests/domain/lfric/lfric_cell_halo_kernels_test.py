# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024-2025, Science and Technology Facilities Council.
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

import pytest
from fparser import api as fpapi
from psyclone.domain.lfric import LFRicKern, LFRicKernMetadata
from psyclone.errors import InternalError
from psyclone.psyir.nodes import Loop
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.utilities import get_invoke

TEST_API = "lfric"


@pytest.mark.parametrize("operates_on", ["halo_cell_column",
                                         "owned_and_halo_cell_column"])
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


@pytest.mark.parametrize("operates_on", ["halo_cell_column",
                                         "owned_and_halo_cell_column"])
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
    psy, _ = get_invoke("1.4_into_halos_invoke.f90", TEST_API,
                        dist_mem=dist_mem, idx=0)
    gen_code = str(psy.gen).lower()

    # A halo kernel needs to look up the last halo column in the mesh.
    # Therefore we require a mesh object.
    if dist_mem:
        assert "integer, intent(in) :: hdepth" in gen_code

        assert "type(mesh_type), pointer :: mesh => null()" in gen_code
        assert "mesh => f1_proxy%vspace%get_mesh()" in gen_code
        # Loop must be over halo cells only
        assert "loop0_start = mesh%get_last_edge_cell()+1" in gen_code
        assert ("loop0_stop = mesh%get_last_halo_cell(hdepth)"
                in gen_code)

        assert ("      do cell = loop0_start, loop0_stop, 1\n"
                "        call testkern_halo_only_code(nlayers_f1, a, "
                "f1_data, f2_data, m1_data, m2_data, ndf_w1, undf_w1, "
                "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
                "undf_w3, map_w3(:,cell))"
                in gen_code)

        # Check for appropriate set-dirty/clean calls. Outermost halo remains
        # dirty because field being updated is on continuous function space.
        assert ("      call f1_proxy%set_dirty()\n"
                "      call f1_proxy%set_clean(hdepth - 1)" in gen_code)
    else:
        # No distributed memory so no halo region => no halo depths passed
        # from Alg layer.
        assert (" subroutine invoke_0_testkern_halo_only_type"
                "(a, f1, f2, m1, m2)" in gen_code)
        assert "integer, intent(in) :: hdepth" not in gen_code
        # Kernel is not called.
        assert "call testkern_halo_only_code( " not in gen_code

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Also test that the FortranWriter handles halo kernels as expected.
    # We can't call the FortranWriter directly, since it will first attempt
    # to lower the tree which results in errors.
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
    assert ("call testkern_halo_only_code(nlayers_f1, a, f1_data, "
            "f2_data, m1_data, m2_data, ndf_w1, undf_w1, map_w1(:,cell), "
            "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
            "map_w3(:,cell))" in out)


def test_psy_gen_domain_two_kernel(dist_mem, tmpdir):
    ''' Check the generation of the PSy layer for an invoke consisting of a
    kernel with operates_on=domain and another with
    operates_on=halo_cell_column.
    '''
    psy, _ = get_invoke("1.4.1_into_halos_plus_domain_invoke.f90",
                        TEST_API, dist_mem=dist_mem, idx=0)
    gen_code = str(psy.gen).lower()

    if dist_mem:
        assert "mesh => f1_proxy%vspace%get_mesh()" in gen_code

    assert "integer(kind=i_def) ncell_2d_no_halos" in gen_code

    expected = ""
    if dist_mem:
        expected += (
            "      end do\n"
            "      !\n"
            "      ! set halos dirty/clean for fields modified in the above "
            "loop\n"
            "      !\n"
            "      call f1_proxy%set_dirty()\n"
            "      call f1_proxy%set_clean(hdepth - 1)\n"
            "      !\n")
    expected += (
        "      call testkern_domain_code(nlayers_f1, ncell_2d_no_halos, a, "
        "f1_data, ndf_w3, undf_w3, map_w3)\n")
    assert expected in gen_code
    if dist_mem:
        assert ("      ! set halos dirty/clean for fields modified in the "
                "above kernel\n"
                "      !\n"
                "      call f1_proxy%set_dirty()\n" in gen_code)

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_psy_gen_halo_kernel_discontinuous_space(dist_mem, tmpdir):
    '''
    Test that the correct kernel and set-clean/dirty calls are generated for
    kernels that operate on owned and halo cells for a field on a discontinuous
    function space.

    '''
    psy, _ = get_invoke("1.4.2_multi_into_halos_invoke.f90",
                        TEST_API, dist_mem=dist_mem, idx=0)
    gen_code = str(psy.gen).lower()
    if dist_mem:
        assert "integer, intent(in) :: hdepth, other_depth" in gen_code

        # The halo-only kernel updates a field on a continuous function space
        # and thus leaves the outermost halo cell dirty.
        assert '''call testkern_halo_only_code(nlayers_f1, a, f1_data,\
 f2_data, m1_data, m2_data, ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, \
map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))
      end do
      !
      ! set halos dirty/clean for fields modified in the above loop
      !
      call f1_proxy%set_dirty()
      call f1_proxy%set_clean(hdepth - 1)''' in gen_code

        # testkern_code is a 'normal' kernel and thus leaves all halo cells
        # dirty.
        assert '''call testkern_code(nlayers_f1, a, f1_data, f2_data, m1_data,\
 m2_data, ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), \
ndf_w3, undf_w3, map_w3(:,cell))
      end do
      !
      ! set halos dirty/clean for fields modified in the above loop
      !
      call f1_proxy%set_dirty()
      !''' in gen_code

        # testkern_halo_and_owned_code operates in the halo for a field on a
        # discontinuous function space and therefore the halo is left clean to
        # the specified depth.
        assert '''call testkern_halo_and_owned_code(nlayers_f1, \
a, f1_data, f2_data, m1_data, m2_data, ndf_w3, undf_w3, map_w3(:,cell), \
ndf_w2, undf_w2, map_w2(:,cell))
      end do
      !
      ! set halos dirty/clean for fields modified in the above loop
      !
      call f1_proxy%set_dirty()
      call f1_proxy%set_clean(other_depth)''' in gen_code
    else:
        # No distributed memory.
        # => no halo depths to pass from Algorithm layer.
        assert "integer, intent(in) :: hdepth, other_depth" not in gen_code
        # => no halos so no need to call a kernel which only operates on
        #    halo cells.
        assert "call testkern_halo_only_code(" not in gen_code
        # However, a kernel that operates on owned *and* halo cells must still
        # be called.
        assert "call testkern_halo_and_owned_code(nlayers_f1, a" in gen_code

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_psy_gen_halo_kernel_literal_depths(dist_mem, tmpdir):
    '''
    Test the support for invokes of 'halo' kernels where the halo depth is
    specified using a literal value.

    '''
    psy, _ = get_invoke("1.4.3_literal_depth_into_halos_invoke.f90",
                        TEST_API, dist_mem=dist_mem, idx=0)
    gen_code = str(psy.gen).lower()
    if dist_mem:
        # Make sure we aren't attempting to specify literal values in the
        # argument list to the PSy-layer routine.
        assert "subroutine invoke_0(a, f1, f2, m1, m2, hdepth)" in gen_code
        # First kernel operates into the halo to a depth of '2' but updates a
        # field on a continuous function space so only the level-1 halo is
        # left clean.
        assert '''call f1_proxy%set_dirty()
      call f1_proxy%set_clean(1)
      !''' in gen_code
        assert '''call f1_proxy%set_dirty()
      call f1_proxy%set_clean(hdepth)
      !''' in gen_code
        assert '''call f1_proxy%set_dirty()
      call f1_proxy%set_clean(5)
      !''' in gen_code
    else:
        assert "call testkern_halo_only_code(" not in gen_code
        assert "call testkern_halo_and_owned_code(nlayers_f1, a" in gen_code
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_halo_kernel_depth_as_expression(dist_mem):
    '''
    Check that the expected exception is raised if the Algorithm layer
    specifies the halo depth as an expression.

    '''
    with pytest.raises(InternalError) as err:
        _, _ = get_invoke("1.4.4_exprn_depth_into_halos_invoke.f90", TEST_API,
                          dist_mem=dist_mem, idx=0)
    assert "Unsupported argument structure" in str(err.value)
