# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Laboratory
# Modified: I. Kavcic and L. Turner, Met Office
# Modified: R. W. Ford, STFC Daresbury Laboratory

'''
Module containing pytest tests for the mesh-property support in the kernel-stub
generation functionality with the LFRic (Dynamo0.3) API.
'''

import os
from fparser import api as fpapi
from psyclone.domain.lfric import LFRicKern, LFRicKernMetadata


# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "dynamo0p3")
TEST_API = "dynamo0.3"

MESH_PROP_MDATA = '''
module testkern_mesh_prop_quad_mod
  type, extends(kernel_type) :: testkern_mesh_prop_quad_type
    type(arg_type), dimension(2) :: meta_args =       &
        (/ arg_type(gh_field, gh_real, gh_read,  w1), &
           arg_type(gh_field, gh_real, gh_write, wtheta) /)
    type(func_type), meta_funcs(2) = &
        (/ func_type(w1, gh_basis),  &
           func_type(wtheta, gh_basis) /)
    type(mesh_data_type), dimension(1) :: meta_mesh = &
        (/ mesh_data_type(adjacent_face) /)
    type(reference_element_data_type), dimension(2) ::               &
      meta_reference_element =                                       &
        (/ reference_element_data_type(normals_to_horizontal_faces), &
           reference_element_data_type(outward_normals_to_faces) /)
     integer :: operates_on = cell_column
     integer :: gh_shape = gh_quadrature_xyoz
   contains
     procedure, nopass :: code => testkern_mesh_prop_quad_code
  end type testkern_mesh_prop_quad_type
contains
  subroutine testkern_mesh_prop_quad_code()
  end subroutine testkern_mesh_prop_quad_code
end module testkern_mesh_prop_quad_mod
'''


def test_mesh_prop_stub_gen():
    ''' Check that correct kernel stub code is produced when the kernel
    metadata contains a mesh property. '''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_mesh_prop_mod.F90"),
                      ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    gen = str(kernel.gen_stub).lower()

    output = (
        "  module testkern_mesh_prop_mod\n"
        "    implicit none\n"
        "    contains\n"
        "    subroutine testkern_mesh_prop_code(nlayers, rscalar_1, "
        "field_2_w1, ndf_w1, undf_w1, map_w1, nfaces_re_h, adjacent_face)\n"
        "      use constants_mod\n"
        "      implicit none\n"
        "      integer(kind=i_def), intent(in) :: nlayers\n"
        "      integer(kind=i_def), intent(in) :: ndf_w1\n"
        "      integer(kind=i_def), intent(in), dimension(ndf_w1) :: map_w1\n"
        "      integer(kind=i_def), intent(in) :: undf_w1\n"
        "      real(kind=r_def), intent(in) :: rscalar_1\n"
        "      real(kind=r_def), intent(inout), dimension(undf_w1) :: "
        "field_2_w1\n"
        "      integer(kind=i_def), intent(in) :: nfaces_re_h\n"
        "      integer(kind=i_def), intent(in), dimension(nfaces_re_h) :: "
        "adjacent_face\n"
        "    end subroutine testkern_mesh_prop_code\n"
        "  end module testkern_mesh_prop_mod")
    assert output in gen


def test_mesh_props_quad_stub_gen():
    ''' Check that correct stub code is produced when the kernel metadata
    specifies both mesh and quadrature properties (quadrature
    properties should be placed at the end of subroutine argument list). '''
    ast = fpapi.parse(MESH_PROP_MDATA, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    gen = str(kernel.gen_stub)

    output1 = (
        "  SUBROUTINE testkern_mesh_prop_quad_code(nlayers, field_1_w1, "
        "field_2_wtheta, ndf_w1, undf_w1, map_w1, basis_w1_qr_xyoz, "
        "ndf_wtheta, undf_wtheta, map_wtheta, basis_wtheta_qr_xyoz, "
        "nfaces_re_h, nfaces_re, normals_to_horiz_faces, "
        "out_normals_to_faces, adjacent_face, np_xy_qr_xyoz, "
        "np_z_qr_xyoz, weights_xy_qr_xyoz, weights_z_qr_xyoz)")
    assert output1 in gen
    output2 = (
        "      INTEGER(KIND=i_def), intent(in) :: np_xy_qr_xyoz, "
        "np_z_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), "
        "dimension(3,ndf_w1,np_xy_qr_xyoz,np_z_qr_xyoz) :: basis_w1_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), "
        "dimension(1,ndf_wtheta,np_xy_qr_xyoz,np_z_qr_xyoz) :: "
        "basis_wtheta_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(np_xy_qr_xyoz) :: "
        "weights_xy_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(np_z_qr_xyoz) :: "
        "weights_z_qr_xyoz\n"
        "      INTEGER(KIND=i_def), intent(in) :: nfaces_re_h\n"
        "      INTEGER(KIND=i_def), intent(in) :: nfaces_re\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,nfaces_re_h) :: "
        "normals_to_horiz_faces\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,nfaces_re) :: "
        "out_normals_to_faces\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(nfaces_re_h) :: "
        "adjacent_face\n"
    )
    assert output2 in gen
