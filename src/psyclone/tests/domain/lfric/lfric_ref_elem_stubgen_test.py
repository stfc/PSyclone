# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
Module containing pytest tests for the reference-element stub generation
functionality of the LFRic API.
'''

import os
from psyclone.domain.lfric import LFRicKern, LFRicKernelMetadata


# Constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "../..", "test_files", "lfric")
TEST_API = "lfric"

REF_ELEM_QUAD_MDATA = '''
module testkern_refelem_quad_mod
  type, extends(kernel_type) :: testkern_refelem_quad_type
    type(arg_type), dimension(2) :: meta_args =       &
        (/ arg_type(gh_field, gh_real, gh_read,  w1), &
           arg_type(gh_field, gh_real, gh_write, wtheta) /)
    type(func_type), dimension(2) :: meta_funcs = &
        (/ func_type(w1, gh_basis),  &
           func_type(wtheta, gh_basis) /)
    type(reference_element_data_type), dimension(2) ::          &
      meta_reference_element =                                  &
        (/ reference_element_data_type(normals_to_faces),       &
           reference_element_data_type(outward_normals_to_faces) /)
     integer :: operates_on = cell_column
     integer :: gh_shape = gh_quadrature_xyoz
   contains
     procedure, nopass :: code => testkern_refelem_quad_code
  end type testkern_refelem_quad_type
contains
  subroutine testkern_refelem_quad_code()
  end subroutine testkern_refelem_quad_code
end module testkern_refelem_quad_mod
'''


def test_refelem_stub_gen(fortran_writer, fortran_reader):
    ''' Check that correct kernel stub code is produced when the kernel
    metadata contain reference element properties. '''
    psyir = fortran_reader.psyir_from_file(
        os.path.join(BASE_PATH, "testkern_ref_elem_mod.F90"))
    metadata = LFRicKernelMetadata.create_from_psyir(psyir)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    gen = fortran_writer(kernel.gen_stub)

    assert """\
module testkern_ref_elem_mod
  implicit none
  public

  contains
  subroutine testkern_ref_elem_code(nlayers, rscalar_1, field_2_w1, \
field_3_w2, field_4_w2, field_5_w3, ndf_w1, undf_w1, map_w1, ndf_w2, \
undf_w2, map_w2, ndf_w3, undf_w3, map_w3, nfaces_re_h, nfaces_re_v, \
normals_to_horiz_faces, normals_to_vert_faces)
    use constants_mod
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w1
    integer(kind=i_def), dimension(ndf_w1), intent(in) :: map_w1
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), dimension(ndf_w2), intent(in) :: map_w2
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), dimension(ndf_w3), intent(in) :: map_w3
    integer(kind=i_def), intent(in) :: undf_w1
    integer(kind=i_def), intent(in) :: undf_w2
    integer(kind=i_def), intent(in) :: undf_w3
    real(kind=r_def), intent(in) :: rscalar_1
    real(kind=r_def), dimension(undf_w1), intent(inout) :: field_2_w1
    real(kind=r_def), dimension(undf_w2), intent(in) :: field_3_w2
    real(kind=r_def), dimension(undf_w2), intent(in) :: field_4_w2
    real(kind=r_def), dimension(undf_w3), intent(in) :: field_5_w3
    integer(kind=i_def), intent(in) :: nfaces_re_h
    integer(kind=i_def), intent(in) :: nfaces_re_v
    real(kind=r_def), dimension(3,nfaces_re_h), intent(in) \
:: normals_to_horiz_faces
    real(kind=r_def), dimension(3,nfaces_re_v), intent(in) \
:: normals_to_vert_faces


  end subroutine testkern_ref_elem_code

end module testkern_ref_elem_mod
""" == gen


def test_refelem_quad_stub_gen(fortran_writer, fortran_reader):
    ''' Check that correct stub code is produced when the kernel metadata
    contain reference element and quadrature properties (quadrature
    properties should be placed at the end of subroutine argument list). '''
    psyir = fortran_reader.psyir_from_source(REF_ELEM_QUAD_MDATA)
    metadata = LFRicKernelMetadata.create_from_psyir(psyir)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    gen = fortran_writer(kernel.gen_stub)

    output1 = (
        "  subroutine testkern_refelem_quad_code(nlayers, field_1_w1, "
        "field_2_wtheta, ndf_w1, undf_w1, map_w1, basis_w1_qr_xyoz, "
        "ndf_wtheta, undf_wtheta, map_wtheta, basis_wtheta_qr_xyoz, "
        "nfaces_re, normals_to_faces, out_normals_to_faces, np_xy_qr_xyoz, "
        "np_z_qr_xyoz, weights_xy_qr_xyoz, weights_z_qr_xyoz)")
    assert output1 in gen
    assert """\
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w1
    integer(kind=i_def), dimension(ndf_w1), intent(in) :: map_w1
    integer(kind=i_def), intent(in) :: ndf_wtheta
    integer(kind=i_def), dimension(ndf_wtheta), intent(in) :: map_wtheta
    integer(kind=i_def), intent(in) :: undf_w1
    integer(kind=i_def), intent(in) :: undf_wtheta
    real(kind=r_def), dimension(undf_w1), intent(in) :: field_1_w1
    real(kind=r_def), dimension(undf_wtheta), intent(inout) :: field_2_wtheta
    integer(kind=i_def), intent(in) :: np_xy_qr_xyoz
    integer(kind=i_def), intent(in) :: np_z_qr_xyoz
    real(kind=r_def), dimension(3,ndf_w1,np_xy_qr_xyoz,np_z_qr_xyoz), \
intent(in) :: basis_w1_qr_xyoz
    real(kind=r_def), dimension(1,ndf_wtheta,np_xy_qr_xyoz,np_z_qr_xyoz), \
intent(in) :: basis_wtheta_qr_xyoz
    real(kind=r_def), dimension(np_xy_qr_xyoz), intent(in) \
:: weights_xy_qr_xyoz
    real(kind=r_def), dimension(np_z_qr_xyoz), intent(in) :: weights_z_qr_xyoz
    integer(kind=i_def), intent(in) :: nfaces_re
    real(kind=r_def), dimension(3,nfaces_re), intent(in) :: normals_to_faces
    real(kind=r_def), dimension(3,nfaces_re), intent(in) \
:: out_normals_to_faces""" in gen
