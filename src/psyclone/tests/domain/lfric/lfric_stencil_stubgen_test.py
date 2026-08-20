# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module tests the LFRic kernel-stub generator for using pytest. '''


import os

from fparser import api as fpapi

from psyclone.domain.lfric import LFRicKern, LFRicKernelMetadata

# Constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "../..", "test_files", "lfric")
TEST_API = "lfric"


def test_stub_stencil_extent(fortran_writer):
    '''
    Check that correct stub code is produced when there is a stencil
    access
    '''
    ast = fpapi.parse(os.path.join(BASE_PATH, "testkern_stencil_mod.f90"),
                      ignore_comments=False)
    metadata = LFRicKernelMetadata.create_from_fortran_string(str(ast))
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    generated_code = fortran_writer(kernel.gen_stub)
    result1 = (
        "subroutine testkern_stencil_code(nlayers, field_1_w1, "
        "field_2_w2, field_2_stencil_size, field_2_stencil_dofmap, "
        "field_3_w2, field_4_w3, ndf_w1, undf_w1, map_w1, ndf_w2, "
        "undf_w2, map_w2, ndf_w3, undf_w3, map_w3)")
    assert result1 in generated_code
    result2 = "integer(kind=i_def), intent(in) :: field_2_stencil_size"
    assert result2 in generated_code
    assert (
        "integer(kind=i_def), dimension(ndf_w2,field_2_stencil_size), "
        "intent(in) :: field_2_stencil_dofmap"
        in generated_code)


def test_stub_cross2d_stencil(fortran_writer):
    '''
    Check that the correct stub code is generated when using a CROSS2D
    stencil
    '''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_stencil_cross2d_mod.f90"),
                      ignore_comments=False)

    metadata = LFRicKernelMetadata.create_from_fortran_string(str(ast))
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    generated_code = fortran_writer(kernel.gen_stub)
    result1 = (
        "  subroutine testkern_stencil_cross2d_code(nlayers, field_1_w1, "
        "field_2_w2, field_2_stencil_size, field_2_max_branch_length, "
        "field_2_stencil_dofmap, field_3_w2, field_4_w3, ndf_w1, undf_w1, "
        "map_w1, ndf_w2, undf_w2, map_w2, ndf_w3, undf_w3, map_w3)"
    )
    assert result1 in generated_code
    assert ("integer(kind=i_def), dimension(4), intent(in) :: "
            "field_2_stencil_size\n" in generated_code)
    assert ("integer(kind=i_def), intent(in) :: field_2_max_branch_length\n"
            in generated_code)
    assert ("integer(kind=i_def), dimension(ndf_w2,field_2_max_branch_length,"
            "4), intent(in) :: field_2_stencil_dofmap"
            in generated_code)


def test_stub_stencil_direction(fortran_writer):
    '''
    Check that correct stub code is produced when there is a stencil
    access which requires a direction argument
    '''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_stencil_xory1d_mod.f90"),
                      ignore_comments=False)
    metadata = LFRicKernelMetadata.create_from_fortran_string(str(ast))
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    code = fortran_writer(kernel.gen_stub)
    assert (
        "  subroutine testkern_stencil_xory1d_code(nlayers, field_1_w1, "
        "field_2_w2, field_2_stencil_size, field_2_direction, "
        "field_2_stencil_dofmap, field_3_w2, field_4_w3, ndf_w1, undf_w1, "
        "map_w1, ndf_w2, undf_w2, map_w2, ndf_w3, undf_w3, map_w3)" in code)
    assert "integer(kind=i_def), intent(in) :: field_2_stencil_size\n" in code
    assert "integer(kind=i_def), intent(in) :: field_2_direction\n" in code
    assert ("integer(kind=i_def), dimension(ndf_w2,field_2_stencil_size), "
            "intent(in) :: field_2_stencil_dofmap" in code)


def test_stub_stencil_vector(fortran_writer):
    '''
    Check that correct stub code is produced when there is a stencil
    access which is a vector
    '''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_stencil_vector_mod.f90"),
                      ignore_comments=False)
    metadata = LFRicKernelMetadata.create_from_fortran_string(str(ast))
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    code = fortran_writer(kernel.gen_stub)
    assert (
        "  subroutine testkern_stencil_vector_code(nlayers, field_1_w0_v1, "
        "field_1_w0_v2, field_1_w0_v3, field_2_w3_v1, field_2_w3_v2, "
        "field_2_w3_v3, field_2_w3_v4, field_2_stencil_size, "
        "field_2_stencil_dofmap, ndf_w0, undf_w0, map_w0, ndf_w3, undf_w3, "
        "map_w3)" in code)
    assert "integer(kind=i_def), intent(in) :: field_2_stencil_size\n" in code
    assert ("integer(kind=i_def), dimension(ndf_w3,field_2_stencil_size), "
            "intent(in) :: field_2_stencil_dofmap" in code)


def test_stub_stencil_multi(fortran_writer):
    '''
    Check that correct stub code is produced when there are multiple
    stencils
    '''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_stencil_multi_mod.f90"),
                      ignore_comments=False)
    metadata = LFRicKernelMetadata.create_from_fortran_string(str(ast))
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    code = fortran_writer(kernel.gen_stub)
    assert (
        "  subroutine testkern_stencil_multi_code(nlayers, field_1_w1, "
        "field_2_w2, field_2_stencil_size, field_2_stencil_dofmap, field_3_w2,"
        " field_3_stencil_size, field_3_direction, field_3_stencil_dofmap, "
        "field_4_w3, field_4_stencil_size, field_4_stencil_dofmap, ndf_w1, "
        "undf_w1, map_w1, ndf_w2, undf_w2, map_w2, ndf_w3, undf_w3, map_w3)"
        in code)
    assert ("real(kind=r_def), dimension(undf_w2), intent(in) "
            ":: field_3_w2\n" in code)
    assert ("real(kind=r_def), dimension(undf_w3), intent(in) :: "
            "field_4_w3\n" in code)
    assert "integer(kind=i_def), intent(in) :: field_2_stencil_size" in code
    assert "integer(kind=i_def), intent(in) :: field_3_stencil_size" in code
    assert "integer(kind=i_def), intent(in) :: field_4_stencil_size" in code
    assert "integer(kind=i_def), intent(in) :: field_3_direction\n" in code
    assert ("integer(kind=i_def), dimension(ndf_w2,field_2_stencil_size), "
            "intent(in) :: field_2_stencil_dofmap\n" in code)
    assert ("integer(kind=i_def), dimension(ndf_w2,field_3_stencil_size), "
            "intent(in) :: field_3_stencil_dofmap\n" in code)
    assert ("integer(kind=i_def), dimension(ndf_w3,field_4_stencil_size), "
            "intent(in) :: field_4_stencil_dofmap" in code)
