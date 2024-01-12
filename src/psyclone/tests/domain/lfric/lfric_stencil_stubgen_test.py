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
# Modified I. Kavcic, A. Coughtrie and L. Turner, Met Office
# Modified J. Henrichs, Bureau of Meteorology

''' This module tests the LFRic kernel-stub generator for using pytest. '''


import os

from fparser import api as fpapi

from psyclone.domain.lfric import LFRicKern, LFRicKernMetadata

# Constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "../..", "test_files", "dynamo0p3")
TEST_API = "dynamo0.3"


def test_stub_stencil_extent():
    '''
    Check that correct stub code is produced when there is a stencil
    access
    '''
    ast = fpapi.parse(os.path.join(BASE_PATH, "testkern_stencil_mod.f90"),
                      ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    generated_code = str(kernel.gen_stub)
    result1 = (
        "SUBROUTINE testkern_stencil_code(nlayers, field_1_w1, "
        "field_2_w2, field_2_stencil_size, field_2_stencil_dofmap, "
        "field_3_w2, field_4_w3, ndf_w1, undf_w1, map_w1, ndf_w2, "
        "undf_w2, map_w2, ndf_w3, undf_w3, map_w3)")
    assert result1 in generated_code
    result2 = "INTEGER(KIND=i_def), intent(in) :: field_2_stencil_size"
    assert result2 in generated_code
    assert (
        "INTEGER(KIND=i_def), intent(in), "
        "dimension(ndf_w2,field_2_stencil_size) :: field_2_stencil_dofmap"
        in generated_code)


def test_stub_cross2d_stencil():
    '''
    Check that the correct stub code is generated when using a CROSS2D
    stencil
    '''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_stencil_cross2d_mod.f90"),
                      ignore_comments=False)

    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    generated_code = str(kernel.gen_stub)
    print(generated_code)
    result1 = (
        "    SUBROUTINE testkern_stencil_cross2d_code(nlayers, field_1_w1, "
        "field_2_w2, field_2_stencil_size, field_2_max_branch_length, "
        "field_2_stencil_dofmap, field_3_w2, field_4_w3, ndf_w1, undf_w1, "
        "map_w1, ndf_w2, undf_w2, map_w2, ndf_w3, undf_w3, map_w3)"
    )
    assert result1 in generated_code
    result2 = (
        "      INTEGER(KIND=i_def), intent(in), dimension(4) :: "
        "field_2_stencil_size\n"
        "      INTEGER(KIND=i_def), intent(in) :: field_2_max_branch_length\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2,"
        "field_2_max_branch_length,4) :: field_2_stencil_dofmap")
    assert result2 in generated_code


def test_stub_stencil_direction():
    '''
    Check that correct stub code is produced when there is a stencil
    access which requires a direction argument
    '''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_stencil_xory1d_mod.f90"),
                      ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    generated_code = str(kernel.gen_stub)
    result1 = (
        "    SUBROUTINE testkern_stencil_xory1d_code(nlayers, field_1_w1, "
        "field_2_w2, field_2_stencil_size, field_2_direction, "
        "field_2_stencil_dofmap, field_3_w2, field_4_w3, ndf_w1, undf_w1, "
        "map_w1, ndf_w2, undf_w2, map_w2, ndf_w3, undf_w3, map_w3)")
    assert result1 in generated_code
    result2 = (
        "      INTEGER(KIND=i_def), intent(in) :: field_2_stencil_size\n"
        "      INTEGER(KIND=i_def), intent(in) :: field_2_direction\n"
        "      INTEGER(KIND=i_def), intent(in), "
        "dimension(ndf_w2,field_2_stencil_size) :: field_2_stencil_dofmap")
    assert result2 in generated_code


def test_stub_stencil_vector():
    '''
    Check that correct stub code is produced when there is a stencil
    access which is a vector
    '''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_stencil_vector_mod.f90"),
                      ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    generated_code = str(kernel.gen_stub)
    result1 = (
        "    SUBROUTINE testkern_stencil_vector_code(nlayers, field_1_w0_v1, "
        "field_1_w0_v2, field_1_w0_v3, field_2_w3_v1, field_2_w3_v2, "
        "field_2_w3_v3, field_2_w3_v4, field_2_stencil_size, "
        "field_2_stencil_dofmap, ndf_w0, undf_w0, map_w0, ndf_w3, undf_w3, "
        "map_w3)")
    assert result1 in generated_code
    result2 = (
        "      INTEGER(KIND=i_def), intent(in) :: field_2_stencil_size\n"
        "      INTEGER(KIND=i_def), intent(in), "
        "dimension(ndf_w3,field_2_stencil_size) :: field_2_stencil_dofmap")
    assert result2 in generated_code


def test_stub_stencil_multi():
    '''
    Check that correct stub code is produced when there are multiple
    stencils
    '''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_stencil_multi_mod.f90"),
                      ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    generated_code = str(kernel.gen_stub)
    result1 = (
        "    SUBROUTINE testkern_stencil_multi_code(nlayers, field_1_w1, "
        "field_2_w2, field_2_stencil_size, field_2_stencil_dofmap, field_3_w2,"
        " field_3_stencil_size, field_3_direction, field_3_stencil_dofmap, "
        "field_4_w3, field_4_stencil_size, field_4_stencil_dofmap, ndf_w1, "
        "undf_w1, map_w1, ndf_w2, undf_w2, map_w2, ndf_w3, undf_w3, map_w3)")
    assert result1 in generated_code
    result2 = (
        "      REAL(KIND=r_def), intent(in), dimension(undf_w2) :: "
        "field_3_w2\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w3) :: "
        "field_4_w3\n"
        "      INTEGER(KIND=i_def), intent(in) :: field_2_stencil_size, "
        "field_3_stencil_size, field_4_stencil_size\n"
        "      INTEGER(KIND=i_def), intent(in) :: field_3_direction\n"
        "      INTEGER(KIND=i_def), intent(in), "
        "dimension(ndf_w2,field_2_stencil_size) :: field_2_stencil_dofmap\n"
        "      INTEGER(KIND=i_def), intent(in), "
        "dimension(ndf_w2,field_3_stencil_size) :: field_3_stencil_dofmap\n"
        "      INTEGER(KIND=i_def), intent(in), "
        "dimension(ndf_w3,field_4_stencil_size) :: field_4_stencil_dofmap")

    assert result2 in generated_code
