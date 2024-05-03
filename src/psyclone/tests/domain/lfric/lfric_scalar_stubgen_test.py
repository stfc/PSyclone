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
# Authors: R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab;
#          I. Kavcic, A. Coughtrie, L. Turner and O. Brunt, Met Office;
#          C. M. Maynard, Met Office/University of Reading;
#          J. Henrichs, Bureau of Meteorology.

'''
Module containing pytest tests for kernel stub code generation for the
LFRic scalar arguments.
'''

from __future__ import absolute_import, print_function
import os
import pytest

from fparser import api as fpapi
from psyclone.domain.lfric import (LFRicConstants, LFRicKern,
                                   LFRicKernMetadata, LFRicScalarArgs)
from psyclone.f2pygen import ModuleGen
from psyclone.errors import InternalError
from psyclone.gen_kernel_stub import generate
from psyclone.parse.utils import ParseError

# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "dynamo0p3")
TEST_API = "dynamo0.3"


def test_lfricscalars_stub_err():
    ''' Check that LFRicScalarArgs._stub_declarations() raises the
    expected internal error if it encounters an unrecognised data
    type of a scalar argument when generating a kernel stub.

    '''
    ast = fpapi.parse(os.path.join(BASE_PATH,
                                   "testkern_one_int_scalar_mod.f90"),
                      ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    # Sabotage the scalar argument to make it have an invalid data type
    arg = kernel.arguments.args[1]
    arg.descriptor._data_type = "gh_invalid_scalar"
    with pytest.raises(InternalError) as err:
        LFRicScalarArgs(kernel)._stub_declarations(ModuleGen(name="my_mod"))
    const = LFRicConstants()
    assert (f"Found an unsupported data type 'gh_invalid_scalar' for the "
            f"scalar argument 'iscalar_2'. Supported types are "
            f"{const.VALID_SCALAR_DATA_TYPES}." in str(err.value))


def test_stub_generate_with_scalars():
    ''' Check that the stub generate produces the expected output when
    the kernel has scalar arguments. '''
    result = generate(
        os.path.join(BASE_PATH, "testkern_three_scalars_mod.f90"),
        api=TEST_API)

    expected = (
        "  MODULE testkern_three_scalars_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE testkern_three_scalars_code(nlayers, rscalar_1, "
        "field_2_w1, field_3_w2, field_4_w2, field_5_w3, lscalar_6, "
        "iscalar_7, ndf_w1, undf_w1, map_w1, ndf_w2, undf_w2, map_w2, "
        "ndf_w3, undf_w3, map_w3)\n"
        "      USE constants_mod\n"
        "      IMPLICIT NONE\n"
        "      INTEGER(KIND=i_def), intent(in) :: nlayers\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w1\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w1) :: map_w1\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w2\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2) :: map_w2\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w3\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w3) :: map_w3\n"
        "      INTEGER(KIND=i_def), intent(in) :: undf_w1, undf_w2, undf_w3\n"
        "      REAL(KIND=r_def), intent(in) :: rscalar_1\n"
        "      INTEGER(KIND=i_def), intent(in) :: iscalar_7\n"
        "      LOGICAL(KIND=l_def), intent(in) :: lscalar_6\n"
        "      REAL(KIND=r_def), intent(inout), dimension(undf_w1) :: "
        "field_2_w1\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w2) :: "
        "field_3_w2\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w2) :: "
        "field_4_w2\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w3) :: "
        "field_5_w3\n"
        "    END SUBROUTINE testkern_three_scalars_code\n"
        "  END MODULE testkern_three_scalars_mod")

    assert expected in str(result)


def test_stub_generate_with_scalar_sums_err():
    ''' Check that the stub generator raises an exception when a kernel has
    a reduction (since these are not permitted for user-supplied kernels). '''
    with pytest.raises(ParseError) as err:
        _ = generate(
            os.path.join(BASE_PATH, "testkern_simple_with_reduction_mod.f90"),
            api=TEST_API)
    assert (
        "A user-supplied LFRic kernel must not write/update a scalar "
        "argument but kernel 'simple_with_reduction_type' has a scalar "
        "argument with 'gh_sum' access." in str(err.value))
