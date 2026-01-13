# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab
# Modified: I. Kavcic, L. Turner and O. Brunt, Met Office
# Modified: J. Henrichs, Bureau of Meteorology

''' This module tests the LFRicDofmaps class found within the LFRic
domain. Tests here have been pulled from the LFRic_cma_test.py
test file. '''

import os
import pytest

from psyclone.domain.lfric.lfric_dofmaps import LFRicDofmaps
from psyclone.errors import GenerationError, InternalError
from psyclone.gen_kernel_stub import generate
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory

# Constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         os.pardir, os.pardir, "test_files", "lfric")
TEST_API = "lfric"


# Error tests
def test_lfricdofmap_stubdecln_err():
    '''
    Check that LFRicDofmaps.stub_declarations raises the expected errors
    if the stored CMA information is invalid.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "20.5_multi_cma_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)

    # Test invalid cbanded kernel
    kernel_cbanded = psy.invokes.invoke_list[0].schedule.kernels()[0]
    # Need and nlayers symbols because it is looked-up by the LFRicDofmaps
    kernel_cbanded._stub_symbol_table.find_or_create("nlayers")
    dofmaps = LFRicDofmaps(kernel_cbanded)
    for cma in dofmaps._unique_cbanded_maps.values():
        cma["direction"] = "not-a-direction"
    with pytest.raises(InternalError) as err:
        dofmaps.stub_declarations()
    assert ("Invalid direction ('not-a-direction') found for CMA operator "
            "when collecting column-banded dofmaps" in str(err.value))

    # Test invalid direction kernel
    kernel_direction = psy.invokes.invoke_list[0].schedule.kernels()[1]
    # Need and nlayers symbols because it is looked-up by the LFRicDofmaps
    kernel_direction._stub_symbol_table.find_or_create("nlayers")
    dofmaps = LFRicDofmaps(kernel_direction)
    for cma in dofmaps._unique_indirection_maps.values():
        cma["direction"] = "not-a-direction"
    with pytest.raises(InternalError) as err:
        dofmaps.stub_declarations()
    assert ("Invalid direction ('not-a-direction') found for CMA operator "
            "when collecting indirection dofmaps" in str(err.value))


def test_cma_asm_cbanded_dofmap_error():
    '''
    Check that we raise expected internal error if LFRicDofmaps
    encounters an assembly kernel that has more than one CMA op argument.

    Tests cma operations flagged as "assembly" have only one operator,
    the "gh_columnwise_operator".

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "20.0_cma_assembly.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    calls = invoke.schedule.kernels()
    # We must go in and make the internal state inconsistent in order
    # to trigger the error. So, we set the type of all the arguments
    # in the kernel call to be CMA operators...
    for arg in calls[0].arguments.args:
        arg._argument_type = 'gh_columnwise_operator'
    with pytest.raises(GenerationError) as excinfo:
        invoke.dofmaps.__init__(invoke)
    assert ("Internal error: there should only be one CMA operator argument "
            "for a CMA assembly kernel but found 2") in str(excinfo.value)


def test_cma_apply_indirection_dofmap_error():
    '''
    Check that we raise expected internal error if LFRicDofmaps
    encounters an apply kernel that has more than one CMA op argument.

    Tests cma operations flagged as "apply" have only one operator,
    the "gh_columnwise_operator".

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "20.1_cma_apply.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    calls = invoke.schedule.kernels()
    # We must go in and make the internal state inconsistent in order
    # to trigger the error. So, we set the type of all the arguments
    # in the kernel cal to be CMA operators...
    for arg in calls[0].arguments.args:
        arg._argument_type = 'gh_columnwise_operator'
    with pytest.raises(GenerationError) as excinfo:
        invoke.dofmaps.__init__(invoke)
    assert ("Internal error: there should only be one CMA operator argument "
            "for a kernel that applies a CMA operator but found 3"
            in str(excinfo.value))


# Generation tests
def test_cbanded_test_comments():
    '''
    Check that LFRicDofmaps generates the correct comments for an "assembly"
    cma operation.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "20.0_cma_assembly.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    code = str(psy.gen)

    output = (
        "\n"
        "    ! Look-up required column-banded dofmaps\n"
    )

    assert output in code


def test_unique_fs_comments():
    '''
    Check that LFRicDofmaps generates the correct comments for an "apply"
    cma operation.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "20.1_cma_apply.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    code = str(psy.gen)

    output = (
        "\n"
        "    ! Look-up dofmaps for each function space\n"
        )

    assert output in code


def test_stub_decl_dofmaps():
    '''
    Check that LFRicDofmaps generates the expected declarations in the stub.

    '''

    stub_text = generate(os.path.join(BASE_PATH,
                                      "columnwise_op_asm_kernel_mod.F90"),
                         api=TEST_API)

    assert "integer(kind=i_def), intent(in) :: cma_op_2_ncol" in stub_text
    assert "integer(kind=i_def), intent(in) :: cma_op_2_nrow" in stub_text


def test_lfricdofmaps_stub_gen():
    '''
    Test the kernel-stub generator for a CMA apply kernel. This has
    two fields and one CMA operator as arguments.

    '''
    stub_text = generate(os.path.join(BASE_PATH,
                                      "columnwise_op_app_kernel_mod.F90"),
                         api=TEST_API)

    expected = (
        "  subroutine columnwise_op_app_kernel_code(cell, ncell_2d, "
        "field_1_as1_field_1, field_2_as2_field_2, cma_op_3, "
        "cma_op_3_nrow, cma_op_3_ncol, cma_op_3_bandwidth, cma_op_3_alpha, "
        "cma_op_3_beta, cma_op_3_gamma_m, cma_op_3_gamma_p, "
        "ndf_as1_field_1, undf_as1_field_1, map_as1_field_1, "
        "cma_indirection_map_as1_field_1, ndf_as2_field_2, "
        "undf_as2_field_2, map_as2_field_2, "
        "cma_indirection_map_as2_field_2)\n"
        )
    assert expected in stub_text
