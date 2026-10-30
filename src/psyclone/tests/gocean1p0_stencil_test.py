# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------


'''Stencil tests for PSy-layer code generation that are specific to the
GOcean 1.0 API.'''

import os
import pytest

from psyclone.configuration import Config
from psyclone.domain.gocean.kernel import (
    GOceanPointwiseMetadata, GOceanStencilMetadata)
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory

from psyclone.tests.gocean_build import GOceanBuild

API = "gocean"
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "gocean1p0")


@pytest.fixture(scope="function", autouse=True)
def setup():
    '''Make sure that all tests here use gocean as API.'''
    Config.get().api = "gocean"


def test_stencil_information(tmpdir):
    '''Test that kernel arguments provide the expected pointwise or explicit
    stencil metadata.
    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "test28_invoke_kernel_stencil.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    kernel = schedule.coded_kernels()[0]

    # args 1 and 3 specify pointwise as a stencil access
    for idx in [0, 2]:
        pointwise_arg = kernel.args[idx]
        assert isinstance(pointwise_arg.stencil, GOceanPointwiseMetadata)

    # arg 4 provides grid information so knows nothing about stencils
    grid_arg = kernel.args[3]
    with pytest.raises(AttributeError) as excinfo:
        _ = grid_arg.stencil
    assert "object has no attribute 'stencil'" in str(excinfo.value)

    # arg 2 has a stencil
    stencil_arg = kernel.args[1]
    assert isinstance(stencil_arg.stencil, GOceanStencilMetadata)
    assert stencil_arg.stencil.rows == ("000", "011", "000")

    assert GOceanBuild(tmpdir).code_compiles(psy)
