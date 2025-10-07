# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic, A. Coughtrie, L. Turner and O. Brunt, Met Office
# Modified J. Henrichs, Bureau of Meteorology
# Modified A. B. G. Chalk and N. Nobre, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''Module containing pytest tests for the LFRicGlobalReduction class.'''

import os
from pathlib import Path
import pytest

from psyclone.core import AccessType
from psyclone.domain.common.psylayer.global_reduction import ReductionOp
from psyclone.domain.lfric.lfric_global_reduction import LFRicGlobalReduction
from psyclone.errors import GenerationError, InternalError
from psyclone.tests.utilities import get_invoke

BASE_PATH = Path(os.path.dirname(os.path.abspath(__file__)))
BASE_PATH = BASE_PATH / ".." / ".." / "test_files" / "lfric"

TEST_API = "lfric"


def test_lfricglobalreduction_unsupported_argument():
    ''' Check that an instance of the LFRicGlobalReduction class raises an
    exception for an unsupported argument type. '''
    # Get an instance of a non-scalar argument
    _, invoke = get_invoke("1.6.1_single_invoke_1_int_scalar.f90", TEST_API,
                           dist_mem=True, idx=0)
    schedule = invoke.schedule
    loop = schedule.children[4]
    kernel = loop.loop_body[0]
    argument = kernel.arguments.args[0]
    with pytest.raises(InternalError) as err:
        _ = LFRicGlobalReduction(ReductionOp.SUM, argument)
    assert ("LFRicGlobalReduction.init(): A global reduction argument should "
            "be a scalar but found argument of type 'gh_field'."
            in str(err.value))


def test_lfricglobalreduction_unsupported_scalar():
    ''' Check that an instance of the LFRicGlobalReduction class raises an
    exception if an unsupported scalar type is provided when distributed
    memory is enabled (dm=True).

    '''
    # Get an instance of an integer scalar
    _, invoke = get_invoke("1.6.1_single_invoke_1_int_scalar.f90",
                           TEST_API, dist_mem=True, idx=0)
    schedule = invoke.schedule
    loop = schedule.children[4]
    kernel = loop.loop_body[0]
    argument = kernel.arguments.args[1]
    with pytest.raises(GenerationError) as err:
        _ = LFRicGlobalReduction(ReductionOp.SUM, argument)
    assert ("LFRicGlobalReduction currently only supports real scalars, but "
            "argument 'iflag' in Kernel 'testkern_one_int_scalar_code' "
            "has 'integer' intrinsic type." in str(err.value))


def test_lfricglobalreduction_nodm_error():
    ''' Check that an instance of the LFRicGlobalReduction class raises an
    exception if it is instantiated with no distributed memory enabled
    (dm=False).

    '''
    # Get an instance of a real scalar
    _, invoke = get_invoke("1.9_single_invoke_2_real_scalars.f90",
                           TEST_API, dist_mem=False, idx=0)
    schedule = invoke.schedule
    loop = schedule.children[0]
    kernel = loop.loop_body[0]
    argument = kernel.arguments.args[0]
    with pytest.raises(GenerationError) as err:
        _ = LFRicGlobalReduction(ReductionOp.SUM, argument)
    assert ("It makes no sense to create an LFRicGlobalReduction object when "
            "distributed memory is not enabled (dm=False)."
            in str(err.value))


def test_globalreduction_arg():
    ''' Check that the globalreduction operand is defined as gh_readwrite and
    points to the GlobalSum node '''
    _, invoke = get_invoke("15.14.3_sum_setval_field_builtin.f90",
                           api="lfric", dist_mem=True, idx=0)
    schedule = invoke.schedule
    glob_sum = schedule.children[2]
    glob_sum_arg = glob_sum.operand
    assert glob_sum_arg.access == AccessType.READWRITE
    assert glob_sum_arg.call == glob_sum


def test_globalreduction_args():
    '''Test that the globalreduction class args method returns the appropriate
    argument '''
    _, invoke = get_invoke("15.14.3_sum_setval_field_builtin.f90",
                           api="lfric", dist_mem=True, idx=0)
    schedule = invoke.schedule
    global_sum = schedule.children[2]
    assert len(global_sum.args) == 1
    assert global_sum.args[0] == global_sum.operand
