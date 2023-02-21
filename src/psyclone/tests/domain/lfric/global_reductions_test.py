# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2023, Science and Technology Facilities Council.
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
# Modified I. Kavcic and A. Coughtrie, Met Office,
#          C. M. Maynard, Met Office/University of Reading,
#          J. Henrichs, Bureau of Meteorology.

''' Module containing tests LFRic global reduction classes.
'''

import os
import pytest

from psyclone.domain.lfric.global_reductions import LFRicGlobalReduction
from psyclone.errors import GenerationError, InternalError
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory


# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "dynamo0p3")

# The PSyclone API under test
TEST_API = "dynamo0.3"


def test_lfric_global_reduction_unsupported_argument():
    ''' Check that an instance of the LFRicGlobalReduction class raises
    an exception for an unsupported argument type. '''
    # Get an instance of a non-scalar argument
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.6.1_single_invoke_1_int_scalar.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[4]
    kernel = loop.loop_body[0]
    argument = kernel.arguments.args[0]
    with pytest.raises(InternalError) as err:
        _ = LFRicGlobalReduction(argument)
    assert ("LFRicGlobalReduction.init(): A global reduction argument "
            "should be a scalar but found argument of type 'gh_field'."
            in str(err.value))


def test_lfric_global_reduction_unsupported_scalar():
    ''' Check that an instance of the LFRicGlobalReduction class raises an
    exception if an unsupported scalar type is provided when distributed
    memory is enabled (dm=True).

    '''
    # Get an instance of an integer scalar
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.6.1_single_invoke_1_int_scalar.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[4]
    kernel = loop.loop_body[0]
    argument = kernel.arguments.args[1]
    with pytest.raises(GenerationError) as err:
        _ = LFRicGlobalReduction(argument)
    assert ("LFRicGlobalReduction currently only supports real scalars, but "
            "argument 'iflag' in Kernel 'testkern_one_int_scalar_code' "
            "has 'integer' intrinsic type." in str(err.value))


def test_lfric_global_reduction_nodm_error():
    ''' Check that an instance of the LFRicGlobalReduction class raises an
    exception if it is instantiated with no distributed memory enabled
    (dm=False).

    '''
    # Get an instance of a real scalar
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.9_single_invoke_2_real_scalars.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[0]
    kernel = loop.loop_body[0]
    argument = kernel.arguments.args[0]
    with pytest.raises(GenerationError) as err:
        _ = LFRicGlobalReduction(argument)
    assert ("It makes no sense to create an LFRicGlobalReduction object "
            "when distributed memory is not enabled (dm=False)."
            in str(err.value))
