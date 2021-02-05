# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Author: I. Kavcic, Met Office

'''
Module containing pytest tests for PSy-layer code generation and the related
functionality for the LFRic scalar arguments.
'''

# Imports
from __future__ import absolute_import, print_function
import os
import pytest
from psyclone.domain.lfric import LFRicArgDescriptor
from psyclone.dynamo0p3 import LFRicScalarArgs
from psyclone.f2pygen import ModuleGen
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.errors import InternalError, GenerationError

# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "dynamo0p3")
TEST_API = "dynamo0.3"

# General scalar checks (argument type, data type, etc)


def test_lfricscalars_call_err():
    ''' Check that the LFRicScalarArgs constructor raises the expected
    internal error if it encounters an unrecognised intrinsic type of
    scalar when generating a kernel call.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.7_single_invoke_2scalar.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    kernel = invoke.schedule.coded_kernels()[0]
    # Sabotage the scalar argument to make it have an invalid intrinsic type
    scalar_arg = kernel.arguments.args[0]
    scalar_arg._intrinsic_type = "double-type"
    with pytest.raises(InternalError) as err:
        LFRicScalarArgs(invoke)._invoke_declarations(ModuleGen(name="my_mod"))
    assert ("Found unsupported intrinsic types in Invoke "
            "'invoke_0_testkern_two_scalars_type' declarations for the "
            "scalar arguments ['a']. Supported types are "
            "['real', 'integer']." in str(err.value))


def test_int_real_scalar_invalid():
    ''' Tests that the same scalar cannot have different data types
    in different kernels within the same Invoke.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "4.16_multikernel_invokes_real_int_scalar_invalid.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)

    with pytest.raises(GenerationError) as err:
        _ = psy.gen
    assert ("At least one scalar (['b']) in Invoke "
            "'invoke_real_and_integer_scalars' has different metadata for "
            "data type (['gh_real', 'gh_integer']) in different kernels. "
            "This is invalid." in str(err.value))


def test_scalar_invoke_uniq_declns_valid_intrinsic():
    ''' Tests that all valid access valid intrinsic types for
    user-defined scalar aarguments ('real' and 'integer') are accepted
    by Invoke.unique_declarations().

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]

    # Test 'real' scalars
    scalars_real_args = invoke.unique_declarations(
        LFRicArgDescriptor.VALID_SCALAR_NAMES, intrinsic_type="real")
    scalars_real = [arg.declaration_name for arg in scalars_real_args]
    assert scalars_real == ["a"]

    # Test 'integer' scalars
    scalars_int_args = invoke.unique_declarations(
        LFRicArgDescriptor.VALID_SCALAR_NAMES, intrinsic_type="integer")
    scalars_int = [arg.declaration_name for arg in scalars_int_args]
    assert scalars_int == ["istep"]
