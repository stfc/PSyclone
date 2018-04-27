# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018, Science and Technology Facilities Council
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
# ----------------------------------------------------------------------------
# Author R. W. Ford, STFC Daresbury Lab

'''Stencil tests for PSy-layer code generation that are specific to the
GOcean 1.0 API.'''

from __future__ import absolute_import
import os
import pytest
from psyclone.parse import parse
from psyclone.psyGen import PSyFactory
from psyclone.generator import GenerationError, ParseError

API = "gocean1.0"
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "gocean1p0")

# Section 1
# Tests for the case where an object of type GOStencil has not been
# initialised (i.e. the load method has not been called)

def test_not_initialised():
    '''A GOStencil object can be created in isolation and then have it's
    stencil information initialised using the load() method. If a
    GOStencil object's stencil information has not been initialised
    then asking for stencil information using the has_stencil, name
    and depth methods should return an exception. This test checks
    that an exception is raised as expected.

    '''
    from psyclone.gocean1p0 import GOStencil
    stencil = GOStencil()

    with pytest.raises(GenerationError) as excinfo:
        _ = stencil.has_stencil
    assert "ensure the load() method is called" in str(excinfo.value)

    with pytest.raises(GenerationError) as excinfo:
        _ = stencil.name
    assert "ensure the load() method is called" in str(excinfo.value)

    with pytest.raises(GenerationError) as excinfo:
        _ = stencil.depth(0,0)
    assert "ensure the load() method is called" in str(excinfo.value)

# Section 2
# Tests for the case where the load method in an object of type
# GOStencil is provided with invalid stencil information

def test_stencil_invalid_format():
    ''' xxx '''
    stencil_string = "stencil(000,011,000)"
    from fparser import api as fpapi
    from psyclone.gocean1p0 import GOKernelType1p0
    ast = fpapi.parse(os.path.join(BASE_PATH, "kernel_stencil.f90"))
    print dir(ast)
    print ast.content
    exit(1)
    metadata = GOKernelType1p0(ast, name="compute_cu")

    
# test failure if stencil info has invalid format
# test failure if name is invalid
# test failure if stencil(..) name is invalid
# test failure if nargs in stencil is not 3
# test failure if one of the args is not of length 3
# test failure if one of the args is not 0-9
# test failure if middle value is not 0 or 1
# test failure if stencil is of zero size

def test_stencil_information():
    '''Test that the GOStencil class provides the expected stencil
    information. This exercises the "pointwise" name and the stencil
    description

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "test28_invoke_kernel_stencil.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    kernel = schedule.children[0].children[0].children[0]

    # args 1 and 3 specify pointwise as a stencil access
    for idx in [0, 2]:
        pointwise_arg = kernel.args[idx]
        assert pointwise_arg.stencil
        assert not pointwise_arg.stencil.has_stencil
        assert pointwise_arg.stencil.name == "pointwise"

    # arg 4 provides grid information so knows nothing about stencils
    grid_arg = kernel.args[3]
    with pytest.raises(AttributeError) as err:
        grid_arg.stencil
    assert "object has no attribute 'stencil'" in str(err)

    # arg 2 has a stencil
    stencil_arg = kernel.args[1]
    assert stencil_arg.stencil.has_stencil
    for idx2 in range(-1,2):
        for idx1 in range(-1,2):
            if idx1 in [0, 1] and idx2 == 0:
                expected_depth = 1
            else:
                expected_depth = 0
            assert stencil_arg.stencil.depth(idx1,idx2) == expected_depth

