# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2019, Science and Technology Facilities Council.
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
# -----------------------------------------------------------------------------

'''This module tests the PSyclone Dynamo 0.1 API.  '''

import pytest
from fparser.api import parse as parse1
from psyclone.parse.utils import ParseError
from psyclone.configuration import Config
from psyclone.dynamo0p1 import DynDescriptor, DynKernelType


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use dynamo0.1 as API.'''
    Config.get().api = "dynamo0.1"


def test_dyndescriptor():
    '''Test that a dynamo0.1 api-specific DynDescriptor class can be
    created successfully.

    '''
    tmp = DynDescriptor("gh_rw", "v1", "fe", ".false.", ".false.", ".false.")
    assert tmp.access == "gh_rw"
    assert tmp.function_space == "v1"
    assert tmp.stencil == "fe"
    assert tmp.basis == ".false."
    assert tmp.diff_basis == ".false."
    assert tmp.gauss_quad == ".false."


CODE = (
    "module testkern_mod\n"
    "  type, extends(kernel_type) :: testkern_type\n"
    "     type(arg_type), dimension(1) :: meta_args =    &\n"
    "          (/ arg_type(gh_rw,v1,fe,.false.,.false.,.false.) &\n"
    "           /)\n"
    "     integer, parameter :: iterates_over = cells\n"
    "   contains\n"
    "     procedure() :: code => testkern_code\n"
    "  end type testkern_type\n"
    "contains\n"
    "\n"
    "  subroutine testkern_code()\n"
    "  end subroutine testkern_code\n"
    "end module testkern_mod\n")


def test_dynkerneltype():
    '''Test that a dynamo0.1 api-specific DynKernelType class can be
    created successfully.

    '''
    parse_tree = parse1(CODE)
    tmp = DynKernelType(parse_tree)
    assert tmp.name == "testkern_type"
    assert tmp.iterates_over == "cells"
    assert tmp.nargs == 1
    descriptor = tmp.arg_descriptors[0]
    assert descriptor.access == "gh_rw"
    assert descriptor.function_space == "v1"
    assert descriptor.stencil == "fe"
    assert descriptor.basis == ".false."
    assert descriptor.diff_basis == ".false."
    assert descriptor.gauss_quad == ".false."


def test_dynkerneltype_argtype():
    '''Test that a dynamo0.1 api-specific DynKernelType class raises an
    exception if the meta_arg value is not named 'arg_type'.

    '''

    my_code = CODE.replace("arg_type(", "invalid(")
    parse_tree = parse1(my_code)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernelType(parse_tree)
    assert ("Each meta_arg value must be of type 'arg_type' for the "
            "dynamo0.1 api, but found 'invalid'.") in str(excinfo.value)
