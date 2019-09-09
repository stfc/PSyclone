# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2016-2019, Science and Technology Facilities Council
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
# Author A. Porter, STFC Daresbury Laboratory
# Modified by R. W. Ford, STFC Daresbury Laboratory

''' This module tests the GOcean 0.1 API using pytest. '''

from __future__ import absolute_import, print_function
import os
import pytest
from fparser.api import parse as parse1
from psyclone.configuration import Config
from psyclone.core.access_type import AccessType
from psyclone.psyGen import PSyFactory
from psyclone.gocean0p1 import GODescriptor, GOKernelType
from psyclone.parse.algorithm import parse
from psyclone.parse.utils import ParseError

API = "gocean0.1"


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use gocean0.1 as API.'''
    Config.get().api = "gocean0.1"


def test_loop_bounds_gen_multiple_loops():
    ''' Test that we only generate one assignment for a loop-bounds
    variable when we have multiple loops '''
    _, info = parse(os.path.join(os.path.
                                 dirname(os.path.abspath(__file__)),
                                 "test_files", "gocean0p1",
                                 "openmp_fuse_test.f90"),
                    api=API)
    psy = PSyFactory(API).create(info)
    gen = str(psy.gen)

    expected = (
        "      DO j=1,SIZE(uold,2)\n"
        "        DO i=1,SIZE(uold,1)\n"
        "          CALL time_smooth_code(i, j, u, unew, uold)\n"
        "        END DO \n"
        "      END DO \n"
        "      DO j=1,SIZE(vold,2)\n"
        "        DO i=1,SIZE(vold,1)\n"
        "          CALL time_smooth_code(i, j, v, vnew, vold)\n"
        "        END DO \n"
        "      END DO \n"
        "      DO j=1,SIZE(pold,2)\n"
        "        DO i=1,SIZE(pold,1)\n"
        "          CALL time_smooth_code(i, j, p, pnew, pold)\n"
        "        END DO \n"
        "      END DO ")
    assert expected in gen


def test_gobuiltin_call_factory():
    ''' Test that the GOBuiltInCallFactory does nothing in version 0.1
    of the GOcean API '''
    from psyclone.gocean0p1 import GOBuiltInCallFactory
    # pylint:disable=assignment-from-none
    builtin = GOBuiltInCallFactory.create()
    # pylint:enable=assignment-from-none
    assert builtin is None


def test_godescriptor():
    '''Test that a GOcean descriptor class can be created
    successfully.

    '''
    tmp = GODescriptor("read", "every", "pointwise")
    assert tmp.access == AccessType.READ
    assert tmp.function_space == "every"
    assert tmp.stencil == "pointwise"


CODE = (
    "module test_mod\n"
    "  type, extends(kernel_type) :: test_type\n"
    "    type(arg_type), dimension(1) :: meta_args =    &\n"
    "          (/ arg(READ,EVERY,POINTWISE) /)\n"
    "     integer :: iterates_over = dofs\n"
    "   contains\n"
    "     procedure, nopass :: code => test_code\n"
    "  end type test_type\n"
    "contains\n"
    "  subroutine test_code()\n"
    "  end subroutine test_code\n"
    "end module test_mod\n"

    )


def test_gokerneltype():
    '''Test that a GOcean kerneltype class can be created
    succesfully.

    '''
    my_code = CODE
    parse_tree = parse1(my_code)
    tmp = GOKernelType(parse_tree)
    assert tmp.iterates_over == "dofs"
    assert tmp.nargs == 1
    assert tmp.name == "test_type"
    descriptor = tmp.arg_descriptors[0]
    assert descriptor.access == AccessType.READ
    assert descriptor.function_space == "every"
    assert descriptor.stencil == "pointwise"


def test_gokerneltype_argname():
    '''Test that a GOcean kerneltype class raises an appropriate exception
    if the meta_arg name is not 'arg'.

    '''
    my_code = CODE.replace("arg(", "invalid(")
    parse_tree = parse1(my_code)
    with pytest.raises(ParseError) as excinfo:
        _ = GOKernelType(parse_tree)
    assert ("Each meta_arg value must be of type 'arg' for the gocean0.1 "
            "api, but found 'invalid'.") in str(excinfo.value)


def test_gokerneltype_nargs():
    '''Test that a GOcean kerneltype class raises an appropriate exception
    if a meta_arg has more than 3 entries.

    '''
    my_code = CODE.replace("E)", "E, INVALID)")
    parse_tree = parse1(my_code)
    with pytest.raises(ParseError) as excinfo:
        _ = GOKernelType(parse_tree)
    assert "'arg' type expects 3 arguments but found 4" \
        in str(excinfo.value)
