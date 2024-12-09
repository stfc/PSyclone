# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024, Science and Technology Facilities Council.
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
# Authors O. Brunt, Met Office

'''
This module tests the metadata validation in LFRicKernMetadata of
user-supplied kernels operating on degrees of freedom (dofs)
'''

import pytest

from fparser import api as fpapi

from psyclone.configuration import Config
from psyclone.domain.lfric import LFRicKernMetadata
from psyclone.parse.utils import ParseError


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use lfric as API.'''
    Config.get().api = "lfric"
    yield
    Config._instance = None


CODE = '''
        module testkern_dofs_mod
            type, extends(kernel_type) :: testkern_dofs_type
                type(arg_type), dimension(2) :: meta_args =        &
                    (/ arg_type(gh_field, gh_real, gh_write, w1),  &
                        arg_type(gh_field, gh_real, gh_read, w2)   &
                    /)
                integer :: operates_on = dof
            contains
                procedure, nopass :: code => testkern_dofs_code
            end type testkern_dofs_type

        contains

        subroutine testkern_dofs_code(a, b)
        end subroutine testkern_dofs_code

        end module testkern_dofs_mod
        '''


def test_dof_kernel_mixed_function_spaces():
    ''' Check that we raise an exception if we encounter a dof kernel
    call with arguments of different function spaces.

    '''

    ast = fpapi.parse(CODE, ignore_comments=False)
    name = "testkern_dofs_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("Kernel 'testkern_dofs_type' operates on 'dof' but has "
            "fields on different function spaces: ['w1', 'w2']. This is not "
            "permitted in the LFRic API."
            in str(excinfo.value))


def test_dof_kernel_invalid_arg():
    ''' Check that we raise an exception if we find metadata for a dof kernel
    which specifies arguments that are not fields or scalars.

    '''
    # Substitute field for operator, an invalid arg type for dof kernels
    code = CODE.replace(
        """
                    (/ arg_type(gh_field, gh_real, gh_write, w1),  &
                        arg_type(gh_field, gh_real, gh_read, w2)   &
        """,
        """
                   (/ arg_type(gh_operator, gh_real, gh_write, w1, w1), &
                      arg_type(gh_scalar, gh_real, gh_read)             &
        """,
        1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_dofs_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("In the LFRic API a kernel that operates on 'dof' is only "
            "permitted to accept scalar and field arguments but the "
            "metadata for kernel 'testkern_dofs_type' includes an "
            "argument of type 'gh_operator'"
            in str(excinfo.value))


def test_dof_kernel_invalid_field_vector():
    ''' Check that we raise an exception if we encounter metadata
    for a dof kernel with a field vector.

    '''
    # Substitute field for field vector
    code = CODE.replace(
        """
                    (/ arg_type(gh_field, gh_real, gh_write, w1),  &
                        arg_type(gh_field, gh_real, gh_read, w2)   &
        """,
        """
                   (/ arg_type(gh_field*3, gh_real, gh_write, w1), &
                      arg_type(gh_scalar, gh_real, gh_read)  &
        """,
        1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_dofs_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("Kernel 'testkern_dofs_type' operates on 'dof' but has a vector "
            "argument 'gh_field*3'. This is not permitted in the LFRic API."
            in str(excinfo.value))
