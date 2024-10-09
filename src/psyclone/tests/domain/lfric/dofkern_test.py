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
# Author O. Brunt, Met Office

'''
This module tests metadata validation and code generation of
user-supplied kernels operating on degrees of freedom (dofs)
'''
import os
import pytest

from fparser import api as fpapi

from psyclone.configuration import Config
from psyclone.domain.lfric import LFRicKernMetadata, LFRicKern
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.parse.utils import ParseError
from psyclone.tests.lfric_build import LFRicBuild


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use lfric as API.'''
    Config.get().api = "lfric"
    yield
    Config._instance = None


BASE_PATH = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(
                os.path.abspath(__file__)))), "test_files", "dynamo0p3")
TEST_API = "lfric"

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
    '''
    Check that we raise an exception if we encounter a dof kernel
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
    '''
    Check that we raise an exception if we find metadata for a dof kernel
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
    '''
    Check that we raise an exception if we encounter metadata
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


def test_is_dofkern():
    '''
    Check that the attribute identifying an LFRicKern instance as a user-
    defined dof kernel is set when 'operates_on' metadata arg is set to
    'dof' for a valid kernel.

    '''
    # Substitute default args for valid args
    code = CODE.replace(
        """
                    (/ arg_type(gh_field, gh_real, gh_write, w1),  &
                        arg_type(gh_field, gh_real, gh_read, w2)   &
        """,
        """
                    (/ arg_type(gh_field, gh_real, gh_write, w1),  &
                        arg_type(gh_field, gh_real, gh_read, w1)   &
        """,
        1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_dofs_type"
    # Load the metadata into an empty kernel
    md = LFRicKernMetadata(ast, name=name)
    kern = LFRicKern()
    kern.load_meta(ktype=md)
    # Assert that the identifier is set
    assert kern.iterates_over == "dof"


def test_upper_bound_undf():
    '''
    Checks that the correct upper bound is generated for a dof-kernel when
    distributed memory is set to 'False'. This should be set to the unique
    number of dofs for the single function space in the subroutine, denoted
    by the 'undf' variable.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.14_single_invoke_dofs.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    code = str(psy.gen)

    expected = ("      loop0_start = 1\n"
                "      loop0_stop = undf")

    assert expected in code


def test_upper_bounds(monkeypatch, annexed, dist_mem):
    '''
    Checks that the correct upper bound is generated for a dof-kernel for all
    permutations of the `DISTRIBUTED_MEMORY` and `COMPUTE_ANNEXED_DOFS`
    configuration settings.

    '''
    # Set up annexed dofs
    config = Config.get()
    lfric_config = config.api_conf("lfric")
    monkeypatch.setattr(lfric_config, "_compute_annexed_dofs", annexed)

    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.14_single_invoke_dofs.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(invoke_info)
    code = str(psy.gen)

    # Distributed memory
    if annexed and dist_mem:
        expected = ("      loop0_start = 1\n"
                    "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()"
                    )
    elif not annexed and dist_mem:
        expected = ("      loop0_start = 1\n"
                    "      loop0_stop = f1_proxy%vspace%get_last_dof_owned()"
                    )

    # Shared memory
    elif not annexed and not dist_mem or \
            annexed and not dist_mem:
        expected = ("      loop0_start = 1\n"
                    "      loop0_stop = undf"
                    )

    assert expected in code


def test_indexed_field_args():
    '''
    Checks that the correct array references are generated for all field
    arguments in a dof kernel. The index should be the same as the loop
    index - 'df'.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.14_single_invoke_dofs.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    code = str(psy.gen)

    expected = ("CALL testkern_dofs_code(f1_data(df), f2_data(df), "
                "f3_data(df), f4_data(df), scalar_arg, undf)")

    assert expected in code


def test_undf_initialisation():
    '''
    Test that the 'bare' undf name is used when intialising the undf
    variable for dof kernels.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.14_single_invoke_dofs.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    code = str(psy.gen)

    declaration = "INTEGER(KIND=i_def) undf"
    initalisation = "undf = f1_proxy%vspace%get_undf()"

    assert declaration in code
    assert initalisation in code


def test_compiles(tmpdir):
    '''
    Test that the code PSyclone generates from a DoF kernel compiles without
    error.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.14_single_invoke_dofs.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_function_space_bar_undf():
    '''
    Test that the correct undf name ("undf") is stored in an LFRicInvoke when
    a kernel is found to operate on 'dof'

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.14_single_invoke_dofs.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]

    assert "undf" in invoke.function_spaces._var_list
