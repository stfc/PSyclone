# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024-2025, Science and Technology Facilities Council.
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
# Modified A. Pirrie, Met Office

'''
This module tests metadata validation and code generation of
user-supplied kernels operating on degrees of freedom (dofs)
'''
import os
import pytest

from fparser import api as fpapi

from psyclone.configuration import Config
from psyclone.domain.lfric import LFRicKernMetadata, LFRicLoop
from psyclone.dynamo0p3 import LFRicHaloExchange
from psyclone.parse.algorithm import parse
from psyclone.parse.utils import ParseError
from psyclone.psyGen import PSyFactory
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.transformations import Dynamo0p3RedundantComputationTrans


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


def test_upper_bounds(monkeypatch, annexed, dist_mem, tmpdir):
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
    elif not dist_mem:
        expected = ("      loop0_start = 1\n"
                    "      loop0_stop = undf_w1"
                    )

    assert expected in code
    # Check compilation
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_indexed_field_args(tmpdir):
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
                "f3_data(df), f4_data(df), field_vec_1_data(df), "
                "field_vec_2_data(df), field_vec_3_data(df), scalar_arg)")

    assert expected in code
    # Check compilation
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_redundant_comp_trans(tmpdir, monkeypatch):
    '''
    Check that the correct halo exchanges are added if redundant
    computation is enabled for a dof kernel called before a
    user-supplied kernel.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", True)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.14_single_invoke_dofs.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)

    first_invoke = psy.invokes.invoke_list[0]

    # Since (redundant) computation over annexed dofs is enabled, there
    # should be no halo exchange before the first kernel call
    assert isinstance(first_invoke.schedule[0], LFRicLoop)

    # Now transform the first loop to perform redundant computation out to
    # the level-3 halo
    rtrans = Dynamo0p3RedundantComputationTrans()
    rtrans.apply(first_invoke.schedule[0], options={"depth": 3})

    # There should now be a halo exchange for f2
    assert isinstance(first_invoke.schedule[0], LFRicHaloExchange)
    assert first_invoke.schedule[0].field.name == "f2"
    # Check the correct depth is set
    assert "halo_exchange(depth=3)" in str(psy.gen)
    assert "halo_exchange(depth=1)" not in str(psy.gen)

    # There should be one halo exchange for each field that is not f1
    # (f2, f3, f4)
    assert len([node for node in first_invoke.schedule.walk(LFRicHaloExchange)
                if node.field.name == "f2"]) == 1
    assert len([node for node in first_invoke.schedule.walk(LFRicHaloExchange)
                if node.field.name == "f3"]) == 1
    assert len([node for node in first_invoke.schedule.walk(LFRicHaloExchange)
                if node.field.name == "f4"]) == 1
    # Check compiles
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_multi_invoke_cell_dof_builtin(tmpdir, monkeypatch, annexed, dist_mem):
    '''
    Check that the expected code is generated from a multi-kernel invoke with
    kernels operating on different domains.

    '''
    # Set up annexed dofs
    config = Config.get()
    lfric_config = config.api_conf("lfric")
    monkeypatch.setattr(lfric_config, "_compute_annexed_dofs", annexed)

    _, invoke_info = parse(os.path.join(
        BASE_PATH, "4.17_multikernel_invokes_cell_dof_builtin.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(invoke_info)
    code = str(psy.gen)

    # Work through the generated code and compare what is expected to what is
    # generated

    # Use statements
    output = (
        "      USE testkern_mod, ONLY: testkern_code\n"
        "      USE testkern_dofs_mod, ONLY: testkern_dofs_code\n"
        )
    if dist_mem:
        # Check mesh_mod is added to use statements
        output += ("      USE mesh_mod, ONLY: mesh_type\n")
    assert output in code

    # Consistent declarations
    output = (
        "      REAL(KIND=r_def), intent(in) :: scalar_arg, a\n"
        "      TYPE(field_type), intent(in) :: f1, f2, f3, f4, "
        "field_vec(3), m1, m2\n"
        "      INTEGER(KIND=i_def) cell\n"
        "      INTEGER(KIND=i_def) df\n"
        "      INTEGER(KIND=i_def) loop2_start, loop2_stop\n"
        "      INTEGER(KIND=i_def) loop1_start, loop1_stop\n"
        "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
        "      INTEGER(KIND=i_def) nlayers_f1\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: m2_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: m1_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: field_vec_1_data =>"
        " null(), field_vec_2_data => null(), field_vec_3_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f4_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f3_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f2_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f1_data => null()\n"
        )
    assert output in code

    # Check that dof kernel is called correctly
    output = (
        "      DO df = loop0_start, loop0_stop, 1\n"
        "        CALL testkern_dofs_code(f1_data(df), f2_data(df), "
        "f3_data(df), f4_data(df), field_vec_1_data(df), "
        "field_vec_2_data(df), field_vec_3_data(df), scalar_arg)\n"
        "      END DO\n"
    )
    assert output in code

    # Now check that correct halo exchanges and set clean/dirty are
    # performed after the dof kernel call and before the next kernel (cell)
    if dist_mem:
        if not annexed:
            # Check f1 field has halo exchange performed when annexed == False
            output = (
                "      DO df = loop0_start, loop0_stop, 1\n"
                "        CALL testkern_dofs_code(f1_data(df), f2_data(df), "
                "f3_data(df), f4_data(df), field_vec_1_data(df), "
                "field_vec_2_data(df), field_vec_3_data(df), scalar_arg)\n"
                "      END DO\n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !\n"
                "      CALL f1_proxy%halo_exchange(depth=1)\n"
                )
        else:
            # Check f1 field is set dirty but no halo exchange is performed
            output = (
                "      DO df = loop0_start, loop0_stop, 1\n"
                "        CALL testkern_dofs_code(f1_data(df), f2_data(df), "
                "f3_data(df), f4_data(df), field_vec_1_data(df), "
                "field_vec_2_data(df), field_vec_3_data(df), scalar_arg)\n"
                "      END DO\n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !\n"
                )
        # This should be present in all distributed memory cases:
        # Check halos are set dirty/clean for modified fields in dof
        # kernel (above) and happen before the next kernel (cell_column)
        common_halo_exchange_code = (
                "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
                "        CALL f2_proxy%halo_exchange(depth=1)\n"
                "      END IF\n"
                "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
                "        CALL m1_proxy%halo_exchange(depth=1)\n"
                "      END IF\n"
                "      IF (m2_proxy%is_dirty(depth=1)) THEN\n"
                "        CALL m2_proxy%halo_exchange(depth=1)\n"
                "      END IF\n"
                "      DO cell = loop1_start, loop1_stop, 1\n"
                "        CALL testkern_code"
                )
        output += common_halo_exchange_code     # Append common
        assert output in code

    # Check cell-column kern is called correctly
    output = (
        "      DO cell = loop1_start, loop1_stop, 1\n"
        "        CALL testkern_code(nlayers_f1, a, f1_data, f2_data, m1_data, "
        "m2_data, ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
        "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))\n"
        "      END DO\n"
    )
    assert output in code

    # Check built-in is called correctly
    output = (
        "      DO df = loop2_start, loop2_stop, 1\n"
        "        ! Built-in: inc_aX_plus_Y (real-valued fields)\n"
        "        f1_data(df) = 0.5_r_def * f1_data(df) + f2_data(df)\n"
        "      END DO\n"
    )
    assert output in code

    assert LFRicBuild(tmpdir).code_compiles(psy)
