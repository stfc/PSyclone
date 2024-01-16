# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
# Authors A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified J. Henrichs, Bureau of Meteorology
# Modified R. W. Ford, STFC Daresbury Lab
# Modified I. Kavcic, Met Office

'''Tests for PSy-layer code generation that are specific to the
GOcean 1.0 API.'''

import os
import re

import pytest

from psyclone.configuration import Config
from psyclone.parse.algorithm import Arg, parse
from psyclone.parse.kernel import Descriptor
from psyclone.parse.utils import ParseError
from psyclone.errors import InternalError, GenerationError
from psyclone.psyGen import PSyFactory
from psyclone.gocean1p0 import (GOKern, GOLoop, GOKernelArgument,
                                GOKernelArguments, GOKernelGridArgument,
                                GOBuiltInCallFactory)
from psyclone.tests.utilities import get_base_path, get_invoke
from psyclone.tests.gocean_build import GOceanBuild
from psyclone.psyir.nodes import (Node, StructureReference, Member,
                                  StructureMember, Reference, Literal)
from psyclone.psyir.symbols import (
    ContainerSymbol, ImportInterface, SymbolTable,
    INTEGER_TYPE, DataTypeSymbol, ScalarType)
from psyclone.domain.gocean.transformations import GOConstLoopBoundsTrans

API = "gocean1.0"
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "gocean1p0")


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use gocean1.0 as API.'''
    Config.get().api = "gocean1.0"
    yield
    Config._instance = None


def test_field(tmpdir, dist_mem):
    ''' Tests that a kernel call with only fields produces correct code '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    generated_code = str(psy.gen)

    before_kernel = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_compute_cu(cu_fld, p_fld, u_fld)\n"
        "      USE compute_cu_mod, ONLY: compute_cu_code\n"
        "      TYPE(r2d_field), intent(inout) :: cu_fld\n"
        "      TYPE(r2d_field), intent(inout) :: p_fld\n"
        "      TYPE(r2d_field), intent(inout) :: u_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n\n")
    remaining_code = (
        "      DO j = cu_fld%internal%ystart, cu_fld%internal%ystop, 1\n"
        "        DO i = cu_fld%internal%xstart, cu_fld%internal%xstop, 1\n"
        "          CALL compute_cu_code(i, j, cu_fld%data, p_fld%data, "
        "u_fld%data)\n"
        "        END DO\n"
        "      END DO\n\n"
        "    END SUBROUTINE invoke_0_compute_cu\n"
        "  END MODULE psy_single_invoke_test")

    if dist_mem:
        # Fields that read and have a stencil access insert a halo exchange,
        # in this case p_fld is a stencil but u_fld is pointwise.
        halo_exchange_code = (
            "      CALL p_fld%halo_exchange(1)\n")
        expected_output = before_kernel + halo_exchange_code + remaining_code
    else:
        expected_output = before_kernel + remaining_code

    assert generated_code == expected_output
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_two_kernels(tmpdir, dist_mem):
    ''' Tests that an invoke containing two kernel calls with only
    fields as arguments produces correct code '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke_two_kernels.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    generated_code = psy.gen

    before_kernels = (
        "  MODULE psy_single_invoke_two_kernels\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0(cu_fld, p_fld, u_fld, unew_fld, "
        "uold_fld)\n"
        "      USE compute_cu_mod, ONLY: compute_cu_code\n"
        "      USE time_smooth_mod, ONLY: time_smooth_code\n"
        "      TYPE(r2d_field), intent(inout) :: cu_fld\n"
        "      TYPE(r2d_field), intent(inout) :: p_fld\n"
        "      TYPE(r2d_field), intent(inout) :: u_fld\n"
        "      TYPE(r2d_field), intent(inout) :: unew_fld\n"
        "      TYPE(r2d_field), intent(inout) :: uold_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n\n")
    first_kernel = (
        "      DO j = cu_fld%internal%ystart, cu_fld%internal%ystop, 1\n"
        "        DO i = cu_fld%internal%xstart, cu_fld%internal%xstop, 1\n"
        "          CALL compute_cu_code(i, j, cu_fld%data, p_fld%data, "
        "u_fld%data)\n"
        "        END DO\n"
        "      END DO\n")
    second_kernel = (
        "      DO j = 1, SIZE(uold_fld%data, 2), 1\n"
        "        DO i = 1, SIZE(uold_fld%data, 1), 1\n"
        "          CALL time_smooth_code(i, j, u_fld%data, unew_fld%data, "
        "uold_fld%data)\n"
        "        END DO\n"
        "      END DO\n\n"
        "    END SUBROUTINE invoke_0\n"
        "  END MODULE psy_single_invoke_two_kernels")
    if dist_mem:
        # In this case the second kernel just has pointwise accesses, so it
        # doesn't add any halo exchange.
        halos_first_kernel = "      CALL p_fld%halo_exchange(1)\n"
        expected_output = before_kernels + halos_first_kernel + first_kernel \
            + second_kernel
    else:
        expected_output = before_kernels + first_kernel + second_kernel

    assert str(generated_code) == expected_output
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_two_kernels_with_dependencies(tmpdir, dist_mem):
    ''' Tests that an invoke containing two kernel calls with dependencies
    between them produces correct code '''
    _, invoke_info = parse(os.path.join(os.path.dirname(
        os.path.abspath(__file__)), "test_files", "gocean1p0",
        "single_invoke_two_identical_kernels.f90"), api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    generated_code = psy.gen

    before_kernels = (
        "  MODULE psy_single_invoke_two_kernels\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0(cu_fld, p_fld, u_fld)\n"
        "      USE compute_cu_mod, ONLY: compute_cu_code\n"
        "      TYPE(r2d_field), intent(inout) :: cu_fld\n"
        "      TYPE(r2d_field), intent(inout) :: p_fld\n"
        "      TYPE(r2d_field), intent(inout) :: u_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n\n")
    first_kernel = (
        "      DO j = cu_fld%internal%ystart, cu_fld%internal%ystop, 1\n"
        "        DO i = cu_fld%internal%xstart, cu_fld%internal%xstop, 1\n"
        "          CALL compute_cu_code(i, j, cu_fld%data, p_fld%data, "
        "u_fld%data)\n"
        "        END DO\n"
        "      END DO\n")
    second_kernel = (
        "      DO j = p_fld%internal%ystart, p_fld%internal%ystop, 1\n"
        "        DO i = p_fld%internal%xstart, p_fld%internal%xstop, 1\n"
        "          CALL compute_cu_code(i, j, p_fld%data, cu_fld%data,"
        " u_fld%data)\n"
        "        END DO\n"
        "      END DO\n\n"
        "    END SUBROUTINE invoke_0\n"
        "  END MODULE psy_single_invoke_two_kernels")

    if dist_mem:
        # In this case the second kernel just has a RaW dependency on the
        # cu_fld of the first kernel, so a halo exchange should be inserted
        # between the kernels in addition to the initial p_fld halo exchange.
        halos_first_kernel = "      CALL p_fld%halo_exchange(1)\n"
        halos_second_kernel = "      CALL cu_fld%halo_exchange(1)\n"
        expected_output = before_kernels + halos_first_kernel + first_kernel \
            + halos_second_kernel + second_kernel
    else:
        expected_output = before_kernels + first_kernel + second_kernel

    assert str(generated_code) == expected_output
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_grid_property(tmpdir, dist_mem):
    ''' Tests that an invoke containing a kernel call requiring
    a property of the grid produces correct code '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke_grid_props.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    generated_code = str(psy.gen)

    before_kernels = (
        "  MODULE psy_single_invoke_with_grid_props_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0(cu_fld, u_fld, du_fld, d_fld)\n"
        "      USE kernel_requires_grid_props, ONLY: next_sshu_code\n"
        "      TYPE(r2d_field), intent(inout) :: cu_fld\n"
        "      TYPE(r2d_field), intent(inout) :: u_fld\n"
        "      TYPE(r2d_field), intent(inout) :: du_fld\n"
        "      TYPE(r2d_field), intent(inout) :: d_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n\n")
    first_kernel = (
        "      DO j = cu_fld%internal%ystart, cu_fld%internal%ystop, 1\n"
        "        DO i = cu_fld%internal%xstart, cu_fld%internal%xstop, 1\n"
        "          CALL next_sshu_code(i, j, cu_fld%data, u_fld%data, "
        "u_fld%grid%tmask, u_fld%grid%area_t, u_fld%grid%area_u)\n"
        "        END DO\n"
        "      END DO\n")
    second_kernel = (
        "      DO j = du_fld%internal%ystart, du_fld%internal%ystop, 1\n"
        "        DO i = du_fld%internal%xstart, du_fld%internal%xstop, 1\n"
        "          CALL next_sshu_code(i, j, du_fld%data, d_fld%data, "
        "d_fld%grid%tmask, d_fld%grid%area_t, d_fld%grid%area_u)\n"
        "        END DO\n"
        "      END DO\n\n"
        "    END SUBROUTINE invoke_0\n"
        "  END MODULE psy_single_invoke_with_grid_props_test")
    if dist_mem:
        # Grid properties do not insert halo exchanges, in this case
        # only the u_fld and d_fld have read stencil accesses.
        halos_first_kernel = "      CALL u_fld%halo_exchange(1)\n"
        halos_second_kernel = "      CALL d_fld%halo_exchange(1)\n"
        expected_output = before_kernels + halos_first_kernel + first_kernel \
            + halos_second_kernel + second_kernel
    else:
        expected_output = before_kernels + first_kernel + second_kernel

    assert generated_code == expected_output
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_scalar_int_arg(tmpdir, dist_mem):
    ''' Tests that an invoke containing a kernel call requiring
    an integer, scalar argument produces correct code '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke_scalar_int_arg.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    generated_code = str(psy.gen)

    before_kernels = (
        "  MODULE psy_single_invoke_scalar_int_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_bc_ssh(ncycle, ssh_fld)\n"
        "      USE kernel_scalar_int, ONLY: bc_ssh_code\n"
        "      INTEGER, intent(inout) :: ncycle\n"
        "      TYPE(r2d_field), intent(inout) :: ssh_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n\n")
    first_kernel = (
        "      DO j = ssh_fld%whole%ystart, ssh_fld%whole%ystop, 1\n"
        "        DO i = ssh_fld%whole%xstart, ssh_fld%whole%xstop, 1\n"
        "          CALL bc_ssh_code(i, j, ncycle, ssh_fld%data, "
        "ssh_fld%grid%tmask)\n"
        "        END DO\n"
        "      END DO\n\n"
        "    END SUBROUTINE invoke_0_bc_ssh\n"
        "  END MODULE psy_single_invoke_scalar_int_test")

    # Scalar arguments do not insert halo exchanges in the distributed memory,
    # in this case, since the only field has pointwise access, there are no
    # halo exchanges.
    expected_output = before_kernels + first_kernel

    assert generated_code == expected_output
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_scalar_float_arg(tmpdir, dist_mem):
    ''' Tests that an invoke containing a kernel call requiring
    a real, scalar argument produces correct code '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke_scalar_float_arg.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    generated_code = str(psy.gen)

    before_kernel = (
        "  MODULE psy_single_invoke_scalar_float_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_bc_ssh(a_scalar, ssh_fld)\n"
        "      USE kernel_scalar_float, ONLY: bc_ssh_code\n"
        "      REAL(KIND=go_wp), intent(inout) :: a_scalar\n"
        "      TYPE(r2d_field), intent(inout) :: ssh_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n\n")
    first_kernel = (
        "      DO j = ssh_fld%whole%ystart, ssh_fld%whole%ystop, 1\n"
        "        DO i = ssh_fld%whole%xstart, ssh_fld%whole%xstop, 1\n"
        "          CALL bc_ssh_code(i, j, a_scalar, ssh_fld%data, "
        "ssh_fld%grid%subdomain%internal%xstop, ssh_fld%grid%tmask)\n"
        "        END DO\n"
        "      END DO\n\n"
        "    END SUBROUTINE invoke_0_bc_ssh\n"
        "  END MODULE psy_single_invoke_scalar_float_test")

    # Scalar arguments do not insert halo exchanges in the distributed memory,
    # in this case, since the only field has pointwise access, there are no
    # halo exchanges.
    expected_output = before_kernel + first_kernel

    assert generated_code == expected_output
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_scalar_float_arg_from_module():
    ''' Tests that an invoke containing a kernel call requiring a real, scalar
    argument imported from a module produces correct code '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke_scalar_float_arg.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    # This test expects constant loop bounds
    clb_trans = GOConstLoopBoundsTrans()
    clb_trans.apply(schedule)

    # Substitute 'a_scalar' argument with a global
    my_mod = ContainerSymbol("my_mod")
    symtab = schedule.symbol_table
    symtab.add(my_mod)
    symtab.lookup("a_scalar").interface = ImportInterface(my_mod)
    symtab.specify_argument_list([schedule.symbol_table.lookup("ssh_fld")])

    # Generate the code. 'a_scalar' should now come from a module instead of a
    # declaration.
    generated_code = str(psy.gen)
    expected_output = (
        "  MODULE psy_single_invoke_scalar_float_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_bc_ssh(ssh_fld)\n"
        "      USE my_mod, ONLY: a_scalar\n"
        "      USE kernel_scalar_float, ONLY: bc_ssh_code\n"
        "      TYPE(r2d_field), intent(inout) :: ssh_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop\n"
        "      INTEGER jstop\n\n"
        "      ! Look-up loop bounds\n"
        "      istop = ssh_fld%grid%subdomain%internal%xstop\n"
        "      jstop = ssh_fld%grid%subdomain%internal%ystop\n"
        "      DO j = 1, jstop + 1, 1\n"
        "        DO i = 1, istop + 1, 1\n"
        "          CALL bc_ssh_code(i, j, a_scalar, ssh_fld%data, "
        "ssh_fld%grid%subdomain%internal%xstop, ssh_fld%grid%tmask)\n"
        "        END DO\n"
        "      END DO\n\n"
        "    END SUBROUTINE invoke_0_bc_ssh\n"
        "  END MODULE psy_single_invoke_scalar_float_test")

    assert generated_code == expected_output
    # We don't compile this generated code as the module is made up and
    # the compiler would correctly fail.


def test_ne_offset_cf_points(tmpdir):
    ''' Test that we can generate code for a kernel that expects a NE
    offset and writes to a field on CF points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test14_ne_offset_cf_updated_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    # This test expects constant loop bounds
    clb_trans = GOConstLoopBoundsTrans()
    clb_trans.apply(schedule)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_compute_vort(vort_fld, p_fld, u_fld, v_fld)\n"
        "      USE kernel_ne_offset_cf_mod, ONLY: compute_vort_code\n"
        "      TYPE(r2d_field), intent(inout) :: vort_fld\n"
        "      TYPE(r2d_field), intent(inout) :: p_fld\n"
        "      TYPE(r2d_field), intent(inout) :: u_fld\n"
        "      TYPE(r2d_field), intent(inout) :: v_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop\n"
        "      INTEGER jstop\n\n"
        "      ! Look-up loop bounds\n"
        "      istop = vort_fld%grid%subdomain%internal%xstop\n"
        "      jstop = vort_fld%grid%subdomain%internal%ystop\n"
        "      DO j = 1, jstop - 1, 1\n"
        "        DO i = 1, istop - 1, 1\n"
        "          CALL compute_vort_code(i, j, vort_fld%data, p_fld%data, "
        "u_fld%data, v_fld%data)\n"
        "        END DO\n"
        "      END DO\n\n"
        "    END SUBROUTINE invoke_0_compute_vort\n"
        "  END MODULE psy_single_invoke_test")

    assert generated_code == expected_output
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_ne_offset_ct_points(tmpdir):
    ''' Test that we can generate code for a kernel that expects a NE
    offset and writes to a field on CT points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test15_ne_offset_ct_updated_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    # This test expects constant loop bounds
    clb_trans = GOConstLoopBoundsTrans()
    clb_trans.apply(schedule)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_compute_vort(p_fld, u_fld, v_fld)\n"
        "      USE kernel_ne_offset_ct_mod, ONLY: compute_vort_code\n"
        "      TYPE(r2d_field), intent(inout) :: p_fld\n"
        "      TYPE(r2d_field), intent(inout) :: u_fld\n"
        "      TYPE(r2d_field), intent(inout) :: v_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop\n"
        "      INTEGER jstop\n\n"
        "      ! Look-up loop bounds\n"
        "      istop = p_fld%grid%subdomain%internal%xstop\n"
        "      jstop = p_fld%grid%subdomain%internal%ystop\n"
        "      DO j = 2, jstop, 1\n"
        "        DO i = 2, istop, 1\n"
        "          CALL compute_vort_code(i, j, p_fld%data, u_fld%data, "
        "v_fld%data)\n"
        "        END DO\n"
        "      END DO\n\n"
        "    END SUBROUTINE invoke_0_compute_vort\n"
        "  END MODULE psy_single_invoke_test")

    assert generated_code == expected_output
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_ne_offset_all_cu_points(tmpdir):
    ''' Test that we can generate code for a kernel that expects a NE
    offset and writes to a field on all CU points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test16_ne_offset_cu_updated_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    # This test expects constant loop bounds
    clb_trans = GOConstLoopBoundsTrans()
    clb_trans.apply(schedule)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_bc_solid_u(u_fld)\n"
        "      USE boundary_conditions_ne_offset_mod, ONLY: bc_solid_u_code\n"
        "      TYPE(r2d_field), intent(inout) :: u_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop\n"
        "      INTEGER jstop\n\n"
        "      ! Look-up loop bounds\n"
        "      istop = u_fld%grid%subdomain%internal%xstop\n"
        "      jstop = u_fld%grid%subdomain%internal%ystop\n"
        "      DO j = 1, jstop + 1, 1\n"
        "        DO i = 1, istop, 1\n"
        "          CALL bc_solid_u_code(i, j, u_fld%data, u_fld%grid%tmask)\n"
        "        END DO\n"
        "      END DO\n\n"
        "    END SUBROUTINE invoke_0_bc_solid_u\n"
        "  END MODULE psy_single_invoke_test")

    assert generated_code == expected_output
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_ne_offset_all_cv_points(tmpdir):
    ''' Test that we can generate code for a kernel that expects a NE
    offset and writes to a field on all CV points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test17_ne_offset_cv_updated_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    # This test expects constant loop bounds
    clb_trans = GOConstLoopBoundsTrans()
    clb_trans.apply(schedule)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_bc_solid_v(v_fld)\n"
        "      USE boundary_conditions_ne_offset_mod, ONLY: bc_solid_v_code\n"
        "      TYPE(r2d_field), intent(inout) :: v_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop\n"
        "      INTEGER jstop\n\n"
        "      ! Look-up loop bounds\n"
        "      istop = v_fld%grid%subdomain%internal%xstop\n"
        "      jstop = v_fld%grid%subdomain%internal%ystop\n"
        "      DO j = 1, jstop, 1\n"
        "        DO i = 1, istop + 1, 1\n"
        "          CALL bc_solid_v_code(i, j, v_fld%data, v_fld%grid%tmask)\n"
        "        END DO\n"
        "      END DO\n\n"
        "    END SUBROUTINE invoke_0_bc_solid_v\n"
        "  END MODULE psy_single_invoke_test")

    assert generated_code == expected_output
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_ne_offset_all_cf_points(tmpdir):
    ''' Test that we can generate code for a kernel that expects a NE
    offset and writes to a field on all CF points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test18_ne_offset_cf_updated_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    # This test expects constant loop bounds
    clb_trans = GOConstLoopBoundsTrans()
    clb_trans.apply(schedule)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_bc_solid_f(f_fld)\n"
        "      USE boundary_conditions_ne_offset_mod, ONLY: bc_solid_f_code\n"
        "      TYPE(r2d_field), intent(inout) :: f_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop\n"
        "      INTEGER jstop\n\n"
        "      ! Look-up loop bounds\n"
        "      istop = f_fld%grid%subdomain%internal%xstop\n"
        "      jstop = f_fld%grid%subdomain%internal%ystop\n"
        "      DO j = 1, jstop, 1\n"
        "        DO i = 1, istop, 1\n"
        "          CALL bc_solid_f_code(i, j, f_fld%data, f_fld%grid%tmask)\n"
        "        END DO\n"
        "      END DO\n\n"
        "    END SUBROUTINE invoke_0_bc_solid_f\n"
        "  END MODULE psy_single_invoke_test")

    assert generated_code == expected_output
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_sw_offset_cf_points(tmpdir):
    ''' Test that we can generate code for a kernel that expects a SW
    offset and writes to a field on internal CF points '''
    _, invoke_info = parse(
        os.path.join(os.path.dirname(os.path.abspath(__file__)),
                     "test_files", "gocean1p0",
                     "test19.1_sw_offset_cf_updated_one_invoke.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    # This test expects constant loop bounds
    clb_trans = GOConstLoopBoundsTrans()
    clb_trans.apply(schedule)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_compute_z(z_fld, p_fld, u_fld, v_fld)\n"
        "      USE kernel_sw_offset_cf_mod, ONLY: compute_z_code\n"
        "      TYPE(r2d_field), intent(inout) :: z_fld\n"
        "      TYPE(r2d_field), intent(inout) :: p_fld\n"
        "      TYPE(r2d_field), intent(inout) :: u_fld\n"
        "      TYPE(r2d_field), intent(inout) :: v_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop\n"
        "      INTEGER jstop\n\n"
        "      ! Look-up loop bounds\n"
        "      istop = z_fld%grid%subdomain%internal%xstop\n"
        "      jstop = z_fld%grid%subdomain%internal%ystop\n"
        "      DO j = 2, jstop + 1, 1\n"
        "        DO i = 2, istop + 1, 1\n"
        "          CALL compute_z_code(i, j, z_fld%data, p_fld%data, "
        "u_fld%data, v_fld%data, p_fld%grid%dx, p_fld%grid%dy)\n"
        "        END DO\n"
        "      END DO\n\n"
        "    END SUBROUTINE invoke_0_compute_z\n"
        "  END MODULE psy_single_invoke_test")
    assert generated_code == expected_output
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_sw_offset_all_cf_points(tmpdir):
    ''' Test that we can generate code for a kernel that expects a SW
    offset and writes to a field on all CF points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test19.2_sw_offset_all_cf_updated" +
                                "_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    # This test expects constant loop bounds
    clb_trans = GOConstLoopBoundsTrans()
    clb_trans.apply(schedule)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_apply_bcs_f(z_fld, p_fld, u_fld, v_fld)\n"
        "      USE kernel_sw_offset_cf_mod, ONLY: apply_bcs_f_code\n"
        "      TYPE(r2d_field), intent(inout) :: z_fld\n"
        "      TYPE(r2d_field), intent(inout) :: p_fld\n"
        "      TYPE(r2d_field), intent(inout) :: u_fld\n"
        "      TYPE(r2d_field), intent(inout) :: v_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop\n"
        "      INTEGER jstop\n\n"
        "      ! Look-up loop bounds\n"
        "      istop = z_fld%grid%subdomain%internal%xstop\n"
        "      jstop = z_fld%grid%subdomain%internal%ystop\n"
        "      DO j = 1, jstop + 1, 1\n"
        "        DO i = 1, istop + 1, 1\n"
        "          CALL apply_bcs_f_code(i, j, z_fld%data, p_fld%data, "
        "u_fld%data, v_fld%data)\n"
        "        END DO\n"
        "      END DO\n\n"
        "    END SUBROUTINE invoke_0_apply_bcs_f\n"
        "  END MODULE psy_single_invoke_test")

    assert generated_code == expected_output
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_sw_offset_ct_points(tmpdir):
    ''' Test that we can generate code for a kernel that expects a SW
    offset and writes to a field on internal CT points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test20_sw_offset_ct_updated_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    # This test expects constant loop bounds
    clb_trans = GOConstLoopBoundsTrans()
    clb_trans.apply(schedule)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_compute_h(h_fld, p_fld, u_fld, v_fld)\n"
        "      USE kernel_sw_offset_ct_mod, ONLY: compute_h_code\n"
        "      TYPE(r2d_field), intent(inout) :: h_fld\n"
        "      TYPE(r2d_field), intent(inout) :: p_fld\n"
        "      TYPE(r2d_field), intent(inout) :: u_fld\n"
        "      TYPE(r2d_field), intent(inout) :: v_fld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop\n"
        "      INTEGER jstop\n\n"
        "      ! Look-up loop bounds\n"
        "      istop = h_fld%grid%subdomain%internal%xstop\n"
        "      jstop = h_fld%grid%subdomain%internal%ystop\n"
        "      DO j = 2, jstop, 1\n"
        "        DO i = 2, istop, 1\n"
        "          CALL compute_h_code(i, j, h_fld%data, p_fld%data, "
        "u_fld%data, v_fld%data)\n"
        "        END DO\n"
        "      END DO\n\n"
        "    END SUBROUTINE invoke_0_compute_h\n"
        "  END MODULE psy_single_invoke_test")

    assert generated_code == expected_output
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_sw_offset_all_ct_points(tmpdir):
    ''' Test that we can generate code for a kernel that expects a SW
    offset and writes to a field on all CT points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test21_sw_offset_all_ct_updated" +
                                "_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    # This test expects constant loop bounds
    clb_trans = GOConstLoopBoundsTrans()
    clb_trans.apply(schedule)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_apply_bcs_h(hfld, pfld, ufld, vfld)\n"
        "      USE kernel_sw_offset_ct_mod, ONLY: apply_bcs_h_code\n"
        "      TYPE(r2d_field), intent(inout) :: hfld\n"
        "      TYPE(r2d_field), intent(inout) :: pfld\n"
        "      TYPE(r2d_field), intent(inout) :: ufld\n"
        "      TYPE(r2d_field), intent(inout) :: vfld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop\n"
        "      INTEGER jstop\n\n"
        "      ! Look-up loop bounds\n"
        "      istop = hfld%grid%subdomain%internal%xstop\n"
        "      jstop = hfld%grid%subdomain%internal%ystop\n"
        "      DO j = 1, jstop + 1, 1\n"
        "        DO i = 1, istop + 1, 1\n"
        "          CALL apply_bcs_h_code(i, j, hfld%data, pfld%data, "
        "ufld%data, vfld%data)\n"
        "        END DO\n"
        "      END DO\n\n"
        "    END SUBROUTINE invoke_0_apply_bcs_h\n"
        "  END MODULE psy_single_invoke_test")

    assert generated_code == expected_output
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_sw_offset_all_cu_points(tmpdir):
    ''' Test that we can generate code for a kernel that expects a SW
    offset and writes to a field on all CU points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test22_sw_offset_all_cu_updated" +
                                "_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    # This test expects constant loop bounds
    clb_trans = GOConstLoopBoundsTrans()
    clb_trans.apply(schedule)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_apply_bcs_u(ufld, vfld)\n"
        "      USE kernel_sw_offset_cu_mod, ONLY: apply_bcs_u_code\n"
        "      TYPE(r2d_field), intent(inout) :: ufld\n"
        "      TYPE(r2d_field), intent(inout) :: vfld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop\n"
        "      INTEGER jstop\n\n"
        "      ! Look-up loop bounds\n"
        "      istop = ufld%grid%subdomain%internal%xstop\n"
        "      jstop = ufld%grid%subdomain%internal%ystop\n"
        "      DO j = 1, jstop + 1, 1\n"
        "        DO i = 1, istop + 1, 1\n"
        "          CALL apply_bcs_u_code(i, j, ufld%data, vfld%data)\n"
        "        END DO\n"
        "      END DO\n\n"
        "    END SUBROUTINE invoke_0_apply_bcs_u\n"
        "  END MODULE psy_single_invoke_test")

    assert generated_code == expected_output
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_sw_offset_all_cv_points(tmpdir):
    ''' Test that we can generate code for a kernel that expects a SW
    offset and writes to a field on all CV points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test23_sw_offset_all_cv_updated" +
                                "_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    # This test expects constant loop bounds
    clb_trans = GOConstLoopBoundsTrans()
    clb_trans.apply(schedule)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_apply_bcs_v(vfld, ufld)\n"
        "      USE kernel_sw_offset_cv_mod, ONLY: apply_bcs_v_code\n"
        "      TYPE(r2d_field), intent(inout) :: vfld\n"
        "      TYPE(r2d_field), intent(inout) :: ufld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop\n"
        "      INTEGER jstop\n\n"
        "      ! Look-up loop bounds\n"
        "      istop = vfld%grid%subdomain%internal%xstop\n"
        "      jstop = vfld%grid%subdomain%internal%ystop\n"
        "      DO j = 1, jstop + 1, 1\n"
        "        DO i = 1, istop + 1, 1\n"
        "          CALL apply_bcs_v_code(i, j, vfld%data, ufld%data)\n"
        "        END DO\n"
        "      END DO\n\n"
        "    END SUBROUTINE invoke_0_apply_bcs_v\n"
        "  END MODULE psy_single_invoke_test")

    assert generated_code == expected_output
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_offset_any_all_cu_points(tmpdir):
    ''' Test that we can generate code for a kernel that will operate
    with any offset and writes to a field on all cu points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test25_any_offset_all_cu_update" +
                                "_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    # This test expects constant loop bounds
    clb_trans = GOConstLoopBoundsTrans()
    clb_trans.apply(schedule)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_compute_u(ufld, vfld, hfld)\n"
        "      USE kernel_any_offset_cu_mod, ONLY: compute_u_code\n"
        "      TYPE(r2d_field), intent(inout) :: ufld\n"
        "      TYPE(r2d_field), intent(inout) :: vfld\n"
        "      TYPE(r2d_field), intent(inout) :: hfld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop\n"
        "      INTEGER jstop\n\n"
        "      ! Look-up loop bounds\n"
        "      istop = ufld%grid%subdomain%internal%xstop\n"
        "      jstop = ufld%grid%subdomain%internal%ystop\n"
        "      DO j = 1, jstop, 1\n"
        "        DO i = 1, istop, 1\n"
        "          CALL compute_u_code(i, j, ufld%data, vfld%data, "
        "hfld%data)\n"
        "        END DO\n"
        "      END DO\n\n"
        "    END SUBROUTINE invoke_0_compute_u\n"
        "  END MODULE psy_single_invoke_test")

    assert generated_code == expected_output
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_offset_any_all_points(tmpdir):
    ''' Test that we can generate code for a kernel that will operate
    with any offset and writes to a field on all points '''
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                "test24_any_offset_all_update" +
                                "_one_invoke.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    # This test expects constant loop bounds
    clb_trans = GOConstLoopBoundsTrans()
    clb_trans.apply(schedule)
    generated_code = str(psy.gen)

    expected_output = (
        "  MODULE psy_single_invoke_test\n"
        "    USE field_mod\n"
        "    USE kind_params_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_copy(voldfld, vfld)\n"
        "      USE kernel_field_copy_mod, ONLY: field_copy_code\n"
        "      TYPE(r2d_field), intent(inout) :: voldfld\n"
        "      TYPE(r2d_field), intent(inout) :: vfld\n"
        "      INTEGER j\n"
        "      INTEGER i\n"
        "      INTEGER istop\n"
        "      INTEGER jstop\n\n"
        "      ! Look-up loop bounds\n"
        "      istop = voldfld%grid%subdomain%internal%xstop\n"
        "      jstop = voldfld%grid%subdomain%internal%ystop\n"
        "      DO j = 1, jstop + 1, 1\n"
        "        DO i = 1, istop + 1, 1\n"
        "          CALL field_copy_code(i, j, voldfld%data, vfld%data)\n"
        "        END DO\n"
        "      END DO\n\n"
        "    END SUBROUTINE invoke_0_copy\n"
        "  END MODULE psy_single_invoke_test")
    assert generated_code == expected_output
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_find_grid_access(monkeypatch):
    ''' Tests for the GOKernelArguments.find_grid_access method. This
    identifies the best kernel argument from which to access grid
    properties. '''
    _, invoke = get_invoke("single_invoke.f90", API, idx=0)
    schedule = invoke.schedule
    kern = schedule.coded_kernels()[0]
    assert isinstance(kern, GOKern)
    arg = kern.arguments.find_grid_access()
    assert isinstance(arg, GOKernelArgument)
    # The first read-only argument for this kernel is the pressure field
    assert arg.name == "p_fld"
    # Now monkeypatch the type of each of the kernel arguments so that
    # none of them is a field
    for karg in kern.arguments._args:
        monkeypatch.setattr(karg._arg, "_argument_type", "broken")
    # find_grid_access should now return None
    arg = kern.arguments.find_grid_access()
    assert arg is None


def test_raw_arg_list_error(monkeypatch):
    ''' Test that we raise an internal error in the
    GOKernelArguments.raw_arg_list method if there's no argument from which
    to get the grid properties. '''
    _, invoke = get_invoke("test19.1_sw_offset_cf_updated_one_invoke.f90",
                           API, idx=0)
    schedule = invoke.schedule
    kern = schedule.coded_kernels()[0]
    assert isinstance(kern, GOKern)
    raw_list = kern.arguments.raw_arg_list()
    assert raw_list == ['i', 'j', 'z_fld%data', 'p_fld%data', 'u_fld%data',
                        'v_fld%data', 'p_fld%grid%dx', 'p_fld%grid%dy']
    # Now monkeypatch find_grid_access()
    monkeypatch.setattr(kern.arguments, "find_grid_access", lambda: None)
    kern.arguments._raw_arg_list = None
    with pytest.raises(GenerationError) as err:
        _ = kern.arguments.raw_arg_list()
    assert ("kernel compute_z_code requires grid property dx but does not "
            "have any arguments that are fields" in str(err.value))
    # Now monkeypatch one of the kernel arguments so that it has an
    # unrecognised type
    monkeypatch.setattr(kern.arguments._args[0]._arg, "_argument_type",
                        "broken")
    with pytest.raises(InternalError) as err:
        _ = kern.arguments.raw_arg_list()
    assert ("Kernel compute_z_code, argument z_fld has unrecognised type: "
            "'broken'" in str(err.value))


def test_invalid_access_type():
    ''' Test that we raise an internal error if we try to assign
    an invalid access type (string instead of AccessType) in
    a psygen.Argument.'''
    _, invoke = get_invoke("test19.1_sw_offset_cf_updated_one_invoke.f90",
                           API, idx=0)
    schedule = invoke.schedule
    kern = schedule.coded_kernels()[0]
    # Test that assigning a non-AccessType value to a kernel argument
    # raises an exception.
    with pytest.raises(InternalError) as err:
        kern.arguments.args[0].access = "invalid-type"
    # Note that the error message looks slightly different between
    # python 2 (type str) and 3 (class str):
    assert re.search("Invalid access type 'invalid-type' of type.*str",
                     str(err.value)) is not None


def test_compile_with_dependency(tmpdir):
    ''' Check that we can do test compilation for an invoke of a kernel
    that has a dependency on a non-infrastructure module. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "single_invoke_kern_with_use.f90"),
        api=API)
    psy = PSyFactory(API).create(invoke_info)
    assert GOceanBuild(tmpdir).code_compiles(psy, ["model_mod"])


# -----------------------------------
# Parser Tests for the GOcean 1.0 API
# -----------------------------------


def test00p1_kernel_wrong_meta_arg_count():
    ''' Check that we raise an error if one of the meta-args in
    a kernel's meta-data has the wrong number of arguments '''
    with pytest.raises(ParseError):
        parse(os.path.
              join(os.path.dirname(os.path.abspath(__file__)),
                   "test_files", "gocean1p0",
                   "test00.1_invoke_kernel_wrong_meta_arg_count.f90"),
              api="gocean1.0")


def test00p1_invoke_kernel_using_const_scalar():
    '''Check that using a const scalar as parameter works.'''
    filename = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                            "test_files", "gocean1p0",
                            "test00.1_invoke_kernel_using_const_scalar.f90")
    _, invoke_info = parse(filename, api="gocean1.0")
    out = str(PSyFactory(API).create(invoke_info).gen)
    # Old versions of PSyclone tried to declare '0' as a variable:
    # REAL(KIND=wp), intent(inout) :: 0
    # INTEGER, intent(inout) :: 0
    # Make sure this is not happening anymore
    assert re.search(r"\s*real.*:: *0", out, re.I) is None
    assert re.search(r"\s*integer.*:: *0", out, re.I) is None
    assert re.search(r"\s*real.*:: *real_val", out, re.I) is not None


def test00p2_kernel_invalid_meta_args():
    ''' Check that we raise an error if one of the meta-args in
    a kernel's meta-data is not 'arg' '''
    with pytest.raises(ParseError):
        parse(os.path.
              join(os.path.dirname(os.path.abspath(__file__)),
                   "test_files", "gocean1p0",
                   "test00.2_invoke_kernel_invalid_meta_args.f90"),
              api="gocean1.0")


def test00p3_kern_invalid_meta_arg_type():
    ''' Check that the parser catches the case where the type of
    one of the meta-args in the kernel meta-data is incorrect '''
    test_file = "test00.3_invoke_kernel_invalid_meta_arg_type.f90"
    with pytest.raises(ParseError):
        _, _ = parse(os.path.
                     join(os.path.
                          dirname(os.path.
                                  abspath(__file__)),
                          "test_files", "gocean1p0",
                          test_file),
                     api=API)


def test01_diff_kern_grid_offsets_one_invoke():
    ''' Check that the parser raises an error if two kernels in a
        single invoke specify different index offsets '''
    test_file = "test01_different_grid_offsets_one_invoke.f90"
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                test_file),
                           api=API)
    with pytest.raises(GenerationError):
        _ = PSyFactory(API).create(invoke_info)


def test02_diff_kern_grid_offsets_two_invokes():
    ''' Check that the parser raises an error if the two kernels
        in different invokes specify different index offsets. '''
    test_file = "test02_different_grid_offsets_two_invokes.f90"
    _, invoke_info = parse(os.path.
                           join(os.path.
                                dirname(os.path.
                                        abspath(__file__)),
                                "test_files", "gocean1p0",
                                test_file),
                           api=API)
    with pytest.raises(GenerationError):
        _ = PSyFactory(API).create(invoke_info)


def test03_kernel_missing_index_offset():
    ''' Check that we raise an error if a kernel's meta-data is
    missing the INDEX_OFFSET field. '''
    with pytest.raises(ParseError):
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test03_invoke_kernel_missing_offset.f90"),
              api="gocean1.0")


def test04_kernel_invalid_index_offset():
    ''' Check that we raise an error if a kernel's meta-data is
    contains an invalid value for the INDEX_OFFSET field. '''
    with pytest.raises(ParseError):
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test04_invoke_kernel_invalid_offset.f90"),
              api="gocean1.0")


def test05_kernel_missing_iterates_over():
    ''' Check that we raise an error if a kernel's meta-data is
    missing the ITERATES_OVER field. '''
    with pytest.raises(ParseError):
        parse(os.path.
              join(os.path.dirname(os.path.abspath(__file__)),
                   "test_files", "gocean1p0",
                   "test05_invoke_kernel_missing_iterates_over.f90"),
              api="gocean1.0")


def test05p1_kernel_invalid_iterates_over():
    ''' Check that we raise an error if a kernel's meta-data has
    an invalid ITERATES_OVER field. '''
    with pytest.raises(ParseError):
        parse(os.path.
              join(os.path.dirname(os.path.abspath(__file__)),
                   "test_files", "gocean1p0",
                   "test05.1_invoke_kernel_invalid_iterates_over.f90"),
              api="gocean1.0")


def test05p1_kernel_add_iteration_spaces(tmpdir):
    ''' Check that adding a new iteration space works and the
    GOConstLoopBoundsTrans can also use it. '''

    # Add new iteration space 'go_dofs'
    GOLoop.add_bounds("go_offset_sw:go_cu:go_dofs:1:2:3:{stop}")

    _, invoke_info = \
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test05.1_invoke_kernel_invalid_iterates_over.f90"),
              api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    expected_sched = (
        "GOLoop[variable:'j', loop_type:'outer']\n"
        "Literal[value:'1', Scalar<INTEGER, UNDEFINED>]\n"
        "Literal[value:'2', Scalar<INTEGER, UNDEFINED>]\n"
        "Literal[value:'1', Scalar<INTEGER, UNDEFINED>]\n"
        "Schedule:\n"
        "GOLoop[variable:'i', loop_type:'inner']\n"
        "Literal[value:'3', Scalar<INTEGER, UNDEFINED>]\n"
        "StructureReference[name:'cu_fld']\n"
        "StructureMember[name:'grid']\n"
        "StructureMember[name:'subdomain']\n"
        "StructureMember[name:'internal']\n"
        "Member[name:'xstop']\n"
        "Literal[value:'1', Scalar<INTEGER, UNDEFINED>]\n"
        "Schedule:\n"
        "kern call: compute_cu_code\n")
    assert expected_sched in str(schedule)

    # Also check with constant loop bounds
    clb_trans = GOConstLoopBoundsTrans()
    clb_trans.apply(schedule)
    expected_sched = (
        "GOLoop[variable:'j', loop_type:'outer']\n"
        "Literal[value:'1', Scalar<INTEGER, UNDEFINED>]\n"
        "Literal[value:'2', Scalar<INTEGER, UNDEFINED>]\n"
        "Literal[value:'1', Scalar<INTEGER, UNDEFINED>]\n"
        "Schedule:\n"
        "GOLoop[variable:'i', loop_type:'inner']\n"
        "Literal[value:'3', Scalar<INTEGER, UNDEFINED>]\n"
        "Reference[name:'istop']\n"
        "Literal[value:'1', Scalar<INTEGER, UNDEFINED>]\n"
        "Schedule:\n"
        "kern call: compute_cu_code\n")
    assert expected_sched in str(schedule)
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test06_kernel_invalid_access():
    ''' Check that we raise an error if a kernel's meta-data specifies
    an unrecognised access type for a kernel argument (i.e. something
    other than READ,WRITE,READWRITE) '''
    with pytest.raises(ParseError) as err:
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test06_invoke_kernel_wrong_access.f90"),
              api="gocean1.0")
    assert ("compute_cu: argument access is given as 'wrong' but must be one "
            "of ['go_read', 'go_readwrite', 'go_write']" in str(err.value))


def test07_kernel_wrong_gridpt_type():
    ''' Check that we raise an error if a kernel's meta-data specifies
    an unrecognised grid-point type for a field argument (i.e.
    something other than C{U,V,F,T}, I_SCALAR or R_SCALAR) '''
    with pytest.raises(ParseError):
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test07_invoke_kernel_wrong_gridpt_type.f90"),
              api="gocean1.0")


def test08_kernel_invalid_grid_property():
    ''' Check that the parser raises an error if a kernel's meta-data
    specifies an unrecognised grid property '''
    with pytest.raises(ParseError) as err:
        parse(os.path.
              join(os.path.dirname(os.path.abspath(__file__)),
                   "test_files", "gocean1p0",
                   "test08_invoke_kernel_invalid_grid_property.f90"),
              api="gocean1.0")
    assert "Meta-data error in kernel compute_cu: un-recognised grid " \
           "property 'grid_area_wrong' requested." in str(err.value)

    # GOKernelGridArgument contains also a test for the validity of
    # a grid property. It's easier to create a dummy class to test this:
    class DummyDescriptor():
        '''Dummy class to test error handling.'''
        def __init__(self):
            self.access = "read"
            self.grid_prop = "does not exist"
    descriptor = DummyDescriptor()
    with pytest.raises(GenerationError) as err:
        GOKernelGridArgument(descriptor, None)
    assert re.search("Unrecognised grid property specified. Expected one "
                     "of.* but found 'does not exist'", str(err.value)) \
        is not None


def test08p1_kernel_without_fld_args():
    ''' Check that the parser raises an error if a kernel does not
    have a field object as an argument but requests a grid property '''
    with pytest.raises(ParseError):
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test08.1_invoke_kernel_no_fld_args.f90"),
              api="gocean1.0")


def test09_kernel_missing_stencil_prop():
    '''Check that the parser raises an error if there is no stencil
    specified in the meta-data of a kernel

    '''
    with pytest.raises(ParseError):
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test09_invoke_kernel_missing_stencil.f90"),
              api="gocean1.0")


def test10_kernel_invalid_stencil_prop():
    '''Check that the parser raises an error if there is no stencil
    specified in the meta-data of a kernel

    '''
    with pytest.raises(ParseError):
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test10_invoke_kernel_invalid_stencil.f90"),
              api="gocean1.0")


def test13_kernel_invalid_fortran():
    ''' Check that the parser raises an error if the specified kernel
    code is not valid fortran '''
    with pytest.raises(ParseError):
        parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "test_files", "gocean1p0",
                           "test13_invoke_kernel_invalid_fortran.f90"),
              api="gocean1.0")


def test14_no_builtins():
    ''' Check that we raise an error if we attempt to create a
    built-in '''
    with pytest.raises(GenerationError) as excinfo:
        GOBuiltInCallFactory.create()
    assert "Built-ins are not supported for the GOcean" in str(excinfo.value)


def test_go_descriptor_str():
    '''Tests  the __str__ function of a GO1p0Descriptor.
    '''
    # Parse an existing kernel to create the required kernel_call
    # type.
    _, invoke_info = parse(os.path.join(get_base_path(API),
                                        "single_invoke_scalar_float_arg.f90"),
                           api=API)

    kernel_call = invoke_info.calls[0].kcalls[0]
    arg_descriptors = kernel_call.ktype.arg_descriptors

    assert "Descriptor(READ, go_r_scalar, 0)" == str(arg_descriptors[0])


def test_go_kerneltype_str():
    '''Tests  the __str__ function of a GOKernelType1p0.
    '''
    # Parse an existing kernel to create the required kernel_call
    # type.
    _, invoke_info = parse(os.path.join(get_base_path(API),
                                        "single_invoke_scalar_float_arg.f90"),
                           api=API)

    kernel_call = invoke_info.calls[0].kcalls[0]

    assert ("GOcean 1.0 kernel bc_ssh, index-offset = go_offset_ne, "
            "iterates-over = go_all_pts" == str(kernel_call.ktype))
