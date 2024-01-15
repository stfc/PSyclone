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
# -----------------------------------------------------------------------------
# Author: A. R. Porter, STFC Daresbury Lab
# Modified: I. Kavcic, Met Office
# Modified: R. W. Ford and N. Nobre, STFC Daresbury Lab
# Modified: by J. Henrichs, Bureau of Meteorology

''' Module containing pytest tests of the LFRic general reduction built-in.'''

import os
import pytest

from psyclone.configuration import Config
from psyclone.domain.lfric import lfric_builtins
from psyclone.parse.algorithm import parse
from psyclone.parse.utils import ParseError
from psyclone.psyGen import PSyFactory
from psyclone.tests.lfric_build import LFRicBuild

# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__))))),
    "test_files", "dynamo0p3")

# The PSyclone API under test
API = "dynamo0.3"


# ------------- Invalid built-in with an integer scalar reduction ----------- #

def test_scalar_int_builtin_error(monkeypatch):
    '''
    Test that specifying incorrect meta-data for built-in such that it
    claims to perform a reduction into an integer variable raises the
    expected error.

    '''
    monkeypatch.setattr(lfric_builtins, "BUILTIN_DEFINITIONS_FILE",
                        value=os.path.join(BASE_PATH,
                                           "int_reduction_builtins_mod.f90"))
    with pytest.raises(ParseError) as excinfo:
        _, _ = parse(os.path.join(BASE_PATH,
                                  "16.2_integer_scalar_sum.f90"),
                     api=API)
    assert ("In the LFRic API a reduction access 'gh_sum' is only valid "
            "with a real scalar argument, but a scalar argument with "
            "'gh_integer' data type was found" in str(excinfo.value))


# ------------- Built-ins with reductions ----------------------------------- #


def test_multi_builtin_single_invoke(tmpdir, monkeypatch, annexed, dist_mem):
    '''
    Test that multiple built-ins, including one with reductions,
    produce correct code. Also test with and without annexed DoFs
    being computed as this affects the generated code.

    '''
    # Test with and without annexed DoFs
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.18.1_builtins_reduction_fuse_error.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test code generation
    code = str(psy.gen)

    if dist_mem:
        assert (
            "    SUBROUTINE invoke_0(asum, f1, f2, b)\n"
            "      USE scalar_mod, ONLY: scalar_type\n"
            "      USE mesh_mod, ONLY: mesh_type\n"
            "      REAL(KIND=r_def), intent(out) :: asum\n"
            "      REAL(KIND=r_def), intent(in) :: b\n"
            "      TYPE(field_type), intent(in) :: f1, f2\n"
            "      TYPE(scalar_type) global_sum\n"
            "      INTEGER df\n"
            "      INTEGER(KIND=i_def) loop2_start, loop2_stop\n"
            "      INTEGER(KIND=i_def) loop1_start, loop1_stop\n"
            "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f2_data => "
            "null()\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f1_data => "
            "null()\n"
            "      TYPE(field_proxy_type) f1_proxy, f2_proxy\n") in code
        assert (
            "      f1_proxy = f1%get_proxy()\n"
            "      f1_data => f1_proxy%data\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      f2_data => f2_proxy%data\n"
            "      !\n") in code
        output = (
            "      loop2_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      !\n"
            "      ! Zero summation variables\n"
            "      !\n"
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        asum = asum + f1_data(df) * f2_data(df)\n"
            "      END DO\n"
            "      global_sum%value = asum\n"
            "      asum = global_sum%get_sum()\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        f1_data(df) = b * f1_data(df)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      !\n"
            "      DO df=loop2_start,loop2_stop\n"
            "        f1_data(df) = asum * f1_data(df)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n")
        if not annexed:
            output = output.replace("dof_annexed", "dof_owned")
        assert output in code
    else:
        assert (
            "    SUBROUTINE invoke_0(asum, f1, f2, b)\n"
            "      REAL(KIND=r_def), intent(out) :: asum\n"
            "      REAL(KIND=r_def), intent(in) :: b\n"
            "      TYPE(field_type), intent(in) :: f1, f2\n"
            "      INTEGER df\n"
            "      INTEGER(KIND=i_def) loop2_start, loop2_stop\n"
            "      INTEGER(KIND=i_def) loop1_start, loop1_stop\n"
            "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f2_data => "
            "null()\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f1_data => "
            "null()\n"
            "      TYPE(field_proxy_type) f1_proxy, f2_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f1\n") in code
        assert (
            "      f1_proxy = f1%get_proxy()\n"
            "      f1_data => f1_proxy%data\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      f2_data => f2_proxy%data\n"
            "      !\n"
            "      ! Initialise number of DoFs for aspc1_f1\n"
            "      !\n"
            "      undf_aspc1_f1 = "
            "f1_proxy%vspace%get_undf()\n" in code)
        assert (
            "      loop0_stop = undf_aspc1_f1\n"
            "      loop1_start = 1\n"
            "      loop1_stop = undf_aspc1_f1\n"
            "      loop2_start = 1\n"
            "      loop2_stop = undf_aspc1_f1\n" in code)
        assert (
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        asum = asum + f1_data(df) * f2_data(df)\n"
            "      END DO\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        f1_data(df) = b * f1_data(df)\n"
            "      END DO\n"
            "      DO df=loop2_start,loop2_stop\n"
            "        f1_data(df) = asum * f1_data(df)\n"
            "      END DO\n") in code

    # Test compilation of generated code
    assert LFRicBuild(tmpdir).code_compiles(psy)
