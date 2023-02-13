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
# Author: A. R. Porter, STFC Daresbury Lab
# Modified: I. Kavcic, Met Office
# Modified: R. W. Ford and N. Nobre, STFC Daresbury Lab
# Modified: by J. Henrichs, Bureau of Meteorology

''' Module containing pytest tests of the LFRicXInnerproductYKern and
LFRicXInnerproductXKern built-ins.'''

import os
from psyclone.domain.lfric.lfric_builtins import (LFRicXInnerproductYKern,
                                                  LFRicXInnerproductXKern)
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir.nodes import Loop
from psyclone.tests.lfric_build import LFRicBuild


# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__))))),
    "test_files", "dynamo0p3")

# The PSyclone API under test
API = "dynamo0.3"


# ------------- Inner product of two real fields ---------------------------- #


def test_X_innerproduct_Y(tmpdir, dist_mem):
    ''' Test that 1) the str method of LFRicXInnerproductYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation which calculates inner product of real-valued fields X and Y
    as innprod = innprod + X(:)*Y(:).

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.9.1_X_innerproduct_Y_builtin.f90"),
        api=API)
    psy = PSyFactory(API,
                     distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: X_innerproduct_Y (real-valued fields)"
    # Test kernel instance
    assert isinstance(kern, LFRicXInnerproductYKern)
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output = (
        "      !\n"
        "      ! Initialise field and/or operator proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      !\n")
    assert output in code
    if not dist_mem:
        output_seq = (
            "      ! Initialise number of DoFs for aspc1_f1\n"
            "      !\n"
            "      undf_aspc1_f1 = f1_proxy%vspace%get_undf()\n"
            "      !\n"
            "      ! Set-up all of the loop bounds\n"
            "      !\n"
            "      loop0_start = 1\n"
            "      loop0_stop = undf_aspc1_f1\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      !\n"
            "      ! Zero summation variables\n"
            "      !\n"
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        asum = asum + f1_proxy%data(df) * f2_proxy%data(df)\n"
            "      END DO\n"
            "      !\n")
        assert output_seq in code
    else:
        output_dm = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_owned()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      !\n"
            "      ! Zero summation variables\n"
            "      !\n"
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        asum = asum + f1_proxy%data(df) * f2_proxy%data(df)\n"
            "      END DO\n"
            "      global_sum%value = asum\n"
            "      asum = global_sum%get_sum()\n"
            "      !\n")
        assert output_dm in code
        assert "      USE scalar_mod, ONLY: scalar_type" in code
        assert "      REAL(KIND=r_def), intent(out) :: asum\n" in code
        assert "      TYPE(scalar_type) global_sum\n" in code


def test_X_innerproduct_Y_lowering(fortran_writer):
    '''
    Test that the lower_to_language_level() method of X_innerproduct_Y
    built-in works as expected.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                           "15.9.1_X_innerproduct_Y_builtin.f90"), api=API)
    psy = PSyFactory(API,
                     distributed_memory=False).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    kern.lower_to_language_level()
    loop = first_invoke.schedule.walk(Loop)[0]
    code = fortran_writer(loop)
    assert ("do df = loop0_start, loop0_stop, 1\n"
            "  asum = asum + f1_proxy%data(df) * f2_proxy%data(df)\n"
            "enddo") in code


# ------------- Inner product of a real field with itself ------------------- #


def test_X_innerproduct_X(tmpdir, dist_mem):
    ''' Test that 1) the str method of LFRicXInnerproductXKern returns the
    expected string and 2) we generate correct code for the built-in
    operation which calculates inner product of a real-valued field X by
    itself as innprod = innprod + X(:)*X(:).

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.9.2_X_innerproduct_X_builtin.f90"),
        api=API)
    psy = PSyFactory(API,
                     distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: X_innerproduct_X (real-valued fields)"
    # Test kernel instance
    assert isinstance(kern, LFRicXInnerproductXKern)
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output = (
        "      !\n"
        "      ! Initialise field and/or operator proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      !\n")
    assert output in code
    if not dist_mem:
        output_seq = (
            "      ! Initialise number of DoFs for aspc1_f1\n"
            "      !\n"
            "      undf_aspc1_f1 = f1_proxy%vspace%get_undf()\n"
            "      !\n"
            "      ! Set-up all of the loop bounds\n"
            "      !\n"
            "      loop0_start = 1\n"
            "      loop0_stop = undf_aspc1_f1\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      !\n"
            "      ! Zero summation variables\n"
            "      !\n"
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        asum = asum + f1_proxy%data(df) * f1_proxy%data(df)\n"
            "      END DO\n"
            "      !\n")
        assert output_seq in code
    else:
        output_dm = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_owned()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      !\n"
            "      ! Zero summation variables\n"
            "      !\n"
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        asum = asum + f1_proxy%data(df) * f1_proxy%data(df)\n"
            "      END DO\n"
            "      global_sum%value = asum\n"
            "      asum = global_sum%get_sum()\n"
            "      !\n")
        assert output_dm in code
        assert "      USE scalar_mod, ONLY: scalar_type" in code
        assert "      REAL(KIND=r_def), intent(out) :: asum\n" in code
        assert "      TYPE(scalar_type) global_sum\n" in code


def test_X_innerproduct_X_lowering(fortran_writer):
    '''
    Test that the lower_to_language_level() method of X_innerproduct_X
    built-in works as expected.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                           "15.9.2_X_innerproduct_X_builtin.f90"), api=API)
    psy = PSyFactory(API,
                     distributed_memory=False).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    kern.lower_to_language_level()
    loop = first_invoke.schedule.walk(Loop)[0]
    code = fortran_writer(loop)
    assert ("do df = loop0_start, loop0_stop, 1\n"
            "  asum = asum + f1_proxy%data(df) * f1_proxy%data(df)\n"
            "enddo") in code
