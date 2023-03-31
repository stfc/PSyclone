# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2023, Science and Technology Facilities Council.
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
# Authors: I. Kavcic, Met Office
#          A. R. Porter, STFC Daresbury Laboratory
# Modified: R. W. Ford, STFC Daresbury Lab

''' This module tests the support for integer built-in operations in the
    LFRic API using pytest. Currently all built-in operations are 'pointwise'
    in that they iterate over DOFs. However this may change in the future.

    TODO #1796 - break the tests for each built-in into separate files under
                 the 'builtins' directory.
'''

import os
import pytest

from psyclone.configuration import Config
from psyclone.domain.lfric.kernel import LFRicKernelMetadata
from psyclone.domain.lfric.lfric_builtins import LFRicRealXKern
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory

from psyclone.tests.lfric_build import LFRicBuild

# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "dynamo0p3")
# The PSyclone API under test
API = "dynamo0.3"

# pylint: disable=invalid-name
# ------------- Adding integer fields --------------------------------------- #


def test_int_X_plus_Y(tmpdir, monkeypatch, annexed, dist_mem):
    '''Test that 1) the '__str__' method of 'LFRicIntXPlusYKern' returns
    the expected string and 2) we generate correct code for the
    built-in 'Z = X + Y' where 'X' and 'Y' are integer-valued
    fields. Also check that we generate correct bounds when
    'Config.api_conf(API)._compute_annexed_dofs' is 'False' and
    'True'.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.21.1_int_X_plus_Y_builtin.f90"),
                           api=API)
    psy = PSyFactory(API,
                     distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: int_X_plus_Y (add integer-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Check for the correct field type declarations
    output = (
        "      TYPE(integer_field_type), intent(in) :: f3, f1, f2\n"
        "      INTEGER df\n"
        "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
        "      TYPE(integer_field_proxy_type) f3_proxy, f1_proxy, f2_proxy\n")
    assert output in code

    if not dist_mem:
        # The value of _compute_annexed_dofs should make no difference
        output = (
            "      f3_proxy = f3%get_proxy()\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      !\n"
            "      ! Initialise number of DoFs for aspc1_f3\n"
            "      !\n"
            "      undf_aspc1_f3 = f3_proxy%vspace%get_undf()\n"
            "      !\n"
            "      ! Set-up all of the loop bounds\n"
            "      !\n"
            "      loop0_start = 1\n"
            "      loop0_stop = undf_aspc1_f3\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f3_proxy%data(df) = f1_proxy%data(df) + "
            "f2_proxy%data(df)\n"
            "      END DO")
        assert output in code
    else:
        output_dm_2 = (
            "      loop0_stop = f3_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f3_proxy%data(df) = f1_proxy%data(df) + "
            "f2_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f3_proxy%set_dirty()\n"
            "      !\n")
        if not annexed:
            # Only compute owned dofs if _compute_annexed_dofs is False
            output_dm_2 = output_dm_2.replace("annexed", "owned")
        assert output_dm_2 in code


def test_int_inc_X_plus_Y(tmpdir, monkeypatch, annexed, dist_mem):
    '''Test that 1) the '__str__' method of 'LFRicIntIncXPlusYKern'
    returns the expected string and 2) we generate correct code for
    the built-in 'X = X + Y' where 'X' and 'Y' are integer-valued
    fields. Test with and without annexed dofs being computed as this
    affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)

    _, invoke_info = parse(os.path.join(
        BASE_PATH, "15.21.2_int_inc_X_plus_Y_builtin.f90"), api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert (str(kern) == "Built-in: int_inc_X_plus_Y (increment an "
            "integer-valued field)")
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      undf_aspc1_f1 = f1_proxy%vspace%get_undf()\n"
            "      !\n"
            "      ! Set-up all of the loop bounds\n"
            "      !\n"
            "      loop0_start = 1\n"
            "      loop0_stop = undf_aspc1_f1\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_proxy%data(df) = f1_proxy%data(df) + "
            "f2_proxy%data(df)\n"
            "      END DO\n")
        assert output in code
    else:
        output = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_proxy%data(df) = f1_proxy%data(df) + "
            "f2_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()")
        if not annexed:
            output = output.replace("dof_annexed", "dof_owned")
        assert output in code


def test_int_a_plus_X(tmpdir, monkeypatch, annexed, dist_mem):
    '''Test that 1) the '__str__' method of 'LFRicIntAPlusXKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'Y = a + X' where 'a' is an integer scalar and
    'X' and 'Y' are integer-valued fields. Test with and without
    annexed dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.21.3_int_a_plus_X_builtin.f90"),
                           api=API)

    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule[0].loop_body[0]
    assert str(kern) == "Built-in: int_a_plus_X (integer-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      loop0_stop = undf_aspc1_f2\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_proxy%data(df) = a + f1_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code
    else:
        output_dm = (
            "      loop0_stop = f2_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_proxy%data(df) = a + f1_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f2_proxy%set_dirty()\n"
            "      !\n")
        if not annexed:
            output_dm = output_dm.replace("dof_annexed", "dof_owned")
        assert output_dm in code


def test_int_inc_a_plus_X(tmpdir, monkeypatch, annexed, dist_mem):
    '''Test that 1) the '__str__' method of 'LFRicIntIncAPlusXKern'
    returns the expected string and 2) we generate correct code for
    the built-in operation 'X = a + X' where 'a' is an integer scalar
    and 'X' is an integer-valued field. Test with and without annexed
    dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "15.21.4_int_inc_a_plus_X_builtin.f90"), api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: int_inc_a_plus_X (integer-valued field)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      loop0_stop = undf_aspc1_f1\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_proxy%data(df) = a + f1_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0")
        assert output in code
    else:
        output_dm = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_proxy%data(df) = a + f1_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      !\n")
        if not annexed:
            output_dm = output_dm.replace("dof_annexed", "dof_owned")
        assert output_dm in code


# ------------- Subtracting integer fields ---------------------------------- #


def test_int_X_minus_Y(tmpdir, monkeypatch, annexed, dist_mem):
    '''Test that 1) the '__str__' method of 'LFRicIntXMinusYKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'Z = X - Y' where 'Z', 'X' and 'Y' are
    integer-valued fields. Test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.22.1_int_X_minus_Y_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert (str(kern) == "Built-in: int_X_minus_Y (subtract "
            "integer-valued fields)")
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      f3_proxy = f3%get_proxy()\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      !\n"
            "      ! Initialise number of DoFs for aspc1_f3\n"
            "      !\n"
            "      undf_aspc1_f3 = f3_proxy%vspace%get_undf()\n"
            "      !\n"
            "      ! Set-up all of the loop bounds\n"
            "      !\n"
            "      loop0_start = 1\n"
            "      loop0_stop = undf_aspc1_f3\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f3_proxy%data(df) = f1_proxy%data(df) - "
            "f2_proxy%data(df)\n"
            "      END DO")
        assert output in code
    else:
        output_dm_2 = (
            "      loop0_stop = f3_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f3_proxy%data(df) = f1_proxy%data(df) - "
            "f2_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f3_proxy%set_dirty()\n"
            "      !\n")
        if not annexed:
            output_dm_2 = output_dm_2.replace("dof_annexed", "dof_owned")
        assert output_dm_2 in code


def test_int_inc_X_minus_Y(tmpdir, monkeypatch, annexed, dist_mem):
    '''Test that 1) the '__str__' method of 'LFRicIntIncXMinusYKern'
    returns the expected string and 2) we generate correct code for
    the built-in operation 'X = X - Y' where 'X' and 'Y' are
    integer-valued fields. Test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.22.2_int_inc_X_minus_Y_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert (str(kern) == "Built-in: int_inc_X_minus_Y (decrement an "
            "integer-valued field)")
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      f1_proxy = f1%get_proxy()\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      !\n"
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
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_proxy%data(df) = f1_proxy%data(df) - "
            "f2_proxy%data(df)\n"
            "      END DO\n")
        assert output in code
    else:
        output = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_proxy%data(df) = f1_proxy%data(df) - "
            "f2_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()")
        if not annexed:
            output = output.replace("dof_annexed", "dof_owned")
        assert output in code


def test_int_a_minus_X(tmpdir, monkeypatch, annexed, dist_mem):
    '''Test that 1) the '__str__' method of 'LFRicIntAMinusXKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'Y = a - X' where 'a' is an integer scalar and
    'X' and 'Y' are integer-valued fields. Test with and without
    annexed dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.22.3_int_a_minus_X_builtin.f90"),
                           api=API)

    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule[0].loop_body[0]
    assert str(kern) == "Built-in: int_a_minus_X (integer-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Check for the correct 'use' module statements
    output_mod = (
        "    USE constants_mod, ONLY: i_def\n"
        "    USE integer_field_mod, ONLY: integer_field_type,"
        " integer_field_proxy_type\n")
    assert output_mod in code

    if not dist_mem:
        output = (
            "      loop0_stop = undf_aspc1_f2\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_proxy%data(df) = a - f1_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code
    else:
        output_dm = (
            "      loop0_stop = f2_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_proxy%data(df) = a - f1_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f2_proxy%set_dirty()\n"
            "      !\n")
        if not annexed:
            output_dm = output_dm.replace("dof_annexed", "dof_owned")
        assert output_dm in code


def test_int_inc_a_minus_X(tmpdir, monkeypatch, annexed, dist_mem):
    '''Test that 1) the '__str__' method of 'LFRicIntIncAMinusXKern'
    returns the expected string and 2) we generate correct code for
    the built-in operation 'X = a - X' where 'a' is an integer scalar
    and 'X' is an integer-valued field. Test with and without annexed
    dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "15.22.4_int_inc_a_minus_X_builtin.f90"), api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: int_inc_a_minus_X (integer-valued field)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      loop0_stop = undf_aspc1_f1\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_proxy%data(df) = a - f1_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0")
        assert output in code
    else:
        output_dm = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_proxy%data(df) = a - f1_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      !\n")
        if not annexed:
            output_dm = output_dm.replace("dof_annexed", "dof_owned")
        assert output_dm in code


def test_int_X_minus_a(tmpdir, monkeypatch, annexed, dist_mem):
    '''Test that 1) the '__str__' method of LFRicIntXMinusAKern returns
    the expected string and 2) we generate correct code for the
    built-in operation Y = X - a where 'a' is an integer scalar and X
    and Y are integer-valued fields. Test with and without annexed
    dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.22.5_int_X_minus_a_builtin.f90"),
                           api=API)

    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule[0].loop_body[0]
    assert str(kern) == "Built-in: int_X_minus_a (integer-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      loop0_stop = undf_aspc1_f2\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_proxy%data(df) = f1_proxy%data(df) - a\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code
    else:
        output_dm = (
            "      loop0_stop = f2_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_proxy%data(df) = f1_proxy%data(df) - a\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f2_proxy%set_dirty()\n"
            "      !\n")
        if not annexed:
            output_dm = output_dm.replace("dof_annexed", "dof_owned")
        assert output_dm in code


def test_int_inc_X_minus_a(tmpdir, monkeypatch, annexed, dist_mem):
    '''Test that 1) the '__str__' method of 'LFRicIntIncXMinusAKern'
    returns the expected string and 2) we generate correct code for
    the built-in operation 'X = X - a' where 'a' is an integer scalar
    and 'X' is an integer-valued field. Test with and without annexed
    dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "15.22.6_int_inc_X_minus_a_builtin.f90"), api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: int_inc_X_minus_a (integer-valued field)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      loop0_stop = undf_aspc1_f1\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_proxy%data(df) = f1_proxy%data(df) - a\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0")
        assert output in code
    else:
        output_dm = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_proxy%data(df) = f1_proxy%data(df) - a\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      !\n")
        if not annexed:
            output_dm = output_dm.replace("dof_annexed", "dof_owned")
        assert output_dm in code


# ------------- Multiplying integer fields ---------------------------------- #


def test_int_X_times_Y(tmpdir, monkeypatch, annexed, dist_mem):
    '''Test that 1) the '__str__' method of 'LFRicIntXTimesYKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'Z = X*Y' where 'Z', 'X' and 'Y' are
    integer-valued fields. Test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.23.1_int_X_times_Y_builtin.f90"), api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert (str(kern) == "Built-in: int_X_times_Y (multiply "
            "integer-valued fields)")
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(f3, f1, f2)\n"
            "      TYPE(integer_field_type), intent(in) :: f3, f1, f2\n"
            "      INTEGER df\n"
            "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
            "      TYPE(integer_field_proxy_type) f3_proxy, f1_proxy, "
            "f2_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f3\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
            "      f3_proxy = f3%get_proxy()\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      !\n"
            "      ! Initialise number of DoFs for aspc1_f3\n"
            "      !\n"
            "      undf_aspc1_f3 = f3_proxy%vspace%get_undf()\n"
            "      !\n"
            "      ! Set-up all of the loop bounds\n"
            "      !\n"
            "      loop0_start = 1\n"
            "      loop0_stop = undf_aspc1_f3\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f3_proxy%data(df) = f1_proxy%data(df) * "
            "f2_proxy%data(df)\n"
            "      END DO\n")
        assert output in code
    else:
        output = (
            "      loop0_stop = f3_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f3_proxy%data(df) = f1_proxy%data(df) * "
            "f2_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f3_proxy%set_dirty()")
        if not annexed:
            output = output.replace("dof_annexed", "dof_owned")
        assert output in code


def test_int_inc_X_times_Y(tmpdir, monkeypatch, annexed, dist_mem):
    '''Test that 1) the '__str__' method of 'LFRicIntIncXTimesYKern'
    returns the expected string and 2) we generate correct code for
    the built-in operation 'X = X*Y' where 'X' and 'Y' are
    integer-valued fields. Test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.23.2_int_inc_X_times_Y_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == ("Built-in: int_inc_X_times_Y (multiply one "
                         "integer-valued field by another)")
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      f1_proxy = f1%get_proxy()\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      !\n"
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
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_proxy%data(df) = f1_proxy%data(df) * "
            "f2_proxy%data(df)\n"
            "      END DO")
        assert output in code
    else:
        output_dm_2 = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_proxy%data(df) = f1_proxy%data(df) * "
            "f2_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      !\n")
        if not annexed:
            output_dm_2 = output_dm_2.replace("dof_annexed", "dof_owned")
        assert output_dm_2 in code


# ------------- Scaling integer fields (multiplying by an integer scalar) --- #


def test_int_a_times_X(tmpdir, monkeypatch, annexed, dist_mem):
    '''Test that 1) the '__str__' method of 'LFRicIntATimesXKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'Y = a*X' where 'a' is an integer scalar and
    'X' and 'Y' are integer-valued fields. Test with and without
    annexed dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.24.1_int_a_times_X_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert (str(kern) == "Built-in: int_a_times_X (copy a scaled "
            "integer-valued field)")
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      f2_proxy = f2%get_proxy()\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      !\n"
            "      ! Initialise number of DoFs for aspc1_f2\n"
            "      !\n"
            "      undf_aspc1_f2 = f2_proxy%vspace%get_undf()\n"
            "      !\n"
            "      ! Set-up all of the loop bounds\n"
            "      !\n"
            "      loop0_start = 1\n"
            "      loop0_stop = undf_aspc1_f2\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_proxy%data(df) = a_scalar * f1_proxy%data(df)\n"
            "      END DO")
        assert output in code
    else:
        output_dm = (
            "      loop0_stop = f2_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_proxy%data(df) = a_scalar * f1_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f2_proxy%set_dirty()\n"
            "      !\n")
        if not annexed:
            output_dm = output_dm.replace("dof_annexed", "dof_owned")
        assert output_dm in code


def test_int_inc_a_times_X(tmpdir, monkeypatch, annexed, dist_mem):
    '''Test that 1) the '__str__' method of 'LFRicIntIncATimesXKern'
    returns the expected string and 2) we generate correct code for
    the built-in operation 'X = a*X' where 'a' is an integer scalar
    and 'X' is an integer-valued field. Test with and without annexed
    dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.24.2_int_inc_a_times_X_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert (str(kern) == "Built-in: int_inc_a_times_X (scale an "
            "integer-valued field)")
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(a_scalar, f1)\n"
            "      INTEGER(KIND=i_def), intent(in) :: a_scalar\n"
            "      TYPE(integer_field_type), intent(in) :: f1\n"
            "      INTEGER df\n"
            "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
            "      TYPE(integer_field_proxy_type) f1_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f1\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      !\n"
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
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_proxy%data(df) = a_scalar * f1_proxy%data(df)\n"
            "      END DO\n"
            "      !\n")
        assert output in code
    else:
        output_dm = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_proxy%data(df) = a_scalar * f1_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()")
        if not annexed:
            output_dm = output_dm.replace("dof_annexed", "dof_owned")
        assert output_dm in code


# ------------- Setting integer field elements to an integer value ---------- #


def test_int_setval_c(tmpdir, monkeypatch, annexed, dist_mem):
    '''Test that 1) the '__str__' method of 'LFRicIntSetvalCKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'X = c' where 'c' is an integer constant scalar
    value and 'X' is an integer-valued field. Test with and without
    annexed dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.27.1_int_setval_c_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == ("Built-in: int_setval_c (set an integer-valued "
                         "field to a integer scalar value)")
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(f1, c)\n"
            "      INTEGER(KIND=i_def), intent(in) :: c\n"
            "      TYPE(integer_field_type), intent(in) :: f1\n"
            "      INTEGER df\n"
            "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
            "      TYPE(integer_field_proxy_type) f1_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f1\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      !\n"
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
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_proxy%data(df) = c\n"
            "      END DO")
        assert output in code
    else:
        output_dm_2 = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_proxy%data(df) = c\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      !\n")
        if not annexed:
            output_dm_2 = output_dm_2.replace("dof_annexed", "dof_owned")
        assert output_dm_2 in code


def test_int_setval_X(tmpdir, monkeypatch, annexed, dist_mem):
    '''Test that 1) the '__str__' method of 'LFRicIntSetvalXKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'Y = X' where 'X' and 'Y' are integer-valued
    fields. Also test with and without annexed dofs being computed as
    this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.27.2_int_setval_X_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == ("Built-in: int_setval_X (set an integer-valued "
                         "field equal to another such field)")
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(f2, f1)\n"
            "      TYPE(integer_field_type), intent(in) :: f2, f1\n"
            "      INTEGER df\n"
            "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
            "      TYPE(integer_field_proxy_type) f2_proxy, f1_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f2\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      !\n"
            "      ! Initialise number of DoFs for aspc1_f2\n"
            "      !\n"
            "      undf_aspc1_f2 = f2_proxy%vspace%get_undf()\n"
            "      !\n"
            "      ! Set-up all of the loop bounds\n"
            "      !\n"
            "      loop0_start = 1\n"
            "      loop0_stop = undf_aspc1_f2\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_proxy%data(df) = f1_proxy%data(df)\n"
            "      END DO")
        assert output in code
    else:
        output_dm_2 = (
            "      loop0_stop = f2_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_proxy%data(df) = f1_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f2_proxy%set_dirty()\n"
            "      !\n")
        if not annexed:
            output_dm_2 = output_dm_2.replace("dof_annexed", "dof_owned")
        assert output_dm_2 in code


# ------------- Sign of integer field elements ------------------------------ #


def test_int_sign_X(tmpdir, monkeypatch, annexed, dist_mem):
    '''Test that 1) the '__str__' method of 'LFRicIntSignXKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'Y = sign(a, X)' where 'a' is an integer scalar
    and 'Y' and 'X' are integer-valued fields. Test with and without
    annexed dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.28.1_int_sign_X_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert (str(kern) == "Built-in: int_sign_X (sign of an "
            "integer-valued field)")
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      loop0_stop = undf_aspc1_f2\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_proxy%data(df) = SIGN(a, f1_proxy%data(df))\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code
    else:
        output_dm_2 = (
            "      loop0_stop = f2_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_proxy%data(df) = SIGN(a, f1_proxy%data(df))\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f2_proxy%set_dirty()\n"
            "      !\n")
        if not annexed:
            output_dm_2 = output_dm_2.replace("dof_annexed", "dof_owned")
        assert output_dm_2 in code


# ------------- Maximum of (integer scalar, integer field elements) --------- #


def test_int_max_aX(tmpdir, monkeypatch, annexed, dist_mem):
    '''Test that 1) the '__str__' method of 'LFRicIntMaxAXKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'Y = max(a, X)' where 'a' is an integer scalar
    and 'Y' and 'X' are integer-valued fields. Test with and without
    annexed dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.28.3_int_max_aX_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: int_max_aX (integer-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Check for the correct field and scalar type declarations
    output = (
        "      INTEGER(KIND=i_def), intent(in) :: a\n"
        "      TYPE(integer_field_type), intent(in) :: f2, f1\n"
        "      INTEGER df\n"
        "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
        "      TYPE(integer_field_proxy_type) f2_proxy, f1_proxy\n")
    assert output in code

    if not dist_mem:
        assert "INTEGER(KIND=i_def) undf_aspc1_f2\n" in code
        output = (
            "      loop0_stop = undf_aspc1_f2\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_proxy%data(df) = MAX(a, f1_proxy%data(df))\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code
    else:
        assert "INTEGER(KIND=i_def) max_halo_depth_mesh\n" in code
        output_dm_2 = (
            "      loop0_stop = f2_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_proxy%data(df) = MAX(a, f1_proxy%data(df))\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f2_proxy%set_dirty()\n"
            "      !\n")
        if not annexed:
            output_dm_2 = output_dm_2.replace("dof_annexed", "dof_owned")
        assert output_dm_2 in code


def test_int_inc_max_aX(tmpdir, monkeypatch, annexed, dist_mem):
    '''Test that 1) the '__str__' method of 'LFRicIntIncMaxAXKern'
    returns the expected string and 2) we generate correct code for
    the built-in operation 'X = max(a, X)' where 'a' is an integer
    scalar and 'X' is an integer-valued field. Test with and without
    annexed dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.28.4_int_inc_max_aX_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: int_inc_max_aX (integer-valued field)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Check for the correct field and scalar type declarations
    output = (
        "      INTEGER(KIND=i_def), intent(in) :: a\n"
        "      TYPE(integer_field_type), intent(in) :: f1\n"
        "      INTEGER df\n"
        "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
        "      TYPE(integer_field_proxy_type) f1_proxy\n")
    assert output in code

    if not dist_mem:
        output = (
            "      loop0_stop = undf_aspc1_f1\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_proxy%data(df) = MAX(a, f1_proxy%data(df))\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code
    else:
        output_dm_2 = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_proxy%data(df) = MAX(a, f1_proxy%data(df))\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      !\n")
        if not annexed:
            output_dm_2 = output_dm_2.replace("dof_annexed", "dof_owned")
        assert output_dm_2 in code


# ------------- Minimum of (integer scalar, integer field elements) --------- #


def test_int_min_aX(tmpdir, monkeypatch, annexed, dist_mem):
    '''Test that 1) the '__str__' method of 'LFRicIntMinAXKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'Y = min(a, X)' where 'a' is an integer scalar
    and 'Y' and 'X' are integer-valued fields. Test with and without
    annexed dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.28.5_int_min_aX_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: int_min_aX (integer-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      loop0_stop = undf_aspc1_f2\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_proxy%data(df) = MIN(a, f1_proxy%data(df))\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code
    else:
        output_dm_2 = (
            "      loop0_stop = f2_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_proxy%data(df) = MIN(a, f1_proxy%data(df))\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f2_proxy%set_dirty()\n"
            "      !\n")
        if not annexed:
            output_dm_2 = output_dm_2.replace("dof_annexed", "dof_owned")
        assert output_dm_2 in code


def test_int_inc_min_aX(tmpdir, monkeypatch, annexed, dist_mem):
    '''Test that 1) the '__str__' method of 'LFRicIntIncMinAXKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'X = min(a, X)' where 'a' is an integer scalar
    and 'X' is an integer-valued field. Test with and without annexed
    dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.28.6_int_inc_min_aX_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: int_inc_min_aX (integer-valued field)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      loop0_stop = undf_aspc1_f1\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_proxy%data(df) = MIN(a, f1_proxy%data(df))\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code
    else:
        output_dm_2 = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_proxy%data(df) = MIN(a, f1_proxy%data(df))\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      !\n")
        if not annexed:
            output_dm_2 = output_dm_2.replace("dof_annexed", "dof_owned")
        assert output_dm_2 in code


# ------------- Converting integer to real field elements ------------------- #


def test_real_X(tmpdir, monkeypatch, annexed, dist_mem):
    '''Test that 1) the '__str__' method of 'LFRicRealXKern' returns the
    expected string and 2) we generate correct code for the built-in
    operation 'Y = real(X, r_def)' where 'Y' is a real-valued field,
    'X' is the integer-valued field being converted and the correct
    kind, 'r_def', is read from the PSyclone configuration file. Test
    with and without annexed dofs being computed as this affects the
    generated code. 3): Also test the 'metadata()' method.

    '''
    metadata = LFRicRealXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.28.2_real_X_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == ("Built-in: real_X (convert an integer-valued to a "
                         "real-valued field)")
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # First check that the correct field types and constants are used
    output = (
        "    USE constants_mod, ONLY: r_def, i_def\n"
        "    USE field_mod, ONLY: field_type, field_proxy_type\n"
        "    USE integer_field_mod, ONLY: integer_field_type, "
        "integer_field_proxy_type\n")
    assert output in code

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(f2, f1)\n"
            "      TYPE(field_type), intent(in) :: f2\n"
            "      TYPE(integer_field_type), intent(in) :: f1\n"
            "      INTEGER df\n"
            "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
            "      TYPE(integer_field_proxy_type) f1_proxy\n"
            "      TYPE(field_proxy_type) f2_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f2\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      !\n"
            "      ! Initialise number of DoFs for aspc1_f2\n"
            "      !\n"
            "      undf_aspc1_f2 = f2_proxy%vspace%get_undf()\n"
            "      !\n"
            "      ! Set-up all of the loop bounds\n"
            "      !\n"
            "      loop0_start = 1\n"
            "      loop0_stop = undf_aspc1_f2\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_proxy%data(df) = real(f1_proxy%data(df), r_def)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code
    else:
        output_dm_2 = (
            "      loop0_stop = f2_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_proxy%data(df) = real(f1_proxy%data(df), r_def)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f2_proxy%set_dirty()\n"
            "      !\n")
        if not annexed:
            output_dm_2 = output_dm_2.replace("dof_annexed", "dof_owned")
        assert output_dm_2 in code


@pytest.mark.parametrize("kind_name", ["r_solver", "r_tran"])
def test_real_X_precision(tmpdir, monkeypatch, kind_name):
    '''Test that the built-in picks up and creates correct code for a
    scalar with precision that is not the default i.e. not
    'r_def'. Try with two examples to make sure it works in general.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.28.2_real_X_builtin.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    monkeypatch.setattr(kern.args[0], "_precision", kind_name)
    code = str(psy.gen)
    assert f"USE constants_mod, ONLY: {kind_name}, i_def" in code
    assert f"f2_proxy%data(df) = real(f1_proxy%data(df), {kind_name})" in code
    # Test code generation
    assert LFRicBuild(tmpdir).code_compiles(psy)
