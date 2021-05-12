# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Author: I. Kavcic, Met Office

''' This module tests the support for integer built-in operations in the
    LFRic API using pytest. Currently all built-in operations are 'pointwise'
    in that they iterate over DOFs. However this may change in the future. '''

# imports
from __future__ import absolute_import, print_function
import os
import pytest

from psyclone.configuration import Config
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir.symbols import DeferredType, ScalarType

from psyclone.tests.lfric_build import LFRicBuild

# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "dynamo0p3")
# The PSyclone API under test
API = "dynamo0.3"


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use LFRic (Dynamo0.3) as API.'''
    Config.get().api = "dynamo0.3"


# ------------- Adding integer fields --------------------------------------- #


def test_int_X_plus_Y(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicIntXPlusYKern returns the
    expected string and 2) we generate correct code for the built-in
    Z = X + Y where X and Y are integer-valued fields. Also check that we
    generate correct bounds when Config.api_conf(API)._compute_annexed_dofs
    is False and True.

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
    assert str(kern) == "Built-in: Add integer-valued fields"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Check for the correct field type declarations
    output = (
        "      TYPE(integer_field_type), intent(in) :: f3, f1, f2\n"
        "      INTEGER df\n"
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
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=1,undf_aspc1_f3\n"
            "        f3_proxy%data(df) = f1_proxy%data(df) + "
            "f2_proxy%data(df)\n"
            "      END DO")
        assert output in code
    else:
        output_dm_2 = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f3_proxy%vspace%get_last_dof_annexed()\n"
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
    ''' Test that 1) the str method of LFRicIntIncXPlusYKern returns the
    expected string and 2) we generate correct code for the built-in
    X = X + Y where X and Y are integer-valued fields. Test with and without
    annexed dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)

    _, invoke_info = parse(os.path.join(
        BASE_PATH, "15.21.2_int_inc_X_plus_Y_builtin.f90"), api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: Increment an integer-valued field"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      undf_aspc1_f1 = f1_proxy%vspace%get_undf()\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=1,undf_aspc1_f1\n"
            "        f1_proxy%data(df) = f1_proxy%data(df) + "
            "f2_proxy%data(df)\n"
            "      END DO\n")
        assert output in code
    else:
        output = (
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f1_proxy%vspace%get_last_dof_annexed()\n"
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


def test_int_a_plus_X(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    ''' Test that 1) the str method of LFRicIntAPlusXKern returns the
    expected string and 2) we generate correct code for the built-in
    operation Y = a + X where 'a' is an integer scalar and X and Y
    are integer-valued fields. Test with and without annexed dofs being
    computed as this affects the generated code.
    Also tests the lower_to_language_level() method.

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
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=1,undf_aspc1_f2\n"
            "        f2_proxy%data(df) = a + f1_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code

        # Test the lower_to_language_level() method
        kern.lower_to_language_level()
        # Check the type of the scalar - it will be of DeferredType
        loop = first_invoke.schedule[0]
        scalar = loop.scope.symbol_table.lookup("a")
        assert isinstance(scalar.datatype, DeferredType)
        code = fortran_writer(loop)
        assert ("do df = 1, undf_aspc1_f2, 1\n"
                "  f2_proxy%data(df) = a + f1_proxy%data(df)\n"
                "enddo" in code)

        # Repeat but test the case where the symbol for the scalar argument
        # has not already been created.
        psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
        _ = psy.gen
        loop = psy.invokes.invoke_list[0].schedule[0]

        # We cannot use table.remove() as that does not support DataSymbols.
        # Therefore we monkeypatch the table.lookup() method to make it appear
        # that the scalar symbol does not exist.

        def fake_lookup(name):
            raise KeyError("For testing")
        monkeypatch.setattr(loop.loop_body.symbol_table, "lookup", fake_lookup)
        # This time, the lowering should cause a new symbol to be created for
        # the scalar argument.
        loop.loop_body[0].lower_to_language_level()
        monkeypatch.undo()

        code = fortran_writer(loop)
        assert ("do df = 1, undf_aspc1_f2, 1\n"
                "  f2_proxy%data(df) = a_1 + f1_proxy%data(df)\n"
                "enddo" in code)
        scalar = loop.loop_body[0].scope.symbol_table.lookup("a_1")
        assert isinstance(scalar.datatype, ScalarType)
        assert scalar.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
    else:
        output_dm_2 = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f2_proxy%vspace%get_last_dof_annexed()\n"
            "        f2_proxy%data(df) = a + f1_proxy%data(df)\n"
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


def test_int_inc_a_plus_X(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicIntIncAPlusXKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = a + X where 'a' is an integer scalar and X is an
    integer-valued field. Test with and without annexed dofs being
    computed as this affects the generated code.

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
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=1,undf_aspc1_f1\n"
            "        f1_proxy%data(df) = a + f1_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0")
        assert output in code
    else:
        output_dm_2 = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f1_proxy%vspace%get_last_dof_annexed()\n"
            "        f1_proxy%data(df) = a + f1_proxy%data(df)\n"
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


# ------------- Subtracting integer fields ---------------------------------- #


def test_int_X_minus_Y(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicIntXMinusYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation Z = X - Y where Z, X and Y are integer-valued fields. Test
    with and without annexed dofs being computed as this affects the
    generated code.

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
    assert str(kern) == "Built-in: Subtract integer-valued fields"
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
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=1,undf_aspc1_f3\n"
            "        f3_proxy%data(df) = f1_proxy%data(df) - "
            "f2_proxy%data(df)\n"
            "      END DO")
        assert output in code
    else:
        output_dm_2 = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f3_proxy%vspace%get_last_dof_annexed()\n"
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
    ''' Test that 1) the str method of LFRicIntIncXMinusYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = X - Y where X and Y are integer-valued fields. Test with and
    without annexed dofs being computed as this affects the generated code.

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
    assert str(kern) == "Built-in: Decrement an integer-valued field"
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
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=1,undf_aspc1_f1\n"
            "        f1_proxy%data(df) = f1_proxy%data(df) - "
            "f2_proxy%data(df)\n"
            "      END DO\n")
        assert output in code
    else:
        output = (
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f1_proxy%vspace%get_last_dof_annexed()\n"
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


# ------------- Multiplying integer fields ---------------------------------- #


def test_int_X_times_Y(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicIntXTimesYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation Z = X*Y where Z, X and Y are integer-valued fields. Test
    with and without annexed dofs being computed as this affects the
    generated code.

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
    assert str(kern) == "Built-in: Multiply integer-valued fields"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(f3, f1, f2)\n"
            "      TYPE(integer_field_type), intent(in) :: f3, f1, f2\n"
            "      INTEGER df\n"
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
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=1,undf_aspc1_f3\n"
            "        f3_proxy%data(df) = f1_proxy%data(df) * "
            "f2_proxy%data(df)\n"
            "      END DO\n")
        assert output in code
    else:
        output = (
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f3_proxy%vspace%get_last_dof_annexed()\n"
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
    ''' Test that 1) the str method of LFRicIntIncXTimesYKern returns
    the expected string and 2) we generate correct code for the built-in
    operation X = X*Y where X and Y are integer-valued fields. Test with and
    without annexed dofs being computed as this affects the generated code.

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
    assert str(kern) == ("Built-in: Multiply one integer-valued field "
                         "by another")
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
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=1,undf_aspc1_f1\n"
            "        f1_proxy%data(df) = f1_proxy%data(df) * "
            "f2_proxy%data(df)\n"
            "      END DO")
        assert output in code
    else:
        output_dm_2 = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f1_proxy%vspace%get_last_dof_annexed()\n"
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
    ''' Test that 1) the str method of LFRicIntATimesXKern returns the
    expected string and 2) we generate correct code for the built-in
    operation Y = a*X where 'a' is an integer scalar and X and Y are
    integer-valued fields. Test with and without annexed dofs being
    computed as this affects the generated code.

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
    assert str(kern) == "Built-in: Copy a scaled integer-valued field"
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
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=1,undf_aspc1_f2\n"
            "        f2_proxy%data(df) = a_scalar * f1_proxy%data(df)\n"
            "      END DO")
        assert output in code
    else:
        output_dm_2 = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f2_proxy%vspace%get_last_dof_annexed()\n"
            "        f2_proxy%data(df) = a_scalar * f1_proxy%data(df)\n"
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


def test_int_inc_a_times_X(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicIntIncATimesXKern returns
    the expected string and 2) we generate correct code for the
    built-in operation X = a*X where 'a' is an integer scalar and X is
    an integer-valued field. Test with and without annexed dofs being
    computed as this affects the generated code.

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
    assert str(kern) == "Built-in: Scale an integer-valued field"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(a, f1, b, f2, f3)\n"
            "      INTEGER(KIND=i_def), intent(in) :: a, b\n"
            "      TYPE(integer_field_type), intent(in) :: f1, f2, f3\n"
            "      INTEGER df\n"
            "      INTEGER(KIND=i_def) ndf_aspc1_f1, "
            "undf_aspc1_f1\n"
            "      INTEGER(KIND=i_def) nlayers\n"
            "      TYPE(field_proxy_type) f1_proxy, f2_proxy, f3_proxy\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      f3_proxy = f3%get_proxy()\n"
            "      !\n"
            "      ! Initialise number of layers\n"
            "      !\n"
            "      nlayers = f1_proxy%vspace%get_nlayers()\n"
            "      !\n"
            "      ! Initialise number of DoFs for aspc1_f1\n"
            "      !\n"
            "      ndf_aspc1_f1 = f1_proxy%vspace%get_ndf()\n"
            "      undf_aspc1_f1 = f1_proxy%vspace%get_undf()\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=1,undf_aspc1_f1\n"
            "        f1_proxy%data(df) = a_scalar * f1_proxy%data(df)\n"
            "      END DO\n"
            "      !\n")
    else:
        output = (
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f1_proxy%vspace%get_last_dof_annexed()\n"
            "        f1_proxy%data(df) = a_scalar * f1_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()")
        if not annexed:
            output = output.replace("dof_annexed", "dof_owned")
        assert output in code


# ------------- Setting integer field elements to an integer value ---------- #


def test_int_setval_c(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicIntSetvalCKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = c where 'c' is an integer constant scalar value and X
    is an integer-valued field. Test with and without annexed dofs being
    computed as this affects the generated code.

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
    assert str(kern) == ("Built-in: Set an integer-valued field to an "
                         "integer scalar value")
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(f1, c)\n"
            "      INTEGER(KIND=i_def), intent(in) :: c\n"
            "      TYPE(integer_field_type), intent(in) :: f1\n"
            "      INTEGER df\n"
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
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=1,undf_aspc1_f1\n"
            "        f1_proxy%data(df) = c\n"
            "      END DO")
        assert output in code
    else:
        output_dm_2 = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f1_proxy%vspace%get_last_dof_annexed()\n"
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
    ''' Test that 1) the str method of LFRicIntSetvalXKern returns the
    expected string and 2) we generate correct code for the built-in operation
    Y = X where X and Y are integer-valued fields. Also test with and
    without annexed dofs being computed as this affects the generated code.

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
    assert str(kern) == ("Built-in: Set an integer-valued field equal to "
                         "another such field")
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(f2, f1)\n"
            "      TYPE(integer_field_type), intent(in) :: f2, f1\n"
            "      INTEGER df\n"
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
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=1,undf_aspc1_f2\n"
            "        f2_proxy%data(df) = f1_proxy%data(df)\n"
            "      END DO")
        assert output in code
    else:
        output_dm_2 = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f2_proxy%vspace%get_last_dof_annexed()\n"
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
    ''' Test that 1) the str method of LFRicIntSignXKern returns the
    expected string and 2) we generate correct code for the built-in
    operation Y = sign(a, X) where 'a' is an integer scalar and Y and X
    are integer-valued fields. Test with and without annexed dofs
    being computed as this affects the generated code.

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
    assert str(kern) == "Built-in: Sign of an integer-valued field"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=1,undf_aspc1_f2\n"
            "        f2_proxy%data(df) = sign(a, f1_proxy%data(df))\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code
    else:
        output_dm_2 = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f2_proxy%vspace%get_last_dof_annexed()\n"
            "        f2_proxy%data(df) = sign(a, f1_proxy%data(df))\n"
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


# ------------- Converting integer to real field elements ------------------- #


def test_real_X(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicRealXKern returns the
    expected string and 2) we generate correct code for the built-in
    operation Y = real(X, r_def) where Y is a real-valued field, X is the
    integer-valued field being converted and the correct kind, 'r_def',
    is read from the PSyclone configuration file. Test with and without
    annexed dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.28.2_real_X_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == ("Built-in: Convert an integer-valued to a "
                         "real-valued field")
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
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=1,undf_aspc1_f2\n"
            "        f2_proxy%data(df) = real(f1_proxy%data(df), r_def)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code
    else:
        output_dm_2 = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f2_proxy%vspace%get_last_dof_annexed()\n"
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
