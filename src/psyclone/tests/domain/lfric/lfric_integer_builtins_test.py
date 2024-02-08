# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
    in that they iterate over DoFs. However this may change in the future.

    TODO #1796 - break the tests for each built-in into separate files under
                 the 'builtins' directory.
'''

import os

from psyclone.configuration import Config
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir.nodes import Loop
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

    # Check for the correct 'use' module statements
    output_mod = (
        "    USE constants_mod, ONLY: i_def\n"
        "    USE integer_field_mod, ONLY: integer_field_type,"
        " integer_field_proxy_type\n")
    assert output_mod in code

    # Check for the correct declarations
    output = (
        "      TYPE(integer_field_type), intent(in) :: f3, f1, f2\n"
        "      INTEGER df\n"
        "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: f2_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: f1_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: f3_data => "
        "null()\n"
        "      TYPE(integer_field_proxy_type) f3_proxy, f1_proxy, f2_proxy\n")
    assert output in code

    if not dist_mem:
        # The value of _compute_annexed_dofs should make no difference
        output = (
            "      f3_proxy = f3%get_proxy()\n"
            "      f3_data => f3_proxy%data\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      f1_data => f1_proxy%data\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      f2_data => f2_proxy%data\n"
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
            "        f3_data(df) = f1_data(df) + f2_data(df)\n"
            "      END DO")
        assert output in code
    else:
        output_dm_2 = (
            "      loop0_stop = f3_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f3_data(df) = f1_data(df) + "
            "f2_data(df)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f3_proxy%set_dirty()\n"
            "      !\n")
        if not annexed:
            # Only compute owned DoFs if _compute_annexed_dofs is False
            output_dm_2 = output_dm_2.replace("annexed", "owned")
        assert output_dm_2 in code

    # Test compilation of generated code
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_int_inc_X_plus_Y(tmpdir, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIntIncXPlusYKern'
    returns the expected string and 2) we generate correct code for
    the built-in 'X = X + Y' where 'X' and 'Y' are integer-valued
    fields.

    '''
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "15.21.2_int_inc_X_plus_Y_builtin.f90"), api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)

    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert (str(kern) == "Built-in: int_inc_X_plus_Y (increment an "
            "integer-valued field)")

    # Test compilation of generated code
    code = str(psy.gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Test lower_to_language_level() method
    kern.lower_to_language_level()
    loop = first_invoke.schedule.walk(Loop)[0]
    code = fortran_writer(loop)
    output = (
        "do df = loop0_start, loop0_stop, 1\n"
        "  f1_data(df) = f1_data(df) + f2_data(df)\n"
        "enddo")
    assert output in code


def test_int_a_plus_X(tmpdir, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIntAPlusXKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'Y = a + X' where 'a' is an integer scalar and
    'X' and 'Y' are integer-valued fields.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.21.3_int_a_plus_X_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)

    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule[0].loop_body[0]
    assert str(kern) == "Built-in: int_a_plus_X (integer-valued fields)"

    # Test compilation of generated code
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Test lower_to_language_level() method
    kern.lower_to_language_level()
    loop = first_invoke.schedule.walk(Loop)[0]
    code = fortran_writer(loop)
    output = (
        "do df = loop0_start, loop0_stop, 1\n"
        "  f2_data(df) = a + f1_data(df)\n"
        "enddo")
    assert output in code


def test_int_inc_a_plus_X(tmpdir, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIntIncAPlusXKern'
    returns the expected string and 2) we generate correct code for
    the built-in operation 'X = a + X' where 'a' is an integer scalar
    and 'X' is an integer-valued field.

    '''
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "15.21.4_int_inc_a_plus_X_builtin.f90"), api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)

    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: int_inc_a_plus_X (integer-valued field)"

    # Test compilation of generated code
    code = str(psy.gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Test lower_to_language_level() method
    kern.lower_to_language_level()
    loop = first_invoke.schedule.walk(Loop)[0]
    code = fortran_writer(loop)
    output = (
        "do df = loop0_start, loop0_stop, 1\n"
        "  f1_data(df) = a + f1_data(df)\n"
        "enddo")
    assert output in code


# ------------- Subtracting integer fields ---------------------------------- #


def test_int_X_minus_Y(tmpdir, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIntXMinusYKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'Z = X - Y' where 'Z', 'X' and 'Y' are
    integer-valued fields.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.22.1_int_X_minus_Y_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)

    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert (str(kern) == "Built-in: int_X_minus_Y (subtract "
            "integer-valued fields)")

    # Test compilation of generated code
    code = str(psy.gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Test lower_to_language_level() method
    kern.lower_to_language_level()
    loop = first_invoke.schedule.walk(Loop)[0]
    code = fortran_writer(loop)
    output = (
        "do df = loop0_start, loop0_stop, 1\n"
        "  f3_data(df) = f1_data(df) - f2_data(df)\n"
        "enddo")
    assert output in code


def test_int_inc_X_minus_Y(tmpdir, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIntIncXMinusYKern'
    returns the expected string and 2) we generate correct code for
    the built-in operation 'X = X - Y' where 'X' and 'Y' are
    integer-valued fields.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.22.2_int_inc_X_minus_Y_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)

    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert (str(kern) == "Built-in: int_inc_X_minus_Y (decrement an "
            "integer-valued field)")

    # Test compilation of generated code
    code = str(psy.gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Test lower_to_language_level() method
    kern.lower_to_language_level()
    loop = first_invoke.schedule.walk(Loop)[0]
    code = fortran_writer(loop)
    output = (
        "do df = loop0_start, loop0_stop, 1\n"
        "  f1_data(df) = f1_data(df) - f2_data(df)\n"
        "enddo")
    assert output in code


def test_int_a_minus_X(tmpdir, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIntAMinusXKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'Y = a - X' where 'a' is an integer scalar and
    'X' and 'Y' are integer-valued fields.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.22.3_int_a_minus_X_builtin.f90"),
                           api=API)

    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)

    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule[0].loop_body[0]
    assert str(kern) == "Built-in: int_a_minus_X (integer-valued fields)"

    # Test compilation of generated code
    code = str(psy.gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Test lower_to_language_level() method
    kern.lower_to_language_level()
    loop = first_invoke.schedule.walk(Loop)[0]
    code = fortran_writer(loop)
    output = (
        "do df = loop0_start, loop0_stop, 1\n"
        "  f2_data(df) = a - f1_data(df)\n"
        "enddo")
    assert output in code


def test_int_inc_a_minus_X(tmpdir, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIntIncAMinusXKern'
    returns the expected string and 2) we generate correct code for
    the built-in operation 'X = a - X' where 'a' is an integer scalar
    and 'X' is an integer-valued field.

    '''
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "15.22.4_int_inc_a_minus_X_builtin.f90"), api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)

    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: int_inc_a_minus_X (integer-valued field)"

    # Test compilation of generated code
    code = str(psy.gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Test lower_to_language_level() method
    kern.lower_to_language_level()
    loop = first_invoke.schedule.walk(Loop)[0]
    code = fortran_writer(loop)
    output = (
        "do df = loop0_start, loop0_stop, 1\n"
        "  f1_data(df) = a - f1_data(df)\n"
        "enddo")
    assert output in code


def test_int_X_minus_a(tmpdir, fortran_writer):
    '''Test that 1) the '__str__' method of LFRicIntXMinusAKern returns
    the expected string and 2) we generate correct code for the
    built-in operation Y = X - a where 'a' is an integer scalar and X
    and Y are integer-valued fields.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.22.5_int_X_minus_a_builtin.f90"),
                           api=API)

    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)

    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule[0].loop_body[0]
    assert str(kern) == "Built-in: int_X_minus_a (integer-valued fields)"

    # Test compilation of generated code
    code = str(psy.gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Test lower_to_language_level() method
    kern.lower_to_language_level()
    loop = first_invoke.schedule.walk(Loop)[0]
    code = fortran_writer(loop)
    output = (
        "do df = loop0_start, loop0_stop, 1\n"
        "  f2_data(df) = f1_data(df) - a\n"
        "enddo")
    assert output in code


def test_int_inc_X_minus_a(tmpdir, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIntIncXMinusAKern'
    returns the expected string and 2) we generate correct code for
    the built-in operation 'X = X - a' where 'a' is an integer scalar
    and 'X' is an integer-valued field.

    '''
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "15.22.6_int_inc_X_minus_a_builtin.f90"), api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)

    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: int_inc_X_minus_a (integer-valued field)"

    # Test compilation of generated code
    code = str(psy.gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Test lower_to_language_level() method
    kern.lower_to_language_level()
    loop = first_invoke.schedule.walk(Loop)[0]
    output = (
        "do df = loop0_start, loop0_stop, 1\n"
        "  f1_data(df) = f1_data(df) - a\n"
        "enddo")
    code = fortran_writer(loop)
    assert output in code


# ------------- Multiplying integer fields ---------------------------------- #


def test_int_X_times_Y(tmpdir, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIntXTimesYKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'Z = X*Y' where 'Z', 'X' and 'Y' are
    integer-valued fields.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.23.1_int_X_times_Y_builtin.f90"), api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)

    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert (str(kern) == "Built-in: int_X_times_Y (multiply "
            "integer-valued fields)")

    # Test compilation of generated code
    code = str(psy.gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Test lower_to_language_level() method
    kern.lower_to_language_level()
    loop = first_invoke.schedule.walk(Loop)[0]
    code = fortran_writer(loop)
    output = (
        "do df = loop0_start, loop0_stop, 1\n"
        "  f3_data(df) = f1_data(df) * f2_data(df)\n"
        "enddo")
    assert output in code


def test_int_inc_X_times_Y(tmpdir, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIntIncXTimesYKern'
    returns the expected string and 2) we generate correct code for
    the built-in operation 'X = X*Y' where 'X' and 'Y' are
    integer-valued fields.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.23.2_int_inc_X_times_Y_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)

    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == ("Built-in: int_inc_X_times_Y (multiply one "
                         "integer-valued field by another)")

    # Test compilation of generated code
    code = str(psy.gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Test lower_to_language_level() method
    kern.lower_to_language_level()
    loop = first_invoke.schedule.walk(Loop)[0]
    code = fortran_writer(loop)
    output = (
        "do df = loop0_start, loop0_stop, 1\n"
        "  f1_data(df) = f1_data(df) * f2_data(df)\n"
        "enddo")
    assert output in code


# ------------- Scaling integer fields (multiplying by an integer scalar) --- #


def test_int_a_times_X(tmpdir, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIntATimesXKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'Y = a*X' where 'a' is an integer scalar and
    'X' and 'Y' are integer-valued fields.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.24.1_int_a_times_X_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)

    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert (str(kern) == "Built-in: int_a_times_X (copy a scaled "
            "integer-valued field)")

    # Test compilation of generated code
    code = str(psy.gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Test lower_to_language_level() method
    kern.lower_to_language_level()
    loop = first_invoke.schedule.walk(Loop)[0]
    code = fortran_writer(loop)
    output = (
        "do df = loop0_start, loop0_stop, 1\n"
        "  f2_data(df) = a_scalar * f1_data(df)\n"
        "enddo")
    assert output in code


def test_int_inc_a_times_X(tmpdir, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIntIncATimesXKern'
    returns the expected string and 2) we generate correct code for
    the built-in operation 'X = a*X' where 'a' is an integer scalar
    and 'X' is an integer-valued field.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.24.2_int_inc_a_times_X_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)

    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert (str(kern) == "Built-in: int_inc_a_times_X (scale an "
            "integer-valued field)")

    # Test compilation of generated code
    code = str(psy.gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Test lower_to_language_level() method
    kern.lower_to_language_level()
    loop = first_invoke.schedule.walk(Loop)[0]
    code = fortran_writer(loop)
    output = (
        "do df = loop0_start, loop0_stop, 1\n"
        "  f1_data(df) = a_scalar * f1_data(df)\n"
        "enddo")
    assert output in code


# ------------- Setting integer field elements to an integer value ---------- #


def test_int_setval_c(tmpdir, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIntSetvalCKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'X = c' where 'c' is an integer constant scalar
    value and 'X' is an integer-valued field.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.27.1_int_setval_c_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)

    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == ("Built-in: int_setval_c (set an integer-valued "
                         "field to a integer scalar value)")

    # Test compilation of generated code
    code = str(psy.gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Test lower_to_language_level() method
    kern.lower_to_language_level()
    loop = first_invoke.schedule.walk(Loop)[0]
    code = fortran_writer(loop)
    output = (
        "do df = loop0_start, loop0_stop, 1\n"
        "  f1_data(df) = c\n"
        "enddo")
    assert output in code


def test_int_setval_X(tmpdir, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIntSetvalXKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'Y = X' where 'X' and 'Y' are integer-valued
    fields.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.27.2_int_setval_X_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)

    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == ("Built-in: int_setval_X (set an integer-valued "
                         "field equal to another such field)")

    # Test compilation of generated code
    code = str(psy.gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Test lower_to_language_level() method
    kern.lower_to_language_level()
    loop = first_invoke.schedule.walk(Loop)[0]
    code = fortran_writer(loop)
    output = (
        "do df = loop0_start, loop0_stop, 1\n"
        "  f2_data(df) = f1_data(df)\n"
        "enddo")
    assert output in code


# ------------- Sign of integer field elements ------------------------------ #


def test_int_sign_X(tmpdir, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIntSignXKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'Y = sign(a, X)' where 'a' is an integer scalar
    and 'Y' and 'X' are integer-valued fields.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.28.1_int_sign_X_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)

    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert (str(kern) == "Built-in: int_sign_X (sign of an "
            "integer-valued field)")

    # Test compilation of generated code
    code = str(psy.gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Test lower_to_language_level() method
    kern.lower_to_language_level()
    loop = first_invoke.schedule.walk(Loop)[0]
    code = fortran_writer(loop)
    output = (
        "do df = loop0_start, loop0_stop, 1\n"
        "  f2_data(df) = SIGN(a, f1_data(df))\n"
        "enddo")
    assert output in code


# ------------- Maximum of (integer scalar, integer field elements) --------- #


def test_int_max_aX(tmpdir, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIntMaxAXKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'Y = max(a, X)' where 'a' is an integer scalar
    and 'Y' and 'X' are integer-valued fields.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.28.3_int_max_aX_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)

    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: int_max_aX (integer-valued fields)"

    # Test compilation of generated code
    code = str(psy.gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Test lower_to_language_level() method
    kern.lower_to_language_level()
    loop = first_invoke.schedule.walk(Loop)[0]
    code = fortran_writer(loop)
    output = (
        "do df = loop0_start, loop0_stop, 1\n"
        "  f2_data(df) = MAX(a, f1_data(df))\n"
        "enddo")
    assert output in code


def test_int_inc_max_aX(tmpdir, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIntIncMaxAXKern'
    returns the expected string and 2) we generate correct code for
    the built-in operation 'X = max(a, X)' where 'a' is an integer
    scalar and 'X' is an integer-valued field.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.28.4_int_inc_max_aX_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)

    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: int_inc_max_aX (integer-valued field)"

    # Test compilation of generated code
    code = str(psy.gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Test lower_to_language_level() method
    kern.lower_to_language_level()
    loop = first_invoke.schedule.walk(Loop)[0]
    code = fortran_writer(loop)
    output = (
        "do df = loop0_start, loop0_stop, 1\n"
        "  f1_data(df) = MAX(a, f1_data(df))\n"
        "enddo")
    assert output in code


# ------------- Minimum of (integer scalar, integer field elements) --------- #


def test_int_min_aX(tmpdir, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIntMinAXKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'Y = min(a, X)' where 'a' is an integer scalar
    and 'Y' and 'X' are integer-valued fields.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.28.5_int_min_aX_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)

    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: int_min_aX (integer-valued fields)"

    # Test compilation of generated code
    code = str(psy.gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Test lower_to_language_level() method
    kern.lower_to_language_level()
    loop = first_invoke.schedule.walk(Loop)[0]
    code = fortran_writer(loop)
    output = (
        "do df = loop0_start, loop0_stop, 1\n"
        "  f2_data(df) = MIN(a, f1_data(df))\n"
        "enddo")
    assert output in code


def test_int_inc_min_aX(tmpdir, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIntIncMinAXKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'X = min(a, X)' where 'a' is an integer scalar
    and 'X' is an integer-valued field.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.28.6_int_inc_min_aX_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)

    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: int_inc_min_aX (integer-valued field)"

    # Test compilation of generated code
    code = str(psy.gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Test lower_to_language_level() method
    kern.lower_to_language_level()
    loop = first_invoke.schedule.walk(Loop)[0]
    code = fortran_writer(loop)
    output = (
        "do df = loop0_start, loop0_stop, 1\n"
        "  f1_data(df) = MIN(a, f1_data(df))\n"
        "enddo")
    assert output in code
