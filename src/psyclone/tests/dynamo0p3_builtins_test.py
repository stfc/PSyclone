# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017, Science and Technology Facilities Council
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
# Author A. R. Porter, STFC Daresbury Lab
# Modified I. Kavcic, Met Office

''' This module tests the support for built-in operations in the Dynamo 0.3 API
    using pytest. Currently all built-in operations are 'pointwise' in that
    they iterate over DOFs. However this may change in the future. '''

# imports
from __future__ import absolute_import
import os
import pytest
from psyclone.parse import parse, ParseError
from psyclone.psyGen import PSyFactory, GenerationError
from psyclone import dynamo0p3_builtins
import utils

# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")


# ------------- Tests for built-ins methods and arguments ------------------- #


def test_dynbuiltin_missing_defs():
    ''' Check that we raise an appropriate error if we cannot find the
    file specifying meta-data for built-in kernels '''
    old_name = dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE[:]
    dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE = 'broken'
    with pytest.raises(ParseError) as excinfo:
        _, _ = parse(os.path.join(BASE_PATH,
                                  "15.12.3_single_pointwise_builtin.f90"),
                     api="dynamo0.3")
    dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE = old_name
    assert ("broken' containing the meta-data describing the "
            "Built-in operations" in str(excinfo.value))


def test_dynbuiltin_not_over_dofs():
    ''' Check that we raise an appropriate error if we encounter a
    built-in that does not iterate over dofs '''
    old_name = dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE[:]
    dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE = \
        os.path.join(BASE_PATH, "not_dofs_builtins_mod.f90")
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.12.3_single_pointwise_builtin.f90"),
        api="dynamo0.3")
    # Restore the original file name before doing the assert in case
    # it fails
    dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE = old_name
    with pytest.raises(NotImplementedError) as excinfo:
        _ = PSyFactory("dynamo0.3",
                       distributed_memory=False).create(invoke_info)
    assert ("built-in calls must iterate over DoFs but found cells for "
            "Built-in: Set field " in str(excinfo.value))


def test_builtin_multiple_writes():
    ''' Check that we raise an appropriate error if we encounter a built-in
    that writes to more than one argument '''
    # The file containing broken meta-data for the built-ins
    old_name = dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE[:]
    # Define the built-in name and test file
    test_builtin_name = "aX_plus_Y"
    test_builtin_file = "15.13.1_" + test_builtin_name + \
                        "_builtin_set_by_value.f90"
    dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE = \
        os.path.join(BASE_PATH, "invalid_builtins_mod.f90")
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        test_builtin_file),
                           api="dynamo0.3")
    dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE = old_name
    with pytest.raises(ParseError) as excinfo:
        _ = PSyFactory("dynamo0.3",
                       distributed_memory=False).create(invoke_info)
    assert ("A built-in kernel in the Dynamo 0.3 API must have one and only "
            "one argument that is written to but found 2 for kernel " +
            test_builtin_name.lower() in str(excinfo))


def test_builtin_write_and_inc():
    ''' Check that we raise an appropriate error if we encounter a built-in
    that updates more than one argument where one is gh_write and one is
    gh_inc '''
    # The file containing broken meta-data for the built-ins
    old_name = dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE[:]
    # Define the built-in name and test file
    test_builtin_name = "inc_aX_plus_bY"
    test_builtin_file = "15.1.7_" + test_builtin_name + "_builtin.f90"
    dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE = \
        os.path.join(BASE_PATH, "invalid_builtins_mod.f90")
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        test_builtin_file),
                           api="dynamo0.3")
    dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE = old_name
    with pytest.raises(ParseError) as excinfo:
        _ = PSyFactory("dynamo0.3",
                       distributed_memory=False).create(invoke_info)
    assert ("A built-in kernel in the Dynamo 0.3 API must have one and only "
            "one argument that is written to but found 2 for kernel " +
            test_builtin_name.lower() in str(excinfo))


def test_builtin_sum_and_inc():
    ''' Check that we raise an appropriate error if we encounter a built-in
    that updates more than one argument where one is gh_sum and one is
    gh_inc '''
    # The file containing broken meta-data for the built-ins
    old_name = dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE[:]
    # Define the built-in name and test file
    test_builtin_name = "inc_aX_plus_Y"
    test_builtin_file = "15.1.4_" + test_builtin_name + "_builtin.f90"
    dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE = \
        os.path.join(BASE_PATH, "invalid_builtins_mod.f90")
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        test_builtin_file),
                           api="dynamo0.3")
    dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE = old_name
    with pytest.raises(ParseError) as excinfo:
        _ = PSyFactory("dynamo0.3",
                       distributed_memory=False).create(invoke_info)
    assert ("A built-in kernel in the Dynamo 0.3 API must have one and "
            "only one argument that is written to but found 2 for kernel " +
            test_builtin_name.lower() in str(excinfo))


def test_builtin_zero_writes(monkeypatch):
    ''' Check that we raise an appropriate error if we encounter a built-in
    that does not write to any field '''
    # Use pytest's monkeypatch support to change our configuration to
    # point to a file containing broken meta-data for the
    # built-ins. The definition for aX_plus_bY that it contains erroneously
    # has no argument that is written to.
    # Define the built-in name and test file
    test_builtin_name = "aX_plus_bY"
    test_builtin_file = "15.1.6_" + test_builtin_name + "_builtin.f90"
    monkeypatch.setattr(dynamo0p3_builtins, "BUILTIN_DEFINITIONS_FILE",
                        value=os.path.join(BASE_PATH,
                                           "invalid_builtins_mod.f90"))
    with pytest.raises(ParseError) as excinfo:
        _, _ = parse(os.path.join(BASE_PATH,
                                  test_builtin_file),
                     api="dynamo0.3")
    assert ("A Dynamo 0.3 kernel must have at least one "
            "argument that is updated (written to) but "
            "found none for kernel " + test_builtin_name.lower()
            in str(excinfo))


def test_builtin_no_field_args():
    ''' Check that we raise appropriate error if we encounter a built-in
    that does not have any field arguments '''
    old_name = dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE[:]
    # Define the built-in name and test file
    test_builtin_name = "setval_X"
    test_builtin_file = "15.7.2_" + test_builtin_name + "_builtin.f90"
    dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE = \
        os.path.join(BASE_PATH, "invalid_builtins_mod.f90")
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        test_builtin_file),
                           api="dynamo0.3")
    dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE = old_name
    with pytest.raises(ParseError) as excinfo:
        _ = PSyFactory("dynamo0.3",
                       distributed_memory=False).create(invoke_info)
    assert ("A built-in kernel in the Dynamo 0.3 API "
            "must have at least one field as an argument but "
            "kernel " + test_builtin_name.lower() + " has none"
            in str(excinfo))


def test_builtin_operator_arg():
    ''' Check that we raise appropriate error if we encounter a built-in
    that takes something other than a field or scalar argument '''
    old_name = dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE[:]
    # Change the builtin-definitions file to point to one that has
    # various invalid definitions
    # Define the built-in name and test file
    test_builtin_name = "a_times_X"
    test_builtin_file = "15.4.1_" + test_builtin_name + "_builtin.f90"
    dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE = \
        os.path.join(BASE_PATH, "invalid_builtins_mod.f90")
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     test_builtin_file),
        api="dynamo0.3")
    dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE = old_name
    with pytest.raises(ParseError) as excinfo:
        _ = PSyFactory("dynamo0.3",
                       distributed_memory=False).create(invoke_info)
    assert ("In the Dynamo 0.3 API an argument to a built-in kernel "
            "must be one of ['gh_field', 'gh_real', 'gh_integer'] but " +
            "kernel " + test_builtin_name.lower() + " has an argument of "
            "type gh_operator" in str(excinfo))


def test_builtin_args_not_same_space():
    ''' Check that we raise the correct error if we encounter a built-in
    that has arguments on different function spaces '''
    # Save the name of the actual builtin-definitions file
    old_name = dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE[:]
    # Define the built-in name and test file
    test_builtin_name = "inc_X_divideby_Y"
    test_builtin_file = "15.5.2_" + test_builtin_name + "_builtin.f90"
    # Change the builtin-definitions file to point to one that has
    # various invalid definitions
    dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE = \
        os.path.join(BASE_PATH, "invalid_builtins_mod.f90")
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     test_builtin_file),
        api="dynamo0.3")
    dynamo0p3_builtins.BUILTIN_DEFINITIONS_FILE = old_name
    with pytest.raises(ParseError) as excinfo:
        _ = PSyFactory("dynamo0.3",
                       distributed_memory=False).create(invoke_info)
    assert ("All field arguments to a built-in in the Dynamo 0.3 API "
            "must be on the same space. However, found spaces ['any_space_2', "
            "'any_space_1'] for arguments to " + test_builtin_name.lower() in
            str(excinfo))


def test_dynbuiltincallfactory_str():
    ''' Check that the str method of DynBuiltInCallFactory works as
    expected '''
    from psyclone.dynamo0p3_builtins import DynBuiltInCallFactory
    dyninf = DynBuiltInCallFactory()
    assert str(dyninf) == "Factory for a call to a Dynamo built-in"


def test_dynbuiltin_wrong_name():
    ''' Check that DynInfCallFactory.create() raises an error if it
    doesn't recognise the name of the kernel it is passed '''
    from psyclone.dynamo0p3_builtins import DynBuiltInCallFactory
    dyninf = DynBuiltInCallFactory()
    # We use 'duck-typing' - rather than attempt to create a rather
    # complex Kernel object we use a ParseError object and monkey
    # patch it so that it has a func_name member.
    fake_kern = ParseError("blah")
    fake_kern.func_name = "pw_blah"
    with pytest.raises(ParseError) as excinfo:
        _ = dyninf.create(fake_kern)
    assert ("Unrecognised built-in call. Found 'pw_blah' but "
            "expected one of '[" in str(excinfo.value))


def test_invalid_builtin_kernel():
    ''' Check that we raise an appropriate error if an unrecognised
    built-in is specified in the algorithm layer '''
    with pytest.raises(ParseError) as excinfo:
        _, _ = parse(os.path.join(BASE_PATH,
                                  "15.12.1_invalid_builtin_kernel.f90"),
                     api="dynamo0.3")
    assert ("kernel call 'setva_c' must either be named in a "
            "use statement or be a recognised built-in" in
            str(excinfo.value))


def test_dynbuiltin_str():
    ''' Check that we raise an error if we attempt to call the __str__
    method on the parent DynBuiltIn class '''
    from psyclone.dynamo0p3_builtins import DynBuiltIn
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.12.3_single_pointwise_builtin.f90"),
        api="dynamo0.3")
    for distmem in [True, False]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        with pytest.raises(NotImplementedError) as excinfo:
            DynBuiltIn.__str__(kern)
        assert "DynBuiltIn.__str__ must be overridden" in str(excinfo.value)


def test_dynbuiltin_gen_code():
    ''' Check that we raise an error if we attempt to call the gen_code()
    method on the parent DynBuiltIn class '''
    from psyclone.dynamo0p3_builtins import DynBuiltIn
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.12.3_single_pointwise_builtin.f90"),
        api="dynamo0.3")
    for distmem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        with pytest.raises(NotImplementedError) as excinfo:
            DynBuiltIn.gen_code(kern, None)
        assert "DynBuiltIn.gen_code must be overridden" in str(excinfo.value)


def test_dynbuiltin_cma():
    ''' Check that a DynBuiltIn returns None for CMA type (because
    built-ins don't work with CMA operators) '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.12.3_single_pointwise_builtin.f90"),
        api="dynamo0.3")
    for distmem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        cma_type = kern.cma_operation()
        assert cma_type is None


def test_dynbuiltfactory_str():
    ''' Check that the str method of DynBuiltInCallFactory works as
    expected. '''
    from psyclone.dynamo0p3_builtins import DynBuiltInCallFactory
    factory = DynBuiltInCallFactory()
    assert "Factory for a call to a Dynamo built-in" in str(factory)


# ------------- Adding (scaled) fields ------------------------------------- #


def test_X_plus_Y():
    ''' Test that 1) the str method of DynXPlusYKern returns the
    expected string and 2) we generate correct code for the built-in
    Z = X + Y where X and Y are fields '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.1_X_plus_Y_builtin.f90"),
                           api="dynamo0.3")
    for distmem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        # Test string method
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        assert str(kern) == "Built-in: Add fields"
        # Test code generation
        code = str(psy.gen)
        print code
        if not distmem:
            output = (
                "      f3_proxy = f3%get_proxy()\n"
                "      f1_proxy = f1%get_proxy()\n"
                "      f2_proxy = f2%get_proxy()\n"
                "      !\n"
                "      ! Initialise number of layers\n"
                "      !\n"
                "      nlayers = f3_proxy%vspace%get_nlayers()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f3\n"
                "      !\n"
                "      ndf_any_space_1_f3 = f3_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f3 = f3_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f3\n"
                "        f3_proxy%data(df) = f1_proxy%data(df) + "
                "f2_proxy%data(df)\n"
                "      END DO")
            assert output in code
        else:
            mesh_code_present("f3", code)
            output_dm_2 = (
                "      !\n"
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f3_proxy%vspace%get_last_dof_owned()\n"
                "        f3_proxy%data(df) = f1_proxy%data(df) + "
                "f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f3_proxy%set_dirty()\n"
                "      !\n")
            print output_dm_2
            assert output_dm_2 in code


def test_inc_X_plus_Y():
    ''' Test that 1) the str method of DynIncXPlusYKern returns the
    expected string and 2) we generate correct code for the built-in
    X = X + Y where X and Y are fields '''
    for distmem in [False, True]:
        _, invoke_info = parse(os.path.join(BASE_PATH,
                                            "15.1.2_inc_X_plus_Y_builtin.f90"),
                               distributed_memory=distmem,
                               api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        # Test string method
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        assert str(kern) == "Built-in: Increment field"
        # Test code generation
        code = str(psy.gen)
        print code
        if not distmem:
            output = (
                "      ndf_any_space_1_f1 = f1_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f1 = f1_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = f1_proxy%data(df) + "
                "f2_proxy%data(df)\n"
                "      END DO \n")
            assert output in code
        else:
            mesh_code_present("f1", code)
            output = (
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = f1_proxy%data(df) + "
                "f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()")
            assert output in code


def test_aX_plus_Y():
    ''' Test that 1) the str method of DynAXPlusYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation Z = a*X + Y where 'a' is a scalar and Z, X and Y are fields '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.3_aX_plus_Y_builtin.f90"),
                           api="dynamo0.3")
    for distmem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        # Test string method
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        assert str(kern) == "Built-in: aX_plus_Y"
        # Test code generation
        code = str(psy.gen)
        print code
        if not distmem:
            output = (
                "    SUBROUTINE invoke_0(f3, a, f1, f2)\n"
                "      REAL(KIND=r_def), intent(in) :: a\n"
                "      TYPE(field_type), intent(inout) :: f3\n"
                "      TYPE(field_type), intent(in) :: f1, f2\n"
                "      INTEGER df\n"
                "      INTEGER ndf_any_space_1_f3, undf_any_space_1_f3\n"
                "      INTEGER nlayers\n"
                "      TYPE(field_proxy_type) f3_proxy, f1_proxy, f2_proxy\n"
                "      !\n"
                "      ! Initialise field and/or operator proxies\n"
                "      !\n"
                "      f3_proxy = f3%get_proxy()\n"
                "      f1_proxy = f1%get_proxy()\n"
                "      f2_proxy = f2%get_proxy()\n"
                "      !\n"
                "      ! Initialise number of layers\n"
                "      !\n"
                "      nlayers = f3_proxy%vspace%get_nlayers()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f3\n"
                "      !\n"
                "      ndf_any_space_1_f3 = f3_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f3 = f3_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f3\n"
                "        f3_proxy%data(df) = a*f1_proxy%data(df) + "
                "f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "    END SUBROUTINE invoke_0\n")
            assert output in code
        else:
            mesh_code_present("f3", code)
            output_dm_2 = (
                "      !\n"
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f3_proxy%vspace%get_last_dof_owned()\n"
                "        f3_proxy%data(df) = a*f1_proxy%data(df) + "
                "f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f3_proxy%set_dirty()\n"
                "      !\n")
            print output_dm_2
            assert output_dm_2 in code


def test_inc_aX_plus_Y():
    ''' Test that 1) the str method of DynIncAXPlusYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = a*X + Y where 'a' is a scalar and X and Y are fields '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.4_inc_aX_plus_Y_builtin.f90"),
                           api="dynamo0.3")
    for distmem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        # Test string method
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        assert str(kern) == "Built-in: inc_aX_plus_Y"
        # Test code generation
        code = str(psy.gen)
        print code
        if not distmem:
            output = (
                "    SUBROUTINE invoke_0(a, f1, f2)\n"
                "      REAL(KIND=r_def), intent(in) :: a\n"
                "      TYPE(field_type), intent(inout) :: f1\n"
                "      TYPE(field_type), intent(in) :: f2\n"
                "      INTEGER df\n"
                "      INTEGER ndf_any_space_1_f1, undf_any_space_1_f1\n"
                "      INTEGER nlayers\n"
                "      TYPE(field_proxy_type) f1_proxy, f2_proxy\n"
                "      !\n"
                "      ! Initialise field and/or operator proxies\n"
                "      !\n"
                "      f1_proxy = f1%get_proxy()\n"
                "      f2_proxy = f2%get_proxy()\n"
                "      !\n"
                "      ! Initialise number of layers\n"
                "      !\n"
                "      nlayers = f1_proxy%vspace%get_nlayers()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f1\n"
                "      !\n"
                "      ndf_any_space_1_f1 = f1_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f1 = f1_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = a*f1_proxy%data(df) + "
                "f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "    END SUBROUTINE invoke_0")
            assert output in code
        else:
            mesh_code_present("f1", code)
            output_dm_2 = (
                "      !\n"
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = a*f1_proxy%data(df) + "
                "f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !\n")
            print output_dm_2
            assert output_dm_2 in code


def test_inc_X_plus_bY():
    ''' Test that 1) the str method of DynIncXPlusBYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = X + b*Y where 'b' is a scalar and X and Y are fields '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.5_inc_X_plus_bY_builtin.f90"),
                           api="dynamo0.3")
    for distmem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        # Test string method
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        assert str(kern) == "Built-in: inc_X_plus_bY"
        # Test code generation
        code = str(psy.gen)
        print code
        if not distmem:
            output = (
                "    SUBROUTINE invoke_0(f1, b, f2)\n"
                "      REAL(KIND=r_def), intent(in) :: b\n"
                "      TYPE(field_type), intent(inout) :: f1\n"
                "      TYPE(field_type), intent(in) :: f2\n"
                "      INTEGER df\n"
                "      INTEGER ndf_any_space_1_f1, undf_any_space_1_f1\n"
                "      INTEGER nlayers\n"
                "      TYPE(field_proxy_type) f1_proxy, f2_proxy\n"
                "      !\n"
                "      ! Initialise field and/or operator proxies\n"
                "      !\n"
                "      f1_proxy = f1%get_proxy()\n"
                "      f2_proxy = f2%get_proxy()\n"
                "      !\n"
                "      ! Initialise number of layers\n"
                "      !\n"
                "      nlayers = f1_proxy%vspace%get_nlayers()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f1\n"
                "      !\n"
                "      ndf_any_space_1_f1 = f1_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f1 = f1_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = f1_proxy%data(df) + "
                "b*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "    END SUBROUTINE invoke_0")
            assert output in code
        else:
            mesh_code_present("f1", code)
            output_dm_2 = (
                "      !\n"
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = f1_proxy%data(df) + "
                "b*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !\n")
            print output_dm_2
            assert output_dm_2 in code


def test_aX_plus_bY():
    ''' Test that 1) the str method of DynAXPlusBYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation Z = a*X + b*Y where 'a' and 'b' are scalars and Z, X and
    Y are fields '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.6_aX_plus_bY_builtin.f90"),
                           api="dynamo0.3")
    for distmem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        # Test string method
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        assert str(kern) == "Built-in: aX_plus_bY"
        # Test code generation
        code = str(psy.gen)
        print code
        if not distmem:
            output = (
                "    SUBROUTINE invoke_0(f3, a, f1, b, f2)\n"
                "      REAL(KIND=r_def), intent(in) :: a, b\n"
                "      TYPE(field_type), intent(inout) :: f3\n"
                "      TYPE(field_type), intent(in) :: f1, f2\n"
                "      INTEGER df\n"
                "      INTEGER ndf_any_space_1_f3, undf_any_space_1_f3\n"
                "      INTEGER nlayers\n"
                "      TYPE(field_proxy_type) f3_proxy, f1_proxy, f2_proxy\n"
                "      !\n"
                "      ! Initialise field and/or operator proxies\n"
                "      !\n"
                "      f3_proxy = f3%get_proxy()\n"
                "      f1_proxy = f1%get_proxy()\n"
                "      f2_proxy = f2%get_proxy()\n"
                "      !\n"
                "      ! Initialise number of layers\n"
                "      !\n"
                "      nlayers = f3_proxy%vspace%get_nlayers()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f3\n"
                "      !\n"
                "      ndf_any_space_1_f3 = f3_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f3 = f3_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f3\n"
                "        f3_proxy%data(df) = a*f1_proxy%data(df) + "
                "b*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "    END SUBROUTINE invoke_0\n")
            assert output in code
        else:
            mesh_code_present("f3", code)
            output_dm_2 = (
                "      !\n"
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f3_proxy%vspace%get_last_dof_owned()\n"
                "        f3_proxy%data(df) = a*f1_proxy%data(df) + "
                "b*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f3_proxy%set_dirty()\n"
                "      !\n")
            print output_dm_2
            assert output_dm_2 in code


def test_inc_aX_plus_bY():
    ''' Test that 1) the str method of DynIncAXPlusBYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = a*X + b*Y where 'a' and 'b' are scalars and X and Y
    are fields '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.1.7_inc_aX_plus_bY_builtin.f90"),
        api="dynamo0.3")
    for distmem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        # Test string method
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        assert str(kern) == "Built-in: inc_aX_plus_bY"
        # Test code generation
        code = str(psy.gen)
        print code
        if not distmem:
            output = (
                "    SUBROUTINE invoke_0(a, f1, b, f2)\n"
                "      REAL(KIND=r_def), intent(in) :: a, b\n"
                "      TYPE(field_type), intent(inout) :: f1\n"
                "      TYPE(field_type), intent(in) :: f2\n"
                "      INTEGER df\n"
                "      INTEGER ndf_any_space_1_f1, undf_any_space_1_f1\n"
                "      INTEGER nlayers\n"
                "      TYPE(field_proxy_type) f1_proxy, f2_proxy\n"
                "      !\n"
                "      ! Initialise field and/or operator proxies\n"
                "      !\n"
                "      f1_proxy = f1%get_proxy()\n"
                "      f2_proxy = f2%get_proxy()\n"
                "      !\n"
                "      ! Initialise number of layers\n"
                "      !\n"
                "      nlayers = f1_proxy%vspace%get_nlayers()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f1\n"
                "      !\n"
                "      ndf_any_space_1_f1 = f1_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f1 = f1_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = a*f1_proxy%data(df) + "
                "b*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "    END SUBROUTINE invoke_0\n")
            assert output in code
        else:
            mesh_code_present("f1", code)
            output_dm_2 = (
                "      !\n"
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = a*f1_proxy%data(df) + "
                "b*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !\n")
            print output_dm_2
            assert output_dm_2 in code


# ------------- Subtracting (scaled) fields --------------------------------- #


def test_X_minus_Y():
    ''' Test that 1) the str method of DynXMinusYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation Z = X - Y where Z, X and Y are fields '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.2.1_X_minus_Y_builtin.f90"),
                           api="dynamo0.3")
    for distmem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        # Test string method
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        assert str(kern) == "Built-in: Subtract fields"
        # Test code generation
        code = str(psy.gen)
        print code
        if not distmem:
            output = (
                "      f3_proxy = f3%get_proxy()\n"
                "      f1_proxy = f1%get_proxy()\n"
                "      f2_proxy = f2%get_proxy()\n"
                "      !\n"
                "      ! Initialise number of layers\n"
                "      !\n"
                "      nlayers = f3_proxy%vspace%get_nlayers()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f3\n"
                "      !\n"
                "      ndf_any_space_1_f3 = f3_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f3 = f3_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f3\n"
                "        f3_proxy%data(df) = f1_proxy%data(df) - "
                "f2_proxy%data(df)\n"
                "      END DO")
            assert output in code
        else:
            mesh_code_present("f3", code)
            output_dm_2 = (
                "      !\n"
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f3_proxy%vspace%get_last_dof_owned()\n"
                "        f3_proxy%data(df) = f1_proxy%data(df) - "
                "f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f3_proxy%set_dirty()\n"
                "      !\n")
            print output_dm_2
            assert output_dm_2 in code


def test_inc_X_minus_Y():
    ''' Test that 1) the str method of DynIncXMinusYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = X - Y where X and Y are fields '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.2.2_inc_X_minus_Y_builtin.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        # Test string method
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        assert str(kern) == "Built-in: Decrement field"
        # Test code generation
        code = str(psy.gen)
        print code
        if not distmem:
            output = (
                "      f1_proxy = f1%get_proxy()\n"
                "      f2_proxy = f2%get_proxy()\n"
                "      !\n"
                "      ! Initialise number of layers\n"
                "      !\n"
                "      nlayers = f1_proxy%vspace%get_nlayers()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f1\n"
                "      !\n"
                "      ndf_any_space_1_f1 = f1_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f1 = f1_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = f1_proxy%data(df) - "
                "f2_proxy%data(df)\n"
                "      END DO \n")
            assert output in code
        else:
            mesh_code_present("f1", code)
            output = (
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = f1_proxy%data(df) - "
                "f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()")
            assert output in code


def test_aX_minus_Y():
    ''' Test that 1) the str method of DynAXMinusYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation Z = a*X - Y where 'a' is a scalar and Z, X and Y are fields '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.2.3_aX_minus_Y_builtin.f90"),
                           api="dynamo0.3")
    for distmem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        # Test string method
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        assert str(kern) == "Built-in: aX_minus_Y"
        # Test code generation
        code = str(psy.gen)
        print code
        if not distmem:
            output = (
                "    SUBROUTINE invoke_0(f3, a, f1, f2)\n"
                "      REAL(KIND=r_def), intent(in) :: a\n"
                "      TYPE(field_type), intent(inout) :: f3\n"
                "      TYPE(field_type), intent(in) :: f1, f2\n"
                "      INTEGER df\n"
                "      INTEGER ndf_any_space_1_f3, undf_any_space_1_f3\n"
                "      INTEGER nlayers\n"
                "      TYPE(field_proxy_type) f3_proxy, f1_proxy, f2_proxy\n"
                "      !\n"
                "      ! Initialise field and/or operator proxies\n"
                "      !\n"
                "      f3_proxy = f3%get_proxy()\n"
                "      f1_proxy = f1%get_proxy()\n"
                "      f2_proxy = f2%get_proxy()\n"
                "      !\n"
                "      ! Initialise number of layers\n"
                "      !\n"
                "      nlayers = f3_proxy%vspace%get_nlayers()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f3\n"
                "      !\n"
                "      ndf_any_space_1_f3 = f3_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f3 = f3_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f3\n"
                "        f3_proxy%data(df) = a*f1_proxy%data(df) - "
                "f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "    END SUBROUTINE invoke_0\n")
            assert output in code
        else:
            mesh_code_present("f3", code)
            output_dm_2 = (
                "      !\n"
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f3_proxy%vspace%get_last_dof_owned()\n"
                "        f3_proxy%data(df) = a*f1_proxy%data(df) - "
                "f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f3_proxy%set_dirty()\n"
                "      !\n")
            print output_dm_2
            assert output_dm_2 in code


def test_X_minus_bY():
    ''' Test that 1) the str method of DynXMinusBYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation Z = X - b*Y where 'b' is a scalar and Z, X and Y are fields '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.2.4_X_minus_bY_builtin.f90"),
                           api="dynamo0.3")
    for distmem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        # Test string method
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        assert str(kern) == "Built-in: X_minus_bY"
        # Test code generation
        code = str(psy.gen)
        print code
        if not distmem:
            output = (
                "    SUBROUTINE invoke_0(f3, f1, b, f2)\n"
                "      REAL(KIND=r_def), intent(in) :: b\n"
                "      TYPE(field_type), intent(inout) :: f3\n"
                "      TYPE(field_type), intent(in) :: f1, f2\n"
                "      INTEGER df\n"
                "      INTEGER ndf_any_space_1_f3, undf_any_space_1_f3\n"
                "      INTEGER nlayers\n"
                "      TYPE(field_proxy_type) f3_proxy, f1_proxy, f2_proxy\n"
                "      !\n"
                "      ! Initialise field and/or operator proxies\n"
                "      !\n"
                "      f3_proxy = f3%get_proxy()\n"
                "      f1_proxy = f1%get_proxy()\n"
                "      f2_proxy = f2%get_proxy()\n"
                "      !\n"
                "      ! Initialise number of layers\n"
                "      !\n"
                "      nlayers = f3_proxy%vspace%get_nlayers()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f3\n"
                "      !\n"
                "      ndf_any_space_1_f3 = f3_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f3 = f3_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f3\n"
                "        f3_proxy%data(df) = f1_proxy%data(df) - "
                "b*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "    END SUBROUTINE invoke_0\n")
            assert output in code
        else:
            mesh_code_present("f3", code)
            output_dm_2 = (
                "      !\n"
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f3_proxy%vspace%get_last_dof_owned()\n"
                "        f3_proxy%data(df) = f1_proxy%data(df) - "
                "b*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f3_proxy%set_dirty()\n"
                "      !\n")
            print output_dm_2
            assert output_dm_2 in code


def test_inc_X_minus_bY():
    ''' Test that 1) the str method of DynIncXMinusBYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = X - b*Y where 'b' is a scalar and X and Y are fields '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.2.5_inc_X_minus_bY_builtin.f90"),
                           api="dynamo0.3")
    for distmem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        # Test string method
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        assert str(kern) == "Built-in: inc_X_minus_bY"
        # Test code generation
        code = str(psy.gen)
        print code
        if not distmem:
            output = (
                "    SUBROUTINE invoke_0(f1, b, f2)\n"
                "      REAL(KIND=r_def), intent(in) :: b\n"
                "      TYPE(field_type), intent(inout) :: f1\n"
                "      TYPE(field_type), intent(in) :: f2\n"
                "      INTEGER df\n"
                "      INTEGER ndf_any_space_1_f1, undf_any_space_1_f1\n"
                "      INTEGER nlayers\n"
                "      TYPE(field_proxy_type) f1_proxy, f2_proxy\n"
                "      !\n"
                "      ! Initialise field and/or operator proxies\n"
                "      !\n"
                "      f1_proxy = f1%get_proxy()\n"
                "      f2_proxy = f2%get_proxy()\n"
                "      !\n"
                "      ! Initialise number of layers\n"
                "      !\n"
                "      nlayers = f1_proxy%vspace%get_nlayers()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f1\n"
                "      !\n"
                "      ndf_any_space_1_f1 = f1_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f1 = f1_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = f1_proxy%data(df) - "
                "b*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "    END SUBROUTINE invoke_0")
            assert output in code
        else:
            mesh_code_present("f1", code)
            output_dm_2 = (
                "      !\n"
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = f1_proxy%data(df) - "
                "b*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !\n")
            print output_dm_2
            assert output_dm_2 in code


# ------------- Multiplying (scaled) fields --------------------------------- #


def test_X_times_Y():
    ''' Test that 1) the str method of DynXTimesYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation Z = X*Y where Z, X and Y are fields '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.3.1_X_times_Y_builtin.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        # Test string method
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        assert str(kern) == "Built-in: Multiply fields"
        # Test code generation
        code = str(psy.gen)
        print code
        if not distmem:
            output = (
                "    SUBROUTINE invoke_0(f3, f1, f2)\n"
                "      TYPE(field_type), intent(inout) :: f3\n"
                "      TYPE(field_type), intent(in) :: f1, f2\n"
                "      INTEGER df\n"
                "      INTEGER ndf_any_space_1_f3, undf_any_space_1_f3\n"
                "      INTEGER nlayers\n"
                "      TYPE(field_proxy_type) f3_proxy, f1_proxy, f2_proxy\n"
                "      !\n"
                "      ! Initialise field and/or operator proxies\n"
                "      !\n"
                "      f3_proxy = f3%get_proxy()\n"
                "      f1_proxy = f1%get_proxy()\n"
                "      f2_proxy = f2%get_proxy()\n"
                "      !\n"
                "      ! Initialise number of layers\n"
                "      !\n"
                "      nlayers = f3_proxy%vspace%get_nlayers()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f3\n"
                "      !\n"
                "      ndf_any_space_1_f3 = f3_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f3 = f3_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f3\n"
                "        f3_proxy%data(df) = f1_proxy%data(df) * "
                "f2_proxy%data(df)\n"
                "      END DO \n")
            assert output in code
        else:
            mesh_code_present("f3", code)
            output = (
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f3_proxy%vspace%get_last_dof_owned()\n"
                "        f3_proxy%data(df) = f1_proxy%data(df) * "
                "f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f3_proxy%set_dirty()")
            assert output in code


def test_inc_X_times_Y():
    ''' Test that 1) the str method of DynIncXTimesYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = X*Y where X and Y are fields '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.3.2_inc_X_times_Y_builtin.f90"),
        api="dynamo0.3")
    for distmem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        # Test string method
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        assert str(kern) == "Built-in: Multiply field by another"
        # Test code generation
        code = str(psy.gen)
        print code
        if not distmem:
            output = (
                "      f1_proxy = f1%get_proxy()\n"
                "      f2_proxy = f2%get_proxy()\n"
                "      !\n"
                "      ! Initialise number of layers\n"
                "      !\n"
                "      nlayers = f1_proxy%vspace%get_nlayers()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f1\n"
                "      !\n"
                "      ndf_any_space_1_f1 = f1_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f1 = f1_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = f1_proxy%data(df) * "
                "f2_proxy%data(df)\n"
                "      END DO")
            assert output in code
        else:
            mesh_code_present("f1", code)
            output_dm_2 = (
                "      !\n"
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = f1_proxy%data(df) * "
                "f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !\n")
            print output_dm_2
            assert output_dm_2 in code


def test_inc_aX_times_Y():
    ''' Test that 1) the str method of DynIncAXTimesYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = a*X*Y where 'a' is a scalar and X and Y are fields '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.3.3_inc_aX_times_Y_builtin.f90"),
                           api="dynamo0.3")
    for distmem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        # Test string method
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        assert str(kern) == "Built-in: inc_aX_times_Y"
        # Test code generation
        code = str(psy.gen)
        print code
        if not distmem:
            output = (
                "    SUBROUTINE invoke_0(a, f1, f2)\n"
                "      REAL(KIND=r_def), intent(in) :: a\n"
                "      TYPE(field_type), intent(inout) :: f1\n"
                "      TYPE(field_type), intent(in) :: f2\n"
                "      INTEGER df\n"
                "      INTEGER ndf_any_space_1_f1, undf_any_space_1_f1\n"
                "      INTEGER nlayers\n"
                "      TYPE(field_proxy_type) f1_proxy, f2_proxy\n"
                "      !\n"
                "      ! Initialise field and/or operator proxies\n"
                "      !\n"
                "      f1_proxy = f1%get_proxy()\n"
                "      f2_proxy = f2%get_proxy()\n"
                "      !\n"
                "      ! Initialise number of layers\n"
                "      !\n"
                "      nlayers = f1_proxy%vspace%get_nlayers()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f1\n"
                "      !\n"
                "      ndf_any_space_1_f1 = f1_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f1 = f1_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = a*f1_proxy%data(df) * "
                "f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "    END SUBROUTINE invoke_0")
            assert output in code
        else:
            mesh_code_present("f1", code)
            output_dm_2 = (
                "      !\n"
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = a*f1_proxy%data(df) * "
                "f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !\n")
            print output_dm_2
            assert output_dm_2 in code


# ------------- Scaling fields (multiplying by a scalar --------------------- #


def test_a_times_X():
    ''' Test that 1) the str method of DynATimesXKern returns the
    expected string and 2) we generate correct code for the built-in
    operation Y = a*X where 'a' is a scalar and X and Y are fields '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.4.1_a_times_X_builtin.f90"),
        api="dynamo0.3")
    for distmem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        # Test string method
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        assert str(kern) == "Built-in: Copy scaled field"
        # Test code generation
        code = str(psy.gen)
        print code
        if not distmem:
            output = (
                "      f2_proxy = f2%get_proxy()\n"
                "      f1_proxy = f1%get_proxy()\n"
                "      !\n"
                "      ! Initialise number of layers\n"
                "      !\n"
                "      nlayers = f2_proxy%vspace%get_nlayers()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f2\n"
                "      !\n"
                "      ndf_any_space_1_f2 = f2_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f2 = f2_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f2\n"
                "        f2_proxy%data(df) = a_scalar * f1_proxy%data(df)\n"
                "      END DO")
            assert output in code
        else:
            mesh_code_present("f2", code)
            output_dm_2 = (
                "      !\n"
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f2_proxy%vspace%get_last_dof_owned()\n"
                "        f2_proxy%data(df) = a_scalar * f1_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f2_proxy%set_dirty()\n"
                "      !\n")
            print output_dm_2
            assert output_dm_2 in code


def test_inc_a_times_X():
    ''' Test that 1) the str method of DynIncATimesXKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = a*X where 'a' is a scalar and X is a field '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.4.2_inc_a_times_X_builtin.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        # Test string method
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        assert str(kern) == "Built-in: Scale a field"
        # Test code generation
        code = str(psy.gen)
        print code
        if not distmem:
            output = (
                "    SUBROUTINE invoke_0(a, f1, b, f2, f3)\n"
                "      REAL(KIND=r_def), intent(in) :: a, b\n"
                "      TYPE(field_type), intent(inout) :: f3\n"
                "      TYPE(field_type), intent(in) :: f1, f2\n"
                "      INTEGER df\n"
                "      INTEGER ndf_any_space_1_f1, undf_any_space_1_f1\n"
                "      INTEGER nlayers\n"
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
                "      ! Initialise number of DoFs for any_space_1_f1\n"
                "      !\n"
                "      ndf_any_space_1_f1 = f1_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f1 = f1_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = a_scalar*f1_proxy%data(df)\n"
                "      END DO \n"
                "      !\n")
        else:
            mesh_code_present("f1", code)
            output = (
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = a_scalar*f1_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()")

            assert output in code


# ------------- Dividing (scaled) fields ------------------------------------ #


def test_X_divideby_Y():
    ''' Test that 1) the str method of DynXDividebyYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation Z = X/Y where Z, X and Y are fields '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.5.1_X_divideby_Y_builtin.f90"),
                           api="dynamo0.3")
    for distmem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        # Test string method
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        assert str(kern) == "Built-in: Divide fields"
        # Test code generation
        code = str(psy.gen)
        print code
        if not distmem:
            output = (
                "      f3_proxy = f3%get_proxy()\n"
                "      f1_proxy = f1%get_proxy()\n"
                "      f2_proxy = f2%get_proxy()\n"
                "      !\n"
                "      ! Initialise number of layers\n"
                "      !\n"
                "      nlayers = f3_proxy%vspace%get_nlayers()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f3\n"
                "      !\n"
                "      ndf_any_space_1_f3 = f3_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f3 = f3_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f3\n"
                "        f3_proxy%data(df) = f1_proxy%data(df) / "
                "f2_proxy%data(df)\n"
                "      END DO")
            assert output in code
        else:
            mesh_code_present("f3", code)
            output_dm_2 = (
                "      !\n"
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f3_proxy%vspace%get_last_dof_owned()\n"
                "        f3_proxy%data(df) = f1_proxy%data(df) / "
                "f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f3_proxy%set_dirty()\n"
                "      !\n")
            print output_dm_2
            assert output_dm_2 in code


def test_inc_X_divideby_Y():
    ''' Test that 1) the str method of DynIncXDividebyYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = X/Y where X and Y are fields '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.5.2_inc_X_divideby_Y_builtin.f90"),
                           api="dynamo0.3")
    for distmem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        # Test string method
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        assert str(kern) == "Built-in: Divide one field by another"
        # Test code generation
        code = str(psy.gen)
        print code
        if not distmem:
            output = (
                "      f1_proxy = f1%get_proxy()\n"
                "      f2_proxy = f2%get_proxy()\n"
                "      !\n"
                "      ! Initialise number of layers\n"
                "      !\n"
                "      nlayers = f1_proxy%vspace%get_nlayers()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f1\n"
                "      !\n"
                "      ndf_any_space_1_f1 = f1_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f1 = f1_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = f1_proxy%data(df) / "
                "f2_proxy%data(df)\n"
                "      END DO")
            assert output in code
        else:
            mesh_code_present("f1", code)
            output_dm_2 = (
                "      !\n"
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = f1_proxy%data(df) / "
                "f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !\n")
            print output_dm_2
            assert output_dm_2 in code


# ------------- Raising field to a scalar ----------------------------------- #


def test_inc_X_powreal_a():
    ''' Test that 1) the str method of DynIncXPowrealAKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = X**a where 'a' is a real scalar and X is a field '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.6.1_inc_X_powreal_a_builtin.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        # Test string method
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        assert str(kern) == "Built-in: raise a field to a real power"
        # Test code generation
        code = str(psy.gen)
        print code
        if not distmem:
            output = (
                "      ndf_any_space_1_f1 = f1_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f1 = f1_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = f1_proxy%data(df)**a_scalar\n"
                "      END DO \n"
                "      !\n")
        else:
            mesh_code_present("f1", code)
            output = (
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = f1_proxy%data(df)**a_scalar\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()")

            assert output in code


def test_inc_X_powint_n(tmpdir, f90, f90flags):
    ''' Test that 1) the str method of DynIncXPowintNKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = X**n where 'n' is an integer scalar and X is a field '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.6.2_inc_X_powint_n_builtin.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        # Test string method
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        assert str(kern) == "Built-in: raise a field to an integer power"
        # Test code generation
        code = str(psy.gen)
        print code

        if utils.TEST_COMPILE:
            # If compilation testing has been enabled
            # (--compile --f90="<compiler_name>" flags to py.test)
            assert utils.code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)

        if not distmem:
            output = (
                "      ndf_any_space_1_f1 = f1_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f1 = f1_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = f1_proxy%data(df)**i_scalar\n"
                "      END DO \n"
                "      !\n")
        else:
            mesh_code_present("f1", code)
            output = (
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = f1_proxy%data(df)**i_scalar\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()")

            assert output in code


# ------------- Setting field elements to a value --------------------------- #


def test_setval_c():
    ''' Test that 1) the str method of DynSetvalCKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = c where 'c' is a constant scalar value and X is a field '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.7.1_setval_c_builtin.f90"),
                           api="dynamo0.3")
    for distmem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        # Test string method
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        assert str(kern) == "Built-in: Set field to a scalar value"
        # Test code generation
        code = str(psy.gen)
        print code
        if not distmem:
            output = (
                "    SUBROUTINE invoke_0(f1, c)\n"
                "      REAL(KIND=r_def), intent(in) :: c\n"
                "      TYPE(field_type), intent(inout) :: f1\n"
                "      INTEGER df\n"
                "      INTEGER ndf_any_space_1_f1, undf_any_space_1_f1\n"
                "      INTEGER nlayers\n"
                "      TYPE(field_proxy_type) f1_proxy\n"
                "      !\n"
                "      ! Initialise field and/or operator proxies\n"
                "      !\n"
                "      f1_proxy = f1%get_proxy()\n"
                "      !\n"
                "      ! Initialise number of layers\n"
                "      !\n"
                "      nlayers = f1_proxy%vspace%get_nlayers()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f1\n"
                "      !\n"
                "      ndf_any_space_1_f1 = f1_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f1 = f1_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = c\n"
                "      END DO")
            assert output in code
        else:
            mesh_code_present("f1", code)
            output_dm_2 = (
                "      !\n"
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = c\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !\n")
            print output_dm_2
            assert output_dm_2 in code


def test_setval_X():
    ''' Test that 1) the str method of DynSetvalXKern returns the
    expected string and 2) we generate correct code for the built-in
    operation Y = X where X and Y are fields '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.7.2_setval_X_builtin.f90"),
                           api="dynamo0.3")
    for distmem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        # Test string method
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        assert str(kern) == "Built-in: Set a field equal to another field"
        # Test code generation
        code = str(psy.gen)
        print code
        if not distmem:
            output = (
                "    SUBROUTINE invoke_0(f2, f1)\n"
                "      TYPE(field_type), intent(inout) :: f2\n"
                "      TYPE(field_type), intent(in) :: f1\n"
                "      INTEGER df\n"
                "      INTEGER ndf_any_space_1_f2, undf_any_space_1_f2\n"
                "      INTEGER nlayers\n"
                "      TYPE(field_proxy_type) f2_proxy, f1_proxy\n"
                "      !\n"
                "      ! Initialise field and/or operator proxies\n"
                "      !\n"
                "      f2_proxy = f2%get_proxy()\n"
                "      f1_proxy = f1%get_proxy()\n"
                "      !\n"
                "      ! Initialise number of layers\n"
                "      !\n"
                "      nlayers = f2_proxy%vspace%get_nlayers()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f2\n"
                "      !\n"
                "      ndf_any_space_1_f2 = f2_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f2 = f2_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f2\n"
                "        f2_proxy%data(df) = f1_proxy%data(df)\n"
                "      END DO")
            assert output in code
        else:
            mesh_code_present("f2", code)
            output_dm_2 = (
                "      !\n"
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f2_proxy%vspace%get_last_dof_owned()\n"
                "        f2_proxy%data(df) = f1_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f2_proxy%set_dirty()\n"
                "      !\n")
            print output_dm_2
            assert output_dm_2 in code


# ------------- Inner product of fields ------------------------------------- #


def test_X_innerproduct_Y():
    ''' Test that 1) the str method of DynXInnerproductYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation which calculates inner product of fields X and Y as
    innprod = innprod + X(:)*Y(:) '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.9.1_X_innerproduct_Y_builtin.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        # Test string method
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        assert str(kern) == "Built-in: X_innerproduct_Y"
        # Test code generation
        code = str(psy.gen)
        print code
        output = (
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      !\n"
            "      ! Initialise number of layers\n"
            "      !\n"
            "      nlayers = f1_proxy%vspace%get_nlayers()\n"
            "      !\n")
        assert output in code
        if not distmem:
            output_seq = (
                "      ! Initialise number of DoFs for any_space_1_f1\n"
                "      !\n"
                "      ndf_any_space_1_f1 = f1_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f1 = f1_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      !\n"
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n")
            assert output_seq in code
        else:
            mesh_code_present("f1", code)
            output_dm = (
                "      ! Initialise number of DoFs for any_space_1_f1\n"
                "      !\n"
                "      ndf_any_space_1_f1 = f1_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f1 = f1_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      !\n"
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                "      END DO \n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n"
                "      !\n")
            assert output_dm in code
            assert "      USE scalar_mod, ONLY: scalar_type" in code
            assert "      REAL(KIND=r_def), intent(out) :: asum\n" in code
            assert "      TYPE(scalar_type) global_sum\n" in code


def test_X_innerproduct_X():
    ''' Test that 1) the str method of DynXInnerproductXKern returns the
    expected string and 2) we generate correct code for the built-in
    operation which calculates inner product of a field X by itself as
    innprod = innprod + X(:)*X(:) '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.9.2_X_innerproduct_X_builtin.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        # Test string method
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        assert str(kern) == "Built-in: X_innerproduct_X"
        # Test code generation
        code = str(psy.gen)
        print code
        output = (
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      !\n"
            "      ! Initialise number of layers\n"
            "      !\n"
            "      nlayers = f1_proxy%vspace%get_nlayers()\n"
            "      !\n")
        assert output in code
        if not distmem:
            output_seq = (
                "      ! Initialise number of DoFs for any_space_1_f1\n"
                "      !\n"
                "      ndf_any_space_1_f1 = f1_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f1 = f1_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      !\n"
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        asum = asum+f1_proxy%data(df)*f1_proxy%data(df)\n"
                "      END DO \n"
                "      !\n")
            assert output_seq in code
        else:
            mesh_code_present("f1", code)
            output_dm = (
                "      ! Initialise number of DoFs for any_space_1_f1\n"
                "      !\n"
                "      ndf_any_space_1_f1 = f1_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f1 = f1_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      !\n"
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        asum = asum+f1_proxy%data(df)*f1_proxy%data(df)\n"
                "      END DO \n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n"
                "      !\n")
            assert output_dm in code
            assert "      USE scalar_mod, ONLY: scalar_type" in code
            assert "      REAL(KIND=r_def), intent(out) :: asum\n" in code
            assert "      TYPE(scalar_type) global_sum\n" in code


# ------------- Sum field elements ------------------------------------------ #


def test_sum_X():
    ''' Test that 1) the str method of DynSumXKern returns the
    expected string and 2) we generate correct code for the built-in
    operation which sums elements of a field X as sumfld = sum(X(:)) '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.8.1_sum_X_builtin.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        # Test string method
        first_invoke = psy.invokes.invoke_list[0]
        kern = first_invoke.schedule.children[0].children[0]
        assert str(kern) == "Built-in: sum a field"
        # Test code generation
        code = str(psy.gen)
        print code
        output = (
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      !\n"
            "      ! Initialise number of layers\n"
            "      !\n"
            "      nlayers = f1_proxy%vspace%get_nlayers()\n"
            "      !\n")
        assert output in code
        if not distmem:
            output = (
                "      ndf_any_space_1_f1 = f1_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f1 = f1_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      !\n"
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        asum = asum+f1_proxy%data(df)\n"
                "      END DO ")
            assert output in code
        else:
            mesh_code_present("f1", code)
            output = (
                "      ! Initialise number of DoFs for any_space_1_f1\n"
                "      !\n"
                "      ndf_any_space_1_f1 = f1_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f1 = f1_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      !\n"
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        asum = asum+f1_proxy%data(df)\n"
                "      END DO \n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()")
            assert output in code
            assert "      REAL(KIND=r_def), intent(out) :: asum\n" in code


# ------------- Xfail builtins ---------------------------------------------- #


@pytest.mark.xfail(
    reason="Requires kernel-argument dependency analysis to deduce the "
    "spaces of the fields passed to the built-in kernel")
def test_X_times_Y_on_different_spaces():
    ''' Test that we raise an error if X_times_Y() is called for
    two fields that are on different spaces '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.11.2_X_times_Y_different_spaces.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    with pytest.raises(GenerationError) as excinfo:
        _ = str(psy.gen)
    assert "some string" in str(excinfo.value)


@pytest.mark.xfail(
    reason="Dependency analysis of kernel arguments within an invoke is "
    "not yet implemented")
def test_X_times_Y_deduce_space():
    ''' Test that we generate correct code if X_times_Y() is called
    in an invoke containing another kernel that allows the space of the
    fields to be deduced '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.11.1_X_times_Y_deduce_space.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        code = str(psy.gen)
        print code
        output = (
            "some fortran\n"
        )
        assert output in code


# ------------- Builtins that pass scalars by value ------------------------- #


def test_builtin_set(tmpdir, f90, f90flags):
    ''' Tests that we generate correct code for a serial builtin
    setval_c operation with a scalar passed by value'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.12.3_single_pointwise_builtin.f90"),
        api="dynamo0.3")
    for distmem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        code = str(psy.gen)
        print code

        if utils.TEST_COMPILE:
            # If compilation testing has been enabled
            # (--compile --f90="<compiler_name>" flags to py.test)
            assert utils.code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)

        if not distmem:
            output_seq = (
                "    SUBROUTINE invoke_0(f1)\n"
                "      TYPE(field_type), intent(inout) :: f1\n"
                "      INTEGER df\n"
                "      INTEGER ndf_any_space_1_f1, undf_any_space_1_f1\n"
                "      INTEGER nlayers\n"
                "      TYPE(field_proxy_type) f1_proxy\n"
                "      !\n"
                "      ! Initialise field and/or operator proxies\n"
                "      !\n"
                "      f1_proxy = f1%get_proxy()\n"
                "      !\n"
                "      ! Initialise number of layers\n"
                "      !\n"
                "      nlayers = f1_proxy%vspace%get_nlayers()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f1\n"
                "      !\n"
                "      ndf_any_space_1_f1 = f1_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f1 = f1_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = 0.0\n"
                "      END DO \n"
                "      !\n"
                "    END SUBROUTINE invoke_0\n")
            print output_seq
            assert output_seq in code

        if distmem:
            mesh_code_present("f1", code)
            output_dm_2 = (
                "      !\n"
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = 0.0\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !\n")
            print output_dm_2
            assert output_dm_2 in code


def test_aX_plus_Y_by_value():
    ''' Test that we generate correct code for the builtin
    operation Z = a*X + Y when a scalar is passed by value'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.13.1_aX_plus_Y_builtin_set_by_value.f90"),
        api="dynamo0.3")
    for distmem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        code = str(psy.gen)
        print code
        if not distmem:
            output = (
                "    SUBROUTINE invoke_0(f3, f1, f2)\n"
                "      TYPE(field_type), intent(inout) :: f3\n"
                "      TYPE(field_type), intent(in) :: f1, f2\n"
                "      INTEGER df\n"
                "      INTEGER ndf_any_space_1_f3, undf_any_space_1_f3\n"
                "      INTEGER nlayers\n"
                "      TYPE(field_proxy_type) f3_proxy, f1_proxy, f2_proxy\n"
                "      !\n"
                "      ! Initialise field and/or operator proxies\n"
                "      !\n"
                "      f3_proxy = f3%get_proxy()\n"
                "      f1_proxy = f1%get_proxy()\n"
                "      f2_proxy = f2%get_proxy()\n"
                "      !\n"
                "      ! Initialise number of layers\n"
                "      !\n"
                "      nlayers = f3_proxy%vspace%get_nlayers()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f3\n"
                "      !\n"
                "      ndf_any_space_1_f3 = f3_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f3 = f3_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f3\n"
                "        f3_proxy%data(df) = 0.5_r_def*f1_proxy%data(df) + "
                "f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "    END SUBROUTINE invoke_0")
            assert output in code
        if distmem:
            mesh_code_present("f3", code)
            output_dm_2 = (
                "      !\n"
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f3_proxy%vspace%get_last_dof_owned()\n"
                "        f3_proxy%data(df) = 0.5_r_def*f1_proxy%data(df) + "
                "f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f3_proxy%set_dirty()\n"
                "      !\n")
            print output_dm_2
            assert output_dm_2 in code


def test_aX_plus_bY_by_value():
    ''' Test that we generate correct code for the builtin
    operation Z = a*X + b*Y when scalars 'a' and 'b' are passed by value'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.13.2_aX_plus_bY_builtin_set_by_value.f90"),
        api="dynamo0.3")
    for distmem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        code = str(psy.gen)
        print code
        if not distmem:
            output = (
                "    SUBROUTINE invoke_0(f3, f1, f2)\n"
                "      TYPE(field_type), intent(inout) :: f3\n"
                "      TYPE(field_type), intent(in) :: f1, f2\n"
                "      INTEGER df\n"
                "      INTEGER ndf_any_space_1_f3, undf_any_space_1_f3\n"
                "      INTEGER nlayers\n"
                "      TYPE(field_proxy_type) f3_proxy, f1_proxy, f2_proxy\n"
                "      !\n"
                "      ! Initialise field and/or operator proxies\n"
                "      !\n"
                "      f3_proxy = f3%get_proxy()\n"
                "      f1_proxy = f1%get_proxy()\n"
                "      f2_proxy = f2%get_proxy()\n"
                "      !\n"
                "      ! Initialise number of layers\n"
                "      !\n"
                "      nlayers = f3_proxy%vspace%get_nlayers()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f3\n"
                "      !\n"
                "      ndf_any_space_1_f3 = f3_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f3 = f3_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f3\n"
                "        f3_proxy%data(df) = 0.5d0*f1_proxy%data(df) + "
                "0.8*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "    END SUBROUTINE invoke_0\n")
            assert output in code
        if distmem:
            mesh_code_present("f3", code)
            output_dm_2 = (
                "      !\n"
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f3_proxy%vspace%get_last_dof_owned()\n"
                "        f3_proxy%data(df) = 0.5d0*f1_proxy%data(df) + "
                "0.8*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f3_proxy%set_dirty()\n"
                "      !\n")
            print output_dm_2
            assert output_dm_2 in code


# ------------- Builtins with multiple calls or mixed with kernels ---------- #


def test_multiple_builtin_set():
    ''' Tests that we generate correct code when we have an invoke
    containing multiple set operations '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.14.2_multiple_set_kernels.f90"),
                           api="dynamo0.3")
    for distmem in [False, True]:
        psy = PSyFactory(
            "dynamo0.3", distributed_memory=distmem).create(invoke_info)
        code = str(psy.gen)
        print code
        if not distmem:
            output = (
                "    SUBROUTINE invoke_0(f1, fred, f2, f3, ginger)\n"
                "      REAL(KIND=r_def), intent(in) :: fred, ginger\n"
                "      TYPE(field_type), intent(inout) :: f1, f2, f3\n"
                "      INTEGER df\n"
                "      INTEGER ndf_any_space_1_f1, undf_any_space_1_f1, "
                "ndf_any_space_1_f2, undf_any_space_1_f2, "
                "ndf_any_space_1_f3, undf_any_space_1_f3\n"
                "      INTEGER nlayers\n"
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
                "      ! Initialise number of DoFs for any_space_1_f1\n"
                "      !\n"
                "      ndf_any_space_1_f1 = f1_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f1 = f1_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f2\n"
                "      !\n"
                "      ndf_any_space_1_f2 = f2_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f2 = f2_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f3\n"
                "      !\n"
                "      ndf_any_space_1_f3 = f3_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f3 = f3_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = fred\n"
                "      END DO \n"
                "      DO df=1,undf_any_space_1_f2\n"
                "        f2_proxy%data(df) = 3.0\n"
                "      END DO \n"
                "      DO df=1,undf_any_space_1_f3\n"
                "        f3_proxy%data(df) = ginger\n"
                "      END DO \n")
            assert output in code
        if distmem:
            mesh_code_present("f1", code)
            output_dm_2 = (
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = fred\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !\n"
                "      DO df=1,f2_proxy%vspace%get_last_dof_owned()\n"
                "        f2_proxy%data(df) = 3.0\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f2_proxy%set_dirty()\n"
                "      !\n"
                "      DO df=1,f3_proxy%vspace%get_last_dof_owned()\n"
                "        f3_proxy%data(df) = ginger\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f3_proxy%set_dirty()\n"
                "      !\n")
            print output_dm_2
            assert output_dm_2 in code


def test_builtin_set_plus_normal():
    ''' Tests that we generate correct code for a builtin
    set operation when the invoke also contains a normal kernel '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.14.4_builtin_and_normal_kernel_invoke.f90"),
        api="dynamo0.3")

    for distmem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        code = str(psy.gen)
        print code

        dofmap_output = (
            "      !\n"
            "      ! Look-up dofmaps for each function space\n"
            "      !\n"
            "      map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
            "      map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
            "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n")
        assert dofmap_output in code

        if not distmem:
            output = (
                "      ! Initialise number of DoFs for w3\n"
                "      !\n"
                "      ndf_w3 = m2_proxy%vspace%get_ndf()\n"
                "      undf_w3 = m2_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f1\n"
                "      !\n"
                "      ndf_any_space_1_f1 = f1_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f1 = f1_proxy%vspace%get_undf()\n"
                "      !\n"
                "      ! Call our kernels\n"
                "      !\n"
                "      DO cell=1,f1_proxy%vspace%get_ncell()\n"
                "        !\n"
                "        CALL testkern_code(nlayers, ginger, f1_proxy%data, "
                "f2_proxy%data, "
                "m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, "
                "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
                "undf_w3, map_w3(:,cell))\n"
                "      END DO \n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = 0.0\n"
                "      END DO ")
            assert output in code
        if distmem:
            mesh_code_present("f1", code)
            output_dm_2 = (
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
                "        CALL f2_proxy%halo_exchange(depth=1)\n"
                "      END IF \n"
                "      !\n"
                "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
                "        CALL m1_proxy%halo_exchange(depth=1)\n"
                "      END IF \n"
                "      !\n"
                "      IF (m2_proxy%is_dirty(depth=1)) THEN\n"
                "        CALL m2_proxy%halo_exchange(depth=1)\n"
                "      END IF \n"
                "      !\n"
                "      DO cell=1,mesh%get_last_halo_cell(1)\n"
                "        !\n"
                "        CALL testkern_code(nlayers, ginger, f1_proxy%data, "
                "f2_proxy%data, m1_proxy%data, m2_proxy%data, ndf_w1, "
                "undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), "
                "ndf_w3, undf_w3, map_w3(:,cell))\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = 0.0\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !\n")
            print output_dm_2
            assert output_dm_2 in code


# ------------- Builtins with reductions ------------------------------------ #


def test_multi_builtin_single_invoke():
    '''Test that multiple builtins, including one with reductions,
    produce correct code'''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.18.1_builtins_reduction_fuse_error.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        code = str(psy.gen)
        print code
        if distmem:
            assert(
                "    SUBROUTINE invoke_0(asum, f1, f2, b)\n"
                "      USE scalar_mod, ONLY: scalar_type\n"
                "      USE mesh_mod, ONLY: mesh_type\n"
                "      REAL(KIND=r_def), intent(out) :: asum\n"
                "      REAL(KIND=r_def), intent(in) :: b\n"
                "      TYPE(field_type), intent(inout) :: f1\n"
                "      TYPE(field_type), intent(in) :: f2\n"
                "      TYPE(scalar_type) global_sum\n"
                "      INTEGER df\n"
                "      INTEGER ndf_any_space_1_f1, undf_any_space_1_f1\n"
                "      TYPE(mesh_type), pointer :: mesh => null()\n"
                "      INTEGER nlayers\n"
                "      TYPE(field_proxy_type) f1_proxy, f2_proxy\n") in code
            assert (
                "      f1_proxy = f1%get_proxy()\n"
                "      f2_proxy = f2%get_proxy()\n"
                "      !\n"
                "      ! Initialise number of layers\n"
                "      !\n"
                "      nlayers = f1_proxy%vspace%get_nlayers()\n"
                "      !\n"
                "      ! Create a mesh object\n"
                "      !\n"
                "      mesh => f1%get_mesh()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f1\n"
                "      !\n"
                "      ndf_any_space_1_f1 = f1_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f1 = "
                "f1_proxy%vspace%get_undf()\n") in code
            assert (
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                "      END DO \n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = b*f1_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = asum*f1_proxy%data(df)\n"
                "      END DO \n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n") in code
        else:
            assert (
                "    SUBROUTINE invoke_0(asum, f1, f2, b)\n"
                "      REAL(KIND=r_def), intent(out) :: asum\n"
                "      REAL(KIND=r_def), intent(in) :: b\n"
                "      TYPE(field_type), intent(inout) :: f1\n"
                "      TYPE(field_type), intent(in) :: f2\n"
                "      INTEGER df\n"
                "      INTEGER ndf_any_space_1_f1, undf_any_space_1_f1\n"
                "      INTEGER nlayers\n"
                "      TYPE(field_proxy_type) f1_proxy, f2_proxy\n") in code
            assert (
                "      f1_proxy = f1%get_proxy()\n"
                "      f2_proxy = f2%get_proxy()\n"
                "      !\n"
                "      ! Initialise number of layers\n"
                "      !\n"
                "      nlayers = f1_proxy%vspace%get_nlayers()\n"
                "      !\n"
                "      ! Initialise number of DoFs for any_space_1_f1\n"
                "      !\n"
                "      ndf_any_space_1_f1 = f1_proxy%vspace%get_ndf()\n"
                "      undf_any_space_1_f1 = "
                "f1_proxy%vspace%get_undf()\n") in code
            assert (
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                "      END DO \n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = b*f1_proxy%data(df)\n"
                "      END DO \n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = asum*f1_proxy%data(df)\n"
                "      END DO \n") in code


# ------------- Invalid built-in with an integer scalar reduction ----------- #


def test_scalar_int_builtin_error(monkeypatch):
    ''' Test that specifying incorrect meta-data for built-in such that it
    claims to perform a reduction into an integer variable raises the
    expected error '''
    monkeypatch.setattr(dynamo0p3_builtins, "BUILTIN_DEFINITIONS_FILE",
                        value=os.path.join(BASE_PATH,
                                           "int_reduction_builtins_mod.f90"))
    for dist_mem in [False, True]:
        with pytest.raises(ParseError) as excinfo:
            _, _ = parse(os.path.join(BASE_PATH,
                                      "16.2_integer_scalar_sum.f90"),
                         api="dynamo0.3", distributed_memory=dist_mem)
        assert ("In the dynamo0.3 API a reduction access 'gh_sum' is "
                "only valid with a real scalar argument, but 'gh_integer' "
                "was found" in str(excinfo))


# ------------- Auxiliary mesh code generation function --------------------- #


def mesh_code_present(field_str, code):
    '''This test checks for the existance of mesh code. This exists for
    all builtins with dm = True (although it is not actually required!) so
    each test can call this function. Mesh code is generated from the first
    field in a builtin arguments list, here denoted with field_str.'''
    assert "      USE mesh_mod, ONLY: mesh_type" in code
    assert "      TYPE(mesh_type), pointer :: mesh => null()" in code
    output_dm_1 = (
        "      !\n"
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh => " + field_str + "%get_mesh()\n"
        "      !\n")
    assert output_dm_1 in code
