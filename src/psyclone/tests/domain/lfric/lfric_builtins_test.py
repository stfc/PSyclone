# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2021, Science and Technology Facilities Council.
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
# Modified R. W. Ford, STFC Daresbury Lab
# Modified by J. Henrichs, Bureau of Meteorology

''' This module tests the support for built-in operations in the LFRic API
    using pytest. Currently all built-in operations are 'pointwise' in that
    they iterate over DOFs. However this may change in the future. '''

# imports
from __future__ import absolute_import, print_function
import os
import pytest

from psyclone.parse.algorithm import parse
from psyclone.configuration import Config
from psyclone.errors import GenerationError
from psyclone.parse.utils import ParseError
from psyclone.psyir.nodes import Loop, Reference, UnaryOperation, Literal, \
    StructureReference
from psyclone.psyir.symbols import ScalarType, DataTypeSymbol
from psyclone.psyGen import PSyFactory
from psyclone.domain.lfric import lfric_builtins, LFRicConstants
from psyclone.domain.lfric.lfric_builtins import LFRicBuiltInCallFactory

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


# ------------- Tests for built-ins methods and arguments ------------------- #


def test_lfric_builtin_abstract_methods():
    ''' Check that the LFRicBuiltIn class is abstract and that the __str__
    method is abstract. '''
    with pytest.raises(TypeError) as err:
        lfric_builtins.LFRicBuiltIn()
    assert "abstract class LFRicBuiltIn" in str(err.value)
    assert "__str__" in str(err.value)


def test_lfricbuiltin_missing_defs(monkeypatch):
    ''' Check that we raise an appropriate error if we cannot find the
    file specifying meta-data for built-in kernels '''
    monkeypatch.setattr(lfric_builtins, "BUILTIN_DEFINITIONS_FILE",
                        "broken")
    with pytest.raises(ParseError) as excinfo:
        _, _ = parse(os.path.join(BASE_PATH,
                                  "15.12.3_single_pointwise_builtin.f90"),
                     api=API)
    assert ("broken' containing the meta-data describing the "
            "Built-in operations" in str(excinfo.value))


def test_lfricbuiltin_not_over_dofs():
    ''' Check that we raise an appropriate error if we encounter a
    built-in that does not iterate over dofs. '''
    old_name = lfric_builtins.BUILTIN_DEFINITIONS_FILE[:]
    lfric_builtins.BUILTIN_DEFINITIONS_FILE = \
        os.path.join(BASE_PATH, "not_dofs_builtins_mod.f90")
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.12.3_single_pointwise_builtin.f90"),
        api=API)
    # Restore the original file name before doing the assert in case
    # it fails
    lfric_builtins.BUILTIN_DEFINITIONS_FILE = old_name
    with pytest.raises(ParseError) as excinfo:
        _ = PSyFactory(API,
                       distributed_memory=False).create(invoke_info)
    assert ("built-in calls must operate on DoFs but found 'cell_column' "
            "for Built-in: Set a real-valued field " in str(excinfo.value))


def test_builtin_multiple_writes():
    ''' Check that we raise an appropriate error if we encounter a built-in
    that writes to more than one argument. '''
    # The file containing broken meta-data for the built-ins
    old_name = lfric_builtins.BUILTIN_DEFINITIONS_FILE[:]
    # Define the built-in name and test file
    test_builtin_name = "aX_plus_Y"
    test_builtin_file = "15.13.1_" + test_builtin_name + \
                        "_builtin_set_by_value.f90"
    lfric_builtins.BUILTIN_DEFINITIONS_FILE = \
        os.path.join(BASE_PATH, "invalid_builtins_mod.f90")
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        test_builtin_file),
                           api=API)
    lfric_builtins.BUILTIN_DEFINITIONS_FILE = old_name
    with pytest.raises(ParseError) as excinfo:
        _ = PSyFactory(API,
                       distributed_memory=False).create(invoke_info)
    assert ("A built-in kernel in the LFRic API must have one and only "
            "one argument that is written to but found 2 for kernel '{0}'".
            format(test_builtin_name.lower()) in str(excinfo.value))


def test_builtin_write_and_readwrite():
    ''' Check that we raise an appropriate error if we encounter a built-in
    that updates more than one argument where one is gh_write and one is
    gh_readwrite.

    '''
    # The file containing broken meta-data for the built-ins
    old_name = lfric_builtins.BUILTIN_DEFINITIONS_FILE[:]
    # Define the built-in name and test file
    test_builtin_name = "inc_aX_plus_bY"
    test_builtin_file = "15.1.7_" + test_builtin_name + "_builtin.f90"
    lfric_builtins.BUILTIN_DEFINITIONS_FILE = \
        os.path.join(BASE_PATH, "invalid_builtins_mod.f90")
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        test_builtin_file),
                           api=API)
    lfric_builtins.BUILTIN_DEFINITIONS_FILE = old_name
    with pytest.raises(ParseError) as excinfo:
        _ = PSyFactory(API,
                       distributed_memory=False).create(invoke_info)
    assert ("A built-in kernel in the LFRic API must have one and only "
            "one argument that is written to but found 2 for kernel '{0}'".
            format(test_builtin_name.lower()) in str(excinfo.value))


def test_builtin_sum_and_readwrite():
    ''' Check that we raise an appropriate error if we encounter a built-in
    that updates more than one argument where one is gh_sum and one is
    gh_readwrite.

    '''
    # The file containing broken meta-data for the built-ins
    old_name = lfric_builtins.BUILTIN_DEFINITIONS_FILE[:]
    # Define the built-in name and test file
    test_builtin_name = "inc_aX_plus_Y"
    test_builtin_file = "15.1.4_" + test_builtin_name + "_builtin.f90"
    lfric_builtins.BUILTIN_DEFINITIONS_FILE = \
        os.path.join(BASE_PATH, "invalid_builtins_mod.f90")
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        test_builtin_file),
                           api=API)
    lfric_builtins.BUILTIN_DEFINITIONS_FILE = old_name
    with pytest.raises(ParseError) as excinfo:
        _ = PSyFactory(API,
                       distributed_memory=False).create(invoke_info)
    assert ("A built-in kernel in the LFRic API must have one and only "
            "one argument that is written to but found 2 for kernel '{0}'".
            format(test_builtin_name.lower()) in str(excinfo.value))


def test_builtin_zero_writes(monkeypatch):
    ''' Check that we raise an appropriate error if we encounter a built-in
    that does not write to any field. '''
    # Use pytest's monkeypatch support to change our configuration to
    # point to a file containing broken meta-data for the
    # built-ins. The definition for aX_plus_bY that it contains erroneously
    # has no argument that is written to.
    # Define the built-in name and test file
    test_builtin_name = "aX_plus_bY"
    test_builtin_file = "15.1.6_" + test_builtin_name + "_builtin.f90"
    monkeypatch.setattr(lfric_builtins, "BUILTIN_DEFINITIONS_FILE",
                        value=os.path.join(BASE_PATH,
                                           "invalid_builtins_mod.f90"))
    with pytest.raises(ParseError) as excinfo:
        _, _ = parse(os.path.join(BASE_PATH,
                                  test_builtin_file),
                     api=API)
    assert ("An LFRic kernel must have at least one argument that is "
            "updated (written to) but found none for kernel '{0}'.".
            format(test_builtin_name.lower()) in str(excinfo.value))


def test_builtin_no_field_args():
    ''' Check that we raise appropriate error if we encounter a built-in
    that does not have any field arguments. '''
    old_name = lfric_builtins.BUILTIN_DEFINITIONS_FILE[:]
    # Define the built-in name and test file
    test_builtin_name = "setval_X"
    test_builtin_file = "15.7.2_" + test_builtin_name + "_builtin.f90"
    lfric_builtins.BUILTIN_DEFINITIONS_FILE = \
        os.path.join(BASE_PATH, "invalid_builtins_mod.f90")
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        test_builtin_file),
                           api=API)
    lfric_builtins.BUILTIN_DEFINITIONS_FILE = old_name
    with pytest.raises(ParseError) as excinfo:
        _ = PSyFactory(API,
                       distributed_memory=False).create(invoke_info)
    assert ("A built-in kernel in the LFRic API must have at least "
            "one field as an argument but kernel '{0}' has none".
            format(test_builtin_name.lower()) in str(excinfo.value))


def test_builtin_invalid_argument_type(monkeypatch):
    ''' Check that we raise appropriate error if we encounter a built-in
    that takes something other than a field or scalar argument. '''
    # Define the built-in name and test file
    test_builtin_name = "a_times_X"
    test_builtin_file = "15.4.1_" + test_builtin_name + "_builtin.f90"
    # Change the built-in-definitions file to point to one that has
    # various invalid definitions
    monkeypatch.setattr(
        lfric_builtins, "BUILTIN_DEFINITIONS_FILE",
        os.path.join(BASE_PATH, "invalid_builtins_mod.f90"))
    _, invoke_info = parse(os.path.join(BASE_PATH, test_builtin_file), api=API)
    # Restore the actual built-in-definitions file name
    monkeypatch.undo()
    with pytest.raises(ParseError) as excinfo:
        _ = PSyFactory(API, distributed_memory=False).create(invoke_info)
    const = LFRicConstants()
    assert ("In the LFRic API an argument to a built-in kernel must be one "
            "of {0} but kernel '{1}' has an argument of type 'gh_operator'.".
            format(const.VALID_BUILTIN_ARG_TYPES, test_builtin_name.lower())
            in str(excinfo.value))


def test_builtin_invalid_data_type(monkeypatch):
    ''' Check that we raise appropriate error if we encounter a
    built-in that takes something other than an argument of a 'real'
    or an 'integer' data type.

    '''
    # Define the built-in name and test file
    test_builtin_name = "inc_a_divideby_X"
    test_builtin_file = "15.5.4_" + test_builtin_name + "_builtin.f90"
    # Change the built-in-definitions file to point to one that has
    # various invalid definitions
    monkeypatch.setattr(
        lfric_builtins, "BUILTIN_DEFINITIONS_FILE",
        os.path.join(BASE_PATH, "invalid_builtins_mod.f90"))
    _, invoke_info = parse(os.path.join(BASE_PATH, test_builtin_file), api=API)
    # Restore the actual built-in-definitions file name
    monkeypatch.undo()
    with pytest.raises(ParseError) as excinfo:
        _ = PSyFactory(API, distributed_memory=False).create(invoke_info)
    const = LFRicConstants()
    assert ("In the LFRic API an argument to a built-in kernel must have "
            "one of {0} as a data type but kernel '{1}' has an argument of "
            "data type 'gh_logical'.".
            format(const.VALID_BUILTIN_DATA_TYPES, test_builtin_name.lower())
            in str(excinfo.value))


def test_builtin_args_not_same_space():
    ''' Check that we raise the correct error if we encounter a built-in
    that has arguments on different function spaces. '''
    # Save the name of the actual built-in-definitions file
    old_name = lfric_builtins.BUILTIN_DEFINITIONS_FILE[:]
    # Define the built-in name and test file
    test_builtin_name = "inc_X_divideby_Y"
    test_builtin_file = "15.5.2_" + test_builtin_name + "_builtin.f90"
    # Change the built-in-definitions file to point to one that has
    # various invalid definitions
    lfric_builtins.BUILTIN_DEFINITIONS_FILE = \
        os.path.join(BASE_PATH, "invalid_builtins_mod.f90")
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     test_builtin_file),
        api=API)
    lfric_builtins.BUILTIN_DEFINITIONS_FILE = old_name
    with pytest.raises(ParseError) as excinfo:
        _ = PSyFactory(API,
                       distributed_memory=False).create(invoke_info)
    assert ("All field arguments to a built-in in the LFRic API must "
            "be on the same space. However, found spaces ['any_space_1', "
            "'any_space_2'] for arguments to '{0}'".
            format(test_builtin_name.lower()) in str(excinfo.value))


def test_builtin_fld_args_different_data_type(monkeypatch):
    ''' Check that we raise the correct error if we encounter a built-in
    that has has different data type of its field arguments and is not one
    of the data type conversion built-ins ("int_X" and "real_X").

    '''
    # Define the built-in name and test file
    test_builtin_name = "X_minus_Y"
    test_builtin_file = "15.2.1_" + test_builtin_name + "_builtin.f90"
    # Change the built-in-definitions file to point to one that has
    # various invalid definitions
    monkeypatch.setattr(
        lfric_builtins, "BUILTIN_DEFINITIONS_FILE",
        os.path.join(BASE_PATH, "invalid_builtins_mod.f90"))
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     test_builtin_file),
        api=API)
    # Restore the actual built-in-definitions file name
    monkeypatch.undo()
    with pytest.raises(ParseError) as excinfo:
        _ = PSyFactory(API,
                       distributed_memory=False).create(invoke_info)
    assert ("In the LFRic API only the data type conversion built-ins "
            "['int_X', 'real_X'] are allowed to have field "
            "arguments of different data types. However, found "
            "different data types ['gh_integer', 'gh_real'] for "
            "field arguments to '{0}'.".
            format(test_builtin_name.lower()) in str(excinfo.value))


def test_lfricbuiltincallfactory_str():
    ''' Check that the str method of LFRicBuiltInCallFactory works as
    expected. '''
    lfricinf = LFRicBuiltInCallFactory()
    assert str(lfricinf) == "Factory for a call to an LFRic built-in."


def test_lfricbuiltin_wrong_name():
    ''' Check that LFRicBuiltInCallFactory.create() raises an error if it
    doesn't recognise the name of the kernel it is passed. '''
    lfricinf = LFRicBuiltInCallFactory()
    # We use 'duck-typing' - rather than attempt to create a rather
    # complex Kernel object we use a ParseError object and monkey
    # patch it so that it has a func_name member.
    fake_kern = ParseError("blah")
    fake_kern.func_name = "pw_blah"
    with pytest.raises(ParseError) as excinfo:
        _ = lfricinf.create(fake_kern)
    assert ("Unrecognised built-in call in LFRic API: found 'pw_blah' "
            "but expected one of [" in str(excinfo.value))


def test_invalid_builtin_kernel():
    ''' Check that we raise an appropriate error if an unrecognised
    built-in is specified in the algorithm layer. '''
    with pytest.raises(ParseError) as excinfo:
        _, _ = parse(os.path.join(BASE_PATH,
                                  "15.12.1_invalid_builtin_kernel.f90"),
                     api=API)
    assert ("kernel call 'setva_c' must either be named in a use "
            "statement (found ['constants_mod', 'field_mod']) or be a "
            "recognised built-in" in str(excinfo.value))


def test_lfricbuiltin_cma(dist_mem):
    ''' Check that an LFRicBuiltIn returns None for CMA type (because
    built-ins don't work with CMA operators). '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.12.3_single_pointwise_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    cma_type = kern.cma_operation()
    assert cma_type is None


def test_lfricbuiltfactory_str():
    ''' Check that the str method of LFRicBuiltInCallFactory works as
    expected. '''
    factory = LFRicBuiltInCallFactory()
    assert "Factory for a call to an LFRic built-in." in str(factory)


def test_get_indexed_field_argument_refs():
    ''' Test the LFRicBuiltIn.get_indexed_field_argument_references() method.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.8_a_plus_X_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    loop = psy.invokes.invoke_list[0].schedule[0]
    kern = loop.loop_body[0]
    refs = kern.get_indexed_field_argument_references()
    # Kernel has two field arguments
    assert len(refs) == 2
    for ref in refs:
        assert isinstance(ref, StructureReference)
        assert isinstance(ref.symbol.datatype, DataTypeSymbol)
        assert ref.symbol.datatype.name == "field_proxy_type"
        assert ref.member.name == "data"
        assert len(ref.member.indices) == 1
        assert isinstance(ref.member.indices[0], Reference)
        assert ref.member.indices[0].symbol.name == "df"


def test_get_scalar_argument_references():
    ''' Test the LFRicBuiltIn.get_scalar_argument_references() method.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.6.2_inc_X_powint_n_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    kern = sched[0].loop_body[0]
    refs = kern.get_scalar_argument_references()
    assert len(refs) == 1
    assert isinstance(refs[0], Reference)
    assert refs[0].symbol.name == "i_scalar"
    kern = sched[1].loop_body[0]
    refs = kern.get_scalar_argument_references()
    assert len(refs) == 1
    assert isinstance(refs[0], UnaryOperation)
    assert refs[0].operator == UnaryOperation.Operator.MINUS
    assert isinstance(refs[0].children[0], Literal)
    assert refs[0].children[0].value == "2"


def test_get_dof_loop_index_symbol():
    ''' Test the LFRicBuiltIn.get_dof_loop_index_symbol() method. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.8_a_plus_X_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    loop = psy.invokes.invoke_list[0].schedule[0]
    kern = loop.loop_body[0]
    sym = kern.get_dof_loop_index_symbol()
    assert sym.name == "df"
    assert sym.datatype.intrinsic == ScalarType.Intrinsic.INTEGER


# ------------- Adding (scaled) real fields --------------------------------- #


def test_X_plus_Y(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    ''' Test that:
    1) the str method of LFRicXPlusYKern returns the expected
       string;
    2) we generate correct code for the built-in Z = X + Y
       where X and Y are real-valued fields;
    3) that we generate correct bounds when
       Config.api_conf(API)._compute_annexed_dofs is False and True;
    4) that the lower_to_language_level() method works as expected.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.1_X_plus_Y_builtin.f90"),
                           api=API)
    psy = PSyFactory(API,
                     distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: Add real-valued fields"

    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Check for the correct field type declarations
    output = (
        "      TYPE(field_type), intent(in) :: f3, f1, f2\n"
        "      INTEGER df\n"
        "      TYPE(field_proxy_type) f3_proxy, f1_proxy, f2_proxy\n")
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

        # Test the lower-to-language method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = 1, undf_aspc1_f3, 1\n"
                "  f3_proxy%data(df) = f1_proxy%data(df) + "
                "f2_proxy%data(df)\n"
                "enddo") in code
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


def test_inc_X_plus_Y(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    ''' Test that 1) the str method of LFRicIncXPlusYKern returns the
    expected string and 2) we generate correct code for the built-in
    X = X + Y where X and Y are real-valued fields. Test with and without
    annexed dofs being computed as this affects the generated code.
    Also test the lower_to_language_level() method for this builtin.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)

    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.2_inc_X_plus_Y_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: Increment a real-valued field"
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

        # Test the lowering to language-level PSyIR
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = 1, undf_aspc1_f1, 1\n"
                "  f1_proxy%data(df) = f1_proxy%data(df) + "
                "f2_proxy%data(df)\n"
                "enddo" in code)
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


def test_a_plus_X(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicAPlusXKern returns the
    expected string and 2) we generate correct code for the built-in
    operation Y = a + X where 'a' is a real scalar and X and Y
    are real-valued fields. Test with and without annexed dofs being
    computed as this affects the generated code.
    Also test the lower_to_language_level() method.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.8_a_plus_X_builtin.f90"),
                           api=API)

    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: a_plus_X (real-valued fields)"
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


def test_inc_a_plus_X(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicIncAPlusXKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = a + X where 'a' is a real scalar and X is a
    real-valued field. Test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.9_inc_a_plus_X_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: inc_a_plus_X (real-valued field)"
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


def test_aX_plus_Y(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicAXPlusYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation Z = a*X + Y where 'a' is a real scalar and Z, X and Y
    are real-valued fields. Test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.3_aX_plus_Y_builtin.f90"),
                           api=API)

    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: aX_plus_Y (real-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(f3, a, f1, f2)\n"
            "      REAL(KIND=r_def), intent(in) :: a\n"
            "      TYPE(field_type), intent(in) :: f3, f1, f2\n"
            "      INTEGER df\n"
            "      TYPE(field_proxy_type) f3_proxy, f1_proxy, f2_proxy\n"
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
            "        f3_proxy%data(df) = a*f1_proxy%data(df) + "
            "f2_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code
    else:
        output_dm_2 = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f3_proxy%vspace%get_last_dof_annexed()\n"
            "        f3_proxy%data(df) = a*f1_proxy%data(df) + "
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


def test_inc_aX_plus_Y(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicIncAXPlusYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = a*X + Y where 'a' is a real scalar and X and Y are
    real-valued fields. Test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.4_inc_aX_plus_Y_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: inc_aX_plus_Y (real-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(a, f1, f2)\n"
            "      REAL(KIND=r_def), intent(in) :: a\n"
            "      TYPE(field_type), intent(in) :: f1, f2\n"
            "      INTEGER df\n"
            "      TYPE(field_proxy_type) f1_proxy, f2_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f1\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
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
            "        f1_proxy%data(df) = a*f1_proxy%data(df) + "
            "f2_proxy%data(df)\n"
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
            "        f1_proxy%data(df) = a*f1_proxy%data(df) + "
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


def test_inc_X_plus_bY(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicIncXPlusBYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = X + b*Y where 'b' is a real scalar and X and Y are
    real-valued fields. Test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.5_inc_X_plus_bY_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: inc_X_plus_bY (real-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(f1, b, f2)\n"
            "      REAL(KIND=r_def), intent(in) :: b\n"
            "      TYPE(field_type), intent(in) :: f1, f2\n"
            "      INTEGER df\n"
            "      TYPE(field_proxy_type) f1_proxy, f2_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f1\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
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
            "        f1_proxy%data(df) = f1_proxy%data(df) + "
            "b*f2_proxy%data(df)\n"
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
            "        f1_proxy%data(df) = f1_proxy%data(df) + "
            "b*f2_proxy%data(df)\n"
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


def test_aX_plus_bY(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicAXPlusBYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation Z = a*X + b*Y where 'a' and 'b' are real scalars and Z, X
    and Y are real-valued fields. Test with and without annexed dofs
    being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.6_aX_plus_bY_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: aX_plus_bY (real-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(f3, a, f1, b, f2)\n"
            "      REAL(KIND=r_def), intent(in) :: a, b\n"
            "      TYPE(field_type), intent(in) :: f3, f1, f2\n"
            "      INTEGER df\n"
            "      TYPE(field_proxy_type) f3_proxy, f1_proxy, f2_proxy\n"
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
            "        f3_proxy%data(df) = a*f1_proxy%data(df) + "
            "b*f2_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code
    else:
        output_dm_2 = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f3_proxy%vspace%get_last_dof_annexed()\n"
            "        f3_proxy%data(df) = a*f1_proxy%data(df) + "
            "b*f2_proxy%data(df)\n"
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


def test_inc_aX_plus_bY(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicIncAXPlusBYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = a*X + b*Y where 'a' and 'b' are real scalars and X
    and Y are real-valued fields. Test with and without annexed dofs
    being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.1.7_inc_aX_plus_bY_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: inc_aX_plus_bY (real-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(a, f1, b, f2)\n"
            "      REAL(KIND=r_def), intent(in) :: a, b\n"
            "      TYPE(field_type), intent(in) :: f1, f2\n"
            "      INTEGER df\n"
            "      TYPE(field_proxy_type) f1_proxy, f2_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f1\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
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
            "        f1_proxy%data(df) = a*f1_proxy%data(df) + "
            "b*f2_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code
    else:
        output_dm_2 = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f1_proxy%vspace%get_last_dof_annexed()\n"
            "        f1_proxy%data(df) = a*f1_proxy%data(df) + "
            "b*f2_proxy%data(df)\n"
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


def test_aX_plus_aY(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicAXPlusAYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation Z = a*(X + Y) where 'a' is a real scalar and Z, X and
    Y are real-valued fields. Test with and without annexed dofs
    being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.10_aX_plus_aY_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: aX_plus_aY (real-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=1,undf_aspc1_f3\n"
            "        f3_proxy%data(df) = a * (f1_proxy%data(df) + "
            "f2_proxy%data(df))\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code
    else:
        output_dm_2 = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f3_proxy%vspace%get_last_dof_annexed()\n"
            "        f3_proxy%data(df) = a * (f1_proxy%data(df) + "
            "f2_proxy%data(df))\n"
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


# ------------- Subtracting (scaled) real fields ---------------------------- #


def test_X_minus_Y(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicXMinusYKern returns the expected
    string and 2) we generate correct code for the built-in operation
    Z = X - Y where Z, X and Y are real-valued fields. Test with and without
    annexed dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.2.1_X_minus_Y_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: Subtract real-valued fields"
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


def test_inc_X_minus_Y(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicIncXMinusYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = X - Y where X and Y are real-valued fields. Test with and
    without annexed dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.2.2_inc_X_minus_Y_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: Decrement a real-valued field"
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


def test_aX_minus_Y(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicAXMinusYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation Z = a*X - Y where 'a' is a real scalar and Z, X and Y
    are real-valued fields. Test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.2.3_aX_minus_Y_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: aX_minus_Y (real-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(f3, a, f1, f2)\n"
            "      REAL(KIND=r_def), intent(in) :: a\n"
            "      TYPE(field_type), intent(in) :: f3, f1, f2\n"
            "      INTEGER df\n"
            "      TYPE(field_proxy_type) f3_proxy, f1_proxy, f2_proxy\n"
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
            "        f3_proxy%data(df) = a*f1_proxy%data(df) - "
            "f2_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code
    else:
        output_dm_2 = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f3_proxy%vspace%get_last_dof_annexed()\n"
            "        f3_proxy%data(df) = a*f1_proxy%data(df) - "
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


def test_X_minus_bY(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicXMinusBYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation Z = X - b*Y where 'b' is a real scalar and Z, X and Y
    are real-valued fields. Test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.2.4_X_minus_bY_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: X_minus_bY (real-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(f3, f1, b, f2)\n"
            "      REAL(KIND=r_def), intent(in) :: b\n"
            "      TYPE(field_type), intent(in) :: f3, f1, f2\n"
            "      INTEGER df\n"
            "      TYPE(field_proxy_type) f3_proxy, f1_proxy, f2_proxy\n"
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
            "        f3_proxy%data(df) = f1_proxy%data(df) - "
            "b*f2_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code
    else:
        output_dm_2 = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f3_proxy%vspace%get_last_dof_annexed()\n"
            "        f3_proxy%data(df) = f1_proxy%data(df) - "
            "b*f2_proxy%data(df)\n"
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


def test_inc_X_minus_bY(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicIncXMinusBYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = X - b*Y where 'b' is a real scalar and X and Y are
    real-valued  fields. Test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.2.5_inc_X_minus_bY_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: inc_X_minus_bY (real-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(f1, b, f2)\n"
            "      REAL(KIND=r_def), intent(in) :: b\n"
            "      TYPE(field_type), intent(in) :: f1, f2\n"
            "      INTEGER df\n"
            "      TYPE(field_proxy_type) f1_proxy, f2_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f1\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
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
            "b*f2_proxy%data(df)\n"
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
            "        f1_proxy%data(df) = f1_proxy%data(df) - "
            "b*f2_proxy%data(df)\n"
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


def test_aX_minus_bY(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicAXMinusBYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation Z = a*X - b*Y where 'a' and 'b' are real scalars and Z, X
    and Y are real-valued fields. Test with and without annexed dofs
    being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.2.6_aX_minus_bY_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: aX_minus_bY (real-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(f3, a, f1, b, f2)\n"
            "      REAL(KIND=r_def), intent(in) :: a, b\n"
            "      TYPE(field_type), intent(in) :: f3, f1, f2\n"
            "      INTEGER df\n"
            "      TYPE(field_proxy_type) f3_proxy, f1_proxy, f2_proxy\n"
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
            "        f3_proxy%data(df) = a*f1_proxy%data(df) - "
            "b*f2_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code
    else:
        output_dm_2 = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f3_proxy%vspace%get_last_dof_annexed()\n"
            "        f3_proxy%data(df) = a*f1_proxy%data(df) - "
            "b*f2_proxy%data(df)\n"
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


# ------------- Multiplying (scaled) real fields ---------------------------- #


def test_X_times_Y(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicXTimesYKern returns the expected
    string and 2) we generate correct code for the built-in operation
    Z = X*Y where Z, X and Y are real-valued fields. Test with and without
    annexed dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.3.1_X_times_Y_builtin.f90"), api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: Multiply real-valued fields"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(f3, f1, f2)\n"
            "      TYPE(field_type), intent(in) :: f3, f1, f2\n"
            "      INTEGER df\n"
            "      TYPE(field_proxy_type) f3_proxy, f1_proxy, f2_proxy\n"
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


def test_inc_X_times_Y(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicIncXTimesYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = X*Y where X and Y are real-valued fields. Test with and
    without annexed dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.3.2_inc_X_times_Y_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: Multiply one real-valued field by another"
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


def test_inc_aX_times_Y(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicIncAXTimesYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = a*X*Y where 'a' is a real scalar and X and Y are
    real-valued fields. Test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.3.3_inc_aX_times_Y_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: inc_aX_times_Y (real-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(a, f1, f2)\n"
            "      REAL(KIND=r_def), intent(in) :: a\n"
            "      TYPE(field_type), intent(in) :: f1, f2\n"
            "      INTEGER df\n"
            "      TYPE(field_proxy_type) f1_proxy, f2_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f1\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
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
            "        f1_proxy%data(df) = a * f1_proxy%data(df) * "
            "f2_proxy%data(df)\n"
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
            "        f1_proxy%data(df) = a * f1_proxy%data(df) * "
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


# ------------- Scaling real fields (multiplying by a real scalar) ---------- #


def test_a_times_X(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicATimesXKern returns the expected
    string and 2) we generate correct code for the built-in operation
    Y = a*X where 'a' is a real scalar and X and Y are real-valued fields.
    Test with and without annexed dofs being computed as this affects the
    generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.4.1_a_times_X_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: Copy a scaled real-valued field"
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


def test_inc_a_times_X(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicIncATimesXKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = a*X where 'a' is a real scalar and X is a real-valued
    field. Test with and without annexed dofs being computed as this
    affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.4.2_inc_a_times_X_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: Scale a real-valued field"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(a, f1, b, f2, f3)\n"
            "      REAL(KIND=r_def), intent(in) :: a, b\n"
            "      TYPE(field_type), intent(in) :: f1, f2, f3\n"
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


# ------------- Dividing real fields ---------------------------------------- #


def test_X_divideby_Y(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicXDividebyYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation Z = X/Y where Z, X and Y are fields real-valued. Test with and
    without annexed dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.5.1_X_divideby_Y_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: Divide real-valued fields"
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
            "        f3_proxy%data(df) = f1_proxy%data(df) / "
            "f2_proxy%data(df)\n"
            "      END DO")
        assert output in code
    else:
        output_dm_2 = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f3_proxy%vspace%get_last_dof_annexed()\n"
            "        f3_proxy%data(df) = f1_proxy%data(df) / "
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


def test_inc_X_divideby_Y(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicIncXDividebyYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = X/Y where X and Y are real-valued fields. Test with and
    without annexed dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.5.2_inc_X_divideby_Y_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: Divide one real-valued field by another"
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
            "        f1_proxy%data(df) = f1_proxy%data(df) / "
            "f2_proxy%data(df)\n"
            "      END DO")
        assert output in code
    else:
        output_dm_2 = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f1_proxy%vspace%get_last_dof_annexed()\n"
            "        f1_proxy%data(df) = f1_proxy%data(df) / "
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


# ------------- Inverse scaling of real fields ------------------------------ #


def test_a_divideby_X(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicADividebyXKern returns the
    expected string and 2) we generate correct code for the built-in
    operation Y = a/X where 'a' is a real scalar and X and Y are
    real-valued fields. Test with and without annexed dofs being computed
    as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.5.3_a_divideby_X_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert (str(kern) == "Built-in: Inverse scaling of a real-valued "
            "field (Y = a/X)")
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
            "        f2_proxy%data(df) = a_scalar / f1_proxy%data(df)\n"
            "      END DO")
        assert output in code
    else:
        output_dm_2 = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f2_proxy%vspace%get_last_dof_annexed()\n"
            "        f2_proxy%data(df) = a_scalar / f1_proxy%data(df)\n"
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


def test_inc_a_divideby_X(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicIncADividebyXKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = a/X where 'a' is a real scalar and X is a real-valued
    field. Test with and without annexed dofs being computed as this
    affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.5.4_inc_a_divideby_X_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert (str(kern) == "Built-in: Inverse scaling of a real-valued "
            "field (X = a/X)")
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=1,undf_aspc1_f1\n"
            "        f1_proxy%data(df) = a_scalar / f1_proxy%data(df)\n"
            "      END DO\n"
            "      !\n")
    else:
        output = (
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f1_proxy%vspace%get_last_dof_annexed()\n"
            "        f1_proxy%data(df) = a_scalar / f1_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()")
        if not annexed:
            output = output.replace("dof_annexed", "dof_owned")
        assert output in code


# ------------- Raising a real field to a scalar ---------------------------- #


def test_inc_X_powreal_a(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicIncXPowrealAKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = X**a where 'a' is a real scalar and X is a
    real-valued field. Test with and without annexed dofs being computed
    as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.6.1_inc_X_powreal_a_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: Raise a real-valued field to a real power"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      ndf_aspc1_f1 = f1_proxy%vspace%get_ndf()\n"
            "      undf_aspc1_f1 = f1_proxy%vspace%get_undf()\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=1,undf_aspc1_f1\n"
            "        f1_proxy%data(df) = f1_proxy%data(df) ** a_scalar\n"
            "      END DO\n"
            "      !\n")
    else:
        output = (
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f1_proxy%vspace%get_last_dof_annexed()\n"
            "        f1_proxy%data(df) = f1_proxy%data(df) ** a_scalar\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()")
        if not annexed:
            output = output.replace("dof_annexed", "dof_owned")
        assert output in code
        assert ("f1_proxy%data(df) = f1_proxy%data(df) ** 1.0e-3_r_def\n"
                in code)


def test_inc_X_powint_n(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicIncXPowintNKern returns the
    expected string and 2) we generate correct code for the built-in
    operation X = X**n where 'n' is an integer scalar and X is a
    real-valued field. Also test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.6.2_inc_X_powint_n_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == ("Built-in: Raise a real-valued field to an "
                         "integer power")

    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      ndf_aspc1_f1 = f1_proxy%vspace%get_ndf()\n"
            "      undf_aspc1_f1 = f1_proxy%vspace%get_undf()\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=1,undf_aspc1_f1\n"
            "        f1_proxy%data(df) = f1_proxy%data(df) ** i_scalar\n"
            "      END DO\n"
            "      !\n")
    else:
        output = (
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f1_proxy%vspace%get_last_dof_annexed()\n"
            "        f1_proxy%data(df) = f1_proxy%data(df) ** i_scalar\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()")
        if not annexed:
            output = output.replace("dof_annexed", "dof_owned")
        assert output in code

        assert "f1_proxy%data(df) = f1_proxy%data(df) ** (-2_i_def)\n" in code
        assert ("f1_proxy%data(df) = f1_proxy%data(df) ** my_var_a_scalar\n"
                in code)


# ------------- Setting real field elements to a real value ----------------- #


def test_setval_c(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicSetvalCKern returns the expected
    string and 2) we generate correct code for the built-in operation
    X = c where 'c' is a real constant scalar value and X is a real-valued
    field. Test with and without annexed dofs being computed as this affects
    the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.7.1_setval_c_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == ("Built-in: Set a real-valued field to a real "
                         "scalar value")
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(f1, c)\n"
            "      REAL(KIND=r_def), intent(in) :: c\n"
            "      TYPE(field_type), intent(in) :: f1\n"
            "      INTEGER df\n"
            "      TYPE(field_proxy_type) f1_proxy\n"
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


def test_setval_X(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicSetvalXKern returns the expected
    string and 2) we generate correct code for the built-in operation
    Y = X where X and Y are real-valued fields. Also test with and without
    annexed dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.7.2_setval_X_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == ("Built-in: Set a real-valued field equal to "
                         "another such field")
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(f2, f1)\n"
            "      TYPE(field_type), intent(in) :: f2, f1\n"
            "      INTEGER df\n"
            "      TYPE(field_proxy_type) f2_proxy, f1_proxy\n"
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


# ------------- Inner product of real fields -------------------------------- #


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
            "      ! Call our kernels\n"
            "      !\n"
            "      !\n"
            "      ! Zero summation variables\n"
            "      !\n"
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      DO df=1,undf_aspc1_f1\n"
            "        asum = asum + f1_proxy%data(df)*f2_proxy%data(df)\n"
            "      END DO\n"
            "      !\n")
        assert output_seq in code
    else:
        output_dm = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      !\n"
            "      ! Zero summation variables\n"
            "      !\n"
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
            "        asum = asum + f1_proxy%data(df)*f2_proxy%data(df)\n"
            "      END DO\n"
            "      global_sum%value = asum\n"
            "      asum = global_sum%get_sum()\n"
            "      !\n")
        assert output_dm in code
        assert "      USE scalar_mod, ONLY: scalar_type" in code
        assert "      REAL(KIND=r_def), intent(out) :: asum\n" in code
        assert "      TYPE(scalar_type) global_sum\n" in code


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
            "      ! Call our kernels\n"
            "      !\n"
            "      !\n"
            "      ! Zero summation variables\n"
            "      !\n"
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      DO df=1,undf_aspc1_f1\n"
            "        asum = asum + f1_proxy%data(df)*f1_proxy%data(df)\n"
            "      END DO\n"
            "      !\n")
        assert output_seq in code
    else:
        output_dm = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      !\n"
            "      ! Zero summation variables\n"
            "      !\n"
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
            "        asum = asum + f1_proxy%data(df)*f1_proxy%data(df)\n"
            "      END DO\n"
            "      global_sum%value = asum\n"
            "      asum = global_sum%get_sum()\n"
            "      !\n")
        assert output_dm in code
        assert "      USE scalar_mod, ONLY: scalar_type" in code
        assert "      REAL(KIND=r_def), intent(out) :: asum\n" in code
        assert "      TYPE(scalar_type) global_sum\n" in code


# ------------- Sum real field elements ------------------------------------- #


def test_sum_X(tmpdir, dist_mem):
    ''' Test that 1) the str method of LFRicSumXKern returns the expected
    string and 2) we generate correct code for the built-in operation which
    sums elements of a real-valued field X as sumfld = sum(X(:)).

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.8.1_sum_X_builtin.f90"), api=API)
    psy = PSyFactory(API,
                     distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: Sum a real-valued field"
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
        output = (
            "      undf_aspc1_f1 = f1_proxy%vspace%get_undf()\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      !\n"
            "      ! Zero summation variables\n"
            "      !\n"
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      DO df=1,undf_aspc1_f1\n"
            "        asum = asum + f1_proxy%data(df)\n"
            "      END DO")
        assert output in code
    else:
        output = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      !\n"
            "      ! Zero summation variables\n"
            "      !\n"
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
            "        asum = asum + f1_proxy%data(df)\n"
            "      END DO\n"
            "      global_sum%value = asum\n"
            "      asum = global_sum%get_sum()")
        assert output in code
        assert "      REAL(KIND=r_def), intent(out) :: asum\n" in code


# ------------- Sign of real field elements --------------------------------- #


def test_sign_X(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicSignXKern returns the
    expected string and 2) we generate correct code for the built-in
    operation Y = sign(a, X) where 'a' is a real scalar and Y and X
    are real-valued fields. Test with and without annexed dofs
    being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.10.1_sign_X_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: Sign of a real-valued field"
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


# ------------- Converting real to integer field elements ------------------- #


def test_int_X(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicIntXKern returns the
    expected string and 2) we generate correct code for the built-in
    operation Y = int(X, i_def) where Y is an integer-valued field, X is
    the real-valued field being converted and the correct kind, 'i_def',
    is read from the PSyclone configuration file. Test with and without
    annexed dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.10.3_int_X_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == ("Built-in: Convert a real-valued to an "
                         "integer-valued field")
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
            "      TYPE(field_type), intent(in) :: f1\n"
            "      TYPE(integer_field_type), intent(in) :: f2\n"
            "      INTEGER df\n"
            "      TYPE(integer_field_proxy_type) f2_proxy\n"
            "      TYPE(field_proxy_type) f1_proxy\n"
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
            "        f2_proxy%data(df) = int(f1_proxy%data(df), i_def)\n"
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
            "        f2_proxy%data(df) = int(f1_proxy%data(df), i_def)\n"
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


# ------------- Xfail built-ins --------------------------------------------- #


@pytest.mark.xfail(
    reason="Requires kernel-argument dependency analysis to deduce the "
    "spaces of the fields passed to the built-in kernel")
def test_X_times_Y_on_different_spaces():
    ''' Test that we raise an error if X_times_Y() is called for
    two fields that are on different spaces '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.11.2_X_times_Y_different_spaces.f90"),
        api=API)
    psy = PSyFactory(API).create(invoke_info)
    with pytest.raises(GenerationError) as excinfo:
        _ = str(psy.gen)
    assert "some string" in str(excinfo.value)


@pytest.mark.xfail(
    reason="Dependency analysis of kernel arguments within an invoke is "
    "not yet implemented")
def test_X_times_Y_deduce_space(dist_mem):
    ''' Test that we generate correct code if X_times_Y() is called
    in an invoke containing another kernel that allows the space of the
    fields to be deduced '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.11.1_X_times_Y_deduce_space.f90"),
        api=API)
    psy = PSyFactory(API,
                     distributed_memory=dist_mem).create(invoke_info)
    code = str(psy.gen)

    output = (
        "some fortran\n"
    )
    assert output in code


# ------------- Built-ins that pass scalars by value ------------------------ #


def test_builtin_set(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Tests that we generate correct code for a serial built-in setval_c
    operation with a scalar passed by value. Test with and without annexed
    dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.12.3_single_pointwise_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output_seq = (
            "    SUBROUTINE invoke_0(f1)\n"
            "      TYPE(field_type), intent(in) :: f1\n"
            "      INTEGER df\n"
            "      TYPE(field_proxy_type) f1_proxy\n"
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
            "        f1_proxy%data(df) = 0.0\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output_seq in code

    if dist_mem:
        output_dm_2 = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f1_proxy%vspace%get_last_dof_annexed()\n"
            "        f1_proxy%data(df) = 0.0\n"
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


def test_aX_plus_Y_by_value(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that we generate correct code for the built-in operation
    Z = a*X + Y when a scalar is passed by value. Also test with and
    without annexed dofs being computed as this affects the generated
    code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.13.1_aX_plus_Y_builtin_set_by_value.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(f3, f1, f2)\n"
            "      TYPE(field_type), intent(in) :: f3, f1, f2\n"
            "      INTEGER df\n"
            "      TYPE(field_proxy_type) f3_proxy, f1_proxy, f2_proxy\n"
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
            "        f3_proxy%data(df) = 0.5_r_def*f1_proxy%data(df) + "
            "f2_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0")
        assert output in code
    if dist_mem:
        output_dm_2 = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f3_proxy%vspace%get_last_dof_annexed()\n"
            "        f3_proxy%data(df) = 0.5_r_def*f1_proxy%data(df) + "
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


def test_aX_plus_bY_by_value(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that we generate correct code for the built-in operation
    Z = a*X + b*Y when scalars 'a' and 'b' are passed by value. Test
    with and without annexed dofs being computed as this affects the
    generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.13.2_aX_plus_bY_builtin_set_by_value.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(f3, f1, f2)\n"
            "      TYPE(field_type), intent(in) :: f3, f1, f2\n"
            "      INTEGER df\n"
            "      TYPE(field_proxy_type) f3_proxy, f1_proxy, f2_proxy\n"
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
            "        f3_proxy%data(df) = 0.5d0*f1_proxy%data(df) + "
            "0.8*f2_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code
    if dist_mem:
        output_dm_2 = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f3_proxy%vspace%get_last_dof_annexed()\n"
            "        f3_proxy%data(df) = 0.5d0*f1_proxy%data(df) + "
            "0.8*f2_proxy%data(df)\n"
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


def test_sign_X_by_value(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that we generate correct code for the built-in operation
    Y = sign(a, X) when a scalar is passed by value. Also test with and
    without annexed dofs being computed as this affects the generated
    code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.10.2_sign_X_builtin_set_by_value.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(f2, f1)\n"
            "      TYPE(field_type), intent(in) :: f2, f1\n"
            "      INTEGER df\n"
            "      TYPE(field_proxy_type) f2_proxy, f1_proxy\n"
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
            "        f2_proxy%data(df) = sign(- 2.0_r_def, "
            "f1_proxy%data(df))\n"
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
            "        f2_proxy%data(df) = sign(- 2.0_r_def, "
            "f1_proxy%data(df))\n"
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


# ------------- Built-ins with multiple calls or mixed with kernels --------- #


def test_multiple_builtin_set(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Tests that we generate correct code when we have an invoke
    containing multiple set operations. Test with and without annexed
    dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.14.2_multiple_set_kernels.f90"),
                           api=API)
    psy = PSyFactory(
        API, distributed_memory=dist_mem).create(invoke_info)
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(f1, fred, f2, f3, ginger)\n"
            "      REAL(KIND=r_def), intent(in) :: fred, ginger\n"
            "      TYPE(field_type), intent(in) :: f1, f2, f3\n"
            "      INTEGER df\n"
            "      TYPE(field_proxy_type) f1_proxy, f2_proxy, f3_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f1, "
            "undf_aspc1_f2, undf_aspc1_f3\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      f3_proxy = f3%get_proxy()\n"
            "      !\n"
            "      ! Initialise number of DoFs for aspc1_f1\n"
            "      !\n"
            "      undf_aspc1_f1 = f1_proxy%vspace%get_undf()\n"
            "      !\n"
            "      ! Initialise number of DoFs for aspc1_f2\n"
            "      !\n"
            "      undf_aspc1_f2 = f2_proxy%vspace%get_undf()\n"
            "      !\n"
            "      ! Initialise number of DoFs for aspc1_f3\n"
            "      !\n"
            "      undf_aspc1_f3 = f3_proxy%vspace%get_undf()\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=1,undf_aspc1_f1\n"
            "        f1_proxy%data(df) = fred\n"
            "      END DO\n"
            "      DO df=1,undf_aspc1_f2\n"
            "        f2_proxy%data(df) = 3.0_r_def\n"
            "      END DO\n"
            "      DO df=1,undf_aspc1_f3\n"
            "        f3_proxy%data(df) = ginger\n"
            "      END DO\n")
        assert output in code
    if dist_mem:
        output_dm_2 = (
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f1_proxy%vspace%get_last_dof_annexed()\n"
            "        f1_proxy%data(df) = fred\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      !\n"
            "      DO df=1,f2_proxy%vspace%get_last_dof_annexed()\n"
            "        f2_proxy%data(df) = 3.0_r_def\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f2_proxy%set_dirty()\n"
            "      !\n"
            "      DO df=1,f3_proxy%vspace%get_last_dof_annexed()\n"
            "        f3_proxy%data(df) = ginger\n"
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


def test_builtin_set_plus_normal(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Tests that we generate correct code for a built-in set operation
    when the invoke also contains a normal kernel. Test with and
    without annexed dofs being computed as this affects the generated
    code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.14.4_builtin_and_normal_kernel_invoke.f90"),
        api=API)

    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    dofmap_output = (
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        )
    assert dofmap_output in code

    if not dist_mem:
        output = (
            "      ! Initialise number of DoFs for w3\n"
            "      !\n"
            "      ndf_w3 = m2_proxy%vspace%get_ndf()\n"
            "      undf_w3 = m2_proxy%vspace%get_undf()\n"
            "      !\n"
            "      ! Initialise number of DoFs for aspc1_f1\n"
            "      !\n"
            "      ndf_aspc1_f1 = f1_proxy%vspace%get_ndf()\n"
            "      undf_aspc1_f1 = f1_proxy%vspace%get_undf()\n"
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
            "      END DO\n"
            "      DO df=1,undf_aspc1_f1\n"
            "        f1_proxy%data(df) = 0.0_r_def\n"
            "      END DO")
        assert output in code
    if dist_mem:
        mesh_code_present("f1", code)
        output_dm_2 = (
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
            "        CALL f2_proxy%halo_exchange(depth=1)\n"
            "      END IF\n"
            "      !\n"
            "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
            "        CALL m1_proxy%halo_exchange(depth=1)\n"
            "      END IF\n"
            "      !\n"
            "      IF (m2_proxy%is_dirty(depth=1)) THEN\n"
            "        CALL m2_proxy%halo_exchange(depth=1)\n"
            "      END IF\n"
            "      !\n"
            "      DO cell=1,mesh%get_last_halo_cell(1)\n"
            "        !\n"
            "        CALL testkern_code(nlayers, ginger, f1_proxy%data, "
            "f2_proxy%data, m1_proxy%data, m2_proxy%data, ndf_w1, "
            "undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), "
            "ndf_w3, undf_w3, map_w3(:,cell))\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      !\n"
            "      DO df=1,f1_proxy%vspace%get_last_dof_annexed()\n"
            "        f1_proxy%data(df) = 0.0_r_def\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      !\n")
        if not annexed:
            output_dm_2 = output_dm_2.replace("dof_annexed", "dof_owned")
            f1_hex_code = (
                "      ! Call kernels and communication routines\n"
                "      !\n"
                "      IF (f1_proxy%is_dirty(depth=1)) THEN\n"
                "        CALL f1_proxy%halo_exchange(depth=1)\n"
                "      END IF\n"
                "      !\n")
            output_dm_2 = output_dm_2.replace(
                "      ! Call kernels and communication routines\n"
                "      !\n", f1_hex_code)
        assert output_dm_2 in code


# ------------- Built-ins with reductions ----------------------------------- #


def test_multi_builtin_single_invoke(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that multiple built-ins, including one with reductions, produce
    correct code. Also test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.18.1_builtins_reduction_fuse_error.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if dist_mem:
        assert(
            "    SUBROUTINE invoke_0(asum, f1, f2, b)\n"
            "      USE scalar_mod, ONLY: scalar_type\n"
            "      REAL(KIND=r_def), intent(out) :: asum\n"
            "      REAL(KIND=r_def), intent(in) :: b\n"
            "      TYPE(field_type), intent(in) :: f1, f2\n"
            "      TYPE(scalar_type) global_sum\n"
            "      INTEGER df\n"
            "      TYPE(field_proxy_type) f1_proxy, f2_proxy\n") in code
        assert (
            "      f1_proxy = f1%get_proxy()\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      !\n") in code
        output = (
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
            "        asum = asum + f1_proxy%data(df)*f2_proxy%data(df)\n"
            "      END DO\n"
            "      global_sum%value = asum\n"
            "      asum = global_sum%get_sum()\n"
            "      DO df=1,f1_proxy%vspace%get_last_dof_annexed()\n"
            "        f1_proxy%data(df) = b * f1_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      !\n"
            "      DO df=1,f1_proxy%vspace%get_last_dof_annexed()\n"
            "        f1_proxy%data(df) = asum * f1_proxy%data(df)\n"
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
            "      TYPE(field_proxy_type) f1_proxy, f2_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f1\n") in code
        assert (
            "      f1_proxy = f1%get_proxy()\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      !\n"
            "      ! Initialise number of DoFs for aspc1_f1\n"
            "      !\n"
            "      undf_aspc1_f1 = "
            "f1_proxy%vspace%get_undf()\n") in code
        assert (
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      DO df=1,undf_aspc1_f1\n"
            "        asum = asum + f1_proxy%data(df)*f2_proxy%data(df)\n"
            "      END DO\n"
            "      DO df=1,undf_aspc1_f1\n"
            "        f1_proxy%data(df) = b * f1_proxy%data(df)\n"
            "      END DO\n"
            "      DO df=1,undf_aspc1_f1\n"
            "        f1_proxy%data(df) = asum * f1_proxy%data(df)\n"
            "      END DO\n") in code


# ------------- Invalid built-in with an integer scalar reduction ----------- #


def test_scalar_int_builtin_error(monkeypatch):
    ''' Test that specifying incorrect meta-data for built-in such that it
    claims to perform a reduction into an integer variable raises the
    expected error. '''
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


# ------------- Auxiliary mesh code generation function --------------------- #


def mesh_code_present(field_str, code):
    '''This test checks for the existance of mesh code. This exists for
    all built-ins with dm = True (although it is not actually required!) so
    each test can call this function. Mesh code is generated from the first
    field in a built-in arguments list, here denoted with field_str.'''
    assert "      USE mesh_mod, ONLY: mesh_type" in code
    assert "      TYPE(mesh_type), pointer :: mesh => null()" in code
    output_dm_1 = (
        "      !\n"
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh => " + field_str + "_proxy%vspace%get_mesh()\n"
        "      !\n")
    assert output_dm_1 in code
