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
# Modified: I. Kavcic and O. Brunt, Met Office
# Modified: R. W. Ford and N. Nobre, STFC Daresbury Lab
# Modified: by J. Henrichs, Bureau of Meteorology

''' This module tests the support for built-in operations in the LFRic API
    using pytest. Currently all built-in operations are 'pointwise' in that
    they iterate over DoFs. However this may change in the future.

    TODO #1796 - break the tests for each built-in into separate files under
                 the 'builtins' directory.
 '''

import os
import pytest

from psyclone.configuration import Config
from psyclone.core import Signature, VariablesAccessInfo
from psyclone.domain.lfric import lfric_builtins, LFRicConstants
from psyclone.domain.lfric.kernel import LFRicKernelMetadata, FieldArgMetadata
from psyclone.domain.lfric.lfric_builtins import (
    LFRicBuiltInCallFactory, LFRicBuiltIn)
from psyclone.dynamo0p3 import DynKernelArgument
from psyclone.errors import GenerationError, InternalError
from psyclone.parse.algorithm import BuiltInCall, parse
from psyclone.parse.utils import ParseError
from psyclone.psyGen import PSyFactory
from psyclone.psyir.nodes import (
    ArrayReference, Loop, Reference, UnaryOperation, Literal)
from psyclone.psyir.symbols import (
    ArrayType, ScalarType, UnsupportedFortranType)
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.utilities import get_invoke

# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "dynamo0p3")

# The PSyclone API under test
API = "dynamo0.3"


def dummy_func(self, _1, _2=True):
    '''Dummy routine that replaces '_init_data_type_properties' when used
    with monkeypatch and sets the minimum needed values to return
    without error for the associated tests.

    '''
    self._data_type = "dummy1"
    self._precision = "dummy2"
    self._proxy_data_type = "dummy3"
    self._module_name = "dummy4"


class Dummy1(LFRicBuiltIn):
    '''Utility subclass to enable the raising of an exception in the
    '__init__' method of abstract LFRicBuiltIn class.

    '''
    @staticmethod
    def metadata():
        '''Subclass abstract method.

        :returns: None
        :rtype: NoneType

        '''
        return None


class Dummy2(Dummy1):
    '''Utility subclass to allow instances of the abstract LFRicBuiltIn
    class to be tested.

    '''
    _datatype = "dummy"


class Dummy3(Dummy2):
    '''Utility subclass to allow abstract LFRicBuiltIn class
    '_builtin_metadata' method to be tested.

    '''
    # pylint: disable=too-many-ancestors
    _case_name = "test"


# ------------- Tests for built-ins methods and arguments ------------------- #


def test_lfric_builtin_abstract_method():
    '''Check that the 'LFRicBuiltIn' class is abstract and that the
    'metadata()' method is abstract.

    '''
    with pytest.raises(TypeError) as err:
        # pylint: disable=abstract-class-instantiated
        lfric_builtins.LFRicBuiltIn()
    assert "abstract class LFRicBuiltIn" in str(err.value)
    assert "metadata" in str(err.value)


def test_lfric_builtin_init():
    '''Check initiaisation of the abstract 'LFRicBuiltIn' class.'''
    instance = Dummy2()
    # Check '__init__'
    assert instance.qr_rules == {}
    assert instance.mesh is None
    assert instance._idx_name is None
    # Check 'super' is called from '__init__'
    assert instance._arg_descriptors is None
    # Check static values
    assert instance._case_name is None
    assert instance._datatype == "dummy"
    # Check for exception if '_datatype' is not specified in the subclass.
    with pytest.raises(NotImplementedError) as info:
        _ = Dummy1()
    assert ("An LFRicBuiltIn should be overridden by a subclass that sets "
            "the value of '_datatype', but '_datatype' is not set."
            in str(info.value))


def test_lfric_builtin_builtin_metadata():
    '''Check the '_builtin_metadata' method in the abstract 'LFRicBuiltIn'
    class.

    '''
    meta_args = [FieldArgMetadata("gh_real", "gh_write", "w0")]
    kernel_metadata = Dummy3._builtin_metadata(meta_args)
    assert isinstance(kernel_metadata, LFRicKernelMetadata)
    assert kernel_metadata.meta_args == meta_args
    assert kernel_metadata.operates_on == "dof"
    assert kernel_metadata.procedure_name == "test_code"
    assert kernel_metadata.name == "test"


def test_lfric_builtin_qr_required():
    '''Check the 'qr_required' method in the abstract 'LFRicBuiltIn'
    class.

    '''
    instance = Dummy2()
    assert not instance.qr_required


def test_lfric_builtin_fs_descriptors():
    '''Check the 'fs_descriptors' method in the abstract 'LFRicBuiltIn'
    class.

    '''
    instance = Dummy2()
    assert not instance.fs_descriptors


def test_lfricbuiltin_missing_defs(monkeypatch):
    '''Check that we raise an appropriate error if we cannot find the file
    specifying meta-data for built-in kernels.

    '''
    monkeypatch.setattr(lfric_builtins, "BUILTIN_DEFINITIONS_FILE",
                        "broken")
    with pytest.raises(ParseError) as excinfo:
        _, _ = parse(os.path.join(BASE_PATH,
                                  "15.12.3_single_pointwise_builtin.f90"),
                     api=API)
    assert ("broken' containing the meta-data describing the "
            "Built-in operations" in str(excinfo.value))


def test_lfricbuiltin_validate_not_over_dofs(monkeypatch):
    '''Check that we raise an appropriate error if we encounter a built-in
    that does not iterate over DoFs.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.12.3_single_pointwise_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    # Get a valid built-in kernel and then monkeypatch it.
    kern = psy.invokes.invoke_list[0].schedule.walk(LFRicBuiltIn)[0]
    monkeypatch.setattr(kern, "_iterates_over", "broken")
    with pytest.raises(ParseError) as err:
        kern._validate()
    assert ("built-in calls must operate on one of ['dof'] but found 'broken' "
            "for Built-in: setval_c (set a real-valued field "
            in str(err.value))


def test_builtin_multiple_writes():
    '''Check that we raise an appropriate error if we encounter a built-in
    that writes to more than one argument.

    '''
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
    assert (f"A built-in kernel in the LFRic API must have one and only "
            f"one argument that is written to but found 2 for kernel "
            f"'{test_builtin_name.lower()}'" in str(excinfo.value))


def test_builtin_write_and_readwrite():
    '''Check that we raise an appropriate error if we encounter a built-in
    that updates more than one argument where one is 'gh_write' and
    one is 'gh_readwrite'.

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
    assert (f"A built-in kernel in the LFRic API must have one and only "
            f"one argument that is written to but found 2 for kernel "
            f"'{test_builtin_name.lower()}'" in str(excinfo.value))


def test_builtin_sum_and_readwrite():
    '''Check that we raise an appropriate error if we encounter a built-in
    that updates more than one argument where one is 'gh_sum' and one
    is 'gh_readwrite'.

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
    assert (f"A built-in kernel in the LFRic API must have one and only "
            f"one argument that is written to but found 2 for kernel "
            f"'{test_builtin_name.lower()}'" in str(excinfo.value))


def test_builtin_zero_writes(monkeypatch):
    '''Check that we raise an appropriate error if we encounter a built-in
    that does not write to any field.

    '''
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
    assert (f"An LFRic kernel must have at least one argument that is "
            f"updated (written to) but found none for kernel "
            f"'{test_builtin_name.lower()}'." in str(excinfo.value))


def test_builtin_no_field_args(monkeypatch):
    '''Check that we raise appropriate error if we encounter a built-in
    that does not have any field arguments.

    '''
    # It is not possible to raise the required exception in normal
    # circumstances as PSyclone will complain that the datatype and
    # the metadata do not match. Therefore monkeypatch.
    monkeypatch.setattr(
        DynKernelArgument, "_init_data_type_properties",
        dummy_func)
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
    assert (f"A built-in kernel in the LFRic API must have at least "
            f"one field as an argument but kernel "
            f"'{test_builtin_name.lower()}' has none" in str(excinfo.value))


def test_builtin_invalid_argument_type(monkeypatch):
    '''Check that we raise appropriate error if we encounter a built-in
    that takes something other than a field or scalar argument.

    '''
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
    # It is not possible to raise the required exception in normal
    # circumstances as PSyclone will complain that the datatype and
    # the metadata do not match. Therefore monkeypatch.
    monkeypatch.setattr(
        DynKernelArgument, "_init_data_type_properties",
        dummy_func)
    with pytest.raises(ParseError) as excinfo:
        _ = PSyFactory(API, distributed_memory=False).create(invoke_info)
    const = LFRicConstants()
    assert (f"In the LFRic API an argument to a built-in kernel must be one "
            f"of {const.VALID_BUILTIN_ARG_TYPES} but kernel "
            f"'{test_builtin_name.lower()}' has an argument of type "
            f"'gh_operator'." in str(excinfo.value))


def test_builtin_invalid_data_type(monkeypatch):
    '''Check that we raise appropriate error if we encounter a built-in
    that takes something other than an argument of a 'real' or an
    'integer' data type.

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
    # It is not possible to raise the required exception in normal
    # circumstances as PSyclone will complain that the datatype and
    # the metadata do not match. Therefore monkeypatch.
    monkeypatch.setattr(
        DynKernelArgument, "_init_data_type_properties",
        dummy_func)
    with pytest.raises(ParseError) as excinfo:
        _ = PSyFactory(API, distributed_memory=False).create(invoke_info)
    const = LFRicConstants()
    assert (f"In the LFRic API an argument to a built-in kernel must have "
            f"one of {const.VALID_BUILTIN_DATA_TYPES} as a data type but "
            f"kernel '{test_builtin_name.lower()}' has an argument of "
            f"data type 'gh_logical'." in str(excinfo.value))


def test_builtin_args_not_same_space():
    '''Check that we raise the correct error if we encounter a built-in
    that has arguments on different function spaces.

    '''
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
    assert (f"All field arguments to a built-in in the LFRic API must "
            f"be on the same space. However, found spaces ['any_space_1', "
            f"'any_space_2'] for arguments to '{test_builtin_name.lower()}'"
            in str(excinfo.value))


def test_builtin_fld_args_different_data_type(monkeypatch):
    '''Check that we raise the correct error if we encounter a built-in
    that has has different data type of its field arguments and is not
    one of the data type conversion built-ins ('real_to_int_X',
    'real_to_real_X', and 'int_to_real_X').

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
    # It is not possible to raise the required exception in normal
    # circumstances as PSyclone will complain that the datatype and
    # the metadata do not match. Therefore monkeypatch.
    monkeypatch.setattr(
        DynKernelArgument, "_init_data_type_properties",
        dummy_func)
    with pytest.raises(ParseError) as excinfo:
        _ = PSyFactory(API,
                       distributed_memory=False).create(invoke_info)
    assert (f"In the LFRic API only the data type conversion built-ins "
            f"['real_to_int_X', 'real_to_real_X', 'int_to_real_X'] are "
            f"allowed to have field arguments of different data types. "
            f"However, found different data types ['gh_integer', 'gh_real'] "
            f"for field arguments to '{test_builtin_name.lower()}'."
            in str(excinfo.value))


def test_lfricbuiltincallfactory_str():
    '''Check that the '__str__' method of 'LFRicBuiltInCallFactory' works
    as expected.

    '''
    lfricinf = LFRicBuiltInCallFactory()
    assert str(lfricinf) == "Factory for a call to an LFRic built-in."


def test_lfricbuiltin_wrong_name():
    '''Check that 'LFRicBuiltInCallFactory.create()' raises an error if it
    doesn't recognise the name of the kernel it is passed.

    '''
    lfricinf = LFRicBuiltInCallFactory()
    # We use 'duck-typing' - rather than attempt to create a rather
    # complex Kernel object we use a ParseError object and monkeypatch
    # it so that it has a 'func_name' member.
    fake_kern = ParseError("blah")
    fake_kern.func_name = "pw_blah"
    with pytest.raises(ParseError) as excinfo:
        _ = lfricinf.create(fake_kern)
    assert ("Unrecognised built-in call in LFRic API: found 'pw_blah' "
            "but expected one of [" in str(excinfo.value))


def test_lfricbuiltin_not_dofs():
    '''Check that 'LFRicBuiltInCallFactory.create()' raises an error if
    the built-in doesn't iterate over DoFs.

    '''

    class KtypeDummy():
        '''A fake KernelType class which provides the required variables
        (including an invalid value of iterates_over) to
        allow the BuiltInCall class to be instantiated and passed to the
        LFRicBuiltInCallFactory.

        '''
        def __init__(self):
            self.nargs = 2
            self.name = "x_plus_y"
            self.iterates_over = "wrong"

    factory = LFRicBuiltInCallFactory()
    bincall = BuiltInCall(KtypeDummy(), ["a", "b"])
    with pytest.raises(InternalError) as err:
        factory.create(bincall)
    assert ("An LFRic built-in must iterate over DoFs but kernel 'x_plus_y' "
            "iterates over 'wrong'" in str(err.value))


def test_invalid_builtin_kernel():
    '''Check that we raise an appropriate error if an unrecognised
    built-in is specified in the algorithm layer.

    '''
    with pytest.raises(ParseError) as excinfo:
        _, _ = parse(os.path.join(BASE_PATH,
                                  "15.12.1_invalid_builtin_kernel.f90"),
                     api=API)
    assert ("kernel call 'setva_c' must either be named in a use "
            "statement (found ['constants_mod', 'field_mod']) or be a "
            "recognised built-in" in str(excinfo.value))


def test_lfricbuiltin_cma(dist_mem):
    '''Check that an 'LFRicBuiltIn' returns 'None' for CMA type (because
    built-ins don't work with CMA operators).

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.12.3_single_pointwise_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert kern.cma_operation is None


def test_lfricbuiltfactory_str():
    '''Check that the '__str__' method of 'LFRicBuiltInCallFactory' works
    as expected.

    '''
    factory = LFRicBuiltInCallFactory()
    assert "Factory for a call to an LFRic built-in." in str(factory)


def test_get_indexed_field_argument_refs():
    '''Test the 'LFRicBuiltIn.get_indexed_field_argument_references()'
    method.

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
        assert isinstance(ref, ArrayReference)
        assert isinstance(ref.symbol.datatype, UnsupportedFortranType)
        assert isinstance(ref.symbol.datatype.partial_datatype, ArrayType)
        assert len(ref.symbol.datatype.partial_datatype.shape) == 1
        # The reference in a built-in will have a data type hard coded
        assert isinstance(ref.datatype, ScalarType)
        assert ref.datatype.precision.name == "r_def"


def test_get_scalar_argument_references():
    '''Test the 'LFRicBuiltIn.get_scalar_argument_references()' method.'''
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
    '''Test the 'LFRicBuiltIn.get_dof_loop_index_symbol()' method.'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.8_a_plus_X_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    loop = psy.invokes.invoke_list[0].schedule[0]
    kern = loop.loop_body[0]
    sym = kern.get_dof_loop_index_symbol()
    assert sym.name == "df"
    assert sym.datatype.intrinsic == ScalarType.Intrinsic.INTEGER


def test_reference_accesses(monkeypatch):
    '''Test the 'reference_accesses()' method.'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.8_a_plus_X_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    loop = psy.invokes.invoke_list[0].schedule[0]
    kern = loop.loop_body[0]
    var_info = VariablesAccessInfo()
    kern.reference_accesses(var_info)
    # f2_data(df) = a + f1_data(df)
    assert var_info.is_written(Signature("f2_data"))
    assert var_info.is_read(Signature("f1_data"))
    assert var_info.is_read(Signature("a"))
    # Check for the expected error if an unsupported type of argument
    # is encountered. Use monkeypatch to break one of the existing args.
    monkeypatch.setattr(kern.args[0], "_argument_type", "gh_wrong")
    with pytest.raises(InternalError) as err:
        kern.reference_accesses(var_info)
    assert ("LFRicBuiltin.reference_accesses only supports field and scalar "
            "arguments but got 'f2' of type 'gh_wrong'" in str(err.value))


# ------------- Adding (scaled) real fields --------------------------------- #


def test_X_plus_Y(
        tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that:
    1) the '__str__' method of 'LFRicXPlusYKern' returns the expected
       string;
    2) we generate correct code for the built-in 'Z = X + Y'
       where 'X' and 'Y' are real-valued fields;
    3) that we generate correct bounds when
       'Config.api_conf(API)._compute_annexed_dofs' is False and True;
    4) that the 'lower_to_language_level()' method works as expected;
    5) that the 'metadata()' method of 'LFRicXPlusYKern' returns kernel
       metadata.

    '''
    # pylint: disable=unidiomatic-typecheck
    metadata = lfric_builtins.LFRicXPlusYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    # Test the metadata values in this test. Future tests will just
    # check that an LFRicKernelMetadata instance is returned.
    assert metadata.operates_on == "dof"
    assert metadata.name == "X_plus_Y"
    assert metadata.procedure_name == "X_plus_Y_code"
    assert len(metadata.meta_args) == 3
    assert type(metadata.meta_args[0]) is FieldArgMetadata
    assert metadata.meta_args[0].datatype == "gh_real"
    assert metadata.meta_args[0].access == "gh_write"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert type(metadata.meta_args[1]) is FieldArgMetadata
    assert metadata.meta_args[1].datatype == "gh_real"
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[1].function_space == "any_space_1"
    assert type(metadata.meta_args[2]) is FieldArgMetadata
    assert metadata.meta_args[2].datatype == "gh_real"
    assert metadata.meta_args[2].access == "gh_read"
    assert metadata.meta_args[2].function_space == "any_space_1"
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.1_X_plus_Y_builtin.f90"),
                           api=API)
    psy = PSyFactory(API,
                     distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: X_plus_Y (add real-valued fields)"

    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Check for the correct field type declarations
    output = (
        "      TYPE(field_type), intent(in) :: f3, f1, f2\n"
        "      INTEGER df\n"
        "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f2_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f1_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f3_data => null()\n"
        "      TYPE(field_proxy_type) f3_proxy, f1_proxy, f2_proxy\n")
    assert output in code

    if not dist_mem:
        # The value of '_compute_annexed_dofs' should make no difference
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

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f3_data(df) = f1_data(df) + f2_data(df)\n"
                "enddo") in code
    else:
        output_dm_2 = (
            "      loop0_stop = f3_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f3_data(df) = f1_data(df) + f2_data(df)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f3_proxy%set_dirty()\n"
            "      !\n")
        if not annexed:
            # Only compute owned DoFs if '_compute_annexed_dofs' is False
            output_dm_2 = output_dm_2.replace("annexed", "owned")
        assert output_dm_2 in code


def test_inc_X_plus_Y(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIncXPlusYKern' returns
    the expected string and 2) we generate correct code for the
    built-in 'X = X + Y' where 'X' and 'Y' are real-valued
    fields. Test with and without annexed DoFs being computed as this
    affects the generated code. 3) Also test the
    'lower_to_language_level()' and 'metadata()' methods for this
    built-in.

    '''
    metadata = lfric_builtins.LFRicIncXPlusYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)

    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.2_inc_X_plus_Y_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert (str(kern) == "Built-in: inc_X_plus_Y (increment a "
            "real-valued field)")
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
            "        f1_data(df) = f1_data(df) + f2_data(df)\n"
            "      END DO\n")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f1_data(df) = f1_data(df) + f2_data(df)\n"
                "enddo" in code)
    else:
        output = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = f1_data(df) + f2_data(df)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()")
        if not annexed:
            output = output.replace("dof_annexed", "dof_owned")
        assert output in code


def test_a_plus_X(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicAPlusXKern' returns the
    expected string and 2) we generate correct code for the built-in
    operation 'Y = a + X' where 'a' is a real scalar and 'X' and 'Y'
    are real-valued fields. Test with and without annexed DoFs being
    computed as this affects the generated code. 3) Also test the
    'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicAPlusXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.8_a_plus_X_builtin.f90"),
                           api=API)

    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: a_plus_X (real-valued fields)"
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
            "        f2_data(df) = a + f1_data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        # Check the type of the scalar
        scalar = loop.scope.symbol_table.lookup("a")
        assert isinstance(scalar.datatype, ScalarType)
        assert scalar.datatype.intrinsic == ScalarType.Intrinsic.REAL
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f2_data(df) = a + f1_data(df)\n"
                "enddo") in code
    else:
        output_dm = (
            "      loop0_stop = f2_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_data(df) = a + f1_data(df)\n"
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


def test_inc_a_plus_X(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIncAPlusXKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'X = a + X' where 'a' is a real scalar and 'X'
    is a real-valued field. Test with and without annexed DoFs being
    computed as this affects the generated code. 3) Also test the
    'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicIncAPlusXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.9_inc_a_plus_X_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: inc_a_plus_X (real-valued field)"
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
            "        f1_data(df) = a + f1_data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f1_data(df) = a + f1_data(df)\n"
                "enddo") in code
    else:
        output_dm = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = a + f1_data(df)\n"
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


def test_aX_plus_Y(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicAXPlusYKern' returns the
    expected string and 2) we generate correct code for the built-in
    operation 'Z = a*X + Y' where 'a' is a real scalar and 'Z', 'X'
    and 'Y' are real-valued fields. Test with and without annexed DoFs
    being computed as this affects the generated code. 3) Also test
    the 'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicAXPlusYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.3_aX_plus_Y_builtin.f90"),
                           api=API)

    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: aX_plus_Y (real-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Check for the correct 'use' module statements as a non-default
    # precision, 'r_bl', is used for real-valued fields and scalars.
    output_mod = (
        "    USE constants_mod, ONLY: r_bl, i_def\n"
        "    USE r_bl_field_mod, ONLY: r_bl_field_type, "
        "r_bl_field_proxy_type\n")
    assert output_mod in code

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(f3, a, f1, f2)\n"
            "      REAL(KIND=r_bl), intent(in) :: a\n"
            "      TYPE(r_bl_field_type), intent(in) :: f3, f1, f2\n"
            "      INTEGER df\n"
            "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
            "      REAL(KIND=r_bl), pointer, dimension(:) :: f2_data => "
            "null()\n"
            "      REAL(KIND=r_bl), pointer, dimension(:) :: f1_data => "
            "null()\n"
            "      REAL(KIND=r_bl), pointer, dimension(:) :: f3_data => "
            "null()\n"
            "      TYPE(r_bl_field_proxy_type) f3_proxy, f1_proxy, f2_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f3\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
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
            "        f3_data(df) = a * f1_data(df) + f2_data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f3_data(df) = a * f1_data(df) + f2_data(df)\n"
                "enddo") in code
    else:
        output_dm_2 = (
            "      loop0_stop = f3_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f3_data(df) = a * f1_data(df) + f2_data(df)\n"
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


def test_inc_aX_plus_Y(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIncAXPlusYKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'X = a*X + Y' where 'a' is a real scalar and
    'X' and 'Y' are real-valued fields. Test with and without annexed
    DoFs being computed as this affects the generated code. 3) Also
    test the 'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicIncAXPlusYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.4_inc_aX_plus_Y_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
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
            "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f2_data => "
            "null()\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f1_data => "
            "null()\n"
            "      TYPE(field_proxy_type) f1_proxy, f2_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f1\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      f1_data => f1_proxy%data\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      f2_data => f2_proxy%data\n"
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
            "        f1_data(df) = a * f1_data(df) + f2_data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f1_data(df) = a * f1_data(df) + "
                "f2_data(df)\n"
                "enddo") in code
    else:
        output_dm_2 = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = a * f1_data(df) + "
            "f2_data(df)\n"
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


def test_inc_X_plus_bY(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIncXPlusBYKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'X = X + b*Y' where 'b' is a real scalar and
    'X' and 'Y' are real-valued fields. Test with and without annexed
    DoFs being computed as this affects the generated code. 3) Also
    test the 'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicIncXPlusBYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.5_inc_X_plus_bY_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
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
            "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f2_data => "
            "null()\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f1_data => "
            "null()\n"
            "      TYPE(field_proxy_type) f1_proxy, f2_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f1\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      f1_data => f1_proxy%data\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      f2_data => f2_proxy%data\n"
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
            "        f1_data(df) = f1_data(df) + b * f2_data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f1_data(df) = f1_data(df) + "
                "b * f2_data(df)\n"
                "enddo") in code
    else:
        output_dm_2 = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = f1_data(df) + "
            "b * f2_data(df)\n"
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


def test_aX_plus_bY(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicAXPlusBYKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'Z = a*X + b*Y' where 'a' and 'b' are real
    scalars and 'Z', 'X' and 'Y' are real-valued fields. Test with and
    without annexed DoFs being computed as this affects the generated
    code. 3) Also test the 'lower_to_language_level()' and
    'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicAXPlusBYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.6_aX_plus_bY_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
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
            "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f2_data => "
            "null()\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f1_data => "
            "null()\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f3_data => "
            "null()\n"
            "      TYPE(field_proxy_type) f3_proxy, f1_proxy, f2_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f3\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
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
            "        f3_data(df) = a * f1_data(df) + b * f2_data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f3_data(df) = a * f1_data(df) + b * f2_data(df)\n"
                "enddo") in code
    else:
        output_dm_2 = (
            "      loop0_stop = f3_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f3_data(df) = a * f1_data(df) + b * f2_data(df)\n"
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


def test_inc_aX_plus_bY(
        tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIncAXPlusBYKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'X = a*X + b*Y' where 'a' and 'b' are real
    scalars and 'X' and 'Y' are real-valued fields. Test with and
    without annexed DoFs being computed as this affects the generated
    code. 3) Also test the 'lower_to_language_level()' and
    'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicIncAXPlusBYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.1.7_inc_aX_plus_bY_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
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
            "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f2_data => "
            "null()\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f1_data => "
            "null()\n"
            "      TYPE(field_proxy_type) f1_proxy, f2_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f1\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      f1_data => f1_proxy%data\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      f2_data => f2_proxy%data\n"
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
            "        f1_data(df) = a * f1_data(df) + "
            "b * f2_data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f1_data(df) = a * f1_data(df) + b * f2_data(df)\n"
                "enddo") in code
    else:
        output_dm_2 = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = a * f1_data(df) + b * f2_data(df)\n"
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


def test_aX_plus_aY(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicAXPlusAYKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'Z = a*(X + Y)' where 'a' is a real scalar and
    'Z', 'X' and 'Y' are real-valued fields. Test with and without
    annexed DoFs being computed as this affects the generated code. 3)
    Also test the 'lower_to_language_level()' and 'metadata()'
    methods.

    '''
    metadata = lfric_builtins.LFRicAXPlusAYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.10_aX_plus_aY_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: aX_plus_aY (real-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      loop0_stop = undf_aspc1_f3\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f3_data(df) = a * (f1_data(df) + f2_data(df))\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f3_data(df) = a * (f1_data(df) + "
                "f2_data(df))\n"
                "enddo") in code
    else:
        output_dm_2 = (
            "      loop0_stop = f3_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f3_data(df) = a * (f1_data(df) + "
            "f2_data(df))\n"
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


def test_X_minus_Y(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicXMinusYKern' returns the
    expected string and 2) we generate correct code for the built-in
    operation 'Z = X - Y' where 'Z', 'X' and 'Y' are real-valued
    fields. Test with and without annexed DoFs being computed as this
    affects the generated code. 3) Also test the
    'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicXMinusYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.2.1_X_minus_Y_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: X_minus_Y (subtract real-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
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
            "        f3_data(df) = f1_data(df) - f2_data(df)\n"
            "      END DO")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f3_data(df) = f1_data(df) - "
                "f2_data(df)\n"
                "enddo") in code
    else:
        output_dm_2 = (
            "      loop0_stop = f3_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f3_data(df) = f1_data(df) - "
            "f2_data(df)\n"
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


def test_inc_X_minus_Y(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIncXMinusYKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'X = X - Y' where 'X' and 'Y' are real-valued
    fields. Test with and without annexed DoFs being computed as this
    affects the generated code. 3) Also test the
    'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicIncXMinusYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.2.2_inc_X_minus_Y_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert (str(kern) == "Built-in: inc_X_minus_Y (decrement a "
            "real-valued field)")
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      f1_proxy = f1%get_proxy()\n"
            "      f1_data => f1_proxy%data\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      f2_data => f2_proxy%data\n"
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
            "        f1_data(df) = f1_data(df) - f2_data(df)\n"
            "      END DO\n")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f1_data(df) = f1_data(df) - "
                "f2_data(df)\n"
                "enddo") in code
    else:
        output = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = f1_data(df) - "
            "f2_data(df)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()")
        if not annexed:
            output = output.replace("dof_annexed", "dof_owned")
        assert output in code


def test_a_minus_X(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicAMinusXKern' returns the
    expected string and 2) we generate correct code for the built-in
    operation 'Y = a - X' where 'a' is a real scalar and 'X' and 'Y'
    are real-valued fields. Test with and without annexed DoFs being
    computed as this affects the generated code. 3) Also test the
    'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicAMinusXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.2.7_a_minus_X_builtin.f90"),
                           api=API)

    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: a_minus_X (real-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Check for the correct 'use' module statements
    output_mod = (
        "    USE constants_mod, ONLY: r_def, i_def\n"
        "    USE field_mod, ONLY: field_type, field_proxy_type\n")
    assert output_mod in code

    if not dist_mem:
        output = (
            "      loop0_stop = undf_aspc1_f2\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_data(df) = a - f1_data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f2_data(df) = a - f1_data(df)\n"
                "enddo") in code
    else:
        output_dm = (
            "      loop0_start = 1\n"
            "      loop0_stop = f2_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_data(df) = a - f1_data(df)\n"
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


def test_inc_a_minus_X(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIncAMinusXKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'X = a - X' where 'a' is a real scalar and 'X'
    is a real-valued field. Test with and without annexed DoFs being
    computed as this affects the generated code. 3) Also test the
    'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicIncXMinusYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.2.8_inc_a_minus_X_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: inc_a_minus_X (real-valued field)"
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
            "        f1_data(df) = a - f1_data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f1_data(df) = a - f1_data(df)\n"
                "enddo") in code
    else:
        output_dm = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = a - f1_data(df)\n"
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


def test_X_minus_a(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicAMinusXKern' returns the
    expected string and 2) we generate correct code for the built-in
    operation 'Y = X - a' where 'a' is a real scalar and 'X' and 'Y'
    are real-valued fields. Test with and without annexed DoFs being
    computed as this affects the generated code. 3) Also test the
    'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicIncAMinusXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.2.9_X_minus_a_builtin.f90"),
                           api=API)

    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: X_minus_a (real-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Check for the correct 'use' module statements
    output_mod = (
        "    USE constants_mod, ONLY: r_tran, i_def\n"
        "    USE r_tran_field_mod, ONLY: r_tran_field_type, "
        "r_tran_field_proxy_type\n")
    assert output_mod in code

    if not dist_mem:
        output = (
            "      loop0_stop = undf_aspc1_f2\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_data(df) = f1_data(df) - a\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f2_data(df) = f1_data(df) - a\n"
                "enddo") in code
    else:
        output_dm = (
            "      loop0_start = 1\n"
            "      loop0_stop = f2_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_data(df) = f1_data(df) - a\n"
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


def test_inc_X_minus_a(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIncXMinusAKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'X = X - a' where 'a' is a real scalar and 'X'
    is a real-valued field. Test with and without annexed DoFs being
    computed as this affects the generated code. 3) Also test the
    'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicIncXMinusAKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.2.10_inc_X_minus_a_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: inc_X_minus_a (real-valued field)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Check for the correct field and scalar type declarations
    output = (
        "      REAL(KIND=r_tran), intent(in) :: a\n"
        "      TYPE(r_tran_field_type), intent(in) :: f1\n"
        "      INTEGER df\n"
        "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
        "      REAL(KIND=r_tran), pointer, dimension(:) :: f1_data => "
        "null()\n"
        "      TYPE(r_tran_field_proxy_type) f1_proxy\n")
    assert output in code

    if not dist_mem:
        assert "INTEGER(KIND=i_def) undf_aspc1_f1\n" in code
        output = (
            "      loop0_stop = undf_aspc1_f1\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = f1_data(df) - a\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f1_data(df) = f1_data(df) - a\n"
                "enddo") in code
    else:
        assert "INTEGER(KIND=i_def) max_halo_depth_mesh\n" in code
        output_dm = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = f1_data(df) - a\n"
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


def test_aX_minus_Y(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicAXMinusYKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'Z = a*X - Y' where 'a' is a real scalar and
    'Z', 'X' and 'Y' are real-valued fields. Test with and without
    annexed DoFs being computed as this affects the generated code. 3)
    Also test the 'lower_to_language_level()' and 'metadata()'
    methods.

    '''
    metadata = lfric_builtins.LFRicAXMinusYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.2.3_aX_minus_Y_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
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
            "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f2_data => "
            "null()\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f1_data => "
            "null()\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f3_data => "
            "null()\n"
            "      TYPE(field_proxy_type) f3_proxy, f1_proxy, f2_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f3\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
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
            "        f3_data(df) = a * f1_data(df) - f2_data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f3_data(df) = a * f1_data(df) - f2_data(df)\n"
                "enddo") in code
    else:
        output_dm_2 = (
            "      loop0_stop = f3_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f3_data(df) = a * f1_data(df) - f2_data(df)\n"
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


def test_X_minus_bY(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicXMinusBYKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'Z = X - b*Y' where 'b' is a real scalar and
    'Z', 'X' and 'Y' are real-valued fields. Test with and without
    annexed DoFs being computed as this affects the generated code. 3)
    Also test the 'lower_to_language_level()' and 'metadata()'
    methods.

    '''
    metadata = lfric_builtins.LFRicXMinusBYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.2.4_X_minus_bY_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
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
            "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f2_data => "
            "null()\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f1_data => "
            "null()\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f3_data => "
            "null()\n"
            "      TYPE(field_proxy_type) f3_proxy, f1_proxy, f2_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f3\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
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
            "        f3_data(df) = f1_data(df) - b * f2_data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f3_data(df) = f1_data(df) - b * f2_data(df)\n"
                "enddo") in code
    else:
        output_dm_2 = (
            "      loop0_stop = f3_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f3_data(df) = f1_data(df) - b * f2_data(df)\n"
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


def test_inc_X_minus_bY(
        tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIncXMinusBYKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'X = X - b*Y' where 'b' is a real scalar and
    'X' and 'Y' are real-valued fields. Test with and without annexed
    DoFs being computed as this affects the generated code. 3) Also
    test the lower_to_language() and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicIncXMinusBYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.2.5_inc_X_minus_bY_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
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
            "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f2_data => "
            "null()\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f1_data => "
            "null()\n"
            "      TYPE(field_proxy_type) f1_proxy, f2_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f1\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      f1_data => f1_proxy%data\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      f2_data => f2_proxy%data\n"
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
            "        f1_data(df) = f1_data(df) - b * f2_data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f1_data(df) = f1_data(df) - b * f2_data(df)\n"
                "enddo") in code
    else:
        output_dm_2 = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = f1_data(df) - b * f2_data(df)\n"
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


def test_aX_minus_bY(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicAXMinusBYKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'Z = a*X - b*Y' where 'a' and 'b' are real
    scalars and 'Z', 'X' and 'Y' are real-valued fields. Test with and
    without annexed DoFs being computed as this affects the generated
    code. 3) Also test the 'lower_to_language_level()' and
    'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicAXMinusBYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.2.6_aX_minus_bY_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: aX_minus_bY (real-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
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
            "        f3_data(df) = a * f1_data(df) - b * f2_data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f3_data(df) = a * f1_data(df) - b * f2_data(df)\n"
                "enddo") in code
    else:
        output_dm_2 = (
            "      loop0_stop = f3_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f3_data(df) = a * f1_data(df) - b * f2_data(df)\n"
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


def test_X_times_Y(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of LFRicXTimesYKern returns the
    expected string and 2) we generate correct code for the built-in
    operation 'Z = X*Y' where 'Z', 'X' and 'Y' are real-valued
    fields. Test with and without annexed DoFs being computed as this
    affects the generated code. 3) Also test the
    'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicXTimesYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.3.1_X_times_Y_builtin.f90"), api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: X_times_Y (multiply real-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
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
            "        f3_data(df) = f1_data(df) * f2_data(df)\n"
            "      END DO\n")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f3_data(df) = f1_data(df) * f2_data(df)\n"
                "enddo") in code
    else:
        output = (
            "      loop0_stop = f3_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f3_data(df) = f1_data(df) * f2_data(df)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f3_proxy%set_dirty()")
        if not annexed:
            output = output.replace("dof_annexed", "dof_owned")
        assert output in code


def test_inc_X_times_Y(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of LFRicIncXTimesYKern returns
    the expected string and 2) we generate correct code for the
    built-in operation 'X = X*Y' where 'X' and 'Y' are real-valued
    fields. Test with and without annexed DoFs being computed as this
    affects the generated code. 3) Also test the
    'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicIncXTimesYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.3.2_inc_X_times_Y_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert (str(kern) == "Built-in: inc_X_times_Y (multiply one real-valued "
            "field by another)")
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      f1_proxy = f1%get_proxy()\n"
            "      f1_data => f1_proxy%data\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      f2_data => f2_proxy%data\n"
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
            "        f1_data(df) = f1_data(df) * f2_data(df)\n"
            "      END DO")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f1_data(df) = f1_data(df) * f2_data(df)\n"
                "enddo" in code)
    else:
        output_dm_2 = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = f1_data(df) * f2_data(df)\n"
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


def test_inc_aX_times_Y(
        tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIncAXTimesYKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'X = a*X*Y' where 'a' is a real scalar and 'X'
    and 'Y' are real-valued fields. Test with and without annexed DoFs
    being computed as this affects the generated code. 3) Also test
    the 'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicIncAXTimesYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.3.3_inc_aX_times_Y_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: inc_aX_times_Y (real-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      f1_data => f1_proxy%data\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      f2_data => f2_proxy%data\n"
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
            "        f1_data(df) = a * f1_data(df) * f2_data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f1_data(df) = a * f1_data(df) * f2_data(df)\n"
                "enddo") in code
    else:
        output_dm_2 = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = a * f1_data(df) * f2_data(df)\n"
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


def test_a_times_X(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicATimesXKern' returns the
    expected string and 2) we generate correct code for the built-in
    operation 'Y = a*X' where 'a' is a real scalar and 'X' and 'Y' are
    real-valued fields. Test with and without annexed DoFs being
    computed as this affects the generated code. 3) Also test the
    'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicATimesXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.4.1_a_times_X_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: a_times_X (copy a scaled real-valued field)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Check for the correct 'use' module statements as a non-default
    # precision, 'r_phys', is used for real-valued fields and scalars.
    output_mod = (
        "    USE constants_mod, ONLY: r_phys, i_def\n"
        "    USE r_phys_field_mod, ONLY: r_phys_field_type, "
        "r_phys_field_proxy_type\n")
    assert output_mod in code

    if not dist_mem:
        output = (
            "      REAL(KIND=r_phys), pointer, dimension(:) :: f1_data => "
            "null()\n"
            "      REAL(KIND=r_phys), pointer, dimension(:) :: f2_data => "
            "null()\n"
            "      TYPE(r_phys_field_proxy_type) f2_proxy, f1_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f2\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      f2_data => f2_proxy%data\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      f1_data => f1_proxy%data\n"
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
            "        f2_data(df) = a_scalar * f1_data(df)\n"
            "      END DO")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f2_data(df) = a_scalar * f1_data(df)\n"
                "enddo") in code
    else:
        output_dm = (
            "      loop0_stop = f2_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_data(df) = a_scalar * f1_data(df)\n"
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


def test_inc_a_times_X(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIncATimesXKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'X = a*X' where 'a' is a real scalar and 'X' is
    a real-valued field. Test with and without annexed DoFs being
    computed as this affects the generated code. 3) Also test the
    'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicIncATimesXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.4.2_inc_a_times_X_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: inc_a_times_X (scale a real-valued field)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      REAL(KIND=r_def), pointer, dimension(:) :: f1_data => "
            "null()\n"
            "      TYPE(field_proxy_type) f1_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f1\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      f1_data => f1_proxy%data\n"
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
            "        f1_data(df) = a_scalar * f1_data(df)\n"
            "      END DO\n"
            "      !\n")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f1_data(df) = a_scalar * f1_data(df)\n"
                "enddo") in code
    else:
        output_dm = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = a_scalar * f1_data(df)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()")
        if not annexed:
            output_dm = output_dm.replace("dof_annexed", "dof_owned")
        assert output_dm in code


# ------------- Dividing real fields ---------------------------------------- #


def test_X_divideby_Y(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicXDividebyYKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'Z = X/Y' where 'Z', 'X' and 'Y' are fields
    real-valued. Test with and without annexed DoFs being computed as
    this affects the generated code. 3) Also test the
    'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicXDividebyYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.5.1_X_divideby_Y_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: X_divideby_Y (divide real-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
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
            "        f3_data(df) = f1_data(df) / f2_data(df)\n"
            "      END DO")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f3_data(df) = f1_data(df) / f2_data(df)\n"
                "enddo") in code
    else:
        output_dm_2 = (
            "      loop0_stop = f3_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f3_data(df) = f1_data(df) / f2_data(df)\n"
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


def test_inc_X_divideby_Y(
        tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIncXDividebyYKern'
    returns the expected string and 2) we generate correct code for
    the built-in operation 'X = X/Y' where 'X' and 'Y' are real-valued
    fields. Test with and without annexed DoFs being computed as this
    affects the generated code. 3) Also test the
    'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicIncXDividebyYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.5.2_inc_X_divideby_Y_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert (str(kern) == "Built-in: inc_X_divideby_Y (divide one real-valued "
            "field by another)")
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      f1_proxy = f1%get_proxy()\n"
            "      f1_data => f1_proxy%data\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      f2_data => f2_proxy%data\n"
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
            "        f1_data(df) = f1_data(df) / f2_data(df)\n"
            "      END DO")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f1_data(df) = f1_data(df) / f2_data(df)\n"
                "enddo" in code)
    else:
        output_dm_2 = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = f1_data(df) / f2_data(df)\n"
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


def test_X_divideby_a(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicXDividebyAKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'Y = X/a' where 'a' is a real scalar and 'X'
    and 'Y' are real-valued fields. Test with and without annexed DoFs
    being computed as this affects the generated code. 3) Also test
    the 'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicXDividebyAKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.5.5_X_divideby_a_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == ("Built-in: X_divideby_a (divide a real-valued field "
                         "by a real scalar (Y = X/a))")
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Check for the correct field and scalar type declarations
    output = (
        "      REAL(KIND=r_solver), intent(in) :: a_scalar\n"
        "      TYPE(r_solver_field_type), intent(in) :: f2\n"
        "      TYPE(field_type), intent(in) :: f1\n"
        "      INTEGER df\n"
        "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f1_data => "
        "null()\n"
        "      TYPE(field_proxy_type) f1_proxy\n"
        "      REAL(KIND=r_solver), pointer, dimension(:) :: f2_data => "
        "null()\n"
        "      TYPE(r_solver_field_proxy_type) f2_proxy\n")
    assert output in code

    if not dist_mem:
        output = (
            "      f2_proxy = f2%get_proxy()\n"
            "      f2_data => f2_proxy%data\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      f1_data => f1_proxy%data\n"
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
            "        f2_data(df) = f1_data(df) / a_scalar\n"
            "      END DO")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f2_data(df) = f1_data(df) / a_scalar\n"
                "enddo") in code
    else:
        output_dm_2 = (
            "      loop0_stop = f2_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_data(df) = f1_data(df) / a_scalar\n"
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


def test_inc_X_divideby_a(
        tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIncXDividebyAKern'
    returns the expected string and 2) we generate correct code for
    the built-in operation 'X = X/a' where 'a' is a real scalar and
    'X' is a real-valued field. Test with and without annexed DoFs
    being computed as this affects the generated code. 3) Also test
    the 'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicIncXDividebyAKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.5.6_inc_X_divideby_a_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == ("Built-in: inc_X_divideby_a (divide a real-valued "
                         "field by a real scalar (X = X/a))")
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Check for the correct field and scalar type declarations
    output = (
        "      REAL(KIND=r_def), intent(in) :: a_scalar\n"
        "      TYPE(r_tran_field_type), intent(in) :: f1\n"
        "      INTEGER df\n"
        "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
        "      REAL(KIND=r_tran), pointer, dimension(:) :: f1_data => "
        "null()\n"
        "      TYPE(r_tran_field_proxy_type) f1_proxy\n")
    assert output in code

    if not dist_mem:
        output = (
            "      f1_proxy = f1%get_proxy()\n"
            "      f1_data => f1_proxy%data\n"
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
            "        f1_data(df) = f1_data(df) / a_scalar\n"
            "      END DO")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f1_data(df) = f1_data(df) / a_scalar\n"
                "enddo") in code
    else:
        output_dm_2 = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = f1_data(df) / a_scalar\n"
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


def test_a_divideby_X(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicADividebyXKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'Y = a/X' where 'a' is a real scalar and 'X'
    and 'Y' are real-valued fields. Test with and without annexed DoFs
    being computed as this affects the generated code. 3) Also test
    the 'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicADividebyXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.5.3_a_divideby_X_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert (str(kern) == "Built-in: a_divideby_X (inverse scaling of a "
            "real-valued field (Y = a/X))")
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      f2_proxy = f2%get_proxy()\n"
            "      f2_data => f2_proxy%data\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      f1_data => f1_proxy%data\n"
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
            "        f2_data(df) = a_scalar / f1_data(df)\n"
            "      END DO")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f2_data(df) = a_scalar / f1_data(df)\n"
                "enddo") in code
    else:
        output_dm = (
            "      loop0_stop = f2_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_data(df) = a_scalar / f1_data(df)\n"
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


def test_inc_a_divideby_X(
        tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIncADividebyXKern'
    returns the expected string and 2) we generate correct code for
    the built-in operation 'X = a/X' where 'a' is a real scalar and
    'X' is a real-valued field. Test with and without annexed DoFs
    being computed as this affects the generated code. 3) Also test
    the 'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicIncADividebyXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.5.4_inc_a_divideby_X_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert (str(kern) == "Built-in: inc_a_divideby_X (inverse scaling of a "
            "real-valued field (X = a/X))")
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
            "        f1_data(df) = a_scalar / f1_data(df)\n"
            "      END DO\n"
            "      !\n")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f1_data(df) = a_scalar / f1_data(df)\n"
                "enddo") in code
    else:
        output_dm = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = a_scalar / f1_data(df)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()")
        if not annexed:
            output_dm = output_dm.replace("dof_annexed", "dof_owned")
        assert output_dm in code


# ------------- Raising a real field to a scalar ---------------------------- #


def test_inc_X_powreal_a(
        tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIncXPowrealAKern'
    returns the expected string and 2) we generate correct code for
    the built-in operation 'X = X**a' where 'a' is a real scalar and
    'X' is a real-valued field. Test with and without annexed DoFs
    being computed as this affects the generated code. 3) Also test
    the 'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicIncXPowrealAKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.6.1_inc_X_powreal_a_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert (str(kern) == "Built-in: inc_X_powreal_a (raise a real-valued "
            "field to a real power)")
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      ndf_aspc1_f1 = f1_proxy%vspace%get_ndf()\n"
            "      undf_aspc1_f1 = f1_proxy%vspace%get_undf()\n"
            "      !\n"
            "      ! Set-up all of the loop bounds\n"
            "      !\n"
            "      loop0_start = 1\n"
            "      loop0_stop = undf_aspc1_f1\n"
            "      loop1_start = 1\n"
            "      loop1_stop = undf_aspc1_f1\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = f1_data(df) ** a_scalar\n"
            "      END DO\n"
            "      !\n")

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        # Check the type of the scalar power
        scalar = loop.scope.symbol_table.lookup("a_scalar")
        assert isinstance(scalar.datatype, ScalarType)
        assert scalar.datatype.intrinsic == ScalarType.Intrinsic.REAL
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f1_data(df) = f1_data(df) ** a_scalar\n"
                "enddo") in code
    else:
        output = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      loop1_start = 1\n"
            "      loop1_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = f1_data(df) ** a_scalar\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()")
        if not annexed:
            output = output.replace("dof_annexed", "dof_owned")
        assert output in code
        assert ("f1_data(df) = f1_data(df) ** 1.0e-3_r_def\n"
                in code)


def test_inc_X_powint_n(
        tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIncXPowintNKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'X = X**n' where 'n' is an integer scalar and
    'X' is a real-valued field. Also test with and without annexed
    DoFs being computed as this affects the generated code. 3) Also
    test the 'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicIncXPowintNKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.6.2_inc_X_powint_n_builtin.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == ("Built-in: inc_X_powint_n (raise a real-valued field "
                         "to an integer power)")

    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      ndf_aspc1_f1 = f1_proxy%vspace%get_ndf()\n"
            "      undf_aspc1_f1 = f1_proxy%vspace%get_undf()\n"
            "      !\n"
            "      ! Set-up all of the loop bounds\n"
            "      !\n"
            "      loop0_start = 1\n"
            "      loop0_stop = undf_aspc1_f1\n"
            "      loop1_start = 1\n"
            "      loop1_stop = undf_aspc1_f1\n"
            "      loop2_start = 1\n"
            "      loop2_stop = undf_aspc1_f1\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = f1_data(df) ** i_scalar\n"
            "      END DO\n"
            "      !\n")

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        # Check the type of the scalar power
        scalar = loop.scope.symbol_table.lookup("i_scalar")
        assert isinstance(scalar.datatype, ScalarType)
        assert scalar.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f1_data(df) = f1_data(df) ** i_scalar\n"
                "enddo") in code
    else:
        output = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      loop1_start = 1\n"
            "      loop1_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      loop2_start = 1\n"
            "      loop2_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = f1_data(df) ** i_scalar\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()")
        if not annexed:
            output = output.replace("dof_annexed", "dof_owned")
        assert output in code

        assert "f1_data(df) = f1_data(df) ** (-2_i_def)\n" in code
        assert ("f1_data(df) = f1_data(df) ** my_var_a_scalar\n"
                in code)


# ------------- Setting real field elements to a real value ----------------- #


def test_setval_c(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicSetvalCKern' returns the
    expected string and 2) we generate correct code for the built-in
    operation 'X = c' where 'c' is a real constant scalar value and
    'X' is a real-valued field. Test with and without annexed DoFs
    being computed as this affects the generated code. 3) Also test
    the 'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicSetvalCKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.7.1_setval_c_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == ("Built-in: setval_c (set a real-valued field to "
                         "a real scalar value)")
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "    SUBROUTINE invoke_0(f1, c)\n"
            "      REAL(KIND=r_def), intent(in) :: c\n"
            "      TYPE(field_type), intent(in) :: f1\n"
            "      INTEGER df\n"
            "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f1_data => "
            "null()\n"
            "      TYPE(field_proxy_type) f1_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f1\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      f1_data => f1_proxy%data\n"
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
            "        f1_data(df) = c\n"
            "      END DO")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        # Check the type of the scalar
        scalar = loop.scope.symbol_table.lookup("c")
        assert isinstance(scalar.datatype, ScalarType)
        assert scalar.datatype.intrinsic == ScalarType.Intrinsic.REAL
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f1_data(df) = c\n"
                "enddo") in code
    else:
        output_dm_2 = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = c\n"
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


def test_setval_X(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicSetvalXKern' returns the
    expected string and 2) we generate correct code for the built-in
    operation 'Y = X' where 'X' and 'Y' are real-valued fields. Also
    test with and without annexed DoFs being computed as this affects
    the generated code. 3) Also test the 'lower_to_language_level()'
    and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicSetvalXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.7.2_setval_X_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == ("Built-in: setval_X (set a real-valued field "
                         "equal to another such field)")
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if not dist_mem:
        output = (
            "      f2_proxy = f2%get_proxy()\n"
            "      f2_data => f2_proxy%data\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      f1_data => f1_proxy%data\n"
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
            "        f2_data(df) = f1_data(df)\n"
            "      END DO")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f2_data(df) = f1_data(df)\n"
                "enddo") in code
    else:
        output_dm_2 = (
            "      loop0_stop = f2_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_data(df) = f1_data(df)\n"
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


# ------------- Sign of real field elements --------------------------------- #


def test_sign_X(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicSignXKern' returns the
    expected string and 2) we generate correct code for the built-in
    operation 'Y = sign(a, X)' where 'a' is a real scalar and 'Y' and
    'X' are real-valued fields. Test with and without annexed DoFs
    being computed as this affects the generated code. 3) Also test the
    'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicSignXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.10.1_sign_X_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: sign_X (sign of a real-valued field)"
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
            "        f2_data(df) = SIGN(a, f1_data(df))\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f2_data(df) = SIGN(a, f1_data(df))\n"
                "enddo") in code
    else:
        output_dm_2 = (
            "      loop0_stop = f2_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_data(df) = SIGN(a, f1_data(df))\n"
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


# ------------- Maximum of (real scalar, real field elements) --------------- #


def test_max_aX(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicMaxAXKern' returns the
    expected string and 2) we generate correct code for the built-in
    operation 'Y = max(a, X)' where 'a' is a real scalar and 'Y' and
    'X' are real-valued fields. Test with and without annexed DoFs
    being computed as this affects the generated code. 3) Also test
    the 'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicMaxAXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.10.4_max_aX_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: max_aX (real-valued fields)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Check for the correct field and scalar type declarations
    output = (
        "      REAL(KIND=r_solver), intent(in) :: a\n"
        "      TYPE(r_solver_field_type), intent(in) :: f2, f1\n"
        "      INTEGER df\n"
        "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
        "      REAL(KIND=r_solver), pointer, dimension(:) :: f1_data => "
        "null()\n"
        "      REAL(KIND=r_solver), pointer, dimension(:) :: f2_data => "
        "null()\n"
        "      TYPE(r_solver_field_proxy_type) f2_proxy, f1_proxy\n")
    assert output in code

    if not dist_mem:
        assert "INTEGER(KIND=i_def) undf_aspc1_f2\n" in code
        output = (
            "      loop0_stop = undf_aspc1_f2\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_data(df) = MAX(a, f1_data(df))\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f2_data(df) = MAX(a, f1_data(df))\n"
                "enddo") in code
    else:
        assert "INTEGER(KIND=i_def) max_halo_depth_mesh\n" in code
        output_dm_2 = (
            "      loop0_stop = f2_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_data(df) = MAX(a, f1_data(df))\n"
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


def test_inc_max_aX(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIncMaxAXKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'X = max(a, X)' where 'a' is a real scalar and
    'X' is a real-valued field. Test with and without annexed DoFs
    being computed as this affects the generated code. 3) Also test
    the 'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicIncMaxAXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.10.5_inc_max_aX_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: inc_max_aX (real-valued field)"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Check for the correct field and scalar type declarations
    output = (
        "      REAL(KIND=r_solver), intent(in) :: a\n"
        "      TYPE(r_solver_field_type), intent(in) :: f1\n"
        "      INTEGER df\n"
        "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
        "      REAL(KIND=r_solver), pointer, dimension(:) :: f1_data => "
        "null()\n"
        "      TYPE(r_solver_field_proxy_type) f1_proxy\n")
    assert output in code

    if not dist_mem:
        output = (
            "      loop0_stop = undf_aspc1_f1\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = MAX(a, f1_data(df))\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f1_data(df) = MAX(a, f1_data(df))\n"
                "enddo") in code
    else:
        output_dm_2 = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = MAX(a, f1_data(df))\n"
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


# ------------- Minimum of (real scalar, real field elements) --------------- #


def test_min_aX(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicMinAXKern' returns the
    expected string and 2) we generate correct code for the built-in
    operation 'Y = min(a, X)' where 'a' is a real scalar and 'Y' and
    'X' are real-valued fields. Test with and without annexed DoFs
    being computed as this affects the generated code. 3) Also test
    the 'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicMinAXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.10.6_min_aX_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: min_aX (real-valued fields)"
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
            "        f2_data(df) = MIN(a, f1_data(df))\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f2_data(df) = MIN(a, f1_data(df))\n"
                "enddo") in code
    else:
        output_dm_2 = (
            "      loop0_stop = f2_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_data(df) = MIN(a, f1_data(df))\n"
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


def test_inc_min_aX(tmpdir, monkeypatch, annexed, dist_mem, fortran_writer):
    '''Test that 1) the '__str__' method of 'LFRicIncMinAXKern' returns
    the expected string and 2) we generate correct code for the
    built-in operation 'X = min(a, X)' where 'a' is a real scalar and
    'X' is a real-valued field. Test with and without annexed DoFs
    being computed as this affects the generated code. 3) Also test
    the 'lower_to_language_level()' and 'metadata()' methods.

    '''
    metadata = lfric_builtins.LFRicIncMinAXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.10.7_inc_min_aX_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: inc_min_aX (real-valued field)"
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
            "        f1_data(df) = MIN(a, f1_data(df))\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code

        # Test the 'lower_to_language_level()' method
        kern.lower_to_language_level()
        loop = first_invoke.schedule.walk(Loop)[0]
        code = fortran_writer(loop)
        assert ("do df = loop0_start, loop0_stop, 1\n"
                "  f1_data(df) = MIN(a, f1_data(df))\n"
                "enddo") in code
    else:
        output_dm_2 = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = MIN(a, f1_data(df))\n"
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


# ------------- Xfail built-ins --------------------------------------------- #


@pytest.mark.xfail(
    reason="Requires kernel-argument dependency analysis to deduce the "
    "spaces of the fields passed to the built-in kernel")
def test_X_times_Y_on_different_spaces():
    '''Test that we raise an error if 'X_times_Y()' is called for two
    fields that are on different spaces.

    '''
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
    '''Test that we generate correct code if 'X_times_Y()' is called in an
    'invoke' containing another kernel that allows the space of the
    fields to be deduced.

    '''
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
    '''Tests that we generate correct code for a serial built-in
    'setval_c' operation with a scalar passed by value. Test with and
    without annexed DoFs being computed as this affects the generated
    code.

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
            "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f1_data => "
            "null()\n"
            "      TYPE(field_proxy_type) f1_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f1\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      f1_data => f1_proxy%data\n"
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
            "        f1_data(df) = 0.0\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output_seq in code

    if dist_mem:
        output_dm_2 = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = 0.0\n"
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
    '''Test that we generate correct code for the built-in operation 'Z =
    a*X + Y' when a scalar is passed by value. Also test with and
    without annexed DoFs being computed as this affects the generated
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
            "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f2_data => "
            "null()\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f1_data => "
            "null()\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f3_data => "
            "null()\n"
            "      TYPE(field_proxy_type) f3_proxy, f1_proxy, f2_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f3\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
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
            "        f3_data(df) = 0.5_r_def * f1_data(df) + f2_data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0")
        assert output in code
    if dist_mem:
        output_dm_2 = (
            "      loop0_stop = f3_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f3_data(df) = 0.5_r_def * f1_data(df) + "
            "f2_data(df)\n"
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
    '''Test that we generate correct code for the built-in operation 'Z =
    a*X + b*Y' when scalars 'a' and 'b' are passed by value. Test with
    and without annexed DoFs being computed as this affects the
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
            "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f2_data => "
            "null()\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f1_data => "
            "null()\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f3_data => "
            "null()\n"
            "      TYPE(field_proxy_type) f3_proxy, f1_proxy, f2_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f3\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
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
            "        f3_data(df) = 0.5d0 * f1_data(df) + 0.8 * f2_data(df)\n"
            "      END DO\n"
            "      !\n"
            "    END SUBROUTINE invoke_0\n")
        assert output in code
    if dist_mem:
        output_dm_2 = (
            "      loop0_stop = f3_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f3_data(df) = 0.5d0 * f1_data(df) + "
            "0.8 * f2_data(df)\n"
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
    '''Test that we generate correct code for the built-in operation 'Y =
    sign(a, X)' when a scalar is passed by value. Also test with and
    without annexed DoFs being computed as this affects the generated
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
            "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f1_data => "
            "null()\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f2_data => "
            "null()\n"
            "      TYPE(field_proxy_type) f2_proxy, f1_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f2\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      f2_data => f2_proxy%data\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      f1_data => f1_proxy%data\n"
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
            "        f2_data(df) = SIGN(-2.0_r_def, f1_data(df))\n"
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
            "        f2_data(df) = SIGN(-2.0_r_def, f1_data(df))\n"
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
    '''Tests that we generate correct code when we have an 'invoke'
    containing multiple set operations. Test with and without annexed
    DoFs being computed as this affects the generated code.

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
            "      INTEGER(KIND=i_def) loop2_start, loop2_stop\n"
            "      INTEGER(KIND=i_def) loop1_start, loop1_stop\n"
            "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f3_data => "
            "null()\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f2_data => "
            "null()\n"
            "      REAL(KIND=r_def), pointer, dimension(:) :: f1_data => "
            "null()\n"
            "      TYPE(field_proxy_type) f1_proxy, f2_proxy, f3_proxy\n"
            "      INTEGER(KIND=i_def) undf_aspc1_f1, "
            "undf_aspc1_f2, undf_aspc1_f3\n"
            "      !\n"
            "      ! Initialise field and/or operator proxies\n"
            "      !\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      f1_data => f1_proxy%data\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      f2_data => f2_proxy%data\n"
            "      f3_proxy = f3%get_proxy()\n"
            "      f3_data => f3_proxy%data\n"
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
            "      ! Set-up all of the loop bounds\n"
            "      !\n"
            "      loop0_start = 1\n"
            "      loop0_stop = undf_aspc1_f1\n"
            "      loop1_start = 1\n"
            "      loop1_stop = undf_aspc1_f2\n"
            "      loop2_start = 1\n"
            "      loop2_stop = undf_aspc1_f3\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = fred\n"
            "      END DO\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        f2_data(df) = 3.0_r_def\n"
            "      END DO\n"
            "      DO df=loop2_start,loop2_stop\n"
            "        f3_data(df) = ginger\n"
            "      END DO\n")
        assert output in code
    if dist_mem:
        output_dm_2 = (
            "      loop0_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      loop1_start = 1\n"
            "      loop1_stop = f2_proxy%vspace%get_last_dof_annexed()\n"
            "      loop2_start = 1\n"
            "      loop2_stop = f3_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = fred\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      !\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        f2_data(df) = 3.0_r_def\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f2_proxy%set_dirty()\n"
            "      !\n"
            "      DO df=loop2_start,loop2_stop\n"
            "        f3_data(df) = ginger\n"
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
    '''Tests that we generate correct code for a built-in set operation
    when the 'invoke' also contains a normal kernel. Test with and
    without annexed DoFs being computed as this affects the generated
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
            "      ! Set-up all of the loop bounds\n"
            "      !\n"
            "      loop0_start = 1\n"
            "      loop0_stop = f1_proxy%vspace%get_ncell()\n"
            "      loop1_start = 1\n"
            "      loop1_stop = undf_aspc1_f1\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO cell=loop0_start,loop0_stop\n"
            "        !\n"
            "        CALL testkern_code(nlayers, ginger, f1_data, "
            "f2_data, "
            "m1_data, m2_data, ndf_w1, undf_w1, "
            "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
            "undf_w3, map_w3(:,cell))\n"
            "      END DO\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        f1_data(df) = 0.0_r_def\n"
            "      END DO")
        assert output in code
    if dist_mem:
        mesh_code_present("f1", code)
        output_dm_2 = (
            "      loop0_stop = mesh%get_last_halo_cell(1)\n"
            "      loop1_start = 1\n"
            "      loop1_stop = f1_proxy%vspace%get_last_dof_annexed()\n"
            "      !\n"
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
            "      DO cell=loop0_start,loop0_stop\n"
            "        !\n"
            "        CALL testkern_code(nlayers, ginger, f1_data, "
            "f2_data, m1_data, m2_data, ndf_w1, "
            "undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), "
            "ndf_w3, undf_w3, map_w3(:,cell))\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      !\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        f1_data(df) = 0.0_r_def\n"
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


# ------------- Auxiliary mesh code generation function --------------------- #


def mesh_code_present(field_str, code):
    '''This test checks for the existance of mesh code. This exists for
    all built-ins with dm = True (although it is not actually
    required!) so each test can call this function. Mesh code is
    generated from the first field in a built-in arguments list, here
    denoted with field_str.

    '''
    assert "      USE mesh_mod, ONLY: mesh_type" in code
    assert "      TYPE(mesh_type), pointer :: mesh => null()" in code
    output_dm_1 = (
        "      !\n"
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh => " + field_str + "_proxy%vspace%get_mesh()\n"
        "      max_halo_depth_mesh = mesh%get_halo_depth()\n"
        "      !\n")
    assert output_dm_1 in code


def test_field_access_info_for_arrays_in_builtins():
    '''Tests that array of fields in LFRic built-ins properly report access
    information. For example, 'call invoke( a_plus_X(f2(i), a, f1) )'
    must report the access to 'f2' (which will be 'f2_data', the pointer to
    the data array associated with the field).

    '''
    _, invoke = get_invoke("15.1.8_a_plus_X_builtin_array_of_fields.f90",
                           api=API, idx=0, dist_mem=False)
    schedule = invoke.schedule
    vai = VariablesAccessInfo(schedule)

    assert Signature("f2_data") in vai

    assert ("a: READ, df: READ+WRITE, f1_data: READ, f2_data: WRITE, "
            "loop0_start: READ, loop0_stop: READ" == str(vai))
