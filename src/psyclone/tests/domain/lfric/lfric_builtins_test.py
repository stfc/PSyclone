# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
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
 '''

import re
import os
import pytest

from psyclone.core import Signature
from psyclone.domain.lfric import lfric_builtins, LFRicConstants
from psyclone.domain.lfric.kernel import (
    LFRicKernelMetadata, FieldArgMetadata, ScalarArgMetadata)
from psyclone.domain.lfric.lfric_builtins import (
    LFRicBuiltInCallFactory, LFRicBuiltIn)
from psyclone.lfric import LFRicKernelArgument
from psyclone.errors import GenerationError, InternalError
from psyclone.parse.algorithm import BuiltInCall, parse
from psyclone.parse.utils import ParseError
from psyclone.psyGen import PSyFactory
from psyclone.psyir.nodes import (
    ArrayReference, Loop, Reference, UnaryOperation, Literal, Routine)
from psyclone.psyir.symbols import (
    ArrayType, ScalarType, UnsupportedFortranType)
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.utilities import get_invoke

# The tests in this file use the Builtin capitalisations, which makes them
# non-conformant to test naming guidelines
# pylint: disable=invalid-name

# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "lfric")

# The PSyclone API under test
API = "lfric"


def builtin_from_file(filename):
    '''
    :param str filename: the name of the file to check for the builtin.
    :returns: the first builtin in the first invoke.
    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, filename), api=API)
    psy = PSyFactory(API).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    return first_invoke.schedule.children[0].loop_body[0]


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
    assert not instance.qr_rules
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
    assert ("built-in calls must operate on one of ['dof', 'owned_dof'] but "
            "found 'broken' for Built-in: setval_c (set a real-valued field "
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
        LFRicKernelArgument, "_init_data_type_properties",
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
        LFRicKernelArgument, "_init_data_type_properties",
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
        LFRicKernelArgument, "_init_data_type_properties",
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
        LFRicKernelArgument, "_init_data_type_properties",
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
    assert ("An LFRic built-in must iterate over one of ['dof', 'owned_dof'] "
            "but kernel 'x_plus_y' iterates over 'wrong'" in str(err.value))


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
    var_info = kern.reference_accesses()
    # f2_data(df) = a + f1_data(df)
    assert var_info.is_written(Signature("f2_data"))
    assert var_info.is_read(Signature("f1_data"))
    assert var_info.is_read(Signature("a"))
    # Check for the expected error if an unsupported type of argument
    # is encountered. Use monkeypatch to break one of the existing args.
    monkeypatch.setattr(kern.args[0], "_argument_type", "gh_wrong")
    with pytest.raises(InternalError) as err:
        var_info = kern.reference_accesses()
    assert ("LFRicBuiltin.reference_accesses only supports field and scalar "
            "arguments but got 'f2' of type 'gh_wrong'" in str(err.value))


# ------------- Adding (scaled) real fields --------------------------------- #


def test_X_plus_Y_and_its_int_version(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
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

    kern = builtin_from_file("15.1.1_X_plus_Y_builtin.f90")
    assert str(kern) == "Built-in: X_plus_Y (add real-valued fields)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: X_plus_Y (add real-valued fields)\n"
            "f3_data(df) = f1_data(df) + f2_data(df)\n") in code

    # The integer version has the datatype changed to integer in the metadata
    # and string representation
    kern = builtin_from_file("15.21.1_int_X_plus_Y_builtin.f90")
    assert str(kern) == "Built-in: int_X_plus_Y (add integer-valued fields)"
    assert kern.metadata().meta_args[0].datatype == "gh_integer"
    assert kern.metadata().meta_args[1].datatype == "gh_integer"
    assert kern.metadata().meta_args[2].datatype == "gh_integer"


def test_inc_X_plus_Y_and_its_int_version(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicIncXPlusYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 2
    assert metadata.meta_args[0].access == "gh_readwrite"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[1].function_space == "any_space_1"

    kern = builtin_from_file("15.1.2_inc_X_plus_Y_builtin.f90")
    assert (str(kern) == "Built-in: inc_X_plus_Y (increment a "
            "real-valued field)")

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: inc_X_plus_Y (increment a real-valued field)\n"
            "f1_data(df) = f1_data(df) + f2_data(df)\n" in code)

    # The integer version has the datatype changed to integer in the metadata
    # and string representation
    kern = builtin_from_file("15.21.2_int_inc_X_plus_Y_builtin.f90")
    assert str(kern) == ("Built-in: int_inc_X_plus_Y (increment an integer"
                         "-valued field)")
    assert kern.metadata().meta_args[0].datatype == "gh_integer"
    assert kern.metadata().meta_args[1].datatype == "gh_integer"


def test_a_plus_X_and_its_int_version(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicAPlusXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 3
    assert metadata.meta_args[0].access == "gh_write"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert isinstance(metadata.meta_args[1], ScalarArgMetadata)
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[2].access == "gh_read"
    assert metadata.meta_args[2].function_space == "any_space_1"

    kern = builtin_from_file("15.1.8_a_plus_X_builtin.f90")
    assert str(kern) == "Built-in: a_plus_X (real-valued fields)"

    # Test the 'lower_to_language_level()' method
    lowered = kern.lower_to_language_level()
    # Check the type of the scalar
    scalar = lowered.scope.symbol_table.lookup("a")
    assert isinstance(scalar.datatype, ScalarType)
    assert scalar.datatype.intrinsic == ScalarType.Intrinsic.REAL
    assert scalar.datatype.precision.name == "r_def"
    code = fortran_writer(lowered)
    assert ("! Built-in: a_plus_X (real-valued fields)\n"
            "f2_data(df) = a + f1_data(df)\n") in code

    # The integer version has the datatype changed to integer in the metadata
    # and string representation
    kern = builtin_from_file("15.21.3_int_a_plus_X_builtin.f90")
    assert str(kern) == "Built-in: int_a_plus_X (integer-valued fields)"
    assert kern.metadata().meta_args[0].datatype == "gh_integer"
    assert kern.metadata().meta_args[1].datatype == "gh_integer"
    assert kern.metadata().meta_args[2].datatype == "gh_integer"


def test_inc_a_plus_X_and_its_int_version(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicIncAPlusXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 2
    assert isinstance(metadata.meta_args[0], ScalarArgMetadata)
    assert metadata.meta_args[0].access == "gh_read"
    assert metadata.meta_args[1].access == "gh_readwrite"
    assert metadata.meta_args[1].function_space == "any_space_1"

    kern = builtin_from_file("15.1.9_inc_a_plus_X_builtin.f90")
    assert str(kern) == "Built-in: inc_a_plus_X (real-valued field)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: inc_a_plus_X (real-valued field)\n"
            "f1_data(df) = a + f1_data(df)\n") in code

    # The integer version has the datatype changed to integer in the metadata
    # and string representation
    kern = builtin_from_file("15.21.4_int_inc_a_plus_X_builtin.f90")
    assert str(kern) == "Built-in: int_inc_a_plus_X (integer-valued field)"
    assert kern.metadata().meta_args[0].datatype == "gh_integer"
    assert kern.metadata().meta_args[1].datatype == "gh_integer"


def test_aX_plus_Y(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicAXPlusYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 4
    assert metadata.meta_args[0].access == "gh_write"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert isinstance(metadata.meta_args[1], ScalarArgMetadata)
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[2].access == "gh_read"
    assert metadata.meta_args[2].function_space == "any_space_1"
    assert metadata.meta_args[3].access == "gh_read"
    assert metadata.meta_args[3].function_space == "any_space_1"

    kern = builtin_from_file("15.1.3_aX_plus_Y_builtin.f90")
    assert str(kern) == "Built-in: aX_plus_Y (real-valued fields)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: aX_plus_Y (real-valued fields)\n"
            "f3_data(df) = a * f1_data(df) + f2_data(df)\n") in code


def test_inc_aX_plus_Y(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicIncAXPlusYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 3
    assert isinstance(metadata.meta_args[0], ScalarArgMetadata)
    assert metadata.meta_args[0].access == "gh_read"
    assert metadata.meta_args[1].access == "gh_readwrite"
    assert metadata.meta_args[1].function_space == "any_space_1"
    assert metadata.meta_args[2].access == "gh_read"
    assert metadata.meta_args[2].function_space == "any_space_1"

    kern = builtin_from_file("15.1.4_inc_aX_plus_Y_builtin.f90")
    assert str(kern) == "Built-in: inc_aX_plus_Y (real-valued fields)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: inc_aX_plus_Y (real-valued fields)\n"
            "f1_data(df) = a * f1_data(df) + f2_data(df)\n") in code

    # Also with a literal
    kern = builtin_from_file("15.13.1_aX_plus_Y_builtin_set_by_value.f90")
    assert str(kern) == "Built-in: aX_plus_Y (real-valued fields)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: aX_plus_Y (real-valued fields)\n"
            "f3_data(df) = 0.5_r_def * f1_data(df) + f2_data(df)\n") in code


def test_inc_X_plus_bY(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicIncXPlusBYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 3
    assert metadata.meta_args[0].access == "gh_readwrite"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert isinstance(metadata.meta_args[1], ScalarArgMetadata)
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[2].access == "gh_read"
    assert metadata.meta_args[2].function_space == "any_space_1"

    kern = builtin_from_file("15.1.5_inc_X_plus_bY_builtin.f90")
    assert str(kern) == "Built-in: inc_X_plus_bY (real-valued fields)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: inc_X_plus_bY (real-valued fields)\n"
            "f1_data(df) = f1_data(df) + b * f2_data(df)\n") in code


def test_aX_plus_bY(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicAXPlusBYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 5
    assert metadata.meta_args[0].access == "gh_write"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert isinstance(metadata.meta_args[1], ScalarArgMetadata)
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[2].access == "gh_read"
    assert metadata.meta_args[2].function_space == "any_space_1"
    assert isinstance(metadata.meta_args[3], ScalarArgMetadata)
    assert metadata.meta_args[3].access == "gh_read"
    assert metadata.meta_args[4].access == "gh_read"
    assert metadata.meta_args[4].function_space == "any_space_1"

    kern = builtin_from_file("15.1.6_aX_plus_bY_builtin.f90")
    assert str(kern) == "Built-in: aX_plus_bY (real-valued fields)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: aX_plus_bY (real-valued fields)\n"
            "f3_data(df) = a * f1_data(df) + b * f2_data(df)\n") in code

    # Also with a literal
    kern = builtin_from_file("15.13.2_aX_plus_bY_builtin_set_by_value.f90")
    assert str(kern) == "Built-in: aX_plus_bY (real-valued fields)"

    code = fortran_writer(kern)
    assert ("! Built-in: aX_plus_bY (real-valued fields)\n"
            "f3_data(df) = 0.5d0 * f1_data(df) + 0.8 * f2_data(df)\n" in code)


def test_inc_aX_plus_bY(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicIncAXPlusBYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 4
    assert isinstance(metadata.meta_args[0], ScalarArgMetadata)
    assert metadata.meta_args[0].access == "gh_read"
    assert metadata.meta_args[1].access == "gh_readwrite"
    assert metadata.meta_args[1].function_space == "any_space_1"
    assert isinstance(metadata.meta_args[2], ScalarArgMetadata)
    assert metadata.meta_args[2].access == "gh_read"
    assert metadata.meta_args[3].access == "gh_read"
    assert metadata.meta_args[3].function_space == "any_space_1"

    kern = builtin_from_file("15.1.7_inc_aX_plus_bY_builtin.f90")
    assert str(kern) == "Built-in: inc_aX_plus_bY (real-valued fields)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: inc_aX_plus_bY (real-valued fields)\n"
            "f1_data(df) = a * f1_data(df) + b * f2_data(df)\n") in code


def test_aX_plus_aY(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicAXPlusAYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 4
    assert metadata.meta_args[0].access == "gh_write"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert isinstance(metadata.meta_args[1], ScalarArgMetadata)
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[2].access == "gh_read"
    assert metadata.meta_args[2].function_space == "any_space_1"
    assert metadata.meta_args[3].access == "gh_read"
    assert metadata.meta_args[3].function_space == "any_space_1"

    kern = builtin_from_file("15.1.10_aX_plus_aY_builtin.f90")
    assert str(kern) == "Built-in: aX_plus_aY (real-valued fields)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: aX_plus_aY (real-valued fields)\n"
            "f3_data(df) = a * (f1_data(df) + f2_data(df))\n") in code


# ------------- Subtracting (scaled) real fields ---------------------------- #


def test_X_minus_Y_and_its_int_version(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicXMinusYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 3
    assert metadata.meta_args[0].access == "gh_write"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[1].function_space == "any_space_1"
    assert metadata.meta_args[2].access == "gh_read"
    assert metadata.meta_args[2].function_space == "any_space_1"

    kern = builtin_from_file("15.2.1_X_minus_Y_builtin.f90")
    assert str(kern) == "Built-in: X_minus_Y (subtract real-valued fields)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: X_minus_Y (subtract real-valued fields)\n"
            "f3_data(df) = f1_data(df) - f2_data(df)\n") in code

    # The integer version has the datatype changed to integer in the metadata
    # and string representation
    kern = builtin_from_file("15.22.1_int_X_minus_Y_builtin.f90")
    assert str(kern) == ("Built-in: int_X_minus_Y (subtract integer-valued "
                         "fields)")
    assert kern.metadata().meta_args[0].datatype == "gh_integer"
    assert kern.metadata().meta_args[1].datatype == "gh_integer"
    assert kern.metadata().meta_args[2].datatype == "gh_integer"


def test_inc_X_minus_Y_and_its_int_version(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicIncXMinusYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 2
    assert metadata.meta_args[0].access == "gh_readwrite"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[1].function_space == "any_space_1"

    kern = builtin_from_file("15.2.2_inc_X_minus_Y_builtin.f90")
    assert (str(kern) == "Built-in: inc_X_minus_Y (decrement a "
            "real-valued field)")

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: inc_X_minus_Y (decrement a real-valued field)\n"
            "f1_data(df) = f1_data(df) - f2_data(df)\n") in code

    # The integer version has the datatype changed to integer in the metadata
    # and string representation
    kern = builtin_from_file("15.22.2_int_inc_X_minus_Y_builtin.f90")
    assert str(kern) == ("Built-in: int_inc_X_minus_Y (decrement an integer-"
                         "valued field)")
    assert kern.metadata().meta_args[0].datatype == "gh_integer"
    assert kern.metadata().meta_args[1].datatype == "gh_integer"


def test_a_minus_X_and_its_int_version(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicAMinusXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 3
    assert metadata.meta_args[0].access == "gh_write"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert isinstance(metadata.meta_args[1], ScalarArgMetadata)
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[2].access == "gh_read"
    assert metadata.meta_args[2].function_space == "any_space_1"

    kern = builtin_from_file("15.2.7_a_minus_X_builtin.f90")
    assert str(kern) == "Built-in: a_minus_X (real-valued fields)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: a_minus_X (real-valued fields)\n"
            "f2_data(df) = a - f1_data(df)\n") in code

    # The integer version has the datatype changed to integer in the metadata
    # and string representation
    kern = builtin_from_file("15.22.3_int_a_minus_X_builtin.f90")
    assert str(kern) == "Built-in: int_a_minus_X (integer-valued fields)"
    assert kern.metadata().meta_args[0].datatype == "gh_integer"
    assert kern.metadata().meta_args[1].datatype == "gh_integer"
    assert kern.metadata().meta_args[2].datatype == "gh_integer"


def test_inc_a_minus_X_and_its_int_version(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicIncAMinusXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 2
    assert isinstance(metadata.meta_args[0], ScalarArgMetadata)
    assert metadata.meta_args[0].access == "gh_read"
    assert metadata.meta_args[1].access == "gh_readwrite"
    assert metadata.meta_args[1].function_space == "any_space_1"

    kern = builtin_from_file("15.2.8_inc_a_minus_X_builtin.f90")
    assert str(kern) == "Built-in: inc_a_minus_X (real-valued field)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: inc_a_minus_X (real-valued field)\n"
            "f1_data(df) = a - f1_data(df)\n") in code

    # The integer version has the datatype changed to integer in the metadata
    # and string representation
    kern = builtin_from_file("15.22.4_int_inc_a_minus_X_builtin.f90")
    assert str(kern) == "Built-in: int_inc_a_minus_X (integer-valued field)"
    assert kern.metadata().meta_args[0].datatype == "gh_integer"
    assert kern.metadata().meta_args[1].datatype == "gh_integer"


def test_X_minus_a_and_its_int_version(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicXMinusAKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 3
    assert metadata.meta_args[0].access == "gh_write"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[1].function_space == "any_space_1"
    assert isinstance(metadata.meta_args[2], ScalarArgMetadata)
    assert metadata.meta_args[2].access == "gh_read"

    kern = builtin_from_file("15.2.9_X_minus_a_builtin.f90")
    assert str(kern) == "Built-in: X_minus_a (real-valued fields)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: X_minus_a (real-valued fields)\n"
            "f2_data(df) = f1_data(df) - a\n") in code

    # The integer version has the datatype changed to integer in the metadata
    # and string representation
    kern = builtin_from_file("15.22.5_int_X_minus_a_builtin.f90")
    assert str(kern) == "Built-in: int_X_minus_a (integer-valued fields)"
    assert kern.metadata().meta_args[0].datatype == "gh_integer"
    assert kern.metadata().meta_args[1].datatype == "gh_integer"
    assert kern.metadata().meta_args[2].datatype == "gh_integer"


def test_inc_X_minus_a_and_its_int_version(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicIncXMinusAKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 2
    assert metadata.meta_args[0].access == "gh_readwrite"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert isinstance(metadata.meta_args[1], ScalarArgMetadata)
    assert metadata.meta_args[1].access == "gh_read"

    kern = builtin_from_file("15.2.10_inc_X_minus_a_builtin.f90")
    assert str(kern) == "Built-in: inc_X_minus_a (real-valued field)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: inc_X_minus_a (real-valued field)\n"
            "f1_data(df) = f1_data(df) - a\n") in code

    # The integer version has the datatype changed to integer in the metadata
    # and string representation
    kern = builtin_from_file("15.22.6_int_inc_X_minus_a_builtin.f90")
    assert str(kern) == "Built-in: int_inc_X_minus_a (integer-valued field)"
    assert kern.metadata().meta_args[0].datatype == "gh_integer"
    assert kern.metadata().meta_args[1].datatype == "gh_integer"


def test_aX_minus_Y(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicAXMinusYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 4
    assert metadata.meta_args[0].access == "gh_write"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert isinstance(metadata.meta_args[1], ScalarArgMetadata)
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[2].access == "gh_read"
    assert metadata.meta_args[2].function_space == "any_space_1"
    assert metadata.meta_args[3].access == "gh_read"
    assert metadata.meta_args[3].function_space == "any_space_1"

    kern = builtin_from_file("15.2.3_aX_minus_Y_builtin.f90")
    assert str(kern) == "Built-in: aX_minus_Y (real-valued fields)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: aX_minus_Y (real-valued fields)\n"
            "f3_data(df) = a * f1_data(df) - f2_data(df)\n") in code


def test_X_minus_bY(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicXMinusBYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 4
    assert metadata.meta_args[0].access == "gh_write"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[1].function_space == "any_space_1"
    assert isinstance(metadata.meta_args[2], ScalarArgMetadata)
    assert metadata.meta_args[2].access == "gh_read"
    assert metadata.meta_args[3].access == "gh_read"
    assert metadata.meta_args[3].function_space == "any_space_1"

    kern = builtin_from_file("15.2.4_X_minus_bY_builtin.f90")
    assert str(kern) == "Built-in: X_minus_bY (real-valued fields)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: X_minus_bY (real-valued fields)\n"
            "f3_data(df) = f1_data(df) - b * f2_data(df)\n") in code


def test_inc_X_minus_bY(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicIncXMinusBYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 3
    assert metadata.meta_args[0].access == "gh_readwrite"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert isinstance(metadata.meta_args[1], ScalarArgMetadata)
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[2].access == "gh_read"
    assert metadata.meta_args[2].function_space == "any_space_1"

    kern = builtin_from_file("15.2.5_inc_X_minus_bY_builtin.f90")
    assert str(kern) == "Built-in: inc_X_minus_bY (real-valued fields)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: inc_X_minus_bY (real-valued fields)\n"
            "f1_data(df) = f1_data(df) - b * f2_data(df)\n") in code


def test_aX_minus_bY(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicAXMinusBYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 5
    assert metadata.meta_args[0].access == "gh_write"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert isinstance(metadata.meta_args[1], ScalarArgMetadata)
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[2].access == "gh_read"
    assert metadata.meta_args[2].function_space == "any_space_1"
    assert isinstance(metadata.meta_args[3], ScalarArgMetadata)
    assert metadata.meta_args[3].access == "gh_read"
    assert metadata.meta_args[4].access == "gh_read"
    assert metadata.meta_args[4].function_space == "any_space_1"

    kern = builtin_from_file("15.2.6_aX_minus_bY_builtin.f90")
    assert str(kern) == "Built-in: aX_minus_bY (real-valued fields)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: aX_minus_bY (real-valued fields)\n"
            "f3_data(df) = a * f1_data(df) - b * f2_data(df)\n") in code


# ------------- Multiplying (scaled) real fields ---------------------------- #


def test_X_times_Y_and_its_int_version(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicXTimesYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 3
    assert metadata.meta_args[0].access == "gh_write"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[1].function_space == "any_space_1"
    assert metadata.meta_args[2].access == "gh_read"
    assert metadata.meta_args[2].function_space == "any_space_1"

    kern = builtin_from_file("15.3.1_X_times_Y_builtin.f90")
    assert str(kern) == "Built-in: X_times_Y (multiply real-valued fields)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: X_times_Y (multiply real-valued fields)\n"
            "f3_data(df) = f1_data(df) * f2_data(df)\n") in code

    # The integer version has the datatype changed to integer in the metadata
    # and string representation
    kern = builtin_from_file("15.23.1_int_X_times_Y_builtin.f90")
    assert str(kern) == ("Built-in: int_X_times_Y (multiply integer-valued "
                         "fields)")
    assert kern.metadata().meta_args[0].datatype == "gh_integer"
    assert kern.metadata().meta_args[1].datatype == "gh_integer"
    assert kern.metadata().meta_args[2].datatype == "gh_integer"


def test_inc_X_times_Y_and_its_int_version(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicIncXTimesYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 2
    assert metadata.meta_args[0].access == "gh_readwrite"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[1].function_space == "any_space_1"

    kern = builtin_from_file("15.3.2_inc_X_times_Y_builtin.f90")
    assert (str(kern) == "Built-in: inc_X_times_Y (multiply one real-valued "
            "field by another)")

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: inc_X_times_Y (multiply one real-valued "
            "field by another)\n"
            "f1_data(df) = f1_data(df) * f2_data(df)\n" in code)

    # The integer version has the datatype changed to integer in the metadata
    # and string representation
    kern = builtin_from_file("15.23.2_int_inc_X_times_Y_builtin.f90")
    assert str(kern) == ("Built-in: int_inc_X_times_Y (multiply one integer"
                         "-valued field by another)")
    assert kern.metadata().meta_args[0].datatype == "gh_integer"
    assert kern.metadata().meta_args[1].datatype == "gh_integer"


def test_inc_aX_times_Y(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicIncAXTimesYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 3
    assert metadata.meta_args[0].access == "gh_read"
    assert metadata.meta_args[1].access == "gh_readwrite"
    assert metadata.meta_args[1].function_space == "any_space_1"
    assert metadata.meta_args[2].access == "gh_read"
    assert metadata.meta_args[2].function_space == "any_space_1"

    kern = builtin_from_file("15.3.3_inc_aX_times_Y_builtin.f90")
    assert str(kern) == "Built-in: inc_aX_times_Y (real-valued fields)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: inc_aX_times_Y (real-valued fields)\n"
            "f1_data(df) = a * f1_data(df) * f2_data(df)\n") in code


# ------------- Scaling real fields (multiplying by a real scalar) ---------- #


def test_a_times_X_and_its_int_version(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicATimesXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 3
    assert metadata.meta_args[0].access == "gh_write"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert isinstance(metadata.meta_args[1], ScalarArgMetadata)
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[2].access == "gh_read"
    assert metadata.meta_args[2].function_space == "any_space_1"

    kern = builtin_from_file("15.4.1_a_times_X_builtin.f90")
    assert str(kern) == "Built-in: a_times_X (copy a scaled real-valued field)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: a_times_X (copy a scaled real-valued field)\n"
            "f2_data(df) = a_scalar * f1_data(df)\n") in code

    # The integer version has the datatype changed to integer in the metadata
    # and string representation
    kern = builtin_from_file("15.24.1_int_a_times_X_builtin.f90")
    assert str(kern) == ("Built-in: int_a_times_X (copy a scaled integer"
                         "-valued field)")
    assert kern.metadata().meta_args[0].datatype == "gh_integer"
    assert kern.metadata().meta_args[1].datatype == "gh_integer"
    assert kern.metadata().meta_args[2].datatype == "gh_integer"


def test_inc_a_times_X_and_its_int_version(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicIncATimesXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 2
    assert isinstance(metadata.meta_args[1], ScalarArgMetadata)
    assert metadata.meta_args[0].access == "gh_read"
    assert metadata.meta_args[1].access == "gh_readwrite"
    assert metadata.meta_args[1].function_space == "any_space_1"

    kern = builtin_from_file("15.4.2_inc_a_times_X_builtin.f90")
    assert str(kern) == "Built-in: inc_a_times_X (scale a real-valued field)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: inc_a_times_X (scale a real-valued field)\n"
            "f1_data(df) = a_scalar * f1_data(df)\n") in code

    # The integer version has the datatype changed to integer in the metadata
    # and string representation
    kern = builtin_from_file("15.24.2_int_inc_a_times_X_builtin.f90")
    assert str(kern) == ("Built-in: int_inc_a_times_X (scale an integer"
                         "-valued field)")
    assert kern.metadata().meta_args[0].datatype == "gh_integer"
    assert kern.metadata().meta_args[1].datatype == "gh_integer"

# ------------- Dividing real fields ---------------------------------------- #


def test_X_divideby_Y(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicXDividebyYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 3
    assert metadata.meta_args[0].access == "gh_write"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[1].function_space == "any_space_1"
    assert metadata.meta_args[2].access == "gh_read"
    assert metadata.meta_args[2].function_space == "any_space_1"

    kern = builtin_from_file("15.5.1_X_divideby_Y_builtin.f90")
    assert str(kern) == "Built-in: X_divideby_Y (divide real-valued fields)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: X_divideby_Y (divide real-valued fields)\n"
            "f3_data(df) = f1_data(df) / f2_data(df)\n") in code


def test_inc_X_divideby_Y(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicIncXDividebyYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 2
    assert metadata.meta_args[0].access == "gh_readwrite"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[1].function_space == "any_space_1"

    kern = builtin_from_file("15.5.2_inc_X_divideby_Y_builtin.f90")
    assert (str(kern) == "Built-in: inc_X_divideby_Y (divide one real-valued "
            "field by another)")

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: inc_X_divideby_Y (divide one real-valued "
            "field by another)\n"
            "f1_data(df) = f1_data(df) / f2_data(df)\n" in code)


def test_X_divideby_a(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicXDividebyAKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 3
    assert metadata.meta_args[0].access == "gh_write"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[1].function_space == "any_space_1"
    assert isinstance(metadata.meta_args[2], ScalarArgMetadata)
    assert metadata.meta_args[2].access == "gh_read"

    kern = builtin_from_file("15.5.5_X_divideby_a_builtin.f90")
    assert str(kern) == ("Built-in: X_divideby_a (divide a real-valued field "
                         "by a real scalar (Y = X/a))")

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: X_divideby_a (divide a real-valued field "
            "by a real scalar (Y = X/a))\n"
            "f2_data(df) = f1_data(df) / a_scalar\n") in code


def test_inc_X_divideby_a(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicIncXDividebyAKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 2
    assert metadata.meta_args[0].access == "gh_readwrite"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert isinstance(metadata.meta_args[1], ScalarArgMetadata)
    assert metadata.meta_args[1].access == "gh_read"

    kern = builtin_from_file("15.5.6_inc_X_divideby_a_builtin.f90")
    assert str(kern) == ("Built-in: inc_X_divideby_a (divide a real-valued "
                         "field by a real scalar (X = X/a))")

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: inc_X_divideby_a (divide a real-valued "
            "field by a real scalar (X = X/a))\n"
            "f1_data(df) = f1_data(df) / a_scalar\n") in code


# ------------- Inverse scaling of real fields ------------------------------ #


def test_a_divideby_X(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicADividebyXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 3
    assert metadata.meta_args[0].access == "gh_write"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert isinstance(metadata.meta_args[1], ScalarArgMetadata)
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[2].access == "gh_read"
    assert metadata.meta_args[2].function_space == "any_space_1"

    kern = builtin_from_file("15.5.3_a_divideby_X_builtin.f90")
    assert (str(kern) == "Built-in: a_divideby_X (inverse scaling of a "
            "real-valued field (Y = a/X))")

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: a_divideby_X (inverse scaling of a "
            "real-valued field (Y = a/X))\n"
            "f2_data(df) = a_scalar / f1_data(df)\n") in code


def test_inc_a_divideby_X(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicIncADividebyXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 2
    assert isinstance(metadata.meta_args[0], ScalarArgMetadata)
    assert metadata.meta_args[0].access == "gh_read"
    assert metadata.meta_args[1].access == "gh_readwrite"
    assert metadata.meta_args[1].function_space == "any_space_1"

    kern = builtin_from_file("15.5.4_inc_a_divideby_X_builtin.f90")
    assert (str(kern) == "Built-in: inc_a_divideby_X (inverse scaling of a "
            "real-valued field (X = a/X))")

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: inc_a_divideby_X (inverse scaling of a "
            "real-valued field (X = a/X))\n"
            "f1_data(df) = a_scalar / f1_data(df)\n") in code


# ------------- Raising a real field to a scalar ---------------------------- #


def test_inc_X_powreal_a(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicIncXPowrealAKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 2
    assert metadata.meta_args[0].access == "gh_readwrite"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[1].datatype == "gh_real"

    kern = builtin_from_file("15.6.1_inc_X_powreal_a_builtin.f90")
    assert (str(kern) == "Built-in: inc_X_powreal_a (raise a real-valued "
            "field to a real power)")

    # Test the 'lower_to_language_level()' method
    lowered = kern.lower_to_language_level()
    # Check the type of the scalar power
    scalar = lowered.scope.symbol_table.lookup("a_scalar")
    assert isinstance(scalar.datatype, ScalarType)
    assert scalar.datatype.intrinsic == ScalarType.Intrinsic.REAL
    assert scalar.datatype.precision.name == "r_def"
    code = fortran_writer(lowered)
    assert ("! Built-in: inc_X_powreal_a (raise a real-valued "
            "field to a real power)\n"
            "f1_data(df) = f1_data(df) ** a_scalar\n") in code


def test_inc_X_powint_n(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicIncXPowintNKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 2
    assert metadata.meta_args[0].access == "gh_readwrite"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[1].datatype == "gh_integer"

    kern = builtin_from_file("15.6.2_inc_X_powint_n_builtin.f90")
    assert str(kern) == ("Built-in: inc_X_powint_n (raise a real-valued field "
                         "to an integer power)")

    # Test the 'lower_to_language_level()' method
    lowered = kern.lower_to_language_level()
    # Check the type of the scalar power
    scalar = lowered.scope.symbol_table.lookup("i_scalar")
    assert isinstance(scalar.datatype, ScalarType)
    assert scalar.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert scalar.datatype.precision.name == "i_def"
    code = fortran_writer(lowered)
    assert ("! Built-in: inc_X_powint_n (raise a real-valued field "
            "to an integer power)\n"
            "f1_data(df) = f1_data(df) ** i_scalar\n") in code


# ------------- Setting real field elements to a real value ----------------- #


def test_setval_c_and_its_int_version(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicSetvalCKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 2
    assert metadata.meta_args[0].access == "gh_write"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert isinstance(metadata.meta_args[1], ScalarArgMetadata)
    assert metadata.meta_args[1].access == "gh_read"

    kern = builtin_from_file("15.7.1_setval_c_builtin.f90")
    assert str(kern) == ("Built-in: setval_c (set a real-valued field to "
                         "a real scalar value)")

    # Test the 'lower_to_language_level()' method
    lowered = kern.lower_to_language_level()
    # Check the type of the scalar
    scalar = lowered.scope.symbol_table.lookup("c")
    assert isinstance(scalar.datatype, ScalarType)
    assert scalar.datatype.intrinsic == ScalarType.Intrinsic.REAL
    assert scalar.datatype.precision.name == "r_def"
    code = fortran_writer(lowered)
    assert ("! Built-in: setval_c (set a real-valued field to "
            "a real scalar value)\n"
            "f1_data(df) = c\n") in code

    # The integer version has the datatype changed to integer in the metadata
    # and string representation
    kern = builtin_from_file("15.27.1_int_setval_c_builtin.f90")
    assert str(kern) == ("Built-in: int_setval_c (set an integer"
                         "-valued field to a integer scalar value)")
    assert kern.metadata().meta_args[0].datatype == "gh_integer"
    assert kern.metadata().meta_args[1].datatype == "gh_integer"


def test_setval_X_and_its_int_version(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicSetvalXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 2
    assert metadata.meta_args[0].access == "gh_write"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[1].function_space == "any_space_1"

    kern = builtin_from_file("15.7.2_setval_X_builtin.f90")
    assert str(kern) == ("Built-in: setval_X (set a real-valued field "
                         "equal to another such field)")

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: setval_X (set a real-valued field "
            "equal to another such field)\n"
            "f2_data(df) = f1_data(df)\n") in code

    # The integer version has the datatype changed to integer in the metadata
    # and string representation
    kern = builtin_from_file("15.27.2_int_setval_X_builtin.f90")
    assert str(kern) == ("Built-in: int_setval_X (set an integer"
                         "-valued field equal to another such field)")
    assert kern.metadata().meta_args[0].datatype == "gh_integer"
    assert kern.metadata().meta_args[1].datatype == "gh_integer"


def test_setval_random(fortran_writer, annexed):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicSetvalRandomKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 1
    assert metadata.meta_args[0].access == "gh_write"
    assert metadata.meta_args[0].function_space == "any_space_1"

    if annexed:
        # This kernel cannot perform redundant computation and therefore
        # cannot be used if compute_annexed_dofs is True.
        with pytest.raises(GenerationError) as err:
            _ = builtin_from_file("15.7.4_setval_random_builtin.f90")
        assert ("'setval_random' cannot perform redundant computation (has "
                "OPERATES_ON=owned_dof) but the 'COMPUTE_ANNEXED_DOFS'"
                in str(err.value))
        return
    else:
        kern = builtin_from_file("15.7.4_setval_random_builtin.f90")
    assert str(kern) == ("Built-in: setval_random (fill a real-valued field"
                         " with pseudo-random numbers)")

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: setval_random (fill a real-valued field "
            "with pseudo-random numbers)\n"
            "call RANDOM_NUMBER(f1_data(df))\n") in code


# ------------- Sign of real field elements --------------------------------- #


def test_sign_X_and_its_int_version(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicSignXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 3
    assert metadata.meta_args[0].access == "gh_write"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert isinstance(metadata.meta_args[1], ScalarArgMetadata)
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[2].access == "gh_read"
    assert metadata.meta_args[2].function_space == "any_space_1"

    kern = builtin_from_file("15.10.1_sign_X_builtin.f90")
    assert str(kern) == ("Built-in: sign_X (sign of a real-valued field, "
                         "applied to a scalar argument)")

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: sign_X (sign of a real-valued field, applied to a "
            "scalar argument)\n"
            "f2_data(df) = SIGN(a, f1_data(df))\n") in code

    # Also with a literal
    kern = builtin_from_file("15.10.2_sign_X_builtin_set_by_value.f90")
    assert str(kern) == ("Built-in: sign_X (sign of a real-valued field, "
                         "applied to a scalar argument)")

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: sign_X (sign of a real-valued field, applied to a "
            "scalar argument)\n"
            "f2_data(df) = SIGN(-2.0_r_def, f1_data(df))\n" in code)

    # The integer version has the datatype changed to integer in the metadata
    # and string representation
    kern = builtin_from_file("15.28.1_int_sign_X_builtin.f90")
    assert str(kern) == ("Built-in: int_sign_X (sign of an integer"
                         "-valued field, applied to a scalar argument)")
    assert kern.metadata().meta_args[0].datatype == "gh_integer"
    assert kern.metadata().meta_args[1].datatype == "gh_integer"
    assert kern.metadata().meta_args[2].datatype == "gh_integer"

# ------------- Maximum of (real scalar, real field elements) --------------- #


def test_max_aX_and_its_int_version(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicMaxAXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 3
    assert metadata.meta_args[0].access == "gh_write"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert isinstance(metadata.meta_args[1], ScalarArgMetadata)
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[2].access == "gh_read"
    assert metadata.meta_args[2].function_space == "any_space_1"

    kern = builtin_from_file("15.10.4_max_aX_builtin.f90")
    assert str(kern) == "Built-in: max_aX (real-valued fields)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: max_aX (real-valued fields)\n"
            "f2_data(df) = MAX(a, f1_data(df))\n") in code

    # The integer version has the datatype changed to integer in the metadata
    # and string representation
    kern = builtin_from_file("15.28.3_int_max_aX_builtin.f90")
    assert str(kern) == ("Built-in: int_max_aX (integer-valued fields)")
    assert kern.metadata().meta_args[0].datatype == "gh_integer"
    assert kern.metadata().meta_args[1].datatype == "gh_integer"
    assert kern.metadata().meta_args[2].datatype == "gh_integer"


def test_inc_max_aX_and_its_int_version(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicIncMaxAXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 2
    assert isinstance(metadata.meta_args[0], ScalarArgMetadata)
    assert metadata.meta_args[0].access == "gh_read"
    assert metadata.meta_args[1].access == "gh_readwrite"
    assert metadata.meta_args[1].function_space == "any_space_1"

    kern = builtin_from_file("15.10.5_inc_max_aX_builtin.f90")
    assert str(kern) == "Built-in: inc_max_aX (real-valued field)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: inc_max_aX (real-valued field)\n"
            "f1_data(df) = MAX(a, f1_data(df))\n") in code

    # The integer version has the datatype changed to integer in the metadata
    # and string representation
    kern = builtin_from_file("15.28.4_int_inc_max_aX_builtin.f90")
    assert str(kern) == ("Built-in: int_inc_max_aX (integer-valued field)")
    assert kern.metadata().meta_args[0].datatype == "gh_integer"
    assert kern.metadata().meta_args[1].datatype == "gh_integer"

# ------------- Minimum of (real scalar, real field elements) --------------- #


def test_min_aX_and_its_int_version(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicMinAXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 3
    assert metadata.meta_args[0].access == "gh_write"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert isinstance(metadata.meta_args[1], ScalarArgMetadata)
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[2].access == "gh_read"
    assert metadata.meta_args[2].function_space == "any_space_1"

    kern = builtin_from_file("15.10.6_min_aX_builtin.f90")
    assert str(kern) == "Built-in: min_aX (real-valued fields)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: min_aX (real-valued fields)\n"
            "f2_data(df) = MIN(a, f1_data(df))\n") in code

    # The integer version has the datatype changed to integer in the metadata
    # and string representation
    kern = builtin_from_file("15.28.5_int_min_aX_builtin.f90")
    assert str(kern) == ("Built-in: int_min_aX (integer-valued fields)")
    assert kern.metadata().meta_args[0].datatype == "gh_integer"
    assert kern.metadata().meta_args[1].datatype == "gh_integer"
    assert kern.metadata().meta_args[2].datatype == "gh_integer"


def test_inc_min_aX_and_its_int_version(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicIncMinAXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 2
    assert isinstance(metadata.meta_args[0], ScalarArgMetadata)
    assert metadata.meta_args[0].access == "gh_read"
    assert metadata.meta_args[1].access == "gh_readwrite"
    assert metadata.meta_args[1].function_space == "any_space_1"

    kern = builtin_from_file("15.10.7_inc_min_aX_builtin.f90")
    assert str(kern) == "Built-in: inc_min_aX (real-valued field)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: inc_min_aX (real-valued field)\n"
            "f1_data(df) = MIN(a, f1_data(df))\n") in code

    # The integer version has the datatype changed to integer in the metadata
    # and string representation
    kern = builtin_from_file("15.28.6_int_inc_min_aX_builtin.f90")
    assert str(kern) == ("Built-in: int_inc_min_aX (integer-valued field)")
    assert kern.metadata().meta_args[0].datatype == "gh_integer"
    assert kern.metadata().meta_args[1].datatype == "gh_integer"

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


# ------------- Other builtins   --------- #


def test_sum_x(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicSumXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 2
    assert isinstance(metadata.meta_args[0], ScalarArgMetadata)
    assert metadata.meta_args[0].access == "gh_sum"
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[1].function_space == "any_space_1"

    kern = builtin_from_file("15.8.1_sum_X_builtin.f90")
    assert str(kern) == "Built-in: sum_X (sum a real-valued field)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: sum_X (sum a real-valued field)\n"
            "asum = asum + f1_data(df)\n") in code


def test_x_innerproduct_x(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicXInnerproductXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 2
    assert isinstance(metadata.meta_args[0], ScalarArgMetadata)
    assert metadata.meta_args[0].access == "gh_sum"
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[1].function_space == "any_space_1"

    kern = builtin_from_file("15.9.2_X_innerproduct_X_builtin.f90")
    assert str(kern) == "Built-in: X_innerproduct_X (real-valued field)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: X_innerproduct_X (real-valued field)\n"
            "asum = asum + f1_data(df) * f1_data(df)\n") in code


def test_x_innerproduct_y(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicXInnerproductYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 3
    assert isinstance(metadata.meta_args[0], ScalarArgMetadata)
    assert metadata.meta_args[0].access == "gh_sum"
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[1].function_space == "any_space_1"
    assert metadata.meta_args[2].access == "gh_read"
    assert metadata.meta_args[2].function_space == "any_space_1"

    kern = builtin_from_file("15.9.1_X_innerproduct_Y_builtin.f90")
    assert str(kern) == "Built-in: X_innerproduct_Y (real-valued fields)"

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert ("! Built-in: X_innerproduct_Y (real-valued fields)\n"
            "asum = asum + f1_data(df) * f2_data(df)\n") in code


def test_int_to_real_x(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicIntToRealXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 2
    assert metadata.meta_args[0].access == "gh_write"
    assert metadata.meta_args[0].datatype == "gh_real"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[1].datatype == "gh_integer"
    assert metadata.meta_args[1].function_space == "any_space_1"

    kern = builtin_from_file("15.28.2_int_to_real_X_builtin.f90")
    assert str(kern) == ("Built-in: int_to_real_X (convert an integer-valued "
                         "to a real-valued field)")

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert (
        "! Built-in: int_to_real_X (convert an integer-valued to a "
        "real-valued field)\n"
        "f2_data(df) = REAL(f1_data(df), kind=r_def)\n") in code


@pytest.mark.parametrize("kind_name", ["r_solver", "r_tran", "r_bl"])
def test_int_to_real_x_precision(tmpdir, kind_name):
    '''
    Test that the built-in picks up and creates correct code for field
    data with precision that is not the default, i.e. not 'r_def'. Try with
    all supported LFRic real-valued field precisions to make sure all work.

    '''
    # Note that monkeypatching required to change field type, field proxy
    # type and the field data precisions is extensive and complicated.
    # Modifying the test algorithm is easier and more effective.
    with open(os.path.join(BASE_PATH, "15.28.2_int_to_real_X_builtin.f90"),
              "r", encoding='utf-8') as alg_file:
        alg_code = alg_file.read()

    # Modify the 'real'-valued field type and precision, and store the
    # modified temporary algorithm
    pattern = re.compile(r"\bfield_")
    alg_code = re.sub(pattern, f"{kind_name}_field_", alg_code)
    os.mkdir(str(tmpdir.join("tmp")))
    tmp_fname = str(tmpdir.join("tmp", f"real_{kind_name}_X_builtin_alg.f90"))
    with open(tmp_fname, "w", encoding='utf-8') as tmp_file:
        tmp_file.write(alg_code)
    tmp_file.close()

    # Read and parse the modified algorithm
    with open(tmp_fname, "r", encoding='utf-8') as alg_file:
        _, invoke_info = parse(alg_file, api=API)
    psy = PSyFactory(API).create(invoke_info)
    code = str(psy.gen)

    # Test code generation
    assert "use constants_mod\n" in code
    assert (f"use {kind_name}_field_mod, only : {kind_name}_field_proxy_type, "
            f"{kind_name}_field_type") in code
    assert f"type({kind_name}_field_type), intent(in) :: f2" in code
    assert (f"real(kind={kind_name}), pointer, dimension(:) :: "
            "f2_data => null()") in code
    assert f"type({kind_name}_field_proxy_type) :: f2_proxy" in code
    assert f"f2_data(df) = REAL(f1_data(df), kind={kind_name})" in code

    # Test compilation of generated code
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_real_to_int_x(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicRealToIntXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 2
    assert metadata.meta_args[0].access == "gh_write"
    assert metadata.meta_args[0].datatype == "gh_integer"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[1].datatype == "gh_real"
    assert metadata.meta_args[1].function_space == "any_space_1"

    kern = builtin_from_file("15.10.3_real_to_int_X_builtin.f90")
    assert str(kern) == ("Built-in: real_to_int_X (convert a real-valued to "
                         "an integer-valued field)")

    # Test the 'lower_to_language_level()' method
    code = fortran_writer(kern)
    assert (
        "! Built-in: real_to_int_X (convert a real-valued to an "
        "integer-valued field)\n"
        "f2_data(df) = INT(f1_data(df), kind=i_def)\n") in code


@pytest.mark.parametrize("kind_name", ["i_um", "i_ncdf"])
def test_real_to_int_x_precision(monkeypatch, tmpdir, kind_name):
    '''
    Test that the built-in picks up and creates correct code for field
    data with precision that is not the default, i.e. not 'i_def'.
    At the moment there is no other integer precision for field data
    so we use random integer precisions from 'constants_mod'.
    However, this does mean that we are not able to check whether the
    generated PSy layer compiles.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.10.3_real_to_int_X_builtin.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    table = first_invoke.schedule.symbol_table
    arg = first_invoke.schedule.children[0].loop_body[0].args[0]
    # Set 'f2_data' to another 'i_<prec>'
    sym_kern = table.lookup_with_tag(f"{arg.name}:data")
    monkeypatch.setattr(sym_kern.datatype.partial_datatype.precision.symbol,
                        "_name", f"{kind_name}")

    # Test limited code generation (no equivalent field type)
    code = str(psy.gen)
    assert "use constants_mod\n" in code
    assert ("integer(kind=i_def), pointer, dimension(:) :: f2_data => null()"
            in code)
    assert f"f2_data(df) = INT(f1_data(df), kind={kind_name})" in code

    # Test compilation of generated code
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_real_to_real_x(fortran_writer):
    ''' Test the metadata, str and lower_to_language_level builtin methods. '''
    metadata = lfric_builtins.LFRicRealToRealXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    assert len(metadata.meta_args) == 2
    assert metadata.meta_args[0].access == "gh_write"
    assert metadata.meta_args[0].datatype == "gh_real"
    assert metadata.meta_args[0].function_space == "any_space_1"
    assert metadata.meta_args[1].access == "gh_read"
    assert metadata.meta_args[1].datatype == "gh_real"
    assert metadata.meta_args[1].function_space == "any_space_1"

    kern = builtin_from_file("15.10.8_real_to_real_X_builtin.f90")
    assert str(kern) == ("Built-in: real_to_real_X (convert a real-valued to "
                         "a real-valued field)")

    loops = kern.ancestor(Routine).walk(Loop)
    # Test the 'lower_to_language_level()' method
    for idx, loop in enumerate(loops):
        kern = loop.loop_body[0]
        code = fortran_writer(kern)
        if idx == 0:
            assert (
                "! Built-in: real_to_real_X (convert a real-valued to a "
                "real-valued field)\n"
                "f2_data(df) = REAL(f1_data(df), kind=r_tran)\n") in code
        elif idx == 1:
            assert (
                "! Built-in: real_to_real_X (convert a real-valued to a "
                "real-valued field)\n"
                "f1_data(df) = REAL(f3_data(df), kind=r_def)\n") in code
        elif idx == 2:
            assert (
                "! Built-in: real_to_real_X (convert a real-valued to a "
                "real-valued field)\n"
                "f3_data(df) = REAL(f2_data(df), kind=r_solver)\n") in code
        else:
            assert False, code  # There are only 3 kern


@pytest.mark.parametrize("kind_name", ["r_bl", "r_um"])
def test_real_to_real_x_lowering(monkeypatch, tmpdir, kind_name):
    '''
    Test that the lower_to_language_level() method works as expected.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.10.8_real_to_real_X_builtin.f90"),
                           api=API)
    psy = PSyFactory(API,
                     distributed_memory=False).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    table = first_invoke.schedule.symbol_table
    arg = first_invoke.schedule.children[0].loop_body[0].args[0]
    # Set 'f2_data' to another 'r_<prec>'
    sym_kern = table.lookup_with_tag(f"{arg.name}:data")
    monkeypatch.setattr(sym_kern.datatype.partial_datatype.precision.symbol,
                        "_name", f"{kind_name}")

    # Test limited code generation (no equivalent field type)
    code = str(psy.gen)

    # Check that the kind constants are imported
    assert "use constants_mod\n" in code

    # Assert correct type is set
    assert f"f2_data(df) = REAL(f1_data(df), kind={kind_name})" in code

    # Test compilation of generated code
    assert LFRicBuild(tmpdir).code_compiles(psy)


# ------------- Invalid built-in with an integer scalar reduction ----------- #

def test_scalar_int_builtin_error(monkeypatch):
    '''
    Test that specifying incorrect meta-data for a built-in such that it
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


def test_field_access_info_for_arrays_in_builtins():
    '''Tests that array of fields in LFRic built-ins properly report access
    information. For example, 'call invoke( a_plus_X(f2(i), a, f1) )'
    must report the access to 'f2' (which will be 'f2_data', the pointer to
    the data array associated with the field).

    '''
    _, invoke = get_invoke("15.1.8_a_plus_X_builtin_array_of_fields.f90",
                           api=API, idx=0, dist_mem=False)
    schedule = invoke.schedule
    vam = schedule.reference_accesses()

    assert Signature("f2_data") in vam

    assert (
        "field_type: TYPE_INFO, i_def: TYPE_INFO, r_def: TYPE_INFO, a: READ, "
        "df: WRITE+READ, f1_data: READ, f2_data: WRITE, "
        "uninitialised_loop0_start: READ, uninitialised_loop0_stop: READ"
        == str(vam))
