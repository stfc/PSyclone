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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab;
#         I. Kavcic and A. Coughtrie, Met Office;
#         C. M. Maynard, Met Office/University of Reading;
#         J. Henrichs, Bureau of Meteorology.

'''
Module containing pytest tests for the general LFRic scalar arguments
functionality (e.g. metadata, parsing, invoke calls).
'''

# Imports
from __future__ import absolute_import, print_function
import os
import pytest
import six
import fparser
from fparser import api as fpapi
from psyclone.domain.lfric import LFRicArgDescriptor
from psyclone.dynamo0p3 import LFRicScalarArgs, DynKernMetadata
from psyclone.f2pygen import ModuleGen
from psyclone.parse.algorithm import parse
from psyclone.parse.utils import ParseError
from psyclone.psyGen import PSyFactory
from psyclone.errors import InternalError, GenerationError

# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "dynamo0p3")
TEST_API = "dynamo0.3"

# General scalar checks (argument type, data type, etc)


CODE = '''
module testkern_qr
  type, extends(kernel_type) :: testkern_qr_type
     type(arg_type), meta_args(6) =                              &
          (/ arg_type(gh_scalar,   gh_real,    gh_read),         &
             arg_type(gh_field,    gh_real,    gh_inc,  w1),     &
             arg_type(gh_field,    gh_real,    gh_read, w2),     &
             arg_type(gh_operator, gh_real,    gh_read, w2, w2), &
             arg_type(gh_field,    gh_real,    gh_read, w3),     &
             arg_type(gh_scalar,   gh_integer, gh_read)          &
           /)
     type(func_type), dimension(3) :: meta_funcs =  &
          (/ func_type(w1, gh_basis),               &
             func_type(w2, gh_diff_basis),          &
             func_type(w3, gh_basis, gh_diff_basis) &
           /)
     integer :: operates_on = cell_column
     integer :: gh_shape = gh_quadrature_XYoZ
   contains
     procedure, nopass :: code => testkern_qr_code
  end type testkern_qr_type
contains
  subroutine testkern_qr_code(a, b, c, d)
  end subroutine testkern_qr_code
end module testkern_qr
'''


def test_ad_scalar_old_metadata():
    ''' Test that the LFRicArgDescriptor argument representation supports
    old-style scalar arguments' names GH_REAL and GH_INTEGER.
    TODO: This support and the test will be removed in #874.

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("gh_scalar, ", "")
    ast = fpapi.parse(code, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")

    # Check representation of a real scalar (note: "gh_real"
    # argument type is translated to "gh_scalar")
    real_scalar_descriptor = metadata.arg_descriptors[0]
    result = str(real_scalar_descriptor)
    expected_output = (
        "LFRicArgDescriptor object\n"
        "  argument_type[0]='gh_scalar'\n"
        "  data_type[1]='gh_real'\n"
        "  access_descriptor[2]='gh_read'\n")
    assert expected_output in result

    # Check representation of an integer scalar (note: "gh_integer"
    # argument type is translated to "gh_scalar")
    int_scalar_descriptor = metadata.arg_descriptors[5]
    result = str(int_scalar_descriptor)
    expected_output = (
        "LFRicArgDescriptor object\n"
        "  argument_type[0]='gh_scalar'\n"
        "  data_type[1]='gh_integer'\n"
        "  access_descriptor[2]='gh_read'\n")
    assert expected_output in result


def test_ad_scalar_init_wrong_argument_type():
    ''' Test that an error is raised if something other than a scalar
    is passed to the LFRicArgDescriptor._init_scalar() method. '''
    ast = fpapi.parse(CODE, ignore_comments=False)
    name = "testkern_qr_type"
    metadata = DynKernMetadata(ast, name=name)
    # Get an argument which is not a scalar
    wrong_arg = metadata._inits[3]
    with pytest.raises(InternalError) as excinfo:
        LFRicArgDescriptor(
            wrong_arg, metadata.iterates_over)._init_scalar(wrong_arg)
    assert ("Expected a scalar argument but got an argument of type "
            "'gh_operator'." in str(excinfo.value))


def test_ad_scalar_type_too_few_args():
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar has fewer than 3 args.
    Note: This general check is also valid for all other argument types.

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_qr_type"
    # TODO in #874: Remove support for old-style scalar metadata by removing
    #               index [0] and restoring the scalar names list
    for argname in [LFRicArgDescriptor.VALID_SCALAR_NAMES[0]]:
        code = CODE.replace("arg_type(" + argname + ",   gh_real,    gh_read)",
                            "arg_type(" + argname + ",   gh_real)", 1)
        ast = fpapi.parse(code, ignore_comments=False)
        with pytest.raises(ParseError) as excinfo:
            _ = DynKernMetadata(ast, name=name)
        assert ("In the LFRic API each 'meta_arg' entry must have at least "
                "3 args, but found 2 in 'arg_type(gh_scalar, gh_real)'."
                in str(excinfo.value))


def test_ad_scalar_type_too_many_args():
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar has more than 3 args. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_qr_type"
    # TODO in #874: Remove support for old-style scalar metadata by removing
    #               index [0] and restoring the scalar names list
    for argname in [LFRicArgDescriptor.VALID_SCALAR_NAMES[0]]:
        code = CODE.replace(
            "arg_type(" + argname + ",   gh_integer, gh_read)",
            "arg_type(" + argname + ",   gh_integer, gh_read, w1)", 1)
        ast = fpapi.parse(code, ignore_comments=False)
        with pytest.raises(ParseError) as excinfo:
            _ = DynKernMetadata(ast, name=name)
        assert ("each 'meta_arg' entry must have 3 arguments if its first "
                "argument is 'gh_{r,i}scalar', but found 4 in "
                "'arg_type(gh_scalar, gh_integer, gh_read, w1)'." in
                str(excinfo.value))


def test_ad_scalar_invalid_data_type():
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar has an invalid data type. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_qr_type"
    code = CODE.replace("arg_type(gh_scalar,   gh_real,    gh_read)",
                        "arg_type(gh_scalar,   gh_read)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("In the LFRic API the 2nd argument of a 'meta_arg' "
            "entry should be a valid data type (one of "
            "['gh_real', 'gh_integer']), but found 'gh_read' in "
            "'arg_type(gh_scalar, gh_read)'." in str(excinfo.value))


def test_ad_scalar_init_wrong_data_type(monkeypatch):
    ''' Test that an error is raised if an invalid data type
    is passed to the LFRicArgDescriptor._init_scalar() method. '''
    ast = fpapi.parse(CODE, ignore_comments=False)
    name = "testkern_qr_type"
    metadata = DynKernMetadata(ast, name=name)
    # Get a scalar argument descriptor and set a wrong data type
    scalar_arg = metadata._inits[0]
    scalar_arg.args[1].name = "gh_double"
    # Now try to trip the error by making the initial test think
    # that 'gh_double' is actually a valid data type
    monkeypatch.setattr(
        target=LFRicArgDescriptor, name="VALID_ARG_DATA_TYPES",
        value=LFRicArgDescriptor.VALID_ARG_DATA_TYPES + ["gh_double"])
    with pytest.raises(InternalError) as excinfo:
        LFRicArgDescriptor(
            scalar_arg, metadata.iterates_over)._init_scalar(scalar_arg)
    assert ("Expected one of {0} as the scalar data type but got 'gh_double'.".
            format(LFRicArgDescriptor.VALID_SCALAR_DATA_TYPES) in
            str(excinfo.value))


def test_ad_scalar_type_no_write():
    ''' Tests that an error is raised when the argument descriptor metadata
    for a real or an integer scalar specifies 'GH_WRITE' access. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_qr_type"
    # TODO in #874: Remove support for old-style scalar metadata by removing
    #               index [0] and restoring the scalar names list
    for argname in [LFRicArgDescriptor.VALID_SCALAR_NAMES[0]]:
        code = CODE.replace(
            "arg_type(" + argname + ",   gh_integer, gh_read)",
            "arg_type(" + argname + ",   gh_integer, gh_write)", 1)
        ast = fpapi.parse(code, ignore_comments=False)
        with pytest.raises(ParseError) as excinfo:
            _ = DynKernMetadata(ast, name=name)
        assert ("scalar arguments must have read-only ('gh_read') or a "
                "reduction ['gh_sum'] access but found 'gh_write'" in
                str(excinfo.value))


def test_ad_scalar_type_no_inc():
    ''' Tests that an error is raised when the argument descriptor metadata
    for a real or an integer scalar specifies 'GH_INC' access. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_qr_type"
    # TODO in #874: Remove support for old-style scalar metadata by removing
    #               index [0] and restoring the scalar names list
    for argname in [LFRicArgDescriptor.VALID_SCALAR_NAMES[0]]:
        code = CODE.replace("arg_type(" + argname + ",   gh_real,    gh_read)",
                            "arg_type(" + argname + ",   gh_real, gh_inc)", 1)
        ast = fpapi.parse(code, ignore_comments=False)
        with pytest.raises(ParseError) as excinfo:
            _ = DynKernMetadata(ast, name=name)
        assert ("scalar arguments must have read-only ('gh_read') or a "
                "reduction ['gh_sum'] access but found 'gh_inc'" in
                str(excinfo.value))


def test_ad_int_scalar_type_no_sum():
    ''' Tests that an error is raised when the argument descriptor metadata
    for an integer scalar specifies 'GH_SUM' access (reduction). '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("arg_type(gh_scalar,   gh_integer, gh_read)",
                        "arg_type(gh_scalar,   gh_integer, gh_sum)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("reduction access 'gh_sum' is only valid with a real scalar "
            "argument, but a scalar argument with 'gh_integer' data type "
            in str(excinfo.value))


def test_no_vector_scalar():
    ''' Tests that we raise an error when kernel metadata erroneously
    specifies a vector scalar argument. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_qr_type"
    # TODO in #874: Remove support for old-style scalar metadata by removing
    #               index [0] and restoring the scalar names list
    for argname in [LFRicArgDescriptor.VALID_SCALAR_NAMES[0]]:
        vectname = argname + " * 3"
        code = CODE.replace("arg_type(" + argname + ",   gh_real,    gh_read)",
                            "arg_type(" + vectname + ", gh_real, gh_read)", 1)
        ast = fpapi.parse(code, ignore_comments=False)
        with pytest.raises(ParseError) as excinfo:
            _ = DynKernMetadata(ast, name=name)
        assert ("vector notation is only supported for ['gh_field'] "
                "argument types but found '{0}'".format(vectname) in
                str(excinfo.value))


def test_arg_descriptor_real_scalar():
    ''' Test that the LFRicArgDescriptor argument representation works
    as expected for a real scalar argument. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    scalar_descriptor = metadata.arg_descriptors[0]

    # Assert correct string representation from LFRicArgDescriptor
    result = str(scalar_descriptor)
    expected_output = (
        "LFRicArgDescriptor object\n"
        "  argument_type[0]='gh_scalar'\n"
        "  data_type[1]='gh_real'\n"
        "  access_descriptor[2]='gh_read'\n")
    assert expected_output in result

    # Check LFRicArgDescriptor argument properties
    assert scalar_descriptor.argument_type == "gh_scalar"
    assert scalar_descriptor.data_type == "gh_real"
    assert scalar_descriptor.function_space is None
    assert scalar_descriptor.function_spaces == []
    assert str(scalar_descriptor.access) == "READ"
    assert scalar_descriptor.mesh is None
    assert scalar_descriptor.stencil is None
    assert scalar_descriptor.vector_size == 0


def test_arg_descriptor_int_scalar():
    ''' Test that the LFRicArgDescriptor argument representation works
    as expected for an integer scalar argument. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    scalar_descriptor = metadata.arg_descriptors[5]

    # Assert correct string representation from LFRicArgDescriptor
    result = str(scalar_descriptor)
    expected_output = (
        "LFRicArgDescriptor object\n"
        "  argument_type[0]='gh_scalar'\n"
        "  data_type[1]='gh_integer'\n"
        "  access_descriptor[2]='gh_read'\n")
    assert expected_output in result

    # Check LFRicArgDescriptor argument properties
    assert scalar_descriptor.argument_type == "gh_scalar"
    assert scalar_descriptor.data_type == "gh_integer"
    assert scalar_descriptor.function_space is None
    assert scalar_descriptor.function_spaces == []
    assert str(scalar_descriptor.access) == "READ"
    assert scalar_descriptor.mesh is None
    assert scalar_descriptor.stencil is None
    assert scalar_descriptor.vector_size == 0


def test_lfricscalars_call_err():
    ''' Check that the LFRicScalarArgs constructor raises the expected
    internal error if it encounters an unrecognised intrinsic type of
    scalar when generating a kernel call.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.7_single_invoke_2scalar.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    kernel = invoke.schedule.coded_kernels()[0]
    # Sabotage the scalar argument to make it have an invalid intrinsic type
    scalar_arg = kernel.arguments.args[0]
    scalar_arg._intrinsic_type = "double-type"
    with pytest.raises(InternalError) as err:
        LFRicScalarArgs(invoke)._invoke_declarations(ModuleGen(name="my_mod"))
    test_str = str(err.value)
    if six.PY2:
        test_str = test_str.replace("u'", "'")
    assert ("Found unsupported intrinsic types for the scalar arguments "
            "['a'] to Invoke 'invoke_0_testkern_two_scalars_type'. Supported "
            "types are ['real', 'integer']." in test_str)


def test_dyninvoke_uniq_declns_intent_scalar():
    ''' Tests that DynInvoke.unique_declns_by_intent() returns the correct
    list of arguments for 'gh_scalar' argument type. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    real_args = psy.invokes.invoke_list[0].unique_declns_by_intent(
        ["gh_scalar"], intrinsic_type="real")
    int_args = psy.invokes.invoke_list[0].unique_declns_by_intent(
        ["gh_scalar"], intrinsic_type="integer")
    assert real_args['inout'] == []
    assert real_args['out'] == []
    assert int_args['inout'] == []
    assert int_args['out'] == []
    real_args_in = [arg.declaration_name for arg in real_args['in']]
    int_args_in = [arg.declaration_name for arg in int_args['in']]
    assert real_args_in == ['a']
    assert int_args_in == ['istep']


def test_scalar_invoke_uniq_declns_valid_intrinsic():
    ''' Tests that all valid intrinsic types for user-defined scalar arguments
    ('real' and 'integer') are accepted by Invoke.unique_declarations().

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]

    # Test 'real' scalars
    scalars_real_args = invoke.unique_declarations(
        LFRicArgDescriptor.VALID_SCALAR_NAMES, intrinsic_type="real")
    scalars_real = [arg.declaration_name for arg in scalars_real_args]
    assert scalars_real == ["a"]

    # Test 'integer' scalars
    scalars_int_args = invoke.unique_declarations(
        LFRicArgDescriptor.VALID_SCALAR_NAMES, intrinsic_type="integer")
    scalars_int = [arg.declaration_name for arg in scalars_int_args]
    assert scalars_int == ["istep"]


def test_multiple_updated_scalar_args():
    ''' Check that we raise the expected exception when we encounter a
    kernel that writes to more than one of its field and scalar arguments '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("arg_type(gh_scalar,   gh_real,    gh_read)",
                        "arg_type(gh_scalar,   gh_real,    gh_sum)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("A user-supplied LFRic kernel must not write/update a scalar "
            "argument but kernel 'testkern_qr_type' has a scalar "
            "argument with 'gh_sum' access." in str(excinfo.value))


def test_int_real_scalar_invalid():
    ''' Tests that the same scalar cannot have different data types
    in different kernels within the same Invoke.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "4.16_multikernel_invokes_real_int_scalar_invalid.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)

    with pytest.raises(GenerationError) as err:
        _ = psy.gen
    assert ("Scalar argument(s) ['b'] in Invoke "
            "'invoke_real_and_integer_scalars' have different metadata for "
            "data type (['gh_real', 'gh_integer']) in different kernels. "
            "This is invalid." in str(err.value))
