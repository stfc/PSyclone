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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab;
#         I. Kavcic, A. Coughtrie, L. Turner and O. Brunt, Met Office;
#         C. M. Maynard, Met Office/University of Reading;
#         J. Henrichs, Bureau of Meteorology.

'''
Module containing pytest tests for the general LFRic scalar arguments
functionality (e.g. metadata, parsing, invoke calls).
'''

import os
import pytest
import fparser
from fparser import api as fpapi

from psyclone.domain.lfric import (LFRicArgDescriptor, LFRicConstants,
                                   LFRicKern, LFRicKernMetadata,
                                   LFRicScalarArgs)
from psyclone.errors import InternalError, GenerationError
from psyclone.f2pygen import ModuleGen
from psyclone.parse.algorithm import parse
from psyclone.parse.utils import ParseError
from psyclone.psyGen import FORTRAN_INTENT_NAMES, PSyFactory

# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "dynamo0p3")
TEST_API = "dynamo0.3"


CODE = '''
module testkern_qr
  type, extends(kernel_type) :: testkern_qr_type
     type(arg_type), meta_args(7) =                              &
          (/ arg_type(gh_scalar,   gh_real,    gh_read),         &
             arg_type(gh_field,    gh_real,    gh_inc,  w1),     &
             arg_type(gh_field,    gh_real,    gh_read, w2),     &
             arg_type(gh_operator, gh_real,    gh_read, w2, w2), &
             arg_type(gh_field,    gh_real,    gh_read, w3),     &
             arg_type(gh_scalar,   gh_logical, gh_read),         &
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


def test_ad_scalar_init_wrong_argument_type():
    ''' Test that an error is raised if something other than a scalar
    is passed to the LFRicArgDescriptor._init_scalar() method. '''
    ast = fpapi.parse(CODE, ignore_comments=False)
    name = "testkern_qr_type"
    metadata = LFRicKernMetadata(ast, name=name)
    # Get an argument which is not a scalar
    wrong_arg = metadata._inits[3]
    with pytest.raises(InternalError) as excinfo:
        LFRicArgDescriptor(
            wrong_arg, metadata.iterates_over, 0)._init_scalar(wrong_arg)
    assert ("Expected a scalar argument but got an argument of type "
            "'gh_operator'." in str(excinfo.value))


def test_ad_scalar_type_too_few_args():
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar has fewer than 3 args.
    Note: This general check is also valid for all other argument types.

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_qr_type"
    const = LFRicConstants()
    for argname in const.VALID_SCALAR_NAMES:
        code = CODE.replace("arg_type(" + argname + ",   gh_real,    gh_read)",
                            "arg_type(" + argname + ",   gh_real)", 1)
        ast = fpapi.parse(code, ignore_comments=False)
        with pytest.raises(ParseError) as excinfo:
            _ = LFRicKernMetadata(ast, name=name)
        assert ("In the LFRic API each 'meta_arg' entry must have at least "
                "3 args, but found 2 in 'arg_type(gh_scalar, gh_real)'."
                in str(excinfo.value))


def test_ad_scalar_type_too_many_args():
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar has more than 3 args. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_qr_type"
    const = LFRicConstants()
    for argname in const.VALID_SCALAR_NAMES:
        code = CODE.replace(
            "arg_type(" + argname + ",   gh_integer, gh_read)",
            "arg_type(" + argname + ",   gh_integer, gh_read, w1)", 1)
        ast = fpapi.parse(code, ignore_comments=False)
        with pytest.raises(ParseError) as excinfo:
            _ = LFRicKernMetadata(ast, name=name)
        assert ("each 'meta_arg' entry must have 3 arguments if its first "
                "argument is 'gh_scalar', but found 4 in "
                "'arg_type(gh_scalar, gh_integer, gh_read, w1)'." in
                str(excinfo.value))


def test_ad_scalar_invalid_data_type():
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar has an invalid data type. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_qr_type"
    code = CODE.replace("arg_type(gh_scalar,   gh_real,    gh_read)",
                        "arg_type(gh_scalar, gh_unreal, gh_read)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    const = LFRicConstants()
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert (f"In the LFRic API the 2nd argument of a 'meta_arg' entry should "
            f"be a valid data type (one of {const.VALID_SCALAR_DATA_TYPES}), "
            f"but found 'gh_unreal' in 'arg_type(gh_scalar, gh_unreal, "
            f"gh_read)'." in str(excinfo.value))


def test_ad_scalar_init_wrong_data_type(monkeypatch):
    ''' Test that an error is raised if an invalid data type
    is passed to the LFRicArgDescriptor._init_scalar() method. '''
    ast = fpapi.parse(CODE, ignore_comments=False)
    name = "testkern_qr_type"
    metadata = LFRicKernMetadata(ast, name=name)
    # Get a scalar argument descriptor and set a wrong data type
    scalar_arg = metadata._inits[0]
    scalar_arg.args[1].name = "gh_double"
    const = LFRicConstants()
    # Now try to trip the error by making the initial test think
    # that 'gh_double' is actually a valid data type
    monkeypatch.setattr(
        target=LFRicConstants, name="VALID_ARG_DATA_TYPES",
        value=LFRicConstants.VALID_ARG_DATA_TYPES + ["gh_double"])
    with pytest.raises(InternalError) as excinfo:
        LFRicArgDescriptor(
            scalar_arg, metadata.iterates_over, 0)._init_scalar(scalar_arg)
    assert (f"Expected one of {const.VALID_SCALAR_DATA_TYPES} as the scalar "
            f"data type but got 'gh_double'." in str(excinfo.value))


def test_ad_scalar_type_no_write():
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar specifies 'GH_WRITE' access. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_qr_type"
    const = LFRicConstants()
    for argname in const.VALID_SCALAR_NAMES:
        code = CODE.replace(
            "arg_type(" + argname + ",   gh_integer, gh_read)",
            "arg_type(" + argname + ",   gh_integer, gh_write)", 1)
        ast = fpapi.parse(code, ignore_comments=False)
        with pytest.raises(ParseError) as excinfo:
            _ = LFRicKernMetadata(ast, name=name)
        assert ("scalar arguments must have read-only ('gh_read') or a "
                "reduction ['gh_sum'] access but found 'gh_write'" in
                str(excinfo.value))


def test_ad_scalar_type_no_inc():
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar specifies 'GH_INC' access. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_qr_type"
    const = LFRicConstants()
    for argname in const.VALID_SCALAR_NAMES:
        code = CODE.replace("arg_type(" + argname + ",   gh_real,    gh_read)",
                            "arg_type(" + argname + ",   gh_real, gh_inc)", 1)
        ast = fpapi.parse(code, ignore_comments=False)
        with pytest.raises(ParseError) as excinfo:
            _ = LFRicKernMetadata(ast, name=name)
        assert ("scalar arguments must have read-only ('gh_read') or a "
                "reduction ['gh_sum'] access but found 'gh_inc'" in
                str(excinfo.value))


def test_ad_scalar_type_no_readwrite():
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar specifies 'GH_READWRITE' access. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_qr_type"
    const = LFRicConstants()
    for argname in const.VALID_SCALAR_NAMES:
        code = CODE.replace(
            "arg_type(" + argname + ",   gh_logical, gh_read)",
            "arg_type(" + argname + ",   gh_logical, gh_readwrite)", 1)
        ast = fpapi.parse(code, ignore_comments=False)
        with pytest.raises(ParseError) as excinfo:
            _ = LFRicKernMetadata(ast, name=name)
        assert ("scalar arguments must have read-only ('gh_read') or a "
                "reduction ['gh_sum'] access but found 'gh_readwrite'" in
                str(excinfo.value))


@pytest.mark.parametrize("scalar_type", ["gh_integer", "gh_logical"])
def test_ad_integer_logical_scalar_type_no_sum(scalar_type):
    ''' Tests that an error is raised when the argument descriptor
    metadata for an 'integer' or a 'logical' scalar specifies 'GH_SUM'
    access (reduction).

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace(
        f"arg_type(gh_scalar,   {scalar_type}, gh_read)",
        f"arg_type(gh_scalar,   {scalar_type}, gh_sum)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert (f"reduction access 'gh_sum' is only valid with a real scalar "
            f"argument, but a scalar argument with '{scalar_type}' data type "
            in str(excinfo.value))


def test_no_vector_scalar():
    ''' Tests that we raise an error when kernel metadata erroneously
    specifies a vector scalar argument. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_qr_type"
    const = LFRicConstants()
    for argname in const.VALID_SCALAR_NAMES:
        vectname = argname + " * 3"
        code = CODE.replace("arg_type(" + argname + ",   gh_real,    gh_read)",
                            "arg_type(" + vectname + ", gh_real, gh_read)", 1)
        ast = fpapi.parse(code, ignore_comments=False)
        with pytest.raises(ParseError) as excinfo:
            _ = LFRicKernMetadata(ast, name=name)
        assert (f"vector notation is only supported for ['gh_field'] argument "
                f"types but found '{vectname}'" in str(excinfo.value))


@pytest.mark.parametrize("scalar_ind, scalar_type", [
    (0, "gh_real"), (6, "gh_integer"), (5, "gh_logical")])
def test_arg_descriptor_scalar(scalar_ind, scalar_type):
    ''' Test that the LFRicArgDescriptor argument representation works
    as expected for all three types of valid scalar argument:
    'real', 'integer' and 'logical'.

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = LFRicKernMetadata(ast, name="testkern_qr_type")
    scalar_descriptor = metadata.arg_descriptors[scalar_ind]

    # Assert correct string representation from LFRicArgDescriptor
    result = str(scalar_descriptor)
    expected_output = (
        f"LFRicArgDescriptor object\n"
        f"  argument_type[0]='gh_scalar'\n"
        f"  data_type[1]='{scalar_type}'\n"
        f"  access_descriptor[2]='gh_read'\n")
    assert expected_output in result

    # Check LFRicArgDescriptor argument properties
    assert scalar_descriptor.argument_type == "gh_scalar"
    assert scalar_descriptor.data_type == scalar_type
    assert scalar_descriptor.function_space is None
    assert scalar_descriptor.function_spaces == []
    assert str(scalar_descriptor.access) == "READ"
    assert scalar_descriptor.mesh is None
    assert scalar_descriptor.stencil is None
    assert scalar_descriptor.vector_size == 0


def test_lfricscalars_call_err1():
    ''' Check that the LFRicScalarArgs constructor raises the expected
    internal error if it encounters an unrecognised intrinsic type of
    scalar when generating a kernel call.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.7_single_invoke_3scalar.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    kernel = invoke.schedule.coded_kernels()[0]
    # Sabotage the scalar argument to make it have an invalid intrinsic type
    scalar_arg = kernel.arguments.args[0]
    scalar_arg._intrinsic_type = "double-type"
    with pytest.raises(InternalError) as err:
        LFRicScalarArgs(invoke)._invoke_declarations(ModuleGen(name="my_mod"))
    assert ("Found unsupported intrinsic types for the scalar arguments "
            "['a'] to Invoke 'invoke_0_testkern_three_scalars_type'. "
            "Supported types are ['real', 'integer', 'logical']."
            in str(err.value))


def test_lfricscalars_call_err2():
    '''Check that LFRicScalarArgs _create_declarations method raises the
    expected internal errors for real, integer and logical scalars if
    neither invoke nor kernel is set.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.7_single_invoke_3scalar.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    scalar_args = LFRicScalarArgs(invoke)
    node = ModuleGen("prog")
    # Set up information that _create_declarations requires. Note,
    # this method also calls _create_declarations.
    scalar_args._invoke_declarations(node)

    # Sabotage code so that a call to _create declarations raises the
    # required exceptions.
    scalar_args._invoke = None

    # The first exception comes from real scalars.
    with pytest.raises(InternalError) as error:
        scalar_args._create_declarations(node)
    assert ("Expected the declaration of real scalar kernel arguments to be "
            "for either an invoke or a kernel stub, but it is neither."
            in str(error.value))

    # Remove real scalars so we get the exception for integer scalars.
    for intent in FORTRAN_INTENT_NAMES:
        scalar_args._real_scalars[intent] = None
    with pytest.raises(InternalError) as error:
        scalar_args._create_declarations(node)
    assert ("Expected the declaration of integer scalar kernel arguments to "
            "be for either an invoke or a kernel stub, but it is neither."
            in str(error.value))

    # Remove integer scalars so we get the exception for logical scalars.
    for intent in FORTRAN_INTENT_NAMES:
        scalar_args._integer_scalars[intent] = None
    with pytest.raises(InternalError) as error:
        scalar_args._create_declarations(node)
    assert ("Expected the declaration of logical scalar kernel arguments to "
            "be for either an invoke or a kernel stub, but it is neither."
            in str(error.value))


def test_lfricscalarargs_mp():
    '''Check that the precision of a new scalar integer datatype is
    declared in the psy-layer.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.6.4_scalar_mixed_prec.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    code = str(psy.gen).lower()
    assert "use constants_mod, only: roo_def, r_def, i_def" in code


def test_lfricinvoke_uniq_declns_intent_scalar():
    ''' Tests that LFRicInvoke.unique_declns_by_intent() returns the correct
    list of arguments for 'gh_scalar' argument type. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_3scalar.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)

    # Test 'real' scalar arguments
    real_args = psy.invokes.invoke_list[0].unique_declns_by_intent(
        ["gh_scalar"], intrinsic_type="real")
    assert real_args['inout'] == []
    assert real_args['out'] == []
    real_args_in = [arg.declaration_name for arg in real_args['in']]
    assert real_args_in == ['a']

    # Test 'integer' scalar arguments
    integer_args = psy.invokes.invoke_list[0].unique_declns_by_intent(
        ["gh_scalar"], intrinsic_type="integer")
    assert integer_args['inout'] == []
    assert integer_args['out'] == []
    integer_args_in = [arg.declaration_name for arg in integer_args['in']]
    assert integer_args_in == ['istep']

    # Test 'logical' scalar arguments
    logical_args = psy.invokes.invoke_list[0].unique_declns_by_intent(
        ["gh_scalar"], intrinsic_type="logical")
    assert logical_args['inout'] == []
    assert logical_args['out'] == []
    logical_args_in = [arg.declaration_name for arg in logical_args['in']]
    assert logical_args_in == ['lswitch']


def test_scalar_invoke_uniq_declns_valid_intrinsic():
    ''' Tests that all valid intrinsic types for user-defined scalar
    arguments ('real', 'integer' and 'logical') are accepted by
    Invoke.unique_declarations().

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_3scalar.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]

    # Test 'real' scalars
    const = LFRicConstants()
    scalars_real_args = invoke.unique_declarations(
        const.VALID_SCALAR_NAMES, intrinsic_type="real")
    scalars_real = [arg.declaration_name for arg in scalars_real_args]
    assert scalars_real == ["a"]

    # Test 'integer' scalars
    scalars_integer_args = invoke.unique_declarations(
        const.VALID_SCALAR_NAMES, intrinsic_type="integer")
    scalars_integer = [arg.declaration_name for arg in scalars_integer_args]
    assert scalars_integer == ["istep"]

    # Test 'logical' scalars
    scalars_logical_args = invoke.unique_declarations(
        const.VALID_SCALAR_NAMES, intrinsic_type="logical")
    scalars_logical = [arg.declaration_name for arg in scalars_logical_args]
    assert scalars_logical == ["lswitch"]


def test_scalar_arg_lfricconst_properties(monkeypatch):
    ''' Tests that properties of all supported types of user-defined,
    read-only, scalar arguments ('real', 'integer' and 'logical') defined
    in LFRicConstants are correctly set up in the DynKernelArgument class.

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(CODE, ignore_comments=False)
    name = "testkern_qr_type"
    metadata = LFRicKernMetadata(ast, name=name)
    kernel = LFRicKern()
    kernel.load_meta(metadata)

    # Test 'real' scalars
    scalar_arg = kernel.arguments.args[0]
    assert scalar_arg.module_name is None
    assert scalar_arg.data_type is None
    assert scalar_arg.proxy_data_type is None
    assert scalar_arg.intrinsic_type == "real"
    assert scalar_arg.precision == "r_def"

    # Test 'integer' scalars
    scalar_arg = kernel.arguments.args[6]
    assert scalar_arg.module_name is None
    assert scalar_arg.data_type is None
    assert scalar_arg.proxy_data_type is None
    assert scalar_arg.intrinsic_type == "integer"
    assert scalar_arg.precision == "i_def"

    # Test 'logical' scalars
    scalar_arg = kernel.arguments.args[5]
    assert scalar_arg.module_name is None
    assert scalar_arg.data_type is None
    assert scalar_arg.proxy_data_type is None
    assert scalar_arg.intrinsic_type == "logical"
    assert scalar_arg.precision == "l_def"

    # Monkeypatch to check with an invalid intrinsic type of a
    # scalar argument
    const = LFRicConstants()
    monkeypatch.setattr(scalar_arg, "_intrinsic_type", "tabby")
    with pytest.raises(InternalError) as err:
        scalar_arg._init_data_type_properties(None)
    assert (f"Expected one of {const.VALID_INTRINSIC_TYPES} intrinsic types "
            f"for a scalar argument but found 'tabby' in the metadata of "
            f"kernel testkern_qr_code for argument lscalar_6."
            in str(err.value))


def test_scalar_reduction_lfricconst_properties():
    ''' Tests that properties of 'real' scalar reduction arguments defined
    in LFRicConstants are correctly set up in the DynKernelArgument class.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.9.1_X_innerproduct_Y_builtin.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kernel = schedule.kernels()[0]
    reduction_arg = kernel.arguments.args[0]

    assert reduction_arg.module_name == "scalar_mod"
    assert reduction_arg.data_type == "scalar_type"
    assert reduction_arg.proxy_data_type is None
    assert reduction_arg.intrinsic_type == "real"
    assert reduction_arg.precision == "r_def"


def test_multiple_updated_scalar_args():
    ''' Check that we raise the expected exception when we encounter a
    kernel that writes to more than one of its field and scalar arguments '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("arg_type(gh_scalar,   gh_real,    gh_read)",
                        "arg_type(gh_scalar,   gh_real,    gh_sum)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("A user-supplied LFRic kernel must not write/update a scalar "
            "argument but kernel 'testkern_qr_type' has a scalar "
            "argument with 'gh_sum' access." in str(excinfo.value))


def test_scalar_different_data_types_invoke():
    ''' Tests that the same scalar cannot have different data types
    in different kernels within the same Invoke.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "4.16_multikernel_invokes_real_int_scalar_invalid.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)

    const = LFRicConstants()
    with pytest.raises(GenerationError) as excinfo:
        _ = psy.gen
    assert (f"Scalar argument(s) ['b'] in Invoke "
            f"'invoke_real_and_integer_scalars' have different metadata for "
            f"data type ({const.VALID_SCALAR_DATA_TYPES}) in different "
            f"kernels. This is invalid." in str(excinfo.value))
