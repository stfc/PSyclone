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
Module containing pytest tests for PSy-layer code generation and the related
functionality for the LFRic fields.
'''

# Imports
from __future__ import absolute_import, print_function
import os
import pytest
import six
import fparser
from fparser import api as fpapi
from psyclone.core.access_type import AccessType
from psyclone.domain.lfric import LFRicArgDescriptor, FunctionSpace
from psyclone.dynamo0p3 import DynKernMetadata, LFRicFields
from psyclone.f2pygen import ModuleGen
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.parse.utils import ParseError
from psyclone.configuration import Config
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.errors import InternalError, GenerationError


# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "dynamo0p3")
TEST_API = "dynamo0.3"

# General field checks (argument type, data type, etc)

FIELD_CODE = '''
module testkern_field_mod
  type, extends(kernel_type) :: testkern_field_type
     type(arg_type), meta_args(6) =                             &
          (/ arg_type(gh_scalar, gh_real,    gh_read),          &
             arg_type(gh_field,  gh_real,    gh_inc,   w1),     &
             arg_type(gh_field,  gh_real,    gh_read,  w2),     &
             arg_type(gh_field,  gh_integer, gh_write, wtheta), &
             arg_type(gh_field,  gh_integer, gh_read,  w3),     &
             arg_type(gh_scalar, gh_integer, gh_read)           &
           /)
     type(func_type), dimension(2) :: meta_funcs =  &
          (/ func_type(w1, gh_basis),               &
             func_type(w3, gh_basis, gh_diff_basis) &
           /)
     integer :: operates_on = cell_column
     integer :: gh_shape = gh_quadrature_XYoZ
   contains
     procedure, nopass :: code => testkern_field_code
  end type testkern_field_type
contains
  subroutine testkern_field_code()
  end subroutine testkern_field_code
end module testkern_field_mod
'''


def test_ad_fld_type_1st_arg():
    ''' Tests that an error is raised when the first argument descriptor
    metadata for a field is invalid. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = FIELD_CODE.replace("arg_type(gh_field,  gh_real,    gh_inc,   w1)",
                              "arg_type(gh_hedge, gh_real, gh_inc, w1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_field_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("the 1st argument of a 'meta_arg' entry should be a valid "
            "argument type (one of {0}), but found 'gh_hedge'".
            format(LFRicArgDescriptor.VALID_ARG_TYPE_NAMES)
            in str(excinfo.value))


def test_ad_field_invalid_data_type():
    ''' Tests that an error is raised when the argument descriptor
    metadata for a field has an invalid data type. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_field_type"
    # Check real field
    code = FIELD_CODE.replace(
        "arg_type(gh_field,  gh_real,    gh_inc,   w1)",
        "arg_type(gh_field,  gh_unreal,  gh_inc,   w1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("In the LFRic API the 2nd argument of a 'meta_arg' entry should "
            "be a valid data type (one of {0}), but found 'gh_unreal' "
            "in 'arg_type(gh_field, gh_unreal, gh_inc, w1)'.".
            format(LFRicArgDescriptor.VALID_FIELD_DATA_TYPES)
            in str(excinfo.value))
    # Check integer field
    code = FIELD_CODE.replace(
        "arg_type(gh_field,  gh_integer, gh_read,  w3)",
        "arg_type(gh_field,  gh_double,  gh_read,  w3)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("but found 'gh_double' in 'arg_type(gh_field, gh_double, "
            "gh_read, w3)'." in str(excinfo.value))


def test_field_gh_sum_invalid():
    ''' Tests that an error is raised when a field is specified with
    access type 'gh_sum'. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = FIELD_CODE.replace("arg_type(gh_field,  gh_real,    gh_read,  w2)",
                              "arg_type(gh_field, gh_real, gh_sum, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_field_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("In the LFRic API, allowed accesses for fields on "
            "continuous function spaces that are arguments to kernels "
            "that operate on cell-columns are ['gh_read', 'gh_inc'], but "
            "found 'gh_sum' for 'w2'" in str(excinfo.value))


def test_ad_field_type_too_few_args():
    ''' Tests that an error is raised when the field argument descriptor
    metadata for a field has fewer than 3 args. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = FIELD_CODE.replace("arg_type(gh_field,  gh_real,    gh_inc,   w1)",
                              "arg_type(gh_field,  gh_real,    gh_inc)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_field_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("each 'meta_arg' entry must have at least 4 arguments if its "
            "first argument is of ['gh_field'] type" in str(excinfo.value))


def test_ad_fld_type_too_many_args():
    ''' Tests that an error is raised when the field argument descriptor
    metadata has more than 4 args. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = FIELD_CODE.replace(
        "arg_type(gh_field,  gh_real,    gh_inc,   w1)",
        "arg_type(gh_field,  gh_real,    gh_inc,   w1, w1, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_field_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("each 'meta_arg' entry must have at most 5 arguments if its "
            "first argument is of ['gh_field'] type" in str(excinfo.value))


def test_ad_field_init_wrong_type():
    ''' Test that an error is raised if something other than a field
    is passed to the LFRicArgDescriptor._init_field() method. '''
    ast = fpapi.parse(FIELD_CODE, ignore_comments=False)
    name = "testkern_field_type"
    metadata = DynKernMetadata(ast, name=name)
    # Get an argument which is not a field
    wrong_arg = metadata._inits[0]
    with pytest.raises(InternalError) as excinfo:
        LFRicArgDescriptor(
            wrong_arg, metadata.iterates_over)._init_field(
                wrong_arg, metadata.iterates_over)
    assert ("Expected a field argument but got an argument of type "
            "'gh_scalar'" in str(excinfo.value))


def test_ad_field_init_wrong_data_type(monkeypatch):
    ''' Test that an error is raised if an invalid data type
    is passed to the LFRicArgDescriptor._init_field() method. '''
    ast = fpapi.parse(FIELD_CODE, ignore_comments=False)
    name = "testkern_field_type"
    metadata = DynKernMetadata(ast, name=name)
    # Get a real field argument descriptor and set a wrong data type
    real_field_arg = metadata._inits[1]
    real_field_arg.args[1].name = "gh_double"
    # Get an integer field argument descriptor and set a wrong data type
    int_field_arg = metadata._inits[2]
    int_field_arg.args[1].name = "gh_double"
    # Now try to trip the error by making the initial test think
    # that 'gh_double' is actually a valid data type
    monkeypatch.setattr(
        target=LFRicArgDescriptor, name="VALID_ARG_DATA_TYPES",
        value=LFRicArgDescriptor.VALID_ARG_DATA_TYPES + ["gh_double"])
    # Check real field
    with pytest.raises(InternalError) as excinfo:
        LFRicArgDescriptor(
            real_field_arg, metadata.iterates_over)._init_field(
                real_field_arg, metadata.iterates_over)
    assert ("Expected one of {0} as the field data type but got 'gh_double'.".
            format(LFRicArgDescriptor.VALID_FIELD_DATA_TYPES) in
            str(excinfo.value))
    # Check integer field
    with pytest.raises(InternalError) as excinfo:
        LFRicArgDescriptor(
            int_field_arg, metadata.iterates_over)._init_field(
                int_field_arg, metadata.iterates_over)
    assert ("Expected one of {0} as the field data type but got 'gh_double'.".
            format(LFRicArgDescriptor.VALID_FIELD_DATA_TYPES) in
            str(excinfo.value))


def test_arg_descriptor_invalid_fs():
    ''' Tests that an error is raised when an invalid function space
    name is provided as the third argument for a field. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_field_type"
    # Check real field
    code = FIELD_CODE.replace(
        "arg_type(gh_field,  gh_real,    gh_inc,   w1)",
        "arg_type(gh_field,  gh_real,    gh_inc,   w4)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("In the LFRic API argument 4 of a 'meta_arg' field entry "
            "must be a valid function-space name (one of {0}) if its "
            "first argument is of ['gh_field'] type, but found 'w4'".
            format(FunctionSpace.VALID_FUNCTION_SPACE_NAMES)
            in str(excinfo.value))
    # Check integer field
    code = FIELD_CODE.replace(
        "arg_type(gh_field,  gh_integer, gh_read,  w3)",
        "arg_type(gh_field,  gh_integer, gh_read,  w10)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("if its first argument is of ['gh_field'] type, but found 'w10'"
            in str(excinfo.value))


def test_ad_field_init_wrong_iteration_space():
    ''' Test that an error is raised if a wrong iteration space
    (other than ['cell_column', 'dof']) is passed to the
    LFRicArgDescriptor._init_field() method.

    TODO #870 update this test with correct error msg once 'dofs' and
    'cells' are no longer permitted.

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(FIELD_CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_field_type")
    field_arg = metadata._inits[1]
    # Set a wrong iteration space
    with pytest.raises(InternalError) as excinfo:
        LFRicArgDescriptor(
            field_arg, metadata.iterates_over)._init_field(
                field_arg, "ncolours")
    assert ("Invalid operates_on 'ncolours' in the kernel metadata (expected "
            "one of ['cells', 'cell_column', 'domain', 'dofs', 'dof'])." in
            str(excinfo.value))


def test_fs_discontinuous_inc_error():
    ''' Test that an error is raised if a discontinuous function space
    and 'gh_inc' are provided for the same field in the metadata. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    for fspace in FunctionSpace.VALID_DISCONTINUOUS_NAMES:
        code = FIELD_CODE.replace(
            "arg_type(gh_field,  gh_integer, gh_read,  w3)",
            "arg_type(gh_field,  gh_integer, gh_inc, " + fspace + ")", 1)
        ast = fpapi.parse(code, ignore_comments=False)
        with pytest.raises(ParseError) as excinfo:
            _ = DynKernMetadata(ast, name="testkern_field_type")
        assert ("In the LFRic API, allowed accesses for fields on "
                "discontinuous function spaces that are arguments to kernels "
                "that operate on either cell-columns or the domain are "
                "['gh_read', 'gh_write', 'gh_readwrite'], but found 'gh_inc' "
                "for '{0}'".format(fspace) in str(excinfo.value))


def test_fs_continuous_cells_write_or_readwrite_error():
    ''' Test that an error is raised if a field on a continuous
    function space is specified as having an access of 'gh_write'
    or 'gh_readwrite' in kernel metadata.

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    for fspace in FunctionSpace.CONTINUOUS_FUNCTION_SPACES:
        for acc in ["gh_write", "gh_readwrite"]:
            code = FIELD_CODE.replace(
                "arg_type(gh_field,  gh_real,    gh_read,  w2)",
                "arg_type(gh_field, gh_real, " + acc + ", " + fspace + ")", 1)
            ast = fpapi.parse(code, ignore_comments=False)
            with pytest.raises(ParseError) as excinfo:
                _ = DynKernMetadata(ast, name="testkern_field_type")
            assert ("In the LFRic API, allowed accesses for fields on "
                    "continuous function spaces that are arguments to "
                    "kernels that operate on cell-columns are ['gh_read', "
                    "'gh_inc'], but found '{0}' for '{1}'".
                    format(acc, fspace) in str(excinfo.value))


def test_fs_anyspace_cells_write_or_readwrite_error():
    ''' Test that an error is raised if a field that is on 'any_space' "
    "(and therefore may be continuous) is specified as having 'gh_write' "
    "or 'gh_readwrite' access in the metadata.

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    for fspace in FunctionSpace.VALID_ANY_SPACE_NAMES:
        for acc in ["gh_write", "gh_readwrite"]:
            code = FIELD_CODE.replace(
                "arg_type(gh_field,  gh_real,    gh_read,  w2)",
                "arg_type(gh_field, gh_real, " + acc + ", " + fspace + ")", 1)
            ast = fpapi.parse(code, ignore_comments=False)
            with pytest.raises(ParseError) as excinfo:
                _ = DynKernMetadata(ast, name="testkern_field_type")
            assert ("In the LFRic API, allowed accesses for fields on "
                    "continuous function spaces that are arguments to "
                    "kernels that operate on cell-columns are ['gh_read', "
                    "'gh_inc'], but found '{0}' for '{1}'".
                    format(acc, fspace) in str(excinfo.value))


def test_fs_anyspace_dofs_inc_error():
    ''' Test that an error is raised if a field on 'any_space' with
    'gh_inc' access is specified for a kernel that operates on DoFs. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    dof_code = FIELD_CODE.replace("integer :: operates_on = cell_column",
                                  "integer :: operates_on = dof", 1)
    for fspace in FunctionSpace.VALID_ANY_SPACE_NAMES:
        code = dof_code.replace(
            "arg_type(gh_field,  gh_real,    gh_inc,   w1)",
            "arg_type(gh_field, gh_real, gh_inc, " + fspace + ")", 1)
        ast = fpapi.parse(code, ignore_comments=False)
        with pytest.raises(ParseError) as excinfo:
            _ = DynKernMetadata(ast, name="testkern_field_type")
        assert ("In the LFRic API, allowed field accesses for a kernel "
                "that operates on DoFs are ['gh_read', 'gh_write', "
                "'gh_readwrite'], but found 'gh_inc' for '{0}'".
                format(fspace) in str(excinfo.value))


def test_arg_descriptor_field():
    ''' Test that the LFRicArgDescriptor argument representation works
    as expected for a field argument. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(FIELD_CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_field_type")
    field_descriptor = metadata.arg_descriptors[1]

    # Assert correct string representation from LFRicArgDescriptor
    result = str(field_descriptor)
    expected_output = (
        "LFRicArgDescriptor object\n"
        "  argument_type[0]='gh_field'\n"
        "  data_type[1]='gh_real'\n"
        "  access_descriptor[2]='gh_inc'\n"
        "  function_space[3]='w1'")
    assert expected_output in result

    # Check LFRicArgDescriptor argument properties
    assert field_descriptor.argument_type == "gh_field"
    assert field_descriptor.data_type == "gh_real"
    assert field_descriptor.function_space == "w1"
    assert field_descriptor.function_spaces == ['w1']
    assert str(field_descriptor.access) == "INC"
    assert field_descriptor.mesh is None
    assert field_descriptor.stencil is None
    assert field_descriptor.vector_size == 1


def test_invalid_vector_operator():
    ''' Tests that an error is raised when a field vector does not
    use "*" as its operator. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = FIELD_CODE.replace(
        "(gh_field,  gh_real,    gh_inc,   w1)",
        "(gh_field+3,  gh_real,    gh_inc,  w1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_field_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert "must use '*' as the separator" in str(excinfo.value)


def test_invalid_vector_value_type():
    ''' Tests that an error is raised when a vector value is not a valid
    integer. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = FIELD_CODE.replace("(gh_field,  gh_real,    gh_inc,   w1)",
                              "(gh_field*n,  gh_real,    gh_inc,   w1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_field_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("the field vector notation must be in the format 'field*n' "
            "where 'n' is an integer, but the following 'n' was found "
            in str(excinfo.value))


def test_invalid_vector_value_range():
    ''' Tests that an error is raised when a vector value is not a valid
    value (<2). '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = FIELD_CODE.replace("(gh_field,  gh_real,    gh_inc,   w1)",
                              "(gh_field*1,  gh_real,    gh_inc,   w1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_field_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("the 1st argument of a 'meta_arg' entry may be a field vector "
            "with format 'field*n' where n is an integer > 1. However, "
            "found n = 1" in str(excinfo.value))

# Testing that an error is raised when a vector value is not provided is
# not required here as it causes a parse error in the generic code.


def test_arg_descriptor_field_vector():
    ''' Test that the LFRicArgDescriptor argument representation works
    as expected when we have a field vector. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    # Change the meta-data so that the second argument is a vector
    code = FIELD_CODE.replace("(gh_field,  gh_real,    gh_inc,   w1)",
                              "(gh_field*3,  gh_real,    gh_inc,  w1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_field_type"
    dkm = DynKernMetadata(ast, name=name)
    field_descriptor = dkm.arg_descriptors[1]

    # Assert correct string representation from LFRicArgDescriptor
    field_descriptor_str = str(field_descriptor)
    expected = (
        "LFRicArgDescriptor object\n"
        "  argument_type[0]='gh_field'*3\n"
        "  data_type[1]='gh_real'\n"
        "  access_descriptor[2]='gh_inc'\n"
        "  function_space[3]='w1'")
    assert expected in field_descriptor_str

    # Check LFRicArgDescriptor argument properties
    assert field_descriptor.argument_type == "gh_field"
    assert field_descriptor.data_type == "gh_real"
    assert field_descriptor.function_space == "w1"
    assert field_descriptor.function_spaces == ['w1']
    assert str(field_descriptor.access) == "INC"
    assert field_descriptor.mesh is None
    assert field_descriptor.stencil is None
    assert field_descriptor.vector_size == 3


def test_lfricfields_call_err():
    ''' Check that the LFRicFields constructor raises the expected internal
    error if it encounters an unrecognised intrinsic type of a field
    argument when generating a kernel call.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1.5_single_invoke_fs.f90"), api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    kernel = invoke.schedule.coded_kernels()[0]
    # Sabotage the field argument to make it have an invalid intrinsic type
    fld_arg = kernel.arguments.args[0]
    fld_arg._intrinsic_type = "triple-type"
    with pytest.raises(InternalError) as err:
        LFRicFields(invoke)._invoke_declarations(ModuleGen(name="my_mod"))
    test_str = str(err.value)
    if six.PY2:
        test_str = test_str.replace("u'", "'")
    assert ("Found unsupported intrinsic types for the field arguments "
            "['f1'] to Invoke 'invoke_0_testkern_fs_type'. Supported "
            "types are ['real', 'integer']." in test_str)


def test_dyninvoke_uniq_declns_intent_fields():
    ''' Tests that DynInvoke.unique_declns_by_intent() returns the correct
    list of arguments for 'gh_field' argument type. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    args = psy.invokes.invoke_list[0].unique_declns_by_intent(["gh_field"])
    args_inout = [arg.declaration_name for arg in args['inout']]
    assert args_inout == ['f1']
    assert args['out'] == []
    args_in = [arg.declaration_name for arg in args['in']]
    assert args_in == ['f2', 'm1', 'm2']


def test_field_invoke_uniq_declns_valid_intrinsic():
    ''' Tests that all valid intrinsic types for user-defined field arguments
    ('real' and 'integer') are accepted by Invoke.unique_declarations().

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "4.14_multikernel_invokes_real_int_field_fs.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]

    # Return 'real'-valued fields
    fields_real_args = invoke.unique_declarations(
        LFRicArgDescriptor.VALID_FIELD_NAMES, intrinsic_type="real")
    fields_real = [arg.declaration_name for arg in fields_real_args]
    assert fields_real == ["f1", "f2", "m1", "m2", "f3", "f4", "m3",
                           "m4", "f5", "f6", "m5", "m6", "m7"]

    # Return 'integer'-valued fields
    fields_int_args = invoke.unique_declarations(
        LFRicArgDescriptor.VALID_FIELD_NAMES, intrinsic_type="integer")
    fields_int = [arg.declaration_name for arg in fields_int_args]
    assert fields_int == ["i1", "i2", "n1", "n2", "i3", "i4", "n3", "n4",
                          "i5", "i6", "n5", "n6", "i7", "i8", "n7"]


def test_multiple_updated_field_args():
    ''' Check that we successfully parse a kernel that writes to more
    than one of its field arguments '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = FIELD_CODE.replace("arg_type(gh_field,  gh_real,    gh_read,  w2)",
                              "arg_type(gh_field, gh_real, gh_inc, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_field_type"
    metadata = DynKernMetadata(ast, name=name)
    count = 0
    for descriptor in metadata.arg_descriptors:
        if descriptor.argument_type == "gh_field" and \
                descriptor.access != AccessType.READ:
            count += 1
    assert count == 3


def test_field_arg_discontinuous(monkeypatch, annexed):
    ''' Test that the discontinuous method in the LFRic API argument
    class returns the correct values. Check that the code is generated
    correctly when annexed DoFs are and are not computed by default as
    the number of halo exchanges produced is different in the two
    cases.

    '''

    # 1) Discontinuous fields return true
    # 1a) Check w3, wtheta and w2v in turn
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    if annexed:
        # no halo exchanges produced for the w3 example (reads from
        # continuous spaces)
        idchld_list = [0, 0, 0]
    else:
        # 3 halo exchanges produced for the w3 example (reads from
        # continuous spaces)
        idchld_list = [3, 0, 0]
    idarg_list = [4, 0, 0]
    fs_dict = dict(zip(FunctionSpace.DISCONTINUOUS_FUNCTION_SPACES[0:3],
                       zip(idchld_list, idarg_list)))
    for fspace in fs_dict.keys():
        filename = "1_single_invoke_" + fspace + ".f90"
        idchld = fs_dict[fspace][0]
        idarg = fs_dict[fspace][1]
        _, info = parse(os.path.join(BASE_PATH, filename),
                        api=TEST_API)
        psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
        schedule = psy.invokes.invoke_list[0].schedule
        kernel = schedule.children[idchld].loop_body[0]
        field = kernel.arguments.args[idarg]
        assert field.space == fspace
        assert field.discontinuous

    # 1b) w2broken, w2vtrace and wchi return true
    _, info = parse(
        os.path.join(BASE_PATH, "1.5.1_single_invoke_write_multi_fs.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    if annexed:
        index = 12
    else:
        index = 13
    kernel = schedule.children[index].loop_body[0]
    # Test w2broken
    field = kernel.arguments.args[7]
    assert field.space == 'w2broken'
    assert field.discontinuous
    # Test w2vtrace
    field = kernel.arguments.args[11]
    assert field.space == 'w2vtrace'
    assert field.discontinuous
    # Test wchi
    field = kernel.arguments.args[4]
    assert field.space == 'wchi'
    assert not field.discontinuous

    # 1c) any_discontinuous_space returns true
    _, info = parse(
        os.path.join(BASE_PATH,
                     "1_single_invoke_any_discontinuous_space.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    if annexed:
        index = 0
    else:
        index = 2
    kernel = schedule.children[index].loop_body[0]
    field = kernel.arguments.args[0]
    assert field.space == 'any_discontinuous_space_1'
    assert field.discontinuous

    # 2) any_space field returns false
    _, info = parse(os.path.join(BASE_PATH, "11_any_space.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    if annexed:
        index = 4
    else:
        index = 5
    kernel = schedule.children[index].loop_body[0]
    field = kernel.arguments.args[0]
    assert field.space == 'any_space_1'
    assert not field.discontinuous

    # 3) Continuous field returns false
    # 3a) Test w1
    _, info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    if annexed:
        index = 3
    else:
        index = 4
    kernel = schedule.children[index].loop_body[0]
    field = kernel.arguments.args[1]
    assert field.space == 'w1'
    assert not field.discontinuous
    # 3b) Test w2trace and w2htrace
    _, info = parse(
        os.path.join(BASE_PATH,
                     "1.5.4_single_invoke_write_anyspace_w2trace.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    if annexed:
        index = 6
    else:
        index = 8
    kernel = schedule.children[index].loop_body[0]
    # Test w2trace
    field = kernel.arguments.args[3]
    assert field.space == 'w2trace'
    assert not field.discontinuous
    # Test w2htrace
    field = kernel.arguments.args[7]
    assert field.space == 'w2htrace'
    assert not field.discontinuous


# Tests for PSy-layer code generation for field arguments


def test_field(tmpdir):
    ''' Tests that a call with a set of fields, no basis functions and
    no distributed memory, produces correct code.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    generated_code = psy.gen
    output = (
        "  MODULE single_invoke_psy\n"
        "    USE constants_mod, ONLY: r_def, i_def\n"
        "    USE field_mod, ONLY: field_type, field_proxy_type\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_testkern_type(a, f1, f2, m1, m2)\n"
        "      USE testkern_mod, ONLY: testkern_code\n"
        "      REAL(KIND=r_def), intent(in) :: a\n"
        "      TYPE(field_type), intent(in) :: f1, f2, m1, m2\n"
        "      INTEGER(KIND=i_def) cell\n"
        "      INTEGER(KIND=i_def) nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, m2_proxy\n"
        "      INTEGER(KIND=i_def), pointer :: map_w1(:,:) => null(), "
        "map_w2(:,:) => null(), map_w3(:,:) => null()\n"
        "      INTEGER(KIND=i_def) ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, "
        "undf_w3\n"
        "      !\n"
        "      ! Initialise field and/or operator proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      m1_proxy = m1%get_proxy()\n"
        "      m2_proxy = m2%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w1\n"
        "      !\n"
        "      ndf_w1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_w1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2\n"
        "      !\n"
        "      ndf_w2 = f2_proxy%vspace%get_ndf()\n"
        "      undf_w2 = f2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w3\n"
        "      !\n"
        "      ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "      undf_w3 = m2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO cell=1,f1_proxy%vspace%get_ncell()\n"
        "        !\n"
        "        CALL testkern_code(nlayers, a, f1_proxy%data, f2_proxy%data, "
        "m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))\n"
        "      END DO\n"
        "      !\n"
        "    END SUBROUTINE invoke_0_testkern_type\n"
        "  END MODULE single_invoke_psy")
    assert output in str(generated_code)


def test_field_deref(tmpdir, dist_mem):
    ''' Tests that a call with a set of fields (some obtained by
    de-referencing derived types) and no basis functions produces
    correct code.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.13_single_invoke_field_deref.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    generated_code = str(psy.gen)
    output = (
        "    SUBROUTINE invoke_0_testkern_type(a, f1, est_f2, m1, "
        "est_m2)\n"
        "      USE testkern_mod, ONLY: testkern_code\n")
    assert output in generated_code
    if dist_mem:
        output = "      USE mesh_mod, ONLY: mesh_type\n"
        assert output in generated_code

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output = (
        "      REAL(KIND=r_def), intent(in) :: a\n"
        "      TYPE(field_type), intent(in) :: f1, est_f2, m1, est_m2\n"
        "      INTEGER(KIND=i_def) cell\n"
        "      INTEGER(KIND=i_def) nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy, est_f2_proxy, m1_proxy, "
        "est_m2_proxy\n"
        "      INTEGER(KIND=i_def), pointer :: map_w1(:,:) => null(), "
        "map_w2(:,:) => null(), map_w3(:,:) => null()\n"
        "      INTEGER(KIND=i_def) ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, "
        "undf_w3\n")
    assert output in generated_code
    if dist_mem:
        output = "      TYPE(mesh_type), pointer :: mesh => null()\n"
        assert output in generated_code
    output = (
        "      !\n"
        "      ! Initialise field and/or operator proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      est_f2_proxy = est_f2%get_proxy()\n"
        "      m1_proxy = m1%get_proxy()\n"
        "      est_m2_proxy = est_m2%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n")
    assert output in generated_code
    if dist_mem:
        output = (
            "      !\n"
            "      ! Create a mesh object\n"
            "      !\n"
            "      mesh => f1_proxy%vspace%get_mesh()\n"
        )
        assert output in generated_code
    output = (
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2 => est_f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => est_m2_proxy%vspace%get_whole_dofmap()\n"
        "      !\n")
    assert output in generated_code
    output = (
        "      ! Initialise number of DoFs for w1\n"
        "      !\n"
        "      ndf_w1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_w1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2\n"
        "      !\n"
        "      ndf_w2 = est_f2_proxy%vspace%get_ndf()\n"
        "      undf_w2 = est_f2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w3\n"
        "      !\n"
        "      ndf_w3 = est_m2_proxy%vspace%get_ndf()\n"
        "      undf_w3 = est_m2_proxy%vspace%get_undf()\n"
        "      !\n")
    assert output in generated_code
    if dist_mem:
        output = (
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      IF (f1_proxy%is_dirty(depth=1)) THEN\n"
            "        CALL f1_proxy%halo_exchange(depth=1)\n"
            "      END IF\n"
            "      !\n"
            "      IF (est_f2_proxy%is_dirty(depth=1)) THEN\n"
            "        CALL est_f2_proxy%halo_exchange(depth=1)\n"
            "      END IF\n"
            "      !\n"
            "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
            "        CALL m1_proxy%halo_exchange(depth=1)\n"
            "      END IF\n"
            "      !\n"
            "      IF (est_m2_proxy%is_dirty(depth=1)) THEN\n"
            "        CALL est_m2_proxy%halo_exchange(depth=1)\n"
            "      END IF\n"
            "      !\n"
            "      DO cell=1,mesh%get_last_halo_cell(1)\n")
        assert output in generated_code
    else:
        output = (
            "      ! Call our kernels\n"
            "      !\n"
            "      DO cell=1,f1_proxy%vspace%get_ncell()\n")
        assert output in generated_code
    output = (
        "        !\n"
        "        CALL testkern_code(nlayers, a, f1_proxy%data, "
        "est_f2_proxy%data, m1_proxy%data, est_m2_proxy%data, ndf_w1, "
        "undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), "
        "ndf_w3, undf_w3, map_w3(:,cell))\n"
        "      END DO\n")
    assert output in generated_code
    if dist_mem:
        output = (
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      !")
        assert output in generated_code


def test_field_fs(tmpdir):
    ''' Tests that a call with a set of fields making use of all
    function spaces and no basis functions produces correct code.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1.5_single_invoke_fs.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    generated_code = str(psy.gen)
    output = (
        "  MODULE single_invoke_fs_psy\n"
        "    USE constants_mod, ONLY: r_def, i_def\n"
        "    USE field_mod, ONLY: field_type, field_proxy_type\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_testkern_fs_type(f1, f2, m1, m2, f3, f4, "
        "m3, m4, f5, f6, m5, m6, m7)\n"
        "      USE testkern_fs_mod, ONLY: testkern_fs_code\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      TYPE(field_type), intent(in) :: f1, f2, m1, m2, f3, f4, m3, "
        "m4, f5, f6, m5, m6, m7\n"
        "      INTEGER(KIND=i_def) cell\n"
        "      INTEGER(KIND=i_def) nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, "
        "m2_proxy, f3_proxy, f4_proxy, m3_proxy, m4_proxy, f5_proxy, "
        "f6_proxy, m5_proxy, m6_proxy, m7_proxy\n"
        "      INTEGER(KIND=i_def), pointer :: map_any_w2(:,:) => null(), "
        "map_w0(:,:) => null(), map_w1(:,:) => null(), map_w2(:,:) => "
        "null(), map_w2broken(:,:) => null(), map_w2h(:,:) => null(), "
        "map_w2htrace(:,:) => null(), map_w2trace(:,:) => null(), "
        "map_w2v(:,:) => null(), map_w2vtrace(:,:) => null(), map_w3(:,:) "
        "=> null(), map_wchi(:,:) => null(), map_wtheta(:,:) => null()\n"
        "      INTEGER(KIND=i_def) ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w0, "
        "undf_w0, ndf_w3, undf_w3, ndf_wtheta, undf_wtheta, ndf_w2h, "
        "undf_w2h, ndf_w2v, undf_w2v, ndf_w2broken, undf_w2broken, "
        "ndf_w2trace, undf_w2trace, ndf_w2htrace, undf_w2htrace, "
        "ndf_w2vtrace, undf_w2vtrace, ndf_wchi, undf_wchi, ndf_any_w2, "
        "undf_any_w2\n"
        "      TYPE(mesh_type), pointer :: mesh => null()\n")
    assert output in generated_code
    output = (
        "      ! Initialise field and/or operator proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      m1_proxy = m1%get_proxy()\n"
        "      m2_proxy = m2%get_proxy()\n"
        "      f3_proxy = f3%get_proxy()\n"
        "      f4_proxy = f4%get_proxy()\n"
        "      m3_proxy = m3%get_proxy()\n"
        "      m4_proxy = m4%get_proxy()\n"
        "      f5_proxy = f5%get_proxy()\n"
        "      f6_proxy = f6%get_proxy()\n"
        "      m5_proxy = m5%get_proxy()\n"
        "      m6_proxy = m6%get_proxy()\n"
        "      m7_proxy = m7%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh => f1_proxy%vspace%get_mesh()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w0 => m1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "      map_wtheta => f3_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2h => f4_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2v => m3_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2broken => m4_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2trace => f5_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2htrace => f6_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2vtrace => m5_proxy%vspace%get_whole_dofmap()\n"
        "      map_wchi => m6_proxy%vspace%get_whole_dofmap()\n"
        "      map_any_w2 => m7_proxy%vspace%get_whole_dofmap()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w1\n"
        "      !\n"
        "      ndf_w1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_w1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2\n"
        "      !\n"
        "      ndf_w2 = f2_proxy%vspace%get_ndf()\n"
        "      undf_w2 = f2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w0\n"
        "      !\n"
        "      ndf_w0 = m1_proxy%vspace%get_ndf()\n"
        "      undf_w0 = m1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w3\n"
        "      !\n"
        "      ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "      undf_w3 = m2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for wtheta\n"
        "      !\n"
        "      ndf_wtheta = f3_proxy%vspace%get_ndf()\n"
        "      undf_wtheta = f3_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2h\n"
        "      !\n"
        "      ndf_w2h = f4_proxy%vspace%get_ndf()\n"
        "      undf_w2h = f4_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2v\n"
        "      !\n"
        "      ndf_w2v = m3_proxy%vspace%get_ndf()\n"
        "      undf_w2v = m3_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2broken\n"
        "      !\n"
        "      ndf_w2broken = m4_proxy%vspace%get_ndf()\n"
        "      undf_w2broken = m4_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2trace\n"
        "      !\n"
        "      ndf_w2trace = f5_proxy%vspace%get_ndf()\n"
        "      undf_w2trace = f5_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2htrace\n"
        "      !\n"
        "      ndf_w2htrace = f6_proxy%vspace%get_ndf()\n"
        "      undf_w2htrace = f6_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2vtrace\n"
        "      !\n"
        "      ndf_w2vtrace = m5_proxy%vspace%get_ndf()\n"
        "      undf_w2vtrace = m5_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for wchi\n"
        "      !\n"
        "      ndf_wchi = m6_proxy%vspace%get_ndf()\n"
        "      undf_wchi = m6_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for any_w2\n"
        "      !\n"
        "      ndf_any_w2 = m7_proxy%vspace%get_ndf()\n"
        "      undf_any_w2 = m7_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call kernels and communication routines\n"
        "      !\n"
        "      IF (f1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f1_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
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
        "      IF (f4_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f4_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m3_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m3_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m4_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m4_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f5_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f5_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f6_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f6_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m5_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m5_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m6_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m6_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m7_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m7_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        CALL testkern_fs_code(nlayers, f1_proxy%data, f2_proxy%data, "
        "m1_proxy%data, m2_proxy%data, f3_proxy%data, f4_proxy%data, "
        "m3_proxy%data, m4_proxy%data, f5_proxy%data, f6_proxy%data, "
        "m5_proxy%data, m6_proxy%data, m7_proxy%data, ndf_w1, undf_w1, "
        "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w0, undf_w0, "
        "map_w0(:,cell), ndf_w3, undf_w3, map_w3(:,cell), ndf_wtheta, "
        "undf_wtheta, map_wtheta(:,cell), ndf_w2h, undf_w2h, "
        "map_w2h(:,cell), ndf_w2v, undf_w2v, map_w2v(:,cell), ndf_w2broken, "
        "undf_w2broken, map_w2broken(:,cell), ndf_w2trace, undf_w2trace, "
        "map_w2trace(:,cell), ndf_w2htrace, undf_w2htrace, "
        "map_w2htrace(:,cell), ndf_w2vtrace, undf_w2vtrace, "
        "map_w2vtrace(:,cell), ndf_wchi, undf_wchi, map_wchi(:,cell), "
        "ndf_any_w2, undf_any_w2, map_any_w2(:,cell))\n"
        "      END DO\n"
        "      !\n"
        "      ! Set halos dirty/clean for fields modified in the above loop\n"
        "      !\n"
        "      CALL f1_proxy%set_dirty()\n"
        "      CALL f3_proxy%set_dirty()\n"
        "      CALL f3_proxy%set_clean(1)\n"
        "      !\n"
        "      !\n"
        "    END SUBROUTINE invoke_0_testkern_fs_type\n"
        "  END MODULE single_invoke_fs_psy")
    assert output in generated_code


def test_vector_field(tmpdir):
    ''' Tests that a vector field is declared correctly in the PSy
    layer. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "8_vector_field.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert ("SUBROUTINE invoke_0_testkern_coord_w0_type(f1, chi, f2)" in
            generated_code)
    assert "TYPE(field_type), intent(in) :: f1, chi(3), f2" in generated_code


def test_vector_field_2(tmpdir):
    ''' Tests that a vector field is indexed correctly in the PSy layer. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "8_vector_field_2.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # All references to chi_proxy should be chi_proxy(1)
    assert "chi_proxy%" not in generated_code
    assert generated_code.count("chi_proxy(1)%vspace") == 5
    # Use each chi field individually in the kernel
    assert ("chi_proxy(1)%data, chi_proxy(2)%data, chi_proxy(3)%data" in
            generated_code)


def test_vector_field_deref(tmpdir, dist_mem):
    ''' Tests that a vector field is declared correctly in the PSy
    layer when it is obtained by de-referencing a derived type in the
    Algorithm layer.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "8.1_vector_field_deref.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    generated_code = str(psy.gen)
    assert ("SUBROUTINE invoke_0_testkern_coord_w0_type(f1, box_chi, f2)" in
            generated_code)
    assert ("TYPE(field_type), intent(in) :: f1, box_chi(3), f2" in
            generated_code)

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_mkern_invoke_vec_fields():
    ''' Test that correct code is produced when there are multiple
    kernels within an invoke with vector fields '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.2_multikernel_invokes.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)
    # 1st test for duplication of name vector-field declaration
    assert ("TYPE(field_type), intent(in) :: f1, chi(3), chi(3)"
            not in generated_code)
    # 2nd test for duplication of name vector-field declaration
    assert ("TYPE(field_proxy_type) f1_proxy, chi_proxy(3), chi_proxy(3)"
            not in generated_code)


# Tests for Invokes calling kernels that contain integer-valued fields


def test_int_field_fs(tmpdir):
    ''' Tests that a call with a set of integer-valued fields making use of
    all function spaces and no basis functions produces correct code. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.5.5_single_invoke_write_multi_fs_int_field.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    generated_code = str(psy.gen)
    output = (
        "  MODULE single_invoke_fs_int_field_psy\n"
        "    USE constants_mod, ONLY: r_def, i_def\n"
        "    USE integer_field_mod, ONLY: integer_field_type, "
        "integer_field_proxy_type\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_testkern_fs_int_field_type(f1, f2, m1, m2, "
        "f3, f4, m3, m4, f5, f6, m5, m6, f7, f8, m7)\n"
        "      USE testkern_fs_int_field_mod, ONLY: "
        "testkern_fs_int_field_code\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      TYPE(integer_field_type), intent(in) :: f1, f2, m1, m2, f3, "
        "f4, m3, m4, f5, f6, m5, m6, f7, f8, m7\n"
        "      INTEGER(KIND=i_def) cell\n"
        "      INTEGER(KIND=i_def) nlayers\n"
        "      TYPE(integer_field_proxy_type) f1_proxy, f2_proxy, m1_proxy, "
        "m2_proxy, f3_proxy, f4_proxy, m3_proxy, m4_proxy, f5_proxy, "
        "f6_proxy, m5_proxy, m6_proxy, f7_proxy, f8_proxy, m7_proxy\n"
        "      INTEGER(KIND=i_def), pointer :: map_adspc1_m7(:,:) => null(), "
        "map_any_w2(:,:) => null(), map_aspc1_f8(:,:) => null(), "
        "map_w0(:,:) => null(), map_w1(:,:) => null(), map_w2(:,:) => "
        "null(), map_w2broken(:,:) => null(), map_w2h(:,:) => null(), "
        "map_w2htrace(:,:) => null(), map_w2trace(:,:) => null(), "
        "map_w2v(:,:) => null(), map_w2vtrace(:,:) => null(), map_w3(:,:) "
        "=> null(), map_wchi(:,:) => null(), map_wtheta(:,:) => null()\n"
        "      INTEGER(KIND=i_def) ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w0, "
        "undf_w0, ndf_w3, undf_w3, ndf_wtheta, undf_wtheta, ndf_w2h, "
        "undf_w2h, ndf_w2v, undf_w2v, ndf_w2broken, undf_w2broken, "
        "ndf_w2trace, undf_w2trace, ndf_w2htrace, undf_w2htrace, "
        "ndf_w2vtrace, undf_w2vtrace, ndf_wchi, undf_wchi, ndf_any_w2, "
        "undf_any_w2, ndf_aspc1_f8, undf_aspc1_f8, ndf_adspc1_m7, "
        "undf_adspc1_m7\n"
        "      TYPE(mesh_type), pointer :: mesh => null()\n")
    assert output in generated_code
    output = (
        "      ! Initialise field and/or operator proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      m1_proxy = m1%get_proxy()\n"
        "      m2_proxy = m2%get_proxy()\n"
        "      f3_proxy = f3%get_proxy()\n"
        "      f4_proxy = f4%get_proxy()\n"
        "      m3_proxy = m3%get_proxy()\n"
        "      m4_proxy = m4%get_proxy()\n"
        "      f5_proxy = f5%get_proxy()\n"
        "      f6_proxy = f6%get_proxy()\n"
        "      m5_proxy = m5%get_proxy()\n"
        "      m6_proxy = m6%get_proxy()\n"
        "      f7_proxy = f7%get_proxy()\n"
        "      f8_proxy = f8%get_proxy()\n"
        "      m7_proxy = m7%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh => f1_proxy%vspace%get_mesh()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w0 => m1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "      map_wtheta => f3_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2h => f4_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2v => m3_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2broken => m4_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2trace => f5_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2htrace => f6_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2vtrace => m5_proxy%vspace%get_whole_dofmap()\n"
        "      map_wchi => m6_proxy%vspace%get_whole_dofmap()\n"
        "      map_any_w2 => f7_proxy%vspace%get_whole_dofmap()\n"
        "      map_aspc1_f8 => f8_proxy%vspace%get_whole_dofmap()\n"
        "      map_adspc1_m7 => m7_proxy%vspace%get_whole_dofmap()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w1\n"
        "      !\n"
        "      ndf_w1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_w1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2\n"
        "      !\n"
        "      ndf_w2 = f2_proxy%vspace%get_ndf()\n"
        "      undf_w2 = f2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w0\n"
        "      !\n"
        "      ndf_w0 = m1_proxy%vspace%get_ndf()\n"
        "      undf_w0 = m1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w3\n"
        "      !\n"
        "      ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "      undf_w3 = m2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for wtheta\n"
        "      !\n"
        "      ndf_wtheta = f3_proxy%vspace%get_ndf()\n"
        "      undf_wtheta = f3_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2h\n"
        "      !\n"
        "      ndf_w2h = f4_proxy%vspace%get_ndf()\n"
        "      undf_w2h = f4_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2v\n"
        "      !\n"
        "      ndf_w2v = m3_proxy%vspace%get_ndf()\n"
        "      undf_w2v = m3_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2broken\n"
        "      !\n"
        "      ndf_w2broken = m4_proxy%vspace%get_ndf()\n"
        "      undf_w2broken = m4_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2trace\n"
        "      !\n"
        "      ndf_w2trace = f5_proxy%vspace%get_ndf()\n"
        "      undf_w2trace = f5_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2htrace\n"
        "      !\n"
        "      ndf_w2htrace = f6_proxy%vspace%get_ndf()\n"
        "      undf_w2htrace = f6_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2vtrace\n"
        "      !\n"
        "      ndf_w2vtrace = m5_proxy%vspace%get_ndf()\n"
        "      undf_w2vtrace = m5_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for wchi\n"
        "      !\n"
        "      ndf_wchi = m6_proxy%vspace%get_ndf()\n"
        "      undf_wchi = m6_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for any_w2\n"
        "      !\n"
        "      ndf_any_w2 = f7_proxy%vspace%get_ndf()\n"
        "      undf_any_w2 = f7_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for aspc1_f8\n"
        "      !\n"
        "      ndf_aspc1_f8 = f8_proxy%vspace%get_ndf()\n"
        "      undf_aspc1_f8 = f8_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for adspc1_m7\n"
        "      !\n"
        "      ndf_adspc1_m7 = m7_proxy%vspace%get_ndf()\n"
        "      undf_adspc1_m7 = m7_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call kernels and communication routines\n"
        "      !\n"
        "      IF (f1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f1_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
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
        "      IF (f4_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f4_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m3_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m3_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m4_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m4_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f5_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f5_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f6_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f6_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m5_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m5_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m6_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m6_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f7_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f7_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f8_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f8_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m7_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m7_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        CALL testkern_fs_int_field_code(nlayers, f1_proxy%data, "
        "f2_proxy%data, m1_proxy%data, m2_proxy%data, f3_proxy%data, "
        "f4_proxy%data, m3_proxy%data, m4_proxy%data, f5_proxy%data, "
        "f6_proxy%data, m5_proxy%data, m6_proxy%data, f7_proxy%data, "
        "f8_proxy%data, m7_proxy%data, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w0, undf_w0, map_w0(:,cell), "
        "ndf_w3, undf_w3, map_w3(:,cell), ndf_wtheta, undf_wtheta, "
        "map_wtheta(:,cell), ndf_w2h, undf_w2h, map_w2h(:,cell), ndf_w2v, "
        "undf_w2v, map_w2v(:,cell), ndf_w2broken, undf_w2broken, "
        "map_w2broken(:,cell), ndf_w2trace, undf_w2trace, "
        "map_w2trace(:,cell), ndf_w2htrace, undf_w2htrace, "
        "map_w2htrace(:,cell), ndf_w2vtrace, undf_w2vtrace, "
        "map_w2vtrace(:,cell), ndf_wchi, undf_wchi, map_wchi(:,cell), "
        "ndf_any_w2, undf_any_w2, map_any_w2(:,cell), ndf_aspc1_f8, "
        "undf_aspc1_f8, map_aspc1_f8(:,cell), ndf_adspc1_m7, "
        "undf_adspc1_m7, map_adspc1_m7(:,cell))\n"
        "      END DO\n"
        "      !\n"
        "      ! Set halos dirty/clean for fields modified in the above loop\n"
        "      !\n"
        "      CALL f2_proxy%set_dirty()\n"
        "      CALL f3_proxy%set_dirty()\n"
        "      CALL f8_proxy%set_dirty()\n"
        "      CALL m7_proxy%set_dirty()\n"
        "      CALL f3_proxy%set_clean(1)\n"
        "      CALL m7_proxy%set_clean(1)\n"
        "      !\n"
        "      !\n"
        "    END SUBROUTINE invoke_0_testkern_fs_int_field_type\n"
        "  END MODULE single_invoke_fs_int_field_psy")
    assert output in generated_code


def test_int_field_2qr_shapes(dist_mem, tmpdir):
    ''' Check that we can generate correct call for a kernel that requires
    two types of quadrature (here XYoZ and face) for integer fields
    Note: Basis and differential basis functions for all fields (real and
    integer) are real-valued.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.1.9_single_invoke_2qr_shapes_int_field.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(invoke_info)
    assert LFRicBuild(tmpdir).code_compiles(psy)
    gen_code = str(psy.gen)
    # Check that the qr-related variables are all declared
    assert ("      TYPE(quadrature_xyoz_type), intent(in) :: qr_xyoz\n"
            "      TYPE(quadrature_face_type), intent(in) :: qr_face\n"
            in gen_code)
    assert ("REAL(KIND=r_def), allocatable :: basis_w2_qr_xyoz(:,:,:,:), "
            "basis_w2_qr_face(:,:,:,:), diff_basis_wchi_qr_xyoz(:,:,:,:), "
            "diff_basis_wchi_qr_face(:,:,:,:), "
            "basis_adspc1_f3_qr_xyoz(:,:,:,:), "
            "diff_basis_adspc1_f3_qr_xyoz(:,:,:,:), "
            "basis_adspc1_f3_qr_face(:,:,:,:), "
            "diff_basis_adspc1_f3_qr_face(:,:,:,:)\n" in gen_code)
    assert ("      REAL(KIND=r_def), pointer :: weights_xyz_qr_face(:,:) "
            "=> null()\n"
            "      INTEGER(KIND=i_def) np_xyz_qr_face, nfaces_qr_face\n"
            "      REAL(KIND=r_def), pointer :: weights_xy_qr_xyoz(:) => "
            "null(), weights_z_qr_xyoz(:) => null()\n"
            "      INTEGER(KIND=i_def) np_xy_qr_xyoz, np_z_qr_xyoz\n"

            in gen_code)
    assert ("      TYPE(quadrature_face_proxy_type) qr_face_proxy\n"
            "      TYPE(quadrature_xyoz_proxy_type) qr_xyoz_proxy\n"
            in gen_code)
    # Allocation and computation of (some of) the basis/differential
    # basis functions
    assert ("      ALLOCATE (basis_adspc1_f3_qr_xyoz(dim_adspc1_f3, "
            "ndf_adspc1_f3, np_xy_qr_xyoz, np_z_qr_xyoz))\n"
            "      ALLOCATE (diff_basis_adspc1_f3_qr_xyoz(diff_dim_adspc1_f3, "
            "ndf_adspc1_f3, np_xy_qr_xyoz, np_z_qr_xyoz))\n"
            "      ALLOCATE (basis_adspc1_f3_qr_face(dim_adspc1_f3, "
            "ndf_adspc1_f3, np_xyz_qr_face, nfaces_qr_face))\n"
            "      ALLOCATE (diff_basis_adspc1_f3_qr_face(diff_dim_adspc1_f3, "
            "ndf_adspc1_f3, np_xyz_qr_face, nfaces_qr_face))\n"
            in gen_code)
    assert ("      CALL qr_xyoz%compute_function(BASIS, f3_proxy%vspace, "
            "dim_adspc1_f3, ndf_adspc1_f3, basis_adspc1_f3_qr_xyoz)\n"
            "      CALL qr_xyoz%compute_function(DIFF_BASIS, "
            "f3_proxy%vspace, diff_dim_adspc1_f3, ndf_adspc1_f3, "
            "diff_basis_adspc1_f3_qr_xyoz)\n"
            "      CALL qr_face%compute_function(BASIS, f3_proxy%vspace, "
            "dim_adspc1_f3, ndf_adspc1_f3, basis_adspc1_f3_qr_face)\n"
            "      CALL qr_face%compute_function(DIFF_BASIS, "
            "f3_proxy%vspace, diff_dim_adspc1_f3, ndf_adspc1_f3, "
            "diff_basis_adspc1_f3_qr_face)\n" in gen_code)
    # Check that the kernel call itself is correct
    assert (
        "testkern_2qr_int_field_code(nlayers, f1_proxy%data, "
        "f2_proxy(1)%data, f2_proxy(2)%data, f2_proxy(3)%data, f3_proxy%data, "
        "istp, ndf_w2, undf_w2, map_w2(:,cell), basis_w2_qr_xyoz, "
        "basis_w2_qr_face, ndf_wchi, undf_wchi, map_wchi(:,cell), "
        "diff_basis_wchi_qr_xyoz, diff_basis_wchi_qr_face, ndf_adspc1_f3, "
        "undf_adspc1_f3, map_adspc1_f3(:,cell), basis_adspc1_f3_qr_xyoz, "
        "basis_adspc1_f3_qr_face, diff_basis_adspc1_f3_qr_xyoz, "
        "diff_basis_adspc1_f3_qr_face, np_xy_qr_xyoz, np_z_qr_xyoz, "
        "weights_xy_qr_xyoz, weights_z_qr_xyoz, nfaces_qr_face, "
        "np_xyz_qr_face, weights_xyz_qr_face)\n" in gen_code)


def test_multiple_stencils_int_field(dist_mem, tmpdir):
    ''' Test for correct output when there is more than one stencil in a
    kernel that contains integer-valued fields. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "19.25_multiple_stencils_int_field.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output1 = (
        "    USE integer_field_mod, ONLY: integer_field_type, "
        "integer_field_proxy_type\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_testkern_stencil_multi_int_field_type(f1, "
        "f2, f3, f4, f2_extent, f3_extent, f3_direction)")
    assert output1 in result
    output2 = (
        "      USE stencil_dofmap_mod, ONLY: STENCIL_1DX, STENCIL_1DY\n"
        "      USE flux_direction_mod, ONLY: x_direction, y_direction\n"
        "      USE stencil_dofmap_mod, ONLY: STENCIL_CROSS\n"
        "      USE stencil_dofmap_mod, ONLY: stencil_dofmap_type\n"
        "      TYPE(integer_field_type), intent(in) :: f1, f2, f3, f4\n"
        "      INTEGER(KIND=i_def), intent(in) :: f2_extent, f3_extent\n"
        "      INTEGER(KIND=i_def), intent(in) :: f3_direction\n")
    assert output2 in result
    output3 = (
        "      TYPE(integer_field_proxy_type) f1_proxy, f2_proxy, "
        "f3_proxy, f4_proxy\n")
    assert output3 in result
    output4 = (
        "      INTEGER(KIND=i_def), pointer :: f4_stencil_size(:) => null()\n"
        "      INTEGER(KIND=i_def), pointer :: f4_stencil_dofmap(:,:,:) => "
        "null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f4_stencil_map => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer :: f3_stencil_size(:) => null()\n"
        "      INTEGER(KIND=i_def), pointer :: f3_stencil_dofmap(:,:,:) => "
        "null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f3_stencil_map => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_size(:) => null()\n"
        "      INTEGER(KIND=i_def), pointer :: f2_stencil_dofmap(:,:,:) => "
        "null()\n"
        "      TYPE(stencil_dofmap_type), pointer :: f2_stencil_map => "
        "null()\n")
    assert output4 in result
    output5 = (
        "      ! Initialise stencil dofmaps\n"
        "      !\n"
        "      f2_stencil_map => f2_proxy%vspace%get_stencil_dofmap("
        "STENCIL_CROSS,f2_extent)\n"
        "      f2_stencil_dofmap => f2_stencil_map%get_whole_dofmap()\n"
        "      f2_stencil_size => f2_stencil_map%get_stencil_sizes()\n"
        "      IF (f3_direction .eq. x_direction) THEN\n"
        "        f3_stencil_map => f3_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DX,f3_extent)\n"
        "      END IF\n"
        "      IF (f3_direction .eq. y_direction) THEN\n"
        "        f3_stencil_map => f3_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DY,f3_extent)\n"
        "      END IF\n"
        "      f3_stencil_dofmap => f3_stencil_map%get_whole_dofmap()\n"
        "      f3_stencil_size => f3_stencil_map%get_stencil_sizes()\n"
        "      f4_stencil_map => f4_proxy%vspace%get_stencil_dofmap("
        "STENCIL_1DX,2)\n"
        "      f4_stencil_dofmap => f4_stencil_map%get_whole_dofmap()\n"
        "      f4_stencil_size => f4_stencil_map%get_stencil_sizes()\n"
        "      !\n")
    assert output5 in result
    if dist_mem:
        output6 = (
            "      !\n"
            "      IF (f3_proxy%is_dirty(depth=f3_extent)) THEN\n"
            "        CALL f3_proxy%halo_exchange(depth=f3_extent)\n"
            "      END IF\n"
            "      !\n"
            "      IF (f4_proxy%is_dirty(depth=2)) THEN\n"
            "        CALL f4_proxy%halo_exchange(depth=2)\n"
            "      END IF\n")
        assert output6 in result
    output7 = (
        "        CALL testkern_stencil_multi_int_field_code(nlayers, "
        "f1_proxy%data, f2_proxy%data, f2_stencil_size(cell), "
        "f2_stencil_dofmap(:,:,cell), f3_proxy%data, f3_stencil_size(cell), "
        "f3_direction, f3_stencil_dofmap(:,:,cell), f4_proxy%data, "
        "f4_stencil_size(cell), f4_stencil_dofmap(:,:,cell), ndf_w2broken, "
        "undf_w2broken, map_w2broken(:,cell), ndf_w1, undf_w1, "
        "map_w1(:,cell), ndf_w0, undf_w0, map_w0(:,cell), ndf_w2v, "
        "undf_w2v, map_w2v(:,cell))")
    assert output7 in result


# Tests for Invokes calling kernels that contain real- and
# integer-valued fields


def test_int_real_field_invalid():
    ''' Tests that the same field cannot have different data types
    in different kernels within the same Invoke. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "4.15_multikernel_invokes_real_int_field_invalid.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)

    with pytest.raises(GenerationError) as err:
        _ = psy.gen
    assert ("Field argument(s) ['n1'] in Invoke "
            "'invoke_integer_and_real_field' have different metadata for "
            "data type (['gh_real', 'gh_integer']) in different kernels. "
            "This is invalid." in str(err.value))


def test_int_real_field_fs(dist_mem, tmpdir):
    ''' Tests that an invoke calling a kernel with integer-valued
    fields and a kernel with real-valued fields on all function
    spaces produces correct code.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "4.14_multikernel_invokes_real_int_field_fs.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(invoke_info)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    generated_code = str(psy.gen)

    output = (
        "  MODULE multikernel_invokes_real_int_field_fs_psy\n"
        "    USE constants_mod, ONLY: r_def, i_def\n"
        "    USE field_mod, ONLY: field_type, field_proxy_type\n"
        "    USE integer_field_mod, ONLY: integer_field_type, "
        "integer_field_proxy_type\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_integer_and_real_field(i1, i2, n1, n2, i3, "
        "i4, n3, n4, i5, i6, n5, n6, i7, i8, n7, f1, f2, m1, m2, f3, f4, "
        "m3, m4, f5, f6, m5, m6, m7)\n")
    assert output in generated_code
    output = (
        "      TYPE(field_type), intent(in) :: f1, f2, m1, m2, f3, f4, "
        "m3, m4, f5, f6, m5, m6, m7\n"
        "      TYPE(integer_field_type), intent(in) :: i1, i2, n1, n2, "
        "i3, i4, n3, n4, i5, i6, n5, n6, i7, i8, n7\n"
        "      INTEGER(KIND=i_def) cell\n"
        "      INTEGER(KIND=i_def) nlayers\n"
        "      TYPE(integer_field_proxy_type) i1_proxy, i2_proxy, n1_proxy, "
        "n2_proxy, i3_proxy, i4_proxy, n3_proxy, n4_proxy, i5_proxy, "
        "i6_proxy, n5_proxy, n6_proxy, i7_proxy, i8_proxy, n7_proxy\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, "
        "m2_proxy, f3_proxy, f4_proxy, m3_proxy, m4_proxy, f5_proxy, "
        "f6_proxy, m5_proxy, m6_proxy, m7_proxy\n")
    assert output in generated_code
    # Number of layers and the mesh are determined from the first integer
    # field. Maps for function spaces are determined from the first kernel
    # call with integer fields
    output = (
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = i1_proxy%vspace%get_nlayers()\n"
        "      !\n")
    if dist_mem:
        output += (
            "      ! Create a mesh object\n"
            "      !\n"
            "      mesh => i1_proxy%vspace%get_mesh()\n"
            "      !\n")
    output += (
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w1 => i1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2 => i2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w0 => n1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => n2_proxy%vspace%get_whole_dofmap()\n"
        "      map_wtheta => i3_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2h => i4_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2v => n3_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2broken => n4_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2trace => i5_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2htrace => i6_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2vtrace => n5_proxy%vspace%get_whole_dofmap()\n"
        "      map_wchi => n6_proxy%vspace%get_whole_dofmap()\n"
        "      map_any_w2 => i7_proxy%vspace%get_whole_dofmap()\n"
        "      map_aspc1_i8 => i8_proxy%vspace%get_whole_dofmap()\n"
        "      map_adspc1_n7 => n7_proxy%vspace%get_whole_dofmap()\n")
    assert output in generated_code
    # Kernel calls are the same regardless of distributed memory
    kern1_call = (
        "        CALL testkern_fs_int_field_code(nlayers, i1_proxy%data, "
        "i2_proxy%data, n1_proxy%data, n2_proxy%data, i3_proxy%data, "
        "i4_proxy%data, n3_proxy%data, n4_proxy%data, i5_proxy%data, "
        "i6_proxy%data, n5_proxy%data, n6_proxy%data, i7_proxy%data, "
        "i8_proxy%data, n7_proxy%data, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w0, undf_w0, "
        "map_w0(:,cell), ndf_w3, undf_w3, map_w3(:,cell), ndf_wtheta, "
        "undf_wtheta, map_wtheta(:,cell), ndf_w2h, undf_w2h, "
        "map_w2h(:,cell), ndf_w2v, undf_w2v, map_w2v(:,cell), "
        "ndf_w2broken, undf_w2broken, map_w2broken(:,cell), ndf_w2trace, "
        "undf_w2trace, map_w2trace(:,cell), ndf_w2htrace, undf_w2htrace, "
        "map_w2htrace(:,cell), ndf_w2vtrace, undf_w2vtrace, "
        "map_w2vtrace(:,cell), ndf_wchi, undf_wchi, map_wchi(:,cell), "
        "ndf_any_w2, undf_any_w2, map_any_w2(:,cell), ndf_aspc1_i8, "
        "undf_aspc1_i8, map_aspc1_i8(:,cell), ndf_adspc1_n7, "
        "undf_adspc1_n7, map_adspc1_n7(:,cell))\n")
    assert kern1_call in generated_code
    kern2_call = (
        "        CALL testkern_fs_code(nlayers, f1_proxy%data, f2_proxy%data, "
        "m1_proxy%data, m2_proxy%data, f3_proxy%data, f4_proxy%data, "
        "m3_proxy%data, m4_proxy%data, f5_proxy%data, f6_proxy%data, "
        "m5_proxy%data, m6_proxy%data, m7_proxy%data, ndf_w1, undf_w1, "
        "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w0, undf_w0, "
        "map_w0(:,cell), ndf_w3, undf_w3, map_w3(:,cell), ndf_wtheta, "
        "undf_wtheta, map_wtheta(:,cell), ndf_w2h, undf_w2h, map_w2h(:,cell), "
        "ndf_w2v, undf_w2v, map_w2v(:,cell), ndf_w2broken, undf_w2broken, "
        "map_w2broken(:,cell), ndf_w2trace, undf_w2trace, "
        "map_w2trace(:,cell), ndf_w2htrace, undf_w2htrace, "
        "map_w2htrace(:,cell), ndf_w2vtrace, undf_w2vtrace, "
        "map_w2vtrace(:,cell), ndf_wchi, undf_wchi, map_wchi(:,cell), "
        "ndf_any_w2, undf_any_w2, map_any_w2(:,cell))\n")
    assert kern2_call in generated_code
    # Check loop bounds for kernel calls
    if not dist_mem:
        kern1_loop = "DO cell=1,i2_proxy%vspace%get_ncell()\n"
        kern2_loop = "DO cell=1,f1_proxy%vspace%get_ncell()\n"
    else:
        kern1_loop = "DO cell=1,mesh%get_last_halo_cell(1)\n"
        kern2_loop = "DO cell=1,mesh%get_last_halo_cell(1)\n"
    assert kern1_loop in generated_code
    assert kern2_loop in generated_code
    # Check that the field halo flags after the kernel calls
    if dist_mem:
        halo1_flags = (
            "      CALL i2_proxy%set_dirty()\n"
            "      CALL i3_proxy%set_dirty()\n"
            "      CALL i8_proxy%set_dirty()\n"
            "      CALL n7_proxy%set_dirty()\n"
            "      CALL i3_proxy%set_clean(1)\n"
            "      CALL n7_proxy%set_clean(1)\n")
        halo2_flags = (
            "      CALL f1_proxy%set_dirty()\n"
            "      CALL f3_proxy%set_dirty()\n"
            "      CALL f3_proxy%set_clean(1)\n")
        assert halo1_flags in generated_code
        assert halo2_flags in generated_code
