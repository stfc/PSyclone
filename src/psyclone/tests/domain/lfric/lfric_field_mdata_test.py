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
Module containing pytest tests for the general LFRic field arguments
functionality (e.g. metadata, parsing, invoke calls).
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
from psyclone.errors import InternalError


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
