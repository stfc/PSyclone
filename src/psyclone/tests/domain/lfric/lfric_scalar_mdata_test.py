# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
Module containing pytest tests for the general LFRic scalar arguments
functionality (e.g. metadata, parsing, invoke calls).
'''

import os
import pytest
import fparser

from psyclone.domain.lfric import (LFRicConstants, LFRicKern,
                                   LFRicScalarArgs, LFRicScalarArrayArgs)
from psyclone.errors import InternalError, GenerationError
from psyclone.parse.algorithm import parse
from psyclone.parse.utils import ParseError
from psyclone.psyGen import PSyFactory
from psyclone.tests.utilities import create_lfric_metadata

# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "lfric")
TEST_API = "lfric"


CODE = '''
module testkern_qr
  type, extends(kernel_type) :: testkern_qr_type
     type(arg_type), dimension(7) :: meta_args =                              &
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
  subroutine testkern_qr_code()
end subroutine testkern_qr_code
end module testkern_qr
'''


def test_ad_scalar_type_too_few_args(fortran_reader):
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
        psyir = fortran_reader.psyir_from_source(code)
        with pytest.raises(ParseError) as excinfo:
            _ = create_lfric_metadata(psyir, name=name)
        assert "must have at least three arguments" in str(excinfo.value)


def test_ad_scalar_type_too_many_args(fortran_reader):
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar has more than 3 args. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_qr_type"
    const = LFRicConstants()
    for argname in const.VALID_SCALAR_NAMES:
        code = CODE.replace(
            "arg_type(" + argname + ",   gh_integer, gh_read)",
            "arg_type(" + argname + ",   gh_integer, gh_read, w1)", 1)
        psyir = fortran_reader.psyir_from_source(code)
        with pytest.raises(ParseError) as excinfo:
            _ = create_lfric_metadata(psyir, name=name)
        assert "Scalar metadata must have three arguments" in str(
            excinfo.value)


def test_ad_scalar_invalid_data_type(fortran_reader):
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar has an invalid data type. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_qr_type"
    code = CODE.replace("arg_type(gh_scalar,   gh_real,    gh_read)",
                        "arg_type(gh_scalar, gh_unreal, gh_read)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    with pytest.raises(ParseError) as excinfo:
        _ = create_lfric_metadata(psyir, name=name)
    assert "scalar datatype descriptor" in str(excinfo.value)
    assert "'gh_unreal'" in str(excinfo.value)


def test_ad_scalar_type_no_write(fortran_reader):
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar specifies 'GH_WRITE' access. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_qr_type"
    const = LFRicConstants()
    for argname in const.VALID_SCALAR_NAMES:
        code = CODE.replace(
            "arg_type(" + argname + ",   gh_integer, gh_read)",
            "arg_type(" + argname + ",   gh_integer, gh_write)", 1)
        psyir = fortran_reader.psyir_from_source(code)
        with pytest.raises(ParseError) as excinfo:
            _ = create_lfric_metadata(psyir, name=name)
        assert "scalar access descriptor" in str(excinfo.value)
        assert "'gh_write'" in str(excinfo.value)


def test_ad_scalar_type_no_inc(fortran_reader):
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar specifies 'GH_INC' access. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_qr_type"
    const = LFRicConstants()
    for argname in const.VALID_SCALAR_NAMES:
        code = CODE.replace("arg_type(" + argname + ",   gh_real,    gh_read)",
                            "arg_type(" + argname + ",   gh_real, gh_inc)", 1)
        psyir = fortran_reader.psyir_from_source(code)
        with pytest.raises(ParseError) as excinfo:
            _ = create_lfric_metadata(psyir, name=name)
        assert "scalar access descriptor" in str(excinfo.value)
        assert "'gh_inc'" in str(excinfo.value)


def test_ad_scalar_type_no_readwrite(fortran_reader):
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar specifies 'GH_READWRITE' access. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_qr_type"
    const = LFRicConstants()
    for argname in const.VALID_SCALAR_NAMES:
        code = CODE.replace(
            "arg_type(" + argname + ",   gh_logical, gh_read)",
            "arg_type(" + argname + ",   gh_logical, gh_readwrite)", 1)
        psyir = fortran_reader.psyir_from_source(code)
        with pytest.raises(ParseError) as excinfo:
            _ = create_lfric_metadata(psyir, name=name)
        assert "scalar access descriptor" in str(excinfo.value)
        assert "'gh_readwrite'" in str(excinfo.value)


@pytest.mark.parametrize("scalar_type", ["gh_integer", "gh_logical"])
def test_ad_integer_logical_scalar_type_no_sum(scalar_type, fortran_reader):
    ''' Tests that an error is raised when the argument descriptor
    metadata for an 'integer' or a 'logical' scalar specifies 'GH_REDUCTION'
    access (reduction).

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace(
        f"arg_type(gh_scalar,   {scalar_type}, gh_read)",
        f"arg_type(gh_scalar,   {scalar_type}, gh_reduction)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = create_lfric_metadata(psyir, name=name)
    assert "Reduction access is only valid for a real scalar" in str(
        excinfo.value)


def test_no_vector_scalar(fortran_reader):
    ''' Tests that we raise an error when kernel metadata erroneously
    specifies a vector scalar argument. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_qr_type"
    const = LFRicConstants()
    for argname in const.VALID_SCALAR_NAMES:
        vectname = argname + " * 3"
        code = CODE.replace("arg_type(" + argname + ",   gh_real,    gh_read)",
                            "arg_type(" + vectname + ", gh_real, gh_read)", 1)
        psyir = fortran_reader.psyir_from_source(code)
        with pytest.raises(ParseError) as excinfo:
            _ = create_lfric_metadata(psyir, name=name)
        assert "Vector notation is only supported for gh_field" in str(
            excinfo.value)
        assert f"'{argname}'" in str(excinfo.value)


@pytest.mark.parametrize("scalar_ind, scalar_type", [
    (0, "gh_real"), (6, "gh_integer"), (5, "gh_logical")])
def test_arg_descriptor_scalar(scalar_ind, scalar_type, fortran_reader):
    ''' Test that the typed argument-metadata representation works
    as expected for all three types of valid scalar argument:
    'real', 'integer' and 'logical'.

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    psyir = fortran_reader.psyir_from_source(CODE)
    metadata = create_lfric_metadata(psyir, name="testkern_qr_type")
    scalar_descriptor = metadata.meta_args[scalar_ind]

    assert scalar_descriptor.form == "gh_scalar"
    assert scalar_descriptor.datatype == scalar_type
    assert scalar_descriptor.access == "gh_read"
    assert scalar_descriptor.fortran_string() == (
        f"arg_type(gh_scalar, {scalar_type}, gh_read)")


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
        LFRicScalarArgs(invoke).invoke_declarations()
    assert ("Found unsupported intrinsic types for the scalar arguments "
            "['a'] to Invoke 'invoke_0_testkern_three_scalars_type'. "
            "Supported types are ['real', 'integer', 'logical']."
            in str(err.value))


def test_lfricscalararray_call_err1():
    ''' Check that the LFRicScalarArrayArgs constructor raises the
    expected internal error if it encounters an unrecognised
    intrinsic type of ScalarArray when generating a kernel call.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "28.scalar_array_invoke.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    kernel = invoke.schedule.coded_kernels()[0]
    # Sabotage the scalar argument to make it have an invalid intrinsic type
    scalar_arr_arg = kernel.arguments.args[1]
    scalar_arr_arg._intrinsic_type = "double-type"
    with pytest.raises(InternalError) as err:
        LFRicScalarArrayArgs(invoke).invoke_declarations()
    assert ("Found unsupported intrinsic types for the ScalarArray arguments "
            "['real_array'] to Invoke 'invoke_0'. Supported types are "
            "['real', 'integer', 'logical']."
            in str(err.value))


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
    assert "use constants_mod, only : r_def, roo_def\n" in code


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


def test_scalar_arg_lfricconst_properties(monkeypatch, fortran_reader):
    ''' Tests that properties of all supported types of user-defined,
    read-only, scalar arguments ('real', 'integer' and 'logical') defined
    in LFRicConstants are correctly set up in the LFRicKernelArgument class.

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    psyir = fortran_reader.psyir_from_source(CODE)
    name = "testkern_qr_type"
    metadata = create_lfric_metadata(psyir, name=name)
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
    in LFRicConstants are correctly set up in the LFRicKernelArgument class.

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


def test_multiple_updated_scalar_args(fortran_reader):
    ''' Check that we raise the expected exception when we encounter a
    kernel that writes to more than one of its field and scalar arguments '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("arg_type(gh_scalar,   gh_real,    gh_read)",
                        "arg_type(gh_scalar,   gh_real,    gh_reduction)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = create_lfric_metadata(psyir, name=name)
    assert ("A user-supplied LFRic kernel must not write/update a scalar "
            "argument but kernel 'testkern_qr_type' does" in
            str(excinfo.value))


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


def test_scalar_array_different_data_types_invoke():
    ''' Tests that the same scalar cannot have different data types
    in different kernels within the same Invoke.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "28.1_multikernel_invokes_scalar_array_invalid.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)

    with pytest.raises(GenerationError):
        _ = psy.gen
    assert ("ScalarArray argument(s) ['b'] in Invoke "
            "'invoke_real_and_logical_scalars' is/are passed to more than "
            "one kernel and the kernel metadata for the corresponding "
            "arguments specifies different intrinsic types.")
