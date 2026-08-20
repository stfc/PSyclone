# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
Module containing pytest tests for the general LFRic field arguments
functionality (e.g. metadata, parsing, invoke calls).
'''

import os
import pytest
from psyclone.domain.lfric import LFRicConstants, LFRicFields
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.parse.utils import ParseError
from psyclone.configuration import Config
from psyclone.errors import InternalError
from psyclone.tests.utilities import create_lfric_metadata


# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "lfric")
TEST_API = "lfric"


FIELD_CODE = '''
module testkern_field_mod
  type, extends(kernel_type) :: testkern_field_type
     type(arg_type), dimension(7) :: meta_args =                    &
          (/ arg_type(gh_scalar, gh_real,    gh_read),            &
             arg_type(gh_field,  gh_real,    gh_readinc, w0),     &
             arg_type(gh_field,  gh_real,    gh_inc,     w1),     &
             arg_type(gh_field,  gh_real,    gh_read,    w2),     &
             arg_type(gh_field,  gh_integer, gh_write,   wtheta), &
             arg_type(gh_field,  gh_integer, gh_read,    w3),     &
             arg_type(gh_scalar, gh_integer, gh_read)             &
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


def test_ad_fld_type_1st_arg(fortran_reader):
    ''' Tests that an error is raised when the first argument descriptor
    metadata for a field is invalid. '''
    code = FIELD_CODE.replace(
        "arg_type(gh_field,  gh_real,    gh_inc,     w1)",
        "arg_type(gh_hedge,  gh_real,    gh_inc,     w1)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    name = "testkern_field_type"
    with pytest.raises(ParseError) as excinfo:
        _ = create_lfric_metadata(psyir, name=name)
    assert "The first argument of arg_type must be one of" in str(
        excinfo.value)
    assert "'gh_hedge'" in str(excinfo.value)


def test_ad_field_invalid_data_type(fortran_reader):
    ''' Tests that an error is raised when the argument descriptor
    metadata for a field has an invalid data type. '''
    name = "testkern_field_type"
    # Check real field
    code = FIELD_CODE.replace(
        "arg_type(gh_field,  gh_real,    gh_inc,     w1)",
        "arg_type(gh_field,  gh_unreal,  gh_inc,     w1)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    with pytest.raises(ParseError) as excinfo:
        _ = create_lfric_metadata(psyir, name=name)
    assert "field datatype descriptor" in str(excinfo.value)
    assert "'gh_unreal'" in str(excinfo.value)
    # Check integer field
    code = FIELD_CODE.replace(
        "arg_type(gh_field,  gh_integer, gh_read,    w3)",
        "arg_type(gh_field,  gh_double,  gh_read,    w3)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    with pytest.raises(ParseError) as excinfo:
        _ = create_lfric_metadata(psyir, name=name)
    assert "field datatype descriptor" in str(excinfo.value)
    assert "'gh_double'" in str(excinfo.value)


def test_field_gh_reduction_invalid(fortran_reader):
    ''' Tests that an error is raised when a field is specified with
    access type 'gh_reduction'. '''
    code = FIELD_CODE.replace(
        "arg_type(gh_field,  gh_real,    gh_read,    w2)",
        "arg_type(gh_field,  gh_real,    gh_reduction,     w2)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    name = "testkern_field_type"
    with pytest.raises(ParseError) as excinfo:
        _ = create_lfric_metadata(psyir, name=name)
    assert "field access descriptor" in str(excinfo.value)
    assert "'gh_reduction'" in str(excinfo.value)


def test_ad_fld_type_too_few_args(fortran_reader):
    ''' Tests that an error is raised when the field argument descriptor
    metadata for a field has fewer than 3 args. '''
    code = FIELD_CODE.replace(
        "arg_type(gh_field,  gh_real,    gh_inc,     w1)",
        "arg_type(gh_field,  gh_real,    gh_inc)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    name = "testkern_field_type"
    with pytest.raises(ParseError) as excinfo:
        _ = create_lfric_metadata(psyir, name=name)
    assert "Field metadata must have a function space" in str(excinfo.value)


def test_ad_fld_type_too_many_args(fortran_reader):
    ''' Tests that an error is raised when the field argument descriptor
    metadata has more than 7 args. '''
    code = FIELD_CODE.replace(
        "arg_type(gh_field,  gh_real,    gh_inc,     w1)",
        "arg_type(gh_field,  gh_real,    gh_inc,   w1, w1, w2, w3, w3)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    name = "testkern_field_type"
    with pytest.raises(ParseError) as excinfo:
        _ = create_lfric_metadata(psyir, name=name)
    assert "must have at most 7 arguments" in str(excinfo.value)


def test_arg_descriptor_invalid_fs(fortran_reader):
    ''' Tests that an error is raised when an invalid function space
    name is provided as the third argument for a field. '''
    name = "testkern_field_type"
    # Check real field
    code = FIELD_CODE.replace(
        "arg_type(gh_field,  gh_real,    gh_inc,     w1)",
        "arg_type(gh_field,  gh_real,    gh_inc,     w4)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    with pytest.raises(ParseError) as excinfo:
        _ = create_lfric_metadata(psyir, name=name)
    assert "field function space" in str(excinfo.value)
    assert "'w4'" in str(excinfo.value)
    # Check integer field
    code = FIELD_CODE.replace(
        "arg_type(gh_field,  gh_integer, gh_read,    w3)",
        "arg_type(gh_field,  gh_integer, gh_read,    w10)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    with pytest.raises(ParseError) as excinfo:
        _ = create_lfric_metadata(psyir, name=name)
    assert "field function space" in str(excinfo.value)
    assert "'w10'" in str(excinfo.value)


def test_fs_discontinuous_inc_error(fortran_reader):
    ''' Test that an error is raised if a discontinuous function space
    and 'gh_inc' are provided for the same field in the metadata. '''
    const = LFRicConstants()
    for fspace in const.VALID_DISCONTINUOUS_NAMES:
        code = FIELD_CODE.replace(
            "arg_type(gh_field,  gh_integer, gh_read,    w3)",
            "arg_type(gh_field,  gh_integer, gh_inc, " + fspace + ")", 1)
        psyir = fortran_reader.psyir_from_source(code)
        with pytest.raises(ParseError) as excinfo:
            _ = create_lfric_metadata(psyir, name="testkern_field_type")
        assert f"Field '{fspace}'" in str(excinfo.value)
        assert ("must have one of ['gh_read', 'gh_write', 'gh_readwrite']" in
                str(excinfo.value))
        assert "'gh_inc'" in str(excinfo.value)


def test_fs_continuous_cells_readwrite_error(fortran_reader):
    ''' Test that an error is raised if a field on a continuous
    function space is specified as having an access of 'gh_readwrite'
    in kernel metadata.

    '''
    const = LFRicConstants()
    for fspace in const.CONTINUOUS_FUNCTION_SPACES:
        acc = "gh_readwrite"
        code = FIELD_CODE.replace(
            "arg_type(gh_field,  gh_real,    gh_read,    w2)",
            f"arg_type(gh_field, gh_real, {acc}, {fspace})", 1)
        psyir = fortran_reader.psyir_from_source(code)
        with pytest.raises(ParseError) as excinfo:
            _ = create_lfric_metadata(psyir, name="testkern_field_type")
        assert f"Field '{fspace}'" in str(excinfo.value)
        assert "must have one of ['gh_read', 'gh_write', 'gh_inc'" in str(
            excinfo.value)
        assert f"'{acc}'" in str(excinfo.value)


def test_fs_anyspace_cells_readwrite_error(fortran_reader):
    ''' Test that an error is raised if a field that is on 'any_space' "
    "(and therefore may be continuous) is specified as having "
    "'gh_readwrite' access in the metadata.

    '''
    const = LFRicConstants()
    for fspace in const.VALID_ANY_SPACE_NAMES:
        acc = "gh_readwrite"
        code = FIELD_CODE.replace(
            "arg_type(gh_field,  gh_real,    gh_read,    w2)",
            f"arg_type(gh_field, gh_real, {acc}, {fspace})", 1)
        psyir = fortran_reader.psyir_from_source(code)
        with pytest.raises(ParseError) as excinfo:
            _ = create_lfric_metadata(psyir, name="testkern_field_type")
        assert f"Field '{fspace}'" in str(excinfo.value)
        assert "must have one of ['gh_read', 'gh_write', 'gh_inc'" in str(
            excinfo.value)
        assert f"'{acc}'" in str(excinfo.value)


@pytest.mark.parametrize("access", ["gh_inc", "gh_readinc"])
def test_fs_anyspace_dofs_inc_error(access, fortran_reader):
    '''Test that an error is raised if a field on 'any_space' with
    'gh_inc' or 'gh_readinc' access is specified for a kernel that
    operates on DoFs.

    '''
    dof_code = FIELD_CODE.replace("integer :: operates_on = cell_column",
                                  "integer :: operates_on = dof", 1)
    # gh_readinc also causes an exception so remove it for this test.
    dof_code = dof_code.replace("gh_readinc", "gh_read")
    const = LFRicConstants()
    for fspace in const.VALID_ANY_SPACE_NAMES:
        code = dof_code.replace(
            "arg_type(gh_field,  gh_real,    gh_inc,     w1)",
            f"arg_type(gh_field, gh_real, {access}, {fspace})", 1)
        psyir = fortran_reader.psyir_from_source(code)
        with pytest.raises(ParseError) as excinfo:
            _ = create_lfric_metadata(psyir, name="testkern_field_type")
        assert f"Field '{fspace}'" in str(excinfo.value)
        assert "operating on 'dof'" in str(excinfo.value)
        assert f"'{access}'" in str(excinfo.value)


def test_arg_descriptor_field(fortran_reader):
    ''' Test that the typed argument-metadata representation works
    as expected for a field argument. '''
    psyir = fortran_reader.psyir_from_source(FIELD_CODE)
    metadata = create_lfric_metadata(psyir, name="testkern_field_type")
    field_descriptor = metadata.meta_args[2]

    assert field_descriptor.form == "gh_field"
    assert field_descriptor.datatype == "gh_real"
    assert field_descriptor.function_space == "w1"
    assert field_descriptor.access == "gh_inc"
    assert field_descriptor.stencil is None
    assert field_descriptor.nlevels is None
    assert field_descriptor.ndata == "1"
    assert field_descriptor.fortran_string() == (
        "arg_type(gh_field, gh_real, gh_inc, w1)")


def test_fld_nlevels(fortran_reader):
    '''
    Test a field argument with the optional 'nlevels' metatadata.
    '''
    code = FIELD_CODE.replace(
        "arg_type(gh_scalar, gh_integer, gh_read)",
        "arg_type(gh_field, gh_real, gh_read, w3, nlevels='double')", 1)
    psyir = fortran_reader.psyir_from_source(code)
    name = "testkern_field_type"
    mdata = create_lfric_metadata(psyir, name=name)
    # By default, nlevels is left as None.
    field_descriptor = mdata.meta_args[5]
    assert field_descriptor.nlevels is None
    # The seventh argument has nlevels specified as "double"
    field_descriptor = mdata.meta_args[6]
    assert field_descriptor.nlevels == "double"


def test_fld_ndata(fortran_reader):
    '''
    Test a field argument with the optional 'ndata' metatadata.
    '''
    code = FIELD_CODE.replace(
        "arg_type(gh_scalar, gh_integer, gh_read)",
        "arg_type(gh_field, gh_real, gh_read, w3, ndata='2')", 1)
    psyir = fortran_reader.psyir_from_source(code)
    name = "testkern_field_type"
    mdata = create_lfric_metadata(psyir, name=name)
    # By default, ndata is 1.
    field_descriptor = mdata.meta_args[5]
    assert field_descriptor.ndata == "1"
    # The seventh argument has ndata specified as "2"
    field_descriptor = mdata.meta_args[6]
    assert field_descriptor.ndata == "2"


def test_invalid_vector_operator(fortran_reader):
    ''' Tests that an error is raised when a field vector does not
    use "*" as its operator. '''
    code = FIELD_CODE.replace(
        "(gh_field,  gh_real,    gh_inc,     w1)",
        "(gh_field+3,  gh_real,    gh_inc,    w1)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    name = "testkern_field_type"
    with pytest.raises(ParseError) as excinfo:
        _ = create_lfric_metadata(psyir, name=name)
    assert "Field vectors must use multiplication syntax" in str(
        excinfo.value)


def test_invalid_vector_value_type(fortran_reader):
    ''' Tests that an error is raised when a vector value is not a valid
    integer. '''
    code = FIELD_CODE.replace("(gh_field,  gh_real,    gh_inc,     w1)",
                              "(gh_field*n,  gh_real,    gh_inc,     w1)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    name = "testkern_field_type"
    with pytest.raises(ParseError) as excinfo:
        _ = create_lfric_metadata(psyir, name=name)
    assert "Vector length must be an integer string" in str(excinfo.value)


def test_invalid_vector_value_range(fortran_reader):
    ''' Tests that an error is raised when a vector value is not a valid
    value (<2). '''
    code = FIELD_CODE.replace("(gh_field,  gh_real,    gh_inc,     w1)",
                              "(gh_field*1,  gh_real,    gh_inc,     w1)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    name = "testkern_field_type"
    with pytest.raises(ParseError) as excinfo:
        _ = create_lfric_metadata(psyir, name=name)
    assert "Vector length must be greater than one" in str(excinfo.value)

# Testing that an error is raised when a vector value is not provided is
# not required here as it causes a parse error in the generic code.


def test_arg_descriptor_field_vector(fortran_reader):
    ''' Test that the typed argument-metadata representation works
    as expected when we have a field vector. '''
    # Change the meta-data so that the second argument is a vector
    code = FIELD_CODE.replace("(gh_field,  gh_real,    gh_inc,     w1)",
                              "(gh_field*3,  gh_real,    gh_inc,    w1)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    name = "testkern_field_type"
    dkm = create_lfric_metadata(psyir, name=name)
    field_descriptor = dkm.meta_args[2]

    assert field_descriptor.form == "gh_field"
    assert field_descriptor.datatype == "gh_real"
    assert field_descriptor.function_space == "w1"
    assert field_descriptor.access == "gh_inc"
    assert field_descriptor.stencil is None
    assert field_descriptor.vector_length == "3"
    assert field_descriptor.fortran_string() == (
        "arg_type(gh_field*3, gh_real, gh_inc, w1)")


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
        LFRicFields(invoke).invoke_declarations()
    test_str = str(err.value)
    assert ("Found unsupported intrinsic types for the field arguments "
            "['f1'] to Invoke 'invoke_0_testkern_fs_type'. Supported "
            "types are ['real', 'integer', 'logical']." in test_str)


def test_lfricinvoke_uniq_declns_intent_fields():
    ''' Tests that LFRicInvoke.unique_declns_by_intent() returns the correct
    list of arguments for 'gh_field' argument type. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_3scalar.f90"),
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
    const = LFRicConstants()
    fields_real_args = invoke.unique_declarations(
        const.VALID_FIELD_NAMES, intrinsic_type="real")
    fields_real = [arg.declaration_name for arg in fields_real_args]
    assert fields_real == ["f1", "f2", "m1", "m2", "f3", "f4", "m3",
                           "m4", "f5", "f6", "m5", "m6", "m7"]

    # Return 'integer'-valued fields
    fields_int_args = invoke.unique_declarations(const.VALID_FIELD_NAMES,
                                                 intrinsic_type="integer")
    fields_int = [arg.declaration_name for arg in fields_int_args]
    assert fields_int == ["i1", "i2", "n1", "n2", "i3", "i4", "n3", "n4",
                          "i5", "i6", "n5", "n6", "i7", "i8", "n7"]


def test_field_arg_lfricconst_properties(monkeypatch):
    ''' Tests that properties of all supported types of field arguments
    ('real'-valued 'field_type' and 'integer'-valued 'integer_field_type')
    defined in LFRicConstants are correctly set up in the LFRicKernelArgument
    class.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "4.14_multikernel_invokes_real_int_field_fs.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    # Test 'real'-valued fields of 'field_type'
    kernel = schedule.kernels()[1]
    field_arg = kernel.arguments.args[0]
    assert field_arg.module_name == "field_mod"
    assert field_arg.data_type == "field_type"
    assert field_arg.proxy_data_type == "field_proxy_type"
    assert field_arg.intrinsic_type == "real"
    assert field_arg.precision == "r_def"

    # Test 'integer'-valued fields of 'integer_field_type'
    kernel = schedule.kernels()[0]
    field_arg = kernel.arguments.args[0]
    assert field_arg.module_name == "integer_field_mod"
    assert field_arg.data_type == "integer_field_type"
    assert field_arg.proxy_data_type == "integer_field_proxy_type"
    assert field_arg.intrinsic_type == "integer"
    assert field_arg.precision == "i_def"

    # Monkeypatch to check with an invalid intrinsic type of a
    # field argument
    const = LFRicConstants()
    monkeypatch.setattr(field_arg, "_intrinsic_type", "black")
    with pytest.raises(InternalError) as err:
        field_arg._init_data_type_properties(None, False)
    assert (f"Expected one of {const.VALID_FIELD_INTRINSIC_TYPES} intrinsic "
            f"types for a field argument but found 'black'." in str(err.value))


def test_multiple_updated_field_args(fortran_reader):
    ''' Check that we successfully parse a kernel that writes to more
    than one of its field arguments '''
    code = FIELD_CODE.replace("arg_type(gh_field,  gh_real,    gh_read,  w2)",
                              "arg_type(gh_field, gh_real, gh_inc, w2)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    name = "testkern_field_type"
    metadata = create_lfric_metadata(psyir, name=name)
    count = 0
    for descriptor in metadata.meta_args:
        if (descriptor.form == "gh_field" and
                descriptor.access != "gh_read"):
            count += 1
    assert count == 3


def test_field_arg_discontinuous(monkeypatch, annexed):
    ''' Test that the discontinuous method in the LFRic API argument
    class returns the correct values. Check that the code is generated
    correctly when annexed DoFs are and are not computed by default as
    the number of halo exchanges produced is different in the two
    cases.

    '''

    # pylint: disable=too-many-branches, too-many-statements
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
    const = LFRicConstants()
    fs_dict = dict(zip(const.DISCONTINUOUS_FUNCTION_SPACES[0:3],
                       zip(idchld_list, idarg_list)))
    for fspace, (idchld, idarg) in fs_dict.items():
        filename = "1_single_invoke_" + fspace + ".f90"
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
