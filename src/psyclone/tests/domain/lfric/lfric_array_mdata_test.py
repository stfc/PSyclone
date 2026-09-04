# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
Module containing pytest tests for the general LFRic array arguments
functionality (e.g. metadata, parsing, invoke calls).
'''

import os
import pytest
import fparser
from psyclone.parse.utils import ParseError
from psyclone.tests.utilities import create_lfric_metadata

# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "lfric")
TEST_API = "lfric"


ARRAY_CODE = '''
module testkern_mod

  type, extends(kernel_type) :: testkern_array_type
     type(arg_type), dimension(5) :: meta_args =                             &
          (/ arg_type(gh_scalar_array,   gh_real,    gh_read, 1),            &
             arg_type(gh_scalar_array,   gh_integer, gh_read, 2),            &
             arg_type(gh_scalar_array,   gh_logical, gh_read, 4),            &
             arg_type(gh_operator, gh_real,   gh_read, w2, w2),              &
             arg_type(gh_field,    gh_real,   gh_write, w3)                  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_code
  end type testkern_array_type
contains
  subroutine testkern_code()
end subroutine testkern_code
end module testkern_mod
'''


def test_ad_array_type_wrong_num_of_args(fortran_reader):
    ''' Tests that an error is raised when the ScalarArray argument
    descriptor metadata for a ScalarArray has fewer than 4 args. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = ARRAY_CODE.replace(
        "arg_type(gh_scalar_array,   gh_real,    gh_read, 1)",
        "arg_type(gh_scalar_array,   gh_real,    gh_read)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    name = "testkern_array_type"
    with pytest.raises(ParseError) as excinfo:
        _ = create_lfric_metadata(psyir, name=name)
    assert "Scalar-array metadata must have four arguments" in str(
        excinfo.value)


def test_ad_array_invalid_data_type(fortran_reader):
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar has an invalid data type. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_array_type"
    # check real array
    code = ARRAY_CODE.replace(
        "arg_type(gh_scalar_array,   gh_real,    gh_read, 1)",
        "arg_type(gh_scalar_array, gh_unreal,    gh_read, 1)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    with pytest.raises(ParseError) as excinfo:
        _ = create_lfric_metadata(psyir, name=name)
    assert "scalar-array datatype descriptor" in str(excinfo.value)
    assert "'gh_unreal'" in str(excinfo.value)
    # check integer array
    code = ARRAY_CODE.replace(
        "arg_type(gh_scalar_array,   gh_integer, gh_read, 2)",
        "arg_type(gh_scalar_array,   gh_frac,    gh_read, 2)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    with pytest.raises(ParseError) as excinfo:
        _ = create_lfric_metadata(psyir, name=name)
    assert "scalar-array datatype descriptor" in str(excinfo.value)
    assert "'gh_frac'" in str(excinfo.value)
    # check logical array
    code = ARRAY_CODE.replace(
        "arg_type(gh_scalar_array,   gh_logical, gh_read, 4)",
        "arg_type(gh_scalar_array, gh_illogical, gh_read, 4)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    with pytest.raises(ParseError) as excinfo:
        _ = create_lfric_metadata(psyir, name=name)
    assert "scalar-array datatype descriptor" in str(excinfo.value)
    assert "'gh_illogical'" in str(excinfo.value)


def test_ad_array_type_no_write(fortran_reader):
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar specifies 'GH_WRITE' access. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_array_type"
    code = ARRAY_CODE.replace(
        "arg_type(gh_scalar_array,   gh_real,    gh_read, 1)",
        "arg_type(gh_scalar_array,   gh_real,    gh_write, 1)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    with pytest.raises(ParseError) as excinfo:
        _ = create_lfric_metadata(psyir, name=name)
    assert "scalar-array access descriptor" in str(excinfo.value)
    assert "'gh_write'" in str(excinfo.value)


def test_ad_array_type_no_inc(fortran_reader):
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar specifies 'GH_INC' access. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_array_type"
    code = ARRAY_CODE.replace(
        "arg_type(gh_scalar_array,   gh_real,    gh_read, 1)",
        "arg_type(gh_scalar_array,   gh_real,    gh_inc, 1)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    with pytest.raises(ParseError) as excinfo:
        _ = create_lfric_metadata(psyir, name=name)
    assert "scalar-array access descriptor" in str(excinfo.value)
    assert "'gh_inc'" in str(excinfo.value)


def test_ad_array_type_no_readwrite(fortran_reader):
    ''' Tests that an error is raised when the argument descriptor
    metadata for an array specifies 'GH_READWRITE' access. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_array_type"
    code = ARRAY_CODE.replace(
        "arg_type(gh_scalar_array,   gh_real,    gh_read, 1)",
        "arg_type(gh_scalar_array,   gh_real, gh_readwrite, 1)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    with pytest.raises(ParseError) as excinfo:
        _ = create_lfric_metadata(psyir, name=name)
    assert "scalar-array access descriptor" in str(excinfo.value)
    assert "'gh_readwrite'" in str(excinfo.value)


def test_ad_array_type_no_sum(fortran_reader):
    ''' Tests that an error is raised when the argument descriptor
    metadata for an array specifies 'GH_REDUCTION' access (reduction). '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = ARRAY_CODE.replace(
        "arg_type(gh_scalar_array,   gh_real,    gh_read, 1)",
        "arg_type(gh_scalar_array,   gh_real,    gh_reduction,  1)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    name = "testkern_array_type"
    with pytest.raises(ParseError) as excinfo:
        _ = create_lfric_metadata(psyir, name=name)
    assert "scalar-array access descriptor" in str(excinfo.value)
    assert "'gh_reduction'" in str(excinfo.value)


def test_no_vector_array(fortran_reader):
    ''' Tests that we raise an error when kernel metadata erroneously
    specifies a vector scalar argument. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_array_type"
    code = ARRAY_CODE.replace(
        "arg_type(gh_scalar_array,   gh_real,    gh_read, 1)",
        "arg_type(gh_scalar_array*3, gh_real,  gh_read, 1)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    with pytest.raises(ParseError) as excinfo:
        _ = create_lfric_metadata(psyir, name=name)
    assert "Vector notation is only supported for gh_field" in str(
        excinfo.value)
    assert "'gh_scalar_array'" in str(excinfo.value)


@pytest.mark.parametrize("array_ind, array_type, array_ndims", [
    (0, "gh_real", 1), (1, "gh_integer", 2), (2, "gh_logical", 4)])
def test_arg_descriptor_array(
        array_ind, array_type, array_ndims, fortran_reader):
    ''' Test that the typed argument-metadata representation works
    as expected for all three types of valid ScalarArray argument:
    'real', 'integer' and 'logical'.

    '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    psyir = fortran_reader.psyir_from_source(ARRAY_CODE)
    metadata = create_lfric_metadata(psyir, name="testkern_array_type")
    array_descriptor = metadata.meta_args[array_ind]

    assert array_descriptor.form == "gh_scalar_array"
    assert array_descriptor.datatype == array_type
    assert array_descriptor.array_ndims == array_ndims
    assert array_descriptor.access == "gh_read"
    assert array_descriptor.fortran_string() == (
        f"arg_type(gh_scalar_array, {array_type}, gh_read, {array_ndims})")


def test_n_not_integer(fortran_reader):
    ''' Tests that we raise an error when n is not an integer'''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_array_type"
    code = ARRAY_CODE.replace(
        "arg_type(gh_scalar_array,   gh_real,    gh_read, 1)",
        "arg_type(gh_scalar_array,  gh_real,  gh_read, 0.5)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    with pytest.raises(ParseError) as excinfo:
        _ = create_lfric_metadata(psyir, name=name)
    assert "invalid literal for int()" in str(excinfo.value)
    assert "'0.5'" in str(excinfo.value)


def test_n_less_than_one(fortran_reader):
    ''' Tests that we raise an error when n is less than 1'''
    fparser.logging.disable(fparser.logging.CRITICAL)
    name = "testkern_array_type"
    code = ARRAY_CODE.replace(
        "arg_type(gh_scalar_array,   gh_real,    gh_read, 1)",
        "arg_type(gh_scalar_array,   gh_real,    gh_read, 0)", 1)
    psyir = fortran_reader.psyir_from_source(code)
    with pytest.raises(ParseError) as excinfo:
        _ = create_lfric_metadata(psyir, name=name)
    assert "dimensions must be an integer greater than or equal to one" in str(
        excinfo.value)
