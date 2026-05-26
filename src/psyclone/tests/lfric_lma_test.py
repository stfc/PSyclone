# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2026 Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and N. Nobre, STFC Daresbury Lab
# Modified I. Kavcic and L. Turner, Met Office
# Modified J. Henrichs, Bureau of Meteorology

''' This module tests the support for LMA operators in the LFRic
    API using pytest.

'''

import copy
import os
import pytest

from fparser import api as fpapi

from psyclone.configuration import Config
from psyclone.core.access_type import AccessType
from psyclone.domain.lfric import (LFRicArgDescriptor, LFRicConstants,
                                   LFRicKern, LFRicKernMetadata)
from psyclone.lfric import LFRicFuncDescriptor, FunctionSpace
from psyclone.errors import GenerationError, InternalError
from psyclone.parse.algorithm import parse
from psyclone.parse.utils import ParseError
from psyclone.psyGen import PSyFactory, args_filter
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir import symbols

# Constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "lfric")

TEST_API = "lfric"

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
     type(func_type), dimension(3) :: meta_funcs =   &
          (/ func_type(w1, gh_basis),                &
             func_type(w2, gh_diff_basis),           &
             func_type(w3, gh_basis, gh_diff_basis)  &
           /)
     integer :: operates_on = cell_column
     integer :: gh_shape = gh_quadrature_XYoZ
   contains
     procedure, nopass :: code => testkern_qr_code
  end type testkern_qr_type
contains
  subroutine testkern_qr_code(a, b ,c, d)
  end subroutine testkern_qr_code
end module testkern_qr
'''


@pytest.fixture(scope="function", autouse=True)
def setup():
    '''Make sure that all tests here use LFRic as API.'''
    Config.get().api = "lfric"


def test_get_op_wrong_name():
    ''' Tests that the get_operator_name() utility raises an error
    if passed the name of something that is not a valid operator '''
    with pytest.raises(GenerationError) as err:
        FunctionSpace("w3", None).get_operator_name("not_an_op")
    assert "Unsupported name 'not_an_op' found" in str(err.value)


def test_ad_op_type_invalid_data_type():
    ''' Tests that an error is raised when the argument descriptor
    metadata for an operator has an invalid data type. '''
    code = CODE.replace(
        "arg_type(gh_operator, gh_real,    gh_read, w2, w2)",
        "arg_type(gh_operator, gh_clear,    gh_read, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    const = LFRicConstants()
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert (f"In the LFRic API the 2nd argument of a 'meta_arg' entry should "
            f"be a valid data type (one of {const.VALID_SCALAR_DATA_TYPES}), "
            f"but found 'gh_clear' in 'arg_type(gh_operator, gh_clear, "
            f"gh_read, w2)'." in str(excinfo.value))


def test_ad_op_type_too_few_args():
    ''' Tests that an error is raised when the operator descriptor
    metadata has fewer than 5 args. '''
    code = CODE.replace(
        "arg_type(gh_operator, gh_real,    gh_read, w2, w2)",
        "arg_type(gh_operator, gh_real,    gh_read, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    const = LFRicConstants()
    assert (f"'meta_arg' entry must have 5 arguments if its first "
            f"argument is an operator (one of {const.VALID_OPERATOR_NAMES})"
            in str(excinfo.value))


def test_ad_op_type_too_many_args():
    ''' Tests that an error is raised when the operator descriptor
    metadata has more than 5 args. '''
    code = CODE.replace(
        "arg_type(gh_operator, gh_real,    gh_read, w2, w2)",
        "arg_type(gh_operator, gh_real,    gh_read, w2, w2, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert "'meta_arg' entry must have 5 arguments" in str(excinfo.value)


def test_ad_op_type_4th_arg_not_space():
    ''' Tests that an error is raised when the 4th entry in the operator
    metadata contains something that is not a valid function space. '''
    code = CODE.replace(
        "arg_type(gh_operator, gh_real,    gh_read, w2, w2)",
        "arg_type(gh_operator, gh_real,    gh_read, wbroke, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("LFRic API argument 4 of a 'meta_arg' operator entry "
            "must be a valid function-space name" in str(excinfo.value))


def test_ad_op_type_5th_arg_not_space():
    ''' Tests that an error is raised when the 5th entry in the operator
    metadata contains something that is not a valid function space. '''
    code = CODE.replace(
        "arg_type(gh_operator, gh_real,    gh_read, w2, w2)",
        "arg_type(gh_operator, gh_real,    gh_read, w2, wbroke)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("LFRic API argument 5 of a 'meta_arg' operator entry "
            "must be a valid function-space name" in str(excinfo.value))


def test_no_vector_operator():
    ''' Test that we raise an error when kernel metadata erroneously
    specifies a vector operator argument. '''
    code = CODE.replace(
        "arg_type(gh_operator, gh_real,    gh_read, w2, w2)",
        "arg_type(gh_operator*3, gh_real,    gh_read, w2, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("vector notation is only supported for ['gh_field'] "
            "argument types but found 'gh_operator * 3'" in
            str(excinfo.value))


def test_ad_op_type_init_wrong_argument_type():
    ''' Test that an error is raised if something other than an operator
    is passed to the LFRicArgDescriptor._init_operator() method. '''
    ast = fpapi.parse(CODE, ignore_comments=False)
    name = "testkern_qr_type"
    metadata = LFRicKernMetadata(ast, name=name)
    # Get an argument which is not an operator
    wrong_arg = metadata._inits[1]
    with pytest.raises(InternalError) as excinfo:
        LFRicArgDescriptor(
            wrong_arg, metadata.iterates_over, 0)._init_operator(wrong_arg)
    assert ("Expected an operator argument but got an argument of type "
            "'gh_field'." in str(excinfo.value))


def test_ad_op_type_init_wrong_data_type():
    ''' Test that an error is raised if an invalid data type
    is passed to the LFRicArgDescriptor._init_operator() method. '''
    ast = fpapi.parse(CODE, ignore_comments=False)
    name = "testkern_qr_type"
    metadata = LFRicKernMetadata(ast, name=name)
    # Get an operator argument descriptor and set a wrong data type
    op_arg = metadata._inits[3]
    op_arg.args[1].name = "gh_integer"
    with pytest.raises(ParseError) as excinfo:
        LFRicArgDescriptor(
            op_arg, metadata.iterates_over, 0)._init_operator(op_arg)
    const = LFRicConstants()
    assert (f"In the LFRic API the allowed data types for operator "
            f"arguments are one of {const.VALID_OPERATOR_DATA_TYPES}, but "
            f"found 'gh_integer' in 'arg_type(gh_operator, gh_integer, "
            f"gh_read, w2, w2)'." in str(excinfo.value))


def test_ad_op_type_wrong_access():
    ''' Test that an error is raised if an operator has 'gh_inc' access. '''
    code = CODE.replace(
        "arg_type(gh_operator, gh_real,    gh_read, w2, w2)",
        "arg_type(gh_operator, gh_real,    gh_inc, w2, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("In the LFRic API, allowed accesses for operators are "
            "['gh_read', 'gh_write', 'gh_readwrite'] because they behave "
            "as discontinuous quantities, but found 'gh_inc'" in
            str(excinfo.value))


def test_ad_op_invalid_field_data_type():
    ''' Check that we raise the expected error if the metadata for a kernel
    that has an LMA operator argument contains a field argument with an
    invalid data type (other than 'gh_real'). '''
    code = CODE.replace(
        "arg_type(gh_field,    gh_real,    gh_read, w3)",
        "arg_type(gh_field,    gh_integer, gh_read, w3)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("In the LFRic API a kernel that has an LMA operator argument "
            "must only have field arguments with 'gh_real' data type but "
            "kernel 'testkern_qr_type' has a field argument with "
            "'gh_integer' data type." in str(excinfo.value))


def test_arg_descriptor_op():
    ''' Test that the LFRicArgDescriptor argument representation works
    as expected when we have an operator. '''
    ast = fpapi.parse(CODE, ignore_comments=False)
    name = "testkern_qr_type"
    metadata = LFRicKernMetadata(ast, name=name)
    operator_descriptor = metadata.arg_descriptors[3]

    # Assert correct string representation from LFRicArgDescriptor
    result = str(operator_descriptor)
    expected_output = (
        "LFRicArgDescriptor object\n"
        "  argument_type[0]='gh_operator'\n"
        "  data_type[1]='gh_real'\n"
        "  access_descriptor[2]='gh_read'\n"
        "  function_space_to[3]='w2'\n"
        "  function_space_from[4]='w2'\n")
    assert expected_output in result

    # Check LFRicArgDescriptor argument properties
    assert operator_descriptor.argument_type == "gh_operator"
    assert operator_descriptor.data_type == "gh_real"
    assert operator_descriptor.function_space_to == "w2"
    assert operator_descriptor.function_space_from == "w2"
    assert operator_descriptor.function_space == "w2"
    assert operator_descriptor.function_spaces == ['w2', 'w2']
    assert str(operator_descriptor.access) == "READ"
    assert operator_descriptor.mesh is None
    assert operator_descriptor.stencil is None
    assert operator_descriptor.vector_size == 1


def test_fs_descriptor_wrong_type():
    ''' Tests that an error is raised when the function space descriptor
    metadata is not of type func_type. '''
    code = CODE.replace("func_type(w2", "funced_up_type(w2", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert ("'meta_funcs' metadata must consist of an array of structure "
            "constructors, all of type 'func_type'" in str(excinfo.value))
    # Check that the LFRicFuncDescriptor rejects it too

    class FakeCls():
        ''' Class that just has a name property (which is not "func_type") '''
        name = "not-func-type"

    with pytest.raises(ParseError) as excinfo:
        LFRicFuncDescriptor(FakeCls())
    assert ("each meta_func entry must be of type 'func_type' but found "
            in str(excinfo.value))


def test_fs_descriptor_too_few_args():
    ''' Tests that an error is raised when there are two few arguments in
    the function space descriptor metadata (must be at least 2). '''
    code = CODE.replace("w1, gh_basis", "w1", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert 'meta_func entry must have at least 2 args' in str(excinfo.value)


def test_fs_desc_invalid_fs_type():
    ''' Tests that an error is raised when an invalid function space name
    is provided as the first argument. '''
    code = CODE.replace("w3, gh_basis", "w4, gh_basis", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert '1st argument of a meta_func entry should be a valid function ' + \
        'space name' in str(excinfo.value)


def test_fs_desc_replicated_fs_type():
    ''' Tests that an error is raised when a function space name
    is replicated. '''
    code = CODE.replace("w3, gh_basis", "w1, gh_basis", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert "function spaces specified in 'meta_funcs' must be unique" \
        in str(excinfo.value)


def test_fs_desc_invalid_op_type():
    ''' Tests that an error is raised when an invalid function space
    operator name is provided as an argument. '''
    code = CODE.replace("w2, gh_diff_basis", "w2, gh_dif_basis", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert '2nd argument and all subsequent arguments of a meta_func ' + \
        'entry should be one of' in str(excinfo.value)


def test_fs_desc_replicated_op_type():
    ''' Tests that an error is raised when a function space
    operator name is replicated as an argument. '''
    code = CODE.replace("w3, gh_basis, gh_diff_basis",
                        "w3, gh_basis, gh_basis", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert 'error to specify an operator name more than once' \
        in str(excinfo.value)


def test_fsdesc_fs_not_in_argdesc():
    ''' Tests that an error is raised when a function space
    name is provided that has not been used in the arg descriptor. '''
    code = CODE.replace("w3, gh_basis", "w0, gh_basis", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = LFRicKernMetadata(ast, name=name)
    assert "function spaces specified in 'meta_funcs' must exist in " + \
        "'meta_args'" in str(excinfo.value)


def test_invoke_uniq_declns_valid_access_op():
    ''' Tests that all valid access modes for user-defined LMA operator
    arguments (AccessType.READ, AccessType.WRITE, AccessType.READWRITE)
    are accepted by Invoke.unique_declarations(). Also tests the
    correctness of names of arguments and their proxies.

    '''
    # Test READ
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.5.2_multikernel_invokes.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    ops_read_args = (psy.invokes.invoke_list[0].unique_declarations(
        ["gh_operator"], access=AccessType.READ))
    ops_read = [arg.declaration_name for arg in ops_read_args]
    ops_proxy_read = [arg.proxy_declaration_name for arg in ops_read_args]
    assert ops_read == ["op", "op3", "op4", "op5"]
    assert ops_proxy_read == ["op_proxy", "op3_proxy",
                              "op4_proxy", "op5_proxy"]

    # Test READWRITE
    ops_readwritten_args = (psy.invokes.invoke_list[0].unique_declarations(
        ["gh_operator"], access=AccessType.READWRITE))
    ops_readwritten = [arg.declaration_name for arg in ops_readwritten_args]
    ops_proxy_readwritten = [arg.proxy_declaration_name for arg in
                             ops_readwritten_args]
    assert ops_readwritten == ["op", "op2"]
    assert ops_proxy_readwritten == ["op_proxy", "op2_proxy"]

    # Test WRITE
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "11.4_any_discontinuous_space.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    ops_written_args = (psy.invokes.invoke_list[0].unique_declarations(
        ["gh_operator"], access=AccessType.WRITE))
    ops_written = [arg.declaration_name for arg in ops_written_args]
    ops_proxy_written = [arg.proxy_declaration_name for arg
                         in ops_written_args]
    assert ops_written == ["op4"]
    assert ops_proxy_written == ["op4_proxy"]


def test_operator_arg_lfricconst_properties(monkeypatch):
    ''' Tests that properties of supported LMA operator arguments
    ('real'-valued 'operator_type') defined in LFRicConstants are
    correctly set up in the LFRicKernelArgument class.

    '''
    ast = fpapi.parse(CODE, ignore_comments=False)
    name = "testkern_qr_type"
    metadata = LFRicKernMetadata(ast, name=name)
    kernel = LFRicKern()
    kernel.load_meta(metadata)

    op_arg = kernel.arguments.args[3]
    assert op_arg.module_name == "operator_mod"
    assert op_arg.data_type == "operator_type"
    assert op_arg.proxy_data_type == "operator_proxy_type"
    assert op_arg.intrinsic_type == "real"
    assert op_arg.precision == "r_def"

    # Monkeypatch to check with an invalid argument type of an
    # operator argument. The LFRicConstants class needs to be
    # initialised before the monkeypatch.
    _ = LFRicConstants()
    monkeypatch.setattr(LFRicConstants, "VALID_OPERATOR_NAMES",
                        ["tuxedo"])
    monkeypatch.setattr(op_arg, "_argument_type", "tuxedo")
    with pytest.raises(InternalError) as err:
        op_arg._init_data_type_properties(None, check=False)
    assert ("Expected 'gh_operator' or 'gh_columnwise_operator' "
            "argument type but found 'tuxedo'." in str(err.value))


def test_operator(tmpdir):
    ''' Tests that an LMA operator is implemented correctly in the PSy
    layer. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "10_operator.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert (
        "subroutine invoke_0_testkern_operator_type(mm_w0, coord, a, qr)"
        in generated_code)
    assert "type(operator_type), intent(in) :: mm_w0" in generated_code
    assert "type(operator_proxy_type) :: mm_w0_proxy" in generated_code
    assert "mm_w0_proxy = mm_w0%get_proxy()" in generated_code
    assert ("call testkern_operator_code(cell, nlayers_mm_w0, "
            "mm_w0_proxy%ncell_3d, mm_w0_local_stencil, coord_1_data, "
            "coord_2_data, coord_3_data, a, ndf_w0, undf_w0, "
            "map_w0(:,cell), basis_w0_qr, diff_basis_w0_qr, np_xy_qr, "
            "np_z_qr, weights_xy_qr, weights_z_qr)") in generated_code


def test_operator_different_spaces(tmpdir):
    ''' Tests that an operator with different to and from spaces is
    implemented correctly in the PSy layer.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.3_operator_different_spaces.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert """\
module operator_example_psy
  use operator_mod, only : operator_proxy_type, operator_type
  use field_mod, only : field_proxy_type, field_type
  use assemble_weak_derivative_w3_w2_kernel_mod, only : \
assemble_weak_derivative_w3_w2_kernel_code
  use constants_mod, only : r_def
  implicit none
  public

  contains
  subroutine invoke_0_assemble_weak_derivative_w3_w2_kernel_type(mapping, \
coord, qr)
    use quadrature_xyoz_mod, only : quadrature_xyoz_proxy_type, \
quadrature_xyoz_type
    use mesh_mod, only : mesh_type
    use function_space_mod, only : BASIS, DIFF_BASIS
    use constants_mod, only : i_def
    type(operator_type), intent(in) :: mapping
    type(field_type), dimension(3), intent(in) :: coord
    type(quadrature_xyoz_type), intent(in) :: qr
    integer(kind=i_def) :: cell
    type(mesh_type), pointer :: mesh => null()
    integer(kind=i_def) :: max_halo_depth_mesh
    real(kind=r_def), pointer, dimension(:) :: coord_1_data => null()
    real(kind=r_def), pointer, dimension(:) :: coord_2_data => null()
    real(kind=r_def), pointer, dimension(:) :: coord_3_data => null()
    real(kind=r_def), pointer, dimension(:,:,:) :: mapping_local_stencil => \
null()
    integer(kind=i_def) :: nlayers_mapping
    integer(kind=i_def) :: ndf_w3
    integer(kind=i_def) :: ndf_w2
    integer(kind=i_def) :: ndf_w0
    integer(kind=i_def) :: undf_w0
    integer(kind=i_def), pointer :: map_w0(:,:) => null()
    type(field_proxy_type), dimension(3) :: coord_proxy
    type(operator_proxy_type) :: mapping_proxy
    integer(kind=i_def) :: np_xy_qr
    integer(kind=i_def) :: np_z_qr
    real(kind=r_def), pointer :: weights_xy_qr(:) => null()
    real(kind=r_def), pointer :: weights_z_qr(:) => null()
    type(quadrature_xyoz_proxy_type) :: qr_proxy
    integer(kind=i_def) :: diff_dim_w0
    integer(kind=i_def) :: dim_w3
    integer(kind=i_def) :: diff_dim_w2
    real(kind=r_def), allocatable :: diff_basis_w0_qr(:,:,:,:)
    real(kind=r_def), allocatable :: basis_w3_qr(:,:,:,:)
    real(kind=r_def), allocatable :: diff_basis_w2_qr(:,:,:,:)
    integer(kind=i_def) :: loop0_start
    integer(kind=i_def) :: loop0_stop

    ! Initialise field and/or operator proxies
    mapping_proxy = mapping%get_proxy()
    mapping_local_stencil => mapping_proxy%local_stencil
    coord_proxy(1) = coord(1)%get_proxy()
    coord_1_data => coord_proxy(1)%data
    coord_proxy(2) = coord(2)%get_proxy()
    coord_2_data => coord_proxy(2)%data
    coord_proxy(3) = coord(3)%get_proxy()
    coord_3_data => coord_proxy(3)%data

    ! Initialise number of layers
    nlayers_mapping = mapping_proxy%fs_from%get_nlayers()

    ! Create a mesh object
    mesh => mapping_proxy%fs_from%get_mesh()
    max_halo_depth_mesh = mesh%get_halo_depth()

    ! Look-up dofmaps for each function space
    map_w0 => coord_proxy(1)%vspace%get_whole_dofmap()

    ! Initialise number of DoFs for w3
    ndf_w3 = mapping_proxy%fs_to%get_ndf()

    ! Initialise number of DoFs for w2
    ndf_w2 = mapping_proxy%fs_from%get_ndf()

    ! Initialise number of DoFs for w0
    ndf_w0 = coord_proxy(1)%vspace%get_ndf()
    undf_w0 = coord_proxy(1)%vspace%get_undf()

    ! Look-up quadrature variables
    qr_proxy = qr%get_quadrature_proxy()
    np_xy_qr = qr_proxy%np_xy
    np_z_qr = qr_proxy%np_z
    weights_xy_qr => qr_proxy%weights_xy
    weights_z_qr => qr_proxy%weights_z

    ! Allocate basis/diff-basis arrays
    diff_dim_w0 = coord_proxy(1)%vspace%get_dim_space_diff()
    dim_w3 = mapping_proxy%fs_to%get_dim_space()
    diff_dim_w2 = mapping_proxy%fs_from%get_dim_space_diff()
    ALLOCATE(diff_basis_w0_qr(diff_dim_w0,ndf_w0,np_xy_qr,np_z_qr))
    ALLOCATE(basis_w3_qr(dim_w3,ndf_w3,np_xy_qr,np_z_qr))
    ALLOCATE(diff_basis_w2_qr(diff_dim_w2,ndf_w2,np_xy_qr,np_z_qr))

    ! Compute basis/diff-basis arrays
    call qr%compute_function(DIFF_BASIS, coord_proxy(1)%vspace, diff_dim_w0, \
ndf_w0, diff_basis_w0_qr)
    call qr%compute_function(BASIS, mapping_proxy%fs_to, dim_w3, ndf_w3, \
basis_w3_qr)
    call qr%compute_function(DIFF_BASIS, mapping_proxy%fs_from, diff_dim_w2, \
ndf_w2, diff_basis_w2_qr)

    ! Set-up all of the loop bounds
    loop0_start = 1
    loop0_stop = mesh%get_last_halo_cell(1)

    ! Call kernels and communication routines
    if (coord_proxy(1)%is_dirty(depth=1)) then
      call coord_proxy(1)%halo_exchange(depth=1)
    end if
    if (coord_proxy(2)%is_dirty(depth=1)) then
      call coord_proxy(2)%halo_exchange(depth=1)
    end if
    if (coord_proxy(3)%is_dirty(depth=1)) then
      call coord_proxy(3)%halo_exchange(depth=1)
    end if
    do cell = loop0_start, loop0_stop, 1
      call assemble_weak_derivative_w3_w2_kernel_code(cell, nlayers_mapping, \
mapping_proxy%ncell_3d, mapping_local_stencil, coord_1_data, coord_2_data, \
coord_3_data, ndf_w3, basis_w3_qr, ndf_w2, diff_basis_w2_qr, ndf_w0, undf_w0, \
map_w0(:,cell), diff_basis_w0_qr, np_xy_qr, np_z_qr, weights_xy_qr, \
weights_z_qr)
    enddo

    ! Deallocate basis arrays
    DEALLOCATE(basis_w3_qr, diff_basis_w0_qr, diff_basis_w2_qr)

  end subroutine invoke_0_assemble_weak_derivative_w3_w2_kernel_type

end module operator_example_psy
""" == generated_code


def test_operator_nofield(tmpdir):
    ''' Tests that an operator with no field on the same space is
    implemented correctly in the PSy layer. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.1_operator_nofield.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    code_str = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert (
        "subroutine invoke_0_testkern_operator_nofield_type(mm_w2, coord, qr)"
        in code_str)
    assert "type(operator_type), intent(in) :: mm_w2" in code_str
    assert "type(operator_proxy_type) :: mm_w2_proxy" in code_str
    assert "mm_w2_proxy = mm_w2%get_proxy()" in code_str
    assert "mm_w2_local_stencil => mm_w2_proxy%local_stencil" in code_str
    assert "undf_w2" not in code_str
    assert "map_w2" not in code_str
    assert ("call testkern_operator_nofield_code(cell, nlayers_mm_w2, "
            "mm_w2_proxy%ncell_3d, mm_w2_local_stencil, "
            "coord_1_data, coord_2_data, coord_3_data, "
            "ndf_w2, basis_w2_qr, ndf_w0, undf_w0, "
            "map_w0(:,cell), diff_basis_w0_qr, np_xy_qr, np_z_qr, "
            "weights_xy_qr, weights_z_qr)" in code_str)


def test_operator_nofield_different_space(tmpdir):
    ''' Tests that an operator with no field on different spaces is
    implemented correctly in the PSy layer. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.5_operator_no_field_different_"
                                        "space.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    gen = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert "mesh => my_mapping_proxy%fs_from%get_mesh()" in gen
    assert "nlayers_my_mapping = my_mapping_proxy%fs_from%get_nlayers()" in gen
    assert "ndf_w3 = my_mapping_proxy%fs_from%get_ndf()" in gen
    assert "ndf_w2 = my_mapping_proxy%fs_to%get_ndf()" in gen
    # We compute operators redundantly (out to the L1 halo)
    assert "loop0_stop = mesh%get_last_halo_cell(1)" in gen
    assert ("(cell, nlayers_my_mapping, my_mapping_proxy%ncell_3d, "
            "my_mapping_local_stencil, ndf_w2, ndf_w3)" in gen)


def test_operator_nofield_scalar(tmpdir):
    ''' Tests that an operator with no field and a
    scalar argument is implemented correctly in the PSy layer '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.6_operator_no_field_scalar.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    gen = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)
    assert "mesh => my_mapping_proxy%fs_from%get_mesh()" in gen
    assert "nlayers_my_mapping = my_mapping_proxy%fs_from%get_nlayers()" in gen
    assert "ndf_w2 = my_mapping_proxy%fs_from%get_ndf()" in gen
    assert "loop0_stop = mesh%get_last_halo_cell(1)" in gen
    assert ("(cell, nlayers_my_mapping, my_mapping_proxy%ncell_3d, my_mapping_"
            "local_stencil, b, ndf_w2, basis_w2_qr, np_xy_qr, np_z_qr, "
            "weights_xy_qr, weights_z_qr)" in gen)


def test_operator_no_dofmap_lookup():
    ''' Check that we use a field rather than an operator to look-up
    a dofmap, even when the operator precedes the field in the argument
    list. Also checks that the correct space of the operator is used
    when looking up ndf and undf. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.9_operator_first.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    code = str(psy.gen)

    # Check that we use the field and not the operator to look-up the dofmap
    assert "theta_proxy%vspace%get_whole_dofmap()" in code
    assert code.count("get_whole_dofmap") == 1
    # Check also that the lookups for ndf and undf use the correct "to"
    # function space of the operator.
    assert ('''\
    ! Initialise number of DoFs for wtheta
    ndf_wtheta = ptheta2_proxy%fs_to%get_ndf()
    undf_wtheta = ptheta2_proxy%fs_to%get_undf()''' in code)


def test_operator_read_level1_halo(tmpdir):
    ''' Check that we raise an error if a kernel attempts to read from an
    operator beyond the level-1 halo. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.7_operator_read.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[0]
    # Modify the loop bound so that we attempt to read from the L2 halo
    # (of the operator)
    loop.set_upper_bound("cell_halo", halo_depth=2)
    # Attempt to generate the code
    with pytest.raises(VisitorError) as excinfo:
        _ = psy.gen
    assert ("Kernel 'testkern_operator_read_code' reads from an operator and "
            "therefore cannot be used for cells beyond the level 1 halo. "
            "However the containing loop goes out to level 2"
            in str(excinfo.value))


def test_operator_bc_kernel(tmpdir):
    ''' Tests that a kernel with a particular name (starting by
    'boundary_dofs_') is recognised as a kernel that applies boundary
    conditions to operators and that appropriate code is added to support
    this: the function space to get the boundary_dofs is the fs_to of the
    associated operator.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "12.4_enforce_op_bc_kernel.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)
    output1 = (
        "integer(kind=i_def), pointer :: boundary_dofs_op_a(:,:) => null()")
    assert output1 in generated_code
    output2 = "boundary_dofs_op_a => op_a_proxy%fs_to%get_boundary_dofs()"
    assert output2 in generated_code
    output3 = (
        "call enforce_operator_bc_code(cell, nlayers_op_a, "
        "op_a_proxy%ncell_3d, op_a_local_stencil, ndf_as1_op_a, "
        "ndf_as2_op_a, boundary_dofs_op_a)")
    assert output3 in generated_code

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_operator_bc_kernel_fld_err(monkeypatch, dist_mem):
    ''' Test that we reject the recognised operator boundary conditions
    kernel if its argument is not an operator '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "12.4_enforce_op_bc_kernel.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[0]
    call = loop.loop_body[0]
    arg = call.arguments.args[0]
    # Monkeypatch the argument object so that it thinks it is a
    # field rather than an operator
    monkeypatch.setattr(arg, "_argument_type", value="gh_field")
    # We have to populate the Symbol table to get to the desired error.
    schedule.symbol_table.find_or_create_tag("op_a:data")
    schedule.symbol_table.find_or_create("undf_as1_op_a",
                                         symbol_type=symbols.DataSymbol,
                                         datatype=symbols.UnresolvedType())
    with pytest.raises(VisitorError) as excinfo:
        _ = psy.gen
    assert ("Expected an LMA operator from which to look-up boundary dofs "
            "but kernel enforce_operator_bc_code has argument gh_field") \
        in str(excinfo.value)


def test_operator_bc_kernel_multi_args_err(dist_mem):
    ''' Test that we reject the recognised operator boundary conditions
    kernel if it has more than one argument '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "12.4_enforce_op_bc_kernel.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[0]
    call = loop.loop_body[0]
    arg = call.arguments.args[0]
    # We have to populate the Symbol table to get to the desired error.
    schedule.symbol_table.find_or_create_tag("op_a:data")
    schedule.symbol_table.find_or_create("undf_as1_op_a",
                                         symbol_type=symbols.DataSymbol,
                                         datatype=symbols.UnresolvedType())
    # Make the list of arguments invalid by duplicating (a copy of)
    # this argument. We take a copy because otherwise, when we change
    # the type of arg 1 below, we change it for both.
    call.arguments.args.append(copy.copy(arg))
    with pytest.raises(VisitorError) as excinfo:
        _ = psy.gen
    assert ("Kernel enforce_operator_bc_code has 2 arguments when it "
            "should only have 1 (an LMA operator)") in str(excinfo.value)
    # And again but make the second argument a field this time
    call.arguments.args[1]._argument_type = "gh_field"
    with pytest.raises(VisitorError) as excinfo:
        _ = psy.gen
    assert ("Kernel enforce_operator_bc_code has 2 arguments when it "
            "should only have 1 (an LMA operator)") in str(excinfo.value)


def test_operator_bc_kernel_wrong_access_err(dist_mem, monkeypatch):
    ''' Test that we reject the recognised operator boundary conditions
    kernel if its operator argument has the wrong access type '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "12.4_enforce_op_bc_kernel.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[0]
    call = loop.loop_body[0]
    arg = call.arguments.args[0]
    monkeypatch.setattr(arg, "_access", AccessType.READ)
    # We have to monkeypatch iteration_space_arg() too to reach the error
    # we want to exercise.
    monkeypatch.setattr(call.arguments, "iteration_space_arg",
                        lambda: symbols.Symbol("var"))
    with pytest.raises(VisitorError) as excinfo:
        _ = psy.gen
    assert ("applies boundary conditions to an operator. However its "
            "operator argument has access gh_read rather than "
            "gh_readwrite") in str(excinfo.value)


# Operators : spaces and intent (except for Wchi space as the fields on
# this space are read-only).
OPERATORS = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(13) =                                 &
          (/ arg_type(gh_operator, gh_real, gh_write,     w0, w0),   &
             arg_type(gh_operator, gh_real, gh_readwrite, w1, w1),   &
             arg_type(gh_operator, gh_real, gh_read,      w2, w2),   &
             arg_type(gh_operator, gh_real, gh_read,      w2h, w2h), &
             arg_type(gh_operator, gh_real, gh_readwrite, w2v, w2v), &
             arg_type(gh_operator, gh_real, gh_write,     w2broken,  &
                                                          w2broken), &
             arg_type(gh_operator, gh_real, gh_read,      w2trace,   &
                                                          w2trace),  &
             arg_type(gh_operator, gh_real, gh_read,      w2htrace,  &
                                                          w2htrace), &
             arg_type(gh_operator, gh_real, gh_readwrite, w2vtrace,  &
                                                          w2vtrace), &
             arg_type(gh_operator, gh_real, gh_write,     w3, w3),   &
             arg_type(gh_operator, gh_real, gh_write,     wtheta,    &
                                                          wtheta),   &
             arg_type(gh_operator, gh_real, gh_read,                 &
                                          any_space_1, any_space_1), &
             arg_type(gh_operator, gh_real, gh_read,                 &
               any_discontinuous_space_1, any_discontinuous_space_1) &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => dummy_code
  end type dummy_type
contains
  subroutine dummy_code()
  end subroutine dummy_code
end module dummy_mod
'''


def test_operators(fortran_writer):
    ''' Test that operators are handled correctly for kernel stubs (except
    for Wchi space as the fields on this space are read-only).

    '''
    ast = fpapi.parse(OPERATORS, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    generated_code = fortran_writer(kernel.gen_stub)
    assert """\
module dummy_mod
  implicit none
  public

  contains
  subroutine dummy_code(cell, nlayers, op_1_ncell_3d, op_1, op_2_ncell_3d, \
op_2, op_3_ncell_3d, op_3, op_4_ncell_3d, op_4, op_5_ncell_3d, op_5, \
op_6_ncell_3d, op_6, op_7_ncell_3d, op_7, op_8_ncell_3d, op_8, op_9_ncell_3d, \
op_9, op_10_ncell_3d, op_10, op_11_ncell_3d, op_11, op_12_ncell_3d, op_12, \
op_13_ncell_3d, op_13, ndf_w0, ndf_w1, ndf_w2, ndf_w2h, ndf_w2v, \
ndf_w2broken, ndf_w2trace, ndf_w2htrace, ndf_w2vtrace, ndf_w3, ndf_wtheta, \
ndf_as1_op_12, ndf_ads1_op_13)
    use constants_mod
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w0
    integer(kind=i_def), intent(in) :: ndf_w1
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: ndf_w2h
    integer(kind=i_def), intent(in) :: ndf_w2v
    integer(kind=i_def), intent(in) :: ndf_w2broken
    integer(kind=i_def), intent(in) :: ndf_w2trace
    integer(kind=i_def), intent(in) :: ndf_w2htrace
    integer(kind=i_def), intent(in) :: ndf_w2vtrace
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: ndf_wtheta
    integer(kind=i_def), intent(in) :: ndf_as1_op_12
    integer(kind=i_def), intent(in) :: ndf_ads1_op_13
    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: op_1_ncell_3d
    real(kind=r_def), dimension(op_1_ncell_3d,ndf_w0,ndf_w0), intent(inout) \
:: op_1
    integer(kind=i_def), intent(in) :: op_2_ncell_3d
    real(kind=r_def), dimension(op_2_ncell_3d,ndf_w1,ndf_w1), intent(inout) \
:: op_2
    integer(kind=i_def), intent(in) :: op_3_ncell_3d
    real(kind=r_def), dimension(op_3_ncell_3d,ndf_w2,ndf_w2), intent(in) \
:: op_3
    integer(kind=i_def), intent(in) :: op_4_ncell_3d
    real(kind=r_def), dimension(op_4_ncell_3d,ndf_w2h,ndf_w2h), intent(in) \
:: op_4
    integer(kind=i_def), intent(in) :: op_5_ncell_3d
    real(kind=r_def), dimension(op_5_ncell_3d,ndf_w2v,ndf_w2v), \
intent(inout) :: op_5
    integer(kind=i_def), intent(in) :: op_6_ncell_3d
    real(kind=r_def), dimension(op_6_ncell_3d,ndf_w2broken,ndf_w2broken), \
intent(inout) :: op_6
    integer(kind=i_def), intent(in) :: op_7_ncell_3d
    real(kind=r_def), dimension(op_7_ncell_3d,ndf_w2trace,ndf_w2trace), \
intent(in) :: op_7
    integer(kind=i_def), intent(in) :: op_8_ncell_3d
    real(kind=r_def), dimension(op_8_ncell_3d,ndf_w2htrace,ndf_w2htrace), \
intent(in) :: op_8
    integer(kind=i_def), intent(in) :: op_9_ncell_3d
    real(kind=r_def), dimension(op_9_ncell_3d,ndf_w2vtrace,ndf_w2vtrace), \
intent(inout) :: op_9
    integer(kind=i_def), intent(in) :: op_10_ncell_3d
    real(kind=r_def), dimension(op_10_ncell_3d,ndf_w3,ndf_w3), intent(inout) \
:: op_10
    integer(kind=i_def), intent(in) :: op_11_ncell_3d
    real(kind=r_def), dimension(op_11_ncell_3d,ndf_wtheta,ndf_wtheta\
), intent(inout) :: op_11
    integer(kind=i_def), intent(in) :: op_12_ncell_3d
    real(kind=r_def), dimension(op_12_ncell_3d,ndf_as1_op_12,\
ndf_as1_op_12), intent(in) :: op_12
    integer(kind=i_def), intent(in) :: op_13_ncell_3d
    real(kind=r_def), dimension(op_13_ncell_3d,ndf_ads1_op_13,\
ndf_ads1_op_13), intent(in) :: op_13


  end subroutine dummy_code

end module dummy_mod
""" == generated_code

    # Try with unsupported types
    lma_args = args_filter(kernel.arguments.args, arg_types=["gh_operator"])
    lma_args[0]._intrinsic_type = "logical"
    with pytest.raises(NotImplementedError) as err:
        _ = kernel.gen_stub
    assert ("Only REAL and INTEGER LMA Operator types are supported, but found"
            " 'logical'" in str(err.value))


OPERATOR_DIFFERENT_SPACES = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(1) =                           &
          (/ arg_type(gh_operator, gh_real, gh_write, w0, w1) &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => dummy_code
  end type dummy_type
contains
  subroutine dummy_code()
  end subroutine dummy_code
end module dummy_mod
'''


def test_stub_operator_different_spaces(fortran_writer):
    ''' Test that the correct function spaces are provided in the
    correct order when generating a kernel stub with an operator on
    different spaces.

    '''
    # Check the original code (to- and from- spaces both continuous)
    ast = fpapi.parse(OPERATOR_DIFFERENT_SPACES, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    result = fortran_writer(kernel.gen_stub)
    assert "(cell, nlayers, op_1_ncell_3d, op_1, ndf_w0, ndf_w1)" in result
    assert "dimension(op_1_ncell_3d,ndf_w0,ndf_w1)" in result
    # Check for discontinuous to- and from- spaces
    code = OPERATOR_DIFFERENT_SPACES.replace(
        "(gh_operator, gh_real, gh_write, w0, w1)",
        "(gh_operator, gh_real, gh_write, w3, any_discontinuous_space_2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    result = fortran_writer(kernel.gen_stub)
    assert ("(cell, nlayers, op_1_ncell_3d, op_1, ndf_w3, ndf_ads2_op_1)"
            in result)
    assert "dimension(op_1_ncell_3d,ndf_w3,ndf_ads2_op_1)" in result
    field_descriptor = metadata.arg_descriptors[0]
    result = str(field_descriptor)
    assert "function_space_to[3]='w3'" in result
    assert "function_space_from[4]='any_discontinuous_space_2'" in result
