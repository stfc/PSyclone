# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019 Science and Technology Facilities Council.
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
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab
# Modified I. Kavcic, Met Office

''' This module tests the support for LMA operators in the Dynamo 0.3 API
using pytest. '''

# imports
from __future__ import absolute_import, print_function
import os
import pytest
import fparser
from fparser import api as fpapi
from psyclone.parse import parse, ParseError
from psyclone.psyGen import PSyFactory, GenerationError
from psyclone.dynamo0p3 import DynKernMetadata, DynKern, FunctionSpace
from psyclone_test_utils import code_compiles, TEST_COMPILE

# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")

TEST_API = "dynamo0.3"

CODE = '''
module testkern_qr
  type, extends(kernel_type) :: testkern_qr_type
     type(arg_type), meta_args(6) =                 &
          (/ arg_type(gh_real, gh_read),            &
             arg_type(gh_field,gh_write,w1),        &
             arg_type(gh_field,gh_read, w2),        &
             arg_type(gh_operator,gh_read, w2, w2), &
             arg_type(gh_field,gh_read, w3),        &
             arg_type(gh_integer, gh_read)          &
           /)
     type(func_type), dimension(3) :: meta_funcs =  &
          (/ func_type(w1, gh_basis),               &
             func_type(w2, gh_diff_basis),          &
             func_type(w3, gh_basis, gh_diff_basis) &
           /)
     integer, parameter :: iterates_over = cells
     integer, parameter :: gh_shape = gh_quadrature_XYoZ
   contains
     procedure() :: code => testkern_qr_code
  end type testkern_qr_type
contains
  subroutine testkern_qr_code(a,b,c,d)
  end subroutine testkern_qr_code
end module testkern_qr
'''


def test_get_op_wrong_name():
    ''' Tests that the get_operator_name() utility raises an error
    if passed the name of something that is not a valid operator '''
    from psyclone.dynamo0p3 import get_fs_operator_name
    with pytest.raises(GenerationError) as err:
        get_fs_operator_name("not_an_op", FunctionSpace("w3", None))
    assert "Unsupported name 'not_an_op' found" in str(err)


def test_ad_op_type_too_few_args():
    ''' Tests that an error is raised when the operator descriptor
    metadata has fewer than 4 args. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("arg_type(gh_operator,gh_read, w2, w2)",
                        "arg_type(gh_operator,gh_read, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'meta_arg entry must have 4 arguments' in str(excinfo.value)


def test_ad_op_type_too_many_args():
    ''' Tests that an error is raised when the operator descriptor
    metadata has more than 4 args. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("arg_type(gh_operator,gh_read, w2, w2)",
                        "arg_type(gh_operator,gh_read, w2, w2, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'meta_arg entry must have 4 arguments' in str(excinfo.value)


def test_ad_op_type_wrong_3rd_arg():
    ''' Tests that an error is raised when the 3rd entry in the operator
    descriptor metadata is invalid. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("arg_type(gh_operator,gh_read, w2, w2)",
                        "arg_type(gh_operator,gh_read, woops, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("dynamo0.3 API the 3rd argument of a meta_arg entry must be "
            "a valid function space name" in str(excinfo.value))


def test_ad_op_type_1st_arg_not_space():
    ''' Tests that an error is raised when the operator descriptor
    metadata contains something that is not a valid space. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("arg_type(gh_operator,gh_read, w2, w2)",
                        "arg_type(gh_operator,gh_read, wbroke, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'meta_arg entry must be a valid function space' in \
        str(excinfo.value)


def test_ad_op_type_wrong_access():
    ''' Test that an error is raised if an operator has gh_inc access. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("arg_type(gh_operator,gh_read, w2, w2)",
                        "arg_type(gh_operator,gh_inc, w2, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("In the dynamo0.3 API operators cannot have a 'gh_inc' access"
            in str(excinfo.value))


def test_fs_descriptor_wrong_type():
    ''' Tests that an error is raised when the function space descriptor
    metadata is not of type func_type. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("func_type(w2", "funced_up_type(w2", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert "each meta_func entry must be of type 'func_type'" in \
        str(excinfo.value)


def test_fs_descriptor_too_few_args():
    ''' Tests that an error is raised when there are two few arguments in
    the function space descriptor metadata (must be at least 2). '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("w1, gh_basis", "w1", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'meta_func entry must have at least 2 args' in str(excinfo.value)


def test_fs_desc_invalid_fs_type():
    ''' Tests that an error is raised when an invalid function space name
    is provided as the first argument. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("w3, gh_basis", "w4, gh_basis", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert '1st argument of a meta_func entry should be a valid function ' + \
        'space name' in str(excinfo.value)


def test_fs_desc_replicated_fs_type():
    ''' Tests that an error is raised when a function space name
    is replicated. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("w3, gh_basis", "w1, gh_basis", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'function spaces specified in meta_funcs must be unique' \
        in str(excinfo.value)


def test_fs_desc_invalid_op_type():
    ''' Tests that an error is raised when an invalid function space
    operator name is provided as an argument. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("w2, gh_diff_basis", "w2, gh_dif_basis", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert '2nd argument and all subsequent arguments of a meta_func ' + \
        'entry should be one of' in str(excinfo.value)


def test_fs_desc_replicated_op_type():
    ''' Tests that an error is raised when a function space
    operator name is replicated as an argument. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("w3, gh_basis, gh_diff_basis",
                        "w3, gh_basis, gh_basis", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'error to specify an operator name more than once' \
        in str(excinfo.value)


def test_fsdesc_fs_not_in_argdesc():
    ''' Tests that an error is raised when a function space
    name is provided that has not been used in the arg descriptor. '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    code = CODE.replace("w3, gh_basis", "w0, gh_basis", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'function spaces specified in meta_funcs must exist in ' + \
        'meta_args' in str(excinfo)


def test_operator():
    ''' Tests that a LMA operator is implemented correctly in the PSy
    layer. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "10_operator.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)
    print(generated_code)
    assert generated_code.find("SUBROUTINE invoke_0_testkern_operator"
                               "_type(mm_w0, chi, a, qr)") != -1
    assert generated_code.find("TYPE(operator_type), intent(inout) ::"
                               " mm_w0") != -1
    assert generated_code.find("TYPE(operator_proxy_type) mm_w0_"
                               "proxy") != -1
    assert generated_code.find("mm_w0_proxy = mm_w0%get_proxy()") != -1
    assert ("CALL testkern_operator_code(cell, nlayers, mm_w0_proxy%ncell_3d, "
            "mm_w0_proxy%local_stencil, chi_proxy(1)%data, chi_proxy(2)%data, "
            "chi_proxy(3)%data, a, ndf_w0, undf_w0, map_w0(:,cell), "
            "basis_w0_qr, diff_basis_w0_qr, np_xy_qr, np_z_qr, weights_xy_qr, "
            "weights_z_qr)") in generated_code


def test_operator_different_spaces(tmpdir, f90, f90flags):
    '''tests that an operator with different to and from spaces is
    implemented correctly in the PSy layer'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.3_operator_different_spaces.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)
    print(generated_code)

    if TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag to py.test)
        assert code_compiles(TEST_API, psy, tmpdir, f90, f90flags)

    decl_output = (
        "    SUBROUTINE invoke_0_assemble_weak_derivative_w3_w2_kernel_type"
        "(mapping, chi, qr)\n"
        "      USE assemble_weak_derivative_w3_w2_kernel_mod, ONLY: "
        "assemble_weak_derivative_w3_w2_kernel_code\n"
        "      USE quadrature_xyoz_mod, ONLY: quadrature_xyoz_type, "
        "quadrature_xyoz_proxy_type\n"
        "      USE function_space_mod, ONLY: BASIS, DIFF_BASIS\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      TYPE(field_type), intent(in) :: chi(3)\n"
        "      TYPE(operator_type), intent(inout) :: mapping\n"
        "      TYPE(quadrature_xyoz_type), intent(in) :: qr\n"
        "      INTEGER, pointer :: orientation_w2(:) => null()\n"
        "      INTEGER cell\n"
        "      REAL(KIND=r_def), allocatable :: diff_basis_w0_qr(:,:,:,:), "
        "basis_w3_qr(:,:,:,:), diff_basis_w2_qr(:,:,:,:)\n"
        "      INTEGER diff_dim_w0, dim_w3, diff_dim_w2\n"
        "      REAL(KIND=r_def), pointer :: weights_xy_qr(:) => null(), "
        "weights_z_qr(:) => null()\n"
        "      INTEGER np_xy_qr, np_z_qr\n"
        "      INTEGER nlayers\n"
        "      TYPE(operator_proxy_type) mapping_proxy\n"
        "      TYPE(field_proxy_type) chi_proxy(3)\n"
        "      TYPE(quadrature_xyoz_proxy_type) qr_proxy\n"
        "      INTEGER, pointer :: map_w0(:,:) => null()\n"
        "      INTEGER ndf_w3, ndf_w2, ndf_w0, undf_w0\n"
        "      TYPE(mesh_type), pointer :: mesh => null()\n")
    assert decl_output in generated_code
    output = (
        "      !\n"
        "      ! Initialise field and/or operator proxies\n"
        "      !\n"
        "      mapping_proxy = mapping%get_proxy()\n"
        "      chi_proxy(1) = chi(1)%get_proxy()\n"
        "      chi_proxy(2) = chi(2)%get_proxy()\n"
        "      chi_proxy(3) = chi(3)%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = mapping_proxy%fs_from%get_nlayers()\n"
        "      !\n"
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh => mapping%get_mesh()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w0 => chi_proxy(1)%vspace%get_whole_dofmap()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w3\n"
        "      !\n"
        "      ndf_w3 = mapping_proxy%fs_to%get_ndf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2\n"
        "      !\n"
        "      ndf_w2 = mapping_proxy%fs_from%get_ndf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w0\n"
        "      !\n"
        "      ndf_w0 = chi_proxy(1)%vspace%get_ndf()\n"
        "      undf_w0 = chi_proxy(1)%vspace%get_undf()\n"
        "      !\n"
        "      ! Look-up quadrature variables\n"
        "      !\n"
        "      qr_proxy = qr%get_quadrature_proxy()\n"
        "      np_xy_qr = qr_proxy%np_xy\n"
        "      np_z_qr = qr_proxy%np_z\n"
        "      weights_xy_qr => qr_proxy%weights_xy\n"
        "      weights_z_qr => qr_proxy%weights_z\n"
        "      !\n"
        "      ! Allocate basis/diff-basis arrays\n"
        "      !\n"
        "      diff_dim_w0 = chi_proxy(1)%vspace%get_dim_space_diff()\n"
        "      dim_w3 = mapping_proxy%fs_to%get_dim_space()\n"
        "      diff_dim_w2 = mapping_proxy%fs_from%get_dim_space_diff()\n"
        "      ALLOCATE (diff_basis_w0_qr(diff_dim_w0, ndf_w0, np_xy_qr, "
        "np_z_qr))\n"
        "      ALLOCATE (basis_w3_qr(dim_w3, ndf_w3, np_xy_qr, np_z_qr))\n"
        "      ALLOCATE (diff_basis_w2_qr(diff_dim_w2, ndf_w2, np_xy_qr, "
        "np_z_qr))\n"
        "      !\n"
        "      ! Compute basis/diff-basis arrays\n"
        "      !\n"
        "      CALL qr%compute_function(DIFF_BASIS, chi_proxy(1)%vspace, "
        "diff_dim_w0, ndf_w0, diff_basis_w0_qr)\n"
        "      CALL qr%compute_function(BASIS, mapping_proxy%fs_to, "
        "dim_w3, ndf_w3, basis_w3_qr)\n"
        "      CALL qr%compute_function(DIFF_BASIS, mapping_proxy%fs_from, "
        "diff_dim_w2, ndf_w2, diff_basis_w2_qr)\n"
        "      !\n"
        "      ! Call kernels and communication routines\n"
        "      !\n"
        "      IF (chi_proxy(1)%is_dirty(depth=1)) THEN\n"
        "        CALL chi_proxy(1)%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (chi_proxy(2)%is_dirty(depth=1)) THEN\n"
        "        CALL chi_proxy(2)%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (chi_proxy(3)%is_dirty(depth=1)) THEN\n"
        "        CALL chi_proxy(3)%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        orientation_w2 => mapping_proxy%fs_from%get_cell_orientation("
        "cell)\n"
        "        !\n"
        "        CALL assemble_weak_derivative_w3_w2_kernel_code(cell, "
        "nlayers, mapping_proxy%ncell_3d, mapping_proxy%local_stencil, "
        "chi_proxy(1)%data, chi_proxy(2)%data, chi_proxy(3)%data, ndf_w3, "
        "basis_w3_qr, ndf_w2, diff_basis_w2_qr, orientation_w2, "
        "ndf_w0, undf_w0, map_w0(:,cell), diff_basis_w0_qr, "
        "np_xy_qr, np_z_qr, weights_xy_qr, weights_z_qr)\n"
        "      END DO \n"
        "      !\n"
        "      ! Deallocate basis arrays\n"
        "      !\n"
        "      DEALLOCATE (basis_w3_qr, diff_basis_w0_qr, diff_basis_w2_qr)\n"
        "      !\n"
        "    END SUBROUTINE invoke_0_assemble_weak_derivative_w3_w2_kernel_"
        "type")
    assert output in generated_code


def test_operator_nofield(tmpdir, f90, f90flags):
    ''' tests that an operator with no field on the same space is
    implemented correctly in the PSy layer '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.1_operator_nofield.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    gen_code_str = str(psy.gen)
    print(gen_code_str)

    if TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag to py.test)
        assert code_compiles(TEST_API, psy, tmpdir, f90, f90flags)

    assert gen_code_str.find("SUBROUTINE invoke_0_testkern_operator_"
                             "nofield_type(mm_w2, chi, qr)") != -1
    assert gen_code_str.find("TYPE(operator_type), intent(inout) :: "
                             "mm_w2") != -1
    assert gen_code_str.find("TYPE(operator_proxy_type) mm_w2_proxy") != -1
    assert gen_code_str.find("mm_w2_proxy = mm_w2%get_proxy()") != -1
    assert gen_code_str.find("undf_w2") == -1
    assert gen_code_str.find("map_w2") == -1
    assert ("CALL testkern_operator_nofield_code(cell, nlayers, "
            "mm_w2_proxy%ncell_3d, mm_w2_proxy%local_stencil, "
            "chi_proxy(1)%data, chi_proxy(2)%data, chi_proxy(3)%data, "
            "ndf_w2, basis_w2_qr, ndf_w0, undf_w0, "
            "map_w0(:,cell), diff_basis_w0_qr, np_xy_qr, np_z_qr, "
            "weights_xy_qr, weights_z_qr)" in gen_code_str)


def test_operator_nofield_different_space(
        tmpdir, f90, f90flags):
    ''' tests that an operator with no field on different spaces is
    implemented correctly in the PSy layer '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.5_operator_no_field_different_"
                                        "space.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    gen = str(psy.gen)
    print(gen)

    if TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag to py.test)
        assert code_compiles(TEST_API, psy, tmpdir, f90, f90flags)

    assert "mesh => my_mapping%get_mesh()" in gen
    assert "nlayers = my_mapping_proxy%fs_from%get_nlayers()" in gen
    assert "ndf_w3 = my_mapping_proxy%fs_from%get_ndf()" in gen
    assert "ndf_w2 = my_mapping_proxy%fs_to%get_ndf()" in gen
    # We compute operators redundantly (out to the L1 halo)
    assert "DO cell=1,mesh%get_last_halo_cell(1)" in gen
    assert ("(cell, nlayers, my_mapping_proxy%ncell_3d, my_mapping_proxy%"
            "local_stencil, ndf_w2, ndf_w3)" in gen)


def test_operator_nofield_scalar():
    ''' tests that an operator with no field and a
    scalar argument is implemented correctly in the PSy layer '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.6_operator_no_field_scalar.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    gen = str(psy.gen)
    print(gen)
    assert "mesh => my_mapping%get_mesh()" in gen
    assert "nlayers = my_mapping_proxy%fs_from%get_nlayers()" in gen
    assert "ndf_w2 = my_mapping_proxy%fs_from%get_ndf()" in gen
    assert "DO cell=1,mesh%get_last_halo_cell(1)" in gen
    assert ("(cell, nlayers, my_mapping_proxy%ncell_3d, my_mapping_proxy%"
            "local_stencil, b, ndf_w2, basis_w2_qr, np_xy_qr, np_z_qr, "
            "weights_xy_qr, weights_z_qr)" in gen)


def test_operator_nofield_scalar_deref(
        tmpdir, f90, f90flags):
    ''' Tests that an operator with no field and a
    scalar argument is implemented correctly in the PSy layer when both
    are obtained by dereferencing derived type objects '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "10.6.1_operator_no_field_scalar_deref.f90"),
        api=TEST_API)
    for dist_mem in [True, False]:
        psy = PSyFactory(TEST_API,
                         distributed_memory=dist_mem).create(invoke_info)
        gen = str(psy.gen)
        print(gen)

        if TEST_COMPILE:
            assert code_compiles(TEST_API, psy, tmpdir, f90, f90flags)

        if dist_mem:
            assert "mesh => opbox_my_mapping%get_mesh()" in gen
        assert "nlayers = opbox_my_mapping_proxy%fs_from%get_nlayers()" in gen
        assert "ndf_w2 = opbox_my_mapping_proxy%fs_from%get_ndf()" in gen
        assert ("qr_get_instance%compute_function(BASIS, "
                "opbox_my_mapping_proxy%fs_from, "
                "dim_w2, ndf_w2, basis_w2_qr_get_instance)" in gen)
        if dist_mem:
            assert "DO cell=1,mesh%get_last_halo_cell(1)" in gen
        else:
            assert (
                "DO cell=1,opbox_my_mapping_proxy%fs_from%get_ncell()" in gen)
        assert (
            "(cell, nlayers, opbox_my_mapping_proxy%ncell_3d, "
            "opbox_my_mapping_proxy%local_stencil, box_b, ndf_w2, "
            "basis_w2_qr_get_instance, np_xy_qr_get_instance, "
            "np_z_qr_get_instance, weights_xy_qr_get_instance,"
            " weights_z_qr_get_instance)" in gen)


def test_operator_orientation(tmpdir, f90, f90flags):
    ''' tests that an operator requiring orientation information is
    implemented correctly in the PSy layer '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.2_operator_orient.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    gen_str = str(psy.gen)
    print(gen_str)

    if TEST_COMPILE:
        assert code_compiles(TEST_API, psy, tmpdir, f90, f90flags)

    assert gen_str.find("SUBROUTINE invoke_0_testkern_operator"
                        "_orient_type(mm_w1, chi, qr)") != -1
    assert gen_str.find("TYPE(operator_type), intent(inout) ::"
                        " mm_w1") != -1
    assert gen_str.find("TYPE(operator_proxy_type) mm_w1_"
                        "proxy") != -1
    assert gen_str.find("mm_w1_proxy = mm_w1%get_proxy()") != -1
    assert gen_str.find(
        "orientation_w1 => mm_w1_proxy%fs_from%get_cell_orientation"
        "(cell)") != -1
    assert ("CALL testkern_operator_orient_code(cell, nlayers, "
            "mm_w1_proxy%ncell_3d, mm_w1_proxy%local_stencil, "
            "chi_proxy(1)%data, chi_proxy(2)%data, chi_proxy(3)%data, ndf_w1, "
            "basis_w1_qr, orientation_w1, ndf_w0, undf_w0, map_w0(:,cell), "
            "diff_basis_w0_qr, np_xy_qr, np_z_qr, weights_xy_qr, "
            "weights_z_qr)" in gen_str)


def test_op_orient_different_space(
        tmpdir, f90, f90flags):
    '''tests that an operator on different spaces requiring orientation
    information is implemented correctly in the PSy layer. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.4_operator_orient_different_"
                                        "space.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    gen_str = str(psy.gen)
    print(gen_str)

    if TEST_COMPILE:
        assert code_compiles(TEST_API, psy, tmpdir, f90, f90flags)

    assert (
        "INTEGER, pointer :: orientation_w1(:) => null(), orientation_w2(:)"
        " => null()" in gen_str)
    assert "ndf_w2 = my_mapping_proxy%fs_from%get_ndf()" in gen_str
    assert "ndf_w1 = my_mapping_proxy%fs_to%get_ndf()" in gen_str
    assert "dim_w1 = my_mapping_proxy%fs_to%get_dim_space()" in gen_str
    assert ("CALL qr%compute_function(BASIS, my_mapping_proxy%fs_to, "
            "dim_w1, ndf_w1, basis_w1_qr)" in gen_str)
    assert (
        "orientation_w2 => my_mapping_proxy%fs_from%get_cell_orientation("
        "cell)" in gen_str)
    assert (
        "orientation_w1 => my_mapping_proxy%fs_to%get_cell_orientation(cell)"
        in gen_str)
    assert ("(cell, nlayers, my_mapping_proxy%ncell_3d, "
            "my_mapping_proxy%local_stencil, chi_proxy(1)%data, "
            "chi_proxy(2)%data, chi_proxy(3)%data, ndf_w1, basis_w1_qr, "
            "orientation_w1, ndf_w2, orientation_w2, ndf_w0, undf_w0, "
            "map_w0(:,cell), diff_basis_w0_qr, np_xy_qr, np_z_qr, "
            "weights_xy_qr, weights_z_qr)" in gen_str)


def test_operator_deref(tmpdir, f90, f90flags):
    ''' Tests that we generate correct names for an operator in the PSy
    layer when obtained by de-referencing a derived type in the Algorithm
    layer '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "10.8_operator_deref.f90"),
                           api=TEST_API)
    for dist_mem in [True, False]:
        psy = PSyFactory(TEST_API,
                         distributed_memory=dist_mem).create(invoke_info)
        generated_code = str(psy.gen)
        print(generated_code)
        if TEST_COMPILE:
            assert code_compiles(TEST_API, psy, tmpdir, f90, f90flags)

        assert generated_code.find("SUBROUTINE invoke_0_testkern_operator"
                                   "_type(mm_w0_op, chi, a, qr)") != -1
        assert generated_code.find("TYPE(operator_type), intent(inout) ::"
                                   " mm_w0_op") != -1
        assert generated_code.find("TYPE(operator_proxy_type) mm_w0_op_"
                                   "proxy") != -1
        assert (
            generated_code.find("mm_w0_op_proxy = mm_w0_op%get_proxy()") != -1)
        assert generated_code.find(
            "CALL testkern_operator_code(cell, nlayers, "
            "mm_w0_op_proxy%ncell_3d, mm_w0_op_proxy%local_stencil, "
            "chi_proxy(1)%data, chi_proxy(2)%data, chi_proxy(3)%data, a, "
            "ndf_w0, undf_w0, map_w0(:,cell), basis_w0_qr, "
            "diff_basis_w0_qr, np_xy_qr, np_z_qr, weights_xy_qr, "
            "weights_z_qr)") != -1


def test_operator_no_dofmap_lookup():
    ''' Check that we use a field rather than an operator to look-up
    a dofmap, even when the operator precedes the field in the argument
    list. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.9_operator_first.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    gen_code = str(psy.gen)
    print(gen_code)
    # Check that we use the field and not the operator to look-up the dofmap
    assert "theta_proxy%vspace%get_whole_dofmap()" in gen_code
    assert gen_code.count("get_whole_dofmap") == 1


def test_operator_read_level1_halo():
    ''' Check that we raise an error if a kernel attempts to read from an
    operator beyond the level-1 halo '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.7_operator_read.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[0]
    # Modify the loop bound so that we attempt to read from the L2 halo
    # (of the operator)
    loop.set_upper_bound("cell_halo", index=2)
    # Attempt to generate the code
    with pytest.raises(GenerationError) as excinfo:
        _ = psy.gen
    assert ("Kernel 'testkern_operator_code' reads from an operator and "
            "therefore cannot be used for cells beyond the level 1 halo. "
            "However the containing loop goes out to level 2" in str(excinfo))


def test_operator_bc_kernel(tmpdir, f90, f90flags):
    ''' Tests that a kernel with a particular name is recognised as a
    kernel that applies boundary conditions to operators and that
    appropriate code is added to support this. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "12.4_enforce_op_bc_kernel.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)
    print(generated_code)
    output1 = "INTEGER, pointer :: boundary_dofs(:,:) => null()"
    assert output1 in generated_code
    output2 = "boundary_dofs => op_a_proxy%fs_to%get_boundary_dofs()"
    assert output2 in generated_code
    output3 = (
        "CALL enforce_operator_bc_code(cell, nlayers, op_a_proxy%ncell_3d, "
        "op_a_proxy%local_stencil, ndf_any_space_1_op_a, "
        "ndf_any_space_2_op_a, boundary_dofs)")
    assert output3 in generated_code

    if TEST_COMPILE:
        # If compilation testing has been enabled
        # (--compile --f90="<compiler_name>" flags to py.test)
        assert code_compiles(TEST_API, psy, tmpdir, f90, f90flags)


def test_operator_bc_kernel_fld_err(monkeypatch):
    ''' Test that we reject the recognised operator boundary conditions
    kernel if its argument is not an operator '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "12.4_enforce_op_bc_kernel.f90"),
                           api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API,
                         distributed_memory=dist_mem).create(invoke_info)
        schedule = psy.invokes.invoke_list[0].schedule
        loop = schedule.children[0]
        call = loop.children[0]
        arg = call.arguments.args[0]
        # Monkeypatch the argument object so that it thinks it is a
        # field rather than an operator
        monkeypatch.setattr(arg, "_type", value="gh_field")
        with pytest.raises(GenerationError) as excinfo:
            _ = psy.gen
        assert ("Expected a LMA operator from which to look-up boundary dofs "
                "but kernel enforce_operator_bc_code has argument gh_field") \
            in str(excinfo)


def test_operator_bc_kernel_multi_args_err():
    ''' Test that we reject the recognised operator boundary conditions
    kernel if it has more than one argument '''
    import copy
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "12.4_enforce_op_bc_kernel.f90"),
                           api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API,
                         distributed_memory=dist_mem).create(invoke_info)
        schedule = psy.invokes.invoke_list[0].schedule
        loop = schedule.children[0]
        call = loop.children[0]
        arg = call.arguments.args[0]
        # Make the list of arguments invalid by duplicating (a copy of)
        # this argument. We take a copy because otherwise, when we change
        # the type of arg 1 below, we change it for both.
        call.arguments.args.append(copy.copy(arg))
        with pytest.raises(GenerationError) as excinfo:
            _ = psy.gen
        assert ("Kernel enforce_operator_bc_code has 2 arguments when it "
                "should only have 1 (an LMA operator)") in str(excinfo)
        # And again but make the second argument a field this time
        call.arguments.args[1]._type = "gh_field"
        with pytest.raises(GenerationError) as excinfo:
            _ = psy.gen
        assert ("Kernel enforce_operator_bc_code has 2 arguments when it "
                "should only have 1 (an LMA operator)") in str(excinfo)


def test_operator_bc_kernel_wrong_access_err():
    ''' Test that we reject the recognised operator boundary conditions
    kernel if its operator argument has the wrong access type '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "12.4_enforce_op_bc_kernel.f90"),
                           api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API,
                         distributed_memory=dist_mem).create(invoke_info)
        schedule = psy.invokes.invoke_list[0].schedule
        loop = schedule.children[0]
        call = loop.children[0]
        arg = call.arguments.args[0]
        arg._access = "gh_read"
        with pytest.raises(GenerationError) as excinfo:
            _ = psy.gen
        assert ("applies boundary conditions to an operator. However its "
                "operator argument has access gh_read rather than "
                "gh_readwrite") in str(excinfo)


# operators : spaces and intent
OPERATORS = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(5) =                                        &
          (/ arg_type(gh_operator, gh_write,     w0, w0),                  &
             arg_type(gh_operator, gh_readwrite, w1, w1),                  &
             arg_type(gh_operator, gh_read,      w2, w2),                  &
             arg_type(gh_operator, gh_write,     w3, w3),                  &
             arg_type(gh_operator, gh_read,      any_space_1, any_space_1) &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => dummy_code
  end type dummy_type
contains
  subroutine dummy_code()
  end subroutine dummy_code
end module dummy_mod
'''


def test_operators():
    ''' test that operators are handled correctly for kernel stubs '''
    ast = fpapi.parse(OPERATORS, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    generated_code = str(kernel.gen_stub)
    output = (
        "  MODULE dummy_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE dummy_code(cell, nlayers, op_1_ncell_3d, op_1, "
        "op_2_ncell_3d, op_2, op_3_ncell_3d, op_3, op_4_ncell_3d, op_4, "
        "op_5_ncell_3d, op_5, ndf_w0, ndf_w1, ndf_w2, ndf_w3, "
        "ndf_any_space_1_op_5)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: nlayers\n"
        "      INTEGER, intent(in) :: ndf_w0, ndf_w1, ndf_w2, ndf_w3, "
        "ndf_any_space_1_op_5\n"
        "      INTEGER, intent(in) :: cell\n"
        "      INTEGER, intent(in) :: op_1_ncell_3d\n"
        "      REAL(KIND=r_def), intent(out), dimension(ndf_w0,ndf_w0,"
        "op_1_ncell_3d) :: op_1\n"
        "      INTEGER, intent(in) :: op_2_ncell_3d\n"
        "      REAL(KIND=r_def), intent(inout), dimension(ndf_w1,ndf_w1,"
        "op_2_ncell_3d) :: op_2\n"
        "      INTEGER, intent(in) :: op_3_ncell_3d\n"
        "      REAL(KIND=r_def), intent(in), dimension(ndf_w2,ndf_w2,"
        "op_3_ncell_3d) :: op_3\n"
        "      INTEGER, intent(in) :: op_4_ncell_3d\n"
        "      REAL(KIND=r_def), intent(out), dimension(ndf_w3,ndf_w3,"
        "op_4_ncell_3d) :: op_4\n"
        "      INTEGER, intent(in) :: op_5_ncell_3d\n"
        "      REAL(KIND=r_def), intent(in), dimension(ndf_any_space_1_op_5,"
        "ndf_any_space_1_op_5,op_5_ncell_3d) :: op_5\n"
        "    END SUBROUTINE dummy_code\n"
        "  END MODULE dummy_mod")
    print(output)
    print(generated_code)
    assert output in generated_code


def test_arg_descriptor_op_str():
    ''' Tests that the string method for DynArgDescriptor03 works as
    expected when we have an operator '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(OPERATORS, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    field_descriptor = metadata.arg_descriptors[0]
    result = str(field_descriptor)
    expected_output = (
        "DynArgDescriptor03 object\n"
        "  argument_type[0]='gh_operator'\n"
        "  access_descriptor[1]='gh_write'\n"
        "  function_space_to[2]='w0'\n"
        "  function_space_from[3]='w0'\n")
    print(result)
    assert expected_output in result


OPERATOR_DIFFERENT_SPACES = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(1) =    &
          (/ arg_type(gh_operator,gh_write, w0, w1) &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => dummy_code
  end type dummy_type
contains
  subroutine dummy_code()
  end subroutine dummy_code
end module dummy_mod
'''


def test_stub_operator_different_spaces():
    ''' test that the correct function spaces are provided in the
    correct order when generating a kernel stub with an operator on
    different spaces '''
    ast = fpapi.parse(OPERATOR_DIFFERENT_SPACES, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    result = str(kernel.gen_stub)
    assert "(cell, nlayers, op_1_ncell_3d, op_1, ndf_w0, ndf_w1)" in result
    assert "dimension(ndf_w0,ndf_w1,op_1_ncell_3d)" in result
