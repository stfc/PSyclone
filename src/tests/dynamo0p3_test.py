# ------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# ------------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

''' This module tests the Dynamo 0.3 API using pytest. '''

# imports
import pytest
from parse import parse, ParseError
from psyGen import PSyFactory, GenerationError
import os
import fparser
from fparser import api as fpapi
from dynamo0p3 import DynKernMetadata, DynKern
from transformations import LoopFuseTrans
from genkernelstub import generate

# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")
CODE = '''
module testkern_qr
  type, extends(kernel_type) :: testkern_qr_type
     type(arg_type), meta_args(4) =    &
          (/ arg_type(gh_field,gh_write,w1), &
             arg_type(gh_field,gh_read, w2), &
             arg_type(gh_operator,gh_read, w2, w2), &
             arg_type(gh_field,gh_read, w3)  &
           /)
     type(func_type), dimension(3) :: meta_funcs =    &
          (/ func_type(w1, gh_basis), &
             func_type(w2, gh_diff_basis), &
             func_type(w3, gh_basis, gh_diff_basis)  &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => testkern_qr_code
  end type testkern_qr_type
contains
  subroutine testkern_qr_code(a,b,c,d)
  end subroutine testkern_qr_code
end module testkern_qr
'''

# functions


def test_arg_descriptor_wrong_type():
    ''' Tests that an error is raised when the argument descriptor
    metadata is not of type arg_type. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_field,gh_read, w2)",
                        "arg_typ(gh_field,gh_read, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert "each meta_arg entry must be of type 'arg_type'" \
        in str(excinfo.value)


def test_ad_field_type_too_few_args():
    ''' Tests that an error is raised when the argument descriptor
    metadata has fewer than 3 args. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_field,gh_write,w1)",
                        "arg_type(gh_field,gh_write)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'each meta_arg entry must have at least 3 args' \
        in str(excinfo.value)


def test_ad_fld_type_too_many_args():
    ''' Tests that an error is raised when the argument descriptor
    metadata has more than 3 args. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_field,gh_write,w1)",
                        "arg_type(gh_field,gh_write,w1,w1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'each meta_arg entry must have 3 arguments' \
        in str(excinfo.value)


def test_ad_fld_type_1st_arg():
    ''' Tests that an error is raised when the 1st argument is
    invalid'''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_field,gh_write,w1)",
                        "arg_type(gh_hedge,gh_write,w1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'the 1st argument of a meta_arg entry should be a valid ' \
        'argument type' in str(excinfo.value)


def test_ad_op_type_too_few_args():
    ''' Tests that an error is raised when the operator descriptor
    metadata has fewer than 4 args. '''
    fparser.logging.disable('CRITICAL')
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
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_operator,gh_read, w2, w2)",
                        "arg_type(gh_operator,gh_read, w2, w2, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'meta_arg entry must have 4 arguments' in str(excinfo.value)


def test_ad_invalid_type():
    ''' Tests that an error is raised when an invalid descriptor type
    name is provided as the first argument. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("gh_operator", "gh_operato", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert '1st argument of a meta_arg entry should be a valid argument type' \
        in str(excinfo.value)


def test_ad_invalid_access_type():
    ''' Tests that an error is raised when an invalid access
    name is provided as the second argument. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("gh_read", "gh_ead", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert '2nd argument of a meta_arg entry' in str(excinfo.value)


def test_arg_descriptor_invalid_fs1():
    ''' Tests that an error is raised when an invalid function space
    name is provided as the third argument. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("gh_field,gh_read, w3", "gh_field,gh_read, w4", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert '3rd argument of a meta_arg entry' in str(excinfo.value)


def test_arg_descriptor_invalid_fs2():
    ''' Tests that an error is raised when an invalid function space
    name is provided as the third argument. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("w2, w2", "w2, w4", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert '4th argument of a meta_arg entry' in str(excinfo.value)


def test_invalid_vector_operator():
    ''' Tests that an error is raised when a vector does not use "*"
    as it's operator. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("gh_field,gh_write,w1", "gh_field+3,gh_write,w1", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert "must use '*' as the separator" in str(excinfo.value)


def test_invalid_vector_value_type():
    ''' Tests that an error is raised when a vector value is not a valid
    integer '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("gh_field,gh_write,w1", "gh_field*n,gh_write,w1", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'vector notation expects the format (field*n)' in str(excinfo.value)


def test_invalid_vector_value_range():
    ''' Tests that an error is raised when a vector value is not a valid
    value (<2) '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("gh_field,gh_write,w1", "gh_field*1,gh_write,w1", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'must contain a valid integer vector size' in str(excinfo.value)

# Testing that an error is raised when a vector value is not provided is
# not required here as it causes a parse error in the generic code.


def test_fs_descriptor_wrong_type():
    ''' Tests that an error is raised when the function space descriptor
    metadata is not of type func_type. '''
    fparser.logging.disable('CRITICAL')
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
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("w1, gh_basis", "w1", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'meta_func entry must have at least 2 args' in str(excinfo.value)


def test_fs_desc_invalid_fs_type():
    ''' Tests that an error is raised when an invalid function space name
    is provided as the first argument. '''
    fparser.logging.disable('CRITICAL')
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
    fparser.logging.disable('CRITICAL')
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
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("w2, gh_diff_basis", "w2, gh_dif_basis", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert '2nd argument and all subsequent arguments of a meta_func ' + \
        'entry should be a valid operator name' in str(excinfo.value)


def test_fs_desc_replicated_op_type():
    ''' Tests that an error is raised when a function space
    operator name is replicated as an argument. '''
    fparser.logging.disable('CRITICAL')
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
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("w3, gh_basis", "w0, gh_basis", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'function spaces specified in meta_funcs must exist in ' + \
        'meta_args' in str(excinfo)


def test_field():
    ''' Tests that a call with a set of fields and no basis
    functions produces correct code. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    output = (
        "  MODULE psy_single_invoke\n"
        "    USE constants_mod, ONLY: r_def\n"
        "    USE quadrature_mod, ONLY: quadrature_type\n"
        "    USE operator_mod, ONLY: operator_type, operator_proxy_type\n"
        "    USE field_mod, ONLY: field_type, field_proxy_type\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_testkern_type(f1, f2, m1, m2)\n"
        "      USE testkern, ONLY: testkern_code\n"
        "      TYPE(field_type), intent(inout) :: f1, f2, m1, m2\n"
        "      INTEGER, pointer :: map_w1(:) => null(), map_w2(:) => null(), "
        "map_w3(:) => null()\n"
        "      INTEGER cell\n"
        "      INTEGER ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, undf_w3\n"
        "      INTEGER nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, m2_proxy\n"
        "      !\n"
        "      ! Initialise field proxies\n"
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
        "      ! Initialise sizes and allocate any basis arrays for w1\n"
        "      !\n"
        "      ndf_w1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_w1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for w2\n"
        "      !\n"
        "      ndf_w2 = f2_proxy%vspace%get_ndf()\n"
        "      undf_w2 = f2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for w3\n"
        "      !\n"
        "      ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "      undf_w3 = m2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO cell=1,f1_proxy%vspace%get_ncell()\n"
        "        !\n"
        "        map_w1 => f1_proxy%vspace%get_cell_dofmap(cell)\n"
        "        map_w2 => f2_proxy%vspace%get_cell_dofmap(cell)\n"
        "        map_w3 => m2_proxy%vspace%get_cell_dofmap(cell)\n"
        "        !\n"
        "        CALL testkern_code(nlayers, f1_proxy%data, f2_proxy%data, "
        "m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, map_w1, ndf_w2, "
        "undf_w2, map_w2, ndf_w3, undf_w3, map_w3)\n"
        "      END DO \n"
        "      !\n"
        "    END SUBROUTINE invoke_0_testkern_type\n"
        "  END MODULE psy_single_invoke")
    assert str(generated_code).find(output) != -1


def test_field_fs():
    ''' Tests that a call with a set of fields making use of all
    function spaces and no basis functions produces correct code.'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1.5_single_invoke_fs.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    output = (
        "  MODULE psy_single_invoke_fs\n"
        "    USE constants_mod, ONLY: r_def\n"
        "    USE quadrature_mod, ONLY: quadrature_type\n"
        "    USE operator_mod, ONLY: operator_type, operator_proxy_type\n"
        "    USE field_mod, ONLY: field_type, field_proxy_type\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_testkern_fs_type(f1, f2, m1, m2, f3, f4, "
        "m3)\n"
        "      USE testkern_fs, ONLY: testkern_code\n"
        "      TYPE(field_type), intent(inout) :: f1, f2, m1, m2, f3, f4, m3\n"
        "      INTEGER, pointer :: map_w1(:) => null(), map_w2(:) => null(), "
        "map_w3(:) => null(), map_wtheta(:) => null(), map_w2h(:) => null(), "
        "map_w2v(:) => null()\n"
        "      INTEGER cell\n"
        "      INTEGER ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, undf_w3, "
        "ndf_wtheta, undf_wtheta, ndf_w2h, undf_w2h, ndf_w2v, undf_w2v\n"
        "      INTEGER nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, m2_proxy, "
        "f3_proxy, f4_proxy, m3_proxy\n"
        "      !\n"
        "      ! Initialise field proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      m1_proxy = m1%get_proxy()\n"
        "      m2_proxy = m2%get_proxy()\n"
        "      f3_proxy = f3%get_proxy()\n"
        "      f4_proxy = f4%get_proxy()\n"
        "      m3_proxy = m3%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for w1\n"
        "      !\n"
        "      ndf_w1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_w1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for w2\n"
        "      !\n"
        "      ndf_w2 = f2_proxy%vspace%get_ndf()\n"
        "      undf_w2 = f2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for w3\n"
        "      !\n"
        "      ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "      undf_w3 = m2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for wtheta\n"
        "      !\n"
        "      ndf_wtheta = f3_proxy%vspace%get_ndf()\n"
        "      undf_wtheta = f3_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for w2h\n"
        "      !\n"
        "      ndf_w2h = f4_proxy%vspace%get_ndf()\n"
        "      undf_w2h = f4_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for w2v\n"
        "      !\n"
        "      ndf_w2v = m3_proxy%vspace%get_ndf()\n"
        "      undf_w2v = m3_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO cell=1,f1_proxy%vspace%get_ncell()\n"
        "        !\n"
        "        map_w1 => f1_proxy%vspace%get_cell_dofmap(cell)\n"
        "        map_w2 => f2_proxy%vspace%get_cell_dofmap(cell)\n"
        "        map_w3 => m2_proxy%vspace%get_cell_dofmap(cell)\n"
        "        map_wtheta => f3_proxy%vspace%get_cell_dofmap(cell)\n"
        "        map_w2h => f4_proxy%vspace%get_cell_dofmap(cell)\n"
        "        map_w2v => m3_proxy%vspace%get_cell_dofmap(cell)\n"
        "        !\n"
        "        CALL testkern_code(nlayers, f1_proxy%data, f2_proxy%data, "
        "m1_proxy%data, m2_proxy%data, f3_proxy%data, f4_proxy%data, "
        "m3_proxy%data, ndf_w1, undf_w1, map_w1, ndf_w2, undf_w2, map_w2, "
        "ndf_w3, undf_w3, map_w3, ndf_wtheta, undf_wtheta, map_wtheta, "
        "ndf_w2h, undf_w2h, map_w2h, ndf_w2v, undf_w2v, map_w2v)\n"
        "      END DO \n"
        "      !\n"
        "    END SUBROUTINE invoke_0_testkern_fs_type\n"
        "  END MODULE psy_single_invoke_fs")
    print str(generated_code)
    print output
    assert str(generated_code).find(output) != -1


def test_field_qr():
    ''' Tests that a call, with a set of fields requiring
    quadrature, produces correct code. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.1_single_invoke_qr.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    output = (
        "    SUBROUTINE invoke_0_testkern_qr_type(f1, f2, m1, m2, qr)\n"
        "      USE testkern_qr, ONLY: testkern_qr_code\n"
        "      TYPE(field_type), intent(inout) :: f1, f2, m1, m2\n"
        "      TYPE(quadrature_type), intent(in) :: qr\n"
        "      INTEGER, pointer :: map_w1(:) => null(), map_w2(:) => null(), "
        "map_w3(:) => null()\n"
        "      INTEGER cell\n"
        "      REAL(KIND=r_def), allocatable :: basis_w1(:,:,:,:), "
        "diff_basis_w2(:,:,:,:), basis_w3(:,:,:,:), diff_basis_w3(:,:,:,:)\n"
        "      INTEGER dim_w1, diff_dim_w2, dim_w3, diff_dim_w3\n"
        "      INTEGER ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, undf_w3\n"
        "      REAL(KIND=r_def), pointer :: zp(:) => null(), wh(:) => null(), "
        "wv(:) => null()\n"
        "      REAL(KIND=r_def), pointer :: xp(:,:) => null()\n"
        "      INTEGER nqp_h, nqp_v\n"
        "      INTEGER nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, m2_proxy\n"
        "      !\n"
        "      ! Initialise field proxies\n"
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
        "      ! Initialise qr values\n"
        "      !\n"
        "      wv => qr%get_wqp_v()\n"
        "      xp => qr%get_xqp_h()\n"
        "      zp => qr%get_xqp_v()\n"
        "      wh => qr%get_wqp_h()\n"
        "      nqp_h = qr%get_nqp_h()\n"
        "      nqp_v = qr%get_nqp_v()\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for w1\n"
        "      !\n"
        "      ndf_w1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_w1 = f1_proxy%vspace%get_undf()\n"
        "      dim_w1 = f1_proxy%vspace%get_dim_space()\n"
        "      ALLOCATE (basis_w1(dim_w1, ndf_w1, nqp_h, nqp_v))\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for w2\n"
        "      !\n"
        "      ndf_w2 = f2_proxy%vspace%get_ndf()\n"
        "      undf_w2 = f2_proxy%vspace%get_undf()\n"
        "      diff_dim_w2 = f2_proxy%vspace%get_dim_space_diff()\n"
        "      ALLOCATE (diff_basis_w2(diff_dim_w2, ndf_w2, nqp_h, nqp_v))\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for w3\n"
        "      !\n"
        "      ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "      undf_w3 = m2_proxy%vspace%get_undf()\n"
        "      dim_w3 = m2_proxy%vspace%get_dim_space()\n"
        "      ALLOCATE (basis_w3(dim_w3, ndf_w3, nqp_h, nqp_v))\n"
        "      diff_dim_w3 = m2_proxy%vspace%get_dim_space_diff()\n"
        "      ALLOCATE (diff_basis_w3(diff_dim_w3, ndf_w3, nqp_h, nqp_v))\n"
        "      !\n"
        "      ! Compute basis arrays\n"
        "      !\n"
        "      CALL f1_proxy%vspace%compute_basis_function(basis_w1, ndf_w1, "
        "nqp_h, nqp_v, xp, zp)\n"
        "      CALL f2_proxy%vspace%compute_diff_basis_function("
        "diff_basis_w2, ndf_w2, nqp_h, nqp_v, xp, zp)\n"
        "      CALL m2_proxy%vspace%compute_basis_function(basis_w3, ndf_w3, "
        "nqp_h, nqp_v, xp, zp)\n"
        "      CALL m2_proxy%vspace%compute_diff_basis_function("
        "diff_basis_w3, ndf_w3, nqp_h, nqp_v, xp, zp)\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO cell=1,f1_proxy%vspace%get_ncell()\n"
        "        !\n"
        "        map_w1 => f1_proxy%vspace%get_cell_dofmap(cell)\n"
        "        map_w2 => f2_proxy%vspace%get_cell_dofmap(cell)\n"
        "        map_w3 => m2_proxy%vspace%get_cell_dofmap(cell)\n"
        "        !\n"
        "        CALL testkern_qr_code(nlayers, f1_proxy%data, f2_proxy%data, "
        "m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, map_w1, basis_w1, "
        "ndf_w2, undf_w2, map_w2, diff_basis_w2, ndf_w3, undf_w3, map_w3, "
        "basis_w3, diff_basis_w3, nqp_h, nqp_v, wh, wv)\n"
        "      END DO \n"
        "      !\n"
        "      ! Deallocate basis arrays\n"
        "      !\n"
        "      DEALLOCATE (basis_w1, diff_basis_w2, basis_w3, diff_basis_w3)\n"
        "      !\n"
        "    END SUBROUTINE invoke_0_testkern_qr_type")
    assert str(generated_code).find(output) != -1


def test_vector_field():
    ''' tests that a vector field is declared correctly in the PSy
    layer '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "8_vector_field.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    assert str(generated_code).find("SUBROUTINE invoke_0_testkern_chi_"
                                    "type(f1, chi)") != -1
    assert str(generated_code).find("TYPE(field_type), intent(inout)"
                                    " :: f1, chi(3)") != -1


def test_vector_field_2():
    ''' Tests that a vector field is indexed correctly in the PSy layer. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "8_vector_field_2.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    # all references to chi_proxy should be chi_proxy(1)
    assert str(generated_code).find("chi_proxy%") == -1
    assert str(generated_code).count("chi_proxy(1)%vspace") == 5
    # use each chi field individually in the kernel
    assert str(generated_code).find("chi_proxy(1)%data, chi_proxy(2)%data,"
                                    " chi_proxy(3)%data") != -1


def test_orientation():
    ''' tests that orientation information is created correctly in
    the PSy '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "9_orientation.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    print str(generated_code)
    assert str(generated_code).find("INTEGER, pointer :: orientation_w2(:)"
                                    " => null()") != -1
    assert str(generated_code).find("orientation_w2 => f2_proxy%vspace%"
                                    "get_cell_orientation(cell)") != -1


def test_operator():
    ''' tests that an operator is implemented correctly in the PSy
    layer '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "10_operator.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    assert str(generated_code).find("SUBROUTINE invoke_0_testkern_operator"
                                    "_type(mm_w0, chi, qr)") != -1
    assert str(generated_code).find("TYPE(operator_type), intent(inout) ::"
                                    " mm_w0") != -1
    assert str(generated_code).find("TYPE(operator_proxy_type) mm_w0_"
                                    "proxy") != -1
    assert str(generated_code).find("mm_w0_proxy = mm_w0%get_proxy()") != -1
    assert str(generated_code).find(
        "CALL testkern_operator_code(cell, nlayers, mm_w0_proxy%ncell_3d, mm_"
        "w0_proxy%local_stencil, chi_proxy(1)%data, chi_proxy(2)%data, chi_pr"
        "oxy(3)%data, ndf_w0, undf_w0, map_w0, basis_w0, diff_basis_w0, nqp_h"
        ", nqp_v, wh, wv)") != -1


def test_operator_different_spaces():
    '''tests that an operator with different to and from spaces is
    implemented correctly in the PSy layer'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.3_operator_different_spaces.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    output = (
        "    SUBROUTINE invoke_0_assemble_weak_derivative_w3_w2_kernel_type"
        "(mapping, chi, qr)\n"
        "      USE assemble_weak_derivative_w3_w2_kernel_mod, ONLY: "
        "assemble_weak_derivative_w3_w2_kernel_code\n"
        "      TYPE(field_type), intent(inout) :: chi(3)\n"
        "      TYPE(operator_type), intent(inout) :: mapping\n"
        "      TYPE(quadrature_type), intent(in) :: qr\n"
        "      INTEGER, pointer :: orientation_w2(:) => null()\n"
        "      INTEGER, pointer :: map_w0(:) => null()\n"
        "      INTEGER cell\n"
        "      REAL(KIND=r_def), allocatable :: basis_w3(:,:,:,:), "
        "diff_basis_w2(:,:,:,:), diff_basis_w0(:,:,:,:)\n"
        "      INTEGER dim_w3, diff_dim_w2, diff_dim_w0\n"
        "      INTEGER ndf_w3, ndf_w2, ndf_w0, undf_w0\n"
        "      REAL(KIND=r_def), pointer :: zp(:) => null(), wh(:) => null(), "
        "wv(:) => null()\n"
        "      REAL(KIND=r_def), pointer :: xp(:,:) => null()\n"
        "      INTEGER nqp_h, nqp_v\n"
        "      INTEGER nlayers\n"
        "      TYPE(operator_proxy_type) mapping_proxy\n"
        "      TYPE(field_proxy_type) chi_proxy(3)\n"
        "      !\n"
        "      ! Initialise field proxies\n"
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
        "      ! Initialise qr values\n"
        "      !\n"
        "      wv => qr%get_wqp_v()\n"
        "      xp => qr%get_xqp_h()\n"
        "      zp => qr%get_xqp_v()\n"
        "      wh => qr%get_wqp_h()\n"
        "      nqp_h = qr%get_nqp_h()\n"
        "      nqp_v = qr%get_nqp_v()\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for w3\n"
        "      !\n"
        "      ndf_w3 = mapping_proxy%fs_to%get_ndf()\n"
        "      dim_w3 = mapping_proxy%fs_to%get_dim_space()\n"
        "      ALLOCATE (basis_w3(dim_w3, ndf_w3, nqp_h, nqp_v))\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for w2\n"
        "      !\n"
        "      ndf_w2 = mapping_proxy%fs_from%get_ndf()\n"
        "      diff_dim_w2 = mapping_proxy%fs_from%get_dim_space_diff()\n"
        "      ALLOCATE (diff_basis_w2(diff_dim_w2, ndf_w2, nqp_h, nqp_v))\n"
        "      !\n"
        "      ! Initialise sizes and allocate any basis arrays for w0\n"
        "      !\n"
        "      ndf_w0 = chi_proxy(1)%vspace%get_ndf()\n"
        "      undf_w0 = chi_proxy(1)%vspace%get_undf()\n"
        "      diff_dim_w0 = chi_proxy(1)%vspace%get_dim_space_diff()\n"
        "      ALLOCATE (diff_basis_w0(diff_dim_w0, ndf_w0, nqp_h, nqp_v))\n"
        "      !\n"
        "      ! Compute basis arrays\n"
        "      !\n"
        "      CALL mapping_proxy%fs_to%compute_basis_function(basis_w3, "
        "ndf_w3, nqp_h, nqp_v, xp, zp)\n"
        "      CALL mapping_proxy%fs_from%compute_diff_basis_function("
        "diff_basis_w2, ndf_w2, nqp_h, nqp_v, xp, zp)\n"
        "      CALL chi_proxy(1)%vspace%compute_diff_basis_function("
        "diff_basis_w0, ndf_w0, nqp_h, nqp_v, xp, zp)\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO cell=1,mapping_proxy%fs_from%get_ncell()\n"
        "        !\n"
        "        map_w0 => chi_proxy(1)%vspace%get_cell_dofmap(cell)\n"
        "        !\n"
        "        orientation_w2 => mapping_proxy%fs_from%get_cell_orientation("
        "cell)\n"
        "        !\n"
        "        CALL assemble_weak_derivative_w3_w2_kernel_code(cell, "
        "nlayers, mapping_proxy%ncell_3d, mapping_proxy%local_stencil, "
        "chi_proxy(1)%data, chi_proxy(2)%data, chi_proxy(3)%data, ndf_w3, "
        "basis_w3, ndf_w2, diff_basis_w2, orientation_w2, ndf_w0, undf_w0, "
        "map_w0, diff_basis_w0, nqp_h, nqp_v, wh, wv)\n"
        "      END DO \n"
        "      !\n"
        "      ! Deallocate basis arrays\n"
        "      !\n"
        "      DEALLOCATE (basis_w3, diff_basis_w2, diff_basis_w0)\n"
        "      !\n"
        "    END SUBROUTINE invoke_0_assemble_weak_derivative_w3_w2_kernel_"
        "type")
    print generated_code
    print output
    assert output in generated_code


def test_operator_nofield():
    ''' tests that an operator with no field on the same space is
    implemented correctly in the PSy layer '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.1_operator_nofield.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    gen_code_str = str(psy.gen)
    assert gen_code_str.find("SUBROUTINE invoke_0_testkern_operator_"
                             "nofield_type(mm_w2, chi, qr)") != -1
    assert gen_code_str.find("TYPE(operator_type), intent(inout) :: "
                             "mm_w2") != -1
    assert gen_code_str.find("TYPE(operator_proxy_type) mm_w2_proxy") != -1
    assert gen_code_str.find("mm_w2_proxy = mm_w2%get_proxy()") != -1
    assert gen_code_str.find("undf_w2") == -1
    assert gen_code_str.find("map_w2") == -1
    assert gen_code_str.find(
        "CALL testkern_operator_code(cell, nlayers, mm_w2_proxy%ncell_3d,"
        " mm_w2_proxy%local_stencil, chi_proxy(1)%data, chi_proxy(2)%data"
        ", chi_proxy(3)%data, ndf_w2, basis_w2, ndf_w0, undf_w0, map_w0, "
        "diff_basis_w0, nqp_h, nqp_v, wh, wv)") != -1


def test_operator_nofield_different_space():
    ''' tests that an operator with no field on different spaces is
    implemented correctly in the PSy layer '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.5_operator_no_field_different_"
                                        "space.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    gen_code_str = str(psy.gen)
    print gen_code_str
    assert gen_code_str.find(
        "nlayers = my_mapping_proxy%fs_from%get_nlayers()")
    assert gen_code_str.find("ndf_w3 = my_mapping_proxy%fs_from%get_ndf()")
    assert gen_code_str.find("ndf_w2 = my_mapping_proxy%fs_to%get_ndf()")
    assert gen_code_str.find("DO cell=1,my_mapping_proxy%fs_from%get_ncell()")
    assert gen_code_str.find(
        "(cell, nlayers, my_mapping_proxy%ncell_3d, my_mapping_proxy%"
        "local_stencil, ndf_w3, ndf_w2)")


def test_operator_orientation():
    ''' tests that an operator requiring orientation information is
    implemented correctly in the PSy layer '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.2_operator_orient.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    gen_str = str(psy.gen)
    print gen_str
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
    assert gen_str.find(
        "CALL testkern_operator_orient_code(cell, nlayers, mm_w1_proxy%ncell_"
        "3d, mm_w1_proxy%local_stencil, chi_proxy(1)%data, chi_proxy(2)%data,"
        " chi_proxy(3)%data, ndf_w1, basis_w1, orientation_w1, ndf_w0, undf_w"
        "0, map_w0, diff_basis_w0, nqp_h, nqp_v, wh, wv)") != -1


def test_operator_orientation_different_space():
    '''tests that an operator on different spaces requiring orientation
    information is implemented correctly in the PSy layer. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.4_operator_orient_different_"
                                        "space.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    gen_str = str(psy.gen)
    assert gen_str.find(
        "INTEGER, pointer :: orientation_w1(:) => null(), orientation_w2(:)"
        "=> null()")
    assert gen_str.find("ndf_w2 = my_mapping_proxy%fs_from%get_ndf()")
    assert gen_str.find("ndf_w1 = my_mapping_proxy%fs_to%get_ndf()")
    assert gen_str.find("dim_w1 = my_mapping_proxy%fs_to%get_dim_space()")
    assert gen_str.find(
        "CALL my_mapping_proxy%fs_to%compute_basis_function(basis_w1, ndf_w1,"
        "nqp_h, nqp_v, xp, zp)")
    assert gen_str.find(
        "orientation_w2 => my_mapping_proxy%fs_from%get_cell_orientation("
        "cell)")
    assert gen_str.find(
        "orientation_w1 => my_mapping_proxy%fs_to%get_cell_orientation(cell)")
    assert gen_str.find(
        "(cell, nlayers, my_mapping_proxy%ncell_3d, my_mapping_proxy%local_"
        "stencil, chi_proxy(1)%data, chi_proxy(2)%data, chi_proxy(3)%data, "
        "ndf_w2, orientation_w2, ndf_w1, basis_w1, orientation_w1, ndf_w0, "
        "undf_w0, map_w0, diff_basis_w0, nqp_h, nqp_v, wh, wv)")


def test_any_space_1():
    ''' tests that any_space is implemented correctly in the PSy
    layer. Includes more than one type of any_space declaration
    and func_type basis functions on any_space. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "11_any_space.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    assert str(generated_code).find(
        "INTEGER, pointer :: map_any_space_1(:) => null(), map_any_space_2"
        "(:) => null()") != -1
    assert str(generated_code).find(
        "REAL(KIND=r_def), allocatable :: basis_any_space_1(:,:,:,:), "
        "basis_any_space_2(:,:,:,:)") != -1
    assert str(generated_code).find(
        "ALLOCATE (basis_any_space_1(dim_any_space_1, ndf_any_space_1, "
        "nqp_h, nqp_v))") != -1
    assert str(generated_code).find(
        "ALLOCATE (basis_any_space_2(dim_any_space_2, ndf_any_space_2, "
        "nqp_h, nqp_v))") != -1
    assert str(generated_code).find(
        "map_any_space_1 => a_proxy%vspace%get_cell_dofmap(cell)") != -1
    assert str(generated_code).find(
        "map_any_space_2 => b_proxy%vspace%get_cell_dofmap(cell)") != -1
    assert str(generated_code).find(
        "CALL testkern_any_space_1_code(nlayers, a_proxy%data, b_proxy%"
        "data, c_proxy(1)%data, c_proxy(2)%data, c_proxy(3)%data, ndf_a"
        "ny_space_1, undf_any_space_1, map_any_space_1, basis_any_space"
        "_1, ndf_any_space_2, undf_any_space_2, map_any_space_2, basis_"
        "any_space_2, ndf_w0, undf_w0, map_w0, diff_basis_w0, nqp_h, nq"
        "p_v, wh, wv)") != -1
    assert str(generated_code).find(
        "DEALLOCATE (basis_any_space_1, basis_any_space_2, diff_basis_w"
        "0)") != -1


def test_any_space_2():
    ''' tests that any_space is implemented correctly in the PSy
    layer. Includes multiple declarations of the same space, no
    func_type declarations and any_space used with an
    operator. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "11.1_any_space.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    assert str(generated_code).find(
        "INTEGER, pointer :: map_any_space_1(:) => null()") != -1
    assert str(generated_code).find(
        "INTEGER ndf_any_space_1, undf_any_space_1") != -1
    assert str(generated_code).find(
        "ndf_any_space_1 = a_proxy%vspace%get_ndf()") != -1
    assert str(generated_code).find(
        "undf_any_space_1 = a_proxy%vspace%get_undf()") != -1
    assert str(generated_code).find(
        "map_any_space_1 => a_proxy%vspace%get_cell_dofmap(cell)") != -1
    assert str(generated_code).find(
        "CALL testkern_any_space_2_code(cell, nlayers, a_proxy%data, b_pro"
        "xy%data, c_proxy%ncell_3d, c_proxy%local_stencil, ndf_any_space_1"
        ", undf_any_space_1, map_any_space_1)") != -1


def test_operator_any_space_different_space_1():
    ''' tests that any_space is implemented correctly in the PSy
    layer. Includes different spaces for an operator and no other
    fields.'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "11.2_any_space.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    print generated_code
    assert str(generated_code).find(
        "ndf_any_space_2 = a_proxy%fs_from%get_ndf()") != -1
    assert str(generated_code).find(
        "ndf_any_space_1 = a_proxy%fs_to%get_ndf()") != -1


def test_operator_any_space_different_space_2():
    ''' tests that any_space is implemented correctly in the PSy
    layer in a more complicated example. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "11.3_any_space.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    print generated_code
    assert str(generated_code).find(
        "ndf_any_space_1 = b_proxy%fs_to%get_ndf()") != -1
    assert str(generated_code).find(
        "dim_any_space_1 = b_proxy%fs_to%get_dim_space()") != -1
    assert str(generated_code).find(
        "ndf_any_space_2 = b_proxy%fs_from%get_ndf()") != -1
    assert str(generated_code).find(
        "ndf_any_space_3 = c_proxy%fs_to%get_ndf()") != -1
    assert str(generated_code).find(
        "ndf_any_space_4 = d_proxy%fs_from%get_ndf()") != -1
    assert str(generated_code).find(
        "undf_any_space_4 = d_proxy%fs_from%get_undf()") != -1
    assert str(generated_code).find(
        "dim_any_space_4 = d_proxy%fs_from%get_dim_space()") != -1
    assert str(generated_code).find(
        "ndf_any_space_5 = a_proxy%vspace%get_ndf()") != -1
    assert str(generated_code).find(
        "undf_any_space_5 = a_proxy%vspace%get_undf()") != -1
    assert str(generated_code).find(
        "CALL b_proxy%fs_to%compute_basis_function") != -1
    assert str(generated_code).find(
        "CALL d_proxy%fs_from%compute_basis_function") != -1
    assert str(generated_code).find(
        "CALL d_proxy%fs_from%compute_diff_basis_function") != -1
    assert str(generated_code).find(
        "map_any_space_5 => a_proxy%vspace%get_cell_dofmap(cell)") != -1
    assert str(generated_code).find(
        "map_any_space_4 => d_proxy%fs_from%get_cell_dofmap(cell)") != -1


def test_kernel_specific():
    '''tests that kernel-specific code is added to the
    matrix_vector_kernel_mm kernel. This code is required as the
    dynamo0.3 api does not know about boundary conditions but this
    kernel requires them. This "hack" is only supported to get
    PSyclone to generate correct code for the current
    implementation of dynamo. Future API's will not support any
    hacks. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "12_kernel_specific.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    output0 = "USE enforce_bc_kernel_mod, ONLY: enforce_bc_code"
    assert str(generated_code).find(output0) != -1
    output1 = "USE function_space_mod, ONLY: w2"
    assert str(generated_code).find(output1) != -1
    output2 = "INTEGER fs"
    assert str(generated_code).find(output2) != -1
    output3 = "INTEGER, pointer :: boundary_dofs_w2(:,:) => null()"
    assert str(generated_code).find(output3) != -1
    output4 = "fs = f2%which_function_space()"
    assert str(generated_code).find(output4) != -1
    output5 = '''IF (fs .eq. w2) THEN
        boundary_dofs_w2 => f2_proxy%vspace%get_boundary_dofs()
      END IF'''
    assert str(generated_code).find(output5) != -1
    output6 = (
        "IF (fs .eq. w2) THEN\n"
        "          CALL enforce_bc_code(nlayers, f1_proxy%data, "
        "ndf_any_space_1, undf_any_space_1, map_any_space_1, "
        "boundary_dofs_w2)")
    assert str(generated_code).find(output6) != -1


def test_bc_kernel():
    '''tests that a kernel with a particular name is recognised as a
    boundary condition kernel and that appopriate code is added to
    support this. This code is required as the dynamo0.3 api does not
    know about boundary conditions but this kernel requires them. This
    "hack" is only supported to get PSyclone to generate correct code
    for the current implementation of dynamo. Future API's will not
    support any hacks. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "12.2_enforce_bc_kernel.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    output1 = "INTEGER, pointer :: boundary_dofs(:,:) => null()"
    assert str(generated_code).find(output1) != -1
    output2 = "boundary_dofs => a_proxy%vspace%get_boundary_dofs()"
    assert str(generated_code).find(output2) != -1
    output3 = (
        "CALL enforce_bc_code(nlayers, a_proxy%data, ndf_any_space_1, "
        "undf_any_space_1, map_any_space_1, boundary_dofs)")
    assert str(generated_code).find(output3) != -1


def test_multikernel_invoke_1():
    ''' Test that correct code is produced when there are multiple
    kernels within an invoke. We test the parts of the code that
    are incorrect at the time of writing '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    # check that argument names are not replicated
    output1 = "SUBROUTINE invoke_0(f1, f2, m1, m2)"
    assert str(generated_code).find(output1) != -1
    # check that only one proxy initialisation is produced
    output2 = "f1_proxy = f1%get_proxy()"
    assert str(generated_code).count(output2) == 1


def test_multikernel_invoke_qr():
    ''' Test that correct code is produced when there are multiple
    kernels with (the same) QR within an invoke. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.1_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    # simple check that two kernel calls exist
    assert str(generated_code).count("CALL testkern_qr_code") == 2


def test_mkern_invoke_vec_fields():
    ''' Test that correct code is produced when there are multiple
    kernels within an invoke with vector fields '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.2_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    # 1st test for duplication of name vector-field declaration
    output1 = "TYPE(field_type), intent(inout) :: f1, chi(3), chi(3)"
    assert str(generated_code).find(output1) == -1
    # 2nd test for duplication of name vector-field declaration
    output2 = "TYPE(field_proxy_type) f1_proxy, chi_proxy(3), chi_proxy(3)"
    assert str(generated_code).find(output2) == -1


def test_multikern_invoke_orient():
    ''' Test that correct code is produced when there are multiple
    kernels within an invoke with orientation '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.3_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    # 1st test for duplication of name vector-field declaration
    output1 = "TYPE(field_type), intent(inout) :: f1, f2, f3(3), f3(3)"
    assert str(generated_code).find(output1) == -1
    # 2nd test for duplication of name vector-field declaration
    output2 = (
        "TYPE(field_proxy_type) f1_proxy, f2_proxy, f3_proxy(3), f3_proxy(3)")
    assert str(generated_code).find(output2) == -1


def test_multikern_invoke_oper():
    ''' Test that correct code is produced when there are multiple
    kernels within an invoke with operators '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.4_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    # 1st test for duplication of name vector-field declaration
    output1 = "TYPE(field_type), intent(inout) :: f1(3), f1(3)"
    assert str(generated_code).find(output1) == -1
    # 2nd test for duplication of name vector-field declaration
    output2 = "TYPE(field_proxy_type) f1_proxy(3), f1_proxy(3)"
    assert str(generated_code).find(output2) == -1


def test_multikern_invoke_any_space():
    ''' Test that an error is thrown when there are multiple
    kernels within an invoke with kernel fields declared as
    any_space. This is not yet supported as any_space with
    different kernels in an invoke must either inherit the space
    from the variable (which needs analysis) or have a unique name
    for the space used by each kernel and at the moment neither of
    these is the case.c'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.5_multikernel_invokes.f90"),
                           api="dynamo0.3")
    with pytest.raises(GenerationError) as excinfo:
        _ = PSyFactory("dynamo0.3").create(invoke_info)
    assert 'multiple kernels within this invoke with kernel arguments ' + \
        'declared as any_space' in str(excinfo.value)


@pytest.mark.xfail(reason="bug : loop fuse replicates maps in loops")
def test_loopfuse():
    ''' Tests whether loop fuse actually fuses and whether
    multiple maps are produced or not. Multiple maps are not an
    error but it would be nicer if there were only one '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    invoke = psy.invokes.get("invoke_0")
    schedule = invoke.schedule
    loop1 = schedule.children[0]
    loop2 = schedule.children[1]
    trans = LoopFuseTrans()
    schedule, _ = trans.apply(loop1, loop2)
    invoke.schedule = schedule
    generated_code = psy.gen
    # only one loop
    assert str(generated_code).count("DO cell") == 1
# only one map for each space
    assert str(generated_code).count("map_w1 =>") == 1
    assert str(generated_code).count("map_w2 =>") == 1
    assert str(generated_code).count("map_w3 =>") == 1
    # kernel call tests
    kern_idxs = []
    for idx, line in enumerate(str(generated_code).split('\n')):
        if line.find("DO cell") != -1:
            do_idx = idx
        if line.find("CALL testkern_code(") != -1:
            kern_idxs.append(idx)
        if line.find("END DO") != -1:
            enddo_idx = idx
    # two kernel calls
    assert len(kern_idxs) == 2
    # both kernel calls are within the loop
    for kern_id in kern_idxs:
        assert kern_id > do_idx and kern_id < enddo_idx

# tests for dynamo0.3 stub generator


def test_stub_non_existant_filename():
    ''' fail if the file does not exist '''
    with pytest.raises(IOError) as excinfo:
        generate("non_existant_file.f90", api="dynamo0.3")
    assert "file 'non_existant_file.f90' not found" in str(excinfo.value)


def test_stub_invalid_api():
    ''' fail if the specified api is not supported '''
    with pytest.raises(GenerationError) as excinfo:
        generate("test_files/dynamo0p3/ru_kernel_mod.f90", api="dynamo0.1")
    assert "Unsupported API 'dynamo0.1' specified" in str(excinfo.value)


def test_stub_file_content_not_fortran():
    ''' fail if the kernel file does not contain fortran '''
    with pytest.raises(ParseError) as excinfo:
        generate("dynamo0p3_test.py", api="dynamo0.3")
    assert 'the file does not contain a module. Is it a Kernel file?' \
        in str(excinfo.value)


def test_stub_file_fortran_invalid():
    ''' fail if the fortran in the kernel is not valid '''
    with pytest.raises(ParseError) as excinfo:
        generate("test_files/dynamo0p3/testkern_invalid_fortran.F90",
                 api="dynamo0.3")
    assert 'invalid Fortran' in str(excinfo.value)


def test_file_fortran_not_kernel():
    ''' fail if file is valid fortran but is not a kernel file '''
    with pytest.raises(ParseError) as excinfo:
        generate("test_files/dynamo0p3/1_single_invoke.f90", api="dynamo0.3")
    assert 'file does not contain a module. Is it a Kernel file?' \
        in str(excinfo.value)


def test_module_name_too_short():
    ''' fail if length of kernel module name is too short '''
    with pytest.raises(ParseError) as excinfo:
        generate("test_files/dynamo0p3/testkern_short_name.F90",
                 api="dynamo0.3")
    assert "too short to have '_mod' as an extension" in str(excinfo.value)


def test_module_name_convention():
    ''' fail if kernel module name does not have _mod at end '''
    with pytest.raises(ParseError) as excinfo:
        generate("test_files/dynamo0p3/testkern.F90", api="dynamo0.3")
    assert "does not have '_mod' as an extension" in str(excinfo.value)


def test_kernel_datatype_not_found():
    ''' fail if kernel datatype is not found '''
    with pytest.raises(RuntimeError) as excinfo:
        generate("test_files/dynamo0p3/testkern_no_datatype.F90",
                 api="dynamo0.3")
    assert 'Kernel type testkern_type does not exist' in str(excinfo.value)

SIMPLE = (
    "  MODULE simple_mod\n"
    "    IMPLICIT NONE\n"
    "    CONTAINS\n"
    "    SUBROUTINE simple_code(nlayers, field_1_w1, ndf_w1, undf_w1,"
    " map_w1)\n"
    "      USE constants_mod, ONLY: r_def\n"
    "      IMPLICIT NONE\n"
    "      INTEGER, intent(in) :: nlayers\n"
    "      INTEGER, intent(in) :: undf_w1\n"
    "      REAL(KIND=r_def), intent(out), dimension(undf_w1) ::"
    " field_1_w1\n"
    "      INTEGER, intent(in) :: ndf_w1\n"
    "      INTEGER, intent(in), dimension(ndf_w1) :: map_w1\n"
    "    END SUBROUTINE simple_code\n"
    "  END MODULE simple_mod")


def test_stub_generate_working():
    ''' check that the stub generate produces the expected output '''
    result = generate("test_files/dynamo0p3/simple.f90",
                      api="dynamo0.3")
    print result
    assert str(result).find(SIMPLE) != -1


def test_stub_generate_working_noapi():
    ''' check that the stub generate produces the expected output when
    we use the default api (which should be dynamo0.3)'''
    result = generate("test_files/dynamo0p3/simple.f90")
    print result
    assert str(result).find(SIMPLE) != -1


# fields : intent
INTENT = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(3) =    &
          (/ arg_type(gh_field,gh_write,w1), &
             arg_type(gh_field,gh_inc, w1), &
             arg_type(gh_field,gh_read, w1)  &
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


def test_intent():
    ''' test that field intent is generated correctly for kernel stubs '''
    ast = fpapi.parse(INTENT, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    generated_code = kernel.gen_stub
    output = (
        "  MODULE dummy_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE dummy_code(nlayers, field_1_w1, field_2_w1, "
        "field_3_w1, ndf_w1, undf_w1, map_w1)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: nlayers\n"
        "      INTEGER, intent(in) :: undf_w1\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w1) :: "
        "field_1_w1\n"
        "      REAL(KIND=r_def), intent(inout), dimension(undf_w1) :: "
        "field_2_w1\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w1) :: "
        "field_3_w1\n"
        "      INTEGER, intent(in) :: ndf_w1\n"
        "      INTEGER, intent(in), dimension(ndf_w1) :: map_w1\n"
        "    END SUBROUTINE dummy_code\n"
        "  END MODULE dummy_mod")
    print output
    print str(generated_code)
    assert str(generated_code).find(output) != -1

# fields : spaces
SPACES = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(7) =               &
          (/ arg_type(gh_field,gh_write, w0),     &
             arg_type(gh_field,gh_write, w1),     &
             arg_type(gh_field,gh_write, w2),     &
             arg_type(gh_field,gh_write, w3),     &
             arg_type(gh_field,gh_write, wtheta), &
             arg_type(gh_field,gh_write, w2h),    &
             arg_type(gh_field,gh_write, w2v)     &
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


def test_spaces():
    ''' test that field spaces are handled correctly for kernel stubs '''
    ast = fpapi.parse(SPACES, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    generated_code = kernel.gen_stub
    output = (
        "  MODULE dummy_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE dummy_code(nlayers, field_1_w0, field_2_w1, "
        "field_3_w2, field_4_w3, field_5_wtheta, field_6_w2h, field_7_w2v, "
        "ndf_w0, undf_w0, map_w0, ndf_w1, undf_w1, map_w1, ndf_w2, undf_w2, "
        "map_w2, ndf_w3, undf_w3, map_w3, ndf_wtheta, undf_wtheta, "
        "map_wtheta, ndf_w2h, undf_w2h, map_w2h, ndf_w2v, undf_w2v, "
        "map_w2v)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: nlayers\n"
        "      INTEGER, intent(in) :: undf_w0\n"
        "      INTEGER, intent(in) :: undf_w1\n"
        "      INTEGER, intent(in) :: undf_w2\n"
        "      INTEGER, intent(in) :: undf_w3\n"
        "      INTEGER, intent(in) :: undf_wtheta\n"
        "      INTEGER, intent(in) :: undf_w2h\n"
        "      INTEGER, intent(in) :: undf_w2v\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w0) :: "
        "field_1_w0\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w1) :: "
        "field_2_w1\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w2) :: "
        "field_3_w2\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w3) :: "
        "field_4_w3\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_wtheta) :: "
        "field_5_wtheta\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w2h) :: "
        "field_6_w2h\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w2v) :: "
        "field_7_w2v\n"
        "      INTEGER, intent(in) :: ndf_w0\n"
        "      INTEGER, intent(in), dimension(ndf_w0) :: map_w0\n"
        "      INTEGER, intent(in) :: ndf_w1\n"
        "      INTEGER, intent(in), dimension(ndf_w1) :: map_w1\n"
        "      INTEGER, intent(in) :: ndf_w2\n"
        "      INTEGER, intent(in), dimension(ndf_w2) :: map_w2\n"
        "      INTEGER, intent(in) :: ndf_w3\n"
        "      INTEGER, intent(in), dimension(ndf_w3) :: map_w3\n"
        "      INTEGER, intent(in) :: ndf_wtheta\n"
        "      INTEGER, intent(in), dimension(ndf_wtheta) :: map_wtheta\n"
        "      INTEGER, intent(in) :: ndf_w2h\n"
        "      INTEGER, intent(in), dimension(ndf_w2h) :: map_w2h\n"
        "      INTEGER, intent(in) :: ndf_w2v\n"
        "      INTEGER, intent(in), dimension(ndf_w2v) :: map_w2v\n"
        "    END SUBROUTINE dummy_code\n"
        "  END MODULE dummy_mod")
    print output
    print str(generated_code)
    assert str(generated_code).find(output) != -1

# fields : vectors
VECTORS = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(1) =    &
          (/ arg_type(gh_field*3,gh_write, w0) &
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


def test_vectors():
    ''' test that field vectors are handled correctly for kernel stubs '''
    ast = fpapi.parse(VECTORS, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    generated_code = kernel.gen_stub
    output = (
        "  MODULE dummy_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE dummy_code(nlayers, field_1_w0_v1, "
        "field_1_w0_v2, field_1_w0_v3, ndf_w0, undf_w0, map_w0)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: nlayers\n"
        "      INTEGER, intent(in) :: undf_w0\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w0) :: "
        "field_1_w0_v1\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w0) :: "
        "field_1_w0_v2\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w0) :: "
        "field_1_w0_v3\n"
        "      INTEGER, intent(in) :: ndf_w0\n"
        "      INTEGER, intent(in), dimension(ndf_w0) :: map_w0\n"
        "    END SUBROUTINE dummy_code\n"
        "  END MODULE dummy_mod")
    print output
    print str(generated_code)
    assert str(generated_code).find(output) != -1

# operators : spaces and intent
OPERATORS = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(5) =    &
          (/ arg_type(gh_operator,gh_write, w0, w0), &
             arg_type(gh_operator,gh_inc,   w1, w1), &
             arg_type(gh_operator,gh_read,  w2, w2), &
             arg_type(gh_operator,gh_write, w3, w3), &
             arg_type(gh_operator,gh_read, any_space_1, any_space_1)  &
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
    generated_code = kernel.gen_stub
    output = (
        "  MODULE dummy_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE dummy_code(cell, nlayers, op_1_ncell_3d, op_1, "
        "op_2_ncell_3d, op_2, op_3_ncell_3d, op_3, op_4_ncell_3d, op_4, "
        "op_5_ncell_3d, op_5, ndf_w0, ndf_w1, ndf_w2, ndf_w3, "
        "ndf_any_space_1)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: cell\n"
        "      INTEGER, intent(in) :: nlayers\n"
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
        "      REAL(KIND=r_def), intent(in), dimension(ndf_any_space_1,"
        "ndf_any_space_1,op_5_ncell_3d) :: op_5\n"
        "      INTEGER, intent(in) :: ndf_w0\n"
        "      INTEGER, intent(in) :: ndf_w1\n"
        "      INTEGER, intent(in) :: ndf_w2\n"
        "      INTEGER, intent(in) :: ndf_w3\n"
        "      INTEGER, intent(in) :: ndf_any_space_1\n"
        "    END SUBROUTINE dummy_code\n"
        "  END MODULE dummy_mod")
    print output
    print str(generated_code)
    assert str(generated_code).find(output) != -1

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


# basis function : spaces
BASIS = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(7) =    &
          (/ arg_type(gh_field,   gh_write,w0), &
             arg_type(gh_operator,gh_inc,  w1, w1), &
             arg_type(gh_field,   gh_read, w2), &
             arg_type(gh_operator,gh_write,w3, w3),  &
             arg_type(gh_field,   gh_write, wtheta), &
             arg_type(gh_operator,gh_inc, w2h, w2h), &
             arg_type(gh_field,   gh_read, w2v)  &
           /)
     type(func_type), meta_funcs(7) =     &
          (/ func_type(w0, gh_basis),     &
             func_type(w1, gh_basis),     &
             func_type(w2, gh_basis),     &
             func_type(w3, gh_basis),     &
             func_type(wtheta, gh_basis), &
             func_type(w2h, gh_basis),    &
             func_type(w2v, gh_basis)     &
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


def test_basis():
    ''' Test that basis functions are handled correctly for kernel stubs '''
    ast = fpapi.parse(BASIS, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    generated_code = kernel.gen_stub
    output = (
        "  MODULE dummy_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE dummy_code(cell, nlayers, field_1_w0, op_2_ncell_3d, "
        "op_2, field_3_w2, op_4_ncell_3d, op_4, field_5_wtheta, "
        "op_6_ncell_3d, op_6, field_7_w2v, ndf_w0, undf_w0, map_w0, "
        "basis_w0, ndf_w1, basis_w1, ndf_w2, undf_w2, map_w2, basis_w2, "
        "ndf_w3, basis_w3, ndf_wtheta, undf_wtheta, map_wtheta, "
        "basis_wtheta, ndf_w2h, basis_w2h, ndf_w2v, undf_w2v, map_w2v, "
        "basis_w2v, nqp_h, nqp_v, wh, wv)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: cell\n"
        "      INTEGER, intent(in) :: nlayers\n"
        "      INTEGER, intent(in) :: undf_w0\n"
        "      INTEGER, intent(in) :: undf_w2\n"
        "      INTEGER, intent(in) :: undf_wtheta\n"
        "      INTEGER, intent(in) :: undf_w2v\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w0) :: "
        "field_1_w0\n"
        "      INTEGER, intent(in) :: op_2_ncell_3d\n"
        "      REAL(KIND=r_def), intent(inout), dimension(ndf_w1,ndf_w1,"
        "op_2_ncell_3d) :: op_2\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w2) :: "
        "field_3_w2\n"
        "      INTEGER, intent(in) :: op_4_ncell_3d\n"
        "      REAL(KIND=r_def), intent(out), dimension(ndf_w3,ndf_w3,"
        "op_4_ncell_3d) :: op_4\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_wtheta) :: "
        "field_5_wtheta\n"
        "      INTEGER, intent(in) :: op_6_ncell_3d\n"
        "      REAL(KIND=r_def), intent(inout), dimension(ndf_w2h,ndf_w2h,"
        "op_6_ncell_3d) :: op_6\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w2v) :: "
        "field_7_w2v\n"
        "      INTEGER, intent(in) :: ndf_w0\n"
        "      INTEGER, intent(in), dimension(ndf_w0) :: map_w0\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w0,nqp_h,nqp_v) "
        ":: basis_w0\n"
        "      INTEGER, intent(in) :: ndf_w1\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w1,nqp_h,nqp_v) "
        ":: basis_w1\n"
        "      INTEGER, intent(in) :: ndf_w2\n"
        "      INTEGER, intent(in), dimension(ndf_w2) :: map_w2\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w2,nqp_h,nqp_v) "
        ":: basis_w2\n"
        "      INTEGER, intent(in) :: ndf_w3\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w3,nqp_h,nqp_v) "
        ":: basis_w3\n"
        "      INTEGER, intent(in) :: ndf_wtheta\n"
        "      INTEGER, intent(in), dimension(ndf_wtheta) :: map_wtheta\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_wtheta,nqp_h,"
        "nqp_v) :: basis_wtheta\n"
        "      INTEGER, intent(in) :: ndf_w2h\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w2h,nqp_h,nqp_v) "
        ":: basis_w2h\n"
        "      INTEGER, intent(in) :: ndf_w2v\n"
        "      INTEGER, intent(in), dimension(ndf_w2v) :: map_w2v\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w2v,nqp_h,nqp_v) "
        ":: basis_w2v\n"
        "      INTEGER, intent(in) :: nqp_h, nqp_v\n"
        "      REAL(KIND=r_def), intent(in), dimension(nqp_h) :: wh\n"
        "      REAL(KIND=r_def), intent(in), dimension(nqp_v) :: wv\n"
        "    END SUBROUTINE dummy_code\n"
        "  END MODULE dummy_mod")

    print output
    print str(generated_code)
    assert str(generated_code).find(output) != -1

BASIS_UNSUPPORTED_SPACE = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(1) =    &
          (/ arg_type(gh_field,gh_write, any_space_1) &
           /)
     type(func_type), meta_funcs(1) =    &
          (/ func_type(any_space_1, gh_basis) &
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


def test_basis_unsupported_space():
    ''' test that an error is raised when a basis function is on an
    unsupported space (currently any_space_*) '''
    ast = fpapi.parse(BASIS_UNSUPPORTED_SPACE, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    with pytest.raises(GenerationError) as excinfo:
        _ = kernel.gen_stub
    assert 'Unsupported space for basis function' in str(excinfo.value)

# diff basis function : spaces
DIFF_BASIS = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(7) =    &
          (/ arg_type(gh_field,   gh_write,w0), &
             arg_type(gh_operator,gh_inc,  w1, w1), &
             arg_type(gh_field,   gh_read, w2), &
             arg_type(gh_operator,gh_write,w3, w3),  &
             arg_type(gh_field,   gh_write, wtheta), &
             arg_type(gh_operator,gh_inc, w2h, w2h), &
             arg_type(gh_field,   gh_read, w2v)  &
           /)
     type(func_type), meta_funcs(7) =          &
          (/ func_type(w0, gh_diff_basis),     &
             func_type(w1, gh_diff_basis),     &
             func_type(w2, gh_diff_basis),     &
             func_type(w3, gh_diff_basis),     &
             func_type(wtheta, gh_diff_basis), &
             func_type(w2h, gh_diff_basis),    &
             func_type(w2v, gh_diff_basis)     &
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


def test_diff_basis():
    ''' Test that differential basis functions are handled correctly
    for kernel stubs '''
    ast = fpapi.parse(DIFF_BASIS, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    generated_code = kernel.gen_stub
    output = (
        "  MODULE dummy_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE dummy_code(cell, nlayers, field_1_w0, op_2_ncell_3d, "
        "op_2, field_3_w2, op_4_ncell_3d, op_4, field_5_wtheta, "
        "op_6_ncell_3d, op_6, field_7_w2v, ndf_w0, undf_w0, map_w0, "
        "diff_basis_w0, ndf_w1, diff_basis_w1, ndf_w2, undf_w2, map_w2, "
        "diff_basis_w2, ndf_w3, diff_basis_w3, ndf_wtheta, undf_wtheta, "
        "map_wtheta, diff_basis_wtheta, ndf_w2h, diff_basis_w2h, ndf_w2v, "
        "undf_w2v, map_w2v, diff_basis_w2v, nqp_h, nqp_v, wh, wv)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: cell\n"
        "      INTEGER, intent(in) :: nlayers\n"
        "      INTEGER, intent(in) :: undf_w0\n"
        "      INTEGER, intent(in) :: undf_w2\n"
        "      INTEGER, intent(in) :: undf_wtheta\n"
        "      INTEGER, intent(in) :: undf_w2v\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w0) :: "
        "field_1_w0\n"
        "      INTEGER, intent(in) :: op_2_ncell_3d\n"
        "      REAL(KIND=r_def), intent(inout), dimension(ndf_w1,ndf_w1,"
        "op_2_ncell_3d) :: op_2\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w2) :: "
        "field_3_w2\n"
        "      INTEGER, intent(in) :: op_4_ncell_3d\n"
        "      REAL(KIND=r_def), intent(out), dimension(ndf_w3,ndf_w3,"
        "op_4_ncell_3d) :: op_4\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_wtheta) :: "
        "field_5_wtheta\n"
        "      INTEGER, intent(in) :: op_6_ncell_3d\n"
        "      REAL(KIND=r_def), intent(inout), dimension(ndf_w2h,ndf_w2h,"
        "op_6_ncell_3d) :: op_6\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w2v) :: "
        "field_7_w2v\n"
        "      INTEGER, intent(in) :: ndf_w0\n"
        "      INTEGER, intent(in), dimension(ndf_w0) :: map_w0\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w0,nqp_h,nqp_v) "
        ":: diff_basis_w0\n"
        "      INTEGER, intent(in) :: ndf_w1\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w1,nqp_h,nqp_v) "
        ":: diff_basis_w1\n"
        "      INTEGER, intent(in) :: ndf_w2\n"
        "      INTEGER, intent(in), dimension(ndf_w2) :: map_w2\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w2,nqp_h,nqp_v) "
        ":: diff_basis_w2\n"
        "      INTEGER, intent(in) :: ndf_w3\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w3,nqp_h,nqp_v) "
        ":: diff_basis_w3\n"
        "      INTEGER, intent(in) :: ndf_wtheta\n"
        "      INTEGER, intent(in), dimension(ndf_wtheta) :: map_wtheta\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_wtheta,nqp_h,"
        "nqp_v) :: diff_basis_wtheta\n"
        "      INTEGER, intent(in) :: ndf_w2h\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w2h,nqp_h,nqp_v) "
        ":: diff_basis_w2h\n"
        "      INTEGER, intent(in) :: ndf_w2v\n"
        "      INTEGER, intent(in), dimension(ndf_w2v) :: map_w2v\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w2v,nqp_h,nqp_v) "
        ":: diff_basis_w2v\n"
        "      INTEGER, intent(in) :: nqp_h, nqp_v\n"
        "      REAL(KIND=r_def), intent(in), dimension(nqp_h) :: wh\n"
        "      REAL(KIND=r_def), intent(in), dimension(nqp_v) :: wv\n"
        "    END SUBROUTINE dummy_code\n"
        "  END MODULE dummy_mod")
    print output
    print str(generated_code)
    assert str(generated_code).find(output) != -1

DIFF_BASIS_UNSUPPORTED_SPACE = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(1) =    &
          (/ arg_type(gh_field,gh_write, any_space_1) &
           /)
     type(func_type), meta_funcs(1) =    &
          (/ func_type(any_space_1, gh_diff_basis) &
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


def test_diff_basis_unsupp_space():
    ''' test that an error is raised when a differential basis
    function is on an unsupported space (currently any_space_*)'''
    ast = fpapi.parse(DIFF_BASIS_UNSUPPORTED_SPACE, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    with pytest.raises(GenerationError) as excinfo:
        _ = kernel.gen_stub
    assert 'Unsupported space for differential basis function' \
        in str(excinfo.value)

# orientation : spaces

ORIENTATION_OUTPUT = (
    "    SUBROUTINE dummy_orientation_code(cell, nlayers, field_1_w0, "
    "op_2_ncell_3d, op_2, field_3_w2, op_4_ncell_3d, op_4, ndf_w0, "
    "undf_w0, map_w0, orientation_w0, ndf_w1, orientation_w1, ndf_w2, "
    "undf_w2, map_w2, orientation_w2, ndf_w3, orientation_w3, nqp_h, "
    "nqp_v, wh, wv)\n"
    "      USE constants_mod, ONLY: r_def\n"
    "      IMPLICIT NONE\n"
    "      INTEGER, intent(in) :: cell\n"
    "      INTEGER, intent(in) :: nlayers\n"
    "      INTEGER, intent(in) :: undf_w0\n"
    "      INTEGER, intent(in) :: undf_w2\n"
    "      REAL(KIND=r_def), intent(out), dimension(undf_w0) :: "
    "field_1_w0\n"
    "      INTEGER, intent(in) :: op_2_ncell_3d\n"
    "      REAL(KIND=r_def), intent(inout), dimension(ndf_w1,ndf_w1,"
    "op_2_ncell_3d) :: op_2\n"
    "      REAL(KIND=r_def), intent(in), dimension(undf_w2) :: "
    "field_3_w2\n"
    "      INTEGER, intent(in) :: op_4_ncell_3d\n"
    "      REAL(KIND=r_def), intent(out), dimension(ndf_w3,ndf_w3,"
    "op_4_ncell_3d) :: op_4\n"
    "      INTEGER, intent(in) :: ndf_w0\n"
    "      INTEGER, intent(in), dimension(ndf_w0) :: map_w0\n"
    "      INTEGER, intent(in), dimension(ndf_w0) :: orientation_w0\n"
    "      INTEGER, intent(in) :: ndf_w1\n"
    "      INTEGER, intent(in), dimension(ndf_w1) :: orientation_w1\n"
    "      INTEGER, intent(in) :: ndf_w2\n"
    "      INTEGER, intent(in), dimension(ndf_w2) :: map_w2\n"
    "      INTEGER, intent(in), dimension(ndf_w2) :: orientation_w2\n"
    "      INTEGER, intent(in) :: ndf_w3\n"
    "      INTEGER, intent(in), dimension(ndf_w3) :: orientation_w3\n"
    "      INTEGER, intent(in) :: nqp_h, nqp_v\n"
    "      REAL(KIND=r_def), intent(in), dimension(nqp_h) :: wh\n"
    "      REAL(KIND=r_def), intent(in), dimension(nqp_v) :: wv\n"
    "    END SUBROUTINE dummy_orientation_code\n"
    "  END MODULE dummy_orientation_mod")


def test_orientation_stubs():
    ''' Test that orientation is handled correctly for kernel
    stubs '''
    # Read-in the meta-data from file (it's in a file because it's also
    # used when testing the genkernelstub script from the command
    # line).
    with open(os.path.join(BASE_PATH, "dummy_orientation_mod.f90"),
              "r") as myfile:
        orientation = myfile.read()

    ast = fpapi.parse(orientation, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    generated_code = kernel.gen_stub
    print str(generated_code)
    assert str(generated_code).find(ORIENTATION_OUTPUT) != -1


def test_enforce_bc_kernel_stub_gen():
    ''' Test that the enforce_bc_kernel boundary layer argument modification
    is handled correctly for kernel stubs'''
    ast = fpapi.parse(os.path.join(BASE_PATH, "enforce_bc_kernel_mod.f90"),
                      ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    generated_code = kernel.gen_stub
    output = (
        "  MODULE enforce_bc_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE enforce_bc_code(nlayers, field_1_any_space_1, "
        "ndf_any_space_1, undf_any_space_1, map_any_space_1, boundary_dofs)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: nlayers\n"
        "      INTEGER, intent(in) :: undf_any_space_1\n"
        "      REAL(KIND=r_def), intent(inout), dimension(undf_any_space_1)"
        " :: field_1_any_space_1\n"
        "      INTEGER, intent(in) :: ndf_any_space_1\n"
        "      INTEGER, intent(in), dimension(ndf_any_space_1) :: "
        "map_any_space_1\n"
        "      INTEGER, intent(in), dimension(ndf_any_space_1,2) :: "
        "boundary_dofs\n"
        "    END SUBROUTINE enforce_bc_code\n"
        "  END MODULE enforce_bc_mod")
    print output
    print str(generated_code)
    assert str(generated_code).find(output) != -1

# note, we do not need a separate test for qr as it is implicitly
# tested for in the above examples.
# fields : intent

SUB_NAME = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(1) =    &
          (/ arg_type(gh_field,gh_write,w1) &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => dummy
  end type dummy_type
contains
  subroutine dummy()
  end subroutine dummy
end module dummy_mod
'''


def test_sub_name():
    ''' test for expected behaviour when the kernel subroutine does
    not conform to the convention of having "_code" at the end of its
    name. In this case we append "_code to the name and _mod to the
    kernel name.'''
    ast = fpapi.parse(SUB_NAME, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    generated_code = kernel.gen_stub
    output = (
        "  MODULE dummy_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE dummy_code(nlayers, field_1_w1, "
        "ndf_w1, undf_w1, map_w1)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: nlayers\n"
        "      INTEGER, intent(in) :: undf_w1\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w1) :: "
        "field_1_w1\n"
        "      INTEGER, intent(in) :: ndf_w1\n"
        "      INTEGER, intent(in), dimension(ndf_w1) :: map_w1\n"
        "    END SUBROUTINE dummy_code\n"
        "  END MODULE dummy_mod")
    print output
    print str(generated_code)
    assert str(generated_code).find(output) != -1


def test_kernel_stub_usage():
    ''' Check that the kernel-stub generator prints a usage message
    if no arguments are supplied '''
    from subprocess import Popen, STDOUT, PIPE

    usage_msg = (
        "usage: genkernelstub.py [-h] [-o OUTFILE] [-api API] [-l] filename\n"
        "genkernelstub.py: error: too few arguments")

    # We use the Popen constructor here rather than check_output because
    # the latter is only available in Python 2.7 onwards.
    out = Popen(['python', '../genkernelstub.py'],
                stdout=PIPE,
                stderr=STDOUT).communicate()[0]
    assert usage_msg in out


def test_kernel_stub_gen_cmd_line():
    ''' Check that we can call the kernel-stub generator from the
    command line '''
    from subprocess import Popen, PIPE
    # We use the Popen constructor here rather than check_output because
    # the latter is only available in Python 2.7 onwards.
    out = Popen(["python", "../genkernelstub.py",
                 os.path.join(BASE_PATH, "dummy_orientation_mod.f90")],
                stdout=PIPE).communicate()[0]

    print "Output was: ", out
    assert ORIENTATION_OUTPUT in out


def test_arg_descriptor_functions_method_error():
    ''' Tests that an internal error is raised in DynArgDescriptor03
    when function_spaces is called and the internal type is an
    unexpected value. It should not be possible to get to here so we
    need to mess about with internal values to trip this.'''
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    field_descriptor = metadata.arg_descriptors[0]
    field_descriptor._type = "gh_fire_starter"
    with pytest.raises(RuntimeError) as excinfo:
        _ = field_descriptor.function_spaces
    assert 'Internal error, DynArgDescriptor03:function_spaces(), should ' \
        'not get to here' in str(excinfo.value)


def test_arg_ref_name_method_error1():
    ''' Tests that an internal error is raised in DynKernelArgument
    when ref_name() is called with a function space that is not
    associated with this field'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    first_kernel = first_invoke.schedule.kern_calls()[0]
    first_argument = first_kernel.arguments.args[0]
    # the argument is a field and is on "w1"
    with pytest.raises(GenerationError) as excinfo:
        _ = first_argument.ref_name("w3")
    assert 'not one of the function spaces associated with this argument' \
        in str(excinfo.value)


def test_arg_ref_name_method_error2():
    ''' Tests that an internal error is raised in DynKernelArgument
    when ref_name() is called when the argument type is not one of
    gh_field or gh_operator'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    first_kernel = first_invoke.schedule.kern_calls()[0]
    first_argument = first_kernel.arguments.args[0]
    first_argument._type = "gh_funky_instigator"
    with pytest.raises(GenerationError) as excinfo:
        _ = first_argument.ref_name()
    assert 'ref_name: Error, unsupported arg type' in str(excinfo)


def test_arg_descriptor_function_method_error():
    ''' Tests that an internal error is raised in DynArgDescriptor03
    when function_space is called and the internal type is an
    unexpected value. It should not be possible to get to here so we
    need to mess about with internal values to trip this.'''
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    field_descriptor = metadata.arg_descriptors[0]
    field_descriptor._type = "gh_fire_starter"
    with pytest.raises(RuntimeError) as excinfo:
        _ = field_descriptor.function_space
    assert 'Internal error, DynArgDescriptor03:function_space(), should ' \
        'not get to here' in str(excinfo.value)


def test_arg_descriptor_str():
    ''' Tests that the string method for DynArgDescriptor03 works as
    expected '''
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    field_descriptor = metadata.arg_descriptors[0]
    result = str(field_descriptor)
    expected_output = (
        "DynArgDescriptor03 object\n"
        "  argument_type[0]='gh_field'\n"
        "  access_descriptor[1]='gh_write'\n"
        "  function_space[2]='w1'")
    assert expected_output in result


def test_arg_descriptor_str_error():
    ''' Tests that an internal error is raised in DynArgDescriptor03
    when __str__ is called and the internal type is an
    unexpected value. It should not be possible to get to here so we
    need to mess about with internal values to trip this.'''
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    field_descriptor = metadata.arg_descriptors[0]
    field_descriptor._type = "gh_fire_starter"
    with pytest.raises(ParseError) as excinfo:
        _ = str(field_descriptor)
    assert 'Internal error in DynArgDescriptor03.__str__' \
        in str(excinfo.value)


def test_arg_descriptor_repr():
    ''' Tests that the repr method for DynArgDescriptor03 works as
    expected '''
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    field_descriptor = metadata.arg_descriptors[0]
    result = repr(field_descriptor)
    assert 'DynArgDescriptor03(arg_type(gh_field, gh_write, w1))' \
        in result


def test_arg_descriptor_function_space_tofrom_error():
    ''' Tests that an internal error is raised in DynArgDescriptor03
    when function_space_to or function_space_from is called and the
    internal type is not gh_operator.'''
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    field_descriptor = metadata.arg_descriptors[0]
    with pytest.raises(RuntimeError) as excinfo:
        _ = field_descriptor.function_space_to
    assert 'function_space_to only makes sense for a gh_operator' \
        in str(excinfo.value)
    with pytest.raises(RuntimeError) as excinfo:
        _ = field_descriptor.function_space_from
    assert 'function_space_from only makes sense for a gh_operator' \
        in str(excinfo.value)


def test_arg_descriptor_init_error():
    ''' Tests that an internal error is raised in DynArgDescriptor03
    when an invalid type is provided. However this error never gets
    tripped due to an earlier test so we need to force the error by
    changing the internal state.'''
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    field_descriptor = metadata.arg_descriptors[0]
    # extract an arg_type object that we can use to create a
    # DynArgDescriptor03 object
    arg_type = field_descriptor._arg_type
    # Now try to trip the error by making the initial test think
    # that GH_INVALID is actually valid
    from dynamo0p3 import VALID_ARG_TYPE_NAMES, DynArgDescriptor03
    keep = []
    keep.extend(VALID_ARG_TYPE_NAMES)
    VALID_ARG_TYPE_NAMES.append("GH_INVALID")
    arg_type.args[0].name = "GH_INVALID"
    with pytest.raises(ParseError) as excinfo:
        _ = DynArgDescriptor03(arg_type)
    assert 'Internal error in DynArgDescriptor03.__init__' \
        in str(excinfo.value)
    VALID_ARG_TYPE_NAMES = keep


def test_func_descriptor_repr():
    ''' Tests the __repr__ output of a func_descriptor '''
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    func_descriptor = metadata.func_descriptors[0]
    func_str = repr(func_descriptor)
    assert "DynFuncDescriptor03(func_type(w1, gh_basis))" in func_str


def test_func_descriptor_str():
    ''' Tests the __str__ output of a func_descriptor '''
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    func_descriptor = metadata.func_descriptors[0]
    func_str = str(func_descriptor)
    output = (
        "DynFuncDescriptor03 object\n"
        "  name='func_type'\n"
        "  nargs=2\n"
        "  function_space_name[0] = 'w1'\n"
        "  operator_name[1] = 'gh_basis'")
    assert output in func_str
