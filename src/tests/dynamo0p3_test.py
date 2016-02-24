# ------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# ------------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

''' This module tests the Dynamo 0.3 API using pytest. '''

# imports
import os
import pytest
from parse import parse, ParseError
from psyGen import PSyFactory, GenerationError
import fparser
from fparser import api as fpapi
from dynamo0p3 import DynKernMetadata, DynKern, DynLoop
from transformations import LoopFuseTrans, ColourTrans
from genkernelstub import generate

# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")
CODE = '''
module testkern_qr
  type, extends(kernel_type) :: testkern_qr_type
     type(arg_type), meta_args(6) =                 &
          (/ arg_type(gh_rscalar, gh_read),         &
             arg_type(gh_field,gh_write,w1),        &
             arg_type(gh_field,gh_read, w2),        &
             arg_type(gh_operator,gh_read, w2, w2), &
             arg_type(gh_field,gh_read, w3),        &
             arg_type(gh_iscalar, gh_read)          &
           /)
     type(func_type), dimension(3) :: meta_funcs =  &
          (/ func_type(w1, gh_basis),               &
             func_type(w2, gh_diff_basis),          &
             func_type(w3, gh_basis, gh_diff_basis) &
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


def test_arg_descriptor_vector_str():
    ''' Test the str method of an argument descriptor containing a vector '''
    fparser.logging.disable('CRITICAL')
    # Change the meta-data so that the second argument is a vector
    code = CODE.replace("gh_field,gh_write,w1", "gh_field*3,gh_write,w1", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    dkm = DynKernMetadata(ast, name=name)
    dkm_str = str(dkm.arg_descriptors[1])
    expected = (
        "DynArgDescriptor03 object\n"
        "  argument_type[0]='gh_field'*3\n"
        "  access_descriptor[1]='gh_write'\n"
        "  function_space[2]='w1'")
    assert expected in dkm_str


def test_arg_descriptor_op_str():
    '''Test the str method of an argument descriptor containing an
    operator

    '''
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(CODE, ignore_comments=False)
    name = "testkern_qr_type"
    dkm = DynKernMetadata(ast, name=name)
    dkm_str = str(dkm.arg_descriptors[3])
    expected = (
        "DynArgDescriptor03 object\n"
        "  argument_type[0]='gh_operator'\n"
        "  access_descriptor[1]='gh_read'\n"
        "  function_space_to[2]='w2'\n"
        "  function_space_from[3]='w2'")
    print dkm_str
    assert expected in dkm_str


def test_ad_scalar_type_too_few_args():
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar has fewer than 2 args. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_rscalar, gh_read)",
                        "arg_type(gh_rscalar)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'each meta_arg entry must have at least 2 args' \
        in str(excinfo.value)


def test_ad_scalar_type_too_many_args():
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar has more than 2 args. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_rscalar, gh_read)",
                        "arg_type(gh_rscalar, gh_read, w1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'each meta_arg entry must have 2 arguments if' \
        in str(excinfo.value)


def test_ad_scalar_type_no_write():
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar specifies GH_WRITE '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_rscalar, gh_read)",
                        "arg_type(gh_rscalar, gh_write)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert ("scalar arguments must be read-only (gh_read) but found "
            "'gh_write'" in str(excinfo.value))


def test_ad_scalar_type_no_inc():
    ''' Tests that an error is raised when the argument descriptor
    metadata for a scalar specifies GH_INC '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_rscalar, gh_read)",
                        "arg_type(gh_rscalar, gh_inc)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert "scalar arguments must be read-only (gh_read) but found 'gh_inc'" \
        in str(excinfo.value)


def test_ad_field_type_too_few_args():
    ''' Tests that an error is raised when the argument descriptor
    metadata for a field has fewer than 3 args. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_field,gh_write,w1)",
                        "arg_type(gh_field,gh_write)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'each meta_arg entry must have at least 3 arguments' \
        in str(excinfo.value)


def test_ad_fld_type_too_many_args():
    ''' Tests that an error is raised when the argument descriptor
    metadata has more than 4 args. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_field,gh_write,w1)",
                        "arg_type(gh_field,gh_write,w1,w1,w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert "each meta_arg entry must have at most 4 arguments if its first" \
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


def test_ad_op_type_1st_arg_not_space():
    ''' Tests that an error is raised when the operator descriptor
    metadata contains something that is not a valid space. '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_operator,gh_read, w2, w2)",
                        "arg_type(gh_operator,gh_read, wbroke, w2)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'meta_arg entry must be a valid function space' in \
        str(excinfo.value)


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
    ''' Tests that a call with a set of fields, no basis functions and
    no distributed memory, produces correct code.'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    generated_code = psy.gen
    output = (
        "  MODULE psy_single_invoke\n"
        "    USE constants_mod, ONLY: r_def\n"
        "    USE quadrature_mod, ONLY: quadrature_type\n"
        "    USE operator_mod, ONLY: operator_type, operator_proxy_type\n"
        "    USE field_mod, ONLY: field_type, field_proxy_type\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_testkern_type(a, f1, f2, m1, m2)\n"
        "      USE testkern, ONLY: testkern_code\n"
        "      REAL(KIND=r_def), intent(inout) :: a\n"
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
        "        CALL testkern_code(nlayers, a, f1_proxy%data, f2_proxy%data, "
        "m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, map_w1, ndf_w2, "
        "undf_w2, map_w2, ndf_w3, undf_w3, map_w3)\n"
        "      END DO \n"
        "      !\n"
        "    END SUBROUTINE invoke_0_testkern_type\n"
        "  END MODULE psy_single_invoke")
    print output
    print generated_code
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
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      TYPE(field_type), intent(inout) :: f1, f2, m1, m2, f3, f4, m3\n"
        "      INTEGER, pointer :: map_w1(:) => null(), map_w2(:) => null(), "
        "map_w3(:) => null(), map_wtheta(:) => null(), map_w2h(:) => null(), "
        "map_w2v(:) => null()\n"
        "      INTEGER cell\n"
        "      INTEGER ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, undf_w3, "
        "ndf_wtheta, undf_wtheta, ndf_w2h, undf_w2h, ndf_w2v, undf_w2v\n"
        "      TYPE(mesh_type) mesh\n"
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
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh = f1%get_mesh()\n"
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
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
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
        "      ! Set halos dirty for fields modified in the above loop\n"
        "      !\n"
        "      CALL f1_proxy%set_dirty()\n"
        "      CALL f3_proxy%set_dirty()\n"
        "      !\n"
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
    generated_code = str(psy.gen)
    print generated_code
    print generated_code
    output = (
        "    SUBROUTINE invoke_0_testkern_qr_type(f1, f2, m1, a, m2, istp,"
        " qr)\n"
        "      USE testkern_qr, ONLY: testkern_qr_code\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      REAL(KIND=r_def), intent(inout) :: a\n"
        "      INTEGER, intent(inout) :: istp\n"
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
        "      TYPE(mesh_type) mesh\n"
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
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh = f1%get_mesh()\n"
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
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        map_w1 => f1_proxy%vspace%get_cell_dofmap(cell)\n"
        "        map_w2 => f2_proxy%vspace%get_cell_dofmap(cell)\n"
        "        map_w3 => m2_proxy%vspace%get_cell_dofmap(cell)\n"
        "        !\n"
        "        CALL testkern_qr_code(nlayers, f1_proxy%data, f2_proxy%data, "
        "m1_proxy%data, a, m2_proxy%data, istp, ndf_w1, undf_w1, map_w1, "
        "basis_w1, ndf_w2, undf_w2, map_w2, diff_basis_w2, ndf_w3, undf_w3, "
        "map_w3, basis_w3, diff_basis_w3, nqp_h, nqp_v, wh, wv)\n"
        "      END DO \n"
        "      !\n"
        "      ! Set halos dirty for fields modified in the above loop\n"
        "      !\n"
        "      CALL f1_proxy%set_dirty()\n"
        "      !\n"
        "      !\n"
        "      ! Deallocate basis arrays\n"
        "      !\n"
        "      DEALLOCATE (basis_w1, diff_basis_w2, basis_w3, diff_basis_w3)\n"
        "      !\n"
        "    END SUBROUTINE invoke_0_testkern_qr_type"
    )
    assert output in generated_code


def test_real_scalar():
    ''' tests that we generate correct code when a kernel takes a single,
    real scalar argument (plus fields)'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    expected = (
        "    SUBROUTINE invoke_0_testkern_type(a, f1, f2, m1, m2)\n"
        "      USE testkern, ONLY: testkern_code\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      REAL(KIND=r_def), intent(inout) :: a\n"
        "      TYPE(field_type), intent(inout) :: f1, f2, m1, m2\n"
        "      INTEGER, pointer :: map_w1(:) => null(), map_w2(:) => null(), "
        "map_w3(:) => null()\n"
        "      INTEGER cell\n"
        "      INTEGER ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, undf_w3\n"
        "      TYPE(mesh_type) mesh\n"
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
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh = f1%get_mesh()\n"
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
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        map_w1 => f1_proxy%vspace%get_cell_dofmap(cell)\n"
        "        map_w2 => f2_proxy%vspace%get_cell_dofmap(cell)\n"
        "        map_w3 => m2_proxy%vspace%get_cell_dofmap(cell)\n"
        "        !\n"
        "        CALL testkern_code(nlayers, a, f1_proxy%data, f2_proxy%data,"
        " m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, map_w1, ndf_w2, "
        "undf_w2, map_w2, ndf_w3, undf_w3, map_w3)\n")
    assert expected in generated_code


def test_int_scalar():
    ''' tests that we generate correct code when a kernel takes a single,
    integer scalar argument (plus fields) '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.6.1_single_invoke_1_int_scalar.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    expected = (
        "    SUBROUTINE invoke_0_testkern_type(f1, iflag, f2, m1, m2)\n"
        "      USE testkern_one_int_scalar, ONLY: testkern_code\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      INTEGER, intent(inout) :: iflag\n"
        "      TYPE(field_type), intent(inout) :: f1, f2, m1, m2\n"
        "      INTEGER, pointer :: map_w1(:) => null(), map_w2(:) => null(), "
        "map_w3(:) => null()\n"
        "      INTEGER cell\n"
        "      INTEGER ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, undf_w3\n"
        "      TYPE(mesh_type) mesh\n"
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
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh = f1%get_mesh()\n"
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
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        map_w1 => f1_proxy%vspace%get_cell_dofmap(cell)\n"
        "        map_w2 => f2_proxy%vspace%get_cell_dofmap(cell)\n"
        "        map_w3 => m2_proxy%vspace%get_cell_dofmap(cell)\n"
        "        !\n"
        "        CALL testkern_code(nlayers, f1_proxy%data, iflag, "
        "f2_proxy%data, m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, "
        "map_w1, ndf_w2, undf_w2, map_w2, ndf_w3, undf_w3, map_w3)\n")
    assert expected in generated_code


def test_two_real_scalars():
    ''' tests that we generate correct code when a kernel has two real,
    scalar arguments '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.9_single_invoke_2_real_scalars.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    expected = (
        "    SUBROUTINE invoke_0_testkern_type(a, f1, f2, m1, m2, b)\n"
        "      USE testkern_two_real_scalars, ONLY: testkern_code\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      REAL(KIND=r_def), intent(inout) :: a, b\n"
        "      TYPE(field_type), intent(inout) :: f1, f2, m1, m2\n"
        "      INTEGER, pointer :: map_w1(:) => null(), map_w2(:) => null(), "
        "map_w3(:) => null()\n"
        "      INTEGER cell\n"
        "      INTEGER ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, undf_w3\n"
        "      TYPE(mesh_type) mesh\n"
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
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh = f1%get_mesh()\n"
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
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        map_w1 => f1_proxy%vspace%get_cell_dofmap(cell)\n"
        "        map_w2 => f2_proxy%vspace%get_cell_dofmap(cell)\n"
        "        map_w3 => m2_proxy%vspace%get_cell_dofmap(cell)\n"
        "        !\n"
        "        CALL testkern_code(nlayers, a, f1_proxy%data, "
        "f2_proxy%data, m1_proxy%data, m2_proxy%data, b, ndf_w1, "
        "undf_w1, map_w1, ndf_w2, undf_w2, map_w2, ndf_w3, undf_w3, map_w3)\n")
    assert expected in generated_code


def test_two_int_scalars():
    ''' tests that we generate correct code when a kernel has two integer,
    scalar arguments '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.6_single_invoke_2_int_scalars.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    expected = (
        "    SUBROUTINE invoke_0_testkern_type(iflag, f1, f2, m1, m2, istep)\n"
        "      USE testkern_two_int_scalars, ONLY: testkern_code\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      INTEGER, intent(inout) :: iflag, istep\n"
        "      TYPE(field_type), intent(inout) :: f1, f2, m1, m2\n"
        "      INTEGER, pointer :: map_w1(:) => null(), map_w2(:) => null(), "
        "map_w3(:) => null()\n"
        "      INTEGER cell\n"
        "      INTEGER ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, undf_w3\n"
        "      TYPE(mesh_type) mesh\n"
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
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh = f1%get_mesh()\n"
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
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        map_w1 => f1_proxy%vspace%get_cell_dofmap(cell)\n"
        "        map_w2 => f2_proxy%vspace%get_cell_dofmap(cell)\n"
        "        map_w3 => m2_proxy%vspace%get_cell_dofmap(cell)\n"
        "        !\n"
        "        CALL testkern_code(nlayers, iflag, f1_proxy%data, "
        "f2_proxy%data, m1_proxy%data, m2_proxy%data, istep, ndf_w1, "
        "undf_w1, map_w1, ndf_w2, undf_w2, map_w2, ndf_w3, undf_w3, map_w3)\n")
    assert expected in generated_code


def test_two_scalars():
    ''' tests that we generate correct code when a kernel has two scalar
    arguments, one real and one integer '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    expected = (
        "    SUBROUTINE invoke_0_testkern_type(a, f1, f2, m1, m2, istep)\n"
        "      USE testkern_two_scalars, ONLY: testkern_code\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      REAL(KIND=r_def), intent(inout) :: a\n"
        "      INTEGER, intent(inout) :: istep\n"
        "      TYPE(field_type), intent(inout) :: f1, f2, m1, m2\n"
        "      INTEGER, pointer :: map_w1(:) => null(), map_w2(:) => null(), "
        "map_w3(:) => null()\n"
        "      INTEGER cell\n"
        "      INTEGER ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, undf_w3\n"
        "      TYPE(mesh_type) mesh\n"
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
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh = f1%get_mesh()\n"
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
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        map_w1 => f1_proxy%vspace%get_cell_dofmap(cell)\n"
        "        map_w2 => f2_proxy%vspace%get_cell_dofmap(cell)\n"
        "        map_w3 => m2_proxy%vspace%get_cell_dofmap(cell)\n"
        "        !\n"
        "        CALL testkern_code(nlayers, a, f1_proxy%data, f2_proxy%data,"
        " m1_proxy%data, m2_proxy%data, istep, ndf_w1, undf_w1, map_w1, "
        "ndf_w2, undf_w2, map_w2, ndf_w3, undf_w3, map_w3)\n")
    assert expected in generated_code


@pytest.mark.xfail(reason="We currently only support scalars which are "
                   "gh_read and so the kernel used for this test has no "
                   "argument that is written to and that triggers a "
                   "different exception. This test can be re-instated once "
                   "we support gh_inc for scalars")
def test_scalar_only():
    ''' tests that we raise an error when a kernel erroneously
    only has scalar arguments '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.8_single_invoke_no_fields.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    with pytest.raises(GenerationError) as excinfo:
        _ = str(psy.gen)
    assert 'Cannot create an Invoke with no field/operator arg' in \
        str(excinfo.value)


def test_no_vector_scalar():
    ''' Tests that we raise an error when kernel meta-data erroneously
    specifies a vector scalar '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_rscalar, gh_read)",
                        "arg_type(gh_rscalar*3, gh_read)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    name = "testkern_qr_type"
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name=name)
    assert 'vector notation is not supported for scalar arguments' in \
        str(excinfo.value)


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
    print generated_code
    # all references to chi_proxy should be chi_proxy(1)
    assert str(generated_code).find("chi_proxy%") == -1
    assert str(generated_code).count("chi_proxy(1)%vspace") == 4
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
    generated_code = str(psy.gen)
    assert generated_code.find("SUBROUTINE invoke_0_testkern_operator"
                               "_type(mm_w0, chi, a, qr)") != -1
    assert generated_code.find("TYPE(operator_type), intent(inout) ::"
                               " mm_w0") != -1
    assert generated_code.find("TYPE(operator_proxy_type) mm_w0_"
                               "proxy") != -1
    assert generated_code.find("mm_w0_proxy = mm_w0%get_proxy()") != -1
    assert generated_code.find(
        "CALL testkern_operator_code(cell, nlayers, mm_w0_proxy%ncell_3d, mm_"
        "w0_proxy%local_stencil, chi_proxy(1)%data, chi_proxy(2)%data, chi_pr"
        "oxy(3)%data, a, ndf_w0, undf_w0, map_w0, basis_w0, diff_basis_w0, "
        "nqp_h, nqp_v, wh, wv)") != -1


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
        "      USE mesh_mod, ONLY: mesh_type\n"
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
        "      TYPE(mesh_type) mesh\n"
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
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh = mapping%get_mesh()\n"
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
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
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
    gen = str(psy.gen)
    print gen
    assert "mesh = my_mapping%get_mesh()" in gen
    assert "nlayers = my_mapping_proxy%fs_from%get_nlayers()" in gen
    assert "ndf_w3 = my_mapping_proxy%fs_from%get_ndf()" in gen
    assert "ndf_w2 = my_mapping_proxy%fs_to%get_ndf()" in gen
    assert "DO cell=1,mesh%get_last_edge_cell()" in gen
    assert ("(cell, nlayers, my_mapping_proxy%ncell_3d, my_mapping_proxy%"
            "local_stencil, ndf_w2, ndf_w3)" in gen)


def test_operator_nofield_scalar():
    ''' tests that an operator with no field and a
    scalar argument is implemented correctly in the PSy layer '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.6_operator_no_field_scalar.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    gen = str(psy.gen)
    print gen
    assert "mesh = my_mapping%get_mesh()" in gen
    assert "nlayers = my_mapping_proxy%fs_from%get_nlayers()" in gen
    assert "ndf_w2 = my_mapping_proxy%fs_from%get_ndf()" in gen
    assert "DO cell=1,mesh%get_last_halo_cell(1)" in gen
    assert (
        "(cell, nlayers, my_mapping_proxy%ncell_3d, my_mapping_proxy%"
        "local_stencil, b, ndf_w2, basis_w2, nqp_h, nqp_v, wh, wv)" in gen)


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
    print gen_str
    assert (
        "INTEGER, pointer :: orientation_w1(:) => null(), orientation_w2(:)"
        " => null()" in gen_str)
    assert "ndf_w2 = my_mapping_proxy%fs_from%get_ndf()" in gen_str
    assert "ndf_w1 = my_mapping_proxy%fs_to%get_ndf()" in gen_str
    assert "dim_w1 = my_mapping_proxy%fs_to%get_dim_space()" in gen_str
    assert (
        "CALL my_mapping_proxy%fs_to%compute_basis_function(basis_w1, ndf_w1,"
        " nqp_h, nqp_v, xp, zp)" in gen_str)
    assert (
        "orientation_w2 => my_mapping_proxy%fs_from%get_cell_orientation("
        "cell)" in gen_str)
    assert (
        "orientation_w1 => my_mapping_proxy%fs_to%get_cell_orientation(cell)"
        in gen_str)
    assert (
        "(cell, nlayers, my_mapping_proxy%ncell_3d, my_mapping_proxy%local_"
        "stencil, chi_proxy(1)%data, chi_proxy(2)%data, chi_proxy(3)%data, "
        "ndf_w1, basis_w1, orientation_w1, ndf_w2, orientation_w2, ndf_w0, "
        "undf_w0, map_w0, diff_basis_w0, nqp_h, nqp_v, wh, wv)" in gen_str)


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
        "CALL testkern_any_space_1_code(nlayers, a_proxy%data, rdt, b_proxy%"
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
    generated_code = str(psy.gen)
    print generated_code
    assert "INTEGER, intent(inout) :: istp" in generated_code
    assert generated_code.find(
        "INTEGER, pointer :: map_any_space_1(:) => null()") != -1
    assert generated_code.find(
        "INTEGER ndf_any_space_1, undf_any_space_1") != -1
    assert generated_code.find(
        "ndf_any_space_1 = a_proxy%vspace%get_ndf()") != -1
    assert generated_code.find(
        "undf_any_space_1 = a_proxy%vspace%get_undf()") != -1
    assert generated_code.find(
        "map_any_space_1 => a_proxy%vspace%get_cell_dofmap(cell)") != -1
    assert generated_code.find(
        "CALL testkern_any_space_2_code(cell, nlayers, a_proxy%data, b_pro"
        "xy%data, c_proxy%ncell_3d, c_proxy%local_stencil, istp, "
        "ndf_any_space_1, undf_any_space_1, map_any_space_1)") != -1


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


def test_dyninvoke_uniq_declns():
    ''' tests that we raise an error when DynInvoke.unique_declarations() is
    called for an invalid type '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    with pytest.raises(GenerationError) as excinfo:
        psy.invokes.invoke_list[0].unique_declarations("not_a_type")
    assert 'unique_declarations called with an invalid datatype' \
        in str(excinfo.value)


def test_dyninvoke_arg_for_fs():
    ''' tests that we raise an error when DynInvoke.arg_for_funcspace() is
    called for an un-used space '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_2scalar.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    with pytest.raises(GenerationError) as excinfo:
        psy.invokes.invoke_list[0].arg_for_funcspace("wtheta")
    assert 'No argument found on wtheta space' \
        in str(excinfo.value)


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
    output1 = "SUBROUTINE invoke_0(a, f1, f2, m1, m2)"
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
    these is the case. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.5_multikernel_invokes.f90"),
                           api="dynamo0.3")
    with pytest.raises(GenerationError) as excinfo:
        _ = PSyFactory("dynamo0.3").create(invoke_info)
    print str(excinfo.value)
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

SIMPLE_WITH_SCALARS = (
    "  MODULE simple_with_scalars_mod\n"
    "    IMPLICIT NONE\n"
    "    CONTAINS\n"
    "    SUBROUTINE simple_with_scalars_code(nlayers, rscalar_1, field_2_w1, "
    "iscalar_3, ndf_w1, undf_w1, map_w1)\n"
    "      USE constants_mod, ONLY: r_def\n"
    "      IMPLICIT NONE\n"
    "      INTEGER, intent(in) :: nlayers\n"
    "      REAL(KIND=r_def), intent(in) :: rscalar_1\n"
    "      INTEGER, intent(in) :: undf_w1\n"
    "      REAL(KIND=r_def), intent(out), dimension(undf_w1) ::"
    " field_2_w1\n"
    "      INTEGER, intent(in) :: iscalar_3\n"
    "      INTEGER, intent(in) :: ndf_w1\n"
    "      INTEGER, intent(in), dimension(ndf_w1) :: map_w1\n"
    "    END SUBROUTINE simple_with_scalars_code\n"
    "  END MODULE simple_with_scalars_mod")


def test_stub_generate_with_scalars():
    ''' check that the stub generate produces the expected output when
    the kernel has scalar arguments '''
    result = generate("test_files/dynamo0p3/simple_with_scalars.f90",
                      api="dynamo0.3")
    print result
    assert str(result).find(SIMPLE_WITH_SCALARS) != -1

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


def test_load_meta_wrong_type():
    ''' Test that the load_meta function raises an appropriate error
    if the meta-data contains an un-recognised type '''
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(INTENT, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    # Break the meta-data
    metadata.arg_descriptors[0]._type = "gh_hedge"
    with pytest.raises(GenerationError) as excinfo:
        kernel.load_meta(metadata)
    assert "load_meta expected one of '['gh_field'," in str(excinfo.value)


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

STENCIL_CODE = '''
module stencil_mod
  type, extends(kernel_type) :: stencil_type
     type(arg_type), meta_args(2) =    &
          (/ arg_type(gh_field,gh_write,w1), &
             arg_type(gh_field,gh_read, w2, stencil(cross,1)) &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => stencil_code
  end type stencil_type
contains
  subroutine stencil_code()
  end subroutine stencil_code
end module stencil_mod
'''


@pytest.mark.xfail(reason="stencils not yet supported")
def test_stencil_metadata():
    ''' Check that we can parse Kernels with stencil metadata '''
    ast = fpapi.parse(STENCIL_CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    stencil_descriptor_0 = metadata.arg_descriptors[0]
    assert stencil_descriptor_0.stencil is None
    stencil_descriptor_1 = metadata.arg_descriptors[1]
    assert stencil_descriptor_1.stencil['type'] == 'cross'
    assert stencil_descriptor_1.stencil['extent'] == 1


def test_field_metadata_too_many_arguments():
    '''Check that we raise an exception if more than 4 arguments are
    provided in the metadata for a gh_field arg_type.'''
    result = STENCIL_CODE.replace(
        "gh_field,gh_read, w2, stencil(cross,1)",
        "gh_field,gh_read, w2, stencil(cross,1), w1", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast)
    assert "each meta_arg entry must have at most 4 arguments" \
        in str(excinfo.value)


def test_invalid_stencil_form_1():
    '''Check that we raise an exception if the stencil does not obey the
    stencil(<type>,<extent) format by being a literal integer'''
    result = STENCIL_CODE.replace("stencil(cross,1)", "1", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast)
    assert "entry must be a valid stencil specification" \
        in str(excinfo.value)
    assert "but found the literal" \
        in str(excinfo.value)


def test_invalid_stencil_form_2():
    '''Check that we raise an exception if the stencil does not obey the
    stencil(<type>,<extent) format by having an invalid name'''
    result = STENCIL_CODE.replace("stencil(cross,1)", "stenci(cross,1)", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast)
    assert "entry must be a valid stencil specification" \
        in str(excinfo.value)


def test_invalid_stencil_form_3():
    '''Check that we raise an exception if the stencil does not obey the
    stencil(<type>,<extent) format by not having brackets'''
    result = STENCIL_CODE.replace("stencil(cross,1)", "stencil", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast)
    assert "entry must be a valid stencil specification" \
        in str(excinfo.value)


def test_invalid_stencil_form_4():
    '''Check that we raise an exception if the stencil does not obey the
    stencil(<type>,<extent) format by not containing two values in the
    brackets '''
    result = STENCIL_CODE.replace("stencil(cross,1)", "stencil(cross)", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast)
    assert "entry must be a valid stencil specification" \
        in str(excinfo.value)
    assert "there are not two arguments inside the brackets" \
        in str(excinfo.value)


def test_invalid_stencil_first_arg_1():
    '''Check that we raise an exception if the value of the stencil type in
    stencil(<type>,<extent) is not valid and is an integer'''
    result = STENCIL_CODE.replace("stencil(cross,1)", "stencil(1,1)", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast)
    assert "not one of the valid types" in str(excinfo.value)
    assert "is a literal" in str(excinfo.value)


def test_invalid_stencil_first_arg_2():
    '''Check that we raise an exception if the value of the stencil type in
    stencil(<type>,<extent) is not valid and is a name'''
    result = STENCIL_CODE.replace("stencil(cross,1)", "stencil(cros,1)", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast)
    assert "not one of the valid types" in str(excinfo.value)


def test_invalid_stencil_first_arg_3():
    '''Check that we raise an exception if the value of the stencil type in
    stencil(<type>,<extent) is not valid and has brackets'''
    result = STENCIL_CODE.replace("stencil(cross,1)", "stencil(x1d(xx),1)", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast)
    assert "the specified <type>" in str(excinfo.value)
    assert "includes brackets" in str(excinfo.value)


def test_invalid_stencil_second_arg_1():
    '''Check that we raise an exception if the value of the stencil extent in
    stencil(<type>,<extent) is not an integer'''
    result = STENCIL_CODE.replace("stencil(cross,1)", "stencil(x1d,x1d)", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast)
    assert "the specified <extent>" in str(excinfo.value)
    assert "is not an integer" in str(excinfo.value)


def test_invalid_stencil_second_arg_2():
    '''Check that we raise an exception if the value of the stencil extent in
    stencil(<type>,<extent) is less than 1'''
    result = STENCIL_CODE.replace("stencil(cross,1)", "stencil(x1d,0)", 1)
    ast = fpapi.parse(result, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast)
    assert "the specified <extent>" in str(excinfo.value)
    assert "is less than 1" in str(excinfo.value)


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
    first_argument = first_kernel.arguments.args[1]
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


def test_arg_descriptor_fld_str():
    ''' Tests that the string method for DynArgDescriptor03 works as
    expected for a field argument'''
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    field_descriptor = metadata.arg_descriptors[1]
    result = str(field_descriptor)
    print result
    expected_output = (
        "DynArgDescriptor03 object\n"
        "  argument_type[0]='gh_field'\n"
        "  access_descriptor[1]='gh_write'\n"
        "  function_space[2]='w1'")
    assert expected_output in result


def test_arg_descriptor_scalar_str():
    ''' Tests that the string method for DynArgDescriptor03 works as
    expected for a scalar argument'''
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(CODE, ignore_comments=False)
    metadata = DynKernMetadata(ast, name="testkern_qr_type")
    field_descriptor = metadata.arg_descriptors[0]
    result = str(field_descriptor)
    print result
    expected_output = (
        "DynArgDescriptor03 object\n"
        "  argument_type[0]='gh_rscalar'\n"
        "  access_descriptor[1]='gh_read'\n")
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
    print result
    assert 'DynArgDescriptor03(arg_type(gh_rscalar, gh_read))' \
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


def test_dist_memory_true():
    ''' test that the distributed memory flag is on by default '''
    import config
    assert config.DISTRIBUTED_MEMORY


def test_halo_dirty_1():
    ''' check halo_dirty call is added correctly with a simple example '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    expected = (
        "     END DO \n"
        "      !\n"
        "      ! Set halos dirty for fields modified in the above loop\n"
        "      !\n"
        "      CALL f1_proxy%set_dirty()\n")
    assert expected in generated_code


def test_halo_dirty_2():
    ''' check halo_dirty calls only for write and inc (not for read) '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "14.1_halo_writers.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    expected = (
        "      END DO \n"
        "      !\n"
        "      ! Set halos dirty for fields modified in the above loop\n"
        "      !\n"
        "      CALL f1_proxy%set_dirty()\n"
        "      CALL f3_proxy%set_dirty()\n"
        "      CALL f5_proxy%set_dirty()\n"
        "      CALL f6_proxy%set_dirty()\n"
        "      CALL f7_proxy%set_dirty()\n"
        "      CALL f8_proxy%set_dirty()\n")

    assert expected in generated_code


def test_halo_dirty_3():
    ''' check halo_dirty calls with multiple kernel calls '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = psy.gen
    print generated_code
    assert str(generated_code).count("CALL f1_proxy%set_dirty()") == 2


def test_halo_dirty_4():
    ''' check halo_dirty calls with field vectors '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "8_vector_field_2.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    expected = (
        "      END DO \n"
        "      !\n"
        "      ! Set halos dirty for fields modified in the above loop\n"
        "      !\n"
        "      CALL chi_proxy(1)%set_dirty()\n"
        "      CALL chi_proxy(2)%set_dirty()\n"
        "      CALL chi_proxy(3)%set_dirty()\n"
        "      CALL f1_proxy%set_dirty()\n")
    assert expected in generated_code


def test_halo_dirty_5():
    ''' check no halo_dirty calls for operators '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "10.1_operator_nofield.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    assert "set_dirty()" not in generated_code
    assert "! Set halos dirty" not in generated_code


def test_no_halo_dirty():
    '''check that no halo_dirty code is produced if distributed_memory is
    set to False'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    assert "set_dirty()" not in generated_code
    assert "! Set halos dirty" not in generated_code


@pytest.mark.xfail(reason="stencils not yet supported")
def test_halo_exchange():
    ''' test that a halo_exchange call is added for a loop with a
    stencil operation '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "14.2_halo_readers.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code
    output = (
        "     IF (f2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      DO cell=1,mesh%get_last_halo_cell(1)\n")
    assert output in generated_code


def test_halo_exchange_inc():
    ''' test that halo exchange calls are added if we have a gh_inc
    operation and that the loop bounds included computation in the l1
    halo'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.6_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    result = str(psy.gen)
    print result
    output1 = ("      IF (a_proxy%is_dirty(depth=1)) THEN\n"
               "        CALL a_proxy%halo_exchange(depth=1)\n"
               "      END IF \n"
               "      !\n"
               "      DO cell=1,mesh%get_last_halo_cell(1)\n")
    assert output1 in result
    output2 = ("      IF (f_proxy%is_dirty(depth=1)) THEN\n"
               "        CALL f_proxy%halo_exchange(depth=1)\n"
               "      END IF \n"
               "      !\n"
               "      DO cell=1,mesh%get_last_halo_cell(1)\n")
    assert output2 in result
    assert result.count("halo_exchange") == 2


@pytest.mark.xfail(reason="stencils not yet supported")
def test_halo_exchange_different_spaces():
    '''test that all of our different function spaces with a stencil
    access result in halo calls including any_space'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "14.3_halo_readers_all_fs.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    result = str(psy.gen)
    print result
    assert result.count("halo_exchange") == 9


@pytest.mark.xfail(reason="stencils not yet supported")
def test_halo_exchange_vectors():
    ''' test that halo exchange produces correct code for vector
    fields. Test both a field with a stencil and a field with gh_inc '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "14.4_halo_vector.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    result = str(psy.gen)
    print result
    assert result.count("halo_exchange(") == 7
    for idx in range(1, 4):
        assert "f1_proxy("+str(idx)+")%halo_exchange(depth=1)" in result
        assert "f2_proxy("+str(idx)+")%halo_exchange(depth=2)" in result
    expected = ("      IF (f2_proxy%is_dirty(depth=2)) THEN\n"
                "        CALL f2_proxy(4)%halo_exchange(depth=2)\n"
                "      END IF \n"
                "      !\n"
                "      DO cell=1,mesh%get_last_halo_cell(1)\n")
    assert expected in result


@pytest.mark.xfail(reason="stencils not yet supported")
def test_halo_exchange_depths():
    ''' test that halo exchange (and gh_inc) includes the correct halo
    depth with gh_write '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "14.5_halo_depth.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    result = str(psy.gen)
    print result
    expected = ("      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
                "        CALL f2_proxy%halo_exchange(depth=1)\n"
                "      END IF \n"
                "      !\n"
                "      IF (f3_proxy%is_dirty(depth=2)) THEN\n"
                "        CALL f3_proxy%halo_exchange(depth=2)\n"
                "      END IF \n"
                "      !\n"
                "      IF (f4_proxy%is_dirty(depth=3)) THEN\n"
                "        CALL f4_proxy%halo_exchange(depth=3)\n"
                "      END IF \n"
                "      !\n"
                "      DO cell=1,mesh%get_last_edge_cell()\n")
    assert expected in result


@pytest.mark.xfail(reason="stencils not yet supported")
def test_halo_exchange_depths_gh_inc():
    ''' test that halo exchange includes the correct halo depth when
    we have a gh_inc as this increases the required depth by 1 (as
    redundant computation is performed in the l1 halo) '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "14.6_halo_depth_2.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    result = str(psy.gen)
    print result
    expected = ("      IF (f1_proxy%is_dirty(depth=1)) THEN\n"
                "        CALL f1_proxy%halo_exchange(depth=1)\n"
                "      END IF \n"
                "      !\n"
                "      IF (f2_proxy%is_dirty(depth=2)) THEN\n"
                "        CALL f2_proxy%halo_exchange(depth=2)\n"
                "      END IF \n"
                "      !\n"
                "      IF (f3_proxy%is_dirty(depth=3)) THEN\n"
                "        CALL f3_proxy%halo_exchange(depth=3)\n"
                "      END IF \n"
                "      !\n"
                "      IF (f4_proxy%is_dirty(depth=4)) THEN\n"
                "        CALL f4_proxy%halo_exchange(depth=4)\n"
                "      END IF \n"
                "      !\n"
                "      DO cell=1,mesh%get_last_halo_cell(1)\n")
    assert expected in result


@pytest.mark.xfail(reason="stencils not yet supported")
def test_stencil_read_only():
    '''test that an error is raised if a field with a stencil is not
    accessed as gh_read'''
    fparser.logging.disable('CRITICAL')
    code = STENCIL_CODE.replace("gh_read, w2, stencil(cross,1)",
                                "gh_write, w2, stencil(cross,1)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name="stencil_type")
    assert "a stencil must be read only" in str(excinfo.value)


# def test_halo_exchange_conflicting_stencil(): '''
# two different stencils for same space in a kernel *** and gh_inc '''
# only an issue when we have more than one kernel per loop i.e. we
# need loop fusion. Therefore should go in dynamo0p3_transformations.py


def test_w3_and_inc_error():
    '''test that an error is raised if w3 and gh_inc are provided for the
    same field in the metadata '''
    fparser.logging.disable('CRITICAL')
    code = CODE.replace("arg_type(gh_field,gh_read, w3)",
                        "arg_type(gh_field,gh_inc, w3)", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name="testkern_qr_type")
    assert ("it does not make sense for a 'w3' space to have a 'gh_inc' "
            "access") in str(excinfo.value)


@pytest.mark.xfail(reason="stencils not yet supported")
def test_halo_exchange_view(capsys):
    ''' test that the halo exchange view method returns what we expect '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "14.2_halo_readers.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    schedule = psy.invokes.get('invoke_0_testkern_stencil_type').schedule
    schedule.view()
    result, _ = capsys.readouterr()
    expected = (
        "Schedule[invoke='invoke_0_testkern_stencil_type']\n"
        "    HaloExchange[field='f2', type='cross', depth=1, "
        "check_dirty=True]\n"
        "    Loop[type='',field_space='w1',it_space='cells']\n"
        "        KernCall testkern_stencil_code(f1,f2,f3,f4) "
        "[module_inline=False]")
    print expected
    print result
    assert expected in result


def test_no_mesh_mod():
    '''test that we do not add a mesh module to the PSy layer if one is
    not required. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.6_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    result = str(psy.gen)
    print result
    assert "USE mesh_mod, ONLY: mesh_type" not in result
    assert "TYPE(mesh_type) mesh" not in result
    assert "mesh = a%get_mesh()" not in result


def test_mesh_mod():
    '''test that a mesh module is added to the PSy layer and a mesh object
    is created when required. One is required when we determine loop
    bounds for distributed memory '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.6_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    result = str(psy.gen)
    print result
    assert "USE mesh_mod, ONLY: mesh_type" in result
    assert "TYPE(mesh_type) mesh" in result
    output = ("      !\n"
              "      ! Create a mesh object\n"
              "      !\n"
              "      mesh = a%get_mesh()\n")
    assert output in result

# when we add build tests we should test that we can we get the mesh
# object from an operator


def test_no_dm_and_colour():
    '''test that we raise an exception if colouring and distributed
    memory are attempted together, as there are a few bugs and there is
    currently no agreed API for the colouring'''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(info)
    invoke = psy.invokes.get('invoke_0_testkern_type')
    schedule = invoke.schedule
    ctrans = ColourTrans()
    with pytest.raises(GenerationError) as excinfo:
        # try to Colour the loop
        _, _ = ctrans.apply(schedule.children[0])
    assert 'distributed memory and colours not yet supported' in \
        str(excinfo.value)


def test_no_stencil_support():
    '''test that we raise an exception if we encounter a stencil kernel
    as the infrastructure API for this is not yet decided '''
    ast = fpapi.parse(STENCIL_CODE, ignore_comments=False)
    with pytest.raises(GenerationError) as excinfo:
        _ = DynKernMetadata(ast)
    assert 'not supported in PSyclone' in str(excinfo.value)


def test_set_bounds_functions():
    '''test that we raise appropriate exceptions when the lower bound of
    a loop is set to an invalid value '''
    my_loop = DynLoop()
    with pytest.raises(GenerationError) as excinfo:
        my_loop.set_lower_bound("invalid_loop_bounds_name")
    assert "lower bound loop name is invalid" in str(excinfo.value)
    with pytest.raises(GenerationError) as excinfo:
        my_loop.set_lower_bound("inner", index=0)
    assert "specified index" in str(excinfo.value)
    assert "lower loop bound is invalid" in str(excinfo.value)
    with pytest.raises(GenerationError) as excinfo:
        my_loop.set_upper_bound("invalid_loop_bounds_name")
    assert "upper bound loop name is invalid" in str(excinfo.value)
    with pytest.raises(GenerationError) as excinfo:
        my_loop.set_upper_bound("start")
    assert "'start' is not a valid upper bound" in str(excinfo.value)
    with pytest.raises(GenerationError) as excinfo:
        my_loop.set_upper_bound("inner", index=0)
    assert "specified index" in str(excinfo.value)
    assert "upper loop bound is invalid" in str(excinfo.value)


def test_lower_bound_fortran():
    '''tests we raise an exception in the DynLoop:_lower_bound_fortran()
    method'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    my_loop = psy.invokes.invoke_list[0].schedule.children[0]
    my_loop.set_lower_bound("inner", index=1)
    with pytest.raises(GenerationError) as excinfo:
        _ = my_loop._lower_bound_fortran()
    assert ("lower bound must be 'start' if we are sequential" in
            str(excinfo.value))
    my_loop.set_upper_bound("halo", index=1)
    with pytest.raises(GenerationError) as excinfo:
        _ = my_loop._upper_bound_fortran()
    assert ("upper bound must be 'cells' if we are sequential" in
            str(excinfo.value))


def test_multi_field_name_halo():
    '''tests the case where we have multiple kernels within an invoke and
    the same field requires clean halos in more than one Kernel. In
    this case we raise an error as we don't expect this case to happen. See
    ticket 420 for more info.'''
    # parse an example where halo exchanges are needed for each loop
    # and the variable name is the samefor the Kernel in each
    # loop. Don't use distributed memory as this will place halo's and
    # we want to loop fuse without worrying about that.
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.6.2_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    psy.invokes.invoke_list[0].schedule.view()
    invoke = psy.invokes.invoke_list[0]
    # Loop fuse so two Kernels requiring halo exchange calls are in the
    # same loop
    loop1 = invoke.schedule.children[0]
    loop2 = invoke.schedule.children[1]
    trans = LoopFuseTrans()
    schedule, _ = trans.apply(loop1, loop2)
    invoke.schedule = schedule
    loop1 = schedule.children[0]
    # Now check the fused loop
    with pytest.raises(GenerationError) as excinfo:
        _ = loop1.unique_fields_with_halo_reads()
    assert "non-unique fields are not expected" in str(excinfo.value)
