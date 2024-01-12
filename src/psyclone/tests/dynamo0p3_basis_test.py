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
# Authors: R. W. Ford, A. R. Porter and N. Nobre, STFC Daresbury Lab
# Modified: I. Kavcic and L. Turner, Met Office
# Modified: J. Henrichs, Bureau of Meteorology

''' Module containing py.test tests for functionality related to
evaluators in the LFRic API '''

import os
import pytest
import fparser
from fparser import api as fpapi
from psyclone.configuration import Config
from psyclone.domain.lfric import LFRicConstants, LFRicKern, LFRicKernMetadata
from psyclone.dynamo0p3 import DynBasisFunctions
from psyclone.f2pygen import ModuleGen
from psyclone.parse.algorithm import parse
from psyclone.parse.utils import ParseError
from psyclone.psyGen import PSyFactory
from psyclone.errors import GenerationError, InternalError
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.utilities import print_diffs


# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")

API = "dynamo0.3"

CODE = '''
module testkern_eval
  type, extends(kernel_type) :: testkern_eval_type
    type(arg_type) :: meta_args(2) = (/            &
         arg_type(GH_FIELD, GH_REAL, GH_INC,  W0), &
         arg_type(GH_FIELD, GH_REAL, GH_READ, W1)  &
         /)
    type(func_type) :: meta_funcs(2) = (/          &
         func_type(W0, GH_BASIS),                  &
         func_type(W1, GH_DIFF_BASIS)              &
         /)
    integer :: gh_shape = gh_evaluator
    integer :: gh_evaluator_targets(2) = [W0, W1]
    integer :: operates_on = cell_column
  contains
    procedure, nopass :: code => testkern_eval_code
  end type testkern_eval_type
contains
  subroutine testkern_eval_code()
  end subroutine testkern_eval_code
end module testkern_eval
'''


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use dynamo0.3 as API.'''
    Config.get().api = "dynamo0.3"
    yield
    Config._instance = None


def test_eval_mdata():
    ''' Check that we recognise "evaluator" as a valid gh_shape '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    ast = fpapi.parse(CODE, ignore_comments=False)
    dkm = LFRicKernMetadata(ast, name="testkern_eval_type")
    assert dkm.get_integer_variable('gh_shape') == 'gh_evaluator'


def test_multi_updated_arg():
    ''' Check that we handle any kernel requiring an evaluator
    if it writes to more than one argument. (This used to be rejected.) '''
    fparser.logging.disable(fparser.logging.CRITICAL)
    # Change the access of the read-only argument
    code = CODE.replace("GH_READ", "GH_INC", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    dkm = LFRicKernMetadata(ast, name="testkern_eval_type")
    # Evaluator targets list remains unchanged
    assert dkm._eval_targets == ['w0', 'w1']
    # Change the gh_shape element to specify quadrature and then test again
    qr_code = code.replace("gh_evaluator", "gh_quadrature_xyoz")
    ast = fpapi.parse(qr_code, ignore_comments=False)
    dkm = LFRicKernMetadata(ast, name="testkern_eval_type")
    assert dkm.get_integer_variable('gh_shape') == "gh_quadrature_xyoz"


def test_eval_targets():
    ''' Check that we can specify multiple evaluator targets using
    the 'gh_evaluator_targets' metadata entry. '''
    ast = fpapi.parse(CODE, ignore_comments=False)
    dkm = LFRicKernMetadata(ast, name="testkern_eval_type")
    assert dkm._eval_targets == ["w0", "w1"]


def test_eval_targets_err():
    ''' Check that needlessly specifying 'gh_evaluator_targets' raises the
    expected errors. '''
    # When the shape is gh_quadrature_* instead of gh_evaluator
    code = CODE.replace("gh_evaluator\n", "gh_quadrature_xyoz\n")
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as err:
        _ = LFRicKernMetadata(ast, name="testkern_eval_type")
    assert ("specifies 'gh_evaluator_targets' (['w0', 'w1']) but does not "
            "need an evaluator because gh_shape=['gh_quadrature_xyoz']"
            in str(err.value))
    # When there are no basis/diff-basis functions required
    code = CODE.replace(
        "    type(func_type) :: meta_funcs(2) = (/          &\n"
        "         func_type(W0, GH_BASIS),                  &\n"
        "         func_type(W1, GH_DIFF_BASIS)              &\n"
        "         /)\n", "")
    code = code.replace("    integer :: gh_shape = gh_evaluator\n",
                        "")
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as err:
        _ = LFRicKernMetadata(ast, name="testkern_eval_type")
    assert ("specifies 'gh_evaluator_targets' (['w0', 'w1']) but does not "
            "need an evaluator because no basis or differential basis "
            "functions are required" in str(err.value))


def test_eval_targets_wrong_space():
    ''' Check that we reject metadata where there is no argument for one of
    the function spaces listed in gh_evaluator_targets. '''
    code = CODE.replace("[W0, W1]", "[W0, W3]")
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as err:
        _ = LFRicKernMetadata(ast, name="testkern_eval_type")
    assert ("specifies that an evaluator is required on 'w3' but does not "
            "have an argument on this space" in str(err.value))


def test_eval_targets_op_space():
    ''' Check that listing a space associated with an operator in
    gh_evaluator_targets works OK. '''
    opstring = "    arg_type(GH_OPERATOR, GH_REAL, GH_READ, W2, W1), &"
    code = CODE.split("\n")
    code.insert(5, opstring)
    code = "\n".join(code).replace("meta_args(2)", "meta_args(3)")
    code = code.replace("[W0, W1]", "[W0, W3]")
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as err:
        _ = LFRicKernMetadata(ast, name="testkern_eval_type")
    assert ("specifies that an evaluator is required on 'w3' but does not "
            "have an argument on this space" in str(err.value))
    # Change to a space that is referenced by an operator
    code = code.replace("[W0, W3]", "[W0, W2]")
    ast = fpapi.parse(code, ignore_comments=False)
    dkm = LFRicKernMetadata(ast, name="testkern_eval_type")
    assert isinstance(dkm, LFRicKernMetadata)


def test_single_kern_eval(tmpdir):
    ''' Check that we generate correct code for a single kernel that
    requires both basis and differential basis functions for an
    evaluator. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "6.1_eval_invoke.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    gen_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Check module declarations
    expected_module_declns = (
        "    USE constants_mod, ONLY: r_def, i_def\n"
        "    USE field_mod, ONLY: field_type, field_proxy_type\n")
    assert expected_module_declns in gen_code

    # Check subroutine declarations
    expected_decl = (
        "    SUBROUTINE invoke_0_testkern_eval_type(f0, cmap)\n"
        "      USE testkern_eval_mod, ONLY: testkern_eval_code\n"
        "      USE function_space_mod, ONLY: BASIS, DIFF_BASIS\n"
        "      TYPE(field_type), intent(in) :: f0, cmap\n"
        "      INTEGER(KIND=i_def) cell\n"
        "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
        "      INTEGER(KIND=i_def) df_nodal, df_w0, df_w1\n"
        "      REAL(KIND=r_def), allocatable :: basis_w0_on_w0(:,:,:), "
        "diff_basis_w1_on_w0(:,:,:)\n"
        "      INTEGER(KIND=i_def) dim_w0, diff_dim_w1\n"
        "      REAL(KIND=r_def), pointer :: nodes_w0(:,:) => null()\n"
        "      INTEGER(KIND=i_def) nlayers\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: "
        "cmap_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f0_data => null()\n"
        "      TYPE(field_proxy_type) f0_proxy, cmap_proxy\n"
        "      INTEGER(KIND=i_def), pointer :: map_w0(:,:) => null(), "
        "map_w1(:,:) => null()\n"
        "      INTEGER(KIND=i_def) ndf_w0, undf_w0, ndf_w1, undf_w1\n")
    assert expected_decl in gen_code
    # Second, check the executable statements
    expected_code = (
        "      !\n"
        "      ! Initialise field and/or operator proxies\n"
        "      !\n"
        "      f0_proxy = f0%get_proxy()\n"
        "      f0_data => f0_proxy%data\n"
        "      cmap_proxy = cmap%get_proxy()\n"
        "      cmap_data => cmap_proxy%data\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f0_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w0 => f0_proxy%vspace%get_whole_dofmap()\n"
        "      map_w1 => cmap_proxy%vspace%get_whole_dofmap()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w0\n"
        "      !\n"
        "      ndf_w0 = f0_proxy%vspace%get_ndf()\n"
        "      undf_w0 = f0_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w1\n"
        "      !\n"
        "      ndf_w1 = cmap_proxy%vspace%get_ndf()\n"
        "      undf_w1 = cmap_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise evaluator-related quantities for the target "
        "function spaces\n"
        "      !\n"
        "      nodes_w0 => f0_proxy%vspace%get_nodes()\n"
        "      !\n"
        "      ! Allocate basis/diff-basis arrays\n"
        "      !\n"
        "      dim_w0 = f0_proxy%vspace%get_dim_space()\n"
        "      diff_dim_w1 = cmap_proxy%vspace%get_dim_space_diff()\n"
        "      ALLOCATE (basis_w0_on_w0(dim_w0, ndf_w0, ndf_w0))\n"
        "      ALLOCATE (diff_basis_w1_on_w0(diff_dim_w1, ndf_w1, ndf_w0))\n"
        "      !\n"
        "      ! Compute basis/diff-basis arrays\n"
        "      !\n"
        "      DO df_nodal=1,ndf_w0\n"
        "        DO df_w0=1,ndf_w0\n"
        "          basis_w0_on_w0(:,df_w0,df_nodal) = "
        "f0_proxy%vspace%call_function(BASIS,df_w0,nodes_w0(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n"
        "      DO df_nodal=1,ndf_w0\n"
        "        DO df_w1=1,ndf_w1\n"
        "          diff_basis_w1_on_w0(:,df_w1,df_nodal) = cmap_proxy%vspace%"
        "call_function(DIFF_BASIS,df_w1,nodes_w0(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n"
        "      !\n"
        "      ! Set-up all of the loop bounds\n"
        "      !\n"
        "      loop0_start = 1\n"
        "      loop0_stop = f0_proxy%vspace%get_ncell()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO cell=loop0_start,loop0_stop\n"
        "        !\n"
        "        CALL testkern_eval_code(nlayers, f0_data, "
        "cmap_data, ndf_w0, undf_w0, map_w0(:,cell), basis_w0_on_w0, "
        "ndf_w1, undf_w1, map_w1(:,cell), diff_basis_w1_on_w0)\n"
        "      END DO\n"
        "      !\n"
    )
    assert expected_code in gen_code
    dealloc_code = (
        "      DEALLOCATE (basis_w0_on_w0, diff_basis_w1_on_w0)\n"
        "      !\n"
        "    END SUBROUTINE invoke_0_testkern_eval_type\n"
    )
    assert dealloc_code in gen_code


def test_single_kern_eval_op(tmpdir):
    ''' Check that we generate correct code for a single kernel which
    writes to an operator and requires both basis and differential basis
    functions for an evaluator '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "6.1.1_eval_op_invoke.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    gen_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Kernel writes to an operator, the 'to' space of which is W0. Kernel
    # requires basis on W2 ('from'-space of operator) and diff-basis on
    # W3 (space of the field).
    decln_output = (
        "      USE function_space_mod, ONLY: BASIS, DIFF_BASIS\n"
        "      TYPE(field_type), intent(in) :: f1\n"
        "      TYPE(operator_type), intent(in) :: op1\n"
        "      INTEGER(KIND=i_def) cell\n"
        "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
        "      INTEGER(KIND=i_def) df_nodal, df_w2, df_w3\n"
        "      REAL(KIND=r_def), allocatable :: basis_w2_on_w0(:,:,:), "
        "diff_basis_w3_on_w0(:,:,:)\n"
        "      INTEGER(KIND=i_def) dim_w2, diff_dim_w3\n"
        "      REAL(KIND=r_def), pointer :: nodes_w0(:,:) => null()\n"
        "      INTEGER(KIND=i_def) nlayers\n"
        "      REAL(KIND=r_def), pointer, dimension(:,:,:) :: "
        "op1_local_stencil => null()\n"
        "      TYPE(operator_proxy_type) op1_proxy\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f1_data => null()\n"
        "      TYPE(field_proxy_type) f1_proxy\n"
        "      INTEGER(KIND=i_def), pointer :: map_w3(:,:) => null()\n"
        "      INTEGER(KIND=i_def) ndf_w0, ndf_w2, ndf_w3, undf_w3\n")
    assert decln_output in gen_code
    init_output = (
        "      nodes_w0 => op1_proxy%fs_to%get_nodes()\n"
        "      !\n"
        "      ! Allocate basis/diff-basis arrays\n"
        "      !\n"
        "      dim_w2 = op1_proxy%fs_from%get_dim_space()\n"
        "      diff_dim_w3 = f1_proxy%vspace%get_dim_space_diff()\n"
        "      ALLOCATE (basis_w2_on_w0(dim_w2, ndf_w2, ndf_w0))\n"
        "      ALLOCATE (diff_basis_w3_on_w0(diff_dim_w3, ndf_w3, ndf_w0))\n"
        "      !\n"
        "      ! Compute basis/diff-basis arrays\n"
        "      !\n"
        "      DO df_nodal=1,ndf_w0\n"
        "        DO df_w2=1,ndf_w2\n"
        "          basis_w2_on_w0(:,df_w2,df_nodal) = op1_proxy%fs_from%"
        "call_function(BASIS,df_w2,nodes_w0(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n"
        "      DO df_nodal=1,ndf_w0\n"
        "        DO df_w3=1,ndf_w3\n"
        "          diff_basis_w3_on_w0(:,df_w3,df_nodal) = f1_proxy%vspace%"
        "call_function(DIFF_BASIS,df_w3,nodes_w0(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n"
    )
    assert init_output in gen_code
    assert "loop0_stop = op1_proxy%fs_from%get_ncell()\n" in gen_code
    kern_call = (
        "      DO cell=loop0_start,loop0_stop\n"
        "        !\n"
        "        CALL testkern_eval_op_code(cell, nlayers, op1_proxy%ncell_3d,"
        " op1_local_stencil, f1_data, ndf_w0, ndf_w2, "
        "basis_w2_on_w0, ndf_w3, undf_w3, map_w3(:,cell), "
        "diff_basis_w3_on_w0)\n"
        "      END DO\n")
    assert kern_call in gen_code
    dealloc = ("      DEALLOCATE (basis_w2_on_w0, diff_basis_w3_on_w0)\n")
    assert dealloc in gen_code


def test_two_qr_same_shape(tmpdir):
    ''' Check that we handle an invoke containing two kernels that each
    require quadrature (with the same shape). '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.1.2_single_invoke_2qr.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    gen_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    expected_module_declns = (
        "    USE constants_mod, ONLY: r_def, i_def\n"
        "    USE field_mod, ONLY: field_type, field_proxy_type\n")
    assert expected_module_declns in gen_code

    expected_declns = (
        "    SUBROUTINE invoke_0(f1, f2, m1, a, m2, istp, g1, g2, n1, b, "
        "n2, qr, qr2)\n"
        "      USE testkern_qr_mod, ONLY: testkern_qr_code\n"
        "      USE quadrature_xyoz_mod, ONLY: quadrature_xyoz_type, "
        "quadrature_xyoz_proxy_type\n"
        "      USE function_space_mod, ONLY: BASIS, DIFF_BASIS\n"
        "      REAL(KIND=r_def), intent(in) :: a, b\n"
        "      INTEGER(KIND=i_def), intent(in) :: istp\n"
        "      TYPE(field_type), intent(in) :: f1, f2, m1, m2, g1, g2, "
        "n1, n2\n"
        "      TYPE(quadrature_xyoz_type), intent(in) :: qr, qr2\n"
        "      INTEGER(KIND=i_def) cell\n"
        "      INTEGER(KIND=i_def) loop1_start, loop1_stop\n"
        "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
        "      REAL(KIND=r_def), allocatable :: basis_w1_qr(:,:,:,:), "
        "diff_basis_w2_qr(:,:,:,:), basis_w3_qr(:,:,:,:), "
        "diff_basis_w3_qr(:,:,:,:), basis_w1_qr2(:,:,:,:), "
        "diff_basis_w2_qr2(:,:,:,:), basis_w3_qr2(:,:,:,:), "
        "diff_basis_w3_qr2(:,:,:,:)\n"
        "      INTEGER(KIND=i_def) dim_w1, diff_dim_w2, dim_w3, diff_dim_w3\n"
        "      REAL(KIND=r_def), pointer :: weights_xy_qr2(:) => null(), "
        "weights_z_qr2(:) => null()\n"
        "      INTEGER(KIND=i_def) np_xy_qr2, np_z_qr2\n"
        "      REAL(KIND=r_def), pointer :: weights_xy_qr(:) => null(), "
        "weights_z_qr(:) => null()\n"
        "      INTEGER(KIND=i_def) np_xy_qr, np_z_qr\n"
        "      INTEGER(KIND=i_def) nlayers\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: n2_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: n1_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: g2_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: g1_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: m2_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: m1_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f2_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f1_data => null()\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, "
        "m2_proxy, g1_proxy, g2_proxy, n1_proxy, n2_proxy\n"
        "      TYPE(quadrature_xyoz_proxy_type) qr_proxy, qr2_proxy\n"
        "      INTEGER(KIND=i_def), pointer :: map_w1(:,:) => null(), "
        "map_w2(:,:) => null(), map_w3(:,:) => null()\n"
        "      INTEGER(KIND=i_def) ndf_w1, undf_w1, ndf_w2, undf_w2, "
        "ndf_w3, undf_w3\n"
    )
    assert expected_declns in gen_code
    expected_code = (
        "      !\n"
        "      ! Look-up quadrature variables\n"
        "      !\n"
        "      qr_proxy = qr%get_quadrature_proxy()\n"
        "      np_xy_qr = qr_proxy%np_xy\n"
        "      np_z_qr = qr_proxy%np_z\n"
        "      weights_xy_qr => qr_proxy%weights_xy\n"
        "      weights_z_qr => qr_proxy%weights_z\n"
        "      qr2_proxy = qr2%get_quadrature_proxy()\n"
        "      np_xy_qr2 = qr2_proxy%np_xy\n"
        "      np_z_qr2 = qr2_proxy%np_z\n"
        "      weights_xy_qr2 => qr2_proxy%weights_xy\n"
        "      weights_z_qr2 => qr2_proxy%weights_z\n"
        "      !\n"
        "      ! Allocate basis/diff-basis arrays\n"
        "      !\n"
        "      dim_w1 = f1_proxy%vspace%get_dim_space()\n"
        "      diff_dim_w2 = f2_proxy%vspace%get_dim_space_diff()\n"
        "      dim_w3 = m2_proxy%vspace%get_dim_space()\n"
        "      diff_dim_w3 = m2_proxy%vspace%get_dim_space_diff()\n"
        "      ALLOCATE (basis_w1_qr(dim_w1, ndf_w1, np_xy_qr, np_z_qr))\n"
        "      ALLOCATE (diff_basis_w2_qr(diff_dim_w2, ndf_w2, np_xy_qr, "
        "np_z_qr))\n"
        "      ALLOCATE (basis_w3_qr(dim_w3, ndf_w3, np_xy_qr, np_z_qr))\n"
        "      ALLOCATE (diff_basis_w3_qr(diff_dim_w3, ndf_w3, np_xy_qr, "
        "np_z_qr))\n"
        "      ALLOCATE (basis_w1_qr2(dim_w1, ndf_w1, np_xy_qr2, np_z_qr2))\n"
        "      ALLOCATE (diff_basis_w2_qr2(diff_dim_w2, ndf_w2, np_xy_qr2, "
        "np_z_qr2))\n"
        "      ALLOCATE (basis_w3_qr2(dim_w3, ndf_w3, np_xy_qr2, np_z_qr2))\n"
        "      ALLOCATE (diff_basis_w3_qr2(diff_dim_w3, ndf_w3, np_xy_qr2, "
        "np_z_qr2))\n"
        "      !\n"
        "      ! Compute basis/diff-basis arrays\n"
        "      !\n"
        "      CALL qr%compute_function("
        "BASIS, f1_proxy%vspace, dim_w1, ndf_w1, basis_w1_qr)\n"
        "      CALL qr%compute_function(DIFF_BASIS, "
        "f2_proxy%vspace, diff_dim_w2, ndf_w2, diff_basis_w2_qr)\n"
        "      CALL qr%compute_function("
        "BASIS, m2_proxy%vspace, dim_w3, ndf_w3, basis_w3_qr)\n"
        "      CALL qr%compute_function(DIFF_BASIS, "
        "m2_proxy%vspace, diff_dim_w3, ndf_w3, diff_basis_w3_qr)\n"
        "      CALL qr2%compute_function("
        "BASIS, g1_proxy%vspace, dim_w1, ndf_w1, basis_w1_qr2)\n"
        "      CALL qr2%compute_function(DIFF_BASIS, "
        "g2_proxy%vspace, diff_dim_w2, ndf_w2, diff_basis_w2_qr2)\n"
        "      CALL qr2%compute_function("
        "BASIS, n2_proxy%vspace, dim_w3, ndf_w3, basis_w3_qr2)\n"
        "      CALL qr2%compute_function(DIFF_BASIS, "
        "n2_proxy%vspace, diff_dim_w3, ndf_w3, diff_basis_w3_qr2)\n"
        "      !\n")
    if expected_code not in gen_code:
        print_diffs(expected_code, gen_code)
        assert 0
    assert ("      loop0_stop = f1_proxy%vspace%get_ncell()\n"
            "      loop1_start = 1\n"
            "      loop1_stop = g1_proxy%vspace%get_ncell()\n" in gen_code)
    expected_kern_call = (
        "      ! Call our kernels\n"
        "      !\n"
        "      DO cell=loop0_start,loop0_stop\n"
        "        !\n"
        "        CALL testkern_qr_code(nlayers, f1_data, f2_data, "
        "m1_data, a, m2_data, istp, "
        "ndf_w1, undf_w1, map_w1(:,cell), basis_w1_qr, "
        "ndf_w2, undf_w2, map_w2(:,cell), diff_basis_w2_qr, "
        "ndf_w3, undf_w3, map_w3(:,cell), basis_w3_qr, diff_basis_w3_qr, "
        "np_xy_qr, np_z_qr, weights_xy_qr, weights_z_qr)\n"
        "      END DO\n"
        "      DO cell=loop1_start,loop1_stop\n"
        "        !\n"
        "        CALL testkern_qr_code(nlayers, g1_data, g2_data, "
        "n1_data, b, n2_data, istp, "
        "ndf_w1, undf_w1, map_w1(:,cell), basis_w1_qr2, "
        "ndf_w2, undf_w2, map_w2(:,cell), diff_basis_w2_qr2, "
        "ndf_w3, undf_w3, map_w3(:,cell), basis_w3_qr2, diff_basis_w3_qr2, "
        "np_xy_qr2, np_z_qr2, weights_xy_qr2, weights_z_qr2)\n"
        "      END DO\n"
        "      !\n"
        "      ! Deallocate basis arrays\n"
        "      !\n"
        "      DEALLOCATE (basis_w1_qr, basis_w1_qr2, basis_w3_qr, "
        "basis_w3_qr2, diff_basis_w2_qr, diff_basis_w2_qr2, diff_basis_w3_qr, "
        "diff_basis_w3_qr2)\n"
    )
    if expected_kern_call not in gen_code:
        print_diffs(expected_kern_call, gen_code)
        assert 0


def test_two_identical_qr(tmpdir):
    ''' Check that we handle an invoke containing two kernels that each
    require quadrature and are passed the same qr object '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1.1.3_single_invoke_2_identical_qr.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    gen_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    expected_init = (
        "      ! Look-up quadrature variables\n"
        "      !\n"
        "      qr_proxy = qr%get_quadrature_proxy()\n"
        "      np_xy_qr = qr_proxy%np_xy\n"
        "      np_z_qr = qr_proxy%np_z\n"
        "      weights_xy_qr => qr_proxy%weights_xy\n"
        "      weights_z_qr => qr_proxy%weights_z\n"
        "      !\n")
    assert expected_init in gen_code
    expected_alloc = (
        "      !\n"
        "      dim_w1 = f1_proxy%vspace%get_dim_space()\n"
        "      diff_dim_w2 = f2_proxy%vspace%get_dim_space_diff()\n"
        "      dim_w3 = m2_proxy%vspace%get_dim_space()\n"
        "      diff_dim_w3 = m2_proxy%vspace%get_dim_space_diff()\n"
        "      ALLOCATE (basis_w1_qr(dim_w1, ndf_w1, np_xy_qr, np_z_qr))\n"
        "      ALLOCATE (diff_basis_w2_qr(diff_dim_w2, ndf_w2, np_xy_qr, "
        "np_z_qr))\n"
        "      ALLOCATE (basis_w3_qr(dim_w3, ndf_w3, np_xy_qr, np_z_qr))\n"
        "      ALLOCATE (diff_basis_w3_qr(diff_dim_w3, ndf_w3, np_xy_qr, "
        "np_z_qr))\n"
        "      !\n")
    assert expected_alloc in gen_code
    expected_basis_init = (
        "      !\n"
        "      CALL qr%compute_function(BASIS, f1_proxy%vspace, "
        "dim_w1, ndf_w1, basis_w1_qr)\n"
        "      CALL qr%compute_function(DIFF_BASIS, f2_proxy%vspace, "
        "diff_dim_w2, ndf_w2, diff_basis_w2_qr)\n"
        "      CALL qr%compute_function(BASIS, m2_proxy%vspace, "
        "dim_w3, ndf_w3, basis_w3_qr)\n"
        "      CALL qr%compute_function(DIFF_BASIS, m2_proxy%vspace, "
        "diff_dim_w3, ndf_w3, diff_basis_w3_qr)\n"
        "      !\n")
    assert expected_basis_init in gen_code
    assert ("      loop0_stop = f1_proxy%vspace%get_ncell()\n"
            "      loop1_start = 1\n"
            "      loop1_stop = g1_proxy%vspace%get_ncell()\n" in gen_code)
    expected_kern_call = (
        "      DO cell=loop0_start,loop0_stop\n"
        "        !\n"
        "        CALL testkern_qr_code(nlayers, f1_data, f2_data,"
        " m1_data, a, m2_data, istp, ndf_w1, undf_w1, "
        "map_w1(:,cell), basis_w1_qr, ndf_w2, undf_w2, map_w2(:,cell), "
        "diff_basis_w2_qr, ndf_w3, undf_w3, map_w3(:,cell), basis_w3_qr, "
        "diff_basis_w3_qr, np_xy_qr, np_z_qr, weights_xy_qr, weights_z_qr)\n"
        "      END DO\n"
        "      DO cell=loop1_start,loop1_stop\n"
        "        !\n"
        "        CALL testkern_qr_code(nlayers, g1_data, g2_data, "
        "n1_data, b, n2_data, istp, ndf_w1, undf_w1, "
        "map_w1(:,cell), basis_w1_qr, ndf_w2, undf_w2, map_w2(:,cell), "
        "diff_basis_w2_qr, ndf_w3, undf_w3, map_w3(:,cell), basis_w3_qr, "
        "diff_basis_w3_qr, np_xy_qr, np_z_qr, weights_xy_qr, weights_z_qr)\n"
        "      END DO\n")
    assert expected_kern_call in gen_code
    expected_dealloc = (
        "DEALLOCATE (basis_w1_qr, basis_w3_qr, diff_basis_w2_qr, "
        "diff_basis_w3_qr)")
    assert expected_dealloc in gen_code


def test_two_qr_different_shapes(tmpdir):
    ''' Check that we handle an invoke containing two kernels that each
    require quadrature (with different shapes). '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.1.8_single_invoke_2qr_shapes.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    gen_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert "TYPE(quadrature_face_proxy_type) qrf_proxy" in gen_code
    assert "TYPE(quadrature_xyoz_proxy_type) qr_proxy" in gen_code

    assert "qr_proxy = qr%get_quadrature_proxy()" in gen_code
    assert "np_xy_qr = qr_proxy%np_xy" in gen_code
    assert "np_z_qr = qr_proxy%np_z" in gen_code
    assert "weights_xy_qr => qr_proxy%weights_xy" in gen_code
    assert "weights_z_qr => qr_proxy%weights_z" in gen_code

    assert "qrf_proxy = qrf%get_quadrature_proxy()" in gen_code
    assert "np_xyz_qrf = qrf_proxy%np_xyz" in gen_code
    assert "nfaces_qrf = qrf_proxy%nfaces" in gen_code
    assert "weights_xyz_qrf => qrf_proxy%weights_xyz" in gen_code

    assert ("CALL testkern_qr_code(nlayers, f1_data, f2_data, "
            "m1_data, a, m2_data, istp, ndf_w1, undf_w1, "
            "map_w1(:,cell), basis_w1_qr, ndf_w2, undf_w2, map_w2(:,cell), "
            "diff_basis_w2_qr, ndf_w3, undf_w3, map_w3(:,cell), basis_w3_qr, "
            "diff_basis_w3_qr, np_xy_qr, np_z_qr, weights_xy_qr, weights_z_qr)"
            in gen_code)
    assert ("CALL testkern_qr_faces_code(nlayers, f1_data, "
            "f2_data, m1_data, m2_data, ndf_w1, undf_w1, "
            "map_w1(:,cell), basis_w1_qrf, ndf_w2, undf_w2, map_w2(:,cell), "
            "diff_basis_w2_qrf, ndf_w3, undf_w3, map_w3(:,cell), basis_w3_qrf,"
            " diff_basis_w3_qrf, nfaces_qrf, np_xyz_qrf, weights_xyz_qrf)"
            in gen_code)


def test_anyw2(tmpdir, dist_mem):
    ''' Check generated code works correctly when we have any_w2 fields
    and basis functions. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "21.2_single_invoke_multi_anyw2_basis.f90"),
        api=API)
    psy = PSyFactory(API,
                     distributed_memory=dist_mem).create(invoke_info)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output = (
        "      ! Initialise number of DoFs for any_w2\n"
        "      !\n"
        "      ndf_any_w2 = f1_proxy%vspace%get_ndf()\n"
        "      undf_any_w2 = f1_proxy%vspace%get_undf()\n"
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
        "      dim_any_w2 = f1_proxy%vspace%get_dim_space()\n"
        "      diff_dim_any_w2 = f1_proxy%vspace%"
        "get_dim_space_diff()\n"
        "      ALLOCATE (basis_any_w2_qr(dim_any_w2, ndf_any_w2, "
        "np_xy_qr, np_z_qr))\n"
        "      ALLOCATE (diff_basis_any_w2_qr(diff_dim_any_w2, "
        "ndf_any_w2, np_xy_qr, np_z_qr))\n"
        "      !\n"
        "      ! Compute basis/diff-basis arrays\n"
        "      !\n"
        "      CALL qr%compute_function(BASIS, f1_proxy%vspace, "
        "dim_any_w2, ndf_any_w2, basis_any_w2_qr)\n"
        "      CALL qr%compute_function(DIFF_BASIS, f1_proxy%vspace, "
        "diff_dim_any_w2, ndf_any_w2, diff_basis_any_w2_qr)")
    assert output in generated_code


def test_qr_plus_eval(tmpdir):
    ''' Check that we handle an invoke containing two kernels, one
    requiring quadrature and one requiring an evaluator '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "6.2_qr_eval_invoke.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    gen_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    expected_module_declns = (
        "    USE constants_mod, ONLY: r_def, i_def\n"
        "    USE field_mod, ONLY: field_type, field_proxy_type\n")
    assert expected_module_declns in gen_code

    output_decls = (
        "    SUBROUTINE invoke_0(f0, f1, f2, m1, a, m2, istp, qr)\n"
        "      USE testkern_qr_mod, ONLY: testkern_qr_code\n"
        "      USE testkern_eval_mod, ONLY: testkern_eval_code\n"
        "      USE quadrature_xyoz_mod, ONLY: quadrature_xyoz_type, "
        "quadrature_xyoz_proxy_type\n"
        "      USE function_space_mod, ONLY: BASIS, DIFF_BASIS\n"
        "      REAL(KIND=r_def), intent(in) :: a\n"
        "      INTEGER(KIND=i_def), intent(in) :: istp\n"
        "      TYPE(field_type), intent(in) :: f0, f1, f2, m1, m2\n"
        "      TYPE(quadrature_xyoz_type), intent(in) :: qr\n"
        "      INTEGER(KIND=i_def) cell\n"
        "      INTEGER(KIND=i_def) loop1_start, loop1_stop\n"
        "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
        "      INTEGER(KIND=i_def) df_nodal, df_w0, df_w1\n"
        "      REAL(KIND=r_def), allocatable :: basis_w0_on_w0(:,:,:), "
        "diff_basis_w1_on_w0(:,:,:), basis_w1_qr(:,:,:,:), "
        "diff_basis_w2_qr(:,:,:,:), basis_w3_qr(:,:,:,:), "
        "diff_basis_w3_qr(:,:,:,:)\n"
        "      INTEGER(KIND=i_def) dim_w0, diff_dim_w1, dim_w1, "
        "diff_dim_w2, dim_w3, diff_dim_w3\n"
        "      REAL(KIND=r_def), pointer :: nodes_w0(:,:) => null()\n"
        "      REAL(KIND=r_def), pointer :: weights_xy_qr(:) => null(), "
        "weights_z_qr(:) => null()\n"
        "      INTEGER(KIND=i_def) np_xy_qr, np_z_qr\n"
        "      INTEGER(KIND=i_def) nlayers\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: m2_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: m1_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f2_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f1_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f0_data => null()\n"

        "      TYPE(field_proxy_type) f0_proxy, f1_proxy, f2_proxy, "
        "m1_proxy, m2_proxy\n"
        "      TYPE(quadrature_xyoz_proxy_type) qr_proxy\n"
        "      INTEGER(KIND=i_def), pointer :: map_w0(:,:) => null(), "
        "map_w1(:,:) => null(), map_w2(:,:) => null(), map_w3(:,:) => "
        "null()\n"
        "      INTEGER(KIND=i_def) ndf_w0, undf_w0, ndf_w1, undf_w1, "
        "ndf_w2, undf_w2, ndf_w3, undf_w3\n")
    assert output_decls in gen_code
    output_setup = (
        "      ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "      undf_w3 = m2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Look-up quadrature variables\n"
        "      !\n"
        "      qr_proxy = qr%get_quadrature_proxy()\n"
        "      np_xy_qr = qr_proxy%np_xy\n"
        "      np_z_qr = qr_proxy%np_z\n"
        "      weights_xy_qr => qr_proxy%weights_xy\n"
        "      weights_z_qr => qr_proxy%weights_z\n"
        "      !\n"
        "      ! Initialise evaluator-related quantities for the target "
        "function spaces\n"
        "      !\n"
        "      nodes_w0 => f0_proxy%vspace%get_nodes()\n"
        "      !\n"
        "      ! Allocate basis/diff-basis arrays\n"
        "      !\n"
        "      dim_w0 = f0_proxy%vspace%get_dim_space()\n"
        "      diff_dim_w1 = f1_proxy%vspace%get_dim_space_diff()\n"
        "      dim_w1 = f1_proxy%vspace%get_dim_space()\n"
        "      diff_dim_w2 = f2_proxy%vspace%get_dim_space_diff()\n"
        "      dim_w3 = m2_proxy%vspace%get_dim_space()\n"
        "      diff_dim_w3 = m2_proxy%vspace%get_dim_space_diff()\n"
        "      ALLOCATE (basis_w0_on_w0(dim_w0, ndf_w0, ndf_w0))\n"
        "      ALLOCATE (diff_basis_w1_on_w0(diff_dim_w1, ndf_w1, "
        "ndf_w0))\n"
        "      ALLOCATE (basis_w1_qr(dim_w1, ndf_w1, np_xy_qr, np_z_qr))\n"
        "      ALLOCATE (diff_basis_w2_qr(diff_dim_w2, ndf_w2, np_xy_qr, "
        "np_z_qr))\n"
        "      ALLOCATE (basis_w3_qr(dim_w3, ndf_w3, np_xy_qr, np_z_qr))\n"
        "      ALLOCATE (diff_basis_w3_qr(diff_dim_w3, ndf_w3, np_xy_qr, "
        "np_z_qr))\n"
        "      !\n"
        "      ! Compute basis/diff-basis arrays\n"
        "      !\n"
        "      DO df_nodal=1,ndf_w0\n"
        "        DO df_w0=1,ndf_w0\n"
        "          basis_w0_on_w0(:,df_w0,df_nodal) = f0_proxy%vspace%"
        "call_function(BASIS,df_w0,nodes_w0(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n"
        "      DO df_nodal=1,ndf_w0\n"
        "        DO df_w1=1,ndf_w1\n"
        "          diff_basis_w1_on_w0(:,df_w1,df_nodal) = f1_proxy%vspace%"
        "call_function(DIFF_BASIS,df_w1,nodes_w0(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n"
        "      CALL qr%compute_function(BASIS, f1_proxy%vspace, "
        "dim_w1, ndf_w1, basis_w1_qr)\n"
        "      CALL qr%compute_function(DIFF_BASIS, f2_proxy%vspace, "
        "diff_dim_w2, ndf_w2, diff_basis_w2_qr)\n"
        "      CALL qr%compute_function(BASIS, m2_proxy%vspace, "
        "dim_w3, ndf_w3, basis_w3_qr)\n"
        "      CALL qr%compute_function(DIFF_BASIS, m2_proxy%vspace, "
        "diff_dim_w3, ndf_w3, diff_basis_w3_qr)\n")
    assert output_setup in gen_code
    assert ("      loop0_stop = f0_proxy%vspace%get_ncell()\n"
            "      loop1_start = 1\n"
            "      loop1_stop = f1_proxy%vspace%get_ncell()\n" in gen_code)
    output_kern_call = (
        "      DO cell=loop0_start,loop0_stop\n"
        "        !\n"
        "        CALL testkern_eval_code(nlayers, f0_data, "
        "f1_data, ndf_w0, undf_w0, map_w0(:,cell), basis_w0_on_w0, "
        "ndf_w1, undf_w1, map_w1(:,cell), diff_basis_w1_on_w0)\n"
        "      END DO\n"
        "      DO cell=loop1_start,loop1_stop\n"
        "        !\n"
        "        CALL testkern_qr_code(nlayers, f1_data, f2_data, "
        "m1_data, a, m2_data, istp, ndf_w1, undf_w1, "
        "map_w1(:,cell), basis_w1_qr, ndf_w2, undf_w2, map_w2(:,cell), "
        "diff_basis_w2_qr, ndf_w3, undf_w3, map_w3(:,cell), basis_w3_qr, "
        "diff_basis_w3_qr, np_xy_qr, np_z_qr, weights_xy_qr, weights_z_qr)\n"
        "      END DO\n")
    assert output_kern_call in gen_code
    output_dealloc = (
        "      DEALLOCATE (basis_w0_on_w0, basis_w1_qr, basis_w3_qr, "
        "diff_basis_w1_on_w0, diff_basis_w2_qr, diff_basis_w3_qr)\n")
    assert output_dealloc in gen_code


def test_two_eval_same_space(tmpdir):
    ''' Check that we generate correct code when two kernels in an invoke
    both require evaluators and the arguments that are written to are on
    the same space '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "6.3_2eval_invoke.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    gen_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output_init = (
        "      !\n"
        "      ! Initialise evaluator-related quantities for the target "
        "function spaces\n"
        "      !\n"
        "      nodes_w0 => f0_proxy%vspace%get_nodes()\n"
        "      !\n"
        "      ! Allocate basis/diff-basis arrays\n"
        "      !\n"
        "      dim_w0 = f0_proxy%vspace%get_dim_space()\n"
        "      diff_dim_w1 = f1_proxy%vspace%get_dim_space_diff()\n"
        "      ALLOCATE (basis_w0_on_w0(dim_w0, ndf_w0, ndf_w0))\n"
        "      ALLOCATE (diff_basis_w1_on_w0(diff_dim_w1, ndf_w1, ndf_w0))\n")
    assert output_init in gen_code
    output_code = (
        "      !\n"
        "      ! Compute basis/diff-basis arrays\n"
        "      !\n"
        "      DO df_nodal=1,ndf_w0\n"
        "        DO df_w0=1,ndf_w0\n"
        "          basis_w0_on_w0(:,df_w0,df_nodal) = f0_proxy%vspace%"
        "call_function(BASIS,df_w0,nodes_w0(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n"
        "      DO df_nodal=1,ndf_w0\n"
        "        DO df_w1=1,ndf_w1\n"
        "          diff_basis_w1_on_w0(:,df_w1,df_nodal) = f1_proxy%vspace%"
        "call_function(DIFF_BASIS,df_w1,nodes_w0(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n"
        "      !\n"
        "      ! Set-up all of the loop bounds\n"
        "      !\n"
        "      loop0_start = 1\n"
        "      loop0_stop = f0_proxy%vspace%get_ncell()\n"
        "      loop1_start = 1\n"
        "      loop1_stop = f2_proxy%vspace%get_ncell()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO cell=loop0_start,loop0_stop\n"
        "        !\n"
        "        CALL testkern_eval_code(nlayers, f0_data, "
        "f1_data, ndf_w0, undf_w0, map_w0(:,cell), basis_w0_on_w0, "
        "ndf_w1, undf_w1, map_w1(:,cell), diff_basis_w1_on_w0)\n"
        "      END DO\n"
        "      DO cell=loop1_start,loop1_stop\n"
        "        !\n"
        "        CALL testkern_eval_code(nlayers, f2_data, "
        "f3_data, ndf_w0, undf_w0, map_w0(:,cell), basis_w0_on_w0, "
        "ndf_w1, undf_w1, map_w1(:,cell), diff_basis_w1_on_w0)\n"
        "      END DO\n"
    )
    assert output_code in gen_code


def test_two_eval_diff_space(tmpdir):
    ''' Check that we generate correct code when two kernels in an invoke
    both require evaluators and the arguments that are written to are on
    different spaces '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "6.4_2eval_op_invoke.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    gen_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # The first kernel in the invoke (testkern_eval_type) requires basis and
    # diff basis functions for the spaces of the first and second field
    # arguments, respectively. It writes to a field on W0 and therefore
    # the basis functions must be evaluated on the nodes of the W0 space.
    # The second kernel in the invoke (testkern_eval_op_type) requires basis
    # functions on the 'from' space of the operator and differential basis
    # functions for the space of the field argument. Since it writes to the op
    # arg we require basis functions on the nodal points of the 'to' space
    # of that operator (W0 in this case).
    expected_init = (
        "      ! Initialise evaluator-related quantities for the target "
        "function spaces\n"
        "      !\n"
        "      nodes_w0 => f0_proxy%vspace%get_nodes()\n"
        "      !\n"
        "      ! Allocate basis/diff-basis arrays\n"
        "      !\n"
        "      dim_w0 = f0_proxy%vspace%get_dim_space()\n"
        "      diff_dim_w1 = f1_proxy%vspace%get_dim_space_diff()\n"
        "      dim_w2 = op1_proxy%fs_from%get_dim_space()\n"
        "      diff_dim_w3 = f2_proxy%vspace%get_dim_space_diff()\n"
        "      ALLOCATE (basis_w0_on_w0(dim_w0, ndf_w0, ndf_w0))\n"
        "      ALLOCATE (diff_basis_w1_on_w0(diff_dim_w1, ndf_w1, ndf_w0))\n"
        "      ALLOCATE (basis_w2_on_w0(dim_w2, ndf_w2, ndf_w0))\n"
        "      ALLOCATE (diff_basis_w3_on_w0(diff_dim_w3, ndf_w3, ndf_w0))\n")
    assert expected_init in gen_code
    expected_code = (
        "      ! Compute basis/diff-basis arrays\n"
        "      !\n"
        "      DO df_nodal=1,ndf_w0\n"
        "        DO df_w0=1,ndf_w0\n"
        "          basis_w0_on_w0(:,df_w0,df_nodal) = f0_proxy%vspace%"
        "call_function(BASIS,df_w0,nodes_w0(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n"
        "      DO df_nodal=1,ndf_w0\n"
        "        DO df_w1=1,ndf_w1\n"
        "          diff_basis_w1_on_w0(:,df_w1,df_nodal) = f1_proxy%vspace%"
        "call_function(DIFF_BASIS,df_w1,nodes_w0(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n"
        "      DO df_nodal=1,ndf_w0\n"
        "        DO df_w2=1,ndf_w2\n"
        "          basis_w2_on_w0(:,df_w2,df_nodal) = op1_proxy%fs_from%"
        "call_function(BASIS,df_w2,nodes_w0(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n"
        "      DO df_nodal=1,ndf_w0\n"
        "        DO df_w3=1,ndf_w3\n"
        "          diff_basis_w3_on_w0(:,df_w3,df_nodal) = f2_proxy%vspace%"
        "call_function(DIFF_BASIS,df_w3,nodes_w0(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n"
        "      !\n"
        "      ! Set-up all of the loop bounds\n"
        "      !\n"
        "      loop0_start = 1\n"
        "      loop0_stop = f0_proxy%vspace%get_ncell()\n"
        "      loop1_start = 1\n"
        "      loop1_stop = op1_proxy%fs_from%get_ncell()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO cell=loop0_start,loop0_stop\n"
        "        !\n"
        "        CALL testkern_eval_code(nlayers, f0_data, "
        "f1_data, ndf_w0, undf_w0, map_w0(:,cell), basis_w0_on_w0, "
        "ndf_w1, undf_w1, map_w1(:,cell), diff_basis_w1_on_w0)\n"
        "      END DO\n"
        "      DO cell=loop1_start,loop1_stop\n"
        "        !\n"
        "        CALL testkern_eval_op_code(cell, nlayers, op1_proxy%ncell_3d,"
        " op1_local_stencil, f2_data, ndf_w0, ndf_w2, "
        "basis_w2_on_w0, ndf_w3, undf_w3, map_w3(:,cell), "
        "diff_basis_w3_on_w0)\n"
        "      END DO\n")
    assert expected_code in gen_code


def test_two_eval_same_var_same_space(tmpdir):
    ''' Check that we generate correct code when two kernels in an invoke
    both require evaluators for the same variable declared as being on the
    same space. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "6.7_2eval_same_var_invoke.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    gen_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # We should only get one set of basis and diff-basis functions in the
    # generated code
    assert gen_code.count(
        "ndf_adspc1_f0 = f0_proxy%vspace%get_ndf()") == 1
    assert gen_code.count(
        "      DO df_nodal=1,ndf_adspc1_f0\n"
        "        DO df_w0=1,ndf_w0\n"
        "          basis_w0_on_adspc1_f0(:,df_w0,df_nodal) = f1_proxy%vspace"
        "%call_function(BASIS,df_w0,nodes_adspc1_f0(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n") == 1
    assert gen_code.count(
        "      DO df_nodal=1,ndf_adspc1_f0\n"
        "        DO df_w1=1,ndf_w1\n"
        "          diff_basis_w1_on_adspc1_f0(:,df_w1,df_nodal) = f2_proxy"
        "%vspace%call_function(DIFF_BASIS,df_w1,nodes_adspc1_f0(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n") == 1
    assert gen_code.count(
        "DEALLOCATE (basis_w0_on_adspc1_f0, diff_basis_w1_on_adspc1_f0)") == 1


def test_two_eval_op_to_space(tmpdir):
    ''' Check that we generate correct code when two kernels in an invoke
    both require evaluators and the arguments that are written to are on
    different spaces, one of which is the 'to' space of an operator.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "6.5_2eval_op_to_invoke.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    gen_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # testkern_eval writes to W0. testkern_eval_op_to writes to W3.
    # testkern_eval requires basis fns on W0 and eval_op_to requires basis
    # fns on W2 which is the 'to' space of the operator arg
    init_code = (
        "      ndf_w2 = op1_proxy%fs_to%get_ndf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w3\n"
        "      !\n"
        "      ndf_w3 = f2_proxy%vspace%get_ndf()\n"
        "      undf_w3 = f2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise evaluator-related quantities for the target"
        " function spaces\n"
        "      !\n"
        "      nodes_w0 => f0_proxy%vspace%get_nodes()\n"
        "      nodes_w3 => f2_proxy%vspace%get_nodes()\n"
    )
    assert init_code in gen_code
    alloc_code = (
        "      dim_w0 = f0_proxy%vspace%get_dim_space()\n"
        "      diff_dim_w1 = f1_proxy%vspace%get_dim_space_diff()\n"
        "      dim_w2 = op1_proxy%fs_to%get_dim_space()\n"
        "      diff_dim_w2 = op1_proxy%fs_to%get_dim_space_diff()\n"
        "      diff_dim_w3 = f2_proxy%vspace%get_dim_space_diff()\n"
        "      ALLOCATE (basis_w0_on_w0(dim_w0, ndf_w0, ndf_w0))\n"
        "      ALLOCATE (diff_basis_w1_on_w0(diff_dim_w1, ndf_w1, "
        "ndf_w0))\n"
        "      ALLOCATE (basis_w2_on_w3(dim_w2, ndf_w2, ndf_w3))\n"
        "      ALLOCATE (diff_basis_w2_on_w3(diff_dim_w2, ndf_w2, "
        "ndf_w3))\n"
        "      ALLOCATE (diff_basis_w3_on_w3(diff_dim_w3, ndf_w3, "
        "ndf_w3))\n"
    )
    assert alloc_code in gen_code
    # testkern_eval requires diff-basis fns on W1 and testkern_eval_op_to
    # requires them on W2 and W3.
    basis_comp = (
        "      DO df_nodal=1,ndf_w0\n"
        "        DO df_w0=1,ndf_w0\n"
        "          basis_w0_on_w0(:,df_w0,df_nodal) = f0_proxy%vspace%"
        "call_function(BASIS,df_w0,nodes_w0(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n"
        "      DO df_nodal=1,ndf_w0\n"
        "        DO df_w1=1,ndf_w1\n"
        "          diff_basis_w1_on_w0(:,df_w1,df_nodal) = f1_proxy%vspace%"
        "call_function(DIFF_BASIS,df_w1,nodes_w0(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n"
        "      DO df_nodal=1,ndf_w3\n"
        "        DO df_w2=1,ndf_w2\n"
        "          basis_w2_on_w3(:,df_w2,df_nodal) = op1_proxy%fs_to%"
        "call_function(BASIS,df_w2,nodes_w3(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n"
        "      DO df_nodal=1,ndf_w3\n"
        "        DO df_w2=1,ndf_w2\n"
        "          diff_basis_w2_on_w3(:,df_w2,df_nodal) = op1_proxy%fs_to%"
        "call_function(DIFF_BASIS,df_w2,nodes_w3(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n"
        "      DO df_nodal=1,ndf_w3\n"
        "        DO df_w3=1,ndf_w3\n"
        "          diff_basis_w3_on_w3(:,df_w3,df_nodal) = f2_proxy%vspace%"
        "call_function(DIFF_BASIS,df_w3,nodes_w3(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n")
    assert basis_comp in gen_code
    assert ("      loop0_start = 1\n"
            "      loop0_stop = f0_proxy%vspace%get_ncell()\n"
            "      loop1_start = 1\n"
            "      loop1_stop = f2_proxy%vspace%get_ncell()\n" in gen_code)
    kernel_calls = (
        "      DO cell=loop0_start,loop0_stop\n"
        "        !\n"
        "        CALL testkern_eval_code(nlayers, f0_data, "
        "f1_data, ndf_w0, undf_w0, map_w0(:,cell), basis_w0_on_w0, "
        "ndf_w1, undf_w1, map_w1(:,cell), diff_basis_w1_on_w0)\n"
        "      END DO\n"
        "      DO cell=loop1_start,loop1_stop\n"
        "        !\n"
        "        CALL testkern_eval_op_to_code(cell, nlayers, "
        "op1_proxy%ncell_3d, op1_local_stencil, f2_data, "
        "ndf_w2, basis_w2_on_w3, diff_basis_w2_on_w3, ndf_w0, ndf_w3, "
        "undf_w3, map_w3(:,cell), diff_basis_w3_on_w3)\n"
        "      END DO\n"
    )
    assert kernel_calls in gen_code


def test_eval_diff_nodal_space(tmpdir):
    ''' Check that we generate correct code when evaluators are
    required for the same function space but with different nodal
    function spaces '''
    # Kernel testkern_eval_op_to requires an evaluator for basis on W2
    # and W3 and the updated argument is on W3.  Kernel
    # testkern_eval_op_to_w0 also requires an evaluator for basis on
    # W2 and W3 but this time the updated argument is on W0. We
    # therefore require two different basis functions, e.g. basis
    # functions for W2 evaluated at the nodes of W3 and another set
    # evaluated at the nodes of W0.
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "6.6_2eval_diff_nodal_space_invoke.f90"),
        api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    gen_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    expected_alloc = (
        "      nodes_w3 => f1_proxy%vspace%get_nodes()\n"
        "      nodes_w0 => op1_proxy%fs_from%get_nodes()\n"
        "      !\n"
        "      ! Allocate basis/diff-basis arrays\n"
        "      !\n"
        "      dim_w2 = op2_proxy%fs_to%get_dim_space()\n"
        "      diff_dim_w2 = op2_proxy%fs_to%get_dim_space_diff()\n"
        "      diff_dim_w3 = f1_proxy%vspace%get_dim_space_diff()\n"
        "      ALLOCATE (basis_w2_on_w3(dim_w2, ndf_w2, ndf_w3))\n"
        "      ALLOCATE (diff_basis_w2_on_w3(diff_dim_w2, ndf_w2, ndf_w3))\n"
        "      ALLOCATE (diff_basis_w3_on_w3(diff_dim_w3, ndf_w3, ndf_w3))\n"
        "      ALLOCATE (basis_w2_on_w0(dim_w2, ndf_w2, ndf_w0))\n"
        "      ALLOCATE (diff_basis_w2_on_w0(diff_dim_w2, ndf_w2, ndf_w0))\n"
        "      ALLOCATE (diff_basis_w3_on_w0(diff_dim_w3, ndf_w3, ndf_w0))\n"
    )
    assert expected_alloc in gen_code
    expected_compute = (
        "      DO df_nodal=1,ndf_w3\n"
        "        DO df_w2=1,ndf_w2\n"
        "          basis_w2_on_w3(:,df_w2,df_nodal) = op2_proxy%fs_to%"
        "call_function(BASIS,df_w2,nodes_w3(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n"
        "      DO df_nodal=1,ndf_w3\n"
        "        DO df_w2=1,ndf_w2\n"
        "          diff_basis_w2_on_w3(:,df_w2,df_nodal) = op2_proxy%fs_to%"
        "call_function(DIFF_BASIS,df_w2,nodes_w3(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n"
        "      DO df_nodal=1,ndf_w3\n"
        "        DO df_w3=1,ndf_w3\n"
        "          diff_basis_w3_on_w3(:,df_w3,df_nodal) = f1_proxy%vspace%"
        "call_function(DIFF_BASIS,df_w3,nodes_w3(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n"
        "      DO df_nodal=1,ndf_w0\n"
        "        DO df_w2=1,ndf_w2\n"
        "          basis_w2_on_w0(:,df_w2,df_nodal) = op1_proxy%fs_to%"
        "call_function(BASIS,df_w2,nodes_w0(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n"
        "      DO df_nodal=1,ndf_w0\n"
        "        DO df_w2=1,ndf_w2\n"
        "          diff_basis_w2_on_w0(:,df_w2,df_nodal) = op1_proxy%fs_to%"
        "call_function(DIFF_BASIS,df_w2,nodes_w0(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n"
        "      DO df_nodal=1,ndf_w0\n"
        "        DO df_w3=1,ndf_w3\n"
        "          diff_basis_w3_on_w0(:,df_w3,df_nodal) = f0_proxy%vspace%"
        "call_function(DIFF_BASIS,df_w3,nodes_w0(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n"
    )
    assert expected_compute in gen_code

    assert ("      loop0_start = 1\n"
            "      loop0_stop = f1_proxy%vspace%get_ncell()\n"
            "      loop1_start = 1\n"
            "      loop1_stop = f2_proxy%vspace%get_ncell()\n" in gen_code)

    expected_kern_call = (
        "      DO cell=loop0_start,loop0_stop\n"
        "        !\n"
        "        CALL testkern_eval_op_to_code(cell, nlayers, "
        "op2_proxy%ncell_3d, op2_local_stencil, f1_data, "
        "ndf_w2, basis_w2_on_w3, diff_basis_w2_on_w3, ndf_w0, ndf_w3, "
        "undf_w3, map_w3(:,cell), diff_basis_w3_on_w3)\n"
        "      END DO\n"
        "      DO cell=loop1_start,loop1_stop\n"
        "        !\n"
        "        CALL testkern_eval_op_to_w0_code(cell, nlayers, "
        "op1_proxy%ncell_3d, op1_local_stencil, f0_data, "
        "f2_data, ndf_w2, basis_w2_on_w0, diff_basis_w2_on_w0, "
        "ndf_w0, undf_w0, map_w0(:,cell), ndf_w3, undf_w3, map_w3(:,cell), "
        "diff_basis_w3_on_w0)\n"
        "      END DO\n"
    )
    assert expected_kern_call in gen_code
    expected_dealloc = (
        "      ! Deallocate basis arrays\n"
        "      !\n"
        "      DEALLOCATE ("
        "basis_w2_on_w0, basis_w2_on_w3, diff_basis_w2_on_w0, "
        "diff_basis_w2_on_w3, diff_basis_w3_on_w0, diff_basis_w3_on_w3)\n"
    )
    assert expected_dealloc in gen_code


def test_eval_2fs(tmpdir):
    ''' Test that we generate correct code when a kernel requires that
    a differential basis function be evaluated on two different FS. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "6.8_eval_2fs_invoke.f90"), api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    gen_code = str(psy.gen)

    assert ("      REAL(KIND=r_def), allocatable :: "
            "diff_basis_w1_on_w0(:,:,:), diff_basis_w1_on_w1(:,:,:)\n"
            "      INTEGER(KIND=i_def) diff_dim_w1\n" in
            gen_code)
    assert ("      diff_dim_w1 = f1_proxy%vspace%get_dim_space_diff()\n"
            "      ALLOCATE (diff_basis_w1_on_w0(diff_dim_w1, ndf_w1, "
            "ndf_w0))\n"
            "      ALLOCATE (diff_basis_w1_on_w1(diff_dim_w1, ndf_w1, "
            "ndf_w1))\n" in gen_code)
    assert ("CALL testkern_eval_2fs_code(nlayers, f0_data, "
            "f1_data, ndf_w0, undf_w0, map_w0(:,cell), ndf_w1, undf_w1, "
            "map_w1(:,cell), diff_basis_w1_on_w0, diff_basis_w1_on_w1)" in
            gen_code)
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_2eval_2fs(tmpdir):
    ''' Test that we generate correct code when we have an invoke with two
    kernels that both require a differential basis function that is evaluated
    on the same two FSs. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "6.9_2eval_2fs_invoke.f90"), api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    gen_code = str(psy.gen)

    assert ("REAL(KIND=r_def), allocatable :: diff_basis_w1_on_w0(:,:,:), "
            "diff_basis_w1_on_w1(:,:,:)\n" in gen_code)
    # Check for duplication
    for idx in range(2):
        assert gen_code.count(f"REAL(KIND=r_def), pointer :: nodes_w{idx}(:,:)"
                              f" => null()") == 1
        assert gen_code.count(
            f"      nodes_w{idx} => f{idx}_proxy%vspace%get_nodes()\n") == 1

        assert gen_code.count(f"ALLOCATE (diff_basis_w1_on_w{idx}(diff_dim_w1,"
                              f" ndf_w1, ndf_w{idx}))") == 1

        assert gen_code.count(
            f"diff_basis_w1_on_w{idx}(:,df_w1,df_nodal) = f1_proxy%vspace%"
            f"call_function(DIFF_BASIS,df_w1,nodes_w{idx}(:,df_nodal))") == 1
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_2eval_1qr_2fs(tmpdir):
    ''' Test that we generate correct code for an invoke requiring multiple,
    different evaluators *and* quadrature. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "6.10_2eval_2fs_qr_invoke.f90"), api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    gen_code = str(psy.gen)

    assert gen_code.count(
        "REAL(KIND=r_def), allocatable :: diff_basis_w1_on_w0(:,:,:), "
        "diff_basis_w1_on_w1(:,:,:), basis_w2_on_w0(:,:,:), "
        "diff_basis_w3_on_w0(:,:,:), basis_w1_qr(:,:,:,:), "
        "diff_basis_w2_qr(:,:,:,:), basis_w3_qr(:,:,:,:), "
        "diff_basis_w3_qr(:,:,:,:)\n") == 1

    # 1st kernel requires diff basis on W1, evaluated at W0 and W1
    # 2nd kernel requires diff basis on W3, evaluated at W0
    assert gen_code.count(
        "      diff_dim_w1 = f1_proxy%vspace%get_dim_space_diff()\n") == 1
    assert gen_code.count(
        "      ALLOCATE (diff_basis_w1_on_w0(diff_dim_w1, ndf_w1, ndf_w0))\n"
        "      ALLOCATE (diff_basis_w1_on_w1(diff_dim_w1, ndf_w1, "
        "ndf_w1))\n") == 1
    assert gen_code.count(
        "      diff_dim_w3 = m2_proxy%vspace%get_dim_space_diff()\n") == 1
    assert gen_code.count(
        "      ALLOCATE (diff_basis_w3_on_w0(diff_dim_w3, ndf_w3, "
        "ndf_w0))\n") == 1

    assert gen_code.count(
        "      DO df_nodal=1,ndf_w0\n"
        "        DO df_w1=1,ndf_w1\n"
        "          diff_basis_w1_on_w0(:,df_w1,df_nodal) = "
        "f1_proxy%vspace%call_function(DIFF_BASIS,df_w1,nodes_w0(:,"
        "df_nodal))\n"
        "        END DO\n"
        "      END DO\n") == 1
    assert gen_code.count(
        "      DO df_nodal=1,ndf_w1\n"
        "        DO df_w1=1,ndf_w1\n"
        "          diff_basis_w1_on_w1(:,df_w1,df_nodal) = f1_proxy%vspace%"
        "call_function(DIFF_BASIS,df_w1,nodes_w1(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n") == 1
    assert gen_code.count(
        "      DO df_nodal=1,ndf_w0\n"
        "        DO df_w3=1,ndf_w3\n"
        "          diff_basis_w3_on_w0(:,df_w3,df_nodal) = m2_proxy%vspace%"
        "call_function(DIFF_BASIS,df_w3,nodes_w0(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n") == 1

    # 2nd kernel requires basis on W2 and diff-basis on W3, both evaluated
    # on W0 (the to-space of the operator that is written to)
    assert gen_code.count(
        "      dim_w2 = op1_proxy%fs_from%get_dim_space()\n") == 1
    assert gen_code.count(
        "      ALLOCATE (basis_w2_on_w0(dim_w2, ndf_w2, ndf_w0))\n") == 1

    assert gen_code.count(
        "      DO df_nodal=1,ndf_w0\n"
        "        DO df_w2=1,ndf_w2\n"
        "          basis_w2_on_w0(:,df_w2,df_nodal) = op1_proxy%fs_from%"
        "call_function(BASIS,df_w2,nodes_w0(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n") == 1

    # 3rd kernel requires XYoZ quadrature: basis on W1, diff basis on W2 and
    # basis+diff basis on W3.
    assert gen_code.count(
        "      CALL qr%compute_function(DIFF_BASIS, f2_proxy%vspace, "
        "diff_dim_w2, ndf_w2, diff_basis_w2_qr)\n") == 1
    assert gen_code.count(
        "      CALL qr%compute_function(DIFF_BASIS, m2_proxy%vspace, "
        "diff_dim_w3, ndf_w3, diff_basis_w3_qr)\n") == 1

    assert ("      loop0_start = 1\n"
            "      loop0_stop = f0_proxy%vspace%get_ncell()\n"
            "      loop1_start = 1\n"
            "      loop1_stop = op1_proxy%fs_from%get_ncell()\n"
            "      loop2_start = 1\n"
            "      loop2_stop = f1_proxy%vspace%get_ncell()\n" in gen_code)

    assert ("      DO cell=loop0_start,loop0_stop\n"
            "        !\n"
            "        CALL testkern_eval_2fs_code(nlayers, f0_data, "
            "f1_data, ndf_w0, undf_w0, map_w0(:,cell), ndf_w1, undf_w1,"
            " map_w1(:,cell), diff_basis_w1_on_w0, diff_basis_w1_on_w1)\n"
            "      END DO\n"
            "      DO cell=loop1_start,loop1_stop\n"
            "        !\n"
            "        CALL testkern_eval_op_code(cell, nlayers, "
            "op1_proxy%ncell_3d, op1_local_stencil, m2_data, "
            "ndf_w0, ndf_w2, basis_w2_on_w0, ndf_w3, undf_w3, map_w3(:,cell),"
            " diff_basis_w3_on_w0)\n"
            "      END DO\n"
            "      DO cell=loop2_start,loop2_stop\n"
            "        !\n"
            "        CALL testkern_qr_code(nlayers, f1_data, "
            "f2_data, m1_data, a, m2_data, istp, ndf_w1, "
            "undf_w1, map_w1(:,cell), basis_w1_qr, ndf_w2, undf_w2, "
            "map_w2(:,cell), diff_basis_w2_qr, ndf_w3, undf_w3, "
            "map_w3(:,cell), basis_w3_qr, diff_basis_w3_qr, np_xy_qr, "
            "np_z_qr, weights_xy_qr, weights_z_qr)\n"
            "      END DO\n" in gen_code)

    assert gen_code.count(
        "DEALLOCATE (basis_w1_qr, basis_w2_on_w0, basis_w3_qr, "
        "diff_basis_w1_on_w0, diff_basis_w1_on_w1, diff_basis_w2_qr, "
        "diff_basis_w3_on_w0, diff_basis_w3_qr)\n") == 1

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_eval_agglomerate(tmpdir):
    ''' Check that we aglomerate evaluators when different kernels require
    the same function on the same space but evaluated on different spaces. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "6.11_2eval_2kern_invoke.f90"), api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    gen_code = str(psy.gen)
    # We should compute differential basis functions for W1 evaluated on both
    # W0 and W1.
    assert gen_code.count("diff_basis_w1_on_w0(:,df_w1,df_nodal) = ") == 1
    assert gen_code.count("diff_basis_w1_on_w1(:,df_w1,df_nodal) = ") == 1
    assert LFRicBuild(tmpdir).code_compiles(psy)


BASIS_EVAL = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(12) =                                       &
          (/ arg_type(gh_field,    gh_real, gh_inc,   w0),                 &
             arg_type(gh_operator, gh_real, gh_read,  w1, w1),             &
             arg_type(gh_field,    gh_real, gh_read,  w2),                 &
             arg_type(gh_operator, gh_real, gh_read,  w3, w3),             &
             arg_type(gh_field,    gh_real, gh_read,  wtheta),             &
             arg_type(gh_operator, gh_real, gh_read,  w2h, w2h),           &
             arg_type(gh_field,    gh_real, gh_read,  w2v),                &
             arg_type(gh_operator, gh_real, gh_read,  w2broken, w2broken), &
             arg_type(gh_field,    gh_real, gh_read,  wchi),               &
             arg_type(gh_operator, gh_real, gh_read,  w2trace, w2trace),   &
             arg_type(gh_field,    gh_real, gh_read,  w2vtrace),           &
             arg_type(gh_operator, gh_real, gh_read,  w2htrace, w2htrace)  &
           /)
     type(func_type), meta_funcs(12) =      &
          (/ func_type(w0, gh_basis),       &
             func_type(w1, gh_basis),       &
             func_type(w2, gh_basis),       &
             func_type(w3, gh_basis),       &
             func_type(wtheta, gh_basis),   &
             func_type(w2h, gh_basis),      &
             func_type(w2v, gh_basis),      &
             func_type(w2broken, gh_basis), &
             func_type(wchi, gh_basis),     &
             func_type(w2trace, gh_basis),  &
             func_type(w2vtrace, gh_basis), &
             func_type(w2htrace, gh_basis)  &
           /)
     integer :: operates_on = cell_column
     integer :: gh_shape = gh_evaluator
   contains
     procedure, nopass :: code => dummy_code
  end type dummy_type
contains
  subroutine dummy_code()
  end subroutine dummy_code
end module dummy_mod
'''


def test_basis_evaluator():
    ''' Check that basis functions for an evaluator are handled correctly for
    kernel stubs.

    '''
    ast = fpapi.parse(BASIS_EVAL, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    generated_code = str(kernel.gen_stub)

    output_arg_list = (
        "    SUBROUTINE dummy_code(cell, nlayers, field_1_w0, op_2_ncell_3d, "
        "op_2, field_3_w2, op_4_ncell_3d, op_4, field_5_wtheta, "
        "op_6_ncell_3d, op_6, field_7_w2v, op_8_ncell_3d, op_8, field_9_wchi, "
        "op_10_ncell_3d, op_10, field_11_w2vtrace, op_12_ncell_3d, op_12, "
        "ndf_w0, undf_w0, map_w0, basis_w0_on_w0, ndf_w1, basis_w1_on_w0, "
        "ndf_w2, undf_w2, map_w2, basis_w2_on_w0, ndf_w3, basis_w3_on_w0, "
        "ndf_wtheta, undf_wtheta, map_wtheta, basis_wtheta_on_w0, ndf_w2h, "
        "basis_w2h_on_w0, ndf_w2v, undf_w2v, map_w2v, basis_w2v_on_w0, "
        "ndf_w2broken, basis_w2broken_on_w0, ndf_wchi, undf_wchi, map_wchi, "
        "basis_wchi_on_w0, ndf_w2trace, basis_w2trace_on_w0, ndf_w2vtrace, "
        "undf_w2vtrace, map_w2vtrace, basis_w2vtrace_on_w0, ndf_w2htrace, "
        "basis_w2htrace_on_w0)\n")
    assert output_arg_list in generated_code
    output_declns = (
        "      INTEGER(KIND=i_def), intent(in) :: nlayers\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w0\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w0) :: map_w0\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w2\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2) :: map_w2\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w2v\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2v) "
        ":: map_w2v\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w2vtrace\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2vtrace) "
        ":: map_w2vtrace\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_wchi\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_wchi) "
        ":: map_wchi\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_wtheta\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_wtheta) "
        ":: map_wtheta\n"
        "      INTEGER(KIND=i_def), intent(in) :: undf_w0, ndf_w1, undf_w2, "
        "ndf_w3, undf_wtheta, ndf_w2h, undf_w2v, ndf_w2broken, undf_wchi, "
        "ndf_w2trace, undf_w2vtrace, ndf_w2htrace\n"
        "      REAL(KIND=r_def), intent(inout), dimension(undf_w0) "
        ":: field_1_w0\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w2) "
        ":: field_3_w2\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_wtheta) "
        ":: field_5_wtheta\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w2v) "
        ":: field_7_w2v\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_wchi) "
        ":: field_9_wchi\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w2vtrace) "
        ":: field_11_w2vtrace\n"
        "      INTEGER(KIND=i_def), intent(in) :: cell\n"
        "      INTEGER(KIND=i_def), intent(in) :: op_2_ncell_3d\n"
        "      REAL(KIND=r_def), intent(in), dimension(ndf_w1,ndf_w1,"
        "op_2_ncell_3d) :: op_2\n"
        "      INTEGER(KIND=i_def), intent(in) :: op_4_ncell_3d\n"
        "      REAL(KIND=r_def), intent(in), dimension(ndf_w3,ndf_w3,"
        "op_4_ncell_3d) :: op_4\n"
        "      INTEGER(KIND=i_def), intent(in) :: op_6_ncell_3d\n"
        "      REAL(KIND=r_def), intent(in), dimension(ndf_w2h,ndf_w2h,"
        "op_6_ncell_3d) :: op_6\n"
        "      INTEGER(KIND=i_def), intent(in) :: op_8_ncell_3d\n"
        "      REAL(KIND=r_def), intent(in), dimension(ndf_w2broken,"
        "ndf_w2broken,op_8_ncell_3d) :: op_8\n"
        "      INTEGER(KIND=i_def), intent(in) :: op_10_ncell_3d\n"
        "      REAL(KIND=r_def), intent(in), dimension(ndf_w2trace,"
        "ndf_w2trace,op_10_ncell_3d) :: op_10\n"
        "      INTEGER(KIND=i_def), intent(in) :: op_12_ncell_3d\n"
        "      REAL(KIND=r_def), intent(in), dimension(ndf_w2htrace,"
        "ndf_w2htrace,op_12_ncell_3d) :: op_12\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w0,ndf_w0) "
        ":: basis_w0_on_w0\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w1,ndf_w0) "
        ":: basis_w1_on_w0\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w2,ndf_w0) "
        ":: basis_w2_on_w0\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w3,ndf_w0) "
        ":: basis_w3_on_w0\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_wtheta,ndf_w0) "
        ":: basis_wtheta_on_w0\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w2h,ndf_w0) "
        ":: basis_w2h_on_w0\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w2v,ndf_w0) "
        ":: basis_w2v_on_w0\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w2broken,"
        "ndf_w0) :: basis_w2broken_on_w0\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_wchi,ndf_w0) "
        ":: basis_wchi_on_w0\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w2trace,"
        "ndf_w0) :: basis_w2trace_on_w0\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w2vtrace,"
        "ndf_w0) :: basis_w2vtrace_on_w0\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w2htrace,"
        "ndf_w0) :: basis_w2htrace_on_w0\n"
    )
    assert output_declns in generated_code


BASIS_UNSUPPORTED_SPACE = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(1) =                           &
          (/ arg_type(gh_field, gh_real, gh_inc, any_space_1) &
           /)
     type(func_type), meta_funcs(1) =         &
          (/ func_type(any_space_1, gh_basis) &
           /)
     integer :: operates_on = cell_column
     integer :: gh_shape = gh_quadrature_XYoZ
   contains
     procedure, nopass :: code => dummy_code
  end type dummy_type
contains
  subroutine dummy_code()
  end subroutine dummy_code
end module dummy_mod
'''


def test_basis_unsupported_space():
    ''' Test that an error is raised when a basis function is on an
    unsupported space (currently any_space_* and any_discontinuous_space_*)
    in kernel stub generation. This information will be passed from the
    PSy layer to the kernels (see issue #461). '''
    # Test any_space_*
    ast = fpapi.parse(BASIS_UNSUPPORTED_SPACE, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    with pytest.raises(GenerationError) as excinfo:
        _ = kernel.gen_stub
    const = LFRicConstants()
    assert ("Unsupported space for basis function, expecting one of " +
            str(const.VALID_FUNCTION_SPACES) + " but found " +
            "'any_space_1'" in str(excinfo.value))
    # Test any_discontinuous_space_*
    code = BASIS_UNSUPPORTED_SPACE.replace("any_space_1",
                                           "any_discontinuous_space_5")
    code = code.replace("gh_inc", "gh_readwrite")
    ast = fpapi.parse(code, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    with pytest.raises(GenerationError) as excinfo:
        _ = kernel.gen_stub
    assert "Unsupported space for basis function" in str(excinfo.value)
    assert "but found 'any_discontinuous_space_5'" in str(excinfo.value)


# diff basis function : spaces
DIFF_BASIS = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(12) =                                 &
          (/ arg_type(gh_field,    gh_real, gh_inc,       w0),       &
             arg_type(gh_operator, gh_real, gh_readwrite, w1, w1),   &
             arg_type(gh_field,    gh_real, gh_read,      w2),       &
             arg_type(gh_operator, gh_real, gh_write,     w3, w3),   &
             arg_type(gh_field,    gh_real, gh_write,     wtheta),   &
             arg_type(gh_operator, gh_real, gh_readwrite, w2h, w2h), &
             arg_type(gh_field,    gh_real, gh_read,      w2v),      &
             arg_type(gh_operator, gh_real, gh_readwrite, w2broken,  &
                                                          w2broken), &
             arg_type(gh_field,    gh_real, gh_read,      wchi),     &
             arg_type(gh_operator, gh_real, gh_write,     w2trace,   &
                                                          w2trace),  &
             arg_type(gh_field,    gh_real, gh_inc,       w2htrace), &
             arg_type(gh_operator, gh_real, gh_read,      w2vtrace,  &
                                                          w2vtrace)  &
           /)
     type(func_type), meta_funcs(12) =           &
          (/ func_type(w0, gh_diff_basis),       &
             func_type(w1, gh_diff_basis),       &
             func_type(w2, gh_diff_basis),       &
             func_type(w3, gh_diff_basis),       &
             func_type(wtheta, gh_diff_basis),   &
             func_type(w2h, gh_diff_basis),      &
             func_type(w2v, gh_diff_basis),      &
             func_type(w2broken, gh_diff_basis), &
             func_type(wchi, gh_diff_basis),     &
             func_type(w2trace, gh_diff_basis),  &
             func_type(w2htrace, gh_diff_basis), &
             func_type(w2vtrace, gh_diff_basis)  &
           /)
     integer :: operates_on = cell_column
     integer :: gh_shape = gh_quadrature_XYoZ
   contains
     procedure, nopass :: code => dummy_code
  end type dummy_type
contains
  subroutine dummy_code()
  end subroutine dummy_code
end module dummy_mod
'''


def test_diff_basis():
    ''' Test that differential basis functions are handled correctly
    for kernel stubs with quadrature.

    '''
    ast = fpapi.parse(DIFF_BASIS, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    generated_code = str(kernel.gen_stub)
    output = (
        "  MODULE dummy_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE dummy_code(cell, nlayers, field_1_w0, op_2_ncell_3d, "
        "op_2, field_3_w2, op_4_ncell_3d, op_4, field_5_wtheta, "
        "op_6_ncell_3d, op_6, field_7_w2v, op_8_ncell_3d, op_8, field_9_wchi, "
        "op_10_ncell_3d, op_10, field_11_w2htrace, op_12_ncell_3d, op_12, "
        "ndf_w0, undf_w0, map_w0, diff_basis_w0_qr_xyoz, ndf_w1, "
        "diff_basis_w1_qr_xyoz, ndf_w2, undf_w2, map_w2, "
        "diff_basis_w2_qr_xyoz, ndf_w3, diff_basis_w3_qr_xyoz, ndf_wtheta, "
        "undf_wtheta, map_wtheta, diff_basis_wtheta_qr_xyoz, ndf_w2h, "
        "diff_basis_w2h_qr_xyoz, ndf_w2v, undf_w2v, map_w2v, "
        "diff_basis_w2v_qr_xyoz, ndf_w2broken, diff_basis_w2broken_qr_xyoz, "
        "ndf_wchi, undf_wchi, map_wchi, diff_basis_wchi_qr_xyoz, ndf_w2trace, "
        "diff_basis_w2trace_qr_xyoz, ndf_w2htrace, undf_w2htrace, "
        "map_w2htrace, diff_basis_w2htrace_qr_xyoz, ndf_w2vtrace, "
        "diff_basis_w2vtrace_qr_xyoz, np_xy_qr_xyoz, np_z_qr_xyoz, "
        "weights_xy_qr_xyoz, weights_z_qr_xyoz)\n"
        "      USE constants_mod\n"
        "      IMPLICIT NONE\n"
        "      INTEGER(KIND=i_def), intent(in) :: nlayers\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w0\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w0) :: map_w0\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w2\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2) :: map_w2\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w2htrace\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2htrace) "
        ":: map_w2htrace\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w2v\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2v) "
        ":: map_w2v\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_wchi\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_wchi) "
        ":: map_wchi\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_wtheta\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_wtheta) "
        ":: map_wtheta\n"
        "      INTEGER(KIND=i_def), intent(in) :: undf_w0, ndf_w1, undf_w2, "
        "ndf_w3, undf_wtheta, ndf_w2h, undf_w2v, ndf_w2broken, undf_wchi, "
        "ndf_w2trace, undf_w2htrace, ndf_w2vtrace\n"
        "      REAL(KIND=r_def), intent(inout), dimension(undf_w0) "
        ":: field_1_w0\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w2) "
        ":: field_3_w2\n"
        "      REAL(KIND=r_def), intent(inout), dimension(undf_wtheta) "
        ":: field_5_wtheta\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w2v) "
        ":: field_7_w2v\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_wchi) "
        ":: field_9_wchi\n"
        "      REAL(KIND=r_def), intent(inout), dimension(undf_w2htrace) "
        ":: field_11_w2htrace\n"
        "      INTEGER(KIND=i_def), intent(in) :: cell\n"
        "      INTEGER(KIND=i_def), intent(in) :: op_2_ncell_3d\n"
        "      REAL(KIND=r_def), intent(inout), dimension(ndf_w1,ndf_w1,"
        "op_2_ncell_3d) :: op_2\n"
        "      INTEGER(KIND=i_def), intent(in) :: op_4_ncell_3d\n"
        "      REAL(KIND=r_def), intent(inout), dimension(ndf_w3,ndf_w3,"
        "op_4_ncell_3d) :: op_4\n"
        "      INTEGER(KIND=i_def), intent(in) :: op_6_ncell_3d\n"
        "      REAL(KIND=r_def), intent(inout), dimension(ndf_w2h,ndf_w2h,"
        "op_6_ncell_3d) :: op_6\n"
        "      INTEGER(KIND=i_def), intent(in) :: op_8_ncell_3d\n"
        "      REAL(KIND=r_def), intent(inout), dimension(ndf_w2broken,"
        "ndf_w2broken,op_8_ncell_3d) :: op_8\n"
        "      INTEGER(KIND=i_def), intent(in) :: op_10_ncell_3d\n"
        "      REAL(KIND=r_def), intent(inout), dimension(ndf_w2trace,"
        "ndf_w2trace,op_10_ncell_3d) :: op_10\n"
        "      INTEGER(KIND=i_def), intent(in) :: op_12_ncell_3d\n"
        "      REAL(KIND=r_def), intent(in), dimension(ndf_w2vtrace,"
        "ndf_w2vtrace,op_12_ncell_3d) :: op_12\n"
        "      INTEGER(KIND=i_def), intent(in) :: np_xy_qr_xyoz, "
        "np_z_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w0,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: diff_basis_w0_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w1,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: diff_basis_w1_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w2,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: diff_basis_w2_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w3,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: diff_basis_w3_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_wtheta,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: diff_basis_wtheta_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w2h,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: diff_basis_w2h_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w2v,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: diff_basis_w2v_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w2broken,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: diff_basis_w2broken_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_wchi,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: diff_basis_wchi_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w2trace,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: diff_basis_w2trace_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w2htrace,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: diff_basis_w2htrace_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w2vtrace,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: diff_basis_w2vtrace_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(np_xy_qr_xyoz) "
        ":: weights_xy_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(np_z_qr_xyoz) "
        ":: weights_z_qr_xyoz\n"
        "    END SUBROUTINE dummy_code\n"
        "  END MODULE dummy_mod")
    assert output in generated_code


# Metadata for a kernel that requires differential basis functions
# evaluated only on W2 (the to-space of the operator that this kernel
# writes to).
DIFF_BASIS_EVAL = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(12) =                                 &
          (/ arg_type(gh_field,    gh_real, gh_read,      w0),       &
             arg_type(gh_operator, gh_real, gh_readwrite, w2, w1),   &
             arg_type(gh_field,    gh_real, gh_read,      w2),       &
             arg_type(gh_operator, gh_real, gh_read,      w3, w3),   &
             arg_type(gh_field,    gh_real, gh_read,      wtheta),   &
             arg_type(gh_operator, gh_real, gh_read,      w2h, w2h), &
             arg_type(gh_field,    gh_real, gh_read,      w2v),      &
             arg_type(gh_operator, gh_real, gh_read,      w2broken,  &
                                                          w2broken), &
             arg_type(gh_field,    gh_real, gh_read,      wchi),     &
             arg_type(gh_operator, gh_real, gh_read,      w2trace,   &
                                                          w2trace),  &
             arg_type(gh_field,    gh_real, gh_read,      w2vtrace), &
             arg_type(gh_operator, gh_real, gh_read,      w2htrace,  &
                                                          w2htrace)  &
           /)
     type(func_type), meta_funcs(12) =           &
          (/ func_type(w0, gh_diff_basis),       &
             func_type(w1, gh_diff_basis),       &
             func_type(w2, gh_diff_basis),       &
             func_type(w3, gh_diff_basis),       &
             func_type(wtheta, gh_diff_basis),   &
             func_type(w2h, gh_diff_basis),      &
             func_type(w2v, gh_diff_basis),      &
             func_type(w2broken, gh_diff_basis), &
             func_type(wchi, gh_diff_basis),     &
             func_type(w2trace, gh_diff_basis),  &
             func_type(w2vtrace, gh_diff_basis), &
             func_type(w2htrace, gh_diff_basis)  &
           /)
     integer :: operates_on = cell_column
     integer :: gh_shape = gh_evaluator
   contains
     procedure, nopass :: code => dummy_code
  end type dummy_type
contains
  subroutine dummy_code()
  end subroutine dummy_code
end module dummy_mod
'''


def test_diff_basis_eval():
    ''' Test that differential basis functions are handled correctly
    for kernel stubs with an evaluator.

    '''
    ast = fpapi.parse(DIFF_BASIS_EVAL, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    generated_code = str(kernel.gen_stub)

    output_args = (
        "  MODULE dummy_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE dummy_code(cell, nlayers, field_1_w0, op_2_ncell_3d, "
        "op_2, field_3_w2, op_4_ncell_3d, op_4, field_5_wtheta, "
        "op_6_ncell_3d, op_6, field_7_w2v, op_8_ncell_3d, op_8, field_9_wchi, "
        "op_10_ncell_3d, op_10, field_11_w2vtrace, op_12_ncell_3d, op_12, "
        "ndf_w0, undf_w0, map_w0, diff_basis_w0_on_w2, ndf_w2, undf_w2, "
        "map_w2, diff_basis_w2_on_w2, ndf_w1, diff_basis_w1_on_w2, ndf_w3, "
        "diff_basis_w3_on_w2, ndf_wtheta, undf_wtheta, map_wtheta, "
        "diff_basis_wtheta_on_w2, ndf_w2h, diff_basis_w2h_on_w2, ndf_w2v, "
        "undf_w2v, map_w2v, diff_basis_w2v_on_w2, ndf_w2broken, "
        "diff_basis_w2broken_on_w2, ndf_wchi, undf_wchi, map_wchi, "
        "diff_basis_wchi_on_w2, ndf_w2trace, diff_basis_w2trace_on_w2, "
        "ndf_w2vtrace, undf_w2vtrace, map_w2vtrace, "
        "diff_basis_w2vtrace_on_w2, ndf_w2htrace, "
        "diff_basis_w2htrace_on_w2)\n")
    assert output_args in generated_code
    output_declns = (
        "      INTEGER(KIND=i_def), intent(in) :: nlayers\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w0\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w0) :: map_w0\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w2\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2) :: map_w2\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w2v\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2v) "
        ":: map_w2v\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w2vtrace\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2vtrace) "
        ":: map_w2vtrace\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_wchi\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_wchi) "
        ":: map_wchi\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_wtheta\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_wtheta) "
        ":: map_wtheta\n"
        "      INTEGER(KIND=i_def), intent(in) :: undf_w0, undf_w2, ndf_w1, "
        "ndf_w3, undf_wtheta, ndf_w2h, undf_w2v, ndf_w2broken, undf_wchi, "
        "ndf_w2trace, undf_w2vtrace, ndf_w2htrace\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w0) "
        ":: field_1_w0\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w2) "
        ":: field_3_w2\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_wtheta) "
        ":: field_5_wtheta\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w2v) "
        ":: field_7_w2v\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_wchi) "
        ":: field_9_wchi\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w2vtrace) "
        ":: field_11_w2vtrace\n"
        "      INTEGER(KIND=i_def), intent(in) :: cell\n"
        "      INTEGER(KIND=i_def), intent(in) :: op_2_ncell_3d\n"
        "      REAL(KIND=r_def), intent(inout), dimension(ndf_w2,ndf_w1,"
        "op_2_ncell_3d) :: op_2\n"
        "      INTEGER(KIND=i_def), intent(in) :: op_4_ncell_3d\n"
        "      REAL(KIND=r_def), intent(in), dimension(ndf_w3,ndf_w3,"
        "op_4_ncell_3d) :: op_4\n"
        "      INTEGER(KIND=i_def), intent(in) :: op_6_ncell_3d\n"
        "      REAL(KIND=r_def), intent(in), dimension(ndf_w2h,ndf_w2h,"
        "op_6_ncell_3d) :: op_6\n"
        "      INTEGER(KIND=i_def), intent(in) :: op_8_ncell_3d\n"
        "      REAL(KIND=r_def), intent(in), dimension(ndf_w2broken,"
        "ndf_w2broken,op_8_ncell_3d) :: op_8\n"
        "      INTEGER(KIND=i_def), intent(in) :: op_10_ncell_3d\n"
        "      REAL(KIND=r_def), intent(in), dimension(ndf_w2trace,"
        "ndf_w2trace,op_10_ncell_3d) :: op_10\n"
        "      INTEGER(KIND=i_def), intent(in) :: op_12_ncell_3d\n"
        "      REAL(KIND=r_def), intent(in), dimension(ndf_w2htrace,"
        "ndf_w2htrace,op_12_ncell_3d) :: op_12\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w0,ndf_w2) "
        ":: diff_basis_w0_on_w2\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w1,ndf_w2) "
        ":: diff_basis_w1_on_w2\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w2,ndf_w2) "
        ":: diff_basis_w2_on_w2\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w3,ndf_w2) "
        ":: diff_basis_w3_on_w2\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_wtheta,ndf_w2) "
        ":: diff_basis_wtheta_on_w2\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w2h,ndf_w2) "
        ":: diff_basis_w2h_on_w2\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w2v,ndf_w2) "
        ":: diff_basis_w2v_on_w2\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w2broken,"
        "ndf_w2) :: diff_basis_w2broken_on_w2\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_wchi,ndf_w2) "
        ":: diff_basis_wchi_on_w2\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w2trace,"
        "ndf_w2) :: diff_basis_w2trace_on_w2\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w2vtrace,"
        "ndf_w2) :: diff_basis_w2vtrace_on_w2\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w2htrace,"
        "ndf_w2) :: diff_basis_w2htrace_on_w2\n"
        "    END SUBROUTINE dummy_code\n"
    )
    assert output_declns in generated_code


def test_2eval_stubgen():
    ''' Check that we generate the correct kernel stub when an evaluator is
    required on more than one space.

    '''
    # Modify the metadata so that it specifies that evaluators be provided
    # on two function spaces
    twoeval_meta = DIFF_BASIS_EVAL.replace(
        "     integer :: gh_shape = gh_evaluator\n",
        "     integer :: gh_shape = gh_evaluator\n"
        "     integer :: gh_evaluator_targets(2) = (/w2h, wtheta/)\n")
    ast = fpapi.parse(twoeval_meta, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    generated_code = str(kernel.gen_stub)

    assert (
        "SUBROUTINE dummy_code(cell, nlayers, field_1_w0, op_2_ncell_3d, "
        "op_2, field_3_w2, op_4_ncell_3d, op_4, field_5_wtheta, "
        "op_6_ncell_3d, op_6, field_7_w2v, op_8_ncell_3d, op_8, "
        "field_9_wchi, op_10_ncell_3d, op_10, field_11_w2vtrace, "
        "op_12_ncell_3d, op_12, ndf_w0, undf_w0, map_w0, "
        "diff_basis_w0_on_w2h, diff_basis_w0_on_wtheta, ndf_w2, undf_w2, "
        "map_w2, diff_basis_w2_on_w2h, diff_basis_w2_on_wtheta, ndf_w1, "
        "diff_basis_w1_on_w2h, diff_basis_w1_on_wtheta, ndf_w3, "
        "diff_basis_w3_on_w2h, diff_basis_w3_on_wtheta, ndf_wtheta, "
        "undf_wtheta, map_wtheta, diff_basis_wtheta_on_w2h, "
        "diff_basis_wtheta_on_wtheta, ndf_w2h, diff_basis_w2h_on_w2h, "
        "diff_basis_w2h_on_wtheta, ndf_w2v, undf_w2v, map_w2v, "
        "diff_basis_w2v_on_w2h, diff_basis_w2v_on_wtheta, ndf_w2broken, "
        "diff_basis_w2broken_on_w2h, diff_basis_w2broken_on_wtheta, "
        "ndf_wchi, undf_wchi, map_wchi, diff_basis_wchi_on_w2h, "
        "diff_basis_wchi_on_wtheta, ndf_w2trace, diff_basis_w2trace_on_w2h, "
        "diff_basis_w2trace_on_wtheta, ndf_w2vtrace, undf_w2vtrace, "
        "map_w2vtrace, diff_basis_w2vtrace_on_w2h, "
        "diff_basis_w2vtrace_on_wtheta, ndf_w2htrace, "
        "diff_basis_w2htrace_on_w2h, diff_basis_w2htrace_on_wtheta)\n" in
        generated_code)
    assert (
        "      INTEGER(KIND=i_def), intent(in) :: nlayers\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w0\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w0) :: map_w0\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w2\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2) :: map_w2\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w2v\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2v) "
        ":: map_w2v\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w2vtrace\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2vtrace) "
        ":: map_w2vtrace\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_wchi\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_wchi) "
        ":: map_wchi\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_wtheta\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_wtheta) "
        ":: map_wtheta\n"
        "      INTEGER(KIND=i_def), intent(in) :: undf_w0, undf_w2, ndf_w1, "
        "ndf_w3, undf_wtheta, ndf_w2h, undf_w2v, ndf_w2broken, undf_wchi, "
        "ndf_w2trace, undf_w2vtrace, ndf_w2htrace\n" in generated_code)

    for space in ["w2h", "wtheta"]:
        assert (f"REAL(KIND=r_def), intent(in), dimension(3,ndf_w0,"
                f"ndf_{space}) :: diff_basis_w0_on_{space}" in generated_code)
        assert (f"REAL(KIND=r_def), intent(in), dimension(1,ndf_w2,"
                f"ndf_{space}) :: diff_basis_w2_on_{space}" in generated_code)
        assert (f"REAL(KIND=r_def), intent(in), dimension(3,ndf_w1,"
                f"ndf_{space}) :: diff_basis_w1_on_{space}" in generated_code)
        assert (f"REAL(KIND=r_def), intent(in), dimension(3,ndf_w3,"
                f"ndf_{space}) :: diff_basis_w3_on_{space}" in generated_code)
        assert (f"REAL(KIND=r_def), intent(in), dimension(3,ndf_wtheta,"
                f"ndf_{space}) :: diff_basis_wtheta_on_{space}" in
                generated_code)
        assert (f"REAL(KIND=r_def), intent(in), dimension(1,ndf_w2h,"
                f"ndf_{space}) :: diff_basis_w2h_on_{space}" in generated_code)
        assert (f"REAL(KIND=r_def), intent(in), dimension(1,ndf_w2v,"
                f"ndf_{space}) :: diff_basis_w2v_on_{space}" in generated_code)
        assert (f"REAL(KIND=r_def), intent(in), dimension(1,ndf_w2broken,"
                f"ndf_{space}) :: diff_basis_w2broken_on_{space}" in
                generated_code)
        assert (f"REAL(KIND=r_def), intent(in), dimension(3,ndf_wchi,"
                f"ndf_{space}) :: diff_basis_wchi_on_{space}" in
                generated_code)
        assert (f"REAL(KIND=r_def), intent(in), dimension(3,ndf_w2trace,"
                f"ndf_{space}) :: diff_basis_w2trace_on_{space}" in
                generated_code)
        assert (f"REAL(KIND=r_def), intent(in), dimension(3,ndf_w2vtrace,"
                f"ndf_{space}) :: diff_basis_w2vtrace_on_{space}" in
                generated_code)
        assert (f"REAL(KIND=r_def), intent(in), dimension(3,ndf_w2htrace,"
                f"ndf_{space}) :: diff_basis_w2htrace_on_{space}" in
                generated_code)


DIFF_BASIS_UNSUPPORTED_SPACE = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(1) =                           &
          (/ arg_type(gh_field, gh_real, gh_inc, any_space_1) &
           /)
     type(func_type), meta_funcs(1) =              &
          (/ func_type(any_space_1, gh_diff_basis) &
           /)
     integer :: operates_on = cell_column
     integer :: gh_shape = gh_quadrature_XYoZ
   contains
     procedure, nopass :: code => dummy_code
  end type dummy_type
contains
  subroutine dummy_code()
  end subroutine dummy_code
end module dummy_mod
'''


def test_diff_basis_unsupp_space():
    ''' Test that an error is raised when a differential basis
    function is on an unsupported space (currently any_space_*
    and any_discontinuous_space_*) in kernel stub generation.
    This information will be passed from the PSy layer to the
    kernels (see issue #461). '''
    # Test any_space_*
    ast = fpapi.parse(DIFF_BASIS_UNSUPPORTED_SPACE, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    with pytest.raises(GenerationError) as excinfo:
        _ = kernel.gen_stub
    const = LFRicConstants()
    assert ("Unsupported space for differential basis function, expecting one "
            "of " + str(const.VALID_FUNCTION_SPACES) + " but found "
            "'any_space_1'" in str(excinfo.value))
    # Test any_discontinuous_space_*
    code = DIFF_BASIS_UNSUPPORTED_SPACE.replace("any_space_1",
                                                "any_discontinuous_space_5")
    code = code.replace("gh_inc", "gh_readwrite")
    ast = fpapi.parse(code, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    with pytest.raises(GenerationError) as excinfo:
        _ = kernel.gen_stub
    assert ("Unsupported space for differential basis function"
            in str(excinfo.value))
    assert "but found 'any_discontinuous_space_5'" in str(excinfo.value)


def test_dynbasisfns_unsupp_qr(monkeypatch):
    ''' Check that the expected error is raised in
    DynBasisFunctions._stub_declarations() if an un-supported quadrature
    shape is encountered. '''
    ast = fpapi.parse(DIFF_BASIS, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    dbasis = DynBasisFunctions(kernel)
    monkeypatch.setattr(
        dbasis, "_qr_vars", {"unsupported-shape": None})
    with pytest.raises(InternalError) as err:
        dbasis._stub_declarations(ModuleGen(name="my_mod"))
    assert ("Quadrature shapes other than ['gh_quadrature_xyoz', "
            "'gh_quadrature_face', 'gh_quadrature_edge'] are not yet "
            "supported - got: 'unsupported-shape'" in str(err.value))


def test_dynbasisfns_declns(monkeypatch):
    ''' Check the various internal errors that
    DynBasisFunctions._basis_fn_declns can raise. '''
    ast = fpapi.parse(DIFF_BASIS, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kernel = LFRicKern()
    kernel.load_meta(metadata)
    dbasis = DynBasisFunctions(kernel)
    # Missing name for qr variable
    dbasis._basis_fns[0]['qr_var'] = None
    with pytest.raises(InternalError) as err:
        dbasis._basis_fn_declns()
    assert ("Quadrature 'gh_quadrature_xyoz' is required but have no name for "
            "the associated Quadrature object" in str(err.value))
    dbasis._basis_fns[0]['qr_var'] = "qr_xyoz"
    # Invalid quadrature shape
    dbasis._basis_fns[0]["shape"] = "not-a-shape"
    with pytest.raises(InternalError) as err:
        dbasis._basis_fn_declns()
    assert ("Unrecognised evaluator shape: 'not-a-shape'. Should"
            in str(err.value))
    # No Kernel object
    monkeypatch.setattr(dbasis, "_kernel", None)
    dbasis._basis_fns[0]['type'] = "basis"
    with pytest.raises(InternalError) as err:
        dbasis._basis_fn_declns()
    assert ("basis functions but do not have either a Kernel or an Invoke. "
            "Should be" in str(err.value))
    dbasis._basis_fns[0]['type'] = "diff-basis"
    with pytest.raises(InternalError) as err:
        dbasis._basis_fn_declns()
    assert ("differential basis functions but do not have either a Kernel or "
            "an Invoke. Should be" in str(err.value))
    # Unsupported type of basis function
    for fun in dbasis._basis_fns:
        fun['type'] = "broken"
    with pytest.raises(InternalError) as err:
        dbasis._basis_fn_declns()
    assert ("Unrecognised type of basis function: 'broken'. Should be "
            "either 'basis' or 'diff-basis'" in str(err.value))
