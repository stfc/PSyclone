# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017, Science and Technology Facilities Council
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2016.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
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
# Author R. Ford and A. R. Porter, STFC Daresbury Lab

''' Module containing py.test tests for functionality related to quadrature
and evaluators in the LFRic API '''

import os
import pytest
import fparser
from fparser import api as fpapi
from psyclone.parse import parse, ParseError
from psyclone.psyGen import PSyFactory

# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")

CODE = '''
module testkern_eval
  type, extends(kernel_type) :: testkern_eval_type
    type(arg_type) :: meta_args(2) = (/                                  &
         arg_type(GH_FIELD,   GH_INC,  W0),                              &
         arg_type(GH_FIELD,   GH_READ, W1)                               &
         /)
    type(func_type) :: meta_funcs(2) = (/                                &
         func_type(W0, GH_BASIS),                                        &
         func_type(W1, GH_DIFF_BASIS)                                    &
         /)
    integer, parameter :: gh_shape = gh_evaluator
    integer, parameter :: iterates_over = cells
  contains
    procedure() :: code => testkern_eval_code
  end type testkern_eval_type
contains
  subroutine testkern_eval_code()
  end subroutine testkern_eval_code
end module testkern_eval
'''


def test_eval_mdata():
    ''' Check that we recognise "evaluator" as a valid gh_shape '''
    from psyclone.dynamo0p3 import DynKernMetadata
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(CODE, ignore_comments=False)
    dkm = DynKernMetadata(ast, name="testkern_eval_type")
    assert dkm.get_integer_variable('gh_shape') == 'gh_evaluator'


def test_single_updated_arg():
    ''' Check that we reject any kernel requiring an evaluator
    if it writes to more than one argument '''
    from psyclone.dynamo0p3 import DynKernMetadata
    fparser.logging.disable('CRITICAL')
    # Change the access of the read-only argument
    code = CODE.replace("GH_READ", "GH_WRITE", 1)
    ast = fpapi.parse(code, ignore_comments=False)
    with pytest.raises(ParseError) as excinfo:
        _ = DynKernMetadata(ast, name="testkern_eval_type")
    assert ("kernel testkern_eval_type requires gh_evaluator and updates "
            "2 arguments") in str(excinfo)
    # Change the gh_shape element to specify quadrature and then test again
    qr_code = code.replace("gh_evaluator","gh_quadrature_xyoz")
    ast = fpapi.parse(qr_code, ignore_comments=False)
    dkm = DynKernMetadata(ast, name="testkern_eval_type")
    assert dkm.get_integer_variable('gh_shape') == "gh_quadrature_xyoz"


def test_single_kern_eval():
    ''' Check that we generate correct code for a single kernel that
    requires both basis and differential basis functions for an
    evaluator '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "6.1_eval_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    gen_code = str(psy.gen)
    print gen_code
    # First, check the declarations
    expected_decl = (
        "    SUBROUTINE invoke_0_testkern_eval_type(f0, f1)\n"
        "      USE testkern_eval, ONLY: testkern_eval_code\n"
        "      USE evaluate_function_mod, ONLY: BASIS, DIFF_BASIS\n"
        "      TYPE(field_type), intent(inout) :: f0\n"
        "      TYPE(field_type), intent(in) :: f1\n"
        "      INTEGER cell\n"
        "      INTEGER df_w1, df_w0, df_nodal\n"
        "      REAL(KIND=r_def), allocatable :: basis_w0(:,:,:,:), "
        "diff_basis_w1(:,:,:,:)\n"
        "      INTEGER ndf_nodal_w0, dim_w0, diff_dim_w1\n"
        "      REAL(KIND=r_def), pointer :: nodes_w0(:,:) => null()\n"
        "      INTEGER ndf_w0, undf_w0, ndf_w1, undf_w1\n"
        "      INTEGER nlayers\n"
        "      TYPE(field_proxy_type) f0_proxy, f1_proxy\n"
        "      INTEGER, pointer :: map_w0(:,:) => null(), "
        "map_w1(:,:) => null()\n")
    assert expected_decl in gen_code
    # Second, check the executable statements
    expected_code = (
        "      !\n"
        "      ! Initialise field and/or operator proxies\n"
        "      !\n"
        "      f0_proxy = f0%get_proxy()\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f0_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w0 => f0_proxy%vspace%get_whole_dofmap()\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w0\n"
        "      !\n"
        "      ndf_w0 = f0_proxy%vspace%get_ndf()\n"
        "      undf_w0 = f0_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w1\n"
        "      !\n"
        "      ndf_w1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_w1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise evaluator-related quantities using the field(s) "
        "that are written to\n"
        "      !\n"
        "      ndf_nodal_w0 = f0_proxy%vspace%get_ndf()\n"
        "      nodes_w0 => f0_proxy%vspace%get_nodes()\n"
        "      !\n"
        "      ! Allocate basis arrays\n"
        "      !\n"
        "      dim_w0 = f0_proxy%vspace%get_dim_space()\n"
        "      ALLOCATE (basis_w0(dim_w0, ndf_w0, ndf_nodal_w0))\n"
        "      !\n"
        "      ! Allocate differential basis arrays\n"
        "      !\n"
        "      diff_dim_w1 = f1_proxy%vspace%get_dim_space_diff()\n"
        "      ALLOCATE (diff_basis_w1(diff_dim_w1, ndf_w1, ndf_nodal_w0))\n"
        "      !\n"
        "      ! Compute basis arrays\n"
        "      !\n"
        "      DO df_nodal=1,ndf_nodal_w0\n"
        "        DO df_w0=1,ndf_w0\n"
        "          basis_w0(:,df_w0,df_nodal) = "
        "f0_proxy%vspace%evaluate_function(BASIS,df_w0,nodes_w0(:,df_nodal))\n"
        "        END DO \n"
        "      END DO \n"
        "      !\n"
        "      ! Compute differential basis arrays\n"
        "      !\n"
        "      DO df_nodal=1,ndf_nodal_w0\n"
        "        DO df_w1=1,ndf_w1\n"
        "          diff_basis_w1(:,df_w1,df_nodal) = f1_proxy%vspace%"
        "evaluate_function(DIFF_BASIS,df_w1,nodes_w0(:,df_nodal))\n"
        "        END DO \n"
        "      END DO \n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO cell=1,f0_proxy%vspace%get_ncell()\n"
        "        !\n"
        "        CALL testkern_eval_code(nlayers, f0_proxy%data, "
        "f1_proxy%data, ndf_w0, undf_w0, map_w0(:,cell), basis_w0, "
        "ndf_w1, undf_w1, map_w1(:,cell), diff_basis_w1)\n"
        "      END DO \n"
        "      !\n"
    )
    assert expected_code in gen_code


def test_two_qr():
    ''' Check that we handle an invoke containing two kernels that each
    require quadrature '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.1.2_single_invoke_2qr.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    gen_code = str(psy.gen)
    print gen_code
    expected_declns = (
        "    SUBROUTINE invoke_0(f1, f2, m1, a, m2, istp, g1, g2, n1, b, "
        "n2, qr, qr2)\n"
        "      USE testkern_qr, ONLY: testkern_qr_code\n"
        "      REAL(KIND=r_def), intent(in) :: a, b\n"
        "      INTEGER, intent(in) :: istp\n"
        "      TYPE(field_type), intent(inout) :: f1, g1\n"
        "      TYPE(field_type), intent(in) :: f2, m1, m2, g2, n1, n2\n"
        "      TYPE(quadrature_type), intent(in) :: qr, qr2\n"
        "      INTEGER cell\n"
        "      REAL(KIND=r_def), allocatable :: basis_w1_qr(:,:,:,:), "
        "basis_w3_qr(:,:,:,:), basis_w1_qr2(:,:,:,:), basis_w3_qr2(:,:,:,:), "
        "diff_basis_w2_qr(:,:,:,:), diff_basis_w3_qr(:,:,:,:), "
        "diff_basis_w2_qr2(:,:,:,:), diff_basis_w3_qr2(:,:,:,:)\n"
        "      INTEGER dim_w1, dim_w3, diff_dim_w2, diff_dim_w3\n"
        "      REAL(KIND=r_def), pointer :: wv_qr(:) => null(), "
        "zp_qr(:) => null(), wh_qr(:) => null()\n"
        "      REAL(KIND=r_def), pointer :: xp_qr(:,:) => null()\n"
        "      INTEGER nqp_h_qr, nqp_v_qr\n"
        "      REAL(KIND=r_def), pointer :: wv_qr2(:) => null(), "
        "zp_qr2(:) => null(), wh_qr2(:) => null()\n"
        "      REAL(KIND=r_def), pointer :: xp_qr2(:,:) => null()\n"
        "      INTEGER nqp_h_qr2, nqp_v_qr2\n"
        "      INTEGER ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, undf_w3\n"
        "      INTEGER nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, "
        "m2_proxy, g1_proxy, g2_proxy, n1_proxy, n2_proxy\n"
        "      INTEGER, pointer :: map_w2(:,:) => null(), "
        "map_w3(:,:) => null(), map_w1(:,:) => null()\n"
    )
    assert expected_declns in gen_code
    expected_code = (
        "      !\n"
        "      ! Look-up quadrature variables\n"
        "      !\n"
        "      wv_qr2 => qr2%get_wqp_v()\n"
        "      xp_qr2 => qr2%get_xqp_h()\n"
        "      zp_qr2 => qr2%get_xqp_v()\n"
        "      wh_qr2 => qr2%get_wqp_h()\n"
        "      nqp_h_qr2 = qr2%get_nqp_h()\n"
        "      nqp_v_qr2 = qr2%get_nqp_v()\n"
        "      wv_qr => qr%get_wqp_v()\n"
        "      xp_qr => qr%get_xqp_h()\n"
        "      zp_qr => qr%get_xqp_v()\n"
        "      wh_qr => qr%get_wqp_h()\n"
        "      nqp_h_qr = qr%get_nqp_h()\n"
        "      nqp_v_qr = qr%get_nqp_v()\n"
        "      !\n"
        "      ! Allocate basis arrays\n"
        "      !\n"
        "      dim_w1 = f1_proxy%vspace%get_dim_space()\n"
        "      ALLOCATE (basis_w1_qr(dim_w1, ndf_w1, nqp_h_qr, nqp_v_qr))\n"
        "      dim_w3 = m2_proxy%vspace%get_dim_space()\n"
        "      ALLOCATE (basis_w3_qr(dim_w3, ndf_w3, nqp_h_qr, nqp_v_qr))\n"
        "      ALLOCATE (basis_w1_qr2(dim_w1, ndf_w1, nqp_h_qr2, nqp_v_qr2))\n"
        "      ALLOCATE (basis_w3_qr2(dim_w3, ndf_w3, nqp_h_qr2, nqp_v_qr2))\n"
        "      !\n"
        "      ! Allocate differential basis arrays\n"
        "      !\n"
        "      diff_dim_w2 = f2_proxy%vspace%get_dim_space_diff()\n"
        "      ALLOCATE (diff_basis_w2_qr(diff_dim_w2, ndf_w2, nqp_h_qr, "
        "nqp_v_qr))\n"
        "      diff_dim_w3 = m2_proxy%vspace%get_dim_space_diff()\n"
        "      ALLOCATE (diff_basis_w3_qr(diff_dim_w3, ndf_w3, nqp_h_qr, "
        "nqp_v_qr))\n"
        "      ALLOCATE (diff_basis_w2_qr2(diff_dim_w2, ndf_w2, nqp_h_qr2, "
        "nqp_v_qr2))\n"
        "      ALLOCATE (diff_basis_w3_qr2(diff_dim_w3, ndf_w3, nqp_h_qr2, "
        "nqp_v_qr2))\n"
        "      !\n"
        "      ! Compute basis arrays\n"
        "      !\n"
        "      CALL f1_proxy%vspace%compute_basis_function("
        "basis_w1_qr, ndf_w1, nqp_h_qr, nqp_v_qr, xp_qr, zp_qr)\n"
        "      CALL m2_proxy%vspace%compute_basis_function("
        "basis_w3_qr, ndf_w3, nqp_h_qr, nqp_v_qr, xp_qr, zp_qr)\n"
        "      CALL g1_proxy%vspace%compute_basis_function("
        "basis_w1_qr2, ndf_w1, nqp_h_qr2, nqp_v_qr2, xp_qr2, zp_qr2)\n"
        "      CALL n2_proxy%vspace%compute_basis_function("
        "basis_w3_qr2, ndf_w3, nqp_h_qr2, nqp_v_qr2, xp_qr2, zp_qr2)\n"
        "      !\n"
        "      ! Compute differential basis arrays\n"
        "      !\n"
        "      CALL f2_proxy%vspace%compute_diff_basis_function("
        "diff_basis_w2_qr, ndf_w2, nqp_h_qr, nqp_v_qr, xp_qr, zp_qr)\n"
        "      CALL m2_proxy%vspace%compute_diff_basis_function("
        "diff_basis_w3_qr, ndf_w3, nqp_h_qr, nqp_v_qr, xp_qr, zp_qr)\n"
        "      CALL g2_proxy%vspace%compute_diff_basis_function("
        "diff_basis_w2_qr2, ndf_w2, nqp_h_qr2, nqp_v_qr2, xp_qr2, zp_qr2)\n"
        "      CALL n2_proxy%vspace%compute_diff_basis_function("
        "diff_basis_w3_qr2, ndf_w3, nqp_h_qr2, nqp_v_qr2, xp_qr2, zp_qr2)\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO cell=1,f1_proxy%vspace%get_ncell()\n"
        "        !\n"
        "        CALL testkern_qr_code(nlayers, f1_proxy%data, f2_proxy%data, "
        "m1_proxy%data, a, m2_proxy%data, istp, "
        "ndf_w1, undf_w1, map_w1(:,cell), basis_w1_qr, "
        "ndf_w2, undf_w2, map_w2(:,cell), diff_basis_w2_qr, "
        "ndf_w3, undf_w3, map_w3(:,cell), basis_w3_qr, diff_basis_w3_qr, "
        "nqp_h_qr, nqp_v_qr, wh_qr, wv_qr)\n"
        "      END DO \n"
        "      DO cell=1,g1_proxy%vspace%get_ncell()\n"
        "        !\n"
        "        CALL testkern_qr_code(nlayers, g1_proxy%data, g2_proxy%data, "
        "n1_proxy%data, b, n2_proxy%data, istp, "
        "ndf_w1, undf_w1, map_w1(:,cell), basis_w1_qr2, "
        "ndf_w2, undf_w2, map_w2(:,cell), diff_basis_w2_qr2, "
        "ndf_w3, undf_w3, map_w3(:,cell), basis_w3_qr2, diff_basis_w3_qr2, "
        "nqp_h_qr2, nqp_v_qr2, wh_qr2, wv_qr2)\n"
        "      END DO \n"
        "      !\n"
        "      ! Deallocate basis arrays\n"
        "      !\n"
        "      DEALLOCATE (basis_w1_qr, basis_w3_qr, basis_w1_qr2, "
        "basis_w3_qr2, diff_basis_w2_qr, diff_basis_w3_qr, "
        "diff_basis_w2_qr2, diff_basis_w3_qr2)\n"
    )
    assert expected_code in gen_code


def test_anyw2():
    '''Check generated code works correctly when we have any_w2 fields
    and basis functions'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "21.2_single_invoke_multi_anyw2_basis.f90"),
        api="dynamo0.3")
    for dist_mem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        generated_code = str(psy.gen)
        print generated_code
        output = (
            "      ! Initialise number of DoFs for any_w2\n"
            "      !\n"
            "      ndf_any_w2 = f1_proxy%vspace%get_ndf()\n"
            "      undf_any_w2 = f1_proxy%vspace%get_undf()\n"
            "      !\n"
            "      ! Look-up quadrature variables\n"
            "      !\n"
            "      wv_qr => qr%get_wqp_v()\n"
            "      xp_qr => qr%get_xqp_h()\n"
            "      zp_qr => qr%get_xqp_v()\n"
            "      wh_qr => qr%get_wqp_h()\n"
            "      nqp_h_qr = qr%get_nqp_h()\n"
            "      nqp_v_qr = qr%get_nqp_v()\n"
            "      !\n"
            "      ! Allocate basis arrays\n"
            "      !\n"
            "      dim_any_w2 = f1_proxy%vspace%get_dim_space()\n"
            "      ALLOCATE (basis_any_w2_qr(dim_any_w2, ndf_any_w2, "
            "nqp_h_qr, nqp_v_qr))\n"
            "      !\n"
            "      ! Allocate differential basis arrays\n"
            "      !\n"
            "      diff_dim_any_w2 = f1_proxy%vspace%"
            "get_dim_space_diff()\n"
            "      ALLOCATE (diff_basis_any_w2_qr(diff_dim_any_w2, "
            "ndf_any_w2, nqp_h_qr, nqp_v_qr))\n"
            "      !\n"
            "      ! Compute basis arrays\n"
            "      !\n"
            "      CALL f1_proxy%vspace%compute_basis_function("
            "basis_any_w2_qr, ndf_any_w2, nqp_h_qr, nqp_v_qr, xp_qr, zp_qr)\n"
            "      !\n"
            "      ! Compute differential basis arrays\n"
            "      !\n"
            "      CALL f1_proxy%vspace%compute_diff_basis_function("
            "diff_basis_any_w2_qr, ndf_any_w2, nqp_h_qr, nqp_v_qr, xp_qr, "
            "zp_qr)")
        assert output in generated_code


def test_qr_plus_eval():
    ''' Check that we handle an invoke containing two kernels, one
    requiring quadrature and one requiring an evaluator '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "6.2_qr_eval_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    gen_code = str(psy.gen)
    print gen_code
    output_decls = (
        "    SUBROUTINE invoke_0(f0, f1, f2, m1, a, m2, istp, qr)\n"
        "      USE testkern_qr, ONLY: testkern_qr_code\n"
        "      USE testkern_eval, ONLY: testkern_eval_code\n"
        "      USE evaluate_function_mod, ONLY: BASIS, DIFF_BASIS\n"
        "      REAL(KIND=r_def), intent(in) :: a\n"
        "      INTEGER, intent(in) :: istp\n"
        "      TYPE(field_type), intent(inout) :: f0, f1\n"
        "      TYPE(field_type), intent(in) :: f2, m1, m2\n"
        "      TYPE(quadrature_type), intent(in) :: qr\n"
        "      INTEGER cell\n"
        "      INTEGER df_w1, df_w0, df_nodal\n"
        "      REAL(KIND=r_def), allocatable :: basis_w0(:,:,:,:), "
        "basis_w1_qr(:,:,:,:), basis_w3_qr(:,:,:,:), diff_basis_w1(:,:,:,:), "
        "diff_basis_w2_qr(:,:,:,:), diff_basis_w3_qr(:,:,:,:)\n"
        "      INTEGER ndf_nodal_w0, dim_w0, dim_w1, dim_w3, diff_dim_w1, "
        "diff_dim_w2, diff_dim_w3\n"
        "      REAL(KIND=r_def), pointer :: nodes_w0(:,:) => null()\n"
        "      REAL(KIND=r_def), pointer :: wv_qr(:) => null(), "
        "zp_qr(:) => null(), wh_qr(:) => null()\n"
        "      REAL(KIND=r_def), pointer :: xp_qr(:,:) => null()\n"
        "      INTEGER nqp_h_qr, nqp_v_qr\n"
        "      INTEGER ndf_w0, undf_w0, ndf_w1, undf_w1, ndf_w2, undf_w2, "
        "ndf_w3, undf_w3\n"
        "      INTEGER nlayers\n"
        "      TYPE(field_proxy_type) f0_proxy, f1_proxy, f2_proxy, "
        "m1_proxy, m2_proxy\n"
        "      INTEGER, pointer :: map_w2(:,:) => null(), "
        "map_w3(:,:) => null(), map_w0(:,:) => null(), map_w1(:,:) => null()\n")
    assert output_decls in gen_code
    output_setup = (
        "      ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "      undf_w3 = m2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Look-up quadrature variables\n"
        "      !\n"
        "      wv_qr => qr%get_wqp_v()\n"
        "      xp_qr => qr%get_xqp_h()\n"
        "      zp_qr => qr%get_xqp_v()\n"
        "      wh_qr => qr%get_wqp_h()\n"
        "      nqp_h_qr = qr%get_nqp_h()\n"
        "      nqp_v_qr = qr%get_nqp_v()\n"
        "      !\n"
        "      ! Initialise evaluator-related quantities using the "
        "field(s) that are written to\n"
        "      !\n"
        "      ndf_nodal_w0 = f0_proxy%vspace%get_ndf()\n"
        "      nodes_w0 => f0_proxy%vspace%get_nodes()\n"
        "      !\n"
        "      ! Allocate basis arrays\n"
        "      !\n"
        "      dim_w0 = f0_proxy%vspace%get_dim_space()\n"
        "      ALLOCATE (basis_w0(dim_w0, ndf_w0, ndf_nodal_w0))\n"
        "      dim_w1 = f1_proxy%vspace%get_dim_space()\n"
        "      ALLOCATE (basis_w1_qr(dim_w1, ndf_w1, nqp_h_qr, nqp_v_qr))\n"
        "      dim_w3 = m2_proxy%vspace%get_dim_space()\n"
        "      ALLOCATE (basis_w3_qr(dim_w3, ndf_w3, nqp_h_qr, nqp_v_qr))\n"
        "      !\n"
        "      ! Allocate differential basis arrays\n"
        "      !\n"
        "      diff_dim_w1 = f1_proxy%vspace%get_dim_space_diff()\n"
        "      ALLOCATE (diff_basis_w1(diff_dim_w1, ndf_w1, ndf_nodal_w0))\n"
        "      diff_dim_w2 = f2_proxy%vspace%get_dim_space_diff()\n"
        "      ALLOCATE (diff_basis_w2_qr(diff_dim_w2, ndf_w2, nqp_h_qr, "
        "nqp_v_qr))\n"
        "      diff_dim_w3 = m2_proxy%vspace%get_dim_space_diff()\n"
        "      ALLOCATE (diff_basis_w3_qr(diff_dim_w3, ndf_w3, nqp_h_qr, "
        "nqp_v_qr))\n"
        "      !\n"
        "      ! Compute basis arrays\n"
        "      !\n"
        "      DO df_nodal=1,ndf_nodal_w0\n"
        "        DO df_w0=1,ndf_w0\n"
        "          basis_w0(:,df_w0,df_nodal) = f0_proxy%vspace%evaluate_function(BASIS,df_w0,nodes_w0(:,df_nodal))\n"
        "        END DO \n"
        "      END DO \n"
        "      CALL f1_proxy%vspace%compute_basis_function(basis_w1_qr, ndf_w1, nqp_h_qr, nqp_v_qr, xp_qr, zp_qr)\n"
        "      CALL m2_proxy%vspace%compute_basis_function(basis_w3_qr, ndf_w3, nqp_h_qr, nqp_v_qr, xp_qr, zp_qr)\n"
        "      !\n"
        "      ! Compute differential basis arrays\n"
        "      !\n"
        "      DO df_nodal=1,ndf_nodal_w0\n"
        "        DO df_w1=1,ndf_w1\n"
        "          diff_basis_w1(:,df_w1,df_nodal) = f1_proxy%vspace%evaluate_function(DIFF_BASIS,df_w1,nodes_w0(:,df_nodal))\n"
        "        END DO \n"
        "      END DO \n"
        "      CALL f2_proxy%vspace%compute_diff_basis_function(diff_basis_w2_qr, ndf_w2, nqp_h_qr, nqp_v_qr, xp_qr, zp_qr)\n"
        "      CALL m2_proxy%vspace%compute_diff_basis_function(diff_basis_w3_qr, ndf_w3, nqp_h_qr, nqp_v_qr, xp_qr, zp_qr)\n")


def test_two_eval_same_space():
    ''' Check that we generate correct code when two kernels in an invoke
    both require evaluators and the arguments that are written to are on
    the same space '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "6.3_2eval_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    gen_code = str(psy.gen)
    print gen_code
    output_init = (
        "      !\n"
        "      ! Initialise evaluator-related quantities using the field(s) "
        "that are written to\n"
        "      !\n"
        "      ndf_nodal_w0 = f0_proxy%vspace%get_ndf()\n"
        "      nodes_w0 => f0_proxy%vspace%get_nodes()\n"
        "      !\n"
        "      ! Allocate basis arrays\n"
        "      !\n"
        "      dim_w0 = f0_proxy%vspace%get_dim_space()\n"
        "      ALLOCATE (basis_w0(dim_w0, ndf_w0, ndf_nodal_w0))\n"
        "      !\n"
        "      ! Allocate differential basis arrays\n"
        "      !\n"
        "      diff_dim_w1 = f1_proxy%vspace%get_dim_space_diff()\n"
        "      ALLOCATE (diff_basis_w1(diff_dim_w1, ndf_w1, ndf_nodal_w0))\n")
    assert output_init in gen_code
    output_code = (
        "      !\n"
        "      ! Compute basis arrays\n"
        "      !\n"
        "      DO df_nodal=1,ndf_nodal_w0\n"
        "        DO df_w0=1,ndf_w0\n"
        "          basis_w0(:,df_w0,df_nodal) = f0_proxy%vspace%"
        "evaluate_function(BASIS,df_w0,nodes_w0(:,df_nodal))\n"
        "        END DO \n"
        "      END DO \n"
        "      !\n"
        "      ! Compute differential basis arrays\n"
        "      !\n"
        "      DO df_nodal=1,ndf_nodal_w0\n"
        "        DO df_w1=1,ndf_w1\n"
        "          diff_basis_w1(:,df_w1,df_nodal) = f1_proxy%vspace%"
        "evaluate_function(DIFF_BASIS,df_w1,nodes_w0(:,df_nodal))\n"
        "        END DO \n"
        "      END DO \n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO cell=1,f0_proxy%vspace%get_ncell()\n"
        "        !\n"
        "        CALL testkern_eval_code(nlayers, f0_proxy%data, "
        "f1_proxy%data, ndf_w0, undf_w0, map_w0(:,cell), basis_w0, "
        "ndf_w1, undf_w1, map_w1(:,cell), diff_basis_w1)\n"
        "      END DO \n"
        "      DO cell=1,f2_proxy%vspace%get_ncell()\n"
        "        !\n"
        "        CALL testkern_eval_code(nlayers, f2_proxy%data, "
        "f3_proxy%data, ndf_w0, undf_w0, map_w0(:,cell), basis_w0, "
        "ndf_w1, undf_w1, map_w1(:,cell), diff_basis_w1)\n"
        "      END DO \n"
    )
    assert output_code in gen_code


def test_two_eval_diff_space():
    ''' Check that we generate correct code when two kernels in an invoke
    both require evaluators and the arguments that are written to are on
    different spaces '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "6.4_2eval_op_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    gen_code = str(psy.gen)
    print gen_code
    assert 0
