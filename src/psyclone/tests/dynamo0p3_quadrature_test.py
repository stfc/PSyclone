# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017, Science and Technology Facilities Council
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
# Author R. W. Ford and A. R. Porter, STFC Daresbury Lab

''' Module containing py.test tests for functionality related to
quadrature in the LFRic API '''

# Since this is a file containing tests which often have to get in and
# change the internal state of objects we disable pylint's warning
# about such accesses
# pylint: disable=protected-access

import os
import pytest
from fparser import api as fpapi
from psyclone.parse import parse
from psyclone.psyGen import PSyFactory, GenerationError
from psyclone.dynamo0p3 import DynKernMetadata, DynKern
import utils

# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")
API = "dynamo0.3"


def test_field_xyoz(tmpdir, f90, f90flags):
    ''' Tests that a call, with a set of fields requiring XYoZ
    quadrature, produces correct code. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.1.0_single_invoke_xyoz_qr.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    generated_code = str(psy.gen)
    print generated_code

    if utils.TEST_COMPILE:
        assert utils.code_compiles(API, psy, tmpdir, f90, f90flags)

    output_decls = (
        "    SUBROUTINE invoke_0_testkern_qr_type(f1, f2, m1, a, m2, istp,"
        " qr)\n"
        "      USE testkern_qr, ONLY: testkern_qr_code\n"
        "      USE quadrature_xyoz_mod, ONLY: quadrature_xyoz_type, "
        "quadrature_xyoz_proxy_type\n"
        "      USE function_space_mod, ONLY: BASIS, DIFF_BASIS\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      REAL(KIND=r_def), intent(in) :: a\n"
        "      INTEGER, intent(in) :: istp\n"
        "      TYPE(field_type), intent(inout) :: f1\n"
        "      TYPE(field_type), intent(in) :: f2, m1, m2\n"
        "      TYPE(quadrature_xyoz_type), intent(in) :: qr\n"
        "      INTEGER cell\n"
        "      REAL(KIND=r_def), allocatable :: basis_w1_qr(:,:,:,:), "
        "basis_w3_qr(:,:,:,:), diff_basis_w2_qr(:,:,:,:), "
        "diff_basis_w3_qr(:,:,:,:)\n"
        "      INTEGER dim_w1, dim_w3, diff_dim_w2, diff_dim_w3\n"
        "      REAL(KIND=r_def), pointer :: weights_xy_qr(:) => null(), "
        "weights_z_qr(:) => null()\n"
        "      INTEGER np_xy_qr, np_z_qr\n"
        "      INTEGER ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, undf_w3\n"
        "      TYPE(mesh_type), pointer :: mesh => null()\n"
        "      INTEGER nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, m2_proxy\n"
        "      TYPE(quadrature_xyoz_proxy_type) qr_proxy\n"
        "      INTEGER, pointer :: map_w2(:,:) => null(), "
        "map_w3(:,:) => null(), map_w1(:,:) => null()\n")
    assert output_decls in generated_code
    init_output = (
        "      !\n"
        "      ! Initialise field and/or operator proxies\n"
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
        "      mesh => f1%get_mesh()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w1\n"
        "      !\n"
        "      ndf_w1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_w1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2\n"
        "      !\n"
        "      ndf_w2 = f2_proxy%vspace%get_ndf()\n"
        "      undf_w2 = f2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w3\n"
        "      !\n"
        "      ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "      undf_w3 = m2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Look-up quadrature variables\n"
        "      !\n"
        "      qr_proxy = qr%get_quadrature_proxy()\n"
        "      np_xy_qr = qr_proxy%np_xy\n"
        "      np_z_qr = qr_proxy%np_z\n"
        "      weights_xy_qr => qr_proxy%weights_xy\n"
        "      weights_z_qr => qr_proxy%weights_z\n")
    assert init_output in generated_code
    compute_output = (
        "      !\n"
        "      ! Allocate basis arrays\n"
        "      !\n"
        "      dim_w1 = f1_proxy%vspace%get_dim_space()\n"
        "      ALLOCATE (basis_w1_qr(dim_w1, ndf_w1, np_xy_qr, np_z_qr))\n"
        "      dim_w3 = m2_proxy%vspace%get_dim_space()\n"
        "      ALLOCATE (basis_w3_qr(dim_w3, ndf_w3, np_xy_qr, np_z_qr))\n"
        "      !\n"
        "      ! Allocate differential basis arrays\n"
        "      !\n"
        "      diff_dim_w2 = f2_proxy%vspace%get_dim_space_diff()\n"
        "      ALLOCATE (diff_basis_w2_qr(diff_dim_w2, ndf_w2, np_xy_qr, "
        "np_z_qr))\n"
        "      diff_dim_w3 = m2_proxy%vspace%get_dim_space_diff()\n"
        "      ALLOCATE (diff_basis_w3_qr(diff_dim_w3, ndf_w3, np_xy_qr, "
        "np_z_qr))\n"
        "      !\n"
        "      ! Compute basis arrays\n"
        "      !\n"
        "      CALL qr%compute_function(BASIS, f1_proxy%vspace, dim_w1, "
        "ndf_w1, basis_w1_qr)\n"
        "      CALL qr%compute_function(BASIS, m2_proxy%vspace, dim_w3, "
        "ndf_w3, basis_w3_qr)\n"
        "      !\n"
        "      ! Compute differential basis arrays\n"
        "      !\n"
        "      CALL qr%compute_function(DIFF_BASIS, f2_proxy%vspace, "
        "diff_dim_w2, ndf_w2, diff_basis_w2_qr)\n"
        "      CALL qr%compute_function(DIFF_BASIS, m2_proxy%vspace, "
        "diff_dim_w3, ndf_w3, diff_basis_w3_qr)\n"
        "      !\n"
        "      ! Call kernels and communication routines\n"
        "      !\n"
        "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        CALL testkern_qr_code(nlayers, f1_proxy%data, f2_proxy%data, "
        "m1_proxy%data, a, m2_proxy%data, istp, ndf_w1, undf_w1, "
        "map_w1(:,cell), basis_w1_qr, ndf_w2, undf_w2, map_w2(:,cell), "
        "diff_basis_w2_qr, ndf_w3, undf_w3, map_w3(:,cell), basis_w3_qr, "
        "diff_basis_w3_qr, np_xy_qr, np_z_qr, weights_xy_qr, weights_z_qr)\n"
        "      END DO \n"
        "      !\n"
        "      ! Set halos dirty/clean for fields modified in the above loop\n"
        "      !\n"
        "      CALL f1_proxy%set_dirty()\n"
        "      !\n"
        "      !\n"
        "      ! Deallocate basis arrays\n"
        "      !\n"
        "      DEALLOCATE (diff_basis_w2_qr, basis_w1_qr, basis_w3_qr, "
        "diff_basis_w3_qr)\n"
        "      !\n"
        "    END SUBROUTINE invoke_0_testkern_qr_type"
    )
    assert compute_output in generated_code


def test_field_qr_deref(tmpdir, f90, f90flags):
    ''' Tests that a call, with a set of fields requiring
    quadrature, produces correct code when the quadrature is supplied as the
    component of a derived type. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.1.1_single_invoke_qr_deref.f90"),
                           api="dynamo0.3")
    for dist_mem in [True, False]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)

        if utils.TEST_COMPILE:
            assert utils.code_compiles(API, psy, tmpdir, f90, f90flags)
        gen = str(psy.gen)
        print gen
        assert (
            "    SUBROUTINE invoke_0_testkern_qr_type(f1, f2, m1, a, m2, istp,"
            " qr_data)\n" in gen)
        assert "TYPE(quadrature_xyoz_type), intent(in) :: qr_data" in gen


def test_internal_qr_err(monkeypatch):
    ''' Check that internal error for unrecognised QR type is raised
    as expected '''
    from psyclone import dynamo0p3
    # Monkeypatch the list of valid quadrature and evaluator shapes so we
    # get past some of the earlier checks
    monkeypatch.setattr(dynamo0p3, "VALID_EVALUATOR_SHAPES",
                        value=["gh_quadrature_xyz", "gh_quadrature_xyoz",
                               "gh_quadrature_xoyoz", "gh_quadrature_wrong"])
    monkeypatch.setattr(dynamo0p3, "VALID_QUADRATURE_SHAPES",
                        value=["gh_quadrature_xyz", "gh_quadrature_xyoz",
                               "gh_quadrature_xoyoz", "gh_quadrature_wrong"])
    _, invoke_info = parse(os.path.join(BASE_PATH, "1.1.4_wrong_qr_shape.f90"),
                           api=API)
    with pytest.raises(GenerationError) as excinfo:
        _ = PSyFactory(API).create(invoke_info)
    assert ("Internal error: unsupported shape (gh_quadrature_wrong) "
            "found" in str(excinfo))


def test_dyninvokebasisfns(monkeypatch):
    ''' Check that we raise internal errors as required '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.1.0_single_invoke_xyoz_qr.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    # Get hold of a DynInvokeBasisFns object
    evaluator = psy.invokes.invoke_list[0].evaluators

    # Test the error check in dynamo0p3.qr_basis_alloc_args() by passing in a
    # dictionary containing an invalid shape entry
    basis_dict = {"shape": "gh_wrong_shape"}
    from psyclone import dynamo0p3
    with pytest.raises(GenerationError) as excinfo:
        _ = dynamo0p3.qr_basis_alloc_args("size1", basis_dict)
    assert "unrecognised shape (gh_wrong_shape) specified " in str(excinfo)

    # Monkey-patch it so that it doesn't have any quadrature args
    monkeypatch.setattr(evaluator, "_qr_vars", value=[])
    # Check that calling the various _initialise_... routines does nothing.
    # We pass parent=None so that if any of the routines get beyond the
    # initial check then they will fail.
    evaluator._initialise_xyz_qr(None)
    evaluator._initialise_xyoz_qr(None)
    evaluator._initialise_xoyoz_qr(None)


def test_dynkern_setup(monkeypatch):
    ''' Check that internal-consistency checks in DynKern._setup() work
    as expected '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.1.0_single_invoke_xyoz_qr.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    # Get hold of a DynKern object
    schedule = psy.invokes.invoke_list[0].schedule
    kern = schedule.children[3].children[0]
    # Monkeypatch a couple of __init__ routines so that we can get past
    # them in the _setup() routine.
    from psyclone.psyGen import Kern
    monkeypatch.setattr(Kern, "__init__",
                        lambda me, ktype, kcall, parent, check: None)
    from psyclone.parse import KernelCall
    monkeypatch.setattr(KernelCall, "__init__",
                        lambda me, mname, ktype, args: None)
    # Break the shape of the quadrature for this kernel
    monkeypatch.setattr(kern, "_eval_shape", value="gh_wrong_shape")
    # Rather than try and mock-up a DynKernMetadata object, it's easier
    # to make one properly by parsing the kernel code.
    ast = fpapi.parse(os.path.join(BASE_PATH, "testkern_qr.F90"),
                      ignore_comments=False)
    name = "testkern_qr_type"
    dkm = DynKernMetadata(ast, name=name)
    # Finally, call the _setup() method
    with pytest.raises(GenerationError) as excinfo:
        kern._setup(dkm, "my module", None, None)
    assert ("Internal error: evaluator shape 'gh_wrong_shape' is not "
            "recognised" in str(excinfo))


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
     integer, parameter :: gh_shape = gh_quadrature_xyoz
   contains
     procedure() :: code => dummy_code
  end type dummy_type
contains
  subroutine dummy_code()
  end subroutine dummy_code
end module dummy_mod
'''


def test_qr_basis_stub():
    ''' Test that basis functions for quadrature are handled correctly for
    kernel stubs '''
    ast = fpapi.parse(BASIS, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    generated_code = str(kernel.gen_stub)
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
        "basis_w2v, np_xy, np_z, weights_xy, weights_z)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: cell\n"
        "      INTEGER, intent(in) :: nlayers\n"
        "      INTEGER, intent(in) :: ndf_w0\n"
        "      INTEGER, intent(in) :: undf_w0\n"
        "      INTEGER, intent(in) :: ndf_w1\n"
        "      INTEGER, intent(in) :: ndf_w2\n"
        "      INTEGER, intent(in) :: undf_w2\n"
        "      INTEGER, intent(in) :: ndf_w3\n"
        "      INTEGER, intent(in) :: ndf_wtheta\n"
        "      INTEGER, intent(in) :: undf_wtheta\n"
        "      INTEGER, intent(in) :: ndf_w2h\n"
        "      INTEGER, intent(in) :: ndf_w2v\n"
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
        "      INTEGER, intent(in), dimension(ndf_w0) :: map_w0\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w0,np_xy,np_z) "
        ":: basis_w0\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w1,np_xy,np_z) "
        ":: basis_w1\n"
        "      INTEGER, intent(in), dimension(ndf_w2) :: map_w2\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w2,np_xy,np_z) "
        ":: basis_w2\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w3,np_xy,np_z) "
        ":: basis_w3\n"
        "      INTEGER, intent(in), dimension(ndf_wtheta) :: map_wtheta\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_wtheta,np_xy,"
        "np_z) :: basis_wtheta\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w2h,np_xy,np_z) "
        ":: basis_w2h\n"
        "      INTEGER, intent(in), dimension(ndf_w2v) :: map_w2v\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w2v,np_xy,np_z) "
        ":: basis_w2v\n"
        "      INTEGER, intent(in) :: np_xy, np_z\n"
        "      REAL(KIND=r_def), intent(in), dimension(np_xy) :: weights_xy\n"
        "      REAL(KIND=r_def), intent(in), dimension(np_z) :: weights_z\n"
        "    END SUBROUTINE dummy_code\n"
        "  END MODULE dummy_mod")
    print output
    print generated_code
    assert output in generated_code


def test_stub_basis_wrong_shape(monkeypatch):
    ''' Check that stub generation for a kernel requiring basis functions
    for quadrature raises the correct errors if the kernel meta-data is
    broken '''
    from psyclone import dynamo0p3
    ast = fpapi.parse(BASIS, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    monkeypatch.setattr(kernel, "_eval_shape",
                        value="gh_quadrature_wrong")
    with pytest.raises(GenerationError) as excinfo:
        _ = kernel.gen_stub
    assert (
        "Internal error: unrecognised evaluator shape (gh_quadrature_wrong)"
        in str(excinfo))
    monkeypatch.setattr(dynamo0p3, "VALID_QUADRATURE_SHAPES",
                        value=["gh_quadrature_xyz", "gh_quadrature_xyoz",
                               "gh_quadrature_xoyoz", "gh_quadrature_wrong"])
    with pytest.raises(GenerationError) as excinfo:
        _ = kernel.gen_stub
    assert ("shapes other than GH_QUADRATURE_XYoZ are not yet supported" in
            str(excinfo))


def test_stub_dbasis_wrong_shape(monkeypatch):  # pylint: disable=invalid-name
    ''' Check that stub generation for a kernel requiring differential basis
    functions for quadrature raises the correct errors if the kernel meta-data
    is broken '''
    from psyclone import dynamo0p3
    # Change meta-data to specify differential basis functions
    diff_basis = BASIS.replace("gh_basis", "gh_diff_basis")
    print diff_basis
    ast = fpapi.parse(diff_basis, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    monkeypatch.setattr(kernel, "_eval_shape",
                        value="gh_quadrature_wrong")
    with pytest.raises(GenerationError) as excinfo:
        _ = kernel.gen_stub
    assert (
        "Internal error: unrecognised evaluator shape (gh_quadrature_wrong)"
        in str(excinfo))
    monkeypatch.setattr(dynamo0p3, "VALID_QUADRATURE_SHAPES",
                        value=["gh_quadrature_xyz", "gh_quadrature_xyoz",
                               "gh_quadrature_xoyoz", "gh_quadrature_wrong"])
    with pytest.raises(NotImplementedError) as excinfo:
        _ = kernel.gen_stub
    assert ("diff-basis for quadrature shape 'gh_quadrature_wrong' not yet "
            "implemented" in str(excinfo))
