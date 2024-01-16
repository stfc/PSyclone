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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab;
#         I. Kavcic and A. Coughtrie, Met Office;
#         C. M. Maynard, Met Office/University of Reading;
#         J. Henrichs, Bureau of Meteorology.

'''
Module containing pytest tests for PSy-layer code generation for the
LFRic field arguments.
'''

import os

from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.tests.lfric_build import LFRicBuild


# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "dynamo0p3")
TEST_API = "dynamo0.3"


def test_field(tmpdir):
    ''' Tests that a call with a set of fields, no basis functions and
    no distributed memory, produces correct code.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    generated_code = psy.gen
    output = (
        "  MODULE single_invoke_psy\n"
        "    USE constants_mod, ONLY: r_def, i_def\n"
        "    USE field_mod, ONLY: field_type, field_proxy_type\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_testkern_type(a, f1, f2, m1, m2)\n"
        "      USE testkern_mod, ONLY: testkern_code\n"
        "      REAL(KIND=r_def), intent(in) :: a\n"
        "      TYPE(field_type), intent(in) :: f1, f2, m1, m2\n"
        "      INTEGER(KIND=i_def) cell\n"
        "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
        "      INTEGER(KIND=i_def) nlayers\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: m2_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: m1_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f2_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f1_data => null()\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, m2_proxy\n"
        "      INTEGER(KIND=i_def), pointer :: map_w1(:,:) => null(), "
        "map_w2(:,:) => null(), map_w3(:,:) => null()\n"
        "      INTEGER(KIND=i_def) ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, "
        "undf_w3\n"
        "      !\n"
        "      ! Initialise field and/or operator proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      f1_data => f1_proxy%data\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      f2_data => f2_proxy%data\n"
        "      m1_proxy = m1%get_proxy()\n"
        "      m1_data => m1_proxy%data\n"
        "      m2_proxy = m2%get_proxy()\n"
        "      m2_data => m2_proxy%data\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
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
        "      ! Set-up all of the loop bounds\n"
        "      !\n"
        "      loop0_start = 1\n"
        "      loop0_stop = f1_proxy%vspace%get_ncell()\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO cell=loop0_start,loop0_stop\n"
        "        !\n"
        "        CALL testkern_code(nlayers, a, f1_data, f2_data, "
        "m1_data, m2_data, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))\n"
        "      END DO\n"
        "      !\n"
        "    END SUBROUTINE invoke_0_testkern_type\n"
        "  END MODULE single_invoke_psy")
    assert output in str(generated_code)


def test_field_deref(tmpdir, dist_mem):
    ''' Tests that a call with a set of fields (some obtained by
    de-referencing derived types) and no basis functions produces
    correct code.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.13_single_invoke_field_deref.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    generated_code = str(psy.gen)
    output = (
        "    SUBROUTINE invoke_0_testkern_type(a, f1, est_f2, m1, "
        "est_m2)\n"
        "      USE testkern_mod, ONLY: testkern_code\n")
    assert output in generated_code
    if dist_mem:
        output = "      USE mesh_mod, ONLY: mesh_type\n"
        assert output in generated_code

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output = (
        "      REAL(KIND=r_def), intent(in) :: a\n"
        "      TYPE(field_type), intent(in) :: f1, est_f2, m1, est_m2\n"
        "      INTEGER(KIND=i_def) cell\n"
        "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
        "      INTEGER(KIND=i_def) nlayers\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: est_m2_data => "
        "null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: m1_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: est_f2_data => "
        "null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f1_data => null()\n"
        "      TYPE(field_proxy_type) f1_proxy, est_f2_proxy, m1_proxy, "
        "est_m2_proxy\n"
        "      INTEGER(KIND=i_def), pointer :: map_w1(:,:) => null(), "
        "map_w2(:,:) => null(), map_w3(:,:) => null()\n"
        "      INTEGER(KIND=i_def) ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, "
        "undf_w3\n")
    assert output in generated_code
    if dist_mem:
        output = "      TYPE(mesh_type), pointer :: mesh => null()\n"
        assert output in generated_code
    output = (
        "      !\n"
        "      ! Initialise field and/or operator proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      f1_data => f1_proxy%data\n"
        "      est_f2_proxy = est_f2%get_proxy()\n"
        "      est_f2_data => est_f2_proxy%data\n"
        "      m1_proxy = m1%get_proxy()\n"
        "      m1_data => m1_proxy%data\n"
        "      est_m2_proxy = est_m2%get_proxy()\n"
        "      est_m2_data => est_m2_proxy%data\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n")
    assert output in generated_code
    if dist_mem:
        output = (
            "      !\n"
            "      ! Create a mesh object\n"
            "      !\n"
            "      mesh => f1_proxy%vspace%get_mesh()\n"
        )
        assert output in generated_code
    output = (
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2 => est_f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => est_m2_proxy%vspace%get_whole_dofmap()\n"
        "      !\n")
    assert output in generated_code
    output = (
        "      ! Initialise number of DoFs for w1\n"
        "      !\n"
        "      ndf_w1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_w1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2\n"
        "      !\n"
        "      ndf_w2 = est_f2_proxy%vspace%get_ndf()\n"
        "      undf_w2 = est_f2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w3\n"
        "      !\n"
        "      ndf_w3 = est_m2_proxy%vspace%get_ndf()\n"
        "      undf_w3 = est_m2_proxy%vspace%get_undf()\n"
        "      !\n")
    assert output in generated_code
    if dist_mem:
        assert "loop0_stop = mesh%get_last_halo_cell(1)\n" in generated_code
        output = (
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      IF (f1_proxy%is_dirty(depth=1)) THEN\n"
            "        CALL f1_proxy%halo_exchange(depth=1)\n"
            "      END IF\n"
            "      !\n"
            "      IF (est_f2_proxy%is_dirty(depth=1)) THEN\n"
            "        CALL est_f2_proxy%halo_exchange(depth=1)\n"
            "      END IF\n"
            "      !\n"
            "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
            "        CALL m1_proxy%halo_exchange(depth=1)\n"
            "      END IF\n"
            "      !\n"
            "      IF (est_m2_proxy%is_dirty(depth=1)) THEN\n"
            "        CALL est_m2_proxy%halo_exchange(depth=1)\n"
            "      END IF\n"
            "      !\n"
            "      DO cell=loop0_start,loop0_stop\n")
        assert output in generated_code
    else:
        assert "loop0_stop = f1_proxy%vspace%get_ncell()\n" in generated_code
        output = (
            "      ! Call our kernels\n"
            "      !\n"
            "      DO cell=loop0_start,loop0_stop\n")
        assert output in generated_code
    output = (
        "        !\n"
        "        CALL testkern_code(nlayers, a, f1_data, est_f2_data, m1_data,"
        " est_m2_data, ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
        "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))\n"
        "      END DO\n")
    assert output in generated_code
    if dist_mem:
        output = (
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      !")
        assert output in generated_code


def test_field_fs(tmpdir):
    ''' Tests that a call with a set of fields making use of all
    function spaces and no basis functions produces correct code.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1.5_single_invoke_fs.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    generated_code = str(psy.gen)
    output = (
        "  MODULE single_invoke_fs_psy\n"
        "    USE constants_mod, ONLY: r_def, i_def\n"
        "    USE field_mod, ONLY: field_type, field_proxy_type\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_testkern_fs_type(f1, f2, m1, m2, f3, f4, "
        "m3, m4, f5, f6, m5, m6, m7)\n"
        "      USE testkern_fs_mod, ONLY: testkern_fs_code\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      TYPE(field_type), intent(in) :: f1, f2, m1, m2, f3, f4, m3, "
        "m4, f5, f6, m5, m6, m7\n"
        "      INTEGER(KIND=i_def) cell\n"
        "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
        "      INTEGER(KIND=i_def) nlayers\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: m7_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: m6_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: m5_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f6_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f5_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: m4_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: m3_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f4_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f3_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: m2_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: m1_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f2_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f1_data => null()\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, "
        "m2_proxy, f3_proxy, f4_proxy, m3_proxy, m4_proxy, f5_proxy, "
        "f6_proxy, m5_proxy, m6_proxy, m7_proxy\n"
        "      INTEGER(KIND=i_def), pointer :: map_any_w2(:,:) => null(), "
        "map_w0(:,:) => null(), map_w1(:,:) => null(), map_w2(:,:) => "
        "null(), map_w2broken(:,:) => null(), map_w2h(:,:) => null(), "
        "map_w2htrace(:,:) => null(), map_w2trace(:,:) => null(), "
        "map_w2v(:,:) => null(), map_w2vtrace(:,:) => null(), map_w3(:,:) "
        "=> null(), map_wchi(:,:) => null(), map_wtheta(:,:) => null()\n"
        "      INTEGER(KIND=i_def) ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w0, "
        "undf_w0, ndf_w3, undf_w3, ndf_wtheta, undf_wtheta, ndf_w2h, "
        "undf_w2h, ndf_w2v, undf_w2v, ndf_w2broken, undf_w2broken, "
        "ndf_w2trace, undf_w2trace, ndf_w2htrace, undf_w2htrace, "
        "ndf_w2vtrace, undf_w2vtrace, ndf_wchi, undf_wchi, ndf_any_w2, "
        "undf_any_w2\n"
        "      INTEGER(KIND=i_def) max_halo_depth_mesh\n"
        "      TYPE(mesh_type), pointer :: mesh => null()\n")
    assert output in generated_code
    output = (
        "      ! Initialise field and/or operator proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      f1_data => f1_proxy%data\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      f2_data => f2_proxy%data\n"
        "      m1_proxy = m1%get_proxy()\n"
        "      m1_data => m1_proxy%data\n"
        "      m2_proxy = m2%get_proxy()\n"
        "      m2_data => m2_proxy%data\n"
        "      f3_proxy = f3%get_proxy()\n"
        "      f3_data => f3_proxy%data\n"
        "      f4_proxy = f4%get_proxy()\n"
        "      f4_data => f4_proxy%data\n"
        "      m3_proxy = m3%get_proxy()\n"
        "      m3_data => m3_proxy%data\n"
        "      m4_proxy = m4%get_proxy()\n"
        "      m4_data => m4_proxy%data\n"
        "      f5_proxy = f5%get_proxy()\n"
        "      f5_data => f5_proxy%data\n"
        "      f6_proxy = f6%get_proxy()\n"
        "      f6_data => f6_proxy%data\n"
        "      m5_proxy = m5%get_proxy()\n"
        "      m5_data => m5_proxy%data\n"
        "      m6_proxy = m6%get_proxy()\n"
        "      m6_data => m6_proxy%data\n"
        "      m7_proxy = m7%get_proxy()\n"
        "      m7_data => m7_proxy%data\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh => f1_proxy%vspace%get_mesh()\n"
        "      max_halo_depth_mesh = mesh%get_halo_depth()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w0 => m1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "      map_wtheta => f3_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2h => f4_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2v => m3_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2broken => m4_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2trace => f5_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2htrace => f6_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2vtrace => m5_proxy%vspace%get_whole_dofmap()\n"
        "      map_wchi => m6_proxy%vspace%get_whole_dofmap()\n"
        "      map_any_w2 => m7_proxy%vspace%get_whole_dofmap()\n"
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
        "      ! Initialise number of DoFs for w0\n"
        "      !\n"
        "      ndf_w0 = m1_proxy%vspace%get_ndf()\n"
        "      undf_w0 = m1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w3\n"
        "      !\n"
        "      ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "      undf_w3 = m2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for wtheta\n"
        "      !\n"
        "      ndf_wtheta = f3_proxy%vspace%get_ndf()\n"
        "      undf_wtheta = f3_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2h\n"
        "      !\n"
        "      ndf_w2h = f4_proxy%vspace%get_ndf()\n"
        "      undf_w2h = f4_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2v\n"
        "      !\n"
        "      ndf_w2v = m3_proxy%vspace%get_ndf()\n"
        "      undf_w2v = m3_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2broken\n"
        "      !\n"
        "      ndf_w2broken = m4_proxy%vspace%get_ndf()\n"
        "      undf_w2broken = m4_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2trace\n"
        "      !\n"
        "      ndf_w2trace = f5_proxy%vspace%get_ndf()\n"
        "      undf_w2trace = f5_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2htrace\n"
        "      !\n"
        "      ndf_w2htrace = f6_proxy%vspace%get_ndf()\n"
        "      undf_w2htrace = f6_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2vtrace\n"
        "      !\n"
        "      ndf_w2vtrace = m5_proxy%vspace%get_ndf()\n"
        "      undf_w2vtrace = m5_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for wchi\n"
        "      !\n"
        "      ndf_wchi = m6_proxy%vspace%get_ndf()\n"
        "      undf_wchi = m6_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for any_w2\n"
        "      !\n"
        "      ndf_any_w2 = m7_proxy%vspace%get_ndf()\n"
        "      undf_any_w2 = m7_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Set-up all of the loop bounds\n"
        "      !\n"
        "      loop0_start = 1\n"
        "      loop0_stop = mesh%get_last_halo_cell(1)\n"
        "      !\n"
        "      ! Call kernels and communication routines\n"
        "      !\n"
        "      IF (f1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f1_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f4_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f4_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m3_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m3_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m4_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m4_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f5_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f5_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f6_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f6_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m5_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m5_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m6_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m6_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m7_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m7_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      DO cell=loop0_start,loop0_stop\n"
        "        !\n"
        "        CALL testkern_fs_code(nlayers, f1_data, f2_data, "
        "m1_data, m2_data, f3_data, f4_data, "
        "m3_data, m4_data, f5_data, f6_data, "
        "m5_data, m6_data, m7_data, ndf_w1, undf_w1, "
        "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w0, undf_w0, "
        "map_w0(:,cell), ndf_w3, undf_w3, map_w3(:,cell), ndf_wtheta, "
        "undf_wtheta, map_wtheta(:,cell), ndf_w2h, undf_w2h, "
        "map_w2h(:,cell), ndf_w2v, undf_w2v, map_w2v(:,cell), ndf_w2broken, "
        "undf_w2broken, map_w2broken(:,cell), ndf_w2trace, undf_w2trace, "
        "map_w2trace(:,cell), ndf_w2htrace, undf_w2htrace, "
        "map_w2htrace(:,cell), ndf_w2vtrace, undf_w2vtrace, "
        "map_w2vtrace(:,cell), ndf_wchi, undf_wchi, map_wchi(:,cell), "
        "ndf_any_w2, undf_any_w2, map_any_w2(:,cell))\n"
        "      END DO\n"
        "      !\n"
        "      ! Set halos dirty/clean for fields modified in the above loop\n"
        "      !\n"
        "      CALL f1_proxy%set_dirty()\n"
        "      CALL f3_proxy%set_dirty()\n"
        "      CALL f3_proxy%set_clean(1)\n"
        "      !\n"
        "      !\n"
        "    END SUBROUTINE invoke_0_testkern_fs_type\n"
        "  END MODULE single_invoke_fs_psy")
    assert output in generated_code


def test_vector_field(tmpdir):
    ''' Tests that a vector field is declared correctly in the PSy
    layer. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "8_vector_field.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert ("SUBROUTINE invoke_0_testkern_coord_w0_type(f1, chi, f2)" in
            generated_code)
    assert "TYPE(field_type), intent(in) :: f1, chi(3), f2" in generated_code


def test_vector_field_2(tmpdir):
    ''' Tests that a vector field is indexed correctly in the PSy layer. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "8_vector_field_2.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # All references to chi_proxy should be chi_proxy(1)
    assert "chi_proxy%" not in generated_code
    assert generated_code.count("chi_proxy(1)%vspace") == 5
    # Use each chi field individually in the kernel
    assert ("chi_1_data, chi_2_data, chi_3_data" in
            generated_code)


def test_mkern_invoke_vec_fields():
    ''' Test that correct code is produced when there are multiple
    kernels within an invoke with vector fields '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.2_multikernel_invokes.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)
    # 1st test for duplication of name vector-field declaration
    assert ("TYPE(field_type), intent(in) :: f1, chi(3), chi(3)"
            not in generated_code)
    # 2nd test for duplication of name vector-field declaration
    assert ("TYPE(field_proxy_type) f1_proxy, chi_proxy(3), chi_proxy(3)"
            not in generated_code)


# Tests for Invokes calling kernels that contain integer-valued fields


def test_int_field_fs(tmpdir):
    ''' Tests that a call with a set of integer-valued fields making use of
    all function spaces and no basis functions produces correct code. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.5.5_single_invoke_write_multi_fs_int_field.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    generated_code = str(psy.gen)
    output = (
        "  MODULE single_invoke_fs_int_field_psy\n"
        "    USE constants_mod, ONLY: i_def\n"
        "    USE integer_field_mod, ONLY: integer_field_type, "
        "integer_field_proxy_type\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_0_testkern_fs_int_field_type(f1, f2, m1, m2, "
        "f3, f4, m3, m4, f5, f6, m5, m6, f7, f8, m7)\n"
        "      USE testkern_fs_int_field_mod, ONLY: "
        "testkern_fs_int_field_code\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      TYPE(integer_field_type), intent(in) :: f1, f2, m1, m2, f3, "
        "f4, m3, m4, f5, f6, m5, m6, f7, f8, m7\n"
        "      INTEGER(KIND=i_def) cell\n"
        "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
        "      INTEGER(KIND=i_def) nlayers\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: m7_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: f8_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: f7_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: m6_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: m5_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: f6_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: f5_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: m4_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: m3_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: f4_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: f3_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: m2_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: m1_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: f2_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: f1_data => "
        "null()\n"
        "      TYPE(integer_field_proxy_type) f1_proxy, f2_proxy, m1_proxy, "
        "m2_proxy, f3_proxy, f4_proxy, m3_proxy, m4_proxy, f5_proxy, "
        "f6_proxy, m5_proxy, m6_proxy, f7_proxy, f8_proxy, m7_proxy\n"
        "      INTEGER(KIND=i_def), pointer :: map_adspc1_m7(:,:) => null(), "
        "map_any_w2(:,:) => null(), map_aspc1_f8(:,:) => null(), "
        "map_w0(:,:) => null(), map_w1(:,:) => null(), map_w2(:,:) => "
        "null(), map_w2broken(:,:) => null(), map_w2h(:,:) => null(), "
        "map_w2htrace(:,:) => null(), map_w2trace(:,:) => null(), "
        "map_w2v(:,:) => null(), map_w2vtrace(:,:) => null(), map_w3(:,:) "
        "=> null(), map_wchi(:,:) => null(), map_wtheta(:,:) => null()\n"
        "      INTEGER(KIND=i_def) ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w0, "
        "undf_w0, ndf_w3, undf_w3, ndf_wtheta, undf_wtheta, ndf_w2h, "
        "undf_w2h, ndf_w2v, undf_w2v, ndf_w2broken, undf_w2broken, "
        "ndf_w2trace, undf_w2trace, ndf_w2htrace, undf_w2htrace, "
        "ndf_w2vtrace, undf_w2vtrace, ndf_wchi, undf_wchi, ndf_any_w2, "
        "undf_any_w2, ndf_aspc1_f8, undf_aspc1_f8, ndf_adspc1_m7, "
        "undf_adspc1_m7\n"
        "      INTEGER(KIND=i_def) max_halo_depth_mesh\n"
        "      TYPE(mesh_type), pointer :: mesh => null()\n")
    assert output in generated_code
    output = (
        "      ! Initialise field and/or operator proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      f1_data => f1_proxy%data\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      f2_data => f2_proxy%data\n"
        "      m1_proxy = m1%get_proxy()\n"
        "      m1_data => m1_proxy%data\n"
        "      m2_proxy = m2%get_proxy()\n"
        "      m2_data => m2_proxy%data\n"
        "      f3_proxy = f3%get_proxy()\n"
        "      f3_data => f3_proxy%data\n"
        "      f4_proxy = f4%get_proxy()\n"
        "      f4_data => f4_proxy%data\n"
        "      m3_proxy = m3%get_proxy()\n"
        "      m3_data => m3_proxy%data\n"
        "      m4_proxy = m4%get_proxy()\n"
        "      m4_data => m4_proxy%data\n"
        "      f5_proxy = f5%get_proxy()\n"
        "      f5_data => f5_proxy%data\n"
        "      f6_proxy = f6%get_proxy()\n"
        "      f6_data => f6_proxy%data\n"
        "      m5_proxy = m5%get_proxy()\n"
        "      m5_data => m5_proxy%data\n"
        "      m6_proxy = m6%get_proxy()\n"
        "      m6_data => m6_proxy%data\n"
        "      f7_proxy = f7%get_proxy()\n"
        "      f7_data => f7_proxy%data\n"
        "      f8_proxy = f8%get_proxy()\n"
        "      f8_data => f8_proxy%data\n"
        "      m7_proxy = m7%get_proxy()\n"
        "      m7_data => m7_proxy%data\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh => f1_proxy%vspace%get_mesh()\n"
        "      max_halo_depth_mesh = mesh%get_halo_depth()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w0 => m1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "      map_wtheta => f3_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2h => f4_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2v => m3_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2broken => m4_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2trace => f5_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2htrace => f6_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2vtrace => m5_proxy%vspace%get_whole_dofmap()\n"
        "      map_wchi => m6_proxy%vspace%get_whole_dofmap()\n"
        "      map_any_w2 => f7_proxy%vspace%get_whole_dofmap()\n"
        "      map_aspc1_f8 => f8_proxy%vspace%get_whole_dofmap()\n"
        "      map_adspc1_m7 => m7_proxy%vspace%get_whole_dofmap()\n"
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
        "      ! Initialise number of DoFs for w0\n"
        "      !\n"
        "      ndf_w0 = m1_proxy%vspace%get_ndf()\n"
        "      undf_w0 = m1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w3\n"
        "      !\n"
        "      ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "      undf_w3 = m2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for wtheta\n"
        "      !\n"
        "      ndf_wtheta = f3_proxy%vspace%get_ndf()\n"
        "      undf_wtheta = f3_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2h\n"
        "      !\n"
        "      ndf_w2h = f4_proxy%vspace%get_ndf()\n"
        "      undf_w2h = f4_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2v\n"
        "      !\n"
        "      ndf_w2v = m3_proxy%vspace%get_ndf()\n"
        "      undf_w2v = m3_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2broken\n"
        "      !\n"
        "      ndf_w2broken = m4_proxy%vspace%get_ndf()\n"
        "      undf_w2broken = m4_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2trace\n"
        "      !\n"
        "      ndf_w2trace = f5_proxy%vspace%get_ndf()\n"
        "      undf_w2trace = f5_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2htrace\n"
        "      !\n"
        "      ndf_w2htrace = f6_proxy%vspace%get_ndf()\n"
        "      undf_w2htrace = f6_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2vtrace\n"
        "      !\n"
        "      ndf_w2vtrace = m5_proxy%vspace%get_ndf()\n"
        "      undf_w2vtrace = m5_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for wchi\n"
        "      !\n"
        "      ndf_wchi = m6_proxy%vspace%get_ndf()\n"
        "      undf_wchi = m6_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for any_w2\n"
        "      !\n"
        "      ndf_any_w2 = f7_proxy%vspace%get_ndf()\n"
        "      undf_any_w2 = f7_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for aspc1_f8\n"
        "      !\n"
        "      ndf_aspc1_f8 = f8_proxy%vspace%get_ndf()\n"
        "      undf_aspc1_f8 = f8_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for adspc1_m7\n"
        "      !\n"
        "      ndf_adspc1_m7 = m7_proxy%vspace%get_ndf()\n"
        "      undf_adspc1_m7 = m7_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Set-up all of the loop bounds\n"
        "      !\n"
        "      loop0_start = 1\n"
        "      loop0_stop = mesh%get_last_halo_cell(1)\n"
        "      !\n"
        "      ! Call kernels and communication routines\n"
        "      !\n"
        "      IF (f1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f1_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f4_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f4_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m3_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m3_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m4_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m4_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f5_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f5_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f6_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f6_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m5_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m5_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m6_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m6_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f7_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f7_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f8_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f8_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m7_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m7_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      DO cell=loop0_start,loop0_stop\n"
        "        !\n"
        "        CALL testkern_fs_int_field_code(nlayers, f1_data, "
        "f2_data, m1_data, m2_data, f3_data, "
        "f4_data, m3_data, m4_data, f5_data, "
        "f6_data, m5_data, m6_data, f7_data, "
        "f8_data, m7_data, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w0, undf_w0, map_w0(:,cell), "
        "ndf_w3, undf_w3, map_w3(:,cell), ndf_wtheta, undf_wtheta, "
        "map_wtheta(:,cell), ndf_w2h, undf_w2h, map_w2h(:,cell), ndf_w2v, "
        "undf_w2v, map_w2v(:,cell), ndf_w2broken, undf_w2broken, "
        "map_w2broken(:,cell), ndf_w2trace, undf_w2trace, "
        "map_w2trace(:,cell), ndf_w2htrace, undf_w2htrace, "
        "map_w2htrace(:,cell), ndf_w2vtrace, undf_w2vtrace, "
        "map_w2vtrace(:,cell), ndf_wchi, undf_wchi, map_wchi(:,cell), "
        "ndf_any_w2, undf_any_w2, map_any_w2(:,cell), ndf_aspc1_f8, "
        "undf_aspc1_f8, map_aspc1_f8(:,cell), ndf_adspc1_m7, "
        "undf_adspc1_m7, map_adspc1_m7(:,cell))\n"
        "      END DO\n"
        "      !\n"
        "      ! Set halos dirty/clean for fields modified in the above loop\n"
        "      !\n"
        "      CALL f2_proxy%set_dirty()\n"
        "      CALL f3_proxy%set_dirty()\n"
        "      CALL f3_proxy%set_clean(1)\n"
        "      CALL f8_proxy%set_dirty()\n"
        "      CALL m7_proxy%set_dirty()\n"
        "      CALL m7_proxy%set_clean(1)\n"
        "      !\n"
        "      !\n"
        "    END SUBROUTINE invoke_0_testkern_fs_int_field_type\n"
        "  END MODULE single_invoke_fs_int_field_psy")
    assert output in generated_code


def test_int_field_2qr_shapes(dist_mem, tmpdir):
    ''' Check that we can generate correct call for a kernel that requires
    two types of quadrature (here XYoZ and face) for integer fields
    Note: Basis and differential basis functions for all fields (real and
    integer) are real-valued.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.1.9_single_invoke_2qr_shapes_int_field.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(invoke_info)
    assert LFRicBuild(tmpdir).code_compiles(psy)
    gen_code = str(psy.gen)
    # Check that the qr-related variables are all declared
    assert ("      TYPE(quadrature_xyoz_type), intent(in) :: qr_xyoz\n"
            "      TYPE(quadrature_face_type), intent(in) :: qr_face\n"
            in gen_code)
    assert ("REAL(KIND=r_def), allocatable :: basis_w2_qr_xyoz(:,:,:,:), "
            "basis_w2_qr_face(:,:,:,:), diff_basis_wchi_qr_xyoz(:,:,:,:), "
            "diff_basis_wchi_qr_face(:,:,:,:), "
            "basis_adspc1_f3_qr_xyoz(:,:,:,:), "
            "diff_basis_adspc1_f3_qr_xyoz(:,:,:,:), "
            "basis_adspc1_f3_qr_face(:,:,:,:), "
            "diff_basis_adspc1_f3_qr_face(:,:,:,:)\n" in gen_code)
    assert ("      REAL(KIND=r_def), pointer :: weights_xyz_qr_face(:,:) "
            "=> null()\n"
            "      INTEGER(KIND=i_def) np_xyz_qr_face, nfaces_qr_face\n"
            "      REAL(KIND=r_def), pointer :: weights_xy_qr_xyoz(:) => "
            "null(), weights_z_qr_xyoz(:) => null()\n"
            "      INTEGER(KIND=i_def) np_xy_qr_xyoz, np_z_qr_xyoz\n"

            in gen_code)
    assert ("      TYPE(quadrature_face_proxy_type) qr_face_proxy\n"
            "      TYPE(quadrature_xyoz_proxy_type) qr_xyoz_proxy\n"
            in gen_code)
    # Allocation and computation of (some of) the basis/differential
    # basis functions
    assert ("      ALLOCATE (basis_adspc1_f3_qr_xyoz(dim_adspc1_f3, "
            "ndf_adspc1_f3, np_xy_qr_xyoz, np_z_qr_xyoz))\n"
            "      ALLOCATE (diff_basis_adspc1_f3_qr_xyoz(diff_dim_adspc1_f3, "
            "ndf_adspc1_f3, np_xy_qr_xyoz, np_z_qr_xyoz))\n"
            "      ALLOCATE (basis_adspc1_f3_qr_face(dim_adspc1_f3, "
            "ndf_adspc1_f3, np_xyz_qr_face, nfaces_qr_face))\n"
            "      ALLOCATE (diff_basis_adspc1_f3_qr_face(diff_dim_adspc1_f3, "
            "ndf_adspc1_f3, np_xyz_qr_face, nfaces_qr_face))\n"
            in gen_code)
    assert ("      CALL qr_xyoz%compute_function(BASIS, f3_proxy%vspace, "
            "dim_adspc1_f3, ndf_adspc1_f3, basis_adspc1_f3_qr_xyoz)\n"
            "      CALL qr_xyoz%compute_function(DIFF_BASIS, "
            "f3_proxy%vspace, diff_dim_adspc1_f3, ndf_adspc1_f3, "
            "diff_basis_adspc1_f3_qr_xyoz)\n"
            "      CALL qr_face%compute_function(BASIS, f3_proxy%vspace, "
            "dim_adspc1_f3, ndf_adspc1_f3, basis_adspc1_f3_qr_face)\n"
            "      CALL qr_face%compute_function(DIFF_BASIS, "
            "f3_proxy%vspace, diff_dim_adspc1_f3, ndf_adspc1_f3, "
            "diff_basis_adspc1_f3_qr_face)\n" in gen_code)
    # Check that the kernel call itself is correct
    assert (
        "testkern_2qr_int_field_code(nlayers, f1_data, "
        "f2_1_data, f2_2_data, f2_3_data, f3_data, "
        "istp, ndf_w2, undf_w2, map_w2(:,cell), basis_w2_qr_xyoz, "
        "basis_w2_qr_face, ndf_wchi, undf_wchi, map_wchi(:,cell), "
        "diff_basis_wchi_qr_xyoz, diff_basis_wchi_qr_face, ndf_adspc1_f3, "
        "undf_adspc1_f3, map_adspc1_f3(:,cell), basis_adspc1_f3_qr_xyoz, "
        "basis_adspc1_f3_qr_face, diff_basis_adspc1_f3_qr_xyoz, "
        "diff_basis_adspc1_f3_qr_face, np_xy_qr_xyoz, np_z_qr_xyoz, "
        "weights_xy_qr_xyoz, weights_z_qr_xyoz, nfaces_qr_face, "
        "np_xyz_qr_face, weights_xyz_qr_face)\n" in gen_code)


# Tests for Invokes calling kernels that contain real- and
# integer-valued fields


def test_int_real_field_fs(dist_mem, tmpdir):
    ''' Tests that an invoke calling a kernel with integer-valued
    fields and a kernel with real-valued fields on all function
    spaces produces correct code.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "4.14_multikernel_invokes_real_int_field_fs.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(invoke_info)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    generated_code = str(psy.gen)

    output = (
        "  MODULE multikernel_invokes_real_int_field_fs_psy\n"
        "    USE constants_mod, ONLY: r_def, i_def\n"
        "    USE field_mod, ONLY: field_type, field_proxy_type\n"
        "    USE integer_field_mod, ONLY: integer_field_type, "
        "integer_field_proxy_type\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE invoke_integer_and_real_field(i1, i2, n1, n2, i3, "
        "i4, n3, n4, i5, i6, n5, n6, i7, i8, n7, f1, f2, m1, m2, f3, f4, "
        "m3, m4, f5, f6, m5, m6, m7)\n")
    assert output in generated_code
    output = (
        "      TYPE(field_type), intent(in) :: f1, f2, m1, m2, f3, f4, "
        "m3, m4, f5, f6, m5, m6, m7\n"
        "      TYPE(integer_field_type), intent(in) :: i1, i2, n1, n2, "
        "i3, i4, n3, n4, i5, i6, n5, n6, i7, i8, n7\n"
        "      INTEGER(KIND=i_def) cell\n"
        "      INTEGER(KIND=i_def) loop1_start, loop1_stop\n"
        "      INTEGER(KIND=i_def) loop0_start, loop0_stop\n"
        "      INTEGER(KIND=i_def) nlayers\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: n7_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: i8_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: i7_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: n6_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: n5_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: i6_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: i5_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: n4_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: n3_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: i4_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: i3_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: n2_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: n1_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: i2_data => "
        "null()\n"
        "      INTEGER(KIND=i_def), pointer, dimension(:) :: i1_data => "
        "null()\n"
        "      TYPE(integer_field_proxy_type) i1_proxy, i2_proxy, n1_proxy, "
        "n2_proxy, i3_proxy, i4_proxy, n3_proxy, n4_proxy, i5_proxy, "
        "i6_proxy, n5_proxy, n6_proxy, i7_proxy, i8_proxy, n7_proxy\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: m7_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: m6_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: m5_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f6_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f5_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: m4_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: m3_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f4_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f3_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: m2_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: m1_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f2_data => null()\n"
        "      REAL(KIND=r_def), pointer, dimension(:) :: f1_data => null()\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, "
        "m2_proxy, f3_proxy, f4_proxy, m3_proxy, m4_proxy, f5_proxy, "
        "f6_proxy, m5_proxy, m6_proxy, m7_proxy\n")
    assert output in generated_code
    # Number of layers and the mesh are determined from the first integer
    # field. Maps for function spaces are determined from the first kernel
    # call with integer fields
    output = (
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = i1_proxy%vspace%get_nlayers()\n"
        "      !\n")
    if dist_mem:
        output += (
            "      ! Create a mesh object\n"
            "      !\n"
            "      mesh => i1_proxy%vspace%get_mesh()\n"
            "      max_halo_depth_mesh = mesh%get_halo_depth()\n"
            "      !\n")
    output += (
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w1 => i1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2 => i2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w0 => n1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => n2_proxy%vspace%get_whole_dofmap()\n"
        "      map_wtheta => i3_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2h => i4_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2v => n3_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2broken => n4_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2trace => i5_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2htrace => i6_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2vtrace => n5_proxy%vspace%get_whole_dofmap()\n"
        "      map_wchi => n6_proxy%vspace%get_whole_dofmap()\n"
        "      map_any_w2 => i7_proxy%vspace%get_whole_dofmap()\n"
        "      map_aspc1_i8 => i8_proxy%vspace%get_whole_dofmap()\n"
        "      map_adspc1_n7 => n7_proxy%vspace%get_whole_dofmap()\n")
    assert output in generated_code
    # Kernel calls are the same regardless of distributed memory
    kern1_call = (
        "        CALL testkern_fs_int_field_code(nlayers, i1_data, "
        "i2_data, n1_data, n2_data, i3_data, "
        "i4_data, n3_data, n4_data, i5_data, "
        "i6_data, n5_data, n6_data, i7_data, "
        "i8_data, n7_data, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w0, undf_w0, "
        "map_w0(:,cell), ndf_w3, undf_w3, map_w3(:,cell), ndf_wtheta, "
        "undf_wtheta, map_wtheta(:,cell), ndf_w2h, undf_w2h, "
        "map_w2h(:,cell), ndf_w2v, undf_w2v, map_w2v(:,cell), "
        "ndf_w2broken, undf_w2broken, map_w2broken(:,cell), ndf_w2trace, "
        "undf_w2trace, map_w2trace(:,cell), ndf_w2htrace, undf_w2htrace, "
        "map_w2htrace(:,cell), ndf_w2vtrace, undf_w2vtrace, "
        "map_w2vtrace(:,cell), ndf_wchi, undf_wchi, map_wchi(:,cell), "
        "ndf_any_w2, undf_any_w2, map_any_w2(:,cell), ndf_aspc1_i8, "
        "undf_aspc1_i8, map_aspc1_i8(:,cell), ndf_adspc1_n7, "
        "undf_adspc1_n7, map_adspc1_n7(:,cell))\n")
    assert kern1_call in generated_code
    kern2_call = (
        "        CALL testkern_fs_code(nlayers, f1_data, f2_data, "
        "m1_data, m2_data, f3_data, f4_data, "
        "m3_data, m4_data, f5_data, f6_data, "
        "m5_data, m6_data, m7_data, ndf_w1, undf_w1, "
        "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w0, undf_w0, "
        "map_w0(:,cell), ndf_w3, undf_w3, map_w3(:,cell), ndf_wtheta, "
        "undf_wtheta, map_wtheta(:,cell), ndf_w2h, undf_w2h, map_w2h(:,cell), "
        "ndf_w2v, undf_w2v, map_w2v(:,cell), ndf_w2broken, undf_w2broken, "
        "map_w2broken(:,cell), ndf_w2trace, undf_w2trace, "
        "map_w2trace(:,cell), ndf_w2htrace, undf_w2htrace, "
        "map_w2htrace(:,cell), ndf_w2vtrace, undf_w2vtrace, "
        "map_w2vtrace(:,cell), ndf_wchi, undf_wchi, map_wchi(:,cell), "
        "ndf_any_w2, undf_any_w2, map_any_w2(:,cell))\n")
    assert kern2_call in generated_code
    # Check loop bounds for kernel calls
    if not dist_mem:
        assert "loop0_stop = i2_proxy%vspace%get_ncell()\n" in generated_code
        assert "loop1_stop = f1_proxy%vspace%get_ncell()\n" in generated_code
    else:
        assert "loop0_stop = mesh%get_last_halo_cell(1)\n" in generated_code
        assert "loop1_stop = mesh%get_last_halo_cell(1)\n" in generated_code
    # Check that the field halo flags after the kernel calls
    if dist_mem:
        halo1_flags = (
            "      CALL i2_proxy%set_dirty()\n"
            "      CALL i3_proxy%set_dirty()\n"
            "      CALL i3_proxy%set_clean(1)\n"
            "      CALL i8_proxy%set_dirty()\n"
            "      CALL n7_proxy%set_dirty()\n"
            "      CALL n7_proxy%set_clean(1)\n")
        halo2_flags = (
            "      CALL f1_proxy%set_dirty()\n"
            "      CALL f3_proxy%set_dirty()\n"
            "      CALL f3_proxy%set_clean(1)\n")
        assert halo1_flags in generated_code
        assert halo2_flags in generated_code
