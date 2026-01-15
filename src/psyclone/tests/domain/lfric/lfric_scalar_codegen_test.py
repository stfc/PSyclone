# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
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
#         I. Kavcic, A. Coughtrie and A. Pirrie, Met Office;
#         C. M. Maynard, Met Office/University of Reading;
#         J. Henrichs, Bureau of Meteorology.

'''
Module containing pytest tests for PSy-layer code generation for the
LFRic scalar arguments.
'''

import os
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.utilities import get_invoke

# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "lfric")
TEST_API = "lfric"


def test_real_scalar(tmpdir):
    ''' Tests that we generate correct code when a kernel takes a single,
    real scalar argument (plus fields).

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)

    expected = (
        "  subroutine invoke_0_testkern_type(a, f1, f2, m1, m2)\n"
        "    use mesh_mod, only : mesh_type\n"
        "    use testkern_mod, only : testkern_code\n"
        "    real(kind=r_def), intent(in) :: a\n"
        "    type(field_type), intent(in) :: f1\n"
        "    type(field_type), intent(in) :: f2\n"
        "    type(field_type), intent(in) :: m1\n"
        "    type(field_type), intent(in) :: m2\n"
        "    integer(kind=i_def) :: cell\n"
        "    type(mesh_type), pointer :: mesh => null()\n"
        "    integer(kind=i_def) :: max_halo_depth_mesh\n"
        "    real(kind=r_def), pointer, dimension(:) :: f1_data => null()\n"
        "    real(kind=r_def), pointer, dimension(:) :: f2_data => null()\n"
        "    real(kind=r_def), pointer, dimension(:) :: m1_data => null()\n"
        "    real(kind=r_def), pointer, dimension(:) :: m2_data => null()\n"
        "    integer(kind=i_def) :: nlayers_f1\n"
        "    integer(kind=i_def) :: ndf_w1\n"
        "    integer(kind=i_def) :: undf_w1\n"
        "    integer(kind=i_def) :: ndf_w2\n"
        "    integer(kind=i_def) :: undf_w2\n"
        "    integer(kind=i_def) :: ndf_w3\n"
        "    integer(kind=i_def) :: undf_w3\n"
        "    integer(kind=i_def), pointer :: map_w1(:,:) => null()\n"
        "    integer(kind=i_def), pointer :: map_w2(:,:) => null()\n"
        "    integer(kind=i_def), pointer :: map_w3(:,:) => null()\n"
        "    type(field_proxy_type) :: f1_proxy\n"
        "    type(field_proxy_type) :: f2_proxy\n"
        "    type(field_proxy_type) :: m1_proxy\n"
        "    type(field_proxy_type) :: m2_proxy\n"
        "    integer(kind=i_def) :: loop0_start\n"
        "    integer(kind=i_def) :: loop0_stop\n"
        "\n"
        "    ! Initialise field and/or operator proxies\n"
        "    f1_proxy = f1%get_proxy()\n"
        "    f1_data => f1_proxy%data\n"
        "    f2_proxy = f2%get_proxy()\n"
        "    f2_data => f2_proxy%data\n"
        "    m1_proxy = m1%get_proxy()\n"
        "    m1_data => m1_proxy%data\n"
        "    m2_proxy = m2%get_proxy()\n"
        "    m2_data => m2_proxy%data\n"
        "\n"
        "    ! Initialise number of layers\n"
        "    nlayers_f1 = f1_proxy%vspace%get_nlayers()\n"
        "\n"
        "    ! Create a mesh object\n"
        "    mesh => f1_proxy%vspace%get_mesh()\n"
        "    max_halo_depth_mesh = mesh%get_halo_depth()\n"
        "\n"
        "    ! Look-up dofmaps for each function space\n"
        "    map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "    map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "    map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "\n"
        "    ! Initialise number of DoFs for w1\n"
        "    ndf_w1 = f1_proxy%vspace%get_ndf()\n"
        "    undf_w1 = f1_proxy%vspace%get_undf()\n"
        "\n"
        "    ! Initialise number of DoFs for w2\n"
        "    ndf_w2 = f2_proxy%vspace%get_ndf()\n"
        "    undf_w2 = f2_proxy%vspace%get_undf()\n"
        "\n"
        "    ! Initialise number of DoFs for w3\n"
        "    ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "    undf_w3 = m2_proxy%vspace%get_undf()\n"
        "\n"
        "    ! Set-up all of the loop bounds\n"
        "    loop0_start = 1\n"
        "    loop0_stop = mesh%get_last_halo_cell(1)\n"
        "\n"
        "    ! Call kernels and communication routines\n"
        "    if (f1_proxy%is_dirty(depth=1)) then\n"
        "      call f1_proxy%halo_exchange(depth=1)\n"
        "    end if\n"
        "    if (f2_proxy%is_dirty(depth=1)) then\n"
        "      call f2_proxy%halo_exchange(depth=1)\n"
        "    end if\n"
        "    if (m1_proxy%is_dirty(depth=1)) then\n"
        "      call m1_proxy%halo_exchange(depth=1)\n"
        "    end if\n"
        "    if (m2_proxy%is_dirty(depth=1)) then\n"
        "      call m2_proxy%halo_exchange(depth=1)\n"
        "    end if\n"
        "    do cell = loop0_start, loop0_stop, 1\n"
        "      call testkern_code(nlayers_f1, a, f1_data, f2_data,"
        " m1_data, m2_data, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))\n")
    assert expected in generated_code
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_int_scalar(tmpdir):
    ''' Tests that we generate correct code when a kernel takes a single,
    integer scalar argument (plus fields).

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.6.1_single_invoke_1_int_scalar.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)

    expected = (
        "  subroutine invoke_0_testkern_one_int_scalar_type"
        "(f1, iflag, f2, m1, m2)\n"
        "    use mesh_mod, only : mesh_type\n"
        "    use testkern_one_int_scalar_mod, only : "
        "testkern_one_int_scalar_code\n"
        "    type(field_type), intent(in) :: f1\n"
        "    integer(kind=i_def), intent(in) :: iflag\n"
        "    type(field_type), intent(in) :: f2\n"
        "    type(field_type), intent(in) :: m1\n"
        "    type(field_type), intent(in) :: m2\n"
        "    integer(kind=i_def) :: cell\n"
        "    type(mesh_type), pointer :: mesh => null()\n"
        "    integer(kind=i_def) :: max_halo_depth_mesh\n"
        "    real(kind=r_def), pointer, dimension(:) :: f1_data => null()\n"
        "    real(kind=r_def), pointer, dimension(:) :: f2_data => null()\n"
        "    real(kind=r_def), pointer, dimension(:) :: m1_data => null()\n"
        "    real(kind=r_def), pointer, dimension(:) :: m2_data => null()\n"
        "    integer(kind=i_def) :: nlayers_f1\n"
        "    integer(kind=i_def) :: ndf_w1\n"
        "    integer(kind=i_def) :: undf_w1\n"
        "    integer(kind=i_def) :: ndf_w2\n"
        "    integer(kind=i_def) :: undf_w2\n"
        "    integer(kind=i_def) :: ndf_w3\n"
        "    integer(kind=i_def) :: undf_w3\n"
        "    integer(kind=i_def), pointer :: map_w1(:,:) => null()\n"
        "    integer(kind=i_def), pointer :: map_w2(:,:) => null()\n"
        "    integer(kind=i_def), pointer :: map_w3(:,:) => null()\n"
        "    type(field_proxy_type) :: f1_proxy\n"
        "    type(field_proxy_type) :: f2_proxy\n"
        "    type(field_proxy_type) :: m1_proxy\n"
        "    type(field_proxy_type) :: m2_proxy\n"
        "    integer(kind=i_def) :: loop0_start\n"
        "    integer(kind=i_def) :: loop0_stop\n"
        "\n"
        "    ! Initialise field and/or operator proxies\n"
        "    f1_proxy = f1%get_proxy()\n"
        "    f1_data => f1_proxy%data\n"
        "    f2_proxy = f2%get_proxy()\n"
        "    f2_data => f2_proxy%data\n"
        "    m1_proxy = m1%get_proxy()\n"
        "    m1_data => m1_proxy%data\n"
        "    m2_proxy = m2%get_proxy()\n"
        "    m2_data => m2_proxy%data\n"
        "\n"
        "    ! Initialise number of layers\n"
        "    nlayers_f1 = f1_proxy%vspace%get_nlayers()\n"
        "\n"
        "    ! Create a mesh object\n"
        "    mesh => f1_proxy%vspace%get_mesh()\n"
        "    max_halo_depth_mesh = mesh%get_halo_depth()\n"
        "\n"
        "    ! Look-up dofmaps for each function space\n"
        "    map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "    map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "    map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "\n"
        "    ! Initialise number of DoFs for w1\n"
        "    ndf_w1 = f1_proxy%vspace%get_ndf()\n"
        "    undf_w1 = f1_proxy%vspace%get_undf()\n"
        "\n"
        "    ! Initialise number of DoFs for w2\n"
        "    ndf_w2 = f2_proxy%vspace%get_ndf()\n"
        "    undf_w2 = f2_proxy%vspace%get_undf()\n"
        "\n"
        "    ! Initialise number of DoFs for w3\n"
        "    ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "    undf_w3 = m2_proxy%vspace%get_undf()\n"
        "\n"
        "    ! Set-up all of the loop bounds\n"
        "    loop0_start = 1\n"
        "    loop0_stop = mesh%get_last_halo_cell(1)\n"
        "\n"
        "    ! Call kernels and communication routines\n"
        "    if (f1_proxy%is_dirty(depth=1)) then\n"
        "      call f1_proxy%halo_exchange(depth=1)\n"
        "    end if\n"
        "    if (f2_proxy%is_dirty(depth=1)) then\n"
        "      call f2_proxy%halo_exchange(depth=1)\n"
        "    end if\n"
        "    if (m1_proxy%is_dirty(depth=1)) then\n"
        "      call m1_proxy%halo_exchange(depth=1)\n"
        "    end if\n"
        "    if (m2_proxy%is_dirty(depth=1)) then\n"
        "      call m2_proxy%halo_exchange(depth=1)\n"
        "    end if\n"
        "    do cell = loop0_start, loop0_stop, 1\n"
        "      call testkern_one_int_scalar_code(nlayers_f1, f1_data, "
        "iflag, f2_data, m1_data, m2_data, ndf_w1, undf_w1, "
        "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
        "map_w3(:,cell))\n")
    assert expected in generated_code
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_two_real_scalars(tmpdir):
    ''' Tests that we generate correct code when a kernel has two real,
    scalar arguments.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.9_single_invoke_2_real_scalars.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)

    expected = (
        "  subroutine invoke_0_testkern_two_real_scalars_type(a, f1, f2, "
        "m1, m2, b)\n"
        "    use mesh_mod, only : mesh_type\n"
        "    use testkern_two_real_scalars_mod, only : "
        "testkern_two_real_scalars_code\n"
        "    real(kind=r_def), intent(in) :: a\n"
        "    type(field_type), intent(in) :: f1\n"
        "    type(field_type), intent(in) :: f2\n"
        "    type(field_type), intent(in) :: m1\n"
        "    type(field_type), intent(in) :: m2\n"
        "    real(kind=r_def), intent(in) :: b\n"
        "    integer(kind=i_def) :: cell\n"
        "    type(mesh_type), pointer :: mesh => null()\n"
        "    integer(kind=i_def) :: max_halo_depth_mesh\n"
        "    real(kind=r_def), pointer, dimension(:) :: f1_data => null()\n"
        "    real(kind=r_def), pointer, dimension(:) :: f2_data => null()\n"
        "    real(kind=r_def), pointer, dimension(:) :: m1_data => null()\n"
        "    real(kind=r_def), pointer, dimension(:) :: m2_data => null()\n"
        "    integer(kind=i_def) :: nlayers_f1\n"
        "    integer(kind=i_def) :: ndf_w1\n"
        "    integer(kind=i_def) :: undf_w1\n"
        "    integer(kind=i_def) :: ndf_w2\n"
        "    integer(kind=i_def) :: undf_w2\n"
        "    integer(kind=i_def) :: ndf_w3\n"
        "    integer(kind=i_def) :: undf_w3\n"
        "    integer(kind=i_def), pointer :: map_w1(:,:) => null()\n"
        "    integer(kind=i_def), pointer :: map_w2(:,:) => null()\n"
        "    integer(kind=i_def), pointer :: map_w3(:,:) => null()\n"
        "    type(field_proxy_type) :: f1_proxy\n"
        "    type(field_proxy_type) :: f2_proxy\n"
        "    type(field_proxy_type) :: m1_proxy\n"
        "    type(field_proxy_type) :: m2_proxy\n"
        "    integer(kind=i_def) :: loop0_start\n"
        "    integer(kind=i_def) :: loop0_stop\n"
        "\n"
        "    ! Initialise field and/or operator proxies\n"
        "    f1_proxy = f1%get_proxy()\n"
        "    f1_data => f1_proxy%data\n"
        "    f2_proxy = f2%get_proxy()\n"
        "    f2_data => f2_proxy%data\n"
        "    m1_proxy = m1%get_proxy()\n"
        "    m1_data => m1_proxy%data\n"
        "    m2_proxy = m2%get_proxy()\n"
        "    m2_data => m2_proxy%data\n"
        "\n"
        "    ! Initialise number of layers\n"
        "    nlayers_f1 = f1_proxy%vspace%get_nlayers()\n"
        "\n"
        "    ! Create a mesh object\n"
        "    mesh => f1_proxy%vspace%get_mesh()\n"
        "    max_halo_depth_mesh = mesh%get_halo_depth()\n"
        "\n"
        "    ! Look-up dofmaps for each function space\n"
        "    map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "    map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "    map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "\n"
        "    ! Initialise number of DoFs for w1\n"
        "    ndf_w1 = f1_proxy%vspace%get_ndf()\n"
        "    undf_w1 = f1_proxy%vspace%get_undf()\n"
        "\n"
        "    ! Initialise number of DoFs for w2\n"
        "    ndf_w2 = f2_proxy%vspace%get_ndf()\n"
        "    undf_w2 = f2_proxy%vspace%get_undf()\n"
        "\n"
        "    ! Initialise number of DoFs for w3\n"
        "    ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "    undf_w3 = m2_proxy%vspace%get_undf()\n"
        "\n"
        "    ! Set-up all of the loop bounds\n"
        "    loop0_start = 1\n"
        "    loop0_stop = mesh%get_last_halo_cell(1)\n"
        "\n"
        "    ! Call kernels and communication routines\n"
        "    if (f1_proxy%is_dirty(depth=1)) then\n"
        "      call f1_proxy%halo_exchange(depth=1)\n"
        "    end if\n"
        "    if (f2_proxy%is_dirty(depth=1)) then\n"
        "      call f2_proxy%halo_exchange(depth=1)\n"
        "    end if\n"
        "    if (m1_proxy%is_dirty(depth=1)) then\n"
        "      call m1_proxy%halo_exchange(depth=1)\n"
        "    end if\n"
        "    if (m2_proxy%is_dirty(depth=1)) then\n"
        "      call m2_proxy%halo_exchange(depth=1)\n"
        "    end if\n"
        "    do cell = loop0_start, loop0_stop, 1\n"
        "      call testkern_two_real_scalars_code(nlayers_f1, a, "
        "f1_data, f2_data, m1_data, m2_data, "
        "b, ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, "
        "map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))\n")
    assert expected in generated_code
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_two_int_scalars(tmpdir):
    ''' Tests that we generate correct code when a kernel has two integer,
    scalar arguments.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.6_single_invoke_2_int_scalars.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)

    expected = (
        "  subroutine invoke_0(iflag, f1, f2, m1, m2, istep)\n"
        "    use mesh_mod, only : mesh_type\n"
        "    use testkern_two_int_scalars_mod, only : "
        "testkern_two_int_scalars_code\n"
        "    integer(kind=i_def), intent(in) :: iflag\n"
        "    type(field_type), intent(in) :: f1\n"
        "    type(field_type), intent(in) :: f2\n"
        "    type(field_type), intent(in) :: m1\n"
        "    type(field_type), intent(in) :: m2\n"
        "    integer(kind=i_def), intent(in) :: istep\n"
        "    integer(kind=i_def) :: cell\n"
        "    type(mesh_type), pointer :: mesh => null()\n"
        "    integer(kind=i_def) :: max_halo_depth_mesh\n"
        "    real(kind=r_def), pointer, dimension(:) :: f1_data => null()\n"
        "    real(kind=r_def), pointer, dimension(:) :: f2_data => null()\n"
        "    real(kind=r_def), pointer, dimension(:) :: m1_data => null()\n"
        "    real(kind=r_def), pointer, dimension(:) :: m2_data => null()\n"
        "    integer(kind=i_def) :: nlayers_f1\n"
        "    integer(kind=i_def) :: ndf_w1\n"
        "    integer(kind=i_def) :: undf_w1\n"
        "    integer(kind=i_def) :: ndf_w2\n"
        "    integer(kind=i_def) :: undf_w2\n"
        "    integer(kind=i_def) :: ndf_w3\n"
        "    integer(kind=i_def) :: undf_w3\n"
        "    integer(kind=i_def), pointer :: map_w1(:,:) => null()\n"
        "    integer(kind=i_def), pointer :: map_w2(:,:) => null()\n"
        "    integer(kind=i_def), pointer :: map_w3(:,:) => null()\n"
        "    type(field_proxy_type) :: f1_proxy\n"
        "    type(field_proxy_type) :: f2_proxy\n"
        "    type(field_proxy_type) :: m1_proxy\n"
        "    type(field_proxy_type) :: m2_proxy\n"
        "    integer(kind=i_def) :: loop0_start\n"
        "    integer(kind=i_def) :: loop0_stop\n"
        "    integer(kind=i_def) :: loop1_start\n"
        "    integer(kind=i_def) :: loop1_stop\n"
        "\n"
        "    ! Initialise field and/or operator proxies\n"
        "    f1_proxy = f1%get_proxy()\n"
        "    f1_data => f1_proxy%data\n"
        "    f2_proxy = f2%get_proxy()\n"
        "    f2_data => f2_proxy%data\n"
        "    m1_proxy = m1%get_proxy()\n"
        "    m1_data => m1_proxy%data\n"
        "    m2_proxy = m2%get_proxy()\n"
        "    m2_data => m2_proxy%data\n"
        "\n"
        "    ! Initialise number of layers\n"
        "    nlayers_f1 = f1_proxy%vspace%get_nlayers()\n"
        "\n"
        "    ! Create a mesh object\n"
        "    mesh => f1_proxy%vspace%get_mesh()\n"
        "    max_halo_depth_mesh = mesh%get_halo_depth()\n"
        "\n"
        "    ! Look-up dofmaps for each function space\n"
        "    map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "    map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "    map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "\n"
        "    ! Initialise number of DoFs for w1\n"
        "    ndf_w1 = f1_proxy%vspace%get_ndf()\n"
        "    undf_w1 = f1_proxy%vspace%get_undf()\n"
        "\n"
        "    ! Initialise number of DoFs for w2\n"
        "    ndf_w2 = f2_proxy%vspace%get_ndf()\n"
        "    undf_w2 = f2_proxy%vspace%get_undf()\n"
        "\n"
        "    ! Initialise number of DoFs for w3\n"
        "    ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "    undf_w3 = m2_proxy%vspace%get_undf()\n"
        "\n"
        "    ! Set-up all of the loop bounds\n"
        "    loop0_start = 1\n"
        "    loop0_stop = mesh%get_last_halo_cell(1)\n"
        "    loop1_start = 1\n"
        "    loop1_stop = mesh%get_last_halo_cell(1)\n"
        "\n"
        "    ! Call kernels and communication routines\n"
        "    if (f1_proxy%is_dirty(depth=1)) then\n"
        "      call f1_proxy%halo_exchange(depth=1)\n"
        "    end if\n"
        "    if (f2_proxy%is_dirty(depth=1)) then\n"
        "      call f2_proxy%halo_exchange(depth=1)\n"
        "    end if\n"
        "    if (m1_proxy%is_dirty(depth=1)) then\n"
        "      call m1_proxy%halo_exchange(depth=1)\n"
        "    end if\n"
        "    if (m2_proxy%is_dirty(depth=1)) then\n"
        "      call m2_proxy%halo_exchange(depth=1)\n"
        "    end if\n"
        "    do cell = loop0_start, loop0_stop, 1\n"
        "      call testkern_two_int_scalars_code(nlayers_f1, iflag, "
        "f1_data, f2_data, m1_data, m2_data, istep, "
        "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), "
        "ndf_w3, undf_w3, map_w3(:,cell))\n")
    assert expected in generated_code
    # Check that we pass iflag by value in the second kernel call
    expected = (
        "    do cell = loop1_start, loop1_stop, 1\n"
        "      call testkern_two_int_scalars_code(nlayers_f1, 1, "
        "f1_data, f2_data, m1_data, m2_data, iflag, "
        "ndf_w1, undf_w1, map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), "
        "ndf_w3, undf_w3, map_w3(:,cell))\n")
    assert expected in generated_code
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_three_scalars(tmpdir):
    ''' Tests that we generate correct code when a kernel has all three
    types of valid scalar argument: 'real', 'integer' and 'logical'.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.7_single_invoke_3scalar.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)

    generated_code = str(psy.gen)
    expected = (
        "module single_invoke_psy\n"
        "  use constants_mod\n"
        "  use field_mod, only : field_proxy_type, field_type\n"
        "  implicit none\n"
        "  public\n"
        "\n"
        "  contains\n"
        "  subroutine invoke_0_testkern_three_scalars_type(a, f1, f2, m1, "
        "m2, lswitch, istep)\n"
        "    use mesh_mod, only : mesh_type\n"
        "    use testkern_three_scalars_mod, only : "
        "testkern_three_scalars_code\n"
        "    real(kind=r_def), intent(in) :: a\n"
        "    type(field_type), intent(in) :: f1\n"
        "    type(field_type), intent(in) :: f2\n"
        "    type(field_type), intent(in) :: m1\n"
        "    type(field_type), intent(in) :: m2\n"
        "    logical(kind=l_def), intent(in) :: lswitch\n"
        "    integer(kind=i_def), intent(in) :: istep\n"
        "    integer(kind=i_def) :: cell\n"
        "    type(mesh_type), pointer :: mesh => null()\n"
        "    integer(kind=i_def) :: max_halo_depth_mesh\n"
        "    real(kind=r_def), pointer, dimension(:) :: f1_data => null()\n"
        "    real(kind=r_def), pointer, dimension(:) :: f2_data => null()\n"
        "    real(kind=r_def), pointer, dimension(:) :: m1_data => null()\n"
        "    real(kind=r_def), pointer, dimension(:) :: m2_data => null()\n"
        "    integer(kind=i_def) :: nlayers_f1\n"
        "    integer(kind=i_def) :: ndf_w1\n"
        "    integer(kind=i_def) :: undf_w1\n"
        "    integer(kind=i_def) :: ndf_w2\n"
        "    integer(kind=i_def) :: undf_w2\n"
        "    integer(kind=i_def) :: ndf_w3\n"
        "    integer(kind=i_def) :: undf_w3\n"
        "    integer(kind=i_def), pointer :: map_w1(:,:) => null()\n"
        "    integer(kind=i_def), pointer :: map_w2(:,:) => null()\n"
        "    integer(kind=i_def), pointer :: map_w3(:,:) => null()\n"
        "    type(field_proxy_type) :: f1_proxy\n"
        "    type(field_proxy_type) :: f2_proxy\n"
        "    type(field_proxy_type) :: m1_proxy\n"
        "    type(field_proxy_type) :: m2_proxy\n"
        "    integer(kind=i_def) :: loop0_start\n"
        "    integer(kind=i_def) :: loop0_stop\n"
        "\n"
        "    ! Initialise field and/or operator proxies\n"
        "    f1_proxy = f1%get_proxy()\n"
        "    f1_data => f1_proxy%data\n"
        "    f2_proxy = f2%get_proxy()\n"
        "    f2_data => f2_proxy%data\n"
        "    m1_proxy = m1%get_proxy()\n"
        "    m1_data => m1_proxy%data\n"
        "    m2_proxy = m2%get_proxy()\n"
        "    m2_data => m2_proxy%data\n"
        "\n"
        "    ! Initialise number of layers\n"
        "    nlayers_f1 = f1_proxy%vspace%get_nlayers()\n"
        "\n"
        "    ! Create a mesh object\n"
        "    mesh => f1_proxy%vspace%get_mesh()\n"
        "    max_halo_depth_mesh = mesh%get_halo_depth()\n"
        "\n"
        "    ! Look-up dofmaps for each function space\n"
        "    map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "    map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "    map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "\n"
        "    ! Initialise number of DoFs for w1\n"
        "    ndf_w1 = f1_proxy%vspace%get_ndf()\n"
        "    undf_w1 = f1_proxy%vspace%get_undf()\n"
        "\n"
        "    ! Initialise number of DoFs for w2\n"
        "    ndf_w2 = f2_proxy%vspace%get_ndf()\n"
        "    undf_w2 = f2_proxy%vspace%get_undf()\n"
        "\n"
        "    ! Initialise number of DoFs for w3\n"
        "    ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "    undf_w3 = m2_proxy%vspace%get_undf()\n"
        "\n"
        "    ! Set-up all of the loop bounds\n"
        "    loop0_start = 1\n"
        "    loop0_stop = mesh%get_last_halo_cell(1)\n"
        "\n"
        "    ! Call kernels and communication routines\n"
        "    if (f1_proxy%is_dirty(depth=1)) then\n"
        "      call f1_proxy%halo_exchange(depth=1)\n"
        "    end if\n"
        "    if (f2_proxy%is_dirty(depth=1)) then\n"
        "      call f2_proxy%halo_exchange(depth=1)\n"
        "    end if\n"
        "    if (m1_proxy%is_dirty(depth=1)) then\n"
        "      call m1_proxy%halo_exchange(depth=1)\n"
        "    end if\n"
        "    if (m2_proxy%is_dirty(depth=1)) then\n"
        "      call m2_proxy%halo_exchange(depth=1)\n"
        "    end if\n"
        "    do cell = loop0_start, loop0_stop, 1\n"
        "      call testkern_three_scalars_code(nlayers_f1, a, f1_data, "
        "f2_data, m1_data, m2_data, lswitch, istep, ndf_w1, undf_w1, "
        "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3,"
        " map_w3(:,cell))\n")
    assert expected in generated_code
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_scalar_array(tmpdir):
    ''' Tests that we generate correct code when a kernel has all three
    types of valid scalar array argument: 'real', 'integer' and 'logical'.

    '''
    psy, invoke = get_invoke("28.scalar_array_invoke.f90", TEST_API, idx=0)

    generated_code = str(psy.gen)
    expected = (
        "module scalar_array_invoke_psy\n"
        "  use constants_mod\n"
        "  use field_mod, only : field_proxy_type, field_type\n"
        "  implicit none\n"
        "  public\n"
        "\n"
        "  contains\n"
        "  subroutine invoke_0_testkern_scalar_array_type(afield, "
        "real_array, logical_array, integer_array, a_scalar, "
        "dims_real_array, dims_logical_array, dims_integer_array)\n"
        "    use mesh_mod, only : mesh_type\n"
        "    use testkern_scalar_array_mod, only : "
        "testkern_scalar_array_code\n"
        "    type(field_type), intent(in) :: afield\n"
        "    integer(kind=i_def), intent(in) :: a_scalar\n"
        "    integer(kind=i_def), dimension(2), intent(in) :: "
        "dims_real_array\n"
        "    real(kind=r_def), dimension(dims_real_array(1),"
        "dims_real_array(2)), intent(in) :: real_array\n"
        "    integer(kind=i_def), dimension(1), intent(in) :: "
        "dims_logical_array\n"
        "    logical(kind=l_def), dimension(dims_logical_array(1)), "
        "intent(in) :: logical_array\n"
        "    integer(kind=i_def), dimension(4), intent(in) :: "
        "dims_integer_array\n"
        "    integer(kind=i_def), dimension(dims_integer_array(1),"
        "dims_integer_array(2),dims_integer_array(3),dims_integer_array(4)), "
        "intent(in) :: integer_array\n"
        "    integer(kind=i_def) :: cell\n"
        "    type(mesh_type), pointer :: mesh => null()\n"
        "    integer(kind=i_def) :: max_halo_depth_mesh\n"
        "    real(kind=r_def), pointer, dimension(:) :: afield_data => "
        "null()\n"
        "    integer(kind=i_def) :: nlayers_afield\n"
        "    integer(kind=i_def) :: ndf_w1\n"
        "    integer(kind=i_def) :: undf_w1\n"
        "    integer(kind=i_def), pointer :: map_w1(:,:) => null()\n"
        "    type(field_proxy_type) :: afield_proxy\n"
        "    integer(kind=i_def) :: loop0_start\n"
        "    integer(kind=i_def) :: loop0_stop\n"
        "\n"
        "    ! Initialise field and/or operator proxies\n"
        "    afield_proxy = afield%get_proxy()\n"
        "    afield_data => afield_proxy%data\n"
        "\n"
        "    ! Initialise number of layers\n"
        "    nlayers_afield = afield_proxy%vspace%get_nlayers()\n"
        "\n"
        "    ! Create a mesh object\n"
        "    mesh => afield_proxy%vspace%get_mesh()\n"
        "    max_halo_depth_mesh = mesh%get_halo_depth()\n"
        "\n"
        "    ! Look-up dofmaps for each function space\n"
        "    map_w1 => afield_proxy%vspace%get_whole_dofmap()\n"
        "\n"
        "    ! Initialise number of DoFs for w1\n"
        "    ndf_w1 = afield_proxy%vspace%get_ndf()\n"
        "    undf_w1 = afield_proxy%vspace%get_undf()\n"
        "\n"
        "    ! Set-up all of the loop bounds\n"
        "    loop0_start = 1\n"
        "    loop0_stop = mesh%get_last_halo_cell(1)\n"
        "\n"
        "    ! Call kernels and communication routines\n"
        "    if (afield_proxy%is_dirty(depth=1)) then\n"
        "      call afield_proxy%halo_exchange(depth=1)\n"
        "    end if\n"
        "    do cell = loop0_start, loop0_stop, 1\n"
        "      call testkern_scalar_array_code(nlayers_afield, "
        "afield_data, dims_real_array, real_array, dims_logical_array, "
        "logical_array, dims_integer_array, integer_array, a_scalar, "
        "ndf_w1, undf_w1, map_w1(:,cell))\n"
    )
    assert expected in generated_code
    assert LFRicBuild(tmpdir).code_compiles(psy)
