# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------


''' This module contains pytest tests for LFRic kernels which operate on
    the 'domain'. '''

import os
import pytest
from psyclone.parse.algorithm import parse
from psyclone.parse.utils import ParseError
from psyclone.psyGen import PSyFactory
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.utilities import create_lfric_metadata

BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "lfric")
TEST_API = "lfric"


def test_domain_kernel(fortran_reader):
    ''' Check that we can successfully parse metadata that specifies a
    kernel with operates_on = DOMAIN. '''
    psyir = fortran_reader.psyir_from_source('''
module testkern_domain_mod
  type, extends(kernel_type) :: testkern_domain_type
     type(arg_type), dimension(5) :: meta_args =                             &
          (/ arg_type(gh_scalar, gh_real,    gh_read),          &
             arg_type(gh_field,  gh_real,    gh_readwrite, w3), &
             arg_type(gh_field,  gh_real,    gh_read,      w3), &
             arg_type(gh_field,  gh_real,    gh_read,      w3), &
             arg_type(gh_scalar, gh_integer, gh_read)           &
           /)
     integer :: operates_on = domain
   contains
     procedure, nopass :: code => testkern_domain_code
  end type testkern_domain_type
contains
  subroutine testkern_domain_code()
end subroutine testkern_domain_code
end module testkern_domain_mod
''')
    dkm = create_lfric_metadata(psyir, name="testkern_domain_type")
    assert dkm.operates_on == "domain"


def test_invalid_arg_domain_kernel(fortran_reader):
    ''' Check that we reject a domain kernel if its metadata specifies
    an operator argument. '''
    psyir = fortran_reader.psyir_from_source('''module testkern_domain_mod
  type, extends(kernel_type) :: testkern_domain_type
     type(arg_type), dimension(4) :: meta_args =                    &
          (/ arg_type(gh_scalar,   gh_real, gh_read),             &
             arg_type(gh_field,    gh_real, gh_readwrite, w3),    &
             arg_type(gh_field,    gh_real, gh_read,      w3),    &
             arg_type(gh_operator, gh_real, gh_read,      w2, w2) &
           /)
     integer :: operates_on = domain
   contains
     procedure, nopass :: code => testkern_domain_code
  end type testkern_domain_type
contains
  subroutine testkern_domain_code()
end subroutine testkern_domain_code
end module testkern_domain_mod
''')
    with pytest.raises(ParseError) as err:
        create_lfric_metadata(psyir, name="testkern_domain_type")
    assert "Domain kernels may only contain scalar or field arguments" in str(
        err.value)


def test_invalid_space_domain_kernel(fortran_reader):
    ''' Check that we reject a domain kernel if its metadata specifies a
    field argument on a continuous space. '''
    psyir = fortran_reader.psyir_from_source('''module testkern_domain_mod
  type, extends(kernel_type) :: testkern_domain_type
     type(arg_type), dimension(3) :: meta_args =                          &
          (/ arg_type(gh_scalar, gh_real, gh_read),          &
             arg_type(gh_field,  gh_real, gh_readwrite, w3), &
             arg_type(gh_field,  gh_real, gh_read,      w2)  &
           /)
     integer :: operates_on = domain
   contains
     procedure, nopass :: code => testkern_domain_code
  end type testkern_domain_type
contains
  subroutine testkern_domain_code()
end subroutine testkern_domain_code
end module testkern_domain_mod
''')
    with pytest.raises(ParseError) as err:
        create_lfric_metadata(psyir, name="testkern_domain_type")
    assert ("Domain kernels only accept fields on discontinuous function "
            "spaces but found 'w2'" in str(err.value))


def test_no_stencil_domain_kernel(fortran_reader):
    ''' Check that we reject a domain kernel if it has an argument with a
    stencil access. '''
    psyir = fortran_reader.psyir_from_source('''module testkern_domain_mod
  type, extends(kernel_type) :: testkern_domain_type
     type(arg_type), dimension(3) :: meta_args =                              &
          (/ arg_type(gh_scalar, gh_real, gh_read),                         &
             arg_type(gh_field,  gh_real, gh_readwrite, w3),                &
             arg_type(gh_field,  gh_real, gh_read,      w3, stencil(cross)) &
           /)
     integer :: operates_on = domain
   contains
     procedure, nopass :: code => testkern_domain_code
  end type testkern_domain_type
contains
  subroutine testkern_domain_code()
end subroutine testkern_domain_code
end module testkern_domain_mod
''')
    with pytest.raises(ParseError) as err:
        create_lfric_metadata(psyir, name="testkern_domain_type")
    assert ("Domain kernels may not have arguments with a stencil access "
            "but found 'arg_type(gh_field, gh_real, gh_read, w3, "
            "stencil(cross))'" in str(err.value))


def test_invalid_basis_domain_kernel(fortran_reader):
    ''' Check that we reject a kernel with operates_on=domain if it requires
    basis functions. '''
    psyir = fortran_reader.psyir_from_source('''
module testkern_domain_mod
  type, extends(kernel_type) :: testkern_domain_type
     type(arg_type), dimension(3) :: meta_args =                          &
          (/ arg_type(gh_scalar, gh_real, gh_read),          &
             arg_type(gh_field,  gh_real, gh_readwrite, w3), &
             arg_type(gh_field,  gh_real, gh_read,      w3)  &
           /)
     type(func_type), dimension(1) :: meta_funcs =  &
          (/ func_type(w3, gh_basis)                &
           /)
     integer :: operates_on = domain
     integer :: gh_shape = gh_quadrature_XYoZ
   contains
     procedure, nopass :: code => testkern_domain_code
  end type testkern_domain_type
contains
  subroutine testkern_domain_code()
end subroutine testkern_domain_code
end module testkern_domain_mod
''')
    with pytest.raises(ParseError) as err:
        create_lfric_metadata(psyir, name="testkern_domain_type")
    assert "Domain kernels may not request basis functions" in str(err.value)


def test_invalid_mesh_props_domain_kernel(fortran_reader):
    ''' Check that we reject a kernel with operates_on=domain if it requires
    properties of the mesh. '''
    psyir = fortran_reader.psyir_from_source('''
module testkern_domain_mod
  type, extends(kernel_type) :: testkern_domain_type
     type(arg_type), dimension(2) :: meta_args =                         &
          (/ arg_type(gh_scalar, gh_real, gh_read),         &
             arg_type(gh_field,  gh_real, gh_readwrite, w3) &
           /)
     type(mesh_data_type), dimension(1) :: meta_mesh = &
                        (/ mesh_data_type(adjacent_face) /)
     integer :: operates_on = domain
   contains
     procedure, nopass :: code => testkern_domain_code
  end type testkern_domain_type
contains
  subroutine testkern_domain_code()
end subroutine testkern_domain_code
end module testkern_domain_mod
''')
    with pytest.raises(ParseError) as err:
        create_lfric_metadata(psyir, name="testkern_domain_type")
    assert "Domain kernels may not request basis functions or mesh" in str(
        err.value)


def test_invalid_ref_elem_props_domain_kernel(fortran_reader):
    ''' Check that we reject a kernel with operates_on=domain if it requires
    properties of the reference element. '''
    psyir = fortran_reader.psyir_from_source('''
module testkern_domain_mod
  type, extends(kernel_type) :: testkern_domain_type
     type(arg_type), dimension(2) :: meta_args =                         &
          (/ arg_type(gh_scalar, gh_real, gh_read),         &
             arg_type(gh_field,  gh_real, gh_readwrite, w3) &
           /)
     type(reference_element_data_type), dimension(1) :: &
         meta_reference_element =                       &
            (/ reference_element_data_type(normals_to_horizontal_faces) /)
     integer :: operates_on = domain
   contains
     procedure, nopass :: code => testkern_domain_code
  end type testkern_domain_type
contains
  subroutine testkern_domain_code()
end subroutine testkern_domain_code
end module testkern_domain_mod
''')
    with pytest.raises(ParseError) as err:
        create_lfric_metadata(psyir, name="testkern_domain_type")
    assert ("Kernels operating on 'domain' may not request evaluator, "
            "reference-element or mesh data" in str(err.value))


def test_invalid_mg_domain_kernel(fortran_reader):
    ''' Check that we reject a kernel with operates_on=domain if it involves
    multi-grid (fields on different grids). '''
    psyir = fortran_reader.psyir_from_source('''
module restrict_mod
type, public, extends(kernel_type) :: restrict_kernel_type
   private
   type(arg_type) :: meta_args(2) = (/                          &
       arg_type(GH_FIELD, GH_REAL, GH_READWRITE,                &
                ANY_DISCONTINUOUS_SPACE_1, mesh_arg=GH_COARSE), &
       arg_type(GH_FIELD, GH_REAL, GH_READ,                     &
                ANY_DISCONTINUOUS_SPACE_2, mesh_arg=GH_FINE  )  &
       /)
  integer :: operates_on = domain
contains
  procedure, nopass :: restrict_kernel_code
end type restrict_kernel_type
contains
  subroutine restrict_kernel_code()
  end subroutine restrict_kernel_code
end module restrict_mod
''')
    with pytest.raises(ParseError) as err:
        create_lfric_metadata(psyir, name="restrict_kernel_type")
    assert "An inter-grid kernel must operate on cell_column" in str(err.value)


def test_psy_gen_domain_kernel(dist_mem, tmpdir, fortran_writer):
    ''' Check the generation of the PSy layer for an invoke consisting of a
    single kernel with operates_on=domain. '''
    _, info = parse(os.path.join(BASE_PATH, "25.0_domain.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
    code = str(psy.gen).lower()

    # A domain kernel needs the number of columns in the mesh. Therefore
    # we require a mesh object.
    assert "type(mesh_type), pointer :: mesh => null()" in code
    assert "mesh => f1_proxy%vspace%get_mesh()" in code
    assert "integer(kind=i_def) :: ncell_2d_no_halos" in code
    assert "ncell_2d_no_halos = mesh%get_last_edge_cell()" in code

    # Kernel call should include whole dofmap and not be within a loop
    assert ("    call testkern_domain_code(nlayers_f1, ncell_2d_no_halos, "
            "b, f1_data, ndf_w3, undf_w3, map_w3)" in code)
    assert "do " not in code

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_psy_gen_domain_two_kernel(dist_mem, tmpdir):
    ''' Check the generation of the PSy layer for an invoke consisting of a
    kernel with operates_on=domain and another with operates_on=cell_column.
    '''
    _, info = parse(os.path.join(BASE_PATH, "25.1_2kern_domain.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
    code = str(psy.gen).lower()

    assert "mesh => f2_proxy%vspace%get_mesh()" in code
    assert "integer(kind=i_def) :: ncell_2d_no_halos" in code

    expected = (
        "    enddo\n")
    if dist_mem:
        expected += (
            "\n"
            "    ! set halos dirty/clean for fields modified in the above "
            "loop(s)\n"
            "    call f2_proxy%set_dirty()\n")
    expected += (
        "    call testkern_domain_code(nlayers_f1, ncell_2d_no_halos, b, "
        "f1_data, ndf_w3, undf_w3, map_w3)\n")
    assert expected in code
    if dist_mem:
        assert (
                # "    ! set halos dirty/clean for fields modified in the "
                # "above kernel\n"
                "  call f1_proxy%set_dirty()\n" in code)

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_psy_gen_domain_multi_kernel(dist_mem, tmpdir):
    ''' Check the generation of the PSy layer for an invoke consisting of
    several kernels, two with operates_on=domain and another with
    operates_on=cell_column.
    '''
    _, info = parse(os.path.join(BASE_PATH, "25.2_multikern_domain.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
    code = str(psy.gen).lower()

    # Check that we only have one last-edge-cell assignment
    assert code.count("ncell_2d_no_halos = mesh%get_last_edge_cell()") == 1

    expected = (
                "    call testkern_domain_code(nlayers_f1, ncell_2d_no_halos, "
                "b, f1_data, ndf_w3, undf_w3, map_w3)\n")
    if dist_mem:
        assert "loop1_stop = mesh%get_last_halo_cell(1)\n" in code
        expected += (
                     "\n"
                     "    ! set halos dirty/clean for fields modified in "
                     "the above loop(s)\n"
                     "    call f1_proxy%set_dirty()\n"
                     "    if (f2_proxy%is_dirty(depth=1)) then\n"
                     "      call f2_proxy%halo_exchange(depth=1)\n"
                     "    end if\n"
                     "    if (f3_proxy%is_dirty(depth=1)) then\n"
                     "      call f3_proxy%halo_exchange(depth=1)\n"
                     "    end if\n"
                     "    if (f4_proxy%is_dirty(depth=1)) then\n"
                     "      call f4_proxy%halo_exchange(depth=1)\n"
                     "    end if\n"
                     "    call f1_proxy%halo_exchange(depth=1)\n")
    else:
        assert "loop1_stop = f2_proxy%vspace%get_ncell()\n" in code
    expected += "    do cell = loop1_start, loop1_stop, 1\n"
    assert expected in code

    expected = (
        "    enddo\n")
    if dist_mem:
        expected += (
            "\n"
            "    ! set halos dirty/clean for fields modified in the above "
            "loop(s)\n"
            "    call f1_proxy%set_dirty()\n")
    expected += (
        "    call testkern_domain_code(nlayers_f1, ncell_2d_no_halos, c, "
        "f1_data, ndf_w3, undf_w3, map_w3)\n")
    assert expected in code
    if dist_mem:
        assert (
                "    ! set halos dirty/clean for fields modified in the "
                "above loop(s)\n"
                "    call f5_proxy%set_dirty()\n"
                "\n"
                "  end subroutine invoke_0" in code)

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_domain_plus_cma_kernels(dist_mem, tmpdir):
    '''
    Check that we look-up and use the number of columns with and without halos
    when an invoke contains both a domain and a CMA kernel.
    '''
    _, info = parse(os.path.join(BASE_PATH, "25.3_multikern_domain_cma.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
    code = str(psy.gen).lower()

    assert "type(mesh_type), pointer :: mesh => null()" in code
    assert "integer(kind=i_def) :: ncell_2d" in code
    assert "integer(kind=i_def) :: ncell_2d_no_halos" in code
    assert "mesh => f1_proxy%vspace%get_mesh()" in code
    assert "ncell_2d = mesh%get_ncells_2d()" in code
    assert "ncell_2d_no_halos = mesh%get_last_edge_cell()" in code
    assert ("call testkern_domain_code(nlayers_f1, ncell_2d_no_halos, b, "
            "f1_data, ndf_w3, undf_w3, map_w3)" in code)
    assert ("call columnwise_op_asm_kernel_code(cell, nlayers_lma_op1, "
            "ncell_2d, lma_op1_proxy%ncell_3d," in code)

    assert LFRicBuild(tmpdir).code_compiles(psy)
