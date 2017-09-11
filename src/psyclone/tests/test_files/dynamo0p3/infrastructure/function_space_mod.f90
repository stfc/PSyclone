!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------

! BSD 3-Clause License
!
! Modifications copyright (c) 2017, Science and Technology Facilities Council
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! * Redistributions of source code must retain the above copyright notice, this
!   list of conditions and the following disclaimer.
!
! * Redistributions in binary form must reproduce the above copyright notice,
!   this list of conditions and the following disclaimer in the documentation
!   and/or other materials provided with the distribution.
!
! * Neither the name of the copyright holder nor the names of its
!   contributors may be used to endorse or promote products derived from
!   this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------

module function_space_mod

use constants_mod,         only: r_def, i_def
use mesh_mod,              only: mesh_type
use stencil_dofmap_mod,    only: stencil_dofmap_type, STENCIL_POINT

implicit none

private

type, public :: function_space_type

  private

  integer(i_def) :: ndof_cell
  integer(i_def) :: ndof_glob
  integer(i_def) :: ndof_interior
  integer(i_def) :: ndof_exterior
  integer(i_def) :: ndof_vert
  integer(i_def) :: ndof_edge
  integer(i_def) :: ndof_face
  integer(i_def) :: ndof_vol
  integer(i_def) :: fs
  integer(i_def) :: element_order
  integer(i_def) :: fs_order
  integer(i_def) :: dim_space
  integer(i_def) :: dim_space_diff
  type(mesh_type), pointer :: mesh => null()
  real(r_def),    allocatable :: nodal_coords(:,:)
  integer(i_def), allocatable :: dof_on_vert_boundary(:,:)

  integer(i_def), allocatable :: basis_order(:,:)
  integer(i_def), allocatable :: basis_index(:,:)
  real(r_def),    allocatable :: basis_vector(:,:)
  real(r_def),    allocatable :: basis_x(:,:,:)

  integer(i_def), allocatable :: global_dof_id(:)

  integer(i_def) :: last_dof_owned
  integer(i_def) :: last_dof_annexed
  integer(i_def), allocatable :: last_dof_halo(:)

contains

  procedure, public :: get_undf

  procedure, public :: get_ncell
  procedure, public :: get_nlayers
  procedure, public :: get_cell_dofmap
  procedure, public :: get_whole_dofmap
  procedure, public :: get_ndf
  procedure, public :: get_nodes
  procedure, public :: which
  procedure, public :: get_boundary_dofs
  procedure, public :: evaluate_function
  procedure, public :: evaluate_basis
  procedure, public :: evaluate_diff_basis
  procedure, public :: compute_basis_function
  procedure, public :: compute_nodal_basis_function
  procedure, public :: compute_diff_basis_function
  procedure, public :: compute_nodal_diff_basis_function
  procedure, public :: get_dim_space
  procedure, public :: get_dim_space_diff
  procedure, public :: get_mesh
  procedure, public :: get_mesh_id
  procedure, public :: get_element_order
  procedure, public :: get_fs_order
  procedure get_last_dof_owned
  procedure get_last_dof_halo
  procedure, public   :: get_stencil_dofmap
  procedure, public  :: get_colours
  procedure, public  :: get_ncolours
  procedure, public  :: set_colours

end type function_space_type

contains

integer function get_undf(self)
  implicit none
  class(function_space_type), intent(in) :: self

  get_undf = 0
  return
end function get_undf

function get_ncell(self) result(ncells_2d)

  implicit none
  class(function_space_type), intent(in) :: self
  integer(i_def) :: ncells_2d

  ncells_2d = 0
  return
end function get_ncell

function get_nlayers(self) result(nlayers)

  implicit none
  class(function_space_type), intent(in) :: self
  integer(i_def) :: nlayers

  nlayers = 0
  return
end function get_nlayers

function get_ndf(self) result(ndof_cell)

  implicit none
  class(function_space_type), intent(in) :: self
  integer(i_def) :: ndof_cell

  ndof_cell = 0

  return
end function get_ndf

function get_cell_dofmap(self,cell_lid) result(map)

  implicit none
  class(function_space_type), target, intent(in) :: self
  integer(i_def),                     intent(in) :: cell_lid
  integer(i_def), pointer                        :: map(:)
  map => null()
  return
end function get_cell_dofmap

function get_whole_dofmap(self) result(map)

  implicit none
  class(function_space_type), target, intent(in) :: self
  integer(i_def), pointer                        :: map(:,:)

  map => null()
  return
end function get_whole_dofmap

function get_nodes(self) result(nodal_coords)

  implicit none
  class(function_space_type), target, intent(in)  :: self
  real(r_def), pointer :: nodal_coords(:,:)

  nodal_coords => null()
  return
end function get_nodes

function get_boundary_dofs(self) result(boundary_dofs)

  implicit none
  class(function_space_type), target, intent(in) :: self
  integer(i_def), pointer :: boundary_dofs(:,:)

  boundary_dofs => null()
  return
end function get_boundary_dofs

function which(self) result(fs)

  implicit none
  class(function_space_type),  intent(in) :: self
  integer(i_def) :: fs

  fs = 0
  return
end function which

function get_dim_space(self) result(dim)
  implicit none
  class(function_space_type), intent(in) :: self
  integer(i_def) :: dim

  dim = 0
  return
end function get_dim_space

function get_dim_space_diff(self) result(dim)
  implicit none
  class(function_space_type), intent(in) :: self
  integer(i_def) :: dim

  dim = 0
  return
end function get_dim_space_diff

function evaluate_function(self, func_to_call, df, xi) result(evaluate)

  class(function_space_type)  :: self
  integer(i_def), intent(in)  :: func_to_call
  integer(i_def), intent(in)  :: df
  real(r_def),    intent(in)  :: xi(3)
  real(r_def),   allocatable  :: evaluate(:)

  allocate(evaluate(1))
  evaluate(:) = 1.0_r_def
end function evaluate_function

function evaluate_basis(self, df, xi) result(p)

  class(function_space_type), intent(in)  :: self
  integer(i_def), intent(in)  :: df
  real(r_def),    intent(in)  :: xi(3)
  real(r_def)                 :: p(self%dim_space)

  p(:) = 1.0_r_def
end function evaluate_basis

pure function evaluate_diff_basis(self, df, xi) result(evaluate)

  class(function_space_type), intent(in)  :: self
  integer(i_def), intent(in)  :: df
  real(r_def),    intent(in)  :: xi(3)
  real(r_def)                 :: evaluate(self%dim_space_diff)
  real(r_def)                 :: dpdx(3)

  evaluate(:) = 1.0_r_def
end function evaluate_diff_basis

subroutine compute_basis_function(self, basis, ndf, qp_h, qp_v, x_qp, z_qp)

  implicit none

  class(function_space_type), intent(in) :: self

  integer(i_def), intent(in)  :: ndf
  integer(i_def), intent(in)  :: qp_h
  integer(i_def), intent(in)  :: qp_v

  real(r_def), intent(in)  :: x_qp (qp_h,2)
  real(r_def), intent(in)  :: z_qp (qp_v)
  real(r_def), intent(out) :: basis(self%dim_space,ndf,qp_h,qp_v)

  basis = 0.0_r_def
end subroutine compute_basis_function

subroutine compute_nodal_basis_function(self, basis, ndf, n_node, x_node)

  implicit none
  class(function_space_type), intent(in)  :: self

  integer(i_def), intent(in) :: ndf
  integer(i_def), intent(in) :: n_node

  real(r_def), dimension(3,n_node),                  intent(in)  :: x_node
  real(r_def), dimension(self%dim_space,ndf,n_node), intent(out) :: basis

  basis = 0.0_r_def
end subroutine compute_nodal_basis_function

subroutine compute_diff_basis_function(self,                                & 
                                       dbasis,                              &
                                       ndf,                                 &
                                       qp_h,                                &
                                       qp_v,                                &
                                       x_qp,                                &
                                       z_qp)
  implicit none

  class(function_space_type), intent(in) :: self

  integer(i_def), intent(in) :: ndf
  integer(i_def), intent(in) :: qp_h
  integer(i_def), intent(in) :: qp_v

  real(r_def), intent(in)  :: x_qp(qp_h,2)
  real(r_def), intent(in)  :: z_qp(qp_v)

  real(r_def), intent(out) :: dbasis(self%dim_space_diff, ndf, qp_h, qp_v)

  dbasis = 0.0_r_def
end subroutine compute_diff_basis_function

subroutine compute_nodal_diff_basis_function(self, &
     dbasis, ndf, n_node, x_node)
  implicit none
  class(function_space_type), intent(in)  :: self
  integer,                                                intent(in)  :: ndf
  integer,                                                intent(in)  :: n_node
  real(kind=r_def), dimension(3,n_node),                  intent(in)  :: x_node
  real(kind=r_def), dimension(self%dim_space_diff,ndf,n_node), intent(out) :: dbasis

  dbasis = 0.0_r_def  
end subroutine compute_nodal_diff_basis_function

function get_element_order(self) result (element_order)

  implicit none
  class(function_space_type), intent(in) :: self
  integer(i_def) :: element_order

  element_order = 0
  return
end function get_element_order

function get_fs_order(self) result (fs_order)

  implicit none
  class(function_space_type), intent(in) :: self
  integer(i_def) :: fs_order

  fs_order = 0
  return
end function get_fs_order

function get_mesh(self) result (mesh)

  implicit none

  class(function_space_type), intent(in) :: self
  type(mesh_type), pointer :: mesh

  mesh => null()
  return
end function get_mesh

function get_mesh_id(self) result (mesh_id)
  implicit none
  class(function_space_type), intent(in) :: self
  integer(i_def) :: mesh_id
  
  mesh_id = 0
  return
end function get_mesh_id

function get_last_dof_owned(self) result (last_dof_owned)

  implicit none
  class(function_space_type) :: self
  integer(i_def) :: last_dof_owned

  last_dof_owned = 0
  return
end function get_last_dof_owned

function get_last_dof_halo(self) result (last_dof_halo)

  implicit none
  class(function_space_type) :: self

  integer(i_def) :: last_dof_halo

  last_dof_halo = 0
  return
end function get_last_dof_halo

function get_stencil_dofmap(self, stencil_shape, stencil_size) result(map)

  implicit none

  class(function_space_type), intent(inout) :: self
  integer(i_def),             intent(in) :: stencil_shape
  integer(i_def),             intent(in) :: stencil_size
  type(stencil_dofmap_type), pointer  :: map

  map => null()
end function get_stencil_dofmap

subroutine set_colours(self)
  implicit none
  class(function_space_type), intent(inout) :: self

  return
end subroutine set_colours

function get_ncolours(self) result(ncolours)
  implicit none
  class(function_space_type), intent(in) :: self
  integer(i_def)                         :: ncolours

  ncolours = 0
end function get_ncolours

subroutine get_colours(self, ncolours, ncells_per_colour, colour_map)
  implicit none
  class(function_space_type), intent(in)    :: self
  integer(i_def), intent(out)               :: ncolours
  integer(i_def), pointer, intent(out)  :: ncells_per_colour(:)
  integer(i_def), pointer, intent(out)  :: colour_map(:,:)

  ncolours = 0
  ncells_per_colour => null()
  colour_map => null()

end subroutine get_colours

end module function_space_mod
