! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

! Example of a Kernel containing a call to another routine that exists in
! the same module.
module testkern_with_local_call_mod

  use argument_mod, only: arg_type, func_type, GH_FIELD, GH_SCALAR, GH_REAL, &
       GH_WRITE, GH_READ, GH_EVALUATOR, GH_BASIS, ANY_SPACE_9, CELL_COLUMN
  use fs_continuity_mod, only: w3
  use kernel_mod, only: kernel_type
  use constants_mod, only: r_def, i_def
  use coord_transform_mod, only: xyz2llr

  implicit none

  type, extends(kernel_type) :: testkern_with_local_call_type
  private
  type(arg_type) :: meta_args(4) = (/                         &
       arg_type(gh_field, gh_real, gh_write, w3),             &
       arg_type(gh_field * 3, gh_real, gh_read, any_space_9), &
       arg_type(gh_scalar, gh_real, gh_read),                 &
       arg_type(gh_scalar, gh_real, gh_read)/)
  TYPE(func_type) :: meta_funcs(1) = (/func_type(any_space_9, gh_basis)/)
  INTEGER :: operates_on = cell_column
  INTEGER :: gh_shape = gh_evaluator
   contains
     procedure, nopass :: code => testkern_with_local_call_code
  end type testkern_with_local_call_type

  interface a_local_polymorph
     module procedure local1, local2
  end interface

  private :: a_local_routine, a_local_polymorph, local1, local2

contains

  subroutine a_local_routine(g)
    real(kind=r_def), intent(inout) :: g
    real(kind=r_def) :: lon, lat, radius
    real(kind=r_def), parameter :: unity = 1.0_r_def
    g = g + unity
    ! Call to a local interface.
    call a_local_polymorph(g)
    ! Call to a routine imported from external module.
    call xyz2llr(1.0_r_def, 1.0_r_def, 1.0_r_def, lon, lat, radius)
  end subroutine a_local_routine

  subroutine local1(arg)
    integer(kind=i_def), intent(inout) :: arg
    arg = arg + 1_i_def
  end subroutine local1

  subroutine local2(arg)
    real(kind=r_def), intent(inout) :: arg
    integer(kind=i_def) :: ilocal
    arg = arg + 1.0_r_def
    ilocal = NINT(arg)
    ! Repeatedly call the same local routine.
    call local1(ilocal)
    call local1(ilocal)
  end subroutine local2

  subroutine testkern_with_local_call_code(nlayers, phi, chi_1, chi_2, chi_3, &
       gravity, planet_radius, ndf_w3, undf_w3, map_w3, &
       ndf_chi, undf_chi, map_chi, chi_basis)
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: undf_w3
    integer(kind=i_def), intent(in) :: ndf_chi
    integer(kind=i_def), intent(in) :: undf_chi
    integer(kind=i_def), dimension(ndf_w3), intent(in) :: map_w3
    integer(kind=i_def), dimension(ndf_chi), intent(in) :: map_chi
    real(kind=r_def), dimension(undf_w3), intent(inout) :: phi
    real(kind=r_def), dimension(undf_chi), intent(in) :: chi_1
    real(kind=r_def), dimension(undf_chi), intent(in) :: chi_2
    real(kind=r_def), dimension(undf_chi), intent(in) :: chi_3
    real(kind=r_def), dimension(1,ndf_chi,ndf_w3), intent(in) :: chi_basis
    real(kind=r_def), intent(in) :: gravity
    real(kind=r_def), intent(in) :: planet_radius
    integer(kind=i_def) :: df
    integer(kind=i_def) :: dfc
    integer(kind=i_def) :: k
    real(kind=r_def), dimension(3) :: coord
    real(kind=r_def) :: lat
    real(kind=r_def) :: lon
    real(kind=r_def) :: radius
    real(kind=r_def) :: shallow_switch
    real(kind=r_def) :: height
    real(kind=r_def), dimension(ndf_chi) :: chi_1_e
    real(kind=r_def), dimension(ndf_chi) :: chi_2_e
    real(kind=r_def), dimension(ndf_chi) :: chi_3_e
    real(kind=r_def) :: phi_shallow
    real(kind=r_def) :: phi_deep
    real(kind=r_def) :: my_gravity

    do k = 0, nlayers-1
       do dfc = 1, ndf_chi
          chi_1_e(dfc) = chi_1( map_chi(dfc) + k)
          chi_2_e(dfc) = chi_2( map_chi(dfc) + k)
          chi_3_e(dfc) = chi_3( map_chi(dfc) + k)
       end do

       do df = 1, ndf_w3
          coord(:) = 0.0_r_def
          do dfc = 1, ndf_chi
             coord(1) = coord(1) + chi_1_e(dfc)*chi_basis(1,dfc,df)
             coord(2) = coord(2) + chi_2_e(dfc)*chi_basis(1,dfc,df)
             coord(3) = coord(3) + chi_3_e(dfc)*chi_basis(1,dfc,df)
          end do
          ! A call to a routine that is in the same module as this kernel.
          my_gravity = gravity
          call a_local_routine(my_gravity)

          phi_shallow = gravity * radius
          phi_deep = -gravity * planet_radius * &
               (planet_radius / radius - 1.0_r_def)
          phi(map_w3(df) + k) = shallow_switch * phi_shallow + &
               (1.0_r_def - shallow_switch) * phi_deep
       end do
    end do

  end subroutine testkern_with_local_call_code

end module testkern_with_local_call_mod
