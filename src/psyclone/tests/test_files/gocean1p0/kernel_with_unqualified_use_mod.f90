! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

!> Test kernel that contains an unqualified USE statement.
module kernel_with_use_mod
  use argument_mod
  use grid_mod
  use kernel_mod
  use kind_params_mod
  implicit none

  private

  public :: kernel_with_use, kernel_with_use_code
  
  type, extends(kernel_type) :: kernel_with_use
     type(go_arg), dimension(3) :: meta_args =                 &
          (/ go_arg(GO_READ,      GO_I_SCALAR, GO_POINTWISE),  &
             go_arg(GO_READWRITE, GO_CT,       GO_POINTWISE),  &
             go_arg(GO_READ,                   GO_GRID_MASK_T) &
           /)

     !> Although this is a boundary-conditions kernel, it only
     !! acts on the internal points of the domain
     integer :: ITERATES_OVER = GO_INTERNAL_PTS

     integer :: index_offset = GO_OFFSET_NE

  contains
    procedure, nopass :: code => kernel_with_use_code
 end type kernel_with_use

contains
 
  subroutine kernel_with_use_code(ji, jj, istep, ssha, tmask)
    use model_mod
    implicit none
    integer, intent(in)  :: ji, jj
    integer, dimension(:,:),  intent(in)    :: tmask
    integer,                  intent(in)    :: istep
    real(go_wp), dimension(:,:), intent(inout) :: ssha
    ! Locals
    real(go_wp) :: amp_tide, omega_tide, rtime

    amp_tide   = 0.2_go_wp
    omega_tide = 2.0_go_wp * 3.14159_go_wp / (12.42_go_wp * 3600._go_wp)
    rtime = real(istep) * rdt

    if(tmask(ji,jj) <= 0) return
    IF     (tmask(ji,jj-1) < 0) THEN
       ssha(ji,jj) = amp_tide * sin(omega_tide * rtime)
    ELSE IF(tmask(ji,jj+1) < 0) THEN
       ssha(ji,jj) = amp_tide * sin(omega_tide * rtime)
    ELSE IF(tmask(ji+1,jj) < 0) THEN
       ssha(ji,jj) = amp_tide * sin(omega_tide * rtime)
    ELSE IF(tmask(ji-1,jj) < 0) THEN
       ssha(ji,jj) = amp_tide * sin(omega_tide * rtime)
    END IF

  end subroutine kernel_with_use_code

end module kernel_with_use_mod
