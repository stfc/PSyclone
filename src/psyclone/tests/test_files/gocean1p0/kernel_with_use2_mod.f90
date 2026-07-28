! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module kernel_with_use2_mod
  use argument_mod
  use grid_mod
  use kernel_mod
  use kind_params_mod
  implicit none

  private

  public bc_ssh
  public bc_ssh_code

  !=======================================

  type, extends(kernel_type) :: kernel_with_use2
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
    procedure, nopass :: code => kernel_with_use2_code
 end type kernel_with_use2

contains
 
  subroutine kernel_with_use2_code(ji, jj, istep, ssha, tmask)
    use model_mod, only: cbfr, rdt
    implicit none
    integer, intent(in)  :: ji, jj
    integer, dimension(:,:),  intent(in)    :: tmask
    integer,                  intent(in)    :: istep
    real(go_wp), dimension(:,:), intent(inout) :: ssha


    if(tmask(ji,jj) > 0) then
       ssha(ji,jj) = ssha(ji,jj) * cbfr * rdt
    END IF

  end subroutine kernel_with_use2_code

end module kernel_with_use2_mod
