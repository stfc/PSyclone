! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2013-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

module summation_w0_to_w3_kernel_mod

  use argument_mod
  use kernel_mod
  use fs_continuity_mod, only: w0, W3

  use constants_mod

  implicit none

  private

  type, public, extends(kernel_type) :: summation_w0_to_w3_kernel_type
     private
     type(arg_type), dimension(2) :: meta_args =            &
          (/ arg_type(gh_field, gh_real, gh_readwrite, w3), &
             arg_type(gh_field, gh_real, gh_read,      w0)  &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => summation_w0_to_w3_kernel_code
  end type summation_w0_to_w3_kernel_type

  public :: summation_w0_to_w3_kernel_code

contains

    ! This kernel adds all values from the field on W0 to the corresponding
    ! element in W3.

    SUBROUTINE summation_w0_to_w3_kernel_code(nlayers, field_w3, field_w0, ndf_w3, &
                                              undf_w3, map_w3, ndf_w0, undf_w0, map_w0)
      USE constants_mod
      IMPLICIT NONE
      INTEGER(KIND=i_def), intent(in)                     :: nlayers
      INTEGER(KIND=i_def), intent(in)                     :: ndf_w0
      INTEGER(KIND=i_def), intent(in), dimension(ndf_w0)  :: map_w0
      INTEGER(KIND=i_def), intent(in)                     :: ndf_w3
      INTEGER(KIND=i_def), intent(in), dimension(ndf_w3)  :: map_w3
      INTEGER(KIND=i_def), intent(in)                     :: undf_w3, undf_w0
      REAL(KIND=r_def), intent(inout), dimension(undf_w3) :: field_w3
      REAL(KIND=r_def), intent(in), dimension(undf_w0)    :: field_w0

      integer(kind=i_def)                                 :: i, k

      do k=0, nlayers-1
        do i=1, ndf_w0
          field_w3(map_w3(1)+k) = field_w3(map_w3(1)+k) + field_w0(map_w0(i)+k)
        end do
      end do

    END SUBROUTINE summation_w0_to_w3_kernel_code

end module summation_w0_to_w3_kernel_mod
