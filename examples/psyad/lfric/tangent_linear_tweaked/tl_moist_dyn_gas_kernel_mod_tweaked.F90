!-----------------------------------------------------------------------------
! (c) Crown copyright 2022-2025 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief Compute the tangent linear for moisture-dependent factors

! recip_epsilon is passed via a use statement so PSyAD does not know
! its type so raises an exception. Here we declare a temporary with
! the correct type. This allows the kernel to be processed by PSyAD
! and then the original use statement can be re-instated and this
! temporary variable removed.

module tl_moist_dyn_gas_kernel_mod

    use argument_mod,                  only: arg_type,          &
                                             GH_FIELD, GH_REAL, &
                                             GH_SCALAR,         &
                                             GH_WRITE, GH_READ, &
                                             CELL_COLUMN
    use constants_mod,                 only: r_def, i_def
    use fs_continuity_mod,             only: Wtheta
    use kernel_mod,                    only: kernel_type
    use planet_config_mod,             only: recip_epsilon

    implicit none

    private

    !--------------------------------------------------------------------------
    ! Public types
    !--------------------------------------------------------------------------
    !> The type declaration for the kernel. Contains the metadata needed by the Psy layer
    type, public, extends(kernel_type) :: tl_moist_dyn_gas_kernel_type
        private
        type(arg_type) :: meta_args(2) = (/                 &
             arg_type(GH_FIELD, GH_REAL, GH_WRITE, Wtheta), &
             arg_type(GH_FIELD, GH_REAL, GH_READ,  Wtheta)  &
             /)
        integer :: operates_on = CELL_COLUMN
    contains
        procedure, nopass :: tl_moist_dyn_gas_code
    end type

    !--------------------------------------------------------------------------
    ! Contained functions/subroutines
    !--------------------------------------------------------------------------
    public :: tl_moist_dyn_gas_code

contains

    !> @brief Compute the tangent linear moist dynamical factors
    !! @param[in]     nlayers       Integer the number of layers
    !! @param[in,out] moist_dyn_gas Change in Gas factor (m_v / epsilon)
    !! @param[in]     mr_v          Change in Water vapour mixing ratio
    !! @param[in]     ndf_wtheta    The number of degrees of freedom per cell
    !!                              for wtheta
    !! @param[in]     udf_wtheta    The number of total degrees of freedom for
    !!                              wtheta
    !! @param[in]     map_wtheta    Integer array holding the dofmap for the
    !!                              cell at the base of the column
    subroutine tl_moist_dyn_gas_code( nlayers, moist_dyn_gas, mr_v,         &
                                      ndf_wtheta, undf_wtheta, map_wtheta )

        implicit none

        ! Arguments
        integer(kind=i_def), intent(in) :: nlayers, ndf_wtheta, undf_wtheta

        real(kind=r_def), dimension(undf_wtheta),   intent(inout) :: moist_dyn_gas
        real(kind=r_def), dimension(undf_wtheta),   intent(in)    :: mr_v
        integer(kind=i_def), dimension(ndf_wtheta), intent(in)    :: map_wtheta

        ! Internal variables
        integer(kind=i_def)                 :: k, df

        real(kind=r_def)                    :: mr_v_at_dof
        real(kind=r_def)                    :: recip_epsilon_tmp

        ! compute the pointwise mr profile
        do k = 0, nlayers-1

          do df = 1, ndf_wtheta
            mr_v_at_dof  = mr_v(map_wtheta(df) + k)
            moist_dyn_gas(map_wtheta(df) + k) = recip_epsilon_tmp * mr_v_at_dof
          end do

        end do

    end subroutine tl_moist_dyn_gas_code

end module tl_moist_dyn_gas_kernel_mod
