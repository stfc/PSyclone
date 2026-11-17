! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program alg
    use field_mod
    use gocean_mod
    use grid_mod
    use kind_params_mod, only: go_wp
    use parallel_mod, only: get_rank, on_master

    use kern_use_var_mod, only: kern_use_var

    implicit none

    real(go_wp) :: dx = 1.0
    real(go_wp) :: dy = 1.0

    type(grid_type), target :: model_grid
    type(r2d_field) :: fld1
    integer, allocatable :: tmask(:,:)
    integer :: ierr

    call gocean_initialise()

    !> Create our grid object
    model_grid = grid_type(GO_ARAKAWA_C, &
                           (/GO_BC_EXTERNAL,GO_BC_EXTERNAL,GO_BC_NONE/), &
                           GO_OFFSET_NE)
    !> Generate a domain decomposition. Automatically uses the number of
    !! available MPI ranks.
    call model_grid%decompose(100, 100, 1, 1, 1)

    !> Create a T-point mask describing the (local) domain
    allocate(tmask(model_grid%subdomain%global%nx,  &
                   model_grid%subdomain%global%ny), Stat=ierr)
    if(ierr /= 0)then
       call gocean_stop('Failed to allocate T-mask')
    end if
    ! To keep things simple for this example we set all points to be wet
    ! and within the domain
    tmask(:,:) = 1
    !> Complete the initialisation of the grid using the T-mask and
    !! grid resolution
    call grid_init(model_grid, dx, dy, tmask)

    !> Create a field on U-points of the grid
    fld1 = r2d_field(model_grid, GO_U_POINTS)

    ! Invoke calls a kernel which USEs a module variable
    call invoke(kern_use_var(fld1))
end program alg
