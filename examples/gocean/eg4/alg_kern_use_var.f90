! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2019-2024, Science and Technology Facilities Council.
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
! Author: A. R. Porter, STFC Daresbury Lab.
! Modified: S. Siso, STFC Daresbury Laboratory

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
