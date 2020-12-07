! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2018-2019, Science and Technology Facilities Council.
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
! Author: A. R. Porter, STFC Daresbury Lab

program alg
  use kind_params_mod, only: go_wp
  use parallel_mod
  use grid_mod
  use field_mod, only: r2d_field, GO_T_POINTS
  use gocean_mod, only: gocean_initialise
  use inc_field_mod, only: inc_field
  implicit none
  !> Our domain decomposition
  type(decomposition_type) :: decomp
  integer :: my_rank
  integer :: istp, ierr, this_step
  integer :: jpiglo, jpjglo
  integer, parameter :: nsteps = 10
  
  integer, allocatable, dimension(:,:) :: tmask
  type(r2d_field) :: fld1
  type(grid_type), target :: grid1
  integer :: nx, ny
  
  ! Dimensions of our domain
  jpiglo = 50
  jpjglo = 50

  call gocean_initialise()

  ! Create our grid
  grid1 = grid_type(GO_ARAKAWA_C,                                   &
                    (/GO_BC_PERIODIC, GO_BC_PERIODIC, GO_BC_NONE/), &
                    GO_OFFSET_SW)

  !> Generate a domain decomposition
  call grid1%decompose(jpiglo, jpjglo)
  my_rank = get_rank()

  ! Set-up the T mask for the local domain. This defines the model domain.
  allocate(tmask(grid1%subdomain%global%nx, &
                 grid1%subdomain%global%ny), stat=ierr)
  if(ierr /= 0)then
     stop 'Failed to allocate T mask'
  end if

  tmask(:,:) = 0

  ! Having specified the T points mask, we can set up mesh parameters
  call grid_init(grid1, 1000.0_go_wp, 1000.0_go_wp, tmask)
  
  ! Create fields on this grid
  fld1 = r2d_field(grid1, GO_T_POINTS)

  fld1%data(:,:) = 0.0_go_wp

  nx = fld1%whole%nx
  ny = fld1%whole%ny
  do istp = 1, nsteps
     this_step = istp ! Workaround the fact that PSyclone declares all arguments
                      ! to the PSy-layer as INOUT and we can't do that for a
                      ! loop variable
     call invoke( inc_field(fld1, nx, ny, this_step) )
  end do

  ! Manually pull the data back from the GPU and do some "IO"
  !$acc update self(fld1%data)
  write (*,*) "nsteps = ", nsteps, "field(2,2) = ", fld1%data(2,2)

end program alg
