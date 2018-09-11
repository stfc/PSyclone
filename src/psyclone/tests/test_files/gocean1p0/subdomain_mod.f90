!------------------------------------------------------------------------------
! BSD 2-Clause License
! 
! Copyright (c) 2017-2018, Science and Technology Facilities Council
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
!------------------------------------------------------------------------------
! Author: A. R. Porter, STFC Daresbury Laboratory

!> Module containing the definition of the sub-domain and
!! domain-decomposition types.
module subdomain_mod
  use region_mod, only: region_type
  implicit none

  ! The subdomain type captures everything we need to know about
  ! the definition of a subdomain within a larger, global domain.
  ! e.g. if '.' indicates a grid point in the global domain that
  ! does not belong to the current subdomain, 'h' is a halo grid
  ! point in the current subdomain and 'i' is an 'internal' grid
  ! point in the current subdomain, then:
  !
  ! 14 .  .  .  .  .  .  .  .  .  .  .  .
  ! 13 .  .  .  .  .  .  .  .  .  .  .  .
  ! 12 .  .  .  .  .  .  .  .  .  .  .  .
  ! 11 .  .  .  h  h  h  h  h  h  .  .  .
  ! 10 .  .  .  h  i  i  i  i  h  .  .  .
  !  9 .  .  .  h  i  i  i  i  h  .  .  .
  !  8 .  .  .  h  i  i  i  i  h  .  .  .
  !  7 .  .  .  h  i  i  i  i  h  .  .  .
  !  6 .  .  .  h  i  i  i  i  h  .  .  .
  !  5 .  .  .  h  i  i  i  i  h  .  .  .
  !  4 .  .  .  h  i  i  i  i  h  .  .  .
  !  3 .  .  .  h  h  h  h  h  h  .  .  .
  !  2 .  .  .  .  .  .  .  .  .  .  .  .
  !  1 .  .  .  .  .  .  .  .  .  .  .  .
  !    1  2  3  4  5  6  7  8  9  10 11 12
  !             Global x index -->
  !
  ! Position of the internal part of the subdomain in the global domain
  ! global%(xstart, ystart) = (5, 4), global%(xstop, ystop) = (8, 10)
  ! Width and height of the *whole* subdomain
  ! global%nx = 9 - 4 + 1 = 6
  ! global%ny = 11 - 3 + 1 = 9
  !
  ! Position of the 'internal' region (i.e. excluding halos, boundary
  ! points) within the sub-domain.
  ! internal%(xstart, ystart) = (2, 2), internal%(xstop, ystop) = (5, 8)
  ! Width and height of the internal region
  ! internal%nx = 5 - 2 + 1 = 4
  ! internal%ny = 8 - 1 + 1 = 7

  !> Type encapsulating the information required to define a single
  !! sub-domain
  type :: subdomain_type
     !> The definition of this subdomain in terms of the global domain
     type(region_type) :: global
     !> The internal region of this subdomain (excluding halo and
     !! boundary points)
     type(region_type) :: internal
  end type subdomain_type

  !> Type encapsulating all information regarding a regular, 2D
  !! domain decomposition
  type :: decomposition_type
     !> Dimensions of the grid of sub-domains
     integer :: nx, ny
     !> Number of sub-domains (=nx*ny)
     integer :: ndomains
     !> Max dimensions of any sub-domain
     integer :: max_width, max_height
     !> Array of the sub-domain definitions
     type(subdomain_type), allocatable :: subdomains(:)
  end type decomposition_type

contains

  !> Decompose a domain consisting of domainx x domainy points
  !! into a 2D grid.
  !! Returns an array of tiles describing each of the subdomains.    
  function decompose(domainx, domainy,             &
                     ndomains, ndomainx, ndomainy, &
                     halo_width) result(decomp)
    use parallel_mod, only: get_num_ranks, parallel_abort
    implicit none
    !> The decomposition that this function will return
    type(decomposition_type), target :: decomp
    !> Dimensions of the domain to decompose
    integer, intent(in) :: domainx, domainy
    !> No. of domains to decompose into. If not supplied will use the
    !! number of MPI ranks (or 1 if serial).
    integer, intent(in), optional :: ndomains
    !> Optional specification of the dimensions of the tiling grid
    integer, intent(in), optional :: ndomainx, ndomainy
    !> Width of halo to allow for when constructing sub-domains.
    !! Default value is 1. Must be > 0 for a multi-processor run.
    integer, intent(in), optional :: halo_width
    ! Locals
    integer :: ival, jval ! For tile extent calculation
    integer :: internal_width, internal_height
    integer :: ierr, nwidth
    integer :: ji,jj, ith
    integer :: junder, height
    integer :: iunder, width
    ! For doing stats on tile sizes
    integer :: nvects, nvects_sum, nvects_min, nvects_max 
    logical, parameter :: print_tiles = .TRUE.
    ! Whether to automatically compute the dimensions of the tiling grid
    logical :: auto_tile = .TRUE.
    integer :: xlen, ylen
    integer :: ntilex, ntiley
    ! Local var to hold number of sub-domains
    integer :: ndom = 1
    ! Local var to hold halo width
    integer :: hwidth = 1
    ! Pointer to the current subdomain being defined
    type(subdomain_type), pointer :: subdomain

    ! Deal with optional arguments to this routine
    if(.not. present(ndomains))then
       if(.not. present(ndomainx) .and. .not. present(ndomainy))then
          ! Automatically use the number of MPI processes
          ndom = get_num_ranks()
          auto_tile = .TRUE.
       else if(present(ndomainx) .and. present(ndomainy))then
          ndom = ndomainx * ndomainy
          auto_tile = .FALSE.
       else
          call parallel_abort('decompose: invalid arguments supplied')
       end if
    else
       ndom = ndomains
       auto_tile = .TRUE.
    end if

    if(present(halo_width))then
       if(halo_width < 1 .and. get_num_ranks() > 1)then
          call parallel_abort('decompose: halo width must be > 0 if '// &
                              'running on more than one process')
       end if
       hwidth = halo_width
    end if

    decomp%ndomains = ndom
    allocate(decomp%subdomains(ndom), Stat=ierr)
    if(ierr /= 0)then
       call parallel_abort('decompose: failed to allocate tiles array')
    end if

    xlen = domainx
    ylen = domainy
    
    if(auto_tile)then

       ntilex = INT( SQRT(REAL(ndom)) )
       do WHILE(MOD(ndom, ntilex) /= 0)
          ntilex = ntilex - 1
       end do
       ntiley = ndom/ntilex

       ! Match longest dimension of domain to longest dimension of 
       ! process grid
       if(xlen > ylen)then
          if( ntilex < ntiley )then
             ierr   = ntiley
             ntiley = ntilex
             ntilex = ierr
          end if
       else
          ! N >= M so want nprocy >= nprocx
          if( ntiley < ntilex )then
             ierr   = ntiley
             ntiley = ntilex
             ntilex = ierr
          end if
       end if
    else
       ntilex = ndomainx
       ntiley = ndomainy
    end if ! automatic determination of tiling grid

    WRITE (*,"('decompose: using grid of ',I3,'x',I3)") ntilex, ntiley
    decomp%nx = ntilex
    decomp%ny = ntiley

    ! If we think about the internal regions of the tiles,
    ! then they should share the domain between them:
    internal_width = xlen / ntilex
    internal_height = ylen / ntiley

    ! Integer arithmetic means that ntiley tiles of height
    ! internal_height might actually span a height less than ylen. If
    ! so, we try and increase the height of each row by just one until
    ! we've accounted for the <jover> extra rows.
    nwidth = ntiley * internal_height
    if(nwidth < ylen)then
       junder = ylen - nwidth
    else
       junder = 0
    end if
    ! Ditto for x dimension
    nwidth = ntilex * internal_width
    if(nwidth < xlen)then
       iunder = xlen - nwidth
    else
       iunder = 0
    end if

    if(print_tiles)then
       write(*, "('Tile width = ',I4,', tile height = ',I4)") &
                                  internal_width, internal_height
       write(*, "('iunder, junder = ', I3, 1x, I3)") iunder, junder
    end if

    ! We start with the first domain (where else?)
    ith = 1
    ! The starting point of the tiles in y
    jval = 1

    nvects_max = 0
    nvects_min = 1000000
    nvects_sum = 0
    decomp%max_width  = 0
    decomp%max_height = 0

    if(print_tiles) write(*, "(/'Sub-domains:')")

    do jj = 1, ntiley, 1

       ! Point to subdomain we are about to define (prevents compiler
       ! warning for assignment following ji-loop below).
       subdomain => decomp%subdomains(ith)

       ! If necessary, correct the height of this tile row
       if(junder > 0)then
          height = internal_height + 1
          junder = junder - 1
       else
          height = internal_height
       end if

       ! The starting point of the tiles in x
       ival = 1

       do ji = 1, ntilex, 1

          ! If necessary, correct the width of this tile column
          if(iunder > 0)then
             width = internal_width + 1
             iunder = iunder - 1
          else
             width = internal_width
          end if

          subdomain => decomp%subdomains(ith)
          subdomain%internal%xstart = hwidth+1
          subdomain%internal%xstop = subdomain%internal%xstart+width-1
          subdomain%internal%nx = width
          ! Which part of the global domain the internal part of this
          ! subdomain covers
          subdomain%global%xstart = ival
          subdomain%global%xstop = subdomain%global%xstart + width - 1
          ! Full width of this subdomain (including halo and boundary points)
          subdomain%global%nx  = 2*hwidth + width

          subdomain%internal%ystart = hwidth+1
          subdomain%internal%ystop = subdomain%internal%ystart+height-1
          subdomain%internal%ny = height
          ! Which part of the global domain this subdomain covers
          subdomain%global%ystart = jval
          subdomain%global%ystop = subdomain%global%ystart + height - 1
          ! Full height of this subdomain (incl. halo and boundary points)
          subdomain%global%ny = 2*hwidth + subdomain%internal%ny

          if(print_tiles)then
             write(*, "('subdomain[',I4,'](',I4,':',I4,')(',I4,':',I4,'),"// &
                  & " interior:(',I4,':',I4,')(',I4,':',I4,') ')")      &
                  ith,                                                  &
                  subdomain%global%xstart, subdomain%global%xstop,      &
                  subdomain%global%ystart, subdomain%global%ystop,      &
                  subdomain%internal%xstart, subdomain%internal%xstop,  &
                  subdomain%internal%ystart, subdomain%internal%ystop
          end if

          ! Collect some data on the distribution of tile sizes for 
          ! loadbalance info
          nvects = subdomain%internal%nx * subdomain%internal%ny
          nvects_sum = nvects_sum + nvects
          nvects_min = min(nvects_min, nvects)
          nvects_max = max(nvects_max, nvects)

          ! For use when allocating tile-'private' work arrays
          decomp%max_width  = max(decomp%max_width, subdomain%global%nx)
          decomp%max_height = max(decomp%max_height, subdomain%global%ny)

          ival = subdomain%global%xstop + 1
          ith = ith + 1
       end do
       jval = subdomain%global%ystop + 1
    end do

    ! Print tile-size statistics
    if(print_tiles)then
       write(*, "(/'Mean sub-domain size = ',F8.1,' pts = ',F7.1,' KB')")    &
                                   real(nvects_sum) / real(decomp%ndomains), &
                            real(8*nvects_sum) / real(decomp%ndomains*1024)
       write(*, "('Min,max sub-domain size (pts) = ',I6,',',I6)") &
                                                      nvects_min, nvects_max
       write(*, "('Domain load imbalance (%) =',F6.2)") &
                             100.0*(nvects_max-nvects_min) / real(nvects_min)
       write(*, "('Max sub-domain dims are ',I4,'x',I4/)") decomp%max_width, &
                                                          decomp%max_height
    end if

  end function decompose

end module subdomain_mod
