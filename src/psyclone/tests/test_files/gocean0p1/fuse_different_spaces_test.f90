!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author A. Porter STFC Daresbury Lab
! Funded by the GOcean project

PROGRAM fuse_different_spaces_test

  ! Fake Fortran program for testing aspects of
  ! the PSyclone code generation system.

  use kind_params_mod
  use time_smooth_mod,  only: time_smooth_type
  use compute_cu_mod,   only: compute_cu_type 
  implicit none

  !> Component of vel in x at current time step
  REAL(wp), ALLOCATABLE, DIMENSION(:,:) :: u
  !> Component of vel in x at previous time step
  REAL(wp), ALLOCATABLE, DIMENSION(:,:) :: uold
  !> Pressure at current time step
  REAL(wp), ALLOCATABLE, DIMENSION(:,:) :: p
  !> Pressure at next time step
  REAL(wp), ALLOCATABLE, DIMENSION(:,:) :: pnew
  !> Pressure at previous time step
  REAL(wp), ALLOCATABLE, DIMENSION(:,:) :: pold

  !> Loop counter for time-stepping loop
  INTEGER :: ncycle
  INTEGER :: M, N

  allocate(u(m,n), uold(m,n), unew(m,n), &
           v(m,n), vold(m,n), vnew(m,n), &
           p(m,n), pold(m,n), pnew(m,n) )

  u(:,:) = 1.0d0 ; uold(:,:) = 1.0d0 ; unew(:,:) = 1.0d0
  v(:,:) = 1.0d0 ; vold(:,:) = 1.0d0 ; vnew(:,:) = 1.0d0
  p(:,:) = 1.0d0 ; pold(:,:) = 1.0d0 ; pnew(:,:) = 1.0d0

  !  ** Start of time loop ** 
  DO ncycle=1,100
    
     ! We re-use uold as cu here since this is just for
     ! testing PSyclone
     call invoke(compute_cu_type(uold, p, u),  &
                 time_smooth_type(p,pnew,pold))

  END DO

  !===================================================

END PROGRAM fuse_different_spaces_test
