!-------------------------------------------------------------------------------
! (c) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!>  @brief   Routines for transforming the chi coordinate fields
!!
!!  @details Contains routines for conversion of chi coordinate fields. These
!!           are accessed through the chi2ABC interface functions, so that
!!           which coord_system chi is in, it will convert the
!!           coordinates to the ABC system
!------------------------------------------------------------------------------
module chi_transform_mod

use constants_mod,             only : r_def, i_def
use coord_transform_mod,       only : alphabetar2llr, &
                                      alphabetar2xyz, &
                                      xyz2alphabetar, &
                                      llr2xyz, xyz2llr
use planet_config_mod,         only : scaled_radius

implicit none

private

! Public subroutines
public :: init_chi_transforms
public :: final_chi_transforms
public :: chi2xyz
public :: chi2abr
public :: chi2llr
public :: chir2xyz

!------------------------------------------------------------------------------
! Interfaces for coordinate transformations
!------------------------------------------------------------------------------

interface

  !-----------------------------------------------------------------------------
  !> @brief Transforms a coordinate field chi from any system into global
  !>        Cartesian (X,Y,Z) coordinates. If chi is in a spherical coordinate
  !>        system, the third coordinate should be height, and the scaled_radius
  !>        will be added to the height to give the radius before the coordinates
  !>        are transformed to (X,Y,Z) coordinates.
  !!
  !! @param[in]   chi_1      The first coordinate field in
  !! @param[in]   chi_2      The second coordinate field in
  !! @param[in]   chi_3      The third coordinate field in
  !! @param[in]   panel_id   The mesh panel ID
  !! @param[out]  x          The first coordinate field out (global Cartesian X)
  !! @param[out]  y          The second coordinate field out (global Cartesian Y)
  !! @param[out]  z          The third coordinate field out (global Cartesian Z)
  !-----------------------------------------------------------------------------
  subroutine chi2xyz_interface(chi_1, chi_2, chi_3, panel_id, x, y, z)

    import :: r_def, i_def

    implicit none

    integer(kind=i_def), intent(in)  :: panel_id
    real(kind=r_def),    intent(in)  :: chi_1, chi_2, chi_3
    real(kind=r_def),    intent(out) :: x, y, z

  end subroutine chi2xyz_interface

end interface

interface

  !-----------------------------------------------------------------------------
  !> @brief Transforms a coordinate field chi from any system into spherical polar
  !>        (longitude, latitude, radius) coordinates
  !!
  !! @param[in]   chi_1      The first coordinate field in
  !! @param[in]   chi_2      The second coordinate field in
  !! @param[in]   chi_3      The third coordinate field in
  !! @param[in]   panel_id   The mesh panel ID
  !! @param[out]  longitude  The first coordinate field out (longitude)
  !! @param[out]  latitude   The second coordinate field out (latitude)
  !! @param[out]  radius     The third coordinate field out (radius)
  !-----------------------------------------------------------------------------
  subroutine chi2llr_interface(chi_1, chi_2, chi_3, panel_id, lon, lat, r)

    import :: r_def, i_def

    implicit none

    integer(kind=i_def), intent(in)  :: panel_id
    real(kind=r_def),    intent(in)  :: chi_1, chi_2, chi_3
    real(kind=r_def),    intent(out) :: lon, lat, r

  end subroutine chi2llr_interface

end interface

interface

  !-----------------------------------------------------------------------------
  !> @brief Transforms a coordinate field chi from any system into equiangular
  !>        cubed sphere (alpha,beta,radius) coordinates
  !!
  !! @param[in]   chi_1      The first coordinate field in
  !! @param[in]   chi_2      The second coordinate field in
  !! @param[in]   chi_3      The third coordinate field in
  !! @param[in]   panel_id   The mesh panel ID
  !! @param[out]  alpha      The first coordinate field out (alpha)
  !! @param[out]  beta       The second coordinate field out (beta)
  !! @param[out]  radius     The third coordinate field out (radius)
  !-----------------------------------------------------------------------------
  subroutine chi2abr_interface(chi_1, chi_2, chi_3, panel_id, alpha, beta, r)

    import :: r_def, i_def

    implicit none

    integer(kind=i_def), intent(in)  :: panel_id
    real(kind=r_def),    intent(in)  :: chi_1, chi_2, chi_3
    real(kind=r_def),    intent(out) :: alpha, beta, r

  end subroutine chi2abr_interface

end interface


interface

  !-----------------------------------------------------------------------------
  !> @brief Transforms a coordinate field chi from any system into global
  !>        Cartesian (X,Y,Z) coordinates. If chi is in a spherical coordinate
  !>        system, the third coordinate should be radius (distinguishing this
  !>        function from chi2xyz above). Therefore this will not add the
  !>        scaled_radius to transform.
  !!
  !! @param[in]   chi_1      The first coordinate field in
  !! @param[in]   chi_2      The second coordinate field in
  !! @param[in]   chi_3      The third coordinate field in ()
  !! @param[in]   panel_id   The mesh panel ID
  !! @param[out]  x          The first coordinate field out (global Cartesian X)
  !! @param[out]  y          The second coordinate field out (global Cartesian Y)
  !! @param[out]  z          The third coordinate field out (global Cartesian Z)
  !-----------------------------------------------------------------------------
  subroutine chir2xyz_interface(chi_1, chi_2, chi_3, panel_id, x, y, z)

    import :: r_def, i_def

    implicit none

    integer(kind=i_def), intent(in)  :: panel_id
    real(kind=r_def),    intent(in)  :: chi_1, chi_2, chi_3
    real(kind=r_def),    intent(out) :: x, y, z

  end subroutine chir2xyz_interface

end interface

!------------------------------------------------------------------------------
! Declare procedure pointers
!------------------------------------------------------------------------------

procedure(chi2xyz_interface),  protected, pointer :: chi2xyz => null()
procedure(chi2llr_interface),  protected, pointer :: chi2llr => null()
procedure(chi2abr_interface),  protected, pointer :: chi2abr => null()
procedure(chir2xyz_interface), protected, pointer :: chir2xyz => null()


!------------------------------------------------------------------------------
! Contained functions / subroutines
!------------------------------------------------------------------------------
contains

!------------------------------------------------------------------------------
!>  @brief  Initialise the coordinate transform procedure pointers
!------------------------------------------------------------------------------
subroutine init_chi_transforms()

  use finite_element_config_mod, only : coord_system,            &
                                        coord_system_xyz,        &
                                        coord_system_alphabetaz, &
                                        coord_system_lonlatz
  use log_mod,                   only : log_event, LOG_LEVEL_ERROR

  implicit none

  ! Set procedure pointers based on the chosen coordinate system
  select case ( coord_system )
  case ( coord_system_xyz )
    chi2xyz  => chi2chi
    chi2llr  => xyz2llr_panel
    chi2abr  => xyz2alphabetar
    chir2xyz => chi2chi
  case ( coord_system_lonlatz )
    chi2xyz  => lonlatz2xyz_panel
    chi2llr  => chih2chir
    chi2abr  => lonlatz2abr
    chir2xyz => llr2xyz_panel
  case ( coord_system_alphabetaz )
    chi2xyz  => alphabetaz2xyz
    chi2llr  => alphabetaz2llr
    chi2abr  => chih2chir
    chir2xyz => alphabetar2xyz
  case default
    call log_event('Coordinate system not recognised', LOG_LEVEL_ERROR)
  end select

end subroutine init_chi_transforms

!------------------------------------------------------------------------------
!>  @brief  Nullify the coordinate transform procedure pointers
!------------------------------------------------------------------------------
subroutine final_chi_transforms()

  implicit none

  nullify(chi2xyz, chi2abr, chi2llr, chir2xyz)

end subroutine final_chi_transforms


!-----------------------------------------------------------------------------
! Private subroutines for doing coordinate transformations
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
!> @brief Does the identity transformation on a coordinate vector, which is
!>        used with the coordinate transformation procedure pointers
!!
!! @param[in]   chi_1_in   The first coordinate field in
!! @param[in]   chi_2_in   The second coordinate field in
!! @param[in]   chi_3_in   The third coordinate field in
!! @param[in]   panel_id   The mesh panel ID
!! @param[out]  chi_1_out  The first coordinate field out
!! @param[out]  chi_2_out  The second coordinate field out
!! @param[out]  chi_3_out  The third coordinate field out
!-----------------------------------------------------------------------------
subroutine chi2chi(chi_1_in, chi_2_in, chi_3_in, panel_id, &
                   chi_1_out, chi_2_out, chi_3_out)

  implicit none

  integer(kind=i_def), intent(in)  :: panel_id
  real(kind=r_def),    intent(in)  :: chi_1_in, chi_2_in, chi_3_in
  real(kind=r_def),    intent(out) :: chi_1_out, chi_2_out, chi_3_out

  chi_1_out = chi_1_in
  chi_2_out = chi_2_in
  chi_3_out = chi_3_in

end subroutine chi2chi

!-----------------------------------------------------------------------------
!> @brief Does the identity transformation on a coordinate vector, but also
!>        transforms the height coordinate into radius. This is used when
!>        moving from a system whose final coordinate is height to one whose
!>        final coordinate is radius.
!!
!! @param[in]   chi_1_in   The first coordinate field in
!! @param[in]   chi_2_in   The second coordinate field in
!! @param[in]   height     The third coordinate field in (height)
!! @param[in]   panel_id   The mesh panel ID
!! @param[out]  chi_1_out  The first coordinate field out
!! @param[out]  chi_2_out  The second coordinate field out
!! @param[out]  radius     The third coordinate field out (radius)
!-----------------------------------------------------------------------------
subroutine chih2chir(chi_1_in, chi_2_in, height, panel_id, &
                     chi_1_out, chi_2_out, radius)

  implicit none

  integer(kind=i_def), intent(in)  :: panel_id
  real(kind=r_def),    intent(in)  :: chi_1_in, chi_2_in, height
  real(kind=r_def),    intent(out) :: chi_1_out, chi_2_out, radius

  chi_1_out = chi_1_in
  chi_2_out = chi_2_in
  radius = height + scaled_radius

end subroutine chih2chir

!-----------------------------------------------------------------------------
!> @brief Transforms from global Cartesian (X,Y,Z) coordinates to
!>        (longitude, latitude, radius) coordinates, with panel_id
!>        as an argument for the procedure pointers.
!!
!! @param[in]   x          The first coordinate field in (global Cartesian X)
!! @param[in]   y          The second coordinate field in (global Cartesian Y)
!! @param[in]   z          The third coordinate field in (global Cartesian Z)
!! @param[in]   panel_id   The mesh panel ID
!! @param[out]  longitude  The first coordinate field out (longitude)
!! @param[out]  latitude   The second coordinate field out (latitude)
!! @param[out]  radius     The third coordinate field out (radius)
!-----------------------------------------------------------------------------
subroutine xyz2llr_panel(x, y, z, panel_id, longitude, latitude, radius)

  implicit none

  integer(kind=i_def), intent(in)  :: panel_id
  real(kind=r_def),    intent(in)  :: x, y, z
  real(kind=r_def),    intent(out) :: longitude, latitude, radius

  call xyz2llr(x, y, z, longitude, latitude, radius)

end subroutine xyz2llr_panel

!-----------------------------------------------------------------------------
!> @brief Transforms from (longitude, latitude, height) coordinates to
!>        global Cartesian (X,Y,Z) coordinates, with panel_id
!>        as an argument for the procedure pointers.
!!
!! @param[in]   longitude  The first coordinate field in (longitude)
!! @param[in]   latitude   The second coordinate field in (latitude)
!! @param[in]   height     The third coordinate field in (height)
!! @param[in]   panel_id   The mesh panel ID
!! @param[out]  x          The first coordinate field out (global Cartesian X)
!! @param[out]  y          The second coordinate field out (global Cartesian Y)
!! @param[out]  z          The third coordinate field out (global Cartesian Z)
!-----------------------------------------------------------------------------
subroutine lonlatz2xyz_panel(longitude, latitude, height, panel_id, x, y, z)

  implicit none

  integer(kind=i_def), intent(in)  :: panel_id
  real(kind=r_def),    intent(in)  :: longitude, latitude, height
  real(kind=r_def),    intent(out) :: x, y, z

  call llr2xyz(longitude, latitude, height+scaled_radius, x, y, z)

end subroutine lonlatz2xyz_panel

!-----------------------------------------------------------------------------
!> @brief Transforms from (longitude, latitude, radius) coordinates to
!>        global Cartesian (X,Y,Z) coordinates, with panel_id
!>        as an argument for the procedure pointers.
!!
!! @param[in]   longitude  The first coordinate field in (longitude)
!! @param[in]   latitude   The second coordinate field in (latitude)
!! @param[in]   radius     The third coordinate field in (radius)
!! @param[in]   panel_id   The mesh panel ID
!! @param[out]  x          The first coordinate field out (global Cartesian X)
!! @param[out]  y          The second coordinate field out (global Cartesian Y)
!! @param[out]  z          The third coordinate field out (global Cartesian Z)
!-----------------------------------------------------------------------------
subroutine llr2xyz_panel(longitude, latitude, radius, panel_id, x, y, z)

  implicit none

  integer(kind=i_def), intent(in)  :: panel_id
  real(kind=r_def),    intent(in)  :: longitude, latitude, radius
  real(kind=r_def),    intent(out) :: x, y, z

  call llr2xyz(longitude, latitude, radius, x, y, z)

end subroutine llr2xyz_panel

!-----------------------------------------------------------------------------
!> @brief Transforms from (longitude, latitude, height) coordinates to
!>        equiangular cubed sphere (alpha, beta, r) coordinates.
!!
!! @param[in]   longitude  The first coordinate field in (longitude)
!! @param[in]   latitude   The second coordinate field in (latitude)
!! @param[in]   height     The third coordinate field in (height)
!! @param[in]   panel_id   The mesh panel ID
!! @param[out]  alpha      The first coordinate field out (alpha)
!! @param[out]  beta       The second coordinate field out (beta)
!! @param[out]  radius     The third coordinate field out (radius)
!-----------------------------------------------------------------------------
subroutine lonlatz2abr(longitude, latitude, height, panel_id, alpha, beta, radius)

  implicit none

  integer(kind=i_def), intent(in)  :: panel_id
  real(kind=r_def),    intent(in)  :: longitude, latitude, height
  real(kind=r_def),    intent(out) :: alpha, beta, radius

  ! Internal arguments
  real(kind=r_def) :: x, y, z

  ! Transform via X, Y, Z coordinates
  call llr2xyz(longitude, latitude, height+scaled_radius, x, y, z)
  call xyz2alphabetar(x, y, z, panel_id, alpha, beta, radius)

  ! Avoid loss of accuracy of going via XYZ by storing radius at the end
  radius = height + scaled_radius

end subroutine lonlatz2abr

!-----------------------------------------------------------------------------
!> @brief Transforms from equiangular cubed sphere coordinates
!>        (alpha, beta, height) to global Cartesian (X,Y,Z) coordinates.
!!
!! @param[in]   alpha      The first coordinate field in (alpha)
!! @param[in]   beta       The second coordinate field in (beta)
!! @param[in]   height     The third coordinate field in (height)
!! @param[in]   panel_id   The mesh panel ID
!! @param[out]  x          The first coordinate field out (global Cartesian X)
!! @param[out]  y          The second coordinate field out (global Cartesian Y)
!! @param[out]  z          The third coordinate field out (global Cartesian Z)
!-----------------------------------------------------------------------------
subroutine alphabetaz2xyz(alpha, beta, height, panel_id, x, y, z)

  implicit none

  integer(kind=i_def), intent(in)  :: panel_id
  real(kind=r_def),    intent(in)  :: alpha, beta, height
  real(kind=r_def),    intent(out) :: x, y, z

  call alphabetar2xyz(alpha, beta, height+scaled_radius, panel_id, x, y, z)

end subroutine alphabetaz2xyz

!-----------------------------------------------------------------------------
!> @brief Transforms from equiangular cubed sphere coordinates
!>        (alpha, beta, height) to (longitude, latitude, radius) coordinates.
!!
!! @param[in]   alpha      The first coordinate field in (alpha)
!! @param[in]   beta       The second coordinate field in (beta)
!! @param[in]   height     The third coordinate field in (height)
!! @param[in]   panel_id   The mesh panel ID
!! @param[out]  longitude  The first coordinate field out (longitude)
!! @param[out]  latitude   The second coordinate field out (latitude)
!! @param[out]  radius     The third coordinate field out (radius)
!-----------------------------------------------------------------------------
subroutine alphabetaz2llr(alpha, beta, height, panel_id, longitude, latitude, radius)

  implicit none

  integer(kind=i_def), intent(in)  :: panel_id
  real(kind=r_def),    intent(in)  :: alpha, beta, height
  real(kind=r_def),    intent(out) :: longitude, latitude, radius

  radius = height + scaled_radius
  call alphabetar2llr(alpha, beta, radius, panel_id, longitude, latitude)

end subroutine alphabetaz2llr

end module chi_transform_mod

