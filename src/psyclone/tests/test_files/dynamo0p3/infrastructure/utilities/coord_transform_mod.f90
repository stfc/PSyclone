

!-----------------------------------------------------------------------------
! Copyright (c) 2017-2024,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
!-----------------------------------------------------------------------------
! However, it has been created by John Thuburn.
!------------------------------------------------------------------------------
!>  @brief   Routines for coordinate transformations.
!!
!!  @details Contains routines for conversion of e.g. lat-long to Cartesian
!!           XYZ.
!------------------------------------------------------------------------------
module coord_transform_mod

use constants_mod, only : r_def, PI, eps

implicit none

private

! Public subroutines
public :: ll2xyz
public :: llr2xyz
public :: xyz2ll
public :: xyz2llr
public :: starea2
public :: spherical_distance
public :: cartesian_distance
public :: sphere2cart_vector
public :: cart2sphere_vector
public :: central_angle
public :: rodrigues_rotation

!------------------------------------------------------------------------------
! Contained functions / subroutines
!------------------------------------------------------------------------------
contains

!------------------------------------------------------------------------------
!>  @brief  Converts longitude and latitude to Cartesian coordinates on the
!!          unit sphere.
!!
!!  @param[in]   longitude  Longitude to convert.
!!  @param[in]   latitude   Latitude  to convert.
!!  @param[out]  x     Cartesian x coordinate.
!!  @param[out]  y     Cartesian y coordinate.
!!  @param[out]  z     Cartesian z coordinate.
!------------------------------------------------------------------------------
subroutine ll2xyz(longitude,latitude,x,y,z)

  implicit none

  ! Arguments
  real(kind=r_def), intent(in)  :: longitude, latitude
  real(kind=r_def), intent(out) :: x,y,z

  ! Internal variables
  real(kind=r_def) :: cos_longitude, sin_longitude, cos_latitude, sin_latitude

  sin_longitude = sin(longitude)
  cos_longitude = cos(longitude)
  sin_latitude = sin(latitude)
  cos_latitude = cos(latitude)

  x = cos_longitude * cos_latitude
  y = sin_longitude * cos_latitude
  z = sin_latitude

  return
end subroutine ll2xyz

!------------------------------------------------------------------------------
!>  @brief  Converts longitude and latitude to Cartesian coordinates on
!!          a sphere with some specified radius.
!!
!!  @param[in]   longitude Longitude to convert.
!!  @param[in]   latitude  Latitude  to convert.
!!  @param[in]   radius    Radius of the sphere.
!!  @param[out]  x         Cartesian x coordinate.
!!  @param[out]  y         Cartesian y coordinate.
!!  @param[out]  z         Cartesian z coordinate.
!------------------------------------------------------------------------------
subroutine llr2xyz(longitude, latitude, radius, x, y, z)

  implicit none

  ! Arguments
  real(kind=r_def), intent(in)  :: longitude, latitude, radius
  real(kind=r_def), intent(out) :: x, y, z

  ! Internal variables
  real(kind=r_def) :: cos_long, sin_long
  real(kind=r_def) :: cos_latitude, sin_latitude

  sin_long     = sin(longitude)
  cos_long     = cos(longitude)
  sin_latitude = sin(latitude)
  cos_latitude = cos(latitude)

  x = radius * cos_long * cos_latitude
  y = radius * sin_long * cos_latitude
  z = radius * sin_latitude

  return
end subroutine llr2xyz

!------------------------------------------------------------------------------
!>  @brief  Converts Cartesian coordinates to longitude and latitude.
!!
!!  @param[in]   x     Cartesian x coordinate to convert.
!!  @param[in]   y     Cartesian y coordinate to convert.
!!  @param[in]   z     Cartesian z coordinate to convert.
!!  @param[out]  longitude  -PI   <  Longitude <= PI   (radians).
!!  @param[out]  latitude   -PI/2 <= Latitude  <= PI/2 (radians).
!------------------------------------------------------------------------------
subroutine xyz2ll(x, y, z, longitude, latitude)

  implicit none

  ! Arguments
  real(r_def), intent(in)  :: x, y, z
  real(r_def), intent(out) :: longitude, latitude

  ! Internal variables
  real(r_def) :: tan_longitude
  real(r_def) :: tan_latitude
  real(r_def) :: radius

  ! Calculate longitude in range
  ! -180 < longitude <= 180
  if (x == 0.0_r_def) then
    if (y >= 0.0_r_def) then
      longitude =  0.5_r_def*PI
    else
      longitude = -0.5_r_def*PI
    end if
  else
    tan_longitude  = y/x
    longitude = atan(tan_longitude)

    if (x < 0.0_r_def) then
      if (y >= 0.0_r_def) then
        longitude = longitude + PI
      else
        longitude = longitude - PI
      end if
    end if
  end if


  ! Calculate latitude in range
  ! -90 <= longitude <= +90
  radius = sqrt(x*x+y*y)
  if (radius <= eps) then
    if (z > 0.0_r_def) then
      latitude = 0.5_r_def*PI
    else
      latitude = -0.5_r_def*PI
    end if
    ! Ensure consisent value for longitude is
    ! output for Latitudes of -90 and 90.
    longitude = 0.0_r_def
  else
    tan_latitude = z/radius
    latitude = atan(tan_latitude)
  end if

  return
end subroutine xyz2ll

!------------------------------------------------------------------------------
!>  @brief  Converts Cartesian coordinates to longitude, latitude and radius
!!  @param[in]   x     Cartesian x coordinate to convert.
!!  @param[in]   y     Cartesian y coordinate to convert.
!!  @param[in]   z     Cartesian z coordinate to convert.
!!  @param[out]  longitude  -PI   <  Longitude <= PI   (radians).
!!  @param[out]  latitude   -PI/2 <= Latitude  <= PI/2 (radians).
!!  @param[out]  radius     Radius of the sphere(m).
!------------------------------------------------------------------------------
subroutine xyz2llr( x, y, z, &
                    longitude, latitude, radius )

  implicit none

  real(r_def), intent(in)  :: x, y, z
  real(r_def), intent(out) :: longitude, latitude, radius

  ! Local variables
  real(r_def) :: tan_longitude, tan_latitude
  real(r_def) :: tol = 10.0e-8_r_def


  ! Calculate longitude in range
  ! -180 < longitude <= 180
  if (x == 0.0_r_def) then

    if (y >= 0.0_r_def) then
      longitude =  0.5_r_def*PI
    else
      longitude = -0.5_r_def*PI
    end if

  else

    tan_longitude = y/x
    longitude = atan(tan_longitude)

    if (x < 0.0_r_def) then
      if (y >= 0.0_r_def) then
        longitude = longitude + PI
      else
        longitude = longitude - PI
      end if
    end if

  end if


  ! Calculate latitude in range
  ! -90 <= longitude <= +90
  radius = sqrt(x*x + y*y)
  if ( abs(radius) < tol ) then
    if (z > 0.0_r_def ) then
      latitude =  0.5_r_def*PI
    else
      latitude = -0.5_r_def*PI
    end if
    ! Ensure consisent value for longitude is
    ! output for Latitudes of -90/90.
    longitude = 0.0_r_def
  else
    tan_latitude = z/radius
    latitude = atan(tan_latitude)
  end if

  ! Radius
  radius = sqrt(x*x + y*y + z*z)

end subroutine xyz2llr


!-------------------------------------------------------------------------------
!>  @brief  Calculates the area of a spherical triangle.
!!
!!  @details Calculates the area of the spherical triangle whose corners have
!!           Cartesian coordinates (X0,Y0,Z0), (X1,Y1,Z1), (X2,Y2,Z2).
!!           The formula below is more robust to roundoff error than the better
!!           known sum of angle - PI formula
!!
!!  @param[in]   x0    Coordinate of a triangle corner (x0,y0,z0).
!!  @param[in]   x1    Coordinate of a triangle corner (x1,y1,z1).
!!  @param[in]   x2    Coordinate of a triangle corner (x2,y2,z2).
!!  @return  area  Area of the spherical triangle.
!-------------------------------------------------------------------------------
function starea2(x0,x1,x2) result(area)

  implicit none

  ! Arguments
  real(kind=r_def), dimension(3), intent(in)  :: x0, x1, x2
  real(kind=r_def)                            :: area

  ! Internal variables
  real(kind=r_def) :: d0,d1,d2,s,t0,t1,t2,t3

  ! Distances between pairs of points
  d2 = spherical_distance(x0,x1)
  d0 = spherical_distance(x1,x2)
  d1 = spherical_distance(x2,x0)

  ! Half perimeter
  s=0.5_r_def*(d0+d1+d2)

  ! Tangents
  t0 = tan(0.5_r_def*(s-d0))
  t1 = tan(0.5_r_def*(s-d1))
  t2 = tan(0.5_r_def*(s-d2))
  t3 = tan(0.5_r_def*s)

  ! Area
  area = 4.0_r_def*atan(sqrt(t0*t1*t2*t3))

  return
end function starea2

!-------------------------------------------------------------------------------
!> @brief  Calculates the spherical distance between two points.
!!
!! @details  Calculates the spherical distance s between two points with
!!           Cartesian coordinates (x1,y1,z1), (x2,y2,z2) on the unit sphere.
!!
!! @param[in]  x1  First Cartesian coordinate (x1,y1,z1).
!! @param[in]  x2  Second Cartesian coordinate (x2,y2,z2).
!! @return     s   Spherical distance between the points.
!-------------------------------------------------------------------------------
function spherical_distance(x1,x2) result(s)
  ! Calculate the spherical distance S between two points with Cartesian
  ! coordinates (X1,Y1,Z1), (X2,Y2,Z2)

  implicit none

  real(kind=r_def), dimension(3), intent(in) :: x1, x2
  real(kind=r_def)                           :: s, ad
  real(kind=r_def), dimension(3)             :: dx

  dx = x2 - x1
  ad = sqrt(sum(dx*dx))
  s = 2.0_r_def*asin(0.5_r_def*ad)

end function spherical_distance

!-------------------------------------------------------------------------------
!> @brief  Calculates the central angle between two points
!!
!! @details  Calculates the central angle between two points with
!!           latitude-longitude coordinates (lat1,long1), (lat2,long2)
!!
!! @param[in]  lat1    Latitude of first point
!! @param[in]  long1   Longitude of first point
!! @param[in]  lat2    Latitude of second point
!! @param[in]  long2   Longitude of second point
!! @param[out] angle   Angle between the points.
!-------------------------------------------------------------------------------
subroutine central_angle(long1,lat1,long2,lat2,angle)

  ! Calculates the central angle between points 1 and 2 with latitude and longitude

  implicit none

  ! Arguments
  real(kind=r_def), intent(in)  :: long1,lat1,long2,lat2
  real(kind=r_def), intent(out) :: angle

  ! Internal variables
  real(kind=r_def) :: deltalong

  deltalong = long2-long1
  angle = acos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(deltalong))

  return
end subroutine central_angle

!-------------------------------------------------------------------------------
!> @brief  Calculates the Cartesian distance between two points.
!!
!! @details  Calculates the Cartesian distance s between two points with
!!           Cartesian coordinates (x1,y1,z1), (x2,y2,z2)
!!
!! @param[in]  x First point in Cartesian coordinates.
!! @param[in]  y Second point in Cartesian coordinates.
!! @result     s Cartesian distance between the points.
!-------------------------------------------------------------------------------
pure function cartesian_distance(x,y) result( s )

  implicit none

  !Arguments
  real(kind=r_def), intent(in)  :: x(3), y(3)
  real(kind=r_def)              :: s

  !Internal variables
  real(kind=r_def) :: dx, dy, dz

  dx = y(1) - x(1)
  dy = y(2) - x(2)
  dz = y(3) - x(3)

  s = sqrt(dx*dx + dy*dy + dz*dz)
  return
end function cartesian_distance


!-------------------------------------------------------------------------------
!> @brief Converts a vector in sperical coordinates to one in
!> Cartesian coodinates
!> @param[in] llr Location in spherical coodinates
!> @param[in] dlambda Input vector in spherical coordinates
!> @param[return] dx Output vector in Cartesian coordinates
!-------------------------------------------------------------------------------
function sphere2cart_vector( dlambda, llr ) result ( dx )

  use constants_mod,     only: r_def
  use matrix_invert_mod, only: matrix_invert_3x3

  implicit none

  real(kind=r_def), intent(in)  :: dlambda(3)
  real(kind=r_def), intent(in)  :: llr(3)
  real(kind=r_def)              :: dx(3)

  real(kind=r_def)              :: A(3,3), A_inv(3,3)

  ! Form transformation matrix
  A(1,:) = (/ -sin(llr(1)),              cos(llr(1)),              0.0_r_def   /)
  A(2,:) = (/ -sin(llr(2))*cos(llr(1)), -sin(llr(2))*sin(llr(1)),  cos(llr(2)) /)
  A(3,:) = (/  cos(llr(2))*cos(llr(1)),  cos(llr(2))*sin(llr(1)),  sin(llr(2)) /)

  ! Form inverse
  A_inv = matrix_invert_3x3(A)
  dx(:) = matmul(A_inv, dlambda)
  return

end function sphere2cart_vector

!-------------------------------------------------------------------------------
!> @brief Converts a flux vector in Cartesian coordinates (x,y,z)
!> to one in spherical coodinates (lambda,phi,r)
!>
!> Converts 3d Cartesian velocity u = (u1,u2,u3) in Cartesian coordinates at
!> (x,y,z) to spherical velocity v = (v1,v2,v3) (in m/s) in spherical
!> coordinates at (lambda,phi,r)
!>
!> @param[in] x_vec vector (x,y,z) location in Cartesian coodinates
!> @param[in] Cartesian_vec Components of a flux vector (u,v,w)
!>                          in Cartesian coordinates
!> @result    spherical_vec Components of a flux vector (u,v,w)
!>                          on spherical coordinates
!>
!-------------------------------------------------------------------------------
pure function cart2sphere_vector(x_vec, cartesian_vec) result ( spherical_vec )

  implicit none

  real(kind=r_def), intent(in)  :: x_vec(3)
  real(kind=r_def), intent(in)  :: cartesian_vec(3)
  real(kind=r_def)              :: spherical_vec(3)

  real(kind=r_def)              :: r, t, phi

  t = x_vec(1)**2 + x_vec(2)**2
  r = sqrt(t + x_vec(3)**2)

  spherical_vec(1) = (- x_vec(2)*cartesian_vec(1) &
                      + x_vec(1)*cartesian_vec(2) ) / t
  spherical_vec(2) = (- x_vec(1)*x_vec(3)*cartesian_vec(1) &
                      - x_vec(2)*x_vec(3)*cartesian_vec(2) &
                      + t*cartesian_vec(3))/(r*r*sqrt(t))
  spherical_vec(3) = (  x_vec(1)*cartesian_vec(1) &
                      + x_vec(2)*cartesian_vec(2) &
                      + x_vec(3)*cartesian_vec(3)) / r

  ! Convert from (dlambda/dt,dphi/dt,dr/dt) to (u,v,w) in m/s
  phi = 0.5_r_def*PI - acos(x_vec(3)/r)
  spherical_vec(1) = spherical_vec(1)*r*cos(phi)
  spherical_vec(2) = spherical_vec(2)*r

end function cart2sphere_vector

!-------------------------------------------------------------------------------
!> @brief Rotate a vector x_vec through an angle alpha around an
!>        axis rot_vec using Rodrigues rotation formula
!> @details Rotates a 3d Cartesian vector x_vec = (x,y,z) about an axis
!>          rot_vec = (r1,r2,r3) by an angle alpha
!> @param[in] x_vec vector (x,y,z) location in Cartesian coodinates
!> @param[in] rot_vec vector (r1,r2,r3) direction of axis of rotation
!> @param[in] alpha real angle of rotation
!> @result    y_vec 3d Cartesian rotated vector
!-------------------------------------------------------------------------------
pure function rodrigues_rotation(x_vec, rot_vec, alpha) result(y_vec)
  use constants_mod,     only: r_def
  use cross_product_mod, only: cross_product
  implicit none

  real(kind=r_def), intent(in)  :: x_vec(3)
  real(kind=r_def), intent(in)  :: rot_vec(3)
  real(kind=r_def), intent(in)  :: alpha

  real(kind=r_def) :: y_vec(3)

  real(kind=r_def) :: unit_rot_vec(3)

  ! Create a normalised vector in the direction of the rotation vector
  unit_rot_vec=rot_vec/sqrt(rot_vec(1)**2+rot_vec(2)**2+rot_vec(3)**2)

  ! Create a rotated vector using the normalised rotation vector
  y_vec = x_vec * cos(alpha) +                                                     &
          cross_product( unit_rot_vec, x_vec ) * sin( alpha )                      &
          + unit_rot_vec * dot_product( x_vec, unit_rot_vec ) * ( 1.0 -cos(alpha) )

end function rodrigues_rotation

end module coord_transform_mod

