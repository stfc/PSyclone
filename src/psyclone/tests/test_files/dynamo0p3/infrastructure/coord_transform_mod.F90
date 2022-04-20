!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-------------------------------------------------------------------------------
! Contributed by John Thuburn.
!------------------------------------------------------------------------------
!>  @brief   Routines for coordinate transformations.
!!
!!  @details Contains routines for conversion of e.g. lat-long to Cartesian
!!           XYZ.
!------------------------------------------------------------------------------
module coord_transform_mod

use constants_mod,        only : r_def, i_def, PI, EPS

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
public :: identify_panel
public :: xyz2alphabetarpanel
public :: xyz2alphabetar
public :: alphabetar2llr
public :: alphabetar2xyz
public :: alphabetar2xyz_vector
public :: xyz2alphabetar_vector
public :: PANEL_ROT_MATRIX
public :: INVERSE_PANEL_ROT_MATRIX
public :: rebase_longitude_range

! A list of matrices, one for each panel i of cubed sphere, which rotates
! a point (X,Y,Z) from panel 1 to the respective point for i-th panel
integer(kind=i_def), parameter :: PANEL_ROT_MATRIX(3,3,6) = &
   reshape([ [[1,  0,  0], [0,  1,  0], [0,  0,  1]], & ! panel 1 (identity)
             [[0, -1,  0], [1,  0,  0], [0,  0,  1]], & ! panel 2
             [[-1, 0,  0], [0,  0,  1], [0,  1,  0]], & ! panel 3
             [[0,  0, -1], [-1, 0,  0], [0,  1,  0]], & ! panel 4
             [[0,  0, -1], [0,  1,  0], [1,  0,  0]], & ! panel 5
             [[0, -1,  0], [0,  0,  1], [-1, 0,  0]]  & ! panel 6
             ], shape=[3,3,6], order=[2,1,3])

! A list of matrices, one for each panel i of cubed sphere, which rotates
! a point (X,Y,Z) on the i-th panel back to the respective point on panel 1
integer(kind=i_def), parameter :: INVERSE_PANEL_ROT_MATRIX(3,3,6) = &
  reshape([ [[1,  0,  0], [0,  1,  0], [0,  0,  1]], & ! panel 1 (identity)
            [[0,  1,  0], [-1, 0,  0], [0,  0,  1]], & ! panel 2
            [[-1, 0,  0], [0,  0,  1], [0,  1,  0]], & ! panel 3
            [[0, -1,  0], [0,  0,  1], [-1, 0,  0]], & ! panel 4
            [[0,  0,  1], [0,  1,  0], [-1, 0,  0]], & ! panel 5
            [[0,  0, -1], [-1, 0,  0], [0,  1,  0]]  & ! panel 6
            ], shape=[3,3,6], order=[2,1,3])

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
!!  @param[out]  longitude  -PI   <=  Longitude < PI   (radians).
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
  ! -180 degrees <= longitude < 180 degrees
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
      if (y > 0.0_r_def) then
        longitude = longitude + PI
      else
        longitude = longitude - PI
      end if
    end if
  end if

  ! Calculate latitude in range
  ! -90 degrees  <= longitude <= +90 degrees
  radius = sqrt(x*x+y*y)
  if (radius <= EPS) then
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
!> @brief Converts a vector in (lon,lat,r) coordinates to one in
!> Cartesian coodinates
!> @param[in] llr Location in (lon,lat,r) coodinates
!> @param[in] dlambda Input vector in (lon,lat,r) coordinates
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
!> to one in spherical polar coordinates (lambda,phi,r)
!>
!> Converts 3d Cartesian velocity u = (u1,u2,u3) in Cartesian coordinates at
!> (x,y,z) to spherical velocity v = (v1,v2,v3) (in m/s) in spherical
!> coordinates at (lambda,phi,r)
!>
!> @param[in] x_vec vector (x,y,z) location in Cartesian coodinates
!> @param[in] Cartesian_vec Components of a flux vector (u,v,w)
!>                          in Cartesian coordinates
!> @result    spherical_vec Components of a flux vector (u,v,w)
!>                          in (lon,lat,r) coordinates
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

!-----------------------------------------------------------------------------
!> @brief Finds the cubed sphere panel given a point's Cartesian coordinates
!!
!! @param[in] x         Global geocentric Cartesian x coordinate.
!! @param[in] y         Global geocentric Cartesian y coordinate.
!! @param[in] z         Global geocentric Cartesian z coordinate.
!! @return    panel_id  The integer panel ID
!! @TODO: This function can be removed once we can read the panel_id directly
!! from the mesh, which is necessary for rotated and/or stretched cubed spheres.
!-----------------------------------------------------------------------------
function identify_panel(x,y,z) result(panel_id)

  implicit none
  real(kind=r_def), intent(in) :: x,y,z
  integer(kind=i_def) :: panel_id

  real(kind=r_def) :: lon, lat, radius

  if (z<-abs(x) .and. z<-abs(y))then
    panel_id = 6
  elseif (z>abs(x) .and. z>abs(y))then
    panel_id = 5
  else
    call xyz2llr(x, y, z, lon, lat, radius)
    panel_id = identify_longitude_sector(lon)
  end if

end function identify_panel

!-----------------------------------------------------------------------------
!> @brief Finds the cubed sphere panel (for panels 1 to 4) given a longitude
!!
!! @param[in] lon   Longitude in radians
!! @return    panel The integer panel ID
!! @TODO: This function can be removed once we can read the panel_id directly
!! from the mesh, which is necessary for rotated and/or stretched cubed spheres.
!-----------------------------------------------------------------------------
function identify_longitude_sector(lon) result(panel)
  implicit none
  real(kind=r_def), intent(in) :: lon
  integer(kind=i_def)          :: panel

  if (lon >= -5.0_r_def*PI/4.0_r_def .and. lon<-3.0_r_def*PI/4.0_r_def) then
    panel = 3
  elseif (lon >= -3.0_r_def*PI/4.0_r_def .and. lon<-PI/4.0_r_def) then
    panel = 4
  elseif (lon >= -PI/4.0_r_def .and. lon<PI/4.0_r_def) then
    panel = 1
  elseif (lon>=PI/4.0_r_def .and. lon<3.0_r_def*PI/4.0_r_def) then
    panel = 2
  elseif (lon>=3.0_r_def*PI/4.0_r_def .and. lon<5.0_r_def*PI/4.0_r_def) then
    panel = 3
  elseif (lon>=5.0_r_def*PI/4.0_r_def .and. lon<7.0_r_def*PI/4.0_r_def) then
    panel = 4
  elseif (lon>=7.0_r_def*PI/4.0_r_def) then
    panel = 1
  end if

end function identify_longitude_sector

!-----------------------------------------------------------------------------
!> @brief Convert from global geocentric (x,y,z) coords to (alpha,beta,r) coords
!!
!! @param[in]  global_x Geocentric Cartesian x coordinate
!! @param[in]  global_y Geocentric Cartesian y coordinate
!! @param[in]  global_z Geocentric Cartesian z coordinate
!! @param[in]  panel_id ID of cubed sphere panel
!! @param[out] alpha    Cubed sphere alpha coordinate
!! @param[out] beta     Cubed sphere beta coordinate
!! @param[out] radius   Cubed sphere radial coordinate
!-----------------------------------------------------------------------------
subroutine xyz2alphabetar(global_x, global_y, global_z, &
                          panel_id, alpha, beta, radius)

  implicit none

  integer(kind=i_def), intent(in) :: panel_id
  real(kind=r_def),    intent(in) :: global_x, global_y, global_z
  real(kind=r_def),   intent(out) :: alpha, beta, radius

  ! Internal arguments
  real(kind=r_def),  dimension(3) :: global_xyz, panel_1_xyz

  global_xyz(:) = (/ global_x, global_y, global_z /)

  ! We can get the radius straight from the global X, Y, Z values
  radius = sqrt(global_x**2 + global_y**2 + global_z**2)

  ! To find alpha and beta, rotate the X, Y, Z coordinates back to panel_1
  panel_1_xyz(:) = matmul(INVERSE_PANEL_ROT_MATRIX(:,:,panel_id), global_xyz(:))

  alpha = atan2(panel_1_xyz(2), panel_1_xyz(1))
  beta = atan2(panel_1_xyz(3), panel_1_xyz(1))

end subroutine xyz2alphabetar

!-----------------------------------------------------------------------------
!> @brief Convert from global geocentric (x,y,z) coords to panel id and
!!        (alpha,beta,r) coords
!! @param[in]  global_x Geocentric Cartesian x coordinate
!! @param[in]  global_y Geocentric Cartesian y coordinate
!! @param[in]  global_z Geocentric Cartesian z coordinate
!! @param[out] alpha    Cubed sphere alpha coordinate
!! @param[out] beta     Cubed sphere beta coordinate
!! @param[out] radius   Cubed sphere radial coordinate
!! @param[out] panel_id ID of cubed sphere panel
!-----------------------------------------------------------------------------
subroutine xyz2alphabetarpanel(global_x, global_y, global_z, &
                               alpha, beta, radius, panel_id)

  implicit none

  integer(kind=i_def), intent(out) :: panel_id
  real(kind=r_def),    intent(out) :: alpha, beta, radius
  real(kind=r_def),     intent(in) :: global_x, global_y, global_z

  ! Identify which panel the cell lies in
  panel_id = identify_panel(global_x, global_y, global_z)

  call xyz2alphabetar(global_x, global_y, global_z, &
                      panel_id, alpha, beta, radius)

end subroutine xyz2alphabetarpanel

!-----------------------------------------------------------------------------
!> @brief Convert from (alpha,beta,r) coords to global geocentric (x,y,z) coords
!!
!! @param[in]  alpha    Cubed sphere alpha coordinate
!! @param[in]  beta     Cubed sphere beta coordinate
!! @param[in]  radius   Cubed sphere radial coordinate
!! @param[in]  panel_id ID of cubed sphere panel
!! @param[out] global_x Geocentric Cartesian x coordinate
!! @param[out] global_y Geocentric Cartesian y coordinate
!! @param[out] global_z Geocentric Cartesian z coordinate
!-----------------------------------------------------------------------------
subroutine alphabetar2xyz(alpha, beta, radius, panel_id, &
                          global_x, global_y, global_z)

  implicit none

  integer(kind=i_def), intent(in) :: panel_id
  real(kind=r_def),    intent(in) :: alpha, beta, radius
  real(kind=r_def),   intent(out) :: global_x, global_y, global_z

  ! Internal arguments
  real(kind=r_def)                :: panel_rho
  real(kind=r_def), dimension(3)  :: panel_1_xyz(3), global_xyz(3)

  ! First calculate X, Y, Z for the first panel from alpha, beta and radius
  panel_rho = sqrt(1.0_r_def + (tan(alpha))**2 + (tan(beta))**2)
  panel_1_xyz = radius / panel_rho * (/ 1.0_r_def, tan(alpha), tan(beta) /)

  ! Rotate to the global X, Y, Z from the panel_id
  global_xyz(:) = matmul(PANEL_ROT_MATRIX(:,:,panel_id), panel_1_xyz(:))

  global_x = global_xyz(1)
  global_y = global_xyz(2)
  global_z = global_xyz(3)

end subroutine alphabetar2xyz

!-----------------------------------------------------------------------------
!> @brief Convert from (alpha,beta,r) coords to polar (long,lat,r) coords
!!
!! @param[in]     alpha    Cubed sphere alpha coordinate
!! @param[in]     beta     Cubed sphere beta coordinate
!! @param[in,out] radius   Cubed sphere radial coordinate
!! @param[in]     panel_id ID of cubed sphere panel
!! @param[out]    long     Latitude coordinate
!! @param[out]    lat      Latitude coordinate
!-----------------------------------------------------------------------------
subroutine alphabetar2llr(alpha, beta, radius, panel_id, long, lat)

  implicit none

  integer(kind=i_def), intent(in) :: panel_id
  real(kind=r_def),    intent(in) :: alpha, beta
  real(kind=r_def), intent(inout) :: radius
  real(kind=r_def),   intent(out) :: long, lat

  ! Internal arguments
  real(kind=r_def) :: global_x, global_y, global_z

  call alphabetar2xyz(alpha, beta, radius, panel_id, global_x, global_y, global_z)
  call xyz2llr(global_X, global_Y, global_Z, long, lat, radius)

end subroutine alphabetar2llr

!-----------------------------------------------------------------------------
!> @brief Convert a vector with (alpha,beta,r) components to one with
!>        global Cartesian (x,y,z) components
!!
!! @param[in] sphere_vec Input vector with (alpha,beta,r) components
!! @param[in] abr        The (alpha,beta,r) coordinates of the point
!! @param[in] panel_id   Integer giving the ID of the cubed sphere panel
!! @return    cart_vec   Output vector with global Cartesian components
!-----------------------------------------------------------------------------
function alphabetar2xyz_vector(sphere_vec, abr, panel_id) result (cart_vec)

  implicit none

  real(kind=r_def),    intent(in) :: sphere_vec(3)
  real(kind=r_def),    intent(in) :: abr(3)
  integer(kind=i_def), intent(in) :: panel_id

  real(kind=r_def)                :: cart_vec(3), panel_1_vec(3)
  real(kind=r_def)                :: panel_rho
  real(kind=r_def)                :: transform_matrix(3,3)

  ! It is helpful to introduce panel_rho
  panel_rho = sqrt(1 + tan(abr(1))**2 + tan(abr(2))**2)

  ! Matrix giving weights for alphabetar unit basis vectors
  ! in terms of XYZ Cartesian unit basis vectors
  ! f_X = f_alpha dot(e_alpha, e_X) + f_beta dot(e_beta, e_X) + f_r dot(e_r, e_X)
  transform_matrix(1,:) = 1 / panel_rho * &
  (/ -cos(abr(2))*tan(abr(2)), -cos(abr(1))*tan(abr(2)), 1.0_r_def /)

  ! f_Y = f_alpha dot(e_alpha, e_Y) + f_beta dot(e_beta, e_Y) + f_r dot(e_r, e_Y)
  transform_matrix(2,:) = 1 / panel_rho * &
  (/ 1.0_r_def/cos(abr(2)), -cos(abr(1))*tan(abr(1))*tan(abr(2)), tan(abr(1)) /)

  ! f_Z = f_alpha dot(e_alpha, e_Z) + f_beta dot(e_beta, e_Z) + f_r dot(e_r, e_Z)
  transform_matrix(3,:) = 1 / panel_rho * &
  (/ -cos(abr(2))*tan(abr(1))*tan(abr(2)), 1.0_r_def/cos(abr(1)), tan(abr(1)) /)

  ! Obtain the XYZ vector components if point was on panel 1
  panel_1_vec(:) = matmul(transform_matrix(:,:), sphere_vec(:))

  ! Transform panel_1_vec back to its appropriate panel
  cart_vec(:) = matmul(PANEL_ROT_MATRIX(:,:,panel_id), panel_1_vec(:))

  return

end function alphabetar2xyz_vector

!-----------------------------------------------------------------------------
!> @brief Convert a vector with global Cartesian (x,y,z) components
!>        to one with (alpha,beta,r) components
!!
!! @param[in] cart_vec   Input vector with global Cartesian components
!! @param[in] xyz        The (x,y,z) coordinates of the point
!! @return    sphere_vec Output vector with (alpha,beta,r) components
!-----------------------------------------------------------------------------
function xyz2alphabetar_vector(cart_vec, xyz) result(sphere_vec)

  implicit none

  real(kind=r_def), intent(in) :: cart_vec(3)
  real(kind=r_def), intent(in) :: xyz(3)
  real(kind=r_def)             :: sphere_vec(3)

  ! Internal arguments
  integer(kind=i_def)          :: panel_id
  real(kind=r_def)             :: panel_1_vec(3), panel_1_xyz(3)
  real(kind=r_def)             :: f_dot_ealpha, f_dot_ebeta
  real(kind=r_def)             :: f_alpha, f_beta, f_r
  real(kind=r_def)             :: X, Y, Z, r

  ! Identify panel id
  panel_id = identify_panel(xyz(1), xyz(2), xyz(3))

  ! It's helpful to introduce r
  r = sqrt(xyz(1)**2 + xyz(2)**2 + xyz(3)**2)

  ! Rotate vector to equivalent on panel 1
  panel_1_vec(:) = matmul(INVERSE_PANEL_ROT_MATRIX(:,:,panel_id), cart_vec(:))

  ! Similarly rotate position
  panel_1_xyz(:) = matmul(INVERSE_PANEL_ROT_MATRIX(:,:,panel_id), xyz(:))
  X = panel_1_xyz(1)
  Y = panel_1_xyz(2)
  Z = panel_1_xyz(3)

  ! e_alpha and e_beta are not necessarily orthogonal
  ! so can't just apply transform matrix
  f_dot_ealpha = 1.0_r_def / (r*sqrt(X**2 + Z**2)) * &
    (-Y*X*panel_1_vec(1) + (X**2 + Z**2)*panel_1_vec(2) - Y*Z*panel_1_vec(3))
  f_dot_ebeta = 1.0_r_def / (r*sqrt(X**2 + Y**2)) * &
    (-Z*X*panel_1_vec(1) - Y*Z*panel_1_vec(2) + (X**2 + Y**2)*panel_1_vec(3))

  ! Solving simultaneous equations for f_alpha and f_beta gives the following:
  f_alpha = (X**2 + Z**2) * (X**2 + Y**2) / (X**2 * r**2) &
    * (f_dot_ealpha + Y*Z*f_dot_ebeta / sqrt((X**2 + Z**2) * (X**2 + Y**2)))
  f_beta = (X**2 + Z**2) * (X**2 + Y**2) / (X**2 * r**2) &
    * (f_dot_ebeta + Y*Z*f_dot_ealpha / sqrt((X**2 + Z**2) * (X**2 + Y**2)))

  ! f_r can be calculated as normal
  f_r = 1.0_r_def / r * (X*panel_1_vec(1) + Y*panel_1_vec(2) + Z*panel_1_vec(3))

  sphere_vec(:) = (/ f_alpha, f_beta, f_r /)

  return

end function xyz2alphabetar_vector

!-----------------------------------------------------------------------------
!> @brief   Calculate a longitude equivalent to the input, but in a specified
!!          range.
!> @details The returned longitude (degrees) is recalculated to be in the
!!          range [lon_min, lon_min+360).  E.g. if lon_min is -180, then
!!          an input longitude 270 degrees would return -90 degrees.
!!
!! @param[in] longitude         Input longitude (degrees)
!! @param[in] lon_min           Lower bound on the longitude range (degrees)
!! @return    rebased_longitude Output longitude in new range.
!-----------------------------------------------------------------------------
function rebase_longitude_range(longitude, lon_min) result(rebased_longitude)

  implicit none

  real(kind=r_def),    intent(in)  :: longitude
  real(kind=r_def),    intent(in)  :: lon_min
  real(kind=r_def)                 :: rebased_longitude

  rebased_longitude =  MODULO( longitude - lon_min, 360.0_r_def) + lon_min

  return

end function rebase_longitude_range

end module coord_transform_mod
