!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!> @brief Define various constants for the application.
!>
!> @details Various computational, physical and geometrical constants are
!>          defined in this module. Their values are also set here.
module constants_mod

  use, intrinsic :: iso_fortran_env, only : real32, real64, int32

  implicit none

  ! Define default application-defined kinds for all intrinsic data types

  !> @name Set up default kinds for real and double-precision variables.
  !> @{
  real,             private :: r_val  !< A native real used to compute kind of native real.
  double precision, private :: dp_val !< A native double-precision used to compute kind of native dp.

  integer, parameter :: r_def     = real64 !< Default real kind for application.
  integer, parameter :: r_single  = real32 !< Default single precision real kind for application.
  integer, parameter :: r_double  = real64 !< Default double precision real kind for application.

  integer, parameter :: r_native  = kind(r_val)  !< Native kind for real.
  integer, parameter :: dp_native = kind(dp_val) !< Native kind for double precision.
  !> @}

  !> @name Complex
  !> @{
  !> @}

  !> @name Set up default kinds for integers.
  !> @{
  integer, private   :: i_val     !< A native integer used to compute kind of native integer.

  integer, parameter :: i_def     = int32       !< Default integer kind for application.
  integer, parameter :: i_native  = kind(i_val) !< Native kind for integer.
  !> @}

  !> @name Set up default kinds for logicals.
  !> @{
  logical, private   :: l_val     !< A native logical used to compute kind of native logical.

  integer, parameter :: l_def     = kind(l_val) !< Default logical kind for application.
  integer, parameter :: l_native  = kind(l_val) !< Native kind for logical.
  !> @}

  !> @name Set up default kinds for character variables.
  !> @{
  character, private :: c_val     !< A native character used to compute kind of native character.

  integer, parameter :: c_def     = kind(c_val) !< Default character kind for application.
  integer, parameter :: c_native  = kind(c_val) !< Native kind for character.
  !> @}

  !> @name Set up default lengths for string variables.
  !> @{
  integer, parameter :: str_short        = 16  !< Length of "short" strings.
  integer, parameter :: str_def          = 128 !< Default string length for normal strings.
  integer, parameter :: str_long         = 255 !< Default length of long string.
  integer, parameter :: str_max_filename = 255 !< Default maximum length of a file-name.
  !> @}

  !> @name Platform constants
  !> @{
  real(kind=r_def), parameter :: LARGE_REAL = huge(0.0_r_def) !< The largest number of kind r_def that is not an infinity.
  integer, parameter :: cache_block = 256 !< Size of a cache block, used for padding arrays to ensure access to different cache lines

  !> @}

  !> @name Numerical constants
  !> @{
  real(kind=r_def), parameter :: EPS = 3.0e-15_r_def !< Relative precision: if (abs(x-y) > EPS) then assume x==y.
  !> @}

  !> @name Mathematical constants
  !> @{
  real(kind=r_def), parameter :: PI  = 3.141592654_r_def !< Value of pi.
  !> @}

  !> @{
  ! Missing data indicators
  real    (r_def), parameter :: RMDI = -huge(0.0_r_def)        !< Missing data indicator value for real numbers
  integer (i_def), parameter :: IMDI = -huge(0_i_def)          !< Missing data indicator value for integer numbers
  !> @}

end module constants_mod

