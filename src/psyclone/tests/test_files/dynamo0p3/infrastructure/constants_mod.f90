!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------

! BSD 3-Clause License
!
! Modifications copyright (c) 2017, Science and Technology Facilities Council
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
!
!> @brief Define various constants for the application.
!>
!> @details Various computational, physical and geometrical constants are
!>          defined in this module. Their values are also set here.
module constants_mod

  use, intrinsic :: iso_fortran_env, only : real32, real64, int32

  implicit none

  real,             private :: r_val
  double precision, private :: dp_val

  integer, parameter :: r_def     = real64
  integer, parameter :: r_single  = real32
  integer, parameter :: r_double  = real64

  integer, parameter :: r_native  = kind(r_val)
  integer, parameter :: dp_native = kind(dp_val)

  integer, private   :: i_val
  integer, parameter :: i_def     = int32
  integer, parameter :: i_native  = kind(i_val)

  logical, private   :: l_val

  integer, parameter :: l_def     = kind(l_val)
  integer, parameter :: l_native  = kind(l_val)

  character, private :: c_val

  integer, parameter :: c_def     = kind(c_val)
  integer, parameter :: c_native  = kind(c_val)

  integer, parameter :: str_short        = 16
  integer, parameter :: str_def          = 128
  integer, parameter :: str_long         = 255
  integer, parameter :: str_max_filename = 255

  real(kind=r_def), parameter :: LARGE_REAL = huge(0.0_r_def)
  integer, parameter :: cache_block = 256

  real(kind=r_def), parameter :: EPS = 3.0e-15_r_def

  real(kind=r_def), parameter :: PI  = 3.141592654_r_def

  real    (r_def), parameter :: RMDI = -huge(0.0_r_def)
  integer (i_def), parameter :: IMDI = -huge(0_i_def)

end module constants_mod
