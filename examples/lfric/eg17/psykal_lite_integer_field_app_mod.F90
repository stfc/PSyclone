!-----------------------------------------------------------------------------
! (C) Crown copyright 2020 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
! LICENCE is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE
! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2021, Science and Technology Facilities Council
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
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
! FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
! COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
! BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
! ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.
!------------------------------------------------------------------------------
! Modified by I. Kavcic, Met Office
!------------------------------------------------------------------------------

!> @brief Provides an implementation of the PSy layer for integer fields

! This PSyKAl-lite code will become redundant when appropriate built-in
! support for integer fields is introduced in issue #853
module psykal_lite_integer_field_app_mod

    USE constants_mod,     ONLY: i_def
    USE field_mod,         ONLY: field_type, field_proxy_type
    USE integer_field_mod, ONLY: integer_field_type, integer_field_proxy_type

    IMPLICIT NONE

    CONTAINS

    SUBROUTINE invoke_integer_setval_c(field_1, s)
      INTEGER(KIND=i_def), intent(in) :: s
      TYPE(integer_field_type), intent(inout) :: field_1

      TYPE(integer_field_proxy_type) field_1_proxy
      INTEGER df
      !
      ! Initialise field and/or operator proxies
      !
      field_1_proxy = field_1%get_proxy()
      !
      ! Call kernels and communication routines
      !
      DO df=1,field_1_proxy%vspace%get_last_dof_owned()
        field_1_proxy%data(df) = s
      END DO 
      !
      ! Set halos dirty/clean for fields modified in the above loop
      !
      CALL field_1_proxy%set_dirty()

    END SUBROUTINE invoke_integer_setval_c

end module psykal_lite_integer_field_app_mod
