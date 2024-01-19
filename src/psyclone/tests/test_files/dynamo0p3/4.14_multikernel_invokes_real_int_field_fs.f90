! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
! -----------------------------------------------------------------------------
! Author: I. Kavcic, Met Office
! Modified: A. R. Porter, STFC Daresbury Lab

program multikernel_invokes_real_int_field_fs

  ! Description: two kernel calls using all supported function spaces
  ! with the first kernel operating on integer-valued fields and the
  ! second kernel operating on real-valued fields
  use field_mod,                 only: field_type
  use integer_field_mod,         only: integer_field_type
  use testkern_fs_int_field_mod, only: testkern_fs_int_field_type
  use testkern_fs_mod,           only: testkern_fs_type

  implicit none

  type(integer_field_type) :: i1, i2, i3, i4, i5, i6, i7, i8, &
                              n1, n2, n3, n4, n5, n6, n7
  type(field_type) :: f1, f2, f3, f4, f5, f6, &
                      m1, m2, m3, m4, m5, m6, m7

  call invoke(name = "Integer_and_real_field",            &
       testkern_fs_int_field_type(i1, i2, n1, n2, i3, i4, &
                                  n3, n4, i5, i6, n5, n6, &
                                  i7, i8, n7),            &
       testkern_fs_type(f1, f2, m1, m2, f3, f4, m3, m4,   &
                        f5, f6, m5, m6, m7)               &
             )

end program multikernel_invokes_real_int_field_fs
