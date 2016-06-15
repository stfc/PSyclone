!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author A. R. Porter STFC Daresbury Lab

program single_invoke

  ! Description: single point-wise set operation specified in an invoke call
  ! with the scalar value passed by reference
  use testkern, only: testkern_type
  use inf,      only: field_type
  implicit none
  type(field_type) :: f1
  real(r_def) :: fred

  fred = 20.1_r_def

  call invoke(                      &
       set_field_scalar(fred, f1)   &
          )

end program single_invoke

subroutine expected_code(value, fld)
  do df = 1, undf_w3
    fld(df) = value
  end do
end subroutine expected_code
