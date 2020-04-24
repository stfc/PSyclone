program any_space_example

  use testkern_any_space_2_mod, only : testkern_any_space_2_type
  integer :: istp

!  call invoke(testkern_any_space_2_type(a, b, c, istp))
  call invoke(testkern_any_space_2_type(a, b, c, istp), &
              testkern_any_space_2_type(d, e, f, istp))
!              testkern_any_space_2_type(a, b, c, istp))
!  call invoke(testkern_any_space_2_type(a, b, c, istp))
!  call invoke(testkern_any_space_2_type(d, e, f, istp))

end program any_space_example
