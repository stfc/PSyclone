program single_invoke_annexed

  ! Description: f1 and f2 are written to over cells and then read. f1
  ! is on the w1 function space and f2 is on the w2 function space, so
  ! both are continuous and therefore have annexed dofs. By default
  ! loops over cells write to the level 1 halo in order to ensure that
  ! annexed dofs are correct (clean). Therefore halo exchanges will
  ! not be required so that for both f1 and f2 are clean when they are
  ! read.
  use testkern_w3, only: testkern_w3_type
  use testkern_w2_only, only: testkern_w2_only_type
  use inf, only: field_type
  implicit none
  type(field_type) :: f1, f2, f3, f4, m1, m2
  real(r_def) :: a

  call invoke(                         &
       ! update f1 locally
       setval_c(f1,0.0),               &
       ! update f2 in l1 halo
       testkern_w2_only_type(f2,f3),   &
       ! read f1 and f2 annexed dofs
       ! no halo exchange should be added for f2
       testkern_w3_type(a,f1,f2,m1,m2) &
          )

end program single_invoke_annexed
