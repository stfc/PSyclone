program multi_kernel_multi_invokes

  ! Description: two invokes, each with a two kernel calls
  use testkern1, only: testkern1_type
  use testkern2, only: testkern2_type
  use testkern3, only: testkern3_type
  use testkern4, only: testkern4_type
  implicit none
  type(field_type) :: f1, f2, f3, f4

  call invoke(                       &
       testkern1_type(f1,f2,f3,f4)   &
       testkern2_type(f2,f3,f4,f1)   &
          )

  call invoke(                       &
       testkern3_type(f3,f4,f1,f2)   &
       testkern4_type(f4,f1,f2,f3)   &
          )

end program multi_kernel_multi_invokes
