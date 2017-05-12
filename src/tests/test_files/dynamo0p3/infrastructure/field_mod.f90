module field_mod
  use constants_mod, only: r_def
  use function_space_mod

  type field_type
     integer :: a
     real(kind=r_def), dimension(:), allocatable :: data
  end type field_type

  type field_proxy_type
     integer :: b
     type(function_space_type) :: vspace
  end type field_proxy_type

end module field_mod
