module res_cpp
use, intrinsic :: iso_c_binding
implicit none
  interface
  subroutine &
    run_nemo_from_host_cpp( &
        isize,  &
        jsize,  &
        ksize,  &
        tsn     &
    ) bind(c)
      use, intrinsic :: iso_c_binding          
        integer(c_int), value :: isize 
        integer(c_int), value :: jsize 
        integer(c_int), value :: ksize 
        real(c_double), dimension(*), target :: tsn 
    end subroutine
  end interface
end module
