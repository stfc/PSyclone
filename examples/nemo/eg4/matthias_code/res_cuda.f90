module res_cuda
use, intrinsic :: iso_c_binding
implicit none
  interface
  subroutine &
    run_nemo_from_host_cuda( &
        isize,  &
        jsize,  &
        ksize,  &
        ztfreez,  &
        pwn,  &
        vmask,  &
        rnfmsk,  &
        mydomain,  &
        tmask,  &
        umask,  &
        tsn,  &
        pvn, &
	      rnfmask_z,  &
        pun,  &
        upsmsk,  & 
        zslpx &
    ) bind(c)
      use, intrinsic :: iso_c_binding     
        integer(c_int), value :: isize 
        integer(c_int), value :: jsize 
        integer(c_int), value :: ksize 
        real(c_double), dimension(*), target :: ztfreez 
        real(c_double), dimension(*), target :: pwn
        real(c_double), dimension(*), target :: vmask 
        real(c_double), dimension(*), target :: rnfmsk 
        real(c_double), dimension(*), target :: mydomain 
        real(c_double), dimension(*), target :: tmask 
        real(c_double), dimension(*), target :: umask 
        real(c_double), dimension(*), target :: tsn 
        real(c_double), dimension(*), target :: pvn
	      real(c_double), dimension(*), target :: rnfmask_z 
        real(c_double), dimension(*), target :: pun 
        real(c_double), dimension(*), target :: upsmsk 
        real(c_double), dimension(*), target :: zslpx
    end subroutine
  end interface
end module  