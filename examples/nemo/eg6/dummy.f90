program dummy
   real, dimension(10, 10)   :: umask
   character(len=10) :: char_var
   integer  :: ji, jj
   logical :: logical_var
   integer(kind=8) :: offset
   integer, intrinsic :: loc, sizeof

   logical_var = .false.
   char_var = "test"

   do jj = 1, 10
      do ji = 1, 10
         if (char_var .eq. "abc" .and. logical_var) then
            umask(ji,jj) = 1
         else
            umask(ji,jj) = 3
         endif
       end do

    ! VERY NAUGHTY CODE AHEAD - there be dragon!!
    ! We modify the values of some read-only fields by using offsets
    ! in a written array. This will abort if the code is compiled
    ! with array bound check of course.
    ! TODO: Unfortunately, PSyclone does not support loc or sizeof,
    ! so the code below does not work. But you can create the psy-layer,
    ! and then copy the following lines into psy.f90, and recompile the
    ! program. This will then issue a warning about `logical_var` being
    ! overwritten.
    ! offset = ( loc(logical_var) - loc(umask(1,1)) ) / sizeof(logical_var)
    ! umask(1+offset, 1) = 123.0
    end do

    print *,umask(1,1), logical_var
end program dummy
