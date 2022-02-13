module output_field_mod

    private
    public output_field

contains

    subroutine output_field(field)
        USE field_mod, only         : r2d_field

        implicit none
        TYPE(r2d_field), intent(in) :: field

        integer                     :: j, xstart, xstop, ystart, ystop

        xstart = field%internal%xstart+1
        xstop  = field%internal%xstop-1
        ystart = field%internal%ystart+1
        ystop  = field%internal%ystop-1

        do j=ystart, ystop
            write(*,"(99F2.0)") field%data(xstart:xstop, j)
        enddo
        write(*,*)

    end subroutine output_field

end module output_field_mod