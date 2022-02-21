module output_field_mod

    private
    public output_field

contains

    subroutine output_field(field)
        USE field_mod, only         : r2d_field

        implicit none
        TYPE(r2d_field), intent(in) :: field

        integer                     :: j, xstart, xstop, ystart, ystop

        xstart = field%internal%xstart
        xstop  = field%internal%xstop
        ystart = field%internal%ystart
        ystop  = field%internal%ystop

        do j=ystart, ystop
            write(*,"(99F2.0)") field%data(xstart:xstop, j)
        enddo
        write(*,*)

    end subroutine output_field

end module output_field_mod