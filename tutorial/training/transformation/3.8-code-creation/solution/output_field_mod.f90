module output_field_mod

    private
    public output_field

contains

    subroutine output_field(field)
        implicit none
        real(kind=8), dimension(:,:), allocatable :: field

        integer                                  :: j

        do j=lbound(field, 2)+1, ubound(field, 2)-1
            write(*,"(99F2.0)") field(2:ubound(field,1)-1, j)
        enddo
        write(*,*)

    end subroutine output_field

end module output_field_mod