module output_field_mod

    private
    public output_field

contains

    subroutine output_field(field)
        USE field_mod, only      : r2d_field
        USE kind_params_mod, only: go_wp
        USE parallel_mod, only: on_master

        implicit none
        TYPE(r2d_field), intent(in) :: field

        real(go_wp), dimension(:,:), allocatable :: global_data
        integer                                  :: j

!$acc update self(field, field%data)
        ! Gather the (potentially distributed) arrays into a
        ! 2d Fortran array
        call field%gather_inner_data(global_data)

        if (on_master()) then
            do j=lbound(global_data, 2), ubound(global_data, 2)
                write(*,"(99F2.0)") global_data(1:ubound(global_data,1), j)
            enddo
            write(*,*)
        endif
        deallocate(global_data)

    end subroutine output_field

end module output_field_mod