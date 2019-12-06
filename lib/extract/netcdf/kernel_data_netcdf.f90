module psy_data_mod

    implicit none

    ! Contains functionality to write and read variables
    type, public:: PSyDataType
        integer, private                   :: ncid
        integer, dimension(:), allocatable :: var_id
        integer                            :: num_pre_vars, &
                                              num_post_vars
        integer                            :: next_var_index
    contains
        procedure :: DeclareScalarInteger, WriteScalarInteger
        procedure :: DeclareScalarReal,    WriteScalarReal
        procedure :: DeclareFieldReal,     WriteFieldReal
        procedure :: PreStart, PreEndDeclaration, PreEnd
        procedure :: PostStart, PostEnd

        generic, public :: PreDeclareVariable => DeclareScalarInteger, &
                                                 DeclareScalarReal,    &
                                                 DeclareFieldReal

        generic, public :: WriteVariable => WriteScalarInteger, &
                                            WriteScalarReal,    &
                                            WriteFieldReal
    end type PSyDataType

Contains

    ! -------------------------------------------------------------------------
    ! Checks if the return value from a netcdf call indicates an error.
    ! If so, print the corresponding error message.
    function CheckError(retval) 
        use netcdf
        implicit none
        integer :: CheckError
        integer, intent(in) :: retval
        if (retval /= nf90_noerr) then
            print *,"NetCDF Error:"
            print *,trim(nf90_strerror(retval))
        endif
        CheckError = retval
    end function CheckError

    ! -------------------------------------------------------------------------
    subroutine PreStart(this, module_name, kernel_name, num_pre_vars, &
                        num_post_vars)
        use netcdf, only : nf90_create, NF90_CLOBBER
        implicit none
        class(PSyDataType), intent(inout) :: this
        character(*), intent(in) :: module_name, kernel_name
        integer, intent(in)      :: num_pre_vars, num_post_vars
        integer :: retval

        retval = CheckError(nf90_create(module_name//kernel_name//".nc", &
                                        NF90_CLOBBER, this%ncid))
        allocate(this%var_id(num_pre_vars+num_post_vars))
        this%num_pre_vars = num_pre_vars
        this%num_post_vars = num_post_vars
        this%next_var_index = 1
    end subroutine PreStart

    ! -------------------------------------------------------------------------
    subroutine PreEndDeclaration(this)
        use netcdf, only : nf90_close
        implicit none
        class(PSyDataType), intent(inout) :: this
    end subroutine PreEndDeclaration
    ! -------------------------------------------------------------------------
    subroutine PreEnd(this)
        use netcdf, only : nf90_close
        implicit none
        class(PSyDataType), intent(inout) :: this
    end subroutine PreEnd
    ! -------------------------------------------------------------------------
    subroutine PostStart(this)
        use netcdf, only : nf90_close
        implicit none
        class(PSyDataType), intent(inout) :: this
    end subroutine PostStart
    ! -------------------------------------------------------------------------
    subroutine PostEnd(this)
        use netcdf, only : nf90_close
        implicit none
        class(PSyDataType), intent(inout) :: this
        integer :: retval
        retval = CheckError(nf90_close(this%ncid))
    end subroutine PostEnd

    ! -------------------------------------------------------------------------
    subroutine CheckLastDeclaration(this)
        use netcdf
        implicit none
        type(PSyDataType), intent(inout) :: this
        integer :: retval
        ! Test if this was the last declaration
        if (this%next_var_index > this%num_pre_vars+this%num_post_vars) then
            this%next_var_index = 1
            retval = CheckError(nf90_enddef(this%ncid))
        endif   
    end subroutine CheckLastDeclaration

    ! -------------------------------------------------------------------------
    subroutine DeclareScalarInteger(this, name, value)
        use netcdf
        implicit none
        class(PSyDataType), intent(inout) :: this
        character(*), intent(in) :: name
        integer, intent(in) :: value
        integer :: retval
        retval = CheckError(nf90_def_var(this%ncid, name, Nf90_INT,     &
                                         this%var_id(this%next_var_index)))
        this%next_var_index = this%next_var_index + 1
        call CheckLastDeclaration(this)
    end subroutine DeclareScalarInteger

    ! -------------------------------------------------------------------------
    subroutine WriteScalarInteger(this, name, value)
        use netcdf
        implicit none
        class(PSyDataType), intent(inout) :: this
        character(*), intent(in) :: name
        integer, intent(in) :: value
        integer :: retval
        retval = CheckError(nf90_put_var(this%ncid, this%var_id(this%next_var_index),  &
                                         value))
        this%next_var_index = this%next_var_index + 1
    end subroutine WriteScalarInteger

    ! -------------------------------------------------------------------------
    subroutine DeclareScalarReal(this, name, value)
        use netcdf
        implicit none
        class(PSyDataType), intent(inout) :: this
        character(*), intent(in) :: name
        real, intent(in) :: value
        integer :: retval
        retval = CheckError(nf90_def_var(this%ncid, name, Nf90_REAL,     &
                                         this%var_id(this%next_var_index)))
        this%next_var_index = this%next_var_index + 1
        call CheckLastDeclaration(this)
        print *,"declared", name
    end subroutine DeclareScalarReal

    ! -------------------------------------------------------------------------
    subroutine WriteScalarReal(this, name, value)
        use netcdf
        implicit none
        class(PSyDataType), intent(inout) :: this
        character(*), intent(in) :: name
        real, intent(in) :: value
        integer :: retval
        retval = CheckError(nf90_put_var(this%ncid, this%var_id(this%next_var_index),  &
                                         value))
        this%next_var_index = this%next_var_index + 1
    end subroutine WriteScalarReal

    ! -------------------------------------------------------------------------
    subroutine WriteScalarDoublePrecision(name, value)
        implicit none
        character(*), intent(in) :: name
        double precision, intent(in) :: value
    end subroutine WriteScalarDoublePrecision
    
    ! -------------------------------------------------------------------------
    subroutine DeclareFieldReal(this, name, value)
        use netcdf
        use field_mod, only : r2d_field
        implicit none
        class(PSyDataType), intent(inout) :: this
        character(*), intent(in) :: name
        type(r2d_field), intent(in) :: value
        integer :: x_dimid, y_dimid, retval
        integer, dimension(2) :: dimids

        retval = CheckError(nf90_def_dim(this%ncid, name//"dim1",  &
                                         value%grid%nx, x_dimid))
        retval = CheckError(nf90_def_dim(this%ncid, name//"dim2",  &
                                         value%grid%ny, y_dimid))
        dimids =  (/ x_dimid, y_dimid /)
        retval = CheckError(nf90_def_var(this%ncid, name, Nf90_REAL8,     &
                                         dimids, this%var_id(this%next_var_index)))

        this%next_var_index = this%next_var_index + 1
        call CheckLastDeclaration(this)
    end subroutine DeclareFieldReal

    ! -------------------------------------------------------------------------
    subroutine WriteFieldReal(this, name, value)
        use netcdf
        use field_mod, only : r2d_field
        implicit none
        class(PSyDataType), intent(inout) :: this
        character(*), intent(in) :: name
        type(r2d_field), intent(in) :: value
        integer :: retval
        retval = CheckError(nf90_put_var(this%ncid, this%var_id(this%next_var_index),  &
                                         value%data(:,:)))

        this%next_var_index = this%next_var_index + 1
    end subroutine WriteFieldReal

    ! -------------------------------------------------------------------------

end module psy_data_mod