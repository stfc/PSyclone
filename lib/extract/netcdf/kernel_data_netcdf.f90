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
        procedure :: DeclareScalarDouble,  WriteScalarDouble
        procedure :: DeclareFieldDouble,   WriteFieldDouble
        procedure :: ReadScalarInteger, ReadScalarReal, ReadScalarDouble
        procedure :: ReadFieldDouble
        procedure :: PreStart, PreEndDeclaration, PreEnd
        procedure :: PostStart, PostEnd
        procedure :: OpenRead

        generic, public :: PreDeclareVariable => DeclareScalarInteger, &
                                                 DeclareScalarReal,    &
                                                 DeclareScalarDouble,  &
                                                 DeclareFieldDouble

        generic, public :: ProvideVariable => WriteScalarInteger, &
                                              WriteScalarReal,    &
                                              WriteScalarDouble,  &
                                              WriteFieldDouble

        generic, public :: ReadVariable => ReadScalarInteger, &
                                           ReadScalarReal,    &
                                           ReadScalarDouble,  &
                                           ReadFieldDouble 
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

        retval = CheckError(nf90_create(module_name//"-"//kernel_name//".nc", &
                                        NF90_CLOBBER, this%ncid))
        allocate(this%var_id(num_pre_vars+num_post_vars))
        this%num_pre_vars = num_pre_vars
        this%num_post_vars = num_post_vars
        this%next_var_index = 1
    end subroutine PreStart

    ! -------------------------------------------------------------------------
    subroutine OpenRead(this, module_name, kernel_name)
        use netcdf, only : nf90_open, NF90_NOWRITE
        implicit none
        class(PSyDataType), intent(inout) :: this
        character(*), intent(in) :: module_name, kernel_name
        integer :: retval

        retval = CheckError(nf90_open(module_name//"-"//kernel_name//".nc", &
                                        NF90_NOWRITE, this%ncid))
    end subroutine OpenRead
    ! -------------------------------------------------------------------------
    subroutine PreEndDeclaration(this)
        use netcdf, only : nf90_enddef
        implicit none
        class(PSyDataType), intent(inout) :: this
        integer :: retval
        retval = CheckError(nf90_enddef(this%ncid))
    end subroutine PreEndDeclaration
    ! -------------------------------------------------------------------------
    subroutine PreEnd(this)
        implicit none
        class(PSyDataType), intent(inout) :: this
    end subroutine PreEnd
    ! -------------------------------------------------------------------------
    subroutine PostStart(this)
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
    subroutine ReadScalarInteger(this, name, value)
        use netcdf
        implicit none

        class(PSyDataType), intent(inout) :: this
        character(*), intent(in) :: name
        integer :: value
        integer :: retval, varid
        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        retval = CheckError(nf90_get_var(this%ncid, varid, value))
    end subroutine ReadScalarInteger

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
    subroutine ReadScalarReal(this, name, value)
        use netcdf
        implicit none

        class(PSyDataType), intent(inout) :: this
        character(*), intent(in) :: name
        real :: value
        integer :: retval, varid
        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        retval = CheckError(nf90_get_var(this%ncid, varid, value))
    end subroutine ReadScalarReal

    ! -------------------------------------------------------------------------
    subroutine DeclareScalarDouble(this, name, value)
        use netcdf
        implicit none
        class(PSyDataType), intent(inout) :: this
        character(*), intent(in) :: name
        double precision, intent(in) :: value
        integer :: retval
        retval = CheckError(nf90_def_var(this%ncid, name, Nf90_DOUBLE,     &
                                         this%var_id(this%next_var_index)))
        this%next_var_index = this%next_var_index + 1
        call CheckLastDeclaration(this)
    end subroutine DeclareScalarDouble

    ! -------------------------------------------------------------------------
    subroutine WriteScalarDouble(this, name, value)
        use netcdf
        implicit none
        class(PSyDataType), intent(inout) :: this
        character(*), intent(in) :: name
        double precision, intent(in) :: value
        integer :: retval
        retval = CheckError(nf90_put_var(this%ncid, this%var_id(this%next_var_index),  &
                                         value))
        this%next_var_index = this%next_var_index + 1
    end subroutine WriteScalarDouble
    
    ! -------------------------------------------------------------------------
    subroutine ReadScalarDouble(this, name, value)
        use netcdf
        implicit none

        class(PSyDataType), intent(inout) :: this
        character(*), intent(in) :: name
        double precision :: value
        integer :: retval, varid
        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        retval = CheckError(nf90_get_var(this%ncid, varid, value))
    end subroutine ReadScalarDouble

    ! -------------------------------------------------------------------------
    subroutine DeclareFieldDouble(this, name, value, prefix)
        use netcdf
        use field_mod, only : r2d_field
        implicit none
        class(PSyDataType), intent(inout) :: this
        character(*), intent(in) :: name
        type(r2d_field), intent(in) :: value
        character(*), intent(in), optional :: prefix
        integer :: x_dimid, y_dimid, retval
        integer, dimension(2) :: dimids
        character(128) :: real_prefix

        if (present(prefix)) then
            real_prefix = prefix
        else
            real_prefix = ""
        endif
        retval = CheckError(nf90_def_dim(this%ncid, name//"dim1",  &
                                         value%grid%nx, x_dimid))
        retval = CheckError(nf90_def_dim(this%ncid, name//"dim2",  &
                                         value%grid%ny, y_dimid))
        dimids =  (/ x_dimid, y_dimid /)
        retval = CheckError(nf90_def_var(this%ncid, trim(name//real_prefix), Nf90_REAL8,     &
                                         dimids, this%var_id(this%next_var_index)))

        this%next_var_index = this%next_var_index + 1
        call CheckLastDeclaration(this)
    end subroutine DeclareFieldDouble

    ! -------------------------------------------------------------------------
    subroutine WriteFieldDouble(this, name, value)
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
    end subroutine WriteFieldDouble

    ! -------------------------------------------------------------------------
    subroutine ReadFieldDouble(this, name, value)
        use netcdf
        implicit none

        class(PSyDataType), intent(inout) :: this
        character(*), intent(in) :: name
        double precision, dimension(:,:), allocatable :: value
        integer :: retval, varid
        integer :: dim1_id, dim1
        integer :: dim2_id, dim2, i
        character(100) :: x

        retval = CheckError(nf90_inq_dimid(this%ncid, trim(name//"dim1"), dim1_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim1_id,  &
                                                   len=dim1))
        retval = CheckError(nf90_inq_dimid(this%ncid, name//"dim2", dim2_id))
        retval = CheckError(nf90_inquire_dimension(this%ncid, dim2_id,  &
                                                   len=dim2))
        allocate(value(dim1, dim2))
        ! Initialise it with 0, so that an array comparison will work
        ! even though e.g. boundary areas or so might not be set at all.
        value = 0
        retval = CheckError(nf90_inq_varid(this%ncid, name, varid))
        retval = CheckError(nf90_get_var(this%ncid, varid, value))
    end subroutine ReadFieldDouble
    ! -------------------------------------------------------------------------

end module psy_data_mod