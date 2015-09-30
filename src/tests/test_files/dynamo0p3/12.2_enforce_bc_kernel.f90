    program enforce_bc_kernel_example

    ! this boundary condition kernel has been created as a temporary measure
    ! as boundary layer information is not currently described
    ! in the API. Therefore, for the moment, users can add this kernel
    ! when they want to enforce boundary conditions

    use enforce_bc_kernel_mod, only : enforce_bc_kernel_type

    call invoke(enforce_bc_kernel_type(a))

    end program enforce_bc_kernel_example
