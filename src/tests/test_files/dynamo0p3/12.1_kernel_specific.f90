    program kernel_specific_example2

    ! the ru_kernel kernel currently requires additional
    ! boundary layer information to be set up which is not described
    ! in the API. Therefore, for the moment, we add this in when we
    ! see the ru_kernel_code

    use ru_kernel_mod, only : ru_kernel_type

    call invoke(ru_kernel_type(a, b, c, d, e))

    end program kernel_specific_example2
