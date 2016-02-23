    program any_space_example

    use testkern_any_space_1_mod, only : testkern_any_space_1_type
    real(r_def) :: rdt

    call invoke(testkern_any_space_1_type(a, rdt, b, c, qr))

    end program any_space_example
