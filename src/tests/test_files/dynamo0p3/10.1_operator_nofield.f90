    program operator_example_nofield

    ! an example with an operator on w2 space but no field on w2 space

    use field_mod,                     only : field_type
    use function_space_mod,            only : function_space_type, W2
    use quadrature_mod,                only : quadrature_type, QR3
    use operator_mod,                  only : operator_type
    use testkern_operator_nofield_mod, only : testkern_operator_nofield_type

    type(quadrature_type), pointer :: qr => null()
    type(field_type)               :: chi(3)
    type(operator_type)            :: mm_w2
    type(function_space_type)      :: fs

    mm_w2 = operator_type(fs%get_instance(W2),fs%get_instance(W2))
    qr => qr%get_instance(QR3,9,3)

    call invoke(testkern_operator_nofield_type(mm_w2, chi, qr))

    end program operator_example_nofield
