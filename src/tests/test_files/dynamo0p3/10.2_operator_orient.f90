    program operator_example

    use field_mod,             only : field_type
    use function_space_mod,    only : function_space_type, W0
    use quadrature_mod,        only : quadrature_type, QR3
    use operator_mod,          only : operator_type
    use testkern_operator_orient_mod, only : testkern_operator_orient_type

    type(quadrature_type), pointer :: qr => null()
    type(field_type)               :: chi(3)
    type(operator_type)            :: mm_w1
    type(function_space_type)      :: fs

    mm_w1 = operator_type(fs%get_instance(W1),fs%get_instance(W1))
    qr => qr%get_instance(QR3,9,3)

    call invoke(testkern_operator_orient_type(mm_w1, chi, qr))

    end program operator_example
