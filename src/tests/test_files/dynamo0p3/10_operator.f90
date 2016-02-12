    program operator_example

    use field_mod,             only : field_type
    use function_space_mod,    only : function_space_type, W0
    use quadrature_mod,        only : quadrature_type, QR3
    use operator_mod,          only : operator_type
    use testkern_operator_mod, only : testkern_operator_type

    type(quadrature_type), pointer :: qr => null()
    type(field_type)               :: chi(3)
    type(operator_type)            :: mm_w0
    type(function_space_type)      :: fs
    real(r_def)                    :: a

    a = 1.0_r_def
    mm_w0 = operator_type(fs%get_instance(W0),fs%get_instance(W0))
    qr => qr%get_instance(QR3,9,3)

    call invoke(testkern_operator_type(mm_w0, chi, a, qr))

    end program operator_example
