    program operator_example

    use field_mod,             only : field_type
    use function_space_mod,    only : function_space_type, W0
    use quadrature_mod,        only : quadrature_type, QR3
    use operator_mod,          only : operator_type
    use assemble_weak_derivative_w3_w2_kernel_mod, only : assemble_weak_derivative_w3_w2_kernel_type

    type(quadrature_type), pointer :: qr => null()
    type(field_type)               :: chi(3)
    type(operator_type)            :: mapping
    type(function_space_type)      :: fs

    mapping = operator_type(fs%get_instance(W3),fs%get_instance(W2))
    qr => qr%get_instance(QR3,9,3)

    call invoke(assemble_weak_derivative_w3_w2_kernel_type(mapping, chi, qr))

    end program operator_example
