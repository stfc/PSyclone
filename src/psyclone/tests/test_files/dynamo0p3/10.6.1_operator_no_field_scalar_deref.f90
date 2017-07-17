program operator_example

    use field_mod,             only : field_type
    use function_space_mod,    only : function_space_type, W0
    use quadrature_mod,        only : quadrature_type, QR3
    use operator_mod,          only : operator_type
    use testkern_operator_nofield_scalar_mod, only : testkern_operator_nofield_scalar_type

    type(quadrature_type), pointer :: qr => null()
    type(field_type)               :: chi(3)
    type(function_space_type)      :: fs
    ! Pretend we have some derived types that we must dereference in order
    ! to get the scalar and operator arguments of the kernel
    type(some_scalar_type)         :: box
    type(some_operator_type)       :: opbox

    call invoke(testkern_operator_nofield_scalar_type(opbox%my_mapping, &
                                                      box%b(1),   &
                                                      qr%get_instance(QR3,9,3)))

  end program operator_example
