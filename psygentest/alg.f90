module alg
contains
subroutine run
    use psy, only : kern1,kern2
    use types, only : fieldtype
    type(fieldtype) :: value
    call kern1(value)
    call kern2(kern1,value, &
               kern1,value)
end subroutine run
end module alg
