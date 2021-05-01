module x
contains
  subroutine y(field1,field2,field3, n)
    integer, intent(in) :: n
    real, intent(out) :: field1(n)
    real, intent(inout) :: field2(n), field3(n)
    integer :: i
    do i=1,10
       field1(i) = field2(i) * field3(i)
    end do
  end subroutine y
end module x
