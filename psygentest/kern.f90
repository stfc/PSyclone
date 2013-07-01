module kern
implicit none
private
public kern1
contains
  subroutine kern1(value)
  real,intent(in) :: value(:)
  print *,"kern1: value=",value
  end subroutine kern1
end module kern
