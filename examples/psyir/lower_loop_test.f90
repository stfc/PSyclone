subroutine test(a,b,c,ni,nj,nk)
  !
  implicit none
  !
  real, intent(in), dimension(ni,nj,nk) :: a,b
  real, intent(out), dimension(ni,nj,nk) :: c
  integer, intent(in) :: ni,nj,nk
  !
  real :: tmp1, tmp2(nj), tmp3(nj,nk)
  !
  integer :: i,j,k
  !
  j=1
  k=1
  do i=1,ni
     tmp1 = a(i,j,k)
     tmp2(j) = b(i,j,k)
     call work(tmp1,tmp2,tmp3)
     c(i,j,k) = tmp3(j,k)
  end do
end subroutine test
