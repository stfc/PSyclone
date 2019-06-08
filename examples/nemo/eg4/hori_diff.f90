program hori_diff
  do k=1,n
     do j=1,n
        do i=1,n
           lap(i,j,k)=(-4.0)*in(i,j,k)+coeff(i,j,k)*(in(i+1,j,k)+in(i-1,j,k)+in(i,j+1,k)+in(i,j-1,k))
           out(i,j,k)=(-4.0)*lap(i,j,k)+coeff(i,j,k)*(lap(i+1,j,k)+lap(i-1,j,k)+lap(i,j+1,k)+lap(i,j-1,k))
        end do
     end do
  end do
end program hori_diff
