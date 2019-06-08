program tridiagonal_solve
  do k=1,n
     do j=1,n
        do i=1,n
           c(i,j,k) = c(i,j,k)/b(i,j,k)
        end do
     end do
  end do
  do k=2,n
     do j=1,n
        do i=1,n
           m = 1.0/(b(i,j,k)-a(i,j,k)*c(i,j,k-1))
           c(i,j,k) = c(i,j,k)*m
           d(i,j,k) = (d(i,j,k)-a(i,j,k)*d(i,j,k-1))*m
        end do
     end do
  end do  
  do k=n,1,-1
     do j=1,n
        do i=1,n
           d(i,j,k) = d(i,j,k) - c(i,j,k)*d(i,j,k+1)
        end do
     end do
  end do
  
end program tridiagonal_solve

