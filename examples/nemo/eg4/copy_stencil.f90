program copy_stencil
  do k=1,n
     do j=1,n
        do i=1,n
           out(i,j,k) = in(i+1,j,k)
        end do
     end do
  end do
end program copy_stencil
