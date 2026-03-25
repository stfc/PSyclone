program add_parallelism_example

    integer, dimension(100) :: a, b, c, map
    integer, dimension(10,10) :: array
    integer, dimension(10) :: tmp
    integer :: result
    integer :: i, j

    do i = 1, 100
       a(i) = b(i) + c(i)
    end do

    do i = 1, 100
       write (*,*) i
       a(i) = b(i) + c(i)
    end do

    do i = 1, 100
       result = result + c(i)
    end do

    do i = 1, 10
       do j=1, 10
          tmp(j) = j
       enddo
       do j=1, 10
          array(i, j) = tmp(j) + 3
       enddo
    end do

    do i = 1, 100
       ! We know map does not repeat indices
       a(map(i)) = b(i)
    end do

    do i = 1, 100
       a(1) = i
       b(i) = a(1) + i
    end do

end program add_parallelism_example
