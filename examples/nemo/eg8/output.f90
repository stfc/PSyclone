program add_parallelism_example
  integer, dimension(100) :: a
  integer, dimension(100) :: b
  integer, dimension(100) :: c
  integer, dimension(100) :: map
  integer, dimension(10,10) :: array
  integer, dimension(10) :: tmp
  integer :: result
  integer :: i
  integer :: j

  !$omp parallel do default(shared) private(i) schedule(auto)
  do i = 1, 100, 1
    a(i) = b(i) + c(i)
  enddo
  !$omp end parallel do
  !$omp parallel do default(shared) private(i) schedule(auto)
  do i = 1, 100, 1
    ! PSyclone CodeBlock (unsupported code) reason:
    !  - Unsupported statement: Write_Stmt
    WRITE(*, *) i
    a(i) = b(i) + c(i)
  enddo
  !$omp end parallel do
  !$omp parallel do default(shared) private(i) schedule(auto) reduction(+: result)
  do i = 1, 100, 1
    result = result + c(i)
  enddo
  !$omp end parallel do
  !$omp parallel do default(shared) private(i,j,tmp) schedule(auto)
  do i = 1, 10, 1
    do j = 1, 10, 1
      tmp(j) = j
    enddo
    do j = 1, 10, 1
      array(i,j) = tmp(j) + 3
    enddo
  enddo
  !$omp end parallel do
  !$omp parallel do default(shared) private(i) schedule(auto)
  do i = 1, 100, 1
    a(map(i)) = b(i)
  enddo
  !$omp end parallel do
  !$omp parallel do default(shared) private(i) firstprivate(a) schedule(auto)
  do i = 1, 100, 1
    a(1) = i
    b(i) = a(1) + i
  enddo
  !$omp end parallel do

end program add_parallelism_example
