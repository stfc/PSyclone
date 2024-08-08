subroutine main()
    use mpi
    real*8, dimension(:,:,:), allocatable :: su, sv, sw, u, v, w
    real*8, dimension(:,:,:), allocatable :: tzc1, tzc2, tzd1, tzd2
    integer :: k, j, i, nx, ny, nz, rank, fixed

    call MPI_Init()

    nx=256
    ny=512
    nz=256
    
    fixed=1

    allocate(su(nz, ny, nx))
    allocate(sv(nz, ny, nx))
    allocate(sw(nz, ny, nx))
    allocate(u(nz, ny, nx))
    allocate(v(nz, ny, nx))
    allocate(w(nz, ny, nx))
    allocate(tzc1(nz, fixed, fixed))
    allocate(tzc2(nz, fixed, fixed))
    allocate(tzd1(nz, fixed, fixed))
    allocate(tzd2(nz, fixed, fixed))

    do i=1, nx
      do j=1, ny
        do k=1, nz
          u(k,j,i)=10.0
          v(k,j,i)=20.0
          w(k,j,i)=30.0
        end do
      end do
    end do

    do k=1, nz
      tzc1(k, fixed, fixed)=50.0
      tzc2(k, fixed, fixed)=15.0
      tzd1(k, fixed, fixed)=100.0
      tzd2(k, fixed, fixed)=5.0
    end do

    do i=2,nx-1
      do j=2,ny-1
        do k=2,nz-1
          su(k, j, i)=&
            (2.0*(u(k, j, i-1)*(u(k, j, i)+u(k, j, i-1))-u(k, j, i+1)*(u(k, j, i)+u(k, j, i+1)))) + &
            (1.0*(u(k, j-1, i)*(v(k, j-1, i)+v(k, j-1, i+1))-u(k, j+1, i)*(v(k, j, i)+v(k, j, i+1)))) + &
            (tzc1(k, fixed, fixed)*u(k-1, j, i)*(w(k-1, j, i)+w(k-1, j, i+1))-tzc2(k, fixed, fixed)*u(k+1, j, i)*(w(k, j, i)+w(k, j, i+1)))

          sv(k, j, i)=&
            (2.0*(v(k, j-1, i)*(v(k, j, i)+v(k, j-1, i))-v(k, j+1, i)*(v(k, j, i)+v(k, j+1, i)))) + &
            (2.0*(v(k, j, i-1)*(u(k, j, i-1)+u(k, j+1, i-1))-v(k, j, i+1)*(u(k, j, i)+u(k, j+1, i)))) + &
            (tzc1(k, fixed, fixed)*v(k-1, j, i)*(w(k-1, j, i)+w(k-1, j+1, i))-tzc2(k, fixed, fixed)*v(k+1, j, i)*(w(k, j, i)+w(k, j+1, i)))

          sw(k, j, i)=&
            (tzd1(k, fixed, fixed)*w(k-1, j, i)*(w(k, j, i)+w(k-1, j, i))-tzd2(k, fixed, fixed)*w(k+1, j, i)*(w(k, j, i)+w(k+1, j, i))) + &
            (2.0*(w(k, j, i-1)*(u(k, j, i-1)+u(k+1, j, i-1))-w(k, j, i+1)*(u(k, j, i)+u(k+1, j, i)))) + &
            (2.0*(w(k, j-1, i)*(v(k, j-1, i)+v(k+1, j-1, i))-w(k, j+1, i)*(v(k, j, i)+v(k+1, j, i))))
        end do
      end do
    end do

    deallocate(su)
    deallocate(sv)
    deallocate(sw)
    deallocate(u)
    deallocate(v)
    deallocate(w)
    deallocate(tzc1)
    deallocate(tzc2)
    deallocate(tzd1)
    deallocate(tzd2)

    call MPI_Finalize()
end subroutine main
