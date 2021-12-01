program simple
  use res_cpp, only: run_nemo_from_host_cpp
  implicit none
  REAL*8, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: tsn 
  integer :: ji,jj,jk,jt
  integer :: jpi,jpj,jpk
  REAL*8 :: tsn_sum
  jpi=2
  jpj=3
  jpk=4
  ALLOCATE(tsn(jpi,jpj,jpk))

  tsn_sum = 0.0
  DO jk = 1, jpk
     DO jj = 1, jpj
        DO ji = 1, jpi
           tsn(ji,jj,jk)= ji*jj*jk
           tsn_sum = tsn_sum + tsn(ji,jj,jk)
        END DO
     END DO
  END DO
  print *, jpi, jpj, jpk
  print *, "TSN_SUM IS",tsn_sum
  DO jk = 1, jpk
     DO jj = 1, jpj
        DO ji = 1, jpi
           print *, ji,jj,jk,tsn(ji,jj,jk)
        end do
     end do
  end do
     
  do jt = 1, 1
     call run_nemo_from_host_cpp(jpi, jpj, jpk, tsn)
  end do

  deallocate(tsn)

end program simple
