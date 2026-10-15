! -----------------------------------------------------------------------------
! Code Governed by the CeCILL licence (http://www.cecill.info)
! -----------------------------------------------------------------------------

! Code extracted from the tra_adv benchmark making use of intrinsics
! MIN, ABS and SIGN. Literals have had the Fortran double precision
! specification (e.g. 1.d0) removed. This code is provided purely to
! demonstrate the removal of intrinsics via PSyclone transformations,
! it is not designed to be run.
program test_intrinsics

  integer, parameter :: jpi=10, jpj=10, jpk=10
  real, dimension(jpi,jpj,jpk) :: zwx, zwy, zslpx, zslpy
  integer :: ji, jj, jk
  
  DO jk = 1, jpk-1    
     DO jj = 2, jpj
        DO ji = 2, jpi
           zslpx(ji,jj,jk) = SIGN( 1.0, zslpx(ji,jj,jk) ) * MIN(    ABS( zslpx(ji  ,jj,jk) ),   &
           &                                                2.0*ABS( zwx  (ji-1,jj,jk) ),   &
           &                                                2.0*ABS( zwx  (ji  ,jj,jk) ) )
           zslpy(ji,jj,jk) = SIGN( 1.0, zslpy(ji,jj,jk) ) * MIN(    ABS( zslpy(ji,jj  ,jk) ),   &
           &                                                2.0*ABS( zwy  (ji,jj-1,jk) ),   &
           &                                                2.0*ABS( zwy  (ji,jj  ,jk) ) )
        END DO
     END DO
  END DO

end program test_intrinsics
