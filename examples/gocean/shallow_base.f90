PROGRAM shallow

!     BENCHMARK WEATHER PREDICTION PROGRAM FOR COMPARING THE
!     PREFORMANCE OF CURRENT SUPERCOMPUTERS. THE MODEL IS
!     BASED OF THE PAPER - THE DYNAMICS OF FINITE-DIFFERENCE
!     MODELS OF THE SHALLOW-WATER EQUATIONS, BY ROBERT SADOURNY
!     J. ATM. SCIENCES, VOL 32, NO 4, APRIL 1975.
!     
!     CODE BY PAUL N. SWARZTRAUBER, NATIONAL CENTER FOR
!     ATMOSPHERIC RESEARCH, BOULDER, CO,  OCTOBER 1984.
!     Modified by Juliana Rew, NCAR, January 2006
!
!     In this version, shallow4.f, initial and calculated values
!     of U, V, and P are written to a netCDF file
!     for later use in visualizing the results. The netCDF data
!     management library is freely available from
!     http://www.unidata.ucar.edu/software/netcdf
!     This code is still serial but has been brought up to modern
!     Fortran constructs and uses portable intrinsic Fortran 90 timing routines
!     This can be compiled on the IBM SP using:
!     xlf90 -qmaxmem=-1 -g -o shallow4 -qfixed=132 -qsclk=micro \
!     -I/usr/local/include shallow4.f -L/usr/local/lib32/r4i4 -l netcdf
!     where the -L and -I point to local installation of netCDF
!     
!     Changes from shallow4.f (Annette Osprey, January 2010):
!     - Converted to free-form fortran 90.  
!     - Some tidying up of old commented-out code.   
!     - Explicit type declarations.
!     - Variables n, m, itmax and mprint read in from namelist. 
!     - Dynamic array allocation.
!     - Only write to netcdf at mprint timesteps.
!     - Don't write wrap-around points to NetCDF file.
!     - Use 8-byte reals. 
!
!     This version heavily modified as part of the GOcean-2D project
!     with the mantra "all computation must occur in a kernel."
!     Andrew Porter, April 2014

  USE shallow_IO
  USE timing
  USE model
  USE initial_conditions
  USE time_smooth,  ONLY: manual_invoke_time_smooth
  USE apply_bcs_cf, ONLY: manual_invoke_apply_bcs_cf
  USE apply_bcs_ct, ONLY: manual_invoke_apply_bcs_ct
  USE apply_bcs_cu, ONLY: manual_invoke_apply_bcs_cu
  USE apply_bcs_cv, ONLY: manual_invoke_apply_bcs_cv
  USE manual_invoke_apply_bcs_mod, ONLY: manual_invoke_apply_bcs_uvtf
  USE compute_cu,   ONLY: manual_invoke_compute_cu
  USE compute_cv,   ONLY: manual_invoke_compute_cv
  USE compute_z,    ONLY: manual_invoke_compute_z
  USE compute_h,    ONLY: manual_invoke_compute_h
  USE manual_invoke_compute_new_fields_mod, ONLY: manual_invoke_compute_new_fields
  IMPLICIT NONE

  !> Checksum used for each array
  REAL(KIND=8) :: csum

  !> Loop counter for time-stepping loop
  INTEGER :: ncycle
   
  !> Integer tags for timers
  INTEGER :: idxt0, idxt1

  !  ** Initialisations of model parameters (dt etc) ** 
  CALL model_init()

  ! NOTE BELOW THAT TWO DELTA T (TDT) IS SET TO DT ON THE FIRST
  ! CYCLE AFTER WHICH IT IS RESET TO DT+DT.
  ! dt and tdt are prototypical fields that are actually a 
  ! single parameter.
  CALL copy_field(dt, tdt)
 
  !     INITIAL VALUES OF THE STREAM FUNCTION AND P

  CALL init_initial_condition_params()
  CALL invoke_init_stream_fn_kernel(PSI)
  CALL init_pressure(P)

  !     INITIALIZE VELOCITIES
 
  CALL init_velocity_u(u, psi, m, n)
  CALL init_velocity_v(v, psi, m, n)

  !     PERIODIC CONTINUATION
  CALL manual_invoke_apply_bcs_cu(U)
  CALL manual_invoke_apply_bcs_cv(V)

  ! Initialise fields that will hold data at previous time step
  CALL copy_field(U, UOLD)
  CALL copy_field(V, VOLD)
  CALL copy_field(P, POLD)
     
  ! Write intial values of p, u, and v into a netCDF file   
  CALL model_write(0, p, u, v)

  !     Start timer
  CALL timer_start('Time-stepping',idxt0)

  !  ** Start of time loop ** 
  DO ncycle=1,itmax
    
    ! COMPUTE CAPITAL U, CAPITAL V, Z AND H

    CALL timer_start('Compute c{u,v},z,h', idxt1)

    CALL manual_invoke_compute_cu(CU, P, U)
    CALL manual_invoke_compute_cv(CV, P, V)
    CALL manual_invoke_compute_z(z, P, U, V)
    CALL manual_invoke_compute_h(h, P, U, V)

    CALL timer_stop(idxt1)

    ! PERIODIC CONTINUATION

    CALL timer_start('PBCs',idxt1)
    CALL manual_invoke_apply_bcs_uvtf(CU, CV, H, Z)
    CALL timer_stop(idxt1)

    ! COMPUTE NEW VALUES U,V AND P

    CALL timer_start('Compute new fields', idxt1)
    CALL manual_invoke_compute_new_fields(unew, uold, vnew, vold, &
                                          pnew, pold, &
                                          z, cu, cv, h, tdt%data)
    CALL timer_stop(idxt1)

    ! PERIODIC CONTINUATION
    CALL timer_start('PBCs',idxt1)
    CALL manual_invoke_apply_bcs_cu(UNEW)
    CALL manual_invoke_apply_bcs_cv(VNEW)
    CALL manual_invoke_apply_bcs_ct(PNEW)
    CALL timer_stop(idxt1)

    ! Time is in seconds but we never actually need it
    !CALL increment(time, dt)

    CALL model_write(ncycle, p, u, v)

    ! TIME SMOOTHING AND UPDATE FOR NEXT CYCLE
    IF(NCYCLE .GT. 1) then

      CALL timer_start('Time smoothing',idxt1)

      CALL manual_invoke_time_smooth(U, UNEW, UOLD)
      CALL manual_invoke_time_smooth(V, VNEW, VOLD)
      CALL manual_invoke_time_smooth(P, PNEW, POLD)

      CALL timer_stop(idxt1)

    ELSE ! ncycle == 1

      ! Make TDT actually = 2*DT
      CALL increment(tdt, tdt)

    ENDIF ! ncycle > 1

    CALL timer_start('Field copy',idxt1)

    CALL copy_field(UNEW, U)
    CALL copy_field(VNEW, V)
    CALL copy_field(PNEW, P)

    CALL timer_stop(idxt1)

  END DO

  !  ** End of time loop ** 

  CALL timer_stop(idxt0)

  CALL compute_checksum(pnew, csum)
  CALL model_write_log("('P CHECKSUM after ',I6,' steps = ',E15.7)", &
                       itmax, csum)

  CALL compute_checksum(unew, csum)
  CALL model_write_log("('U CHECKSUM after ',I6,' steps = ',E15.7)", &
                       itmax, csum)

  CALL compute_checksum(vnew, csum)
  CALL model_write_log("('V CHECKSUM after ',I6,' steps = ',E15.7)", &
                       itmax, csum)

  CALL model_finalise()

CONTAINS

  !===================================================

  SUBROUTINE compute_checksum(field, val)
    IMPLICIT none
    REAL(KIND=8), INTENT(in), DIMENSION(:,:) :: field
    REAL(KIND=8), INTENT(out) :: val

    val = SUM(field)

  END SUBROUTINE compute_checksum

  !===================================================

END PROGRAM shallow
