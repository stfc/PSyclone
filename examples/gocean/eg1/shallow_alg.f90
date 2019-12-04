program shallow

!> @mainpage BENCHMARK WEATHER PREDICTION PROGRAM FOR COMPARING THE
!! PREFORMANCE OF CURRENT SUPERCOMPUTERS. THE MODEL IS
!! BASED OF THE PAPER - THE DYNAMICS OF FINITE-DIFFERENCE
!! MODELS OF THE SHALLOW-WATER EQUATIONS, BY ROBERT SADOURNY
!! J. ATM. SCIENCES, VOL 32, NO 4, APRIL 1975.
!!     
!! CODE BY PAUL N. SWARZTRAUBER, NATIONAL CENTER FOR
!! ATMOSPHERIC RESEARCH, BOULDER, CO,  OCTOBER 1984.
!! Modified by Juliana Rew, NCAR, January 2006
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

  use shallow_io_mod
  use timing_mod
  use gocean_mod, only: model_write_log
  use model_mod
  use grid_mod
  use field_mod
  use initial_conditions_mod
  use time_smooth_mod,  only: time_smooth
  use apply_bcs_mod,    only: invoke_apply_bcs
  use compute_cu_mod,   only: compute_cu
  use compute_cv_mod,   only: compute_cv
  use compute_z_mod,    only: compute_z
  use compute_h_mod,    only: compute_h
  use compute_unew_mod, only: compute_unew
  use compute_vnew_mod, only: compute_vnew
  use compute_pnew_mod, only: compute_pnew
  use infrastructure_mod,only: copy
  implicit none

  type(grid_type), target :: model_grid
  !> Pressure at {current,previous,next} time step
  type(r2d_field) :: p_fld, pold_fld, pnew_fld
  !> Velocity in x direction at {current,previous,next} time step
  type(r2d_field) :: u_fld, uold_fld, unew_fld
  !> Velocity in x direction at {current,previous,next} time step
  type(r2d_field) :: v_fld, vold_fld, vnew_fld
  !> Mass flux in x and y directions
  type(r2d_field) :: cu_fld, cv_fld
  !> Potential vorticity
  type(r2d_field) :: z_fld
  !> Surface height
  type(r2d_field) :: h_fld
  !> Stream function
  type(r2d_field) :: psi_fld

  !> Loop counter for time-stepping loop
  INTEGER :: ncycle
   
  !> Integer tags for timers
  INTEGER :: idxt0, idxt1

  ! Create the model grid
  !> \todo The call to grid_type here should *not* specify the grid
  !! offset choice as that is an implementation detail. PSyclone
  !! should re-write this call to pass the offset information after it
  !! has examined the kernels to see what they are expecting.
  model_grid = grid_type(ARAKAWA_C,                           &
                         (/BC_PERIODIC,BC_PERIODIC,BC_NONE/), &
                         OFFSET_SW)

  !  ** Initialisations of model parameters (dt etc) ** 
  CALL model_init(model_grid)
 
  ! Create fields on this grid
  p_fld    = r2d_field(model_grid, T_POINTS)
  pold_fld = r2d_field(model_grid, T_POINTS)
  pnew_fld = r2d_field(model_grid, T_POINTS)

  u_fld    = r2d_field(model_grid, U_POINTS)
  uold_fld = r2d_field(model_grid, U_POINTS)
  unew_fld = r2d_field(model_grid, U_POINTS)

  v_fld    = r2d_field(model_grid, V_POINTS)
  vold_fld = r2d_field(model_grid, V_POINTS)
  vnew_fld = r2d_field(model_grid, V_POINTS)

  cu_fld = r2d_field(model_grid, U_POINTS)

  cv_fld = r2d_field(model_grid, V_POINTS)

  z_fld = r2d_field(model_grid, F_POINTS)

  h_fld = r2d_field(model_grid, T_POINTS)

  psi_fld = r2d_field(model_grid, F_POINTS)

  ! NOTE BELOW THAT TWO DELTA T (TDT) IS SET TO DT ON THE FIRST
  ! CYCLE AFTER WHICH IT IS RESET TO DT+DT.
  tdt = dt

  !     INITIAL VALUES OF THE STREAM FUNCTION AND P

  call init_initial_condition_params(p_fld)
  call invoke_init_stream_fn_kernel(psi_fld)
  call init_pressure(p_fld)

  !     INITIALIZE VELOCITIES
 
  call init_velocity_u(u_fld, psi_fld)
  call init_velocity_v(v_fld, psi_fld)

  !     PERIODIC CONTINUATION
  call invoke_apply_bcs(u_fld)
  call invoke_apply_bcs(v_fld)

  ! Generate and output checksums of initial fields
  CALL model_write_log("('psi initial CHECKSUM = ',E24.16)", &
                       field_checksum(psi_fld))
  CALL model_write_log("('P initial CHECKSUM = ',E24.16)",   &
                         field_checksum(p_fld))
  CALL model_write_log("('U initial CHECKSUM = ',E24.16)",   &
                       field_checksum(u_fld))
  CALL model_write_log("('V initial CHECKSUM = ',E24.16)",   &
                       field_checksum(v_fld))

  ! Initialise fields that will hold data at previous time step
  CALL copy_field(u_fld, uold_fld)
  CALL copy_field(v_fld, vold_fld)
  CALL copy_field(p_fld, pold_fld)
     
  ! Write intial values of p, u, and v into a netCDF file   
  call ascii_write(0, 'psifld.dat', psi_fld%data,            &
                   psi_fld%internal%nx, psi_fld%internal%ny, &
                   psi_fld%internal%xstart, psi_fld%internal%ystart)
  CALL model_write(0, p_fld, u_fld, v_fld)

  !     Start timer
  CALL timer_start('Time-stepping',idxt0)

  !  ** Start of time loop ** 
  DO ncycle=1,itmax
    
    ! COMPUTE CAPITAL U, CAPITAL V, Z AND H

    CALL timer_start('Compute c{u,v},z,h', idxt1)

    call invoke( compute_cu(CU_fld, p_fld, u_fld),      &
                 compute_cv(CV_fld, p_fld, v_fld),      &
                 compute_z(z_fld, p_fld, u_fld, v_fld), &
                 compute_h(h_fld, p_fld, u_fld, v_fld) )

    call timer_stop(idxt1)

    ! PERIODIC CONTINUATION

    call timer_start('PBCs-1',idxt1)
    ! This call could be generated automatically by PSyclone
    call invoke_apply_bcs(CU_fld)
    call invoke_apply_bcs(CV_fld)
    call invoke_apply_bcs(H_fld)
    call invoke_apply_bcs(Z_fld)
    call timer_stop(idxt1)

    ! COMPUTE NEW VALUES U,V AND P

    call timer_start('Compute new fields', idxt1)
    call invoke( compute_unew(unew_fld, uold_fld, z_fld, cv_fld, h_fld, tdt), &
                 compute_vnew(vnew_fld, vold_fld, z_fld, cu_fld, h_fld, tdt), &
                 compute_pnew(pnew_fld, pold_fld, cu_fld, cv_fld, tdt) )
    call timer_stop(idxt1)

    ! PERIODIC CONTINUATION
    call timer_start('PBCs-2',idxt1)
    ! This call could be generated by PSyclone
    call invoke_apply_bcs(UNEW_fld)
    call invoke_apply_bcs(VNEW_fld)
    call invoke_apply_bcs(PNEW_fld)
    call timer_stop(idxt1)

    ! Time is in seconds but we never actually need it
    !time = time + dt

    call model_write(ncycle, p_fld, u_fld, v_fld)

    ! TIME SMOOTHING AND UPDATE FOR NEXT CYCLE
    if(NCYCLE .GT. 1) then

      call timer_start('Time smoothing',idxt1)

      call invoke( time_smooth(u_fld, UNEW_fld, UOLD_fld), &
                   time_smooth(v_fld, VNEW_fld, VOLD_fld), &
                   time_smooth(p_fld, PNEW_fld, POLD_fld) )

      call timer_stop(idxt1)

    else ! ncycle == 1

      ! Make TDT actually = 2*DT
       tdt = tdt + dt

    endif ! ncycle > 1

    call timer_start('Field copy',idxt1)

    call invoke(                       &
                copy(u_fld, unew_fld), &
                copy(v_fld, vnew_fld), &
                copy(p_fld, pnew_fld)  &
               )
!    call copy_field(UNEW_fld, U_fld)
!    call copy_field(VNEW_fld, V_fld)
!    call copy_field(PNEW_fld, p_fld)

    call timer_stop(idxt1)

  end do

  !  ** End of time loop ** 

  call timer_stop(idxt0)

  ! Output field checksums at end of run for correctness check
  call model_write_log("('P CHECKSUM after ',I6,' steps = ',E24.16)", &
                       itmax, field_checksum(pnew_fld))
  call model_write_log("('U CHECKSUM after ',I6,' steps = ',E24.16)", &
                       itmax, field_checksum(unew_fld))
  call model_write_log("('V CHECKSUM after ',I6,' steps = ',E24.16)", &
                       itmax, field_checksum(vnew_fld))

  call model_finalise()

end program shallow
