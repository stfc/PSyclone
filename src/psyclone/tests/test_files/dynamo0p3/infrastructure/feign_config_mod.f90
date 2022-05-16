!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! Handles the feigning of namelists.
!
module feign_config_mod

  use constants_mod, only : i_def, i_medium, i_native, l_def, r_def, r_second, str_def, str_max_filename
  use log_mod,       only : log_scratch_space, log_event, LOG_LEVEL_ERROR
  use mpi_mod,       only : get_comm_rank

  implicit none

  private
  public :: &!feign_aerosol_config, &
            feign_base_mesh_config, &
            !feign_blayer_config, &
            !feign_boundaries_config, &
            !feign_checks_config, &
            !feign_cloud_config, &
            !feign_convection_config, &
            !feign_damping_layer_config, &
            !feign_departure_points_config, &
            !feign_development_config, &
            !feign_domain_size_config, &
            !feign_esm_couple_config, &
            !feign_external_forcing_config, &
            !feign_extrusion_config, &
            !feign_files_config, &
            feign_finite_element_config, &
            !feign_formulation_config, &
            !feign_helmholtz_solver_config, &
            !feign_idealised_config, &
            !feign_initial_density_config, &
            !feign_initial_pressure_config, &
            !feign_initial_temperature_config, &
            !feign_initial_vapour_config, &
            !feign_initial_wind_config, &
            !feign_initialization_config, &
            !feign_io_config, &
            !feign_jules_surface_config, &
            !feign_jules_vegetation_config, &
            !feign_logging_config, &
            !feign_microphysics_config, &
            !feign_mixed_solver_config, &
            !feign_mixing_config, &
            !feign_multigrid_config, &
            !feign_multires_coupling_config, &
            !feign_orbit_config, &
            !feign_orographic_drag_config, &
            !feign_orography_config, &
            !feign_orography_agnesi_cartesian_config, &
            !feign_orography_agnesi_spherical_config, &
            !feign_orography_bell_cartesian_config, &
            !feign_orography_dcmip200_spherical_config, &
            !feign_orography_schar_cartesian_config, &
            !feign_orography_schar_spherical_config, &
            !feign_partitioning_config, &
            !feign_physics_config, &
            feign_planet_config
            !feign_radiation_config, &
            !feign_section_choice_config, &
            !feign_solver_config, &
            !feign_spectral_gwd_config, &
            !feign_star_config, &
            !feign_stochastic_physics_config, &
            !feign_subgrid_config, &
            !feign_surface_config, &
            !feign_time_config, &
            !feign_timestepping_config, &
            !feign_transport_config, &
            !feign_well_mixed_gases_config

  integer(i_native) :: local_rank = -1
  integer(i_native), parameter :: temporary_unit = 3

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_aerosol_config( aclw_file, &
!!$                                   acsw_file, &
!!$                                   activation_scheme, &
!!$                                   anlw_file, &
!!$                                   answ_file, &
!!$                                   crlw_file, &
!!$                                   crsw_file, &
!!$                                   glomap_mode, &
!!$                                   l_radaer, &
!!$                                   prec_file, &
!!$                                   sulphuric_strat_climatology, &
!!$                                   sulphuric_strat_column )
!!$
!!$    use aerosol_config_mod, only : read_aerosol_namelist, &
!!$                                   postprocess_aerosol_namelist, &
!!$                                   key_from_activation_scheme, &
!!$                                   activation_scheme_from_key, &
!!$                                   key_from_glomap_mode, &
!!$                                   glomap_mode_from_key
!!$
!!$    implicit none
!!$
!!$    character(*), intent(in) :: aclw_file
!!$    character(*), intent(in) :: acsw_file
!!$    integer(i_native), intent(in) :: activation_scheme
!!$    character(*), intent(in) :: anlw_file
!!$    character(*), intent(in) :: answ_file
!!$    character(*), intent(in) :: crlw_file
!!$    character(*), intent(in) :: crsw_file
!!$    integer(i_native), intent(in) :: glomap_mode
!!$    logical(l_def), intent(in) :: l_radaer
!!$    character(*), intent(in) :: prec_file
!!$    logical(l_def), intent(in) :: sulphuric_strat_climatology
!!$    real(r_def), intent(in) :: sulphuric_strat_column
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_aerosol_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&aerosol")' )
!!$    write( temporary_unit, '("aclw_file = ''", A, "''")' ) aclw_file
!!$    write( temporary_unit, '("acsw_file = ''", A, "''")' ) acsw_file
!!$    write( temporary_unit, '("activation_scheme = ''", A, "''")' ) key_from_activation_scheme( activation_scheme )
!!$    write( temporary_unit, '("anlw_file = ''", A, "''")' ) anlw_file
!!$    write( temporary_unit, '("answ_file = ''", A, "''")' ) answ_file
!!$    write( temporary_unit, '("crlw_file = ''", A, "''")' ) crlw_file
!!$    write( temporary_unit, '("crsw_file = ''", A, "''")' ) crsw_file
!!$    write( temporary_unit, '("glomap_mode = ''", A, "''")' ) key_from_glomap_mode( glomap_mode )
!!$    write( temporary_unit, '("l_radaer = ", L2)' ) l_radaer
!!$    write( temporary_unit, '("prec_file = ''", A, "''")' ) prec_file
!!$    write( temporary_unit, '("sulphuric_strat_climatology = ", L2)' ) sulphuric_strat_climatology
!!$    write( temporary_unit, '("sulphuric_strat_column = ", E14.7)' ) sulphuric_strat_column
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_aerosol_namelist( temporary_unit, local_rank )
!!$    call postprocess_aerosol_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_aerosol_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_aerosol_config

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine feign_base_mesh_config( f_lat_deg, &
                                     filename, &
                                     fplane, &
                                     geometry, &
                                     offline_partitioning, &
                                     prime_mesh_name, &
                                     topology )

    use base_mesh_config_mod, only : read_base_mesh_namelist, &
                                     postprocess_base_mesh_namelist, &
                                     key_from_geometry, &
                                     geometry_from_key, &
                                     key_from_topology, &
                                     topology_from_key

    implicit none

    real(r_def), intent(in) :: f_lat_deg
    character(*), intent(in) :: filename
    logical(l_def), intent(in) :: fplane
    integer(i_native), intent(in) :: geometry
    logical(l_def), intent(in) :: offline_partitioning
    character(*), intent(in) :: prime_mesh_name
    integer(i_native), intent(in) :: topology

    integer(i_native)  :: condition
    integer(i_native)  :: i
    character(str_max_filename) :: tmp_str
    character(str_def) :: fmt_str

    if (local_rank == -1) then
      local_rank = get_comm_rank()
    end if

    open( temporary_unit, status='scratch', action='readwrite', &
          iostat=condition )

    if (condition /= 0) then
      write( 6, '("feign_base_mesh_config: ", I0)' ) condition
      stop
    end if

    write( temporary_unit, '("&base_mesh")' )
    write( temporary_unit, '("f_lat_deg = ", E14.7)' ) f_lat_deg
    write( temporary_unit, '("filename = ''", A, "''")' ) filename
    write( temporary_unit, '("fplane = ", L2)' ) fplane
    write( temporary_unit, '("geometry = ''", A, "''")' ) key_from_geometry( geometry )
    write( temporary_unit, '("offline_partitioning = ", L2)' ) offline_partitioning
    write( temporary_unit, '("prime_mesh_name = ''", A, "''")' ) prime_mesh_name
    write( temporary_unit, '("topology = ''", A, "''")' ) key_from_topology( topology )
    write( temporary_unit, '("/")' )

    rewind(temporary_unit)
    call read_base_mesh_namelist( temporary_unit, local_rank )
    call postprocess_base_mesh_namelist()
    close(temporary_unit, iostat=condition )
    if (condition /= 0) stop &
        'feign_base_mesh_config: '// &
        'Unable to close temporary file'

  end subroutine feign_base_mesh_config

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_blayer_config( a_ent_shr, &
!!$                                  bl_levels, &
!!$                                  bl_mix_w, &
!!$                                  cbl_opt, &
!!$                                  dyn_diag, &
!!$                                  fixed_flux_e, &
!!$                                  fixed_flux_h, &
!!$                                  flux_bc_opt, &
!!$                                  free_atm_mix, &
!!$                                  fric_heating, &
!!$                                  interp_local, &
!!$                                  new_kcloudtop, &
!!$                                  noice_in_turb, &
!!$                                  p_unstable, &
!!$                                  reduce_fa_mix, &
!!$                                  relax_sc_over_cu, &
!!$                                  sbl_opt, &
!!$                                  sg_orog_mixing, &
!!$                                  zhloc_depth_fac )
!!$
!!$    use blayer_config_mod, only : read_blayer_namelist, &
!!$                                  postprocess_blayer_namelist, &
!!$                                  key_from_cbl_opt, &
!!$                                  cbl_opt_from_key, &
!!$                                  key_from_dyn_diag, &
!!$                                  dyn_diag_from_key, &
!!$                                  key_from_flux_bc_opt, &
!!$                                  flux_bc_opt_from_key, &
!!$                                  key_from_free_atm_mix, &
!!$                                  free_atm_mix_from_key, &
!!$                                  key_from_interp_local, &
!!$                                  interp_local_from_key, &
!!$                                  key_from_reduce_fa_mix, &
!!$                                  reduce_fa_mix_from_key, &
!!$                                  key_from_sbl_opt, &
!!$                                  sbl_opt_from_key, &
!!$                                  key_from_sg_orog_mixing, &
!!$                                  sg_orog_mixing_from_key
!!$
!!$    implicit none
!!$
!!$    real(r_def), intent(in) :: a_ent_shr
!!$    integer(i_def), intent(in) :: bl_levels
!!$    logical(l_def), intent(in) :: bl_mix_w
!!$    integer(i_native), intent(in) :: cbl_opt
!!$    integer(i_native), intent(in) :: dyn_diag
!!$    real(r_def), intent(in) :: fixed_flux_e
!!$    real(r_def), intent(in) :: fixed_flux_h
!!$    integer(i_native), intent(in) :: flux_bc_opt
!!$    integer(i_native), intent(in) :: free_atm_mix
!!$    logical(l_def), intent(in) :: fric_heating
!!$    integer(i_native), intent(in) :: interp_local
!!$    logical(l_def), intent(in) :: new_kcloudtop
!!$    logical(l_def), intent(in) :: noice_in_turb
!!$    real(r_def), intent(in) :: p_unstable
!!$    integer(i_native), intent(in) :: reduce_fa_mix
!!$    logical(l_def), intent(in) :: relax_sc_over_cu
!!$    integer(i_native), intent(in) :: sbl_opt
!!$    integer(i_native), intent(in) :: sg_orog_mixing
!!$    real(r_def), intent(in) :: zhloc_depth_fac
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_blayer_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&blayer")' )
!!$    write( temporary_unit, '("a_ent_shr = ", E14.7)' ) a_ent_shr
!!$    write( temporary_unit, '("bl_levels = ", I0)' ) bl_levels
!!$    write( temporary_unit, '("bl_mix_w = ", L2)' ) bl_mix_w
!!$    write( temporary_unit, '("cbl_opt = ''", A, "''")' ) key_from_cbl_opt( cbl_opt )
!!$    write( temporary_unit, '("dyn_diag = ''", A, "''")' ) key_from_dyn_diag( dyn_diag )
!!$    write( temporary_unit, '("fixed_flux_e = ", E14.7)' ) fixed_flux_e
!!$    write( temporary_unit, '("fixed_flux_h = ", E14.7)' ) fixed_flux_h
!!$    write( temporary_unit, '("flux_bc_opt = ''", A, "''")' ) key_from_flux_bc_opt( flux_bc_opt )
!!$    write( temporary_unit, '("free_atm_mix = ''", A, "''")' ) key_from_free_atm_mix( free_atm_mix )
!!$    write( temporary_unit, '("fric_heating = ", L2)' ) fric_heating
!!$    write( temporary_unit, '("interp_local = ''", A, "''")' ) key_from_interp_local( interp_local )
!!$    write( temporary_unit, '("new_kcloudtop = ", L2)' ) new_kcloudtop
!!$    write( temporary_unit, '("noice_in_turb = ", L2)' ) noice_in_turb
!!$    write( temporary_unit, '("p_unstable = ", E14.7)' ) p_unstable
!!$    write( temporary_unit, '("reduce_fa_mix = ''", A, "''")' ) key_from_reduce_fa_mix( reduce_fa_mix )
!!$    write( temporary_unit, '("relax_sc_over_cu = ", L2)' ) relax_sc_over_cu
!!$    write( temporary_unit, '("sbl_opt = ''", A, "''")' ) key_from_sbl_opt( sbl_opt )
!!$    write( temporary_unit, '("sg_orog_mixing = ''", A, "''")' ) key_from_sg_orog_mixing( sg_orog_mixing )
!!$    write( temporary_unit, '("zhloc_depth_fac = ", E14.7)' ) zhloc_depth_fac
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_blayer_namelist( temporary_unit, local_rank )
!!$    call postprocess_blayer_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_blayer_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_blayer_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_boundaries_config( blend_frequency, &
!!$                                      blending_weights, &
!!$                                      boundary_e, &
!!$                                      boundary_n, &
!!$                                      boundary_s, &
!!$                                      boundary_w, &
!!$                                      edge_cells_ew, &
!!$                                      edge_cells_ns, &
!!$                                      inner_width_ew, &
!!$                                      inner_width_ns, &
!!$                                      lbc_method, &
!!$                                      limited_area, &
!!$                                      normal_only, &
!!$                                      outer_width_ew, &
!!$                                      outer_width_ns, &
!!$                                      output_lbcs, &
!!$                                      rim_width_ew, &
!!$                                      rim_width_ns, &
!!$                                      solver_boundary_depth, &
!!$                                      transport_boundary_depth )
!!$
!!$    use boundaries_config_mod, only : read_boundaries_namelist, &
!!$                                      postprocess_boundaries_namelist, &
!!$                                      key_from_blend_frequency, &
!!$                                      blend_frequency_from_key, &
!!$                                      key_from_lbc_method, &
!!$                                      lbc_method_from_key
!!$
!!$    implicit none
!!$
!!$    integer(i_native), intent(in) :: blend_frequency
!!$    real(r_def), intent(in) :: blending_weights(:)
!!$    integer(i_def), intent(in) :: boundary_e
!!$    integer(i_def), intent(in) :: boundary_n
!!$    integer(i_def), intent(in) :: boundary_s
!!$    integer(i_def), intent(in) :: boundary_w
!!$    integer(i_def), intent(in) :: edge_cells_ew
!!$    integer(i_def), intent(in) :: edge_cells_ns
!!$    integer(i_def), intent(in) :: inner_width_ew
!!$    integer(i_def), intent(in) :: inner_width_ns
!!$    integer(i_native), intent(in) :: lbc_method
!!$    logical(l_def), intent(in) :: limited_area
!!$    logical(l_def), intent(in) :: normal_only
!!$    integer(i_def), intent(in) :: outer_width_ew
!!$    integer(i_def), intent(in) :: outer_width_ns
!!$    logical(l_def), intent(in) :: output_lbcs
!!$    integer(i_def), intent(in) :: rim_width_ew
!!$    integer(i_def), intent(in) :: rim_width_ns
!!$    integer(i_def), intent(in) :: solver_boundary_depth
!!$    integer(i_def), intent(in) :: transport_boundary_depth
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_boundaries_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&boundaries")' )
!!$    write( temporary_unit, '("blend_frequency = ''", A, "''")' ) key_from_blend_frequency( blend_frequency )
!!$    if (size(blending_weights) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(blending_weights)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'blending_weights = ', blending_weights
!!$    else
!!$      write( temporary_unit, '("blending_weights = ", E14.7)' ) blending_weights
!!$    end if
!!$    write( temporary_unit, '("boundary_e = ", I0)' ) boundary_e
!!$    write( temporary_unit, '("boundary_n = ", I0)' ) boundary_n
!!$    write( temporary_unit, '("boundary_s = ", I0)' ) boundary_s
!!$    write( temporary_unit, '("boundary_w = ", I0)' ) boundary_w
!!$    write( temporary_unit, '("edge_cells_ew = ", I0)' ) edge_cells_ew
!!$    write( temporary_unit, '("edge_cells_ns = ", I0)' ) edge_cells_ns
!!$    write( temporary_unit, '("inner_width_ew = ", I0)' ) inner_width_ew
!!$    write( temporary_unit, '("inner_width_ns = ", I0)' ) inner_width_ns
!!$    write( temporary_unit, '("lbc_method = ''", A, "''")' ) key_from_lbc_method( lbc_method )
!!$    write( temporary_unit, '("limited_area = ", L2)' ) limited_area
!!$    write( temporary_unit, '("normal_only = ", L2)' ) normal_only
!!$    write( temporary_unit, '("outer_width_ew = ", I0)' ) outer_width_ew
!!$    write( temporary_unit, '("outer_width_ns = ", I0)' ) outer_width_ns
!!$    write( temporary_unit, '("output_lbcs = ", L2)' ) output_lbcs
!!$    write( temporary_unit, '("rim_width_ew = ", I0)' ) rim_width_ew
!!$    write( temporary_unit, '("rim_width_ns = ", I0)' ) rim_width_ns
!!$    write( temporary_unit, '("solver_boundary_depth = ", I0)' ) solver_boundary_depth
!!$    write( temporary_unit, '("transport_boundary_depth = ", I0)' ) transport_boundary_depth
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_boundaries_namelist( temporary_unit, local_rank )
!!$    call postprocess_boundaries_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_boundaries_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_boundaries_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_checks_config( limit_cfl, &
!!$                                  max_cfl )
!!$
!!$    use checks_config_mod, only : read_checks_namelist, &
!!$                                  postprocess_checks_namelist
!!$
!!$    implicit none
!!$
!!$    logical(l_def), intent(in) :: limit_cfl
!!$    real(r_def), intent(in) :: max_cfl
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_checks_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&checks")' )
!!$    write( temporary_unit, '("limit_cfl = ", L2)' ) limit_cfl
!!$    write( temporary_unit, '("max_cfl = ", E14.7)' ) max_cfl
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_checks_namelist( temporary_unit, local_rank )
!!$    call postprocess_checks_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_checks_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_checks_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_cloud_config( cff_spread_rate, &
!!$                                 cld_fsd_hill, &
!!$                                 cloud_horizontal_fsd, &
!!$                                 ez_max, &
!!$                                 ez_subcrit, &
!!$                                 falliceshear_method, &
!!$                                 filter_optical_depth, &
!!$                                 ice_width, &
!!$                                 mphys_erosion, &
!!$                                 opt_depth_thresh, &
!!$                                 pc2ini, &
!!$                                 rh_crit, &
!!$                                 rh_crit_opt, &
!!$                                 scheme, &
!!$                                 subgrid_qv, &
!!$                                 use_fsd_eff_res )
!!$
!!$    use cloud_config_mod, only : read_cloud_namelist, &
!!$                                 postprocess_cloud_namelist, &
!!$                                 key_from_falliceshear_method, &
!!$                                 falliceshear_method_from_key, &
!!$                                 key_from_pc2ini, &
!!$                                 pc2ini_from_key, &
!!$                                 key_from_rh_crit_opt, &
!!$                                 rh_crit_opt_from_key, &
!!$                                 key_from_scheme, &
!!$                                 scheme_from_key
!!$
!!$    implicit none
!!$
!!$    real(r_def), intent(in) :: cff_spread_rate
!!$    logical(l_def), intent(in) :: cld_fsd_hill
!!$    real(r_def), intent(in) :: cloud_horizontal_fsd
!!$    real(r_def), intent(in) :: ez_max
!!$    logical(l_def), intent(in) :: ez_subcrit
!!$    integer(i_native), intent(in) :: falliceshear_method
!!$    logical(l_def), intent(in) :: filter_optical_depth
!!$    real(r_def), intent(in) :: ice_width
!!$    logical(l_def), intent(in) :: mphys_erosion
!!$    real(r_def), intent(in) :: opt_depth_thresh
!!$    integer(i_native), intent(in) :: pc2ini
!!$    real(r_def), intent(in) :: rh_crit(:)
!!$    integer(i_native), intent(in) :: rh_crit_opt
!!$    integer(i_native), intent(in) :: scheme
!!$    logical(l_def), intent(in) :: subgrid_qv
!!$    logical(l_def), intent(in) :: use_fsd_eff_res
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_cloud_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&cloud")' )
!!$    write( temporary_unit, '("cff_spread_rate = ", E14.7)' ) cff_spread_rate
!!$    write( temporary_unit, '("cld_fsd_hill = ", L2)' ) cld_fsd_hill
!!$    write( temporary_unit, '("cloud_horizontal_fsd = ", E14.7)' ) cloud_horizontal_fsd
!!$    write( temporary_unit, '("ez_max = ", E14.7)' ) ez_max
!!$    write( temporary_unit, '("ez_subcrit = ", L2)' ) ez_subcrit
!!$    write( temporary_unit, '("falliceshear_method = ''", A, "''")' ) key_from_falliceshear_method( falliceshear_method )
!!$    write( temporary_unit, '("filter_optical_depth = ", L2)' ) filter_optical_depth
!!$    write( temporary_unit, '("ice_width = ", E14.7)' ) ice_width
!!$    write( temporary_unit, '("mphys_erosion = ", L2)' ) mphys_erosion
!!$    write( temporary_unit, '("opt_depth_thresh = ", E14.7)' ) opt_depth_thresh
!!$    write( temporary_unit, '("pc2ini = ''", A, "''")' ) key_from_pc2ini( pc2ini )
!!$    if (size(rh_crit) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(rh_crit)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'rh_crit = ', rh_crit
!!$    else
!!$      write( temporary_unit, '("rh_crit = ", E14.7)' ) rh_crit
!!$    end if
!!$    write( temporary_unit, '("rh_crit_opt = ''", A, "''")' ) key_from_rh_crit_opt( rh_crit_opt )
!!$    write( temporary_unit, '("scheme = ''", A, "''")' ) key_from_scheme( scheme )
!!$    write( temporary_unit, '("subgrid_qv = ", L2)' ) subgrid_qv
!!$    write( temporary_unit, '("use_fsd_eff_res = ", L2)' ) use_fsd_eff_res
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_cloud_namelist( temporary_unit, local_rank )
!!$    call postprocess_cloud_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_cloud_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_cloud_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_convection_config( cv_scheme, &
!!$                                      number_of_convection_substeps, &
!!$                                      use_jules_flux )
!!$
!!$    use convection_config_mod, only : read_convection_namelist, &
!!$                                      postprocess_convection_namelist, &
!!$                                      key_from_cv_scheme, &
!!$                                      cv_scheme_from_key
!!$
!!$    implicit none
!!$
!!$    integer(i_native), intent(in) :: cv_scheme
!!$    integer(i_def), intent(in) :: number_of_convection_substeps
!!$    logical(l_def), intent(in) :: use_jules_flux
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_convection_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&convection")' )
!!$    write( temporary_unit, '("cv_scheme = ''", A, "''")' ) key_from_cv_scheme( cv_scheme )
!!$    write( temporary_unit, '("number_of_convection_substeps = ", I0)' ) number_of_convection_substeps
!!$    write( temporary_unit, '("use_jules_flux = ", L2)' ) use_jules_flux
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_convection_namelist( temporary_unit, local_rank )
!!$    call postprocess_convection_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_convection_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_convection_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_damping_layer_config( dl_base, &
!!$                                         dl_str, &
!!$                                         dl_type )
!!$
!!$    use damping_layer_config_mod, only : read_damping_layer_namelist, &
!!$                                         postprocess_damping_layer_namelist, &
!!$                                         key_from_dl_type, &
!!$                                         dl_type_from_key
!!$
!!$    implicit none
!!$
!!$    real(r_def), intent(in) :: dl_base
!!$    real(r_def), intent(in) :: dl_str
!!$    integer(i_native), intent(in) :: dl_type
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_damping_layer_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&damping_layer")' )
!!$    write( temporary_unit, '("dl_base = ", E14.7)' ) dl_base
!!$    write( temporary_unit, '("dl_str = ", E14.7)' ) dl_str
!!$    write( temporary_unit, '("dl_type = ''", A, "''")' ) key_from_dl_type( dl_type )
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_damping_layer_namelist( temporary_unit, local_rank )
!!$    call postprocess_damping_layer_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_damping_layer_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_damping_layer_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_departure_points_config( method, &
!!$                                            n_dep_pt_iterations, &
!!$                                            vertical_method )
!!$
!!$    use departure_points_config_mod, only : read_departure_points_namelist, &
!!$                                            postprocess_departure_points_namelist, &
!!$                                            key_from_method, &
!!$                                            method_from_key, &
!!$                                            key_from_vertical_method, &
!!$                                            vertical_method_from_key
!!$
!!$    implicit none
!!$
!!$    integer(i_native), intent(in) :: method
!!$    integer(i_def), intent(in) :: n_dep_pt_iterations
!!$    integer(i_native), intent(in) :: vertical_method
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_departure_points_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&departure_points")' )
!!$    write( temporary_unit, '("method = ''", A, "''")' ) key_from_method( method )
!!$    write( temporary_unit, '("n_dep_pt_iterations = ", I0)' ) n_dep_pt_iterations
!!$    write( temporary_unit, '("vertical_method = ''", A, "''")' ) key_from_vertical_method( vertical_method )
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_departure_points_namelist( temporary_unit, local_rank )
!!$    call postprocess_departure_points_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_departure_points_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_departure_points_config

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_development_config( example )
!!$
!!$    use development_config_mod, only : read_development_namelist, &
!!$                                       postprocess_development_namelist
!!$
!!$    implicit none
!!$
!!$    logical(l_def), intent(in) :: example
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_development_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&development")' )
!!$    write( temporary_unit, '("example = ", L2)' ) example
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_development_namelist( temporary_unit, local_rank )
!!$    call postprocess_development_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_development_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_development_config

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_domain_size_config( planar_domain_max_x, &
!!$                                       planar_domain_max_y, &
!!$                                       planar_domain_min_x, &
!!$                                       planar_domain_min_y )
!!$
!!$    use domain_size_config_mod, only : read_domain_size_namelist, &
!!$                                       postprocess_domain_size_namelist
!!$
!!$    implicit none
!!$
!!$    real(r_def), intent(in) :: planar_domain_max_x
!!$    real(r_def), intent(in) :: planar_domain_max_y
!!$    real(r_def), intent(in) :: planar_domain_min_x
!!$    real(r_def), intent(in) :: planar_domain_min_y
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_domain_size_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&domain_size")' )
!!$    write( temporary_unit, '("planar_domain_max_x = ", E14.7)' ) planar_domain_max_x
!!$    write( temporary_unit, '("planar_domain_max_y = ", E14.7)' ) planar_domain_max_y
!!$    write( temporary_unit, '("planar_domain_min_x = ", E14.7)' ) planar_domain_min_x
!!$    write( temporary_unit, '("planar_domain_min_y = ", E14.7)' ) planar_domain_min_y
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_domain_size_namelist( temporary_unit, local_rank )
!!$    call postprocess_domain_size_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_domain_size_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_domain_size_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_esm_couple_config( l_esm_couple_test )
!!$
!!$    use esm_couple_config_mod, only : read_esm_couple_namelist, &
!!$                                      postprocess_esm_couple_namelist
!!$
!!$    implicit none
!!$
!!$    logical(l_def), intent(in) :: l_esm_couple_test
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_esm_couple_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&esm_couple")' )
!!$    write( temporary_unit, '("l_esm_couple_test = ", L2)' ) l_esm_couple_test
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_esm_couple_namelist( temporary_unit, local_rank )
!!$    call postprocess_esm_couple_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_esm_couple_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_esm_couple_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_external_forcing_config( hs_random, &
!!$                                            temp_tend_coord, &
!!$                                            temp_tend_data, &
!!$                                            temp_tend_heights, &
!!$                                            temp_tend_size, &
!!$                                            theta_forcing, &
!!$                                            wind_forcing )
!!$
!!$    use external_forcing_config_mod, only : read_external_forcing_namelist, &
!!$                                            postprocess_external_forcing_namelist, &
!!$                                            key_from_temp_tend_coord, &
!!$                                            temp_tend_coord_from_key, &
!!$                                            key_from_theta_forcing, &
!!$                                            theta_forcing_from_key, &
!!$                                            key_from_wind_forcing, &
!!$                                            wind_forcing_from_key
!!$
!!$    implicit none
!!$
!!$    logical(l_def), intent(in) :: hs_random
!!$    integer(i_native), intent(in) :: temp_tend_coord
!!$    real(r_def), intent(in) :: temp_tend_data(:)
!!$    real(r_def), intent(in) :: temp_tend_heights(:)
!!$    integer(i_def), intent(in) :: temp_tend_size
!!$    integer(i_native), intent(in) :: theta_forcing
!!$    integer(i_native), intent(in) :: wind_forcing
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_external_forcing_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&external_forcing")' )
!!$    write( temporary_unit, '("hs_random = ", L2)' ) hs_random
!!$    write( temporary_unit, '("temp_tend_coord = ''", A, "''")' ) key_from_temp_tend_coord( temp_tend_coord )
!!$    if (size(temp_tend_data) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(temp_tend_data)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'temp_tend_data = ', temp_tend_data
!!$    else
!!$      write( temporary_unit, '("temp_tend_data = ", E14.7)' ) temp_tend_data
!!$    end if
!!$    if (size(temp_tend_heights) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(temp_tend_heights)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'temp_tend_heights = ', temp_tend_heights
!!$    else
!!$      write( temporary_unit, '("temp_tend_heights = ", E14.7)' ) temp_tend_heights
!!$    end if
!!$    write( temporary_unit, '("temp_tend_size = ", I0)' ) temp_tend_size
!!$    write( temporary_unit, '("theta_forcing = ''", A, "''")' ) key_from_theta_forcing( theta_forcing )
!!$    write( temporary_unit, '("wind_forcing = ''", A, "''")' ) key_from_wind_forcing( wind_forcing )
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_external_forcing_namelist( temporary_unit, local_rank )
!!$    call postprocess_external_forcing_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_external_forcing_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_external_forcing_config

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_extrusion_config( domain_top, &
!!$                                     method, &
!!$                                     number_of_layers, &
!!$                                     stretching_height, &
!!$                                     stretching_method )
!!$
!!$    use extrusion_config_mod, only : read_extrusion_namelist, &
!!$                                     postprocess_extrusion_namelist, &
!!$                                     key_from_method, &
!!$                                     method_from_key, &
!!$                                     key_from_stretching_method, &
!!$                                     stretching_method_from_key
!!$
!!$    implicit none
!!$
!!$    real(r_def), intent(in) :: domain_top
!!$    integer(i_native), intent(in) :: method
!!$    integer(i_def), intent(in) :: number_of_layers
!!$    real(r_def), intent(in) :: stretching_height
!!$    integer(i_native), intent(in) :: stretching_method
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_extrusion_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&extrusion")' )
!!$    write( temporary_unit, '("domain_top = ", E14.7)' ) domain_top
!!$    write( temporary_unit, '("method = ''", A, "''")' ) key_from_method( method )
!!$    write( temporary_unit, '("number_of_layers = ", I0)' ) number_of_layers
!!$    write( temporary_unit, '("stretching_height = ", E14.7)' ) stretching_height
!!$    write( temporary_unit, '("stretching_method = ''", A, "''")' ) key_from_stretching_method( stretching_method )
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_extrusion_namelist( temporary_unit, local_rank )
!!$    call postprocess_extrusion_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_extrusion_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_extrusion_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_files_config( aerosols_ancil_path, &
!!$                                 albedo_nir_ancil_path, &
!!$                                 albedo_vis_ancil_path, &
!!$                                 ancil_directory, &
!!$                                 checkpoint_stem_name, &
!!$                                 diag_stem_name, &
!!$                                 dms_conc_ocean_ancil_path, &
!!$                                 emiss_bc_biofuel_ancil_path, &
!!$                                 emiss_bc_biomass_ancil_path, &
!!$                                 emiss_bc_fossil_ancil_path, &
!!$                                 emiss_dms_land_ancil_path, &
!!$                                 emiss_monoterp_ancil_path, &
!!$                                 emiss_om_biofuel_ancil_path, &
!!$                                 emiss_om_biomass_ancil_path, &
!!$                                 emiss_om_fossil_ancil_path, &
!!$                                 emiss_so2_high_ancil_path, &
!!$                                 emiss_so2_low_ancil_path, &
!!$                                 emiss_so2_nat_ancil_path, &
!!$                                 h2o2_limit_ancil_path, &
!!$                                 ho2_ancil_path, &
!!$                                 hydtop_ancil_path, &
!!$                                 land_area_ancil_path, &
!!$                                 lbc_directory, &
!!$                                 lbc_filename, &
!!$                                 ls_directory, &
!!$                                 ls_filename, &
!!$                                 no3_ancil_path, &
!!$                                 o3_ancil_path, &
!!$                                 oh_ancil_path, &
!!$                                 orography_ancil_path, &
!!$                                 ozone_ancil_path, &
!!$                                 plant_func_ancil_path, &
!!$                                 sea_ancil_path, &
!!$                                 sea_ice_ancil_path, &
!!$                                 soil_ancil_path, &
!!$                                 soil_dust_ancil_path, &
!!$                                 sst_ancil_path, &
!!$                                 start_dump_directory, &
!!$                                 start_dump_filename, &
!!$                                 surface_frac_ancil_path )
!!$
!!$    use files_config_mod, only : read_files_namelist, &
!!$                                 postprocess_files_namelist
!!$
!!$    implicit none
!!$
!!$    character(*), intent(in) :: aerosols_ancil_path
!!$    character(*), intent(in) :: albedo_nir_ancil_path
!!$    character(*), intent(in) :: albedo_vis_ancil_path
!!$    character(*), intent(in) :: ancil_directory
!!$    character(*), intent(in) :: checkpoint_stem_name
!!$    character(*), intent(in) :: diag_stem_name
!!$    character(*), intent(in) :: dms_conc_ocean_ancil_path
!!$    character(*), intent(in) :: emiss_bc_biofuel_ancil_path
!!$    character(*), intent(in) :: emiss_bc_biomass_ancil_path
!!$    character(*), intent(in) :: emiss_bc_fossil_ancil_path
!!$    character(*), intent(in) :: emiss_dms_land_ancil_path
!!$    character(*), intent(in) :: emiss_monoterp_ancil_path
!!$    character(*), intent(in) :: emiss_om_biofuel_ancil_path
!!$    character(*), intent(in) :: emiss_om_biomass_ancil_path
!!$    character(*), intent(in) :: emiss_om_fossil_ancil_path
!!$    character(*), intent(in) :: emiss_so2_high_ancil_path
!!$    character(*), intent(in) :: emiss_so2_low_ancil_path
!!$    character(*), intent(in) :: emiss_so2_nat_ancil_path
!!$    character(*), intent(in) :: h2o2_limit_ancil_path
!!$    character(*), intent(in) :: ho2_ancil_path
!!$    character(*), intent(in) :: hydtop_ancil_path
!!$    character(*), intent(in) :: land_area_ancil_path
!!$    character(*), intent(in) :: lbc_directory
!!$    character(*), intent(in) :: lbc_filename
!!$    character(*), intent(in) :: ls_directory
!!$    character(*), intent(in) :: ls_filename
!!$    character(*), intent(in) :: no3_ancil_path
!!$    character(*), intent(in) :: o3_ancil_path
!!$    character(*), intent(in) :: oh_ancil_path
!!$    character(*), intent(in) :: orography_ancil_path
!!$    character(*), intent(in) :: ozone_ancil_path
!!$    character(*), intent(in) :: plant_func_ancil_path
!!$    character(*), intent(in) :: sea_ancil_path
!!$    character(*), intent(in) :: sea_ice_ancil_path
!!$    character(*), intent(in) :: soil_ancil_path
!!$    character(*), intent(in) :: soil_dust_ancil_path
!!$    character(*), intent(in) :: sst_ancil_path
!!$    character(*), intent(in) :: start_dump_directory
!!$    character(*), intent(in) :: start_dump_filename
!!$    character(*), intent(in) :: surface_frac_ancil_path
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_files_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&files")' )
!!$    write( temporary_unit, '("aerosols_ancil_path = ''", A, "''")' ) aerosols_ancil_path
!!$    write( temporary_unit, '("albedo_nir_ancil_path = ''", A, "''")' ) albedo_nir_ancil_path
!!$    write( temporary_unit, '("albedo_vis_ancil_path = ''", A, "''")' ) albedo_vis_ancil_path
!!$    write( temporary_unit, '("ancil_directory = ''", A, "''")' ) ancil_directory
!!$    write( temporary_unit, '("checkpoint_stem_name = ''", A, "''")' ) checkpoint_stem_name
!!$    write( temporary_unit, '("diag_stem_name = ''", A, "''")' ) diag_stem_name
!!$    write( temporary_unit, '("dms_conc_ocean_ancil_path = ''", A, "''")' ) dms_conc_ocean_ancil_path
!!$    write( temporary_unit, '("emiss_bc_biofuel_ancil_path = ''", A, "''")' ) emiss_bc_biofuel_ancil_path
!!$    write( temporary_unit, '("emiss_bc_biomass_ancil_path = ''", A, "''")' ) emiss_bc_biomass_ancil_path
!!$    write( temporary_unit, '("emiss_bc_fossil_ancil_path = ''", A, "''")' ) emiss_bc_fossil_ancil_path
!!$    write( temporary_unit, '("emiss_dms_land_ancil_path = ''", A, "''")' ) emiss_dms_land_ancil_path
!!$    write( temporary_unit, '("emiss_monoterp_ancil_path = ''", A, "''")' ) emiss_monoterp_ancil_path
!!$    write( temporary_unit, '("emiss_om_biofuel_ancil_path = ''", A, "''")' ) emiss_om_biofuel_ancil_path
!!$    write( temporary_unit, '("emiss_om_biomass_ancil_path = ''", A, "''")' ) emiss_om_biomass_ancil_path
!!$    write( temporary_unit, '("emiss_om_fossil_ancil_path = ''", A, "''")' ) emiss_om_fossil_ancil_path
!!$    write( temporary_unit, '("emiss_so2_high_ancil_path = ''", A, "''")' ) emiss_so2_high_ancil_path
!!$    write( temporary_unit, '("emiss_so2_low_ancil_path = ''", A, "''")' ) emiss_so2_low_ancil_path
!!$    write( temporary_unit, '("emiss_so2_nat_ancil_path = ''", A, "''")' ) emiss_so2_nat_ancil_path
!!$    write( temporary_unit, '("h2o2_limit_ancil_path = ''", A, "''")' ) h2o2_limit_ancil_path
!!$    write( temporary_unit, '("ho2_ancil_path = ''", A, "''")' ) ho2_ancil_path
!!$    write( temporary_unit, '("hydtop_ancil_path = ''", A, "''")' ) hydtop_ancil_path
!!$    write( temporary_unit, '("land_area_ancil_path = ''", A, "''")' ) land_area_ancil_path
!!$    write( temporary_unit, '("lbc_directory = ''", A, "''")' ) lbc_directory
!!$    write( temporary_unit, '("lbc_filename = ''", A, "''")' ) lbc_filename
!!$    write( temporary_unit, '("ls_directory = ''", A, "''")' ) ls_directory
!!$    write( temporary_unit, '("ls_filename = ''", A, "''")' ) ls_filename
!!$    write( temporary_unit, '("no3_ancil_path = ''", A, "''")' ) no3_ancil_path
!!$    write( temporary_unit, '("o3_ancil_path = ''", A, "''")' ) o3_ancil_path
!!$    write( temporary_unit, '("oh_ancil_path = ''", A, "''")' ) oh_ancil_path
!!$    write( temporary_unit, '("orography_ancil_path = ''", A, "''")' ) orography_ancil_path
!!$    write( temporary_unit, '("ozone_ancil_path = ''", A, "''")' ) ozone_ancil_path
!!$    write( temporary_unit, '("plant_func_ancil_path = ''", A, "''")' ) plant_func_ancil_path
!!$    write( temporary_unit, '("sea_ancil_path = ''", A, "''")' ) sea_ancil_path
!!$    write( temporary_unit, '("sea_ice_ancil_path = ''", A, "''")' ) sea_ice_ancil_path
!!$    write( temporary_unit, '("soil_ancil_path = ''", A, "''")' ) soil_ancil_path
!!$    write( temporary_unit, '("soil_dust_ancil_path = ''", A, "''")' ) soil_dust_ancil_path
!!$    write( temporary_unit, '("sst_ancil_path = ''", A, "''")' ) sst_ancil_path
!!$    write( temporary_unit, '("start_dump_directory = ''", A, "''")' ) start_dump_directory
!!$    write( temporary_unit, '("start_dump_filename = ''", A, "''")' ) start_dump_filename
!!$    write( temporary_unit, '("surface_frac_ancil_path = ''", A, "''")' ) surface_frac_ancil_path
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_files_namelist( temporary_unit, local_rank )
!!$    call postprocess_files_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_files_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_files_config

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine feign_finite_element_config( cellshape, &
                                          coord_order, &
                                          coord_system, &
                                          element_order, &
                                          rehabilitate, &
                                          vorticity_in_w1 )

    use finite_element_config_mod, only : read_finite_element_namelist, &
                                          postprocess_finite_element_namelist, &
                                          key_from_cellshape, &
                                          cellshape_from_key, &
                                          key_from_coord_system, &
                                          coord_system_from_key

    implicit none

    integer(i_native), intent(in) :: cellshape
    integer(i_def), intent(in) :: coord_order
    integer(i_native), intent(in) :: coord_system
    integer(i_def), intent(in) :: element_order
    logical(l_def), intent(in) :: rehabilitate
    logical(l_def), intent(in) :: vorticity_in_w1

    integer(i_native)  :: condition
    integer(i_native)  :: i
    character(str_max_filename) :: tmp_str
    character(str_def) :: fmt_str

    if (local_rank == -1) then
      local_rank = get_comm_rank()
    end if

    open( temporary_unit, status='scratch', action='readwrite', &
          iostat=condition )

    if (condition /= 0) then
      write( 6, '("feign_finite_element_config: ", I0)' ) condition
      stop
    end if

    write( temporary_unit, '("&finite_element")' )
    write( temporary_unit, '("cellshape = ''", A, "''")' ) key_from_cellshape( cellshape )
    write( temporary_unit, '("coord_order = ", I0)' ) coord_order
    write( temporary_unit, '("coord_system = ''", A, "''")' ) key_from_coord_system( coord_system )
    write( temporary_unit, '("element_order = ", I0)' ) element_order
    write( temporary_unit, '("rehabilitate = ", L2)' ) rehabilitate
    write( temporary_unit, '("vorticity_in_w1 = ", L2)' ) vorticity_in_w1
    write( temporary_unit, '("/")' )

    rewind(temporary_unit)
    call read_finite_element_namelist( temporary_unit, local_rank )
    call postprocess_finite_element_namelist()
    close(temporary_unit, iostat=condition )
    if (condition /= 0) stop &
        'feign_finite_element_config: '// &
        'Unable to close temporary file'

  end subroutine feign_finite_element_config

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_formulation_config( dlayer_on, &
!!$                                       dry_static_adjust, &
!!$                                       eos_method, &
!!$                                       exner_from_eos, &
!!$                                       init_exner_bt, &
!!$                                       l_multigrid, &
!!$                                       moisture_formulation, &
!!$                                       rotating, &
!!$                                       shallow, &
!!$                                       si_momentum_equation, &
!!$                                       use_multires_coupling, &
!!$                                       use_physics, &
!!$                                       use_wavedynamics, &
!!$                                       vector_invariant )
!!$
!!$    use formulation_config_mod, only : read_formulation_namelist, &
!!$                                       postprocess_formulation_namelist, &
!!$                                       key_from_eos_method, &
!!$                                       eos_method_from_key, &
!!$                                       key_from_moisture_formulation, &
!!$                                       moisture_formulation_from_key
!!$
!!$    implicit none
!!$
!!$    logical(l_def), intent(in) :: dlayer_on
!!$    logical(l_def), intent(in) :: dry_static_adjust
!!$    integer(i_native), intent(in) :: eos_method
!!$    logical(l_def), intent(in) :: exner_from_eos
!!$    logical(l_def), intent(in) :: init_exner_bt
!!$    logical(l_def), intent(in) :: l_multigrid
!!$    integer(i_native), intent(in) :: moisture_formulation
!!$    logical(l_def), intent(in) :: rotating
!!$    logical(l_def), intent(in) :: shallow
!!$    logical(l_def), intent(in) :: si_momentum_equation
!!$    logical(l_def), intent(in) :: use_multires_coupling
!!$    logical(l_def), intent(in) :: use_physics
!!$    logical(l_def), intent(in) :: use_wavedynamics
!!$    logical(l_def), intent(in) :: vector_invariant
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_formulation_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&formulation")' )
!!$    write( temporary_unit, '("dlayer_on = ", L2)' ) dlayer_on
!!$    write( temporary_unit, '("dry_static_adjust = ", L2)' ) dry_static_adjust
!!$    write( temporary_unit, '("eos_method = ''", A, "''")' ) key_from_eos_method( eos_method )
!!$    write( temporary_unit, '("exner_from_eos = ", L2)' ) exner_from_eos
!!$    write( temporary_unit, '("init_exner_bt = ", L2)' ) init_exner_bt
!!$    write( temporary_unit, '("l_multigrid = ", L2)' ) l_multigrid
!!$    write( temporary_unit, '("moisture_formulation = ''", A, "''")' ) key_from_moisture_formulation( moisture_formulation )
!!$    write( temporary_unit, '("rotating = ", L2)' ) rotating
!!$    write( temporary_unit, '("shallow = ", L2)' ) shallow
!!$    write( temporary_unit, '("si_momentum_equation = ", L2)' ) si_momentum_equation
!!$    write( temporary_unit, '("use_multires_coupling = ", L2)' ) use_multires_coupling
!!$    write( temporary_unit, '("use_physics = ", L2)' ) use_physics
!!$    write( temporary_unit, '("use_wavedynamics = ", L2)' ) use_wavedynamics
!!$    write( temporary_unit, '("vector_invariant = ", L2)' ) vector_invariant
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_formulation_namelist( temporary_unit, local_rank )
!!$    call postprocess_formulation_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_formulation_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_formulation_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_helmholtz_solver_config( diagnostic_norm, &
!!$                                            gcrk, &
!!$                                            method, &
!!$                                            normalise, &
!!$                                            preconditioner, &
!!$                                            si_pressure_a_tol, &
!!$                                            si_pressure_maximum_iterations, &
!!$                                            si_pressure_tolerance )
!!$
!!$    use helmholtz_solver_config_mod, only : read_helmholtz_solver_namelist, &
!!$                                            postprocess_helmholtz_solver_namelist, &
!!$                                            key_from_method, &
!!$                                            method_from_key, &
!!$                                            key_from_preconditioner, &
!!$                                            preconditioner_from_key
!!$
!!$    implicit none
!!$
!!$    logical(l_def), intent(in) :: diagnostic_norm
!!$    integer(i_def), intent(in) :: gcrk
!!$    integer(i_native), intent(in) :: method
!!$    logical(l_def), intent(in) :: normalise
!!$    integer(i_native), intent(in) :: preconditioner
!!$    real(r_def), intent(in) :: si_pressure_a_tol
!!$    integer(i_def), intent(in) :: si_pressure_maximum_iterations
!!$    real(r_def), intent(in) :: si_pressure_tolerance
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_helmholtz_solver_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&helmholtz_solver")' )
!!$    write( temporary_unit, '("diagnostic_norm = ", L2)' ) diagnostic_norm
!!$    write( temporary_unit, '("gcrk = ", I0)' ) gcrk
!!$    write( temporary_unit, '("method = ''", A, "''")' ) key_from_method( method )
!!$    write( temporary_unit, '("normalise = ", L2)' ) normalise
!!$    write( temporary_unit, '("preconditioner = ''", A, "''")' ) key_from_preconditioner( preconditioner )
!!$    write( temporary_unit, '("si_pressure_a_tol = ", E14.7)' ) si_pressure_a_tol
!!$    write( temporary_unit, '("si_pressure_maximum_iterations = ", I0)' ) si_pressure_maximum_iterations
!!$    write( temporary_unit, '("si_pressure_tolerance = ", E14.7)' ) si_pressure_tolerance
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_helmholtz_solver_namelist( temporary_unit, local_rank )
!!$    call postprocess_helmholtz_solver_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_helmholtz_solver_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_helmholtz_solver_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_idealised_config( test )
!!$
!!$    use idealised_config_mod, only : read_idealised_namelist, &
!!$                                     postprocess_idealised_namelist, &
!!$                                     key_from_test, &
!!$                                     test_from_key
!!$
!!$    implicit none
!!$
!!$    integer(i_native), intent(in) :: test
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_idealised_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&idealised")' )
!!$    write( temporary_unit, '("test = ''", A, "''")' ) key_from_test( test )
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_idealised_namelist( temporary_unit, local_rank )
!!$    call postprocess_idealised_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_idealised_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_idealised_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_initial_density_config( r1, &
!!$                                           r2, &
!!$                                           tracer_background, &
!!$                                           tracer_max, &
!!$                                           x1, &
!!$                                           x2, &
!!$                                           y1, &
!!$                                           y2, &
!!$                                           z1, &
!!$                                           z2 )
!!$
!!$    use initial_density_config_mod, only : read_initial_density_namelist, &
!!$                                           postprocess_initial_density_namelist
!!$
!!$    implicit none
!!$
!!$    real(r_def), intent(in) :: r1
!!$    real(r_def), intent(in) :: r2
!!$    real(r_def), intent(in) :: tracer_background
!!$    real(r_def), intent(in) :: tracer_max
!!$    real(r_def), intent(in) :: x1
!!$    real(r_def), intent(in) :: x2
!!$    real(r_def), intent(in) :: y1
!!$    real(r_def), intent(in) :: y2
!!$    real(r_def), intent(in) :: z1
!!$    real(r_def), intent(in) :: z2
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_initial_density_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&initial_density")' )
!!$    write( temporary_unit, '("r1 = ", E14.7)' ) r1
!!$    write( temporary_unit, '("r2 = ", E14.7)' ) r2
!!$    write( temporary_unit, '("tracer_background = ", E14.7)' ) tracer_background
!!$    write( temporary_unit, '("tracer_max = ", E14.7)' ) tracer_max
!!$    write( temporary_unit, '("x1 = ", E14.7)' ) x1
!!$    write( temporary_unit, '("x2 = ", E14.7)' ) x2
!!$    write( temporary_unit, '("y1 = ", E14.7)' ) y1
!!$    write( temporary_unit, '("y2 = ", E14.7)' ) y2
!!$    write( temporary_unit, '("z1 = ", E14.7)' ) z1
!!$    write( temporary_unit, '("z2 = ", E14.7)' ) z2
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_initial_density_namelist( temporary_unit, local_rank )
!!$    call postprocess_initial_density_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_initial_density_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_initial_density_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_initial_pressure_config( method, &
!!$                                            surface_pressure )
!!$
!!$    use initial_pressure_config_mod, only : read_initial_pressure_namelist, &
!!$                                            postprocess_initial_pressure_namelist, &
!!$                                            key_from_method, &
!!$                                            method_from_key
!!$
!!$    implicit none
!!$
!!$    integer(i_native), intent(in) :: method
!!$    real(r_def), intent(in) :: surface_pressure
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_initial_pressure_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&initial_pressure")' )
!!$    write( temporary_unit, '("method = ''", A, "''")' ) key_from_method( method )
!!$    write( temporary_unit, '("surface_pressure = ", E14.7)' ) surface_pressure
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_initial_pressure_namelist( temporary_unit, local_rank )
!!$    call postprocess_initial_pressure_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_initial_pressure_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_initial_pressure_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_initial_temperature_config( bvf_square, &
!!$                                               pert_width_scaling, &
!!$                                               perturb, &
!!$                                               profile_data, &
!!$                                               profile_heights, &
!!$                                               profile_size, &
!!$                                               theta_surf )
!!$
!!$    use initial_temperature_config_mod, only : read_initial_temperature_namelist, &
!!$                                               postprocess_initial_temperature_namelist, &
!!$                                               key_from_perturb, &
!!$                                               perturb_from_key
!!$
!!$    implicit none
!!$
!!$    real(r_def), intent(in) :: bvf_square
!!$    real(r_def), intent(in) :: pert_width_scaling
!!$    integer(i_native), intent(in) :: perturb
!!$    real(r_def), intent(in) :: profile_data(:)
!!$    real(r_def), intent(in) :: profile_heights(:)
!!$    integer(i_def), intent(in) :: profile_size
!!$    real(r_def), intent(in) :: theta_surf
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_initial_temperature_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&initial_temperature")' )
!!$    write( temporary_unit, '("bvf_square = ", E14.7)' ) bvf_square
!!$    write( temporary_unit, '("pert_width_scaling = ", E14.7)' ) pert_width_scaling
!!$    write( temporary_unit, '("perturb = ''", A, "''")' ) key_from_perturb( perturb )
!!$    if (size(profile_data) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(profile_data)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'profile_data = ', profile_data
!!$    else
!!$      write( temporary_unit, '("profile_data = ", E14.7)' ) profile_data
!!$    end if
!!$    if (size(profile_heights) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(profile_heights)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'profile_heights = ', profile_heights
!!$    else
!!$      write( temporary_unit, '("profile_heights = ", E14.7)' ) profile_heights
!!$    end if
!!$    write( temporary_unit, '("profile_size = ", I0)' ) profile_size
!!$    write( temporary_unit, '("theta_surf = ", E14.7)' ) theta_surf
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_initial_temperature_namelist( temporary_unit, local_rank )
!!$    call postprocess_initial_temperature_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_initial_temperature_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_initial_temperature_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_initial_vapour_config( profile_data, &
!!$                                          profile_heights, &
!!$                                          profile_size )
!!$
!!$    use initial_vapour_config_mod, only : read_initial_vapour_namelist, &
!!$                                          postprocess_initial_vapour_namelist
!!$
!!$    implicit none
!!$
!!$    real(r_def), intent(in) :: profile_data(:)
!!$    real(r_def), intent(in) :: profile_heights(:)
!!$    integer(i_def), intent(in) :: profile_size
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_initial_vapour_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&initial_vapour")' )
!!$    if (size(profile_data) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(profile_data)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'profile_data = ', profile_data
!!$    else
!!$      write( temporary_unit, '("profile_data = ", E14.7)' ) profile_data
!!$    end if
!!$    if (size(profile_heights) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(profile_heights)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'profile_heights = ', profile_heights
!!$    else
!!$      write( temporary_unit, '("profile_heights = ", E14.7)' ) profile_heights
!!$    end if
!!$    write( temporary_unit, '("profile_size = ", I0)' ) profile_size
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_initial_vapour_namelist( temporary_unit, local_rank )
!!$    call postprocess_initial_vapour_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_initial_vapour_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_initial_vapour_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_initial_wind_config( nl_constant, &
!!$                                        profile, &
!!$                                        sbr_angle_lat, &
!!$                                        sbr_angle_lon, &
!!$                                        shear, &
!!$                                        smp_init_wind, &
!!$                                        u0, &
!!$                                        v0, &
!!$                                        wavelength, &
!!$                                        wind_time_period )
!!$
!!$    use initial_wind_config_mod, only : read_initial_wind_namelist, &
!!$                                        postprocess_initial_wind_namelist, &
!!$                                        key_from_profile, &
!!$                                        profile_from_key
!!$
!!$    implicit none
!!$
!!$    real(r_def), intent(in) :: nl_constant
!!$    integer(i_native), intent(in) :: profile
!!$    real(r_def), intent(in) :: sbr_angle_lat
!!$    real(r_def), intent(in) :: sbr_angle_lon
!!$    real(r_def), intent(in) :: shear
!!$    logical(l_def), intent(in) :: smp_init_wind
!!$    real(r_def), intent(in) :: u0
!!$    real(r_def), intent(in) :: v0
!!$    real(r_def), intent(in) :: wavelength
!!$    real(r_def), intent(in) :: wind_time_period
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_initial_wind_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&initial_wind")' )
!!$    write( temporary_unit, '("nl_constant = ", E14.7)' ) nl_constant
!!$    write( temporary_unit, '("profile = ''", A, "''")' ) key_from_profile( profile )
!!$    write( temporary_unit, '("sbr_angle_lat = ", E14.7)' ) sbr_angle_lat
!!$    write( temporary_unit, '("sbr_angle_lon = ", E14.7)' ) sbr_angle_lon
!!$    write( temporary_unit, '("shear = ", E14.7)' ) shear
!!$    write( temporary_unit, '("smp_init_wind = ", L2)' ) smp_init_wind
!!$    write( temporary_unit, '("u0 = ", E14.7)' ) u0
!!$    write( temporary_unit, '("v0 = ", E14.7)' ) v0
!!$    write( temporary_unit, '("wavelength = ", E14.7)' ) wavelength
!!$    write( temporary_unit, '("wind_time_period = ", E14.7)' ) wind_time_period
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_initial_wind_namelist( temporary_unit, local_rank )
!!$    call postprocess_initial_wind_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_initial_wind_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_initial_wind_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_initialization_config( ancil_option, &
!!$                                          hydro_coriolis, &
!!$                                          init_option, &
!!$                                          lbc_option, &
!!$                                          ls_option, &
!!$                                          read_w2h_wind )
!!$
!!$    use initialization_config_mod, only : read_initialization_namelist, &
!!$                                          postprocess_initialization_namelist, &
!!$                                          key_from_ancil_option, &
!!$                                          ancil_option_from_key, &
!!$                                          key_from_init_option, &
!!$                                          init_option_from_key, &
!!$                                          key_from_lbc_option, &
!!$                                          lbc_option_from_key, &
!!$                                          key_from_ls_option, &
!!$                                          ls_option_from_key
!!$
!!$    implicit none
!!$
!!$    integer(i_native), intent(in) :: ancil_option
!!$    logical(l_def), intent(in) :: hydro_coriolis
!!$    integer(i_native), intent(in) :: init_option
!!$    integer(i_native), intent(in) :: lbc_option
!!$    integer(i_native), intent(in) :: ls_option
!!$    logical(l_def), intent(in) :: read_w2h_wind
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_initialization_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&initialization")' )
!!$    write( temporary_unit, '("ancil_option = ''", A, "''")' ) key_from_ancil_option( ancil_option )
!!$    write( temporary_unit, '("hydro_coriolis = ", L2)' ) hydro_coriolis
!!$    write( temporary_unit, '("init_option = ''", A, "''")' ) key_from_init_option( init_option )
!!$    write( temporary_unit, '("lbc_option = ''", A, "''")' ) key_from_lbc_option( lbc_option )
!!$    write( temporary_unit, '("ls_option = ''", A, "''")' ) key_from_ls_option( ls_option )
!!$    write( temporary_unit, '("read_w2h_wind = ", L2)' ) read_w2h_wind
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_initialization_namelist( temporary_unit, local_rank )
!!$    call postprocess_initialization_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_initialization_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_initialization_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_io_config( checkpoint_frequency, &
!!$                              checkpoint_read, &
!!$                              checkpoint_write, &
!!$                              counter_output_suffix, &
!!$                              diagnostic_frequency, &
!!$                              nodal_output_on_w3, &
!!$                              subroutine_counters, &
!!$                              subroutine_timers, &
!!$                              timer_output_path, &
!!$                              use_xios_io, &
!!$                              write_conservation_diag, &
!!$                              write_diag, &
!!$                              write_dump, &
!!$                              write_fluxes, &
!!$                              write_minmax_tseries )
!!$
!!$    use io_config_mod, only : read_io_namelist, &
!!$                              postprocess_io_namelist
!!$
!!$    implicit none
!!$
!!$    integer(i_def), intent(in) :: checkpoint_frequency
!!$    logical(l_def), intent(in) :: checkpoint_read
!!$    logical(l_def), intent(in) :: checkpoint_write
!!$    character(*), intent(in) :: counter_output_suffix
!!$    integer(i_medium), intent(in) :: diagnostic_frequency
!!$    logical(l_def), intent(in) :: nodal_output_on_w3
!!$    logical(l_def), intent(in) :: subroutine_counters
!!$    logical(l_def), intent(in) :: subroutine_timers
!!$    character(*), intent(in) :: timer_output_path
!!$    logical(l_def), intent(in) :: use_xios_io
!!$    logical(l_def), intent(in) :: write_conservation_diag
!!$    logical(l_def), intent(in) :: write_diag
!!$    logical(l_def), intent(in) :: write_dump
!!$    logical(l_def), intent(in) :: write_fluxes
!!$    logical(l_def), intent(in) :: write_minmax_tseries
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_io_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&io")' )
!!$    write( temporary_unit, '("checkpoint_frequency = ", I0)' ) checkpoint_frequency
!!$    write( temporary_unit, '("checkpoint_read = ", L2)' ) checkpoint_read
!!$    write( temporary_unit, '("checkpoint_write = ", L2)' ) checkpoint_write
!!$    write( temporary_unit, '("counter_output_suffix = ''", A, "''")' ) counter_output_suffix
!!$    write( temporary_unit, '("diagnostic_frequency = ", I0)' ) diagnostic_frequency
!!$    write( temporary_unit, '("nodal_output_on_w3 = ", L2)' ) nodal_output_on_w3
!!$    write( temporary_unit, '("subroutine_counters = ", L2)' ) subroutine_counters
!!$    write( temporary_unit, '("subroutine_timers = ", L2)' ) subroutine_timers
!!$    write( temporary_unit, '("timer_output_path = ''", A, "''")' ) timer_output_path
!!$    write( temporary_unit, '("use_xios_io = ", L2)' ) use_xios_io
!!$    write( temporary_unit, '("write_conservation_diag = ", L2)' ) write_conservation_diag
!!$    write( temporary_unit, '("write_diag = ", L2)' ) write_diag
!!$    write( temporary_unit, '("write_dump = ", L2)' ) write_dump
!!$    write( temporary_unit, '("write_fluxes = ", L2)' ) write_fluxes
!!$    write( temporary_unit, '("write_minmax_tseries = ", L2)' ) write_minmax_tseries
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_io_namelist( temporary_unit, local_rank )
!!$    call postprocess_io_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_io_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_io_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_jules_surface_config( cor_mo_iter, &
!!$                                         fd_stability_dep, &
!!$                                         formdrag, &
!!$                                         l_vary_z0m_soil, &
!!$                                         srf_ex_cnv_gust )
!!$
!!$    use jules_surface_config_mod, only : read_jules_surface_namelist, &
!!$                                         postprocess_jules_surface_namelist, &
!!$                                         key_from_cor_mo_iter, &
!!$                                         cor_mo_iter_from_key, &
!!$                                         key_from_fd_stability_dep, &
!!$                                         fd_stability_dep_from_key, &
!!$                                         key_from_formdrag, &
!!$                                         formdrag_from_key
!!$
!!$    implicit none
!!$
!!$    integer(i_native), intent(in) :: cor_mo_iter
!!$    integer(i_native), intent(in) :: fd_stability_dep
!!$    integer(i_native), intent(in) :: formdrag
!!$    logical(l_def), intent(in) :: l_vary_z0m_soil
!!$    logical(l_def), intent(in) :: srf_ex_cnv_gust
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_jules_surface_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&jules_surface")' )
!!$    write( temporary_unit, '("cor_mo_iter = ''", A, "''")' ) key_from_cor_mo_iter( cor_mo_iter )
!!$    write( temporary_unit, '("fd_stability_dep = ''", A, "''")' ) key_from_fd_stability_dep( fd_stability_dep )
!!$    write( temporary_unit, '("formdrag = ''", A, "''")' ) key_from_formdrag( formdrag )
!!$    write( temporary_unit, '("l_vary_z0m_soil = ", L2)' ) l_vary_z0m_soil
!!$    write( temporary_unit, '("srf_ex_cnv_gust = ", L2)' ) srf_ex_cnv_gust
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_jules_surface_namelist( temporary_unit, local_rank )
!!$    call postprocess_jules_surface_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_jules_surface_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_jules_surface_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_jules_vegetation_config( can_rad_mod, &
!!$                                            l_limit_canhc, &
!!$                                            l_spec_veg_z0 )
!!$
!!$    use jules_vegetation_config_mod, only : read_jules_vegetation_namelist, &
!!$                                            postprocess_jules_vegetation_namelist, &
!!$                                            key_from_can_rad_mod, &
!!$                                            can_rad_mod_from_key
!!$
!!$    implicit none
!!$
!!$    integer(i_native), intent(in) :: can_rad_mod
!!$    logical(l_def), intent(in) :: l_limit_canhc
!!$    logical(l_def), intent(in) :: l_spec_veg_z0
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_jules_vegetation_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&jules_vegetation")' )
!!$    write( temporary_unit, '("can_rad_mod = ''", A, "''")' ) key_from_can_rad_mod( can_rad_mod )
!!$    write( temporary_unit, '("l_limit_canhc = ", L2)' ) l_limit_canhc
!!$    write( temporary_unit, '("l_spec_veg_z0 = ", L2)' ) l_spec_veg_z0
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_jules_vegetation_namelist( temporary_unit, local_rank )
!!$    call postprocess_jules_vegetation_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_jules_vegetation_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_jules_vegetation_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_logging_config( run_log_level )
!!$
!!$    use logging_config_mod, only : read_logging_namelist, &
!!$                                   postprocess_logging_namelist, &
!!$                                   key_from_run_log_level, &
!!$                                   run_log_level_from_key
!!$
!!$    implicit none
!!$
!!$    integer(i_native), intent(in) :: run_log_level
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_logging_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&logging")' )
!!$    write( temporary_unit, '("run_log_level = ''", A, "''")' ) key_from_run_log_level( run_log_level )
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_logging_namelist( temporary_unit, local_rank )
!!$    call postprocess_logging_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_logging_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_logging_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_microphysics_config( a_ratio_exp, &
!!$                                        a_ratio_fac, &
!!$                                        droplet_tpr, &
!!$                                        ndrop_surf, &
!!$                                        qcl_rime, &
!!$                                        shape_rime, &
!!$                                        turb_gen_mixph, &
!!$                                        z_surf )
!!$
!!$    use microphysics_config_mod, only : read_microphysics_namelist, &
!!$                                        postprocess_microphysics_namelist
!!$
!!$    implicit none
!!$
!!$    real(r_def), intent(in) :: a_ratio_exp
!!$    real(r_def), intent(in) :: a_ratio_fac
!!$    logical(l_def), intent(in) :: droplet_tpr
!!$    real(r_def), intent(in) :: ndrop_surf
!!$    real(r_def), intent(in) :: qcl_rime
!!$    logical(l_def), intent(in) :: shape_rime
!!$    logical(l_def), intent(in) :: turb_gen_mixph
!!$    real(r_def), intent(in) :: z_surf
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_microphysics_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&microphysics")' )
!!$    write( temporary_unit, '("a_ratio_exp = ", E14.7)' ) a_ratio_exp
!!$    write( temporary_unit, '("a_ratio_fac = ", E14.7)' ) a_ratio_fac
!!$    write( temporary_unit, '("droplet_tpr = ", L2)' ) droplet_tpr
!!$    write( temporary_unit, '("ndrop_surf = ", E14.7)' ) ndrop_surf
!!$    write( temporary_unit, '("qcl_rime = ", E14.7)' ) qcl_rime
!!$    write( temporary_unit, '("shape_rime = ", L2)' ) shape_rime
!!$    write( temporary_unit, '("turb_gen_mixph = ", L2)' ) turb_gen_mixph
!!$    write( temporary_unit, '("z_surf = ", E14.7)' ) z_surf
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_microphysics_namelist( temporary_unit, local_rank )
!!$    call postprocess_microphysics_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_microphysics_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_microphysics_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_mixed_solver_config( eliminate_variables, &
!!$                                        gcrk, &
!!$                                        guess_np1, &
!!$                                        mixed_solver_a_tol, &
!!$                                        normalise, &
!!$                                        reference_reset_freq, &
!!$                                        si_diagnostic_norm, &
!!$                                        si_maximum_iterations, &
!!$                                        si_method, &
!!$                                        si_preconditioner, &
!!$                                        si_tolerance, &
!!$                                        split_w )
!!$
!!$    use mixed_solver_config_mod, only : read_mixed_solver_namelist, &
!!$                                        postprocess_mixed_solver_namelist, &
!!$                                        key_from_eliminate_variables, &
!!$                                        eliminate_variables_from_key, &
!!$                                        key_from_si_method, &
!!$                                        si_method_from_key, &
!!$                                        key_from_si_preconditioner, &
!!$                                        si_preconditioner_from_key
!!$
!!$    implicit none
!!$
!!$    integer(i_native), intent(in) :: eliminate_variables
!!$    integer(i_def), intent(in) :: gcrk
!!$    logical(l_def), intent(in) :: guess_np1
!!$    real(r_def), intent(in) :: mixed_solver_a_tol
!!$    logical(l_def), intent(in) :: normalise
!!$    integer(i_def), intent(in) :: reference_reset_freq
!!$    logical(l_def), intent(in) :: si_diagnostic_norm
!!$    integer(i_def), intent(in) :: si_maximum_iterations
!!$    integer(i_native), intent(in) :: si_method
!!$    integer(i_native), intent(in) :: si_preconditioner
!!$    real(r_def), intent(in) :: si_tolerance
!!$    logical(l_def), intent(in) :: split_w
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_mixed_solver_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&mixed_solver")' )
!!$    write( temporary_unit, '("eliminate_variables = ''", A, "''")' ) key_from_eliminate_variables( eliminate_variables )
!!$    write( temporary_unit, '("gcrk = ", I0)' ) gcrk
!!$    write( temporary_unit, '("guess_np1 = ", L2)' ) guess_np1
!!$    write( temporary_unit, '("mixed_solver_a_tol = ", E14.7)' ) mixed_solver_a_tol
!!$    write( temporary_unit, '("normalise = ", L2)' ) normalise
!!$    write( temporary_unit, '("reference_reset_freq = ", I0)' ) reference_reset_freq
!!$    write( temporary_unit, '("si_diagnostic_norm = ", L2)' ) si_diagnostic_norm
!!$    write( temporary_unit, '("si_maximum_iterations = ", I0)' ) si_maximum_iterations
!!$    write( temporary_unit, '("si_method = ''", A, "''")' ) key_from_si_method( si_method )
!!$    write( temporary_unit, '("si_preconditioner = ''", A, "''")' ) key_from_si_preconditioner( si_preconditioner )
!!$    write( temporary_unit, '("si_tolerance = ", E14.7)' ) si_tolerance
!!$    write( temporary_unit, '("split_w = ", L2)' ) split_w
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_mixed_solver_namelist( temporary_unit, local_rank )
!!$    call postprocess_mixed_solver_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_mixed_solver_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_mixed_solver_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_mixing_config( leonard_kl, &
!!$                                  leonard_term, &
!!$                                  method, &
!!$                                  mix_factor, &
!!$                                  smagorinsky, &
!!$                                  viscosity, &
!!$                                  viscosity_mu )
!!$
!!$    use mixing_config_mod, only : read_mixing_namelist, &
!!$                                  postprocess_mixing_namelist, &
!!$                                  key_from_method, &
!!$                                  method_from_key
!!$
!!$    implicit none
!!$
!!$    real(r_def), intent(in) :: leonard_kl
!!$    logical(l_def), intent(in) :: leonard_term
!!$    integer(i_native), intent(in) :: method
!!$    real(r_def), intent(in) :: mix_factor
!!$    logical(l_def), intent(in) :: smagorinsky
!!$    logical(l_def), intent(in) :: viscosity
!!$    real(r_def), intent(in) :: viscosity_mu
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_mixing_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&mixing")' )
!!$    write( temporary_unit, '("leonard_kl = ", E14.7)' ) leonard_kl
!!$    write( temporary_unit, '("leonard_term = ", L2)' ) leonard_term
!!$    write( temporary_unit, '("method = ''", A, "''")' ) key_from_method( method )
!!$    write( temporary_unit, '("mix_factor = ", E14.7)' ) mix_factor
!!$    write( temporary_unit, '("smagorinsky = ", L2)' ) smagorinsky
!!$    write( temporary_unit, '("viscosity = ", L2)' ) viscosity
!!$    write( temporary_unit, '("viscosity_mu = ", E14.7)' ) viscosity_mu
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_mixing_namelist( temporary_unit, local_rank )
!!$    call postprocess_mixing_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_mixing_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_mixing_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_multigrid_config( chain_mesh_tags, &
!!$                                     multigrid_chain_nitems, &
!!$                                     n_coarsesmooth, &
!!$                                     n_postsmooth, &
!!$                                     n_presmooth, &
!!$                                     smooth_relaxation )
!!$
!!$    use multigrid_config_mod, only : read_multigrid_namelist, &
!!$                                     postprocess_multigrid_namelist
!!$
!!$    implicit none
!!$
!!$    character(*), intent(in) :: chain_mesh_tags(:)
!!$    integer(i_def), intent(in) :: multigrid_chain_nitems
!!$    integer(i_def), intent(in) :: n_coarsesmooth
!!$    integer(i_def), intent(in) :: n_postsmooth
!!$    integer(i_def), intent(in) :: n_presmooth
!!$    real(r_def), intent(in) :: smooth_relaxation
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_multigrid_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&multigrid")' )
!!$    write( tmp_str,'(A)') "'"//trim(chain_mesh_tags(1))//"'"
!!$    if (size(chain_mesh_tags) > 1) then
!!$      do i=2, size(chain_mesh_tags)
!!$        write( tmp_str,'(A)') trim(tmp_str)//",'"//trim(chain_mesh_tags(i))//"'"
!!$      end do
!!$    end if
!!$    write( temporary_unit, '(A)' ) 'chain_mesh_tags = '// trim(tmp_str)
!!$    write( temporary_unit, '("multigrid_chain_nitems = ", I0)' ) multigrid_chain_nitems
!!$    write( temporary_unit, '("n_coarsesmooth = ", I0)' ) n_coarsesmooth
!!$    write( temporary_unit, '("n_postsmooth = ", I0)' ) n_postsmooth
!!$    write( temporary_unit, '("n_presmooth = ", I0)' ) n_presmooth
!!$    write( temporary_unit, '("smooth_relaxation = ", E14.7)' ) smooth_relaxation
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_multigrid_namelist( temporary_unit, local_rank )
!!$    call postprocess_multigrid_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_multigrid_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_multigrid_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_multires_coupling_config( dynamics_mesh_name, &
!!$                                             multires_coupling_mesh_tags, &
!!$                                             multires_coupling_mode, &
!!$                                             physics_mesh_name )
!!$
!!$    use multires_coupling_config_mod, only : read_multires_coupling_namelist, &
!!$                                             postprocess_multires_coupling_namelist, &
!!$                                             key_from_multires_coupling_mode, &
!!$                                             multires_coupling_mode_from_key
!!$
!!$    implicit none
!!$
!!$    character(*), intent(in) :: dynamics_mesh_name
!!$    character(*), intent(in) :: multires_coupling_mesh_tags(:)
!!$    integer(i_native), intent(in) :: multires_coupling_mode
!!$    character(*), intent(in) :: physics_mesh_name
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_multires_coupling_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&multires_coupling")' )
!!$    write( temporary_unit, '("dynamics_mesh_name = ''", A, "''")' ) dynamics_mesh_name
!!$    write( tmp_str,'(A)') "'"//trim(multires_coupling_mesh_tags(1))//"'"
!!$    if (size(multires_coupling_mesh_tags) > 1) then
!!$      do i=2, size(multires_coupling_mesh_tags)
!!$        write( tmp_str,'(A)') trim(tmp_str)//",'"//trim(multires_coupling_mesh_tags(i))//"'"
!!$      end do
!!$    end if
!!$    write( temporary_unit, '(A)' ) 'multires_coupling_mesh_tags = '// trim(tmp_str)
!!$    write( temporary_unit, '("multires_coupling_mode = ''", A, "''")' ) key_from_multires_coupling_mode( multires_coupling_mode )
!!$    write( temporary_unit, '("physics_mesh_name = ''", A, "''")' ) physics_mesh_name
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_multires_coupling_namelist( temporary_unit, local_rank )
!!$    call postprocess_multires_coupling_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_multires_coupling_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_multires_coupling_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_orbit_config( arg_periapsis, &
!!$                                 arg_periapsis_inc, &
!!$                                 eccentricity, &
!!$                                 eccentricity_inc, &
!!$                                 elements, &
!!$                                 epoch, &
!!$                                 fixed_azimuth_angle, &
!!$                                 fixed_zenith_angle, &
!!$                                 hour_angle, &
!!$                                 hour_angle_inc, &
!!$                                 mean_anomaly, &
!!$                                 mean_anomaly_inc, &
!!$                                 obliquity, &
!!$                                 obliquity_inc, &
!!$                                 observer_lat, &
!!$                                 observer_lon, &
!!$                                 semimajor_axis, &
!!$                                 semimajor_axis_inc, &
!!$                                 spin )
!!$
!!$    use orbit_config_mod, only : read_orbit_namelist, &
!!$                                 postprocess_orbit_namelist, &
!!$                                 key_from_elements, &
!!$                                 elements_from_key, &
!!$                                 key_from_spin, &
!!$                                 spin_from_key
!!$
!!$    implicit none
!!$
!!$    real(r_def), intent(in) :: arg_periapsis
!!$    real(r_def), intent(in) :: arg_periapsis_inc
!!$    real(r_def), intent(in) :: eccentricity
!!$    real(r_def), intent(in) :: eccentricity_inc
!!$    integer(i_native), intent(in) :: elements
!!$    real(r_def), intent(in) :: epoch
!!$    real(r_def), intent(in) :: fixed_azimuth_angle
!!$    real(r_def), intent(in) :: fixed_zenith_angle
!!$    real(r_def), intent(in) :: hour_angle
!!$    real(r_def), intent(in) :: hour_angle_inc
!!$    real(r_def), intent(in) :: mean_anomaly
!!$    real(r_def), intent(in) :: mean_anomaly_inc
!!$    real(r_def), intent(in) :: obliquity
!!$    real(r_def), intent(in) :: obliquity_inc
!!$    real(r_def), intent(in) :: observer_lat
!!$    real(r_def), intent(in) :: observer_lon
!!$    real(r_def), intent(in) :: semimajor_axis
!!$    real(r_def), intent(in) :: semimajor_axis_inc
!!$    integer(i_native), intent(in) :: spin
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_orbit_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&orbit")' )
!!$    write( temporary_unit, '("arg_periapsis = ", E14.7)' ) arg_periapsis
!!$    write( temporary_unit, '("arg_periapsis_inc = ", E14.7)' ) arg_periapsis_inc
!!$    write( temporary_unit, '("eccentricity = ", E14.7)' ) eccentricity
!!$    write( temporary_unit, '("eccentricity_inc = ", E14.7)' ) eccentricity_inc
!!$    write( temporary_unit, '("elements = ''", A, "''")' ) key_from_elements( elements )
!!$    write( temporary_unit, '("epoch = ", E14.7)' ) epoch
!!$    write( temporary_unit, '("fixed_azimuth_angle = ", E14.7)' ) fixed_azimuth_angle
!!$    write( temporary_unit, '("fixed_zenith_angle = ", E14.7)' ) fixed_zenith_angle
!!$    write( temporary_unit, '("hour_angle = ", E14.7)' ) hour_angle
!!$    write( temporary_unit, '("hour_angle_inc = ", E14.7)' ) hour_angle_inc
!!$    write( temporary_unit, '("mean_anomaly = ", E14.7)' ) mean_anomaly
!!$    write( temporary_unit, '("mean_anomaly_inc = ", E14.7)' ) mean_anomaly_inc
!!$    write( temporary_unit, '("obliquity = ", E14.7)' ) obliquity
!!$    write( temporary_unit, '("obliquity_inc = ", E14.7)' ) obliquity_inc
!!$    write( temporary_unit, '("observer_lat = ", E14.7)' ) observer_lat
!!$    write( temporary_unit, '("observer_lon = ", E14.7)' ) observer_lon
!!$    write( temporary_unit, '("semimajor_axis = ", E14.7)' ) semimajor_axis
!!$    write( temporary_unit, '("semimajor_axis_inc = ", E14.7)' ) semimajor_axis_inc
!!$    write( temporary_unit, '("spin = ''", A, "''")' ) key_from_spin( spin )
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_orbit_namelist( temporary_unit, local_rank )
!!$    call postprocess_orbit_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_orbit_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_orbit_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_orographic_drag_config( cd_flow_blocking, &
!!$                                           fr_crit_gwd, &
!!$                                           fr_sat_gwd, &
!!$                                           gwd_scaling, &
!!$                                           include_moisture, &
!!$                                           mountain_height_scaling, &
!!$                                           orographic_blocking_heating, &
!!$                                           orographic_gwd_heating, &
!!$                                           vertical_smoothing )
!!$
!!$    use orographic_drag_config_mod, only : read_orographic_drag_namelist, &
!!$                                           postprocess_orographic_drag_namelist, &
!!$                                           key_from_include_moisture, &
!!$                                           include_moisture_from_key
!!$
!!$    implicit none
!!$
!!$    real(r_def), intent(in) :: cd_flow_blocking
!!$    real(r_def), intent(in) :: fr_crit_gwd
!!$    real(r_def), intent(in) :: fr_sat_gwd
!!$    real(r_def), intent(in) :: gwd_scaling
!!$    integer(i_native), intent(in) :: include_moisture
!!$    real(r_def), intent(in) :: mountain_height_scaling
!!$    logical(l_def), intent(in) :: orographic_blocking_heating
!!$    logical(l_def), intent(in) :: orographic_gwd_heating
!!$    logical(l_def), intent(in) :: vertical_smoothing
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_orographic_drag_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&orographic_drag")' )
!!$    write( temporary_unit, '("cd_flow_blocking = ", E14.7)' ) cd_flow_blocking
!!$    write( temporary_unit, '("fr_crit_gwd = ", E14.7)' ) fr_crit_gwd
!!$    write( temporary_unit, '("fr_sat_gwd = ", E14.7)' ) fr_sat_gwd
!!$    write( temporary_unit, '("gwd_scaling = ", E14.7)' ) gwd_scaling
!!$    write( temporary_unit, '("include_moisture = ''", A, "''")' ) key_from_include_moisture( include_moisture )
!!$    write( temporary_unit, '("mountain_height_scaling = ", E14.7)' ) mountain_height_scaling
!!$    write( temporary_unit, '("orographic_blocking_heating = ", L2)' ) orographic_blocking_heating
!!$    write( temporary_unit, '("orographic_gwd_heating = ", L2)' ) orographic_gwd_heating
!!$    write( temporary_unit, '("vertical_smoothing = ", L2)' ) vertical_smoothing
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_orographic_drag_namelist( temporary_unit, local_rank )
!!$    call postprocess_orographic_drag_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_orographic_drag_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_orographic_drag_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_orography_config( orog_init_option, &
!!$                                     profile )
!!$
!!$    use orography_config_mod, only : read_orography_namelist, &
!!$                                     postprocess_orography_namelist, &
!!$                                     key_from_orog_init_option, &
!!$                                     orog_init_option_from_key, &
!!$                                     key_from_profile, &
!!$                                     profile_from_key
!!$
!!$    implicit none
!!$
!!$    integer(i_native), intent(in) :: orog_init_option
!!$    integer(i_native), intent(in) :: profile
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_orography_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&orography")' )
!!$    write( temporary_unit, '("orog_init_option = ''", A, "''")' ) key_from_orog_init_option( orog_init_option )
!!$    write( temporary_unit, '("profile = ''", A, "''")' ) key_from_profile( profile )
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_orography_namelist( temporary_unit, local_rank )
!!$    call postprocess_orography_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_orography_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_orography_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_orography_agnesi_cartesian_config( direction, &
!!$                                                      half_width_x, &
!!$                                                      half_width_y, &
!!$                                                      mountain_height, &
!!$                                                      x_centre, &
!!$                                                      y_centre )
!!$
!!$    use orography_agnesi_cartesian_config_mod, only : read_orography_agnesi_cartesian_namelist, &
!!$                                                      postprocess_orography_agnesi_cartesian_namelist, &
!!$                                                      key_from_direction, &
!!$                                                      direction_from_key
!!$
!!$    implicit none
!!$
!!$    integer(i_native), intent(in) :: direction
!!$    real(r_def), intent(in) :: half_width_x
!!$    real(r_def), intent(in) :: half_width_y
!!$    real(r_def), intent(in) :: mountain_height
!!$    real(r_def), intent(in) :: x_centre
!!$    real(r_def), intent(in) :: y_centre
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_orography_agnesi_cartesian_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&orography_agnesi_cartesian")' )
!!$    write( temporary_unit, '("direction = ''", A, "''")' ) key_from_direction( direction )
!!$    write( temporary_unit, '("half_width_x = ", E14.7)' ) half_width_x
!!$    write( temporary_unit, '("half_width_y = ", E14.7)' ) half_width_y
!!$    write( temporary_unit, '("mountain_height = ", E14.7)' ) mountain_height
!!$    write( temporary_unit, '("x_centre = ", E14.7)' ) x_centre
!!$    write( temporary_unit, '("y_centre = ", E14.7)' ) y_centre
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_orography_agnesi_cartesian_namelist( temporary_unit, local_rank )
!!$    call postprocess_orography_agnesi_cartesian_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_orography_agnesi_cartesian_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_orography_agnesi_cartesian_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_orography_agnesi_spherical_config( half_width, &
!!$                                                      lambda_centre_dec, &
!!$                                                      lambda_focus_dec, &
!!$                                                      mountain_height, &
!!$                                                      phi_centre_dec, &
!!$                                                      phi_focus_dec )
!!$
!!$    use orography_agnesi_spherical_config_mod, only : read_orography_agnesi_spherical_namelist, &
!!$                                                      postprocess_orography_agnesi_spherical_namelist
!!$
!!$    implicit none
!!$
!!$    real(r_def), intent(in) :: half_width
!!$    real(r_def), intent(in) :: lambda_centre_dec
!!$    real(r_def), intent(in) :: lambda_focus_dec
!!$    real(r_def), intent(in) :: mountain_height
!!$    real(r_def), intent(in) :: phi_centre_dec
!!$    real(r_def), intent(in) :: phi_focus_dec
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_orography_agnesi_spherical_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&orography_agnesi_spherical")' )
!!$    write( temporary_unit, '("half_width = ", E14.7)' ) half_width
!!$    write( temporary_unit, '("lambda_centre_dec = ", E14.7)' ) lambda_centre_dec
!!$    write( temporary_unit, '("lambda_focus_dec = ", E14.7)' ) lambda_focus_dec
!!$    write( temporary_unit, '("mountain_height = ", E14.7)' ) mountain_height
!!$    write( temporary_unit, '("phi_centre_dec = ", E14.7)' ) phi_centre_dec
!!$    write( temporary_unit, '("phi_focus_dec = ", E14.7)' ) phi_focus_dec
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_orography_agnesi_spherical_namelist( temporary_unit, local_rank )
!!$    call postprocess_orography_agnesi_spherical_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_orography_agnesi_spherical_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_orography_agnesi_spherical_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_orography_bell_cartesian_config( direction, &
!!$                                                    half_width_x, &
!!$                                                    half_width_y, &
!!$                                                    mountain_height, &
!!$                                                    x_centre, &
!!$                                                    y_centre )
!!$
!!$    use orography_bell_cartesian_config_mod, only : read_orography_bell_cartesian_namelist, &
!!$                                                    postprocess_orography_bell_cartesian_namelist, &
!!$                                                    key_from_direction, &
!!$                                                    direction_from_key
!!$
!!$    implicit none
!!$
!!$    integer(i_native), intent(in) :: direction
!!$    real(r_def), intent(in) :: half_width_x
!!$    real(r_def), intent(in) :: half_width_y
!!$    real(r_def), intent(in) :: mountain_height
!!$    real(r_def), intent(in) :: x_centre
!!$    real(r_def), intent(in) :: y_centre
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_orography_bell_cartesian_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&orography_bell_cartesian")' )
!!$    write( temporary_unit, '("direction = ''", A, "''")' ) key_from_direction( direction )
!!$    write( temporary_unit, '("half_width_x = ", E14.7)' ) half_width_x
!!$    write( temporary_unit, '("half_width_y = ", E14.7)' ) half_width_y
!!$    write( temporary_unit, '("mountain_height = ", E14.7)' ) mountain_height
!!$    write( temporary_unit, '("x_centre = ", E14.7)' ) x_centre
!!$    write( temporary_unit, '("y_centre = ", E14.7)' ) y_centre
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_orography_bell_cartesian_namelist( temporary_unit, local_rank )
!!$    call postprocess_orography_bell_cartesian_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_orography_bell_cartesian_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_orography_bell_cartesian_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_orography_dcmip200_spherical_config( lambda_centre_dec, &
!!$                                                        mountain_height, &
!!$                                                        osc_half_width_dec, &
!!$                                                        phi_centre_dec, &
!!$                                                        radius_dec )
!!$
!!$    use orography_dcmip200_spherical_config_mod, only : read_orography_dcmip200_spherical_namelist, &
!!$                                                        postprocess_orography_dcmip200_spherical_namelist
!!$
!!$    implicit none
!!$
!!$    real(r_def), intent(in) :: lambda_centre_dec
!!$    real(r_def), intent(in) :: mountain_height
!!$    real(r_def), intent(in) :: osc_half_width_dec
!!$    real(r_def), intent(in) :: phi_centre_dec
!!$    real(r_def), intent(in) :: radius_dec
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_orography_dcmip200_spherical_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&orography_dcmip200_spherical")' )
!!$    write( temporary_unit, '("lambda_centre_dec = ", E14.7)' ) lambda_centre_dec
!!$    write( temporary_unit, '("mountain_height = ", E14.7)' ) mountain_height
!!$    write( temporary_unit, '("osc_half_width_dec = ", E14.7)' ) osc_half_width_dec
!!$    write( temporary_unit, '("phi_centre_dec = ", E14.7)' ) phi_centre_dec
!!$    write( temporary_unit, '("radius_dec = ", E14.7)' ) radius_dec
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_orography_dcmip200_spherical_namelist( temporary_unit, local_rank )
!!$    call postprocess_orography_dcmip200_spherical_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_orography_dcmip200_spherical_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_orography_dcmip200_spherical_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_orography_schar_cartesian_config( direction, &
!!$                                                     half_width_x, &
!!$                                                     half_width_y, &
!!$                                                     mountain_height, &
!!$                                                     wavelength, &
!!$                                                     x_centre, &
!!$                                                     y_centre )
!!$
!!$    use orography_schar_cartesian_config_mod, only : read_orography_schar_cartesian_namelist, &
!!$                                                     postprocess_orography_schar_cartesian_namelist, &
!!$                                                     key_from_direction, &
!!$                                                     direction_from_key
!!$
!!$    implicit none
!!$
!!$    integer(i_native), intent(in) :: direction
!!$    real(r_def), intent(in) :: half_width_x
!!$    real(r_def), intent(in) :: half_width_y
!!$    real(r_def), intent(in) :: mountain_height
!!$    real(r_def), intent(in) :: wavelength
!!$    real(r_def), intent(in) :: x_centre
!!$    real(r_def), intent(in) :: y_centre
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_orography_schar_cartesian_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&orography_schar_cartesian")' )
!!$    write( temporary_unit, '("direction = ''", A, "''")' ) key_from_direction( direction )
!!$    write( temporary_unit, '("half_width_x = ", E14.7)' ) half_width_x
!!$    write( temporary_unit, '("half_width_y = ", E14.7)' ) half_width_y
!!$    write( temporary_unit, '("mountain_height = ", E14.7)' ) mountain_height
!!$    write( temporary_unit, '("wavelength = ", E14.7)' ) wavelength
!!$    write( temporary_unit, '("x_centre = ", E14.7)' ) x_centre
!!$    write( temporary_unit, '("y_centre = ", E14.7)' ) y_centre
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_orography_schar_cartesian_namelist( temporary_unit, local_rank )
!!$    call postprocess_orography_schar_cartesian_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_orography_schar_cartesian_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_orography_schar_cartesian_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_orography_schar_spherical_config( half_width, &
!!$                                                     lambda_centre_dec, &
!!$                                                     mountain_height, &
!!$                                                     phi_centre_dec, &
!!$                                                     wavelength )
!!$
!!$    use orography_schar_spherical_config_mod, only : read_orography_schar_spherical_namelist, &
!!$                                                     postprocess_orography_schar_spherical_namelist
!!$
!!$    implicit none
!!$
!!$    real(r_def), intent(in) :: half_width
!!$    real(r_def), intent(in) :: lambda_centre_dec
!!$    real(r_def), intent(in) :: mountain_height
!!$    real(r_def), intent(in) :: phi_centre_dec
!!$    real(r_def), intent(in) :: wavelength
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_orography_schar_spherical_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&orography_schar_spherical")' )
!!$    write( temporary_unit, '("half_width = ", E14.7)' ) half_width
!!$    write( temporary_unit, '("lambda_centre_dec = ", E14.7)' ) lambda_centre_dec
!!$    write( temporary_unit, '("mountain_height = ", E14.7)' ) mountain_height
!!$    write( temporary_unit, '("phi_centre_dec = ", E14.7)' ) phi_centre_dec
!!$    write( temporary_unit, '("wavelength = ", E14.7)' ) wavelength
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_orography_schar_spherical_namelist( temporary_unit, local_rank )
!!$    call postprocess_orography_schar_spherical_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_orography_schar_spherical_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_orography_schar_spherical_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_partitioning_config( panel_decomposition, &
!!$                                        panel_xproc, &
!!$                                        panel_yproc, &
!!$                                        partitioner )
!!$
!!$    use partitioning_config_mod, only : read_partitioning_namelist, &
!!$                                        postprocess_partitioning_namelist, &
!!$                                        key_from_panel_decomposition, &
!!$                                        panel_decomposition_from_key, &
!!$                                        key_from_partitioner, &
!!$                                        partitioner_from_key
!!$
!!$    implicit none
!!$
!!$    integer(i_native), intent(in) :: panel_decomposition
!!$    integer(i_def), intent(in) :: panel_xproc
!!$    integer(i_def), intent(in) :: panel_yproc
!!$    integer(i_native), intent(in) :: partitioner
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_partitioning_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&partitioning")' )
!!$    write( temporary_unit, '("panel_decomposition = ''", A, "''")' ) key_from_panel_decomposition( panel_decomposition )
!!$    write( temporary_unit, '("panel_xproc = ", I0)' ) panel_xproc
!!$    write( temporary_unit, '("panel_yproc = ", I0)' ) panel_yproc
!!$    write( temporary_unit, '("partitioner = ''", A, "''")' ) key_from_partitioner( partitioner )
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_partitioning_namelist( temporary_unit, local_rank )
!!$    call postprocess_partitioning_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_partitioning_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_partitioning_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_physics_config( blayer_placement, &
!!$                                   convection_placement, &
!!$                                   lowest_level, &
!!$                                   microphysics_placement, &
!!$                                   orographic_drag_placement, &
!!$                                   radiation_placement, &
!!$                                   sample_physics_scalars, &
!!$                                   sample_physics_winds, &
!!$                                   spectral_gwd_placement, &
!!$                                   stochastic_physics_placement )
!!$
!!$    use physics_config_mod, only : read_physics_namelist, &
!!$                                   postprocess_physics_namelist, &
!!$                                   key_from_blayer_placement, &
!!$                                   blayer_placement_from_key, &
!!$                                   key_from_convection_placement, &
!!$                                   convection_placement_from_key, &
!!$                                   key_from_lowest_level, &
!!$                                   lowest_level_from_key, &
!!$                                   key_from_microphysics_placement, &
!!$                                   microphysics_placement_from_key, &
!!$                                   key_from_orographic_drag_placement, &
!!$                                   orographic_drag_placement_from_key, &
!!$                                   key_from_radiation_placement, &
!!$                                   radiation_placement_from_key, &
!!$                                   key_from_spectral_gwd_placement, &
!!$                                   spectral_gwd_placement_from_key, &
!!$                                   key_from_stochastic_physics_placement, &
!!$                                   stochastic_physics_placement_from_key
!!$
!!$    implicit none
!!$
!!$    integer(i_native), intent(in) :: blayer_placement
!!$    integer(i_native), intent(in) :: convection_placement
!!$    integer(i_native), intent(in) :: lowest_level
!!$    integer(i_native), intent(in) :: microphysics_placement
!!$    integer(i_native), intent(in) :: orographic_drag_placement
!!$    integer(i_native), intent(in) :: radiation_placement
!!$    logical(l_def), intent(in) :: sample_physics_scalars
!!$    logical(l_def), intent(in) :: sample_physics_winds
!!$    integer(i_native), intent(in) :: spectral_gwd_placement
!!$    integer(i_native), intent(in) :: stochastic_physics_placement
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_physics_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&physics")' )
!!$    write( temporary_unit, '("blayer_placement = ''", A, "''")' ) key_from_blayer_placement( blayer_placement )
!!$    write( temporary_unit, '("convection_placement = ''", A, "''")' ) key_from_convection_placement( convection_placement )
!!$    write( temporary_unit, '("lowest_level = ''", A, "''")' ) key_from_lowest_level( lowest_level )
!!$    write( temporary_unit, '("microphysics_placement = ''", A, "''")' ) key_from_microphysics_placement( microphysics_placement )
!!$    write( temporary_unit, '("orographic_drag_placement = ''", A, "''")' ) key_from_orographic_drag_placement( orographic_drag_placement )
!!$    write( temporary_unit, '("radiation_placement = ''", A, "''")' ) key_from_radiation_placement( radiation_placement )
!!$    write( temporary_unit, '("sample_physics_scalars = ", L2)' ) sample_physics_scalars
!!$    write( temporary_unit, '("sample_physics_winds = ", L2)' ) sample_physics_winds
!!$    write( temporary_unit, '("spectral_gwd_placement = ''", A, "''")' ) key_from_spectral_gwd_placement( spectral_gwd_placement )
!!$    write( temporary_unit, '("stochastic_physics_placement = ''", A, "''")' ) key_from_stochastic_physics_placement( stochastic_physics_placement )
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_physics_namelist( temporary_unit, local_rank )
!!$    call postprocess_physics_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_physics_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_physics_config

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine feign_planet_config( cp, &
                                  gravity, &
                                  omega, &
                                  p_zero, &
                                  radius, &
                                  rd, &
                                  scaling_factor )

    use planet_config_mod, only : read_planet_namelist, &
                                  postprocess_planet_namelist

    implicit none

    real(r_def), intent(in) :: cp
    real(r_def), intent(in) :: gravity
    real(r_def), intent(in) :: omega
    real(r_def), intent(in) :: p_zero
    real(r_def), intent(in) :: radius
    real(r_def), intent(in) :: rd
    real(r_def), intent(in) :: scaling_factor

    integer(i_native)  :: condition
    integer(i_native)  :: i
    character(str_max_filename) :: tmp_str
    character(str_def) :: fmt_str

    if (local_rank == -1) then
      local_rank = get_comm_rank()
    end if

    open( temporary_unit, status='scratch', action='readwrite', &
          iostat=condition )

    if (condition /= 0) then
      write( 6, '("feign_planet_config: ", I0)' ) condition
      stop
    end if

    write( temporary_unit, '("&planet")' )
    write( temporary_unit, '("cp = ", E14.7)' ) cp
    write( temporary_unit, '("gravity = ", E14.7)' ) gravity
    write( temporary_unit, '("omega = ", E14.7)' ) omega
    write( temporary_unit, '("p_zero = ", E14.7)' ) p_zero
    write( temporary_unit, '("radius = ", E14.7)' ) radius
    write( temporary_unit, '("rd = ", E14.7)' ) rd
    write( temporary_unit, '("scaling_factor = ", E14.7)' ) scaling_factor
    write( temporary_unit, '("/")' )

    rewind(temporary_unit)
    call read_planet_namelist( temporary_unit, local_rank )
    call postprocess_planet_namelist()
    close(temporary_unit, iostat=condition )
    if (condition /= 0) stop &
        'feign_planet_config: '// &
        'Unable to close temporary file'

  end subroutine feign_planet_config

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_radiation_config( cloud_inhomogeneity, &
!!$                                     cloud_overlap, &
!!$                                     cloud_representation, &
!!$                                     cloud_vertical_decorr, &
!!$                                     constant_droplet_effective_radius, &
!!$                                     droplet_effective_radius, &
!!$                                     i_cloud_ice_type_lw, &
!!$                                     i_cloud_ice_type_lwinc, &
!!$                                     i_cloud_ice_type_sw, &
!!$                                     i_cloud_ice_type_swinc, &
!!$                                     i_cloud_liq_type_lw, &
!!$                                     i_cloud_liq_type_lwinc, &
!!$                                     i_cloud_liq_type_sw, &
!!$                                     i_cloud_liq_type_swinc, &
!!$                                     l_cfc113_lw, &
!!$                                     l_cfc11_lw, &
!!$                                     l_cfc12_lw, &
!!$                                     l_ch4_lw, &
!!$                                     l_ch4_sw, &
!!$                                     l_co2_lw, &
!!$                                     l_co2_sw, &
!!$                                     l_continuum_lw, &
!!$                                     l_continuum_sw, &
!!$                                     l_h2o_lw, &
!!$                                     l_h2o_sw, &
!!$                                     l_hcfc22_lw, &
!!$                                     l_hfc134a_lw, &
!!$                                     l_inc_radstep, &
!!$                                     l_n2o_lw, &
!!$                                     l_n2o_sw, &
!!$                                     l_o2_sw, &
!!$                                     l_o3_lw, &
!!$                                     l_o3_sw, &
!!$                                     l_planet_grey_surface, &
!!$                                     l_rayleigh_sw, &
!!$                                     l_trans_zen_correction, &
!!$                                     mcica_data_file, &
!!$                                     n_inc_radstep, &
!!$                                     n_radstep, &
!!$                                     planet_albedo, &
!!$                                     planet_emissivity, &
!!$                                     spectral_file_lw, &
!!$                                     spectral_file_lwinc, &
!!$                                     spectral_file_sw, &
!!$                                     spectral_file_swinc )
!!$
!!$    use radiation_config_mod, only : read_radiation_namelist, &
!!$                                     postprocess_radiation_namelist, &
!!$                                     key_from_cloud_inhomogeneity, &
!!$                                     cloud_inhomogeneity_from_key, &
!!$                                     key_from_cloud_overlap, &
!!$                                     cloud_overlap_from_key, &
!!$                                     key_from_cloud_representation, &
!!$                                     cloud_representation_from_key, &
!!$                                     key_from_droplet_effective_radius, &
!!$                                     droplet_effective_radius_from_key
!!$
!!$    implicit none
!!$
!!$    integer(i_native), intent(in) :: cloud_inhomogeneity
!!$    integer(i_native), intent(in) :: cloud_overlap
!!$    integer(i_native), intent(in) :: cloud_representation
!!$    real(r_def), intent(in) :: cloud_vertical_decorr
!!$    real(r_def), intent(in) :: constant_droplet_effective_radius
!!$    integer(i_native), intent(in) :: droplet_effective_radius
!!$    integer(i_def), intent(in) :: i_cloud_ice_type_lw
!!$    integer(i_def), intent(in) :: i_cloud_ice_type_lwinc
!!$    integer(i_def), intent(in) :: i_cloud_ice_type_sw
!!$    integer(i_def), intent(in) :: i_cloud_ice_type_swinc
!!$    integer(i_def), intent(in) :: i_cloud_liq_type_lw
!!$    integer(i_def), intent(in) :: i_cloud_liq_type_lwinc
!!$    integer(i_def), intent(in) :: i_cloud_liq_type_sw
!!$    integer(i_def), intent(in) :: i_cloud_liq_type_swinc
!!$    logical(l_def), intent(in) :: l_cfc113_lw
!!$    logical(l_def), intent(in) :: l_cfc11_lw
!!$    logical(l_def), intent(in) :: l_cfc12_lw
!!$    logical(l_def), intent(in) :: l_ch4_lw
!!$    logical(l_def), intent(in) :: l_ch4_sw
!!$    logical(l_def), intent(in) :: l_co2_lw
!!$    logical(l_def), intent(in) :: l_co2_sw
!!$    logical(l_def), intent(in) :: l_continuum_lw
!!$    logical(l_def), intent(in) :: l_continuum_sw
!!$    logical(l_def), intent(in) :: l_h2o_lw
!!$    logical(l_def), intent(in) :: l_h2o_sw
!!$    logical(l_def), intent(in) :: l_hcfc22_lw
!!$    logical(l_def), intent(in) :: l_hfc134a_lw
!!$    logical(l_def), intent(in) :: l_inc_radstep
!!$    logical(l_def), intent(in) :: l_n2o_lw
!!$    logical(l_def), intent(in) :: l_n2o_sw
!!$    logical(l_def), intent(in) :: l_o2_sw
!!$    logical(l_def), intent(in) :: l_o3_lw
!!$    logical(l_def), intent(in) :: l_o3_sw
!!$    logical(l_def), intent(in) :: l_planet_grey_surface
!!$    logical(l_def), intent(in) :: l_rayleigh_sw
!!$    logical(l_def), intent(in) :: l_trans_zen_correction
!!$    character(*), intent(in) :: mcica_data_file
!!$    integer(i_def), intent(in) :: n_inc_radstep
!!$    integer(i_def), intent(in) :: n_radstep
!!$    real(r_def), intent(in) :: planet_albedo
!!$    real(r_def), intent(in) :: planet_emissivity
!!$    character(*), intent(in) :: spectral_file_lw
!!$    character(*), intent(in) :: spectral_file_lwinc
!!$    character(*), intent(in) :: spectral_file_sw
!!$    character(*), intent(in) :: spectral_file_swinc
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_radiation_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&radiation")' )
!!$    write( temporary_unit, '("cloud_inhomogeneity = ''", A, "''")' ) key_from_cloud_inhomogeneity( cloud_inhomogeneity )
!!$    write( temporary_unit, '("cloud_overlap = ''", A, "''")' ) key_from_cloud_overlap( cloud_overlap )
!!$    write( temporary_unit, '("cloud_representation = ''", A, "''")' ) key_from_cloud_representation( cloud_representation )
!!$    write( temporary_unit, '("cloud_vertical_decorr = ", E14.7)' ) cloud_vertical_decorr
!!$    write( temporary_unit, '("constant_droplet_effective_radius = ", E14.7)' ) constant_droplet_effective_radius
!!$    write( temporary_unit, '("droplet_effective_radius = ''", A, "''")' ) key_from_droplet_effective_radius( droplet_effective_radius )
!!$    write( temporary_unit, '("i_cloud_ice_type_lw = ", I0)' ) i_cloud_ice_type_lw
!!$    write( temporary_unit, '("i_cloud_ice_type_lwinc = ", I0)' ) i_cloud_ice_type_lwinc
!!$    write( temporary_unit, '("i_cloud_ice_type_sw = ", I0)' ) i_cloud_ice_type_sw
!!$    write( temporary_unit, '("i_cloud_ice_type_swinc = ", I0)' ) i_cloud_ice_type_swinc
!!$    write( temporary_unit, '("i_cloud_liq_type_lw = ", I0)' ) i_cloud_liq_type_lw
!!$    write( temporary_unit, '("i_cloud_liq_type_lwinc = ", I0)' ) i_cloud_liq_type_lwinc
!!$    write( temporary_unit, '("i_cloud_liq_type_sw = ", I0)' ) i_cloud_liq_type_sw
!!$    write( temporary_unit, '("i_cloud_liq_type_swinc = ", I0)' ) i_cloud_liq_type_swinc
!!$    write( temporary_unit, '("l_cfc113_lw = ", L2)' ) l_cfc113_lw
!!$    write( temporary_unit, '("l_cfc11_lw = ", L2)' ) l_cfc11_lw
!!$    write( temporary_unit, '("l_cfc12_lw = ", L2)' ) l_cfc12_lw
!!$    write( temporary_unit, '("l_ch4_lw = ", L2)' ) l_ch4_lw
!!$    write( temporary_unit, '("l_ch4_sw = ", L2)' ) l_ch4_sw
!!$    write( temporary_unit, '("l_co2_lw = ", L2)' ) l_co2_lw
!!$    write( temporary_unit, '("l_co2_sw = ", L2)' ) l_co2_sw
!!$    write( temporary_unit, '("l_continuum_lw = ", L2)' ) l_continuum_lw
!!$    write( temporary_unit, '("l_continuum_sw = ", L2)' ) l_continuum_sw
!!$    write( temporary_unit, '("l_h2o_lw = ", L2)' ) l_h2o_lw
!!$    write( temporary_unit, '("l_h2o_sw = ", L2)' ) l_h2o_sw
!!$    write( temporary_unit, '("l_hcfc22_lw = ", L2)' ) l_hcfc22_lw
!!$    write( temporary_unit, '("l_hfc134a_lw = ", L2)' ) l_hfc134a_lw
!!$    write( temporary_unit, '("l_inc_radstep = ", L2)' ) l_inc_radstep
!!$    write( temporary_unit, '("l_n2o_lw = ", L2)' ) l_n2o_lw
!!$    write( temporary_unit, '("l_n2o_sw = ", L2)' ) l_n2o_sw
!!$    write( temporary_unit, '("l_o2_sw = ", L2)' ) l_o2_sw
!!$    write( temporary_unit, '("l_o3_lw = ", L2)' ) l_o3_lw
!!$    write( temporary_unit, '("l_o3_sw = ", L2)' ) l_o3_sw
!!$    write( temporary_unit, '("l_planet_grey_surface = ", L2)' ) l_planet_grey_surface
!!$    write( temporary_unit, '("l_rayleigh_sw = ", L2)' ) l_rayleigh_sw
!!$    write( temporary_unit, '("l_trans_zen_correction = ", L2)' ) l_trans_zen_correction
!!$    write( temporary_unit, '("mcica_data_file = ''", A, "''")' ) mcica_data_file
!!$    write( temporary_unit, '("n_inc_radstep = ", I0)' ) n_inc_radstep
!!$    write( temporary_unit, '("n_radstep = ", I0)' ) n_radstep
!!$    write( temporary_unit, '("planet_albedo = ", E14.7)' ) planet_albedo
!!$    write( temporary_unit, '("planet_emissivity = ", E14.7)' ) planet_emissivity
!!$    write( temporary_unit, '("spectral_file_lw = ''", A, "''")' ) spectral_file_lw
!!$    write( temporary_unit, '("spectral_file_lwinc = ''", A, "''")' ) spectral_file_lwinc
!!$    write( temporary_unit, '("spectral_file_sw = ''", A, "''")' ) spectral_file_sw
!!$    write( temporary_unit, '("spectral_file_swinc = ''", A, "''")' ) spectral_file_swinc
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_radiation_namelist( temporary_unit, local_rank )
!!$    call postprocess_radiation_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_radiation_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_radiation_config

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_section_choice_config( aerosol, &
!!$                                          boundary_layer, &
!!$                                          cloud, &
!!$                                          convection, &
!!$                                          dynamics, &
!!$                                          external_forcing, &
!!$                                          methane_oxidation, &
!!$                                          microphysics, &
!!$                                          orographic_drag, &
!!$                                          radiation, &
!!$                                          spectral_gwd, &
!!$                                          stochastic_physics, &
!!$                                          surface )
!!$
!!$    use section_choice_config_mod, only : read_section_choice_namelist, &
!!$                                          postprocess_section_choice_namelist, &
!!$                                          key_from_aerosol, &
!!$                                          aerosol_from_key, &
!!$                                          key_from_boundary_layer, &
!!$                                          boundary_layer_from_key, &
!!$                                          key_from_cloud, &
!!$                                          cloud_from_key, &
!!$                                          key_from_convection, &
!!$                                          convection_from_key, &
!!$                                          key_from_dynamics, &
!!$                                          dynamics_from_key, &
!!$                                          key_from_microphysics, &
!!$                                          microphysics_from_key, &
!!$                                          key_from_orographic_drag, &
!!$                                          orographic_drag_from_key, &
!!$                                          key_from_radiation, &
!!$                                          radiation_from_key, &
!!$                                          key_from_spectral_gwd, &
!!$                                          spectral_gwd_from_key, &
!!$                                          key_from_stochastic_physics, &
!!$                                          stochastic_physics_from_key, &
!!$                                          key_from_surface, &
!!$                                          surface_from_key
!!$
!!$    implicit none
!!$
!!$    integer(i_native), intent(in) :: aerosol
!!$    integer(i_native), intent(in) :: boundary_layer
!!$    integer(i_native), intent(in) :: cloud
!!$    integer(i_native), intent(in) :: convection
!!$    integer(i_native), intent(in) :: dynamics
!!$    logical(l_def), intent(in) :: external_forcing
!!$    logical(l_def), intent(in) :: methane_oxidation
!!$    integer(i_native), intent(in) :: microphysics
!!$    integer(i_native), intent(in) :: orographic_drag
!!$    integer(i_native), intent(in) :: radiation
!!$    integer(i_native), intent(in) :: spectral_gwd
!!$    integer(i_native), intent(in) :: stochastic_physics
!!$    integer(i_native), intent(in) :: surface
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_section_choice_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&section_choice")' )
!!$    write( temporary_unit, '("aerosol = ''", A, "''")' ) key_from_aerosol( aerosol )
!!$    write( temporary_unit, '("boundary_layer = ''", A, "''")' ) key_from_boundary_layer( boundary_layer )
!!$    write( temporary_unit, '("cloud = ''", A, "''")' ) key_from_cloud( cloud )
!!$    write( temporary_unit, '("convection = ''", A, "''")' ) key_from_convection( convection )
!!$    write( temporary_unit, '("dynamics = ''", A, "''")' ) key_from_dynamics( dynamics )
!!$    write( temporary_unit, '("external_forcing = ", L2)' ) external_forcing
!!$    write( temporary_unit, '("methane_oxidation = ", L2)' ) methane_oxidation
!!$    write( temporary_unit, '("microphysics = ''", A, "''")' ) key_from_microphysics( microphysics )
!!$    write( temporary_unit, '("orographic_drag = ''", A, "''")' ) key_from_orographic_drag( orographic_drag )
!!$    write( temporary_unit, '("radiation = ''", A, "''")' ) key_from_radiation( radiation )
!!$    write( temporary_unit, '("spectral_gwd = ''", A, "''")' ) key_from_spectral_gwd( spectral_gwd )
!!$    write( temporary_unit, '("stochastic_physics = ''", A, "''")' ) key_from_stochastic_physics( stochastic_physics )
!!$    write( temporary_unit, '("surface = ''", A, "''")' ) key_from_surface( surface )
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_section_choice_namelist( temporary_unit, local_rank )
!!$    call postprocess_section_choice_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_section_choice_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_section_choice_config

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_solver_config( diagnostic_norm, &
!!$                                  fixed_iterations, &
!!$                                  gcrk, &
!!$                                  maximum_iterations, &
!!$                                  method, &
!!$                                  preconditioner, &
!!$                                  tolerance )
!!$
!!$    use solver_config_mod, only : read_solver_namelist, &
!!$                                  postprocess_solver_namelist, &
!!$                                  key_from_method, &
!!$                                  method_from_key, &
!!$                                  key_from_preconditioner, &
!!$                                  preconditioner_from_key
!!$
!!$    implicit none
!!$
!!$    logical(l_def), intent(in) :: diagnostic_norm
!!$    integer(i_def), intent(in) :: fixed_iterations
!!$    integer(i_def), intent(in) :: gcrk
!!$    integer(i_def), intent(in) :: maximum_iterations
!!$    integer(i_native), intent(in) :: method
!!$    integer(i_native), intent(in) :: preconditioner
!!$    real(r_def), intent(in) :: tolerance
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_solver_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&solver")' )
!!$    write( temporary_unit, '("diagnostic_norm = ", L2)' ) diagnostic_norm
!!$    write( temporary_unit, '("fixed_iterations = ", I0)' ) fixed_iterations
!!$    write( temporary_unit, '("gcrk = ", I0)' ) gcrk
!!$    write( temporary_unit, '("maximum_iterations = ", I0)' ) maximum_iterations
!!$    write( temporary_unit, '("method = ''", A, "''")' ) key_from_method( method )
!!$    write( temporary_unit, '("preconditioner = ''", A, "''")' ) key_from_preconditioner( preconditioner )
!!$    write( temporary_unit, '("tolerance = ", E14.7)' ) tolerance
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_solver_namelist( temporary_unit, local_rank )
!!$    call postprocess_solver_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_solver_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_solver_config

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_spectral_gwd_config( add_cgw, &
!!$                                        cgw_scale_factor, &
!!$                                        ussp_heating, &
!!$                                        ussp_launch_factor, &
!!$                                        wavelstar )
!!$
!!$    use spectral_gwd_config_mod, only : read_spectral_gwd_namelist, &
!!$                                        postprocess_spectral_gwd_namelist
!!$
!!$    implicit none
!!$
!!$    logical(l_def), intent(in) :: add_cgw
!!$    real(r_def), intent(in) :: cgw_scale_factor
!!$    logical(l_def), intent(in) :: ussp_heating
!!$    real(r_def), intent(in) :: ussp_launch_factor
!!$    real(r_def), intent(in) :: wavelstar
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_spectral_gwd_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&spectral_gwd")' )
!!$    write( temporary_unit, '("add_cgw = ", L2)' ) add_cgw
!!$    write( temporary_unit, '("cgw_scale_factor = ", E14.7)' ) cgw_scale_factor
!!$    write( temporary_unit, '("ussp_heating = ", L2)' ) ussp_heating
!!$    write( temporary_unit, '("ussp_launch_factor = ", E14.7)' ) ussp_launch_factor
!!$    write( temporary_unit, '("wavelstar = ", E14.7)' ) wavelstar
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_spectral_gwd_namelist( temporary_unit, local_rank )
!!$    call postprocess_spectral_gwd_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_spectral_gwd_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_spectral_gwd_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_star_config( stellar_constant, &
!!$                                stellar_radius )
!!$
!!$    use star_config_mod, only : read_star_namelist, &
!!$                                postprocess_star_namelist
!!$
!!$    implicit none
!!$
!!$    real(r_def), intent(in) :: stellar_constant
!!$    real(r_def), intent(in) :: stellar_radius
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_star_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&star")' )
!!$    write( temporary_unit, '("stellar_constant = ", E14.7)' ) stellar_constant
!!$    write( temporary_unit, '("stellar_radius = ", E14.7)' ) stellar_radius
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_star_namelist( temporary_unit, local_rank )
!!$    call postprocess_star_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_star_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_star_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_stochastic_physics_config( spt_add_increments, &
!!$                                              spt_convection_cfl_limit, &
!!$                                              spt_decorrelation_time, &
!!$                                              spt_level_begin_tappering_bottom, &
!!$                                              spt_level_begin_tappering_top, &
!!$                                              spt_level_bottom, &
!!$                                              spt_level_top, &
!!$                                              spt_moisture_conservation, &
!!$                                              spt_mse_conservation, &
!!$                                              spt_n_max, &
!!$                                              spt_n_smoothing_iters, &
!!$                                              spt_orog_forcing_pattern_thresh, &
!!$                                              spt_stddev_convection, &
!!$                                              spt_stddev_microphysics, &
!!$                                              spt_stddev_orog_thres, &
!!$                                              spt_stddev_radiation, &
!!$                                              spt_use_convection, &
!!$                                              spt_use_microphysics, &
!!$                                              spt_use_radiation, &
!!$                                              use_spt )
!!$
!!$    use stochastic_physics_config_mod, only : read_stochastic_physics_namelist, &
!!$                                              postprocess_stochastic_physics_namelist
!!$
!!$    implicit none
!!$
!!$    logical(l_def), intent(in) :: spt_add_increments
!!$    logical(l_def), intent(in) :: spt_convection_cfl_limit
!!$    real(r_def), intent(in) :: spt_decorrelation_time
!!$    integer(i_def), intent(in) :: spt_level_begin_tappering_bottom
!!$    integer(i_def), intent(in) :: spt_level_begin_tappering_top
!!$    integer(i_def), intent(in) :: spt_level_bottom
!!$    integer(i_def), intent(in) :: spt_level_top
!!$    logical(l_def), intent(in) :: spt_moisture_conservation
!!$    logical(l_def), intent(in) :: spt_mse_conservation
!!$    integer(i_def), intent(in) :: spt_n_max
!!$    integer(i_def), intent(in) :: spt_n_smoothing_iters
!!$    real(r_def), intent(in) :: spt_orog_forcing_pattern_thresh
!!$    real(r_def), intent(in) :: spt_stddev_convection
!!$    real(r_def), intent(in) :: spt_stddev_microphysics
!!$    real(r_def), intent(in) :: spt_stddev_orog_thres
!!$    real(r_def), intent(in) :: spt_stddev_radiation
!!$    logical(l_def), intent(in) :: spt_use_convection
!!$    logical(l_def), intent(in) :: spt_use_microphysics
!!$    logical(l_def), intent(in) :: spt_use_radiation
!!$    logical(l_def), intent(in) :: use_spt
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_stochastic_physics_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&stochastic_physics")' )
!!$    write( temporary_unit, '("spt_add_increments = ", L2)' ) spt_add_increments
!!$    write( temporary_unit, '("spt_convection_cfl_limit = ", L2)' ) spt_convection_cfl_limit
!!$    write( temporary_unit, '("spt_decorrelation_time = ", E14.7)' ) spt_decorrelation_time
!!$    write( temporary_unit, '("spt_level_begin_tappering_bottom = ", I0)' ) spt_level_begin_tappering_bottom
!!$    write( temporary_unit, '("spt_level_begin_tappering_top = ", I0)' ) spt_level_begin_tappering_top
!!$    write( temporary_unit, '("spt_level_bottom = ", I0)' ) spt_level_bottom
!!$    write( temporary_unit, '("spt_level_top = ", I0)' ) spt_level_top
!!$    write( temporary_unit, '("spt_moisture_conservation = ", L2)' ) spt_moisture_conservation
!!$    write( temporary_unit, '("spt_mse_conservation = ", L2)' ) spt_mse_conservation
!!$    write( temporary_unit, '("spt_n_max = ", I0)' ) spt_n_max
!!$    write( temporary_unit, '("spt_n_smoothing_iters = ", I0)' ) spt_n_smoothing_iters
!!$    write( temporary_unit, '("spt_orog_forcing_pattern_thresh = ", E14.7)' ) spt_orog_forcing_pattern_thresh
!!$    write( temporary_unit, '("spt_stddev_convection = ", E14.7)' ) spt_stddev_convection
!!$    write( temporary_unit, '("spt_stddev_microphysics = ", E14.7)' ) spt_stddev_microphysics
!!$    write( temporary_unit, '("spt_stddev_orog_thres = ", E14.7)' ) spt_stddev_orog_thres
!!$    write( temporary_unit, '("spt_stddev_radiation = ", E14.7)' ) spt_stddev_radiation
!!$    write( temporary_unit, '("spt_use_convection = ", L2)' ) spt_use_convection
!!$    write( temporary_unit, '("spt_use_microphysics = ", L2)' ) spt_use_microphysics
!!$    write( temporary_unit, '("spt_use_radiation = ", L2)' ) spt_use_radiation
!!$    write( temporary_unit, '("use_spt = ", L2)' ) use_spt
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_stochastic_physics_namelist( temporary_unit, local_rank )
!!$    call postprocess_stochastic_physics_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_stochastic_physics_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_stochastic_physics_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_subgrid_config( calculate_detj, &
!!$                                   dep_pt_stencil_extent, &
!!$                                   rho_approximation, &
!!$                                   rho_approximation_stencil_extent )
!!$
!!$    use subgrid_config_mod, only : read_subgrid_namelist, &
!!$                                   postprocess_subgrid_namelist, &
!!$                                   key_from_calculate_detj, &
!!$                                   calculate_detj_from_key, &
!!$                                   key_from_rho_approximation, &
!!$                                   rho_approximation_from_key
!!$
!!$    implicit none
!!$
!!$    integer(i_native), intent(in) :: calculate_detj
!!$    integer(i_def), intent(in) :: dep_pt_stencil_extent
!!$    integer(i_native), intent(in) :: rho_approximation
!!$    integer(i_def), intent(in) :: rho_approximation_stencil_extent
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_subgrid_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&subgrid")' )
!!$    write( temporary_unit, '("calculate_detj = ''", A, "''")' ) key_from_calculate_detj( calculate_detj )
!!$    write( temporary_unit, '("dep_pt_stencil_extent = ", I0)' ) dep_pt_stencil_extent
!!$    write( temporary_unit, '("rho_approximation = ''", A, "''")' ) key_from_rho_approximation( rho_approximation )
!!$    write( temporary_unit, '("rho_approximation_stencil_extent = ", I0)' ) rho_approximation_stencil_extent
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_subgrid_namelist( temporary_unit, local_rank )
!!$    call postprocess_subgrid_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_subgrid_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_subgrid_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_surface_config( alb_leaf_nir, &
!!$                                   alb_leaf_vis, &
!!$                                   alb_sice_melt, &
!!$                                   alb_snocov_max, &
!!$                                   alb_snocov_nvg, &
!!$                                   alb_snofree_nvg, &
!!$                                   albedo_obs, &
!!$                                   basal_melting, &
!!$                                   blue_sky_alb, &
!!$                                   buddy_sea, &
!!$                                   can_cap_nvg, &
!!$                                   can_clump, &
!!$                                   cansnowpft, &
!!$                                   cdn_hw_sea, &
!!$                                   cdn_max_sea, &
!!$                                   dpsids_dsdz, &
!!$                                   dt_ice_albedo, &
!!$                                   emis_sea, &
!!$                                   emis_sice, &
!!$                                   evap_scale_sea, &
!!$                                   fixed_sea_albedo, &
!!$                                   fsmc_p0, &
!!$                                   grain_growth, &
!!$                                   heat_cap_nvg, &
!!$                                   heat_cap_sea, &
!!$                                   i_high_wind_drag, &
!!$                                   iceformdrag_lupkes, &
!!$                                   knl, &
!!$                                   l_10m_neut, &
!!$                                   l_hapke_soil, &
!!$                                   l_partition_albsoil, &
!!$                                   l_var_rainfrac, &
!!$                                   l_vg_soil, &
!!$                                   light_extinct, &
!!$                                   n_lai_exposed, &
!!$                                   n_sea_ice_tile, &
!!$                                   non_iso_scatter, &
!!$                                   ratio_albsoil, &
!!$                                   relayer_opt, &
!!$                                   rho_snow_fresh, &
!!$                                   scat_coef_nir, &
!!$                                   scat_coef_vis, &
!!$                                   sea_alb_method, &
!!$                                   sea_alb_var_chl, &
!!$                                   sea_surf_alg, &
!!$                                   sice_heatflux, &
!!$                                   soil_sat_down, &
!!$                                   stability_lupkes, &
!!$                                   surf_tile_fracs, &
!!$                                   swdn_frac_albsoil, &
!!$                                   therm_cond_sea, &
!!$                                   therm_cond_sice, &
!!$                                   therm_cond_sice_snow, &
!!$                                   u_cdn_hw, &
!!$                                   u_cdn_max, &
!!$                                   unload_rate_u, &
!!$                                   use_hydrology, &
!!$                                   use_variable_sst, &
!!$                                   z0hm_ratio_nveg, &
!!$                                   z0hm_ratio_pft, &
!!$                                   z0v )
!!$
!!$    use surface_config_mod, only : read_surface_namelist, &
!!$                                   postprocess_surface_namelist, &
!!$                                   key_from_basal_melting, &
!!$                                   basal_melting_from_key, &
!!$                                   key_from_buddy_sea, &
!!$                                   buddy_sea_from_key, &
!!$                                   key_from_grain_growth, &
!!$                                   grain_growth_from_key, &
!!$                                   key_from_i_high_wind_drag, &
!!$                                   i_high_wind_drag_from_key, &
!!$                                   key_from_relayer_opt, &
!!$                                   relayer_opt_from_key, &
!!$                                   key_from_sea_alb_method, &
!!$                                   sea_alb_method_from_key, &
!!$                                   key_from_sea_surf_alg, &
!!$                                   sea_surf_alg_from_key
!!$
!!$    implicit none
!!$
!!$    real(r_def), intent(in) :: alb_leaf_nir(:)
!!$    real(r_def), intent(in) :: alb_leaf_vis(:)
!!$    real(r_def), intent(in) :: alb_sice_melt
!!$    real(r_def), intent(in) :: alb_snocov_max(:)
!!$    real(r_def), intent(in) :: alb_snocov_nvg(:)
!!$    real(r_def), intent(in) :: alb_snofree_nvg(:)
!!$    logical(l_def), intent(in) :: albedo_obs
!!$    integer(i_native), intent(in) :: basal_melting
!!$    logical(l_def), intent(in) :: blue_sky_alb
!!$    integer(i_native), intent(in) :: buddy_sea
!!$    real(r_def), intent(in) :: can_cap_nvg(:)
!!$    real(r_def), intent(in) :: can_clump(:)
!!$    logical(l_def), intent(in) :: cansnowpft(:)
!!$    real(r_def), intent(in) :: cdn_hw_sea
!!$    real(r_def), intent(in) :: cdn_max_sea
!!$    logical(l_def), intent(in) :: dpsids_dsdz
!!$    real(r_def), intent(in) :: dt_ice_albedo
!!$    real(r_def), intent(in) :: emis_sea
!!$    real(r_def), intent(in) :: emis_sice
!!$    real(r_def), intent(in) :: evap_scale_sea
!!$    real(r_def), intent(in) :: fixed_sea_albedo
!!$    real(r_def), intent(in) :: fsmc_p0(:)
!!$    integer(i_native), intent(in) :: grain_growth
!!$    real(r_def), intent(in) :: heat_cap_nvg(:)
!!$    real(r_def), intent(in) :: heat_cap_sea
!!$    integer(i_native), intent(in) :: i_high_wind_drag
!!$    logical(l_def), intent(in) :: iceformdrag_lupkes
!!$    real(r_def), intent(in) :: knl(:)
!!$    logical(l_def), intent(in) :: l_10m_neut
!!$    logical(l_def), intent(in) :: l_hapke_soil
!!$    logical(l_def), intent(in) :: l_partition_albsoil
!!$    logical(l_def), intent(in) :: l_var_rainfrac
!!$    logical(l_def), intent(in) :: l_vg_soil
!!$    real(r_def), intent(in) :: light_extinct(:)
!!$    real(r_def), intent(in) :: n_lai_exposed(:)
!!$    integer(i_def), intent(in) :: n_sea_ice_tile
!!$    logical(l_def), intent(in) :: non_iso_scatter
!!$    real(r_def), intent(in) :: ratio_albsoil
!!$    integer(i_native), intent(in) :: relayer_opt
!!$    real(r_def), intent(in) :: rho_snow_fresh
!!$    real(r_def), intent(in) :: scat_coef_nir(:)
!!$    real(r_def), intent(in) :: scat_coef_vis(:)
!!$    integer(i_native), intent(in) :: sea_alb_method
!!$    logical(l_def), intent(in) :: sea_alb_var_chl
!!$    integer(i_native), intent(in) :: sea_surf_alg
!!$    logical(l_def), intent(in) :: sice_heatflux
!!$    logical(l_def), intent(in) :: soil_sat_down
!!$    logical(l_def), intent(in) :: stability_lupkes
!!$    real(r_def), intent(in) :: surf_tile_fracs(:)
!!$    real(r_def), intent(in) :: swdn_frac_albsoil
!!$    real(r_def), intent(in) :: therm_cond_sea
!!$    real(r_def), intent(in) :: therm_cond_sice
!!$    real(r_def), intent(in) :: therm_cond_sice_snow
!!$    real(r_def), intent(in) :: u_cdn_hw
!!$    real(r_def), intent(in) :: u_cdn_max
!!$    real(r_def), intent(in) :: unload_rate_u(:)
!!$    logical(l_def), intent(in) :: use_hydrology
!!$    logical(l_def), intent(in) :: use_variable_sst
!!$    real(r_def), intent(in) :: z0hm_ratio_nveg(:)
!!$    real(r_def), intent(in) :: z0hm_ratio_pft(:)
!!$    real(r_def), intent(in) :: z0v(:)
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_surface_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&surface")' )
!!$    if (size(alb_leaf_nir) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(alb_leaf_nir)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'alb_leaf_nir = ', alb_leaf_nir
!!$    else
!!$      write( temporary_unit, '("alb_leaf_nir = ", E14.7)' ) alb_leaf_nir
!!$    end if
!!$    if (size(alb_leaf_vis) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(alb_leaf_vis)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'alb_leaf_vis = ', alb_leaf_vis
!!$    else
!!$      write( temporary_unit, '("alb_leaf_vis = ", E14.7)' ) alb_leaf_vis
!!$    end if
!!$    write( temporary_unit, '("alb_sice_melt = ", E14.7)' ) alb_sice_melt
!!$    if (size(alb_snocov_max) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(alb_snocov_max)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'alb_snocov_max = ', alb_snocov_max
!!$    else
!!$      write( temporary_unit, '("alb_snocov_max = ", E14.7)' ) alb_snocov_max
!!$    end if
!!$    if (size(alb_snocov_nvg) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(alb_snocov_nvg)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'alb_snocov_nvg = ', alb_snocov_nvg
!!$    else
!!$      write( temporary_unit, '("alb_snocov_nvg = ", E14.7)' ) alb_snocov_nvg
!!$    end if
!!$    if (size(alb_snofree_nvg) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(alb_snofree_nvg)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'alb_snofree_nvg = ', alb_snofree_nvg
!!$    else
!!$      write( temporary_unit, '("alb_snofree_nvg = ", E14.7)' ) alb_snofree_nvg
!!$    end if
!!$    write( temporary_unit, '("albedo_obs = ", L2)' ) albedo_obs
!!$    write( temporary_unit, '("basal_melting = ''", A, "''")' ) key_from_basal_melting( basal_melting )
!!$    write( temporary_unit, '("blue_sky_alb = ", L2)' ) blue_sky_alb
!!$    write( temporary_unit, '("buddy_sea = ''", A, "''")' ) key_from_buddy_sea( buddy_sea )
!!$    if (size(can_cap_nvg) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(can_cap_nvg)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'can_cap_nvg = ', can_cap_nvg
!!$    else
!!$      write( temporary_unit, '("can_cap_nvg = ", E14.7)' ) can_cap_nvg
!!$    end if
!!$    if (size(can_clump) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(can_clump)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'can_clump = ', can_clump
!!$    else
!!$      write( temporary_unit, '("can_clump = ", E14.7)' ) can_clump
!!$    end if
!!$    if (size(cansnowpft) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(cansnowpft)-1, "(L2,','),L2)"
!!$      write( temporary_unit, fmt_str ) 'cansnowpft = ', cansnowpft
!!$    else
!!$      write( temporary_unit, '("cansnowpft = ", L2)' ) cansnowpft
!!$    end if
!!$    write( temporary_unit, '("cdn_hw_sea = ", E14.7)' ) cdn_hw_sea
!!$    write( temporary_unit, '("cdn_max_sea = ", E14.7)' ) cdn_max_sea
!!$    write( temporary_unit, '("dpsids_dsdz = ", L2)' ) dpsids_dsdz
!!$    write( temporary_unit, '("dt_ice_albedo = ", E14.7)' ) dt_ice_albedo
!!$    write( temporary_unit, '("emis_sea = ", E14.7)' ) emis_sea
!!$    write( temporary_unit, '("emis_sice = ", E14.7)' ) emis_sice
!!$    write( temporary_unit, '("evap_scale_sea = ", E14.7)' ) evap_scale_sea
!!$    write( temporary_unit, '("fixed_sea_albedo = ", E14.7)' ) fixed_sea_albedo
!!$    if (size(fsmc_p0) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(fsmc_p0)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'fsmc_p0 = ', fsmc_p0
!!$    else
!!$      write( temporary_unit, '("fsmc_p0 = ", E14.7)' ) fsmc_p0
!!$    end if
!!$    write( temporary_unit, '("grain_growth = ''", A, "''")' ) key_from_grain_growth( grain_growth )
!!$    if (size(heat_cap_nvg) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(heat_cap_nvg)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'heat_cap_nvg = ', heat_cap_nvg
!!$    else
!!$      write( temporary_unit, '("heat_cap_nvg = ", E14.7)' ) heat_cap_nvg
!!$    end if
!!$    write( temporary_unit, '("heat_cap_sea = ", E14.7)' ) heat_cap_sea
!!$    write( temporary_unit, '("i_high_wind_drag = ''", A, "''")' ) key_from_i_high_wind_drag( i_high_wind_drag )
!!$    write( temporary_unit, '("iceformdrag_lupkes = ", L2)' ) iceformdrag_lupkes
!!$    if (size(knl) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(knl)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'knl = ', knl
!!$    else
!!$      write( temporary_unit, '("knl = ", E14.7)' ) knl
!!$    end if
!!$    write( temporary_unit, '("l_10m_neut = ", L2)' ) l_10m_neut
!!$    write( temporary_unit, '("l_hapke_soil = ", L2)' ) l_hapke_soil
!!$    write( temporary_unit, '("l_partition_albsoil = ", L2)' ) l_partition_albsoil
!!$    write( temporary_unit, '("l_var_rainfrac = ", L2)' ) l_var_rainfrac
!!$    write( temporary_unit, '("l_vg_soil = ", L2)' ) l_vg_soil
!!$    if (size(light_extinct) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(light_extinct)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'light_extinct = ', light_extinct
!!$    else
!!$      write( temporary_unit, '("light_extinct = ", E14.7)' ) light_extinct
!!$    end if
!!$    if (size(n_lai_exposed) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(n_lai_exposed)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'n_lai_exposed = ', n_lai_exposed
!!$    else
!!$      write( temporary_unit, '("n_lai_exposed = ", E14.7)' ) n_lai_exposed
!!$    end if
!!$    write( temporary_unit, '("n_sea_ice_tile = ", I0)' ) n_sea_ice_tile
!!$    write( temporary_unit, '("non_iso_scatter = ", L2)' ) non_iso_scatter
!!$    write( temporary_unit, '("ratio_albsoil = ", E14.7)' ) ratio_albsoil
!!$    write( temporary_unit, '("relayer_opt = ''", A, "''")' ) key_from_relayer_opt( relayer_opt )
!!$    write( temporary_unit, '("rho_snow_fresh = ", E14.7)' ) rho_snow_fresh
!!$    if (size(scat_coef_nir) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(scat_coef_nir)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'scat_coef_nir = ', scat_coef_nir
!!$    else
!!$      write( temporary_unit, '("scat_coef_nir = ", E14.7)' ) scat_coef_nir
!!$    end if
!!$    if (size(scat_coef_vis) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(scat_coef_vis)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'scat_coef_vis = ', scat_coef_vis
!!$    else
!!$      write( temporary_unit, '("scat_coef_vis = ", E14.7)' ) scat_coef_vis
!!$    end if
!!$    write( temporary_unit, '("sea_alb_method = ''", A, "''")' ) key_from_sea_alb_method( sea_alb_method )
!!$    write( temporary_unit, '("sea_alb_var_chl = ", L2)' ) sea_alb_var_chl
!!$    write( temporary_unit, '("sea_surf_alg = ''", A, "''")' ) key_from_sea_surf_alg( sea_surf_alg )
!!$    write( temporary_unit, '("sice_heatflux = ", L2)' ) sice_heatflux
!!$    write( temporary_unit, '("soil_sat_down = ", L2)' ) soil_sat_down
!!$    write( temporary_unit, '("stability_lupkes = ", L2)' ) stability_lupkes
!!$    if (size(surf_tile_fracs) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(surf_tile_fracs)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'surf_tile_fracs = ', surf_tile_fracs
!!$    else
!!$      write( temporary_unit, '("surf_tile_fracs = ", E14.7)' ) surf_tile_fracs
!!$    end if
!!$    write( temporary_unit, '("swdn_frac_albsoil = ", E14.7)' ) swdn_frac_albsoil
!!$    write( temporary_unit, '("therm_cond_sea = ", E14.7)' ) therm_cond_sea
!!$    write( temporary_unit, '("therm_cond_sice = ", E14.7)' ) therm_cond_sice
!!$    write( temporary_unit, '("therm_cond_sice_snow = ", E14.7)' ) therm_cond_sice_snow
!!$    write( temporary_unit, '("u_cdn_hw = ", E14.7)' ) u_cdn_hw
!!$    write( temporary_unit, '("u_cdn_max = ", E14.7)' ) u_cdn_max
!!$    if (size(unload_rate_u) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(unload_rate_u)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'unload_rate_u = ', unload_rate_u
!!$    else
!!$      write( temporary_unit, '("unload_rate_u = ", E14.7)' ) unload_rate_u
!!$    end if
!!$    write( temporary_unit, '("use_hydrology = ", L2)' ) use_hydrology
!!$    write( temporary_unit, '("use_variable_sst = ", L2)' ) use_variable_sst
!!$    if (size(z0hm_ratio_nveg) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(z0hm_ratio_nveg)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'z0hm_ratio_nveg = ', z0hm_ratio_nveg
!!$    else
!!$      write( temporary_unit, '("z0hm_ratio_nveg = ", E14.7)' ) z0hm_ratio_nveg
!!$    end if
!!$    if (size(z0hm_ratio_pft) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(z0hm_ratio_pft)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'z0hm_ratio_pft = ', z0hm_ratio_pft
!!$    else
!!$      write( temporary_unit, '("z0hm_ratio_pft = ", E14.7)' ) z0hm_ratio_pft
!!$    end if
!!$    if (size(z0v) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(z0v)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'z0v = ', z0v
!!$    else
!!$      write( temporary_unit, '("z0v = ", E14.7)' ) z0v
!!$    end if
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_surface_namelist( temporary_unit, local_rank )
!!$    call postprocess_surface_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_surface_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_surface_config

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_time_config( calendar, &
!!$                                calendar_start, &
!!$                                calendar_type, &
!!$                                timestep_end, &
!!$                                timestep_start )
!!$
!!$    use time_config_mod, only : read_time_namelist, &
!!$                                postprocess_time_namelist, &
!!$                                key_from_calendar, &
!!$                                calendar_from_key, &
!!$                                key_from_calendar_type, &
!!$                                calendar_type_from_key
!!$
!!$    implicit none
!!$
!!$    integer(i_native), intent(in) :: calendar
!!$    character(*), intent(in) :: calendar_start
!!$    integer(i_native), intent(in) :: calendar_type
!!$    character(*), intent(in) :: timestep_end
!!$    character(*), intent(in) :: timestep_start
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_time_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&time")' )
!!$    write( temporary_unit, '("calendar = ''", A, "''")' ) key_from_calendar( calendar )
!!$    write( temporary_unit, '("calendar_start = ''", A, "''")' ) calendar_start
!!$    write( temporary_unit, '("calendar_type = ''", A, "''")' ) key_from_calendar_type( calendar_type )
!!$    write( temporary_unit, '("timestep_end = ''", A, "''")' ) timestep_end
!!$    write( temporary_unit, '("timestep_start = ''", A, "''")' ) timestep_start
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_time_namelist( temporary_unit, local_rank )
!!$    call postprocess_time_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_time_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_time_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_timestepping_config( alpha, &
!!$                                        dt, &
!!$                                        inner_iterations, &
!!$                                        method, &
!!$                                        outer_iterations, &
!!$                                        runge_kutta_method, &
!!$                                        spinup_alpha, &
!!$                                        spinup_period, &
!!$                                        spinup_winds, &
!!$                                        tau_r, &
!!$                                        tau_t, &
!!$                                        tau_u )
!!$
!!$    use timestepping_config_mod, only : read_timestepping_namelist, &
!!$                                        postprocess_timestepping_namelist, &
!!$                                        key_from_method, &
!!$                                        method_from_key, &
!!$                                        key_from_runge_kutta_method, &
!!$                                        runge_kutta_method_from_key
!!$
!!$    implicit none
!!$
!!$    real(r_def), intent(in) :: alpha
!!$    real(r_second), intent(in) :: dt
!!$    integer(i_def), intent(in) :: inner_iterations
!!$    integer(i_native), intent(in) :: method
!!$    integer(i_def), intent(in) :: outer_iterations
!!$    integer(i_native), intent(in) :: runge_kutta_method
!!$    logical(l_def), intent(in) :: spinup_alpha
!!$    real(r_second), intent(in) :: spinup_period
!!$    logical(l_def), intent(in) :: spinup_winds
!!$    real(r_def), intent(in) :: tau_r
!!$    real(r_def), intent(in) :: tau_t
!!$    real(r_def), intent(in) :: tau_u
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_timestepping_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&timestepping")' )
!!$    write( temporary_unit, '("alpha = ", E14.7)' ) alpha
!!$    write( temporary_unit, '("dt = ", E14.7)' ) dt
!!$    write( temporary_unit, '("inner_iterations = ", I0)' ) inner_iterations
!!$    write( temporary_unit, '("method = ''", A, "''")' ) key_from_method( method )
!!$    write( temporary_unit, '("outer_iterations = ", I0)' ) outer_iterations
!!$    write( temporary_unit, '("runge_kutta_method = ''", A, "''")' ) key_from_runge_kutta_method( runge_kutta_method )
!!$    write( temporary_unit, '("spinup_alpha = ", L2)' ) spinup_alpha
!!$    write( temporary_unit, '("spinup_period = ", E14.7)' ) spinup_period
!!$    write( temporary_unit, '("spinup_winds = ", L2)' ) spinup_winds
!!$    write( temporary_unit, '("tau_r = ", E14.7)' ) tau_r
!!$    write( temporary_unit, '("tau_t = ", E14.7)' ) tau_t
!!$    write( temporary_unit, '("tau_u = ", E14.7)' ) tau_u
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_timestepping_namelist( temporary_unit, local_rank )
!!$    call postprocess_timestepping_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_timestepping_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_timestepping_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_transport_config( cfl_mol_1d_stab, &
!!$                                     cfl_mol_2d_stab, &
!!$                                     cfl_mol_3d_stab, &
!!$                                     consistent_metric, &
!!$                                     dry_field_name, &
!!$                                     enforce_min_value, &
!!$                                     ffsl_advective_splitting, &
!!$                                     ffsl_flux_splitting, &
!!$                                     ffsl_scheme, &
!!$                                     field_names, &
!!$                                     fv_advective_order, &
!!$                                     fv_flux_order, &
!!$                                     horizontal_method, &
!!$                                     log_space, &
!!$                                     min_value, &
!!$                                     moisture_eqn, &
!!$                                     monotone, &
!!$                                     oned_reconstruction, &
!!$                                     operators, &
!!$                                     profile_size, &
!!$                                     runge_kutta_method, &
!!$                                     scheme, &
!!$                                     slice_order, &
!!$                                     splitting, &
!!$                                     use_density_predictor, &
!!$                                     vertical_method, &
!!$                                     vertical_sl_order )
!!$
!!$    use transport_config_mod, only : read_transport_namelist, &
!!$                                     postprocess_transport_namelist, &
!!$                                     key_from_ffsl_advective_splitting, &
!!$                                     ffsl_advective_splitting_from_key, &
!!$                                     key_from_ffsl_flux_splitting, &
!!$                                     ffsl_flux_splitting_from_key, &
!!$                                     key_from_ffsl_scheme, &
!!$                                     ffsl_scheme_from_key, &
!!$                                     key_from_moisture_eqn, &
!!$                                     moisture_eqn_from_key, &
!!$                                     key_from_operators, &
!!$                                     operators_from_key, &
!!$                                     key_from_runge_kutta_method, &
!!$                                     runge_kutta_method_from_key, &
!!$                                     key_from_slice_order, &
!!$                                     slice_order_from_key, &
!!$                                     key_from_vertical_sl_order, &
!!$                                     vertical_sl_order_from_key
!!$
!!$    implicit none
!!$
!!$    real(r_def), intent(in) :: cfl_mol_1d_stab
!!$    real(r_def), intent(in) :: cfl_mol_2d_stab
!!$    real(r_def), intent(in) :: cfl_mol_3d_stab
!!$    logical(l_def), intent(in) :: consistent_metric
!!$    character(*), intent(in) :: dry_field_name
!!$    logical(l_def), intent(in) :: enforce_min_value(:)
!!$    integer(i_native), intent(in) :: ffsl_advective_splitting
!!$    integer(i_native), intent(in) :: ffsl_flux_splitting
!!$    integer(i_native), intent(in) :: ffsl_scheme
!!$    character(*), intent(in) :: field_names(:)
!!$    integer(i_def), intent(in) :: fv_advective_order
!!$    integer(i_def), intent(in) :: fv_flux_order
!!$    integer(i_def), intent(in) :: horizontal_method(:)
!!$    logical(l_def), intent(in) :: log_space(:)
!!$    real(r_def), intent(in) :: min_value(:)
!!$    integer(i_native), intent(in) :: moisture_eqn
!!$    integer(i_def), intent(in) :: monotone(:)
!!$    logical(l_def), intent(in) :: oned_reconstruction
!!$    integer(i_native), intent(in) :: operators
!!$    integer(i_def), intent(in) :: profile_size
!!$    integer(i_native), intent(in) :: runge_kutta_method
!!$    integer(i_def), intent(in) :: scheme(:)
!!$    integer(i_native), intent(in) :: slice_order
!!$    integer(i_def), intent(in) :: splitting(:)
!!$    logical(l_def), intent(in) :: use_density_predictor
!!$    integer(i_def), intent(in) :: vertical_method(:)
!!$    integer(i_native), intent(in) :: vertical_sl_order
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_transport_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&transport")' )
!!$    write( temporary_unit, '("cfl_mol_1d_stab = ", E14.7)' ) cfl_mol_1d_stab
!!$    write( temporary_unit, '("cfl_mol_2d_stab = ", E14.7)' ) cfl_mol_2d_stab
!!$    write( temporary_unit, '("cfl_mol_3d_stab = ", E14.7)' ) cfl_mol_3d_stab
!!$    write( temporary_unit, '("consistent_metric = ", L2)' ) consistent_metric
!!$    write( temporary_unit, '("dry_field_name = ''", A, "''")' ) dry_field_name
!!$    if (size(enforce_min_value) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(enforce_min_value)-1, "(L2,','),L2)"
!!$      write( temporary_unit, fmt_str ) 'enforce_min_value = ', enforce_min_value
!!$    else
!!$      write( temporary_unit, '("enforce_min_value = ", L2)' ) enforce_min_value
!!$    end if
!!$    write( temporary_unit, '("ffsl_advective_splitting = ''", A, "''")' ) key_from_ffsl_advective_splitting( ffsl_advective_splitting )
!!$    write( temporary_unit, '("ffsl_flux_splitting = ''", A, "''")' ) key_from_ffsl_flux_splitting( ffsl_flux_splitting )
!!$    write( temporary_unit, '("ffsl_scheme = ''", A, "''")' ) key_from_ffsl_scheme( ffsl_scheme )
!!$    write( tmp_str,'(A)') "'"//trim(field_names(1))//"'"
!!$    if (size(field_names) > 1) then
!!$      do i=2, size(field_names)
!!$        write( tmp_str,'(A)') trim(tmp_str)//",'"//trim(field_names(i))//"'"
!!$      end do
!!$    end if
!!$    write( temporary_unit, '(A)' ) 'field_names = '// trim(tmp_str)
!!$    write( temporary_unit, '("fv_advective_order = ", I0)' ) fv_advective_order
!!$    write( temporary_unit, '("fv_flux_order = ", I0)' ) fv_flux_order
!!$    if (size(horizontal_method) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(horizontal_method)-1, "(I0,','),I0)"
!!$      write( temporary_unit, fmt_str ) 'horizontal_method = ', horizontal_method
!!$    else
!!$      write( temporary_unit, '("horizontal_method = ", I0)' ) horizontal_method
!!$    end if
!!$    if (size(log_space) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(log_space)-1, "(L2,','),L2)"
!!$      write( temporary_unit, fmt_str ) 'log_space = ', log_space
!!$    else
!!$      write( temporary_unit, '("log_space = ", L2)' ) log_space
!!$    end if
!!$    if (size(min_value) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(min_value)-1, "(E14.7,','),E14.7)"
!!$      write( temporary_unit, fmt_str ) 'min_value = ', min_value
!!$    else
!!$      write( temporary_unit, '("min_value = ", E14.7)' ) min_value
!!$    end if
!!$    write( temporary_unit, '("moisture_eqn = ''", A, "''")' ) key_from_moisture_eqn( moisture_eqn )
!!$    if (size(monotone) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(monotone)-1, "(I0,','),I0)"
!!$      write( temporary_unit, fmt_str ) 'monotone = ', monotone
!!$    else
!!$      write( temporary_unit, '("monotone = ", I0)' ) monotone
!!$    end if
!!$    write( temporary_unit, '("oned_reconstruction = ", L2)' ) oned_reconstruction
!!$    write( temporary_unit, '("operators = ''", A, "''")' ) key_from_operators( operators )
!!$    write( temporary_unit, '("profile_size = ", I0)' ) profile_size
!!$    write( temporary_unit, '("runge_kutta_method = ''", A, "''")' ) key_from_runge_kutta_method( runge_kutta_method )
!!$    if (size(scheme) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(scheme)-1, "(I0,','),I0)"
!!$      write( temporary_unit, fmt_str ) 'scheme = ', scheme
!!$    else
!!$      write( temporary_unit, '("scheme = ", I0)' ) scheme
!!$    end if
!!$    write( temporary_unit, '("slice_order = ''", A, "''")' ) key_from_slice_order( slice_order )
!!$    if (size(splitting) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(splitting)-1, "(I0,','),I0)"
!!$      write( temporary_unit, fmt_str ) 'splitting = ', splitting
!!$    else
!!$      write( temporary_unit, '("splitting = ", I0)' ) splitting
!!$    end if
!!$    write( temporary_unit, '("use_density_predictor = ", L2)' ) use_density_predictor
!!$    if (size(vertical_method) > 1) then
!!$      write( fmt_str,'(A,I0,A)')  "(A,", size(vertical_method)-1, "(I0,','),I0)"
!!$      write( temporary_unit, fmt_str ) 'vertical_method = ', vertical_method
!!$    else
!!$      write( temporary_unit, '("vertical_method = ", I0)' ) vertical_method
!!$    end if
!!$    write( temporary_unit, '("vertical_sl_order = ''", A, "''")' ) key_from_vertical_sl_order( vertical_sl_order )
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_transport_namelist( temporary_unit, local_rank )
!!$    call postprocess_transport_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_transport_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_transport_config
!!$
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  subroutine feign_well_mixed_gases_config( cfc113_mix_ratio, &
!!$                                            cfc11_mix_ratio, &
!!$                                            cfc12_mix_ratio, &
!!$                                            ch4_mix_ratio, &
!!$                                            co2_mix_ratio, &
!!$                                            hcfc22_mix_ratio, &
!!$                                            hfc134a_mix_ratio, &
!!$                                            n2_mix_ratio, &
!!$                                            n2o_mix_ratio, &
!!$                                            o2_mix_ratio, &
!!$                                            so2_mix_ratio )
!!$
!!$    use well_mixed_gases_config_mod, only : read_well_mixed_gases_namelist, &
!!$                                            postprocess_well_mixed_gases_namelist
!!$
!!$    implicit none
!!$
!!$    real(r_def), intent(in) :: cfc113_mix_ratio
!!$    real(r_def), intent(in) :: cfc11_mix_ratio
!!$    real(r_def), intent(in) :: cfc12_mix_ratio
!!$    real(r_def), intent(in) :: ch4_mix_ratio
!!$    real(r_def), intent(in) :: co2_mix_ratio
!!$    real(r_def), intent(in) :: hcfc22_mix_ratio
!!$    real(r_def), intent(in) :: hfc134a_mix_ratio
!!$    real(r_def), intent(in) :: n2_mix_ratio
!!$    real(r_def), intent(in) :: n2o_mix_ratio
!!$    real(r_def), intent(in) :: o2_mix_ratio
!!$    real(r_def), intent(in) :: so2_mix_ratio
!!$
!!$    integer(i_native)  :: condition
!!$    integer(i_native)  :: i
!!$    character(str_max_filename) :: tmp_str
!!$    character(str_def) :: fmt_str
!!$
!!$    if (local_rank == -1) then
!!$      local_rank = get_comm_rank()
!!$    end if
!!$
!!$    open( temporary_unit, status='scratch', action='readwrite', &
!!$          iostat=condition )
!!$
!!$    if (condition /= 0) then
!!$      write( 6, '("feign_well_mixed_gases_config: ", I0)' ) condition
!!$      stop
!!$    end if
!!$
!!$    write( temporary_unit, '("&well_mixed_gases")' )
!!$    write( temporary_unit, '("cfc113_mix_ratio = ", E14.7)' ) cfc113_mix_ratio
!!$    write( temporary_unit, '("cfc11_mix_ratio = ", E14.7)' ) cfc11_mix_ratio
!!$    write( temporary_unit, '("cfc12_mix_ratio = ", E14.7)' ) cfc12_mix_ratio
!!$    write( temporary_unit, '("ch4_mix_ratio = ", E14.7)' ) ch4_mix_ratio
!!$    write( temporary_unit, '("co2_mix_ratio = ", E14.7)' ) co2_mix_ratio
!!$    write( temporary_unit, '("hcfc22_mix_ratio = ", E14.7)' ) hcfc22_mix_ratio
!!$    write( temporary_unit, '("hfc134a_mix_ratio = ", E14.7)' ) hfc134a_mix_ratio
!!$    write( temporary_unit, '("n2_mix_ratio = ", E14.7)' ) n2_mix_ratio
!!$    write( temporary_unit, '("n2o_mix_ratio = ", E14.7)' ) n2o_mix_ratio
!!$    write( temporary_unit, '("o2_mix_ratio = ", E14.7)' ) o2_mix_ratio
!!$    write( temporary_unit, '("so2_mix_ratio = ", E14.7)' ) so2_mix_ratio
!!$    write( temporary_unit, '("/")' )
!!$
!!$    rewind(temporary_unit)
!!$    call read_well_mixed_gases_namelist( temporary_unit, local_rank )
!!$    call postprocess_well_mixed_gases_namelist()
!!$    close(temporary_unit, iostat=condition )
!!$    if (condition /= 0) stop &
!!$        'feign_well_mixed_gases_config: '// &
!!$        'Unable to close temporary file'
!!$
!!$  end subroutine feign_well_mixed_gases_config

end module feign_config_mod
