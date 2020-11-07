! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2020, Science and Technology Facilities Council.
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! * Redistributions of source code must retain the above copyright notice, this
!   list of conditions and the following disclaimer.
!
! * Redistributions in binary form must reproduce the above copyright notice,
!   this list of conditions and the following disclaimer in the documentation
!   and/or other materials provided with the distribution.
!
! * Neither the name of the copyright holder nor the names of its
!   contributors may be used to endorse or promote products derived from
!   this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
! FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
! COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
! BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
! ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Author: I. Kavcic, Met Office
!
!------------------------------------------------------------------------------
! Drives the execution of the algorithms and kernels in Example 3.
! Based on the pared-down version of LFRic infrastructure stored in
! $PSYCLONE_DIR/src/psyclone/tests/test_files/dynamo0p3/infrastructure
!------------------------------------------------------------------------------
program example3_driver

  ! Infrastructure
  use constants_mod,          only : i_def, i_native, r_def, str_short
  use global_mesh_netcdf_mod, only : global_mesh_type => global_mesh_netcdf_type
  use mesh_mod,               only : mesh_type
  use partition_mod,          only : partition_type,     &
                                     partitioner_planar, &
                                     partitioner_interface
  use extrusion_mod,          only : uniform_extrusion_type
  use fs_continuity_mod,      only : W0, Wchi, W3
  use function_space_mod,     only : function_space_type
  use field_mod,              only : field_type
  use log_mod,                only : log_event,          &
                                     log_scratch_space,  &
                                     LOG_LEVEL_INFO
  ! Configuration
  use configuration_mod,      only : read_configuration, &
                                     final_configuration
  use base_mesh_config_mod,   only : filename, &
                                     prime_mesh_name
  use extrusion_uniform_config_mod, &
                              only : domain_top, &
                                     number_of_layers
  use finite_element_config_mod, &
                              only : element_order, &
                                     coordinate_order
  use partitioning_config_mod, &
                              only : panel_xproc, &
                                     panel_yproc
  ! Coordinates
  use assign_coordinate_field_mod, &
                              only : assign_coordinate_field
  ! Algorithms
  use example3_alg_mod,       only : example3_alg_init
  use diagnostic_alg_mod,     only : diagnostic_alg

  implicit none

  ! Global and local mesh data types
  type(global_mesh_type), target  :: global_mesh
  type(global_mesh_type), pointer :: global_mesh_ptr => null()
  type(mesh_type), target         :: mesh
  ! Extrusion
  type(uniform_extrusion_type), target  :: extrusion
  type(uniform_extrusion_type), pointer :: extrusion_ptr => null()
  ! Partition
  type(partition_type)                      :: partition
  procedure(partitioner_interface), pointer :: partitioner_ptr => null()
  ! Function spaces Wchi and W3 for coordinate and perturbation field
  type(function_space_type), target  :: fs_wchi
  type(function_space_type), pointer :: fs_wchi_ptr => null()
  type(function_space_type), target  :: fs_w3
  type(function_space_type), pointer :: fs_w3_ptr => null()
  ! Coordinate fields
  type(field_type)                   :: chi(3)
  ! Perturbation field
  type(field_type)                   :: perturbation
  ! Number of data per degree of freedom in fields
  integer(kind=i_def), parameter     :: ndata_sz = 1
  ! Maximum depth (of cells outside the cell over which the stencil is based)
  ! of the stencil to be used on fields with this partition.
  ! A single cell stencil will, therefore, have a  max_stencil_depth=0.
  ! A nine-point square region stencil will have max_stencil_depth=1.
  integer(kind=i_def)                :: max_stencil_depth
  ! Number of the MPI rank of this process
  integer(kind=i_native)             :: local_rank
  ! Total number of MPI ranks (processes) in this job
  integer(kind=i_def)                :: total_ranks
  ! Auxiliary variables for coordinate fields (function space ID, loop counter)
  integer(kind=i_def)                :: chi_space, i
  ! Auxiliary variable for naming coordinate fields
  character(len=str_short)           :: cind

  !-----------------------------------------------------------------------------
  ! Set model parameters from the configuration file
  !-----------------------------------------------------------------------------
  call log_event( "Setting 'example3_driver' model parameters", LOG_LEVEL_INFO )
  call read_configuration( "configuration.nml", local_rank )

  !-----------------------------------------------------------------------------
  ! Set partitioner parameters (planar mesh on a single process)
  !-----------------------------------------------------------------------------
  max_stencil_depth = 0
  local_rank = 0
  total_ranks = 1

  !-----------------------------------------------------------------------------
  ! Create global mesh, partition and local mesh objects
  !-----------------------------------------------------------------------------
  ! Read global 2D mesh from the NetCDF file
  call log_event( "Creating global 2D mesh", LOG_LEVEL_INFO )
  global_mesh = global_mesh_type(filename, prime_mesh_name)
  global_mesh_ptr => global_mesh

  ! Generate the partition object
  call log_event( "Creating partition", LOG_LEVEL_INFO )
  partitioner_ptr => partitioner_planar
  partition = partition_type( global_mesh_ptr,   &
                              partitioner_ptr,   &
                              panel_xproc,       &
                              panel_yproc,       &
                              max_stencil_depth, &
                              local_rank,        &
                              total_ranks )

  ! Create extrusion object
  extrusion = uniform_extrusion_type( 0.0_r_def, domain_top, number_of_layers )
  extrusion_ptr => extrusion
  ! Create local mesh
  call log_event( "Creating local 3D mesh", LOG_LEVEL_INFO )
  mesh = mesh_type( global_mesh_ptr, partition, extrusion_ptr )

  !-----------------------------------------------------------------------------
  ! Create and compute coordinate fields
  !-----------------------------------------------------------------------------
  call log_event( "Computing model coordinates", LOG_LEVEL_INFO )

  ! Select function space based on the coordinate_order parameter
  if ( coordinate_order == 0 ) then
    chi_space = W0
    call log_event( "Computing coordinate fields on W0 space", LOG_LEVEL_INFO )
  else
    chi_space = Wchi
    call log_event( "Computing coordinate fields on Wchi space", LOG_LEVEL_INFO )
  end if
  fs_wchi = function_space_type( mesh, element_order, chi_space, ndata_sz )
  fs_wchi_ptr => fs_wchi

  ! Create and compute coordinate fields
  do i = 1, size(chi)
    write(cind, '(I5)') i
    call chi(i)%initialise( vector_space = fs_wchi_ptr, &
                            name="chi_"//trim(adjustl(cind)) )
  end do
  call assign_coordinate_field(chi, mesh)

  !-----------------------------------------------------------------------------
  ! Create perturbation field
  !-----------------------------------------------------------------------------
  call log_event( "Creating perturbation field", LOG_LEVEL_INFO )

  ! Create W3 function space and the perturbation field on it
  fs_w3 = function_space_type( mesh, element_order, W3, ndata_sz )
  fs_w3_ptr => fs_w3
  !---------------------------------------------------------------------------
  ! TO COMPLETE: Create perturbation field on W3 function space
  call perturbation%initialise( vector_space = fs_w3_ptr, &
                                name = "perturbation" )
  !---------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! Call algorithms
  !-----------------------------------------------------------------------------
  call log_event( "Calling 'example3_alg_init'", LOG_LEVEL_INFO )
  call example3_alg_init(mesh, chi, perturbation)
  call diagnostic_alg(chi, perturbation, "out.txt")

  !-----------------------------------------------------------------------------
  ! Tidy up after a run
  !-----------------------------------------------------------------------------
  call log_event( "Finalising 'example3_driver'", LOG_LEVEL_INFO )
  call final_configuration()
  nullify( global_mesh_ptr, partitioner_ptr, extrusion_ptr, &
           fs_wchi_ptr, fs_w3_ptr )

end program example3_driver
