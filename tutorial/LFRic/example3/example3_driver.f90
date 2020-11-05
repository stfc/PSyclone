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

  use constants_mod,          only : i_def, r_def, str_def
  use global_mesh_netcdf_mod, only: global_mesh_type => global_mesh_netcdf_type
  use mesh_mod,               only : mesh_type
  use mesh_constructor_helper_functions_mod, &
                              only : domain_size_type
  use partition_mod,          only : partition_type,     &
                                     partitioner_planar, &
                                     partitioner_interface
  use extrusion_mod,          only : uniform_extrusion_type
  use log_mod,                only : log_event,          &
                                     log_scratch_space,  &
                                     LOG_LEVEL_INFO
  !!use example3_alg_mod,     only : example3_alg

  implicit none

  ! Global and local mesh data types
  type(global_mesh_type), target  :: global_mesh
  type(global_mesh_type), pointer :: global_mesh_ptr => null()
  type(mesh_type), target         :: mesh
  type(domain_size_type)          :: domain_size
  ! Extrusion
  type(uniform_extrusion_type), target  :: extrusion
  type(uniform_extrusion_type), pointer :: extrusion_ptr => null()
  ! Partition
  type(partition_type)                      :: partition
  procedure(partitioner_interface), pointer :: partitioner_ptr => null()
  ! Number of ranks the mesh is partitioned over in the x- and y-directions
  ! (across a single face for a cubed-sphere mesh)
  integer(i_def) :: xproc, yproc
  ! Maximum depth (of cells outside the cell over which the stencil is based)
  ! of the stencil to be used on fields with this partition.
  ! A single cell stencil will, therefore, have a  max_stencil_depth=0.
  ! A nine-point square region stencil will have max_stencil_depth=1.
  integer(kind=i_def) :: max_stencil_depth
  ! Number of the MPI rank of this process
  integer(kind=i_def) :: local_rank
  ! Total number of MPI ranks (processes) in this job
  integer(kind=i_def) :: total_ranks
  ! Vertical extrusion parameters
  integer(kind=i_def) :: nlayers_vert
  real(kind=r_def) :: atmosphere_height
  ! Finite element method (FEM) order
  integer(kind=i_def) :: element_order
  ! NetCDF file name where the global 2D mesh data is stored
  character(len=str_def) :: mesh_filename
  ! Name of the global 2D mesh in the NetCDF file
  character(len=str_def) :: mesh_name

  !-----------------------------------------------------------------------------
  ! Set model parameters
  !-----------------------------------------------------------------------------
  call log_event( "Setting 'example3_driver' model parameters", LOG_LEVEL_INFO )
  ! FEM order
  element_order = 0
  ! Height of atmosphere in meters
  atmosphere_height = 10000.0_r_def 
  ! Number of layers in the vertical
  nlayers_vert = 10

  !-----------------------------------------------------------------------------
  ! Set partitioner parameters (planar mesh on a single process)
  !-----------------------------------------------------------------------------
  xproc = 1
  yproc = 1
  max_stencil_depth = 0
  local_rank = 0 
  total_ranks = 1

  !-----------------------------------------------------------------------------
  ! Create mesh
  !-----------------------------------------------------------------------------
  mesh_filename = "mesh_BiP100x100-1000x1000.nc"
  mesh_name = "dynamics"
  call log_event( "Creating mesh", LOG_LEVEL_INFO )
  ! Read global 2D mesh from the NetCDF file
  global_mesh = global_mesh_type(mesh_filename, mesh_name)
  global_mesh_ptr => global_mesh
  ! Get domain size from the mesh object for info
  domain_size = mesh%get_domain_size()
  write(*,*) "domain size = ", domain_size
  write(log_scratch_space, '( A, 3(2F10.2, A) )')                     &
    'Mesh "'//trim(mesh_filename)//                                   &
    '" has x limits [', domain_size%minimum%x, domain_size%maximum%x, &
    '], y limits [', domain_size%minimum%y, domain_size%maximum%y,    &
    '] and z limits [', domain_size%minimum%z, domain_size%maximum%z, ']'
  call log_event( trim(log_scratch_space), LOG_LEVEL_INFO )

  ! Generate the partition object
  partitioner_ptr => partitioner_planar
  partition = partition_type( global_mesh_ptr,   &
                              partitioner_ptr,   &
                              xproc,             &
                              yproc,             &
                              max_stencil_depth, &
                              local_rank,        &
                              total_ranks )

  ! Create extrusion object
  extrusion = uniform_extrusion_type( 0.0_r_def, atmosphere_height, nlayers_vert )
  extrusion_ptr => extrusion

  ! Create local mesh
  mesh = mesh_type( global_mesh_ptr, partition, extrusion_ptr )

  !-----------------------------------------------------------------------------
  ! Call algorithms
  !-----------------------------------------------------------------------------
  !!call log_event( "Calling 'example3_alg'", LOG_LEVEL_INFO )
  !!call example3_alg(mesh, element_order)

  !-----------------------------------------------------------------------------
  ! Tidy up after a run
  !-----------------------------------------------------------------------------
  call log_event( "Finalising 'example3_driver'", LOG_LEVEL_INFO )
  nullify( global_mesh_ptr, partitioner_ptr, extrusion_ptr )

end program example3_driver
