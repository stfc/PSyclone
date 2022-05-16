!-----------------------------------------------------------------------------
! (c) Crown copyright 2017 Met Office. All rights reserved.
! For further details please refer to the file LICENCE which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------

!> @brief    Initialisation and finalisation for FEM-specific choices for model.
!> @details  Create and destroy a collection of function spaces and the coordinate
!!           field (chi).
module driver_fem_mod

  use chi_transform_mod,              only: init_chi_transforms, &
                                            final_chi_transforms
  use constants_mod,                  only: i_def, i_native, l_def
  use base_mesh_config_mod,           only: geometry, geometry_planar
  use finite_element_config_mod,      only: element_order,    &
                                            coord_order,      &
                                            coord_system,     &
                                            coord_system_xyz
!  use halo_routing_collection_mod,    only: halo_routing_collection_type, &
!                                            halo_routing_collection
  use field_mod,                      only: field_type
  use fs_continuity_mod,              only: W0, W1, W2, W3, Wtheta, Wchi, W2v, W2h
  use function_space_mod,             only: function_space_type
  use function_space_collection_mod,  only: function_space_collection_type, &
                                            function_space_collection
  use function_space_chain_mod,       only: function_space_chain_type,          &
                                            single_layer_function_space_chain,  &
                                            multigrid_function_space_chain,     &
                                            W2_multigrid_function_space_chain,  &
                                            W2v_multigrid_function_space_chain, &
                                            W2h_multigrid_function_space_chain, &
                                            wtheta_multigrid_function_space_chain
  use driver_coordinates_mod,         only: assign_coordinate_field
  use log_mod,                        only: log_event,         &
                                            LOG_LEVEL_INFO,    &
                                            LOG_LEVEL_ERROR,   &
                                            log_scratch_space
  use mesh_mod,                       only: mesh_type
  use mesh_collection_mod,            only: mesh_collection

  implicit none

  private
  public :: init_fem, final_fem

contains

  !> @brief  Initialises the coordinate fields (chi) and FEM components.
  !>
  !> @param[in]      mesh                           Mesh used for chi field
  !> @param[in,out]  chi                            Coordinate field
  !> @param[in,out]  panel_id                       Field giving the ID of the mesh panels
  !> @param[in]      shifted_mesh                   Optional, mesh used for shifted chi field
  !> @param[in,out]  shifted_chi                    Optional, coordinates of vertically shifted mesh
  !> @param[in]      double_level_mesh              Optional, mesh used for double-level chi field
  !> @param[in,out]  double_level_chi               Optional, coordinates of double level mesh
  !> @param[in]      multigrid_mesh_ids             Optional, mesh ID array for multigrid function spaces chain
  !> @param[in]      multigrid_2d_mesh_ids          Optional, 2D-mesh ID array for multigrid function spaces chain
  !> @param[in,out]  chi_mg                         Optional, coordinates for multigrid meshes
  !> @param[in,out]  panel_id_mg                    Optional, field giving the ID of the mesh panels for multigrid meshes
  !> @param[in]      use_multigrid                  Optional, configuration switch for multigrid
  !> @param[in]      multires_coupling_mesh_ids     Optional, mesh ID array for multires_coupling chi fields
  !> @param[in]      multires_coupling_2d_mesh_ids  Optional, 2D-mesh ID array for multires_coupling chi fields
  !> @param[in,out]  chi_multires_coupling          Optional, coordinates for multires_coupling meshes
  !> @param[in,out]  panel_id_multires_coupling     Optional, field giving the ID of the mesh panels for multires_coupling meshes
  !> @param[in]      use_multires_coupling          Optional, logical flag to enable multiresolution atmospheric coupling
  subroutine init_fem( mesh, chi, panel_id,                       &
                       shifted_mesh, shifted_chi,                 &
                       double_level_mesh,                         &
                       double_level_chi,                          &
                       multigrid_mesh_ids, multigrid_2D_mesh_ids, &
                       chi_mg, panel_id_mg,                       &
                       use_multigrid,                             &
                       multires_coupling_mesh_ids,                &
                       multires_coupling_2D_mesh_ids,             &
                       chi_multires_coupling,                     &
                       panel_id_multires_coupling,                &
                       use_multires_coupling )

    implicit none

    ! Coordinate field
    type(mesh_type),  intent(in), pointer :: mesh
    type(field_type), intent(inout)       :: chi(:)
    type(field_type), intent(inout)       :: panel_id

    type( mesh_type ),   optional, intent(in), pointer :: shifted_mesh
    type( mesh_type ),   optional, intent(in), pointer :: double_level_mesh

    type(field_type),    optional, intent(inout) :: shifted_chi(:)
    type(field_type),    optional, intent(inout) :: double_level_chi(:)
    integer(kind=i_def), optional, intent(in)    :: multigrid_mesh_ids(:)
    integer(kind=i_def), optional, intent(in)    :: multigrid_2d_mesh_ids(:)
    type(field_type),    optional, intent(inout), allocatable :: chi_mg(:,:)
    type(field_type),    optional, intent(inout), allocatable :: panel_id_mg(:)
    logical(kind=l_def), optional, intent(in)    :: use_multigrid
    integer(kind=i_def), optional, intent(in)    :: multires_coupling_mesh_ids(:)
    integer(kind=i_def), optional, intent(in)    :: multires_coupling_2d_mesh_ids(:)
    type(field_type),    optional, intent(inout), allocatable :: chi_multires_coupling(:,:)
    type(field_type),    optional, intent(inout), allocatable :: panel_id_multires_coupling(:)
    logical(kind=l_def), optional, intent(in) :: use_multires_coupling

    logical(kind=l_def) :: create_shifted_chi            = .false.
    logical(kind=l_def) :: create_double_level_chi       = .false.
    logical(kind=l_def) :: create_multigrid_fs_chain     = .false.
    logical(kind=l_def) :: create_multires_coupling_chi  = .false.

    integer(kind=i_native), parameter :: fs_list(5) = [W0, W1, W2, W3, Wtheta]

    type(function_space_type), pointer :: fs => null()
    type(function_space_type), pointer :: shifted_fs => null()
    type(function_space_type), pointer :: double_level_fs => null()

    integer(kind=i_native) :: fs_index

    integer(kind=i_def) :: chi_space
    integer(kind=i_def) :: coord
    integer(kind=i_def) :: mesh_ctr, i

    type( mesh_type ),           pointer :: mesh_ptr => null()
    type( function_space_type ), pointer :: fs_ptr   => null()

    ! Set control flags
    !=================================================================
    if ( present(use_multigrid) ) then
      if ( use_multigrid                  .and. &
           present(multigrid_mesh_ids)    .and. &
           present(multigrid_2d_mesh_ids) .and. &
           present(chi_mg)                .and. &
           present(panel_id_mg) ) create_multigrid_fs_chain = .true.
    end if
    if (present(use_multires_coupling)) then
      if ( use_multires_coupling                  .and. &
           present(multires_coupling_mesh_ids)    .and. &
           present(multires_coupling_2d_mesh_ids) .and. &
           present(chi_multires_coupling)         .and. &
           present(panel_id_multires_coupling) ) create_multires_coupling_chi = .true.
    end if

    if ( present(shifted_mesh) .and. &
         present(shifted_chi) ) create_shifted_chi = .true.

    if ( present(double_level_mesh) .and. &
         present(double_level_chi) ) create_double_level_chi = .true.


    ! Create a collection for holding FEM info that is specific to a field
!    allocate( halo_routing_collection, &
!              source = halo_routing_collection_type() )

    call log_event( 'FEM specifics: creating function spaces...', LOG_LEVEL_INFO )

    allocate( function_space_collection, &
              source = function_space_collection_type() )

    ! Create function spaces from W0 to Wtheta
    ! =========================================
    !do fs_index = 1, size(fs_list)
    !  fs => function_space_collection%get_fs( mesh,          &
    !                                          element_order, &
    !                                          fs_list(fs_index) )
    !end do


    ! Create model coordinate field
    ! =========================================

    ! Initialise coordinate transformations
    call init_chi_transforms()

    fs => function_space_collection%get_fs(mesh, 0, W3)
    call panel_id%initialise(vector_space = fs )

    if ( coord_order == 0 ) then
      chi_space = W0
      call log_event( "FEM specifics: Computing W0 coordinate fields", LOG_LEVEL_INFO )
    else
      chi_space = Wchi
      call log_event( "FEM specifics: Computing Wchi coordinate fields", LOG_LEVEL_INFO )
    end if
    fs => function_space_collection%get_fs(mesh, coord_order, chi_space)

    ! Check that the geometry is compatible with the coordinate system
    if ( (geometry == geometry_planar) .and. &
         (coord_system /= coord_system_xyz) ) then
        call log_event( "Error: For planar geometry must use coord_system = xyz", LOG_LEVEL_ERROR )
    end if

    do coord = 1, size(chi)
      call chi(coord)%initialise(vector_space = fs )
    end do

    call assign_coordinate_field(chi, panel_id, mesh)

    ! Create shifted mesh coordinate field.
    ! =========================================
!!$    if ( create_shifted_chi ) then
!!$      call log_event( "FEM specifics: Making shifted level mesh spaces",     &
!!$                      LOG_LEVEL_INFO )
!!$
!!$      shifted_fs => function_space_collection%get_fs( shifted_mesh, &
!!$                                                      coord_order,  &
!!$                                                      chi_space )
!!$      do coord = 1, size(chi)
!!$        call shifted_chi(coord)%initialise(vector_space = shifted_fs)
!!$      end do
!!$
!!$      call assign_coordinate_field(shifted_chi, panel_id, shifted_mesh)
!!$
!!$      nullify(shifted_fs)
!!$    end if

    ! Create double level coordinate field.
    ! =========================================
!!$    if ( create_double_level_chi ) then
!!$      call log_event( "FEM specifics: Making double level mesh spaces", &
!!$                      LOG_LEVEL_INFO )
!!$
!!$      double_level_fs =>                                       &
!!$          function_space_collection%get_fs( double_level_mesh, &
!!$                                            coord_order,       &
!!$                                            chi_space )
!!$      do coord = 1, size(chi)
!!$        call double_level_chi(coord)%initialise(vector_space = double_level_fs)
!!$      end do
!!$
!!$      call assign_coordinate_field(double_level_chi, panel_id, &
!!$                                   double_level_mesh)
!!$
!!$      nullify( double_level_fs )
!!$    end if

    ! Create multires_coupling coordinate fields
    ! =========================================
!!$    if ( create_multires_coupling_chi ) then
!!$      call log_event( "FEM specifics: Making spaces for multires_coupling meshes", &
!!$                      LOG_LEVEL_INFO )
!!$
!!$      if (allocated(chi_multires_coupling)) deallocate(chi_multires_coupling)
!!$      if (allocated(panel_id_multires_coupling)) deallocate(panel_id_multires_coupling)
!!$      allocate(chi_multires_coupling(3,size(multires_coupling_mesh_ids)))
!!$      allocate(panel_id_multires_coupling(size(multires_coupling_mesh_ids)))
!!$
!!$      do mesh_ctr = 1, size(multires_coupling_mesh_ids)
!!$
!!$        mesh_ptr => mesh_collection%get_mesh( multires_coupling_mesh_ids(mesh_ctr) )
!!$        fs_ptr   => function_space_collection%get_fs( mesh_ptr, coord_order, chi_space)
!!$
!!$        ! Set up the coordinate fields for non-primary multires_coupling meshes
!!$        do i = 1, 3
!!$          call chi_multires_coupling( i, mesh_ctr )%initialise( vector_space = fs_ptr )
!!$        end do
!!$
!!$        call panel_id_multires_coupling(mesh_ctr)%initialise(vector_space =  &
!!$          function_space_collection%get_fs( mesh_ptr, 0, W3) )
!!$
!!$        call assign_coordinate_field( chi_multires_coupling(:,mesh_ctr),     &
!!$                                      panel_id_multires_coupling(mesh_ctr),  &
!!$                                      mesh_ptr )
!!$      end do
!!$
!!$      nullify( mesh_ptr, fs_ptr )
!!$
!!$    end if

    ! Create function space chains
    ! =========================================
!!$    if ( create_multigrid_fs_chain ) then
!!$
!!$      multigrid_function_space_chain        = function_space_chain_type()
!!$      w2_multigrid_function_space_chain     = function_space_chain_type()
!!$      w2v_multigrid_function_space_chain    = function_space_chain_type()
!!$      w2h_multigrid_function_space_chain    = function_space_chain_type()
!!$      wtheta_multigrid_function_space_chain = function_space_chain_type()
!!$
!!$      if (allocated(chi_mg)) deallocate(chi_mg)
!!$      if (allocated(panel_id_mg)) deallocate(panel_id_mg)
!!$      allocate(chi_mg(3,size(multigrid_mesh_ids)))
!!$      allocate(panel_id_mg(size(multigrid_mesh_ids)))
!!$
!!$      do i = 1, 3
!!$         call chi(i)%copy_field(chi_mg(i,1))
!!$      end do
!!$      call panel_id%copy_field(panel_id_mg(1))
!!$
!!$      write(log_scratch_space,'(A,I1,A)')                      &
!!$          'Initialising MultiGrid ', size(multigrid_mesh_ids), &
!!$          '-level function space chain.'
!!$      call log_event( log_scratch_space, LOG_LEVEL_INFO )
!!$
!!$      do mesh_ctr = 1, size(multigrid_mesh_ids)
!!$
!!$        mesh_ptr => mesh_collection%get_mesh( multigrid_mesh_ids(mesh_ctr) )
!!$
!!$        ! Make sure this function_space is in the collection
!!$        fs => function_space_collection%get_fs( mesh_ptr, 0, W3 )
!!$        call multigrid_function_space_chain%add( fs )
!!$
!!$        fs => function_space_collection%get_fs( mesh_ptr, 0, W2 )
!!$        call w2_multigrid_function_space_chain%add( fs )
!!$
!!$        fs => function_space_collection%get_fs( mesh_ptr, 0, W2v )
!!$        call w2v_multigrid_function_space_chain%add( fs )
!!$
!!$        fs => function_space_collection%get_fs( mesh_ptr, 0, W2h )
!!$        call w2h_multigrid_function_space_chain%add( fs )
!!$
!!$        fs => function_space_collection%get_fs( mesh_ptr, 0, Wtheta )
!!$        call wtheta_multigrid_function_space_chain%add( fs )
!!$
!!$        ! Set up the coordinate fields for multigrid levels greater than 1
!!$        if (mesh_ctr > 1) then
!!$          do i = 1, 3
!!$            call chi_mg( i, mesh_ctr )%initialise( vector_space = &
!!$              function_space_collection%get_fs( mesh_ptr, coord_order, chi_space) )
!!$          end do
!!$
!!$          call panel_id_mg(mesh_ctr)%initialise(vector_space = &
!!$            function_space_collection%get_fs( mesh_ptr, 0, W3) )
!!$
!!$          call assign_coordinate_field( chi_mg(:,mesh_ctr),          &
!!$                                        panel_id_mg(mesh_ctr),       &
!!$                                        mesh_ptr )
!!$        end if
!!$
!!$        nullify(mesh_ptr)
!!$      end do
!!$
!!$      single_layer_function_space_chain = function_space_chain_type()
!!$      do mesh_ctr = 1, size(multigrid_2d_mesh_ids)
!!$        mesh_ptr => mesh_collection%get_mesh( multigrid_2d_mesh_ids(mesh_ctr) )
!!$        fs => function_space_collection%get_fs( mesh_ptr, 0, W3 )
!!$        call single_layer_function_space_chain%add( fs )
!!$        nullify(mesh_ptr)
!!$      end do
!!$
!!$    end if ! create_multigrid_fs_chain

    nullify( fs )
    call log_event( 'FEM specifics created', LOG_LEVEL_INFO )

  end subroutine init_fem

  !> @brief  Finalises the function_space_collection.
  subroutine final_fem()

    implicit none

    if (allocated(function_space_collection)) then
      call function_space_collection%clear()
      deallocate(function_space_collection)
    end if

    call final_chi_transforms()

  end subroutine final_fem

end module driver_fem_mod
