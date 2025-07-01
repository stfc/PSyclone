!-----------------------------------------------------------------------------
! (C) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief Provides access to utilities for providing halo exchanges
!>
!> Provides functionality for providing support of halo exchanges. At the
!> moment, this takes the form of a wrapper around the YAXT library.
!>
module halo_comms_mod

  use, intrinsic :: iso_fortran_env, only : real32, real64, int32

  use constants_mod,         only: i_def, i_halo_index
  use linked_list_data_mod,  only: linked_list_data_type
  use lfric_mpi_mod,         only: global_mpi, get_lfric_datatype, &
                                   lfric_comm_type, lfric_datatype_type
  use log_mod,               only: log_event, LOG_LEVEL_ERROR
#ifdef NO_MPI
  ! If this is a non-mpi build - YAXT won't work - so don't "use" it
#else
  use yaxt,                  only: xt_initialize, xt_finalize, &
                                   xt_redist, xt_idxlist, xt_xmap, &
                                   xt_idxvec_new, &
                                   xt_xmap_dist_dir_new, &
                                   xt_redist_p2p_off_new, &
                                   xt_redist_delete, &
                                   xt_idxlist_delete, &
                                   xt_xmap_delete, &
                                   xt_redist_s_exchange, &
                                   xt_redist_a_exchange, &
                                   xt_request, xt_request_wait
#endif
  implicit none

  private

  public initialise_halo_comms, finalise_halo_comms, &
         perform_halo_exchange, perform_halo_exchange_start, &
         perform_halo_exchange_finish

  !> @details A wrapper type for a YAXT xt_redist object (which holds a halo
  !> routing table), along with metadata that describes the fields for which
  !> that routing table is valid
  !
  type, extends(linked_list_data_type), public :: halo_routing_type
    private
    !> Id of the mesh used in the function space that this information
    !> is valid for
    integer(i_def) :: mesh_id
    !> Horizontal order of the function space that this information is valid for
    integer(i_def) :: element_order_h
    !> Vertical order of the function space that this information is valid for
    integer(i_def) :: element_order_v
    !> Enumerated value representing the continutity of the function space
    !> that this information is valid for
    integer(i_def) :: lfric_fs
    !> The number of multidata values per dof location that this information
    !> is valid for
    integer(i_def) :: ndata
    !> Description of the type of data in the field to be halo swapped
    integer(i_def) :: fortran_type
    !> Description of the kind of data in the field to be halo swapped
    integer(i_def) :: fortran_kind
    !> Depth of halo this routing is computed to
    integer(i_def) :: halo_depth
    !> Number of redistribution map objects
    integer(i_def) :: max_depth
    !> YAXT redistribution map
#ifdef NO_MPI
    ! If this is a non-mpi, serial build, redistribution maps are meaningless
    ! so we don't need one, but we need something  for get_redist to return
    ! so just use an integer
    integer(i_def), allocatable :: redist(:)
#else
    type(xt_redist), allocatable :: redist(:)
#endif
  contains
    !> Gets the mesh_id for which the halo_routing object is valid
    procedure, public :: get_mesh_id
    !> Gets the element_order_h for which the halo_routing object is valid
    procedure, public :: get_element_order_h
    !> Gets the element_order_v for which the halo_routing object is valid
    procedure, public :: get_element_order_v
    !> Gets the function space continuity type for which the halo_routing
    !> object is valid
    procedure, public :: get_lfric_fs
    !> Gets the  number of multidata values per dof location for which the
    !> halo_routing object is valid
    procedure, public :: get_ndata
    !> Gets the fortran type of the data for which the halo_routing
    !> object is valid
    procedure, public :: get_fortran_type
    !> Gets the fortran kind of the data for which the halo_routing
    !> object is valid
    procedure, public :: get_fortran_kind
    !> Gets a YAXT redistribution map for halo swapping
    procedure, public :: get_redist
    !> Get halo depth
    procedure, public :: get_halo_depth
    !> Clear all the redistribution structures
    procedure, public :: clear
  end type halo_routing_type
  !---------------------------------------------------------------------

  interface halo_routing_type
    module procedure halo_routing_constructor
  end interface

  !> @details A wrapper type for a YAXT xt_request object (which holds an
  !> id for a halo exchange). This is used to identify asynchronous halo
  !> exchanges when we want to see if they have completed
  !
  type, public :: halo_exchange_id_type
    private
#ifdef NO_MPI
    ! Dummy contents for a non-mpi, serial build where this won't be used
    integer(i_def) :: halo_request
#else
    !> Unique identifier used to identify a halo exchange, so the start of an
    !> asynchronous halo exchange can be matched with the end
    type(xt_request) :: halo_request
#endif
  contains

  end type halo_exchange_id_type
  !---------------------------------------------------------------------

  ! Generic interface for specific synchronous halo exchange functions
  interface perform_halo_exchange
   module procedure perform_halo_exchange_real32, &
                    perform_halo_exchange_real64, &
                    perform_halo_exchange_int32
  end interface

  ! Generic interface for specific asynchronous halo exchange start functions
  interface perform_halo_exchange_start
   module procedure perform_halo_exchange_start_real32, &
                    perform_halo_exchange_start_real64, &
                    perform_halo_exchange_start_int32
  end interface
contains

!-----------------------------------------------------------------------
! Type bound procedures for the halo_routing type

!> @brief Constructor for a halo_routing object
!> @param [in] global_dof_id   List of global dof ids known to the local
!>                             partition
!> @param [in] last_owned_dof  The position of the last owned dof in the list
!> @param [in] halo_start      The position of the first halo dof in the list
!> @param [in] halo_finish     The positions of the last halo dof un each of
!>                             the halo depths
!> @param [in] mesh_id         Id of the mesh for which this information will
!>                             be valid
!> @param [in] element_order_h The horizontal element order for which this
!>                             information will be valid
!> @param [in] element_order_v The vertical element order for which this
!>                             information will be valid
!> @param [in] lfric_fs        The function space continuity type for which this
!>                             information will be valid
!> @param [in] ndata           The number of multidata values per dof location
!>                             for which this information will be valid
!> @param [in] fortran_type    The Fortran type of the data for which this
!>                             information will be valid
!> @param [in] fortran_kind    The Fortran kind of the data for which this
!>                             information will be valid
!> @return The new halo_routing object
function halo_routing_constructor( global_dof_id,   &
                                   last_owned_dof,  &
                                   halo_start,      &
                                   halo_finish,     &
                                   mesh_id,         &
                                   element_order_h, &
                                   element_order_v, &
                                   lfric_fs,        &
                                   ndata,           &
                                   fortran_type,    &
                                   fortran_kind,    &
                                   halo_depth ) result(self)

  implicit none

  integer(i_halo_index), intent(in) :: global_dof_id(:)
  integer(i_def), intent(in) :: last_owned_dof
  integer(i_def), intent(in) :: halo_start(:)
  integer(i_def), intent(in) :: halo_finish(:)
  integer(i_def), intent(in) :: mesh_id
  integer(i_def), intent(in) :: element_order_h
  integer(i_def), intent(in) :: element_order_v
  integer(i_def), intent(in) :: lfric_fs
  integer(i_def), intent(in) :: ndata
  integer(i_def), intent(in) :: fortran_type
  integer(i_def), intent(in) :: fortran_kind
  integer(i_def), intent(in) :: halo_depth

  type(halo_routing_type) :: self
  integer(i_def) :: max_depth
  integer(i_def) :: idepth

  max_depth = size(halo_start)

  self%mesh_id = mesh_id
  self%element_order_h = element_order_h
  self%element_order_v = element_order_v
  self%lfric_fs = lfric_fs
  self%ndata = ndata
  self%fortran_type = fortran_type
  self%fortran_kind = fortran_kind
  self%halo_depth = halo_depth
  self%max_depth = max_depth

  allocate( self%redist(max_depth) )

#ifdef NO_MPI
  self%redist(:) = 0
  idepth=0 ! Set local variables to avoid unused variable errors
#else
  do idepth = 1, max_depth
    ! Get the redistribution map objects for doing halo exchanges later
    self%redist(idepth) = generate_redistribution_map( &
                     global_dof_id(1:last_owned_dof), &
                     global_dof_id( halo_start(idepth):halo_finish(idepth) ), &
                     get_lfric_datatype( fortran_type, fortran_kind ) )
  end do
#endif

end function halo_routing_constructor

!> @brief Gets the mesh_id for which this object is valid
!> @return Id of the mesh that this information is valid for
function get_mesh_id(self) result (mesh_id)
  implicit none
  class(halo_routing_type), intent(in) :: self
  integer(i_def) :: mesh_id
  mesh_id = self%mesh_id
  return
end function get_mesh_id

!> @brief Gets the element_order_h for which this object is valid
!> @return The element order that this information is valid for
function get_element_order_h(self) result (element_order_h)
  implicit none
  class(halo_routing_type), intent(in) :: self
  integer(i_def) :: element_order_h
  element_order_h = self%element_order_h
  return
end function get_element_order_h

!> @brief Gets the element_order_v for which this object is valid
!> @return The element order that this information is valid for
function get_element_order_v(self) result (element_order_v)
  implicit none
  class(halo_routing_type), intent(in) :: self
  integer(i_def) :: element_order_v
  element_order_v = self%element_order_v
  return
end function get_element_order_v

!> @brief Gets the function space continuity type for which this object is valid
!> @return The function space continuity type that this information is valid for
function get_lfric_fs(self) result (lfric_fs)
  implicit none
  class(halo_routing_type), intent(in) :: self
  integer(i_def) :: lfric_fs
  lfric_fs = self%lfric_fs
  return
end function get_lfric_fs

!> @brief Gets the number of multidata values per dof location for which this
!>        object is valid
!> @return The number of multidata values per dof location that this
!>         information is valid for
function get_ndata(self) result (ndata)
  implicit none
  class(halo_routing_type), intent(in) :: self
  integer(i_def) :: ndata
  ndata = self%ndata
  return
end function get_ndata

!> @brief Gets the Fortran type of the data for which this object is valid
!> @return The Fortran type of the data that this information is valid for
function get_fortran_type(self) result (fortran_type)
  implicit none
  class(halo_routing_type), intent(in) :: self
  integer(i_def) :: fortran_type
  fortran_type = self%fortran_type
  return
end function get_fortran_type

!> @brief Gets the Fortran kind of the data for which this object is valid
!> @return The Fortran kind of the data that this information is valid for
function get_fortran_kind(self) result (fortran_kind)
  implicit none
  class(halo_routing_type), intent(in) :: self
  integer(i_def) :: fortran_kind
  fortran_kind = self%fortran_kind
  return
end function get_fortran_kind

!> @brief Gets the halo depth for which this object is valid
!> @return The halo depth for which this object is valid
function get_halo_depth(self) result (halo_depth)
  implicit none
  class(halo_routing_type), intent(in) :: self
  integer(i_def) :: halo_depth
  halo_depth = self%halo_depth
  return
end function get_halo_depth

!> @brief Clear all the redistribution structures
!> by calling the (YAXT) destructor
subroutine clear(self)
  implicit none
  class(halo_routing_type), intent(inout) :: self
  integer(i_def) :: max_depth
  integer(i_def) :: idepth

#ifdef NO_MPI
  ! No finalization step is needed
  max_depth = 0 ! Set local variables to avoid unused variable errors
  idepth=0
#else
  max_depth = self%max_depth
  do idepth = 1, max_depth
    call xt_redist_delete(self%redist(idepth))
  end do
#endif
  if (allocated(self%redist)) deallocate(self%redist)
end subroutine clear

!> @brief Gets a YAXT redistribution map for halo swapping
!> @param [in] depth The depth of halo exchange that the redistribution map
!>                   will be used for
!> @return The YAXT redistribution map for a halo exchange of a particular
!>         depth of halo, valid for a particualar field type
function get_redist(self, depth) result (redist)

  implicit none

  class(halo_routing_type), intent(in), target :: self
  integer(i_def), intent(in) :: depth

#ifdef NO_MPI
  integer(i_def), pointer :: redist
#else
  type(xt_redist), pointer :: redist
#endif

  redist => self%redist(depth)

  return
end function get_redist

!-----------------------------------------------------------------------
! Non-type-bound halo comms functionality

!> Initialises the halo communications
!>
!> @param [in] comm The MPI communicator.
!>
subroutine initialise_halo_comms(comm)
  implicit none
  type(lfric_comm_type), intent(in) :: comm

#ifdef NO_MPI
  ! Don't initialise YAXT in non-mpi build.
#else
  integer(i_def) ::comm_mpi_val

  ! Initialise YAXT

  comm_mpi_val = comm%get_comm_mpi_val()
  call xt_initialize( comm_mpi_val )
#endif

end subroutine initialise_halo_comms

!> Finalises the halo communications
!>
subroutine finalise_halo_comms()
  implicit none

#ifdef NO_MPI
  ! Don't finalise YAXT in non-mpi build.
#else
  call xt_finalize()
#endif

end subroutine finalise_halo_comms

!! Perform a blocking halo exchange on 32-bit data
!> @param [inout] data_with_halos The data on which to peform the halo exchange.
!>                                Includes both owned and halo data.
!> @param [in] halo_routing The pre-generated routing table.
!> @param [in] depth The depth to which the halo exchange should be performed.
!!
subroutine perform_halo_exchange_real32( data_with_halos, &
                                         halo_routing, &
                                         depth )

  implicit none

  real(kind=real32),       intent(inout), pointer :: data_with_halos(:)
  type(halo_routing_type), intent(in),    pointer :: halo_routing
  integer(i_def), intent(in)                      :: depth

#ifdef NO_MPI
  ! Don't need to do anything to the halos in a serial, non-mpi build.
#else
  type(xt_redist), pointer :: redist

  ! Start a blocking (synchronous) halo exchange
  redist => halo_routing%get_redist(depth)
  call xt_redist_s_exchange(redist, data_with_halos, data_with_halos)
#endif

end subroutine perform_halo_exchange_real32

!! Perform a blocking halo exchange on 64-bit data
!> @param [inout] data_with_halos The data on which to peform the halo exchange.
!>                                Includes both owned and halo data.
!> @param [in] halo_routing The pre-generated routing table.
!> @param [in] depth The depth to which the halo exchange should be performed.
!!
subroutine perform_halo_exchange_real64( data_with_halos, &
                                         halo_routing, &
                                         depth )
  implicit none

  real(kind=real64),       intent(inout), pointer :: data_with_halos(:)
  type(halo_routing_type), intent(in),    pointer :: halo_routing
  integer(i_def), intent(in)                      :: depth

#ifdef NO_MPI
  ! Don't need to do anything to the halos in a serial, non-mpi build.
#else
  type(xt_redist), pointer :: redist

  ! Start a blocking (synchronous) halo exchange
  redist => halo_routing%get_redist(depth)
  call xt_redist_s_exchange(redist, data_with_halos, data_with_halos)
#endif

end subroutine perform_halo_exchange_real64

!! Perform a blocking halo exchange on 32-bit integer data
!!
!> @param [inout] data_with_halos The data on which to peform the halo exchange.
!>                                Includes both owned and halo data.
!> @param [in] halo_routing The pre-generated routing table.
!> @param [in] depth The depth to which the halo exchange should be performed.
subroutine perform_halo_exchange_int32( data_with_halos, &
                                        halo_routing, &
                                        depth )

  implicit none

  integer(kind=int32),     intent(inout), pointer :: data_with_halos(:)
  type(halo_routing_type), intent(in),    pointer :: halo_routing
  integer(i_def), intent(in)                      :: depth

#ifdef NO_MPI
  ! Don't need to do anything to the halos in a serial, non-mpi build.
#else
  type(xt_redist), pointer :: redist

  ! Start a blocking (synchronous) halo exchange
  redist => halo_routing%get_redist(depth)
  call xt_redist_s_exchange(redist, data_with_halos, data_with_halos)
#endif

end subroutine perform_halo_exchange_int32

!! Start an asynchronous halo exchange on 32-bit data
!> @param [inout] data_with_halos The data on which to peform the halo exchange.
!>                                Includes both owned and halo data.
!> @param [in] halo_routing The pre-generated routing table.
!> @param [in] depth The depth to which the halo exchange should be performed.
!> @param [out] halo_id Identifier to use when checking this asynchronous
!>                      halo exchange has completed
!!
subroutine perform_halo_exchange_start_real32( data_with_halos, &
                                               halo_routing, &
                                               depth, &
                                               halo_id )
  implicit none

  real(kind=real32),       intent(inout), pointer :: data_with_halos(:)
  type(halo_routing_type), intent(in),    pointer :: halo_routing
  integer(i_def), intent(in)                      :: depth
  type(halo_exchange_id_type), intent(out)        :: halo_id

#ifdef NO_MPI
  ! Don't need to do anything to the halos in a serial, non-mpi build,
  ! so just set the intent(out) variable to a dummy value
  halo_id%halo_request = 0
#else
  type(xt_redist), pointer :: redist

  ! Start a non-blocking (asynchronous) halo exchange
  redist => halo_routing%get_redist(depth)
  call xt_redist_a_exchange(redist, &
                            data_with_halos, &
                            data_with_halos, &
                            halo_id%halo_request)
#endif

end subroutine perform_halo_exchange_start_real32

!! Start an asynchronous halo exchange on 64-bit data
!> @param [inout] data_with_halos The data on which to peform the halo exchange.
!>                                Includes both owned and halo data.
!> @param [in] halo_routing The pre-generated routing table.
!> @param [in] depth The depth to which the halo exchange should be performed.
!> @param [out] halo_id Identifier to use when checking this asynchronous
!>                      halo exchange has completed
!!
subroutine perform_halo_exchange_start_real64( data_with_halos, &
                                               halo_routing, &
                                               depth, &
                                               halo_id )
  implicit none

  real(kind=real64),       intent(inout), pointer :: data_with_halos(:)
  type(halo_routing_type), intent(in),    pointer :: halo_routing
  integer(i_def), intent(in)                      :: depth
  type(halo_exchange_id_type), intent(out)        :: halo_id

#ifdef NO_MPI
  ! Don't need to do anything to the halos in a serial, non-mpi build,
  ! so just set the intent(out) variable to a dummy value
  halo_id%halo_request = 0
#else
  type(xt_redist), pointer :: redist

  ! Start a non-blocking (asynchronous) halo exchange
  redist => halo_routing%get_redist(depth)
  call xt_redist_a_exchange(redist, &
                            data_with_halos, &
                            data_with_halos, &
                            halo_id%halo_request)
#endif

end subroutine perform_halo_exchange_start_real64

!! Start an asynchronous halo exchange on 32-bit integer data
!> @param [inout] data_with_halos The data on which to peform the halo exchange.
!>                                Includes both owned and halo data.
!> @param [in] halo_routing The pre-generated routing table.
!> @param [in] depth The depth to which the halo exchange should be performed.
!> @param [out] halo_id Identifier to use when checking this asynchronous
!>                      halo exchange has completed
!!
subroutine perform_halo_exchange_start_int32( data_with_halos, &
                                              halo_routing, &
                                              depth, &
                                              halo_id )
  implicit none

  integer(kind=int32),     intent(inout), pointer :: data_with_halos(:)
  type(halo_routing_type), intent(in),    pointer :: halo_routing
  integer(i_def), intent(in)                      :: depth
  type(halo_exchange_id_type), intent(out)        :: halo_id

#ifdef NO_MPI
  ! Don't need to do anything to the halos in a serial, non-mpi build,
  ! so just set the intent(out) variable to a dummy value
  halo_id%halo_request = 0
#else
  type(xt_redist), pointer :: redist

  ! Start a non-blocking (asynchronous) halo exchange
  redist => halo_routing%get_redist(depth)
  call xt_redist_a_exchange(redist, &
                            data_with_halos, &
                            data_with_halos, &
                            halo_id%halo_request)
#endif

end subroutine perform_halo_exchange_start_int32

!! Wait for an asynchronous halo exchange to complete
!> @param [inout] halo_id Id to indentify which asynchronous halo exchange
!>                        to wait for completion
!!
subroutine perform_halo_exchange_finish( halo_id )

  implicit none

  type(halo_exchange_id_type), intent(inout) :: halo_id

#ifdef NO_MPI
  ! There's nothing to wait for in a non-mpi build, so just return
#else
  ! Wait for the asynchronous halo exchange to complete
  call xt_request_wait(halo_id%halo_request)
#endif

end subroutine perform_halo_exchange_finish

!> Private function to generate the halo exchange redistribution object
!> to be used for future halo exchanges
!>
!> @param src_indices [in] The global indices of all the owned points in this
!>                    MPI task
!> @param tgt_indices [in] The global indices of all the halo points in this
!>                    MPI task
!> @param datatype [in] The MPI datatype of a single element in the data to be
!>                    exchanged
!> @return redist     The halo exchange redistribution object
!>
function generate_redistribution_map(src_indices, tgt_indices, datatype) &
                                     result(redist)
  implicit none

  integer(i_halo_index),     intent(in) :: src_indices(:), tgt_indices(:)
  type(lfric_datatype_type), intent(in) :: datatype
#ifdef NO_MPI
  !  Redistribution maps are meaningless in a non-mpi build, so just return 0
  integer(i_def) :: redist

  redist = 0
#else
  type(xt_redist) :: redist
  type(xt_idxlist) :: src_idxlist, tgt_idxlist
  type(xt_xmap) :: xmap
  integer(i_def), allocatable :: src_offsets(:)
  integer(i_def), allocatable :: tgt_offsets(:)
  integer(i_def) :: i
  type(lfric_comm_type) :: comm
  integer(i_def) :: datatype_mpi_val

  if( global_mpi%is_comm_set() )then
    ! create decomposition descriptors
    src_idxlist = xt_idxvec_new( src_indices, size(src_indices) )
    tgt_idxlist = xt_idxvec_new( tgt_indices, size(tgt_indices) )

    ! generate exchange map
    comm = global_mpi%get_comm()
    xmap = xt_xmap_dist_dir_new( src_idxlist, tgt_idxlist, &
                                 comm%get_comm_mpi_val() )

    allocate(src_offsets( size(src_indices) ))
    allocate(tgt_offsets( size(tgt_indices) ))

    do i = 1, size(src_indices)
      src_offsets(i) = i - 1
    end do

    do i = 1, size(tgt_indices)
      tgt_offsets(i) = i + size(src_indices) - 1
    end do

    datatype_mpi_val = datatype%get_datatype_mpi_val()
    redist = xt_redist_p2p_off_new(xmap, src_offsets,tgt_offsets, datatype_mpi_val)

    call xt_xmap_delete(xmap)
    call xt_idxlist_delete(tgt_idxlist)
    call xt_idxlist_delete(src_idxlist)
    deallocate(src_offsets)
    deallocate(tgt_offsets)
  else
    call log_event( &
    'Call to generate_redistribution_map failed. Must initialise mpi first', &
    LOG_LEVEL_ERROR )
  end if
#endif

end function generate_redistribution_map

end module halo_comms_mod
