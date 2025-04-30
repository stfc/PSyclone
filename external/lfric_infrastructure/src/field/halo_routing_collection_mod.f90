!-----------------------------------------------------------------------------
! (C) Crown copyright 2019 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!>
!> @brief Holds and manages halo_routing objects created during a model run.
!>
!> @details A container which a collection of halo_routing objects.
!>          The collection holds halo_routing objects as
!>          singletons. It will handle the creation and storing of
!>          requested halo_routing objects.
!
module halo_routing_collection_mod

  use constants_mod,      only: i_def, i_halo_index, l_def
  use function_space_mod, only: function_space_type
  use function_space_collection_mod, &
                          only: function_space_collection_type, &
                                function_space_collection
  use halo_comms_mod,     only: halo_routing_type
  use linked_list_mod,    only: linked_list_type, &
                                linked_list_item_type
  use mesh_mod,           only: mesh_type

  implicit none

  private

  !-----------------------------------------------------------------------------
  ! Type which is is a collection of function spaces held in a linked list
  !-----------------------------------------------------------------------------
  type, public :: halo_routing_collection_type
    private
    !> Linked list which will hold the halo_routing objects
    type(linked_list_type) :: halo_routing_list
  contains
    !> Extracts a specific halo_routing object from the list
    procedure, public :: get_halo_routing
    !> Clears the link list of any content
    procedure, public :: clear
    ! Destructor, calls clear.
    final             :: halo_routing_collection_destructor
  end type halo_routing_collection_type
  !-----------------------------------------------------------------------------

  interface halo_routing_collection_type
    module procedure halo_routing_collection_constructor
  end interface

  ! Module level variable to make the function space collection
  ! globally available
  type(halo_routing_collection_type), public, allocatable :: &
      halo_routing_collection

contains

!> Function to construct a function space collection
!> @return self The new halo_routing_collection object
function halo_routing_collection_constructor() result(self)

  implicit none

  type(halo_routing_collection_type) :: self

  self%halo_routing_list = linked_list_type()

end function halo_routing_collection_constructor

!> Function to get an instance of a halo_routing object from the linked list
!> or create it if it doesn't exist (and return the newly created one)
!> @param [in] mesh     Mesh for which this information will be valid for
!> @param [in] element_order_h The horizontal element order for which this
!>                             information will be valid
!> @param [in] element_order_v The vertical element order for which this
!>                             information will be valid
!> @param [in] lfric_fs The function space continuity type for which this
!>                      information will be valid
!> @param [in] ndata The number of multidata values per dof location for
!.                   which this information will be valid
!> @param [in] fortran_type The Fortran type of the data for which this
!>                      information will be valid
!> @param [in] fortran_kind The Fortran kind of the data for which this
!>                      information will be valid
!> @param [in] halo_depth Halo depth to get the routing to
!> @return The halo_routing object that matches the input parameters
function get_halo_routing( self,            &
                           mesh,            &
                           element_order_h, &
                           element_order_v, &
                           lfric_fs,        &
                           ndata,           &
                           fortran_type,    &
                           fortran_kind,    &
                           halo_depth )  result(halo_routing)
  implicit none

  class(halo_routing_collection_type), intent(inout) :: self

  type(halo_routing_type), pointer :: halo_routing

  type(mesh_type), intent(in), pointer :: mesh

  integer(i_def), intent(in) :: element_order_h
  integer(i_def), intent(in) :: element_order_v
  integer(i_def), intent(in) :: lfric_fs
  integer(i_def), intent(in) :: ndata
  integer(i_def), intent(in) :: fortran_type
  integer(i_def), intent(in) :: fortran_kind
  integer(i_def), intent(in) :: halo_depth

  type(function_space_type), pointer :: function_space
  integer(i_halo_index), pointer :: global_dof_id(:)
  integer(i_def), allocatable :: halo_start(:)
  integer(i_def), allocatable :: halo_finish(:)
  integer(i_def) :: idepth
  integer(i_def) :: last_owned_dof
  integer(i_def) :: mesh_id

  nullify( function_space, global_dof_id )

  halo_routing => get_halo_routing_from_list( self,            &
                                              mesh,            &
                                              element_order_h, &
                                              element_order_v, &
                                              lfric_fs,        &
                                              ndata,           &
                                              fortran_type,    &
                                              fortran_kind,    &
                                              halo_depth )

  if (.not. associated(halo_routing)) then

    !Get indices of owned and halo cells
    function_space => function_space_collection%get_fs( mesh,            &
                                                        element_order_h, &
                                                        element_order_v, &
                                                        lfric_fs,        &
                                                        ndata = ndata )

    last_owned_dof = function_space%get_last_dof_owned()

    ! Set up the global dof index array
    global_dof_id => function_space%get_global_dof_id()

    ! Set up the boundaries of the different depths of halo
    allocate( halo_start(halo_depth) )
    allocate( halo_finish(halo_depth) )

    do idepth = 1, halo_depth

      halo_start(idepth)  = function_space%get_last_dof_owned()+1
      halo_finish(idepth) = function_space%get_last_dof_halo(idepth)
      ! The above assumes there is a halo cell following the last owned cell.
      ! This might not be true (e.g. in a serial run), so fix the start/finish
      ! points when that happens
      if ( halo_start(idepth) > function_space%get_last_dof_halo(idepth) ) then
        halo_start(idepth)  = function_space%get_last_dof_halo(idepth)
        halo_finish(idepth) = halo_start(idepth) - 1
      end if

    end do

    mesh_id = mesh%get_id()

    call self%halo_routing_list%insert_item( halo_routing_type( global_dof_id,   &
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
                                                                halo_depth ) )
    deallocate( halo_start, halo_finish )

    halo_routing => get_halo_routing_from_list( self,            &
                                                mesh,            &
                                                element_order_h, &
                                                element_order_v, &
                                                lfric_fs,        &
                                                ndata,           &
                                                fortran_type,    &
                                                fortran_kind,    &
                                                halo_depth )

  end if

  return
end function get_halo_routing

!------------------------------------------------------------------------------
! Note: This routine is not part of the API - but is used by get_halo_routing
! Function to scan the halo_routing collection for a
! halo_routing object with the given signature and return a pointer to it.
! A null pointer is returned if the requested halo_routing object does not exist.
!
function get_halo_routing_from_list(self,            &
                                    mesh,            &
                                    element_order_h, &
                                    element_order_v, &
                                    lfric_fs,        &
                                    ndata,           &
                                    fortran_type,    &
                                    fortran_kind,    &
                                    halo_depth)      &
    result(instance)

  implicit none

  class(halo_routing_collection_type), intent(inout) :: self

  type(mesh_type), intent(in), pointer :: mesh

  integer(i_def),  intent(in) :: element_order_h
  integer(i_def),  intent(in) :: element_order_v
  integer(i_def),  intent(in) :: lfric_fs
  integer(i_def),  intent(in) :: ndata
  integer(i_def),  intent(in) :: fortran_type
  integer(i_def),  intent(in) :: fortran_kind
  integer(i_def),  intent(in) :: halo_depth

  type(halo_routing_type),   pointer  :: instance

  type(linked_list_item_type), pointer  :: loop

  integer(i_def) :: mesh_id

  mesh_id = mesh%get_id()
  ! Point to head of the function space linked list
  loop => self%halo_routing_list%get_head()

  ! Loop through the linked list
  do
    if ( .not. associated(loop) ) then
      ! Have reached the end of the list so either
      ! the list is empty or at the end of list.
      instance => null()

      loop => self%halo_routing_list%get_tail()
      exit
    end if

    ! 'cast' to the halo_routing_type
    select type(listhalo_routing => loop%payload)
      type is (halo_routing_type)
      if ( mesh_id == listhalo_routing%get_mesh_id()                 .and. &
           element_order_h == listhalo_routing%get_element_order_h() .and. &
           element_order_v == listhalo_routing%get_element_order_v() .and. &
           lfric_fs == listhalo_routing%get_lfric_fs()               .and. &
           ndata == listhalo_routing%get_ndata()                     .and. &
           fortran_kind == listhalo_routing%get_fortran_kind()       .and. &
           halo_depth == listhalo_routing%get_halo_depth() ) then
        instance => listhalo_routing
        exit
      end if
    end select

    loop => loop%next
  end do

  nullify(loop)
  return
end function get_halo_routing_from_list

!> Function to clear all items from the field info collection linked list
subroutine clear(self)

  implicit none

  class(halo_routing_collection_type), intent(inout) :: self
  type(linked_list_item_type), pointer :: loop

  ! Point to head of the function space linked list
  loop => self%halo_routing_list%get_head()

  ! Loop through the linked list and clear
  ! each redistribution structure
  do
    if ( .not. associated(loop) ) then
      exit
    end if
    select type(listhalo_routing => loop%payload)
      type is (halo_routing_type)
        call listhalo_routing%clear()
    end select
    loop => loop%next
  end do

  nullify(loop)

  call self%halo_routing_list%clear()

  return
end subroutine clear

!> Destructor that will be called by the Fortran runtime environment when the
!> field info collection object goes out of scope
subroutine halo_routing_collection_destructor(self)

  implicit none

  type (halo_routing_collection_type), intent(inout) :: self

  call self%clear()

  return
end subroutine halo_routing_collection_destructor

end module halo_routing_collection_mod
