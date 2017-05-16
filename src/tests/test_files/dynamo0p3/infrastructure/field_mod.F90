!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!> @brief A module providing field related classes.
!>
!> @details Both a representation of a field which provides no access to the
!> underlying data (to be used in the algorithm layer) and an accessor class
!> (to be used in the Psy layer) are provided.


module field_mod

  use constants_mod,      only: r_def, r_double, i_def, l_def
  use function_space_mod, only: function_space_type
  use mesh_mod,           only: mesh_type

  !use ESMF

  implicit none

  private

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------

  !> Algorithm layer representation of a field.
  !>
  !> Objects of this type hold all the data of the field privately.
  !> Unpacking the data is done via the proxy type accessed by the Psy layer
  !> alone.
  !>
  type, public :: field_type
    private

    !> Each field has a pointer to the function space on which it lives
    type( function_space_type ), pointer         :: vspace => null( )
    !> Pointer array of type real which holds the values of the field
    real(kind=r_def), pointer             :: data( : ) => null()
    !> The data for each field is held within an ESMF array component
    !type(ESMF_Array) :: esmf_array
    !> Flag that holds whether each depth of halo is clean or dirty (dirty=1)
    integer(kind=i_def), allocatable :: halo_dirty(:)

  contains

    !> Function to get a proxy with public pointers to the data in a
    !! field_type.
    procedure, public :: get_proxy

    procedure, public :: write_checksum
    !procedure, public :: log_field
    !procedure, public :: log_dofs
    !procedure, public :: log_minmax

    !> function returns the enumerated integer for the functions_space on which
    !! the field lives
    procedure, public :: which_function_space

    !> Routine to read field
    !procedure         :: read_field

    !> Routine to write field
    !procedure         :: write_field

    !> Routine to return the mesh used by this field
    procedure         :: get_mesh
    procedure         :: get_mesh_id

    !> Overloaded assigment operator
    procedure         :: field_type_assign

    !> Routine to destroy field_type
!!$    final             :: field_destructor_scalar, &
!!$                         field_destructor_array1d, &
!!$                         field_destructor_array2d

    !> Override default assignment for field_type pairs.
    generic           :: assignment(=) => field_type_assign

  end type field_type

!!$  interface field_type
!!$    module procedure field_constructor
!!$  end interface

  !> Psy layer representation of a field.
  !>
  !> This is an accessor class that allows access to the actual field information
  !> with each element accessed via a public pointer.
  !>
  type, public :: field_proxy_type

    private

    !> An unused allocatable integer that prevents an intenal compiler error
    !> with the Gnu Fortran compiler. Adding an allocatable forces the compiler
    !> to accept that the object has a finaliser. It gets confused without it.
    !> This is a workaround for GCC bug id 61767 - when this bug is fixed, the
    !> integer can be removed. 
    integer(kind=i_def), allocatable :: dummy_for_gnu
    !> Each field has a pointer to the function space on which it lives
    type( function_space_type ), pointer, public :: vspace => null()
    !> Allocatable array of type real which holds the values of the field
    real(kind=r_def), public, pointer         :: data( : ) => null()
    !> pointer to the ESMF array
    !type(ESMF_Array), pointer :: esmf_array => null()
    !> pointer to array that holds halo dirtiness
    integer(kind=i_def), pointer :: halo_dirty(:) => null()

  contains

    !> Performs a blocking halo exchange operation on the field.
    !> @todo This is temporarily required by PSyclone for initial development
    !! Eventually, the PSy layer will call the asynchronous versions of 
    !! halo_exchange and this function should be removed.
    !! @param[in] depth The depth to which the halos should be exchanged
    procedure, public :: halo_exchange
    !> Starts a halo exchange operation on the field. The halo exchange
    !> is non-blocking, so this call only starts the process. On Return
    !> from this call, outbound data will have been transferred, but no
    !> guarantees are made for in-bound data elements at this stage.
    !! @param[in] depth The depth to which the halos should be exchanged
    procedure, public :: halo_exchange_start

    !> Wait (i.e. block) until the transfer of data in a halo exchange
    !> (started by a call to halo_exchange_start) has completed.
    !! @param[in] depth The depth to which the halos have been exchanged
    procedure, public :: halo_exchange_finish

    !> Perform a global sum operation on the field
    !> @return The global sum of the field values over all ranks
    procedure, public :: get_sum

    !> Calculate the global minimum of the field
    !> @return The minimum of the field values over all ranks
    procedure, public :: get_min

    !> Calculate the global maximum of the field
    !> @return The maximum of the field values over all ranks
    procedure, public :: get_max

    !> Wait (i.e. block) until all current non-blocking reductions
    !> (sum, max, min) are complete.
    !>
    !> ESMF have only implemented blocking reductions, so this
    !> subroutine currently returns without waiting.
    procedure reduction_finish

    !> Returns whether the halos at the given depth are dirty or clean
    !! @param[in] depth The depth at which to check the halos
    !! @return True if the halos are dirty or false if they are clean
    procedure is_dirty

    !> Flags all halos as being dirty
    procedure set_dirty

    !> Flags all the halos up the given depth as clean
    !! @param[in] depth The depth up to which to set the halo to clean
    procedure set_clean

  end type field_proxy_type

contains

  !> Function to create a proxy with access to the data in the field_type.
  !>
  !> @return The proxy type with public pointers to the elements of
  !> field_type
  type(field_proxy_type ) function get_proxy(self)
    implicit none
    class(field_type), target, intent(in)  :: self

    get_proxy % vspace                 => self % vspace
    get_proxy % data                   => self % data
    get_proxy % halo_dirty             => self % halo_dirty

  end function get_proxy

  !> Assignment operator between field_type pairs.
  !>
  !> @param[out] dest   field_type lhs
  !> @param[in]  source field_type rhs
  subroutine field_type_assign(dest, source)

    !use log_mod,         only : log_event, &
    !                            LOG_LEVEL_ERROR
    implicit none
    class(field_type), intent(out)     :: dest
    class(field_type), intent(in)      :: source

    integer(i_def), allocatable :: global_dof_id(:)
    integer(i_def) :: rc
    integer(i_def) :: halo_start, halo_finish

    type (mesh_type), pointer   :: mesh => null()

    dest%data(:) = source%data(:)

  end subroutine field_type_assign

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------

  !> Function to get mesh information from the field.
  !>
  !> @return Mesh object
  function get_mesh(self) result(mesh)

    implicit none

    class (field_type) :: self
    type (mesh_type), pointer :: mesh

    mesh => self%vspace%get_mesh()

    return
  end function get_mesh

  !> Function to get mesh id from the field.
  !>
  !> @return mesh_id
  function get_mesh_id(self) result(mesh_id)
    implicit none

    class (field_type) :: self
    integer(i_def) :: mesh_id

    mesh_id = self%vspace%get_mesh_id()

    return
  end function get_mesh_id

  

  !> Writes a checksum of this field to a file.
  !>
  !> \param[in] stream I/O unit number for output.
  !> \param[in] label Title to identify the field added to output.
  !>
  subroutine write_checksum( self, stream, label )

    implicit none

    class(field_type), intent(in) :: self
    integer,           intent(in) :: stream
    character(*),      intent(in) :: label

    integer(i_def)          :: cell
    integer(i_def)          :: layer
    integer(i_def)          :: df
    integer(i_def), pointer :: map( : )
    real(r_double)          :: fraction_checksum
    integer(i_def)          :: exponent_checksum

    fraction_checksum = 0.0_r_double
    exponent_checksum = 0_i_def

    do cell=1,self%vspace%get_ncell()
      map => self%vspace%get_cell_dofmap( cell )
      do df=1,self%vspace%get_ndf()
        do layer=0,self%vspace%get_nlayers()-1
          fraction_checksum = modulo( fraction_checksum + fraction( self%data( map( df ) + layer ) ), 1.0 )
          exponent_checksum = exponent_checksum + exponent( self%data( map( df ) + layer ) )
        end do
      end do
    end do

    write( stream, '("Fraction checksum ", A, " = ", F18.16)' ) &
        trim(label), fraction_checksum
    write( stream, '("Exponent checksum ", A, " = ", I0)' ) &
        trim(label), exponent_checksum

  end subroutine write_checksum

  function which_function_space(self) result(fs)
    implicit none
    class(field_type), intent(in) :: self
    integer(i_def) :: fs

    fs = self%vspace%which()
    return
  end function which_function_space

  !! Perform a blocking halo exchange operation on the field
  !!
  subroutine halo_exchange( self, depth )

    implicit none

    class( field_proxy_type ), target, intent(inout) :: self
    integer(i_def), intent(in) :: depth
    integer(i_def) :: rc
    type (mesh_type), pointer   :: mesh => null()

    mesh=>self%vspace%get_mesh()

    ! Halo exchange is complete so set the halo dirty flag to say it
    ! is clean (or more accurately - not dirty)
    self%halo_dirty(1:depth) = 0

  end subroutine halo_exchange

  !! Start a halo exchange operation on the field
  !!
  subroutine halo_exchange_start( self, depth )

    implicit none

    class( field_proxy_type ), target, intent(inout) :: self
    integer(i_def), intent(in) :: depth
    integer(i_def) :: rc
    type (mesh_type), pointer   :: mesh => null()

    mesh=>self%vspace%get_mesh()

  end subroutine halo_exchange_start

  !! Wait for a halo exchange to complete
  !!
  subroutine halo_exchange_finish( self, depth )

    implicit none

    class( field_proxy_type ), target, intent(inout) :: self
    integer(i_def), intent(in) :: depth
    !type(ESMF_RouteHandle) :: haloHandle
    integer(i_def) :: rc
    type (mesh_type), pointer   :: mesh => null()

    mesh=>self%vspace%get_mesh()

    ! Halo exchange is complete so set the halo dirty flag to say it
    ! is clean (or more accurately - not dirty)
    self%halo_dirty(1:depth) = 0

  end subroutine halo_exchange_finish

  !! Start performing a global sum operation on the field
  !!
  function get_sum(self) result (answer)

    class(field_proxy_type), intent(in) :: self

    real(r_def) :: answer

    integer(i_def) :: rc

    answer = 1.0_r_def
  end function get_sum

  !! Start the calculation of the global minimum of the field
  !!
  function get_min(self) result (answer)

    class(field_proxy_type), intent(in) :: self

    real(r_def) :: answer

    integer(i_def) :: rc

    answer = 1.0_r_def
  end function get_min

  !! Start the calculation of the global maximum of the field
  !!
  function get_max(self) result (answer)

    class(field_proxy_type), intent(in) :: self

    real(r_def) :: answer

    integer(i_def) :: rc

    answer = 1.0_r_def
  end function get_max

  !! Wait for any current (non-blocking) reductions (sum, max, min) to complete
  !!
  !! Currently, ESMF has only implemented blocking reductions, so there is
  !! no need to ever call this subroutine. It is left in here to complete the
  !! API so when non-blocking reductions are implemented, we can support them
  subroutine reduction_finish(self)

    class(field_proxy_type), intent(in) :: self

    integer(i_def) :: rc
    logical(l_def) :: is_dirty_tmp

    is_dirty_tmp=self%is_dirty(1)    ! reduction_finish currently does nothing.
                                    ! The "self" that is passed in automatically
                                    ! to a type-bound subroutine is not used -
                                    ! so the compilers complain -  have to use
                                    ! it for something harmless.
  end subroutine reduction_finish

  ! Returns true if a halo depth is dirty
  ! @param[in] depth The depth of halo to inquire about
  function is_dirty(self, depth) result(dirtiness)

    implicit none

    class(field_proxy_type), intent(in) :: self
    integer(i_def), intent(in) :: depth
    logical(l_def) :: dirtiness
    type (mesh_type), pointer   :: mesh => null()

    mesh=>self%vspace%get_mesh()
    dirtiness = .false.
    if(self%halo_dirty(depth) == 1)dirtiness = .true.

  end function is_dirty

  ! Sets a halo depth to be flagged as dirty
  ! @param[in] depth The depth up to which to make the halo dirty
  subroutine set_dirty( self )

    implicit none

    class(field_proxy_type), intent(inout) :: self

    self%halo_dirty(:) = 1

  end subroutine set_dirty

  ! Sets the halos up to depth to be flagged as clean
  ! @param[in] depth The depth up to which to make the halo clean
  subroutine set_clean(self, depth)

    implicit none

    class(field_proxy_type), intent(inout) :: self
    integer(i_def), intent(in) :: depth
    type (mesh_type), pointer   :: mesh => null()

    mesh=>self%vspace%get_mesh()
    self%halo_dirty(1:depth) = 0

  end subroutine set_clean

end module field_mod
