!-----------------------------------------------------------------------------
! (C) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief   This module defines details for a domain_type object to hold
!>          information on a domain volume.
!> @details A domain volume comprises of three axes:
!>
!>   Axis 1,2 refer to the horizontal extents.
!>   Axis 3   refers to the vertical extent.
!>
!> Axis 1/2: As the domain may either be spherical or cartesian, these axes
!>           are either in longitude/latitude or x/y cartesian. Spherical
!>           units are taken as radians.
!> Axis 3:   The veritical axis is in metres above base height. So the
!>           minimum extent will always be returned as 0.0m
!>
module domain_mod

  use constants_mod, only: r_def, l_def, str_def, i_def
  use log_mod,       only: log_event, log_scratch_space, &
                           LOG_LEVEL_ERROR, LOG_LEVEL_DEBUG

  implicit none

  private

  public :: domain_type

  type :: domain_type

    private

    real(r_def), allocatable :: horizontal_extents(:,:)

    real(r_def)        :: vertical_range(2)
    real(r_def)        :: base_height
    logical(l_def)     :: ll_coords
    character(str_def) :: axis_info

  contains

    procedure :: minimum_lonlat
    procedure :: maximum_lonlat
    procedure :: minimum_xy
    procedure :: maximum_xy
    procedure :: get_base_height
    procedure :: is_lonlat

    procedure :: clear

    ! Destructor
    final :: domain_destructor

  end type domain_type

  interface domain_type
    module procedure domain_constructor
  end interface

  character(*), parameter :: llr_axis  = '[longitude, latitude, radius]'
  character(*), parameter :: xyz_axis  = '[x, y, z]'

contains

  !===========================================================================
  !> @brief Constructor for <domain_type> which holds model domain extents.
  !> @param[in]  horizontal_extents  Domain extents in x/y-axes.
  !> @param[in]  base_height         Reference height at base of 3D domain [m].
  !> @param[in]  depth               Mean vertical depth of 3D domain [m].
  !> @param[in]  ll_coords           Flag for [lon,lat] coordinates in [radians]
  !> @return     self                <domain_type>
  !>
  function domain_constructor( horizontal_extents, &
                               base_height, depth, &
                               ll_coords ) result( self )

    implicit none

    type(domain_type) :: self

    real(r_def),    intent(in)  :: horizontal_extents(:,:)
    real(r_def),    intent(in)  :: base_height
    real(r_def),    intent(in)  :: depth
    logical(l_def), intent(in)  :: ll_coords

    allocate( self%horizontal_extents, source=horizontal_extents )

    self%ll_coords = ll_coords

    if ( self%ll_coords ) then
      self%axis_info   = llr_axis
      self%base_height = base_height
    else
      self%axis_info   = xyz_axis
      self%base_height = 0.0_r_def
    end if

    self%vertical_range = [ 0.0_r_def, depth ]

    write( log_scratch_space,'(A)')                  &
        'Domain type axis integers [1:3] map to '//  &
        trim(self%axis_info)
    call log_event( log_scratch_space, LOG_LEVEL_DEBUG )

    !> @todo Need to do a global reduction of maxs and mins when the
    !> code is parallel

    return
  end function domain_constructor


  !===========================================================================
  !> @brief  Returns the minimum coordinate on a given axis.
  !> @return axis_min_coordinate  Units are dependent on the mesh
  !>                              coordinate system.
  !>
  function minimum_lonlat( self, axis ) result( axis_min_coordinate )

    implicit none

    class(domain_type), intent(in) :: self
    integer(i_def),     intent(in) :: axis

    real(r_def) :: axis_min_coordinate

    if ( (self%ll_coords) .or. (axis == 3) ) then

      select case (axis)

      case(1:3)
        if ( axis == 3 ) then
          ! z height above surface.
          axis_min_coordinate = minval( self%vertical_range )
        else
          axis_min_coordinate = minval( self%horizontal_extents(axis,:) )
        end if

      case default
        write(log_scratch_space,'(A,I0,A)')               &
            'Invalid domain axis (', axis,                &
            '),valid axis choices [1:3] which map to ' // &
            trim(self%axis_info)
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )

      end select
    else

      write(log_scratch_space,'(A,I0,A)')                 &
          'Invalid function, domain does not employ a '// &
          'spherical co-ordinate system.'
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )

    end if

  end function minimum_lonlat


  !===========================================================================
  !> @brief  Returns the maximum coordinate on a given axis.
  !> @return axis_max_coordinate  Units are dependent on the mesh
  !>                              coordinate system.
  !>
  function maximum_lonlat( self, axis ) result( axis_max_coordinate )

    implicit none

    class(domain_type), intent(in) :: self
    integer(i_def),     intent(in) :: axis

    real(r_def) :: axis_max_coordinate

    if ( (self%ll_coords) .or. (axis == 3) ) then

      select case (axis)

      case(1:3)
        ! z height above surface.
        if ( axis == 3 ) then
          axis_max_coordinate = maxval( self%vertical_range )
        else
          axis_max_coordinate = maxval( self%horizontal_extents(axis,:) )
        end if

      case default
        write(log_scratch_space,'(A,I0,A)')               &
            'Invalid domain axis (', axis,                &
            '),valid axis choices [1:3] which map to ' // &
            trim(self%axis_info)
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )

      end select

    else

      write(log_scratch_space,'(A,I0,A)')                 &
          'Invalid function, domain does not employ a '// &
          'spherical co-ordinate system.'
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )

    end if

  end function maximum_lonlat


  !===========================================================================
  !> @brief  Returns the minimum coordinate on a given axis.
  !> @return axis_min_coordinate  Units are dependent on the mesh
  !>                              coordinate system.
  !>
  function minimum_xy( self, axis ) result( axis_min_coordinate )

    implicit none

    class(domain_type), intent(in) :: self
    integer(i_def),     intent(in) :: axis

    real(r_def) :: axis_min_coordinate

    if ( ( .not. self%ll_coords) .or. &
         ( axis == 3 ) ) then

      select case (axis)

      case(1:3)
        if ( axis == 3 ) then
          ! z height above surface.
          axis_min_coordinate = minval( self%vertical_range )
        else
          axis_min_coordinate = minval( self%horizontal_extents(axis,:) )
        end if

      case default
        write(log_scratch_space,'(A,I0,A)')               &
            'Invalid domain axis (', axis,                &
            '),valid axis choices [1:3] which map to ' // &
            trim(self%axis_info)
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )

      end select
    else

      write(log_scratch_space,'(A,I0,A)')                 &
          'Invalid function, domain does not employ a '// &
          'cartesian co-ordinate system.'
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )

    end if

  end function minimum_xy


  !===========================================================================
  !> @brief  Returns the maximum coordinate on a given axis.
  !> @return axis_max_coordinate  Units are dependent on the mesh
  !>                              coordinate system.
  !>
  function maximum_xy( self, axis ) result( axis_max_coordinate )

    implicit none

    class(domain_type), intent(in) :: self
    integer(i_def),     intent(in) :: axis

    real(r_def) :: axis_max_coordinate

    if ( (.not. self%ll_coords) .or. &
         (axis == 3) ) then

      select case (axis)

      case(1:3)
        if ( axis == 3 ) then
          ! z height above surface.
          axis_max_coordinate = maxval( self%vertical_range )
        else
          axis_max_coordinate = maxval( self%horizontal_extents(axis,:) )
        end if

      case default
        write(log_scratch_space,'(A,I0,A)')               &
            'Invalid domain axis (', axis,                &
            '),valid axis choices [1:3] which map to ' // &
            trim(self%axis_info)
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )

      end select
    else

      write(log_scratch_space,'(A,I0,A)')                 &
          'Invalid function, domain does not employ a '// &
          'cartesian co-ordinate system.'
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )

    end if

  end function maximum_xy




  !===========================================================================
  !> @brief  Returns if this domain is based on [lon,lat,z] co-ordinates.size on a given axis.
  !> @return answer True for [lon, lat, z].
  !>
  function is_lonlat( self ) result( answer )

    implicit none

    class(domain_type), intent(in) :: self

    logical :: answer

    answer = self%ll_coords

  end function is_lonlat


  !===========================================================================
  !> @brief  Gets the vertical height of the base of the domain.
  !> @return base_height Height of domain base [m].
  !>
  function get_base_height( self ) result( base_height )

    implicit none

    class(domain_type) :: self
    real(r_def)        :: base_height

    base_height = self%base_height

  end function get_base_height


  !===========================================================================
  !  Function to clear up - called by destructor
  !===========================================================================
  !> @details Explicitly deallocates any allocatable arrays.
  subroutine clear( self )

    implicit none

    class(domain_type), intent(inout) :: self

    if ( allocated(self%horizontal_extents) ) deallocate( self%horizontal_extents )

    return
  end subroutine clear


  !===========================================================================
  ! Domain destructor
  !===========================================================================
  subroutine domain_destructor( self )

    implicit none

    type(domain_type), intent(inout) :: self

    call self%clear()

    return
  end subroutine domain_destructor

end module domain_mod
