!-----------------------------------------------------------------------------
! (C) Crown copyright 2017-2020 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
! LICENCE is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE
!-------------------------------------------------------------------------------

! BSD 3-Clause License
!
! Copyright (c) 2020-2024, Science and Technology Facilities Council
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
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Modified by J. Henrichs, Bureau of Meteorology

!> @brief Provides extrusion methods for converting a 2D mesh to a unitless
!>        3D mesh.
!>
!> An abstract extrusion_type holds the data and provides getters to access
!> it. Concrete classes derived from it then implement the specific extrusion
!> routine.
!>
!> The result of this design is that once an extrusion object is created it
!> may be passed around as its abstract base class. Thus the ultimate point
!> of use need know nothing about which extrusion is being used.
!>
module extrusion_mod

  use constants_mod,         only : i_def, r_def
  use global_mesh_base_mod,  only : global_mesh_type => global_mesh_base_type

  use log_mod,               only : log_scratch_space, log_event, &
                                    log_level_error
  use reference_element_mod, only : reference_element_type

  implicit none

  private

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief All extrusion implementations inherit from this class.
  !>
  type, public, abstract :: extrusion_type

    private

    real(r_def)    :: atmosphere_bottom
    real(r_def)    :: atmosphere_top
    integer(i_def) :: number_of_layers

  contains

    private

    procedure, public :: get_atmosphere_bottom
    procedure, public :: get_atmosphere_top
    procedure, public :: get_number_of_layers
    procedure, public :: get_reference_element
    procedure(extrude_method), public, deferred :: extrude

    procedure, public :: extrusion_constructor

  end type extrusion_type

  interface
    subroutine extrude_method( this, eta )
      import extrusion_type, r_def
      class(extrusion_type), intent(in)  :: this
      real(r_def),           intent(out) :: eta(0:)
    end subroutine extrude_method
  end interface

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Extrudes with equal distribution of layers.
  !>
  type, public, extends(extrusion_type) :: uniform_extrusion_type
    private
  contains
    private
    procedure, public :: extrude => uniform_extrude
  end type uniform_extrusion_type

  interface uniform_extrusion_type
    module procedure uniform_extrusion_constructor
  end interface uniform_extrusion_type

  !> @brief Extrudes with a @f$\left(\frac{layer}{n_{layers}}\right)^2@f$
  !>        distribution of layers.
  !>
  type, public, extends(extrusion_type) :: quadratic_extrusion_type
    private
  contains
    private
    procedure, public :: extrude => quadratic_extrude
  end type quadratic_extrusion_type

  interface quadratic_extrusion_type
    module procedure quadratic_extrusion_constructor
  end interface quadratic_extrusion_type

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Extrudes with "geometric" layers.
  !>
  type, public, extends(extrusion_type) :: geometric_extrusion_type
    private
  contains
    private
    procedure, public :: extrude => geometric_extrude
  end type geometric_extrusion_type

  interface geometric_extrusion_type
    module procedure geometric_extrusion_constructor
  end interface geometric_extrusion_type

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Makes an extrusion from an existing extrusion but with new levels
  !>        halfway between the levels of the original extrusion.
  !>
  type, public, extends(extrusion_type) :: shifted_extrusion_type
    private

    class(extrusion_type), pointer :: base_extrusion

  contains
    private
    procedure, public :: extrude => shifted_extrude
  end type shifted_extrusion_type

  interface shifted_extrusion_type
    module procedure shifted_extrusion_constructor
  end interface shifted_extrusion_type

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Makes a double level extrusion from an existing extrusion. This is
  !> an extrusion whose levels are both the levels of the original extrusion
  !> and also those levels from the shifted mesh -- i.e. it has double the number
  !> of layers.
  !>
  type, public, extends(extrusion_type) :: double_level_extrusion_type
    private

    class(extrusion_type), pointer :: base_extrusion

  contains
    private
    procedure, public :: extrude => double_level_extrude
  end type double_level_extrusion_type

  interface double_level_extrusion_type
    module procedure double_level_extrusion_constructor
  end interface double_level_extrusion_type


contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Creates a uniform_extrusion_type object.
  !>
  !> @param[in] atmosphere_bottom Bottom of the atmosphere in meters.
  !> @param[in] atmosphere_top Top of the atmosphere in meters.
  !> @param[in] number_of_layers Number of layers in the atmosphere.
  !>
  !> @return New uniform_extrusion_type object.
  !>
  function uniform_extrusion_constructor( atmosphere_bottom, &
                                          atmosphere_top,    &
                                          number_of_layers ) result(new)

    implicit none

    real(r_def),    intent(in) :: atmosphere_bottom
    real(r_def),    intent(in) :: atmosphere_top
    integer(i_def), intent(in) :: number_of_layers

    type(uniform_extrusion_type) :: new

    call new%extrusion_constructor( atmosphere_bottom, atmosphere_top, &
                                    number_of_layers )

  end function uniform_extrusion_constructor

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Extrudes the mesh to give constant delta between layers.
  !>
  !> @param[out] eta Nondimensional vertical coordinate.
  !>
  subroutine uniform_extrude( this, eta )

    implicit none

    class(uniform_extrusion_type), intent(in)  :: this
    real(r_def),                   intent(out) :: eta(0:)

    integer(i_def) :: k

    do k = 0, this%number_of_layers
      eta(k) = real(k,r_def)/real(this%number_of_layers,r_def)
    end do

  end subroutine uniform_extrude

  !> @brief Creates a quadratic_extrusion_type object.
  !>
  !> @param[in] atmosphere_bottom Bottom of the atmosphere in meters.
  !> @param[in] atmosphere_top Top of the atmosphere in meters.
  !> @param[in] number_of_layers Number of layers in the atmosphere.
  !>
  !> @return New quadratic_extrusion_type object.
  !>
  function quadratic_extrusion_constructor( atmosphere_bottom, &
                                            atmosphere_top,    &
                                            number_of_layers ) result(new)

    implicit none

    real(r_def),    intent(in) :: atmosphere_bottom
    real(r_def),    intent(in) :: atmosphere_top
    integer(i_def), intent(in) :: number_of_layers

    type(quadratic_extrusion_type) :: new

    call new%extrusion_constructor( atmosphere_bottom, atmosphere_top, &
                                    number_of_layers )

  end function quadratic_extrusion_constructor

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Extrudes the mesh to give layer boundaries
  !>        @f$\frac{l}{n_{layers}}^2@f$.
  !>
  !> @param[out] eta Nondimensional vertical coordinate.
  !>
  subroutine quadratic_extrude( this, eta )

    implicit none

    class(quadratic_extrusion_type), intent(in)  :: this
    real(r_def),                     intent(out) :: eta(0:)

    integer(i_def) :: k

    do k = 0, this%number_of_layers
      eta(k) = ( real(k,r_def)/real(this%number_of_layers,r_def) )**2_i_def
    end do

  end subroutine quadratic_extrude

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Creates a geometric_extrusion_type object.
  !>
  !> @param[in] atmosphere_bottom Bottom of the atmosphere in meters.
  !> @param[in] atmosphere_top Top of the atmosphere in meters.
  !> @param[in] number_of_layers Number of layers in the atmosphere.
  !>
  !> @return New geometric_extrusion_type object.
  !>
  function geometric_extrusion_constructor( atmosphere_bottom, &
                                            atmosphere_top,    &
                                            number_of_layers ) result(new)

    implicit none

    real(r_def),    intent(in) :: atmosphere_bottom
    real(r_def),    intent(in) :: atmosphere_top
    integer(i_def), intent(in) :: number_of_layers

    type(geometric_extrusion_type) :: new

    call new%extrusion_constructor( atmosphere_bottom, atmosphere_top, &
                                    number_of_layers )

  end function geometric_extrusion_constructor

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Extrudes the mesh to give a John Thuburn ENDGame non-staggered grid.
  !>
  !> @param[out] eta Nondimensional vertical coordinate.
  !>
  subroutine geometric_extrude( this, eta )

    implicit none

    class(geometric_extrusion_type), intent(in)  :: this
    real(r_def),                     intent(out) :: eta(0:)

    integer(i_def)          :: k
    real(r_def), parameter  :: stretching_factor = 1.03_r_def
    real(r_def)             :: delta_eta

    delta_eta = (stretching_factor - 1.0_r_def) &
                / (stretching_factor**(this%number_of_layers) - 1.0_r_def)

    eta(0) = 0.0_r_def
    do k = 1, this%number_of_layers
      eta(k) = geometric_func( stretching_factor, real(k,r_def), delta_eta )
    end do

  end subroutine geometric_extrude

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Creates a shifted_extrusion_type object.
  !> @param[in] extrusion An existing extrusion_type object.
  !> @return New shifted_quadratic_extrusion_type object.
  function shifted_extrusion_constructor( extrusion ) result(new)

    implicit none

    class(extrusion_type), target, intent(in) :: extrusion
    type(shifted_extrusion_type)              :: new

    call new%extrusion_constructor( extrusion%atmosphere_bottom, &
                                    extrusion%atmosphere_top,    &
                                    extrusion%number_of_layers + 1 )

    new%base_extrusion => extrusion

  end function shifted_extrusion_constructor

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Finds the nondimensional heights for a shifted extrusion from
  !>        an original extrusion.
  !>
  !> @param[out] eta Nondimensional vertical coordinate.
  !>
  subroutine shifted_extrude( this, eta )

    implicit none

    class(shifted_extrusion_type), intent(in)  :: this
    real(r_def),                   intent(out) :: eta(0:)

    real(r_def)    :: eta_old(0:this%number_of_layers-1)
    integer(i_def) :: k

    ! Obtain original levels and put them into eta_old
    call this%base_extrusion%extrude( eta_old )

    eta(0) = 0.0_r_def

    do k = 1, this%number_of_layers-1
      eta(k) = 0.5_r_def * (eta_old(k) + eta_old(k-1))
    end do
    eta(this%number_of_layers) = 1.0_r_def

  end subroutine shifted_extrude

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Creates a double_level_extrusion_type object.
  !> @param[in] extrusion An existing extrusion_type object.
  !> @return New double_level_extrusion_type object.
  function double_level_extrusion_constructor( extrusion ) result(new)

    implicit none

    class(extrusion_type), target, intent(in) :: extrusion
    type(double_level_extrusion_type)         :: new
    integer(i_def)                            :: nlayers_dl

    nlayers_dl = 2 * extrusion%number_of_layers

    call new%extrusion_constructor( extrusion%atmosphere_bottom, &
                                    extrusion%atmosphere_top,    &
                                    nlayers_dl )

    new%base_extrusion => extrusion

  end function double_level_extrusion_constructor

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Finds the nondimensional heights for a double_level extrusion from
  !>        an original extrusion. The levels are those of the original extrusion
  !>        and the levels halfway between them, to create an extrusion with
  !>        double the number of layers.
  !>
  !> @param[out] eta Nondimensional vertical coordinate.
  !>
  subroutine double_level_extrude( this, eta )

    implicit none

    class(double_level_extrusion_type), intent(in)  :: this
    real(r_def),                        intent(out) :: eta(0:)

    real(r_def)    :: eta_old(0:(this%number_of_layers)/2)
    integer(i_def) :: k, nlayers_old

    nlayers_old = (this%number_of_layers)/2

    ! Obtain original levels and put them into eta_old
    call this%base_extrusion%extrude( eta_old )

    eta(0) = 0.0_r_def

    do k = 1, nlayers_old
      eta(2*k-1) = 0.5_r_def * (eta_old(k) + eta_old(k-1))
      eta(2*k) = eta_old(k)
    end do

  end subroutine double_level_extrude

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Initialises the extrusion base class.
  !>
  !> This method should be called from child method constructors in order to
  !> populate the parent fields.
  !>
  !> @param [in] atmosphere_bottom Bottom of the atmosphere (planet surface)
  !>                               in meters.
  !> @param [in] atmosphere_top Top of the atmosphere in meters.
  !> @param [in] number_of_layers Number of layers to split atmosphere into.
  !>
  subroutine extrusion_constructor( this,              &
                                    atmosphere_bottom, &
                                    atmosphere_top,    &
                                    number_of_layers )

    implicit none

    class(extrusion_type), intent(inout) :: this
    real(r_def),           intent(in) :: atmosphere_bottom
    real(r_def),           intent(in) :: atmosphere_top
    integer(i_def),        intent(in) :: number_of_layers

    this%atmosphere_bottom = atmosphere_bottom
    this%atmosphere_top    = atmosphere_top
    this%number_of_layers  = number_of_layers

  end subroutine extrusion_constructor

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the reference element for this extrusion given a particular
  !>        base mesh.
  !>
  !> @param[in] mesh Base mesh object.
  !> @param[out] reference_element Shape of a 3D element given the extrusion.
  !>
  subroutine get_reference_element( this, mesh, reference_element )

    use reference_element_mod, only : reference_prism_type, &
                                      reference_cube_type
    implicit none

    class(extrusion_type),   intent(in) :: this
    class(global_mesh_type), intent(in) :: mesh
    class(reference_element_type), &
                             intent(out), allocatable :: reference_element

    select case (mesh%get_nverts_per_cell())
      case (3)
        allocate( reference_element, source=reference_prism_type() )
      case (4)
        allocate( reference_element, source=reference_cube_type() )
      case default
        write( log_scratch_space, &
              '("Base mesh with ", I0, " vertices per cell not supported.")' &
             ) mesh%get_nverts_per_cell()
        call log_event( log_scratch_space, log_level_error )
    end select

  end subroutine get_reference_element

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the bottom of the atmosphere or the surface of the planet.
  !>
  !> @return Bottom of the atmosphere in meters.
  !>
  function get_atmosphere_bottom( this ) result(bottom)

    implicit none

    class(extrusion_type), intent(in) :: this
    real(r_def) :: bottom

    bottom = this%atmosphere_bottom

  end function get_atmosphere_bottom

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the top of the atmosphere.
  !>
  !> @return Top of the atmosphere in meters.
  !>
  function get_atmosphere_top( this ) result(top)

    implicit none

    class(extrusion_type), intent(in) :: this
    real(r_def) :: top

    top = this%atmosphere_top

  end function get_atmosphere_top

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the number of layers in the atmosphere.
  !>
  !> @return Number of layers.
  !>
  function get_number_of_layers( this ) result(layers)

    implicit none

    class(extrusion_type), intent(in) :: this
    integer(i_def) :: layers

    layers = this%number_of_layers

  end function get_number_of_layers

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Helper function for generating geometric extrusion
  !>
  !> @param[in] stretching_factor Scaling factor for geometric extrusion
  !> @param[in] vert_position     Vertical position
  !> @param[in] delta_eta         Constant eta factor
  !> @return    eta               Vertical eta coordinate
  !>
  function geometric_func( stretching_factor, vert_position, delta_eta ) result(eta)
    implicit none

    real(r_def), intent(in) :: stretching_factor
    real(r_def), intent(in) :: vert_position
    real(r_def), intent(in) :: delta_eta
    real(r_def) :: eta

    eta = delta_eta*(stretching_factor**vert_position-1.0_r_def) &
                    / (stretching_factor-1.0_r_def)

  end function geometric_func


end module extrusion_mod
