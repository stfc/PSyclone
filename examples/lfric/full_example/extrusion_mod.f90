!-----------------------------------------------------------------------------
! (C) Crown copyright 2017 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

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

    procedure :: extrusion_constructor

  end type extrusion_type

  interface
    subroutine extrude_method( this, eta )
      import extrusion_type, r_def
      class(extrusion_type), intent(in)  :: this
      real(r_def),           intent(out) :: eta(0:this%number_of_layers)
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

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Extrudes with specific UM configuration L38_29t_9s_40km
  !>
  type, public, extends(extrusion_type) :: um_L38_29t_9s_40km_extrusion_type
    private
  contains
    private
    procedure, public :: extrude => um_L38_29t_9s_40km_extrude
  end type um_L38_29t_9s_40km_extrusion_type

  interface um_L38_29t_9s_40km_extrusion_type
    module procedure um_L38_29t_9s_40km_extrusion_constructor
  end interface um_L38_29t_9s_40km_extrusion_type

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Similar to shifted_uniform_extrusion except that the top and
  !>        bottom layers are half the normal height and the remaining layers
  !>        have effectively been shifted by half a cell.
  type, public, extends(extrusion_type) :: shifted_uniform_extrusion_type
    private
  contains
    private
    procedure, public :: extrude => shifted_uniform_extrude
  end type shifted_uniform_extrusion_type

  interface shifted_uniform_extrusion_type
    module procedure shifted_uniform_extrusion_constructor
  end interface shifted_uniform_extrusion_type


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
  !> @brief Similar to the quadratic_extrusion_type except that the top and
  !>        bottom layers are half the normal height and the remaining layers
  !>        have effectively been shifted by half a cell.
  type, public, extends(extrusion_type) :: shifted_quadratic_extrusion_type
    private
  contains
    private
    procedure, public :: extrude => shifted_quadratic_extrude
  end type shifted_quadratic_extrusion_type

  interface shifted_quadratic_extrusion_type
    module procedure shifted_quadratic_extrusion_constructor
  end interface shifted_quadratic_extrusion_type

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
  !> @brief Similar to the geometric_extrusion_type except that the top and
  !>        bottom levels have half the height than geometric_extrusion_type
  !>        with the remaining layers effectively being shifted by half a cell.
  type, public, extends(extrusion_type) :: shifted_geometric_extrusion_type
    private
  contains
    private
    procedure, public :: extrude => shifted_geometric_extrude
  end type shifted_geometric_extrusion_type

  interface shifted_geometric_extrusion_type
    module procedure shifted_geometric_extrusion_constructor
  end interface shifted_geometric_extrusion_type

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Extrudes using DCMIP scheme.
  !>
  type, public, extends(extrusion_type) :: dcmip_extrusion_type
    private
  contains
    private
    procedure, public :: extrude => dcmip_extrude
  end type dcmip_extrusion_type

  interface dcmip_extrusion_type
    module procedure dcmip_extrusion_constructor
  end interface dcmip_extrusion_type

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Similar to dcmip_extrusion_type except that the top and
  !>        bottom levels have half the height than dcmip_extrusion_type
  !>        with the remaining layers effectively being shifted by half a cell.
  type, public, extends(extrusion_type) :: shifted_dcmip_extrusion_type
    private
  contains
    private
    procedure, public :: extrude => shifted_dcmip_extrude
  end type shifted_dcmip_extrusion_type

  interface shifted_dcmip_extrusion_type
    module procedure shifted_dcmip_extrusion_constructor
  end interface shifted_dcmip_extrusion_type

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
    real(r_def),                   intent(out) :: eta(0:this%number_of_layers)

    integer(i_def) :: k

    do k = 0, this%number_of_layers
      eta(k) = real(k,r_def)/real(this%number_of_layers,r_def)
    end do

  end subroutine uniform_extrude

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Creates a um_L38_29t_9s_40km_extrusion_type object.
  !>
  !> @param[in] atmosphere_bottom Bottom of the atmosphere in meters.
  !> @param[in] atmosphere_top Top of the atmosphere in meters.
  !> @param[in] number_of_layers Number of layers in the atmosphere.
  !>
  !> @return New uniform_extrusion_type object.
  !>
  function um_L38_29t_9s_40km_extrusion_constructor( atmosphere_bottom, &
                                                     atmosphere_top,    &
                                                     number_of_layers ) result(new)

    implicit none

    real(r_def),    intent(in) :: atmosphere_bottom
    real(r_def),    intent(in) :: atmosphere_top
    integer(i_def), intent(in) :: number_of_layers

    type(um_L38_29t_9s_40km_extrusion_type) :: new

    call new%extrusion_constructor( atmosphere_bottom, atmosphere_top, &
                                    number_of_layers )

  end function um_L38_29t_9s_40km_extrusion_constructor

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Extrudes the mesh with specific UM configuration L38_29t_9s_40km
  !>
  !> @param[out] eta Nondimensional vertical coordinate.
  !>
  subroutine um_L38_29t_9s_40km_extrude( this, eta )

    implicit none

    class(um_L38_29t_9s_40km_extrusion_type), intent(in)  :: this
    real(r_def),                   intent(out) :: eta(0:this%number_of_layers)


    if (this%number_of_layers /= 38)then
      call log_event( "Extrusion L38_29t_9s_40km reqires 38 levels", log_level_error )
    end if

    eta(0:this%number_of_layers) = (/ 0.0, &
                                      .0005095,  .0020380,  .0045854,  .0081519,  .0127373, &
                                      .0183417,  .0249651,  .0326074,  .0412688,  .0509491, &
                                      .0616485,  .0733668,  .0861040,  .0998603,  .1146356, &
                                      .1304298,  .1472430,  .1650752,  .1839264,  .2037966, &
                                      .2246857,  .2465938,  .2695209,  .2934670,  .3184321, &
                                      .3444162,  .3714396,  .3998142,  .4298913,  .4620737, &
                                      .4968308,  .5347160,  .5763897,  .6230643,  .6772068, &
                                      .7443435,  .8383348, 1.0000000 &
                                   /)

  end subroutine um_L38_29t_9s_40km_extrude

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Creates a shifted_uniform_extrusion_type object.
  !>
  !> @param[in] atmosphere_bottom Bottom of the atmosphere in meters.
  !> @param[in] atmosphere_top Top of the atmosphere in meters.
  !> @param[in] number_of_layers Number of layers in the atmosphere.
  !>
  !> @return New shifted_uniform_extrusion_type object.
  !>
  function shifted_uniform_extrusion_constructor( atmosphere_bottom, &
                                                  atmosphere_top,    &
                                                  number_of_layers ) result(new)

    implicit none

    real(r_def),    intent(in) :: atmosphere_bottom
    real(r_def),    intent(in) :: atmosphere_top
    integer(i_def), intent(in) :: number_of_layers

    type(shifted_uniform_extrusion_type) :: new

    call new%extrusion_constructor( atmosphere_bottom, atmosphere_top, &
                                    number_of_layers )

  end function shifted_uniform_extrusion_constructor


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Extrudes the mesh to give constant delta between layers except for
  !>        the top and bottom levels which are half the height of the other
  !>        layers.
  !>
  !> @param[out] eta Nondimensional vertical coordinate.
  !>
  subroutine shifted_uniform_extrude( this, eta )

    implicit none

    class(shifted_uniform_extrusion_type), intent(in)  :: this
    real(r_def), intent(out) :: eta(0:this%number_of_layers)

    integer(i_def) :: k

    eta(0) = 0.0_r_def
    do k = 1, this%number_of_layers-1
      eta(k) = (real(k,r_def)-0.5_r_def)/real(this%number_of_layers-1,r_def)
    end do

    eta(this%number_of_layers) = 1.0_r_def

  end subroutine shifted_uniform_extrude


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
    real(r_def),                     intent(out) :: eta(0:this%number_of_layers)

    integer(i_def) :: k

    do k = 0, this%number_of_layers
      eta(k) = ( real(k,r_def)/real(this%number_of_layers,r_def) )**2_i_def
    end do

  end subroutine quadratic_extrude


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Creates a shifted_quadratic_extrusion_type object.
  !>
  !> @param[in] atmosphere_bottom Bottom of the atmosphere in meters.
  !> @param[in] atmosphere_top Top of the atmosphere in meters.
  !> @param[in] number_of_layers Number of layers in the atmosphere.
  !>
  !> @return New shifted_quadratic_extrusion_type object.
  !>
  function shifted_quadratic_extrusion_constructor( atmosphere_bottom, &
                                                    atmosphere_top,    &
                                                    number_of_layers ) result(new)

    implicit none

    real(r_def),    intent(in) :: atmosphere_bottom
    real(r_def),    intent(in) :: atmosphere_top
    integer(i_def), intent(in) :: number_of_layers

    type(shifted_quadratic_extrusion_type) :: new

    call new%extrusion_constructor( atmosphere_bottom, atmosphere_top, &
                                    number_of_layers )

  end function shifted_quadratic_extrusion_constructor


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Extrudes the mesh to give a quadratic extrusion of layers but with
  !>        a half level at the top and bottom of the vertical extrusion.
  !>
  !> @param[out] eta Nondimensional vertical coordinate.
  !>
  subroutine shifted_quadratic_extrude( this, eta )

    implicit none

    class(shifted_quadratic_extrusion_type), intent(in)  :: this
    real(r_def),                     intent(out) :: eta(0:this%number_of_layers)

    integer(i_def) :: k

    eta(0) = 0.0_r_def

    do k = 1, this%number_of_layers-1
      eta(k) = ((real(k,r_def)-0.5_r_def)/real(this%number_of_layers-1,r_def))**2
    end do
    eta(this%number_of_layers) = 1.0_r_def

  end subroutine shifted_quadratic_extrude

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
    real(r_def),                     intent(out) :: eta(0:this%number_of_layers)

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
  !> @brief Creates a shifted geometric_extrusion_type object.
  !>
  !> @param[in] atmosphere_bottom Bottom of the atmosphere in meters.
  !> @param[in] atmosphere_top Top of the atmosphere in meters.
  !> @param[in] number_of_layers Number of layers in the atmosphere.
  !>
  !> @return New geometric_extrusion_type object.
  !>
  function shifted_geometric_extrusion_constructor( atmosphere_bottom, &
                                                    atmosphere_top,    &
                                                    number_of_layers ) &
                                                    result(new)

    implicit none

    real(r_def),    intent(in) :: atmosphere_bottom
    real(r_def),    intent(in) :: atmosphere_top
    integer(i_def), intent(in) :: number_of_layers

    type(shifted_geometric_extrusion_type) :: new

    call new%extrusion_constructor( atmosphere_bottom, atmosphere_top, &
                                    number_of_layers )

  end function shifted_geometric_extrusion_constructor



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Extrudes the mesh to give a John Thuburn ENDGame non-staggered grid.
  !>        The mesh is shifted vertically so that a half level exists at the
  !>        top and bottom level
  !>
  !> @param[out] eta Nondimensional vertical coordinate.
  !>
  subroutine shifted_geometric_extrude( this, eta )

    implicit none

    class(shifted_geometric_extrusion_type), intent(in)  :: this
    real(r_def),                     intent(out) :: eta(0:this%number_of_layers)

    integer(i_def)          :: k
    real(r_def), parameter  :: stretching_factor = 1.03_r_def
    real(r_def)             :: delta_eta

    eta(0) = 0.0_r_def
    eta(this%number_of_layers) = 1.0_r_def

    delta_eta = (stretching_factor - 1.0_r_def) &
                / (stretching_factor**(this%number_of_layers-1_i_def) - 1.0_r_def)

    do k = 1, this%number_of_layers-1
      eta(k) = geometric_func( stretching_factor, real(k,r_def)-0.5_r_def, delta_eta )
    end do


  end subroutine shifted_geometric_extrude


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Creates a dcmip_extrusion_type object.
  !>
  !> @param[in] atmosphere_bottom Bottom of the atmosphere in meters.
  !> @param[in] atmosphere_top Top of the atmosphere in meters.
  !> @param[in] number_of_layers Number of layers in the atmosphere.
  !>
  !> @return New dcmip_extrusion_type object.
  !>
  function dcmip_extrusion_constructor( atmosphere_bottom, &
                                        atmosphere_top,    &
                                        number_of_layers ) result(new)

    implicit none

    real(r_def),    intent(in) :: atmosphere_bottom
    real(r_def),    intent(in) :: atmosphere_top
    integer(i_def), intent(in) :: number_of_layers

    type(dcmip_extrusion_type) :: new

    call new%extrusion_constructor( atmosphere_bottom, atmosphere_top, &
                                    number_of_layers )

  end function dcmip_extrusion_constructor

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Extrudes the mesh using the DCMIP scheme.
  !>
  !> For more information see DCMIP-TestCaseDocument_v1.7.pdf,
  !> Appendix F.2. - Eq. 229.
  !>
  !> @param[out] eta Nondimensional vertical coordinate.
  !>
  subroutine dcmip_extrude( this, eta )

    implicit none

    class(dcmip_extrusion_type), intent(in)  :: this
    real(r_def),                 intent(out) :: eta(0:this%number_of_layers)

    real(r_def), parameter :: phi_flatten = 15.0_r_def

    integer(i_def) :: k

    do k = 0, this%number_of_layers
      eta(k) = dcmip_func(real(k,r_def)/real(this%number_of_layers,r_def))
    end do

  end subroutine dcmip_extrude

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Creates a vertically shifted dcmip_extrusion_type object.
  !>
  !> @param[in] atmosphere_bottom Bottom of the atmosphere in meters.
  !> @param[in] atmosphere_top Top of the atmosphere in meters.
  !> @param[in] number_of_layers Number of layers in the atmosphere.
  !>
  !> @return New dcmip_extrusion_type object.
  !>
  function shifted_dcmip_extrusion_constructor( atmosphere_bottom, &
                                        atmosphere_top,    &
                                        number_of_layers ) result(new)

    implicit none

    real(r_def),    intent(in) :: atmosphere_bottom
    real(r_def),    intent(in) :: atmosphere_top
    integer(i_def), intent(in) :: number_of_layers

    type(shifted_dcmip_extrusion_type) :: new

    call new%extrusion_constructor( atmosphere_bottom, atmosphere_top, &
                                    number_of_layers )

  end function shifted_dcmip_extrusion_constructor


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Extrudes the mesh using the DCMIP scheme and shifts the mesh
  !>        vertically by half a layer.
  !>
  !> For more information see DCMIP-TestCaseDocument_v1.7.pdf,
  !> Appendix F.2. - Eq. 229.
  !>
  !> @param[out] eta Nondimensional vertical coordinate.
  !>
  subroutine shifted_dcmip_extrude( this, eta )

    implicit none

    class(shifted_dcmip_extrusion_type), intent(in)  :: this
    real(r_def),                         intent(out) :: eta(0:this%number_of_layers)

    integer(i_def) :: k

    eta(0) = 0.0_r_def
    do k = 1,this%number_of_layers-1
      eta(k) = dcmip_func((real(k,r_def)-0.5_r_def)/real(this%number_of_layers-1,r_def))
    end do
    eta(this%number_of_layers) = 1.0_r_def

  end subroutine shifted_dcmip_extrude


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

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Helper function for generating DCMIP extrusion
  !>
  !> @param[in] eta_uni   Input value which increases incrementally with level number
  !> @return    eta       Vertical eta coordinate
  !>
  function dcmip_func(eta_uni) result(eta)
    implicit none

    real(r_def), intent(in) :: eta_uni
    real(r_def) :: eta

    real(r_def), parameter :: phi_flatten = 15.0_r_def

    eta = ( sqrt(phi_flatten*(eta_uni**2_i_def) + 1.0_r_def) &
                    - 1.0_r_def ) / &
                  ( sqrt(phi_flatten + 1.0_r_def) - 1.0_r_def )

  end function dcmip_func

end module extrusion_mod
