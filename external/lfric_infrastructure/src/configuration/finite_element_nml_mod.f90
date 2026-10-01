!-----------------------------------------------------------------------------
! (C) Crown copyright 2025 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> Manages the finite_element namelist.
!>
module finite_element_nml_mod

  use constants_mod, only: i_def, &
                           l_def, &
                           str_def

  use namelist_mod, only: namelist_type

  implicit none

  private
  public :: finite_element_nml_type

  type, extends(namelist_type) :: finite_element_nml_type
    private
  contains

    procedure :: cellshape
    procedure :: coord_order
    procedure :: coord_order_nonprime
    procedure :: coord_space
    procedure :: coord_system
    procedure :: element_order_h
    procedure :: element_order_v
    procedure :: nqp_h_exact
    procedure :: nqp_v_exact
    procedure :: rehabilitate
    procedure :: vorticity_in_w1

  end type finite_element_nml_type

contains


  function cellshape(self) result(answer)

    implicit none

    class(finite_element_nml_type), intent(in) :: self
    integer(i_def) :: answer

    call self%get_value('cellshape', answer)

  end function cellshape


  function coord_order(self) result(answer)

    implicit none

    class(finite_element_nml_type), intent(in) :: self
    integer(i_def) :: answer

    call self%get_value('coord_order', answer)

  end function coord_order


  function coord_order_nonprime(self) result(answer)

    implicit none

    class(finite_element_nml_type), intent(in) :: self
    integer(i_def) :: answer

    call self%get_value('coord_order_nonprime', answer)

  end function coord_order_nonprime


  function coord_space(self) result(answer)

    implicit none

    class(finite_element_nml_type), intent(in) :: self
    integer(i_def) :: answer

    call self%get_value('coord_space', answer)

  end function coord_space


  function coord_system(self) result(answer)

    implicit none

    class(finite_element_nml_type), intent(in) :: self
    integer(i_def) :: answer

    call self%get_value('coord_system', answer)

  end function coord_system


  function element_order_h(self) result(answer)

    implicit none

    class(finite_element_nml_type), intent(in) :: self
    integer(i_def) :: answer

    call self%get_value('element_order_h', answer)

  end function element_order_h


  function element_order_v(self) result(answer)

    implicit none

    class(finite_element_nml_type), intent(in) :: self
    integer(i_def) :: answer

    call self%get_value('element_order_v', answer)

  end function element_order_v


  function nqp_h_exact(self) result(answer)

    implicit none

    class(finite_element_nml_type), intent(in) :: self
    integer(i_def) :: answer

    call self%get_value('nqp_h_exact', answer)

  end function nqp_h_exact


  function nqp_v_exact(self) result(answer)

    implicit none

    class(finite_element_nml_type), intent(in) :: self
    integer(i_def) :: answer

    call self%get_value('nqp_v_exact', answer)

  end function nqp_v_exact


  function rehabilitate(self) result(answer)

    implicit none

    class(finite_element_nml_type), intent(in) :: self
    logical(l_def) :: answer

    call self%get_value('rehabilitate', answer)

  end function rehabilitate


  function vorticity_in_w1(self) result(answer)

    implicit none

    class(finite_element_nml_type), intent(in) :: self
    logical(l_def) :: answer

    call self%get_value('vorticity_in_w1', answer)

  end function vorticity_in_w1

end module finite_element_nml_mod