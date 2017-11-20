! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017, Science and Technology Facilities Council
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
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
! Author R. Ford and A. R. Porter, STFC Daresbury Lab

module operator_mod
  use constants_mod,            only : r_def
  use function_space_mod,       only : function_space_type
  use mesh_mod

  type, public :: base_operator_type
     integer :: a
     type( function_space_type ), pointer         :: fs_from => null( )
     type( function_space_type ), pointer         :: fs_to => null( )
   contains
     procedure, public :: get_mesh
  end type base_operator_type

  type, extends(base_operator_type) :: operator_type
     real(kind=r_def), allocatable         :: local_stencil( :, :, : )
     integer :: ncell_3d
   contains
     procedure, public :: get_proxy => get_proxy_op
  end type operator_type

  type, extends(base_operator_type) :: operator_proxy_type
     real(kind=r_def), allocatable         :: local_stencil( :, :, : )
     integer :: ncell_3d
  end type operator_proxy_type

  type, extends(base_operator_type) ::  columnwise_operator_type
     integer :: c
  end type columnwise_operator_type

  type, extends(base_operator_type) ::  columnwise_operator_proxy_type
     integer :: d
  end type columnwise_operator_proxy_type

contains

  type(operator_proxy_type ) function get_proxy_op(self)
    implicit none
    class(operator_type), target, intent(in)  :: self

    get_proxy_op % fs_from                 => null()
    get_proxy_op % fs_to                   => null()
  end function get_proxy_op

  
  function get_mesh(self) result(mesh)

    implicit none
    class (base_operator_type), intent(in) :: self
    type(mesh_type), pointer :: mesh
    mesh => null()
  end function get_mesh

end module operator_mod
