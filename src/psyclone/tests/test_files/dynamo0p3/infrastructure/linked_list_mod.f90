!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------

! BSD 3-Clause License
!
! Modifications copyright (c) 2017, Science and Technology Facilities Council
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

module linked_list_mod

  use linked_list_data_mod, only    : linked_list_data_type
  use constants_mod,        only    : i_def

  implicit none

  private

  integer(i_def), parameter, public :: before = -1
  integer(i_def), parameter, public :: after = 1

  type, public                           :: linked_list_type
    private
    integer(i_def) :: length

    type(linked_list_item_type), pointer :: head => null()
    type(linked_list_item_type), pointer :: tail => null()  
    type(linked_list_item_type), pointer :: current => null()

  contains
    procedure, public              :: insert_item
    procedure, public              :: item_exists
    procedure, public              :: get_length
    procedure, public              :: get_current
    procedure, public              :: get_head
    procedure, public              :: get_tail
    procedure, public              :: clear
  end type linked_list_type

  type, public :: linked_list_item_type
    class(linked_list_data_type), pointer :: payload => null()
    type(linked_list_item_type), pointer :: prev => null()
    type(linked_list_item_type), pointer :: next => null()
  end type linked_list_item_type

  interface linked_list_type
     module procedure linked_list_constructor
  end interface linked_list_type

contains

type(linked_list_type) function linked_list_constructor()

  linked_list_constructor%length = 0
end function linked_list_constructor

function get_length(self) result(length)

  class(linked_list_type), intent (in)  :: self
  integer(i_def)                   :: length

  length = 0
end function get_length

function get_current(self) result(curr_item)

  class(linked_list_type), intent (in)  :: self
  class(linked_list_item_type),pointer  :: curr_item

  curr_item => null()
end function get_current

function get_head(self) result(head_item)

  class(linked_list_type), intent (in)  :: self
  class(linked_list_item_type),pointer  :: head_item

  head_item => null()
end function get_head

function get_tail(self) result(tail_item)

  class(linked_list_type), intent (in)  :: self
  class(linked_list_item_type),pointer  :: tail_item

  tail_item => null()
end function get_tail

function item_exists(this, id, start, finish) result(exists)

  class(linked_list_type), intent(in)   :: this
  integer(i_def), intent(in)            :: id
  logical                               :: exists
  type(linked_list_item_type),optional,pointer,intent(in)  :: start
  type(linked_list_item_type),optional,pointer,intent(in)  :: finish

  exists = .false.
  return
end function item_exists

subroutine insert_item(self, new_data, insert_point, placement)

  class(linked_list_type), intent (inout)   :: self
  class(linked_list_data_type), intent (in) :: new_data
  type(linked_list_item_type), optional, pointer, intent(inout) :: insert_point
  integer(i_def), optional, intent (in)                      :: placement

end subroutine insert_item

subroutine clear(self)

  class(linked_list_type), intent(inout):: self
end subroutine clear

end module linked_list_mod
