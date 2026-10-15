! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program operator_example

  use constants_mod,                 only : i_def
  use fs_continuity_mod,             only : W2, W3
  use function_space_collection_mod, only : function_space_collection
  use operator_mod,                  only : operator_type
  use testkern_operator_2_mod,       only : testkern_operator_2_type

  implicit none

  type(operator_type)                 :: my_mapping
  integer(i_def)                      :: mesh_id = 1
  integer(i_def)                      :: element_order_h = 0
  integer(i_def)                      :: element_order_v = 0

  my_mapping = operator_type(function_space_collection%get_fs(mesh_id,         &
                                                              element_order_h, &
                                                              element_order_v,W2), &
                             function_space_collection%get_fs(mesh_id,         &
                                                              element_order_h, &
                                                              element_order_v,W3))

  call invoke(testkern_operator_2_type(my_mapping))

end program operator_example
