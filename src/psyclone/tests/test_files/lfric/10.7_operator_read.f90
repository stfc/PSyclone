! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program operator_example

  use constants_mod,                 only : i_def, r_def
  use fs_continuity_mod,             only : W3
  use function_space_collection_mod, only : function_space_collection
  use field_mod,                     only : field_type
  use operator_mod,                  only : operator_type
  use quadrature_xyoz_mod,           only : quadrature_xyoz_type
  use testkern_operator_read_mod,    only : testkern_operator_read_type

  type(field_type)                    :: coord(3)
  type(operator_type)                 :: mm_w3
  type(quadrature_xyoz_type), pointer :: qr => null
  integer(i_def)                      :: mesh_id = 1
  integer(i_def)                      :: element_order_h = 0
  integer(i_def)                      :: element_order_v = 0
  integer(i_def)                      :: a

  a = 1_i_def
  mm_w3 = operator_type(function_space_collection%get_fs(mesh_id,         &
                                                         element_order_h, &
                                                         element_order_v,W3), &
                        function_space_collection%get_fs(mesh_id,         &
                                                         element_order_h, &
                                                         element_order_v,W3))

  call invoke(testkern_operator_read_type(mm_w3, coord, a, qr))

end program operator_example
