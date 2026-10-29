! -----------------------------------------------------------------------------
! SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
!                         Facilities Council
! SPDX-License-Identifier: BSD-3-Clause
! See the full LICENSE file in the project root for details.
! -----------------------------------------------------------------------------

program operator_example

  use constants_mod,                        only : i_def
  use fs_continuity_mod,                    only : W2
  use function_space_collection_mod,        only : function_space_collection
  use field_mod,                            only : field_type
  use operator_mod,                         only : operator_type
  use quadrature_xyoz_mod,                  only : quadrature_xyoz_type
  use testkern_operator_nofield_scalar_mod, only : testkern_operator_nofield_scalar_type

  implicit none

  type(operator_type)                 :: my_mapping
  type(quadrature_xyoz_type), pointer :: qr => null
  integer(i_def)                      :: mesh_id = 1
  integer(i_def)                      :: element_order_h = 0
  integer(i_def)                      :: element_order_v = 0
  integer(i_def)                      :: b(3)

  my_mapping = operator_type(function_space_collection%get_fs(mesh_id,         &
                                                              element_order_h, &
                                                              element_order_v,W2), &
                             function_space_collection%get_fs(mesh_id,         &
                                                              element_order_h, &
                                                              element_order_v,W2))

  call invoke(testkern_operator_nofield_scalar_type(my_mapping, b(1), qr))

end program operator_example
