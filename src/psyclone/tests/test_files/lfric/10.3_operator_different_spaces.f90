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
  use field_mod,                     only : field_type
  use operator_mod,                  only : operator_type
  use quadrature_xyoz_mod,           only : quadrature_xyoz_type
  use assemble_weak_derivative_w3_w2_kernel_mod, &
                                     only : assemble_weak_derivative_w3_w2_kernel_type

  implicit none

  type(field_type)                    :: coord(3)
  type(operator_type)                 :: mapping
  type(quadrature_xyoz_type), pointer :: qr => null
  integer(i_def)                      :: mesh_id = 1
  integer(i_def)                      :: element_order_h = 0
  integer(i_def)                      :: element_order_v = 0

  ! Do not remove long lines, this is used to check psyclone line-breaking functionality
  mapping = operator_type(function_space_collection%get_fs(mesh_id,element_order_h,element_order_v,W3), function_space_collection%get_fs(mesh_id,element_order_h,element_order_v,W2))

  call invoke(assemble_weak_derivative_w3_w2_kernel_type(mapping, coord, qr))

end program operator_example
