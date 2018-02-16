! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017-2018, Science and Technology Facilities Council
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
! Modified I. Kavcic Met Office

program single_invokes_cma_discontinuous

  ! Description: two single invokes containing multiple CMA-related kernels
  !              on discontinuous spaces w3 and w2v
  use argument_mod,          only : W3, W2V, ANY_SPACE_1, ANY_SPACE_2
  use field_mod,             only : field_type
  use function_space_mod,    only : function_space_type
  use operator_mod,          only : columnwise_operator_type, &
                                    columnwise_operator_proxy_type
  use columnwise_op_app_w3_kernel_mod,  only: &
                            columnwise_op_app_w3_kernel_type
  use columnwise_op_app_w2v_kernel_mod, only: &
                            columnwise_op_app_w2v_kernel_type

  implicit none

  type(field_type)               :: field_a, field_b
  type(field_type)               :: field_c, field_d
  type(columnwise_operator_type) :: cma_op1, cma_op2
  type(function_space_type), pointer :: fs_w3 => null()
  type(function_space_type)      :: fs

! ! !   field_a = field_type( mesh, vector_space = fs%get_instance(W3)          )
! ! !   field_b = field_type( mesh, vector_space = fs%get_instance(ANY_SPACE_1) )
! ! !   field_c = field_type( mesh, vector_space = fs%get_instance(W2V)         )
! ! !   field_d = field_type( mesh, vector_space = fs%get_instance(ANY_SPACE_2) )

!   fs_w3    => function_space_collection%get_fs(mesh_id,       &
!                                   element_order, &
!                                    W2V)

  cma_op1 = columnwise_operator_type( fs%get_instance(W3),fs%get_instance(ANY_SPACE_1)  )
  cma_op2 = columnwise_operator_type( fs%get_instance(W2V),fs%get_instance(ANY_SPACE_2) )

  call invoke( &
         columnwise_op_app_w3_kernel_type(field_a, field_b, cma_op1) )
  call invoke( &
         columnwise_op_app_w2v_kernel_type(field_c, field_d, cma_op2) )

end program single_invokes_cma_discontinuous
