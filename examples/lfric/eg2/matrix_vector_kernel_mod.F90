!-------------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-------------------------------------------------------------------------------
! LICENCE.original is available from the Met Office Science Repository Service:
! https://code.metoffice.gov.uk/trac/lfric/browser/LFRic/trunk/LICENCE.original
! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Modifications copyright (c) 2017-2020, Science and Technology Facilities Council
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
! Modified by I. Kavcic, Met Office

!> @brief Provides access to the members of the W1_solver_kernel and
!! W2_solver_kernel class.

!> @details Accessor functions for the W1 and W2_solver_kernel class are defined in this module.

!> @param solver_v1_code           Code to implement the solver for a v1 field
!> @param gaussian_quadrature      Contains result of quadrature (here Gaussian)

module matrix_vector_mod
use gaussian_quadrature_mod, only : gaussian_quadrature_type,              &
                                    ngp_h,ngp_v
use argument_mod,            only : arg_type,                              &
                                    gh_read, gh_inc, v2, fe, cells
use constants_mod,           only : dp
use kernel_mod,              only : kernel_type

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: matrix_vector_kernel_type
  private
  type(arg_type) :: meta_args(2) = [                                       &
       arg_type(gh_inc,v2,fe,.true.,.false.,.false.,.true.),               &
       arg_type(gh_read ,v2,fe,.false.,.false.,.false.,.false.)            &
       ]
  integer :: iterates_over = cells
contains
  procedure, nopass :: matrix_vector_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public matrix_vector_code

contains

subroutine matrix_vector_code(nlayers,ndf,map,basis,lhs,x,gq)
  ! compute the integral of v*x
  implicit none

  ! Arguments
  integer,                                     intent(in)    :: nlayers, ndf
  integer,                                     intent(in)    :: map(ndf)
  real(kind=dp), dimension(3,ndf,ngp_h,ngp_v), intent(in)    :: basis
  real(kind=dp),                               intent(in)    :: x(*)
  real(kind=dp),                               intent(inout) :: lhs(*)
  type(gaussian_quadrature_type),              intent(inout) :: gq

  ! Internal variables
  integer                                :: df1, df2, k
  integer                                :: qp1, qp2

  real(kind=dp), dimension(ndf)          :: x_e, lhs_e
  real(kind=dp), dimension(ngp_h,ngp_v)  :: f
  real(kind=dp), dimension(ndf,ndf)      :: mass_matrix
  real(kind=dp), dimension(1,3)          :: basisfun_i
  real(kind=dp), dimension(3,1)          :: basisfun_j
  real(kind=dp), dimension(1,1)          :: T_1

  ! compute the LHS integrated over one cell and solve
  do k = 0, nlayers-1
    do df1 = 1, ndf
       do df2 = df1, ndf
          do qp1 = 1, ngp_h
             do qp2 = 1, ngp_v
                basisfun_i(1,:) = basis(:,df1,qp1,qp2)
                basisfun_j(:,1) = basis(:,df2,qp1,qp2)
                T_1 = matmul(basisfun_i,basisfun_j)
                f(qp1,qp2) = T_1(1,1)
             end do
          end do
          mass_matrix(df1,df2) = gq%integrate(f)
       end do
       do df2 = df1, 1, -1
          mass_matrix(df1,df2) = mass_matrix(df2,df1)
       end do
       x_e(df1) = x(map(df1)+k)
    end do
    lhs_e = matmul(mass_matrix,x_e)
    ! push data to global array
    do df1 = 1,ndf
       lhs(map(df1)+k) = lhs(map(df1)+k) + lhs_e(df1)
    end do
 end do

end subroutine matrix_vector_code

end module matrix_vector_mod
