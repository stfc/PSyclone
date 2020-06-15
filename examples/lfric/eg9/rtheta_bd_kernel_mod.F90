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
! Modifications copyright (c) 2018-2020, Science and Technology Facilities
! Council.
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
! Modified by: I. Kavcic, Met Office
!              A. R. Porter, STFC Daresbury Laboratory
!-------------------------------------------------------------------------------
!> @brief Kernel which computes the boundary integral part of rhs of the
!>        thermodynamic equation for the nonlinear equations.
!>
!> @details The kernel computes the boundary integral on rhs of the
!>          thermodynamic equation for the nonlinear equations This consists of:
!>          rtheta_bd = theta * gamma * u * normal.
!>
module rtheta_bd_kernel_mod

  use argument_mod,          only : arg_type, func_type,         &
                                    mesh_data_type,              &
                                    reference_element_data_type, &
                                    GH_FIELD, GH_READ,           &
                                    GH_READWRITE,                &
                                    STENCIL, CROSS,              &
                                    GH_BASIS, GH_DIFF_BASIS,     &
                                    CELLS, GH_QUADRATURE_face,   &
                                    adjacent_face,               &
                                    normals_to_horizontal_faces, &
                                    outward_normals_to_horizontal_faces
  use constants_mod,         only : r_def, i_def, l_def
  use cross_product_mod,     only : cross_product
  use fs_continuity_mod,     only : W2, Wtheta
  use kernel_mod,            only : kernel_type
  use planet_config_mod,     only : cp

  implicit none

  private

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------
  !> The type declaration for the kernel. Contains the metadata needed by the
  !> PSy layer.
  type, public, extends(kernel_type) :: rtheta_bd_kernel_type
    private
    type(arg_type) :: meta_args(3) = (/                                     &
         arg_type(GH_FIELD, GH_READWRITE, Wtheta),                          &
         arg_type(GH_FIELD, GH_READ,      Wtheta, STENCIL(CROSS)),          &
         arg_type(GH_FIELD, GH_READ,      W2,     STENCIL(CROSS))           &
         /)
    type(func_type) :: meta_funcs(2) = (/                                   &
         func_type(W2,     GH_BASIS),                                       &
         func_type(Wtheta, GH_BASIS)                                        &
         /)
    type(mesh_data_type) :: meta_mesh(1) = (/                               &
         mesh_data_type( adjacent_face )                                    &
         /)
    type(reference_element_data_type) :: meta_reference_element(2) = (/     &
         reference_element_data_type( normals_to_horizontal_faces ),        &
         reference_element_data_type( outward_normals_to_horizontal_faces ) &
         /)
    integer :: iterates_over = CELLS
    integer :: gh_shape = GH_QUADRATURE_face
  contains
    procedure, nopass :: rtheta_bd_code
  end type

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------
  public rtheta_bd_code

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Computes the boundary integral terms in the potential temperature.
  !>
  !> @param[in] nlayers Number of layers
  !> @param[in,out] r_theta_bd Right-hand side of the thermodynamic equation
  !> @param[in] theta Potential temperature field
  !> @param[in] stencil_wtheta_size Size of the Wtheta stencil (number of cells)
  !> @param[in] stencil_wtheta_map Wtheta dofmaps for the stencil
  !> @param[in] u Wind field
  !> @param[in] stencil_w2_size Size of the W2 stencil (number of cells)
  !> @param[in] stencil_w2_map W2 dofmaps for the stencil
  !> @param[in] ndf_wtheta Number of degrees of freedom per cell for Wtheta
  !> @param[in] undf_wtheta Number of unique degrees of freedom for Wtheta
  !> @param[in] map_wtheta Dofmap for the cell at the base of the column for
  !!                       Wtheta
  !> @param[in] wtheta_basis_face Wtheta basis functions evaluated at Gaussian
  !!                              quadrature points on horizontal faces
  !> @param[in] ndf_w2 Number of degrees of freedom per cell for W2
  !> @param[in] undf_w2 Number of unique degrees of freedom for W2
  !> @param[in] map_w2 Dofmap for the cell at the base of the column for W2
  !> @param[in] w2_basis_face W2 basis functions evaluated at Gaussian
  !!                          quadrature points on horizontal faces
  !> @param[in] nfaces_re_h Number of reference element faces bisected by a
  !!                        horizontal plane
  !> @param[in] normals_to_horizontal_faces Vector of normals to the reference
  !!                                        element horizontal faces
  !> @param[in] outward_normals_to_horizontal_faces Vector of normals to the
  !!                                                reference element horizontal
  !!                                                "outward faces"
  !> @param[in] adjacent_face Vector containing information on neighbouring
  !!                          face index for the current cell
  !> @param[in] nfaces_qr Number of faces in the quadrature rule
  !> @param[in] nqp_f Number of quadrature points on horizontal faces
  !> @param[in] wqp_f Quadrature weights on horizontal faces
  !>
  subroutine rtheta_bd_code( nlayers,                             &
                             r_theta_bd, theta,                   &
                             stencil_wtheta_size,                 &
                             stencil_wtheta_map,                  &
                             u,                                   &
                             stencil_w2_size, stencil_w2_map,     &
                             ndf_wtheta, undf_wtheta, map_wtheta, &
                             wtheta_basis_face,                   &
                             ndf_w2, undf_w2, map_w2,             &
                             w2_basis_face,                       &
                             nfaces_re_h,                         &
                             normals_to_horizontal_faces,         &
                             outward_normals_to_horizontal_faces, &
                             adjacent_face,                       &
                             nfaces_qr, nqp_f, wqp_f )

    implicit none

    ! Arguments
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: nfaces_qr, nqp_f
    integer(kind=i_def), intent(in) :: ndf_w2, undf_w2
    integer(kind=i_def), intent(in) :: ndf_wtheta, undf_wtheta
    integer(kind=i_def), intent(in) :: nfaces_re_h

    integer(kind=i_def), intent(in) :: stencil_w2_size
    integer(kind=i_def), intent(in) :: stencil_wtheta_size

    integer(kind=i_def), dimension(ndf_w2,stencil_w2_size),         intent(in) :: stencil_w2_map
    integer(kind=i_def), dimension(ndf_wtheta,stencil_wtheta_size), intent(in) :: stencil_wtheta_map

    integer(kind=i_def), dimension(ndf_w2),     intent(in) :: map_w2
    integer(kind=i_def), dimension(ndf_wtheta), intent(in) :: map_wtheta

    real(kind=r_def), dimension(3,ndf_w2,nqp_f,nfaces_qr),     intent(in) :: w2_basis_face
    real(kind=r_def), dimension(1,ndf_wtheta,nqp_f,nfaces_qr), intent(in) :: wtheta_basis_face
    real(kind=r_def), dimension(nqp_f,nfaces_qr),              intent(in) :: wqp_f

    integer(kind=i_def), intent(in) :: adjacent_face(:)

    real(kind=r_def),    intent(in) :: normals_to_horizontal_faces(:,:)
    real(kind=r_def),    intent(in) :: outward_normals_to_horizontal_faces(:,:)

    real(kind=r_def), dimension(undf_wtheta), intent(inout) :: r_theta_bd
    real(kind=r_def), dimension(undf_wtheta), intent(in)    :: theta
    real(kind=r_def), dimension(undf_w2),     intent(in)    :: u

    ! Internal variables
    integer(kind=i_def) :: df, k, face, face_next
    integer(kind=i_def) :: qp
    integer(kind=i_def) :: i_face

    real(kind=r_def), dimension(ndf_wtheta) :: theta_e, theta_next_e
    real(kind=r_def), dimension(ndf_wtheta) :: rtheta_bd_e
    real(kind=r_def), dimension(ndf_w2)     :: u_e, u_next_e

    real(kind=r_def) :: u_at_uquad(3), u_next_at_uquad(3), face_next_inward_normal(3)
    real(kind=r_def) :: theta_at_uquad, theta_next_at_uquad
    real(kind=r_def) :: bdary_term, gamma_wtheta, sign_face_next_outward, flux_term

    logical(kind=l_def) :: upwind = .false.

    ! Assumes same number of horizontal qp in x and y

    do k = 0, nlayers-1

      do df = 1, ndf_wtheta
        rtheta_bd_e(df) = 0.0_r_def
      end do

      do face = 1, nfaces_re_h

        ! Storing opposite face number on neighbouring cell
        face_next = adjacent_face(face)
        i_face = int(floor(real(mod(face_next, nfaces_re_h),r_def)/2.0_r_def) + 1.0_r_def)
        sign_face_next_outward = (-1.0_r_def)**i_face
        face_next_inward_normal(:) = -sign_face_next_outward &
                                     * normals_to_horizontal_faces(:,face_next)

        ! Computing theta and f in adjacent cells

        do df = 1, ndf_w2
          u_e(df)      = u( stencil_w2_map(df, 1) + k )
          u_next_e(df) = u( stencil_w2_map(df, face+1) + k )
        end do

        do df = 1, ndf_wtheta
          theta_e(df)      = theta( stencil_wtheta_map(df, 1) + k )
          theta_next_e(df) = theta( stencil_wtheta_map(df, face+1) + k )
        end do

        ! Compute the boundary RHS integrated over one cell
        do qp = 1, nqp_f
          theta_at_uquad = 0.0_r_def
          theta_next_at_uquad = 0.0_r_def

          do df = 1, ndf_wtheta
            theta_at_uquad       = theta_at_uquad +      &
                                   theta_e(df)*wtheta_basis_face(1,df,qp,face)
            theta_next_at_uquad  = theta_next_at_uquad + &
                                   theta_next_e(df)*wtheta_basis_face(1,df,qp,face_next)
          end do

          u_at_uquad(:) = 0.0_r_def
          u_next_at_uquad(:) = 0.0_r_def

          do df = 1, ndf_w2
            u_at_uquad(:)       = u_at_uquad(:)      + u_e(df)     *w2_basis_face(:,df,qp,face)
            u_next_at_uquad(:)  = u_next_at_uquad(:) + u_next_e(df)*w2_basis_face(:,df,qp,face_next)
          end do

          flux_term = 0.5_r_def * (theta_next_at_uquad *                  &
                                   dot_product(u_next_at_uquad,           &
                                               face_next_inward_normal) + &
                                   theta_at_uquad      *                  &
                                   dot_product(u_at_uquad,                &
                                       outward_normals_to_horizontal_faces(:, face)))

          if (upwind) then
            flux_term = flux_term + 0.5_r_def *                                     &
                            abs(dot_product(                                        &
                                   u_at_uquad,                                      &
                                   outward_normals_to_horizontal_faces(:, face))) * &
                               (dot_product(                                        &
                                   theta_at_uquad *                                 &
                                   outward_normals_to_horizontal_faces(:, face),    &
                                   outward_normals_to_horizontal_faces(:, face)) -  &
                                dot_product(                                        &
                                   theta_next_at_uquad *                            &
                                   face_next_inward_normal,                         &
                                   face_next_inward_normal))
          end if

          do df = 1, ndf_wtheta
            gamma_wtheta  = wtheta_basis_face(1,df,qp,face)

            bdary_term = gamma_wtheta * flux_term
            rtheta_bd_e(df) = rtheta_bd_e(df) +  wqp_f(qp,face) * bdary_term
          end do

        end do ! qp

      end do ! faces

      do df = 1, ndf_wtheta
        r_theta_bd( stencil_wtheta_map(df, 1) + k ) =  &
                    r_theta_bd( stencil_wtheta_map(df, 1) + k ) + rtheta_bd_e(df)
      end do

    end do ! layers

  end subroutine rtheta_bd_code

end module rtheta_bd_kernel_mod
