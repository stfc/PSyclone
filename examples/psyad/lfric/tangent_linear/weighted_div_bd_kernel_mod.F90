!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> @brief Computes boundary part of the weighted divergence operator.
!>
!> @details The kernel computes the boundary part of the weighted divergence
!>          operator.
!>          This consists of \f[<\sigma,\theta*\mathbf{v}\cdot\mathbf{n}> \f]
!>          where sigma is the W3 test function, v is the W2 trial function,
!>          theta is the potential temperature, and \mathbf{n} is the outward
!>          pointing normal.
!>          Each face provides two contributions to a pressure point, one from
!>          the right side, using the potential temperature on the right side
!>          of the face and one from the left side, using the potential
!>          temperature on the left side of the face.
!>
!> @todo Create unit test for this kernel, see #2935
module weighted_div_bd_kernel_mod

  use argument_mod,      only : arg_type, func_type,         &
                                mesh_data_type,              &
                                reference_element_data_type, &
                                GH_OPERATOR, GH_FIELD,       &
                                GH_SCALAR, GH_REAL,          &
                                GH_READ, GH_READWRITE,       &
                                STENCIL, CROSS, GH_BASIS,    &
                                GH_QUADRATURE_face,          &
                                CELL_COLUMN, adjacent_face,  &
                                outward_normals_to_horizontal_faces
  use constants_mod,     only : r_def, i_def
  use fs_continuity_mod, only : W2, W3, Wtheta
  use kernel_mod,        only : kernel_type

  implicit none

  private

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------
  !> The type declaration for the kernel. Contains the metadata needed by the
  !> PSy layer.
  type, public, extends(kernel_type) :: weighted_div_bd_kernel_type
    private
    type(arg_type) :: meta_args(3) = (/                                        &
         arg_type(GH_OPERATOR, GH_REAL, GH_READWRITE, W2, W3),                 &
         arg_type(GH_FIELD,    GH_REAL, GH_READ,      Wtheta, STENCIL(CROSS)), &
         arg_type(GH_SCALAR,   GH_REAL, GH_READ)                               &
         /)
    type(func_type) :: meta_funcs(3) = (/                                      &
         func_type(W2,     GH_BASIS),                                          &
         func_type(W3,     GH_BASIS),                                          &
         func_type(Wtheta, GH_BASIS)                                           &
         /)
    type(mesh_data_type) :: meta_mesh(1) = (/                                  &
         mesh_data_type( adjacent_face )                                       &
         /)
    type(reference_element_data_type) :: meta_reference_element(1) = (/        &
         reference_element_data_type( outward_normals_to_horizontal_faces )    &
         /)
    integer :: operates_on = CELL_COLUMN
    integer :: gh_shape = GH_QUADRATURE_face
  contains
    procedure, nopass :: weighted_div_bd_code
  end type

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------
  public :: weighted_div_bd_code

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Computes the boundary terms in the weighted divergence for the
  !!        Helmholtz lhs.
  !>
  !> @param[in] cell Cell number
  !> @param[in] nlayers Number of layers
  !> @param[in] ncell_3d ncell*ndf
  !> @param[in,out] div Local stencil of the div operator
  !> @param[in] theta Potential temperature
  !> @param[in] stencil_wtheta_size Size of the Wtheta stencil (number of cells)
  !> @param[in] stencil_wtheta_map Wtheta dofmaps for the stencil
  !> @param[in] scalar Real to scale matrix by
  !> @param[in] ndf_w2 Number of degrees of freedom per cell for W2
  !> @param[in] w2_basis_face W2 basis functions evaluated at Gaussian
  !!                          quadrature points on horizontal faces
  !> @param[in] ndf_w3 Number of degrees of freedom per cell for W3
  !> @param[in] w3_basis_face W3 basis functions evaluated at Gaussian
  !!                          quadrature points on horizontal faces
  !> @param[in] ndf_wtheta Number of degrees of freedom per cell for Wtheta
  !> @param[in] undf_wtheta Number of unique degrees of freedom for Wtheta
  !> @param[in] map_wtheta Dofmap for the cell at the base of the column for
  !!                       Wtheta
  !> @param[in] wtheta_basis_face Wtheta basis functions evaluated at Gaussian
  !!                              quadrature points on horizontal faces
  !> @param[in] outward_normals_to_horizontal_faces Vector of normals to the
  !!                                                reference element horizontal
  !!                                                "outward faces"
  !> @param[in] nfaces_re_h Number of reference element faces bisected by a
  !!                        horizontal plane
  !> @param[in] adjacent_face Vector containing information on neighbouring
  !!                          face index for the current cell
  !> @param[in] nfaces_qr Number of faces in the quadrature rule
  !> @param[in] nqp_f Number of quadrature points on horizontal faces
  !> @param[in] wqp_f Quadrature weights on horizontal faces
  !>
  subroutine weighted_div_bd_code( cell, nlayers, ncell_3d,             &
                                   div, theta,                          &
                                   stencil_wtheta_size,                 &
                                   stencil_wtheta_map,                  &
                                   scalar,                              &
                                   ndf_w2, w2_basis_face,               &
                                   ndf_w3, w3_basis_face,               &
                                   ndf_wtheta, undf_wtheta, map_wtheta, &
                                   wtheta_basis_face,                   &
                                   nfaces_re_h,                         &
                                   outward_normals_to_horizontal_faces, &
                                   adjacent_face,                       &
                                   nfaces_qr, nqp_f, wqp_f )

    implicit none

    ! Arguments
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: cell, ncell_3d
    integer(kind=i_def), intent(in) :: nfaces_qr, nqp_f
    integer(kind=i_def), intent(in) :: ndf_w2, ndf_w3
    integer(kind=i_def), intent(in) :: ndf_wtheta, undf_wtheta
    integer(kind=i_def), intent(in) :: nfaces_re_h

    integer(kind=i_def), intent(in) :: stencil_wtheta_size
    integer(kind=i_def), dimension(ndf_wtheta,stencil_wtheta_size), intent(in) :: stencil_wtheta_map

    integer(kind=i_def), dimension(ndf_wtheta), intent(in) :: map_wtheta

    real(kind=r_def), dimension(3,ndf_w2,nqp_f,nfaces_qr),     intent(in) :: w2_basis_face
    real(kind=r_def), dimension(1,ndf_w3,nqp_f,nfaces_qr),     intent(in) :: w3_basis_face
    real(kind=r_def), dimension(1,ndf_wtheta,nqp_f,nfaces_qr), intent(in) :: wtheta_basis_face
    real(kind=r_def), dimension(nqp_f,nfaces_qr),              intent(in) :: wqp_f

    real(kind=r_def), dimension(ndf_w2,ndf_w3,ncell_3d), intent(inout) :: div
    real(kind=r_def), dimension(undf_wtheta),            intent(in)    :: theta
    real(kind=r_def),                                    intent(in)    :: scalar

    integer(kind=i_def), intent(in) :: adjacent_face(nfaces_re_h)

    real(kind=r_def),    intent(in) :: outward_normals_to_horizontal_faces(:,:)

    ! Internal variables
    integer(kind=i_def) :: df, df2, df3, k, ik, face, face_next
    integer(kind=i_def) :: qp

    real(kind=r_def), dimension(ndf_wtheta) :: theta_e, theta_next_e

    real(kind=r_def) :: v_dot_n, integrand
    real(kind=r_def) :: theta_at_fquad, theta_next_at_fquad
    real(kind=r_def) :: this_bd_term, next_bd_term

    ! If we're near the edge of the regional domain then the
    ! stencil size will be less that 5 so don't do anything here
    ! This should be removed with lfric ticket #2958
    if (stencil_wtheta_size < 5)return

    do k = 0, nlayers - 1
      ik = k + 1 + (cell-1)*nlayers

      do face = 1, nfaces_re_h

        face_next = adjacent_face(face)

        do df = 1,ndf_wtheta
          theta_e(df)      = theta(stencil_wtheta_map(df, 1)      + k)
          theta_next_e(df) = theta(stencil_wtheta_map(df, face+1) + k)
        end do

        do qp = 1, nqp_f
          theta_at_fquad      = 0.0_r_def
          theta_next_at_fquad = 0.0_r_def

          do df = 1, ndf_wtheta
            theta_at_fquad       = theta_at_fquad      + theta_e(df)     * &
                                   wtheta_basis_face(1,df,qp,face)
            theta_next_at_fquad  = theta_next_at_fquad + theta_next_e(df)* &
                                   wtheta_basis_face(1,df,qp,face_next)
          end do

          do df3 = 1, ndf_w3
            do df2 = 1, ndf_w2
              v_dot_n  = dot_product(w2_basis_face(:,df2,qp,face), &
                                     outward_normals_to_horizontal_faces(:,face))
              this_bd_term =  v_dot_n*theta_at_fquad
              next_bd_term = -v_dot_n*theta_next_at_fquad
              integrand = wqp_f(qp,face)*w3_basis_face(1,df3,qp,face) &
                           * 0.5_r_def*(this_bd_term + next_bd_term)
              div(df2,df3,ik) = div(df2,df3,ik) - scalar*integrand
            end do ! df2
          end do ! df3

        end do ! qp

      end do ! faces

    end do ! layers

  end subroutine weighted_div_bd_code

 end module weighted_div_bd_kernel_mod
