!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> @brief Computes the boundary part of the projection operator from the
!>        potential temperature space to the velocity space weighted by the
!>        pressure gradient.
!>
!> @details The kernel computes the boundary projection operator
!>          \f[<v.n,{\Pi}*\gamma>\f] where v is in W2, gamma is in the
!>          potential temperature space and exner is computed pointwise from
!>          the equation of state.
!>
!> @todo Create unit test for this kernel, see #2935
module weighted_proj_2theta_bd_kernel_mod

  use argument_mod,      only: arg_type, func_type,         &
                               mesh_data_type,              &
                               reference_element_data_type, &
                               GH_OPERATOR, GH_FIELD,       &
                               GH_SCALAR, GH_REAL,          &
                               GH_READ, GH_READWRITE,       &
                               STENCIL, CROSS, GH_BASIS,    &
                               GH_QUADRATURE_face,          &
                               CELL_COLUMN, adjacent_face,  &
                               outward_normals_to_horizontal_faces
  use constants_mod,     only: r_def, i_def
  use fs_continuity_mod, only: W2, W3, Wtheta
  use kernel_mod,        only: kernel_type

  implicit none

  private

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------
  !> The type declaration for the kernel. Contains the metadata needed by the
  !> PSy layer.
  type, public, extends(kernel_type) :: weighted_proj_2theta_bd_kernel_type
    private
    type(arg_type) :: meta_args(3) = (/                                     &
         arg_type(GH_OPERATOR, GH_REAL, GH_READWRITE, W2, Wtheta),          &
         arg_type(GH_FIELD,    GH_REAL, GH_READ,      W3, STENCIL(CROSS)),  &
         arg_type(GH_SCALAR,   GH_REAL, GH_READ)                            &
         /)
    type(func_type) :: meta_funcs(3) = (/                                   &
         func_type(W2,     GH_BASIS),                                       &
         func_type(Wtheta, GH_BASIS),                                       &
         func_type(W3,     GH_BASIS)                                        &
         /)
    type(mesh_data_type) :: meta_mesh(1) = (/                               &
         mesh_data_type( adjacent_face )                                    &
          /)
    type(reference_element_data_type) :: meta_reference_element(1) = (/     &
         reference_element_data_type( outward_normals_to_horizontal_faces ) &
         /)
    integer :: operates_on = CELL_COLUMN
    integer :: gh_shape = GH_QUADRATURE_face
  contains
    procedure, nopass :: weighted_proj_2theta_bd_code
  end type

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------
  public :: weighted_proj_2theta_bd_code

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Computes the weighted projection from Wtheta to W2.
  !>
  !> @param[in] cell Cell number
  !> @param[in] nlayers Number of layers
  !> @param[in] ncell_3d ncell*ndf
  !> @param[in,out] projection Projection operator to compute
  !> @param[in] exner Exner presssure
  !> @param[in] stencil_w3_size Size of the W3 stencil (number of cells)
  !> @param[in] stencil_w3_map W3 dofmaps for the stencil
  !> @param[in] scalar Real to scale matrix by
  !> @param[in] ndf_w2 Number of degrees of freedom per cell for W2
  !> @param[in] w2_basis_face W2 basis functions evaluated at Gaussian
  !!                          quadrature points on horizontal faces
  !> @param[in] ndf_wtheta Number of degrees of freedom per cell for Wtheta
  !> @param[in] wtheta_basis_face Wtheta basis functions evaluated at Gaussian
  !!                              quadrature points on horizontal faces
  !> @param[in] ndf_w3 Number of degrees of freedom per cell for W3
  !> @param[in] undf_w3 Number of unique degrees of freedom for W3
  !> @param[in] map_w3 Dofmap for the cell at the base of the column for W3
  !> @param[in] w3_basis_face W3 basis functions evaluated at Gaussian
  !!                          quadrature points on horizontal faces
  !> @param[in] nfaces_re_h Number of reference element faces bisected by a
  !!                        horizontal plane
  !> @param[in] outward_normals_to_horizontal_faces Vector of normals to the
  !!                                                reference element horizontal
  !!                                                "outward faces"
  !> @param[in] adjacent_face Vector containing information on neighbouring
  !!                          face index for the current cell
  !> @param[in] nfaces_qr Number of faces in the quadrature rule
  !> @param[in] nqp_f Number of quadrature points on horizontal faces
  !> @param[in] wqp_f Quadrature weights on horizontal faces
  !>
  subroutine weighted_proj_2theta_bd_code( cell, nlayers, ncell_3d,             &
                                           projection, exner,                   &
                                           stencil_w3_size, stencil_w3_map,     &
                                           scalar,                              &
                                           ndf_w2, w2_basis_face,               &
                                           ndf_wtheta, wtheta_basis_face,       &
                                           ndf_w3, undf_w3, map_w3,             &
                                           w3_basis_face,                       &
                                           nfaces_re_h,                         &
                                           outward_normals_to_horizontal_faces, &
                                           adjacent_face,                       &
                                           nfaces_qr, nqp_f, wqp_f )

    use calc_exner_pointwise_mod, only: calc_exner_pointwise

    implicit none

    ! Arguments
    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: nfaces_qr, nqp_f
    integer(kind=i_def), intent(in) :: ncell_3d
    integer(kind=i_def), intent(in) :: undf_w3, ndf_w3, ndf_w2, ndf_wtheta
    integer(kind=i_def), intent(in) :: nfaces_re_h

    integer(kind=i_def), intent(in) :: stencil_w3_size
    integer(kind=i_def), dimension(ndf_w3, stencil_w3_size), intent(in) :: stencil_w3_map

    integer(kind=i_def), dimension(ndf_w3), intent(in) :: map_w3

    real(kind=r_def), dimension(3,ndf_w2,nqp_f,nfaces_qr),     intent(in) :: w2_basis_face
    real(kind=r_def), dimension(1,ndf_w3,nqp_f,nfaces_qr),     intent(in) :: w3_basis_face
    real(kind=r_def), dimension(1,ndf_wtheta,nqp_f,nfaces_qr), intent(in) :: wtheta_basis_face
    real(kind=r_def), dimension(nqp_f,nfaces_qr),              intent(in) :: wqp_f

    real(kind=r_def), dimension(ndf_w2,ndf_wtheta,ncell_3d), intent(inout) :: projection
    real(kind=r_def), dimension(undf_w3),                    intent(in)    :: exner
    real(kind=r_def),                                        intent(in)    :: scalar

    integer(kind=i_def), intent(in) :: adjacent_face(:)

    real(kind=r_def),    intent(in) :: outward_normals_to_horizontal_faces(:,:)

    ! Internal variables
    integer(kind=i_def)                 :: df, df0, df2, k, ik, face, face_next
    integer(kind=i_def)                 :: qp
    real(kind=r_def), dimension(ndf_w3) :: exner_e, exner_next_e

    real(kind=r_def)                    :: v(3), normal(3), integrand
    real(kind=r_def)                    :: exner_at_fquad, &
                                           exner_next_at_fquad, &
                                           exner_av

    ! If we're near the edge of the regional domain then the
    ! stencil size will be less that 5 so don't do anything here
    ! This should be removed with lfric ticket #2958
    if (stencil_w3_size < 5)return

    do k = 0, nlayers - 1
      ik = k + 1 + (cell-1)*nlayers

      do face = 1, nfaces_re_h

        face_next = adjacent_face(face)

        do df = 1,ndf_w3
          exner_e(df)      = exner(stencil_w3_map(df, 1) + k)
          exner_next_e(df) = exner(stencil_w3_map(df, face+1) + k)
        end do

        do qp = 1, nqp_f
          exner_at_fquad      = 0.0_r_def
          exner_next_at_fquad = 0.0_r_def
          do df = 1, ndf_w3
            exner_at_fquad      = exner_at_fquad + exner_e(df)*w3_basis_face(1,df,qp,face)
            exner_next_at_fquad = exner_next_at_fquad + exner_next_e(df)*w3_basis_face(1,df,qp,face_next)
          end do
          exner_av = 0.5_r_def*(exner_at_fquad + exner_next_at_fquad)

          do df0 = 1, ndf_wtheta
            normal =  outward_normals_to_horizontal_faces(:,face)*wtheta_basis_face(1,df0,qp,face)
            do df2 = 1, ndf_w2
              v  = w2_basis_face(:,df2,qp,face)

              integrand = wqp_f(qp,face)*exner_av*dot_product(v, normal)
              projection(df2,df0,ik) = projection(df2,df0,ik) - scalar*integrand
            end do ! df2
          end do ! df0

        end do ! qp

      end do ! faces

    end do ! layers

  end subroutine weighted_proj_2theta_bd_code

end module weighted_proj_2theta_bd_kernel_mod
