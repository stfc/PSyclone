!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!> @brief Module for computing the Jacobian matrix, its determinant and
!> inverse for a coordinate field. Supports coordinate systems defined
!> per panel for certain meshes such as cubed sphere.
module coordinate_jacobian_mod

  use constants_mod,             only: i_def, r_double, r_single
  use finite_element_config_mod, only: coord_system,            &
                                       coord_system_xyz,        &
                                       coord_system_alphabetaz, &
                                       coord_system_lonlatz
  use coord_transform_mod,       only: PANEL_ROT_MATRIX
  use planet_config_mod,         only: scaled_radius

  implicit none

  private

  public :: coordinate_jacobian
  public :: coordinate_jacobian_inverse
  public :: pointwise_coordinate_jacobian
  public :: pointwise_coordinate_jacobian_inverse

  interface coordinate_jacobian
    module procedure &
         coordinate_jacobian_quadrature_r_single,           &
         coordinate_jacobian_quadrature_r_double,           &
         coordinate_jacobian_evaluator_r_single,            &
         coordinate_jacobian_evaluator_r_double
  end interface coordinate_jacobian

  interface coordinate_jacobian_inverse
    module procedure &
         coordinate_jacobian_inverse_quadrature_r_single,   &
         coordinate_jacobian_inverse_quadrature_r_double,   &
         coordinate_jacobian_inverse_evaluator_r_single,    &
         coordinate_jacobian_inverse_evaluator_r_double
  end interface coordinate_jacobian_inverse

  interface pointwise_coordinate_jacobian
    module procedure &
         pointwise_coordinate_jacobian_r_single,            &
         pointwise_coordinate_jacobian_r_double
  end interface

  interface pointwise_coordinate_jacobian_inverse
    module procedure &
         pointwise_coordinate_jacobian_inverse_r_single,    &
         pointwise_coordinate_jacobian_inverse_r_double
  end interface

  interface jacobian_abr2XYZ
    module procedure &
         jacobian_abr2XYZ_r_single, &
         jacobian_abr2XYZ_r_double
  end interface

  interface jacobian_llr2XYZ
    module procedure &
         jacobian_llr2XYZ_r_single, &
         jacobian_llr2XYZ_r_double
  end interface

contains

  !-----------------------------------------------------------------------------
  ! Contained functions/subroutines
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! c o o r d i n a t e _ j a c o b i a n _ q u a d r a t u r e
  !-----------------------------------------------------------------------------
  !> @brief Subroutine Computes the element Jacobian of the coordinate transform from
  !! reference space \f$ \hat{\chi} \f$ to physical space chi
  !> @details Compute the Jacobian of the coordinate transform from
  !> reference space \f[ \hat{\chi} \f] to physical space \f[ \chi \f]
  !> \f[ J^{i,j} = \frac{\partial \chi_i} / {\partial \hat{\chi_j}} \f]
  !> and the determinant det(J)
  !! @param[in] ndf        Size of the chi arrays
  !! @param[in] ngp_h      Number of quadrature points in horizontal direction
  !! @param[in] ngp_v      Number of quadrature points in vertical direction
  !! @param[in] chi_1      1st component of the coordinate field
  !! @param[in] chi_2      2nd component of the coordinate field
  !! @param[in] chi_3      3rd component of the coordinate field
  !! @param[in] panel_id   An integer identifying the mesh panel
  !! @param[in] basis      Wchi basis functions
  !! @param[in] diff_basis Grad of Wchi basis functions
  !! @param[out] jac       Jacobian on quadrature points
  !! @param[out] dj        Determinant of the Jacobian on quadrature points
  subroutine coordinate_jacobian_quadrature_r_single( &
                                           ndf, ngp_h, ngp_v,   &
                                           chi_1, chi_2, chi_3, &
                                           panel_id, basis,     &
                                           diff_basis, jac, dj  )
  !-----------------------------------------------------------------------------
  ! Compute the Jacobian J^{i,j} = d chi_i / d \hat{chi_j} and the
  ! determinant det(J)
  !-----------------------------------------------------------------------------
    implicit none

    integer(kind=i_def),  intent(in) :: ndf, ngp_h, ngp_v
    integer(kind=i_def),  intent(in) :: panel_id

    real(kind=r_single),  intent(in) :: chi_1(ndf), chi_2(ndf), chi_3(ndf)
    real(kind=r_single), intent(out) :: jac(3,3,ngp_h,ngp_v)
    real(kind=r_single), intent(out) :: dj(ngp_h,ngp_v)
    real(kind=r_single),  intent(in) :: basis(1,ndf,ngp_h,ngp_v)
    real(kind=r_single),  intent(in) :: diff_basis(3,ndf,ngp_h,ngp_v)

    ! Local variables
    real(kind=r_single) :: jac_ref2sph(3,3,ngp_h,ngp_v)
    real(kind=r_single) :: jac_sph2XYZ(3,3)
    real(kind=r_single) :: alpha, beta
    real(kind=r_single) :: longitude, latitude
    real(kind=r_single) :: radius

    integer(kind=i_def) :: df, dir
    integer(kind=i_def) :: i, j

    jac_ref2sph(:,:,:,:) = 0.0_r_single
    do j = 1,ngp_v
      do i = 1,ngp_h
        do df = 1,ndf
          do dir = 1,3
            jac_ref2sph(1,dir,i,j) = jac_ref2sph(1,dir,i,j) + &
                                     chi_1(df)*diff_basis(dir,df,i,j)
            jac_ref2sph(2,dir,i,j) = jac_ref2sph(2,dir,i,j) + &
                                     chi_2(df)*diff_basis(dir,df,i,j)
            jac_ref2sph(3,dir,i,j) = jac_ref2sph(3,dir,i,j) + &
                                     chi_3(df)*diff_basis(dir,df,i,j)
          end do
        end do
      end do
    end do

    if (coord_system == coord_system_xyz) then
      jac = jac_ref2sph

    else if (coord_system == coord_system_alphabetaz) then
      do j = 1,ngp_v
        do i = 1,ngp_h
          alpha  = 0.0_r_single
          beta   = 0.0_r_single
          radius = real(scaled_radius, kind=r_single)
          do df = 1,ndf
            alpha  = alpha  + chi_1(df)*basis(1,df,i,j)
            beta   = beta   + chi_2(df)*basis(1,df,i,j)
            radius = radius + chi_3(df)*basis(1,df,i,j)
          end do
          jac_sph2XYZ = jacobian_abr2XYZ(alpha, beta, radius, panel_id)
          jac(:,:,i,j) = matmul(jac_sph2XYZ, jac_ref2sph(:,:,i,j))
        end do
      end do

    else if (coord_system == coord_system_lonlatz) then
      do j = 1,ngp_v
        do i = 1,ngp_h
          longitude = 0.0_r_single
          latitude  = 0.0_r_single
          radius    = real(scaled_radius, kind=r_single)
          do df = 1,ndf
            longitude = longitude + chi_1(df)*basis(1,df,i,j)
            latitude  = latitude  + chi_2(df)*basis(1,df,i,j)
            radius    = radius    + chi_3(df)*basis(1,df,i,j)
          end do
          jac_sph2XYZ = jacobian_llr2XYZ(longitude, latitude, radius)
          jac(:,:,i,j) = matmul(jac_sph2XYZ, jac_ref2sph(:,:,i,j))
        end do
      end do
    end if

    do j = 1,ngp_v
      do i = 1,ngp_h
        dj(i,j) = jac(1,1,i,j)*(jac(2,2,i,j)*jac(3,3,i,j)        &
                              - jac(2,3,i,j)*jac(3,2,i,j))       &
                - jac(1,2,i,j)*(jac(2,1,i,j)*jac(3,3,i,j)        &
                              - jac(2,3,i,j)*jac(3,1,i,j))       &
                + jac(1,3,i,j)*(jac(2,1,i,j)*jac(3,2,i,j)        &
                              - jac(2,2,i,j)*jac(3,1,i,j))
      end do
    end do

  end subroutine coordinate_jacobian_quadrature_r_single

  subroutine coordinate_jacobian_quadrature_r_double( &
                                           ndf, ngp_h, ngp_v,   &
                                           chi_1, chi_2, chi_3, &
                                           panel_id, basis,     &
                                           diff_basis, jac, dj  )
  !-----------------------------------------------------------------------------
  ! Compute the Jacobian J^{i,j} = d chi_i / d \hat{chi_j} and the
  ! determinant det(J)
  !-----------------------------------------------------------------------------
    implicit none

    integer(kind=i_def),  intent(in) :: ndf, ngp_h, ngp_v
    integer(kind=i_def),  intent(in) :: panel_id

    real(kind=r_double),  intent(in) :: chi_1(ndf), chi_2(ndf), chi_3(ndf)
    real(kind=r_double), intent(out) :: jac(3,3,ngp_h,ngp_v)
    real(kind=r_double), intent(out) :: dj(ngp_h,ngp_v)
    real(kind=r_double),  intent(in) :: basis(1,ndf,ngp_h,ngp_v)
    real(kind=r_double),  intent(in) :: diff_basis(3,ndf,ngp_h,ngp_v)

    ! Local variables
    real(kind=r_double) :: jac_ref2sph(3,3,ngp_h,ngp_v)
    real(kind=r_double) :: jac_sph2XYZ(3,3)
    real(kind=r_double) :: alpha, beta
    real(kind=r_double) :: longitude, latitude
    real(kind=r_double) :: radius

    integer(kind=i_def) :: df, dir
    integer(kind=i_def) :: i, j

    jac_ref2sph(:,:,:,:) = 0.0_r_double
    do j = 1,ngp_v
      do i = 1,ngp_h
        do df = 1,ndf
          do dir = 1,3
            jac_ref2sph(1,dir,i,j) = jac_ref2sph(1,dir,i,j) + &
                                     chi_1(df)*diff_basis(dir,df,i,j)
            jac_ref2sph(2,dir,i,j) = jac_ref2sph(2,dir,i,j) + &
                                     chi_2(df)*diff_basis(dir,df,i,j)
            jac_ref2sph(3,dir,i,j) = jac_ref2sph(3,dir,i,j) + &
                                     chi_3(df)*diff_basis(dir,df,i,j)
          end do
        end do
      end do
    end do

    if (coord_system == coord_system_xyz) then
      jac = jac_ref2sph

    else if (coord_system == coord_system_alphabetaz) then
      do j = 1,ngp_v
        do i = 1,ngp_h
          alpha  = 0.0_r_double
          beta   = 0.0_r_double
          radius = real(scaled_radius, kind=r_double)
          do df = 1,ndf
            alpha  = alpha  + chi_1(df)*basis(1,df,i,j)
            beta   = beta   + chi_2(df)*basis(1,df,i,j)
            radius = radius + chi_3(df)*basis(1,df,i,j)
          end do
          jac_sph2XYZ = jacobian_abr2XYZ(alpha, beta, radius, panel_id)
          jac(:,:,i,j) = matmul(jac_sph2XYZ, jac_ref2sph(:,:,i,j))
        end do
      end do

    else if (coord_system == coord_system_lonlatz) then
      do j = 1,ngp_v
        do i = 1,ngp_h
          longitude = 0.0_r_double
          latitude  = 0.0_r_double
          radius    = real(scaled_radius, kind=r_double)
          do df = 1,ndf
            longitude = longitude + chi_1(df)*basis(1,df,i,j)
            latitude  = latitude  + chi_2(df)*basis(1,df,i,j)
            radius    = radius    + chi_3(df)*basis(1,df,i,j)
          end do
          jac_sph2XYZ = jacobian_llr2XYZ(longitude, latitude, radius)
          jac(:,:,i,j) = matmul(jac_sph2XYZ, jac_ref2sph(:,:,i,j))
        end do
      end do

    end if

    do j = 1,ngp_v
      do i = 1,ngp_h
        dj(i,j) = jac(1,1,i,j)*(jac(2,2,i,j)*jac(3,3,i,j)        &
                              - jac(2,3,i,j)*jac(3,2,i,j))       &
                - jac(1,2,i,j)*(jac(2,1,i,j)*jac(3,3,i,j)        &
                              - jac(2,3,i,j)*jac(3,1,i,j))       &
                + jac(1,3,i,j)*(jac(2,1,i,j)*jac(3,2,i,j)        &
                              - jac(2,2,i,j)*jac(3,1,i,j))
      end do
    end do

  end subroutine coordinate_jacobian_quadrature_r_double

  !-----------------------------------------------------------------------------
  ! c o o r d i n a t e _ j a c o b i a n _ e v a l u a t o r
  !-----------------------------------------------------------------------------
  !> @brief Subroutine Computes the element Jacobian of the coordinate transform from
  !! reference space \f$ \hat{\chi} \f$ to physical space chi
  !> @details Compute the Jacobian of the coordinate transform from
  !> reference space \f[ \hat{\chi} \f] to physical space \f[ \chi \f]
  !> \f[ J^{i,j} = \frac{\partial \chi_i} / {\partial \hat{\chi_j}} \f]
  !> and the determinant det(J)
  !! @param[in] ndf          Size of the chi arrays
  !! @param[in] neval_points Number of points basis functions are evaluated on
  !! @param[in] chi_1        1st component of the coordinate field
  !! @param[in] chi_2        2nd component of the coordinate field
  !! @param[in] chi_3        3rd component of the coordinate field
  !! @param[in] panel_id     An integer identifying the mesh panel
  !! @param[in] basis        Wchi basis functions
  !! @param[in] diff_basis   Grad of Wchi basis functions
  !! @param[out] jac         Jacobian on quadrature points
  !! @param[out] dj          Determinant of the Jacobian on quadrature points
  subroutine coordinate_jacobian_evaluator_r_single( &
                                           ndf, neval_points,   &
                                           chi_1, chi_2, chi_3, &
                                           panel_id, basis,     &
                                           diff_basis, jac, dj  )
  !-----------------------------------------------------------------------------
  ! Compute the Jacobian J^{i,j} = d chi_i / d \hat{chi_j} and the
  ! determinant det(J)
  !-----------------------------------------------------------------------------
    implicit none

    integer(kind=i_def),  intent(in) :: ndf, neval_points
    integer(kind=i_def),  intent(in) :: panel_id

    real(kind=r_single),  intent(in) :: chi_1(ndf), chi_2(ndf), chi_3(ndf)
    real(kind=r_single), intent(out) :: jac(3,3,neval_points)
    real(kind=r_single), intent(out) :: dj(neval_points)
    real(kind=r_single),  intent(in) :: basis(1,ndf,neval_points)
    real(kind=r_single),  intent(in) :: diff_basis(3,ndf,neval_points)

    ! Local variables
    real(kind=r_single) :: jac_ref2sph(3,3,neval_points)
    real(kind=r_single) :: jac_sph2XYZ(3,3)
    real(kind=r_single) :: alpha, beta
    real(kind=r_single) :: longitude, latitude
    real(kind=r_single) :: radius

    integer(kind=i_def) :: df, dir
    integer(kind=i_def) :: i

    jac_ref2sph(:,:,:) = 0.0_r_single
    do i = 1,neval_points
      do df = 1,ndf
        do dir = 1,3
          jac_ref2sph(1,dir,i) = jac_ref2sph(1,dir,i) + chi_1(df)*diff_basis(dir,df,i)
          jac_ref2sph(2,dir,i) = jac_ref2sph(2,dir,i) + chi_2(df)*diff_basis(dir,df,i)
          jac_ref2sph(3,dir,i) = jac_ref2sph(3,dir,i) + chi_3(df)*diff_basis(dir,df,i)
        end do
      end do
    end do

    if (coord_system == coord_system_xyz) then
      jac = jac_ref2sph

    else if (coord_system == coord_system_alphabetaz) then
      do i = 1,neval_points
        alpha  = 0.0_r_single
        beta   = 0.0_r_single
        radius = real(scaled_radius, kind=r_single)
        do df = 1,ndf
          alpha  = alpha  + chi_1(df)*basis(1,df,i)
          beta   = beta   + chi_2(df)*basis(1,df,i)
          radius = radius + chi_3(df)*basis(1,df,i)
        end do
        jac_sph2XYZ = jacobian_abr2XYZ(alpha, beta, radius, panel_id)
        jac(:,:,i) = matmul(jac_sph2XYZ, jac_ref2sph(:,:,i))
      end do

    else if (coord_system == coord_system_lonlatz) then
      do i = 1,neval_points
        longitude = 0.0_r_single
        latitude  = 0.0_r_single
        radius    = real(scaled_radius, kind=r_single)
        do df = 1,ndf
          longitude = longitude + chi_1(df)*basis(1,df,i)
          latitude  = latitude  + chi_2(df)*basis(1,df,i)
          radius    = radius    + chi_3(df)*basis(1,df,i)
        end do
        jac_sph2XYZ = jacobian_llr2XYZ(longitude, latitude, radius)
        jac(:,:,i) = matmul(jac_sph2XYZ, jac_ref2sph(:,:,i))
      end do

    end if

    do i = 1,neval_points
      dj(i) = jac(1,1,i)*(jac(2,2,i)*jac(3,3,i)        &
                        - jac(2,3,i)*jac(3,2,i))       &
            - jac(1,2,i)*(jac(2,1,i)*jac(3,3,i)        &
                        - jac(2,3,i)*jac(3,1,i))       &
            + jac(1,3,i)*(jac(2,1,i)*jac(3,2,i)        &
            - jac(2,2,i)*jac(3,1,i))
    end do

  end subroutine coordinate_jacobian_evaluator_r_single

  subroutine coordinate_jacobian_evaluator_r_double( &
                                           ndf, neval_points,   &
                                           chi_1, chi_2, chi_3, &
                                           panel_id, basis,     &
                                           diff_basis, jac, dj  )
  !-----------------------------------------------------------------------------
  ! Compute the Jacobian J^{i,j} = d chi_i / d \hat{chi_j} and the
  ! determinant det(J)
  !-----------------------------------------------------------------------------
    implicit none

    integer(kind=i_def),  intent(in) :: ndf, neval_points
    integer(kind=i_def),  intent(in) :: panel_id

    real(kind=r_double),  intent(in) :: chi_1(ndf), chi_2(ndf), chi_3(ndf)
    real(kind=r_double), intent(out) :: jac(3,3,neval_points)
    real(kind=r_double), intent(out) :: dj(neval_points)
    real(kind=r_double),  intent(in) :: basis(1,ndf,neval_points)
    real(kind=r_double),  intent(in) :: diff_basis(3,ndf,neval_points)

    ! Local variables
    real(kind=r_double) :: jac_ref2sph(3,3,neval_points)
    real(kind=r_double) :: jac_sph2XYZ(3,3)
    real(kind=r_double) :: alpha, beta
    real(kind=r_double) :: longitude, latitude
    real(kind=r_double) :: radius

    integer(kind=i_def) :: df, dir
    integer(kind=i_def) :: i

    jac_ref2sph(:,:,:) = 0.0_r_double
    do i = 1,neval_points
      do df = 1,ndf
        do dir = 1,3
          jac_ref2sph(1,dir,i) = jac_ref2sph(1,dir,i) + chi_1(df)*diff_basis(dir,df,i)
          jac_ref2sph(2,dir,i) = jac_ref2sph(2,dir,i) + chi_2(df)*diff_basis(dir,df,i)
          jac_ref2sph(3,dir,i) = jac_ref2sph(3,dir,i) + chi_3(df)*diff_basis(dir,df,i)
        end do
      end do
    end do

    if (coord_system == coord_system_xyz) then
      jac = jac_ref2sph

    else if (coord_system == coord_system_alphabetaz) then
      do i = 1,neval_points
        alpha  = 0.0_r_double
        beta   = 0.0_r_double
        radius = real(scaled_radius, kind=r_double)
        do df = 1,ndf
          alpha  = alpha  + chi_1(df)*basis(1,df,i)
          beta   = beta   + chi_2(df)*basis(1,df,i)
          radius = radius + chi_3(df)*basis(1,df,i)
        end do
        jac_sph2XYZ = jacobian_abr2XYZ(alpha, beta, radius, panel_id)
        jac(:,:,i) = matmul(jac_sph2XYZ, jac_ref2sph(:,:,i))
      end do

    else if (coord_system == coord_system_lonlatz) then
      do i = 1,neval_points
        longitude = 0.0_r_double
        latitude  = 0.0_r_double
        radius    = real(scaled_radius, kind=r_double)
        do df = 1,ndf
          longitude = longitude + chi_1(df)*basis(1,df,i)
          latitude  = latitude  + chi_2(df)*basis(1,df,i)
          radius    = radius    + chi_3(df)*basis(1,df,i)
        end do
        jac_sph2XYZ = jacobian_llr2XYZ(longitude, latitude, radius)
        jac(:,:,i) = matmul(jac_sph2XYZ, jac_ref2sph(:,:,i))
      end do

    end if

    do i = 1,neval_points
      dj(i) = jac(1,1,i)*(jac(2,2,i)*jac(3,3,i)        &
                        - jac(2,3,i)*jac(3,2,i))       &
            - jac(1,2,i)*(jac(2,1,i)*jac(3,3,i)        &
                        - jac(2,3,i)*jac(3,1,i))       &
            + jac(1,3,i)*(jac(2,1,i)*jac(3,2,i)        &
            - jac(2,2,i)*jac(3,1,i))
    end do

  end subroutine coordinate_jacobian_evaluator_r_double

  !-----------------------------------------------------------------------------
  ! c o o r d i n a t e _ j a c o b i a n _ i n v e r s e _ q u a d r a t u r e
  !-----------------------------------------------------------------------------
  !> @brief Subroutine Computes the inverse of the Jacobian of the coordinate transform from
  !! reference space \f$\hat{\chi}\f$ to physical space \f$ \chi \f$
  !> @details Compute the inverse of the Jacobian
  !> \f[ J^{i,j} = \frac{\partial \chi_i} / {\partial \hat{\chi_j}} \f]
  !> and the determinant det(J)
  !! @param[in] ngp_h      Number of quadrature points in horizontal direction
  !! @param[in] ngp_v      Number of quadrature points in vertical direction
  !! @param[in] jac        Jacobian on quadrature points
  !! @param[in] dj         Determinant of the Jacobian
  !! @param[out] jac_inv   Inverse of the Jacobian on quadrature points
  subroutine coordinate_jacobian_inverse_quadrature_r_single( &
                                         ngp_h, ngp_v, jac, dj, jac_inv)

    use matrix_invert_mod, only: matrix_invert_3x3

    implicit none

    integer(kind=i_def), intent(in)  :: ngp_h, ngp_v

    real(kind=r_single), intent(in)  :: jac(3,3,ngp_h,ngp_v)
    real(kind=r_single), intent(in)  :: dj(ngp_h,ngp_v)
    real(kind=r_single), intent(out) :: jac_inv(3,3,ngp_h,ngp_v)

    real(kind=r_single) :: dummy
    integer(kind=i_def) :: i, k

    !> @todo This is here to maintain the API. If it turns out we don't want
    !> this it should be removed.
    dummy = dj(1,1)

    ! Calculates the inverse Jacobian from the analytic inversion formula
    do k = 1,ngp_v
      do i = 1,ngp_h
        jac_inv(:,:,i,k) = matrix_invert_3x3(jac(:,:,i,k))
      end do
    end do

  end subroutine coordinate_jacobian_inverse_quadrature_r_single

  subroutine coordinate_jacobian_inverse_quadrature_r_double( &
                                         ngp_h, ngp_v, jac, dj, jac_inv)

    use matrix_invert_mod, only: matrix_invert_3x3

    implicit none

    integer(kind=i_def), intent(in)  :: ngp_h, ngp_v

    real(kind=r_double), intent(in)  :: jac(3,3,ngp_h,ngp_v)
    real(kind=r_double), intent(in)  :: dj(ngp_h,ngp_v)
    real(kind=r_double), intent(out) :: jac_inv(3,3,ngp_h,ngp_v)

    real(kind=r_double) :: dummy
    integer(kind=i_def) :: i, k

    !> @todo This is here to maintain the API. If it turns out we don't want
    !> this it should be removed.
    dummy = dj(1,1)

    ! Calculates the inverse Jacobian from the analytic inversion formula
    do k = 1,ngp_v
      do i = 1,ngp_h
        jac_inv(:,:,i,k) = matrix_invert_3x3(jac(:,:,i,k))
      end do
    end do

  end subroutine coordinate_jacobian_inverse_quadrature_r_double

  !---------------------------------------------------------------------------
  ! c o o r d i n a t e _ j a c o b i a n _ i n v e r s e _ e v a l u a t o r
  !---------------------------------------------------------------------------
  !> @brief Subroutine Computes the inverse of the Jacobian of the coordinate transform from
  !! reference space \f$\hat{\chi}\f$ to physical space \f$ \chi \f$
  !> @details Compute the inverse of the Jacobian
  !> \f[ J^{i,j} = \frac{\partial \chi_i} / {\partial \hat{\chi_j}} \f]
  !> and the determinant det(J)
  !! @param[in] neval_points Number of points basis functions are evaluated on
  !! @param[in] jac          Jacobian on quadrature points
  !! @param[in] dj           Determinant of the Jacobian
  !! @param[out] jac_inv     Inverse of the Jacobian on evaluator points
  subroutine coordinate_jacobian_inverse_evaluator_r_single( &
                                           neval_points, jac, dj, jac_inv)

    use matrix_invert_mod, only: matrix_invert_3x3

    implicit none

    integer(kind=i_def), intent(in)  :: neval_points

    real(kind=r_single), intent(in)  :: jac(3,3,neval_points)
    real(kind=r_single), intent(in)  :: dj(neval_points)
    real(kind=r_single), intent(out) :: jac_inv(3,3,neval_points)

    real(kind=r_single) :: dummy
    integer(kind=i_def) :: i

    !> @todo This is here to maintain the API. If it turns out we don't want
    !> this it should be removed.
    dummy = dj(1)

    ! Calculates the inverse Jacobian from the analytic inversion formula
    do i = 1, neval_points
      jac_inv(:,:,i) = matrix_invert_3x3(jac(:,:,i))
    end do

  end subroutine coordinate_jacobian_inverse_evaluator_r_single

  subroutine coordinate_jacobian_inverse_evaluator_r_double( &
                                           neval_points, jac, dj, jac_inv)

    use matrix_invert_mod, only: matrix_invert_3x3

    implicit none

    integer(kind=i_def), intent(in)  :: neval_points

    real(kind=r_double), intent(in)  :: jac(3,3,neval_points)
    real(kind=r_double), intent(in)  :: dj(neval_points)
    real(kind=r_double), intent(out) :: jac_inv(3,3,neval_points)

    real(kind=r_double) :: dummy
    integer(kind=i_def) :: i

    !> @todo This is here to maintain the API. If it turns out we don't want
    !> this it should be removed.
    dummy = dj(1)

    ! Calculates the inverse Jacobian from the analytic inversion formula
    do i = 1, neval_points
      jac_inv(:,:,i) = matrix_invert_3x3(jac(:,:,i))
    end do

  end subroutine coordinate_jacobian_inverse_evaluator_r_double

  !----------------------------------------------------------
  ! p o i n t w i s e _ c o o r d i n a t e _ j a c o b i a n
  !----------------------------------------------------------
  !> @brief Subroutine Computes the element Jacobian of the coordinate transform
  !from
  !! reference space \f$ \hat{\chi} \f$ to physical space chi for a single point
  !> @details Compute the Jacobian of the coordinate transform from
  !> reference space \f[ \hat{\chi} \f] to physical space \f[ \chi \f]
  !> \f[ J^{i,j} = \frac{\partial \chi_i} / {\partial \hat{\chi_j}} \f]
  !> and the determinant det(J) for a single point
  !! @param[in] ndf        Size of the chi arrays
  !! @param[in] chi_1      Coordinate field
  !! @param[in] chi_2      Coordinate field
  !! @param[in] chi_3      Coordinate field
  !! @param[in] panel_id   panel_id
  !! @param[in] basis      Wchi basis functions
  !! @param[in] diff_basis Grad of Wchi basis functions
  !! @param[out] jac       Jacobian on quadrature points
  !! @param[out] dj        Determinant of the Jacobian on quadrature points
  subroutine pointwise_coordinate_jacobian_r_single( &
                                       ndf, chi_1, chi_2, chi_3,      &
                                       panel_id, basis, diff_basis,   &
                                       jac, dj      )
    implicit none

    integer(kind=i_def),  intent(in) :: ndf
    integer(kind=i_def),  intent(in) :: panel_id

    real(kind=r_single),  intent(in) :: chi_1(ndf), chi_2(ndf), chi_3(ndf)
    real(kind=r_single),  intent(in) :: basis(1,ndf)
    real(kind=r_single),  intent(in) :: diff_basis(3,ndf)
    real(kind=r_single), intent(out) :: jac(3,3)
    real(kind=r_single), intent(out) :: dj

    ! Local variables
    real(kind=r_single) :: jac_ref2sph(3,3)
    real(kind=r_single) :: jac_sph2XYZ(3,3)
    real(kind=r_single) :: alpha, beta
    real(kind=r_single) :: longitude, latitude
    real(kind=r_single) :: radius

    integer(kind=i_def) :: df, dir

    jac_ref2sph(:,:) = 0.0_r_single
    do df = 1,ndf
      do dir = 1,3
        jac_ref2sph(1,dir) = jac_ref2sph(1,dir) + chi_1(df)*diff_basis(dir,df)
        jac_ref2sph(2,dir) = jac_ref2sph(2,dir) + chi_2(df)*diff_basis(dir,df)
        jac_ref2sph(3,dir) = jac_ref2sph(3,dir) + chi_3(df)*diff_basis(dir,df)
      end do
    end do

    if (coord_system == coord_system_xyz) then
      jac = jac_ref2sph

    else if (coord_system == coord_system_alphabetaz) then
      alpha  = 0.0_r_single
      beta   = 0.0_r_single
      radius = real(scaled_radius, kind=r_single)
      do df = 1,ndf
        alpha  = alpha  + chi_1(df)*basis(1,df)
        beta   = beta   + chi_2(df)*basis(1,df)
        radius = radius + chi_3(df)*basis(1,df)
      end do
      jac_sph2XYZ = jacobian_abr2XYZ(alpha, beta, radius, panel_id)
      jac = matmul(jac_sph2XYZ, jac_ref2sph)

    else if (coord_system == coord_system_lonlatz) then
      longitude = 0.0_r_single
      latitude  = 0.0_r_single
      radius    = real(scaled_radius, kind=r_single)
      do df = 1,ndf
        longitude = longitude + chi_1(df)*basis(1,df)
        latitude  = latitude  + chi_2(df)*basis(1,df)
        radius    = radius    + chi_3(df)*basis(1,df)
      end do
      jac_sph2XYZ = jacobian_llr2XYZ(longitude, latitude, radius)
      jac = matmul(jac_sph2XYZ, jac_ref2sph)

    end if

    dj = jac(1,1)*(jac(2,2)*jac(3,3)        &
                 - jac(2,3)*jac(3,2))       &
       - jac(1,2)*(jac(2,1)*jac(3,3)        &
                 - jac(2,3)*jac(3,1))       &
       + jac(1,3)*(jac(2,1)*jac(3,2)        &
                 - jac(2,2)*jac(3,1))

  end subroutine pointwise_coordinate_jacobian_r_single

  subroutine pointwise_coordinate_jacobian_r_double( &
                                       ndf, chi_1, chi_2, chi_3,      &
                                       panel_id, basis, diff_basis,   &
                                       jac, dj      )
    implicit none

    integer(kind=i_def),  intent(in) :: ndf
    integer(kind=i_def),  intent(in) :: panel_id

    real(kind=r_double),  intent(in) :: chi_1(ndf), chi_2(ndf), chi_3(ndf)
    real(kind=r_double),  intent(in) :: basis(1,ndf)
    real(kind=r_double),  intent(in) :: diff_basis(3,ndf)
    real(kind=r_double), intent(out) :: jac(3,3)
    real(kind=r_double), intent(out) :: dj

    ! Local variables
    real(kind=r_double) :: jac_ref2sph(3,3)
    real(kind=r_double) :: jac_sph2XYZ(3,3)
    real(kind=r_double) :: alpha, beta
    real(kind=r_double) :: longitude, latitude
    real(kind=r_double) :: radius

    integer(kind=i_def) :: df, dir

    jac_ref2sph(:,:) = 0.0_r_double
    do df = 1,ndf
      do dir = 1,3
        jac_ref2sph(1,dir) = jac_ref2sph(1,dir) + chi_1(df)*diff_basis(dir,df)
        jac_ref2sph(2,dir) = jac_ref2sph(2,dir) + chi_2(df)*diff_basis(dir,df)
        jac_ref2sph(3,dir) = jac_ref2sph(3,dir) + chi_3(df)*diff_basis(dir,df)
      end do
    end do

    if (coord_system == coord_system_xyz) then
      jac = jac_ref2sph

    else if (coord_system == coord_system_alphabetaz) then
      alpha  = 0.0_r_double
      beta   = 0.0_r_double
      radius = real(scaled_radius, kind=r_double)
      do df = 1,ndf
        alpha  = alpha  + chi_1(df)*basis(1,df)
        beta   = beta   + chi_2(df)*basis(1,df)
        radius = radius + chi_3(df)*basis(1,df)
      end do
      jac_sph2XYZ = jacobian_abr2XYZ(alpha, beta, radius, panel_id)
      jac = matmul(jac_sph2XYZ, jac_ref2sph)

    else if (coord_system == coord_system_lonlatz) then
      longitude = 0.0_r_double
      latitude  = 0.0_r_double
      radius    = real(scaled_radius, kind=r_double)
      do df = 1,ndf
        longitude = longitude + chi_1(df)*basis(1,df)
        latitude  = latitude  + chi_2(df)*basis(1,df)
        radius    = radius    + chi_3(df)*basis(1,df)
      end do
      jac_sph2XYZ = jacobian_llr2XYZ(longitude, latitude, radius)
      jac = matmul(jac_sph2XYZ, jac_ref2sph)

    end if

    dj = jac(1,1)*(jac(2,2)*jac(3,3)        &
                 - jac(2,3)*jac(3,2))       &
       - jac(1,2)*(jac(2,1)*jac(3,3)        &
                 - jac(2,3)*jac(3,1))       &
       + jac(1,3)*(jac(2,1)*jac(3,2)        &
                 - jac(2,2)*jac(3,1))

  end subroutine pointwise_coordinate_jacobian_r_double

  !--------------------------------------------------------------------------
  ! p o i n t w i s e _ c o o r d i n a t e _ j a c o b i a n _ i n v e r s e
  !--------------------------------------------------------------------------
  !> @brief Subroutine Computes the inverse of the Jacobian of the coordinate
  !! transform from reference space \f$\hat{\chi}\f$ to physical space
  !! \f$ \chi \f$ for a single point
  !> @details Compute the inverse of the Jacobian
  !> \f[ J^{i,j} = \frac{\partial \chi_i} / {\partial \hat{\chi_j}} \f]
  !> and the determinant det(J)
  !! @param[in] jac        Jacobian on quadrature points
  !! @param[in] dj         Determinant of the Jacobian
  !! @return    jac_inv    Inverse of the Jacobian on quadrature points
  function pointwise_coordinate_jacobian_inverse_r_single(jac, dj) &
                                                          result(jac_inv)
    implicit none

    real(kind=r_single)              :: jac_inv(3,3)
    real(kind=r_single), intent(in)  :: jac(3,3)
    real(kind=r_single), intent(in)  :: dj
    real(kind=r_single)              :: idj

    idj = 1.0_r_single/dj

    jac_inv(1,1) =  (jac(2,2)*jac(3,3) - jac(2,3)*jac(3,2))*idj
    jac_inv(1,2) = -(jac(1,2)*jac(3,3) - jac(1,3)*jac(3,2))*idj
    jac_inv(1,3) =  (jac(1,2)*jac(2,3) - jac(1,3)*jac(2,2))*idj
    jac_inv(2,1) = -(jac(2,1)*jac(3,3) - jac(2,3)*jac(3,1))*idj
    jac_inv(2,2) =  (jac(1,1)*jac(3,3) - jac(1,3)*jac(3,1))*idj
    jac_inv(2,3) = -(jac(1,1)*jac(2,3) - jac(1,3)*jac(2,1))*idj
    jac_inv(3,1) =  (jac(2,1)*jac(3,2) - jac(2,2)*jac(3,1))*idj
    jac_inv(3,2) = -(jac(1,1)*jac(3,2) - jac(1,2)*jac(3,1))*idj
    jac_inv(3,3) =  (jac(1,1)*jac(2,2) - jac(1,2)*jac(2,1))*idj

  end function pointwise_coordinate_jacobian_inverse_r_single

  function pointwise_coordinate_jacobian_inverse_r_double(jac, dj) &
                                                          result(jac_inv)
    implicit none

    real(kind=r_double)              :: jac_inv(3,3)
    real(kind=r_double), intent(in)  :: jac(3,3)
    real(kind=r_double), intent(in)  :: dj
    real(kind=r_double)              :: idj

    idj = 1.0_r_double/dj

    jac_inv(1,1) =  (jac(2,2)*jac(3,3) - jac(2,3)*jac(3,2))*idj
    jac_inv(1,2) = -(jac(1,2)*jac(3,3) - jac(1,3)*jac(3,2))*idj
    jac_inv(1,3) =  (jac(1,2)*jac(2,3) - jac(1,3)*jac(2,2))*idj
    jac_inv(2,1) = -(jac(2,1)*jac(3,3) - jac(2,3)*jac(3,1))*idj
    jac_inv(2,2) =  (jac(1,1)*jac(3,3) - jac(1,3)*jac(3,1))*idj
    jac_inv(2,3) = -(jac(1,1)*jac(2,3) - jac(1,3)*jac(2,1))*idj
    jac_inv(3,1) =  (jac(2,1)*jac(3,2) - jac(2,2)*jac(3,1))*idj
    jac_inv(3,2) = -(jac(1,1)*jac(3,2) - jac(1,2)*jac(3,1))*idj
    jac_inv(3,3) =  (jac(1,1)*jac(2,2) - jac(1,2)*jac(2,1))*idj

  end function pointwise_coordinate_jacobian_inverse_r_double

  !----------------------------------------------------------
  ! f u n c t i o n    j a c o b i a n _ a b r 2 X Y Z
  !----------------------------------------------------------
  !> @brief Compute the pointwise Jacobian for transforming from cubed-sphere
  !         (alpha,beta,r) coordinates to the global Cartesian (X,Y,Z)
  !         coordinates.
  !> @param[in] alpha        The alpha coordinate
  !> @param[in] beta         The beta coordinate
  !> @param[in] radius       The radius coordinate
  !> @param[in] panel_id     Integer giving the ID of the mesh panel
  !> @return    jac_abr2XYZ  3x3 matrix for the Jacobian of the transformation
  function jacobian_abr2XYZ_r_single(alpha, beta, radius, panel_id) &
                                                      result(jac_abr2XYZ)
    implicit none

    real(kind=r_single), intent(in) :: alpha
    real(kind=r_single), intent(in) :: beta
    real(kind=r_single), intent(in) :: radius
    integer(kind=i_def), intent(in) :: panel_id

    real(kind=r_single)             :: jac_abr2XYZ(3,3)
    real(kind=r_single)             :: tan_alpha, tan_beta, panel_rho

    tan_alpha = tan(alpha)
    tan_beta = tan(beta)
    panel_rho = sqrt(1.0_r_single + tan_alpha**2 + tan_beta**2)

    ! First column, g_alpha
    jac_abr2XYZ(1,1) = -tan_alpha*(1.0_r_single + tan_alpha**2)
    jac_abr2XYZ(2,1) = (1.0_r_single + tan_beta**2)*(1.0_r_single + tan_alpha**2)
    jac_abr2XYZ(3,1) = -tan_alpha*tan_beta*(1.0_r_single + tan_alpha**2)

    ! Second column, g_beta
    jac_abr2XYZ(1,2) = -tan_beta*(1.0_r_single + tan_beta**2)
    jac_abr2XYZ(2,2) = -tan_alpha*tan_beta*(1.0_r_single + tan_beta**2)
    jac_abr2XYZ(3,2) = (1.0_r_single + tan_alpha**2)*(1.0_r_single + tan_beta**2)

    ! Third column, g_r
    jac_abr2XYZ(1,3) = panel_rho**2/radius
    jac_abr2XYZ(2,3) = tan_alpha*panel_rho**2/radius
    jac_abr2XYZ(3,3) = tan_beta*panel_rho**2/radius

    ! Rescale by common factor
    jac_abr2XYZ(:,:) = (radius/panel_rho**3)*jac_abr2XYZ(:,:)

    ! Rotate to the appropriate panel
    jac_abr2XYZ(:,:) = matmul(PANEL_ROT_MATRIX(:,:,panel_id), jac_abr2XYZ(:,:))

  end function jacobian_abr2XYZ_r_single

  function jacobian_abr2XYZ_r_double(alpha, beta, radius, panel_id) &
                                                      result(jac_abr2XYZ)
    implicit none

    real(kind=r_double), intent(in) :: alpha
    real(kind=r_double), intent(in) :: beta
    real(kind=r_double), intent(in) :: radius
    integer(kind=i_def), intent(in) :: panel_id

    real(kind=r_double)             :: jac_abr2XYZ(3,3)
    real(kind=r_double)             :: tan_alpha, tan_beta, panel_rho

    tan_alpha = tan(alpha)
    tan_beta = tan(beta)
    panel_rho = sqrt(1.0_r_double + tan_alpha**2 + tan_beta**2)

    ! First column, g_alpha
    jac_abr2XYZ(1,1) = -tan_alpha*(1.0_r_double + tan_alpha**2)
    jac_abr2XYZ(2,1) = (1.0_r_double + tan_beta**2)*(1.0_r_double + tan_alpha**2)
    jac_abr2XYZ(3,1) = -tan_alpha*tan_beta*(1.0_r_double + tan_alpha**2)

    ! Second column, g_beta
    jac_abr2XYZ(1,2) = -tan_beta*(1.0_r_double + tan_beta**2)
    jac_abr2XYZ(2,2) = -tan_alpha*tan_beta*(1.0_r_double + tan_beta**2)
    jac_abr2XYZ(3,2) = (1.0_r_double + tan_alpha**2)*(1.0_r_double + tan_beta**2)

    ! Third column, g_r
    jac_abr2XYZ(1,3) = panel_rho**2/radius
    jac_abr2XYZ(2,3) = tan_alpha*panel_rho**2/radius
    jac_abr2XYZ(3,3) = tan_beta*panel_rho**2/radius

    ! Rescale by common factor
    jac_abr2XYZ(:,:) = (radius/panel_rho**3)*jac_abr2XYZ(:,:)

    ! Rotate to the appropriate panel
    jac_abr2XYZ(:,:) = matmul(PANEL_ROT_MATRIX(:,:,panel_id), jac_abr2XYZ(:,:))

  end function jacobian_abr2XYZ_r_double

  !----------------------------------------------------------
  ! f u n c t i o n    j a c o b i a n _ l l r 2 X Y Z
  !----------------------------------------------------------
  !> @brief Compute the pointwise Jacobian for transforming from (lon,lat,r)
  !         coordinates to the global Cartesian (X,Y,Z) coordinates.
  !> @param[in] longitude    The longitude coordinate
  !> @param[in] latitude     The latitude coordinate
  !> @param[in] radius       The radius coordinate
  !> @return    jac_llr2XYZ  3x3 matrix for the Jacobian of the transformation
  function jacobian_llr2XYZ_r_single(longitude, latitude, radius) &
                                                      result(jac_llr2XYZ)
    implicit none

    real(kind=r_single), intent(in) :: longitude
    real(kind=r_single), intent(in) :: latitude
    real(kind=r_single), intent(in) :: radius

    real(kind=r_single)             :: jac_llr2XYZ(3,3)
    real(kind=r_single)             :: sin_lon, sin_lat, cos_lon, cos_lat

    sin_lat = sin(latitude)
    sin_lon = sin(longitude)
    cos_lat = cos(latitude)
    cos_lon = cos(longitude)

    ! First column, g_lon
    jac_llr2XYZ(1,1) = -radius*sin_lon*cos_lat
    jac_llr2XYZ(2,1) = radius*cos_lon*cos_lat
    jac_llr2XYZ(3,1) = 0.0_r_single

    ! Second column, g_lat
    jac_llr2XYZ(1,2) = -radius*cos_lon*sin_lat
    jac_llr2XYZ(2,2) = -radius*sin_lon*sin_lat
    jac_llr2XYZ(3,2) = radius*cos_lat

    ! Third column, g_r
    jac_llr2XYZ(1,3) = cos_lon*cos_lat
    jac_llr2XYZ(2,3) = sin_lon*cos_lat
    jac_llr2XYZ(3,3) = sin_lat

  end function jacobian_llr2XYZ_r_single

  function jacobian_llr2XYZ_r_double(longitude, latitude, radius) &
                                                      result(jac_llr2XYZ)
    implicit none

    real(kind=r_double), intent(in) :: longitude
    real(kind=r_double), intent(in) :: latitude
    real(kind=r_double), intent(in) :: radius

    real(kind=r_double)             :: jac_llr2XYZ(3,3)
    real(kind=r_double)             :: sin_lon, sin_lat, cos_lon, cos_lat

    sin_lat = sin(latitude)
    sin_lon = sin(longitude)
    cos_lat = cos(latitude)
    cos_lon = cos(longitude)

    ! First column, g_lon
    jac_llr2XYZ(1,1) = -radius*sin_lon*cos_lat
    jac_llr2XYZ(2,1) = radius*cos_lon*cos_lat
    jac_llr2XYZ(3,1) = 0.0_r_double

    ! Second column, g_lat
    jac_llr2XYZ(1,2) = -radius*cos_lon*sin_lat
    jac_llr2XYZ(2,2) = -radius*sin_lon*sin_lat
    jac_llr2XYZ(3,2) = radius*cos_lat

    ! Third column, g_r
    jac_llr2XYZ(1,3) = cos_lon*cos_lat
    jac_llr2XYZ(2,3) = sin_lon*cos_lat
    jac_llr2XYZ(3,3) = sin_lat

  end function jacobian_llr2XYZ_r_double

end module coordinate_jacobian_mod
