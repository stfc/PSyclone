!-----------------------------------------------------------------------------
! (c) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Compute the coefficients of the Helmholtz operator for lowest-order
!!        elements.
!> @details The Helmholtz operator can be written as a sequence of
!!          coefficients (helm_X) multiplied by the Exner pressure increment
!!          at a location, e.g. for a tridiagonal vertical operator this would
!!          give lhs(k) = helm_u(k) * Exner(k+1) + helm_c(k) * Exner(k) + helm_d(k) * Exner(k-1).
!!          The Helmholtz operator in gungho, at lowest order, comprises of a 9-pt
!!          stencil (penta-diagonal in the vertical + the 4 horizontal
!!          neighbours). This kernel computes the values of the coefficients
!!          from the constituent element matrices.
!!          For more details, see the solver section of
!!          https://code.metoffice.gov.uk/trac/lfric/wiki/GhaspSupport/Documentation
module helmholtz_operator_kernel_mod

  use argument_mod,      only : arg_type,                   &
                                GH_FIELD, GH_OPERATOR,      &
                                GH_REAL, GH_READ, GH_WRITE, &
                                STENCIL, CROSS, CELL_COLUMN
  use constants_mod,     only : r_def, i_def
  use fs_continuity_mod, only : W2, W3, Wtheta, W2v
  use kernel_mod,        only : kernel_type

  implicit none

  private

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------

  type, public, extends(kernel_type) :: helmholtz_operator_kernel_type
    private
    type(arg_type) :: meta_args(10) = (/                               &
         arg_type(GH_FIELD*9,  GH_REAL, GH_WRITE, W3),                 & ! Helmholtz operator
         arg_type(GH_FIELD,    GH_REAL, GH_READ,  W2, STENCIL(CROSS)), & ! hb_lumped_inv
         arg_type(GH_FIELD,    GH_REAL, GH_READ,  W2),                 & ! u_normalisation
         arg_type(GH_OPERATOR, GH_REAL, GH_READ,  W2,     W3),         & ! div_star
         arg_type(GH_FIELD,    GH_REAL, GH_READ,  Wtheta),             & ! Mt_lumped_inv
         arg_type(GH_OPERATOR, GH_REAL, GH_READ,  Wtheta, W2),         & ! ptheta2v
         arg_type(GH_OPERATOR, GH_REAL, GH_READ,  W3,     W2),         & ! compound_div
         arg_type(GH_OPERATOR, GH_REAL, GH_READ,  W3,     W3),         & ! M3_exner
         arg_type(GH_OPERATOR, GH_REAL, GH_READ,  W3,     Wtheta),     & ! p3theta
         arg_type(GH_FIELD,    GH_REAL, GH_READ,  W2)                  & ! W2 mask
         /)
    integer :: operates_on = CELL_COLUMN
  contains
    procedure, nopass :: helmholtz_operator_code
  end type

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------
  public :: helmholtz_operator_code

contains

!> @brief Compute the coefficients of the Helmholtz operator, stored in a
!!        sequence of W3 fields: helm_X where X is the compass direction
!!        or vertical level that component applies to.
!> @param[in]     stencil_size    Number of cells in the horizontal stencil
!> @param[in]     cell_stencil    Stencil of horizontal cell indices
!> @param[in]     wsen_map        Index map for cells to the W,S,E and N
!> @param[in]     wsen_map_count  Number of non-zero entries in WSEN index map
!> @param[in]     cell_stencil    Stencil of horizontal cell indices
!> @param[in]     nlayers         Number of layers
!> @param[in,out] helm_c          Diagonal entry to Helmholtz matrix
!> @param[in,out] helm_n          North (j+1) entry to Helmholtz matrix
!> @param[in,out] helm_e          East (i+1) entry to Helmholtz matrix
!> @param[in,out] helm_s          South (j-1) entry to Helmholtz matrix
!> @param[in,out] helm_w          West (j-1) entry to Helmholtz matrix
!> @param[in,out] helm_u          Upper (k+1) entry to Helmholtz matrix
!> @param[in,out] helm_uu         2nd Upper (k+2) entry to Helmholtz matrix
!> @param[in,out] helm_d          Lower (k-1) entry to Helmholtz matrix
!> @param[in,out] helm_dd         2nd Lower (k-2) entry to Helmholtz matrix
!> @param[in]     hb_lumped_inv   Lumped inverse of the HB (mass matrix + buoyancy)
!!                                term
!> @param[in]     smap_size_w2    Number of cells in W2 stencil dofmap
!> @param[in]     smap_w2         Stencil dofmap for the W2 space
!> @param[in]     u_normalisation Normalisation used for the velocity equation
!> @param[in]     ncell_3d_1      Total number of cells for divergence matrix
!> @param[in]     div_star        Weighted transpose of the divergence operator
!> @param[in]     mt_lumped_inv   Lumped inverse of the Wtheta mass matrix
!> @param[in]     ncell_3d_2      Total number of cells for ptheta2v matrix
!> @param[in]     ptheta2v        Weighted projection operator from the vertical
!!                                components of W2 to Wtheta
!> @param[in]     ncell_3d_3      Total number of cells for compound_div matrix
!> @param[in]     compound_div    Divergence operator weighted by reference density
!!                                and mass matrices
!> @param[in]     ncell_3d_4      Total number of cells for m3_exner_star matrix
!> @param[in]     m3_exner_star   Weighted W3 mass matrix
!> @param[in]     ncell_3d_5      Total number of cells for p3theta matrix
!> @param[in]     p3theta         Weighted projection operator from Wtheta to W3
!> @param[in]     w2_mask         LAM mask for W2 space
!> @param[in]     ndf_w3          Number of degrees of freedom per cell for the pressure space
!> @param[in]     undf_w3         Unique number of degrees of freedom  for the pressure space
!> @param[in]     map_w3          Dofmap for the cell at the base of the column for the pressure space
!> @param[in]     ndf_w2          Number of degrees of freedom per cell for the velocity space
!> @param[in]     undf_w2         Unique number of degrees of freedom  for the velocity space
!> @param[in]     map_w2          Dofmap for the cell at the base of the column for the velocity space
!> @param[in]     ndf_wt          Number of degrees of freedom per cell for the temperature space
!> @param[in]     undf_wt         Unique number of degrees of freedom  for the temperature space
!> @param[in]     map_wt          Dofmap for the cell at the base of the column for the temperature space
subroutine helmholtz_operator_code(stencil_size,                     &
                                   cell_stencil,                     &
                                   wsen_map,                         &
                                   wsen_map_count,                   &
                                   nlayers,                          &
                                   helm_c,                           &
                                   helm_n, helm_e, helm_s, helm_w,   &
                                   helm_u, helm_uu, helm_d, helm_dd, &
                                   hb_lumped_inv,                    &
                                   smap_size_w2, smap_w2,            &
                                   u_normalisation,                  &
                                   ncell_3d_1,                       &
                                   div_star,                         &
                                   mt_lumped_inv,                    &
                                   ncell_3d_2,                       &
                                   ptheta2v,                         &
                                   ncell_3d_3,                       &
                                   compound_div,                     &
                                   ncell_3d_4,                       &
                                   m3_exner_star,                    &
                                   ncell_3d_5,                       &
                                   p3theta,                          &
                                   w2_mask,                          &
                                   ndf_w3, undf_w3, map_w3,          &
                                   ndf_w2, undf_w2, map_w2,          &
                                   ndf_wt, undf_wt, map_wt)

  implicit none

  ! Arguments
  integer(kind=i_def),                                  intent(in) :: nlayers
  integer(kind=i_def),                                  intent(in) :: stencil_size
  integer(kind=i_def), dimension(stencil_size),         intent(in) :: cell_stencil
  integer(kind=i_def), dimension(4),                    intent(in) :: wsen_map
  integer(kind=i_def),                                  intent(in) :: wsen_map_count
  integer(kind=i_def),                                  intent(in) :: ncell_3d_1, ncell_3d_2, &
                                                                      ncell_3d_3, ncell_3d_4, &
                                                                      ncell_3d_5
  integer(kind=i_def),                                  intent(in) :: undf_w2, ndf_w2
  integer(kind=i_def),                                  intent(in) :: undf_w3, ndf_w3
  integer(kind=i_def),                                  intent(in) :: undf_wt, ndf_wt
  integer(kind=i_def),                                  intent(in) :: smap_size_w2
  integer(kind=i_def), dimension(ndf_w3),               intent(in) :: map_w3
  integer(kind=i_def), dimension(ndf_w2),               intent(in) :: map_w2
  integer(kind=i_def), dimension(ndf_wt),               intent(in) :: map_wt
  integer(kind=i_def), dimension(ndf_w2, smap_size_w2), intent(in) :: smap_w2

  ! Fields
  real(kind=r_def), dimension(undf_w3), intent(inout) :: helm_c,                         &
                                                         helm_n, helm_e, helm_s, helm_w, &
                                                         helm_u, helm_uu, helm_d, helm_dd
  real(kind=r_def), dimension(undf_w2), intent(in)    :: hb_lumped_inv,   &
                                                         u_normalisation, &
                                                         w2_mask
  real(kind=r_def), dimension(undf_wt), intent(in)    :: mt_lumped_inv

  ! Operators
  real(kind=r_def), dimension(ndf_w2, ndf_w3, ncell_3d_1), intent(in) :: div_star
  real(kind=r_def), dimension(ndf_w3, ndf_w2, ncell_3d_2), intent(in) :: compound_div
  real(kind=r_def), dimension(ndf_w3, ndf_wt, ncell_3d_3), intent(in) :: p3theta
  real(kind=r_def), dimension(ndf_wt, ndf_w2, ncell_3d_4), intent(in) :: ptheta2v
  real(kind=r_def), dimension(ndf_w3, ndf_w3, ncell_3d_5), intent(in) :: m3_exner_star

  ! Internal variables
  integer(kind=i_def) :: k, ik, kk, df, e, stencil_ik

  ! Integer mappings for neighbours in W2 spaces
  ! ( x, y )
  !      N
  !   |--4--|
  ! W 1     3 E
  !   |--2--|
  !      S
  !
  ! ( x, z )
  !     UU
  !   |--8--|
  !   |     |
  !   |  U  |
  !   |--6--|
  ! W 1     3 E
  !   |--5--|
  !   |  D  |
  !   |     |
  !   |--7--|
  !     DD

  ! Number of horizontal neighbours,
  ! this code is specific for quadrilateral meshes
  ! so we can hardwire this here instead of passing in.
  integer(kind=i_def), parameter :: nfaces_h = 4

  ! Number of fields in the Helmholtz operator,
  ! (this is the number of helm_X fields passed in)
  ! and is hardwired to 9.
  integer(kind=i_def), parameter :: helm_operator_size = 9

  ! The Compass points and up/down need to match the location
  ! of W2 DoF's.
  integer(kind=i_def), parameter :: centre   = 0
  integer(kind=i_def), parameter :: west     = 1
  integer(kind=i_def), parameter :: south    = 2
  integer(kind=i_def), parameter :: east     = 3
  integer(kind=i_def), parameter :: north    = 4
  integer(kind=i_def), parameter :: down     = 5
  integer(kind=i_def), parameter :: up       = 6
  integer(kind=i_def), parameter :: downdown = 7
  integer(kind=i_def), parameter :: upup     = 8

  ! The Compass points needed to match the location
  ! of W2 DoF's in neighbouring cells
  integer(kind=i_def), dimension(4) :: adjacent_face
  integer(kind=i_def)               :: d1, d2, dir

  ! Integer mappings for neighbours in Wtheta spaces
  ! ( x, z )
  !      TU
  !   |--2--|
  !   |     |
  !   |--1--|
  !      TD
  integer(kind=i_def), parameter :: t_u = 2
  integer(kind=i_def), parameter :: t_d = 1
  integer(kind=i_def), parameter :: ndf_w2v = 2

  real(kind=r_def), dimension(ndf_w2, ndf_w3,   centre:upup) :: a_op
  real(kind=r_def), dimension(ndf_wt, ndf_w2v,         -1:1) :: b_op
  real(kind=r_def), dimension(ndf_w3, ndf_w2)                :: ec_op
  real(kind=r_def), dimension(ndf_w3, ndf_wt)                :: d_op
  real(kind=r_def), dimension(ndf_w3, ndf_w3)                :: f_op

  ! The mixed system used by the approximate Schur complement is:
  !   (u                    theta        rho           Exner)
  ! | I                      0           0            -Nu*Hb^-1*div_star |
  ! | Mt^-1*pt2v             I           0             0                 |
  ! | tau*dt*M3^-1*D(rho^*)  0           I             0                 |
  ! | 0                      -P3t        M3_rho        M3_exner          |

  ! This can be more compactly written as
  ! | I 0 0 a |
  ! | b I 0 0 |
  ! | c 0 I 0 |
  ! | 0 d e f |

  ! With:                             : fs mapping
  ! A \equiv -Nu*Hb^-1*div_star       : W3  -> W2
  ! B \equiv  Mt^-1*pt2v              : W2v -> Wt
  ! C \equiv  tau*dt*M3^-1*D(rho^*)   : W2  -> W3
  ! D \equiv  -P3t                    : Wt  -> W3
  ! E \equiv  -M3_rho                 : W3  -> W3
  ! F \equiv   M3_exner               : W3  -> W3
  !
  ! u + A*p = Ru
  ! t + B*u = Rt
  ! d + C*u = Rr
  ! D*t + E*r + F*p = Rp
  ! => -D*B*u + E*r + F*p = Rp - D*Rt
  ! => -(D*B + E*C)*u + F*p = Rp - D*Rt - E*Rr
  ! => [(D*B + E*C)*A + F]*p = Rp - D*Rt - E*Rr + (D*B + E*C)*Ru
  !
  ! Given this the Helmholtz operator can be written as:
  ! [F + (D*B + E*C)*A]*Exner' = RHS
  !
  ! and note that e*c equiv -M3_rho*tau*dt*M3^-1*D(rho^*) =
  ! -compound_div
  !
  ! In the code these operators are:
  ! A = a_op
  ! B = b_op
  ! C = c_op
  ! And so we seek to rewrite this operator as the coefficients to
  ! each Exner value in the stencil

  ! Compute direction faces in neighbouring cells that
  ! adjoin each direction of the central cell, i.e
  !
  !          |-----|
  !          |  N  |
  !          |E   W|
  !          |  S  |
  !    |-----|-----|-----|
  !    |  E  |  N  |  N  |
  !    |S   N|E   W|E   W|
  !    |  W  |  S  |  S  |
  !    |-----|-----|-----|
  !          |  E  |
  !          |N   S|
  !          |  W  |
  !          |-----|
  !
  ! Then adjacent face would be
  ! (/ N, E, E, S /)
  ! Since this kernel is only valid for lowest order we can use
  ! the W2 dofmap (one DoF per face) to compute this.
  adjacent_face(:) = -1
  do d1 = 1, wsen_map_count
    dir = wsen_map(d1)
    do d2 = 1, nfaces_h
      if ( smap_w2(d2,1+dir) == smap_w2(dir,1) ) adjacent_face(dir) = d2
    end do
  end do

  ! if the adjacent face is missing, then we arbitrarily assign this to be
  ! the same as the centre face
  do d1 = 1, nfaces_h
    if (adjacent_face(d1) == -1) adjacent_face(d1) = d1
  end do

  do k = 0, nlayers - 1
    ik = 1 + k + (cell_stencil(1)-1)*nlayers

    ! Compute a for all cells in the stencil
    do df = 1, ndf_w2
      ! a maps from W3 points to W2 points,
      ! first index is the location of the local edge of the cell,
      ! last index is the location of the cell in the stencil.
      ! Horizontal stencil:
      !
      !                |--------------|
      !                |   A(N,1,N)   |
      !                |              |
      !                |   A(S,1,N)   |
      ! |--------------|--------------|--------------|
      ! |A(W,1,W)      |              |A(W,1,E)      |
      ! |              |              |              |
      ! |      A(E,1,W)|              |      A(E,1,E)|
      ! |--------------|--------------|--------------|
      !                |   A(N,1,S)   |
      ! y              |              |
      ! |              |   A(S,1,S)   |
      ! 0--x           |--------------|

      ! First the centre value
      e=1
      stencil_ik = 1 + k + (cell_stencil(e)-1)*nlayers
      a_op(df,:,e-1) = -u_normalisation(smap_w2(df,e)+k) &
                       *w2_mask(smap_w2(df,e)+k)         &
                       *hb_lumped_inv(smap_w2(df,e)+k)   &
                       *div_star(df,:,stencil_ik)

      ! Initialise to zero to allow for missing neighbours
      ! in order to fill in any missing horizontal neighbours
      do e = 1,stencil_size
        a_op(df,:,e) = 0.0
      end do

      ! Next the horizontal neighbours (where they exist)
      do e = 2, smap_size_w2
        dir = wsen_map(e-1)
        stencil_ik = 1 + k + (cell_stencil(e)-1)*nlayers
        a_op(df,:,dir) = -u_normalisation(smap_w2(df,e)+k) &
                         *w2_mask(smap_w2(df,e)+k)         &
                         *hb_lumped_inv(smap_w2(df,e)+k)   &
                         *div_star(df,:,stencil_ik)
      end do

      ! Vertical stencil:
      !
      !     |--------------|
      !     |   A(U,1,UU)  |
      ! k+2 |              |
      !     |   A(D,1,UU)  |
      !     |--------------|
      !     |   A(U,1,U)   |
      ! k+1 |              |
      !     |   A(D,1,U)   |
      !     |--------------|
      !     |   A(U,1,C)   |
      ! k   |              |
      !     |   A(D,1,C)   |
      !     |--------------|
      !     |   A(U,1,D)   |
      ! k-1 |              |
      !     |   A(D,1,D)   |
      !     |--------------|
      !     |   A(U,1,DD)  |
      ! k-2 |              |
      !     |   A(D,1,DD)  |
      !     |--------------|
      kk = -2
      if ( k > 1 ) then
        a_op(df,:,downdown) = -u_normalisation(map_w2(df)+k+kk)*w2_mask(map_w2(df)+k+kk) &
                              *hb_lumped_inv(map_w2(df)+k+kk)*div_star(df,:,ik+kk)
      else
        a_op(df,:,downdown) = 0.0_r_def
      end if
      kk = -1
      if ( k > 0 ) then
        a_op(df,:,down) = -u_normalisation(map_w2(df)+k+kk)*w2_mask(map_w2(df)+k+kk) &
                          *hb_lumped_inv(map_w2(df)+k+kk)*div_star(df,:,ik+kk)
      else
        a_op(df,:,down) = 0.0_r_def
      end if
      kk = 1
      if ( k < nlayers-1 ) then
        a_op(df,:,up) = -u_normalisation(map_w2(df)+k+kk)*w2_mask(map_w2(df)+k+kk) &
                        *hb_lumped_inv(map_w2(df)+k+kk)*div_star(df,:,ik+kk)
      else
        a_op(df,:,up) = 0.0_r_def
      end if
      kk = 2
      if ( k < nlayers-2 ) then
        a_op(df,:,upup) = -u_normalisation(map_w2(df)+k+kk)*w2_mask(map_w2(df)+k+kk) &
                          *hb_lumped_inv(map_w2(df)+k+kk)*div_star(df,:,ik+kk)
      else
        a_op(df,:,upup) = 0.0_r_def
      end if
    end do
    ! Apply boundary conditions to A, these are terms that would multiply DoFs on
    ! the boundaries.
    if ( k == 0 )           a_op(down,:,centre:north) = 0.0_r_def
    if ( k == 1 )           a_op(down,:,down)         = 0.0_r_def
    if ( k == 2 )           a_op(down,:,downdown)     = 0.0_r_def
    if ( k == nlayers - 3 ) a_op(up,:,upup)           = 0.0_r_def
    if ( k == nlayers - 2 ) a_op(up,:,up)             = 0.0_r_def
    if ( k == nlayers - 1 ) a_op(up,:,centre:north)   = 0.0_r_def

    ! Compute B for all cells in the stencil
    ! B maps from W2v -> Wtheta points
    ! and we need it for the k-1,k,k+1 cells
    ! The first index is the Wtheta index (1 = cell bottom, 2 = cell top)
    ! The second index is the W2v index  (1 = cell bottom, 2 = cell top)
    ! The third index is the cell in the stencil (-1 = k-1, 0 = k, +1 = k+1)
      !     |--------------|
      !     |   B(U,:,1)   |
      ! k+1 |              |
      !     |   B(D,:,1)   |
      !     |--------------|
      !     |   B(U,:,0)   |
      ! k   |              |
      !     |   B(D,:,0)   |
      !     |--------------|
      !     |   B(U,:,-1)  |
      ! k-1 |              |
      !     |   B(D,:,-1)  |
      !     |--------------|
    do df = 1,ndf_wt
      if ( k > 0 ) then
        b_op(df,:,-1) = mt_lumped_inv(map_wt(df)+k-1)*ptheta2v(df,5:6,ik-1)
      else
        b_op(df,:,-1) = 0.0_r_def
      end if
      b_op(df,:, 0) = mt_lumped_inv(map_wt(df)+k  )*ptheta2v(df,5:6,ik)
      if ( k < nlayers-1 ) then
        b_op(df,:, 1) = mt_lumped_inv(map_wt(df)+k+1)*ptheta2v(df,5:6,ik+1)
      else
        b_op(df,:, 1) = 0.0_r_def
      end if
    end do
    ! Compute E*C for all cells in the stencil,
    ! EC maps from W2 points to W3 points and we only need it for the central
    ! cell.
    ec_op = - compound_div(:,:,ik)

    ! Compute D for all cells in the stencil,
    ! D maps from Wtheta points to W3 points and we only need it for the
    ! central cell.
    d_op = - p3theta(:,:,ik)

    ! Compute F for all cells in the stencil,
    ! F maps from W3 points to W3 points and we only need it for the central
    ! cell.
    f_op = m3_exner_star(:,:,ik)

    ! Now compute the coefficients:

    ! (E*C)*A
    ! helm_e is the term that maps from Exner in the east cell to the central cell,
    ! therefore it is the combination of A for the east cell that maps to the
    ! common face of the central cell and the east cell (given by adjacent_face(east)):
    ! a(adjacent_face(east),1,east) multiplied the component of EC that maps a
    ! W2 DoF on the east face to the W3 point for this cell: EC(1,east)
    ! Most other terms are computed similarly.
    ! helm_c is all terms that map from the central cell to the W2 points
    ! of this cell and then back again and hence has six entries.
    helm_e(map_w3(1)+k)  = ec_op(1,east) *a_op(adjacent_face(east),1,east)
    helm_w(map_w3(1)+k)  = ec_op(1,west) *a_op(adjacent_face(west),1,west)
    helm_n(map_w3(1)+k)  = ec_op(1,north)*a_op(adjacent_face(north),1,north)
    helm_s(map_w3(1)+k)  = ec_op(1,south)*a_op(adjacent_face(south),1,south)
    helm_u(map_w3(1)+k)  = ec_op(1,up)   *a_op(down,1,up)
    helm_d(map_w3(1)+k)  = ec_op(1,down) *a_op(up,1,down)
    helm_uu(map_w3(1)+k) = 0.0_r_def
    helm_dd(map_w3(1)+k) = 0.0_r_def
    helm_c(map_w3(1)+k)  = ec_op(1,east) *a_op(east,1,centre)  &
                         + ec_op(1,west) *a_op(west,1,centre)  &
                         + ec_op(1,north)*a_op(north,1,centre) &
                         + ec_op(1,south)*a_op(south,1,centre) &
                         + ec_op(1,up)   *a_op(up,1,centre)    &
                         + ec_op(1,down) *a_op(down,1,centre)
    ! (D*B)*A
    ! These terms only couple in the vertical.
    ! helm_u is the mapping from the k+1 cell onto this cell.
    ! It is the product of the A term for the k+1 cell
    ! that maps to both its top and bottom faces: a(down,1,up) &
    ! a(up,1,up) with the B term that maps from both W2 points in the k+1 cell
    ! to the Wtheta point on the top face [b(t_d,:,1)] along with the B term for
    ! the central cell that maps the W2 point on the top face of this cell
    ! [b(:,t_u,,0)] to both Wtheta points in the central cell.
    ! Once B*A is computed on the two Wtheta points of this cell
    ! they are then multiplied by the appropriate D term.
    ! Other entries are then computed similarly.
    helm_u(map_w3(1)+k)  = helm_u(map_w3(1)+k) + d_op(1,t_u) &
                          *(b_op(t_d,t_u,1)*a_op(up,1,up)    &
                          + b_op(t_d,t_d,1)*a_op(down,1,up)  &
                          + b_op(t_u,t_u,0)*a_op(down,1,up)) &
                         + d_op(1,t_d)*b_op(t_d,t_u,0)*a_op(down,1,up)
    helm_d(map_w3(1)+k)  = helm_d(map_w3(1)+k) + d_op(1,t_d)   &
                          *(b_op(t_u,t_d,-1)*a_op(down,1,down) &
                          + b_op(t_u,t_u,-1)*a_op(up,1,down)   &
                          + b_op(t_d,t_d,0) *a_op(up,1,down))  &
                         + d_op(1,t_u)*b_op(t_u,t_d,0)*a_op(up,1,down)
    helm_uu(map_w3(1)+k) = helm_uu(map_w3(1)+k) + d_op(1,t_u)*b_op(t_d,t_u,1) *a_op(down,1,upup)
    helm_dd(map_w3(1)+k) = helm_dd(map_w3(1)+k) + d_op(1,t_d)*b_op(t_u,t_d,-1)*a_op(up,1,downdown)
    helm_c(map_w3(1)+k)  = helm_c(map_w3(1)+k) &
                         + d_op(1,t_u)*(b_op(t_d,t_d,1)*a_op(up,1,centre)    &
                                      + b_op(t_u,t_u,0)*a_op(up,1,centre)    &
                                      + b_op(t_u,t_d,0)*a_op(down,1,centre)) &
                         + d_op(1,t_d)*(b_op(t_u,t_u,-1)*a_op(down,1,centre) &
                                      + b_op(t_d,t_u,0) *a_op(up,1,centre)   &
                                      + b_op(t_d,t_d,0) *a_op(down,1,centre))
    ! F
    ! F maps from W3 to W3 and simply maps from the current cell to itself
    helm_c(map_w3(1)+k) = helm_c(map_w3(1)+k) + f_op(1,1)

  end do

end subroutine helmholtz_operator_code

end module helmholtz_operator_kernel_mod
