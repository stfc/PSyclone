module adj_helmholtz_operator_kernel_mod
  use argument_mod, only : arg_type, cell_column, cross, gh_field, gh_operator, gh_read, gh_real, gh_write, stencil
  use constants_mod, only : i_def, r_def
  use fs_continuity_mod, only : w2, w2v, w3, wtheta
  use kernel_mod, only : kernel_type
  implicit none
  type, public, extends(kernel_type) :: adj_helmholtz_operator_kernel_type
  PRIVATE
  TYPE(arg_type) :: meta_args(10) = (/arg_type(GH_FIELD * 9, GH_REAL, GH_READWRITE, W3), arg_type(GH_FIELD, GH_REAL, GH_READ, W2, &
&STENCIL(CROSS)), arg_type(GH_FIELD, GH_REAL, GH_READ, W2), arg_type(GH_OPERATOR, GH_REAL, GH_READ, W2, W3), arg_type(GH_FIELD, &
&GH_REAL, GH_READ, Wtheta), arg_type(GH_OPERATOR, GH_REAL, GH_READ, Wtheta, W2), arg_type(GH_OPERATOR, GH_REAL, GH_READWRITE, W3, W2), &
&arg_type(GH_OPERATOR, GH_REAL, GH_READWRITE, W3, W3), arg_type(GH_OPERATOR, GH_REAL, GH_READWRITE, W3, Wtheta), arg_type(GH_FIELD, GH_REAL, &
&GH_READ, W2)/)
  INTEGER :: operates_on = CELL_COLUMN
  CONTAINS
  PROCEDURE, NOPASS :: adj_helmholtz_operator_code
END TYPE
  private

  public :: adj_helmholtz_operator_code

  contains
  subroutine adj_helmholtz_operator_code(stencil_size, cell_stencil, wsen_map, wsen_map_count, nlayers, helm_c, helm_n, helm_e, &
&helm_s, helm_w, helm_u, helm_uu, helm_d, helm_dd, hb_lumped_inv, smap_size_w2, smap_w2, u_normalisation, ncell_3d_1, div_star, &
&mt_lumped_inv, ncell_3d_2, ptheta2v, ncell_3d_3, compound_div, ncell_3d_4, m3_exner_star, ncell_3d_5, p3theta, w2_mask, ndf_w3, &
&undf_w3, map_w3, ndf_w2, undf_w2, map_w2, ndf_wt, undf_wt, map_wt)
    integer(kind=i_def), parameter :: nfaces_h = 4
    integer(kind=i_def), parameter :: helm_operator_size = 9
    integer(kind=i_def), parameter :: centre = 0
    integer(kind=i_def), parameter :: west = 1
    integer(kind=i_def), parameter :: south = 2
    integer(kind=i_def), parameter :: east = 3
    integer(kind=i_def), parameter :: north = 4
    integer(kind=i_def), parameter :: down = 5
    integer(kind=i_def), parameter :: up = 6
    integer(kind=i_def), parameter :: downdown = 7
    integer(kind=i_def), parameter :: upup = 8
    integer(kind=i_def), parameter :: t_u = 2
    integer(kind=i_def), parameter :: t_d = 1
    integer(kind=i_def), parameter :: ndf_w2v = 2
    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: stencil_size
    integer(kind=i_def), dimension(stencil_size), intent(in) :: cell_stencil
    integer(kind=i_def), dimension(4), intent(in) :: wsen_map
    integer(kind=i_def), intent(in) :: wsen_map_count
    integer(kind=i_def), intent(in) :: ncell_3d_1
    integer(kind=i_def), intent(in) :: ncell_3d_2
    integer(kind=i_def), intent(in) :: ncell_3d_3
    integer(kind=i_def), intent(in) :: ncell_3d_4
    integer(kind=i_def), intent(in) :: ncell_3d_5
    integer(kind=i_def), intent(in) :: undf_w2
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: undf_w3
    integer(kind=i_def), intent(in) :: ndf_w3
    integer(kind=i_def), intent(in) :: undf_wt
    integer(kind=i_def), intent(in) :: ndf_wt
    integer(kind=i_def), intent(in) :: smap_size_w2
    integer(kind=i_def), dimension(ndf_w3), intent(in) :: map_w3
    integer(kind=i_def), dimension(ndf_w2), intent(in) :: map_w2
    integer(kind=i_def), dimension(ndf_wt), intent(in) :: map_wt
    integer(kind=i_def), dimension(ndf_w2,smap_size_w2), intent(in) :: smap_w2
    real(kind=r_def), dimension(undf_w3), intent(inout) :: helm_c
    real(kind=r_def), dimension(undf_w3), intent(inout) :: helm_n
    real(kind=r_def), dimension(undf_w3), intent(inout) :: helm_e
    real(kind=r_def), dimension(undf_w3), intent(inout) :: helm_s
    real(kind=r_def), dimension(undf_w3), intent(inout) :: helm_w
    real(kind=r_def), dimension(undf_w3), intent(inout) :: helm_u
    real(kind=r_def), dimension(undf_w3), intent(inout) :: helm_uu
    real(kind=r_def), dimension(undf_w3), intent(inout) :: helm_d
    real(kind=r_def), dimension(undf_w3), intent(inout) :: helm_dd
    real(kind=r_def), dimension(undf_w2), intent(in) :: hb_lumped_inv
    real(kind=r_def), dimension(undf_w2), intent(in) :: u_normalisation
    real(kind=r_def), dimension(undf_w2), intent(in) :: w2_mask
    real(kind=r_def), dimension(undf_wt), intent(in) :: mt_lumped_inv
    real(kind=r_def), dimension(ndf_w2,ndf_w3,ncell_3d_1), intent(in) :: div_star
    real(kind=r_def), dimension(ndf_w3,ndf_w2,ncell_3d_2), intent(inout) :: compound_div
    real(kind=r_def), dimension(ndf_w3,ndf_wt,ncell_3d_3), intent(inout) :: p3theta
    real(kind=r_def), dimension(ndf_wt,ndf_w2,ncell_3d_4), intent(in) :: ptheta2v
    real(kind=r_def), dimension(ndf_w3,ndf_w3,ncell_3d_5), intent(inout) :: m3_exner_star
    integer(kind=i_def) :: k
    integer(kind=i_def) :: ik
    integer(kind=i_def) :: kk
    integer(kind=i_def) :: df
    integer(kind=i_def) :: e
    integer(kind=i_def) :: stencil_ik
    integer(kind=i_def), dimension(4) :: adjacent_face
    integer(kind=i_def) :: d1
    integer(kind=i_def) :: d2
    integer(kind=i_def) :: dir
    real(kind=r_def), dimension(ndf_w2,ndf_w3,centre:upup) :: a_op
    REAL(KIND = r_def), DIMENSION(ndf_wt, ndf_w2v, - 1 : 1) :: b_op
    real(kind=r_def), dimension(ndf_w3,ndf_w2) :: ec_op
    real(kind=r_def), dimension(ndf_w3,ndf_wt) :: d_op
    real(kind=r_def), dimension(ndf_w3,ndf_w3) :: f_op
    integer :: idx
    integer :: idx_1
    integer :: idx_2
    integer :: idx_3
    integer :: idx_2_1
    integer :: idx_1_1

    f_op = 0.0_r_def
    d_op = 0.0_r_def
    ec_op = 0.0_r_def
    adjacent_face(:) = -1
    do d1 = 1, wsen_map_count, 1
      dir = wsen_map(d1)
      do d2 = 1, nfaces_h, 1
        if (smap_w2(d2,1 + dir) == smap_w2(dir,1)) then
          adjacent_face(dir) = d2
        end if
      enddo
    enddo
    do d1 = 1, nfaces_h, 1
      if (adjacent_face(d1) == (-1)) then
        adjacent_face(d1) = d1
      end if
    enddo
    do k = nlayers - 1, 0, -1
      ik = k + nlayers * cell_stencil(1) - nlayers + 1
      do df = 1, ndf_w2, 1
        e = 1
        stencil_ik = k + nlayers * cell_stencil(e) - nlayers + 1
        a_op(df,:,e - 1) = -u_normalisation(smap_w2(df,e) + k) * w2_mask(smap_w2(df,e) + k) * hb_lumped_inv(smap_w2(df,e) + k) * &
&div_star(df,:,stencil_ik)
        do e = 1, stencil_size, 1
          a_op(df,:,e) = 0.0
        enddo
        do e = 2, smap_size_w2, 1
          dir = wsen_map(e - 1)
          stencil_ik = k + nlayers * cell_stencil(e) - nlayers + 1
          a_op(df,:,dir) = -u_normalisation(smap_w2(df,e) + k) * w2_mask(smap_w2(df,e) + k) * hb_lumped_inv(smap_w2(df,e) + k) * &
&div_star(df,:,stencil_ik)
        enddo
        kk = -2
        if (k > 1) then
          a_op(df,:,downdown) = -u_normalisation(map_w2(df) + k + kk) * w2_mask(map_w2(df) + k + kk) * hb_lumped_inv(map_w2(df) + &
&k + kk) * div_star(df,:,ik + kk)
        else
          a_op(df,:,downdown) = 0.0_r_def
        end if
        kk = -1
        if (k > 0) then
          a_op(df,:,down) = -u_normalisation(map_w2(df) + k + kk) * w2_mask(map_w2(df) + k + kk) * hb_lumped_inv(map_w2(df) + k + &
&kk) * div_star(df,:,ik + kk)
        else
          a_op(df,:,down) = 0.0_r_def
        end if
        kk = 1
        if (k < nlayers - 1) then
          a_op(df,:,up) = -u_normalisation(map_w2(df) + k + kk) * w2_mask(map_w2(df) + k + kk) * hb_lumped_inv(map_w2(df) + k + &
&kk) * div_star(df,:,ik + kk)
        else
          a_op(df,:,up) = 0.0_r_def
        end if
        kk = 2
        if (k < nlayers - 2) then
          a_op(df,:,upup) = -u_normalisation(map_w2(df) + k + kk) * w2_mask(map_w2(df) + k + kk) * hb_lumped_inv(map_w2(df) + k + &
&kk) * div_star(df,:,ik + kk)
        else
          a_op(df,:,upup) = 0.0_r_def
        end if
      enddo
      if (k == 0) then
        a_op(down,:,:north) = 0.0_r_def
      end if
      if (k == 1) then
        a_op(down,:,down) = 0.0_r_def
      end if
      if (k == 2) then
        a_op(down,:,downdown) = 0.0_r_def
      end if
      if (k == nlayers - 3) then
        a_op(up,:,upup) = 0.0_r_def
      end if
      if (k == nlayers - 2) then
        a_op(up,:,up) = 0.0_r_def
      end if
      if (k == nlayers - 1) then
        a_op(up,:,:north) = 0.0_r_def
      end if
      do df = 1, ndf_wt, 1
        if (k > 0) then
          b_op(df,:,-1) = mt_lumped_inv(map_wt(df) + k - 1) * ptheta2v(df,5:6,ik - 1)
        else
          b_op(df,:,-1) = 0.0_r_def
        end if
        b_op(df,:,0) = mt_lumped_inv(map_wt(df) + k) * ptheta2v(df,5:6,ik)
        if (k < nlayers - 1) then
          b_op(df,:,1) = mt_lumped_inv(map_wt(df) + k + 1) * ptheta2v(df,5:6,ik + 1)
        else
          b_op(df,:,1) = 0.0_r_def
        end if
      enddo
      f_op(1,1) = f_op(1,1) + helm_c(map_w3(1) + k)
      d_op(1,t_d) = d_op(1,t_d) + a_op(down,1,centre) * b_op(t_d,t_d,0) * helm_c(map_w3(1) + k)
      d_op(1,t_u) = d_op(1,t_u) + a_op(down,1,centre) * b_op(t_u,t_d,0) * helm_c(map_w3(1) + k)
      d_op(1,t_d) = d_op(1,t_d) + a_op(down,1,centre) * b_op(t_u,t_u,-1) * helm_c(map_w3(1) + k)
      d_op(1,t_u) = d_op(1,t_u) + a_op(up,1,centre) * b_op(t_d,t_d,1) * helm_c(map_w3(1) + k)
      d_op(1,t_d) = d_op(1,t_d) + a_op(up,1,centre) * b_op(t_d,t_u,0) * helm_c(map_w3(1) + k)
      d_op(1,t_u) = d_op(1,t_u) + a_op(up,1,centre) * b_op(t_u,t_u,0) * helm_c(map_w3(1) + k)
      d_op(1,t_d) = d_op(1,t_d) + a_op(up,1,downdown) * b_op(t_u,t_d,-1) * helm_dd(map_w3(1) + k)
      d_op(1,t_u) = d_op(1,t_u) + a_op(down,1,upup) * b_op(t_d,t_u,1) * helm_uu(map_w3(1) + k)
      d_op(1,t_d) = d_op(1,t_d) + a_op(down,1,down) * b_op(t_u,t_d,-1) * helm_d(map_w3(1) + k)
      d_op(1,t_d) = d_op(1,t_d) + a_op(up,1,down) * b_op(t_d,t_d,0) * helm_d(map_w3(1) + k)
      d_op(1,t_u) = d_op(1,t_u) + a_op(up,1,down) * b_op(t_u,t_d,0) * helm_d(map_w3(1) + k)
      d_op(1,t_d) = d_op(1,t_d) + a_op(up,1,down) * b_op(t_u,t_u,-1) * helm_d(map_w3(1) + k)
      d_op(1,t_u) = d_op(1,t_u) + a_op(down,1,up) * b_op(t_d,t_d,1) * helm_u(map_w3(1) + k)
      d_op(1,t_d) = d_op(1,t_d) + a_op(down,1,up) * b_op(t_d,t_u,0) * helm_u(map_w3(1) + k)
      d_op(1,t_u) = d_op(1,t_u) + a_op(down,1,up) * b_op(t_u,t_u,0) * helm_u(map_w3(1) + k)
      d_op(1,t_u) = d_op(1,t_u) + a_op(up,1,up) * b_op(t_d,t_u,1) * helm_u(map_w3(1) + k)
      ec_op(1,down) = ec_op(1,down) + a_op(down,1,centre) * helm_c(map_w3(1) + k)
      ec_op(1,east) = ec_op(1,east) + a_op(east,1,centre) * helm_c(map_w3(1) + k)
      ec_op(1,north) = ec_op(1,north) + a_op(north,1,centre) * helm_c(map_w3(1) + k)
      ec_op(1,south) = ec_op(1,south) + a_op(south,1,centre) * helm_c(map_w3(1) + k)
      ec_op(1,up) = ec_op(1,up) + a_op(up,1,centre) * helm_c(map_w3(1) + k)
      ec_op(1,west) = ec_op(1,west) + a_op(west,1,centre) * helm_c(map_w3(1) + k)
      helm_c(map_w3(1) + k) = 0.0
      helm_dd(map_w3(1) + k) = 0.0
      helm_uu(map_w3(1) + k) = 0.0
      ec_op(1,down) = ec_op(1,down) + a_op(up,1,down) * helm_d(map_w3(1) + k)
      helm_d(map_w3(1) + k) = 0.0
      ec_op(1,up) = ec_op(1,up) + a_op(down,1,up) * helm_u(map_w3(1) + k)
      helm_u(map_w3(1) + k) = 0.0
      ec_op(1,south) = ec_op(1,south) + a_op(adjacent_face(south),1,south) * helm_s(map_w3(1) + k)
      helm_s(map_w3(1) + k) = 0.0
      ec_op(1,north) = ec_op(1,north) + a_op(adjacent_face(north),1,north) * helm_n(map_w3(1) + k)
      helm_n(map_w3(1) + k) = 0.0
      ec_op(1,west) = ec_op(1,west) + a_op(adjacent_face(west),1,west) * helm_w(map_w3(1) + k)
      helm_w(map_w3(1) + k) = 0.0
      ec_op(1,east) = ec_op(1,east) + a_op(adjacent_face(east),1,east) * helm_e(map_w3(1) + k)
      helm_e(map_w3(1) + k) = 0.0
      do idx_2 = ndf_w3, 1, -1
        do idx_3 = ndf_w3, 1, -1
          m3_exner_star(idx_3,idx_2,ik) = m3_exner_star(idx_3,idx_2,ik) + f_op(idx_3,idx_2)
          f_op(idx_3,idx_2) = 0.0
        enddo
      enddo
      do idx_1 = ndf_wt, 1, -1
        do idx_2 = ndf_w3, 1, -1
          p3theta(idx_2,idx_1,ik) = p3theta(idx_2,idx_1,ik) + (-d_op(idx_2,idx_1))
          d_op(idx_2,idx_1) = 0.0
        enddo
      enddo
      do idx = ndf_w2, 1, -1
        do idx_1 = ndf_w3, 1, -1
          compound_div(idx_1,idx,ik) = compound_div(idx_1,idx,ik) + (-ec_op(idx_1,idx))
          ec_op(idx_1,idx) = 0.0
        enddo
      enddo
    enddo

  end subroutine adj_helmholtz_operator_code

end module adj_helmholtz_operator_kernel_mod
