program simple

  use grid_mod
  use field_mod
  use compute_cu_mod,   only: compute_cu
  use compute_cv_mod,   only: compute_cv
  use compute_z_mod,    only: compute_z
  use compute_h_mod,    only: compute_h
  implicit none

  type(grid_type), target :: model_grid

  type(r2d_field) :: p_fld
  type(r2d_field) :: u_fld, v_fld
  !> Mass flux in x and y directions
  type(r2d_field) :: cu_fld, cv_fld
  !> Potential vorticity
  type(r2d_field) :: z_fld
  !> Surface height
  type(r2d_field) :: h_fld

  !> Loop counter for time-stepping loop
  INTEGER :: ncycle

  model_grid = grid_type(ARAKAWA_C,                           &
                         (/BC_PERIODIC,BC_PERIODIC,BC_NONE/), &
                         OFFSET_SW)

  ! Create fields on this grid
  p_fld  = r2d_field(model_grid, T_POINTS)
  u_fld  = r2d_field(model_grid, U_POINTS)
  v_fld  = r2d_field(model_grid, V_POINTS)
  cu_fld = r2d_field(model_grid, U_POINTS)
  cv_fld = r2d_field(model_grid, V_POINTS)
  z_fld = r2d_field(model_grid, F_POINTS)
  h_fld = r2d_field(model_grid, T_POINTS)

  DO ncycle=1,itmax
    
    call invoke( compute_cu(CU_fld, p_fld, u_fld),      &
                 compute_cv(CV_fld, p_fld, v_fld),      &
                 compute_z(z_fld, p_fld, u_fld, v_fld), &
                 compute_h(h_fld, p_fld, u_fld, v_fld) )

  end do

end program simple
