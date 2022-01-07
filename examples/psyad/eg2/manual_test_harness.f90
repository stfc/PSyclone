program adj_test
  use tl_hydrostatic_kernel_mod, only : tl_hydrostatic_code
  use tl_hydrostatic_kernel_mod_adj, only : tl_hydrostatic_code_adj
  use constants_mod, only : r_def
  ! Begin eg17
  USE global_mesh_base_mod, ONLY: global_mesh_base_type
  USE mesh_mod, ONLY: mesh_type, PLANE
  USE partition_mod, ONLY: partition_type, partitioner_planar, partitioner_interface
  USE extrusion_mod, ONLY: uniform_extrusion_type
  USE function_space_mod, ONLY: function_space_type
  USE fs_continuity_mod, ONLY: W0, W1, W2, W2V, W2H, W3
  USE field_mod, ONLY: field_type
  USE constants_mod, ONLY: r_def, i_def
  USE log_mod, ONLY: LOG_LEVEL_ALWAYS
  IMPLICIT NONE
  TYPE(global_mesh_base_type), TARGET :: global_mesh
  CLASS(global_mesh_base_type), POINTER :: global_mesh_ptr
  TYPE(partition_type) :: partition
  TYPE(mesh_type), TARGET :: mesh
  TYPE(uniform_extrusion_type), TARGET :: extrusion
  TYPE(uniform_extrusion_type), POINTER :: extrusion_ptr
  TYPE(function_space_type), TARGET :: vector_space
  TYPE(function_space_type), POINTER :: vector_space_ptr
  PROCEDURE(partitioner_interface), POINTER :: partitioner_ptr
  TYPE(field_type) :: field1, field2
  INTEGER(KIND = i_def) :: lfric_fs = W0
  INTEGER(KIND = i_def) :: element_order = 1
  INTEGER(KIND = i_def) :: ndata_sz
! End of eg17
  integer, parameter :: array_extent = 20
  integer, parameter :: undf_w3 = array_extent
  integer, parameter :: ndf_w2 = array_extent
  integer, parameter :: undf_wt = array_extent
  integer, parameter :: nqp_v = array_extent
  integer, parameter :: undf_w2 = array_extent
  integer, parameter :: nqp_h = array_extent
  integer, parameter :: ndf_w3 = array_extent
  integer, parameter :: ndf_wt = array_extent
  real(kind=r_def), parameter :: overall_tolerance = 1500.0_r_def
  real(kind=r_def) :: inner1
  real(kind=r_def) :: inner2
  integer :: nlayers
  integer :: nlayers_input
  real(kind=r_def), dimension(undf_w2) :: r_u
  real(kind=r_def), dimension(undf_w2) :: r_u_input
  real(kind=r_def), dimension(undf_w3) :: exner
  real(kind=r_def), dimension(undf_w3) :: exner_input
  real(kind=r_def), dimension(undf_wt) :: theta
  real(kind=r_def), dimension(undf_wt) :: theta_input
  real(kind=r_def), dimension(undf_wt) :: moist_dyn_gas
  real(kind=r_def), dimension(undf_wt) :: moist_dyn_gas_input
  real(kind=r_def), dimension(undf_wt) :: moist_dyn_tot
  real(kind=r_def), dimension(undf_wt) :: moist_dyn_tot_input
  real(kind=r_def), dimension(undf_wt) :: moist_dyn_fac
  real(kind=r_def), dimension(undf_wt) :: moist_dyn_fac_input
  real(kind=r_def), dimension(undf_w3) :: ls_exner
  real(kind=r_def), dimension(undf_w3) :: ls_exner_input
  real(kind=r_def), dimension(undf_wt) :: ls_theta
  real(kind=r_def), dimension(undf_wt) :: ls_theta_input
  real(kind=r_def), dimension(undf_wt) :: ls_moist_dyn_gas
  real(kind=r_def), dimension(undf_wt) :: ls_moist_dyn_gas_input
  real(kind=r_def), dimension(undf_wt) :: ls_moist_dyn_tot
  real(kind=r_def), dimension(undf_wt) :: ls_moist_dyn_tot_input
  real(kind=r_def), dimension(undf_wt) :: ls_moist_dyn_fac
  real(kind=r_def), dimension(undf_wt) :: ls_moist_dyn_fac_input
  real(kind=r_def) :: cp
  real(kind=r_def) :: cp_input
  integer, dimension(ndf_w2) :: map_w2
  integer, dimension(ndf_w2) :: map_w2_input
  real(kind=r_def), dimension(3,ndf_w2,nqp_h,nqp_v) :: w2_basis
  real(kind=r_def), dimension(3,ndf_w2,nqp_h,nqp_v) :: w2_basis_input
  real(kind=r_def), dimension(1,ndf_w2,nqp_h,nqp_v) :: w2_diff_basis
  real(kind=r_def), dimension(1,ndf_w2,nqp_h,nqp_v) :: w2_diff_basis_input
  integer, dimension(ndf_w3) :: map_w3
  integer, dimension(ndf_w3) :: map_w3_input
  real(kind=r_def), dimension(1,ndf_w3,nqp_h,nqp_v) :: w3_basis
  real(kind=r_def), dimension(1,ndf_w3,nqp_h,nqp_v) :: w3_basis_input
  integer, dimension(ndf_wt) :: map_wt
  integer, dimension(ndf_wt) :: map_wt_input
  real(kind=r_def), dimension(1,ndf_wt,nqp_h,nqp_v) :: wt_basis
  real(kind=r_def), dimension(1,ndf_wt,nqp_h,nqp_v) :: wt_basis_input
  real(kind=r_def), dimension(3,ndf_wt,nqp_h,nqp_v) :: wt_diff_basis
  real(kind=r_def), dimension(3,ndf_wt,nqp_h,nqp_v) :: wt_diff_basis_input
  real(kind=r_def), dimension(nqp_h) :: wqp_h
  real(kind=r_def), dimension(nqp_h) :: wqp_h_input
  real(kind=r_def), dimension(nqp_v) :: wqp_v
  real(kind=r_def), dimension(nqp_v) :: wqp_v_input
  real(kind=r_def) :: MachineTol
  real(kind=r_def) :: relative_diff

  ! eg17 setup
  global_mesh = global_mesh_base_type()
  global_mesh_ptr => global_mesh
  partitioner_ptr => partitioner_planar
  partition = partition_type(global_mesh_ptr, partitioner_ptr, 1, 1, 0, 0, 1)
  extrusion = uniform_extrusion_type(0.0_r_def, 100.0_r_def, 5)
  extrusion_ptr => extrusion
  mesh = mesh_type(global_mesh_ptr, partition, extrusion_ptr)
  WRITE(*, *) "Mesh has", mesh % get_nlayers(), "layers."
  ndata_sz = 1
  vector_space = function_space_type(mesh, element_order, lfric_fs, ndata_sz)
  vector_space_ptr => vector_space
  CALL field1 % initialise(vector_space = vector_space_ptr, name = "field1")
  CALL field2 % initialise(vector_space = vector_space_ptr, name = "field2")
  ! end eg17 setup

  ! Initialise the kernel arguments and keep copies of them
  CALL random_number(nlayers)
  nlayers_input = nlayers
  CALL random_number(r_u)
  r_u_input = r_u
  CALL random_number(exner)
  exner_input = exner
  CALL random_number(theta)
  theta_input = theta
  CALL random_number(moist_dyn_gas)
  moist_dyn_gas_input = moist_dyn_gas
  CALL random_number(moist_dyn_tot)
  moist_dyn_tot_input = moist_dyn_tot
  CALL random_number(moist_dyn_fac)
  moist_dyn_fac_input = moist_dyn_fac
  CALL random_number(ls_exner)
  ls_exner_input = ls_exner
  CALL random_number(ls_theta)
  ls_theta_input = ls_theta
  CALL random_number(ls_moist_dyn_gas)
  ls_moist_dyn_gas_input = ls_moist_dyn_gas
  CALL random_number(ls_moist_dyn_tot)
  ls_moist_dyn_tot_input = ls_moist_dyn_tot
  CALL random_number(ls_moist_dyn_fac)
  ls_moist_dyn_fac_input = ls_moist_dyn_fac
  CALL random_number(cp)
  cp_input = cp
  CALL random_number(map_w2)
  map_w2_input = map_w2
  CALL random_number(w2_basis)
  w2_basis_input = w2_basis
  CALL random_number(w2_diff_basis)
  w2_diff_basis_input = w2_diff_basis
  CALL random_number(map_w3)
  map_w3_input = map_w3
  CALL random_number(w3_basis)
  w3_basis_input = w3_basis
  CALL random_number(map_wt)
  map_wt_input = map_wt
  CALL random_number(wt_basis)
  wt_basis_input = wt_basis
  CALL random_number(wt_diff_basis)
  wt_diff_basis_input = wt_diff_basis
  CALL random_number(wqp_h)
  wqp_h_input = wqp_h
  CALL random_number(wqp_v)
  wqp_v_input = wqp_v
  ! Call the tangent-linear kernel
  call tl_hydrostatic_code(nlayers, r_u, exner, theta, moist_dyn_gas, moist_dyn_tot, moist_dyn_fac, ls_exner, ls_theta, ls_moist_dyn_gas, ls_moist_dyn_tot, ls_moist_dyn_fac, cp, ndf_w2, undf_w2, map_w2, w2_basis, w2_diff_basis, ndf_w3, undf_w3, map_w3, w3_basis, ndf_wt, undf_wt, map_wt, wt_basis, wt_diff_basis, nqp_h, nqp_v, wqp_h, wqp_v)
  ! Compute the inner product of the results of the tangent-linear kernel
  inner1 = 0.0_r_def
  inner1 = inner1 + nlayers * nlayers
  inner1 = inner1 + DOT_PRODUCT(r_u, r_u)
  inner1 = inner1 + DOT_PRODUCT(exner, exner)
  inner1 = inner1 + DOT_PRODUCT(theta, theta)
  inner1 = inner1 + DOT_PRODUCT(moist_dyn_gas, moist_dyn_gas)
  inner1 = inner1 + DOT_PRODUCT(moist_dyn_tot, moist_dyn_tot)
  inner1 = inner1 + DOT_PRODUCT(moist_dyn_fac, moist_dyn_fac)
  inner1 = inner1 + DOT_PRODUCT(ls_exner, ls_exner)
  inner1 = inner1 + DOT_PRODUCT(ls_theta, ls_theta)
  inner1 = inner1 + DOT_PRODUCT(ls_moist_dyn_gas, ls_moist_dyn_gas)
  inner1 = inner1 + DOT_PRODUCT(ls_moist_dyn_tot, ls_moist_dyn_tot)
  inner1 = inner1 + DOT_PRODUCT(ls_moist_dyn_fac, ls_moist_dyn_fac)
  inner1 = inner1 + cp * cp
  inner1 = inner1 + DOT_PRODUCT(map_w2, map_w2)
  inner1 = inner1 + SUM(w2_basis(:,:,:,:) * w2_basis(:,:,:,:))
  inner1 = inner1 + SUM(w2_diff_basis(:,:,:,:) * w2_diff_basis(:,:,:,:))
  inner1 = inner1 + DOT_PRODUCT(map_w3, map_w3)
  inner1 = inner1 + SUM(w3_basis(:,:,:,:) * w3_basis(:,:,:,:))
  inner1 = inner1 + DOT_PRODUCT(map_wt, map_wt)
  inner1 = inner1 + SUM(wt_basis(:,:,:,:) * wt_basis(:,:,:,:))
  inner1 = inner1 + SUM(wt_diff_basis(:,:,:,:) * wt_diff_basis(:,:,:,:))
  inner1 = inner1 + DOT_PRODUCT(wqp_h, wqp_h)
  inner1 = inner1 + DOT_PRODUCT(wqp_v, wqp_v)
  ! Call the adjoint of the kernel
  call tl_hydrostatic_code_adj(nlayers, r_u, exner, theta, moist_dyn_gas, moist_dyn_tot, moist_dyn_fac, ls_exner, ls_theta, ls_moist_dyn_gas, ls_moist_dyn_tot, ls_moist_dyn_fac, cp, ndf_w2, undf_w2, map_w2, w2_basis, w2_diff_basis, ndf_w3, undf_w3, map_w3, w3_basis, ndf_wt, undf_wt, map_wt, wt_basis, wt_diff_basis, nqp_h, nqp_v, wqp_h, wqp_v)
  ! Compute inner product of results of adjoint kernel with the original inputs to the tangent-linear kernel
  inner2 = 0.0_r_def
  inner2 = inner2 + nlayers * nlayers_input
  inner2 = inner2 + DOT_PRODUCT(r_u, r_u_input)
  inner2 = inner2 + DOT_PRODUCT(exner, exner_input)
  inner2 = inner2 + DOT_PRODUCT(theta, theta_input)
  inner2 = inner2 + DOT_PRODUCT(moist_dyn_gas, moist_dyn_gas_input)
  inner2 = inner2 + DOT_PRODUCT(moist_dyn_tot, moist_dyn_tot_input)
  inner2 = inner2 + DOT_PRODUCT(moist_dyn_fac, moist_dyn_fac_input)
  inner2 = inner2 + DOT_PRODUCT(ls_exner, ls_exner_input)
  inner2 = inner2 + DOT_PRODUCT(ls_theta, ls_theta_input)
  inner2 = inner2 + DOT_PRODUCT(ls_moist_dyn_gas, ls_moist_dyn_gas_input)
  inner2 = inner2 + DOT_PRODUCT(ls_moist_dyn_tot, ls_moist_dyn_tot_input)
  inner2 = inner2 + DOT_PRODUCT(ls_moist_dyn_fac, ls_moist_dyn_fac_input)
  inner2 = inner2 + cp * cp_input
  inner2 = inner2 + DOT_PRODUCT(map_w2, map_w2_input)
  inner2 = inner2 + SUM(w2_basis(:,:,:,:) * w2_basis_input(:,:,:,:))
  inner2 = inner2 + SUM(w2_diff_basis(:,:,:,:) * w2_diff_basis_input(:,:,:,:))
  inner2 = inner2 + DOT_PRODUCT(map_w3, map_w3_input)
  inner2 = inner2 + SUM(w3_basis(:,:,:,:) * w3_basis_input(:,:,:,:))
  inner2 = inner2 + DOT_PRODUCT(map_wt, map_wt_input)
  inner2 = inner2 + SUM(wt_basis(:,:,:,:) * wt_basis_input(:,:,:,:))
  inner2 = inner2 + SUM(wt_diff_basis(:,:,:,:) * wt_diff_basis_input(:,:,:,:))
  inner2 = inner2 + DOT_PRODUCT(wqp_h, wqp_h_input)
  inner2 = inner2 + DOT_PRODUCT(wqp_v, wqp_v_input)
  ! Test the inner-product values for equality, allowing for the precision of the active variables
  MachineTol = SPACING(MAX(ABS(inner1), ABS(inner2)))
  relative_diff = ABS(inner1 - inner2) / MachineTol
  if (relative_diff < overall_tolerance) then
    WRITE(*, *) 'Test of adjoint of ''tl_hydrostatic_code'' PASSED: ', inner1, inner2, relative_diff
  else
    WRITE(*, *) 'Test of adjoint of ''tl_hydrostatic_code'' FAILED: ', inner1, inner2, relative_diff
  end if

end program adj_test
