program lfric_driver
  use mesh_mod, only : mesh_type, plane
  use partition_mod, only : partition_type, partitioner_interface, &
       partitioner_planar
  use global_mesh_base_mod, only : global_mesh_base_type
  use extrusion_mod, only : uniform_extrusion_type
  use field_mod, only : field_type
  use function_space_mod, only : function_space_type
  use fs_continuity_mod, only : w1, w2, w3
  use constants_mod, only : i_def, r_def
  use test_alg_mod, only: test_alg

  integer, parameter :: element_order = 1
  type(partition_type) :: partition
  TYPE(mesh_type), TARGET :: mesh
  TYPE(global_mesh_base_type), TARGET :: global_mesh
  CLASS(global_mesh_base_type), POINTER :: global_mesh_ptr
  TYPE(uniform_extrusion_type), TARGET :: extrusion
  TYPE(uniform_extrusion_type), POINTER :: extrusion_ptr
  PROCEDURE(partitioner_interface), POINTER :: partitioner_ptr
  integer :: ndata_sz
  TYPE(function_space_type), TARGET :: vector_space_w3
  TYPE(function_space_type), POINTER :: vector_space_w3_ptr
  TYPE(function_space_type), TARGET :: vector_space_w2
  TYPE(function_space_type), POINTER :: vector_space_w2_ptr
  TYPE(function_space_type), TARGET :: vector_space_w1
  TYPE(function_space_type), POINTER :: vector_space_w1_ptr
  type(mesh_type), pointer :: mesh_ptr => null()

  global_mesh = global_mesh_base_type()
  global_mesh_ptr => global_mesh
  partitioner_ptr => partitioner_planar
  partition = partition_type(global_mesh_ptr,partitioner_ptr,1,1,0,0,1)
  extrusion = uniform_extrusion_type(0.0_r_def, 100.0_r_def, 20)
  extrusion_ptr => extrusion
  
  mesh = mesh_type(global_mesh_ptr,partition,extrusion_ptr)
  mesh_ptr => mesh

  WRITE(*, *) "Mesh has", mesh % get_nlayers(), "layers."

  call test_alg(mesh_ptr)

end program lfric_driver
