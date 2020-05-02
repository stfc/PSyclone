program main
    use global_mesh_mod,       only : global_mesh_type
    use mesh_mod, only : mesh_type, PLANE
    use partition_mod, only: partition_type, partitioner_planar, partitioner_interface
    use extrusion_mod, only: uniform_extrusion_type
    use function_space_mod, only: function_space_type
      use fs_continuity_mod,     only: W0, W1, W2, W2V, W2H
    use field_mod, only: field_type, field_proxy_type

    !use field_mod,                 only : field_type
    !use finite_element_config_mod, only : element_order
    !use function_space_mod, only: function_space_type
    type(global_mesh_type), target  :: global_mesh
    type(global_mesh_type), pointer :: global_mesh_ptr
    type(partition_type)                    :: partition
    type(mesh_type), target                 :: mesh
    type(mesh_type), pointer                :: mesh_ptr
    type(uniform_extrusion_type), target    :: extrusion
    type(uniform_extrusion_type), pointer   :: extrusion_ptr
    type(function_space_type), target       :: vector_space
    type(function_space_type), pointer      :: vector_space_ptr
    procedure (partitioner_interface), pointer :: partitioner_ptr
    type(field_type) :: wind
    type(field_proxy_type) :: wind_proxy

    integer :: ndata = 1
    integer :: lfric_fs = W0   ! W0
    integer :: element_order = 1

    global_mesh = global_mesh_type("ignore-filename","ignore-mesh-name")
    global_mesh_ptr => global_mesh

    partitioner_ptr => partitioner_planar
    partition = partition_type(global_mesh_ptr, &
                               partitioner_ptr, &
                               1, 1,  &    ! # procs in x and y direction
                               2,     &    ! max stencil size
                               0,     &    ! local rank
                               1)          ! number of proceses

    ! Use the one from unit testing
    !partition = partition_type()
    ! Use the one from unit testing
    !mesh = mesh_type(PLANE)

    ! Important to use d0, otherwise:
    ! Error: Component ‘atmosphere_bottom’ at (1) is a PRIVATE component of ‘extrusion_type’
    ! Because it tries to use this constructor call to assign individual components
    extrusion = uniform_extrusion_type(0.0d0, 100.0d0, 5)
    extrusion_ptr => extrusion
    mesh = mesh_type(global_mesh, partition, extrusion_ptr)
    print *,"Have mesh", mesh%get_nlayers()
    vector_space = function_space_type( mesh,          &
                                        element_order, &
                                        lfric_fs,      &
                                        ndata_sz)
    vector_space_ptr => vector_space
    mesh_ptr => vector_space_ptr%get_mesh()
    call wind%initialise( vector_space = vector_space_ptr, name="wind" )
    print *,"I have wind"
    wind_proxy = wind%get_proxy()
    print *,size(wind_proxy%data), wind_proxy%data
end program main