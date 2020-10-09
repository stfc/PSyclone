F90 ?= gfortran
F90FLAGS ?= -Wall -g -fcheck=bound $$(nf-config --fflags) 

OBJ = main_psy.o main_alg.o global_mesh_base_mod.o mesh_mod.o linked_list_data_mod.o \
		extrusion_mod.o mesh_colouring_mod.o mesh_constructor_helper_functions_mod.o \
		domain_size_config_mod.o coord_transform_mod.o matrix_invert_mod.o \
		cross_product_mod.o partition_mod.o mpi_mod.o linked_list_int_mod.o \
		log_mod.o linked_list_mod.o reference_element_mod.o function_space_mod.o \
		master_dofmap_mod.o stencil_dofmap_mod.o fs_continuity_mod.o field_mod.o \
		function_space_constructor_helper_functions_mod.o polynomial_mod.o \
		field_parent_mod.o pure_abstract_field_mod.o scalar_mod.o count_mod.o \
		operator_mod.o testkern_w0_mod.o argument_mod.o kernel_mod.o constants_mod.o \
		global_mesh_netcdf_mod.o ugrid_2d_mod.o ugrid_file_mod.o file_mod.o \
		ugrid_generator_mod.o ncdf_quad_mod.o

EXEC = example

default: $(EXEC)

$(EXEC): $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $$(nf-config --flibs) -o $(EXEC)


# Dependencies
main_psy.o:	global_mesh_base_mod.o mesh_mod.o function_space_mod.o field_mod.o \
	operator_mod.o testkern_w0_mod.o
testkern_w0_mod.o: argument_mod.o kernel_mod.o

global_mesh_base_mod.o:	linked_list_data_mod.o log_mod.o reference_element_mod.o
linked_list_data_mod.o: constants_mod.o
mesh_mod.o: extrusion_mod.o linked_list_mod.o mesh_colouring_mod.o \
	mesh_constructor_helper_functions_mod.o partition_mod.o
mesh_constructor_helper_functions_mod.o: coord_transform_mod.o \
	domain_size_config_mod.o 
domain_size_config_mod.o: mpi_mod.o
log_mod.o:	constants_mod.o
field_mod.o: field_parent_mod.o scalar_mod.o count_mod.o
field_parent_mod.o: pure_abstract_field_mod.o function_space_mod.o
extrusion_mod.o:	global_mesh_base_mod.o
coord_transform_mod.o: matrix_invert_mod.o cross_product_mod.o
partition_mod.o:	mpi_mod.o linked_list_int_mod.o global_mesh_base_mod.o
linked_list_mod.o: linked_list_data_mod.o
stencil_dofmap_mod.o: master_dofmap_mod.o mesh_mod.o
function_space_mod.o: mesh_mod.o master_dofmap_mod.o stencil_dofmap_mod.o \
	fs_continuity_mod.o function_space_constructor_helper_functions_mod.o \
	polynomial_mod.o
function_space_constructor_helper_functions_mod.o: mesh_mod.o
operator_mod.o:	function_space_mod.o
main_alg.o:	main_psy.o
reference_element_mod.o: constants_mod.o
mesh_colouring_mod.o: reference_element_mod.o
matrix_invert_mod.o: constants_mod.o
cross_product_mod.o: constants_mod.o
mpi_mod.o: log_mod.o
linked_list_int_mod.o: linked_list_data_mod.o
fs_continuity_mod.o: log_mod.o
pure_abstract_field_mod.o: linked_list_data_mod.o
scalar_mod.o: mpi_mod.o
polynomial_mod.o: constants_mod.o
count_mod.o: log_mod.o mpi_mod.o
testkern_w0_mod.o: fs_continuity_mod.o
master_dofmap_mod.o: constants_mod.o
global_mesh_netcdf_mod.o: global_mesh_base_mod.o ugrid_2d_mod.o ncdf_quad_mod.o
ugrid_2d_mod.o: ugrid_file_mod.o ugrid_generator_mod.o
ugrid_file_mod.o: file_mod.o
ncdf_quad_mod.o: ugrid_file_mod.o
file_mod.o: linked_list_data_mod.o

%.o: %.F90
	$(F90) $(F90FLAGS) -c $<

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

# Keep the generated psy and alg files
.precious: main_psy.f90 main_alg.f90

main_alg.f90: main_psy.f90

%_psy.f90:	%.x90
	psyclone --config ../../../config/psyclone.cfg -opsy $*_psy.f90 -oalg $*_alg.f90 $<

clean:
	rm -f *.o *.mod  $(EXEC) main_alg.f90 main_psy.f90
