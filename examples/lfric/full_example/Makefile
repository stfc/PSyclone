F90 ?= gfortran
F90FLAGS ?= -Wall -g $$(nf-config --fflags)

OBJ = main.o global_mesh_mod.o mesh_mod.o linked_list_data_mod.o \
		extrusion_mod.o mesh_colouring_mod.o mesh_constructor_helper_functions_mod.o \
		domain_size_config_mod.o coord_transform_mod.o matrix_invert_mod.o \
		cross_product_mod.o partition_mod.o mpi_mod.o linked_list_int_mod.o \
		log_mod.o linked_list_mod.o reference_element_mod.o function_space_mod.o \
		master_dofmap_mod.o stencil_dofmap_mod.o fs_continuity_mod.o field_mod.o \
		function_space_constructor_helper_functions_mod.o polynomial_mod.o \
		field_parent_mod.o pure_abstract_field_mod.o scalar_mod.o count_mod.o

EXEC = example

default: $(EXEC)

$(EXEC): $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) -o $(EXEC)


# Dependencies
main.o:	global_mesh_mod.o mesh_mod.o function_space_mod.o field_mod.o
global_mesh_mod.o:	linked_list_data_mod.o log_mod.o reference_element_mod.o
linked_list_data_mod.o: constants_mod.o
mesh_mod.o: extrusion_mod.o linked_list_mod.o mesh_colouring_mod.o \
	mesh_constructor_helper_functions_mod.o partition_mod.o
mesh_constructor_helper_functions_mod.o: coord_transform_mod.o \
	domain_size_config_mod.o 
domain_size_config_mod.o: mpi_mod.o
log_mod.o:	constants_mod.o
field_mod.o: field_parent_mod.o scalar_mod.o count_mod.o
field_parent_mod.o: pure_abstract_field_mod.o
extrusion_mod.o:	global_mesh_mod.o
coord_transform_mod.o: matrix_invert_mod.o cross_product_mod.o
partition_mod.o:	mpi_mod.o linked_list_int_mod.o
linked_list_mod.o: linked_list_data_mod.o
stencil_dofmap_mod.o: master_dofmap_mod.o
function_space_mod.o: mesh_mod.o master_dofmap_mod.o stencil_dofmap_mod.o \
	fs_continuity_mod.o function_space_constructor_helper_functions_mod.o \
	polynomial_mod.o
function_space_constructor_helper_functions_mod.o: mesh_mod.o

#field_mod.o:	constants_mod.o function_space_mod.o mesh_mod.o


%.o: %.F90
	$(F90) $(F90FLAGS) -c $<

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

clean:
	rm -f *.o *.mod  $(EXEC)
