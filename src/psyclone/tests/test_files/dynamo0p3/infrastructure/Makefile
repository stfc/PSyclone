# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2022, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# ------------------------------------------------------------------------------
# Author: J. Henrichs, Bureau of Meteorology
# Modified: I. Kavcic, Met Office
# Modified: R. W. Ford, STFC Daresbury Lab

F90 ?= gfortran
F90FLAGS ?= -Wall -g -fcheck=bound

OBJ = global_mesh_base_mod.o mesh_mod.o linked_list_data_mod.o \
		extrusion_mod.o mesh_colouring_mod.o mesh_constructor_helper_functions_mod.o \
		domain_size_config_mod.o coord_transform_mod.o matrix_invert_mod.o \
                driver_water_constants_mod.o \
		cross_product_mod.o partition_mod.o mpi_mod.o linked_list_int_mod.o \
		log_mod.o linked_list_mod.o reference_element_mod.o function_space_mod.o \
		master_dofmap_mod.o stencil_dofmap_mod.o fs_continuity_mod.o \
		field_mod.o integer_field_mod.o r_solver_field_mod.o r_tran_field_mod.o \
		function_space_constructor_helper_functions_mod.o polynomial_mod.o \
		field_parent_mod.o pure_abstract_field_mod.o scalar_mod.o count_mod.o \
		operator_mod.o columnwise_operator_mod.o operator_parent_mod.o \
		argument_mod.o kernel_mod.o constants_mod.o \
		mesh_map_mod.o stencil_dofmap_helper_functions_mod.o stencil_2D_dofmap_mod.o \
		timer_mod.o io_utility_mod.o

# Additional object files required for netcdf support
OBJ_NETCDF = global_mesh_netcdf_mod.o ugrid_2d_mod.o ugrid_file_mod.o file_mod.o \
		ugrid_generator_mod.o ncdf_quad_mod.o

# Note that the files
# quadrature_edge_mod.F90 quadrature_face_mod.F90 quadrature_mod.F90
# quadrature_rule_mod.F90 quadrature_xoyoz_mod.F90 quadrature_xyoz_mod.F90
# quadrature_xyz_mod.F90
# are required for some compilation tests only, they are not required for
# a stand-alone LFRic example, and as such are not compiled when creating
# an infrastructure library.

.PHONY: standalone netcdf default

default: standalone

# Standalone target, no netcdf dependency:
# ----------------------------------------
standalone: liblfric.a
liblfric.a: $(OBJ)
	$(AR) $(ARFLAGS) $@ $(OBJ)


# Netcdf target, like standalone but with support for netcdf files:
# -----------------------------------------------------------------
netcdf: F90FLAGS += $$(nf-config --fflags)
netcdf: liblfric_netcdf.a
liblfric_netcdf.a: $(OBJ) $(OBJ_NETCDF)
	$(AR) $(ARFLAGS) $@ $(OBJ) $(OBJ_NETCDF)


# Dependencies
# ------------
global_mesh_base_mod.o:	linked_list_data_mod.o log_mod.o reference_element_mod.o
global_mesh_netcdf_mod.o: ugrid_2d_mod.o ncdf_quad_mod.o global_mesh_base_mod.o
ugrid_2d_mod.o: ugrid_file_mod.o ugrid_generator_mod.o
ugrid_generator_mod.o: constants_mod.o
ugrid_file_mod.o: file_mod.o
file_mod.o:	constants_mod.o linked_list_data_mod.o
ncdf_quad_mod.o: log_mod.o ugrid_file_mod.o
linked_list_data_mod.o: constants_mod.o
mesh_map_mod.o: linked_list_data_mod.o
mesh_mod.o: mesh_map_mod.o extrusion_mod.o linked_list_mod.o mesh_colouring_mod.o \
	mesh_constructor_helper_functions_mod.o partition_mod.o
mesh_constructor_helper_functions_mod.o: coord_transform_mod.o \
	domain_size_config_mod.o 
domain_size_config_mod.o: mpi_mod.o
log_mod.o:	constants_mod.o
field_mod.o: field_parent_mod.o scalar_mod.o count_mod.o
integer_field_mod.o: field_parent_mod.o mpi_mod.o count_mod.o
r_solver_field_mod.o: constants_mod.o fs_continuity_mod.o function_space_mod.o mesh_mod.o field_parent_mod.o pure_abstract_field_mod.o log_mod.o mpi_mod.o count_mod.o
r_tran_field_mod.o: constants_mod.o fs_continuity_mod.o function_space_mod.o mesh_mod.o field_parent_mod.o pure_abstract_field_mod.o log_mod.o mpi_mod.o count_mod.o
field_parent_mod.o: pure_abstract_field_mod.o function_space_mod.o
extrusion_mod.o:	global_mesh_base_mod.o
coord_transform_mod.o: matrix_invert_mod.o cross_product_mod.o
partition_mod.o:	mpi_mod.o linked_list_int_mod.o global_mesh_base_mod.o
linked_list_mod.o: linked_list_data_mod.o
stencil_dofmap_mod.o: master_dofmap_mod.o mesh_mod.o stencil_dofmap_helper_functions_mod.o
stencil_dofmap_helper_functions_mod.o: constants_mod.o mesh_mod.o
stencil_2D_dofmap_mod.o: master_dofmap_mod.o mesh_mod.o \
	stencil_dofmap_helper_functions_mod.o
function_space_mod.o: mesh_mod.o master_dofmap_mod.o stencil_dofmap_mod.o \
	fs_continuity_mod.o function_space_constructor_helper_functions_mod.o \
	polynomial_mod.o stencil_2D_dofmap_mod.o
function_space_constructor_helper_functions_mod.o: mesh_mod.o
operator_mod.o:	operator_parent_mod.o function_space_mod.o
columnwise_operator_mod.o:	operator_parent_mod.o function_space_mod.o
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
master_dofmap_mod.o: constants_mod.o
io_utility_mod.o: constants_mod.o log_mod.o
timer_mod.o: constants_mod.o io_utility_mod.o log_mod.o mpi_mod.o scalar_mod.o
ugrid_2d_mod.o: ugrid_file_mod.o ugrid_generator_mod.o
ugrid_file_mod.o: file_mod.o
ugrid_generator_mod.o: constants_mod.o

%.o: %.F90
	$(F90) $(F90FLAGS) -c $<

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

%_psy.f90:	%.x90
	psyclone --config ../../../config/psyclone.cfg -opsy $*_psy.f90 -oalg $*_alg.f90 $<

clean:
	rm -f *.o *.mod  $(LIB) liblfric.a liblfric_netcdf.a

allclean: clean
