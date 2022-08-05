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

# MAKEFILE_LIST is a Gnu-make variable that contains all of the
# arguments passed to the first invocation of Make. The last entry
# in this list is the current file.
this_file := $(abspath $(lastword $(MAKEFILE_LIST)))
# PSyclone directory is up two from this file
INFRASTRUCTURE_DIR := $(dir $(this_file))

CONFIGURATION_OBJ = domain_size_config_mod.o
FIELD_OBJ = argument_mod.o field_mod.o field_parent_mod.o kernel_mod.o\
	integer_field_mod.o master_dofmap_mod.o polynomial_mod.o          \
	pure_abstract_field_mod.o r_solver_field_mod.o r_tran_field_mod.o \
	stencil_2d_dofmap_mod.o stencil_dofmap_helper_functions_mod.o       \
	stencil_dofmap_mod.o
FUNCTION_SPACE_OBJ = fs_continuity_mod.o function_space_mod.o           \
    function_space_constructor_helper_functions_mod.o
INITIALISTATION_OBJ = flux_direction_mod.o
IO_OBJ = file_mod.o io_utility_mod.o
MESH_OBJ = extrusion_mod.o mesh_constructor_helper_functions_mod.o      \
	mesh_mod.o reference_element_mod.o global_mesh_base_mod.o         \
	mesh_colouring_mod.o mesh_map_mod.o partition_mod.o
OPERATOR_OBJ = columnwise_operator_mod.o operator_mod.o                 \
	operator_parent_mod.o
SCALAR_OBJ = scalar_mod.o
UTILITIES_OBJ = constants_mod.o count_mod.o linked_list_data_mod.o    \
	linked_list_mod.o matrix_invert_mod.o timer_mod.o                 \
	coord_transform_mod.o cross_product_mod.o linked_list_int_mod.o   \
	log_mod.o mpi_mod.o

DIRS = configuration field function_space initialisation io mesh operator \
	scalar utilities

F90FLAGS += $(foreach dir,$(DIRS),-I ../$(dir))

CONFIGURATION_PATH_OBJ = $(addprefix configuration/,$(CONFIGURATION_OBJ))
FIELD_PATH_OBJ = $(addprefix field/,$(FIELD_OBJ))
FUNCTION_SPACE_PATH_OBJ = $(addprefix function_space/,$(FUNCTION_SPACE_OBJ))
INITIALISATION_PATH_OBJ = $(addprefix initialisation/,$(INITIALISATION_OBJ))
IO_PATH_OBJ = $(addprefix io/,$(IO_OBJ))
MESH_PATH_OBJ = $(addprefix mesh/,$(MESH_OBJ))
OPERATOR_PATH_OBJ = $(addprefix operator/,$(OPERATOR_OBJ))
SCALAR_PATH_OBJ = $(addprefix scalar/,$(SCALAR_OBJ))
UTILITIES_PATH_OBJ = $(addprefix utilities/,$(UTILITIES_OBJ))

OBJ = $(CONFIGURATION_PATH_OBJ) $(FIELD_PATH_OBJ)                    \
	$(FUNCTION_SPACE_PATH_OBJ) $(INITIALISATION_PATH_OBJ) $(IO_PATH_OBJ) \
	$(MESH_PATH_OBJ) $(OPERATOR_PATH_OBJ) $(SCALAR_PATH_OBJ)             \
	$(UTILITIES_PATH_OBJ)

# Additional object files required for netcdf support
OBJ_NETCDF = mesh/global_mesh_netcdf_mod.o io/ugrid_2d_mod.o     \
	io/ugrid_file_mod.o io/file_mod.o io/ugrid_generator_mod.o \
	io/ncdf_quad_mod.o

# Note that the files
# quadrature_edge_mod.F90 quadrature_face_mod.F90 quadrature_mod.F90
# quadrature_rule_mod.F90 quadrature_xoyoz_mod.F90 quadrature_xyoz_mod.F90
# quadrature_xyz_mod.F90
# are required for some compilation tests only, they are not required for
# a stand-alone LFRic example, and as such are not compiled when creating
# an infrastructure library.

.PHONY: standalone netcdf default dirs

# Standalone target, no netcdf dependency:
# ----------------------------------------
standalone: liblfric.a
liblfric.a: dirs $(OBJ)
	$(AR) $(ARFLAGS) $@ $(OBJ)

dirs:
	 mkdir -p $(DIRS)

# Netcdf target, like standalone but with support for netcdf files:
# -----------------------------------------------------------------
netcdf: F90FLAGS += $$(nf-config --fflags)
netcdf: OBJ += $(OBJ_NETCDF)
netcdf: liblfric_netcdf.a
liblfric_netcdf.a: $(OBJ) $(OBJ_NETCDF)
	$(AR) $(ARFLAGS) $@ $(OBJ) $(OBJ_NETCDF)


# Dependencies
# ------------
include $(INFRASTRUCTURE_DIR)/dependency

# Compilation rules
# -----------------

# The following function defines two rules to compile SOMEDIR/*.F90
# and SOMEDIR/*.f90 into SOMEDIR/*.o. Since LFRic stores the mod and
# .o files in the same directory, we mirror this behaviour here. Instead
# of using compiler-specific flags to control the output location for
# .mod files, this changes into the directory where the .o files should
# be stored. This avoids the hassle of handling compiler specific flags.
# An example for the rule created:
# field/%.o: $(INFRASTRUCTURE)/field/%.f90
#     cd field; $(F90) $(F90FLAGS) -c $< -o $(notdir $@)

define compile_rule_template =
$(1)/%.o:	$$(INFRASTRUCTURE_DIR)/$(1)/%.f90
	cd $(1); $$(F90) $$(F90FLAGS) -c $$< -o $$(notdir $$@)

$(1)/%.o:	$$(INFRASTRUCTURE_DIR)/$(1)/%.F90
	cd $(1); $$(F90) $$(F90FLAGS) -c $$< -o $$(notdir $$@)
endef

# Define the required rules for each subdirectory.
$(foreach dir,$(DIRS),$(eval $(call compile_rule_template,$(dir))))

clean:
	rm -f */*.o */*.mod
	rm -f liblfric.a liblfric_netcdf.a

allclean: clean
