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


CONFIGURATION_SRC = domain_size_config_mod.f90
FIELD_SRC = argument_mod.F90 field_mod.F90 field_parent_mod.f90 kernel_mod.o\
	integer_field_mod.F90 master_dofmap_mod.F90 polynomial_mod.F90          \
	pure_abstract_field_mod.f90 r_solver_field_mod.F90 r_tran_field_mod.F90 \
	stencil_2d_dofmap_mod.f90 stencil_dofmap_helper_functions_mod.f90       \
	stencil_dofmap_mod.F90
FUNCTION_SPACE_SRC = fs_continuity_mod.F90 function_space_mod.F90           \
    function_space_constructor_helper_functions_mod.F90
INITIALISTATION_SRC = flux_direction_mod.f90
IO_SRC = file_mod.F90 io_utility_mod.f90
MESH_SRC = extrusion_mod.f90 mesh_constructor_helper_functions_mod.F90      \
	mesh_mod.F90 reference_element_mod.F90 global_mesh_base_mod.F90         \
	mesh_colouring_mod.F90 mesh_map_mod.F90 partition_mod.F90
OPERATOR_SRC = columnwise_operator_mod.f90 operator_mod.F90                 \
	operator_parent_mod.f90
SCALAR_SRC = scalar_mod.F90
UTILITIES_SRC = constants_mod.F90 count_mod.f90 linked_list_data_mod.F90    \
	linked_list_mod.F90 matrix_invert_mod.F90 timer_mod.F90                 \
	coord_transform_mod.F90 cross_product_mod.F90 linked_list_int_mod.F90   \
	log_mod.F90 mpi_mod.f90

DIRS = configuration field function_space initialisation io mesh operator \
	scalar utilities

F90FLAGS += $(foreach dir,$(DIRS),-I ../$(dir))

CONFIGURATION_PATH_SRC = $(addprefix configuration/,$(CONFIGURATION_SRC))
FIELD_PATH_SRC = $(addprefix field/,$(FIELD_SRC))
FUNCTION_SPACE_PATH_SRC = $(addprefix function_space/,$(FUNCTION_SPACE_SRC))
INITIALISATION_PATH_SRC = $(addprefix initialisation/,$(INITIALISATION_SRC))
IO_PATH_SRC = $(addprefix io/,$(IO_SRC))
MESH_PATH_SRC = $(addprefix mesh/,$(MESH_SRC))
OPERATOR_PATH_SRC = $(addprefix operator/,$(OPERATOR_SRC))
SCALAR_PATH_SRC = $(addprefix scalar/,$(SCALAR_SRC))
UTILITIES_PATH_SRC = $(addprefix utilities/,$(UTILITIES_SRC))

ALL_SRC = $(CONFIGURATION_PATH_SRC) $(FIELD_PATH_SRC)                    \
	$(FUNCTION_SPACE_PATH_SRC) $(INITIALISATION_PATH_SRC) $(IO_PATH_SRC) \
	$(MESH_PATH_SRC) $(OPERATOR_PATH_SRC) $(SCALAR_PATH_SRC)             \
	$(UTILITIES_PATH_SRC)

# Replace all *.F90 and *.f90 with .o to get a list of all object files.
OBJ = $(patsubst %.f90,%.o,$(patsubst %.F90,%.o,$(ALL_SRC)))

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

%_psy.f90:	%.x90
	psyclone --config ../../../config/psyclone.cfg -opsy $*_psy.f90 -oalg $*_alg.f90 $<

clean:
	rm -f */*.o */*.mod
	rm -f liblfric.a liblfric_netcdf.a

allclean: clean
