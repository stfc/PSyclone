# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
LFRIC_PATH := $(dir $(this_file))

# $(LFRIC_PATH) is the location of the source files. Define
# $(BUILD_DIR) to point to the build directory, which the include
# makefile will then use instead of $(LFRIC_PATH) for creating the
# include paths (note: do not use $PWD, this variable might not
# get updated when `make -C ...` is used to change the directory)
BUILD_PATH := $(shell pwd)
include $(LFRIC_PATH)/lfric_include_flags.inc

# Since the repo includes the pre-processed files (.f90), we only
# need to search for those here.
ALL_SRC := $(subst $(LFRIC_PATH),,\
		    $(shell find $(LFRIC_PATH) -name '*.f90' -print))
ALL_OBJ = $(subst .F90,.o,$(subst .f90,.o,$(ALL_SRC)))

# Additional object files required for netcdf support
OBJ_NETCDF = mesh/global_mesh_netcdf_mod.o io/ugrid_2d_mod.o     \
	io/ugrid_file_mod.o io/file_mod.o io/ugrid_generator_mod.o \
	io/ncdf_quad_mod.o

# We need to remove the objects only required for NetCDF from the
# list of all objects (since we don't want to require NetCDF for
# most compilation tests):
OBJ := $(filter-out $(OBJ_NETCDF),$(ALL_OBJ))

.PHONY: standalone netcdf dirs get_include_flags preprocess

# Standalone target, no netcdf dependency:
# ----------------------------------------
standalone: liblfric.a

liblfric.a: dirs $(OBJ)
	$(AR) $(ARFLAGS) $@ $(OBJ)

# Netcdf target, like standalone but with support for netcdf files:
# -----------------------------------------------------------------
netcdf: LFRIC_INCLUDE_FLAGS += $$(nf-config --fflags)
netcdf: OBJ += $(OBJ_NETCDF)
netcdf: liblfric_netcdf.a
liblfric_netcdf.a: dirs $(OBJ) $(OBJ_NETCDF)
	$(AR) $(ARFLAGS) $@ $(OBJ) $(OBJ_NETCDF)

# Target for preprocessing the .F90 files, since PSyclone only
# supports pre-processed files. The environment variable FPPFLAGS
# can be set to define preprocessor macros, e.g.:
#    FPPFLAGS="-DRDEF_PRECISION=32" make preprocess
# Since 'make' uses time stamps, you need to provide the -B flag
# to make to enforce re-preprocessing of all files.
# ----------------------------------------------------------------
FILES_TO_PREPROCESS = $(filter %.F90,$(ALL_SRC))
ALL_PREPROCESSED = $(patsubst %.F90,%.f90,$(FILES_TO_PREPROCESS))

preprocess: $(ALL_PREPROCESSED)

# Other rules
dirs:
	# Create all required build directories. $(DIRS) is defined by the
	# included makefile `lfric_include_flags.inc` above.
	mkdir -p $(DIRS)

# Dependencies
# ------------
include $(LFRIC_PATH)/dependency

# Rule for preprocessing:
# -----------------------
%.f90: %.F90
	$(CPP) -P  $(FPPFLAGS) $< > $@

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
#     cd field; $(F90) $(F90FLAGS) $(LFRIC_INCLUDE_FLAGS) -c $< -o $(notdir $@)

define compile_rule_template =
$(1)/%.o:	$$(LFRIC_PATH)/$(1)/%.f90 dirs
	cd $(1); $$(F90) $$(F90FLAGS) $$(LFRIC_INCLUDE_FLAGS) -c $$< -o $$(notdir $$@)

$(1)/%.o:	$$(LFRIC_PATH)/$(1)/%.F90 dirs
	cd $(1); $$(F90) $$(F90FLAGS) $$(LFRIC_INCLUDE_FLAGS) -c $$< -o $$(notdir $$@)
endef

# Define the required rules for each subdirectory. DIRS is defined
# by the included file `lfric_include_flags.inc`
$(foreach dir,$(DIRS),$(eval $(call compile_rule_template,$(dir))))

clean:
	rm -f */*.o */*.mod
	rm -f liblfric.a liblfric_netcdf.a

allclean: clean
