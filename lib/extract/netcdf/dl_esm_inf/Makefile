#------------------------------------------------------------------------------
# BSD 2-Clause License
# 
# Copyright (c) 2020-2021, Science and Technology Facilities Council.
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
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#------------------------------------------------------------------------------
# Author: J. Henrichs, Australian Bureau of Meteorology
# Modified: I. Kavcic, Met Office

# This Makefile picks up the compiler to use plus any flags from
# environment variables. e.g.:
# 
# export F90=mpif90
# export F90FLAGS="-O3"
# export AR=ar
#
# It also uses nf-config to query for flags required for NetCDF compilation.

# ----------- Default "make" values, can be overwritten by the user -----------
# Compiler and compiler flags
F90 ?= gfortran
F90FLAGS ?=
# Path to the 'dl_esm_inf' infrastructure library. It defaults to the version
# distributed with PSyclone. Overwrite for a different infrastructure version.
GOCEAN_INF_DIR ?= ./../../../../external/dl_esm_inf/finite_difference
# Paths to the PSyclone wrapper libraries and required templates. They default
# to the relative paths to the top-level 'lib' and 'lib/extract/netcdf'
# directories. Overwrite for a custom location.
PSYDATA_LIB_DIR ?= ./../../..
LIB_TMPLT_DIR ?= ./..
# -----------------------------------------------------------------------------

INF_INC = $(GOCEAN_INF_DIR)/src
INF_LIB_NAME = _fd
INF_LIB = $(GOCEAN_INF_DIR)/src/lib$(INF_LIB_NAME).a

F90FLAGS += $$(nf-config --fflags) -I$(INF_INC)
PSYDATA_LIB_NAME = _kernel_data_netcdf
PSYDATA_LIB = lib$(PSYDATA_LIB_NAME).a

PROCESS_ARGS = -prefix=extract_ -types=int,real,double \
                       -dims=2
PROCESS = $$($(PSYDATA_LIB_DIR)/get_python.sh) $(PSYDATA_LIB_DIR)/process.py

OBJS = psy_data_base.o kernel_data_netcdf.o extract_netcdf_base.o

default: $(PSYDATA_LIB)

.PHONY: default clean allclean

$(PSYDATA_LIB): $(INF_LIB) $(OBJS)
	${AR} ${ARFLAGS} ${PSYDATA_LIB} $^

kernel_data_netcdf.o:	extract_netcdf_base.o $(INF_LIB)
extract_netcdf_base.o:	psy_data_base.o

psy_data_base.f90: $(PSYDATA_LIB_DIR)/psy_data_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) $< > psy_data_base.f90

extract_netcdf_base.f90: $(LIB_TMPLT_DIR)/extract_netcdf_base.jinja Makefile
	$(PROCESS) $(PROCESS_ARGS) -generic-declare -generic-provide $< > extract_netcdf_base.f90

# The dl_esm_inf library
$(INF_LIB):
	$(MAKE) -C $(GOCEAN_INF_DIR)

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

clean:
	rm -f *.o *.mod $(PSYDATA_LIB) psy_data_base.f90 extract_netcdf_base.f90

allclean: clean
	$(MAKE) -C $(GOCEAN_INF_DIR) clean
