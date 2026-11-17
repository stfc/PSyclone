# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# ----------- Default "make" values, can be overwritten by the user -----------
# Compiler and compiler flags
F90 ?= gfortran
F90FLAGS ?= -g
# -----------------------------------------------------------------------------

# We don't build dl_timer, drhook, nvidia, tau, and Vernier by default
# since they require external dependencies/libraries. So this is the
# list of all wrappers that do not have external dependencies:
NO_DEP_LIBS = lfric_timer simple_timing template

# The list with all libraries (include the ones that have additional
# dependencies):
ALL_LIBS = $(NO_DEP_LIBS) amd dl_timer drhook nvidia tau vernier

.PHONY: default all $(NO_DEP_LIBS) clean allclean \
	amd dl_timer drhook nvidia tau vernier

# By default, compile all libraries that do not have additional dependencies
# The 'all' target is used by the compilation tests, so this also can only
# compile the libraries without additional dependencies.
default: all
all: $(NO_DEP_LIBS)

# Invoke make in the corresponding subdirectory for all available libraries
$(ALL_LIBS):
		$(MAKE) -C $@

# We clean all libraries, even the ones not compiled by default
clean:
	$(foreach lib, $(ALL_LIBS), $(MAKE) -C $(lib) clean; )

allclean:
	$(foreach lib, $(ALL_LIBS), $(MAKE) -C $(lib) allclean; )
