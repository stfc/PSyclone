# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2026, Science and Technology Facilities Council.
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
# -----------------------------------------------------------------------------
# Author J. Henrichs, Bureau of Meteorology
# Modified by A. R. Porter, STFC Daresbury Lab,
#             I. Kavcic, Met Office

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
ALL_LIBS = $(NO_DEP_LIBS) dl_timer drhook nvidia tau vernier

.PHONY: default all $(NO_DEP_LIBS) clean allclean \
	dl_timer drhook nvidia tau vernier

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
