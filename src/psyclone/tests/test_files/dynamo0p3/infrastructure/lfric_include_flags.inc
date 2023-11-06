# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2023, Science and Technology Facilities Council.
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
# Modifications: A. R. Porter, STFC Daresbury Laboratory

# This makefiles finds all subdirectories of $(LFRIC_PATH) and adds
# include directives for each of these paths to F90FLAGS. For example.
# if the directories of $(LFRIC_PATH) are `field` and `mesh`, it would add:
# -I $(LFRIC_PATH)/field -I $(LFRIC_PATH)/mesh
# This makefile is included from the makefile that builds the LFRic
# infrastructure files of PSyclone, but can also be used by any other
# Makefile that needs the LFRic infrastructure (e.g. in the examples or
# PSyData wrapper libraries).
#
# The following variables are defined:
# DIRS: all subdirectories under $(LFRIC_PATH).
# LFRIC_INCLUDE_FLAGS: the required include flags to compile using the
#     LFRic infrastructure files.

# If this file is used to determine the flags of an already compiled
# version of the infrastructure library, the include path must use
# $(LFRIC_PATH), which is the location of the compiled LFRic
# infrastructure files.
# But if this file is used to compile the infrastructure library
# $(LFRIC_PATH) is actually the location of the source files and not
# the build directory. So in this case the calling Makefile has to
# define $(BUILD_PATH) to point to the build directory.

BUILD_PATH ?= $(LFRIC_PATH)
DIRS := $(subst $(LFRIC_PATH),,\
		    $(shell find $(LFRIC_PATH) -type d -print))
LFRIC_INCLUDE_FLAGS ?= $(patsubst %,-I $(BUILD_PATH)/%,$(DIRS))
