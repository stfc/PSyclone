# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025, Science and Technology Facilities Council.
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

# Defines rules and variables used in all of LFRic Makefiles within the
# directory tree of examples/lfric.

# First include the file that includes all settings common to all examples
this_file := $(abspath $(lastword $(MAKEFILE_LIST)))
# PSyclone directory is up two from this file
this_dir := $(abspath $(dir $(this_file)))

include $(this_dir)/../common.mk

# Define the path to the LFRic infrastructure etc:
LFRIC_PATH = ${PSYCLONE_DIR}/external/lfric_infrastructure/src
LFRIC_NAME=lfric
LFRIC_LIB=$(LFRIC_PATH)/lib$(LFRIC_NAME).a

# This will add the required include flags to LFRIC_INCLUDE_FLAGS
include $(LFRIC_PATH)/lfric_include_flags.mk

F90FLAGS += $(LFRIC_INCLUDE_FLAGS)
LDFLAGS += -L $(LFRIC_PATH) -l$(LFRIC_NAME) $$(nf-config --flibs)

# Add a rule to compile the lfric infrastructure library
$(LFRIC_LIB):
	$(MAKE) -C $(LFRIC_PATH)
