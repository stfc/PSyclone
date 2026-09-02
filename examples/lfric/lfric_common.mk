# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

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
