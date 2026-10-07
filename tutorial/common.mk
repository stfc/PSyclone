# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# Make sure we use the configuration file distributed with PSyclone
# instead of any locally-installed version.
#
# **Note** that this code to find the correct directory only works if
#          the tutorial directory is still within the standard PSyclone
#          source tree. If it has been moved then the PSYCLONE_CONFIG
#          environment variable will have to be set to the full path
#          to the config file before make is launched.
#
# MAKEFILE_LIST is a Gnu-make variable that contains all of the
# arguments passed to the first invocation of Make. The last entry
# in this list is the current file.

this_file := $(abspath $(lastword $(MAKEFILE_LIST)))

# PSyclone directory is up one from this file
PSYCLONE_DIR := $(abspath $(dir $(this_file))..)

RM = rm -f
PYTHON ?= python
NOTEBOOK_FILES = $(wildcard ./*ipynb)

ifeq (,$(wildcard ${PSYCLONE_DIR}/config/psyclone.cfg))
  # Failed to find the configuration file so don't attempt to specify it.
  # Will be picked up from default locations or $PSYCLONE_CONFIG.
  PSYCLONE ?= psyclone
  JUPYTER = jupyter
else
  PSYCLONE ?= psyclone -l output --config ${PSYCLONE_DIR}/config/psyclone.cfg
  JUPYTER = PSYCLONE_CONFIG=${PSYCLONE_DIR}/config/psyclone.cfg jupyter
endif

.PHONY: transform compile run clean allclean notebook ${NOTEBOOK_FILES}
.DEFAULT_GOAL := transform

# How we run Jupyter notebooks. We explicitly specify which python kernel
# to use as otherwise it is taken from the notebook meta-data and this might
# not agree with what's currently available (particularly in a CI
# environment).
JUPYTER_ARGS = nbconvert --ExecutePreprocessor.kernel_name=${PYTHON} \
               --to notebook --execute

# Rule that attempts to execute all Jupyter notebooks in the current dir
${NOTEBOOK_FILES}:
	${JUPYTER} ${JUPYTER_ARGS} $@

notebook: ${NOTEBOOK_FILES}
