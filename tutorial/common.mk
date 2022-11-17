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
# Author: A. R. Porter, STFC Daresbury Laboratory
# Modified J. Henrichs, Bureau of Meteorology

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
