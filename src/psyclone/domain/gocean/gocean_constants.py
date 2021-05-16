# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Author: J. Henrichs, Bureau of Meteorology

'''
This module provides a class with all GOcean related constants.
'''

# Imports
from __future__ import print_function, absolute_import


# pylint: disable=too-few-public-methods
class GOceanConstants(object):
    '''This class stores all GOcean constants.
    It stores all values in class variables (to avoid re-evaluating them).
    At this stage it only contains the variables that might be used in
    psyGen.

    '''
    HAS_BEEN_INITIALISED = False

    def __init__(self):
        if GOceanConstants.HAS_BEEN_INITIALISED:
            return

        GOceanConstants.HAS_BEEN_INITIALISED = True

        # Valid intrinsic types of kernel argument data, used in psyGen.
        # Not actually used in GOcean
        GOceanConstants.VALID_INTRINSIC_TYPES = []

        # psyGen argument types
        GOceanConstants.VALID_ARG_TYPE_NAMES = []

        # psyGen names of internal scalar argument types.
        GOceanConstants.VALID_SCALAR_NAMES = ["rscalar", "iscalar"]

        # The different grid-point types that a field can live on
        GOceanConstants.VALID_FIELD_GRID_TYPES = ["go_cu", "go_cv", "go_ct",
                                                  "go_cf", "go_every"]

        # The two scalar types we support
        GOceanConstants.VALID_SCALAR_TYPES = ["go_i_scalar", "go_r_scalar"]

        # Index-offset schemes (for the Arakawa C-grid)
        GOceanConstants.VALID_OFFSET_NAMES = ["go_offset_se", "go_offset_sw",
                                              "go_offset_ne", "go_offset_nw",
                                              "go_offset_any"]

        # The offset schemes for which we can currently generate constant
        # loop bounds in the PSy layer
        GOceanConstants.SUPPORTED_OFFSETS = ["go_offset_ne", "go_offset_sw",
                                             "go_offset_any"]

        # The sets of grid points that a kernel may operate on
        GOceanConstants.VALID_ITERATES_OVER = ["go_all_pts", "go_internal_pts",
                                               "go_external_pts"]

        # The list of valid stencil properties. We currently only support
        # pointwise. This property could probably be removed from the
        # GOcean API altogether.
        GOceanConstants.VALID_STENCIL_NAMES = ["go_pointwise"]

        # The valid types of loop. In this API we expect only doubly-nested
        # loops.
        GOceanConstants.VALID_LOOP_TYPES = ["inner", "outer"]


# =============================================================================
# Documentation utils: The list of module members that we wish AutoAPI to
# generate documentation for (see https://psyclone-ref.readthedocs.io).
__all__ = ['GOceanConstants']
