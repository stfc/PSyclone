# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
# Modified: R. W. Ford, STFC Daresbury Lab

'''
This module provides a class with all GOcean related constants.
'''

# Imports
from psyclone.configuration import Config


# pylint: disable=too-few-public-methods
class GOceanConstants():
    '''This class stores all GOcean constants. It stores all values in
    class variables (to avoid re-evaluating them).

    '''
    HAS_BEEN_INITIALISED = False

    @staticmethod
    def get_valid_access_types():
        '''Return the valid access types for the GOcean API. Reads the values
        from the config file the first time the method is called.

        :returns: valid access types for the GOcean API.
        :rtype: list[str]

        '''
        if not GOceanConstants._VALID_ACCESS_TYPES:
            conf = Config.get().api_conf("gocean1.0")
            GOceanConstants._VALID_ACCESS_TYPES = \
                list(conf.get_access_mapping().keys())
        return GOceanConstants._VALID_ACCESS_TYPES

    def __init__(self):
        if GOceanConstants.HAS_BEEN_INITIALISED:
            return

        GOceanConstants.HAS_BEEN_INITIALISED = True

        # Valid intrinsic types of kernel argument metadata.
        GOceanConstants.VALID_INTRINSIC_TYPES = []

        # Valid access types (GO_READ etc). These are accessed via the
        # get_valid_access_types() method as they are read from the
        # config file rather than being fixed constant values.
        GOceanConstants._VALID_ACCESS_TYPES = []

        # psyGen argument types.
        GOceanConstants.VALID_ARG_TYPE_NAMES = []

        # psyGen names of internal scalar argument types.
        GOceanConstants.VALID_SCALAR_NAMES = ["rscalar", "iscalar"]

        # The different grid-point types that a field can live on.
        GOceanConstants.VALID_FIELD_GRID_TYPES = ["go_cu", "go_cv", "go_ct",
                                                  "go_cf", "go_every"]

        # The two scalar types we support in the kernel argument
        # metadata.
        GOceanConstants.VALID_SCALAR_TYPES = ["go_i_scalar", "go_r_scalar"]

        # Index-offset schemes (for the Arakawa C-grid)
        GOceanConstants.VALID_OFFSET_NAMES = ["go_offset_se", "go_offset_sw",
                                              "go_offset_ne", "go_offset_nw",
                                              "go_offset_any"]

        # The offset schemes for which we can currently generate constant
        # loop bounds in the PSy layer.
        GOceanConstants.SUPPORTED_OFFSETS = ["go_offset_ne", "go_offset_sw",
                                             "go_offset_any"]

        # The sets of grid points that a kernel may operate on.
        GOceanConstants.VALID_ITERATES_OVER = ["go_all_pts", "go_internal_pts",
                                               "go_external_pts"]

        # The list of valid stencil properties. We currently only support
        # pointwise. This property could probably be removed from the
        # GOcean API altogether.
        GOceanConstants.VALID_STENCIL_NAMES = ["go_pointwise"]

        # The name used to indicate there is a stencil. This will
        # contain 3 arguments specifying the type of stencil being
        # used.
        GOceanConstants.VALID_STENCIL_NAME = "go_stencil"

        # The valid types of loop. In this API we expect only doubly-nested
        # loops.
        GOceanConstants.VALID_LOOP_TYPES = ["inner", "outer"]


# =============================================================================
# Documentation utils: The list of module members that we wish AutoAPI to
# generate documentation for (see https://psyclone-ref.readthedocs.io).
__all__ = ['GOceanConstants']
