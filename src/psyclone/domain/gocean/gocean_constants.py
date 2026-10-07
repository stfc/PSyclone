# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
This module provides a class with all GOcean related constants.
'''

from psyclone.core.access_type import AccessType


# pylint: disable=too-few-public-methods
class GOceanConstants():
    '''This class stores all GOcean constants. It stores all values in
    class variables (to avoid re-evaluating them).

    '''
    HAS_BEEN_INITIALISED = False

    def __init__(self):
        if GOceanConstants.HAS_BEEN_INITIALISED:
            return

        GOceanConstants.HAS_BEEN_INITIALISED = True

        # Valid intrinsic types of kernel argument metadata.
        GOceanConstants.VALID_INTRINSIC_TYPES = []

        # Valid access types.
        GOceanConstants.VALID_ACCESS_TYPES = ["go_read",
                                              "go_write",
                                              "go_readwrite"]

        # Mapping from API-specific access types to internal access types.
        GOceanConstants.ACCESS_MAPPING = {"go_read": AccessType.READ,
                                          "go_write": AccessType.WRITE,
                                          "go_readwrite": AccessType.READWRITE
                                          }
        GOceanConstants.REVERSE_ACCESS_MAPPING = {}
        for key, value in GOceanConstants.ACCESS_MAPPING.items():
            GOceanConstants.REVERSE_ACCESS_MAPPING[value] = key

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
# generate documentation for.
__all__ = ['GOceanConstants']
