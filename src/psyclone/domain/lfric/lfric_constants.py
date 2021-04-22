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
This module provides a class with all LFRic related constants.
'''

# Imports
from __future__ import print_function, absolute_import

from collections import OrderedDict

from psyclone.configuration import Config
from psyclone.domain.lfric import LFRicArgDescriptor
from psyclone.domain.lfric.lfric_builtins import BUILTIN_ITERATION_SPACES


# pylint: disable=too-few-public-methods
class LFRicConstants(object):
    '''This class stores all LFRic constants. Note that some constants
    depend on values in the config files, so this class can only be
    used after the config file can be read.
    It stores all values in class variables (to avoid re-evaluating them).
    '''

    HAS_BEEN_INITIALISED = False

    def __init__(self):
        if LFRicConstants.HAS_BEEN_INITIALISED:
            return

        LFRicConstants.HAS_BEEN_INITIALISED = True

        # ---------- Evaluators: quadrature ----------------------------------
        LFRicConstants.VALID_QUADRATURE_SHAPES = \
            ["gh_quadrature_xyoz", "gh_quadrature_face", "gh_quadrature_edge"]
        LFRicConstants.VALID_EVALUATOR_SHAPES = \
            LFRicConstants.VALID_QUADRATURE_SHAPES + ["gh_evaluator"]

        # ---------- Fortran datatypes ---------------------------------------
        # This is only used here, so no class variable:
        supported_fortran_datatypes = Config.get().api_conf(
            "dynamo0.3").supported_fortran_datatypes

        # ---------- Mapping from metadata data_type to Fortran intrinsic type
        LFRicConstants.MAPPING_DATA_TYPES = \
            OrderedDict(zip(LFRicArgDescriptor.VALID_ARG_DATA_TYPES,
                            supported_fortran_datatypes[0:2]))

        # psyGen intrinsic types for kernel argument data as defined in LFRic.
        LFRicConstants.VALID_INTRINSIC_TYPES = \
            list(LFRicConstants.MAPPING_DATA_TYPES.values())

        # ---------- Evaluators -----------------------------------------------

        # Dictionary allowing us to look-up the name of the Fortran module,
        # type and proxy-type associated with each quadrature shape
        LFRicConstants.QUADRATURE_TYPE_MAP = {
            "gh_quadrature_xyoz": {"module": "quadrature_xyoz_mod",
                                   "type": "quadrature_xyoz_type",
                                   "proxy_type": "quadrature_xyoz_proxy_type"},
            "gh_quadrature_face": {"module": "quadrature_face_mod",
                                   "type": "quadrature_face_type",
                                   "proxy_type": "quadrature_face_proxy_type"},
            "gh_quadrature_edge": {"module": "quadrature_edge_mod",
                                   "type": "quadrature_edge_type",
                                   "proxy_type": "quadrature_edge_proxy_type"}}

        # ---------- Loops (bounds, types, names) -----------------------------
        # These are loop bound names which identify positions in a field's
        # halo. It is useful to group these together as we often need to
        # determine whether an access to a field or other object includes
        # access to the halo, or not.
        LFRicConstants.HALO_ACCESS_LOOP_BOUNDS = ["cell_halo", "dof_halo",
                                                  "colour_halo"]

        LFRicConstants.VALID_LOOP_BOUNDS_NAMES = \
            (["start",     # the starting
                           # index. Currently this is
                           # always 1
              "inner",     # a placeholder for when we
                           # support loop splitting into
                           # work that does not access
                           # the halo and work that does.
                           # This will be used to help
                           # overlap computation and
                           # communication
              "ncolour",   # the number of cells with
                           # the current colour
              "ncolours",  # the number of colours in a
                           # coloured loop
              "ncells",    # the number of owned cells
              "ndofs",     # the number of owned dofs
              "nannexed"]  # the number of owned dofs
                           # plus the number of annexed
                           # dofs. As the indices of
                           # dofs are arranged that
                           # owned dofs have lower
                           # indices than annexed dofs,
                           # having this value as an
                           # upper bound will compute
                           # both owned and annexed
                           # dofs.
             + LFRicConstants.HALO_ACCESS_LOOP_BOUNDS)

        # Valid LFRic loop types. The default is "" which is over cell columns
        # (in the horizontal plane). A "null" loop doesn't iterate over
        # anything but is required for the halo-exchange logic.
        LFRicConstants.VALID_LOOP_TYPES = ["dof", "colours", "colour", "",
                                           "null"]

        # Valid LFRic iteration spaces for user-supplied kernels and
        # built-in kernels
        LFRicConstants.USER_KERNEL_ITERATION_SPACES = ["cell_column", "domain"]
        LFRicConstants.VALID_ITERATION_SPACES = \
            LFRicConstants.USER_KERNEL_ITERATION_SPACES + \
            BUILTIN_ITERATION_SPACES

        # psyGen argument types translate to LFRic argument types.
        LFRicConstants.VALID_ARG_TYPE_NAMES = \
            LFRicArgDescriptor.VALID_ARG_TYPE_NAMES


# =============================================================================
# Documentation utils: The list of module members that we wish AutoAPI to
# generate documentation for (see https://psyclone-ref.readthedocs.io).
__all__ = ['LFRicConstants']
