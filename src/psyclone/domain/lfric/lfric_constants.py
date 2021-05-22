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


# pylint: disable=too-few-public-methods
class LFRicConstants(object):
    '''This class stores all LFRic constants. Note that some constants
    depend on values in the config file, so this class can only be
    used after the config file has been read.
    It stores all values in class variables (to avoid re-evaluating them).
    '''

    HAS_BEEN_INITIALISED = False

    def __init__(self):
        if LFRicConstants.HAS_BEEN_INITIALISED:
            return

        LFRicConstants.HAS_BEEN_INITIALISED = True
        api_config = Config.get().api_conf("dynamo0.3")

        # ---------- Evaluators: quadrature ----------------------------------
        LFRicConstants.VALID_QUADRATURE_SHAPES = \
            ["gh_quadrature_xyoz", "gh_quadrature_face", "gh_quadrature_edge"]
        LFRicConstants.VALID_EVALUATOR_SHAPES = \
            LFRicConstants.VALID_QUADRATURE_SHAPES + ["gh_evaluator"]

        # ---------- LFRicArgDescriptor class constants  ---------------------

        # Supported LFRic API argument types (scalars, fields, operators)
        LFRicConstants.VALID_SCALAR_NAMES = ["gh_scalar"]
        LFRicConstants.VALID_FIELD_NAMES = ["gh_field"]
        LFRicConstants.VALID_OPERATOR_NAMES = ["gh_operator",
                                               "gh_columnwise_operator"]
        LFRicConstants.VALID_ARG_TYPE_NAMES = \
            LFRicConstants.VALID_FIELD_NAMES + \
            LFRicConstants.VALID_OPERATOR_NAMES + \
            LFRicConstants.VALID_SCALAR_NAMES

        # Supported API argument data types ('real', 'integer' and 'logical')
        LFRicConstants.VALID_ARG_DATA_TYPES = \
            ["gh_real", "gh_integer", "gh_logical"]
        LFRicConstants.VALID_SCALAR_DATA_TYPES = \
            LFRicConstants.VALID_ARG_DATA_TYPES
        LFRicConstants.VALID_FIELD_DATA_TYPES = ["gh_real", "gh_integer"]
        LFRicConstants.VALID_OPERATOR_DATA_TYPES = ["gh_real"]

        # pylint: disable=too-many-instance-attributes

        # Supported LFRic API stencil types and directions
        LFRicConstants.VALID_STENCIL_TYPES = ["x1d", "y1d", "xory1d", "cross",
                                              "region", "cross2d"]
        # Note, can't use VALID_STENCIL_DIRECTIONS at all locations in this
        # file as it causes failures with py.test 2.8.7. Therefore some parts
        # of the code do not use the VALID_STENCIL_DIRECTIONS variable.
        LFRicConstants.VALID_STENCIL_DIRECTIONS = ["x_direction",
                                                   "y_direction"]

        # Note, xory1d does not have a direct mapping in STENCIL_MAPPING as it
        # indicates either x1d or y1d.
        LFRicConstants.STENCIL_MAPPING = \
            {"x1d": "STENCIL_1DX", "y1d": "STENCIL_1DY",
             "cross": "STENCIL_CROSS", "cross2d": "STENCIL_2D_CROSS",
             "region": "STENCIL_REGION"}

        # Supported LFRic API mesh types that may be specified for a field
        # using the mesh_arg=... meta-data element (for inter-grid kernels that
        # perform prolongation/restriction).
        LFRicConstants.VALID_MESH_TYPES = ["gh_coarse", "gh_fine"]

        # ---------- Fortran datatypes ---------------------------------------
        # This is only used here, so no class variable:
        supported_fortran_datatypes = api_config.supported_fortran_datatypes

        # ---------- Mapping from metadata data_type to Fortran intrinsic type
        LFRicConstants.MAPPING_DATA_TYPES = \
            OrderedDict(zip(LFRicConstants.VALID_ARG_DATA_TYPES,
                            supported_fortran_datatypes))

        # psyGen intrinsic types for kernel argument data as defined in LFRic
        # ('real', 'integer' and 'logical').
        LFRicConstants.VALID_INTRINSIC_TYPES = \
            list(LFRicConstants.MAPPING_DATA_TYPES.values())

        # Valid intrinsic types for field kernel argument data
        # ('real' and 'integer').
        LFRicConstants.VALID_FIELD_INTRINSIC_TYPES = \
            LFRicConstants.VALID_INTRINSIC_TYPES[0:2]

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

        # Valid LFRic iteration spaces for built-in kernels
        LFRicConstants.BUILTIN_ITERATION_SPACES = ["dof"]

        # The types of argument that are valid for built-in kernels in the
        # LFRic API
        LFRicConstants.VALID_BUILTIN_ARG_TYPES = \
            LFRicConstants.VALID_FIELD_NAMES + \
            LFRicConstants.VALID_SCALAR_NAMES

        # The data types of argument that are valid for built-in kernels
        # in the LFRic API ('real' and 'integer')
        LFRicConstants.VALID_BUILTIN_DATA_TYPES = ["gh_real", "gh_integer"]

        # Valid LFRic iteration spaces for user-supplied kernels and
        # built-in kernels
        LFRicConstants.USER_KERNEL_ITERATION_SPACES = ["cell_column", "domain"]
        LFRicConstants.VALID_ITERATION_SPACES = \
            LFRicConstants.USER_KERNEL_ITERATION_SPACES + \
            LFRicConstants.BUILTIN_ITERATION_SPACES

        # ---------- Function spaces (FS) ------------------------------------
        # Discontinuous FS
        LFRicConstants.DISCONTINUOUS_FUNCTION_SPACES = \
            ["w3", "wtheta", "w2v", "w2vtrace", "w2broken"]

        # Continuous FS
        # Note, any_w2 is not a space on its own. any_w2 is used as a common
        # term for any vector "w2*" function space (w2, w2h, w2v, w2broken) but
        # not w2*trace (spaces of scalar functions). As any_w2 stands for all
        # vector "w2*" spaces it needs to a) be treated as continuous and b)
        # have vector basis and scalar differential basis dimensions.
        # TODO #540: resolve what W2* spaces should be included in ANY_W2 list
        # and whether ANY_W2 should be in the continuous function space list.
        LFRicConstants.ANY_W2_FUNCTION_SPACES = \
            ["w2", "w2h", "w2v", "w2broken"]

        LFRicConstants.CONTINUOUS_FUNCTION_SPACES = \
            ["w0", "w1", "w2", "w2trace", "w2h", "w2htrace", "any_w2"]

        # Read-only FS
        LFRicConstants.READ_ONLY_FUNCTION_SPACES = ["wchi"]

        # Valid FS names
        LFRicConstants.VALID_FUNCTION_SPACES = \
            LFRicConstants.DISCONTINUOUS_FUNCTION_SPACES + \
            LFRicConstants.CONTINUOUS_FUNCTION_SPACES + \
            LFRicConstants.READ_ONLY_FUNCTION_SPACES

        # Valid any_space metadata (general FS, could be continuous or
        # discontinuous). The number of 'ANY_SPACE' spaces is set in the
        # PSyclone configuration file.
        LFRicConstants.VALID_ANY_SPACE_NAMES = [
            "any_space_{0}".format(x+1) for x in
            range(api_config.num_any_space)]

        # Valid any_discontinuous_space metadata (general FS known to be
        # discontinuous). The number of 'ANY_DISCONTINUOU_SPACE' spaces is
        # set in the PSyclone configuration file.
        LFRicConstants.VALID_ANY_DISCONTINUOUS_SPACE_NAMES = [
            "any_discontinuous_space_{0}".format(x+1) for x in
            range(api_config.num_any_discontinuous_space)]

        # Valid discontinuous FS names (for optimisation purposes)
        LFRicConstants.VALID_DISCONTINUOUS_NAMES = \
            LFRicConstants.DISCONTINUOUS_FUNCTION_SPACES +\
            LFRicConstants.VALID_ANY_DISCONTINUOUS_SPACE_NAMES

        # FS names consist of all valid names
        LFRicConstants.VALID_FUNCTION_SPACE_NAMES = \
            LFRicConstants.VALID_FUNCTION_SPACES + \
            LFRicConstants.VALID_ANY_SPACE_NAMES + \
            LFRicConstants.VALID_ANY_DISCONTINUOUS_SPACE_NAMES

        # Lists of function spaces that have
        # a) scalar basis functions;
        LFRicConstants.SCALAR_BASIS_SPACE_NAMES = \
            ["w0", "w2trace", "w2htrace", "w2vtrace", "w3", "wtheta", "wchi"]
        # b) vector basis functions;
        LFRicConstants.VECTOR_BASIS_SPACE_NAMES = ["w1", "w2", "w2h", "w2v",
                                                   "w2broken", "any_w2"]
        # c) scalar differential basis functions;
        LFRicConstants.SCALAR_DIFF_BASIS_SPACE_NAMES = ["w2", "w2h", "w2v",
                                                        "w2broken", "any_w2"]
        # d) vector differential basis functions.
        LFRicConstants.VECTOR_DIFF_BASIS_SPACE_NAMES = \
            ["w0", "w1", "w2trace", "w2htrace", "w2vtrace", "w3", "wtheta",
             "wchi"]

        # Evaluators: basis and differential basis
        LFRicConstants.VALID_EVALUATOR_NAMES = ["gh_basis", "gh_diff_basis"]

        # Meta functions
        LFRicConstants.VALID_METAFUNC_NAMES = \
            LFRicConstants.VALID_EVALUATOR_NAMES


# =============================================================================
# Documentation utils: The list of module members that we wish AutoAPI to
# generate documentation for (see https://psyclone-ref.readthedocs.io).
__all__ = ['LFRicConstants']
