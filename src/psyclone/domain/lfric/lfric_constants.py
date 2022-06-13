# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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
# Modified: I. Kavcic, Met Office
# Modified: R. W. Ford, STFC Daresbury Lab

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
        # pylint: disable=too-many-statements
        if LFRicConstants.HAS_BEEN_INITIALISED:
            return

        LFRicConstants.HAS_BEEN_INITIALISED = True
        api_config = Config.get().api_conf("dynamo0.3")

        # ---------- Evaluators: quadrature -----------------------------------
        LFRicConstants.VALID_QUADRATURE_SHAPES = \
            ["gh_quadrature_xyoz", "gh_quadrature_face", "gh_quadrature_edge"]
        LFRicConstants.VALID_EVALUATOR_SHAPES = \
            LFRicConstants.VALID_QUADRATURE_SHAPES + ["gh_evaluator"]

        # ---------- LFRicArgDescriptor class constants  ----------------------

        # Supported LFRic API argument types (scalars, fields, operators)
        LFRicConstants.VALID_SCALAR_NAMES = ["gh_scalar"]
        LFRicConstants.VALID_FIELD_NAMES = ["gh_field"]
        LFRicConstants.VALID_OPERATOR_NAMES = ["gh_operator",
                                               "gh_columnwise_operator"]
        LFRicConstants.VALID_ARG_TYPE_NAMES = \
            LFRicConstants.VALID_FIELD_NAMES + \
            LFRicConstants.VALID_OPERATOR_NAMES + \
            LFRicConstants.VALID_SCALAR_NAMES

        # Supported API argument data types ('gh_real', 'gh_integer'
        # and 'gh_logical')
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

        # ---------- Fortran datatypes ----------------------------------------
        # This is only used here, so no class variable:
        supported_fortran_datatypes = api_config.supported_fortran_datatypes

        # psyGen intrinsic types for kernel argument data as defined in LFRic
        # configuration by the supported Fortran datatypes ('real', 'integer'
        # and 'logical').
        LFRicConstants.VALID_INTRINSIC_TYPES = supported_fortran_datatypes

        # Valid intrinsic types for field kernel argument data
        # ('real' and 'integer').
        LFRicConstants.VALID_FIELD_INTRINSIC_TYPES = ["real", "integer"]

        # ---------- Mapping from metadata data_type to Fortran intrinsic type
        LFRicConstants.MAPPING_DATA_TYPES = \
            OrderedDict(zip(LFRicConstants.VALID_ARG_DATA_TYPES,
                            LFRicConstants.VALID_INTRINSIC_TYPES))

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

        # ---------- Function spaces (FS) -------------------------------------
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
            f"any_space_{x+1}" for x in
            range(api_config.num_any_space)]

        # Valid any_discontinuous_space metadata (general FS known to be
        # discontinuous). The number of 'ANY_DISCONTINUOU_SPACE' spaces is
        # set in the PSyclone configuration file.
        LFRicConstants.VALID_ANY_DISCONTINUOUS_SPACE_NAMES = [
            f"any_discontinuous_space_{x+1}" for x in
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

        # ---------- Map from scalar intrinsic type to its precision ----------
        LFRicConstants.SCALAR_PRECISION_MAP = \
            OrderedDict(zip(LFRicConstants.VALID_INTRINSIC_TYPES,
                            ["r_def", "i_def", "l_def"]))

        # ---------- Infrastructure module maps -------------------------------

        # Dictionary allowing us to look-up the name of the Fortran module,
        # type and proxy-type associated with each LFRic data structure type.
        # Data structure type mandates its proxy name, Fortran intrinsic type
        # of its data and the kind (precision) for the intrinsic type.
        LFRicConstants.DATA_TYPE_MAP = {
            # 'real'-valued scalar reduction of kind 'r_def' (used for global
            # reductions of "field_type" data)
            "reduction": {"module": "scalar_mod",
                          "type": "scalar_type",
                          "proxy_type": None,
                          "intrinsic": "real",
                          "kind": "r_def"},
            # 'real'-valued field with data of kind 'r_def'
            "field": {"module": "field_mod",
                      "type": "field_type",
                      "proxy_type": "field_proxy_type",
                      "intrinsic": "real",
                      "kind": "r_def"},
            # 'real'-valued field with data of kind 'r_solver'
            "r_solver_field": {"module": "r_solver_field_mod",
                               "type": "r_solver_field_type",
                               "proxy_type": "r_solver_field_proxy_type",
                               "intrinsic": "real",
                               "kind": "r_solver"},
            # 'real'-valued field with data of kind 'r_tran'
            "r_tran_field": {"module": "r_tran_field_mod",
                             "type": "r_tran_field_type",
                             "proxy_type": "r_tran_field_proxy_type",
                             "intrinsic": "real",
                             "kind": "r_tran"},
            # 'integer'-valued field with data of kind 'i_def'
            "integer_field": {"module": "integer_field_mod",
                              "type": "integer_field_type",
                              "proxy_type": "integer_field_proxy_type",
                              "intrinsic": "integer",
                              "kind": "i_def"},
            # 'real'-valued operator with data of kind 'r_def'
            "operator": {"module": "operator_mod",
                         "type": "operator_type",
                         "proxy_type": "operator_proxy_type",
                         "intrinsic": "real",
                         "kind": "r_def"},
            # 'real'-valued operator with data of kind 'r_solver'
            "r_solver_operator": {
                "module": "operator_mod",
                "type": "r_solver_operator_type",
                "proxy_type": "r_solver_operator_proxy_type",
                "intrinsic": "real",
                "kind": "r_solver"},
            # 'real'-valued columnwise operator with data of kind 'r_solver'
            "columnwise_operator": {
                "module": "columnwise_operator_mod",
                "type": "columnwise_operator_type",
                "proxy_type": "columnwise_operator_proxy_type",
                "intrinsic": "real",
                "kind": "r_solver"}}

        # Mapping from a vector type used in the algorithm-layer to
        # the actual type used in the PSy-layer.
        LFRicConstants.FIELD_VECTOR_TO_FIELD_MAP = {
            "field_vector_type": "field_type",
            "r_solver_field_vector_type": "r_solver_field_type",
            "r_tran_field_vector_type": "r_tran_field_type"}

        # Dictionary allowing us to look-up the name of the Fortran module
        # and type (if existing) associated with stencil shapes and directions.
        LFRicConstants.STENCIL_TYPE_MAP = {
            "stencil_dofmap": {"module": "stencil_dofmap_mod",
                               "type": "stencil_dofmap_type"},
            "stencil_2D_dofmap": {"module": "stencil_2D_dofmap_mod",
                                  "type": "stencil_2D_dofmap_type"},
            "direction": {"module": "flux_direction_mod"}}

        # Dictionary allowing us to look-up the name of the Fortran module,
        # type and proxy-type associated with each quadrature shape.
        LFRicConstants.QUADRATURE_TYPE_MAP = {
            "gh_quadrature_xyoz": {"module": "quadrature_xyoz_mod",
                                   "type": "quadrature_xyoz_type",
                                   "proxy_type": "quadrature_xyoz_proxy_type",
                                   "intrinsic": "real",
                                   "kind": "r_def"},
            "gh_quadrature_face": {"module": "quadrature_face_mod",
                                   "type": "quadrature_face_type",
                                   "proxy_type": "quadrature_face_proxy_type",
                                   "intrinsic": "real",
                                   "kind": "r_def"},
            "gh_quadrature_edge": {"module": "quadrature_edge_mod",
                                   "type": "quadrature_edge_type",
                                   "proxy_type": "quadrature_edge_proxy_type",
                                   "intrinsic": "real",
                                   "kind": "r_def"}}

        # Dictionary allowing us to look-up the name of the Fortran module
        # and type associated with mesh.
        LFRicConstants.MESH_TYPE_MAP = {
            "mesh": {"module": "mesh_mod",
                     "type": "mesh_type"},
            "mesh_map": {"module": "mesh_map_mod",
                         "type": "mesh_map_type"}}

        # Dictionary allowing us to look-up the name of the Fortran module
        # and type associated with reference element.
        LFRicConstants.REFELEMENT_TYPE_MAP = {
            "refelement": {"module": "reference_element_mod",
                           "type": "reference_element_type"}}

        # Dictionary allowing us to look-up the name of the Fortran module
        # and type (if existing) associated with function space.
        LFRicConstants.FUNCTION_SPACE_TYPE_MAP = {
            # Function space type (for basis and differential basis functions)
            "function_space": {"module": "function_space_mod",
                               "type": "function_space_type"},
            # Function space identifiers
            "fs_continuity": {"module": "fs_continuity_mod"}}

        # Dictionary allowing us to look-up the name of the Fortran modules
        # that store various utilities in LFRic.
        LFRicConstants.UTILITIES_MOD_MAP = {
            # Constants module (stores precisions)
            "constants": {"module": "constants_mod"},
            # Logging module (used for runtime checks)
            "logging": {"module": "log_mod"}}


# =============================================================================
# Documentation utils: The list of module members that we wish AutoAPI to
# generate documentation for (see https://psyclone-ref.readthedocs.io).
__all__ = ['LFRicConstants']
