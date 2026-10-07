# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------


'''Tests for class storing API-specific constants.'''

from psyclone.configuration import Config
from psyclone.domain.lfric import LFRicConstants


def test_lfric_const():
    '''Tests the LFRic constant object.
    '''
    # This guarantees that the first time we use the constant object,
    # we read it from the config file.
    LFRicConstants.HAS_BEEN_INITIALISED = False
    config = Config.get()

    lfric_const = config.api_conf("lfric").get_constants()
    # Don't test intrinsic_types, which comes from the config file
    assert lfric_const.VALID_ARG_TYPE_NAMES == ["gh_field", "gh_operator",
                                                "gh_columnwise_operator",
                                                "gh_scalar_array", "gh_scalar"]

    assert lfric_const.VALID_SCALAR_NAMES == ["gh_scalar"]

    assert LFRicConstants.HAS_BEEN_INITIALISED
    # Test that we don't initialise the object again, i.e. that a
    # modified value is not changed.
    LFRicConstants.VALID_INTRINSIC_TYPES = "INVALID"
    lfric_const = LFRicConstants()
    assert lfric_const.VALID_INTRINSIC_TYPES == "INVALID"
    assert lfric_const.VALID_ARG_TYPE_NAMES == ["gh_field", "gh_operator",
                                                "gh_columnwise_operator",
                                                "gh_scalar_array", "gh_scalar"]
    assert lfric_const.VALID_SCALAR_NAMES == ["gh_scalar"]
    assert lfric_const.VALID_ARG_DATA_TYPES == ["gh_real", "gh_integer",
                                                "gh_logical"]
    assert lfric_const.DATA_TYPE_MAP["operator"]["kind"] == "r_def"
    assert lfric_const.DATA_TYPE_MAP["integer_field"]["kind"] == "i_def"
    assert lfric_const.DATA_TYPE_MAP["r_solver_field"]["kind"] == "r_solver"
    # Make sure the 'INVALID' value is reset when the constant
    # object is created again.
    LFRicConstants.HAS_BEEN_INITIALISED = False
