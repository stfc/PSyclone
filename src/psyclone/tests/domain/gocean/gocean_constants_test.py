# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Tests for GOceanConstants API-specific constants.'''

from psyclone.configuration import Config
from psyclone.domain.gocean import GOceanConstants


def test_gocean_const(monkeypatch):
    '''Tests the GOcean constant object.
    '''
    # This guarantees that the first time we use the constant object,
    # we read it from the config file.
    monkeypatch.setattr(GOceanConstants, "HAS_BEEN_INITIALISED", False)
    config = Config.get()

    gocean_const = config.api_conf("gocean").get_constants()
    assert gocean_const.VALID_ARG_TYPE_NAMES == []
    assert gocean_const.VALID_SCALAR_NAMES == ["rscalar", "iscalar"]

    assert GOceanConstants.HAS_BEEN_INITIALISED
    # Test that we don't re-evalue the constants, i.e. if
    # we modify them, the modified value will not be overwritten.
    GOceanConstants.VALID_INTRINSIC_TYPES = "INVALID"
    gocean_const = GOceanConstants()
    assert gocean_const.VALID_INTRINSIC_TYPES == "INVALID"
    assert gocean_const.VALID_ARG_TYPE_NAMES == []
    assert gocean_const.VALID_SCALAR_NAMES == ["rscalar", "iscalar"]
