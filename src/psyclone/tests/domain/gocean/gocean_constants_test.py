# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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
# ----------------------------------------------------------------------------
# Author: J. Henrichs, Bureau of Meteorology
# Modified: I. Kavcic, Met Office
# Modified: R. W. Ford, STFC Daresbury Lab

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

    gocean_const = config.api_conf("gocean1.0").get_constants()
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
