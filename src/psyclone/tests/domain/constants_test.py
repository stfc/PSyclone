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
# ----------------------------------------------------------------------------
# Author: J. Henrichs, Bureau of Meteorology
# Modified: I. Kavcic, Met Office
# Modified: R. W. Ford, STFC Daresbury Lab


'''Tests for class storing API-specific constants.'''

from __future__ import absolute_import, print_function

from psyclone.configuration import Config
from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.nemo import NemoConstants


def test_lfric_const():
    '''Tests the LFRic constant object.
    '''
    # This guarantees that the first time we use the constant object,
    # we read it from the config file.
    LFRicConstants.HAS_BEEN_INITIALISED = False
    config = Config.get()

    lfric_const = config.api_conf("dynamo0.3").get_constants()
    # Don't test intrinsic_types, which comes from the config file
    assert lfric_const.VALID_ARG_TYPE_NAMES == ["gh_field", "gh_operator",
                                                "gh_columnwise_operator",
                                                "gh_scalar"]

    assert lfric_const.VALID_SCALAR_NAMES == ["gh_scalar"]

    assert LFRicConstants.HAS_BEEN_INITIALISED
    # Test that we don't initialise the object again, i.e. that a
    # modified value is not changed.
    LFRicConstants.VALID_INTRINSIC_TYPES = "INVALID"
    lfric_const = LFRicConstants()
    assert lfric_const.VALID_INTRINSIC_TYPES == "INVALID"
    assert lfric_const.VALID_ARG_TYPE_NAMES == ["gh_field", "gh_operator",
                                                "gh_columnwise_operator",
                                                "gh_scalar"]
    assert lfric_const.VALID_SCALAR_NAMES == ["gh_scalar"]
    assert lfric_const.VALID_ARG_DATA_TYPES == ["gh_real", "gh_integer",
                                                "gh_logical"]
    assert lfric_const.DATA_TYPE_MAP["operator"]["kind"] == "r_def"
    assert lfric_const.DATA_TYPE_MAP["integer_field"]["kind"] == "i_def"
    assert lfric_const.DATA_TYPE_MAP["r_solver_field"]["kind"] == "r_solver"
    # Make sure the 'INVALID' value is reset when the constant
    # object is created again.
    LFRicConstants.HAS_BEEN_INITIALISED = False


def test_nemo_const():
    '''Tests the Nemo constant object.
    '''

    # This guarantees that the first time we use the constant object,
    # we read it from the config file.
    NemoConstants.HAS_BEEN_INITIALISED = False
    nemo_config = Config.get().api_conf("nemo")

    nemo_const = nemo_config.get_constants()
    assert nemo_const.VALID_INTRINSIC_TYPES == []
    assert nemo_const.VALID_ARG_TYPE_NAMES == []
    assert nemo_const.VALID_SCALAR_NAMES == ["rscalar", "iscalar"]

    assert NemoConstants.HAS_BEEN_INITIALISED
    # Test that we don't re-evalue the constants, i.e. if
    # we modify them, the modified value will not be overwritten.
    NemoConstants.VALID_INTRINSIC_TYPES = "INVALID"
    nemo_const = NemoConstants()
    assert nemo_const.VALID_INTRINSIC_TYPES == "INVALID"
    assert nemo_const.VALID_ARG_TYPE_NAMES == []
    assert nemo_const.VALID_SCALAR_NAMES == ["rscalar", "iscalar"]

    # Make sure the loop types are transferred correctly from the config
    # file to the constant object.
    assert nemo_const.VALID_LOOP_TYPES == nemo_config.get_valid_loop_types()

    # Make sure the 'INVALID' value is reset when the constant
    # object is created again.
    NemoConstants.HAS_BEEN_INITIALISED = False
