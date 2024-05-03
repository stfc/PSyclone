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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Laboratory.
# Modified by J. Henrichs, Bureau of Meteorology

'''
Module containing tests for the LFRic (Dynamo0.3) constants class.
'''

import pytest

from psyclone.configuration import Config
from psyclone.domain.lfric import LFRicConstants, LFRicTypes
from psyclone.errors import InternalError
from psyclone import generator


def test_config_loaded_before_constants_created(monkeypatch):
    '''This tests that we get an error message if the config object
    is not loaded when creating an LFRicConst instance. '''
    monkeypatch.setattr(Config, "_HAS_CONFIG_BEEN_INITIALISED", False)
    monkeypatch.setattr(LFRicConstants, "HAS_BEEN_INITIALISED", False)

    with pytest.raises(InternalError) as err:
        LFRicConstants()
    assert ("LFRicConstants is being created before the config file is loaded"
            in str(err.value))

    # If the psyclone command is executed, the flag should be set. The
    # parameters specified here will immediately abort, but still the
    # flag must be set at the end, since the command has to set this flag:
    with pytest.raises(SystemExit) as err:
        generator.main(["some_file.f90"])
    assert Config.has_config_been_initialised() is True


def test_specific_function_space():
    ''' Check that the lookup of a specific function space for a valid
    wildcard name works as expected.

    '''
    name = LFRicConstants().specific_function_space("ANY_W2")
    assert name == "w2"
    name = LFRicConstants().specific_function_space("ANY_space_3")
    assert name == "w0"
    name = LFRicConstants().specific_function_space(
        "ANY_disCONTINUOUS_space_3")
    assert name == "w3"
    name = LFRicConstants().specific_function_space("wtheta")
    assert name == "wtheta"


def test_specific_function_space_invalid():
    ''' Check that the specific_function_space() method rejects an invalid
    function-space name. '''
    with pytest.raises(ValueError) as err:
        LFRicConstants().specific_function_space("wrong")
    assert ("'wrong' is not a recognised LFRic function space (one of"
            in str(err.value))


def test_specific_function_space_internal_error(monkeypatch):
    ''' Check that the lookup of a specific function space raises the expected
    internal error if an unhandled case is found.
    '''
    const = LFRicConstants()
    # We have to monkeypatch the list of valid FS names to get to the bit
    # of code we want to test.
    monkeypatch.setattr(LFRicConstants,
                        "VALID_FUNCTION_SPACE_NAMES", ["any_wrong"])
    with pytest.raises(InternalError) as err:
        const.specific_function_space("any_wrong")
    assert ("Error mapping from meta-data function space to actual space: "
            "cannot handle 'any_wrong'" in str(err.value))


def test_precision_for_type():
    '''Check the precision_for_type() method.'''
    const = LFRicConstants()
    for module_info in const.DATA_TYPE_MAP.values():
        assert (const.precision_for_type(module_info["type"])
                == LFRicTypes(module_info["kind"].upper()))


def test_precision_for_type_error():
    '''Tests that exceptions are raised as expected from
    precision_for_type().
    '''
    with pytest.raises(InternalError) as err:
        LFRicConstants().precision_for_type("invalid")
    assert "Unknown data type 'invalid', expected one of" in str(err.value)


def test_quadrature_type_map():
    '''Check that QUADRATURE_TYPE_MAP contains the expected structure.'''

    quadrature_types = ["gh_quadrature_xyoz", "gh_quadrature_face",
                        "gh_quadrature_edge"]
    quadrature_properties = ["module", "type", "proxy_type", "intrinsic",
                             "kind"]

    assert len(LFRicConstants.QUADRATURE_TYPE_MAP) == len(quadrature_types)
    for quadrature_type in quadrature_types:
        assert quadrature_type in LFRicConstants.QUADRATURE_TYPE_MAP
        info = LFRicConstants.QUADRATURE_TYPE_MAP[quadrature_type]
        assert len(info) == len(quadrature_properties)
        for item in info:
            assert item in quadrature_properties
