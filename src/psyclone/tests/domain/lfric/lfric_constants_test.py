# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------


'''
Module containing tests for the LFRic constants class.
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
    # (We check for two different exceptions as this behaviour seems to
    # change between Python 3.9 and more recent versions.)
    with pytest.raises((FileNotFoundError, SystemExit)) as err:
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
        if module_info["type"] != "scalar_type":
            assert (const.precision_for_type(module_info["type"])
                    == LFRicTypes(module_info["kind"].upper()))


def test_precision_for_type_error():
    '''Tests that exceptions are raised as expected from
    precision_for_type().
    '''
    with pytest.raises(ValueError) as err:
        LFRicConstants().precision_for_type("scalar_type")
    assert "Cannot infer the precision of a 'scalar_type'." in str(err.value)
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
