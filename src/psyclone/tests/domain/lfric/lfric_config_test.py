# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council
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
# Author I. Kavcic, Met Office
# Modified: R. W. Ford and N. Nobre, STFC Daresbury Lab
# Modified: O. Brunt, Met Office

'''
Module containing tests for LFRic (Dynamo0.3) API configuration handling.
'''

from __future__ import absolute_import

import re
import pytest

from psyclone.configuration import Config, ConfigurationError
from psyclone.core.access_type import AccessType


# Constants
TEST_API = "dynamo0.3"

# Valid configuration file content for testing purposes
_CONFIG_CONTENT = '''\
[DEFAULT]
API = dynamo0.3
DEFAULTSTUBAPI = dynamo0.3
DISTRIBUTED_MEMORY = true
REPRODUCIBLE_REDUCTIONS = false
REPROD_PAD_SIZE = 8
[dynamo0.3]
access_mapping = gh_read: read, gh_write: write, gh_readwrite: readwrite,
                 gh_inc: inc, gh_sum: sum
COMPUTE_ANNEXED_DOFS = false
supported_fortran_datatypes = real, integer, logical
default_kind = real: r_def, integer: i_def, logical: l_def
precision_map = i_def: 4,
                l_def: 1,
                r_def: 8,
                r_double: 8,
                r_ncdf: 8,
                r_quad: 16,
                r_second: 8,
                r_single: 4,
                r_solver: 4,
                r_tran: 8,
                r_bl: 8,
                r_phys: 8,
                r_um: 8
RUN_TIME_CHECKS = false
NUM_ANY_SPACE = 10
NUM_ANY_DISCONTINUOUS_SPACE = 10
'''


@pytest.fixture(scope="function", autouse=True)
def clear_config_instance():
    ''' The tests in this module all assume that there is no pre-existing
    Config object, so this fixture ensures that the config instance is
    deleted before and after each test function. The latter makes sure that
    any other test executed next will automatically reload the default
    config file.
    '''

    # Enforce loading of the default config file
    Config._instance = None

    # Now execute all tests
    yield

    # Enforce loading of the default config file
    Config._instance = None


def config(config_file, content):
    ''' A utility function that creates and populates a temporary
    PSyclone configuration file for testing purposes.

    :param config_file: local path to the temporary configuration file.
    :type config: :py:class:`py._path.local.LocalPath`
    :param str content: the entry for the temporary configuration file.

    :returns: a test Config instance.
    :rtype: :py:class:`psyclone.configuration.Config`

    '''
    # Create and populate a temporary config file
    with config_file.open(mode="w") as new_cfg:
        new_cfg.write(content)
        new_cfg.close()
    # Create and populate a test Config object
    config_obj = Config()
    config_obj.load(config_file=str(config_file))
    return config_obj


@pytest.mark.parametrize(
    "option", ["access_mapping", "COMPUTE_ANNEXED_DOFS",
               "supported_fortran_datatypes", "default_kind",
               "precision_map", "RUN_TIME_CHECKS",
               "NUM_ANY_SPACE", "NUM_ANY_DISCONTINUOUS_SPACE"])
def test_no_mandatory_option(tmpdir, option):
    ''' Check that we raise an error if we do not provide mandatory
    configuration options for LFRic (Dynamo0.3) API '''
    config_file = tmpdir.join("config_dyn")

    content = re.sub(f"^{option} = .*$", "",
                     _CONFIG_CONTENT, flags=re.MULTILINE)

    with pytest.raises(ConfigurationError) as err:
        config(config_file, content)
    assert ("Missing mandatory configuration option in the "
            "\'[dynamo0.3]\' section " in str(err.value))
    assert ("Valid options are: ['access_mapping', "
            "'compute_annexed_dofs', 'supported_fortran_datatypes', "
            "'default_kind', 'precision_map', 'run_time_checks', "
            "'num_any_space', 'num_any_discontinuous_space']."
            in str(err.value))


@pytest.mark.parametrize("option", ["COMPUTE_ANNEXED_DOFS", "RUN_TIME_CHECKS"])
def test_entry_not_bool(tmpdir, option):
    ''' Check that we raise an error if the value of any options expecting
    a boolean value are not Boolean '''
    config_file = tmpdir.join("config_dyn")
    content = re.sub(f"^{option} = .*$", f"{option} = tree",
                     _CONFIG_CONTENT, flags=re.MULTILINE)

    with pytest.raises(ConfigurationError) as err:
        config(config_file, content)

    assert f"Error while parsing {option}" in str(err.value)
    assert "Not a boolean: tree" in str(err.value)


@pytest.mark.parametrize("option", ["NUM_ANY_SPACE",
                                    "NUM_ANY_DISCONTINUOUS_SPACE"])
def test_entry_not_int(tmpdir, option):
    ''' Check that we raise an error if the value of any options expecting
    an integer value is not int() with base 10. '''
    config_file = tmpdir.join("config_dyn")
    content = re.sub(f"^{option} = .*$", f"{option} = false",
                     _CONFIG_CONTENT, flags=re.MULTILINE)

    with pytest.raises(ConfigurationError) as err:
        config(config_file, content)

    assert f"Error while parsing {option}" in str(err.value)
    assert ("invalid literal for int() with base 10: 'false'"
            in str(err.value))


def test_invalid_default_kind(tmpdir):
    ''' Check that we raise an error if we supply an invalid datatype or kind
    (precision) in the 'default_kind' list in the '[dynamo0.3]' section of
    the configuration file.

    '''
    config_file = tmpdir.join("config_dyn")

    # Test invalid datatype
    content = re.sub(r"real:", "reality:",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)

    with pytest.raises(ConfigurationError) as err:
        config(config_file, content)

    test_str = str(err.value)
    assert ("Fortran datatypes in the 'default_kind' mapping in the "
            "\'[dynamo0.3]\' section " in test_str)
    assert ("do not match the supported Fortran datatypes [\'real\', "
            "\'integer\', \'logical\']." in test_str)

    # Test invalid kind (precision)
    content = re.sub("integer: i_def,", "integer: ,", _CONFIG_CONTENT)

    with pytest.raises(ConfigurationError) as err:
        config(config_file, content)

    test_str = str(err.value)
    assert ("Supplied kind parameters [\'l_def\', \'r_def\'] in "
            "the \'[dynamo0.3]\' section" in test_str)
    assert ("do not define the default kind for one or more supported "
            "datatypes [\'real\', \'integer\', \'logical\']." in test_str)


def test_invalid_precision_map(tmpdir):
    '''Check that we raise an error if the precision map values include
    a special character or letter/are not a string representation of an
    integer.

    '''
    config_file = tmpdir.join("config_dyn")

    # Test invalid datatype: special character
    content = re.sub(r"r_double: 8,", "r_double: -8,",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)

    with pytest.raises(ConfigurationError) as err:
        config(config_file, content)

    assert ("Wrong type supplied to mapping: '-8' is not a positive"
            " integer or contains special characters." in str(err.value))

    # Test invalid datatype: letter string
    content = re.sub(r"r_double: 8,", "r_double: number 5,",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)

    with pytest.raises(ConfigurationError) as err:
        config(config_file, content)

    assert ("Wrong type supplied to mapping: 'number 5' is not a positive"
            " integer or contains special characters." in str(err.value))

    # Test invalid datatype: float
    content = re.sub(r"r_double: 8,", "r_double: 8.5,",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)

    with pytest.raises(ConfigurationError) as err:
        config(config_file, content)

    assert ("Wrong type supplied to mapping: '8.5' is not a positive"
            " integer or contains special characters." in str(err.value))

    # Test invalid datatype: unicode ('\u00b2' = superscript 2)
    content = re.sub(r"r_double: 8,", "r_double: \u00b2,",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)

    with pytest.raises(ConfigurationError) as err:
        config(config_file, content)

    assert ("Wrong type supplied to mapping: '\u00b2' is not a positive"
            " integer or contains special characters." in str(err.value))


def test_invalid_num_any_anyd_spaces(tmpdir):
    ''' Check that we raise an error if we supply an invalid number
    (less than or equal to 0) of ANY_SPACE and ANY_DISCONTINUOUS_SPACE
    function spaces in the configuration file.

    '''
    config_file = tmpdir.join("config_dyn")

    # Test invalid NUM_ANY_SPACE
    content = re.sub(r"NUM_ANY_SPACE = 10", "NUM_ANY_SPACE = 0",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)

    with pytest.raises(ConfigurationError) as err:
        config(config_file, content)

    assert ("The supplied number of ANY_SPACE function spaces in "
            "the \'[dynamo0.3]\' section " in str(err.value))
    assert ("must be greater than 0 but found 0."
            in str(err.value))

    # Test invalid NUM_ANY_DISCONTINUOUS_SPACE
    content = re.sub(r"NUM_ANY_DISCONTINUOUS_SPACE = 10",
                     "NUM_ANY_DISCONTINUOUS_SPACE = -10",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)

    with pytest.raises(ConfigurationError) as err:
        config(config_file, content)

    assert ("The supplied number of ANY_DISCONTINUOUS_SPACE function "
            "spaces in the \'[dynamo0.3]\' section " in str(err.value))
    assert ("must be greater than 0 but found -10."
            in str(err.value))


def test_access_mapping():
    '''Check that we load the expected default access mapping values'''
    api_config = Config().get().api_conf(TEST_API)
    assert api_config.get_access_mapping()["gh_read"] == AccessType.READ
    assert api_config.get_access_mapping()["gh_write"] == AccessType.WRITE
    assert (api_config.get_access_mapping()["gh_readwrite"] ==
            AccessType.READWRITE)
    assert api_config.get_access_mapping()["gh_inc"] == AccessType.INC
    assert api_config.get_access_mapping()["gh_sum"] == AccessType.SUM


def test_compute_annexed_dofs():
    '''Check that we load the expected default COMPUTE_ANNEXED_DOFS
    value

    '''
    api_config = Config().get().api_conf(TEST_API)
    assert not api_config.compute_annexed_dofs


def test_default_kind():
    '''Check that we load correct default kinds (precisions) for all
    datatypes. This test will be modified to test whether the default
    kinds are in a list of allowed kinds for each datatype when the
    functionality is introduced.

    '''
    api_config = Config().get().api_conf(TEST_API)
    assert api_config.default_kind["real"] == "r_def"
    assert api_config.default_kind["integer"] == "i_def"
    assert api_config.default_kind["logical"] == "l_def"


def test_precision_map():
    '''Check that we load correct precision values for all
    datatypes.

    '''
    api_config = Config().get().api_conf(TEST_API)
    assert api_config.precision_map["i_def"] == 4
    assert api_config.precision_map["l_def"] == 1
    assert api_config.precision_map["r_def"] == 8
    assert api_config.precision_map["r_double"] == 8
    assert api_config.precision_map["r_ncdf"] == 8
    assert api_config.precision_map["r_quad"] == 16
    assert api_config.precision_map["r_second"] == 8
    assert api_config.precision_map["r_single"] == 4
    assert api_config.precision_map["r_solver"] == 4
    assert api_config.precision_map["r_tran"] == 8
    assert api_config.precision_map["r_bl"] == 8
    assert api_config.precision_map["r_phys"] == 8
    assert api_config.precision_map["r_um"] == 8


def test_run_time_checks():
    '''Check that we load the expected default RUN_TIME_CHECKS value
    (False)

    '''
    api_config = Config().get().api_conf(TEST_API)
    assert not api_config.run_time_checks


def test_num_any_space():
    ''' Check that we load the expected default ANY_SPACE value (10).

    '''
    api_config = Config().get().api_conf(TEST_API)
    assert api_config.num_any_space == 10


def test_num_any_discontinuous_space():
    ''' Check that we load the expected default ANY_DISCONTINUOUS_SPACE
    value (10).

    '''
    api_config = Config().get().api_conf(TEST_API)
    assert api_config.num_any_discontinuous_space == 10
