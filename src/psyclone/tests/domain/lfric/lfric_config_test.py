# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council
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
# Modified: R. W. Ford, STFC Daresbury Lab

'''
Module containing tests for LFRic (Dynamo0.3) API configuration handling.
'''

from __future__ import absolute_import

import re
import pytest

from psyclone.configuration import Config, ConfigurationError


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
default_kind = real: r_def, integer: i_def, logical: l_def
RUN_TIME_CHECKS = true
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


@pytest.mark.parametrize("option", ["access_mapping", "COMPUTE_ANNEXED_DOFS",
                                    "default_kind", "RUN_TIME_CHECKS"])
def test_no_mandatory_option(tmpdir, option):
    ''' Check that we raise an error if we do not provide mandatory
    configuration options for LFRic (Dynamo0.3) API '''

    content = re.sub(r"^{0} = .*$".format(option), "",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)
    config_file = tmpdir.join("config_dyn")
    with config_file.open(mode="w") as new_cfg:
        new_cfg.write(content)
        new_cfg.close()
        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(config_file=str(config_file))

        assert ("Missing mandatory configuration option in the "
                "\'[dynamo0.3]\' section " in str(err.value))
        assert ("Valid options are: '['access_mapping', "
                "'compute_annexed_dofs', 'default_kind', "
                "'run_time_checks']" in str(err.value))


@pytest.mark.parametrize("option", ["COMPUTE_ANNEXED_DOFS", "RUN_TIME_CHECKS"])
def test_entry_not_bool(tmpdir, option):
    ''' Check that we raise an error if the value of any options expecting
    a boolean value are not Boolean '''
    content = re.sub(r"^{0} = .*$".format(option),
                     "{0} = tree".format(option),
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)
    config_file = tmpdir.join("config_dyn")
    with config_file.open(mode="w") as new_cfg:
        new_cfg.write(content)
        new_cfg.close()
        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(config_file=str(config_file))

        assert "error while parsing {0}".format(option) in str(err.value)
        assert "Not a boolean: tree" in str(err.value)


def test_invalid_default_kind(tmpdir):
    ''' Check that we raise an error if we supply an invalid datatype or
    kind (precision) in the configuration file '''

    # Test invalid datatype
    content = re.sub(r"real:", "reality:",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)
    config_file = tmpdir.join("config_dyn")
    with config_file.open(mode="w") as new_cfg:
        new_cfg.write(content)
        new_cfg.close()
        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(config_file=str(config_file))

        assert ("Invalid datatype found in the \'[dynamo0.3]\' section "
                in str(err.value))
        assert ("Valid datatypes are: '['real', 'integer', 'logical']'"
                in str(err.value))

    # Test invalid kind (precision)
    content = re.sub("integer: i_def,", "integer: ,", _CONFIG_CONTENT)
    config_file = tmpdir.join("config_dyn")
    with config_file.open(mode="w") as new_cfg:
        new_cfg.write(content)
        new_cfg.close()
        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(config_file=str(config_file))

        assert ("Supplied kind parameters \'[\'l_def\', \'r_def\']\' in "
                "the \'[dynamo0.3]\' section" in str(err.value))
        assert ("do not define the default kind for one or more supported "
                "datatypes \'[\'real\', \'integer\', \'logical\']\'."
                in str(err.value))


def test_access_mapping():
    '''Check that we load the expected default access mapping values'''
    from psyclone.core.access_type import AccessType
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
    assert api_config.compute_annexed_dofs == False


def test_default_kind():
    ''' Check that we load correct default kinds (precisions) for all
    datatypes. This test will be modified to test whether the default
    kinds are in a list of allowed kinds for each datatype when the
    functionality is introduced. '''
    api_config = Config().get().api_conf(TEST_API)
    assert api_config.default_kind["real"] == "r_def"
    assert api_config.default_kind["integer"] == "i_def"
    assert api_config.default_kind["logical"] == "l_def"


def test_run_time_checks():
    '''Check that we load the expected default RUN_TIME_CHECKS value'''
    api_config = Config().get().api_conf(TEST_API)
    assert api_config.run_time_checks == True


