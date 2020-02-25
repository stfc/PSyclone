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

'''
Module containing tests for LFRic (Dynamo0.3) API configuration handling.
'''

from __future__ import absolute_import

import re
import pytest

from psyclone.configuration import Config, ConfigurationError


TEST_API = "dynamo0.3"


# Valid configuration file content with only defaults and
# annexed dofs for testing purposes
_CONFIG_CONTENT = '''\
[DEFAULT]
API = dynamo0.3
DEFAULTSTUBAPI = dynamo0.3
DISTRIBUTED_MEMORY = true
REPRODUCIBLE_REDUCTIONS = false
REPROD_PAD_SIZE = 8
[dynamo0.3]
COMPUTE_ANNEXED_DOFS = false
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


def test_anx_dof_not_bool(tmpdir):
    ''' Check that we raise an error if the COMPUTE_ANNEXED_DOFS
    setting is not a Boolean '''
    content = re.sub(r"^COMPUTE_ANNEXED_DOFS = .*$",
                     "COMPUTE_ANNEXED_DOFS = tree",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)
    config_file = tmpdir.join("config_dyn")
    with config_file.open(mode="w") as new_cfg:
        new_cfg.write(content)
        new_cfg.close()
        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(config_file=str(config_file))

        assert "error while parsing COMPUTE_ANNEXED_DOFS" in str(err.value)
        assert "Not a boolean: tree" in str(err.value)


def test_invalid_default_kind(tmpdir):
    ''' Check that we raise an error if we supply an invalid
    datatype or kind (precision) in the configuration file '''

    # Test invalid datatype
    content = _CONFIG_CONTENT + \
        "default_kind = reality: r_def, integer: i_def, logical: l_def"
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
    content = _CONFIG_CONTENT + \
        "default_kind = real: r_def, integer: , logical: l_def"
    config_file = tmpdir.join("config_dyn")
    with config_file.open(mode="w") as new_cfg:
        new_cfg.write(content)
        new_cfg.close()
        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(config_file=str(config_file))

        assert ("Did not find default kind for one or more datatypes "
                "in the \'[dynamo0.3]\' section " in str(err.value))


def test_default_kind():
    ''' Check that we load correct default kinds (precisions) for all
    datatypes. This test will be modified to test whether the default
    kinds are in a list of allowed kinds for each datatype when the
    functionality is introduced. '''
    config = Config()
    api_config = config.get().api_conf(TEST_API)
    assert api_config.default_kind["real"] == "r_def"
    assert api_config.default_kind["integer"] == "i_def"
    assert api_config.default_kind["logical"] == "l_def"
