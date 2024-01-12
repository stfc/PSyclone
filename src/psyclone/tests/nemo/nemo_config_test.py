# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2024, Science and Technology Facilities Council
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
# Authors: J. Henrichs, Bureau of Meteorology
#          A. R. Porter, STFC Daresbury Laboratory

''' Module containing tests for nemo specific config files.'''

import pytest

from psyclone.configuration import Config, ConfigurationError


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use the nemo API, and that we clean
    up the config file at the end of the tests.'''

    Config.get().api = "nemo"
    yield
    # At the end of all tests make sure that we wipe the Config object
    # so we get a fresh/default one for any further test (and not a
    # left-over one from a test here).
    Config._instance = None


# =============================================================================
def test_invalid_nemo_config_files(tmpdir):
    ''' Test various error conditions.
    '''

    # Valid configuration file without nemo-specific settings.
    # We add several lines in the tests for various error conditions
    # pylint: disable=invalid-name
    _CONFIG_CONTENT = '''\
    [DEFAULT]
    DEFAULTAPI = nemo
    DEFAULTSTUBAPI = dynamo0.3
    DISTRIBUTED_MEMORY = true
    REPRODUCIBLE_REDUCTIONS = false
    REPROD_PAD_SIZE = 8
    [nemo]
    '''
    # Create a config files with a nemo section and a mapping-lon
    # but leave out each required key
    for (key, data) in [("var", "start: 1, stop: jpi"),
                        ("start", "var: ji, stop: jpi"),
                        ("stop", "var: ji, start: 1")]:
        content = _CONFIG_CONTENT + "mapping-lon = " + data
        Config._instance = None
        config_file = tmpdir.join("config1")
        with config_file.open(mode="w") as new_cfg:
            new_cfg.write(content)
            new_cfg.close()

            config = Config()
            with pytest.raises(ConfigurationError) as err:
                config.load(str(config_file))
            assert f"does not contain key '{key}" in str(err.value)

    # Add an invalid index-order
    content = _CONFIG_CONTENT + \
        '''mapping-lon = var: i, start: 1, stop:2
    mapping-lat = var: j, start: 1, stop:2
    index-order = invalid'''
    config_file = tmpdir.join("config2")
    with config_file.open(mode="w") as new_cfg:
        new_cfg.write(content)
        new_cfg.close()

        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(str(config_file))
        assert "Invalid loop type 'invalid' found " in str(err.value)
        assert "Must be one of ['lon', 'lat']" in str(err.value)

    # Add an invalid key:
    content = _CONFIG_CONTENT + "invalid-key=value"
    config_file = tmpdir.join("config3")
    with config_file.open(mode="w") as new_cfg:
        new_cfg.write(content)
        new_cfg.close()

        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(str(config_file))
        assert (f"Invalid key 'invalid-key' found in the 'nemo' section of "
                f"the configuration file '{config_file}'." in str(err.value))

    # Use a variable name more than once:
    content = _CONFIG_CONTENT + \
        '''mapping-lon = var: i, start: 1, stop:2
    mapping-lat = var: i, start: 1, stop:2
    index-order = lon, lat'''
    config_file = tmpdir.join("config4")
    with config_file.open(mode="w") as new_cfg:
        new_cfg.write(content)
        new_cfg.close()

        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(str(config_file))
        assert (f"mapping-lat defines variable 'i' again in the 'nemo' "
                f"section of the file '{config_file}'." in str(err.value))

# =============================================================================
