# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2018, Science and Technology Facilities Council
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
# Author: A. R. Porter, STFC Daresbury Lab

'''
Module containing tests relating to PSyclone configuration handling.
'''

import os
import pytest
from psyclone.configuration import ConfigurationError, Config

# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files")
TEST_CONFIG = os.path.join(BASE_PATH, "dummy_config.cfg")

_CONFIG_CONTENT = '''\
[DEFAULT]
SUPPORTEDAPIS = gunghoproto, dynamo0.1, dynamo0.3, gocean0.1, gocean1.0
DEFAULTAPI = dynamo0.3
SUPPORTEDSTUBAPIS = dynamo0.3
DEFAULTSTUBAPI = dynamo0.3
DISTRIBUTED_MEMORY = true
REPRODUCIBLE_REDUCTIONS = false
REPROD_PAD_SIZE = 8
'''

def test_factory_create():
    '''
    Check that we can create a Config object
    '''
    from psyclone.configuration import ConfigFactory
    _config = ConfigFactory().create()
    assert isinstance(_config, Config)
    # Check that we are creating a singleton instance
    _config2 = ConfigFactory().create()
    assert _config is _config2
    # Check that specifying which config file to use results
    # in a new instance
    _config2 = ConfigFactory(config_file=TEST_CONFIG).create()
    assert _config2 is not _config


def test_missing_file(tmpdir):
    ''' Check that we get the expected error when the specified
    config file cannot be found '''
    with pytest.raises(ConfigurationError) as err:
        _ = Config(config_file=os.path.join(str(tmpdir),
                                            "not_a_file.cfg")).create()
    assert "not_a_file.cfg does not exist" in str(err)


def test_read_values():
    '''
    Check that we get the expected values from the test config file
    '''
    from psyclone.configuration import ConfigFactory
    _config = ConfigFactory(config_file=TEST_CONFIG).create()
    # Whether distributed memory is enabled
    dist_mem = _config.distributed_memory
    assert isinstance(dist_mem, bool)
    assert dist_mem == True
    # Check the setter method
    _config.distributed_memory = False
    assert _config.distributed_memory == False
    # The default API
    api = _config.default_api
    assert isinstance(api, unicode)
    assert api == "dynamo0.3"
    # The list of supported APIs
    api_list = _config.supported_apis
    assert api_list == ['gunghoproto', 'dynamo0.1', 'dynamo0.3',
                        'gocean0.1', 'gocean1.0']
    # The default API for kernel stub generation
    api = _config.default_stub_api
    assert isinstance(api, unicode)
    assert api == "dynamo0.3"
    # The list of supported APIs for kernel stub generation
    api_list = _config.supported_stub_apis
    assert api_list == ['dynamo0.3']
    # Whether reproducible reductions are enabled
    reprod = _config.reproducible_reductions
    assert isinstance(reprod, bool)
    assert reprod == False
    # How much to pad arrays by when doing reproducible reductions
    pad = _config.reprod_pad_size
    assert isinstance(pad, int)
    assert pad == 8
    # The filename of the config file which was parsed to produce
    # the Config object
    assert _config.filename == str(TEST_CONFIG)


def test_list_no_commas():
    ''' Check that we parse a space-delimited list OK. '''
    import re
    import tempfile
    # Remove the commas from the list of supported APIs
    content = re.sub(r"^SUPPORTEDAPIS = .*$",
                     "SUPPORTEDAPIS = dynamo0.3  gocean1.0",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)
    with tempfile.NamedTemporaryFile(delete=False) as new_cfg:
        new_name = new_cfg.name
        new_cfg.write(content)
        new_cfg.close()

        _config = Config(config_file=new_name)
        assert _config.supported_apis == ["dynamo0.3", "gocean1.0"]


def test_default_api_not_in_list():
    ''' Check that we raise an error if the default API is not in
    the list of supported APIs '''
    import re
    import tempfile
    content = re.sub(r"^SUPPORTEDAPIS = .*$",
                     "SUPPORTEDAPIS = gocean1.0",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)
    with tempfile.NamedTemporaryFile(delete=False) as new_cfg:
        new_name = new_cfg.name
        new_cfg.write(content)
        new_cfg.close()

        with pytest.raises(ConfigurationError) as err:
            _ = Config(config_file=new_name)

        assert ("The default API (dynamo0.3) is not in the list of "
                "supported APIs" in str(err))


def test_default_stubapi_not_in_list():
    ''' Check that we raise an error if the default stub API is not in
    the list of supported stub APIs '''
    import re
    import tempfile
    content = re.sub(r"^SUPPORTEDSTUBAPIS = .*$",
                     "SUPPORTEDSTUBAPIS = gocean1.0",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)
    with tempfile.NamedTemporaryFile(delete=False) as new_cfg:
        new_name = new_cfg.name
        new_cfg.write(content)
        new_cfg.close()

        with pytest.raises(ConfigurationError) as err:
            _ = Config(config_file=new_name)

        assert ("The default stub API (dynamo0.3) is not in the list of "
                "supported stub APIs" in str(err))
