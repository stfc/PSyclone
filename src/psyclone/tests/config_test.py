# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018, Science and Technology Facilities Council.
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
import re
import tempfile
import six
import pytest
from psyclone.configuration import ConfigurationError, Config

# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files")
TEST_CONFIG = os.path.join(BASE_PATH, "dummy_config.cfg")

# Valid configuration file content that we will manipulate for
# different tests
_CONFIG_CONTENT = '''\
[DEFAULT]
SUPPORTEDAPIS = gunghoproto, dynamo0.1, dynamo0.3, gocean0.1, gocean1.0
DEFAULTAPI = dynamo0.3
SUPPORTEDSTUBAPIS = dynamo0.3
DEFAULTSTUBAPI = dynamo0.3
DISTRIBUTED_MEMORY = true
REPRODUCIBLE_REDUCTIONS = false
REPROD_PAD_SIZE = 8
[dynamo0.3]
COMPUTE_ANNEXED_DOFS = false
'''

@pytest.fixture(scope="module",
                params=["DISTRIBUTED_MEMORY",
                        "REPRODUCIBLE_REDUCTIONS",
                        "COMPUTE_ANNEXED_DOFS"])
def bool_entry(request):
    '''
    Parameterised fixture that will cause a test that has it as an
    argument to be run for each boolean member of the configuration file
    :param request: Object through which to access current parameter value
    :return: Name of element of config file
    :rtype: str
    '''
    return request.param


@pytest.fixture(scope="module",
                params=["REPROD_PAD_SIZE"])
def int_entry(request):
    '''
    Parameterised fixture that returns the names of integer members of the
    configuration file.
    :param request: Object through which to access current parameter value
    :return: Name of element of config file
    :rtype: str
    '''
    return request.param


def test_factory_create():
    '''
    Check that we can create a Config object
    '''
    from psyclone.configuration import ConfigFactory
    try:
        _config = ConfigFactory().create()
        assert isinstance(_config, Config)
        # Check that we are creating a singleton instance
        _config2 = ConfigFactory().create()
        assert _config is _config2
        # Check that specifying which config file to use results
        # in a new instance
        _config2 = ConfigFactory(config_file=TEST_CONFIG).create()
        assert _config2 is not _config
    finally:
        # Reset the factory
        ConfigFactory._instance = None


def test_missing_file(tmpdir):
    ''' Check that we get the expected error when the specified
    config file cannot be found '''
    with pytest.raises(ConfigurationError) as err:
        _ = Config(config_file=os.path.join(str(tmpdir),
                                            "not_a_file.cfg"))
    assert "not_a_file.cfg does not exist" in str(err)


def test_search_path(monkeypatch, tmpdir):
    ''' Check that the search path for a configuration file is as
    expected '''
    import sys
    # Ensure that PSYCLONE_CONFIG is not set
    monkeypatch.delitem(os.environ, "PSYCLONE_CONFIG", raising=False)
    # We test the search path used by causing the find_file() method
    # to fail to find any file and thus raise an error. The error msg
    # then gives us the list of locations searched.
    monkeypatch.setattr("os.path.isfile", lambda arg: False)
    try:
        # Store our working directory
        oldpwd = tmpdir.chdir()
        cwd = str(tmpdir)
        # Test when (we appear to be) both inside and outside a virtual
        # environment
        for inside_venv in [True, False]:
            monkeypatch.setattr(
                "psyclone.virtual_utils.within_virtual_env",
                lambda: inside_venv)  # pylint: disable=cell-var-from-loop
            with pytest.raises(ConfigurationError) as err:
                _ = Config.find_file()
            err_msg = str(err)
            assert "not found in any of " in err_msg
            # CWD
            cwd_idx = err_msg.find(os.path.join(cwd, ".psyclone"))
            assert cwd_idx != -1
            # Home directory
            home_idx = err_msg.find(os.path.join(os.path.expanduser("~"),
                                                 ".local", "share",
                                                 "psyclone"))
            assert home_idx != -1
            # Some share directory
            share_idx = err_msg.find(os.path.join(sys.prefix, "share",
                                                  "psyclone"))
            assert share_idx != -1
            assert cwd_idx < home_idx
            if inside_venv:
                # When inside a virtual environment, the 'share' directory of
                # that environment takes precedence over the user's home
                # directory
                assert share_idx < home_idx
            else:
                assert home_idx < share_idx
    finally:
        oldpwd.chdir()


def test_search_env(monkeypatch, tmpdir):
    ''' Check that we pick up the configuration file specified in an
    environment variable '''
    try:
        oldpwd = tmpdir.chdir()
        cwd = str(tmpdir)
        # Create a .psyclone/psyclone.cfg in the CWD
        cfg_dir = os.path.join(cwd, ".psyclone")
        os.mkdir(cfg_dir)
        with open(os.path.join(cfg_dir, "psyclone.cfg"), "w") as cfile:
            cfile.write(TEST_CONFIG)
        # Point PSYCLONE_CONFIG to a non-existant file - we should revert
        # to the normal search path in this case
        cfg_file = os.path.join(cwd, "not_a_dir", "psyclone.cfg")
        monkeypatch.setitem(os.environ, "PSYCLONE_CONFIG", cfg_file)
        name = Config.find_file()
        assert name.startswith(cfg_dir)
        assert "not_a_dir" not in name
        # Now point PSYCLONE_CONFIG to a file that does exist
        cfg_file = os.path.join(cwd, "another.cfg")
        with open(cfg_file, "w") as cfile:
            cfile.write(TEST_CONFIG)
        monkeypatch.setitem(os.environ, "PSYCLONE_CONFIG", cfg_file)
        name = Config.find_file()
        assert name == cfg_file
    finally:
        oldpwd.chdir()


def test_read_values():
    '''
    Check that we get the expected values from the test config file
    '''
    from psyclone.configuration import ConfigFactory
    try:
        _config = ConfigFactory(config_file=TEST_CONFIG).create()
        # Whether distributed memory is enabled
        dist_mem = _config.distributed_memory
        assert isinstance(dist_mem, bool)
        assert dist_mem
        # The default API
        api = _config.default_api
        assert isinstance(api, six.text_type)
        assert api == "dynamo0.3"
        # The list of supported APIs
        api_list = _config.supported_apis
        assert api_list == ['gunghoproto', 'dynamo0.1', 'dynamo0.3',
                            'gocean0.1', 'gocean1.0']
        # The default API for kernel stub generation
        api = _config.default_stub_api
        assert isinstance(api, six.text_type)
        assert api == "dynamo0.3"
        # The list of supported APIs for kernel stub generation
        api_list = _config.supported_stub_apis
        assert api_list == ['dynamo0.3']
        # Whether reproducible reductions are enabled
        reprod = _config.reproducible_reductions
        assert isinstance(reprod, bool)
        assert not reprod
        # How much to pad arrays by when doing reproducible reductions
        pad = _config.reprod_pad_size
        assert isinstance(pad, int)
        assert pad == 8
        # The filename of the config file which was parsed to produce
        # the Config object
        assert _config.filename == str(TEST_CONFIG)
    finally:
        # Reset the configuration object held in the factory
        ConfigFactory._instance = None


def test_dm():
    ''' Checks for getter and setter for distributed memory '''
    _config = Config(config_file=TEST_CONFIG)
    # Check the setter method
    _config.distributed_memory = False
    assert not _config.distributed_memory
    with pytest.raises(ConfigurationError) as err:
        _config.distributed_memory = "not-a-bool"
    assert "distributed_memory must be a boolean but got " in str(err)


def test_list_no_commas():
    ''' Check that we parse a space-delimited list OK. '''
    # Remove the commas from the list of supported APIs
    content = re.sub(r"^SUPPORTEDAPIS = .*$",
                     "SUPPORTEDAPIS = dynamo0.3  gocean1.0",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)
    with tempfile.NamedTemporaryFile(delete=False, mode="w") as new_cfg:
        new_name = new_cfg.name
        new_cfg.write(content)
        new_cfg.close()

        _config = Config(config_file=new_name)
        assert _config.supported_apis == ["dynamo0.3", "gocean1.0"]


def test_default_api_not_in_list():
    ''' Check that we raise an error if the default API is not in
    the list of supported APIs '''
    content = re.sub(r"^SUPPORTEDAPIS = .*$",
                     "SUPPORTEDAPIS = gocean1.0",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)
    with tempfile.NamedTemporaryFile(delete=False, mode="w") as new_cfg:
        new_name = new_cfg.name
        new_cfg.write(content)
        new_cfg.close()

        with pytest.raises(ConfigurationError) as err:
            _ = Config(config_file=new_name)

        assert ("The default API (dynamo0.3) is not in the list of "
                "supported APIs" in str(err))


def test_default_stubapi_missing():
    ''' Check that we raise an error if the default stub API is not in
    the list of supported stub APIs '''
    content = re.sub(r"^SUPPORTEDSTUBAPIS = .*$",
                     "SUPPORTEDSTUBAPIS = gocean1.0",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)
    with tempfile.NamedTemporaryFile(delete=False, mode="w") as new_cfg:
        new_name = new_cfg.name
        new_cfg.write(content)
        new_cfg.close()

        with pytest.raises(ConfigurationError) as err:
            _ = Config(config_file=new_name)

        assert ("The default stub API (dynamo0.3) is not in the list of "
                "supported stub APIs" in str(err))


def test_not_bool(bool_entry):
    ''' Check that we catch cases where we expect a boolean in the config
    file but don't get one. '''
    content = re.sub(r"^{0} = .*$".format(bool_entry),
                     "{0} = wrong".format(bool_entry),
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)
    with tempfile.NamedTemporaryFile(delete=False, mode="w") as new_cfg:
        new_name = new_cfg.name
        new_cfg.write(content)
        new_cfg.close()

        with pytest.raises(ConfigurationError) as err:
            _ = Config(config_file=new_name)

        assert "configuration error (file=" in str(err)
        assert ": error while parsing {0}".format(bool_entry) in str(err)
        assert "Not a boolean: wrong" in str(err)


def test_not_int(int_entry):
    ''' Check that we catch cases where we expect an integer in the config
    file but don't get one. '''
    content = re.sub(r"^{0} = .*$".format(int_entry),
                     "{0} = wrong".format(int_entry),
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)
    with tempfile.NamedTemporaryFile(delete=False, mode="w") as new_cfg:
        new_name = new_cfg.name
        new_cfg.write(content)
        new_cfg.close()

        with pytest.raises(ConfigurationError) as err:
            _ = Config(config_file=new_name)

        assert "configuration error (file=" in str(err)
        assert (": error while parsing {0}: invalid literal".format(int_entry)
                in str(err))


def test_broken_fmt():
    ''' Check the error if the formatting of the configuration file is
    wrong. '''
    # Create a 'config' file without any section headers
    content = "COMPUTE_ANNEXED_DOFS = false\n"
    with tempfile.NamedTemporaryFile(delete=False, mode="w") as new_cfg:
        new_name = new_cfg.name
        new_cfg.write(content)
        new_cfg.close()

        with pytest.raises(ConfigurationError) as err:
            _ = Config(config_file=new_name)
        assert ("ConfigParser failed to read the configuration file. Is it "
                "formatted correctly? (Error was: File contains no section "
                "headers" in str(err))


def test_default_missing():
    ''' Check that we produce a suitable error if the [DEFAULT] section
    of the configuration file is missing '''
    content = '''\
[dynamo0.3]
COMPUTE_ANNEXED_DOFS = false
'''
    with tempfile.NamedTemporaryFile(delete=False, mode="w") as new_cfg:
        new_name = new_cfg.name
        new_cfg.write(content)
        new_cfg.close()

        with pytest.raises(ConfigurationError) as err:
            _ = Config(config_file=new_name)

        assert "configuration error (file=" in str(err)
        assert "config file has no [DEFAULT] section" in str(err)


def test_dyn0p3_missing():
    ''' Check that we raise the correct error if there is no [dynamo0.3]
    section '''
    content = re.sub(r"^\[dynamo0.3\]$", "", _CONFIG_CONTENT,
                     flags=re.MULTILINE)
    with tempfile.NamedTemporaryFile(delete=False, mode="w") as new_cfg:
        new_name = new_cfg.name
        new_cfg.write(content)
        new_cfg.close()

        with pytest.raises(ConfigurationError) as err:
            _ = Config(config_file=new_name)

        assert "configuration error (file=" in str(err)
        assert "config file has no [dynamo0.3] section" in str(err)
