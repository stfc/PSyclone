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

from __future__ import absolute_import
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
API = dynamo0.3
DEFAULTSTUBAPI = dynamo0.3
DISTRIBUTED_MEMORY = true
REPRODUCIBLE_REDUCTIONS = false
REPROD_PAD_SIZE = 8
[dynamo0.3]
COMPUTE_ANNEXED_DOFS = false
'''


def setup_module():
    ''' The tests in this module all assume that there is no pre-existing
    Config object. This setup routine ensures that this is the case when
    this module is first entered and the teardown function below guarantees
    it for subsequent tests.  (Necessary when running tests in parallel.)
    '''
    Config._instance = None


def teardown_function():
    '''This teardown function is called at the end of each test and makes
    sure that we wipe the Config object so we get a fresh/default one
    for any further test (and not a left-over one from a test here).
    '''
    # Enforce loading of the default config file
    Config._instance = None


# Disable this pylint warning because otherwise it gets upset about the
# use of these fixtures in the test code.
# pylint:disable=redefined-outer-name


@pytest.fixture(scope="module",
                params=["DISTRIBUTED_MEMORY",
                        "REPRODUCIBLE_REDUCTIONS",
                        "COMPUTE_ANNEXED_DOFS"])
def bool_entry(request):
    '''
    Parameterised fixture that will cause a test that has it as an
    argument to be run for each boolean member of the configuration file

    :param request: Object through which to access current parameter value.
    :type request: :py:class:`_pytest.fixtures.SubRequest`
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
    :param request: Object through which to access current parameter value.
    :type request: :py:class:`_pytest.fixtures.SubRequest`
    :return: Name of element of config file
    :rtype: str
    '''
    return request.param


def test_singleton_create():
    '''
    Check that we can create a Config object
    '''

    # In general loading the config file when using Config.get() will be
    # tested, but if the tests are executed in a specific order (e.g.
    # gocean1p0_config first, which manually loads a Config file), the
    # line to load a config file when the singleton is created might not
    # be executed. So to be certain explicitly delete the singleton here
    # to force that the next line will test loading the default config file.
    _config = Config.get()
    assert isinstance(_config, Config)
    # Check that we are creating a singleton instance
    _config2 = Config.get()
    assert _config is _config2

    # Test that we can not create more than one instance
    with pytest.raises(ConfigurationError) as err:
        Config()
    assert "Only one instance of Config can be created" in str(err)


def test_missing_file(tmpdir):
    ''' Check that we get the expected error when the specified
    config file cannot be found '''
    with pytest.raises(ConfigurationError) as err:
        config = Config()
        config.load(config_file=os.path.join(str(tmpdir),
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
    _config = Config.get()
    _config.load(config_file=TEST_CONFIG)
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
                        'gocean0.1', 'gocean1.0', 'nemo']
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


def test_dm():
    ''' Checks for getter and setter for distributed memory '''
    config = Config()
    config.load(config_file=TEST_CONFIG)
    # Check the setter method
    config.distributed_memory = False
    assert not config.distributed_memory
    with pytest.raises(ConfigurationError) as err:
        # pylint: disable=redefined-variable-type
        config.distributed_memory = "not-a-bool"
    assert "distributed_memory must be a boolean but got " in str(err)


def test_api_not_in_list():
    ''' Check that we raise an error if the default API is not in
    the list of supported APIs '''
    content = re.sub(r"^API = .*$",
                     "API = invalid",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)
    with tempfile.NamedTemporaryFile(delete=False, mode="w") as new_cfg:
        new_name = new_cfg.name
        new_cfg.write(content)
        new_cfg.close()
        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(config_file=new_name)

        assert ("The API (invalid) is not in the list of "
                "supported APIs" in str(err))


def test_default_stubapi_invalid():
    ''' Check that we raise an error if the default stub API is not in
    the list of supported stub APIs '''
    content = re.sub(r"^DEFAULTSTUBAPI = .*$",
                     "DEFAULTSTUBAPI = invalid",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)
    with tempfile.NamedTemporaryFile(delete=False, mode="w") as new_cfg:
        new_name = new_cfg.name
        new_cfg.write(content)
        new_cfg.close()
        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(config_file=new_name)

        assert ("The default stub API (invalid) is not in the list of "
                "supported stub APIs" in str(err))


def test_default_stubapi_missing():
    ''' Check that we raise an error if the default stub API is missing,
    in which case it defaults to the default_api'''
    content = re.sub(r"^DEFAULTSTUBAPI = .*$",
                     "",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)
    with tempfile.NamedTemporaryFile(delete=False, mode="w") as new_cfg:
        new_name = new_cfg.name
        new_cfg.write(content)
        new_cfg.close()
        config = Config()
        config.load(config_file=new_name)

        assert config.default_stub_api == config.default_api


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

        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(config_file=new_name)

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

        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(config_file=new_name)

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
            config = Config()
            config.load(config_file=new_name)
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
            config = Config()
            config.load(config_file=new_name)

        assert "configuration error (file=" in str(err)
        assert "Configuration file has no [DEFAULT] section" in str(err)


def test_wrong_api():
    ''' Check that we raise the correct errors when a user queries
    API-specific configuration options '''
    _config = Config()
    _config.load(config_file=TEST_CONFIG)
    with pytest.raises(ConfigurationError) as err:
        _ = _config.api_conf("blah")
    assert "API 'blah' is not in the list" in str(err)
    with pytest.raises(ConfigurationError) as err:
        _ = _config.api_conf("dynamo0.1")
    assert ("Configuration file did not contain a section for the "
            "'dynamo0.1' API" in str(err))
    with pytest.raises(ValueError) as err:
        _config.api = "invalid"
    assert "'invalid' is not a valid API" in str(err)


def test_api_unimplemented():
    ''' Check that we raise the correct error if we supply a config file
        containing a section for an API for which we've not implemented
        API-specific configuration. '''
    content = re.sub(r"^\[dynamo0.3\]$",
                     "[gocean0.1]",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)
    with tempfile.NamedTemporaryFile(delete=False, mode="w") as new_cfg:
        new_name = new_cfg.name
        new_cfg.write(content)
        new_cfg.close()
        config = Config()
        with pytest.raises(NotImplementedError) as err:
            config.load(new_name)
        assert ("file contains a gocean0.1 section but no Config sub-class "
                "has been implemented for this API" in str(err))


def test_default_api():
    '''If a config file has no default-api specified, but contains only
    a single (non-default) section, this section should be used as the
    default api.
    '''
    content = re.sub(r"^API.*$",
                     "",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)

    with tempfile.NamedTemporaryFile(delete=False, mode="w") as new_cfg:
        new_name = new_cfg.name
        new_cfg.write(content)
        new_cfg.close()
        config = Config()
        config.load(new_name)
        assert config.api == "dynamo0.3"


def test_kernel_naming_setter():
    ''' Check that the setter for the kernel-naming scheme rejects
    unrecognised values. '''
    from psyclone import configuration
    config = Config()
    config.kernel_naming = "single"
    assert config.kernel_naming == "single"
    with pytest.raises(ValueError) as err:
        config.kernel_naming = "not-a-scheme"
    assert ("kernel_naming must be one of '{0}' but got 'not-a-scheme'".
            format(configuration.VALID_KERNEL_NAMING_SCHEMES) in str(err))


def test_incl_path_errors(tmpdir):
    ''' Check that we raise the expected errors if we attempt to set the list
    of include paths to something other than a list or to a location that
    does not exist. '''
    config = Config()
    with pytest.raises(ValueError) as err:
        config.include_paths = config
    assert "include_paths must be a list but got:" in str(err)
    # Create a path that does not exist
    missing_path = tmpdir.join("does_not_exist")
    with pytest.raises(ConfigurationError) as cerr:
        config.include_paths = [missing_path.strpath]
    assert "does_not_exist' does not exist" in str(cerr)
