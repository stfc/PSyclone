# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2024, Science and Technology Facilities Council.
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
# Modified: I. Kavcic and O. Brunt, Met Office,
#          R. W. Ford, STFC Daresbury Lab
#           J. Henrichs, Bureau of Meteorology
#           N. Nobre, STFC Daresbury Lab

'''
Module containing tests relating to PSyclone configuration handling.
'''

import os
import re
import sys

import pytest

import psyclone

from psyclone.configuration import (APISpecificConfig, ConfigurationError,
                                    Config, VALID_KERNEL_NAMING_SCHEMES)
from psyclone.core.access_type import AccessType
from psyclone.domain.gocean import GOceanConstants
from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.nemo import NemoConstants


# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files")
TEST_CONFIG = os.path.join(BASE_PATH, "dummy_config.cfg")

# Valid configuration file content that we will manipulate for
# different tests
_CONFIG_CONTENT = '''\
[DEFAULT]
DEFAULTAPI = dynamo0.3
DEFAULTSTUBAPI = dynamo0.3
DISTRIBUTED_MEMORY = true
REPRODUCIBLE_REDUCTIONS = false
REPROD_PAD_SIZE = 8
VALID_PSY_DATA_PREFIXES = profile, extract
OCL_DEVICES_PER_NODE = 1
BACKEND_CHECKS_ENABLED = false
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


@pytest.fixture(name="bool_entry",
                scope="module",
                params=["DISTRIBUTED_MEMORY",
                        "REPRODUCIBLE_REDUCTIONS",
                        "COMPUTE_ANNEXED_DOFS",
                        "RUN_TIME_CHECKS",
                        "BACKEND_CHECKS_ENABLED"])
def bool_entry_fixture(request):
    '''
    Parameterised fixture that will cause a test that has it as an
    argument to be run for each boolean member of the configuration file

    :param request: Object through which to access current parameter value.
    :type request: :py:class:`_pytest.fixtures.SubRequest`
    :return: Name of element of config file
    :rtype: str
    '''
    return request.param


@pytest.fixture(name="int_entry",
                scope="module",
                params=["REPROD_PAD_SIZE", "OCL_DEVICES_PER_NODE"])
def int_entry_fixture(request):
    '''
    Parameterised fixture that returns the names of integer members of the
    configuration file.
    :param request: Object through which to access current parameter value.
    :type request: :py:class:`_pytest.fixtures.SubRequest`
    :return: Name of element of config file
    :rtype: str
    '''
    return request.param


def get_config(config_file, content):
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


def test_get_repo_config_file():
    '''Check the mechanism by which we ensure that the repository config
    file is picked up by the test suite.
    '''
    config_file = Config.get_repository_config_file()
    assert "config/psyclone.cfg" in config_file


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
    assert "Only one instance of Config can be created" in str(err.value)


def test_missing_file(tmpdir):
    ''' Check that we get the expected error when the specified
    config file cannot be found '''
    with pytest.raises(ConfigurationError) as err:
        config = Config()
        config.load(config_file=os.path.join(str(tmpdir),
                                             "not_a_file.cfg"))
    assert "not_a_file.cfg does not exist" in str(err.value)


@pytest.mark.usefixtures("change_into_tmpdir")
def test_search_path(monkeypatch):
    ''' Check that the search path for a configuration file is as
    expected. It is important to use monkeypatch for manipulating
    PSYCLONE_CONFIG, since all other tests rely on this variable
    (see conftest.setup_psyclone_config).

    '''
    # Ensure that PSYCLONE_CONFIG is not set
    monkeypatch.delitem(os.environ, "PSYCLONE_CONFIG", raising=False)
    # We test the search path used by causing the find_file() method
    # to fail to find any file and thus raise an error. The error msg
    # then gives us the list of locations searched.
    monkeypatch.setattr("os.path.isfile", lambda arg: False)

    cwd = os.getcwd()
    # Test when (we appear to be) both inside and outside a virtual
    # environment
    for inside_venv in [True, False]:
        monkeypatch.setattr(
            "psyclone.utils.within_virtual_env",
            lambda: inside_venv)  # pylint: disable=cell-var-from-loop
        with pytest.raises(ConfigurationError) as err:
            _ = Config.find_file()
        err_msg = str(err.value)
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

        # share directory within package installation directory
        pkg_share_dir = [os.path.join(os.path.dirname(psyclone_path),
                                      "share", "psyclone")
                         for psyclone_path in psyclone.__path__]
        pkg_share_idx = min(err_msg.find(dir) for dir in pkg_share_dir)
        assert pkg_share_idx != -1

        # Check the order of the various directories, which depends on
        # whether we are in a virtualenv or not:
        if inside_venv:
            # When inside a virtual environment, the 'share' directory of
            # that environment takes precedence over the user's home
            # directory
            assert cwd_idx < share_idx < home_idx < pkg_share_idx
        else:
            assert cwd_idx < home_idx < share_idx < pkg_share_idx


@pytest.mark.usefixtures("change_into_tmpdir")
def test_search_env(monkeypatch):
    ''' Check that we pick up the configuration file specified in an
    environment variable. It is important to use monkeypatch for manipulating
    PSYCLONE_CONFIG, since all other tests rely on this variable
    (see conftest.setup_psyclone_config).'''

    # Get the cwd, which is in a temporary directory
    cwd = os.getcwd()
    # Create a .psyclone/psyclone.cfg in the CWD
    cfg_dir = os.path.join(cwd, ".psyclone")
    os.mkdir(cfg_dir)
    fname = os.path.join(cfg_dir, "psyclone.cfg")
    with open(fname, "w", encoding="utf-8") as cfile:
        cfile.write(TEST_CONFIG)
    # Point PSYCLONE_CONFIG to a non-existent file - we should revert
    # to the normal search path in this case
    cfg_file = os.path.join("not_a_dir", "psyclone.cfg")
    monkeypatch.setitem(os.environ, "PSYCLONE_CONFIG", cfg_file)
    name = Config.find_file()
    assert name.startswith(cfg_dir)
    assert "not_a_dir" not in name
    # Now point PSYCLONE_CONFIG to a file that does exist
    cfg_file = "another.cfg"
    with open(cfg_file, "w", encoding="utf-8") as cfile:
        cfile.write(TEST_CONFIG)
    monkeypatch.setitem(os.environ, "PSYCLONE_CONFIG", cfg_file)
    name = Config.find_file()
    assert name == cfg_file


def test_read_values():
    '''
    Check that we get the expected values from the test config file.
    '''
    _config = Config.get()
    _config.load(config_file=TEST_CONFIG)
    # Whether distributed memory is enabled
    dist_mem = _config.distributed_memory
    assert isinstance(dist_mem, bool)
    assert dist_mem
    # The default API
    api = _config.default_api
    assert isinstance(api, str)
    assert api == "dynamo0.3"
    # The list of supported APIs
    api_list = _config.supported_apis
    assert api_list == ['dynamo0.3', 'gocean1.0', 'nemo']
    # The default API for kernel stub generation
    api = _config.default_stub_api
    assert isinstance(api, str)
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
        config.distributed_memory = "not-a-bool"
    assert "distributed_memory must be a boolean but got " in str(err.value)


def test_api_not_in_list(tmpdir):
    ''' Check that we raise an error if the default API is not in
    the list of supported APIs.

    '''
    config_file = tmpdir.join("config")
    content = re.sub(r"^DEFAULTAPI = .*$",
                     "DEFAULTAPI = invalid",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)
    config_file = tmpdir.join("config")

    with pytest.raises(ConfigurationError) as err:
        get_config(config_file, content)

    assert ("The API (invalid) is not in the list of "
            "supported APIs" in str(err.value))


def test_default_stubapi_invalid(tmpdir):
    ''' Check that we raise an error if the default stub API is not in
    the list of supported stub APIs.

    '''
    config_file = tmpdir.join("config")
    content = re.sub(r"^DEFAULTSTUBAPI = .*$",
                     "DEFAULTSTUBAPI = invalid",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)

    with pytest.raises(ConfigurationError) as err:
        get_config(config_file, content)

    assert ("The default stub API (invalid) is not in the list of "
            "supported stub APIs" in str(err.value))


def test_default_stubapi_missing(tmpdir):
    ''' Check that we raise an error if the default stub API is missing,
    in which case it defaults to the default_api.

    '''
    config_file = tmpdir.join("config")
    content = re.sub(r"^DEFAULTSTUBAPI = .*$",
                     "",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)

    test_config = get_config(config_file, content)

    assert test_config.default_stub_api == test_config.default_api


def test_not_bool(bool_entry, tmpdir):
    ''' Check that we catch cases where we expect a boolean in the config
    file but don't get one.

    '''
    config_file = tmpdir.join("config")
    content = re.sub(rf"^{bool_entry} = .*$",
                     f"{bool_entry} = wrong",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)

    with pytest.raises(ConfigurationError) as err:
        get_config(config_file, content)

    assert "configuration error (file=" in str(err.value)
    assert f": Error while parsing {bool_entry}" in str(err.value)
    assert "Not a boolean: wrong" in str(err.value)


def test_not_int(int_entry, tmpdir):
    ''' Check that we catch cases where we expect an integer in the config
    file but don't get one.

    '''
    config_file = tmpdir.join("config")
    content = re.sub(rf"^{int_entry} = .*$",
                     f"{int_entry} = wrong",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)

    with pytest.raises(ConfigurationError) as err:
        get_config(config_file, content)

    assert "configuration error (file=" in str(err.value)
    assert (f": error while parsing {int_entry}: invalid literal"
            in str(err.value))


def test_backend_checks_from_file(tmpdir):
    '''
    Check that the value for BACKEND_CHECKS_ENABLED is correctly read from
    the config. file and defaults to True.

    '''
    config_file = tmpdir.join("config")
    cfg = get_config(config_file, _CONFIG_CONTENT)
    assert cfg.backend_checks_enabled is False
    content = re.sub(r"^BACKEND_CHECKS_ENABLED = false$",
                     "BACKEND_CHECKS_ENABLED = true",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)
    config_file2 = tmpdir.join("config2")
    cfg2 = get_config(config_file2, content)
    assert cfg2.backend_checks_enabled is True
    # Remove it from the config file.
    content = re.sub(r"^BACKEND_CHECKS_ENABLED = false$",
                     "",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)
    config_file3 = tmpdir.join("config3")
    cfg3 = get_config(config_file3, content)
    # Defaults to True if not specified in the file.
    assert cfg3.backend_checks_enabled is True


def test_broken_fmt(tmpdir):
    ''' Check the error if the formatting of the configuration file is
    wrong.

    '''
    config_file = tmpdir.join("config")

    # Create a 'config' file without any section headers
    content = "COMPUTE_ANNEXED_DOFS = false\n"

    with pytest.raises(ConfigurationError) as err:
        get_config(config_file, content)
    assert ("ConfigParser failed to read the configuration file. Is it "
            "formatted correctly? (Error was: File contains no section "
            "headers" in str(err.value))

    # Test for general parsing error (here broken key-value mapping)
    content = re.sub(r"^DEFAULTSTUBAPI = .*$",
                     "DEFAULT",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)

    with pytest.raises(ConfigurationError) as err:
        get_config(config_file, content)
    assert "Error was: Source contains parsing errors" in str(err.value)


def test_default_missing(tmpdir):
    ''' Check that we produce a suitable error if the [DEFAULT] section
    of the configuration file is missing.

    '''
    config_file = tmpdir.join("config")
    content = '''\
[dynamo0.3]
COMPUTE_ANNEXED_DOFS = false
'''

    with pytest.raises(ConfigurationError) as err:
        get_config(config_file, content)

    assert "configuration error (file=" in str(err.value)
    assert "Configuration file has no [DEFAULT] section" in str(err.value)


def test_wrong_api():
    ''' Check that we raise the correct errors when a user queries
    API-specific configuration options '''
    _config = Config()
    _config.load(config_file=TEST_CONFIG)
    with pytest.raises(ConfigurationError) as err:
        _ = _config.api_conf("blah")
    assert "API 'blah' is not in the list" in str(err.value)
    with pytest.raises(ConfigurationError) as err:
        _ = _config.api_conf("nemo")
    assert ("Configuration file did not contain a section for the "
            "'nemo' API" in str(err.value))
    with pytest.raises(ValueError) as err:
        _config.api = "invalid"
    assert "'invalid' is not a valid API" in str(err.value)


def test_api_unimplemented(tmpdir, monkeypatch):
    ''' Check that we raise the correct error if we supply a config file
    containing a section for an API for which we've not implemented
    API-specific configuration.

    '''
    # Since all APIs need a API-specific section, for this error we
    # need to temporarily add a new supported API, that will not
    # be in the config file:
    config_file = tmpdir.join("config")
    monkeypatch.setattr(Config, "_supported_api_list",
                        Config._supported_api_list + ["UNIMPLEMENTED"])
    content = re.sub(r"^\[dynamo0.3\]$",
                     "[UNIMPLEMENTED]",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)

    with pytest.raises(NotImplementedError) as err:
        get_config(config_file, content)
    assert ("file contains a UNIMPLEMENTED section but no Config "
            "sub-class has been implemented for this API" in str(err.value))


def test_default_api(tmpdir):
    '''If a config file has no default-api specified, but contains only
    a single (non-default) section, this section should be used as the
    default api.

    '''
    config_file = tmpdir.join("config")
    content = re.sub(r"^API.*$",
                     "",
                     _CONFIG_CONTENT,
                     flags=re.MULTILINE)

    default_config = get_config(config_file, content)
    assert default_config.api == "dynamo0.3"


def test_root_name_init():
    '''Check that the configuration class has the expected default
    values.

    '''
    assert Config._default_psyir_root_name == "psyir_tmp"
    config = Config()
    assert config._psyir_root_name is None


@pytest.mark.parametrize("content,result",
                         # An empty `default` raises an exception so I've
                         # arbitrarily added API.
                         [("[DEFAULT]\nAPI=dynamo0.3\n", "psyir_tmp"),
                          ("[DEFAULT]\nPSYIR_ROOT_NAME = random\n", "random")])
def test_root_name_load(tmpdir, content, result):
    '''Check that the config class returns appropriate values from a
    config file when PSYIR_ROOT_NAME is and isn't provided.

    '''
    config_file = tmpdir.join("config")

    test_config = get_config(config_file, content)

    assert test_config._psyir_root_name == result
    assert test_config.psyir_root_name == result


def test_enable_backend_checks_setter_getter():
    '''
    Test the setter/getter for the 'backend_checks_enabled' property.
    '''
    config = Config()
    with pytest.raises(TypeError) as err:
        config.backend_checks_enabled = "hllo"
    assert ("backend_checks_enabled must be a boolean but got 'str'" in
            str(err.value))
    config.backend_checks_enabled = True
    assert config.backend_checks_enabled is True


def test_kernel_naming_setter():
    ''' Check that the setter for the kernel-naming scheme rejects
    unrecognised values.

    '''
    config = Config()
    config.kernel_naming = "single"
    assert config.kernel_naming == "single"
    with pytest.raises(ValueError) as err:
        config.kernel_naming = "not-a-scheme"
    assert (f"kernel_naming must be one of '{VALID_KERNEL_NAMING_SCHEMES}' "
            f"but got 'not-a-scheme'" in str(err.value))


def test_incl_path_errors(tmpdir):
    ''' Check that we raise the expected errors if we attempt to set the list
    of include paths to something other than a list or to a location that
    does not exist. '''
    config = Config()
    with pytest.raises(ValueError) as err:
        config.include_paths = config
    assert "include_paths must be a list but got:" in str(err.value)
    # Create a path that does not exist
    missing_path = tmpdir.join("does_not_exist")
    with pytest.raises(ConfigurationError) as cerr:
        config.include_paths = [missing_path.strpath]
    assert "does_not_exist' does not exist" in str(cerr.value)


def test_mappings():
    '''Test the definition of a mapping in the config file.'''
    mapping = APISpecificConfig.create_dict_from_list(["k1:v1", "k2:v2"])
    assert mapping == {"k1": "v1", "k2": "v2"}

    mapping = APISpecificConfig.create_dict_from_list([])
    assert mapping == {}

    # The function only uses the first ":" :
    mapping = \
        APISpecificConfig.create_dict_from_list(
            ["k1 : v1", "k2 : v2 :something"])
    assert mapping == {"k1": "v1", "k2": "v2 :something"}

    # Tests errors: check that '=' instead of ":" is detected as invalid:
    with pytest.raises(ConfigurationError) as err:
        mapping = APISpecificConfig.create_dict_from_list(["k1:v1", "k2=v2"])
    assert "Invalid format for mapping: k2=v2" in str(err.value)


def test_invalid_access_mapping(tmpdir):
    '''Test that providing an invalid access type (i.e. not
    'read', 'write', ...) raises an exception.

    '''
    # Test for an invalid key
    config_file = tmpdir.join("config")
    content = re.sub(r"gh_read: read", "gh_read: invalid", _CONFIG_CONTENT)

    with pytest.raises(ConfigurationError) as cerr:
        get_config(config_file, content)
    assert "Unknown access type 'invalid' found for key 'gh_read'" \
        in str(cerr.value)

    # Test that all values of the mapping are access types:
    api_config = Config.get().api_conf("dynamo0.3")
    for access_mode in api_config.get_access_mapping().values():
        assert isinstance(access_mode, AccessType)


def test_default_access_mapping(tmpdir):
    ''' Test that the default access mapping is correctly converted
    to AccessTypes.

    '''
    config_file = tmpdir.join("config")

    test_config = get_config(config_file, _CONFIG_CONTENT)

    api_config = test_config.api_conf("dynamo0.3")
    for access_mode in api_config.get_access_mapping().values():
        assert isinstance(access_mode, AccessType)


def test_access_mapping_order(tmpdir):
    ''' Test that the order of the access mappings in the config file
    does not affect the correct access type-mode conversion.

    '''
    config_file = tmpdir.join("config")
    content = re.sub(r"gh_write: write, gh_readwrite: readwrite",
                     "gh_readwrite: readwrite, gh_write: write",
                     _CONFIG_CONTENT)
    content = re.sub(r"gh_inc: inc, gh_sum: sum",
                     "gh_sum: sum, gh_inc: inc", content)

    api_config = get_config(config_file, content).get().api_conf("dynamo0.3")

    for access_mode in api_config.get_access_mapping().values():
        assert isinstance(access_mode, AccessType)


def test_psy_data_prefix(tmpdir):
    ''' Check the handling of PSyData class prefixes. '''
    config_file = tmpdir.join("config.correct")

    test_config = get_config(config_file, _CONFIG_CONTENT)

    assert "profile" in test_config.valid_psy_data_prefixes
    assert "extract" in test_config.valid_psy_data_prefixes
    assert len(test_config.valid_psy_data_prefixes) == 2

    # Now handle a config file without psy data prefixes:
    # This should not raise an exception, but define an empty list
    config_file = tmpdir.join("config.no_psydata")
    content = re.sub(r"VALID_PSY_DATA_PREFIXES", "NO-PSY-DATA",
                     _CONFIG_CONTENT)

    test_config = get_config(config_file, content)

    assert not test_config.valid_psy_data_prefixes


def test_invalid_prefix(tmpdir):
    '''Tests invalid PSyData prefixes (i.e. ones that would result
    in invalid Fortran names when used).

    '''
    for prefix in ["1", "&AB", "?", "_ab", "ab'", "cd\"", "ef?"]:
        config_file = tmpdir.join("config.invalid_psydata")
        content = re.sub(r"^VALID_PSY_DATA_PREFIXES.*$",
                         "VALID_PSY_DATA_PREFIXES="+prefix,
                         _CONFIG_CONTENT, flags=re.MULTILINE)

        with pytest.raises(ConfigurationError) as err:
            get_config(config_file, content)
        # When there is a '"' in the invalid prefix, the "'" in the
        # error message is escaped with a '\'. So in order to test the
        # invalid 'cd"' prefix, we need to have two tests in the assert:
        assert (f"Invalid PsyData-prefix '{prefix}' in config file"
                in str(err.value)
                or f"Invalid PsyData-prefix \\'{prefix}\\' in config file"
                in str(err.value))
        assert "The prefix must be valid for use as the start of a " \
               "Fortran variable name." in str(err.value)


def test_get_constants():
    '''Tests the API-independent version of get_constants in
    the Config class.

    '''
    config = Config().get()
    config.api = "dynamo0.3"
    assert isinstance(config.get_constants(), LFRicConstants)
    config.api = "gocean1.0"
    assert isinstance(config.get_constants(), GOceanConstants)
    config.api = "nemo"
    assert isinstance(config.get_constants(), NemoConstants)


def test_config_class_initialised(monkeypatch):
    '''Tests that the flag indicating that the Config class is
    initialised works as expected.'''

    monkeypatch.setattr(Config, "_HAS_CONFIG_BEEN_INITIALISED", False)

    _ = Config().get()
    assert Config.has_config_been_initialised() is True
