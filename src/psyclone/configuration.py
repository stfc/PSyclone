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
# Authors: R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified: J. Henrichs, Bureau of Meteorology
#           I. Kavcic, Met Office
#           N. Nobre, STFC Daresbury Lab

'''
PSyclone configuration management module.

Deals with reading the config file and storing default settings.
'''

import abc
from configparser import (ConfigParser, MissingSectionHeaderError,
                          ParsingError)
from collections import namedtuple
import os
import re
import sys
import psyclone

from psyclone.errors import PSycloneError, InternalError


# Name of the config file we search for
_FILE_NAME = "psyclone.cfg"

# The different naming schemes supported when transforming kernels:
# multiple = Every transformed kernel is given a unique name. This permits
#            multiple versions of the same kernel to be created.
# single = If any given kernel (within a single Application) is transformed
#          more than once then the same transformation must always be
#          applied and only one version of the transformed kernel is created.
VALID_KERNEL_NAMING_SCHEMES = ["multiple", "single"]

LFRIC_API_NAMES = ["lfric", "dynamo0.3"]
GOCEAN_API_NAMES = ["gocean", "gocean1.0"]


# pylint: disable=too-many-lines
class ConfigurationError(PSycloneError):
    '''
    Class for all configuration-related errors.

    :param str value: error message.
    :param config: the Config object associated with the error.
    :type config: :py:class:`psyclone.configuration.Config`.
    '''
    def __init__(self, value, config=None):
        PSycloneError.__init__(self, value)
        self.value = "PSyclone configuration error"
        if config:
            self.value = f"{self.value} (file={config.filename})"
        self.value += f": {value}"


# =============================================================================
class Config:
    # pylint: disable=too-many-instance-attributes, too-many-public-methods
    '''
    Handles all configuration management. It is implemented as a singleton
    using a class _instance variable and a get() function.
    '''
    # Class variable to store the singleton instance
    _instance = None

    # A consistency flag that is set to true the moment the proper config
    # file is loaded. If an instance of this class should be created before
    # the loading of the config file, an exception will be raised.
    _HAS_CONFIG_BEEN_INITIALISED = False

    # List of supported API by PSyclone
    _supported_api_list = LFRIC_API_NAMES + GOCEAN_API_NAMES
    # List for printing purposes (remove duplicates and use prefered names)
    _curated_api_list = [LFRIC_API_NAMES[0], GOCEAN_API_NAMES[0]]

    # List of supported stub API by PSyclone
    _supported_stub_api_list = LFRIC_API_NAMES

    # The default scheme to use when (re)naming transformed kernels.
    # By default we support multiple, different versions of any given
    # kernel by ensuring that each transformed kernel is given a
    # unique name (within the specified kernel-output directory).
    # N.B. the default location to which to write transformed kernels is
    # the current working directory. Since this may change between the
    # importing of this module and the actual generation of code (e.g. as
    # happens in the test suite), we do not store it here. Instead it
    # is set in the Config.kernel_output_dir getter.
    _default_kernel_naming = "multiple"

    # The default name to use when creating new names in the
    # PSyIR symbol table.
    _default_psyir_root_name = "psyir_tmp"

    # The list of valid PSyData class prefixes
    _valid_psy_data_prefixes = []

    @staticmethod
    def get(do_not_load_file=False):
        '''Static function that if necessary creates and returns the singleton
        config instance.

        :param bool do_not_load_file: If set it will not load the default \
               config file. This is used when handling the command line so \
               that the user can specify the file to load.
        '''
        if not Config._instance:
            Config._instance = Config()
            if not do_not_load_file:
                Config._instance.load()
        return Config._instance

    # -------------------------------------------------------------------------
    @staticmethod
    def has_config_been_initialised():
        ''':returns: if the config class has loaded a (potential custom) \
            config file.

        '''
        return Config._HAS_CONFIG_BEEN_INITIALISED

    # -------------------------------------------------------------------------
    @staticmethod
    def get_repository_config_file():
        '''This function returns the absolute path to the config file included
        in the PSyclone repository. It is used by the testing framework to make
        sure all tests get the same config file (see tests/config_tests for the
        only exception).
        :return str: Absolute path to the config file included in the \
                     PSyclone repository.
        '''
        this_dir = os.path.dirname(os.path.abspath(__file__))
        # The psyclone root dir is "../.." from this directory,
        # so to remain portable use dirname twice:
        psyclone_root_dir = os.path.dirname(os.path.dirname(this_dir))
        return os.path.join(psyclone_root_dir, "config", "psyclone.cfg")

    # -------------------------------------------------------------------------
    def __init__(self):
        '''This is the basic constructor that only sets the supported APIs
        and stub APIs, it does not load a config file. The Config instance
        is a singleton, and as such will test that no instance already exists
        and raise an exception otherwise.

        :raises ConfigurationError: If a singleton instance of Config already
                                    exists.
        '''

        if Config._instance is not None:
            raise ConfigurationError("Only one instance of "
                                     "Config can be created")

        # This dictionary stores the API-specific config instances
        # for each API specified in a config file.
        self._api_conf = {}

        # This will store the ConfigParser instance for the specified
        # config file.
        self._config = None

        # The name (including path) of the config file read.
        self._config_file = None

        # The API selected by the user
        self._api = ""

        # True if distributed memory code should be created.
        self._distributed_mem = None

        # True if reproducible reductions should be used.
        self._reproducible_reductions = None

        # Padding size (number of array elements) to be used when
        # reproducible reductions are created.
        self._reprod_pad_size = None

        # Where to write transformed kernels - set at runtime.
        self._kernel_output_dir = None

        # The naming scheme to use for transformed kernels.
        self._kernel_naming = None

        # The list of directories to search for Fortran include files.
        self._include_paths = []

        # The root name to use when creating internal PSyIR names.
        self._psyir_root_name = None

        # Number of OpenCL devices per node
        self._ocl_devices_per_node = 1

        # By default, a PSyIR backend performs validation checks as it
        # traverses the tree. Setting this option to False disables those
        # checks which can be useful in the case of unimplemented features.
        self._backend_checks_enabled = True

    # -------------------------------------------------------------------------
    def load(self, config_file=None):
        '''Loads a configuration file.

        :param str config_file: Override default configuration file to read.
        :raises ConfigurationError: if there are errors or inconsistencies in \
                                the specified config file.
        '''
        # pylint: disable=too-many-branches, too-many-statements
        if config_file:
            # Caller has explicitly provided the full path to the config
            # file to read
            if not os.path.isfile(config_file):
                raise ConfigurationError(
                    f"File {config_file} does not exist")
            self._config_file = config_file[:]
        else:
            # Search for the config file in various default locations
            self._config_file = Config.find_file()
        # Add a getlist method to the ConfigParser instance using the
        # converters argument. The lambda functions also handles the
        # case of an empty specification ('xx = ''), returning an
        # empty list instead of a list containing the empty string:
        self._config = ConfigParser(
            converters={'list': lambda x: [] if not x else
                                          [i.strip() for i in x.split(',')]})
        try:
            self._config.read(self._config_file)
        # Check for missing section headers and general parsing errors
        # (e.g. incomplete or incorrect key-value mapping)
        except (MissingSectionHeaderError, ParsingError) as err:
            raise ConfigurationError(
                f"ConfigParser failed to read the configuration file. Is it "
                f"formatted correctly? (Error was: {err})",
                config=self) from err

        # Check that the configuration file has a [DEFAULT] section. Even
        # if there isn't one in the file, ConfigParser creates an (empty)
        # dictionary entry for it.
        if 'DEFAULT' not in self._config or \
           not self._config['DEFAULT'].keys():
            raise ConfigurationError(
                "Configuration file has no [DEFAULT] section", config=self)

        # The call to the 'read' method above populates a dictionary.
        # All of the entries in that dict are unicode strings so here
        # we pull out the values we want and deal with any type
        # conversion. We protect each of these with a try block so as
        # to catch any conversion errors due to incorrect entries in
        # the config file.
        try:
            self._distributed_mem = self._config['DEFAULT'].getboolean(
                'DISTRIBUTED_MEMORY')
        except ValueError as err:
            raise ConfigurationError(
                f"Error while parsing DISTRIBUTED_MEMORY: {err}",
                config=self) from err

        try:
            self._reproducible_reductions = self._config['DEFAULT'].getboolean(
                'REPRODUCIBLE_REDUCTIONS')
        except ValueError as err:
            raise ConfigurationError(
                f"Error while parsing REPRODUCIBLE_REDUCTIONS: {err}",
                config=self) from err

        try:
            self._reprod_pad_size = self._config['DEFAULT'].getint(
                'REPROD_PAD_SIZE')
        except ValueError as err:
            raise ConfigurationError(
                f"error while parsing REPROD_PAD_SIZE: {err}",
                config=self) from err

        if 'PSYIR_ROOT_NAME' not in self._config['DEFAULT']:
            # Use the default name if no default is specified for the
            # root name.
            self._psyir_root_name = Config._default_psyir_root_name
        else:
            self._psyir_root_name = self._config['DEFAULT']['PSYIR_ROOT_NAME']

        # Read the valid PSyData class prefixes. If the keyword does
        # not exist then return an empty list.
        self._valid_psy_data_prefixes = \
            self._config["DEFAULT"].getlist("VALID_PSY_DATA_PREFIXES", [])
        try:
            self._ocl_devices_per_node = self._config['DEFAULT'].getint(
                'OCL_DEVICES_PER_NODE')
        except ValueError as err:
            raise ConfigurationError(
                f"error while parsing OCL_DEVICES_PER_NODE: {err}",
                config=self) from err

        # Verify that the prefixes will result in valid Fortran names:
        valid_var = re.compile(r"[A-Z][A-Z0-9_]*$", re.I)
        for prefix in self._valid_psy_data_prefixes:
            if not valid_var.match(prefix):
                raise ConfigurationError(
                    f"Invalid PsyData-prefix '{prefix}' in config file. The "
                    f"prefix must be valid for use as the start of a Fortran "
                    f"variable name.", config=self)

        # Whether validation is performed in the PSyIR backends.
        if 'BACKEND_CHECKS_ENABLED' in self._config['DEFAULT']:
            try:
                self._backend_checks_enabled = (
                    self._config['DEFAULT'].getboolean(
                        'BACKEND_CHECKS_ENABLED'))
            except ValueError as err:
                raise ConfigurationError(
                    f"Error while parsing BACKEND_CHECKS_ENABLED: {err}",
                    config=self) from err

        # Now we deal with the API-specific sections of the config file. We
        # create a dictionary to hold the API-specific Config objects.
        self._api_conf = {}
        for api in Config._supported_api_list:
            if api in self._config:
                if api in LFRIC_API_NAMES:
                    key = LFRIC_API_NAMES[0]  # Use the first name internally
                    self._api_conf[key] = LFRicConfig(self, self._config[api])
                elif api in GOCEAN_API_NAMES:
                    key = GOCEAN_API_NAMES[0]  # Use the first name internally
                    self._api_conf[key] = GOceanConfig(self, self._config[api])
                else:
                    raise NotImplementedError(
                        f"Configuration file '{self._config_file}' contains a "
                        f"'{api}' section but no Config sub-class has "
                        f"been implemented for this API")

        # The scheme to use when re-naming transformed kernels.
        # By default we ensure that each transformed kernel is given a
        # unique name (within the specified kernel-output directory).
        self._kernel_naming = Config._default_kernel_naming

        ignore_modules = self._config['DEFAULT'].getlist("IGNORE_MODULES", [])
        # Avoid circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.parse import ModuleManager
        mod_manager = ModuleManager.get()
        for module_name in ignore_modules:
            mod_manager.add_ignore_module(module_name)

        # Set the flag that the config file has been loaded now.
        Config._HAS_CONFIG_BEEN_INITIALISED = True

    def api_conf(self, api=None):
        '''
        Getter for the object holding API-specific configuration options.

        :param str api: Optional, the API for which configuration details are
                required. If none is specified, returns the config for the
                default API.
        :returns: object containing API-specific configuration
        :rtype: One of :py:class:`psyclone.configuration.LFRicConfig`,
                :py:class:`psyclone.configuration.GOceanConfig` or None.

        :raises ConfigurationError: if api is not in the list of supported \
                                    APIs.
        :raises ConfigurationError: if the config file did not contain a \
                                    section for the requested API.
        '''
        if not api:
            api = self._api

        if api not in self.supported_apis:
            raise ConfigurationError(
                f"API '{api}' is not in the list '{self.supported_apis}'' of "
                f"supported APIs.")
        if api not in self._api_conf:
            raise ConfigurationError(
                f"Configuration file did not contain a section for the "
                f"'{api}' API", config=self)
        return self._api_conf[api]

    @staticmethod
    def find_file():
        '''
        Static method that searches various locations for a configuration
        file. If the full path to an existing file has been provided in
        the PSYCLONE_CONFIG environment variable then that is returned.
        Otherwise, we search the following locations, in order:

        - ${PWD}/.psyclone/
        -  if inside-a-virtual-environment:
              <base-dir-of-virtual-env>/share/psyclone/
        - ${HOME}/.local/share/psyclone/
        - <system-install-prefix>/share/psyclone/
        - <psyclone-installation-base>/share/psyclone/

        :returns: the fully-qualified path to the configuration file
        :rtype: str

        :raises ConfigurationError: if no config file is found

        '''
        # Moving this to the top causes test failures
        # pylint: disable=import-outside-toplevel
        from psyclone.utils import within_virtual_env

        # If $PSYCLONE_CONFIG is set then we use that unless the
        # file it points to does not exist
        _psy_config = os.environ.get('PSYCLONE_CONFIG')
        if _psy_config and os.path.isfile(_psy_config):
            return _psy_config

        # Set up list of locations to search
        share_dir = os.path.join(sys.prefix, "share", "psyclone")
        pkg_share_dir = [
            os.path.join(os.path.dirname(psyclone_path), "share", "psyclone")
            for psyclone_path in psyclone.__path__]

        # 1. .psyclone/ in the CWD
        _file_paths = [os.path.join(os.getcwd(), ".psyclone")]
        if within_virtual_env():
            # 2. <virtual-env-base>/share/psyclone/
            _file_paths.append(share_dir)
        # 3. ~/.local/share/psyclone/
        _file_paths.append(os.path.join(os.path.expanduser("~"),
                                        ".local", "share", "psyclone"))
        if not within_virtual_env():
            # 4. <python-installation-base>/share/psyclone/
            _file_paths.append(share_dir)
        # 5. <psyclone-installation-base>/share/psyclone/
        _file_paths.extend(pkg_share_dir)

        for cfile in [os.path.join(cdir, _FILE_NAME) for cdir in _file_paths]:
            if os.path.isfile(cfile):
                return cfile

        # If we get to here then we have failed to find a config file
        raise ConfigurationError(f"{_FILE_NAME} not found in any of "
                                 f"{_file_paths}")

    @property
    def distributed_memory(self):
        '''
        Getter for whether or not distributed memory is enabled

        :returns: True if DM is enabled, False otherwise
        :rtype: bool
        '''
        return self._distributed_mem

    @distributed_memory.setter
    def distributed_memory(self, dist_mem):
        '''
        Setter for whether or not distributed memory support is enabled
        in this configuration.

        :param bool dist_mem: Whether or not dm is enabled
        '''
        if not isinstance(dist_mem, bool):
            raise ConfigurationError(
                f"distributed_memory must be a boolean but got "
                f"{type(dist_mem)}")
        self._distributed_mem = dist_mem

    @property
    def api(self):
        '''Getter for the API selected by the user.

        :returns: The name of the selected API.
        :rtype: str
        '''
        return self._api

    @api.setter
    def api(self, api):
        '''Setter for the API selected by the user.

        :param str api: The name of the API to use.

        :raises ValueError if api is not a supported API.
        '''
        if api not in self._supported_api_list + [""]:
            raise ValueError(f"'{api}' is not a valid API, it must be one "
                             f"of {Config._supported_api_list}'.")
        self._api = api

    @property
    def supported_apis(self):
        '''
        Getter for the list of APIs supported by PSyclone.

        :returns: list of supported APIs
        :rtype: list of str
        '''
        return Config._supported_api_list

    @property
    def curated_api_list(self):
        '''
        :returns: the curated list of PSyKAl DSLs supported.
        :rtype: list[str]
        '''
        return Config._curated_api_list

    @property
    def backend_checks_enabled(self):
        '''
        :returns: whether the validity checks in the PSyIR backend should be
                  disabled.
        :rtype: bool
        '''
        return self._backend_checks_enabled

    @backend_checks_enabled.setter
    def backend_checks_enabled(self, value):
        '''
        Setter for whether or not the PSyIR backend is to perform validation
        checks.

        :param bool value: whether or not to perform validation.

        :raises TypeError: if `value` is not a boolean.

        '''
        if not isinstance(value, bool):
            raise TypeError(f"Config.backend_checks_enabled must be a boolean "
                            f"but got '{type(value).__name__}'")
        self._backend_checks_enabled = value

    @property
    def supported_stub_apis(self):
        '''
        Getter for the list of APIs supported by the stub generator.

        :returns: list of supported APIs.
        :rtype: list of str
        '''
        return Config._supported_stub_api_list

    @property
    def reproducible_reductions(self):
        '''
        Getter for whether reproducible reductions are enabled.

        :returns: True if reproducible reductions are enabled, False otherwise.
        :rtype: bool
        '''
        return self._reproducible_reductions

    @property
    def reprod_pad_size(self):
        '''
        Getter for the amount of padding to use for the array required
        for reproducible OpenMP reductions

        :returns: padding size (no. of array elements)
        :rtype: int
        '''
        return self._reprod_pad_size

    @property
    def psyir_root_name(self):
        '''
        Getter for the root name to use when creating PSyIR names.

        :returns: the PSyIR root name.
        :rtype: str
        '''
        return self._psyir_root_name

    @property
    def filename(self):
        '''
        Getter for the full path and name of the configuration file used
        to initialise this configuration object.

        :returns: full path and name of configuration file
        :rtype: str
        '''
        return self._config_file

    @property
    def kernel_output_dir(self):
        '''
        :returns: the directory to which to write transformed kernels.
        :rtype: str
        '''
        if not self._kernel_output_dir:
            # We use the CWD if no directory has been specified
            self._kernel_output_dir = os.getcwd()
        return self._kernel_output_dir

    @kernel_output_dir.setter
    def kernel_output_dir(self, value):
        '''
        Setter for kernel output directory.
        :param str value: directory to which to write transformed kernels.
        '''
        self._kernel_output_dir = value

    @property
    def kernel_naming(self):
        '''
        :returns: what naming scheme to use when writing transformed kernels \
                  to file.
        :rtype: str
        '''
        return self._kernel_naming

    @kernel_naming.setter
    def kernel_naming(self, value):
        '''
        Setter for how to re-name kernels when writing transformed kernels
        to file.

        :param str value: one of VALID_KERNEL_NAMING_SCHEMES.
        :raises ValueError: if the supplied value is not a recognised \
                            kernel-renaming scheme.
        '''
        if value not in VALID_KERNEL_NAMING_SCHEMES:
            raise ValueError(
                f"kernel_naming must be one of '{VALID_KERNEL_NAMING_SCHEMES}'"
                f" but got '{value}'")
        self._kernel_naming = value

    @property
    def include_paths(self):
        '''
        :returns: the list of paths to search for Fortran include files.
        :rtype: list of str.
        '''
        return self._include_paths

    @include_paths.setter
    def include_paths(self, path_list):
        '''
        Sets the list of paths to search for Fortran include files.

        :param path_list: list of directories to search.
        :type path_list: list of str.

        :raises ValueError: if `path_list` is not a list-like object.
        :raises ConfigurationError: if any of the paths in the list do \
                                    not exist.
        '''
        self._include_paths = []
        try:
            for path in path_list:
                if not os.path.exists(path):
                    raise ConfigurationError(
                        f"Include path '{path}' does not exist")
                self._include_paths.append(path)
        except (TypeError, ValueError) as err:
            raise ValueError(f"include_paths must be a list but got: "
                             f"{type(path_list)}") from err

    @property
    def valid_psy_data_prefixes(self):
        ''':returns: The list of all valid class prefixes.
        :rtype: list of str'''
        return self._valid_psy_data_prefixes

    @property
    def ocl_devices_per_node(self):
        ''':returns: The number of OpenCL devices per node.
        :rtype: int'''
        return self._ocl_devices_per_node

    def get_default_keys(self):
        '''Returns all keys from the default section.
        :returns list: List of all keys of the default section as strings.
        '''
        return self._config.defaults()

    def get_constants(self):
        ''':returns: the constants instance of the current API.
        :rtype: :py:class:`psyclone.domain.lfric.LFRicConstants` |
            :py:class:`psyclone.domain.gocean.GOceanConstants`
        '''
        return self.api_conf().get_constants()


# =============================================================================
class BaseConfig:
    '''A base class for functions that each API-specific class must provide.
    At the moment this is just the function 'access_mapping' that maps between
    API-specific access-descriptor strings and the PSyclone internal
    AccessType.
    :param section: :py:class:`configparser.SectionProxy`
    :raises ConfigurationError: if an access-mapping is provided that \
        assigns an invalid value (i.e. not one of 'read', 'write', \
        'readwrite'), 'inc' or 'sum') to a string.
    '''

    def __init__(self, section):
        # Set a default mapping, this way the test cases all work without
        # having to specify those mappings.
        self._access_mapping = {"read": "read", "write": "write",
                                "readwrite": "readwrite", "inc": "inc",
                                "sum": "sum"}
        # Get the mapping if one exists and convert it into a
        # dictionary. The input is in the format: key1:value1,
        # key2=value2, ...
        if "ACCESS_MAPPING" in section:
            mapping_list = section.getlist("ACCESS_MAPPING")
            if mapping_list is not None:
                self._access_mapping = \
                    BaseConfig.create_dict_from_list(mapping_list)
            # Now convert the string type ("read" etc) to AccessType
            # TODO (issue #710): Add checks for duplicate or missing access
            # key-value pairs
            # Avoid circular import
            # pylint: disable=import-outside-toplevel
            from psyclone.core.access_type import AccessType

            for api_access_name, access_type in self._access_mapping.items():
                try:
                    self._access_mapping[api_access_name] = \
                        AccessType.from_string(access_type)
                except ValueError as err:
                    # Raised by from_string()
                    raise ConfigurationError(
                        f"Unknown access type '{access_type}' found for key "
                        f"'{api_access_name}'") from err

        # Now create the reverse lookup (for better error messages):
        self._reverse_access_mapping = {v: k for k, v in
                                        self._access_mapping.items()}

    @staticmethod
    def create_dict_from_list(input_list):
        '''Takes a list of strings each with the format: key:value and creates
        a dictionary with the key,value pairs. Any leading or trailing
        white space is removed.

        :param input_list: the input list.
        :type input_list: list of str

        :returns: a dictionary with the key,value pairs from the input list.
        :rtype: dict[str, Any]

        :raises ConfigurationError: if any entry in the input list
                does not contain a ":".

        '''
        return_dict = {}
        for entry in input_list:
            try:
                key, value = entry.split(":", 1)
            except ValueError as err:
                # Raised when split does not return two elements:
                raise ConfigurationError(
                    f"Invalid format for mapping: {entry.strip()}") from err
            # Remove spaces and convert unicode to normal strings in Python2
            return_dict[str(key.strip())] = str(value.strip())
        return return_dict

    @staticmethod
    def get_precision_map_dict(section):
        '''Extracts the precision map values from the psyclone.cfg file
        and converts them to a dictionary with integer values.

        :returns: The precision maps to be used by this API.
        :rtype: dict[str, int]
        '''
        precisions_list = section.getlist("precision_map")
        return_dict = {}
        return_dict = BaseConfig.create_dict_from_list(precisions_list)

        for key, value in return_dict.items():
            # isdecimal returns True if all the characters are decimals (0-9).
            # isdigit returns True if all characters are digits (this excludes
            # special characters such as the decimal point).
            if value.isdecimal() and value.isdigit():
                return_dict[key] = int(value)
            else:
                # Raised when key contains special characters or letters:
                raise ConfigurationError(
                    f"Wrong type supplied to mapping: '{value}'"
                    f" is not a positive integer or contains special"
                    f" characters.")
        return return_dict

    def get_access_mapping(self):
        '''Returns the mapping of API-specific access strings (e.g.
        gh_write) to the AccessType (e.g. AccessType.WRITE).
        :returns: The access mapping to be used by this API.
        :rtype: Dictionary of strings
        '''
        return self._access_mapping

    def get_reverse_access_mapping(self):
        '''Returns the reverse mapping of a PSyclone internal access type
        to the API specific string, e.g.: AccessType.READ to 'gh_read'.
        This is used to provide the user with API specific error messages.
        :returns: The mapping of access types to API-specific strings.
        :rtype: Dictionary of strings
        '''
        return self._reverse_access_mapping

    def get_valid_accesses_api(self):
        '''Returns the sorted, API-specific names of all valid access
        names.
        :returns: Sorted list of API-specific valid strings.
        :rtype: List of strings
        '''
        valid_names = list(self._access_mapping.keys())
        valid_names.sort()
        return valid_names

    @abc.abstractmethod
    def get_constants(self):
        ''':returns: an object containing all constants for the API.
        :rtype: :py:class:`psyclone.domain.lfric.LFRicConstants` |
            :py:class:`psyclone.domain.gocean.GOceanConstants`
        '''


# =============================================================================
class LFRicConfig(BaseConfig):
    '''
    LFRic-specific (Dynamo 0.3) Config sub-class. Holds configuration options
    specific to the LFRic (Dynamo 0.3) API.

    :param config: the 'parent' Config object.
    :type config: :py:class:`psyclone.configuration.Config`
    :param section: the entry for the '[lfric]' section of \
                    the configuration file, as produced by ConfigParser.
    :type section: :py:class:`configparser.SectionProxy`

    :raises ConfigurationError: for a missing mandatory configuration option.
    :raises ConfigurationError: for an invalid option for the redundant \
                                computation over annexed dofs.
    :raises ConfigurationError: for an invalid run_time_checks flag.
    :raises ConfigurationError: if argument datatypes in the 'default_kind' \
                                mapping do not match the supported datatypes.
    :raises ConfigurationError: for an invalid argument kind.
    :raises ConfigurationError: for an invalid value type of NUM_ANY_SPACE.
    :raises ConfigurationError: if the supplied number of ANY_SPACE \
                                function spaces is less than or equal to 0.
    :raises ConfigurationError: for an invalid value type of \
                                NUM_ANY_DISCONTINUOUS_SPACE.
    :raises ConfigurationError: if the supplied number of \
                                ANY_DISCONTINUOUS_SPACE function \
                                spaces is less than or equal to 0.

    '''
    # pylint: disable=too-few-public-methods, too-many-instance-attributes
    def __init__(self, config, section):
        super().__init__(section)
        # Ref. to parent Config object
        self._config = config
        # Initialise redundant computation setting
        self._compute_annexed_dofs = None
        # Initialise run_time_checks setting
        self._run_time_checks = None
        # Initialise LFRic datatypes' default kinds (precisions) settings
        self._supported_fortran_datatypes = []
        self._default_kind = {}
        self._precision_map = {}
        # Number of ANY_SPACE and ANY_DISCONTINUOUS_SPACE function spaces
        self._num_any_space = None
        self._num_any_discontinuous_space = None

        # Define and check mandatory keys
        self._mandatory_keys = ["access_mapping",
                                "compute_annexed_dofs",
                                "supported_fortran_datatypes",
                                "default_kind",
                                "precision_map",
                                "run_time_checks",
                                "num_any_space",
                                "num_any_discontinuous_space"]
        mdkeys = set(self._mandatory_keys)
        if not mdkeys.issubset(set(section.keys())):
            raise ConfigurationError(
                f"Missing mandatory configuration option in the "
                f"'[{section.name}]' section of the configuration file "
                f"'{config.filename}'. Valid options are: "
                f"{self._mandatory_keys}.")

        # Parse setting for redundant computation over annexed dofs
        try:
            self._compute_annexed_dofs = section.getboolean(
                "compute_annexed_dofs")
        except ValueError as err:
            raise ConfigurationError(
                f"Error while parsing COMPUTE_ANNEXED_DOFS in the "
                f"'[{section.name}]' section of the configuration file "
                f"'{config.filename}': {str(err)}.",
                config=self._config) from err

        # Parse setting for run_time_checks flag
        try:
            self._run_time_checks = section.getboolean(
                "run_time_checks")
        except ValueError as err:
            raise ConfigurationError(
                f"Error while parsing RUN_TIME_CHECKS in the "
                f"'[{section.name}]' section of the configuration file "
                f"'{config.filename}': {str(err)}.",
                config=self._config) from err

        # Parse setting for the supported Fortran datatypes. No
        # need to check whether the keyword is found as it is
        # mandatory (and therefore already checked).
        self._supported_fortran_datatypes = section.getlist(
            "supported_fortran_datatypes")

        # Parse setting for default kinds (precisions). No need to
        # check whether the keyword is found as it is mandatory
        # (and therefore already checked).
        kind_list = section.getlist("default_kind")
        all_kinds = self.create_dict_from_list(kind_list)
        # Set default kinds (precisions) from config file
        # Check for valid datatypes (filter to remove empty values)
        datatypes = set(filter(None, all_kinds.keys()))
        if datatypes != set(self._supported_fortran_datatypes):
            raise ConfigurationError(
                f"Fortran datatypes in the 'default_kind' mapping in the "
                f"'[{section.name}]' section of the configuration file "
                f"'{config.filename}' do not match the supported Fortran "
                f"datatypes {self._supported_fortran_datatypes}.")
        # Check for valid kinds (filter to remove any empty values)
        datakinds = set(filter(None, all_kinds.values()))
        if len(datakinds) != len(set(self._supported_fortran_datatypes)):
            raise ConfigurationError(
                f"Supplied kind parameters {sorted(datakinds)} in the "
                f"'[{section.name}]' section of the configuration file "
                f"'{config.filename}' do not define the default kind for "
                f"one or more supported datatypes "
                f"{self._supported_fortran_datatypes}.")
        self._default_kind = all_kinds

        # Parse setting for default precision map values.
        all_precisions = self.get_precision_map_dict(section)
        self._precision_map = all_precisions

        # Parse setting for the number of ANY_SPACE function spaces
        # (check for an invalid value and numbers <= 0)
        try:
            self._num_any_space = section.getint("NUM_ANY_SPACE")
        except ValueError as err:
            raise ConfigurationError(
                f"Error while parsing NUM_ANY_SPACE in the '[{section.name}]' "
                f"section of the configuration file '{config.filename}': "
                f"{str(err)}.", config=self._config) from err

        if self._num_any_space <= 0:
            raise ConfigurationError(
                f"The supplied number of ANY_SPACE function spaces "
                f"in the '[{section.name}]' section of the configuration "
                f"file '{config.filename}' must be greater than 0 but found "
                f"{self._num_any_space}.")

        # Parse setting for the number of ANY_DISCONTINUOUS_SPACE
        # function spaces (checks for an invalid value and numbers <= 0)
        try:
            self._num_any_discontinuous_space = section.getint(
                "NUM_ANY_DISCONTINUOUS_SPACE")
        except ValueError as err:
            raise ConfigurationError(
                f"Error while parsing NUM_ANY_DISCONTINUOUS_SPACE in the "
                f"'[{section.name}]' section of the configuration file "
                f"'{config.filename}': {str(err)}.",
                config=self._config) from err

        if self._num_any_discontinuous_space <= 0:
            raise ConfigurationError(
                f"The supplied number of ANY_DISCONTINUOUS_SPACE function "
                f"spaces in the '[{section.name}]' section of the "
                f"configuration file '{config.filename}' must be greater than "
                f"0 but found {self._num_any_discontinuous_space}.")

    @property
    def compute_annexed_dofs(self):
        '''
        Getter for whether or not we perform redundant computation over
        annexed dofs.

        :returns: true if we are to do redundant computation.
        :rtype: bool

        '''
        return self._compute_annexed_dofs

    @property
    def run_time_checks(self):
        '''
        Getter for whether or not we generate run-time checks in the code.

        :returns: true if we are generating run-time checks
        :rtype: bool

        '''
        return self._run_time_checks

    @property
    def supported_fortran_datatypes(self):
        '''
        Getter for the supported Fortran argument datatypes in LFRic.

        :returns: supported Fortran datatypes for LFRic arguments.
        :rtype: list of str

        '''
        return self._supported_fortran_datatypes

    @property
    def default_kind(self):
        '''
        Getter for default kind (precision) for real, integer and logical
        datatypes in LFRic.

        :returns: the default kinds for main datatypes in LFRic.
        :rtype: dict of str

        '''
        return self._default_kind

    @property
    def precision_map(self):
        '''
        Getter for precision map values for supported fortran datatypes
        in LFRic. (Precision in bytes indexed by the name of the LFRic
        kind parameter).

        :returns: the precision map values for main datatypes in LFRic.
        :rtype: dict[str, int]

        '''
        return self._precision_map

    @property
    def num_any_space(self):
        '''
        :returns: the number of ANY_SPACE function spaces in LFRic.
        :rtype: int

        '''
        return self._num_any_space

    @property
    def num_any_discontinuous_space(self):
        '''
        :returns: the number of ANY_DISCONTINUOUS_SPACE function \
                  spaces in LFRic.
        :rtype: int

        '''
        return self._num_any_discontinuous_space

    def get_constants(self):
        ''':returns: an object containing all constants for the API.
        :rtype: :py:class:`psyclone.domain.lfric.LFRicConstants`
        '''
        # Avoid circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.domain.lfric import LFRicConstants

        return LFRicConstants()


# =============================================================================
class GOceanConfig(BaseConfig):
    '''Gocean1.0-specific Config sub-class. Holds configuration options
    specific to the GOcean 1.0 API.

    :param config: The 'parent' Config object.
    :type config: :py:class:`psyclone.configuration.Config`
    :param section: The entry for the gocean section of \
                    the configuration file, as produced by ConfigParser.
    :type section:  :py:class:`configparser.SectionProxy`

    '''
    # pylint: disable=too-few-public-methods, too-many-branches
    def __init__(self, config, section):
        # pylint: disable=too-many-locals
        super().__init__(section)
        # Setup the mapping for the grid properties. This dictionary stores
        # the name of the grid property as key (e.g. ``go_grid_dx_t``),
        # with the value being a named tuple with an entry for 'property'
        # and 'type'. The 'property' is a format string to dereference
        # a property, and 'type' is a string.
        # These values are taken from the psyclone config file.
        self._grid_properties = {}
        # Initialise debug_mode settings (default to False if it doesn't exist)
        self._debug_mode = False
        for key in section.keys():
            # Do not handle any keys from the DEFAULT section
            # since they are handled by Config(), not this class.
            if key in config.get_default_keys():
                continue
            if key == "iteration-spaces":
                # The value associated with the iteration-spaces key is a
                # set of lines, each line defining one new iteration space.
                # Each individual iteration space added is checked
                # in add_bounds for correctness.
                value_as_str = str(section[key])
                new_iteration_spaces = value_as_str.split("\n")
                # Avoid circular import
                # pylint: disable=import-outside-toplevel
                from psyclone.gocean1p0 import GOLoop
                for it_space in new_iteration_spaces:
                    GOLoop.add_bounds(it_space)
            elif key == "access_mapping":
                # Handled in the base class BaseConfig
                pass
            elif key == "debug_mode":
                # Boolean that specifies if debug mode is enabled
                try:
                    self._debug_mode = section.getboolean("debug_mode")
                except ValueError as err:
                    raise ConfigurationError(
                        f"error while parsing DEBUG_MODE in the [gocean1p0] "
                        f"section of the config file: {err}") from err
            elif key == "grid-properties":
                # Grid properties have the format:
                # go_grid_area_u: {0}%%grid%%area_u: array: real,
                # First the name, then the Fortran code to access the property,
                # followed by the type ("array" or "scalar") and then the
                # intrinsic type ("integer" or "real")
                all_props = self.create_dict_from_list(section.getlist(key))
                for grid_property, property_str in all_props.items():
                    try:
                        fortran, variable_type, intrinsic_type = \
                            property_str.split(":")
                    except ValueError as err:
                        # Raised when the string does not contain exactly
                        # three values separated by ":"
                        error = (f"Invalid property '{grid_property}' found "
                                 f"with value '{property_str}' in "
                                 f"'{config.filename}'. It must have exactly "
                                 f"three ':'-delimited separated values: the "
                                 f"property, whether it is a scalar or an "
                                 f"array, and the intrinsic type (real or "
                                 f"integer).")
                        raise ConfigurationError(error) from err
                    # Make sure to remove the spaces which the config
                    # file might contain
                    self._grid_properties[grid_property] = \
                        GOceanConfig.make_property(fortran.strip(),
                                                   variable_type.strip(),
                                                   intrinsic_type.strip())
                # Check that the required values for xstop and ystop
                # are defined:
                for required in ["go_grid_xstop", "go_grid_ystop",
                                 "go_grid_data",
                                 "go_grid_internal_inner_start",
                                 "go_grid_internal_inner_stop",
                                 "go_grid_internal_outer_start",
                                 "go_grid_internal_outer_stop",
                                 "go_grid_whole_inner_start",
                                 "go_grid_whole_inner_stop",
                                 "go_grid_whole_outer_start",
                                 "go_grid_whole_outer_stop"]:
                    if required not in self._grid_properties:
                        error = (f"The config file {config.filename} does not "
                                 f"contain values for the following, mandatory"
                                 f" grid property: '{required}'.")
                        raise ConfigurationError(error)
            else:
                raise ConfigurationError(f"Invalid key '{key}' found in "
                                         f"'{config.filename}'.")

    # ---------------------------------------------------------------------
    @staticmethod
    def make_property(dereference_format, type_name, intrinsic_type):
        '''Creates a property (based on namedtuple) for a given Fortran
        code to access a grid property, and the type.

        :param str dereference_format: The Fortran code to access a property \
            given a field name (which will be used to replace a {0} in the \
            string. E.g. "{0}%whole%xstop").
        :param str type_name: The type of the grid property, must be \
            'scalar' or 'array'.
        :param str intrinsic_type: The intrinsic type of the grid property, \
            must be 'integer' or 'real'.

        :returns: a namedtuple for a grid property given the Fortran
            statement to access it and the type.

        :raises InternalError: if type_name is not 'scalar' or 'array'
        :raises InternalError: if intrinsic_type is not 'integer' or 'real'
        '''
        if type_name not in ['array', 'scalar']:
            raise InternalError(f"Type must be 'array' or 'scalar' but is "
                                f"'{type_name}'.")

        if intrinsic_type not in ['integer', 'real']:
            raise InternalError(f"Intrinsic type must be 'integer' or 'real' "
                                f"but is '{intrinsic_type}'.")

        Property = namedtuple("Property", "fortran type intrinsic_type")
        return Property(dereference_format, type_name, intrinsic_type)

    # ---------------------------------------------------------------------
    @property
    def grid_properties(self):
        ''':returns: the dict containing the grid properties.
        :rtype: a dict with values of \
            namedtuple("Property","fortran type intrinsic_type") instances.
        '''
        return self._grid_properties

    # ---------------------------------------------------------------------
    @property
    def debug_mode(self):
        '''
        :returns: whether we are generating additional debug code.
        :rtype: bool

        '''
        return self._debug_mode

    # ---------------------------------------------------------------------
    def get_constants(self):
        ''':returns: an object containing all constants for GOcean.
        :rtype: :py:class:`psyclone.domain.gocean.GOceanConstants`
        '''
        # Avoid circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.domain.gocean import GOceanConstants
        return GOceanConstants()


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for. (See https://psyclone-ref.readthedocs.io)
__all__ = ["BaseConfig",
           "Config",
           "ConfigurationError",
           "LFRicConfig",
           "GOceanConfig"]
