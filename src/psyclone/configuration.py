# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2019, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''
PSyclone configuration management module.

Deals with reading the config file and storing default settings.
'''

from __future__ import absolute_import, print_function
import os


# Name of the config file we search for
_FILE_NAME = "psyclone.cfg"

# The different naming schemes supported when transforming kernels:
# multiple = Every transformed kernel is given a unique name. This permits
#            multiple versions of the same kernel to be created.
# single = If any given kernel (within a single Application) is transformed
#          more than once then the same transformation must always be
#          applied and only one version of the transformed kernel is created.
VALID_KERNEL_NAMING_SCHEMES = ["multiple", "single"]


class ConfigurationError(Exception):
    '''
    Class for all configuration-related errors.

    :param str value: error message.
    :param config: the Config object associated with the error.
    :type config: :py:class:`psyclone.configuration.Config`.
    '''
    def __init__(self, value, config=None):
        Exception.__init__(self, value)
        self.value = "PSyclone configuration error"
        if config:
            self.value += " (file={0})".format(config.filename)
        self.value += ": "+value

    def __str__(self):
        return repr(self.value)


# =============================================================================
class Config(object):
    # pylint: disable=too-many-instance-attributes
    '''
    Handles all configuration management. It is implemented as a singleton
    using a class _instance variable and a get() function.
    '''
    # Class variable to store the singleton instance
    _instance = None

    # List of supported API by PSyclone
    _supported_api_list = ["dynamo0.1", "dynamo0.3",
                           "gocean0.1", "gocean1.0", "nemo"]

    # List of supported stub API by PSyclone
    _supported_stub_api_list = ["dynamo0.3"]

    # The default API, i.e. the one to be used if neither a command line
    # option is specified nor is the API in the config file used.
    _default_api = u"dynamo0.3"

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
        :raises GenerationError: If a singleton instance of Config already \
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

        # The API selected by the user - either on the command line,
        # or in the config file (or the default if neither).
        self._api = None

        # The default stub API to use.
        self._default_stub_api = None

        # True if distributed memory code should be created/
        self._distributed_mem = None

        # True if reproducible reductions should be used.
        self._reproducible_reductions = None

        # Padding size (number of array elements) to be used when
        # reproducible reductions are created.
        self._reprod_pad_size = None

        # Where to write transformed kernels - set at runtime
        self._kernel_output_dir = None

        # The naming scheme to use for transformed kernels
        self._kernel_naming = None

        # The list of directories to search for Fortran include files
        self._include_paths = []

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
                    "File {0} does not exist".format(config_file))
            self._config_file = config_file[:]
        else:
            # Search for the config file in various default locations
            self._config_file = Config.find_file()
        from configparser import ConfigParser, MissingSectionHeaderError
        self._config = ConfigParser()
        try:
            self._config.read(self._config_file)
        except MissingSectionHeaderError as err:
            raise ConfigurationError(
                "ConfigParser failed to read the configuration file. Is it "
                "formatted correctly? (Error was: {0})".format(str(err)),
                config=self)

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
                "error while parsing DISTRIBUTED_MEMORY: {0}".
                format(str(err)), config=self)

        # API for psyclone
        if "API" in self._config["DEFAULT"]:
            self._api = self._config['DEFAULT']['API']
        else:
            self._api = Config._default_api
            # Test if we have exactly one section (besides DEFAULT).
            # If so, make this section the API (otherwise stick with
            # the default API)
            if len(self._config) == 2:
                for section in self._config:
                    self._api = section.lower()
                    if self._api != "default":
                        break

        # Sanity check
        if self._api not in Config._supported_api_list:
            raise ConfigurationError(
                "The API ({0}) is not in the list of supported "
                "APIs ({1}).".format(self._api,
                                     Config._supported_api_list),
                config=self)

        # Default API for stub-generator
        if 'defaultstubapi' not in self._config['DEFAULT']:
            # Use the default API if no default is specified for stub API
            self._default_stub_api = Config._default_api
        else:
            self._default_stub_api = self._config['DEFAULT']['DEFAULTSTUBAPI']

        # Sanity check for defaultstubapi:
        if self._default_stub_api not in Config._supported_stub_api_list:
            raise ConfigurationError(
                "The default stub API ({0}) is not in the list of "
                "supported stub APIs ({1}).".format(
                    self._default_stub_api,
                    Config._supported_stub_api_list),
                config=self)

        try:
            self._reproducible_reductions = self._config['DEFAULT'].getboolean(
                'REPRODUCIBLE_REDUCTIONS')
        except ValueError as err:
            raise ConfigurationError(
                "error while parsing REPRODUCIBLE_REDUCTIONS: {0}".
                format(str(err)), config=self)

        try:
            self._reprod_pad_size = self._config['DEFAULT'].getint(
                'REPROD_PAD_SIZE')
        except ValueError as err:
            raise ConfigurationError(
                "error while parsing REPROD_PAD_SIZE: {0}".format(str(err)),
                config=self)

        # Now we deal with the API-specific sections of the config file. We
        # create a dictionary to hold the API-specifc Config objects.
        self._api_conf = {}
        for api in Config._supported_api_list:
            if api in self._config:
                if api == "dynamo0.3":
                    self._api_conf[api] = DynConfig(self, self._config[api])
                elif api == "dynamo0.1":
                    # For now we use the same class as dynamo0.3.
                    # However, we use it to read a different section of the
                    # config file, so the dynamo0.1 mapping will be correctly
                    # used.
                    self._api_conf[api] = DynConfig(self, self._config[api])
                elif api == "gocean0.1":
                    # For now we use the same class as gocean1.0.
                    # However, we use it to read a different section of the
                    # config file
                    self._api_conf[api] = GOceanConfig(self, self._config[api])
                elif api == "gocean1.0":
                    self._api_conf[api] = GOceanConfig(self, self._config[api])
                elif api == "nemo":
                    self._api_conf[api] = NemoConfig(self, self._config[api])
                else:
                    raise NotImplementedError(
                        "Configuration file contains a {0} section but no "
                        "Config sub-class has been implemented for this API".
                        format(api))

        # The scheme to use when re-naming transformed kernels.
        # By default we ensure that each transformed kernel is given a
        # unique name (within the specified kernel-output directory).
        self._kernel_naming = Config._default_kernel_naming

    def api_conf(self, api=None):
        '''
        Getter for the object holding API-specific configuration options.

        :param str api: Optional, the API for which configuration details are
                required. If none is specified, returns the config for the
                default API.
        :returns: object containing API-specific configuration
        :rtype: One of :py:class:`psyclone.configuration.DynConfig`,
                :py:class:`psyclone.configuration.GOceanConfig` or None.

        :raises ConfigurationError: if api is not in the list of supported \
                                    APIs.
        :raises ConfigurationError: if the config file did not contain a \
                                    section for the requested API.
        '''

        if not api:
            return self._api_conf[self._api]

        if api not in self.supported_apis:
            raise ConfigurationError(
                "API '{0}' is not in the list '{1}'' of supported APIs."
                .format(api, self.supported_apis))
        if api not in self._api_conf:
            raise ConfigurationError(
                "Configuration file did not contain a section for the '{0}' "
                "API".format(api), config=self)
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

        :returns: the fully-qualified path to the configuration file
        :rtype: str

        :raises ConfigurationError: if no config file is found
        '''
        import sys
        from psyclone.virtual_utils import within_virtual_env

        # If $PSYCLONE_CONFIG is set then we use that unless the
        # file it points to does not exist
        _psy_config = os.environ.get('PSYCLONE_CONFIG')
        if _psy_config and os.path.isfile(_psy_config):
            return _psy_config

        # Set up list of locations to search
        share_dir = os.path.join(sys.prefix, "share", "psyclone")

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

        for cfile in [os.path.join(cdir, _FILE_NAME) for cdir in _file_paths]:
            if os.path.isfile(cfile):
                return cfile

        # If we get to here then we have failed to find a config file
        raise ConfigurationError("{0} not found in any of {1}".
                                 format(_FILE_NAME, _file_paths))

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
                "distributed_memory must be a boolean but got {0}".
                format(type(dist_mem)))
        self._distributed_mem = dist_mem

    @property
    def default_api(self):
        '''
        Getter for the default API used by PSyclone.

        :returns: default PSyclone API
        :rtype: str
        '''
        return self._default_api

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
        if api not in self._supported_api_list:
            raise ValueError("'{0}' is not a valid API, it must be one "
                             "of {1}'.".format(api,
                                               Config._supported_api_list))
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
    def default_stub_api(self):
        '''
        Getter for the default API used by the stub generator.

        :returns: default API for the stub generator
        :rtype: str
        '''
        return self._default_stub_api

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
                "kernel_naming must be one of '{0}' but got '{1}'".
                format(VALID_KERNEL_NAMING_SCHEMES, value))
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
                        "Include path '{0}' does not exist".format(path))
                self._include_paths.append(path)
        except (TypeError, ValueError):
            raise ValueError("include_paths must be a list but got: {0}".
                             format(type(path_list)))

    def get_default_keys(self):
        '''Returns all keys from the default section.
        :returns list: List of all keys of the default section as strings.
        '''
        return self._config.defaults()


# =============================================================================
class APISpecificConfig(object):
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
        # Get the mapping and convert it into a directory. The input is in
        # the format: key1:value1, key2=value2, ...
        mapping = section.get("ACCESS_MAPPING")
        if mapping:
            self._access_mapping = \
                APISpecificConfig.create_dict_from_string(mapping)
        # Now convert the string type ("read" etc) to AccessType
        from psyclone.core.access_type import AccessType
        for api_access_name, access_type in self._access_mapping.items():
            try:
                self._access_mapping[api_access_name] = \
                    AccessType.from_string(access_type)
            except ValueError:
                # Raised by from_string()
                raise ConfigurationError("Unknown access type '{0}' found "
                                         "for key '{1}'"
                                         .format(access_type,
                                                 api_access_name))

        # Now create the reverse lookup (for better error messages):
        self._reverse_access_mapping = {v: k for k, v in
                                        self._access_mapping.items()}

    @staticmethod
    def create_dict_from_string(input_str):
        '''Takes an input string in the format:
        key1:value1, key2:value2, ...
        and creates a dictionary with the key,value pairs.
        Spaces are removed.
        :param str input_str: The input string.
        :returns: A dictionary with the key,value pairs from the input string.
        :rtype: dict.
        :raises ConfigurationError: if the input string contains an entry \
                that does not have a ":".
        '''
        # Remove spaces and convert unicode to normal strings.
        input_str = str(input_str.strip())
        if not input_str:
            # Split will otherwise return a list with '' as only element,
            # which then raises an exception
            return {}

        entries = input_str.split(",")
        return_dict = {}
        for entry in entries:
            try:
                key, value = entry.split(":", 1)
            except ValueError:
                # Raised when split does not return two elements:
                raise ConfigurationError("Invalid format for mapping: {0}".
                                         format(entry.strip()))
            return_dict[key.strip()] = value.strip()
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


# =============================================================================
class DynConfig(APISpecificConfig):
    '''
    Dynamo0.3-specific Config sub-class. Holds configuration options specific
    to the Dynamo 0.3 API.

    :param config: The 'parent' Config object.
    :type config: :py:class:`psyclone.configuration.Config`
    :param section: The entry for the dynamo0.3 section of \
                    the configuration file, as produced by ConfigParser.
    :type section:  :py:class:`configparser.SectionProxy`
    '''
    # pylint: disable=too-few-public-methods
    def __init__(self, config, section):
        super(DynConfig, self).__init__(section)
        self._config = config  # Ref. to parent Config object
        try:
            self._compute_annexed_dofs = section.getboolean(
                'COMPUTE_ANNEXED_DOFS')
        except ValueError as err:
            raise ConfigurationError(
                "error while parsing COMPUTE_ANNEXED_DOFS in the [dynamo0.3] "
                "section of the config file: {0}".format(str(err)),
                config=self._config)

    @property
    def compute_annexed_dofs(self):
        '''
        Getter for whether or not we perform redundant computation over
        annexed dofs.
        :returns: True if we are to do redundant computation
        :rtype: False

        '''
        return self._compute_annexed_dofs


# =============================================================================
class GOceanConfig(APISpecificConfig):
    '''Gocean1.0-specific Config sub-class. Holds configuration options
    specific to the GOcean 1.0 API.

    :param config: The 'parent' Config object.
    :type config: :py:class:`psyclone.configuration.Config`
    :param section: The entry for the gocean1.0 section of \
                    the configuration file, as produced by ConfigParser.
    :type section:  :py:class:`configparser.SectionProxy`

    '''
    # pylint: disable=too-few-public-methods
    def __init__(self, config, section):
        super(GOceanConfig, self).__init__(section)
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
                from psyclone.gocean1p0 import GOLoop
                for it_space in new_iteration_spaces:
                    GOLoop.add_bounds(it_space)
            elif key == "access_mapping":
                # Handled in the base class APISpecificConfig
                pass
            else:
                raise ConfigurationError("Invalid key \"{0}\" found in "
                                         "\"{1}\".".format(key,
                                                           config.filename))


# =============================================================================
class NemoConfig(APISpecificConfig):
    '''Nemo-specific Config sub-class. Holds configuration options
    specific to the Nemo API.

    :param config: The 'parent' Config object.
    :type config: :py:class:`psyclone.configuration.Config`
    :param section: The entry for the NEMO section of \
                    the configuration file, as produced by ConfigParser.
    :type section:  :py:class:`configparser.SectionProxy`

    '''
    # pylint: disable=too-few-public-methods
    def __init__(self, config, section):
        super(NemoConfig, self).__init__(section)

        # Maps a variable name to lon, lat etc. to determine the loop type
        # (e.g. lon, lat, ...)
        self._loop_type_mapping = {}

        # Maps a loop type (lon, ...) to a dictionary containing the
        # corresponding variable name and start/stop values.
        self._loop_type_data = {}

        # The order in which loops should be created in NemoExplicitLoopTrans
        self._index_order = []

        # This is used to detect if a variable name is duplicated in
        # mapping-* keys:
        var_defined = []
        for key in section.keys():
            # Do not handle any keys from the DEFAULT section
            # since they are handled by Config(), not this class.
            if key in config.get_default_keys():
                continue

            # Handle the definition of variables
            if key[:8] == "mapping-":
                loop_type = key[8:]
                data = self.create_dict_from_string(section[key])
                # Make sure the required keys exist:
                for subkey in ["var", "start", "stop"]:
                    if subkey not in data:
                        raise ConfigurationError("mapping-'{0}' does not "
                                                 "contain key '{1}' "
                                                 "in file '{2}'."
                                                 .format(loop_type, subkey,
                                                         config.filename))

                var = data['var']
                if var in var_defined:
                    raise ConfigurationError("mapping-{0} defines variable "
                                             "\"{1}\" again in the \"nemo\" "
                                             "section of the file \"{2}\"."
                                             .format(loop_type, var,
                                                     config.filename))
                var_defined.append(var)

                # Update the mapping of variable to loop type
                self._loop_type_mapping[var] = loop_type
                # And the mapping of loop type to the remaining data
                self._loop_type_data[loop_type] = data

            elif key == "index-order":
                self._index_order = [loop.strip() for
                                     loop in section[key].split(",")]

            else:
                raise ConfigurationError("Invalid key \"{0}\" found in "
                                         "the \"nemo\" section of the "
                                         "configuration file \"{1}\"."
                                         .format(key, config.filename))
        # Consistency test: any value in index-order must have a
        # corresponding key in valid_loop_types:
        for loop_type in self._index_order:
            if loop_type not in self._loop_type_data:
                valid = str(list(self._loop_type_data.keys()))
                raise ConfigurationError("Invalid loop type \"{0}\" found in "
                                         "index-order in \"{1}\".\n"
                                         "Must be one of {2}."
                                         .format(loop_type, config.filename,
                                                 valid))

    def get_loop_type_mapping(self):
        '''
        :returns: the mapping of variable names to loop type.
        :rtype: Dictionary of strings.
        '''
        return self._loop_type_mapping

    def get_loop_type_data(self):
        '''
        :returns: the mapping of a loop type (lon, ...) to a dictionary \
            containing the corresponding variable name and start/stop values.\
            Example: = {"lon": {"var": "ji", "start": "1", "stop": "jpi"}, \
-                       "lat": {"var": "jj", "start": "1", "stop": "jpj"} }

        :rtype: dictionary with str keys, with each value being a \
            dictionary mapping 'var', 'start', and 'stop' to str.
        '''
        return self._loop_type_data

    def get_valid_loop_types(self):
        '''
        :returns: a list of valid loop types.
        :rtype: list of str.
        '''
        return list(self._loop_type_data)

    def get_index_order(self):
        '''
        :returns: the order in which loops should be created in \
            NemoExplicitLoopTrans.
        :rtype: list of str.
        '''
        return self._index_order
