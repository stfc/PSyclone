# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018, Science and Technology Facilities Council
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

from __future__ import absolute_import
import os


# Name of the config file we search for
_FILE_NAME = "psyclone.cfg"


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


def _str_to_list(svalue):
    '''
    Helper routine to take a string containing a list of values and return
    a list of those values.

    :param str svalue: string containing space- or comma-delimited list
                       of items
    :returns: list of strings
    :rtype: list
    '''
    if "," in svalue:
        # Comma delimited
        return [
            str(item.strip()) for item in svalue.split(",")
            if item.strip() != '']
    # Space delimited
    return [
        str(item.strip()) for item in svalue.split(" ")
        if item.strip() != '']


class ConfigFactory(object):
    '''
    Create our singleton Config object. If config_file is specified
    then we throw-away the old Config and create a new one.

    :param str config_file: Specific configuration file to use when \
                            creating the Config object.
    '''
    _instance = None  # Our single Config object

    def __init__(self, config_file=None):
        if not ConfigFactory._instance or config_file:
            # Create a Config object if we've not already got one or if the
            # caller has specified a particular file
            ConfigFactory._instance = Config(config_file)

    @staticmethod
    def create():
        '''
        :returns: the singleton Config instance
        :rtype: :py:class:`psyclone.config.Config`
        '''
        return ConfigFactory._instance


class Config(object):
    '''
    Handles all configuration management.

    :param str config_file: Override default configuration file to read.
    :raises ConfigurationError: if there are errors or inconsistencies in \
                                the specified config file.
    '''
    def __init__(self, config_file=None):
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

        # Default API and supported APIs for psyclone
        self._default_api = self._config['DEFAULT']['DEFAULTAPI']

        self._supported_api_list = _str_to_list(
            self._config['DEFAULT']['SUPPORTEDAPIS'])

        # Sanity check
        if self._default_api not in self._supported_api_list:
            raise ConfigurationError(
                "The default API ({0}) is not in the list of supported "
                "APIs ({1}).".format(self._default_api,
                                     self._supported_api_list),
                config=self)

        # Default API and supported APIs for stub-generator
        self._default_stub_api = self._config['DEFAULT']['DEFAULTSTUBAPI']

        self._supported_stub_api_list = _str_to_list(
            self._config['DEFAULT']['SUPPORTEDSTUBAPIS'])

        # Sanity check
        if self._default_stub_api not in self._supported_stub_api_list:
            raise ConfigurationError(
                "The default stub API ({0}) is not in the list of "
                "supported stub APIs ({1}).".format(
                    self._default_stub_api,
                    self._supported_stub_api_list),
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
        self._api = {}
        for api in self._supported_api_list:
            if api in self._config:
                if api == "dynamo0.3":
                    self._api[api] = DynConfig(self, self._config[api])
                else:
                    raise NotImplementedError(
                        "Configuration file contains a {0} section but no "
                        "Config sub-class has been implemented for this API".
                        format(api))

    def api(self, api):
        '''
        Getter for the object holding API-specific configuration options.

        :param str api: the API for which configuration details are required.
        :returns: object containing API-specific configuration
        :rtype: One of :py:class:`psyclone.configuration.DynConfig` or None.

        :raises ConfigurationError: if api is not in the list of supported \
                                    APIs.
        :raises ConfigurationError: if the config file did not contain a \
                                    section for the requested API.
        '''
        if api not in self.supported_apis:
            raise ConfigurationError(
                "API '{0}' is not one of the supported APIs listed in the "
                "configuration file ({1}).".format(api, self.supported_apis),
                config=self)
        if api not in self._api:
            raise ConfigurationError(
                "Configuration file did not contain a section for the '{0}' "
                "API".format(api), config=self)
        return self._api[api]

    @staticmethod
    def find_file():
        '''
        Static method that searches various locations for a configuration
        file. If the full path to an existing file has been provided in
        the PSYCLONE_CONFIG environment variable then that is returned.
        Otherwise, we search the following locations, in order:
        ${PWD}/.psyclone/
        if inside-a-virtual-environment:
            <base-dir-of-virtual-env>/share/psyclone/
        ${HOME}/.local/share/psyclone/
        <system-install-prefix>/share/psyclone/

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
    def supported_apis(self):
        '''
        Getter for the list of APIs supported by PSyclone.
        :returns: list of supported APIs
        :rtype: list of str
        '''
        return self._supported_api_list

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
        return self._supported_stub_api_list

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


class DynConfig(object):
    '''
    Dynamo0.3-specific Config sub-class. Holds configuration options specific
    to the Dynamo 0.3 API.

    :param config: The 'parent' Config object.
    :type config: :py:class:`psyclone.configuration.Config`
    :param section: The entry for the dynamo0.3 section of \
                    the configuration file, as produced by ConfigParser.
    :type section:  :py:class:`configparser.SectionProxy`

    '''
    def __init__(self, config, section):

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
