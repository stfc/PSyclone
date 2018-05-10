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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

# Specify whether we compute annexed dofs when a kernel is written so
# that it iterates over dofs. This is currently only the case for
# builtins. If annexed dofs are computed then in certain cases we
# remove the need for a halo exchange call.
COMPUTE_ANNEXED_DOFS = False


'''
PSyclone configuration management module.

Deals with reading the config file and storing default settings.
'''

import os


class ConfigurationError(Exception):
    '''
    Class for all configuration-related errors.
    '''
    def __init__(self, value, config=None):
        '''
        :param str value: error message
        :param config: the Config object associated with the error
        :type :py:class:`psyclone.configuration.Config`
        '''
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
    else:
        # Space delimited
        return [
            str(item.strip()) for item in svalue.split(" ")
            if item.strip() != '']


class ConfigFactory(object):
    '''
    Creates a singleton instance of our Config object.
    '''
    _instance = None  # Our single Config object

    def __init__(self, config_file=None):
        '''
        Create our singleton Config object. If config_name is specified
        then we throw-away the old Config and create a new one.

        :param str config_file: Specific configuration file to use when
                                creating Config object
        '''
        if not ConfigFactory._instance or config_file:
            # Create a Config object if we've not already got one or if the
            # caller has specified a particular file
            ConfigFactory._instance = Config(config_file)

    def create(self):
        '''
        :returns: the singleton Config instance
        :rtype: :py:class:`psyclone.config.Config`
        '''
        return ConfigFactory._instance


class Config(object):
    '''
    Handles all configuration management.
    '''

    def __init__(self, config_file=None):
        '''
        Config constructor.

        :param str config_file: Override default configuration file to read

        :raises ConfigurationError: if there are errors or inconsistencies in
                                    the specified config file
        '''
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

        from configparser import ConfigParser
        self._config = ConfigParser()
        self._config.read(self._config_file)

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
                format(err.message), config=self)

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
                format(err.message), config=self)

        try:
            self._reprod_pad_size = self._config['DEFAULT'].getint(
                'REPROD_PAD_SIZE')
        except ValueError as err:
            raise ConfigurationError(
                "error while parsing REPROD_PAD_SIZE: {0}".format(err.message),
                config=self)


    @staticmethod
    def find_file(name=None):
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

        :param str name: override default name of config file to search for
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

        # Name of the config file we search for
        if name is not None:
            _name = name[:]
        else:
            _name = "psyclone.cfg"

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

        for cfile in [os.path.join(cdir, _name) for cdir in _file_paths]:
            if os.path.isfile(cfile):
                return cfile

        # If we get to here then we have failed to find a config file
        raise ConfigurationError("{0} not found in any of {1}".
                                 format(_name, _file_paths))

    @property
    def distributed_memory(self):
        return self._distributed_mem

    @distributed_memory.setter
    def distributed_memory(self, dist_mem):
        self._distributed_mem = dist_mem

    @property
    def default_api(self):
        return self._default_api

    @property
    def supported_apis(self):
        return self._supported_api_list

    @property
    def default_stub_api(self):
        return self._default_stub_api

    @property
    def supported_stub_apis(self):
        return self._supported_stub_api_list

    @property
    def reproducible_reductions(self):
        return self._reproducible_reductions

    @property
    def reprod_pad_size(self):
        return self._reprod_pad_size

    @property
    def filename(self):
        return self._config_file
