# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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
# Author J. Henrichs, Bureau of Meteorology

'''This module contains a singleton class that manages information about
which module is contained in which file (including full location). '''


import os

from psyclone.errors import InternalError
from psyclone.parse.module_info import ModuleInfo


class ModuleManager:
    '''This class implements a singleton that manages module
    dependencies.

    '''
    # Class variable to store the singleton instance
    _instance = None

    # ------------------------------------------------------------------------
    @staticmethod
    def get():
        '''Static function that if necessary creates and returns the singleton
        ModuleManager instance.

        '''
        if not ModuleManager._instance:
            ModuleManager._instance = ModuleManager()
        return ModuleManager._instance

    # ------------------------------------------------------------------------
    def __init__(self):

        if ModuleManager._instance is not None:
            raise InternalError("You need to use 'ModuleManager.get()' "
                                "to get the singleton instance.")
        self._cached_module_info = {}
        self._mod_2_filename = {}
        self._search_paths = []

    # ------------------------------------------------------------------------
    def add_search_path(self, directories, recursive=True):
        '''If the directory is not already contained in the search path,
        add it. Directory can either be a string, in which case it is a single
        directory, or a list of directories, each one a string.

        :param directories: the directory/directories to add.
        :type directories: Union[str, List[str]]

        :param bool recursive: whether recursively all subdirectories should \
            be added to the search path.

        '''
        if isinstance(directories, str):
            # Make sure we always have a list
            directories = [directories]

        for directory in directories:
            if not os.access(directory, os.R_OK):
                raise IOError(f"Directory '{directory}' does not exist.")
            if directory not in self._search_paths:
                self._search_paths.append(directory)
            if not recursive:
                break
            for root, dirs, _ in os.walk(directory):
                for directory in dirs:
                    new_dir = os.path.join(root, directory)
                    if new_dir not in self._search_paths:
                        self._search_paths.append(new_dir)

    # ------------------------------------------------------------------------
    def _add_all_files_from_dir(self, directory):
        '''This function adds all files with an extension of (F/f/X/x)90 in the
        given directory to the mapping of module names to file names. The
        module names are based on the filename using `get_modules_in_file()`.
        By default it is assumed that `a_mod.f90` contains the module `a_mod`.

        :param str directory: the directory to list all files from
        '''

        with os.scandir(directory) as all_entries:
            for entry in all_entries:
                _, ext = os.path.splitext(entry.name)
                if (not entry.is_file()) or \
                        ext not in [".F90", ".f90", ".X90", ".x90"]:
                    continue
                full_path = os.path.join(directory, entry.name)
                all_modules = self.get_modules_in_file(full_path)
                for module in all_modules:
                    if module not in self._mod_2_filename:
                        mod_info = ModuleInfo(module, full_path)
                        self._mod_2_filename[module] = mod_info

    # ------------------------------------------------------------------------
    def get_module_info(self, module_name):
        '''This function returns the ModuleInformation for the specified
        module.

        :param str module_name: name of the module.

        :returns: the filename that contains the module
        :rtype: str

        :raises FileNotFoundError: if the module_name is not found in \
            either the cached data nor in the search path.

        '''
        mod_lower = module_name.lower()

        # First check if we already know about this file:
        mod_info = self._mod_2_filename.get(mod_lower, None)
        if mod_info:
            return mod_info

        # If not, check the search paths. To avoid frequent accesses to
        # the directories, we search directories one at a time, and
        # add the list of all files in that directory to our cache
        # _mod_2_filename

        search_paths_copy = self._search_paths.copy()

        for directory in search_paths_copy:
            self._add_all_files_from_dir(directory)
            self._search_paths.remove(directory)
            mod_info = self._mod_2_filename.get(mod_lower, None)
            if mod_info:
                return mod_info

        raise FileNotFoundError(f"Could not find source file for module "
                                f"'{module_name}'.")

    # ------------------------------------------------------------------------
    def get_modules_in_file(self, filename):
        '''This function returns the list of modules defined in the specified
        file. The base implementation uses the coding style: the file
        `a_mod.f90` implements the module `a_mod`. This function can be
        implemented in a derived class to actually parse the source file if
        required.

        :param str filename: the file name for which to find the list \
            of modules it contains.

        :returns: the list of all modules contained in the specified file.
        :rtype: List[str]

        '''
        basename = os.path.basename(filename)
        root, _ = os.path.splitext(basename)
        if root[-4:].lower() == "_mod":
            return [root]

        return []
