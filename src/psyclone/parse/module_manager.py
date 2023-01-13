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

from fparser.common.readfortran import FortranFileReader
from fparser.two.Fortran2003 import Use_Stmt
from fparser.two.parser import ParserFactory
from fparser.two.utils import walk

from psyclone.errors import InternalError


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
    def add_search_path(self, directories):
        '''If the directory is not already contained in the search path,
        add it. Directory can either be a string, in which case it is a single
        directory, or a list of directories, each one a string.

        :param directories: the directory/directories to add.
        :type directories: Union[str, List[str]]

        '''

        if isinstance(directories, str):
            # Make sure we always have a list
            directories = [directories]

        for directory in directories:
            if directory in self._search_paths:
                continue
            self._search_paths.append(directory)
            if not os.path.exists(directory):
                raise IOError(f"Directory {directory} does not exist.")
            for root, dirs, _ in os.walk(directory):
                for directory in dirs:
                    new_dir = os.path.join(root, directory)
                    self._search_paths.append(new_dir)

    # ------------------------------------------------------------------------
    def _add_all_files_from_dir(self, directory):
        print("Scanning", directory)
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
                        self._mod_2_filename[module] = full_path

    # ------------------------------------------------------------------------
    def get_file_for_module(self, module_name):
        '''This function returns the file name that contains the
        specified module.

        :param str module_name: name of the module.

        :returns: the filename that contains the module
        :rtype: str

        :raises KeyError: if the module_name is not found.

        '''
        mod_lower = module_name.lower()

        # First check if we already know about this file:
        file_name = self._mod_2_filename.get(mod_lower, None)
        if file_name:
            return file_name

        # If not, check the search paths. To avoid frequent accesses to
        # the directories, we reach search directories one at a time, and
        # add the list of all files to the mapping:

        search_paths_copy = self._search_paths.copy()

        for directory in search_paths_copy:
            self._add_all_files_from_dir(directory)
            self._search_paths.remove(directory)
            file_name = self._mod_2_filename.get(mod_lower, None)
            if file_name:
                print("TODO", module_name, self._search_paths)
                return file_name

        print("OOPS")
        return None

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

    # ------------------------------------------------------------------------
    def find_modules_used_in(self, module_name):
        '''This function analyses a given module source file and returns
        a list of 2-tuples containing the module name, and a list of
        all imported symbols from that module. If all symbols are imported,
        the list of symbols will be empty.

        :param str module_name: the file name (including path if required) \
            for which all modules should be found.

        :returns: a list of with all module names and symbols imported.
        :rtype: List[Tuple[str, List[str]]]

        '''
        module_name = module_name.lower()

        # Check if we have cached results:
        result = self._cached_module_info.get(module_name, None)
        if result is not None:
            return result

        # Otherwise, parse the file:
        filename = self.get_file_for_module(module_name)
        if not filename:
            return []

        try:
            reader = FortranFileReader(filename)
        except FileNotFoundError:
            return []
        parser = ParserFactory().create(std="f2003")
        parse_tree = parser(reader)
        results = []
        for use in walk(parse_tree, Use_Stmt):
            # Ignore intrinsic modules:
            if str(use.items[0]) == "INTRINSIC":
                continue

            only_list = use.items[4]
            if not only_list:
                # Imports everything. Indicate this by
                # using an empty list for the symbols
                results.append((str(use.items[2]), []))
                continue
            all_symbols = []
            for symbol in use.items[4].children:
                all_symbols.append(str(symbol))
            results.append((str(use.items[2]), all_symbols))

        self._cached_module_info[module_name] = results
        return results
