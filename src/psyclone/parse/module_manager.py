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


class ModuleManager:
    '''This class implements a singleton that manages module
    dependencies.

    :param str config_file: a file with the list of all file names used \
        in the project.

    '''
    # Class variable to store the singleton instance
    _instance = None

    def __init__(self, config_file="allfiles"):

        self._cached_module_info = {}
        try:
            with open(config_file, mode='r', encoding='utf-8') as file:
                all_files = file.read().split()
        except FileNotFoundError:
            all_files = []

        self._mod_2_filename = {}
        for filename in all_files:
            modules = self.get_modules_in_file(filename)
            for module in modules:
                self._mod_2_filename[module.lower()] = filename

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
    def get_file_for_module(self, module_name):
        '''This function returns the file name that contains the
        specified module.

        :param str module_name: name of the module.

        :returns: the filename that contains the module
        :rtype: str

        :raises KeyError: if the module_name is not found.

        '''
        return self._mod_2_filename[module_name.lower()]

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
