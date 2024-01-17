# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
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


from collections import OrderedDict
import copy
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
        # Cached mapping from module name to filename.
        self._mod_2_filename = {}

        # The list of all search paths which have not yet all their files
        # checked. It is stored as an ordered dict to make it easier to avoid
        # duplicating entries.
        self._remaining_search_paths = OrderedDict()

        self._original_search_paths = []

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
                raise IOError(f"Directory '{directory}' does not exist or "
                              f"cannot be read.")
            self._remaining_search_paths[directory] = 1
            self._original_search_paths.append(directory)
            if recursive:
                for root, dirs, _ in os.walk(directory):
                    for current_dir in dirs:
                        new_dir = os.path.join(root, current_dir)
                        self._remaining_search_paths[new_dir] = 1
                        self._original_search_paths.append(new_dir)

    # ------------------------------------------------------------------------
    def _add_all_files_from_dir(self, directory):
        '''This function adds all files with an extension of (F/f/X/x)90 in the
        given directory to the mapping of module names to file names. The
        module names are based on the filename using `get_modules_in_file()`.
        By default it is assumed that `a_mod.f90` contains the module `a_mod`.

        :param str directory: the directory containing Fortran files \
            to analyse.

        '''
        with os.scandir(directory) as all_entries:
            for entry in all_entries:
                _, ext = os.path.splitext(entry.name)
                if (not entry.is_file()) or \
                        ext not in [".F90", ".f90", ".X90", ".x90"]:
                    continue
                full_path = os.path.join(directory, entry.name)
                # Obtain the names of all modules defined in this source file.
                all_modules = self.get_modules_in_file(full_path)
                for module in all_modules:
                    # Pre-processed file should always take precedence
                    # over non-pre-processed files. So if a module already
                    # exists in the mapping, only overwrite it if the new
                    # file is pre-processed (i.e. .f90). This still means that
                    # if files are not preprocessed (.F90), they will still be
                    # added (but might cause problems parsing later).
                    if module not in self._mod_2_filename or \
                            ext in [".f90", ".x90"]:
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

        # First check if we have already cached this file:
        mod_info = self._mod_2_filename.get(mod_lower, None)
        if mod_info:
            return mod_info

        # If not, check the search paths. To avoid frequent accesses to
        # the directories, we search directories one at a time, and
        # add the list of all files in that directory to our cache
        # _mod_2_filename
        while self._remaining_search_paths:
            # Get the first element from the search path list:
            directory, _ = self._remaining_search_paths.popitem(last=False)
            self._add_all_files_from_dir(directory)
            mod_info = self._mod_2_filename.get(mod_lower, None)
            if mod_info:
                return mod_info

        raise FileNotFoundError(f"Could not find source file for module "
                                f"'{module_name}' in any of the directories "
                                f"'{', '.join(self._original_search_paths)}'. "
                                f"You can add search paths using the '-d' "
                                f"command line option.")

    # ------------------------------------------------------------------------
    def get_modules_in_file(self, filename):
        '''This function returns the list of modules defined in the specified
        file. The base implementation assumes the use of the LFRic coding
        style: the file `a_mod.f90` implements the module `a_mod`. This
        function can be implemented in a derived class to actually parse the
        source file if required.

        :param str filename: the file name for which to find the list \
            of modules it contains.

        :returns: the list of all modules contained in the specified file.
        :rtype: List[str]

        '''
        basename = os.path.basename(filename)
        root, _ = os.path.splitext(basename)
        if root.lower().endswith("_mod"):
            return [root]

        return []

    # ------------------------------------------------------------------------
    def get_all_dependencies_recursively(self, all_mods):
        '''This function collects recursively all module dependencies
        for any of the modules in the ``all_mods`` set. I.e. it will
        add all modules used by any module listed in ``all_mods``,
        and any modules used by the just added modules etc. In the end,
        it will return a dictionary that for each module lists which
        modules it depends on. This dictionary will be complete,
        i.e. all modules that are required for the original set of modules
        (and that could be found) will be a key in the dictionary. It will
        include the original set of modules as well.

        If a module cannot be found (e.g. its path was not given to the
        ModuleManager, or it might be a system module for which the sources
        are not available, a message will be printed, and this module will
        be ignored (i.e. not listed in any dependencies).
        # TODO 2120: allow a choice to abort or ignore.

        :param Set[str] all_mods: the set of all modules for which to collect
            module dependencies.

        :returns: a dictionary with all modules that are required (directly \
            or indirectly) for the modules in ``all_mods``.
        :rtype: Dict[str, Set[str]]

        '''
        # This contains the mapping from each module name to the
        # list of the dependencies and is returned as result:
        module_dependencies = {}

        # Work on a copy to avoid modifying the caller's set:
        todo = all_mods.copy()

        # This set contains module that could not be found (to avoid
        # adding them to the todo list again
        not_found = set()

        while todo:
            # Pick one (random) module to handle:
            module = todo.pop()
            try:
                mod_deps = self.get_module_info(module).get_used_modules()
            except FileNotFoundError:
                if module not in not_found:
                    # We don't have any information about this module,
                    # ignore it.
                    # TODO 2120: allow a choice to abort or ignore.
                    print(f"Could not find module '{module}'.")
                    not_found.add(module)
                    # Remove this module as dependencies from any other
                    # module in our todo list, so the final result will
                    # only contain known modules
                    for dep in module_dependencies.values():
                        if module in dep:
                            dep.remove(module)
                continue

            # Remove all dependencies which we don't know anything about:
            mod_deps = mod_deps.difference(not_found)

            # Add the dependencies of `module` to the result dictionary:
            module_dependencies[module] = mod_deps

            # Remove all dependencies from the list of new dependencies
            # of `module` that have already been handled:
            new_deps = mod_deps.difference(module_dependencies.keys())

            # Then add these really new modules to the list of modules
            # that still need to be handled
            todo |= new_deps

        return module_dependencies

    # -------------------------------------------------------------------------
    @staticmethod
    def sort_modules(module_dependencies):
        '''This function sorts the given dependencies so that all
        dependencies of a module are before any module that
        needs it. Input is a dictionary that contains all modules to
        be sorted as keys, and the value for each module is the set
        of dependencies that the module depends on.

        :param module_dependencies: the list of modules required as keys, \
            with all their dependencies as value.
        :type module_dependencies: Dict[str, Set[str]]

        :returns: the sorted list of modules.
        :rtype: List[str]

        '''
        result = []

        # Create a copy to avoid modifying the callers data structure:
        todo = copy.deepcopy(module_dependencies)

        # Consistency check: test that all dependencies listed are also
        # a key in the list, otherwise there will be a dependency that
        # breaks sorting. If an unknown dependency is detected, print
        # a warning, and remove it (otherwise no sort order could be
        # determined).
        for module, dependencies in todo.items():
            # Take a copy so we can modify the original set of dependencies:
            dependencies_copy = dependencies.copy()
            for dep in dependencies_copy:
                if dep not in todo:
                    print(f"Module '{module}' contains a dependency to "
                          f"'{dep}', for which we have no dependencies.")
                    # TODO 2120: allow a choice to abort or ignore.
                    dependencies.remove(dep)

        while todo:
            # Find one module that has no dependencies, which is the
            # next module to be added to the results.
            for mod, dep in todo.items():
                if not dep:
                    break
            else:
                # If there is no module without a dependency, there
                # is a circular dependency
                print(f"Circular dependency - cannot sort "
                      f"module dependencies: {todo}")
                # TODO 2120: allow a choice to abort or ignore.
                # In this case pick a module with the least number of
                # dependencies, the best we can do in this case - and
                # it's better to provide all modules (even if they cannot)
                # be sorted, than missing some.
                all_mods_sorted = sorted((mod for mod in todo.keys()),
                                         key=lambda x: len(todo[x]))
                mod = all_mods_sorted[0]

            # Add the current module to the result and remove it from
            # the todo list.
            result.append(mod)
            del todo[mod]

            # Then remove this module from the dependencies of all other
            # modules:
            for dep in todo.values():
                if mod in dep:
                    dep.remove(mod)

        return result
