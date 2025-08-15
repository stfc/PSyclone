# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2025, Science and Technology Facilities Council.
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


import copy
from difflib import SequenceMatcher
from typing import Dict, Iterable, List, Set, Union, OrderedDict
import os
import re

from psyclone.errors import InternalError
from psyclone.parse.file_info import FileInfo
from psyclone.parse.module_info import ModuleInfo, ModuleInfoError
from psyclone.psyir.nodes import Container, Node, Routine


class ModuleManager:
    """
    This class implements a singleton that manages module
    dependencies. It should not be created directly.
    Use `ModuleManager.get()` instead.

    :param cache_active: Whether to use (`True`) or
        disable (`False`) caching
    :param cache_path: Path to the cache directory. If `None`, the
        cache file will be created in the same directory as the source
        file with a new file ending `.psycache`.
    """

    # Class variable to store the singleton instance
    _instance = None

    # How similar a file name must be to a module name to trigger reading
    # of the file.
    _threshold_similarity = 0.7

    @staticmethod
    def get(cache_active: bool = None, cache_path: str = None):
        '''Static function that if necessary creates and returns the singleton
        ModuleManager instance.

        :param use_caching: If `True`, a file-based caching of the fparser
            tree will be used. This can significantly accelerate obtaining
            a PSyIR from a source file.
            For parallel builds, parallel race conditions to the cache file
            can happen, but this shouldn't lead to wrong results. However,
            that's untested so far.

        :param cache_path: If set, the cache file will be stored in the given
            path (directory) using a hashsum of the source code to create
            a unique cache file name. If `None`, a cache file will be created
            in the same directory as the source file with a new
            file ending `.psycache`.

        '''
        if not ModuleManager._instance:
            ModuleManager._instance = ModuleManager(cache_active, cache_path)

        return ModuleManager._instance

    # ------------------------------------------------------------------------
    def __init__(
            self,
            cache_active: bool = None,
            cache_path: str = None
    ):
        """
        Set up the module manager. Module manager is actually a singleton
        and should not be created directly. Use `ModuleManager.get()`
        instead.

        :param cache_active: Whether to use (`True`) or
            disable (`False`) caching
        :param cache_path: Path to the cache directory. If `None`, the
            cache file will be created in the same directory as the source
            file with a new file ending `.psycache`.
        """

        if ModuleManager._instance is not None:
            raise InternalError("You need to use 'ModuleManager.get()' "
                                "to get the singleton instance.")

        # Disable caching by default
        self._cache_active = (
            cache_active if cache_active is not None else False)

        # Path to cache
        self._cache_path: str = cache_path

        self._visited_files = {}

        # The list of all search paths which have not yet all their files
        # checked. It is stored as an ordered dict to make it easier to avoid
        # duplicating entries.
        self._remaining_search_paths = OrderedDict()
        self._original_search_paths = []

        # Ordered dictionary to lookup file info from file path
        self._filepath_to_file_info: OrderedDict[str, FileInfo] = OrderedDict()

        # Ordered dictionary to lookup ModuleInfo from a file path
        # Note that there can be multiple modules per file
        self._filepath_to_module_info: OrderedDict[str, List[ModuleInfo]] = \
            OrderedDict()

        # Dictionary of ModuleInfo objects, indexed by module name
        self._modules: OrderedDict[str, ModuleInfo] = \
            OrderedDict()

        self._ignore_modules = set()

        # Setup the regex used to find Fortran modules. Have to be careful not
        # to match e.g. "module procedure :: some_sub".
        self._module_pattern = re.compile(r"^\s*module\s+([a-z]\S*)\s*$",
                                          flags=(re.IGNORECASE | re.MULTILINE))

    # ------------------------------------------------------------------------
    def add_search_path(self, directories, recursive=True):
        '''If the directory is not already contained in the search path,
        add it. Directory can either be a string, in which case it is a single
        directory, or a list of directories, each one a string.

        :param directories: the directory/directories to add.
        :type directories: str | list[str]

        :param bool recursive: whether recursively all subdirectories should
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
        '''This function creates (and caches) FileInfo objects for all files
        with an extension of (F/f/X/x)90 in the given directory that have
        not previously been visited. The new FileInfo objects are returned.

        :param str directory: the directory containing Fortran files
            to analyse.

        :returns: the FileInfo objects for any files that we have not
                  previously visited.
        :rtype: list[:py:class:`psyclone.parse.FileInfo` | None]

        '''
        new_files = []
        with os.scandir(directory) as all_entries:
            for entry in all_entries:
                _, ext = os.path.splitext(entry.name)
                if (not entry.is_file() or
                        ext not in [".F90", ".f90", ".X90", ".x90"]):
                    continue
                full_path = os.path.join(directory, entry.name)
                if full_path in self._visited_files:
                    continue
                self._visited_files[full_path] = \
                    FileInfo(
                            full_path,
                            cache_active=self._cache_active,
                            cache_path=self._cache_path
                        )
                new_files.append(self._visited_files[full_path])
        return new_files

    # ------------------------------------------------------------------------
    def _find_module_in_files(self, name, file_list):
        '''
        Searches the files represented by the supplied list of FileInfo objects
        to find the one defining the named Fortran module.

        :param str name: the name of the module to locate.
        :param file_list: the files to search.
        :type file_list: list[:py:class:`psyclone.parse.FileInfo`]

        :returns: information on the file that contains the module or None if
                  it wasn't found.
        :rtype: :py:class:`psyclone.parse.FileInfo` | None

        '''
        mod_info = None
        for finfo in file_list:
            finfo: FileInfo
            # We remove .psycloned extensions, as we consider them identical
            # to the original file when searching for modules
            filename = finfo.basename.replace('.psycloned', '')
            # We only proceed to read a file to check for a module if its
            # name is sufficiently similar to that of the module.
            score = SequenceMatcher(None, filename, name).ratio()
            if score > self._threshold_similarity:
                mod_names = self.get_modules_in_file(finfo)
                if name in mod_names:
                    # We've found the module we want. Create a ModuleInfo
                    # object for it and cache it.
                    mod_info = ModuleInfo(name, finfo)
                    self._modules[name] = mod_info
                    # A file that has been (or does not require)
                    # preprocessing always takes precendence.
                    if finfo.filename.endswith(".f90"):
                        return mod_info
        return mod_info

    # ------------------------------------------------------------------------
    def add_ignore_module(self, module_name):
        '''Adds the specified module name to the modules to be ignored.

        :param str module_name: name of the module to ignore.

        '''
        self._ignore_modules.add(module_name.lower())

    # ------------------------------------------------------------------------
    def ignores(self):
        ''':returns: the set of modules to ignore.
        :rtype: set[str]

        '''
        return self._ignore_modules

    def add_files(self, filepaths: Union[str, Iterable[str]]) -> None:
        """Utility to add multiple files to the ModuleManager.
        Any file already found to be present in the ModuleManager is skipped.

        :param filepaths: Filename or list of filenames
        """

        if isinstance(filepaths, str):
            filepaths = [filepaths]

        for filepath in filepaths:
            if filepath in self._filepath_to_file_info:
                # Already loaded => skip
                continue

            self._filepath_to_file_info[filepath] = FileInfo(
                filepath,
                cache_active=self._cache_active,
                cache_path=self._cache_path,
            )

    def load_all_source_files(self, verbose: bool = False) -> None:
        """Routine to load the source of all files previously added
        to the module manager

        :param verbose: If `True`, print verbose information
        """

        for fileinfo in self._filepath_to_file_info.values():
            fileinfo: FileInfo
            fileinfo.get_source_code(verbose=verbose)

    def create_all_fparser_trees(self, verbose: bool = False) -> None:
        """
        Routine to load the fparser tree of all files added
        to the module manager

        :param verbose: If `True`, print verbose information
        """

        for fileinfo in self._filepath_to_file_info.values():
            fileinfo: FileInfo
            fileinfo.get_fparser_tree(verbose=verbose)

    def create_all_psyir_nodes(self, verbose: bool = False) -> None:
        """
        Routine to create the psyir nodes of all files added
        to the module manager

        :param verbose: If `True`, print verbose information
        """

        for fileinfo in self._filepath_to_file_info.values():
            fileinfo: FileInfo
            fileinfo.get_psyir(verbose=verbose)

    def load_all_module_infos(
            self,
            error_if_file_already_processed: bool = False,
            error_if_module_already_processed: bool = False,
            verbose: bool = False,
            indent: str = ""
    ):
        """Load the module info using psyir nodes for all FileInfo objects
        in the ModuleManager.

        :param verbose: If `True`, print verbose information
        :param error_if_file_already_processed: If `True`, raise an error
                if a file was already processed.
        :param error_if_module_already_processed: If `True`, raise an error
                if a module was already processed.
        :param indent: Prefix used as indentation for each line of
            verbose output.

        :raises KeyError: If module was already processed if
            error_if_file_already_processed is `True`
        """

        # iterate over all file infos and load psyir
        for file_info in self._filepath_to_file_info.values():
            file_info: FileInfo

            if verbose:
                print(
                    f"{indent}- Loading module information for "
                    f"file '{file_info._filename}"
                )

            psyir_node: Node = file_info.get_psyir(
                verbose=verbose, indent=indent + "  "
            )

            # Collect all module infos in this list
            module_info_in_file: List[ModuleInfo] = []

            # Walk over containers and add respective module information
            for container_node in psyir_node.walk(
                Container, stop_type=Routine
            ):
                if type(container_node) is not Container:
                    # Sort out types which are not exactly of
                    # type 'Container', e.g., 'FileContainer'
                    continue

                container_node: Container

                container_name: str = container_node.name.lower()

                if container_name in self._modules.keys():
                    if error_if_module_already_processed:
                        raise KeyError(
                            f"Module '{container_name}' already processed"
                        )
                    else:
                        print(
                            indent+f"Module '{container_name}' already"
                            " processed"
                        )
                        continue

                module_info = ModuleInfo(
                    container_name, file_info, container_node
                )
                module_info_in_file.append(module_info)

                self._modules[container_name] = module_info

            filepath = file_info.filename

            if filepath in self._filepath_to_module_info.keys():
                if error_if_file_already_processed:
                    raise KeyError(f"File '{filepath}' already processed")
                else:
                    print(indent+f"File '{filepath}' already processed")

                continue

            self._filepath_to_module_info[filepath] = module_info_in_file

    @property
    def all_module_infos(self) -> List[ModuleInfo]:
        """
        :returns: list of all module infos.
        """
        return list(self._modules.values())

    @property
    def all_file_infos(self) -> List[FileInfo]:
        """
        :returns: List of all FileInfo objects.
        """
        return list(self._filepath_to_file_info.values())

    def get_module_info(self, module_name: str) -> ModuleInfo:
        """This function returns the ModuleInfo for the specified
        module.

        :param module_name: Name of the module.

        :returns: object describing the requested module or None if the
                  manager has been configured to ignore this module.

        :raises FileNotFoundError: if the module_name is not found in
            either the cached data nor in the search path.

        """
        mod_lower = module_name.lower()

        if mod_lower in self._ignore_modules:
            return None

        # First check if we have already seen this module. We only end the
        # search early if the file we've found does not require pre-processing
        # (i.e. has a .f90 suffix).
        mod_info: ModuleInfo = self._modules.get(mod_lower, None)
        if mod_info and mod_info.filename.endswith(".f90"):
            return mod_info
        old_mod_info = mod_info
        # Are any of the files that we've already seen a good match?
        mod_info = self._find_module_in_files(mod_lower,
                                              self._visited_files.values())
        if mod_info and mod_info.filename.endswith(".f90"):
            return mod_info
        old_mod_info = mod_info

        # If not, check the search paths. To avoid frequent accesses to
        # the directories, we search directories one at a time, and
        # add the list of all files in that directory to our cache
        # `_visited_files`.
        while self._remaining_search_paths:
            # Get the first element from the search path list:
            directory, _ = self._remaining_search_paths.popitem(last=False)
            new_files = self._add_all_files_from_dir(directory)
            mod_info = self._find_module_in_files(mod_lower, new_files)
            if mod_info:
                return mod_info

        if old_mod_info:
            return old_mod_info

        raise FileNotFoundError(f"Could not find source file for module "
                                f"'{module_name}' in any of the directories "
                                f"'{', '.join(self._original_search_paths)}'. "
                                f"You can add search paths using the '-d' "
                                f"command line option.")

    # ------------------------------------------------------------------------
    def get_modules_in_file(self, finfo: FileInfo) -> List[str]:
        '''
        Uses a regex search to find all modules defined in the file with the
        supplied name.

        :param finfo: object holding information on the file to examine.

        :returns: the names of any modules present in the supplied file.

        '''
        # TODO #2597: perhaps use the fparser FortranReader here as this regex
        # could be defeated by e.g.
        #   module &
        #    my_mod
        # `finfo.get_source_code()` will read the file if it hasn't already
        # been cached.
        mod_names = self._module_pattern.findall(finfo.get_source_code())

        return [name.lower() for name in mod_names]

    def get_all_dependencies_recursively(
            self,
            all_mod_names: List[str],
    ) -> OrderedDict[str, List[str]]:
        '''This function collects recursively all module dependencies
        for any of the modules in the ``all_mod_names`` set. I.e. it will
        add all modules used by any module listed in ``all_mod_names``,
        and any modules used by the just added modules etc. In the end,
        it will return a dictionary that for each module lists which
        modules it depends on. This dictionary will be complete,
        i.e. all modules that are required for the original set of modules
        (and that could be found) will be a key in the dictionary. It will
        include the original set of modules as well.

        If a module cannot be found (e.g. its path was not given to the
        ModuleManager, or it might be a system module for which the sources
        are not available), a message will be printed, and this module will
        be ignored (i.e. not listed in any dependencies).
        # TODO 2120: allow a choice to abort or ignore.

        :param all_mod_names: the set of all module names for which
            to collect module dependencies.

        :returns: an ordered dictionary with all modules that are required
            (directly or indirectly) for the modules in ``all_mod_names``.
        '''

        # This contains the mapping from each module name to the
        # list of the dependencies and is returned as result:
        module_dependencies = OrderedDict()

        # Work on a copy to avoid modifying the caller's set:
        todo = all_mod_names.copy()

        # This list contains all module names that could not be found
        # (to avoid adding them to the todo list again)
        not_found = set()

        while todo:
            # Pick one (random) module to handle (convert to lowercase
            # in case that the code use inconsistent capitalisation)
            module = todo.pop().lower()

            # Ignore any modules that we were asked to ignore
            if module in self.ignores():
                continue
            try:
                mod_deps = self.get_module_info(module).get_used_module_names()
                mod_deps = list(mod_deps)
            except (FileNotFoundError, ModuleInfoError):
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
            mod_deps = [x for x in mod_deps if x not in not_found]

            # Add the dependencies of `module` to the result dictionary:
            module_dependencies[module] = mod_deps

            # Remove all dependencies from the list of new dependencies
            # of `module` that have already been handled:
            module_dependencies_keys = module_dependencies.keys()
            new_deps = [x for x in mod_deps
                        if x not in module_dependencies_keys]

            # Then append these really new modules to the list of modules
            # that still need to be handled
            for dep in new_deps:
                if dep not in todo:
                    todo.append(dep)

        return module_dependencies

    # -------------------------------------------------------------------------
    def sort_modules(
        self, module_dependencies: Dict[str, Set[str]]
    ) -> List[str]:
        '''This function sorts the given dependencies so that all
        dependencies of a module are before any module that
        needs it. Input is a dictionary that contains all modules to
        be sorted as keys, and the value for each module is the set
        of dependencies that the module depends on.

        :param module_dependencies: the list of modules required as keys,
            with all their dependencies as value.

        :returns: the sorted list of modules.

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
                if dep in todo:
                    continue
                # Print a warning if this module is not supposed to be ignored
                if dep not in self.ignores():
                    # TODO 2120: allow a choice to abort or ignore.
                    print(f"Cannot find module `{dep}` which is used by "
                          f"module '{module}'.")
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
                all_mod_names_sorted = sorted((mod for mod in todo.keys()),
                                              key=lambda x: len(todo[x]))
                mod = all_mod_names_sorted[0]

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
