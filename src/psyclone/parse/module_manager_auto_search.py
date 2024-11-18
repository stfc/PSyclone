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

"""This module contains a singleton class that manages information about
which module is contained in which file (including full location). """


from collections import OrderedDict
from difflib import SequenceMatcher
import os
import re
from typing import Dict, Set, List

from psyclone.errors import InternalError, GenerationError
from psyclone.parse.file_info import FileInfo, FileInfoFParserError
from psyclone.parse.module_info import ModuleInfo
from psyclone.parse.module_manager_base import ModuleManagerBase


class ModuleManagerAutoSearch(ModuleManagerBase):
    """This class implements a singleton that manages module
    dependencies.

    """

    # Class variable to store the singleton instance
    _instance = None

    # How similar a file name must be to a module name to trigger reading
    # of the file.
    _threshold_similarity = 0.7

    # ------------------------------------------------------------------------
    @staticmethod
    def get_singleton():
        """Static function that if necessary creates and returns the singleton
        ModuleManager instance.
        """

        if not ModuleManagerAutoSearch._instance:
            ModuleManagerAutoSearch._instance = ModuleManagerAutoSearch()

        return ModuleManagerAutoSearch._instance

    # ------------------------------------------------------------------------
    def __init__(self):
        super().__init__()

        if ModuleManagerAutoSearch._instance is not None:
            raise InternalError(
                "You need to use 'ModuleManager.get()' "
                "to get the singleton instance."
            )

        # Make data structures ready to be used
        self._module_name_to_modinfo = {}
        self._filepath_to_module_info = {}

        self._visited_files = {}

        # The list of all search paths which have not yet all their files
        # checked. It is stored as an ordered dict to make it easier to avoid
        # duplicating entries.
        self._remaining_search_paths = OrderedDict()
        self._original_search_paths = []

        # Setup the regex used to find Fortran modules. Have to be careful not
        # to match e.g. "module procedure :: some_sub".
        self._module_pattern = re.compile(
            r"^\s*module\s+([a-z]\S*)\s*$",
            flags=(re.IGNORECASE | re.MULTILINE),
        )

    # ------------------------------------------------------------------------
    def add_search_path(self, directories, recursive=True):
        """If the directory is not already contained in the search path,
        add it. Directory can either be a string, in which case it is a single
        directory, or a list of directories, each one a string.

        :param directories: the directory/directories to add.
        :type directories: str | list[str]

        :param bool recursive: whether recursively all subdirectories should
            be added to the search path.

        """
        if isinstance(directories, str):
            # Make sure we always have a list
            directories = [directories]

        for directory in directories:
            if not os.access(directory, os.R_OK):
                raise IOError(
                    f"Directory '{directory}' does not exist or "
                    f"cannot be read."
                )
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
        """This function creates (and caches) FileInfo objects for all files
        with an extension of (F/f/X/x)90 in the given directory that have
        not previously been visited. The new FileInfo objects are returned.

        :param str directory: the directory containing Fortran files
            to analyse.

        :returns: the FileInfo objects for any files that we have not
                  previously visited.
        :rtype: list[:py:class:`psyclone.parse.FileInfo` | None]

        """
        new_files = []
        with os.scandir(directory) as all_entries:
            for entry in all_entries:
                _, ext = os.path.splitext(entry.name)
                if not entry.is_file() or ext not in [
                    ".F90",
                    ".f90",
                    ".X90",
                    ".x90",
                ]:
                    continue
                full_path = os.path.join(directory, entry.name)
                if full_path in self._visited_files:
                    continue
                self._visited_files[full_path] = FileInfo(full_path)
                new_files.append(self._visited_files[full_path])
        return new_files

    # ------------------------------------------------------------------------
    def _find_module_in_files(self, name, fileinfo_list: List[FileInfo]):
        """
        Searches the files represented by the supplied list of FileInfo objects
        to find the one defining the named Fortran module.

        :param str name: the name of the module to locate.
        :param file_list: the files to search.
        :type file_list: list[:py:class:`psyclone.parse.FileInfo`]

        :returns: information on the file that contains the module or None if
                  it wasn't found.
        :rtype: :py:class:`psyclone.parse.FileInfo` | None

        """
        mod_info = None
        for file_info in fileinfo_list:
            # We only proceed to read a file to check for a module if its
            # name is sufficiently similar to that of the module.
            score = SequenceMatcher(
                None, file_info.get_basename(), name
            ).ratio()
            if score > self._threshold_similarity:
                mod_names = self._get_modules_in_file_regexp(file_info)
                if name in mod_names:
                    # We've found the module we want. Create a ModuleInfo
                    # object for it and cache it.
                    mod_info = ModuleInfo(name, file_info)
                    self._module_name_to_modinfo[name] = mod_info
                    # A file that has been (or does not require)
                    # preprocessing always takes precendence.
                    if file_info.get_filepath().endswith(".f90"):
                        return mod_info
        return mod_info

    # ------------------------------------------------------------------------
    def get_module_info(self, module_name_lower: str) -> ModuleInfo:
        """This function returns the ModuleInformation for the specified
        module and automatically searches all provided directories for the
        respective file with the module.

        :param str module_name: name of the module.

        :returns: object describing the requested module or None if the
                  manager has been configured to ignore this module.
        :rtype: :py:class:`psyclone.parse.ModuleInfo` | None

        :raises FileNotFoundError: if the module_name is not found in
            either the cached data nor in the search path.

        """
        module_name_lower = module_name_lower.lower()

        if module_name_lower in self._ignore_modules:
            return None

        # First check if we have already seen this module. We only end the
        # search early if the file we've found does not require pre-processing
        # (i.e. has a .f90 suffix).
        mod_info = self._module_name_to_modinfo.get(module_name_lower, None)
        if mod_info and mod_info.filepath.endswith(".f90"):
            return mod_info
        old_mod_info = mod_info
        # Are any of the files that we've already seen a good match?
        mod_info = self._find_module_in_files(
            module_name_lower, self._visited_files.values()
        )
        if mod_info and mod_info.filepath.endswith(".f90"):
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
            mod_info = self._find_module_in_files(module_name_lower, new_files)
            if mod_info:
                return mod_info

        if old_mod_info:
            return old_mod_info

        raise FileNotFoundError(
            f"Could not find source file for module "
            f"'{module_name_lower}' in any of the directories "
            f"'{', '.join(self._original_search_paths)}'. "
            f"You can add search paths using the '-d' "
            f"command line option."
        )

    # ------------------------------------------------------------------------
    def _get_modules_in_file_regexp(self, finfo: FileInfo):
        """
        Uses a regex search to find all modules defined in the file with the
        supplied name.

        :param finfo: object holding information on the file to examine.
        :type finfo: :py:class:`psyclone.parse.FileInfo`

        :returns: the names of any modules present in the supplied file.
        :rtype: list[str]

        """
        mod_names = self._module_pattern.findall(finfo.get_source_code())

        return [name.lower() for name in mod_names]

    # ------------------------------------------------------------------------
    def get_all_dependencies_recursively(
        self, all_mods
    ) -> Dict[str, Set[str]]:
        """This function collects recursively all module dependencies
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

        :param set[str] all_mods: the set of all modules for which to collect
            module dependencies.

        :returns: a dictionary with all modules that are required (directly
            or indirectly) for the modules in ``all_mods``.
        :rtype: dict[str, set[str]]

        """
        # This contains the mapping from each module name to the
        # list of the dependencies and is returned as result:
        module_dependencies = {}

        # Work on a copy to avoid modifying the caller's set:
        todo = all_mods.copy()

        # This set contains module that could not be found (to avoid
        # adding them to the todo list again
        not_found = set()

        while todo:
            # Pick one (random) module to handle (convert to lowercase
            # in case that the code use inconsistent capitalisation)
            module = todo.pop().lower()

            # Ignore any modules that we were asked to ignore
            if module in self.get_ignore_modules():
                continue
            try:
                mod_deps = self.get_module_info(module).get_used_modules()
                # Convert to set
                mod_deps = set(mod_deps)

            except (FileNotFoundError, FileInfoFParserError, GenerationError):
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
