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
# Author: J. Henrichs, Bureau of Meteorology
# Modified: M. Schreiber, Univ. Grenoble Alpes

"""This module contains a singleton class that manages information about
which module is contained in which file (including full location). """


from typing import List, Dict, Set, Union, final
import copy

from psyclone.parse.module_info import ModuleInfo, FileInfo


class ModuleManagerBase:
    """This class implements the basic shared features to manage modules.

    There are two implementations available:
    - One which is based on automatically loading all files depending on each
      other
    - Another one where the files are explicitly provided
    """

    def __init__(self):

        # Dictionary to lookup file info from file path
        self._filepath_to_file_info: Dict[str, FileInfo] = {}

        # Dictionary to lookup modules of all files
        # Note that there can be multiple modules per file
        self._filepath_to_module_info: Dict[str, List[ModuleInfo]] = None

        # Dictionary of modules to lookup module info
        self._module_name_to_modinfo: Dict[str, ModuleInfo] = None

        # List of modules to ignore
        # We use a list which can be sorted
        self._ignore_modules = set()

    @final
    def add_ignore_modules(self, module_names: Union[str, set, list]):
        """Adds the specified module name to the modules to be ignored.

        :param str module_name: name of the module to ignore.
        """

        # Ensure that we always have a set
        if isinstance(module_names, str):
            module_names = {module_names}
        elif not isinstance(module_names, set):
            module_names = set(module_names)

        if len(module_names):
            assert isinstance(next(iter(module_names)), str)

        module_names = {m.lower() for m in module_names}
        self._ignore_modules.update(module_names)

    @final
    def ignore_modules(self) -> Set[str]:
        """:returns: the set of modules to ignore.
        :rtype: set[str]

        """
        return self._ignore_modules

    def get_module_info(self, module_name: str) -> ModuleInfo:
        """This function returns the ModuleInformation for the specified
        module.

        :param str module_name: name of the module.

        :returns: object describing the requested module or None if the
                  manager has been configured to ignore this module.
        :rtype: :py:class:`psyclone.parse.ModuleInfo` | None

        :raises FileNotFoundError: if the module_name is not found in
            either the cached data nor in the search path.
        """

        mod_lower = module_name.lower()

        if mod_lower in self._ignore_modules:
            return None

        mod_info = self._module_name_to_modinfo.get(mod_lower, None)
        if mod_info:
            return mod_info

        raise FileNotFoundError(
            f"Could not find module info for module named '{module_name}'"
        )

    def add_files(self, filepaths: Union[str, List[str], Set[str]]) -> None:
        """Add a file to the list of files

        :param filepath: Path to file
        :type filepath: str
        """

        if isinstance(filepaths, str):
            filepaths = [filepaths]
        elif isinstance(filepaths, set):
            filepaths = list(filepaths)

        for filepath in filepaths:
            assert (
                filepath not in self._filepath_to_file_info.keys()
            ), "Found duplicate entry in dictionary 'filename to FileInfo'."

            self._filepath_to_file_info[filepath] = FileInfo(filepath)

    def load_source_code(self, verbose: bool = False) -> None:
        """Routine to load the source of all files"""

        for fileinfo in self._filepath_to_file_info.values():
            fileinfo: FileInfo
            fileinfo.get_source_code(verbose=verbose)

    def load_fparser_tree(self, verbose: bool = False) -> None:
        """Routine to load the source of all files"""

        for fileinfo in self._filepath_to_file_info.values():
            fileinfo: FileInfo
            fileinfo.get_fparser_node(verbose=verbose)

    def load_psyir_node(self, verbose: bool = False) -> None:
        """Routine to load the psyir representation of all files"""

        for fileinfo in self._filepath_to_file_info.values():
            fileinfo: FileInfo
            fileinfo.get_psyir_node(verbose=verbose)

    def sort_modules(
        self, module_dependencies: Dict[str, Set[str]]
    ) -> List[str]:
        """This function sorts the given dependencies so that all
        dependencies of a module are before any module that
        needs it. Input is a dictionary that contains all modules to
        be sorted as keys, and the value for each module is the set
        of dependencies that the module depends on.

        :param module_dependencies: the list of modules required as keys, \
            with all their dependencies as value.
        :type module_dependencies: dict[str, set[str]]

        :returns: the sorted list of modules.
        :rtype: list[str]

        """
        sorted_modules_list = []

        # Create a copy to avoid modifying the callers data structure:
        todo_modules = copy.deepcopy(module_dependencies)

        # Consistency check: test that all dependencies listed are also
        # a key in the list, otherwise there will be a dependency that
        # breaks sorting. If an unknown dependency is detected, print
        # a warning, and remove it (otherwise no sort order could be
        # determined).
        for module, dependencies in todo_modules.items():
            # Take a copy so we can modify the original set of dependencies:
            dependencies_copy = dependencies.copy()
            for mod_dep in dependencies_copy:
                mod_dep: str
                if mod_dep in todo_modules:
                    continue

                # Print a warning if this module is not supposed to be ignored
                if mod_dep not in self.ignore_modules():
                    # TODO 2120: allow a choice to abort or ignore.
                    print(
                        f"Cannot find module `{mod_dep}` which is used by "
                        f"module '{module}'."
                    )
                dependencies.remove(mod_dep)

        while todo_modules:
            # Find one module that has no dependencies, which is the
            # next module to be added to the results.
            for mod, mod_dep in todo_modules.items():
                if not mod_dep:
                    break
            else:
                # If there is no module without a dependency, there
                # is a circular dependency
                print(
                    f"Circular dependency - cannot sort "
                    f"module dependencies: {todo_modules}"
                )
                # TODO 2120: allow a choice to abort or ignore.
                # In this case pick a module with the least number of
                # dependencies, the best we can do in this case - and
                # it's better to provide all modules (even if they cannot)
                # be sorted, than missing some.
                all_mods_sorted = sorted(
                    (mod for mod in todo_modules.keys()),
                    key=lambda x: len(todo_modules[x]),
                )
                mod = all_mods_sorted[0]

            # Add the current module to the result and remove it from
            # the todo list.
            sorted_modules_list.append(mod)
            del todo_modules[mod]

            # Then remove this module from the dependencies of all other
            # modules:
            for mod_dep in todo_modules.values():
                mod_dep: List[str]

                if mod in mod_dep:
                    mod_dep.remove(mod)

        return sorted_modules_list
