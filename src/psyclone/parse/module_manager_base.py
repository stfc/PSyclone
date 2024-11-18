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
# Modified: M. Schreiber, Univ. Grenoble Alpes, LJK, Inria

"""This module contains a singleton class that manages information about
which module is contained in which file (including full location). """

from abc import ABC
from typing import List, Dict, Set, Union, final
import copy
from psyclone.parse.module_info import ModuleInfo, FileInfo
from psyclone.psyir.nodes import Node, Container, Routine


class ModuleManagerBase(ABC):
    """This class implements the basic shared features to manage modules."""

    def __init__(self, cache_active: bool = False, cache_path: str = None):
        """Constructor of ModuleManager

        :param cache_active: _description_, defaults to False
        :type cache_active: bool, optional
        """

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

        # Is the Cache activated
        self._cache_active: bool = cache_active

        # Path to cache
        self._cache_path: str = cache_path

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
    def get_ignore_modules(self) -> Set[str]:
        """:returns: the set of modules to ignore.
        :rtype: set[str]

        """
        return self._ignore_modules

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

            self._filepath_to_file_info[filepath] = FileInfo(
                filepath,
                cache_active=self._cache_active,
                cache_path=self._cache_path,
            )

    def load_source_code(self, verbose: bool = False) -> None:
        """Routine to load the source of all files"""

        for fileinfo in self._filepath_to_file_info.values():
            fileinfo: FileInfo
            fileinfo.get_source_code(verbose=verbose)

    def load_fparser_tree(self, verbose: bool = False) -> None:
        """Routine to load the source of all files"""

        for fileinfo in self._filepath_to_file_info.values():
            fileinfo: FileInfo
            fileinfo.get_fparser_tree(verbose=verbose)

    def load_psyir_node(self, verbose: bool = False) -> None:
        """Routine to load the psyir representation of all files"""

        for fileinfo in self._filepath_to_file_info.values():
            fileinfo: FileInfo
            fileinfo.get_psyir_node(verbose=verbose)

    def load_module_info(self, verbose: bool = False):
        """Load the module info using psyir nodes

        :raises KeyError: If module was already processed
        """

        if self._module_name_to_modinfo is not None:
            # Already loaded => simply return
            return

        # assert self._module_name_to_modinfo is None
        # assert self._filepath_to_module_info is None

        self._module_name_to_modinfo = {}
        self._filepath_to_module_info = {}

        # iterate over all file infos and load psyir
        for file_info in self._filepath_to_file_info.values():
            file_info: FileInfo

            if verbose:
                print(
                    f"  - Loading module information for "
                    f"file '{file_info._filepath}"
                )

            psyir_node: Node = file_info.get_psyir_node()

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

                if container_name in self._module_name_to_modinfo.keys():
                    raise KeyError(
                        f"Module '{container_name}' already processed"
                    )

                module_info = ModuleInfo(
                    container_name, file_info, container_node
                )
                module_info_in_file.append(module_info)

                self._module_name_to_modinfo[container_name] = module_info

            filepath = file_info.get_filepath()
            if filepath in self._filepath_to_module_info.keys():
                raise KeyError(f"File '{filepath}' already processed")
            self._filepath_to_module_info[filepath] = module_info_in_file

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

        raise ModuleNotFoundError(
            f"Could not find module info for module named '{module_name}'"
        )

    def get_dependency_sorted_modules(
        self,
        module_dependencies: Dict[str, Set[str]] = None,
        verbose: bool = False,
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

        assert type(verbose) is bool

        # Make sure that the module information is loaded
        self.load_module_info()

        # Setup lookup dictionary
        if module_dependencies is None:
            module_dependencies = {}
            for module_info in self._module_name_to_modinfo.values():
                module_dependencies[module_info.name] = copy.deepcopy(
                    module_info.get_used_modules()
                )
        else:
            module_dependencies = copy.deepcopy(module_dependencies)

        assert type(module_dependencies) is dict

        if verbose:
            print("- Removing modules from dependencies which were not loaded")

        # Consistency check: test that all dependencies listed are also
        # a key in the list, otherwise there will be a dependency that
        # breaks sorting. If an unknown dependency is detected, print
        # a warning, and remove it (otherwise no sort order could be
        # determined).
        for module_name_dep, module_dep_list in module_dependencies.items():
            # Take a copy so we can modify the original set of dependencies:
            dependencies_copy = module_dep_list.copy()
            for mod_dep in dependencies_copy:
                mod_dep: str
                if mod_dep in module_dependencies:
                    continue

                # Print a warning if this module is not supposed to be ignored
                if mod_dep not in self.get_ignore_modules():
                    # TODO 2120: allow a choice to abort or ignore.
                    if verbose:
                        print(
                            f"  - Cannot find module '{mod_dep}' which is "
                            f"used by module '{module_name_dep}'."
                        )
                module_dep_list.remove(mod_dep)

        sorted_modules_list = []

        if verbose:
            print("- Computing dependencies")

        while module_dependencies:
            # Find one module that has no dependencies, which is the
            # next module to be added to the results.
            for module_name, mod_deps_list in module_dependencies.items():
                if not mod_deps_list:
                    module_name_no_deps = module_name
                    break
            else:
                # If there is no module without a dependency, there
                # is a circular dependency
                if verbose:
                    print(
                        f"  - Circular dependency - cannot sort "
                        f"module dependencies. Remaining modules: "
                        f"{module_dependencies}"
                    )
                # TODO 2120: allow a choice to abort or ignore.
                # In this case pick a module with the least number of
                # dependencies, the best we can do in this case - and
                # it's better to provide all modules (even if they cannot)
                # be sorted, than missing some.
                all_mods_sorted = sorted(
                    (mod for mod in module_dependencies.keys()),
                    key=lambda x: len(module_dependencies[x]),
                )
                module_name_no_deps = all_mods_sorted[0]

            if verbose:
                print(f"  - Next module in sorted list: {module_name_no_deps}")
            # Add the current module to the result and remove it from
            # the todo list.
            sorted_modules_list.append(module_name_no_deps)
            del module_dependencies[module_name_no_deps]

            # Then remove this module from the dependencies of all other
            # modules:
            for module_name, module_name_dep in module_dependencies.items():
                module_name_dep: List[str]

                if module_name_no_deps in module_name_dep:
                    if verbose:
                        print(
                            f"    - Removing in module '{module_name}' the "
                            f"dependency on '{module_name_no_deps}'"
                        )
                    module_name_dep.remove(module_name_no_deps)

        return sorted_modules_list

    def view(self, indent=""):
        retstr: str = ""
        for module_info in self._module_name_to_modinfo.values():
            module_info: ModuleInfo

            retstr += f"{indent}- Module name: '{module_info.name}'\n"
            retstr += module_info.view(indent + "  ")

        return retstr
