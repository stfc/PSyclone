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


import copy
import warnings
from typing import List, Dict, Set, Union, final

from psyclone.errors import InternalError
from psyclone.parse.file_info import FileInfo
from psyclone.parse.module_info import ModuleInfo


class ModuleManagerBase:
    """This class implements the basic shared features to manage modules.

    There are two implementations available:
    - One which is based on automatically loading all files depending on each
      other
    - Another one where the files are explicitly provided
    """

    def __init__(self):
        # Dictionary to lookup modules of all files
        # Note that there can be multiple modules per file
        self._file_to_modules: Dict[str, List[ModuleInfo]] = {}

        # Dictionary of modules to lookup module info
        self._module_name_to_modinfo: Dict[str, ModuleInfo] = {}

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

    def get_module_info_by_module_name(self, module_name: str) -> ModuleInfo:
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
