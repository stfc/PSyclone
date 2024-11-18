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

from typing import Union, Set, List

from psyclone.errors import InternalError
from psyclone.parse.module_manager_base import ModuleManagerBase
from psyclone.parse.module_manager_auto_search import ModuleManagerAutoSearch
from psyclone.parse.module_manager_files_cached import ModuleManagerFilesCached


class ModuleManagerMultiplexer:
    """This class simply provides a multiplexer to a ModuleManager. This
    allows choosing between different module managers.

    Currently supported:

    - `ModuleManagerFiles`:
      Supports loading files once at the beginning.

    - `ModuleManagerAutoSearch`:
      Automatically finds and solves files of modules.
    """

    _singleton_instance: ModuleManagerBase = None

    # Default module management class
    _module_manager_class: ModuleManagerBase = ModuleManagerAutoSearch

    def __init__(self):
        if ModuleManagerMultiplexer._singleton_instance is not None:
            raise InternalError(
                "You need to use 'ModuleManagerMultiplexer.get_singleton()' "
                "to get the singleton instance."
            )

    @staticmethod
    def set_multiplexer_class(module_manager_class: ModuleManagerBase):
        """Set ModuleManager which should be used

        :param module_manager_class: ModuleManager to be used
        :type module_manager_class: ModuleManagerBase
        """

        assert type(module_manager_class) in [
            ModuleManagerBase,
            ModuleManagerFilesCached,
        ]
        ModuleManagerMultiplexer._module_manager_class = module_manager_class

    @staticmethod
    def get_singleton():
        """Static function that if necessary creates and returns the singleton
        ModuleManager instance.
        """

        if ModuleManagerMultiplexer._singleton_instance is None:
            ModuleManagerMultiplexer._singleton_instance = (
                ModuleManagerMultiplexer._module_manager_class()
            )

        return ModuleManagerMultiplexer._singleton_instance
