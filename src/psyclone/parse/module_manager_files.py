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

from typing import Union, Set, List

from psyclone.parse.file_info import FileInfo
from psyclone.parse.module_info import ModuleInfo
from psyclone.parse.module_manager_base import ModuleManagerBase
from psyclone.psyir.nodes import Node, Container


class ModuleManagerFiles(ModuleManagerBase):
    """This class implements an interface to the ModuleManager
    which is solely based on loading a list of files.
    """

    def __init__(self):
        super().__init__()

        self._module_name_to_modinfo

        self._filepaths_list: List[str] = []

    def load_from_files(self, filepaths: Union[str, List[str], Set[str]]):

        # Add files - this automatically creates file info
        self.add_files(filepaths)

        self.load_files()

    def process_files(self):
        assert self._module_name_to_modinfo == None
        assert self._filepath_to_module_info == None

        # iterate over all file infos and load psyir
        for filepath, file_info in self._filepath_to_file_info:
            file_info: FileInfo

            psyir_node: Node = file_info.get_psyir_node()

            # Walk over containers and add respective module information
            for container in psyir_node.walk(Container):
                container: Container

                container_name: str = container.name

                if container_name in self._module_name_to_modinfo.keys():
                    raise KeyError(
                        f"Module '{container_name}' already processed"
                    )

                self._module_name_to_modinfo[container_name] = ModuleInfo(
                    container_name, file_info
                )

    def load_psyir_node(self, verbose: bool = False):
        for filepath, fileinfo in self._filepath_to_file_info.items():
            fileinfo: FileInfo
            fileinfo.get_psyir_node(verbose=verbose)
