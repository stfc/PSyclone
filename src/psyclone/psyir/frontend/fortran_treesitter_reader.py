# BSD 3-Clause License
#
# Copyright (c) 2025, Science and Technology Facilities Council.
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
# Author: S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' PSyIR TreeSitter Fortran reader '''

from psyclone.psyir.nodes import *
from psyclone.psyir.nodes.codeblock import TSCodeBlock


class FortranTreeSitterReader():
    ''' TreeSitter to PSyIR  '''

    def __init__(self):
        self.location = None
        self._ongoing_codeblock = []
        self.handlers = {
            'translation_unit': self._file_container
        }

    def generate_psyir(self, tsnode):
        return self.get_handler(tsnode)(tsnode)

    def process_nodes(self, list_of_nodes):
        children = []
        for tsnode in list_of_nodes:
            try:
                handler = self.get_handler(tsnode)
                children.append(handler(tsnode))
            except NotImplementedError:
                if not self._ongoing_codeblock:
                    self._ongoing_codeblock.append(tsnode)
                if not isinstance(self.location, Schedule):
                    children.append(self.generate_accomulated_codeblock())
        return children

    def generate_accomulated_codeblock(self, message=None):

        if isinstance(self.location, (Schedule, Container)):
            structure = CodeBlock.Structure.STATEMENT
        elif isinstance(self.location, Directive):
            raise InternalError(
                "Fparser2Reader:nodes_to_code_block: A CodeBlock with "
                "a Directive as parent is not yet supported.")
        else:
            structure = CodeBlock.Structure.EXPRESSION

        code_block = TSCodeBlock(self._ongoing_codeblock, structure)
        self._ongoing_codeblock = []
        if message:
            code_block.preceding_comment = message

        return code_block

    def get_handler(self, tsnode):
        handler = self.handlers.get(tsnode.type)
        if not handler:
            raise NotImplementedError(
                f"Unsupported '{tsnode.type}' tree-sitter node.")
        return handler

    def _file_container(self, tsnode):
        file_container = FileContainer("test")
        self.location = file_container
        file_container.children.extend(self.process_nodes(tsnode.children))
        return file_container
