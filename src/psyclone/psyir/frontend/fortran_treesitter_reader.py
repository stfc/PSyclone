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

from typing import Optional

from psyclone.psyir import nodes
from psyclone.psyir.nodes.codeblock import TreeSitterCodeBlock, CodeBlock


class FortranTreeSitterReader():
    ''' Processes the TreeSitter parse_tree and converts it to PSyIR.

    :param ignore_directives: Whether directives should be ignored or not
        (default True). Currently ignored.
    :param last_comments_as_codeblocks: Whether the last comments in the a
        given block (e.g. subroutine, do, if-then body, etc.) should be kept as
        CodeBlocks or lost (default False). Currently ignored.
    :param resolve_modules: Whether to resolve modules while parsing a file,
        for more precise control it also accepts a list of module names.
        Defaults to False. Currently ignored.

    :raises TypeError: if the constructor argument is not of the expected type.
    '''

    def __init__(self, ignore_directives: bool = True,
                 last_comments_as_codeblocks: bool = False,
                 resolve_modules: bool = False):
        self._ignore_directives = ignore_directives
        self._resolve_modules = resolve_modules
        self._last_comments_as_codeblocks = last_comments_as_codeblocks
        self.location = None
        self._ongoing_codeblock = []
        self.handlers = {
            'translation_unit': self._translation_unit
        }

    @classmethod
    def text_to_parse_tree(cls, source_code, ignore_comments, free_form,
                           ignore_directives, conditional_openmp,
                           partial_code=None):
        def report_errors(node):
            ''' Recursively find and report errors '''
            if node.type == 'ERROR':
                raise ValueError(
                    f"Syntax Error found at line {node.start_point[0] + 1}: "
                    f"{node.text.decode('utf8')}")
            for child in node.children:
                report_errors(child)

        import tree_sitter_fortran
        from tree_sitter import Language, Parser
        language = Language(tree_sitter_fortran.language())
        parser = Parser(language)
        parse_tree = parser.parse(bytes(source_code, "utf8"))
        report_errors(parse_tree.root_node)
        return parse_tree.root_node

    def generate_psyir(self, parse_tree, filename=""):
        '''Translate the supplied treesitter node to PSyIR.

        :param parse_tree: the supplied treesitter parse tree.
        :type parse_tree: :py:class:`fparser.two.Fortran2003.Program`
        :param Optional[str] filename: associated name for FileContainer.

        :returns: PSyIR of the supplied fparser2 parse_tree.
        :rtype: :py:class:`psyclone.psyir.nodes.FileContainer`

        :raises GenerationError: if the root of the supplied fparser2
            parse tree is not a Program.

        '''
        return self.get_handler(parse_tree)(parse_tree)

    def process_nodes(self, list_of_nodes):
        '''
        Create the PSyIR of the supplied list of treesitter nodes.

        :param nodes: List of sibling nodes in fparser2 AST.
        :type nodes: list[:py:class:`fparser.two.utils.Base`]

        '''
        children = []
        for tsnode in list_of_nodes:
            try:
                handler = self.get_handler(tsnode)
                children.append(handler(tsnode))
            except NotImplementedError:
                if not self._ongoing_codeblock:
                    self._ongoing_codeblock.append(tsnode)
                if not isinstance(self.location, nodes.Schedule):
                    children.append(self.generate_accomulated_codeblock())
        return children

    def generate_accomulated_codeblock(self, message: Optional[str] = None):
        '''
        Create a CodeBlock node with the contents accomulated in the
        _ongoing_codeblock list.

        :param message: comment to associate with the CodeBlock.

        '''
        if isinstance(self.location, (nodes.Schedule, nodes.Container)):
            structure = CodeBlock.Structure.STATEMENT
        else:
            structure = CodeBlock.Structure.EXPRESSION

        code_block = TreeSitterCodeBlock(self._ongoing_codeblock, structure)
        self._ongoing_codeblock = []
        if message:
            code_block.preceding_comment = message

        return code_block

    def get_handler(self, tsnode):
        '''
        :param tsnode: a given treesitter node.

        :returns: the method that handles the given node type.
        '''
        handler = self.handlers.get(tsnode.type)
        if not handler:
            raise NotImplementedError(
                f"Unsupported '{tsnode.type}' tree-sitter node.")
        return handler

    def _translation_unit(self, tsnode) -> nodes.Node:
        ''' Handle translation_unit treesitter node.

        :param tsnode: the node the process.
        :returns: the equivatent PSyIR Node.
        '''
        file_container = nodes.FileContainer("test")
        self.location = file_container
        file_container.children.extend(self.process_nodes(tsnode.children))
        return file_container
