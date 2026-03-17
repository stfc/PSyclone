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

from typing import Optional, TYPE_CHECKING

from psyclone.psyir import nodes
from psyclone.psyir.nodes.codeblock import TreeSitterCodeBlock, CodeBlock

if TYPE_CHECKING:
    # Purposely inside typechecking because at runtime we want to lazily
    # import the parser (only if it is actually used)
    from tree_sitter import Node as TSNode


def to_str(node: TSNode) -> str:
    '''
    :param node: a given treesitter node.
    :returns: the string representing the node in utf8
    '''
    return node.text.decode('utf8') if node.text else ""


class FortranTreeSitterReader():
    ''' Processes the TreeSitter parse_tree and converts it to PSyIR.

    Note: this class is in development, currently pretty much only generates
    a CodeBlock for anything provided to it.

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
        # TODO #3038 Arguments are currently not used nor typechecked, but if
        # we decide this is the common reader interface, this can be done in a
        # super class instead of duplicate it here.
        self._ignore_directives = ignore_directives
        self._resolve_modules = resolve_modules
        self._last_comments_as_codeblocks = last_comments_as_codeblocks
        self._psyir_cursor = None
        self._ongoing_codeblock = []
        self.handlers = {
            'translation_unit': self._translation_unit,
            'module': self._module_handler,
        }

    @classmethod
    def generate_parse_tree(
        cls,
        source_code: Optional[str] = None,
        file_path: Optional[str] = None,
        ignore_comments: bool = True,
        free_form: bool = True,
        ignore_directives: bool = True,
        conditional_openmp: bool = True,
        partial_code: str = ""
    ):
        ''' Use the provided source code and frontend options to generate
        a treesitter parsetree.

        :param source_code: the given source code.
        :param ignore_comments: whether to let the parser ignore comments.
        :param free_form: whether to parse using Fortran free_form syntax.
        :param ignore_directives: whether to ignore directives while parsing.
        :param conditional_openmp:
        :param partial_code: if the provided source_code is not a full unit
            this indicates the starting parsing point. It currently supports
            "expression" or "statement".

        '''
        # pylint: disable=unused-argument
        # Purposely inlined to lazily load this modules only when needed
        # pylint: disable=import-outside-toplevel
        import tree_sitter_fortran
        from tree_sitter import Language, Parser

        def report_errors(node):
            ''' Recursively find and report errors '''
            if node.type == 'ERROR':
                raise ValueError(
                    f"Syntax Error found at line {node.start_point[0] + 1}: "
                    f"{to_str(node)}")
            for child in node.children:
                report_errors(child)

        if file_path:
            with open(file_path, encoding="utf-8") as fortran_file:
                source_code = fortran_file.read()

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

        :returns: PSyIR of the supplied treesitter parse_tree.
        :rtype: :py:class:`psyclone.psyir.nodes.FileContainer`

        '''
        result = self.get_handler(parse_tree)(parse_tree)
        if filename and isinstance(result, nodes.FileContainer):
            result.name = filename
        return result

    def process_nodes(self, list_of_nodes):
        '''
        Create the PSyIR of the supplied list of treesitter nodes.

        :param nodes: List of sibling nodes in treesitter AST.
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
                children.append(self.generate_accomulated_codeblock())
        return children

    def generate_accomulated_codeblock(self, message: Optional[str] = None):
        '''
        Create a CodeBlock node with the contents accomulated in the
        _ongoing_codeblock list.

        :param message: comment to associate with the CodeBlock.

        '''
        if isinstance(self._psyir_cursor, (nodes.Schedule, nodes.Container)):
            structure = CodeBlock.Structure.STATEMENT
        else:  # pragma: no-cover
            # TODO #3038 Remove no-cover when parser reaches expressions
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
        file_container = nodes.FileContainer("")
        self._psyir_cursor = file_container
        file_container.children.extend(self.process_nodes(tsnode.children))
        return file_container

    def _module_handler(self, tsnode) -> nodes.Node:
        ''' Handle module treesitter node.

        :param tsnode: the node the process.
        :returns: the equivatent PSyIR Node.
        '''
        module_stmt, internal_proc, _end_module_stmt = tsnode.children
        _module_keyword, module_name = module_stmt.children
        container = nodes.Container(to_str(module_name))
        self._psyir_cursor = container
        container.children.extend(self.process_nodes([internal_proc]))
        return container
