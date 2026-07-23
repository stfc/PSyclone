# BSD 3-Clause License
#
# Copyright (c) 2025-2026, Science and Technology Facilities Council.
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

import codecs
import logging
from typing import TYPE_CHECKING, Iterable, Union, Callable

from psyclone.psyir import nodes
from psyclone.psyir.nodes.codeblock import TreeSitterCodeBlock, CodeBlock

if TYPE_CHECKING:
    # Purposely inside typechecking because at runtime we want to lazily
    # import the parser (only if it is actually used)
    from tree_sitter import Node as TSNode


def log_decode_error_handler(err) -> tuple[str, int]:
    '''
    A custom error handler for use when reading files. Removes any
    characters that cause decoding errors and logs the error.

    :param err: the given error.

    :returns: 2-tuple containing replacement for bad chars (an empty string
        and the position from where encoding should continue.
    '''
    # Log the fact that this character will be removed from the input file
    logging.getLogger(__name__).warning(
        "Skipped bad character in input file, %s", str(err))
    return ("", err.end)


codecs.register_error("treesitter-encoding", log_decode_error_handler)


def to_str(node: 'TSNode') -> str:
    '''
    :param node: a given treesitter node.
    :returns: the string representing the node in utf8.
    '''
    return node.text.decode('utf8') if node.text else ""


class FortranTreeSitterReader():
    ''' Processes the TreeSitter parse_tree and converts it to PSyIR.

    Note: this class is in development, currently only generates
    top-level Modules and CodeBlocks.

    The structure of the expected fortran parse tree can be found in the
    'rules' section of:
    https://github.com/stadelmanma/tree-sitter-fortran/blob/master/grammar.js
    To interpret the rules use:
    https://tree-sitter.github.io/tree-sitter/creating-parsers/
    2-the-grammar-dsl.html

    :param ignore_directives: Whether directives should be ignored or not
        (default True). Currently ignored.
    :param last_comments_as_codeblocks: Whether the last comments in the a
        given block (e.g. subroutine, do, if-then body, etc.) should be kept as
        CodeBlocks or lost (default False). Currently ignored.
    :param resolve_modules: Whether to resolve modules while parsing a file,
        for more precise control it also accepts a list of module names.
        Defaults to False. Currently ignored.
    :param ignore_comments: whether to let the parser ignore comments.
    :param free_form: whether to parse using Fortran free_form syntax.
    :param ignore_directives: whether to ignore directives while parsing.
    :param conditional_openmp: whether to parse conditional OpenMP statements.

    :raises TypeError: if any constructor argument is not of the expected type.
    '''

    def __init__(
        self,
        ignore_directives: bool = True,
        last_comments_as_codeblocks: bool = False,
        resolve_modules: bool = False,
        ignore_comments: bool = True,
        free_form: bool = True,
        conditional_openmp: bool = True,
    ):
        # TODO #3038 Arguments are currently not used nor typechecked, but if
        # we decide this is the common reader interface, this can be done in a
        # super class instead of duplicate it here.
        self._ignore_directives = ignore_directives
        self._resolve_modules = resolve_modules
        self._last_comments_as_codeblocks = last_comments_as_codeblocks
        self._ignore_comments = ignore_comments
        self._free_form = free_form
        self._conditional_openmp = conditional_openmp
        # TODO #3038: Currently this reader uses a cursor pointer instead of
        # passing around a parent argument all the time (like fparser's), but
        # this can be re-evaluated if necessary.
        self._psyir_cursor = None
        # Map from treesitter node types to their handler routine
        self.handlers = {
            'translation_unit': self._translation_unit,
            'module': self._module_handler,
        }

    def generate_parse_tree_from_file(self, file_path) -> 'TSNode':
        '''
        Use the provided file to generate a treesitter parsetree.

        :param file_path: a given file.

        :returns: the treesitter parsetree of the given file.
        '''
        with open(
            file_path, encoding="utf-8", errors="treesitter-encoding"
        ) as fortran_file:
            source_code = fortran_file.read()
        return self.generate_parse_tree_from_source(source_code)

    def generate_parse_tree_from_source(
        self, source_code: str, partial_code: str = ""
    ) -> 'TSNode':
        ''' Use the provided source code to generate a treesitter parsetree.

        :param source_code: the given source code.
        :param partial_code: if the provided source_code is not a full unit
            this indicates the starting parsing point. It currently supports
            "expression" or "statement".

        :returns: the treesitter parsetree of the given source code.
        '''
        # pylint: disable=unused-argument
        # Purposely inlined to lazily load this modules only when needed
        # pylint: disable=import-outside-toplevel
        import tree_sitter_fortran
        from tree_sitter import Language, Parser

        def report_errors(node: 'TSNode'):
            ''' Recursively find and report errors.

            :param node: the given treesitter node

            :raises ValueError: if the given node has a parsing error.
            '''
            if node.type == 'ERROR':
                raise ValueError(
                    f"Syntax Error found at line {node.start_point[0] + 1}: "
                    f"{to_str(node)}")
            for child in node.children:
                report_errors(child)

        language = Language(tree_sitter_fortran.language())
        parser = Parser(language)
        parse_tree = parser.parse(bytes(source_code, "utf8"))
        report_errors(parse_tree.root_node)
        return parse_tree.root_node

    def generate_psyir(self, parse_tree: 'TSNode') -> nodes.Node:
        '''Translate the supplied treesitter node into PSyIR.

        :param parse_tree: the supplied treesitter parse tree.

        :returns: the equivalent PSyIR Node.
        '''
        return self.process_nodes(parse_tree)[0]

    def process_nodes(self, tsnodes: Union["TSNode", Iterable["TSNode"]]):
        '''
        Create the PSyIR that represents the supplied treesitter nodes.

        :param nodes: the list of nodes to process, for convenience it accepts
            a single node or a list of them.

        :returns: the equivalent PSyIR Node.
        '''
        list_of_nodes = tsnodes if isinstance(tsnodes, Iterable) else [tsnodes]
        children = []
        for tsnode in list_of_nodes:
            try:
                handler = self.get_handler(tsnode)
                children.append(handler(tsnode))
            except NotImplementedError as err:
                # TODO #3038: Add support for expression codeblocks and
                # aggregating contiguous codeblocks into a single one.
                structure = CodeBlock.Structure.STATEMENT
                code_block = TreeSitterCodeBlock(tsnode, structure)
                code_block.append_preceding_comment(
                    f"PSyclone CodeBlock (unsupported code) reason:\n"
                    f"- {err}"
                )
                children.append(code_block)
        return children

    def get_handler(self, tsnode: 'TSNode') -> Callable:
        '''
        :param tsnode: a given treesitter node.

        :returns: the method that handles the given node type.

        :raises NotImplementedError: if the given node type does not have a
            handler for it.
        '''
        handler = self.handlers.get(tsnode.type)
        if not handler:
            raise NotImplementedError(
                f"Unsupported '{tsnode.type}' tree-sitter node.")
        return handler

    def _translation_unit(self, tsnode: 'TSNode') -> nodes.Node:
        ''' Handle treesitter 'translation_unit' node.

        :param tsnode: the treesitter node the process.

        :returns: the equivalent PSyIR Node.
        '''
        file_container = nodes.FileContainer("")
        self._psyir_cursor = file_container
        file_container.children.extend(self.process_nodes(tsnode.children))
        return file_container

    def _module_handler(self, tsnode: 'TSNode') -> nodes.Node:
        ''' Handle a treesitter 'module' node.

        :param tsnode: the treesitter node the process.

        :returns: the equivalent PSyIR Node.

        :raises NotImplementedError: if the module has an unsupported child.
        :raises NotImplementedError: if the module permits implicit variables.
        '''
        module_name = None
        internal_proc = None
        implicit_statement = False
        for child in tsnode.children:
            if child.type == "module_statement":
                _module_keyword, module_name = child.children
            elif child.type == "end_module_statement":
                pass
            elif child.type == "internal_procedures":
                internal_proc = child
            elif child.type == "implicit_statement":
                implicit_statement = True
            else:
                raise NotImplementedError(
                    f"Module has an unsupported '{child.type}' node")

        if not implicit_statement:
            raise NotImplementedError(
                "Modules that allow implicit variables are not supported")
        container = nodes.Container(to_str(module_name) if module_name else "")
        self._psyir_cursor = container
        if internal_proc:
            container.children.extend(self.process_nodes(internal_proc))
        return container
