# BSD 3-Clause License
#
# Copyright (c) 2021-2026, Science and Technology Facilities Council.
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
# Modifications: A. R. Porter, N. Nobre and R. W. Ford, STFC Daresbury Lab
#                J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module provides the PSyIR Fortran front-end.'''

import re

from typing import Optional, Union, List
from psyclone.configuration import Config
from psyclone.psyir.nodes import Assignment, Node, Routine, Schedule
from psyclone.psyir.symbols import SymbolTable


class FortranReader():
    ''' PSyIR Fortran frontend. This frontend translates Fortran from a string
    or a file into PSyIR.

    :param free_form: If parsing free-form code or not (default True).
    :param ignore_comments: If comments should be ignored or not
        (default True).
    :param ignore_directives: If directives should be ignored or not
        (default True). Only has an effect if ignore_comments is False.
    :param conditional_openmp_statements: whether to keep statements with the
        OpenMP conditional compilation prefix.
    :param last_comments_as_codeblocks: If the last comments in the a given
        block (e.g. subroutine, do, if-then body, etc.) should be kept as
        Codeblocks or lost (default False). Only has an effect if
        ignore_comments is False.
    :param resolve_modules: Whether to resolve modules while parsing a file,
        for more precise control it also accepts a list of module names.
        Defaults to False.

    :raises ValueError: If ignore_directives is set to False but
                        ignore_comments is set to True.

    '''

    def __init__(self,
                 free_form: bool = True,
                 ignore_comments: bool = True,
                 ignore_directives: bool = True,
                 conditional_openmp_statements: bool = False,
                 last_comments_as_codeblocks: bool = False,
                 resolve_modules: Union[bool, List[str]] = False):

        if ignore_comments and not ignore_directives:
            raise ValueError(
                "Setting ignore_directives to False in the FortranReader "
                "will only have an effect if ignore_comments is also set "
                "to False."
            )
        self._ignore_comments = ignore_comments
        self._ignore_directives = ignore_directives
        self._conditional_openmp_statements = conditional_openmp_statements
        self._free_form = free_form

        # The frontend reader imports are intentionally inside this condition
        # to lazily import them only when they are needed
        # pylint: disable=import-outside-toplevel
        if Config.get().frontend == 'treesitter':
            from psyclone.psyir.frontend.fortran_treesitter_reader import (
                FortranTreeSitterReader)
            self._processor = FortranTreeSitterReader()
        else:
            from psyclone.psyir.frontend.fparser2 import Fparser2Reader
            self._processor = Fparser2Reader(ignore_directives,
                                             last_comments_as_codeblocks,
                                             resolve_modules)

    @staticmethod
    def validate_name(name: str):
        '''
        Utility method that checks that the supplied name is a valid
        Fortran name.

        :param name: the name to check.

        :raises TypeError: if the name is not a string.
        :raises ValueError: if this is not a valid name.

        '''
        if not isinstance(name, str):
            raise TypeError(
                f"A name should be a string, but found "
                f"'{type(name).__name__}'.")
        if not re.match(r"^[A-Z]\w*$", name, flags=re.I):
            raise ValueError(
                f"Invalid Fortran name '{name}' found.")

    def psyir_from_source(self, source_code: str) -> Node:
        ''' Generate the PSyIR tree for the given Fortran source code.

        :param source_code: text representation of the code to be parsed.

        :returns: the PSyIR of the provided Fortran source code.

        :raises ValueError: if the supplied Fortran cannot be parsed.

        '''
        tree = self._processor.generate_parse_tree(
                source_code, None, self._ignore_comments, self._free_form,
                self._ignore_directives, self._conditional_openmp_statements)
        psyir = self._processor.generate_psyir(tree)
        return psyir

    def psyir_from_expression(self, source_code: str,
                              symbol_table: Optional[SymbolTable] = None):
        '''Generate the PSyIR tree for the supplied Fortran statement. The
        symbol table is expected to provide all symbols found in the
        expression.

        :param source_code: text of the expression to be parsed.
        :param symbol_table: the SymbolTable in which to search for any
            symbols that are encountered.

        :returns: PSyIR representing the provided Fortran expression.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        :raises TypeError: if no valid SymbolTable is supplied.
        :raises ValueError: if the supplied source does not represent a
            Fortran expression.

        '''
        if symbol_table is None:
            symbol_table = SymbolTable()
        elif not isinstance(symbol_table, SymbolTable):
            raise TypeError(f"Must be supplied with a valid SymbolTable but "
                            f"got '{type(symbol_table).__name__}'")

        tree = self._processor.generate_parse_tree(
                source_code, None, self._ignore_comments, self._free_form,
                self._ignore_directives, self._conditional_openmp_statements,
                partial_code="expression")

        # Create a fake sub-tree connected to the supplied symbol table so
        # that we can process the expression and lookup any symbols that it
        # references. We provide the SymbolTable directly to the private
        # attribute to avoid the symbol table's node link to connect to the
        # new Schedule and therefore stealing it from its original scope.
        # pylint: disable=protected-access
        fake_parent = Schedule()
        # pylint: disable=protected-access
        fake_parent._symbol_table = symbol_table
        fake_parent.addchild(Assignment())
        self._processor.process_nodes(fake_parent[0], [tree])
        return fake_parent[0].children[0].detach()

    def psyir_from_statement(self, source_code: str,
                             symbol_table: Optional[SymbolTable] = None):
        '''Generate the PSyIR tree for the supplied Fortran statement. The
        symbol table is expected to provide all symbols found in the
        statement.

        :param source_code: text of the statement to be parsed.
        :param symbol_table: the SymbolTable in which to search for any
            symbols that are encountered.

        :returns: PSyIR representing the provided Fortran statement.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        :raises TypeError: if no valid SymbolTable is supplied.
        :raises ValueError: if the supplied source does not represent a
            Fortran statement.

        '''
        if symbol_table is None:
            symbol_table = SymbolTable()
        elif not isinstance(symbol_table, SymbolTable):
            raise TypeError(f"Must be supplied with a valid SymbolTable but "
                            f"got '{type(symbol_table).__name__}'")

        tree = self._processor.generate_parse_tree(
                source_code, None, self._ignore_comments, self._free_form,
                self._ignore_directives, self._conditional_openmp_statements,
                partial_code="statement")
        # Create a fake sub-tree connected to the supplied symbol table so
        # that we can process the statement and lookup any symbols that it
        # references.
        routine_name = "dummy"
        fake_parent = Routine.create(
            routine_name, SymbolTable(), [])
        # pylint: disable=protected-access
        fake_parent._symbol_table = symbol_table

        # Process the statement, giving the Routine we've just
        # created as the parent.
        self._processor.process_nodes(fake_parent, tree.children)
        return fake_parent[0].detach()

    def psyir_from_file(self, file_path):
        ''' Generate the PSyIR tree representing the given Fortran file.

        :param file_path: path of the file to be read and parsed.
        :type file_path: str or any Python Path format.

        :returns: PSyIR representing the provided Fortran file.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        :raises ValueError: if the parser fails to parse the contents of
                            the supplied file.
        '''
        tree = self._processor.generate_parse_tree(
                None,
                file_path,
                self._ignore_comments,
                self._free_form,
                self._ignore_directives,
                self._conditional_openmp_statements
        )
        psyir = self._processor.generate_psyir(tree)
        psyir.name = str(file_path).rsplit('/', maxsplit=1)[-1]
        return psyir


# For Sphinx AutoAPI documentation generation
__all__ = ['FortranReader']
