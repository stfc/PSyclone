# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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
# Modifications: A. R. Porter and N. Nobre, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module provides the PSyIR Fortran front-end.'''

from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.parser import ParserFactory
from fparser.two.symbol_table import SYMBOL_TABLES
from fparser.two.utils import NoMatchError
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import Schedule, Assignment
from psyclone.psyir.symbols import SymbolError, SymbolTable


class FortranReader(object):
    ''' PSyIR Fortran frontend. This frontend translates Fortran from a string
    or a file into PSyIR using the fparser2 utilities.

    '''
    # Save parser object across instances to reduce the initialisation time
    _parser = None

    def __init__(self):
        if not self._parser:
            self._parser = ParserFactory().create(std="f2008")
        self._processor = Fparser2Reader()
        SYMBOL_TABLES.clear()

    def psyir_from_source(self, source_code):
        ''' Generate the PSyIR tree representing the given Fortran source code.

        :param str source_code: text representation of the code to be parsed.

        :returns: PSyIR representing the provided Fortran source code.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        SYMBOL_TABLES.clear()
        string_reader = FortranStringReader(source_code)
        parse_tree = self._parser(string_reader)
        psyir = self._processor.generate_psyir(parse_tree)
        return psyir

    def psyir_from_expression(self, source_code, symbol_table):
        '''
        Generate the PSyIR tree for the supplied Fortran expression.
        Any symbols referenced in the expression must be present in the
        supplied table (or one of its ancestors).

        :param str source_code: text of the expression to be parsed.
        :param symbol_table: the SymbolTable in which to search for any \
                             symbols that are encountered.
        :returns: PSyIR representing the provided Fortran expression.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        :raises TypeError: if no valid SymbolTable is supplied.
        :raises ValueError: if the supplied source does not represent a \
            Fortran expression.
        :raises SymbolError: if the expression references a symbol which \
            cannot be found in the symbol table (and there is no way for it \
            to be brought into scope).
        '''
        if not isinstance(symbol_table, SymbolTable):
            raise TypeError(f"Must be supplied with a valid SymbolTable but "
                            f"got '{type(symbol_table).__name__}'")

        try:
            parse_tree = Fortran2003.Expr(source_code)
        except NoMatchError as err:
            raise ValueError(
                f"Supplied source does not represent a Fortran "
                f"expression: '{source_code}'") from err

        # Create a fake sub-tree connected to the supplied symbol table so
        # that we can process the expression and lookup any symbols that it
        # references. We provide the SymbolTable directly to the private
        # attribute to avoid the symbol table's node link to connect to the
        # new Schedule and therfore stealing it from its original scope.
        # pylint: disable=protected-access
        fake_parent = Schedule()
        fake_parent._symbol_table = symbol_table
        fake_parent.addchild(Assignment())

        try:
            # Process the expression, giving the Assignment we've just
            # created as the parent.
            self._processor.process_nodes(fake_parent[0], [parse_tree])
        except SymbolError as err:
            raise SymbolError(f"Expression '{source_code}' contains symbols "
                              f"which are not present in any symbol table and "
                              f"there are no wildcard imports which might be "
                              f"bringing them into scope.") from err
        return fake_parent[0].children[0].detach()

    def psyir_from_statement(self, source_code, symbol_table):
        '''
        Generate the PSyIR tree for the supplied Fortran statement.
        Any symbols referenced in the statement must be present in the supplied
        table (or one of its ancestors).

        :param str source_code: text of the statement to be parsed.
        :param symbol_table: the SymbolTable in which to search for any \
                             symbols that are encountered.
        :returns: PSyIR representing the provided Fortran statement.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        :raises TypeError: if no valid SymbolTable is supplied.
        :raises ValueError: if the supplied source does not represent a \
            Fortran statement.
        :raises SymbolError: if the expression references a symbol which \
            cannot be found in the symbol table (and there is no way for it \
            to be brought into scope).
        '''
        if not isinstance(symbol_table, SymbolTable):
            raise TypeError(f"Must be supplied with a valid SymbolTable but "
                            f"got '{type(symbol_table).__name__}'")
        string_reader = FortranStringReader(source_code)
        try:
            exec_part = Fortran2003.Execution_Part(string_reader)
        except NoMatchError as err:
            raise ValueError(f"Supplied source does not represent a Fortran "
                             f"statement: '{source_code}'") from err

        # Create a fake sub-tree connected to the supplied symbol table so
        # that we can process the statement and lookup any symbols that it
        # references.
        fake_parent = Schedule(symbol_table=symbol_table.deep_copy())

        try:
            # Process the statement, giving the Schedule we've just
            # created as the parent.
            self._processor.process_nodes(fake_parent, exec_part.children)
        except SymbolError as err:
            raise SymbolError(
                f"Statement '{source_code}' contains symbols which are not "
                f"present in any symbol table and there are no wildcard"
                f"imports which might be bringing them into scope.") from err
        return fake_parent[0].detach()

    def psyir_from_file(self, file_path):
        ''' Generate the PSyIR tree representing the given Fortran file.

        :param file_path: path of the file to be read and parsed.
        :type file_path: str or any Python Path format.

        :returns: PSyIR representing the provided Fortran file.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        SYMBOL_TABLES.clear()

        # Note that this is the main performance hotspot in PSyclone, taking
        # more than 90% of the runtime in some cases. Therefore this is a good
        # place to implement caching in order to avoid repeating parsing steps
        # that have already been done before.

        with open(file_path, "r") as source:
            return self.psyir_from_source(source.read())


# For Sphinx AutoAPI documentation generation
__all__ = ['FortranReader']
