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
# Authors: J. Henrichs, Bureau of Meteorology
#          A. R. Porter, STFC Daresbury Laboratory

"""This module contains the ModuleInfo class, which is used to store
and cache information about a module.

"""

from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.parser import ParserFactory
from fparser.two.utils import FortranSyntaxError, walk

from typing import Dict

import psyclone
from psyclone.configuration import Config
from psyclone.errors import InternalError, PSycloneError, GenerationError
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import Container, FileContainer
from psyclone.psyir.symbols import Symbol, SymbolError
from psyclone.parse import FileInfo

from typing import Set


class ModuleInfoError(PSycloneError):
    """
    PSyclone-specific exception for use when an error with the module manager
    happens - typically indicating that some module information cannot be
    found.

    :param str value: the message associated with the error.
    """

    def __init__(self, value):
        PSycloneError.__init__(self, value)
        self.value = "ModuleInfo error: " + str(value)


class ModuleInfo:
    # pylint: disable=too-many-instance-attributes
    """This class stores mostly cached information about a Fortran module.
    It stores a FileInfo object holding details on the original source file.
    If required it will parse this file and then cache the fparser2 parse tree.
    Similarly, it will also process this parse tree to
    create the corresponding PSyIR which is also then cached.

    :param str name: the module name.
    :param finfo: object holding information on the source file which defines
        this module.
    :type finfo: :py:class:`psyclone.parse.FileInfo`
    """

    def __init__(self, name: str, file_info: FileInfo):
        self._name = name.lower()

        # File handler including fparser and psyir representation
        self._file_info: FileInfo = file_info

        # A cache for the fparser tree solely for this module
        self._fparser_tree: Fortran2003 = None

        # Whether we've attempted to parse the source.
        self._fparser_attempted = False

        # A cache for the PSyIR representation
        self._psyir_node: Container = None

        # A cache for the module dependencies: this is just a set
        # of all modules used by this module. Type: set[str]
        self._used_module_names: Set[str] = None

        # This is a dictionary containing the sets of symbols imported from
        # each module, indexed by the module names: dict[str, set[str]].
        self._used_symbols_from_module_name: Dict[str, set[str]] = None

        self._processor: Fparser2Reader = Fparser2Reader()

    @property
    def name(self) -> str:
        """:returns: the name of this module.
        :rtype: str

        """
        return self._name

    @property
    def filepath(self) -> str:
        """:returns: the filename that contains the source code for this
            module.
        :rtype: str

        """
        return self._file_info.get_filepath

    def get_source_code(self):
        """Returns the source code for the module using the associated
        FileInfo instance (which caches it).

        :returns: the source code.
        :rtype: str

        :raises ModuleInfoError: when the file cannot be read.

        """
        try:
            return self._file_info.get_source_code()
        except FileNotFoundError as err:
            raise ModuleInfoError(
                f"Could not find file '{self._file_info.get_filepath}' when trying"
                f" to read source code for module '{self._name}'"
            ) from err

    def get_fparser_tree(self):
        """Returns the fparser AST for this module. The first time, the file
        will be parsed by fparser using the Fortran 2008 standard. The AST is
        then cached for any future uses.

        :returns: the fparser AST for this module.
        :rtype: :py:class:`fparser.two.Fortran2003.Program`
        """

        if not self._fparser_attempted:
            # This way we avoid that any other function might trigger to
            # parse this file again (in case of parsing errors).
            self._fparser_attempted = True

            self._fparser_tree = self._file_info.get_fparser_node()

        return self._fparser_tree

    def _extract_import_information(self):
        """This internal function analyses a given module source file and
        caches which modules are imported (in self._used_modules), and which
        symbol is imported from each of these modules (in
        self._used_symbols_from_module).
        """

        # Make sure that this is not called twice
        assert self._used_module_names is None

        # Initialise the caches
        self._used_module_names = set()
        self._used_symbols_from_module_name = {}

        try:
            parse_tree = self.get_fparser_tree()
        except FortranSyntaxError:
            # TODO #11: Add proper logging
            # TODO #2120: Handle error
            print(
                f"[ModuleInfo._extract_import_information] Syntax error "
                f"parsing '{self.filepath} - ignored"
            )
            # Hide syntax errors
            return

        for use in walk(parse_tree, Fortran2003.Use_Stmt):
            # Ignore intrinsic modules:
            if str(use.items[0]) == "INTRINSIC":
                continue

            mod_name = str(use.items[2])
            self._used_module_names.add(mod_name)
            all_symbols = set()

            only_list = use.items[4]
            # If there is no only_list, then the set of symbols
            # will stay empty
            if only_list:
                # Parse the only list:
                for symbol in only_list.children:
                    all_symbols.add(str(symbol))

            self._used_symbols_from_module_name[mod_name] = all_symbols

    def get_used_modules(self):
        """This function returns a set of all modules `used` in this
        module. Fortran `intrinsic` modules will be ignored. The information
        is based on the fparser parse tree of the module (since fparser can
        handle more files than PSyir, like LFRic's `constants_mod` which has
        pre-processor directives).

        :returns: a set with all imported module names.
        :rtype: set[str]
        """

        if self._used_module_names is None:
            self._extract_import_information()

        return self._used_module_names

    def get_used_symbols_from_modules(self):
        """This function returns information about which modules are used by
        this module, and also which symbols are imported. The return value is
        a dictionary with the used module name as key, and a set of all
        imported symbol names as value.

        :returns: a dictionary that gives for each module name the set \
            of symbols imported from it.
        :rtype: dict[str, set[str]]
        """

        if self._used_symbols_from_module_name is None:
            self._extract_import_information()

        return self._used_symbols_from_module_name

    def _load_psyir(self):
        """Internal function to split loading the PsyIR from other tasks

        :returns: PSyIR representing this module.
        :rtype: :py:class:`psyclone.psyir.nodes.Container` | NoneType

        :raises InternalError: if the named Container (module) does not
            exist in the PSyIR.
        """

        try:
            fparser_tree = self.get_fparser_tree()
        except FortranSyntaxError as err:
            # TODO #11: Add proper logging
            print(f"Error parsing '{self.filepath}': '{err}'")
            return None

        if not fparser_tree:
            # TODO #11: Add proper logging
            print(f"Empty parse tree returned for '{self.filepath}'")
            return None

        try:
            psyir_node: FileContainer = self._processor.generate_psyir(
                fparser_tree
            )

        except (
            KeyError,
            SymbolError,
            InternalError,
            GenerationError,
        ) as err:
            # TODO #11: Add proper logging
            print(
                f"Error trying to create PSyIR for '{self.filepath}': "
                f"'{err}'"
            )
        return psyir_node

    def get_psyir(self):
        """Returns the PSyIR representation of this module. This is based
        on the fparser tree (see get_fparser_tree), and the information is
        cached. If the PSyIR must be modified, it needs to be copied,
        otherwise the modified tree will be returned from the cache in the
        future.

        If the conversion to PSyIR fails then None is returned.

        :returns: PSyIR representing this module.
        :rtype: :py:class:`psyclone.psyir.nodes.Container` | NoneType

        :raises InternalError: if the named Container (module) does not
            exist in the PSyIR.
        """

        if self._psyir_node is None:
            try:
                ptree = self.get_fparser_tree()
            except FortranSyntaxError as err:
                # TODO #11: Add proper logging
                print(f"Error parsing '{self.filepath}': '{err}'")
                return None

            if not ptree:
                # TODO #11: Add proper logging
                print(f"Empty parse tree returned for '{self.filepath}'")
                return None

            try:
                self._psyir_node = self._processor.generate_psyir(ptree)
            except (
                KeyError,
                SymbolError,
                InternalError,
                GenerationError,
            ) as err:
                # TODO #11: Add proper logging
                print(
                    f"Error trying to create PSyIR for '{self.filepath}': "
                    f"'{err}'"
                )
                return None

        # Return the Container with the correct name.
        for cntr in self._psyir_node.walk(Container):
            cntr: Container
            if cntr.name.lower() == self.name:
                return cntr

        # We failed to find the Container - double-check the parse tree
        for mod_stmt in walk(self.get_fparser_tree(), Fortran2003.Module_Stmt):
            if mod_stmt.children[1].string.lower() == self.name:
                # The module exists but we couldn't create PSyIR for it.
                # TODO #11: Add proper logging
                print(
                    f"File '{self.filepath}' does contain module "
                    f"'{self.name}' but PSyclone is unable to create the "
                    f"PSyIR of it."
                )
                return None

        raise InternalError(
            f"File '{self.filepath}' does not contain a "
            f"module named '{self.name}'"
        )

    def get_symbol_by_name(self, symbol_name: str) -> Symbol:
        """
        Gets the PSyIR Symbol with the supplied name from the Container
        representing this module (if available).

        This utility mainly exists to insulate the user from having to check
        that a Container has been successfully created for this module
        (it might not have been if the source of the module cannot be found
        or cannot be parsed) and that it contains the specified Symbol.

        :param str name: the name of the symbol to get from this module.

        :returns: the Symbol with the supplied name if the Container has
            been successfully created and contains such a symbol and None
            otherwise.
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol` | None
        """

        container = self.get_psyir()
        if not container:
            return None
        try:
            return container.symbol_table.lookup(symbol_name)
        except KeyError:
            return None
