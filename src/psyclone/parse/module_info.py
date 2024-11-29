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
# Modified: M. Schreiber, Univ. Grenoble Alpes / Inria / Lab. Jean-Kuntzmann

'''This module contains the ModuleInfo class, which is used to store
and information about a module.

It makes use of the FileInfo class which is then used for caching.

'''

from typing import Dict, List, Union

from fparser.two import Fortran2003
from fparser.two.utils import FortranSyntaxError, walk

from psyclone.errors import InternalError, PSycloneError, GenerationError
from psyclone.parse.file_info import FileInfo
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import Container, FileContainer, Routine
from psyclone.psyir.symbols import Symbol, SymbolError
from psyclone.parse import FileInfo, FileInfoFParserError

from fparser.two.Fortran2003 import Program

class ModuleNotFoundError(PSycloneError):
    """Triggered when the Fortran module was not found"""

    def __init__(self, value):
        PSycloneError.__init__(self, value)
        self.value = "ModuleNotFoundError: " + str(value)


class ModuleInfoError(PSycloneError):
    """
    PSyclone-specific exception for use when an error with the module manager
    happens - typically indicating that some module information cannot be
    found.

    :param str value: the message associated with the error.

    """
    def __init__(self, value):
        PSycloneError.__init__(self, value)
        self.value = "ModuleInfoError: " + str(value)


# ============================================================================
class ModuleInfo:
    # pylint: disable=too-many-instance-attributes
    '''This class stores mostly cached information about a Fortran module.
    It stores a FileInfo object holding details on the original source file.
    If required it will parse this file and then cache the fparser2 parse tree.
    Similarly, it will also process this parse tree to
    create the corresponding PSyIR which is also then cached.
    '''
    def __init__(
        self,
        module_name: str,
        file_info: FileInfo,
        psyir_container_node: Container = None,
    ):
        """Constructor

        :param module_name: Name of the module
        :param file_info: FileInfo object to load the module from
        :type file_info: FileInfo
        :param psyir_container_node: The psyir container as an alternative to
            loading the psyir from FileInfo, defaults to None
        """
        self._name = module_name.lower()

        # Do some checking
        assert isinstance(file_info, FileInfo)
        if psyir_container_node is not None:
            assert isinstance(psyir_container_node, Container)

        # File handler including fparser and psyir representation
        self._file_info: FileInfo = file_info

        # The PSyIR representation
        self._psyir_container_node: Container = psyir_container_node

        # A cache for the module dependencies: this is just a set
        # of all modules used by this module.
        # We use an (ordered) list to preserve the order of the
        # used modules
        self._used_module_names: List[str] = None

        # This is a dictionary containing the sets of symbols imported from
        # each module, indexed by the module names: dict[str, set[str]].
        self._used_symbols_from_module_name: Dict[str, set[str]] = None

    # ------------------------------------------------------------------------
    @property
    def name(self):
        ''':returns: the name of this module.
        :rtype: str

        '''
        return self._name

    # ------------------------------------------------------------------------
    @property
    def filepath(self):
        ''':returns: the filepath that contains the source code for this
            module.
        :rtype: str

        '''
        return self._file_info.filepath

    # ------------------------------------------------------------------------
    def get_source_code(self):
        '''Returns the source code for the module using the associated
        FileInfo instance (which caches it).

        :returns: the source code.
        :rtype: str

        :raises ModuleInfoError: when the file cannot be read.

        '''
        try:
            return self._file_info.get_source_code()
        except FileNotFoundError as err:
            raise ModuleInfoError(
                f"Could not find file '{self._file_info.filepath}' when trying"
                f" to read source code for module '{self._name}'") from err

    # ------------------------------------------------------------------------
    def get_fparser_tree(self) -> Program:
        '''Returns the fparser AST for this module.

        :returns: The fparser AST for this module.

        :raises ModuleInfoError: When errors have been triggered to
                create the fparser tree.
        '''
        try:
            parse_tree = self._file_info.get_fparser_tree()
        except FileInfoFParserError as err:
            raise ModuleInfoError(f"Error parsing '{self.filepath}'\n"
                                  + str(err)) from err
        except FileNotFoundError as err:
            raise ModuleInfoError(
                f"Could not find file '{self._file_info.basename}' when trying"
                f" to read source code for module '{self.name}'") from err
        except FortranSyntaxError:
            # TODO #11: Add proper logging
            # TODO #2120: Handle error
            print(f"[ModuleInfo._extract_import_information] Syntax error "
                  f"parsing '{self.filepath} - ignored")
            # Hide syntax errors
            return

        return parse_tree
        # if not self._parse_attempted:
        #     # This way we avoid that any other function might trigger to
        #     # parse this file again (in case of parsing errors).
        #     self._parse_attempted = True

        #     reader = FortranStringReader(
        #         self.get_source_code(),
        #         include_dirs=Config.get().include_paths)
        #     parser = ParserFactory().create(std="f2008")
        #     self._parse_tree = parser(reader)

        # return self._parse_tree

    # ------------------------------------------------------------------------
    def _extract_used_module_names_from_fparser_tree(self):
        '''This internal function analyses a given module source file and
        caches which modules are imported (in self._used_modules), and which
        symbol is imported from each of these modules (in
        self._used_symbols_from_module_name).

        '''

        # Make sure that this is not called twice
        assert self._used_module_names is None

        # Initialise the caches
        self._used_module_names = []
        self._used_symbols_from_module_name = {}

        parse_tree = self.get_fparser_tree()

        for use in walk(parse_tree, Fortran2003.Use_Stmt):
            # Ignore intrinsic modules:
            if str(use.items[0]) == "INTRINSIC":
                continue

            mod_name = str(use.items[2])
            if mod_name not in self._used_module_names:
                self._used_module_names.append(mod_name)
            all_symbols = set()

            only_list = use.items[4]
            # If there is no only_list, then the set of symbols
            # will stay empty
            if only_list:
                # Parse the only list:
                for symbol in only_list.children:
                    all_symbols.add(str(symbol))

            self._used_symbols_from_module_name[mod_name] = all_symbols

    # ------------------------------------------------------------------------
    def get_used_modules(self) -> List[str]:
        '''This function returns a set of all modules `used` in this
        module. Fortran `intrinsic` modules will be ignored. The information
        is based on the fparser parse tree of the module (since fparser can
        handle more files than PSyir, like LFRic's `constants_mod` which has
        pre-processor directives).

        :returns: a set with all imported module names.
        :rtype: set[str]

        '''
        if self._used_module_names is None:
            self._extract_used_module_names_from_fparser_tree()

        return self._used_module_names

    # ------------------------------------------------------------------------
    def get_used_symbols_from_modules(self):
        '''This function returns information about which modules are used by
        this module, and also which symbols are imported. The return value is
        a dictionary with the used module name as key, and a set of all
        imported symbol names as value.

        :returns: a dictionary that gives for each module name the set \
            of symbols imported from it.
        :rtype: dict[str, set[str]]

        '''
        if self._used_symbols_from_module_name is None:
            self._extract_used_module_names_from_fparser_tree()

        return self._used_symbols_from_module_name


    def get_psyir_container_node(self):
        """Returns the PSyIR representation of this module. This is
        based on parsing the file with PSyIR.

        Note, that there is also the `get_psyir_node_from_fparser_tree`
        function which provides additional information by falling
        back to fparser.

        :returns: PSyIR representing this module.
        :rtype: :py:class:`psyclone.psyir.nodes.Container` | NoneType

        :raises ModuleNotFoundError: if the named Container (module) was
            not found.
        :raises FileNotFound: if the related file doesn't exist.
        :raises FileInfoFParserError: if the some FileInfoFParserError
            occured.
        :raises GenerationError: if the some PSyIR generation failed.
        """

        # If container node is not provided, we will load it
        if self._psyir_container_node is not None:
            return self._psyir_container_node

        psyir_node: FileContainer = self._file_info.get_psyir_node()
        container_list: List[Container] = psyir_node.walk(
            Container, stop_type=Routine
        )
        for container in container_list:
            container: Container

            # Sort out, e.g., FileContainer
            if type(container) is not Container:
                continue

            container_name: str = container.name.lower()
            if container_name == self._name:
                self._psyir_container_node = container
                return self._psyir_container_node

        raise ModuleNotFoundError(
            f"Unable to find container '{self._name}' in "
            f"file '{self._file_info.get_filepath()}'"
        )

    def get_psyir_container_node_alternative(self):
        '''Returns the PSyIR representation of this module. This is based
        on the fparser tree (see get_parse_tree), and the information is
        cached. If the PSyIR must be modified, it needs to be copied,
        otherwise the modified tree will be returned from the cache in the
        future.

        If the conversion to PSyIR fails then None is returned.

        :returns: PSyIR representing this module.
        :rtype: :py:class:`psyclone.psyir.nodes.Container` | NoneType

        :raises InternalError: if the named Container (module) does not
            exist in the PSyIR.

        '''
        if self._psyir_container_node is None:
            try:
                fparser_tree = self._file_info.get_fparser_tree()
            except ModuleInfoError as err:
                print(f"ModuleInfoError: {err.value}")
                return None
            except FileInfoFParserError as err:
                # TODO #11: Add proper logging
                print(f"Error parsing '{self.filepath}': '{err}'")
                return None
            if not fparser_tree:
                # TODO #11: Add proper logging
                print(f"Empty parse tree returned for '{self.filepath}'")
                return None
            try:
                processor = Fparser2Reader()
                self._psyir_container_node = processor.generate_psyir(
                    fparser_tree)
            except (KeyError,
                    SymbolError,
                    InternalError,
                    GenerationError) as err:
                # TODO #11: Add proper logging
                print(f"Error trying to create PSyIR for '{self.filepath}': "
                      f"'{err}'")
                return None

        # Return the Container with the correct name.
        for container_node in self._psyir_container_node.walk(Container):
            if container_node.name.lower() == self.name:
                return container_node

        # We failed to find the Container - double-check the parse tree
        for mod_stmt in walk(self.get_fparser_tree(), Fortran2003.Module_Stmt):
            if mod_stmt.children[1].string.lower() == self.name:
                # The module exists but we couldn't create PSyIR for it.
                # TODO #11: Add proper logging
                print(f"File '{self.filepath}' does contain module "
                      f"'{self.name}' but PSyclone is unable to create the "
                      f"PSyIR of it.")
                return None

        raise InternalError(f"File '{self.filepath}' does not contain a "
                            f"module named '{self.name}'")

    # ------------------------------------------------------------------------
    def get_symbol_by_name(self, symbol_name: str) -> Union[Symbol,None]:
        '''
        Gets the PSyIR Symbol with the supplied name from the Container
        representing this module (if available).

        This utility mainly exists to insulate the user from having to check
        that a Container has been successfully created for this module
        (it might not have been if the source of the module cannot be found
        or cannot be parsed) and that it contains the specified Symbol.

        :param symbol_name: the name of the symbol to get from this module.

        :returns: the Symbol with the supplied name if the Container has
            been successfully created and contains such a symbol and None
            otherwise.

        '''
        try:
            container: Container = self.get_psyir_container_node_alternative()

            # Handle "alternative" implementation of get_psyir_container_mode" 
            if container is None:
                return None

        except (FileNotFoundError, FileInfoFParserError, GenerationError):
            return None

        assert type(container) is Container

        try:
            return container.symbol_table.lookup(symbol_name)
        except KeyError:
            return None

    def view(self, indent=""):
        retstr = ""
        retstr += f"{indent}- name: '{self.name}'\n"
        retstr += f"{indent}- used_module_names: {self.get_used_modules()}\n"
        return retstr
