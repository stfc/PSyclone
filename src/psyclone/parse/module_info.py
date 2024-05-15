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
# Author J. Henrichs, Bureau of Meteorology

'''This module contains the ModuleInfo class, which is used to store
and cache information about a module: the filename, source code (if requested)
and the fparser tree (if requested), and information about any routines it
includes, and external symbol usage.
'''

import os

from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.parser import ParserFactory
from fparser.two.utils import FortranSyntaxError, walk

from psyclone.configuration import Config
from psyclone.errors import InternalError, PSycloneError
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import Container, FileContainer
from psyclone.psyir.symbols import SymbolError


# ============================================================================
class ModuleInfoError(PSycloneError):
    '''
    PSyclone-specific exception for use when an error with the module manager
    happens - typically indicating that some module information cannot be
    found.

    :param str value: the message associated with the error.

    '''
    def __init__(self, value):
        PSycloneError.__init__(self, value)
        self.value = "ModuleInfo error: "+str(value)


# ============================================================================
class ModuleInfo:
    # pylint: disable=too-many-instance-attributes
    '''This class stores mostly cached information about modules: it stores
    the original filename, if requested it will read the file and then caches
    the plain text file, and if required it will parse the file, and then
    cache the fparser AST.

    :param str name: the module name.
    :param finfo: 

    '''

    def __init__(self, name, finfo):
        self._name = name
        self._file_info = finfo

        # A cache for the fparser tree
        self._parse_tree = None

        # Whether we've attempted to parse the source.
        self._parse_attempted = False

        # A cache for the PSyIR representation
        self._psyir = None

        # A cache for the module dependencies: this is just a set
        # of all modules used by this module. Type: set[str]
        self._used_modules = None

        # This is a dictionary containing the sets of symbols imported from
        # each module, indexed by the module names: dict[str, set[str]].
        self._used_symbols_from_module = None

        # This variable will be a set that stores the name of all routines
        # (based on fparser), so we can test is a routine is defined
        # without having to convert the AST to PSyIR. It is initialised with
        # None so we avoid trying to parse a file more than once (parsing
        # errors would cause routine_names to be empty, so we can test
        # if routine_name is None vs if routine_names is empty)
        # TODO #2435: To be changed once we have support for interfaces
        self._routine_names = None

        # This map contains the list of routine names that are part
        # of the same generic interface.
        # TODO #2435: To be changed once we have support for interfaces
        self._generic_interfaces = {}

        self._processor = Fparser2Reader()

    # ------------------------------------------------------------------------
    @property
    def name(self):
        ''':returns: the name of this module.
        :rtype: str

        '''
        return self._name

    # ------------------------------------------------------------------------
    @property
    def filename(self):
        ''':returns: the filename that contains the source code for this
            module.
        :rtype: str

        '''
        return self._file_info.qualified_filename

    # ------------------------------------------------------------------------
    def get_source_code(self):
        '''Returns the source code for the module. The first time, it
        will be read from the file, but the data is then cached.

        :returns: the source code.
        :rtype: str

        :raises ModuleInfoError: when the file cannot be read.

        '''
        try:
            return self._file_info.source
        except FileNotFoundError as err:
            raise ModuleInfoError(
                f"Could not find file '{self._file_info.filename}' when trying"
                f" to read source code for module '{self._name}'") from err

    # ------------------------------------------------------------------------
    def get_parse_tree(self):
        '''Returns the fparser AST for this module. The first time, the file
        will be parsed by fparser using the Fortran 2008 standard. The AST is
        then cached for any future uses.

        :returns: the fparser AST for this module.
        :rtype: :py:class:`fparser.two.Fortran2003.Program`

        '''
        if not self._parse_attempted:
            # This way we avoid that any other function might trigger to
            # parse this file again (in case of parsing errors).
            self._parese_attempted = True

            reader = FortranStringReader(
                self._file_info.source,
                include_dirs=Config.get().include_paths)
            parser = ParserFactory().create(std="f2008")
            self._parse_tree = parser(reader)

        return self._parse_tree

    # ------------------------------------------------------------------------
    def _extract_import_information(self):
        '''This internal function analyses a given module source file and
        caches which modules are imported (in self._used_modules), and which
        symbol is imported from each of these modules (in
        self._used_symbols_from_module).

        '''
        # Initialise the caches:
        self._used_modules = set()
        self._used_symbols_from_module = {}

        try:
            parse_tree = self.get_parse_tree()
        except FortranSyntaxError:
            # TODO #11: Add proper logging
            # TODO #2120: Handle error
            print(f"[ModuleInfo._extract_import_information] Syntax error "
                  f"parsing '{self.filename} - ignored")
            # Hide syntax errors
            return
        for use in walk(parse_tree, Fortran2003.Use_Stmt):
            # Ignore intrinsic modules:
            if str(use.items[0]) == "INTRINSIC":
                continue

            mod_name = str(use.items[2])
            self._used_modules.add(mod_name)
            all_symbols = set()

            only_list = use.items[4]
            # If there is no only_list, then the set of symbols
            # will stay empty
            if only_list:
                # Parse the only list:
                for symbol in only_list.children:
                    all_symbols.add(str(symbol))

            self._used_symbols_from_module[mod_name] = all_symbols

    # ------------------------------------------------------------------------
    def get_used_modules(self):
        '''This function returns a set of all modules `used` in this
        module. Fortran `intrinsic` modules will be ignored. The information
        is based on the fparser parse tree of the module (since fparser can
        handle more files than PSyir, like LFRic's `constants_mod` which has
        pre-processor directives).

        :returns: a set with all imported module names.
        :rtype: set[str]

        '''
        if self._used_modules is None:
            self._extract_import_information()

        return self._used_modules

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
        if self._used_symbols_from_module is None:
            self._extract_import_information()

        return self._used_symbols_from_module

    # ------------------------------------------------------------------------
    def get_psyir(self):
        '''Returns the PSyIR representation of this module. This is based
        on the fparser tree (see get_parse_tree), and the information is
        cached. If the PSyIR must be modified, it needs to be copied,
        otherwise the modified tree will be returned from the cache in the
        future.
        If the conversion to PSyIR fails, a dummy FileContainer with an
        empty Container (module) is returned, which avoids additional error
        handling in many other subroutines.
        #TODO 2120: This should be revisited when improving on the error
        handling.

        :param routine_name: optional the name of a routine.
        :type routine_name: Optional[str]

        :returns: PSyIR representing this module.
        :rtype: list[:py:class:`psyclone.psyir.nodes.Node`]

        '''
        if self._psyir is None:
            try:
                self._psyir = \
                    self._processor.generate_psyir(self.get_parse_tree())
            except (KeyError, SymbolError, InternalError,
                    FortranSyntaxError) as err:
                print(f"Error trying to parse '{self.filename}': '{err}'")
                # TODO #11: Add proper logging
                # TODO #2120: Handle error better. Long term we should not
                # just ignore errors.
                # Create a dummy FileContainer with a dummy module. This avoids
                # additional error handling in other subroutines, since they
                # will all return 'no information', whatever you ask for
                self._psyir = FileContainer(os.path.basename(self.filename))
                module = Container("invalid-module")
                self._psyir.children.append(module)

        # TODO #2462: needs to be fixed to properly support multiple modules
        # in one file
        # Return the actual module Container (not the FileContainer)
        return self._psyir.children[0]
