# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2019, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the ContainerSymbol and its interfaces.'''

from psyclone.psyir.symbols import Symbol, SymbolError


class ContainerSymbol(Symbol):
    ''' Symbol that represents a reference to a Container. The reference
    is lazy evaluated, this means that the Symbol will be created without
    parsing and importing the referenced container, but this can be imported
    when needed.

    :param str name: name of the symbol.

    :raises TypeError: if the name is not a string
    '''
    def __init__(self, name):
        super(ContainerSymbol, self).__init__(name)

        if not isinstance(name, str):
            raise TypeError(
                "ContainerSymbol name attribute should be of type 'str'"
                " but '{0}' found.".format(type(name)))

        self._reference = None
        # At the moment we just have one ContainerSymbol interface, so we
        # always assign this interface to all ContainerSymbols, we may want
        # to pass the interface as a parameter when we have more than one.
        self._interface = FortranModuleInterface

    @property
    def container(self):
        ''' Returns the referenced container. If it is not available, use
        the interface to import the container

        :returns: referenced container.
        :rtype: :py:class:`psyclone.psyGen.Container`
        '''
        if not self._reference:
            self._reference = self._interface.import_container(self._name)
        return self._reference

    def __str__(self):
        string = self._name + ": <"
        if self._reference:
            string += "linked>"
        else:
            string += "not linked>"
        return string


# Classes below are not exposed in the psyclone.psyir.symbols

class ContainerSymbolInterface(object):
    ''' Abstract implementation of the ContainerSymbol Interface '''

    @staticmethod
    def import_container(name):
        ''' Abstract method to import an external container, the specific
        implementation depends on the language used.

        :param str name: name of the external entity to be imported.

        :raises NotImplementedError: this is an abstract method.
        '''
        raise NotImplementedError("Abstract method")


class FortranModuleInterface(ContainerSymbolInterface):
    ''' Implementation of ContainerSymbolInterface for Fortran modules '''

    @staticmethod
    def import_container(name):
        ''' Imports a Fortran module as a PSyIR container. The module is
        expected to be found in a Fortran source file with the same name
        as the module plus the '.[f|F]90' extension. The search
        locations are provided in-order by the Config include_paths
        attribute ('-I' in the psyclone script).

        :param str name: name of the module to be imported.

        :returns: container associated with the given name.
        :rtype: :py:class:`psyclone.psyGen.Container`

        :raises SymbolError: the given Fortran module is not found on the \
            import path.
        '''
        from os import listdir, path
        from fparser.two.parser import ParserFactory
        from fparser.common.readfortran import FortranFileReader
        from psyclone.configuration import Config
        from psyclone.psyir.frontend.fparser2 import Fparser2Reader

        for directory in Config.get().include_paths:
            for filename in [name+'.f90', name+'.F90']:
                if filename in listdir(directory):
                    # Parse the module source code
                    abspath = path.join(directory, filename)
                    reader = FortranFileReader(abspath,
                                               ignore_comments=True)
                    f2008_parser = ParserFactory().create(std="f2008")
                    ast = f2008_parser(reader)
                    fp2reader = Fparser2Reader()

                    # Generate the PSyIR container
                    container = fp2reader.generate_container(ast)

                    # Check the imported container is the expected one
                    if container.name != name:
                        raise ValueError(
                            "Error importing the Fortran module '{0}' into a "
                            "PSyIR container. The imported module has the "
                            "unexpected name: '{1}'."
                            "".format(name, container.name))

                    return container

        raise SymbolError(
            "Module '{0}' (expected to be found in '{0}.[f|F]90') not found in"
            " any of the include_paths directories {1}."
            "".format(name, Config.get().include_paths))
