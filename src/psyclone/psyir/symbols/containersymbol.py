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

''' File Description '''

from psyclone.psyir.symbols import Symbol, SymbolError


class ContainerSymbol(Symbol):

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

    def resolve(self):
        self._reference = self._interface.import_container(self._name)

    @property
    def container(self):
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

    @staticmethod
    def import_container(name):
        raise NotImplementedError("Abstract method")


class FortranModuleInterface(ContainerSymbolInterface):

    @staticmethod
    def import_container(module_name):
        from os import listdir, path
        from fparser.two.parser import ParserFactory
        from fparser.common.readfortran import FortranFileReader
        from psyclone.configuration import Config
        from psyclone.psyir.frontend.fparser2 import Fparser2Reader

        filename = module_name + '.f90'
        for directory in Config.get().include_paths:
            if filename in listdir(directory):
                # Parse the module source code
                abspath = path.join(directory, filename)
                reader = FortranFileReader(abspath,
                                           ignore_comments=True)
                f2008_parser = ParserFactory().create(std="f2008")
                ast = f2008_parser(reader)
                fp2reader = Fparser2Reader()

                # Generate and return the PSyIR container
                return fp2reader.generate_container(ast)
        else:
            raise SymbolError(
                "Module {0} not found in any of the include_path "
                "directories {1}."
                "".format(filename, Config.get().include_paths))
