# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Authors S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module provides the PSyIR Fortran front-end.'''

from fparser.two.parser import ParserFactory
from fparser.common.readfortran import FortranStringReader
from psyclone.psyir.frontend.fparser2 import Fparser2Reader


class FortranReader(object):
    ''' PSyIR Fortran frontend. This frontend translates Fortran from a string
    or a file into PSyIR using the fparser2 utilities.

    '''
    # Save parser object across instances.
    _parser = None

    def __init__(self):
        if not self._parser:
            self._parser = ParserFactory().create()
        self._processor = Fparser2Reader()

    def psyir_from_source(self, source_code):
        ''' Generate the PSyIR tree representing the given Fortran source code.

        :param str source_code: text representation of the code to be parsed.
        '''
        string_reader = FortranStringReader(source_code)
        parse_tree = self._parser(string_reader)
        psyir = self._processor.generate_psyir(parse_tree)
        return psyir

    def psyir_from_file(self, file_path):
        ''' Generate the PSyIR tree representing the given Fortran file.

        :param file_path: path of the file to be read and parsed.
        :type file_path: str or any Python Path format.
        '''
        # Note that this is the main performance hotspot in PSyclone, taking
        # more the 90% of the runtime in some cases. Therefore this is a good
        # place to implement caching in order to avoid repeating parsing steps
        # that have already been done before.

        with open(file_path, "r") as source:
            return self.psyir_from_source(source.read())


__all__ = ['FortranReader']
