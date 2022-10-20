# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council
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
# Author R. W. Ford, STFC Daresbury Lab

'''Module containing the CommonMetadata base class which captures the
common functionality for LFRic kernel metadata.

'''
from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.parser import ParserFactory


# TODO issue #1886. This class and its subclasses may have
# commonalities with the GOcean metadata processing.
class CommonMetadata:
    '''Class to capture common LFRic kernel metadata.'''

    @staticmethod
    def create_fparser2(fortran_string, encoding):
        '''Creates an fparser2 tree from a Fortran string. The resultant
        parent node of the tree will be the same type as the encoding
        argument if the string conforms to the encoding, otherwise an
        exception will be raised.

        :param str fortran_string: a string containing the metadata in \
           Fortran.
        :param encoding: the parent class with which we will encode the \
            Fortran string.
        :type encoding: subclass of :py:class:`fparser.two.Fortran2003.Base`

        :returns: an fparser2 tree containing a metadata \
            argument.
        :rtype: subclass of :py:class:`fparser.two.Fortran2003.Base`

        :raises ValueError: if the Fortran string is not in the \
            expected form.

        '''
        _ = ParserFactory().create(std="f2003")
        reader = FortranStringReader(fortran_string)
        fparser2_tree = encoding(reader)
        if not fparser2_tree:
            raise ValueError(
                f"Expected kernel metadata to be a Fortran "
                f"{encoding.__name__}, but found '{fortran_string}'.")
        return fparser2_tree
