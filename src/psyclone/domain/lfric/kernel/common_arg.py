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

'''Module containing the BaseArg class which captures the metadata
associated with a generic LFRic argument. Supports the creation,
modification and Fortran output of such an argument.

'''
from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.parser import ParserFactory

from psyclone.domain.lfric import LFRicConstants


class CommonArg():
    '''Class to capture commone LFRic kernel metadata.

    :param Optional[str] datatype: the datatype of this argument.
    :param Optional[str] access: the way the kernel accesses this \
        argument.

    '''
    def __init__(self, datatype=None, access=None):
        self._form = None
        if datatype is None:
            self._datatype = datatype
        else:
            self.datatype = datatype
        if access is None:
            self._access = access
        else:
            self.access = access

    @staticmethod
    def create_part_ref(fortran_string):
        '''Creates an fparser tree from a Fortran string. Expects the parent
        node of the tree to be a Part_Ref object (as it represents a metadata
        argument).

        :param str fortran_string: a string containing the metadata in \
           Fortran.

        :returns: an fparser2 tree containing the PSyIR for a metadata \
            argument.
        :rtype: :py:class:`fparser.two.Fortran2003.Part_Ref`

        :raises ValueError: if the Fortran string is not in the \
            expected form.

        '''
        _ = ParserFactory().create(std="f2003")
        reader = FortranStringReader(fortran_string)
        part_ref = Fortran2003.Part_Ref(reader)
        if not part_ref:
            raise ValueError(
                f"Expected kernel metadata to be a Fortran part reference, "
                f"with the form 'arg_type(...)' but found '{fortran_string}'.")
        return part_ref

    @staticmethod
    def check_psyir(psyir, nargs):
        '''Checks that the psyir argument is valid.

        :param psyir: fparser2 tree containing the PSyIR for a metadata \
            argument.
        :type psyir: :py:class:`fparser.two.Fortran2003.Part_Ref`
        :param Optional[int] nargs: the number of expected arguments.

        :raises TypeError: if the psyir argument is not an fparser2 \
            Part_Ref object.
        :raises ValueError: if the kernel metadata is not in \
            the form arg_type(...).
        :raises ValueError: if the kernel metadata does not \
            contain nargs arguments.

        '''
        if not isinstance(psyir, Fortran2003.Part_Ref):
            raise TypeError(
                f"Expected kernel metadata to be encoded as a "
                f"Fortran Part_Ref object but found type "
                f"'{type(psyir).__name__}' with value '{str(psyir)}'.")
        if not psyir.children[0].tostr().lower() == "arg_type":
            raise ValueError(
                f"Expected kernel metadata to have the name "
                f"'arg_type' and be in the form 'arg_type(...)', but found "
                f"'{str(psyir)}'.")
        if len(psyir.children[1].children) != nargs:
            raise ValueError(
                f"Expected kernel metadata to have {nargs} "
                f"arguments, but found {len(psyir.children[1].children)} in "
                f"'{str(psyir)}'.")

    @property
    def datatype(self):
        '''
        :returns: the datatype for this metadata argument.
        :rtype: str
        '''
        return self._datatype

    @staticmethod
    def check_datatype(value):
        raise NotImplementedError("Error")

    @datatype.setter
    def datatype(self, value):
        '''
        :param str value: set the datatype to the \
            specified value.
        '''
        self.check_datatype(value)
        self._datatype = value

    @staticmethod
    def check_access(value):
        raise NotImplementedError("Error")

    @property
    def access(self):
        '''
        :returns: the access descriptor for this scalar \
            argument.
        :rtype: str
        '''
        return self._access

    @access.setter
    def access(self, value):
        '''
        :param str value: set the access descriptor to the \
            specified value.

        '''
        self.check_access(value)
        self._access = value

    @property
    def form(self):
        '''
        :returns: the form descriptor for this scalar \
            argument. This is always GH_SCALAR.
        :rtype: str
        '''
        return self._form
