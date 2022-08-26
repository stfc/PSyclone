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

'''Module containing the ScalarArg class which captures the metadata
associated with a scalar argument. Supports the creation, modification
and Fortran output of a Scalar argument.

'''
from fparser.two import Fortran2003
from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel import CommonArg


class ScalarArg(CommonArg):
    '''Class to capture LFRic kernel metadata information for a scalar
    argument.

    :param Optional[str] datatype: the datatype of this scalar \
        (GH_INTEGER, ...).
    :param Optional[str] access: the way the kernel accesses this \
        scalar (GH_WRITE, ...).

    '''
    def __init__(self, datatype=None, access=None):
        super().__init__(datatype, access)
        self._form = "GH_SCALAR"

    @staticmethod
    def check_psyir(psyir, nargs=3):
        '''Checks that the psyir argument is valid.

        :param psyir: fparser2 tree containing the PSyIR for a scalar \
            argument.
        :type psyir: :py:class:`fparser.two.Fortran2003.Part_Ref`
        :param Optional[int] nargs: the number of expected arguments.

        :raises TypeError: if the psyir argument is not an fparser2 \
            Part_Ref object.
        :raises ValueError: if the scalar arg kernel metadata is not in \
            the form arg_type(...).
        :raises ValueError: if the scalar arg kernel metadata does not \
            contain nargs arguments.

        '''
        if not isinstance(psyir, Fortran2003.Part_Ref):
            raise TypeError(
                f"Expected scalar arg kernel metadata to be encoded as a "
                f"Fortran Part_Ref object but found type "
                f"'{type(psyir).__name__}' with value '{str(psyir)}'.")
        if not psyir.children[0].tostr().lower() == "arg_type":
            raise ValueError(
                f"Expected scalar arg kernel metadata to have the name "
                f"'arg_type' and be in the form 'arg_type(...)', but found "
                f"'{str(psyir)}'.")
        if not len(psyir.children[1].children) == nargs:
            raise ValueError(
                f"Expected scalar arg kernel metadata to have {nargs} "
                f"arguments, but found {len(psyir.children[1].children)} in "
                f"'{str(psyir)}'.")

    @staticmethod
    def create_from_psyir(psyir):
        '''Create an instance of this class from generic PSyIR. At this moment
        this information is captured in an fparser2 tree.

        :param psyir: fparser2 tree containing the PSyIR for a scalar \
            argument.
        :type psyir: :py:class:`fparser.two.Fortran2003.Part_Ref`

        :returns: an instance of ScalarArg.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.ScalarArg`

        '''
        ScalarArg.check_psyir(psyir)
        datatype = psyir.children[1].children[1].tostr()
        access = psyir.children[1].children[2].tostr()
        return ScalarArg(datatype, access)

    @classmethod
    def create_from_fortran_string(cls, fortran_string):
        '''Create an instance of this class from a Fortran string.

        :param str fortran_string: a string containing the metadata in \
            Fortran.

        :returns: an instance of cls.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.cls`

        '''
        part_ref = cls.create_part_ref(fortran_string)
        return cls.create_from_psyir(part_ref)

    def fortran_string(self):
        '''
        :returns: the metadata represented by this class as a \
            Fortran string.
        :rtype: str

        raises ValueError: if one or more of the datatype, access or \
            function_space values have not been set.

        '''
        if not (self.datatype and self.access):
            raise ValueError(
                f"Values for datatype and access must be "
                f"provided before calling the fortran_string method, but "
                f"found '{self.datatype}' and '{self.access}'.")

        return f"arg_type({self.form}, {self.datatype}, {self.access})"

    @property
    def datatype(self):
        '''
        :returns: the datatype for this scalar \
            argument.
        :rtype: str
        '''
        return self._datatype

    @staticmethod
    def check_datatype(value):
        '''xxx'''
        const = LFRicConstants()
        if not value or value.lower() not in const.VALID_SCALAR_DATA_TYPES:
            raise ValueError(
                f"The second metadata entry for a scalar argument should "
                f"be a recognised datatype descriptor (one of "
                f"{const.VALID_SCALAR_DATA_TYPES}), but found '{value}'.")

    @datatype.setter
    def datatype(self, value):
        '''
        :param str value: set the datatype to the \
            specified value.

        :raises ValueError: if the provided value is not a valid \
            datatype descriptor.

        '''
        self.check_datatype(value)
        self._datatype = value

    @property
    def access(self):
        '''
        :returns: the access descriptor for this scalar \
            argument.
        :rtype: str
        '''
        return self._access

    @staticmethod
    def check_access(value):
        '''
        :raises ValueError: if the provided value is not a valid \
            access type.
        '''

        const = LFRicConstants()
        if not value or value.lower() not in const.VALID_SCALAR_ACCESS_TYPES:
            raise ValueError(
                f"The third metadata entry for a scalar argument should "
                f"be a recognised datatype descriptor (one of "
                f"{const.VALID_SCALAR_ACCESS_TYPES}), but found '{value}'.")

    @access.setter
    def access(self, value):
        '''
        :param str value: set the access descriptor to the \
            specified value.

        '''
        self.check_access(value)
        self._access = value
