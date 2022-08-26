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

'''Module containing the FieldArg class which captures the metadata
associated with a field argument. Supports the creation, modification
and Fortran output of a Field argument.

'''
from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel import CommonArg


class FieldArg(CommonArg):
    '''Class to capture LFRic kernel metadata information for a field
    argument.

    :param Optional[str] datatype: the datatype of this field \
        (GH_INTEGER, ...).
    :param Optional[str] access: the way the kernel accesses this \
        field (GH_WRITE, ...).
    :param Optional[str] function_space: the function space that this \
        field is on (W0, ...).

    '''
    def __init__(self, datatype=None, access=None, function_space=None):
        super().__init__(datatype, access)
        self._form = "GH_FIELD"
        if function_space is None:
            self._function_space = function_space
        else:
            self.function_space = function_space

    @staticmethod
    def create_from_psyir(psyir):
        '''Create an instance of this class from generic PSyIR. At this moment
        this information is captured in an fparser2 tree.

        :param psyir: fparser2 tree containing the PSyIR for a field \
            argument.
        :type psyir: :py:class:`fparser.two.Fortran2003.Part_Ref`

        :returns: an instance of FieldArg.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.FieldArg`

        '''
        FieldArg.check_psyir(psyir, nargs=4)
        datatype = psyir.children[1].children[1].tostr()
        access = psyir.children[1].children[2].tostr()
        function_space = psyir.children[1].children[3].tostr()
        return FieldArg(datatype, access, function_space)

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
        if not (self.datatype and self.access and self.function_space):
            raise ValueError(
                f"Values for datatype, access and function_space must be "
                f"provided before calling the fortran_string method, but "
                f"found '{self.datatype}', '{self.access}' and "
                f"'{self.function_space}'.")

        return (f"arg_type({self.form}, {self.datatype}, {self.access}, "
                f"{self.function_space})")

    @staticmethod
    def check_datatype(value):
        '''
        :param str value: set the datatype to the \
            specified value.

        :raises ValueError: if the provided value is not a valid \
            datatype descriptor.

        '''
        const = LFRicConstants()
        if not value or value.lower() not in const.VALID_FIELD_DATA_TYPES:
            raise ValueError(
                f"The second metadata entry for an argument should "
                f"be a recognised datatype descriptor (one of "
                f"{const.VALID_FIELD_DATA_TYPES}), but found '{value}'.")

    @staticmethod
    def check_access(value):
        '''
        :param str value: set the access descriptor to the \
            specified value.

        :raises ValueError: if the provided value is not a valid \
            access type.

        '''
        const = LFRicConstants()
        if not value or value.lower() not in const.VALID_FIELD_ACCESS_TYPES:
            raise ValueError(
                f"The third metadata entry for an argument should "
                f"be a recognised datatype descriptor (one of "
                f"{const.VALID_FIELD_ACCESS_TYPES}), but found '{value}'.")

    @property
    def function_space(self):
        '''
        :returns: the function space for this field \
            argument.
        :rtype: str
        '''
        return self._function_space

    @function_space.setter
    def function_space(self, value):
        '''
        :param str value: set the access descriptor to the \
            specified value.

        raises ValueError: if the provided value is not a valid \
            function space.

        '''
        const = LFRicConstants()
        if not value or value.lower() not in const.VALID_FUNCTION_SPACE_NAMES:
            raise ValueError(
                f"The fourth metadata entry for an argument should "
                f"be a recognised datatype descriptor (one of "
                f"{const.VALID_FUNCTION_SPACE_NAMES}), but found '{value}'.")
        self._function_space = value
