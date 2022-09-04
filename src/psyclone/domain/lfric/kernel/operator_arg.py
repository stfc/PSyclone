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

'''Module containing the OperatorArg class which captures the metadata
associated with an operator argument. Supports the creation, modification
and Fortran output of a Operator argument.

'''
from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel.field_arg import FieldArg


class OperatorArg(FieldArg):
    '''Class to capture LFRic kernel metadata information for an operator
    argument.

    :param Optional[str] datatype: the datatype of this field \
        (GH_INTEGER, ...).
    :param Optional[str] access: the way the kernel accesses this \
        field (GH_WRITE, ...).
    :param Optional[str] function_space1: the function space that this \
        field maps from (W0, ...).
    :param Optional[str] function_space2: the function space that this \
        field maps to (W0, ...).

    '''
    def __init__(self, datatype=None, access=None, function_space1=None,
                 function_space2=None):
        super().__init__(datatype, access)
        self._form = "GH_OPERATOR"
        if function_space1 is None:
            self._function_space1 = function_space1
        else:
            self.function_space1 = function_space1
        if function_space2 is None:
            self._function_space2 = function_space2
        else:
            self.function_space2 = function_space2

    @staticmethod
    def create_from_psyir(psyir):
        '''Create an instance of this class from generic PSyIR. At this moment
        this information is captured in an fparser2 tree.

        :param psyir: fparser2 tree containing the PSyIR for an operator \
            argument.
        :type psyir: :py:class:`fparser.two.Fortran2003.Part_Ref`

        :returns: an instance of OperatorArg.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.OperatorArg`

        '''
        OperatorArg.check_psyir(psyir, nargs=5)
        datatype = psyir.children[1].children[1].tostr()
        access = psyir.children[1].children[2].tostr()
        function_space1 = psyir.children[1].children[3].tostr()
        function_space2 = psyir.children[1].children[4].tostr()
        return OperatorArg(datatype, access, function_space1, function_space2)

    @classmethod
    def create_from_fortran_string(cls, fortran_string):
        '''Create an instance of this class from a Fortran string.

        :param str fortran_string: a string containing the metadata in \
            Fortran.

        :returns: an instance of cls.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.cls`

        '''
        part_ref = cls.create_psyir(fortran_string)
        return cls.create_from_psyir(part_ref)

    def fortran_string(self):
        ''':returns: the metadata represented by this class as a \
            Fortran string.
        :rtype: str

        raises ValueError: if one or more of the datatype, access or \
            function_space1 and function_space2 values have not been \
            set.

        '''
        if not (self.datatype and self.access and self.function_space1 and
                self.function_space2):
            raise ValueError(
                f"Values for datatype, access, function_space1 and "
                f"function_space2 must be provided before calling the "
                f"fortran_string method, but found '{self.datatype}', "
                f"'{self.access}', '{self.function_space1}' and "
                f"'{self.function_space2}'.")

        return (f"arg_type({self.form}, {self.datatype}, {self.access}, "
                f"{self.function_space1}, {self.function_space2})")

    @staticmethod
    def check_access(value):
        '''
        :raises ValueError: if the provided value is not a valid \
            access type.
        '''

        const = LFRicConstants()
        if not value or value.lower() not in const.VALID_OPERATOR_ACCESS_TYPES:
            raise ValueError(
                f"The third metadata entry for an argument should "
                f"be a recognised datatype descriptor (one of "
                f"{const.VALID_OPERATOR_ACCESS_TYPES}), but found '{value}'.")

    @staticmethod
    def check_datatype(value):
        '''
        :param str value: set the datatype to the \
            specified value.
    
        :raises ValueError: if the provided value is not a valid \
            datatype descriptor.
    
        '''
        const = LFRicConstants()
        if not value or value.lower() not in const.VALID_OPERATOR_DATA_TYPES:
            raise ValueError(
                f"The second metadata entry for an argument should "
                f"be a recognised datatype descriptor (one of "
                f"{const.VALID_OPERATOR_DATA_TYPES}), but found '{value}'.")

    @property
    def function_space1(self):
        '''
        :returns: the first function space for this operator \
            argument.
        :rtype: str
        '''
        return self._function_space1

    @function_space1.setter
    def function_space1(self, value):
        '''
        :param str value: set the access descriptor to the \
            specified value.

        raises ValueError: if the provided value is not a valid \
            function space.

        '''
        const = LFRicConstants()
        if not value or value.lower() not in const.VALID_FUNCTION_SPACES:
            raise ValueError(
                f"The fourth metadata entry for an argument should "
                f"be a recognised function space (one of "
                f"{const.VALID_FUNCTION_SPACES}), but found '{value}'.")
        self._function_space1 = value

    @property
    def function_space2(self):
        '''
        :returns: the second function space for this operator \
            argument.
        :rtype: str
        '''
        return self._function_space2

    @function_space2.setter
    def function_space2(self, value):
        '''
        :param str value: set the access descriptor to the \
            specified value.

        raises ValueError: if the provided value is not a valid \
            function space.

        '''
        const = LFRicConstants()
        if not value or value.lower() not in const.VALID_FUNCTION_SPACES:
            raise ValueError(
                f"The fifth metadata entry for an argument should "
                f"be a recognised function space (one of "
                f"{const.VALID_FUNCTION_SPACES}), but found '{value}'.")
        self._function_space2 = value
