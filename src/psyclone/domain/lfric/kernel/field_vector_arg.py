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

'''Module containing the FieldVector Arg class which captures the metadata
associated with a field vector argument. Supports the creation, modification
and Fortran output of a Field Vector argument.

'''
from psyclone.domain.lfric.kernel.field_arg import FieldArg


class FieldVectorArg(FieldArg):
    '''Class to capture LFRic kernel metadata information for a field
    vector argument.

    :param Optional[str] datatype: the datatype of this field \
        (GH_INTEGER, ...).
    :param Optional[str] access: the way the kernel accesses this \
        field (GH_WRITE, ...).
    :param Optional[str] function_space: the function space that this \
        field is on (W0, ...).
    :param Optional[str] vector_length: the size of the vector.

    '''
    def __init__(self, datatype=None, access=None, function_space=None,
                 vector_length=None):
        super().__init__(
            datatype=datatype, access=access, function_space=function_space)

        if vector_length is None:
            self._vector_length = vector_length
        else:
            self.vector_length = vector_length

    @staticmethod
    def create_from_fortran_string(fortran_string):
        '''Create an instance of this class from a Fortran string.

        :param str fortran_string: a string containing the metadata in \
            Fortran.

        :returns: an instance of FieldVectorArg.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.FieldVectorArg`

        '''
        part_ref = FieldArg.create_part_ref(fortran_string)
        return FieldVectorArg.create_from_psyir(part_ref)

    @staticmethod
    def create_from_psyir(psyir):
        '''Create an instance of this class from generic PSyIR. At this moment
        this information is captured in an fparser2 tree.

        :param psyir: fparser2 tree containing the PSyIR for a field \
            vector argument.
        :type psyir: :py:class:`fparser.two.Fortran2003.Part_Ref`

        :returns: an instance of this class.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.FieldVectorArg`

        :raises TypeError: if the first metadata argument is not in \
            the form datatype*vector_length.

        '''
        FieldArg.check_psyir(psyir)
        vector_datatype = psyir.children[1].children[0].tostr()
        components = vector_datatype.split("*")
        if not len(components) == 2:
            raise TypeError(
                f"Expecting the first argument to be in the form "
                f"'form*vector_length' but found '{vector_datatype}'.")
        vector_length = components[1].strip()
        datatype = psyir.children[1].children[1].tostr()
        access = psyir.children[1].children[2].tostr()
        function_space = psyir.children[1].children[3].tostr()
        return FieldVectorArg(datatype, access, function_space, vector_length)

    def fortran_string(self):
        '''
        :returns: the metadata represented by this class as a \
            Fortran string.
        :rtype: str

        '''
        if not (self.datatype and self.access and self.function_space and
                self.vector_length):
            raise ValueError(
                f"Values for datatype, access, function_space and "
                f"vector_length must be provided before calling the "
                f"fortran_string method, but found '{self.datatype}', "
                f"'{self.access}', '{self.function_space}' and "
                f"'{self.vector_length}'.")

        return (f"arg_type({self.form}*{self.vector_length}, {self.datatype}, "
                f"{self.access}, {self.function_space})")

    @property
    def vector_length(self):
        '''
        :returns: the vector length of this field vector \
            argument.
        :rtype: str
        '''
        return self._vector_length

    @vector_length.setter
    def vector_length(self, value):
        '''
        :param str value: set the field vector length to the specified \
            value.

        :raises TypeError: if the provided value is not of type str.
        :raises ValueError: if the provided value is not greater than 1.

        '''
        if not isinstance(value, str):
            raise TypeError(f"The vector size should be a string but found "
                            f"{type(value).__name__}.")
        int_value = int(value)
        if int_value <= 1:
            raise ValueError(f"The vector size should be an integer greater "
                             f"than 1 but found {value}.")
        self._vector_length = value
