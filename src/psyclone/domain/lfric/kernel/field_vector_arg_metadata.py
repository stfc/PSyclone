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
from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel.field_arg_metadata import FieldArgMetadata


class FieldVectorArgMetadata(FieldArgMetadata):
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
    # The relative position of LFRic vector length metadata. Metadata
    # for a field vector argument is provided in the following format
    # 'arg_type(form*vector_length, datatype, access,
    # function_space)'. Therefore, the index of the vector_length
    # argument (vector_length_arg_index) is 0. Index values not
    # provided here are common to the parent classes and are inherited
    # from them.
    vector_length_arg_index = 0

    def __init__(self, datatype=None, access=None, function_space=None,
                 vector_length=None):
        super().__init__(
            datatype=datatype, access=access, function_space=function_space)

        self._vector_length = None
        if vector_length is not None:
            self.vector_length = vector_length

    @staticmethod
    def create_from_fparser2(fparser2_tree):
        '''Create an instance of this class from an fparser2 tree.

        :param fparser2_tree: fparser2 tree capturing the metadata \
            for a field vector argument.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref`

        :returns: an instance of this class.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.FieldVectorArgMetadata`

        '''
        FieldVectorArgMetadata.check_fparser2(fparser2_tree, "arg_type")
        FieldVectorArgMetadata.check_nargs(fparser2_tree, 4)
        FieldVectorArgMetadata.check_first_arg(
            fparser2_tree, "FieldVector", vector=True)
        vector_length = FieldVectorArgMetadata.get_and_check_vector_length(
            fparser2_tree)
        datatype, access, function_space = \
            FieldVectorArgMetadata.get_type_access_and_fs(fparser2_tree)
        FieldVectorArgMetadata.check_remaining_args(
            fparser2_tree, datatype, access, function_space, vector_length)
        return FieldVectorArgMetadata(
            datatype, access, function_space, vector_length)

    def fortran_string(self):
        '''
        :returns: the metadata represented by this class as Fortran.
        :rtype: str

        :raises ValueError: if all of the properties have not been \
            set.

        '''
        if not (self.datatype and self.access and self.function_space and
                self.vector_length):
            raise ValueError(
                f"Values for datatype, access, function_space and "
                f"vector_length must be provided before calling the "
                f"fortran_string method, but found '{self.datatype}', "
                f"'{self.access}', '{self.function_space}' and "
                f"'{self.vector_length}', respectively.")

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
        try:
            int_value = int(value)
        except ValueError as info:
            raise ValueError(
                f"The vector size should be a string containing an integer, "
                f"but found '{value}'.") from info

        if int_value <= 1:
            raise ValueError(f"The vector size should be an integer greater "
                             f"than 1 but found {value}.")
        self._vector_length = value
