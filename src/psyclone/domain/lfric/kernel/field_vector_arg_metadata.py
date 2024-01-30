# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council
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
from psyclone.domain.lfric.kernel.field_arg_metadata import FieldArgMetadata


class FieldVectorArgMetadata(FieldArgMetadata):
    '''Class to capture LFRic kernel metadata information for a field
    vector argument.

    :param str datatype: the datatype of this field (GH_INTEGER, ...).
    :param str access: the way the kernel accesses this field (GH_WRITE, ...).
    :param str function_space: the function space that this field is on \
        (W0, ...).
    :param str vector_length: the size of the vector.
    :param Optional[str] stencil: the type of stencil used by the \
        kernel when accessing this field.

    '''
    # The relative position of LFRic vector length metadata. Metadata
    # for a field vector argument is provided in the following format
    # 'arg_type(form*vector_length, datatype, access,
    # function_space)'. Therefore, the index of the vector_length
    # argument (vector_length_arg_index) is 0. Index values not
    # provided here are common to the parent classes and are inherited
    # from them.
    vector_length_arg_index = 0
    # The name to use for any exceptions.
    check_name = "field-vector"
    # Whether the class captures vector metadata.
    vector = True

    def __init__(self, datatype, access, function_space, vector_length,
                 stencil=None):
        super().__init__(datatype, access, function_space, stencil=stencil)
        self.vector_length = vector_length

    @classmethod
    def _get_metadata(cls, fparser2_tree):
        '''Extract the required metadata from the fparser2 tree and return it
        as strings. Also check that the metadata is in the expected
        form (but do not check the metadata values as that is done
        separately).

        :param fparser2_tree: fparser2 tree containing the metadata \
            for this argument.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref`

        :returns: a tuple containing the datatype, access, function \
            space, vector-length and stencil metadata.
        :rtype: Tuple[str, str, str, str, Optional[str]]

        '''
        datatype, access, function_space, stencil = super()._get_metadata(
            fparser2_tree)
        vector_length = cls.get_vector_length(fparser2_tree)
        return (datatype, access, function_space, vector_length, stencil)

    def fortran_string(self):
        '''
        :returns: the metadata represented by this class as Fortran.
        :rtype: str
        '''
        if self.stencil:
            return (f"arg_type({self.form}*{self.vector_length}, "
                    f"{self.datatype}, {self.access}, {self.function_space}, "
                    f"stencil({self.stencil}))")
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
        ''':param str value: set the field vector length to the specified \
            value.

        :raises TypeError: if the provided value is not of type str.
        :raises ValueError: if the provided value is not a string \
            containing an integer.
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


__all__ = ["FieldVectorArgMetadata"]
