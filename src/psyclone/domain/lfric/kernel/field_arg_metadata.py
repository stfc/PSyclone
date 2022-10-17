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

'''Module containing the FieldArgMetadata class which captures the metadata
associated with a field argument. Supports the creation, modification
and Fortran output of a Field argument.

'''
from fparser.two import Fortran2003

from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel.common_meta_arg_metadata import \
    CommonMetaArgMetadata


class FieldArgMetadata(CommonMetaArgMetadata):
    '''Class to capture LFRic kernel metadata information for a field
    argument.

    :param Optional[str] datatype: the datatype of this field \
        (GH_INTEGER, ...).
    :param Optional[str] access: the way the kernel accesses this \
        field (GH_WRITE, ...).
    :param Optional[str] function_space: the function space that this \
        field is on (W0, ...).

    '''
    # The name used to specify a field argument in LFRic metadata.
    form = "GH_FIELD"
    # The relative positions of LFRic metadata. Metadata for a field
    # argument is provided in the following format 'arg_type(form,
    # datatype, access, function_space)'. Therefore, for example, the
    # index of the form argument (form_arg_index) is 0.
    form_arg_index = 0
    datatype_arg_index = 1
    access_arg_index = 2
    function_space_arg_index = 3

    def __init__(self, datatype=None, access=None, function_space=None):
        super().__init__(datatype, access)
        if function_space is None:
            self._function_space = function_space
        else:
            self.function_space = function_space

    @staticmethod
    def create_from_fparser2(fparser2_tree):
        '''Create an instance of this class from an fparser2 tree.

        :param fparser2_tree: fparser2 tree capturing the metadata for \
            a field argument.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref`

        :returns: an instance of FieldArgMetadata.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.FieldArgMetadata`

        '''
        FieldArgMetadata.check_fparser2(fparser2_tree, "arg_type")
        FieldArgMetadata.check_nargs(fparser2_tree, nargs=4)
        FieldArgMetadata.check_first_arg(fparser2_tree, "Field")
        datatype, access, function_space = \
            FieldArgMetadata.get_type_access_and_fs(fparser2_tree)
        FieldArgMetadata.check_remaining_args(
            fparser2_tree, datatype, access, function_space)
        return FieldArgMetadata(datatype, access, function_space)

    @classmethod
    def create_from_fortran_string(cls, fortran_string):
        '''Create an instance of this class from Fortran.

        :param str fortran_string: a string containing the metadata in \
            Fortran.

        :returns: an instance of cls.
        :rtype: subclass of :py:class:`psyclone.domain.lfric.kernel.common_arg`

        '''
        fparser2_tree = cls.create_fparser2(
            fortran_string, Fortran2003.Part_Ref)
        return cls.create_from_fparser2(fparser2_tree)

    def fortran_string(self):
        '''
        :returns: the metadata represented by this class as Fortran.
        :rtype: str

        :raises ValueError: if one or more of the datatype, access or \
            function_space values have not been set.

        '''
        if not (self.datatype and self.access and self.function_space):
            raise ValueError(
                f"Values for datatype, access and function_space must be "
                f"provided before calling the fortran_string method, but "
                f"found '{self.datatype}', '{self.access}' and "
                f"'{self.function_space}', respectively.")

        return (f"arg_type({self.form}, {self.datatype}, {self.access}, "
                f"{self.function_space})")

    @staticmethod
    def check_datatype(value):
        '''
        :param str value: the datatype to check for validity.

        :raises ValueError: if the provided value is not a valid \
            datatype descriptor.

        '''
        const = LFRicConstants()
        if not value or value.lower() not in const.VALID_FIELD_DATA_TYPES:
            raise ValueError(
                f"The datatype descriptor metadata for a field should be one "
                f"of {const.VALID_FIELD_DATA_TYPES}, but found '{value}'.")

    @staticmethod
    def check_access(value):
        '''
        :param str value: the access descriptor to validate.

        :raises ValueError: if the provided value is not a valid \
            access type.

        '''
        const = LFRicConstants()
        if not value or value.lower() not in const.VALID_FIELD_ACCESS_TYPES:
            raise ValueError(
                f"The access descriptor metadata for a field should be one of "
                f"{const.VALID_FIELD_ACCESS_TYPES}, but found '{value}'.")

    @property
    def function_space(self):
        '''
        :returns: the function space for this field argument.
        :rtype: str
        '''
        return self._function_space

    @function_space.setter
    def function_space(self, value):
        '''
        :param str value: set the function space to the \
            specified value.

        :raises ValueError: if the provided value is not a valid \
            function space.

        '''
        const = LFRicConstants()
        if not value or value.lower() not in const.VALID_FUNCTION_SPACE_NAMES:
            raise ValueError(
                f"The function space metadata should be one of "
                f"{const.VALID_FUNCTION_SPACE_NAMES}, but found '{value}'.")
        self._function_space = value
