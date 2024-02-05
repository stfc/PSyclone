# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council
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
# Author L. Turner, Met Office

'''Module containing the ArrayArgMetadata class which captures the metadata
associated with an array argument. Supports the creation, modification
and Fortran output of an Array argument.

'''
from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel.scalar_arg_metadata import \
    ScalarArgMetadata


class ArrayArgMetadata(ScalarArgMetadata):
    '''Class to capture LFRic kernel metadata information for an array
    argument.

    :param str datatype: the datatype of this array (GH_INTEGER, ...).
    :param str access: the way the kernel accesses this array (GH_READ).
    :param str function_space: the function space that this array is \
        on (W0, ...).

    '''
    # The name used to specify an array argument in LFRic metadata.
    form = "gh_array"
    # The relative positions of LFRic metadata. Metadata for an array
    # argument is provided in the following format 'arg_type(form,
    # datatype, access, function_space)'. Therefore, for example, the
    # index of the form argument (form_arg_index) is 0.
    form_arg_index = 0
    datatype_arg_index = 1
    access_arg_index = 2
    array_size_arg_index = 3
    # The name to use for any exceptions.
    check_name = "array"
    # The number of arguments in the language-level metadata (min and
    # max values).
    nargs = (4)

    def __init__(self, datatype, access, array_ndims):                       # the information that is given
        super().__init__(datatype, access)                                   # SHARKS (needs test coverage)
    #    self.array_size = array_size                                        # SHARKS (needs test coverage)
        self.array_ndims = array_ndims

    @classmethod
    def _get_metadata(cls, fparser2_tree):
        '''Extract the required metadata from the fparser2 tree and return it
        as strings. Also check that the metadata is in the expected
        form (but do not check the metadata values as that is done
        separately).

        :param fparser2_tree: fparser2 tree containing the metadata \
            for this argument.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref` | \
            :py:class:`fparser.two.Fortran2003.Structure_Constructor`

        :returns: a tuple containing the datatype, access and array nranks \
            metadata.
        :rtype: Tuple[str, str, str]

        '''
        datatype, access = super()._get_metadata(fparser2_tree)
        array_ndims = cls.get_array_ndims(fparser2_tree)
        return (datatype, access, array_ndims)

    def fortran_string(self):
        '''
        :returns: the metadata represented by this class as Fortran.
        :rtype: str
        '''
        return (f"arg_type({self.form}, {self.datatype}, {self.access}, "    # SHARKS (needs test coverage)
                f"{self.array_ndims})")                                      # how to check for full NRANKS*n here?

    @staticmethod
    def check_datatype(value):
        '''
        :param str value: the datatype to check for validity.

        :raises ValueError: if the provided value is not a valid \
            datatype descriptor.

        '''
        const = LFRicConstants()                                             # SHARKS (needs test coverage)
        ArrayArgMetadata.validate_scalar_value(                              # SHARKS (needs test coverage)
            value, const.VALID_ARRAY_DATA_TYPES, "datatype descriptor")

    @staticmethod
    def check_access(value):
        '''
        :param str value: the access descriptor to validate.
        '''
        const = LFRicConstants()                                             # SHARKS (needs test coverage)
        ArrayArgMetadata.validate_scalar_value(                              # SHARKS (needs test coverage)
            value, const.VALID_ARRAY_ACCESS_TYPES, "access descriptor")

    @property
    def array_size(self):
        '''
        :returns: the array size for this array argument.
        :rtype: str
        '''
        return self._array_size                                              # SHARKS (needs test coverage)

    @array_size.setter
    def array_size(self, value):
        '''
        :param str value: set the function space to the \
            specified value.
        '''
        # const = LFRicConstants()
        # FieldArgMetadata.validate_scalar_value(
        #     value, const.VALID_FUNCTION_SPACE_NAMES, "function space")
        # self._array_ndims = cls.get_array_ndims(fparser2_tree)
        self._array_size = value.lower()

    @property
    def array_ndims(self):
        '''
        :returns: the array size for this array argument.
        :rtype: str
        '''
        return self._array_ndims                                             # SHARKS (needs test coverage)

    @array_ndims.setter
    def array_ndims(self, value):
        '''
        :param str value: set the function space to the \
            specified value.
        '''
        if not isinstance(value, str):
            raise TypeError(f"The 'array_size' value should be of type str, "
                            f"but found '{type(value).__name__}'.")
        try:
            int_value = int(value)
        except ValueError as info:
            raise ValueError(
                f"The array size should be a string containing an integer, "
                f"but found '{value}'.") from info

        if int_value < 1:
            raise ValueError(f"The array size should be an integer greater "
                             f"than or equal to 1 but found {value}.")
#        self._array_size = value.lower()
        self._array_ndims = value


__all__ = ["ArrayArgMetadata"]
