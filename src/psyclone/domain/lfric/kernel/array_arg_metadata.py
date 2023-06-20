# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council
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
    :param str access: the way the kernel accesses this array (GH_WRITE, ...).
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
    array_nranks_arg_index = 3
    # The name to use for any exceptions.
    check_name = "array"
    # The number of arguments in the language-level metadata (min and
    # max values).
    nargs = (4)

    def __init__(self, datatype, access, array_nranks):
        super().__init__(datatype, access)
        self.array_nranks = array_nranks

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
        array_nranks = cls.get_vector_length(fparser2_tree)
        return (datatype, access, array_nranks)

    def fortran_string(self):
        '''
        :returns: the metadata represented by this class as Fortran.
        :rtype: str
        '''
        return (f"arg_type({self.form}, {self.datatype}, {self.access}, "
                f"{self.array_nranks})")

    @staticmethod
    def check_datatype(value):
        '''
        :param str value: the datatype to check for validity.

        :raises ValueError: if the provided value is not a valid \
            datatype descriptor.

        '''
        const = LFRicConstants()
        ArrayArgMetadata.validate_scalar_value(
            value, const.VALID_ARRAY_DATA_TYPES, "datatype descriptor")

    @staticmethod
    def check_access(value):
        '''
        :param str value: the access descriptor to validate.
        '''
        const = LFRicConstants()
        ArrayArgMetadata.validate_scalar_value(
            value, const.VALID_ARRAY_ACCESS_TYPES, "access descriptor")

    @property
    def array_nranks(self):
        '''
        :returns: the function space for this array argument.
        :rtype: str
        '''
        return self._array_nranks

#    @array_nranks.setter
#    def array_nranks(self, value):
#        '''
#        :param str value: set the function space to the \
#            specified value.
#        '''
#        const = LFRicConstants()
#        ArrayArgMetadata.validate_scalar_value(
#            value, const.VALID_FUNCTION_SPACE_NAMES, "array nranks")
#        self._array_nranks = value.lower()


__all__ = ["ArrayArgMetadata"]
