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

'''Module containing the ScalarArrayArgMetadata class which captures the
metadata associated with a ScalarArray argument. Supports the creation,
modification and Fortran output of a ScalarArray argument.

'''
from psyclone.domain.lfric.kernel.common_arg_metadata import CommonArgMetadata
from psyclone.domain.lfric.kernel.scalar_arg_metadata import ScalarArgMetadata


class ScalarArrayArgMetadata(ScalarArgMetadata):
    '''Class to capture LFRic kernel metadata information for a scalar array
    argument.

    :param str datatype: the datatype of this scalar array (GH_INTEGER, ...).
    :param str access: the way the kernel accesses this scalar array (GH_READ).
    :param str array_ndims: the rank (number of dimensions) of this scalar
                            array.

    '''
    # The name used to specify an array argument in LFRic metadata.
    form = "gh_scalar_array"
    # The relative positions of LFRic metadata. Metadata for an array
    # argument is provided in the following format 'arg_type(form,
    # datatype, access, function_space)'. Therefore, for example, the
    # index of the form argument (form_arg_index) is 0.
    form_arg_index = 0
    datatype_arg_index = 1
    access_arg_index = 2
    array_ndims_arg_index = 3
    # The name to use for any exceptions.
    check_name = "array"
    # The number of arguments in the language-level metadata (min and
    # max values).
    nargs = (4, 4)

    def __init__(self, datatype, access, array_ndims):
        super().__init__(datatype, access)
        self.array_ndims = array_ndims

    @classmethod
    def _get_metadata(cls, fparser2_tree):
        '''Extract the required metadata from the fparser2 tree and return it
        as strings. Also check that the metadata is in the expected
        form (but do not check the metadata values as that is done
        separately).

        :param fparser2_tree: fparser2 tree containing the metadata
            for this argument.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref` |
            :py:class:`fparser.two.Fortran2003.Structure_Constructor`

        :returns: a tuple containing the datatype, access and array ndims
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
        return (f"arg_type({self.form}, {self.datatype}, {self.access}, "
                f"{self.array_ndims})")

    @property
    def array_ndims(self):
        '''
        :returns: the number of dimensions for this scalar array argument.
        :rtype: str
        '''
        return self._array_ndims

    @array_ndims.setter
    def array_ndims(self, value):
        '''
        :param str value: set the number of dimensions to the specified value.

        '''
        self._array_ndims = value

    @classmethod
    def get_array_ndims(cls, fparser2_tree):
        '''Retrieves the array ndims metadata value found within the
        supplied fparser2 tree and checks that it is valid.

        :param fparser2_tree: fparser2 tree capturing the required metadata.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref`

        :returns: the array ndims value extracted from the fparser2 tree.
        :rtype: str

        :raises TypeError: if the array ndims is not a string.
        :raises ValueError: if the array ndims is not an integer.
        :raises ValueError: if the array ndims is less than 1.

        '''
        array_datatype = CommonArgMetadata.get_arg(
            fparser2_tree, cls.array_ndims_arg_index)
        array_ndims = array_datatype.strip()
        try:
            int_value = int(array_ndims)
        except ValueError as info:
            raise ValueError(f"The number of dimensions of a scalar array "
                             f"should be a string containing an integer, "
                             f"but found '{array_ndims}'.") from info

        if int_value < 1:
            raise ValueError(f"The number of dimensions of a scalar array "
                             f"should be an integer greater than or "
                             f"equal to 1 but found {array_ndims}.")
        return array_ndims


__all__ = ["ScalarArrayArgMetadata"]
