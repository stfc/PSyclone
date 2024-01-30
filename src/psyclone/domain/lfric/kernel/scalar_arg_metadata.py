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

'''Module containing the ScalarArgMetadata class which captures the metadata
associated with a scalar argument. Supports the creation, modification
and Fortran output of a Scalar argument.

'''
from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel.common_meta_arg_metadata import \
    CommonMetaArgMetadata


class ScalarArgMetadata(CommonMetaArgMetadata):
    '''Class to capture LFRic kernel metadata information for a scalar
    argument.

    '''
    # The name used to specify a scalar argument in LFRic metadata.
    form = "gh_scalar"
    # The relative positions of LFRic metadata. Metadata for a scalar
    # argument is provided in the following format 'arg_type(form,
    # datatype, access)'. Therefore, for example, the index of the
    # form argument (form_arg_index) is 0.
    form_arg_index = 0
    datatype_arg_index = 1
    access_arg_index = 2
    # The name to use for any exceptions.
    check_name = "scalar"
    # The number of arguments in the language-level metadata.
    nargs = 3

    @classmethod
    def _get_metadata(cls, fparser2_tree):
        '''Extract the required metadata from the fparser2 tree and return it
        as strings.

        :param fparser2_tree: fparser2 tree containing the metadata \
            for this argument.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref` | \
            :py:class:`fparser.two.Fortran2003.Structure_Constructor`

        :returns: a tuple containing the datatype and access metadata.
        :rtype: Tuple[str, str]

        '''
        return cls._get_datatype_access_metadata(fparser2_tree)

    @classmethod
    def _get_datatype_access_metadata(cls, fparser2_tree):
        '''Extract the datatype and access metadata from the fparser2 tree and
        return them as strings. Also check that the metadata is in the
        expected form (but do not check the metadata values as that is
        done separately).

        :param fparser2_tree: fparser2 tree containing the metadata \
            for this argument.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref` | \
            :py:class:`fparser.two.Fortran2003.Structure_Constructor`

        :returns: a tuple containing the datatype and access metadata.
        :rtype: Tuple[str, str]

        '''
        cls.check_fparser2_arg(fparser2_tree, "arg_type")
        cls.check_nargs(fparser2_tree)
        cls.check_first_arg(fparser2_tree)
        datatype = cls.get_arg(fparser2_tree, cls.datatype_arg_index)
        access = cls.get_arg(fparser2_tree, cls.access_arg_index)
        return (datatype, access)

    def fortran_string(self):
        '''
        :returns: the metadata represented by this class as Fortran.
        :rtype: str
        '''
        return f"arg_type({self.form}, {self.datatype}, {self.access})"

    @staticmethod
    def check_datatype(value):
        '''
        :param str value: the datatype to check for validity.
        '''
        const = LFRicConstants()
        ScalarArgMetadata.validate_scalar_value(
            value, const.VALID_SCALAR_DATA_TYPES, "datatype descriptor")

    @staticmethod
    def check_access(value):
        '''
        :param str value: the access descriptor to validate.
        '''
        const = LFRicConstants()
        ScalarArgMetadata.validate_scalar_value(
            value, const.VALID_SCALAR_ACCESS_TYPES, "access descriptor")


__all__ = ["ScalarArgMetadata"]
