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

'''Module containing the MetaRefElementArgMetadata class which
captures the argument values for the LFRic kernel
REFERENCE_ELEMENT metadata.

'''
from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel.common_arg_metadata import CommonArgMetadata


class MetaRefElementArgMetadata(CommonArgMetadata):
    '''Class to capture the LFRic kernel metadata information for a
    meta_reference_element argument. This specifies any properties of
    the reference element that the kernel requires.

    :param str reference_element: the name of the reference_element property.

    '''
    def __init__(self, reference_element):
        super().__init__()
        self.reference_element = reference_element

    @staticmethod
    def create_from_fparser2(fparser2_tree):
        '''Create an instance of this class from an fparser2 tree.

        :param fparser2_tree: fparser2 tree containing the metadata \
            for a meta_reference_element argument.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref`

        :returns: an instance of this class.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.\
            MetaRefElementArgMetadata`

        '''
        MetaRefElementArgMetadata.check_fparser2_arg(
            fparser2_tree, type_name="reference_element_data_type")
        MetaRefElementArgMetadata.check_nargs(fparser2_tree, 1)
        reference_element = MetaRefElementArgMetadata.get_arg(fparser2_tree, 0)
        return MetaRefElementArgMetadata(reference_element)

    def fortran_string(self):
        '''
        :returns: the metadata represented by this class as Fortran.
        :rtype: str
        '''
        return f"reference_element_data_type({self.reference_element})"

    @property
    def reference_element(self):
        '''
        :returns: the reference element property for this \
            meta_reference_element argument.
        :rtype: str

        '''
        return self._reference_element

    @reference_element.setter
    def reference_element(self, value):
        '''
        :param str value: set the reference element property to the \
            specified value.
        '''
        const = LFRicConstants()
        self.validate_scalar_value(
            value, const.VALID_REF_ELEMENT_NAMES, "reference element property")
        self._reference_element = value.lower()


__all__ = ["MetaRefElementArgMetadata"]
