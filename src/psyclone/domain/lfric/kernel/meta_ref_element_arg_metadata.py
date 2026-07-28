# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

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
