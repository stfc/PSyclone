# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing the MetaRefElementMetadata class which captures
the values for the LFRic kernel meta_ref_element metadata.

'''
from psyclone.domain.lfric.kernel.common_declaration_metadata import \
    CommonDeclarationMetadata
from psyclone.domain.lfric.kernel.meta_ref_element_arg_metadata import \
    MetaRefElementArgMetadata


class MetaRefElementMetadata(CommonDeclarationMetadata):
    '''Class to capture the values of the LFRic kernel
    meta_ref_element metadata. This class supports the creation,
    modification and Fortran output of this metadata.

    meta_ref_element metadata specifies properties of the reference
    element.

    :param meta_ref_element_args: a list of meta_ref_element arguments.
    :type meta_ref_element_args: List[:py:class:`psyclone.domain.lfric.kernel.\
        MetaRefElementArgMetadata`]

    '''
    def __init__(self, meta_ref_element_args):
        super().__init__()
        self.meta_ref_element_args = meta_ref_element_args

    def fortran_string(self):
        '''
        :returns: the meta_ref_element metadata as Fortran.
        :rtype: str
        '''
        return self.type_declaration_string(
            "REFERENCE_ELEMENT_DATA_TYPE", "META_REFERENCE_ELEMENT",
            self._meta_ref_element_args)

    @staticmethod
    def create_from_fparser2(fparser2_tree):
        '''Create an instance of MetaRefElementMetadata from an fparser2
        tree.

        :param fparser2_tree: fparser2 tree capturing the meta \
            reference element metadata.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.\
            Data_Component_Def_Stmt`

        :returns: an instance of MetaRefElementMetadata.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.\
            MetaRefElementMetadata`

        '''
        values_list = MetaRefElementMetadata.\
            get_derived_array_declaration(
                fparser2_tree, "REFERENCE_ELEMENT_DATA_TYPE",
                "META_REFERENCE_ELEMENT")
        meta_obj_list = []
        for value in values_list:
            meta_obj_list.append(
                MetaRefElementArgMetadata.create_from_fortran_string(value))
        return MetaRefElementMetadata(meta_obj_list)

    @property
    def meta_ref_element_args(self):
        '''
        :returns: a list of meta reference element argument objects.
        :rtype: List[:py:class:`psyclone.domain.lfric.kernel.\
            MetaRefElementArgMetadata`]
        '''
        return self._meta_ref_element_args[:]

    @meta_ref_element_args.setter
    def meta_ref_element_args(self, values):
        '''
        :param values: set the meta_ref_element metadata to the \
            supplied list of values.
        :type values: List[:py:class:`psyclone.domain.lfric.kernel.\
            MetaRefElementArgMetadata`]

        '''
        self.validate_list(values, MetaRefElementArgMetadata)
        # Take a copy of the list so that it can't be modified
        # externally.
        self._meta_ref_element_args = values[:]


__all__ = ["MetaRefElementMetadata"]
