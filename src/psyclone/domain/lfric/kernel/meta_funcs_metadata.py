# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing the MetaFuncsMetadata class which captures
the values for the LFRic kernel meta_funcs metadata.

'''
from psyclone.domain.lfric.kernel.common_declaration_metadata import \
    CommonDeclarationMetadata
from psyclone.domain.lfric.kernel.meta_funcs_arg_metadata import \
    MetaFuncsArgMetadata


class MetaFuncsMetadata(CommonDeclarationMetadata):
    '''Class to capture the values of the LFRic kernel
    meta_funcs metadata. This class supports the creation,
    modification and Fortran output of this metadata.

    meta_funcs metadata specifies whether any quadrature or evaluator
    data is required for a given function space.

    :param meta_funcs_args: a list of meta_funcs arguments.
    :type meta_funcs_args: List[:py:class:`psyclone.domain.lfric.kernel.\
        MetaFuncsArgMetadata`]

    '''
    def __init__(self, meta_funcs_args):
        super().__init__()
        self.meta_funcs_args = meta_funcs_args

    def fortran_string(self):
        '''
        :returns: the meta_funcs metadata as Fortran.
        :rtype: str
        '''
        return self.type_declaration_string(
            "FUNC_TYPE", "META_FUNCS", self._meta_funcs_args)

    @staticmethod
    def create_from_fparser2(fparser2_tree):
        '''Create an instance of MetaFuncsMetadata from an fparser2
        tree.

        :param fparser2_tree: fparser2 tree capturing the meta \
            funcs metadata.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.\
            Data_Component_Def_Stmt`

        :returns: an instance of MetaFuncsMetadata.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.\
            MetaFuncsMetadata`

        '''
        values_list = MetaFuncsMetadata.get_derived_array_declaration(
            fparser2_tree, "FUNC_TYPE", "META_FUNCS")
        meta_obj_list = []
        for value in values_list:
            meta_obj_list.append(
                MetaFuncsArgMetadata.create_from_fortran_string(value))
        return MetaFuncsMetadata(meta_obj_list)

    @property
    def meta_funcs_args(self):
        '''
        :returns: a list of meta funcs argument objects.
        :rtype: List[:py:class:`psyclone.domain.lfric.kernel.\
            MetaFuncsArgMetadata`]
        '''
        return self._meta_funcs_args[:]

    @meta_funcs_args.setter
    def meta_funcs_args(self, values):
        '''
        :param values: set the meta_funcs metadata to the \
            supplied list of values.
        :type values: List[:py:class:`psyclone.domain.lfric.kernel.\
            MetaFuncsArgMetadata`]

        '''
        self.validate_list(values, MetaFuncsArgMetadata)
        # Take a copy of the list so that it can't be modified
        # externally.
        self._meta_funcs_args = values[:]


__all__ = ["MetaFuncsMetadata"]
