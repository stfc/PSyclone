# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing the MetaArgsMetadata class which captures
the values for the LFRic kernel meta_args metadata.

'''
from fparser.two import Fortran2003
from fparser.two.utils import walk

from psyclone.domain.lfric.kernel.columnwise_operator_arg_metadata import \
    ColumnwiseOperatorArgMetadata
from psyclone.domain.lfric.kernel.common_meta_arg_metadata import \
    CommonMetaArgMetadata
from psyclone.domain.lfric.kernel.common_declaration_metadata import \
    CommonDeclarationMetadata
from psyclone.domain.lfric.kernel.field_arg_metadata import FieldArgMetadata
from psyclone.domain.lfric.kernel.field_vector_arg_metadata import \
    FieldVectorArgMetadata
from psyclone.domain.lfric.kernel.inter_grid_arg_metadata import \
    InterGridArgMetadata
from psyclone.domain.lfric.kernel.inter_grid_vector_arg_metadata import \
    InterGridVectorArgMetadata
from psyclone.domain.lfric.kernel.operator_arg_metadata import \
    OperatorArgMetadata
from psyclone.domain.lfric.kernel.scalar_arg_metadata import ScalarArgMetadata
from psyclone.parse.utils import ParseError


class MetaArgsMetadata(CommonDeclarationMetadata):
    '''Class to capture the values of the LFRic kernel
    meta_args metadata. This class supports the creation,
    modification and Fortran output of this metadata.

    meta_args metadata specifies information about data that the
    kernel code expects to be passed to it via its argument list.

    :param meta_args_args: a list of meta_args arguments.
    :type meta_args_args: List[:py:class:`psyclone.domain.lfric.kernel.\
        CommonMetaArgMetadata`]

    '''
    def __init__(self, meta_args_args):
        super().__init__()
        self.meta_args_args = meta_args_args

    def fortran_string(self):
        '''
        :returns: the meta_args metadata as Fortran.
        :rtype: str
        '''
        return self.type_declaration_string(
            "ARG_TYPE", "META_ARGS", self._meta_args_args)

    @staticmethod
    def create_from_fparser2(fparser2_tree):
        '''Create an instance of MetaArgsMetadata from an fparser2
        tree.

        :param fparser2_tree: fparser2 tree capturing the meta \
            args metadata.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.\
            Data_Component_Def_Stmt`

        :returns: an instance of MetaArgsMetadata.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.\
            MetaArgsMetadata`

        :raises ParseError: if an unknown MetaArgsArgMetadata argument \
            is found.

        '''
        MetaArgsMetadata.get_derived_array_declaration(
            fparser2_tree, "ARG_TYPE", "META_ARGS")
        args = walk(fparser2_tree, Fortran2003.Ac_Value_List)
        meta_args_args = []
        for meta_arg in args[0].children:
            form = meta_arg.children[1].children[0].tostr()
            form = form.lower()
            if form == "gh_scalar":
                arg = ScalarArgMetadata.create_from_fparser2(meta_arg)
            elif form == "gh_operator":
                arg = OperatorArgMetadata.create_from_fparser2(meta_arg)
            elif form == "gh_columnwise_operator":
                arg = ColumnwiseOperatorArgMetadata.create_from_fparser2(
                    meta_arg)
            elif "gh_field" in form:
                vector_arg = "*" in form
                nargs = len(meta_arg.children[1].children)
                intergrid_arg = False
                if nargs == 5:
                    fifth_arg = meta_arg.children[1].children[4]
                    intergrid_arg = (
                        fifth_arg.children and
                        fifth_arg.children[0].string.lower() == "mesh_arg")

                if intergrid_arg and vector_arg:
                    arg = InterGridVectorArgMetadata.create_from_fparser2(
                        meta_arg)
                elif intergrid_arg and not vector_arg:
                    arg = InterGridArgMetadata.create_from_fparser2(meta_arg)
                elif vector_arg and not intergrid_arg:
                    arg = FieldVectorArgMetadata.create_from_fparser2(meta_arg)
                else:
                    arg = FieldArgMetadata.create_from_fparser2(meta_arg)
            else:
                raise ParseError(
                    f"Expected a 'meta_arg' entry to be a "
                    f"field, a scalar or an operator, but found "
                    f"'{meta_arg}'.")
            meta_args_args.append(arg)
        return MetaArgsMetadata(meta_args_args)

    @property
    def meta_args_args(self):
        '''
        :returns: a list of meta args argument objects.
        :rtype: List[:py:class:`psyclone.domain.lfric.kernel.\
            CommonArg`]
        '''
        return self._meta_args_args[:]

    @meta_args_args.setter
    def meta_args_args(self, values):
        '''
        :param values: set the meta_args metadata to the \
            supplied list of values.
        :type values: List[:py:class:`psyclone.domain.lfric.kernel.\
            CommonArg`]

        '''
        self.validate_list(values, CommonMetaArgMetadata)
        # Take a copy of the list so that it can't be modified
        # externally.
        self._meta_args_args = values[:]


__all__ = ["MetaArgsMetadata"]
