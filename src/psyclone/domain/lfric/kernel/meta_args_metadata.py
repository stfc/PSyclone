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

'''Module containing the MetaArgsMetadata class which captures
the values for the LFRic kernel meta_args metadata.

'''
from fparser.two import Fortran2003
from fparser.two.utils import walk

from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel.columnwise_operator_arg import \
    ColumnwiseOperatorArg
from psyclone.domain.lfric.kernel.common_arg import CommonArg
from psyclone.domain.lfric.kernel.common_declaration_metadata import \
    CommonDeclarationMetadata
from psyclone.domain.lfric.kernel.field_arg import FieldArg
from psyclone.domain.lfric.kernel.field_vector_arg import FieldVectorArg
from psyclone.domain.lfric.kernel.inter_grid_vector_arg import \
    InterGridVectorArg
from psyclone.domain.lfric.kernel.operator_arg import OperatorArg
from psyclone.domain.lfric.kernel.scalar_arg import ScalarArg
from psyclone.domain.lfric.kernel.inter_grid_arg import InterGridArg
from psyclone.domain.lfric.kernel.scalar_arg import ScalarArg
from psyclone.parse.utils import ParseError


class MetaArgsMetadata(CommonDeclarationMetadata):
    '''Class to capture the values of the LFRic kernel
    meta_args metadata. This class supports the creation,
    modification and Fortran output of this metadata.

    meta_args metadata specifies whether any quadrature or evaluator
    data is required for a given function space.

    :param meta_args_args: a list of meta_args arguments.
    :type meta_args_args: List[:py:class:`psyclone.domain.lfric.kernel.\
        CommonArgs`]

    '''
    def __init__(self, meta_args_args):
        self.meta_args_args = meta_args_args

    def fortran_string(self):
        '''
         :returns: the meta_args metadata as Fortran.
         :rtype: str
        '''
        return MetaArgsMetadata.type_declaration_string(
            "ARG_TYPE", "META_ARGS", self._meta_args_args)

    @staticmethod
    def create_from_fparser2(fparser2_tree):
        '''Create an instance of MetaArgsMetadata from an fparser2
        tree.

        LFRic meta args metadata is in array form. Two
        versions of the array form are supported:

        type(func_type) :: meta_args(1) = (/ ... /)
        type(func_type), dimension(1) :: meta_args = (/ ... /)

        :param fparser2_tree: fparser2 tree capturing the meta \
            args metadata.

        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.\
            Data_Component_Def_Stmt`

        :returns: an instance of MetaArgsMetadata.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.\
            MetaArgsMetadata`

        '''
        # TODO We want to process args with fparser2 rather than
        # string as things like nargs are potentially difficult with a
        # string. It is OK for other classes? as they have single
        # value args? Or change those too?
        values_list = MetaArgsMetadata.validate_derived_array_declaration(
            fparser2_tree, "ARG_TYPE", "META_ARGS")

        args = walk(fparser2_tree, Fortran2003.Ac_Value_List)
        meta_args_args = []
        for meta_arg in args[0].children:
            form = meta_arg.children[1].children[0].tostr()
            form = form.lower()
            if form == "gh_scalar":
                arg = ScalarArg.create_from_fparser2(meta_arg)
            elif form == "gh_operator":
                arg = OperatorArg.create_from_fparser2(meta_arg)
            elif form == "gh_columnwise_operator":
                arg = ColumnwiseOperatorArg.create_from_fparser2(meta_arg)
            elif "gh_field" in form:
                vector_arg = "gh_field" in form and "*" in form
                nargs = len(meta_arg.children[1].children)
                intergrid_arg = False
                if nargs == 5:
                    fifth_arg = meta_arg.children[1].children[4]
                    intergrid_arg = fifth_arg.children[0].string == "mesh_arg"

                if intergrid_arg and vector_arg:
                    arg = InterGridVectorArg.create_from_fparser2(meta_arg)
                elif intergrid_arg and not vector_arg:
                    arg = InterGridArg.create_from_fparser2(meta_arg)
                elif vector_arg and not intergrid_arg:
                    arg = FieldVectorArg.create_from_fparser2(meta_arg)
                else:
                    arg = FieldArg.create_from_fparser2(meta_arg)
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

        raises TypeError: if the supplied value is not a list.
        raises TypeError: if the supplied value is an empty list.
        raises TypeError: if any entry in the list is not of the \
            required type.

        '''
        if not isinstance(values, list):
            raise TypeError(f"meta_args values should be provided as "
                            f"a list but found '{type(values).__name__}'.")
        if not values:
            raise TypeError(
                "The meta_args list should contain at least one "
                "entry, but it is empty.")
        const = LFRicConstants()
        for value in values:
            if not isinstance(value, CommonArg):
                raise TypeError(
                    f"The meta_args list should be a list containing objects "
                    f"of type CommonArg but found "
                    f"'{type(value).__name__}'.")
        # Take a copy of the list so that it can't be modified
        # externally.
        self._meta_args_args = values[:]
