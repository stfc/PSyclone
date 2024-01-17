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

'''Module containing the MetaMeshMetadata class which captures
the values for the LFRic kernel meta_mesh metadata.

'''
from psyclone.domain.lfric.kernel.common_declaration_metadata import \
    CommonDeclarationMetadata
from psyclone.domain.lfric.kernel.meta_mesh_arg_metadata import \
    MetaMeshArgMetadata


class MetaMeshMetadata(CommonDeclarationMetadata):
    '''Class to capture the values of the LFRic kernel
    meta_mesh metadata. This class supports the creation,
    modification and Fortran output of this metadata.

    meta_mesh metadata specifies properties of the LFRic mesh object
    that are required by a kernel.

    :param meta_mesh_args: a list of meta_mesh arguments.
    :type meta_mesh_args: List[:py:class:`psyclone.domain.lfric.kernel.\
        MetaMeshArgMetadata`]

    '''
    def __init__(self, meta_mesh_args):
        super().__init__()
        self.meta_mesh_args = meta_mesh_args

    def fortran_string(self):
        '''
        :returns: the meta_mesh metadata as Fortran.
        :rtype: str
        '''
        return self.type_declaration_string(
            "MESH_DATA_TYPE", "META_MESH", self._meta_mesh_args)

    @staticmethod
    def create_from_fparser2(fparser2_tree):
        '''Create an instance of MetaMeshMetadata from an fparser2
        tree.

        LFRic meta mesh metadata is in array form. Two
        versions of the array form are supported:

        type(mesh_data_type) :: meta_mesh(1) = (/ ... /)
        type(mesh_data_type), dimension(1) :: meta_mesh = (/ ... /)

        :param fparser2_tree: fparser2 tree capturing the meta \
            mesh metadata.

        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.\
            Data_Component_Def_Stmt`

        :returns: an instance of MetaMeshMetadata.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.\
            MetaMeshMetadata`

        '''
        values_list = MetaMeshMetadata.get_derived_array_declaration(
            fparser2_tree, "MESH_DATA_TYPE", "META_MESH")
        meta_obj_list = []
        for value in values_list:
            meta_obj_list.append(
                MetaMeshArgMetadata.create_from_fortran_string(value))
        return MetaMeshMetadata(meta_obj_list)

    @property
    def meta_mesh_args(self):
        '''
        :returns: a list of meta mesh argument objects.
        :rtype: List[:py:class:`psyclone.domain.lfric.kernel.\
            MetaMeshArgMetadata`]
        '''
        return self._meta_mesh_args[:]

    @meta_mesh_args.setter
    def meta_mesh_args(self, values):
        '''
        :param values: set the meta_mesh metadata to the \
            supplied list of values.
        :type values: List[:py:class:`psyclone.domain.lfric.kernel.\
            MetaMeshArgMetadata`]

        '''
        self.validate_list(values, MetaMeshArgMetadata)
        # Take a copy of the list so that it can't be modified
        # externally.
        self._meta_mesh_args = values[:]


__all__ = ["MetaMeshMetadata"]
