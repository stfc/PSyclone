# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2023, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic and A. Coughtrie, Met Office
# Modified J. Henrichs, Bureau of Meteorology

'''This module implements a class that manages all of the data references
that must be copied over to a GPU before executing the kernel. Ordering
of the parameters does not matter apart from where we have members of
derived types. In that case, the derived type itself must be specified
first before any members.
'''

from psyclone.domain.lfric import KernCallArgList
from psyclone import psyGen


class KernCallAccArgList(KernCallArgList):
    # TODO: #845 Check that all implicit variables have the right type.
    '''Kernel call arguments that need to be declared by OpenACC
    directives. KernCallArgList only needs to be specialised
    where modified, or additional, arguments are required.
    Scalars are apparently not required but it is valid in
    OpenACC to include them and requires less specialisation
    to keep them in.

    '''
    def cell_map(self, var_accesses=None):
        '''Add cell-map to the list of required arrays.

        :param var_accesses: unused.
        :type var_accesses: :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        cargs = psyGen.args_filter(self._kern.args, arg_meshes=["gh_coarse"])
        carg = cargs[0]
        base_name = "cell_map_" + carg.name
        self.append(base_name)
        # Add the cell map to our argument list
        _, cell_ref = self.cell_ref_name(var_accesses)
        self.append(cell_ref.symbol.name)

    def cell_position(self, var_accesses=None):
        '''Adds a cell argument to the argument list and if supplied stores
        this access in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        cell_ref_name, ref = self.cell_ref_name(var_accesses)
        self.append(ref.symbol.name)

    def field_vector(self, argvect, var_accesses=None):
        '''Add the field vector associated with the argument 'argvect' to the
        argument list. OpenACC requires the field and the
        dereferenced data to be specified. If supplied it also stores
        this access in var_accesses.

        :param argvect: the field vector to add.
        :type argvect: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        # First provide the derived-type object
        for idx in range(1, argvect.vector_size+1):
            self.append(argvect.proxy_name + "(" + str(idx) + ")")
        # Then provide the actual fields that are in the derived-type object
        super().field_vector(argvect, var_accesses)

    def field(self, arg, var_accesses=None):
        '''Add the field array associated with the argument 'arg' to the
        argument list. OpenACC requires the field and the
        dereferenced data to be specified. If supplied it also
        stores this access in var_accesses.

        :param arg: the field to be added.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        text1 = arg.proxy_name
        self.append(text1)
        # This will add the field%data argument, and add the field
        # to the variable access list.
        super().field(arg, var_accesses)

    def stencil(self, arg, var_accesses=None):
        '''Add general stencil information associated with the argument 'arg'
        to the argument list. OpenACC requires the full dofmap to be
        specified. If supplied it also stores this access in var_accesses.

        :param arg: the meta-data description of the kernel \
            argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        # Import here to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.dynamo0p3 import DynStencils
        var_name = DynStencils.dofmap_symbol(self._kern.root.symbol_table,
                                             arg).name
        self.append(var_name, var_accesses)

    def stencil_2d(self, arg, var_accesses=None):
        '''Add general 2D stencil information associated with the argument
        'arg' to the argument list. OpenACC requires the full dofmap to be
        specified. If supplied it also stores this access in var_accesses.This
        method passes through to the stencil method.

        :param arg: the meta-data description of the kernel \
            argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        self.stencil(arg, var_accesses)

    def stencil_unknown_extent(self, arg, var_accesses=None):
        '''Add stencil information to the argument list associated with the
        argument 'arg' if the extent is unknown. If supplied it also stores
        this access in var_accesses.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        # The extent is not specified in the metadata so pass the value in
        # Import here to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.dynamo0p3 import DynStencils
        name = DynStencils.dofmap_size_symbol(self._kern.root.symbol_table,
                                              arg).name
        self.append(name, var_accesses)

    def stencil_2d_unknown_extent(self, arg, var_accesses=None):
        '''Add 2D stencil information to the argument list associated with the
        argument 'arg' if the extent is unknown. If supplied it also stores
        this access in var_accesses. This method passes through to the
        stencil_unknown_extent method.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        self.stencil_unknown_extent(arg, var_accesses)

    def operator(self, arg, var_accesses=None):
        '''Add the operator arguments if they have not already been
        added. OpenACC requires the derived type and the dereferenced
        data to be specified. If supplied it also stores this access in
        var_accesses.

        :param arg: the meta-data description of the operator.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        # In case of OpenACC we do not want to transfer the same
        # data to GPU twice.
        if arg.proxy_name_indexed not in self.arglist:
            self.append(arg.proxy_name_indexed, var_accesses)
            # This adds ncell_3d and local_stencil after the derived type:
            super().operator(arg, var_accesses)

    def fs_compulsory_field(self, function_space, var_accesses=None):
        '''Add compulsory arguments associated with this function space to
        the list. OpenACC requires the full function-space map
        to be specified. If supplied it also stores this access in
        var_accesses.

        :param function_space: the function space for which the compulsory \
            arguments are added.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        if self._kern.iterates_over != "cell_column":
            return
        self.append(function_space.undf_name, var_accesses)
        # The base class only adds one dimension to the list, while OpenACC
        # needs the whole field, so we cannot call the base class.
        self.append(function_space.map_name, var_accesses)

    def fs_intergrid(self, function_space, var_accesses=None):
        '''
        '''
        # Is this FS associated with the coarse or fine mesh? (All fields
        # on a given mesh must be on the same FS.)
        arg = self._kern.arguments.get_arg_on_space(function_space)
        if arg.mesh == "gh_fine":
            # For the fine mesh, we need the *whole* dofmap
            map_name = function_space.map_name
            self.append(map_name, var_accesses)
        else:
            # For the coarse mesh we only need undf and the dofmap for
            # the current column
            self.fs_compulsory_field(function_space,
                                     var_accesses=var_accesses)

    def scalar(self, scalar_arg, var_accesses=None):
        '''
        Override the default implementation as there's no need to specify
        scalars for an OpenACC data region.

        :param scalar_arg: the kernel argument.
        :type scalar_arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance that \
            stores information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''


# ============================================================================
# For automatic documentation creation:
__all__ = ["KernCallAccArgList"]
