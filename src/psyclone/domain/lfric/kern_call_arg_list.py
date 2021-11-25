# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2021, Science and Technology Facilities Council.
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

'''This module implements a class that manages the argument for a kernel
call. It especially adds all implicitly required parameters.
'''

from __future__ import print_function, absolute_import

from collections import namedtuple

from psyclone import psyGen
from psyclone.core import AccessType, Signature
from psyclone.domain.lfric import (ArgOrdering, LFRicConstants)
from psyclone.errors import GenerationError, InternalError


class KernCallArgList(ArgOrdering):
    # pylint: disable=too-many-public-methods
    # TODO: #845 Check that all implicit variables have the right type.
    '''Creates the argument list required to call kernel "kern" from the
    PSy-layer and captures the positions of the following arguments in
    the argument list: nlayers, number of quadrature points and number
    of degrees of freedom. The ordering and type of arguments is
    captured by the base class.

    :param kern: The kernel that is being called.
    :type kern: :py:class:`psyclone.dynamo0p3.DynKern`

    '''
    NdfInfo = namedtuple("NdfInfo", ["position", "function_space"])

    def __init__(self, kern):
        super(KernCallArgList, self).__init__(kern)
        self._nlayers_positions = []
        self._nqp_positions = []
        self._ndf_positions = []
        # Keep a reference to the Invoke SymbolTable as a shortcut
        self._symtab = self._kern.ancestor(psyGen.InvokeSchedule).symbol_table

    def cell_position(self, var_accesses=None):
        '''Adds a cell argument to the argument list and if supplied stores
        this access in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        '''
        self.append(self._cell_ref_name(var_accesses))

    def cell_map(self, var_accesses=None):
        '''Add cell-map and related cell counts (for inter-grid kernels)
        to the argument list. If supplied it also stores these accesses to the
        var_access object.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        cargs = psyGen.args_filter(self._kern.args, arg_meshes=["gh_coarse"])
        carg = cargs[0]
        fargs = psyGen.args_filter(self._kern.args, arg_meshes=["gh_fine"])
        farg = fargs[0]
        base_name = "cell_map_" + carg.name
        map_name = self._symtab.find_or_create_tag(base_name).name
        # Add the cell map to our argument list
        self.append("{0}(:,:,{1})".format(map_name,
                                          self._cell_ref_name(var_accesses)),
                    var_accesses=var_accesses)
        # No. of fine cells per coarse cell in x
        base_name = "ncpc_{0}_{1}_x".format(farg.name, carg.name)
        ncellpercellx = self._symtab.find_or_create_tag(base_name).name
        self.append(ncellpercellx, var_accesses)
        # No. of fine cells per coarse cell in y
        base_name = "ncpc_{0}_{1}_y".format(farg.name, carg.name)
        ncellpercelly = self._symtab.find_or_create_tag(base_name).name
        self.append(ncellpercelly, var_accesses)
        # No. of columns in the fine mesh
        base_name = "ncell_{0}".format(farg.name)
        ncell_fine = self._symtab.find_or_create_tag(base_name).name
        self.append(ncell_fine, var_accesses)

    def mesh_height(self, var_accesses=None):
        '''Add mesh height (nlayers) to the argument list and if supplied
        stores this access in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        nlayers_name = self._symtab.find_or_create_tag("nlayers").name
        self.append(nlayers_name, var_accesses)
        self._nlayers_positions.append(self.num_args)

    # TODO uncomment this method when ensuring we only pass ncell3d once
    # to any given kernel.
    # def mesh_ncell3d(self):
    #     ''' Add the number of cells in the full 3D mesh to the argument
    #     list '''
    #     ncell3d_name = self._name_space_manager.create_name(
    #         root_name="ncell_3d", context="PSyVars", label="ncell3d")
    #     self.append(ncell3d_name)

    def _mesh_ncell2d(self, var_accesses=None):
        '''Add the number of columns in the mesh to the argument list and if
        supplied stores this access in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        name = self._symtab.find_or_create_tag("ncell_2d").name
        self.append(name, var_accesses)

    def _mesh_ncell2d_no_halos(self, var_accesses=None):
        '''Add the number of columns in the mesh (excluding those in the halo)
        to the argument list and store this access in var_accesses (if
        supplied).

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        name = self._symtab.find_or_create_tag("ncell_2d_no_halos").name
        self.append(name, var_accesses)

    def cma_operator(self, arg, var_accesses=None):
        '''Add the CMA operator and associated scalars to the argument
        list and optionally add them to the variable access
        information.

        :param arg: the CMA operator argument.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        components = ["matrix"]
        # Avoid circular import:
        # pylint: disable=import-outside-toplevel
        from psyclone.dynamo0p3 import DynCMAOperators
        if arg.function_space_to.orig_name != (arg.function_space_from.
                                               orig_name):
            components += DynCMAOperators.cma_diff_fs_params
        else:
            components += DynCMAOperators.cma_same_fs_params
        for component in components:
            name = self._symtab.find_or_create_tag(
                arg.name + "_" + component).name
            # Matrix is an output parameter, the rest are input
            if component == "matrix":
                mode = AccessType.WRITE
            else:
                mode = AccessType.READ
            self.append(name, var_accesses, mode=mode)

    def field_vector(self, argvect, var_accesses=None):
        '''Add the field vector associated with the argument 'argvect' to the
        argument list. If supplied it also stores these accesses to the
        var_access object.

        :param argvect: the field vector to add.
        :type argvect: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        # the range function below returns values from
        # 1 to the vector size which is what we
        # require in our Fortran code
        for idx in range(1, argvect.vector_size + 1):
            text = argvect.proxy_name + "(" + str(idx) + ")%data"
            self.append(text)
        if var_accesses is not None:
            # We add the whole field-vector, not the individual accesses.
            var_accesses.add_access(Signature(argvect.name), argvect.access,
                                    self._kern)

    def field(self, arg, var_accesses=None):
        '''Add the field array associated with the argument 'arg' to the
        argument list. If supplied it also stores this access in var_accesses.

        :param arg: the field to be added.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        text = arg.proxy_name + "%data"
        # Add the field object arg%name and not just the proxy part
        # as being read.
        self.append(text, var_accesses, var_access_name=arg.name,
                    mode=arg.access)

    def stencil_unknown_extent(self, arg, var_accesses=None):
        '''Add stencil information to the argument list associated with the
        argument 'arg' if the extent is unknown. If supplied it also stores
        this access in var_accesses.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        # The extent is not specified in the metadata so pass the value in
        # Import here to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.dynamo0p3 import DynStencils
        var_name = DynStencils.dofmap_size_name(self._symtab, arg)
        name = "{0}({1})".format(var_name, self._cell_ref_name(var_accesses))
        self.append(name, var_accesses, var_access_name=var_name)

    def stencil_2d_unknown_extent(self, arg, var_accesses=None):
        '''Add 2D stencil information to the argument list associated with the
        argument 'arg' if the extent is unknown. If supplied it also stores
        this access in var_accesses.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        # The extent is not specified in the metadata so pass the value in
        # Import here to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.dynamo0p3 import DynStencils
        var_name = DynStencils.dofmap_size_name(self._symtab, arg)
        name = "{0}(:,{1})".format(var_name,
                                   self._cell_ref_name(var_accesses))
        self.append(name, var_accesses, var_access_name=var_name)

    def stencil_2d_max_extent(self, arg, var_accesses=None):
        '''Add the maximum branch extent for a 2D stencil associated with the
        argument 'arg' to the argument list. If supplied it also stores this
        in var_accesses.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`pclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional SingleVariableAccessInfo instance \
            to store the information about variable accesses.
        :type var_accesses: \
            :py:class:1psyclone.core.access_info.SingleVariableAccessInfo`

        '''
        # The maximum branch extent is not specified in the metadata so pass
        # the value in.
        # Import here to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.dynamo0p3 import DynStencils
        name = DynStencils.max_branch_length_name(self._symtab, arg)
        self.append(name, var_accesses)

    def stencil_unknown_direction(self, arg, var_accesses=None):
        '''Add stencil information to the argument list associated with the
        argument 'arg' if the direction is unknown (i.e. it's being supplied
        in a variable). If supplied it also stores this access in
        var_accesses.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        # the direction of the stencil is not known so pass the value in
        name = arg.stencil.direction_arg.varname
        self.append(name, var_accesses)

    def stencil(self, arg, var_accesses=None):
        '''Add general stencil information associated with the argument 'arg'
        to the argument list. If supplied it also stores this access in
        var_accesses.

        :param arg: the meta-data description of the kernel \
            argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        # add in stencil dofmap
        # Import here to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.dynamo0p3 import DynStencils
        var_name = DynStencils.dofmap_name(self._symtab, arg)
        name = "{0}(:,:,{1})".format(var_name,
                                     self._cell_ref_name(var_accesses))
        self.append(name, var_accesses, var_access_name=var_name)

    def stencil_2d(self, arg, var_accesses=None):
        '''Add general 2D stencil information associated with the argument
        'arg' to the argument list. If supplied it also stores this access in
        var_accesses.

        :param arg: the meta-data description of the kernel \
            argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        # The stencil_2D differs from the stencil in that the direction
        # of the branch is baked into the stencil_dofmap array.
        # The array dimensions are thus (dof_in_cell, cell_in_branch,
        # branch_in_stencil) where the branch_in_stencil is always ordered
        # West, South, East, North which is standard in LFRic. This allows
        # for knowledge of what direction a stencil cell is in relation
        # to the center even when the stencil is truncated at boundaries.
        # Import here to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.dynamo0p3 import DynStencils
        var_name = DynStencils.dofmap_name(self._symtab, arg)
        name = "{0}(:,:,:,{1})".format(var_name,
                                       self._cell_ref_name(var_accesses))
        self.append(name, var_accesses, var_access_name=var_name)

    def operator(self, arg, var_accesses=None):
        '''Add the operator arguments to the argument list. If supplied it
        also stores this access in var_accesses.

        :param arg: the meta-data description of the operator.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        # TODO we should only be including ncell_3d once in the argument
        # list but this adds it for every operator
        self.append(arg.proxy_name_indexed + "%ncell_3d", var_accesses,
                    mode=AccessType.READ)
        self.append(arg.proxy_name_indexed + "%local_stencil", var_accesses,
                    mode=AccessType.WRITE)

    def fs_common(self, function_space, var_accesses=None):
        '''Add function-space related arguments common to LMA operators and
        fields. If supplied it also stores this access in var_accesses.

        :param function_space: the function space for which the related \
            arguments common to LMA operators and fields are added.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        super(KernCallArgList, self).fs_common(function_space, var_accesses)
        self._ndf_positions.append(
            KernCallArgList.NdfInfo(position=self.num_args,
                                    function_space=function_space.orig_name))

    def fs_compulsory_field(self, function_space, var_accesses=None):
        '''Add compulsory arguments associated with this function space to
        the list. If supplied it also stores this access in var_accesses.

        :param function_space: the function space for which the compulsory \
            arguments are added.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        undf_name = function_space.undf_name
        self.append(undf_name, var_accesses)
        map_name = function_space.map_name
        if self._kern.iterates_over == 'domain':
            # This kernel takes responsibility for iterating over cells so
            # pass the whole dofmap.
            self.append("{0}".format(map_name),
                        var_accesses, var_access_name=map_name)
        else:
            # Pass the dofmap for the cell column
            self.append("{0}(:,{1})".format(map_name,
                                            self._cell_ref_name(var_accesses)),
                        var_accesses, var_access_name=map_name)

    def fs_intergrid(self, function_space, var_accesses=None):
        '''Add function-space related arguments for an intergrid kernel.
        If supplied it also stores this access in var_accesses.

        :param function_space: the function space for which to add arguments
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        # Is this FS associated with the coarse or fine mesh? (All fields
        # on a given mesh must be on the same FS.)
        arg = self._kern.arguments.get_arg_on_space(function_space)
        if arg.mesh == "gh_fine":
            # For the fine mesh, we need ndf, undf and the *whole*
            # dofmap
            self.fs_common(function_space, var_accesses=var_accesses)
            undf_name = function_space.undf_name
            self.append(undf_name, var_accesses)
            map_name = function_space.map_name
            self.append(map_name, var_accesses)
        else:
            # For the coarse mesh we only need undf and the dofmap for
            # the current column
            self.fs_compulsory_field(function_space,
                                     var_accesses=var_accesses)

    def basis(self, function_space, var_accesses=None):
        '''Add basis function information for this function space to the
        argument list and optionally to the variable access information.

        :param function_space: the function space for which the basis \
                               function is required.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        for rule in self._kern.qr_rules.values():
            basis_name = function_space.get_basis_name(qr_var=rule.psy_name)
            self.append(basis_name, var_accesses)

        if "gh_evaluator" in self._kern.eval_shapes:
            # We are dealing with an evaluator and therefore need as many
            # basis functions as there are target function spaces.
            for fs_name in self._kern.eval_targets:
                # The associated FunctionSpace object is the first item in
                # the tuple dict entry associated with the name of the target
                # function space
                fspace = self._kern.eval_targets[fs_name][0]
                basis_name = function_space.get_basis_name(on_space=fspace)
                self.append(basis_name, var_accesses)

    def diff_basis(self, function_space, var_accesses=None):
        '''Add differential basis information for the function space to the
        argument list. If supplied it also stores this access in
        var_accesses.

        :param function_space: the function space for which the differential \
            basis functions are required.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        for rule in self._kern.qr_rules.values():
            diff_basis_name = function_space.get_diff_basis_name(
                qr_var=rule.psy_name)
            self.append(diff_basis_name, var_accesses)

        if "gh_evaluator" in self._kern.eval_shapes:
            # We are dealing with an evaluator and therefore need as many
            # basis functions as there are target function spaces.
            for fs_name in self._kern.eval_targets:
                # The associated FunctionSpace object is the first item in
                # the tuple dict entry associated with the name of the target
                # function space
                fspace = self._kern.eval_targets[fs_name][0]
                diff_basis_name = function_space.get_diff_basis_name(
                    on_space=fspace)
                self.append(diff_basis_name, var_accesses)

    def field_bcs_kernel(self, function_space, var_accesses=None):
        '''Implement the boundary_dofs array fix for a field. If supplied it
        also stores this access in var_accesses.

        :param function_space: the function space for which boundary dofs \
            are required.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises GenerationError: if the bcs kernel does not contain \
            a field as argument (but e.g. an operator).

        '''
        fspace = None
        for fspace in self._kern.arguments.unique_fss:
            if fspace.orig_name == "any_space_1":
                break
        farg = self._kern.arguments.get_arg_on_space(fspace)
        # Sanity check - expect the enforce_bc_code kernel to only have
        # a field argument.
        const = LFRicConstants()
        if not farg.is_field:
            raise GenerationError(
                "Expected an argument of {0} type from which to look-up "
                "boundary dofs for kernel {1} but got '{2}'".
                format(const.VALID_FIELD_NAMES,
                       self._kern.name, farg.argument_type))

        base_name = "boundary_dofs_" + farg.name
        name = self._symtab.find_or_create_tag(base_name).name
        self.append(name, var_accesses)

    def operator_bcs_kernel(self, function_space, var_accesses=None):
        '''Supply necessary additional arguments for the kernel that
        applies boundary conditions to a LMA operator. If supplied it
        also stores this access in var_accesses.

        :param function_space: unused, only for consistency with base class.
        :type function_space: :py:class:`psyclone.dynamo3.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        # This kernel has only a single LMA operator as argument.
        # Checks for this are performed in ArgOrdering.generate()
        op_arg = self._kern.arguments.args[0]
        base_name = "boundary_dofs_" + op_arg.name
        name = self._symtab.find_or_create_tag(base_name).name
        self.append(name, var_accesses)

    def mesh_properties(self, var_accesses=None):
        '''Provide the kernel arguments required for the mesh properties
        specified in the kernel metadata. If supplied it also stores this
        access in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        if self._kern.mesh.properties:
            # Avoid circular import:
            # pylint: disable=import-outside-toplevel
            from psyclone.dynamo0p3 import LFRicMeshProperties
            self.extend(LFRicMeshProperties(self._kern).
                        kern_args(stub=False, var_accesses=var_accesses))

    def quad_rule(self, var_accesses=None):
        '''Add quadrature-related information to the kernel argument list.
        Adds the necessary arguments to the argument list, and optionally
        adds variable access information to the var_accesses object.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        # The QR shapes that this routine supports
        supported_qr_shapes = ["gh_quadrature_xyoz", "gh_quadrature_edge",
                               "gh_quadrature_face"]

        for shape, rule in self._kern.qr_rules.items():
            if shape == "gh_quadrature_xyoz":
                # XYoZ quadrature requires the number of quadrature points in
                # the horizontal and in the vertical.
                self._nqp_positions.append(
                    {"horizontal": self.num_args + 1,
                     "vertical": self.num_args + 2})
                self.extend(rule.kernel_args, var_accesses)

            elif shape == "gh_quadrature_edge":
                # TODO #705 support transformations supplying the number of
                # quadrature points for edge quadrature.
                self.extend(rule.kernel_args, var_accesses)
            elif shape == "gh_quadrature_face":
                # TODO #705 support transformations supplying the number of
                # quadrature points for face quadrature.
                self.extend(rule.kernel_args, var_accesses)
            else:
                raise NotImplementedError(
                    "quad_rule: no support implemented for quadrature with a "
                    "shape of '{0}'. Supported shapes are: {1}.".format(
                        shape, supported_qr_shapes))

    @property
    def nlayers_positions(self):
        ''':returns: the position(s) in the argument list of the \
            variable(s) that passes the number of layers. The generate \
            method must be called first.
        :rtype: list of int.

        :raises InternalError: if the generate() method has not been called.

        '''
        if not self._generate_called:
            raise InternalError(
                "KernCallArgList: the generate() method should be called "
                "before the nlayers_positions() method")
        return self._nlayers_positions

    @property
    def nqp_positions(self):
        ''':return: the positions in the argument list of the variables that \
            pass the number of quadrature points. The number and type of \
            these will change depending on the type of quadrature. A list \
            of dictionaries is returned with the quadrature types \
            being the keys to the dictionaries and their position in the \
            argument list being the values. At the moment only XYoZ is \
            supported (which has horizontal and vertical quadrature \
            points). The generate method must be called first.
        :rtype: [{str: int, ...}]

        :raises InternalError: if the generate() method has not been \
        called.

        '''
        if not self._generate_called:
            raise InternalError(
                "KernCallArgList: the generate() method should be called "
                "before the nqp_positions() method")
        return self._nqp_positions

    @property
    def ndf_positions(self):
        ''':return: the position(s) in the argument list and the function \
            space(s) associated with the variable(s) that pass(es) the \
            number of degrees of freedom for the function space. The \
            generate method must be called first.
        :rtype: list of namedtuple (position=int, function_space=str).

        :raises InternalError: if the generate() method has not been \
            called.

        '''
        if not self._generate_called:
            raise InternalError(
                "KernCallArgList: the generate() method should be called "
                "before the ndf_positions() method")
        return self._ndf_positions

    def _cell_ref_name(self, var_accesses=None):
        '''Utility routine which determines whether to return the cell value
        or the colourmap lookup value. If supplied it also stores this access
        in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :returns: the Fortran code needed to access the current cell index.
        :rtype: str

        '''
        if self._kern.is_coloured():
            if var_accesses is not None:
                var_accesses.add_access(Signature("colour"), AccessType.READ,
                                        self._kern)
                var_accesses.add_access(Signature("cell"), AccessType.READ,
                                        self._kern)
                var_accesses.add_access(Signature(self._kern.colourmap),
                                        AccessType.READ,
                                        self._kern, ["colour", "cell"])
            return self._kern.colourmap + "(colour, cell)"

        if var_accesses is not None:
            var_accesses.add_access(Signature("cell"), AccessType.READ,
                                    self._kern)

        return "cell"


# ============================================================================
# For automatic documentation creation:
__all__ = ["KernCallArgList"]
