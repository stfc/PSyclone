# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2020, Science and Technology Facilities Council.
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
# Modified I. Kavcic, Met Office
# Modified J. Henrichs, Bureau of Meteorology

'''This module implements a class that manages the argument for a kernel
call. It especially adds all implicitly required parameters.
'''

from __future__ import print_function, absolute_import

from collections import namedtuple

from psyclone import psyGen
from psyclone.core.access_type import AccessType
from psyclone.domain.lfric import ArgOrdering
from psyclone.errors import GenerationError, InternalError


class KernCallArgList(ArgOrdering):
    # pylint: disable=too-many-public-methods
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
        ArgOrdering.__init__(self, kern)
        self._nlayers_positions = []
        self._nqp_positions = []
        self._ndf_positions = []

    def cell_position(self, var_accesses=None):
        '''Adds a cell argument to the argument list and if supplied stores
        this access in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        '''
        self._arglist.append(self._cell_ref_name(var_accesses))

    def cell_map(self, var_accesses=None):
        '''Add cell-map and related cell counts to the argument list.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        '''
        symtab = self._kern.root.symbol_table
        cargs = psyGen.args_filter(self._kern.args, arg_meshes=["gh_coarse"])
        carg = cargs[0]
        fargs = psyGen.args_filter(self._kern.args, arg_meshes=["gh_fine"])
        farg = fargs[0]
        base_name = "cell_map_" + carg.name
        map_name = symtab.name_from_tag(base_name)
        # Add the cell map to our argument list
        self._arglist.append("{0}(:,{1})".
                             format(map_name,
                                    self._cell_ref_name(var_accesses)))
        if var_accesses:
            var_accesses.add_access(map_name, AccessType.READ, self._kern)
        # No. of fine cells per coarse cell
        base_name = "ncpc_{0}_{1}".format(farg.name, carg.name)
        ncellpercell = symtab.name_from_tag(base_name)
        self._arglist.append(ncellpercell)
        if var_accesses:
            var_accesses.add_access(ncellpercell, AccessType.READ, self._kern)
        # No. of columns in the fine mesh
        base_name = "ncell_{0}".format(farg.name)
        ncell_fine = symtab.name_from_tag(base_name)
        self._arglist.append(ncell_fine)
        if var_accesses:
            var_accesses.add_access(ncell_fine, AccessType.READ, self._kern)

    def mesh_height(self, var_accesses=None):
        '''Add mesh height (nlayers) to the argument list and if supplied
        stores this access in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        '''
        nlayers_name = \
            self._kern.root.symbol_table.name_from_tag("nlayers")
        self._arglist.append(nlayers_name)
        if var_accesses:
            var_accesses.add_access(nlayers_name, AccessType.READ, self._kern)
        self._nlayers_positions.append(len(self._arglist))

    # TODO uncomment this method when ensuring we only pass ncell3d once
    # to any given kernel.
    # def mesh_ncell3d(self):
    #     ''' Add the number of cells in the full 3D mesh to the argument
    #     list '''
    #     ncell3d_name = self._name_space_manager.create_name(
    #         root_name="ncell_3d", context="PSyVars", label="ncell3d")
    #     self._arglist.append(ncell3d_name)

    def mesh_ncell2d(self, var_accesses=None):
        '''Add the number of columns in the mesh to the argument list and if
        supplied stores this access in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        ncell2d_name = \
            self._kern.root.symbol_table.name_from_tag("ncell_2d")
        self._arglist.append(ncell2d_name)
        if var_accesses:
            var_accesses.add_access(ncell2d_name, AccessType.READ, self._kern)

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
        for idx in range(1, argvect.vector_size+1):
            text = argvect.proxy_name + "(" + str(idx) + ")%data"
            self._arglist.append(text)
        if var_accesses:
            # We add the whole field-vector, not the individual accesses.
            # Add an arbitrary index to mark this field as array.
            var_accesses.add_access(argvect.proxy_name, argvect.access,
                                    self._kern, [1])

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
        self._arglist.append(text)
        if var_accesses:
            # It's an array, so add an arbitrary index value for the
            # stored indices (which is at this stage the only way to
            # indicate an array access).
            var_accesses.add_access(arg.name, arg.access, self._kern, [1])

    def stencil_unknown_extent(self, arg, var_accesses=None):
        '''
        Add stencil information to the argument list associated with the
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
        from psyclone.dynamo0p3 import DynStencils
        name = DynStencils.dofmap_size_name(self._kern.root.symbol_table, arg)
        self._arglist.append(name)
        if var_accesses:
            var_accesses.add_access(name, AccessType.READ, self._kern)

    def stencil_unknown_direction(self, arg, var_accesses=None):
        '''
        Add stencil information to the argument list associated with the
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
        self._arglist.append(name)
        if var_accesses:
            var_accesses.add_access(name, AccessType.READ, self._kern)

    def stencil(self, arg, var_accesses=None):
        '''
        Add general stencil information associated with the argument 'arg'
        to the argument list.If supplied it also stores this access in
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
        from psyclone.dynamo0p3 import DynStencils
        var_name = DynStencils.dofmap_name(self._kern.root.symbol_table, arg)
        name = var_name + "(:,:," + self._cell_ref_name(var_accesses) + ")"
        self._arglist.append(name)
        if var_accesses:
            var_accesses.add_access(var_name, AccessType.READ, self._kern, [1])

    def operator(self, arg, var_accesses=None):
        '''Add the operator arguments to the argument list. If supplied it
        also stores this access in var_accesses.

        :param arg: the operator to be added.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        # TODO we should only be including ncell_3d once in the argument
        # list but this adds it for every operator
        self._arglist.append(arg.proxy_name_indexed+"%ncell_3d")
        self._arglist.append(arg.proxy_name_indexed+"%local_stencil")
        if var_accesses:
            var_accesses.add_access(arg.proxy_name_indexed+"%ncell_3d",
                                    AccessType.READ, self._kern)
            var_accesses.add_access(arg.proxy_name_indexed+"%local_stencil",
                                    AccessType.READ, self._kern, [1])

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
        from psyclone.dynamo0p3 import DynCMAOperators
        if arg.function_space_to.orig_name != \
           arg.function_space_from.orig_name:
            components += DynCMAOperators.cma_diff_fs_params
        else:
            components += DynCMAOperators.cma_same_fs_params
        for component in components:
            name = self._kern.root.symbol_table.\
                name_from_tag(arg.name + "_" + component)
            self._arglist.append(name)
            if var_accesses:
                # All cma parameters are scalar
                var_accesses.add_access(name, AccessType.READ, self._kern)

    def scalar(self, scalar_arg, var_accesses=None):
        '''Add the name associated with the scalar argument to the argument
        list and optionally add this scalar to the variable access
        information.

        :param var_accesses: optional VariablesAccessInfo instance that \
            stores information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        '''

        self._arglist.append(scalar_arg.name)
        if var_accesses:
            var_accesses.add_access(scalar_arg.name, AccessType.READ,
                                    self._kern)

    def fs_common(self, function_space, var_accesses=None):
        '''Add function-space related arguments common to LMA operators and
        fields. If supplied it also stores this access in var_accesses.

        :param function_space: the function space for which the related \
            arguments common to LMA operators and fields are added.
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        # There is currently one argument: "ndf"
        ndf_name = function_space.ndf_name
        self._arglist.append(ndf_name)
        if var_accesses:
            var_accesses.add_access(ndf_name, AccessType.READ, self._kern)

        self._ndf_positions.append(
            KernCallArgList.NdfInfo(position=len(self._arglist),
                                    function_space=function_space.orig_name))

    def fs_compulsory_field(self, function_space, var_accesses=None):
        '''Add compulsory arguments to the argument list, when there is a
        field on this function space. If supplied it also stores this access in
        var_accesses.

        :param function_space: the function space for which the compulsory \
            arguments are added.
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        undf_name = function_space.undf_name
        self._arglist.append(undf_name)
        map_name = function_space.map_name
        self._arglist.append(map_name + "(:," +
                             self._cell_ref_name(var_accesses) + ")")
        if var_accesses:
            var_accesses.add_access(undf_name, AccessType.READ, self._kern)
            # We add the whole map variable,
            # not just the dimension that is used in the call
            var_accesses.add_access(map_name, AccessType.READ, self._kern)

    def fs_intergrid(self, function_space, var_accesses=None):
        '''
        Add function-space related arguments for an intergrid kernel

        :param function_space: the function space for which to add arguments
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
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
            self._arglist.append(undf_name)
            map_name = function_space.map_name
            self._arglist.append(map_name)
            if var_accesses:
                var_accesses.add_access(undf_name, AccessType.READ,
                                        self._kern)
                var_accesses.add_access(map_name, AccessType.READ,
                                        self._kern, [1])
        else:
            # For the coarse mesh we only need undf and the dofmap for
            # the current column
            self.fs_compulsory_field(function_space,
                                     var_accesses=var_accesses)

    def basis(self, function_space, var_accesses=None):
        '''
        Add basis function information for this function space to the
        argument list and optionally to the variable access information.

        :param function_space: the function space for which the basis \
                               function is required.
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        for rule in self._kern.qr_rules.values():
            basis_name = function_space.get_basis_name(qr_var=rule.psy_name)
            self._arglist.append(basis_name)
            if var_accesses:
                var_accesses.add_access(basis_name, AccessType.READ,
                                        self._kern, [1])

        if "gh_evaluator" in self._kern.eval_shapes:
            # We are dealing with an evaluator and therefore need as many
            # basis functions as there are target function spaces.
            for fs_name in self._kern.eval_targets:
                # The associated FunctionSpace object is the first item in
                # the tuple dict entry associated with the name of the target
                # function space
                fspace = self._kern.eval_targets[fs_name][0]
                basis_name = function_space.get_basis_name(on_space=fspace)
                self._arglist.append(basis_name)
                if var_accesses:
                    var_accesses.add_access(basis_name, AccessType.READ,
                                            self._kern, [1])

    def diff_basis(self, function_space, var_accesses=None):
        '''
        Add differential basis information for the function space to the
        argument list. If supplied it also stores this access in
        var_accesses.

        :param function_space: the function space for which the differential \
                               basis functions are required.
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        for rule in self._kern.qr_rules.values():
            diff_basis_name = function_space.get_diff_basis_name(
                qr_var=rule.psy_name)
            self._arglist.append(diff_basis_name)
            if var_accesses:
                var_accesses.add_access(diff_basis_name, AccessType.READ,
                                        self._kern, [1])

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
                self._arglist.append(diff_basis_name)
                if var_accesses:
                    var_accesses.add_access(diff_basis_name, AccessType.READ,
                                            self._kern, [1])

    def orientation(self, function_space, var_accesses=None):
        '''Add orientation information for this function space to the
        argument list. If supplied it also stores this access in
        var_accesses.

        :param function_space: the function space for which orientation \
            information is added.
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        orientation_name = function_space.orientation_name
        self._arglist.append(orientation_name)
        if var_accesses:
            var_accesses.add_access(orientation_name, AccessType.READ,
                                    self._kern, [1])

    def field_bcs_kernel(self, function_space, var_accesses=None):
        '''Implement the boundary_dofs array fix for a field. If supplied it
        also stores this access in var_accesses.

        :param function_space: the function space for which boundary dofs \
            are required.
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        fspace = None
        for fspace in self._kern.arguments.unique_fss:
            if fspace.orig_name == "any_space_1":
                break
        farg = self._kern.arguments.get_arg_on_space(fspace)
        # Sanity check - expect the enforce_bc_code kernel to only have
        # a field argument.
        if farg.type != "gh_field":
            raise GenerationError(
                "Expected a gh_field from which to look-up boundary dofs "
                "for kernel {0} but got {1}".format(self._kern.name,
                                                    farg.type))
        base_name = "boundary_dofs_" + farg.name
        name = self._kern.root.symbol_table.name_from_tag(base_name)
        self._arglist.append(name)
        if var_accesses:
            var_accesses.add_access(name, AccessType.READ, self._kern, [1])

    def operator_bcs_kernel(self, _, var_accesses=None):
        '''
        Supply necessary additional arguments for the kernel that
        applies boundary conditions to a LMA operator.
        :param _: unused, only for consistency with base class.
        :type _: :py:class:`psyclone.dynamo3.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        # This kernel has only a single LMA operator as argument.
        # Checks for this are performed in ArgOrdering.generate()
        op_arg = self._kern.arguments.args[0]
        base_name = "boundary_dofs_"+op_arg.name
        name = self._kern.root.symbol_table.name_from_tag(base_name)
        self._arglist.append(name)
        if var_accesses:
            var_accesses.add_access(name, AccessType.READ, self._kern, [1])

    def mesh_properties(self, var_accesses=None):
        ''' Provide the kernel arguments required for the mesh properties
        specified in the kernel metadata.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        if self._kern.mesh.properties:
            from psyclone.dynamo0p3 import LFRicMeshProperties
            self._arglist.extend(
                LFRicMeshProperties(self._kern).
                kern_args(stub=False, var_accesses=var_accesses))

    def quad_rule(self, var_accesses=None):
        ''' Add quadrature-related information to the kernel argument list.
        Adds the necessary arguments to the self._arglist list, and optionally
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

            if var_accesses:
                for var_name in rule.kernel_args:
                    var_accesses.add_access(var_name, AccessType.READ,
                                            self._kern)

            if shape == "gh_quadrature_xyoz":
                # XYoZ quadrature requires the number of quadrature points in
                # the horizontal and in the vertical.
                self._nqp_positions.append(
                    {"horizontal": len(self._arglist) + 1,
                     "vertical": len(self._arglist) + 2})
                self._arglist.extend(rule.kernel_args)

            elif shape == "gh_quadrature_edge":
                # TODO #705 support transformations supplying the number of
                # quadrature points for edge quadrature.
                self._arglist.extend(rule.kernel_args)
            elif shape == "gh_quadrature_face":
                # TODO #705 support transformations supplying the number of
                # quadrature points for face quadrature.
                self._arglist.extend(rule.kernel_args)
            else:
                raise NotImplementedError(
                    "quad_rule: no support implemented for quadrature with a "
                    "shape of '{0}'. Supported shapes are: {1}.".format(
                        shape, supported_qr_shapes))

    def banded_dofmap(self, function_space, var_accesses=None):
        ''' Add banded dofmap (required for CMA operator assembly).

        :param function_space: the function space for which banded dofmaps
            is added.
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        # Note that the necessary ndf values will already have been added
        # to the argument list as they are mandatory for every function
        # space that appears in the meta-data.
        map_name = function_space.cbanded_map_name
        self._arglist.append(map_name)
        if var_accesses:
            var_accesses.add_access(map_name, AccessType.READ, self._kern)

    def indirection_dofmap(self, function_space, operator=None,
                           var_accesses=None):
        '''Add indirection dofmap required when applying a CMA operator. If
        supplied it also stores this access in var_accesses.

        :param function_space: the function space for which indirect dofmap \
            is required.
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        :param operator: the CMA operator.
        :type operator: :py:class:`psyclone.dynamo0p3.DynKernelArguments`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        map_name = function_space.cma_indirection_map_name
        self._arglist.append(map_name)
        if var_accesses:
            var_accesses.add_access(map_name, AccessType.READ, self._kern)

    @property
    def nlayers_positions(self):
        '''
        :return: the position(s) in the argument list of the \
        variable(s) that passes the number of layers. The generate \
        method must be called first.
        :rtype: list of int.

        :raises InternalError: if the generate() method has not been
        called.

        '''
        if not self._generate_called:
            raise InternalError(
                "KernCallArgList: the generate() method should be called "
                "before the nlayers_positions() method")
        return self._nlayers_positions

    @property
    def nqp_positions(self):
        '''
        :return: the positions in the argument list of the variables that \
        pass the number of quadrature points. The number and type of \
        these will change depending on the type of quadrature. A list \
        of dictionaries is returned with the quadrature directions \
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
        '''
        :return: the position(s) in the argument list and the function \
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

    @property
    def arglist(self):
        '''
        :return: the kernel argument list. The generate method must be \
        called first.
        :rtype: list of str.

        :raises InternalError: if the generate() method has not been \
        called.

        '''
        if not self._generate_called:
            raise InternalError(
                "KernCallArgList: the generate() method should be called "
                "before the arglist() method")
        return self._arglist

    def _cell_ref_name(self, var_accesses=None):
        '''
        Utility routine which determines whether to return the cell value
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
            if var_accesses:
                var_accesses.add_access("colour", AccessType.READ, self._kern)
                var_accesses.add_access("cell", AccessType.READ, self._kern)
                var_accesses.add_access(self._kern.colourmap, AccessType.READ,
                                        self._kern, ["colour", "cell"])
            return self._kern.colourmap + "(colour, cell)"

        if var_accesses:
            var_accesses.add_access("cell", AccessType.READ, self._kern)

        return "cell"
