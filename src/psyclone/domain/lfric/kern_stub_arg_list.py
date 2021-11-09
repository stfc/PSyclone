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

'''This module implements a class that creates the argument list
for a kernel subroutine.
'''

from __future__ import print_function, absolute_import

from psyclone.domain.lfric import ArgOrdering, LFRicConstants
from psyclone.errors import InternalError
from psyclone.psyir.symbols import SymbolTable


class KernStubArgList(ArgOrdering):
    # pylint: disable=too-many-public-methods
    # TODO: #845 Check that all implicit variables have the right type.
    '''Creates the argument list required to create and declare the
    required arguments for a kernel subroutine.  The ordering and type
    of the arguments is captured by the base class.

    :param kern: Kernel for which to create argument list.
    :type kern: :py:class:`psyclone.dynamo0p3.DynKern`

    :raises NotImplementedError: if the kernel is inter-grid.
    :raises NotImplementedError: if the kernel requires properties of the \
                                 reference element.
    '''
    def __init__(self, kern):
        # We don't yet support inter-grid kernels (Issue #162)
        if kern.is_intergrid:
            raise NotImplementedError(
                "Kernel {0} is an inter-grid kernel and stub generation "
                "is not yet supported for inter-grid kernels".
                format(kern.name))
        ArgOrdering.__init__(self, kern)
        # TODO 719 The stub_symtab is not connected to other parts of the
        # Stub generation. Also the symboltable already has an
        # argument_list that may be able to replace the argument list below.
        self._stub_symtab = SymbolTable()

    def cell_position(self, var_accesses=None):
        '''Adds a cell argument to the argument list and if supplied stores
        this access in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        self.append("cell", var_accesses)

    def mesh_height(self, var_accesses=None):
        '''Add mesh height (nlayers) to the argument list and if supplied
        stores this access in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        self.append("nlayers", var_accesses)

    def _mesh_ncell2d(self, var_accesses=None):
        '''Add the number of columns in the mesh to the argument list and if
        supplied stores this access in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        self.append("ncell_2d", var_accesses)

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
        # The CMA operator itself
        self.append(arg.name, var_accesses)
        # Associated scalar parameters
        nrow = arg.name + "_nrow"
        _local_args = [nrow]
        if arg.function_space_to.orig_name != \
           arg.function_space_from.orig_name:
            # If the to- and from-spaces are different then so are ncol and
            # nrow so we pass both of them. If they are the same then we
            # could pass either but choose to pass nrow and not ncol.
            ncol = arg.name + "_ncol"
            _local_args.append(ncol)
        bandwidth = arg.name + "_bandwidth"
        alpha = arg.name + "_alpha"
        beta = arg.name + "_beta"
        gamma_m = arg.name + "_gamma_m"
        gamma_p = arg.name + "_gamma_p"
        _local_args += [bandwidth, alpha, beta, gamma_m, gamma_p]
        self.extend(_local_args, var_accesses)

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
            text = (argvect.name + "_" +
                    argvect.function_space.mangled_name +
                    "_v" + str(idx))
            self.append(text, var_accesses)

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
        text = arg.name + "_" + arg.function_space.mangled_name
        self.append(text, var_accesses, mode=arg.access)

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
        # Avoid circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.dynamo0p3 import DynStencils
        name = DynStencils.dofmap_size_name(self._stub_symtab, arg)
        self.append(name, var_accesses)

    def stencil_unknown_direction(self, arg, var_accesses=None):
        '''Add stencil information to the argument list associated with the
        argument 'arg' if the direction is unknown. If supplied it also stores
        this access in var_accesses.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        # Avoid circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.dynamo0p3 import DynStencils
        name = DynStencils.direction_name(self._stub_symtab, arg)
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
        # Avoid circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.dynamo0p3 import DynStencils
        var_name = DynStencils.dofmap_name(self._stub_symtab, arg)
        self.append(var_name, var_accesses)

    def stencil_2d_max_extent(self, arg, var_accesses=None):
        '''Add the maximum branch extent for a 2D stencil associated with the
        argument 'arg' to the argument list. If supplied it also stores this
        in var_accesses.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional `SingleVariableAccessInfo` \
            instance to store the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.SingleVariableAccessInfo`

        '''
        # The maximum branch extent is not specified in the metadata so pass
        # the value in.
        # Import here to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.dynamo0p3 import DynStencils
        name = DynStencils.max_branch_length_name(self._stub_symtab, arg)
        self.append(name, var_accesses)

    def stencil_2d_unknown_extent(self, arg, var_accesses=None):
        '''Add 2D stencil information to the argument list associated with the
        argument 'arg' if the extent is unknown. If supplied it also stores
        this access in var_accesses.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional `VariablesAccessInfo` instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        # Avoid circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.dynamo0p3 import DynStencils
        name = DynStencils.dofmap_size_name(self._stub_symtab, arg)
        self.append(name, var_accesses)

    def stencil_2d(self, arg, var_accesses=None):
        '''Add general 2D stencil information associated with the argument
        'arg' to the argument list. If supplied it also stores this access in
        var_accesses.

        :param arg: the meta-data description of the kernel \
            argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional `VariablesAccessInfo` instance to store \
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
        # to the centre even when the stencil is truncated at boundaries.
        # Import here to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.dynamo0p3 import DynStencils
        var_name = DynStencils.dofmap_name(self._stub_symtab, arg)
        self.append(var_name, var_accesses)

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
        size = arg.name + "_ncell_3d"
        self.append(size, var_accesses)
        self.append(arg.name, var_accesses)

    def fs_compulsory_field(self, function_space, var_accesses=None):
        ''' Provide compulsory arguments if there is a field on this
        function space. If supplied it also stores this access in
        var_accesses.

        :param function_space: the function space for which the compulsory \
            arguments are added.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        self.append(function_space.undf_name, var_accesses)
        self.append(function_space.map_name, var_accesses)

    def basis(self, function_space, var_accesses=None):
        '''Add basis function information for this function space to the
        argument list and optionally to the variable access information.
        There can be more than one if this is an evaluator and/or multiple
        'gh_shape's have been requested in the kernel metadata. If supplied
        it also stores these accesses in var_accesses.

        :param function_space: the function space for which to provide \
                               the basis functions
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises InternalError: if the evaluator shape is not recognised.

        '''
        const = LFRicConstants()
        for shape in self._kern.eval_shapes:
            if shape in const.VALID_QUADRATURE_SHAPES:
                # A kernel stub won't have a name for the corresponding
                # quadrature argument so we create one by appending the last
                # part of the shape name to "qr_".
                basis_name = function_space.get_basis_name(
                    qr_var="qr_"+shape.split("_")[-1])
                self.append(basis_name, var_accesses)

            elif shape in const.VALID_EVALUATOR_SHAPES:
                # Need a basis array for each target space upon which the basis
                # functions have been evaluated. _kern.eval_targets is a dict
                # where the values are 2-tuples of (FunctionSpace, argument).
                for _, target in self._kern.eval_targets.items():
                    basis_name = \
                        function_space.get_basis_name(on_space=target[0])
                    self.append(basis_name, var_accesses)
            else:
                raise InternalError(
                    "Unrecognised evaluator shape ('{0}'). Expected one of: "
                    "{1}".format(shape, const.VALID_EVALUATOR_SHAPES))

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

        :raises InternalError: if the evaluator shape is not recognised.

        '''
        const = LFRicConstants()
        for shape in self._kern.eval_shapes:
            if shape in const.VALID_QUADRATURE_SHAPES:
                # We need differential basis functions for quadrature. A
                # kernel stub won't have a name for the corresponding
                # quadrature argument so we create one by appending the
                # last part of the shape name to "qr_".
                diff_basis_name = function_space.get_diff_basis_name(
                    qr_var="qr_"+shape.split("_")[-1])
                self.append(diff_basis_name, var_accesses)

            elif shape in const.VALID_EVALUATOR_SHAPES:
                # We need differential basis functions for an evaluator,
                # potentially for multiple target spaces. _kern.eval_targets is
                # a dict where the values are 2-tuples of
                # (FunctionSpace, argument).
                for _, target in self._kern.eval_targets.items():
                    diff_basis_name = function_space.get_diff_basis_name(
                        on_space=target[0])
                    self.append(diff_basis_name, var_accesses)
            else:
                raise InternalError("Unrecognised evaluator shape ('{0}'). "
                                    "Expected one of: {1}".format(
                                        shape, const.VALID_EVALUATOR_SHAPES))

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

        '''
        arg = self._kern.arguments.get_arg_on_space(function_space)
        name = "boundary_dofs_"+arg.name
        self.append(name, var_accesses)

    def operator_bcs_kernel(self, function_space, var_accesses=None):
        '''Supply necessary additional arguments for the kernel that
        applies boundary conditions to a LMA operator. If supplied it
        also stores this access in var_accesses.

        :param function_space: the 'to' function space of the operator.
        :type function_space: :py:class:`psyclone.dynamo3.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        self.field_bcs_kernel(function_space, var_accesses=var_accesses)

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
            # Avoid circular import
            # pylint: disable=import-outside-toplevel
            from psyclone.dynamo0p3 import LFRicMeshProperties
            self.extend(LFRicMeshProperties(self._kern).
                        kern_args(stub=True, var_accesses=var_accesses))

    def quad_rule(self, var_accesses=None):
        '''Add quadrature-related information to the kernel argument list.
        Adds the necessary arguments to the argument list, and optionally
        adds variable access information to the var_accesses object.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        for rule in self._kern.qr_rules.values():
            self.extend(rule.kernel_args, var_accesses)

    def indirection_dofmap(self, function_space, operator=None,
                           var_accesses=None):
        '''Add indirection dofmap required when applying a CMA operator. If
        supplied it also stores this access in var_accesses.

        :param function_space: the function space for which the indirect \
            dofmap is required.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param operator: the CMA operator.
        :type operator: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises InternalError: if no kernel argument is supplied.
        :raises InternalError: if the supplied kernel argument is not a \
            CMA operator.

        '''
        if not operator:
            raise InternalError("No CMA operator supplied.")
        if operator.argument_type != "gh_columnwise_operator":
            raise InternalError(
                "A CMA operator (gh_columnwise_operator) must "
                "be supplied but got {0}".format(operator.argument_type))
        super(KernStubArgList, self).indirection_dofmap(function_space,
                                                        operator,
                                                        var_accesses)


# ============================================================================
# For automatic documentation creation:
__all__ = ["KernStubArgList"]
