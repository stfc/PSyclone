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

'''This module implements the base class for managing arguments to
kernel calls.
'''

from __future__ import print_function, absolute_import
import abc

from psyclone.core import AccessType, Signature
from psyclone.domain.lfric import LFRicConstants
from psyclone.errors import GenerationError, InternalError


class ArgOrdering(object):
    # pylint: disable=too-many-public-methods
    # TODO: #845 Check that all implicit variables have the right type.
    '''Base class capturing the arguments, type and ordering of data in
    a Kernel call. This base class implements some functionality of a list
    (extend and append functions), but not using list as a base class.
    Reason is that many typical functions of a list make only sense to
    be used after ``generate`` is called, which would then require a large
    number of functions to be re-implemented. So instead the property
    ``arglist`` checks that ``generate`` has been called and then provides
    a list.

    :param kern: the kernel call object to use.
    :type kern: :py:class:`psyclone.dynamo0p3.DynKern`

    '''
    def __init__(self, kern):
        self._kern = kern
        self._generate_called = False
        self._arglist = []

    def append(self, var_name, var_accesses=None, var_access_name=None,
               mode=AccessType.READ):
        '''Appends the specified variable name to the list of all arguments.
        If var_accesses is given, it will also record the access to the
        variable. The name of the variable accessed can be overwritten by
        specifying var_access_name. By default it is assumed that access
        mode is READ (which can be set with mode).

        :param str var_name: the name of the variable.
        :param var_accesses: optional class to store variable access \
            information.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        :param str var_access_name: optional name of the variable for \
            which access information is stored (used e.g. when the \
            actual argument is field_proxy, but the access is to be \
            recorded for field).
        :param mode: optional access mode (defaults to READ).
        :type mode: :py:class:`psyclone.core.access_type.AccessType`

        '''
        self._arglist.append(var_name)
        if var_accesses is not None:
            if var_access_name:
                var_accesses.add_access(Signature(var_access_name), mode,
                                        self._kern)
            else:
                var_accesses.add_access(Signature(var_name), mode,
                                        self._kern)

    def extend(self, list_var_name, var_accesses=None,
               mode=AccessType.READ):
        '''Appends all variable names in the argument list to the list of
        all arguments. If var_accesses is given, it will also record the
        access to the variables. By default any access will be recorded as a
        read-only access, but this can be changed (for all variables
        included) using mode.

        :param list_var_name: the list with name of the variables to append.
        :type list_var_name: list of str.
        :param var_accesses: optional class to store variable access \
            information.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        :param mode: optional access mode (defaults to READ).
        :type mode: :py:class:`psyclone.core.access_type.AccessType`

        '''
        self._arglist.extend(list_var_name)
        if var_accesses:
            for var_name in list_var_name:
                var_accesses.add_access(Signature(var_name), mode, self._kern)

    @property
    def num_args(self):
        ''':returns: the current number of arguments stored in _arglist.
        :rtype: int

        '''
        return len(self._arglist)

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
                "The argument list in {0} is empty. "
                "Has the generate() method been called?"
                .format(type(self).__name__))
        return self._arglist

    def generate(self, var_accesses=None):
        # pylint: disable=too-many-statements, too-many-branches
        '''
        Specifies which arguments appear in an argument list, their type
        and their ordering. Calls methods for each type of argument
        that can be specialised by a child class for its particular need.
        If the optional argument var_accesses is supplied, this function
        will also add variable access information for each implicit argument
        (i.e. that is not explicitly listed in kernel metadata) that is
        added. These accesses will be marked as read.

        :param var_accesses: optional VariablesAccessInfo instance that \
            stores the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises GenerationError: if the kernel arguments break the \
                                 rules for the LFRic API.

        '''
        # Setting this first is important, since quite a few derived classes
        # will access self.arglist during generate() (e.g. to test if an
        # argument is already contained in the argument list).
        self._generate_called = True
        if self._kern.arguments.has_operator():
            # All operator types require the cell index to be provided
            self.cell_position(var_accesses=var_accesses)
        # Pass the number of layers in the mesh unless this kernel is
        # applying a CMA operator or doing a CMA matrix-matrix calculation
        if self._kern.cma_operation not in ["apply", "matrix-matrix"]:
            self.mesh_height(var_accesses=var_accesses)
        # Pass the number of cells in the mesh if this kernel has a
        # LMA operator argument
        # TODO this code should replace the code that currently includes
        # this quantity for *every* operator it encounters.
        # if self._kern.arguments.has_operator(op_type="gh_operator"):
        #     self.mesh_ncell3d()

        # Pass the number of columns in the mesh if this kernel operates on
        # the 'domain' or has a CMA operator argument. For the former we
        # exclude halo columns.
        if self._kern.iterates_over == "domain":
            self._mesh_ncell2d_no_halos(var_accesses=var_accesses)

        if self._kern.arguments.has_operator(op_type="gh_columnwise_operator"):
            self._mesh_ncell2d(var_accesses=var_accesses)

        if self._kern.is_intergrid:
            # Inter-grid kernels require special arguments
            # The cell-map for the current column providing the mapping from
            # the coarse to the fine mesh.
            self.cell_map(var_accesses=var_accesses)

        # For each argument in the order they are specified in the
        # kernel metadata, call particular methods depending on what
        # type of argument we find (field, field vector, operator or
        # scalar). If the argument is a field or field vector and also
        # has a stencil access then also call appropriate stencil
        # methods.
        const = LFRicConstants()
        for arg in self._kern.arguments.args:
            if arg.is_field:
                if arg.vector_size > 1:
                    self.field_vector(arg, var_accesses=var_accesses)
                else:
                    self.field(arg, var_accesses=var_accesses)
                if arg.descriptor.stencil:
                    if not arg.descriptor.stencil['extent']:
                        if arg.descriptor.stencil['type'] == "cross2d":
                            # stencil extent is not provided in the
                            # metadata so must be passed from the Algorithm
                            # layer.
                            self.stencil_2d_unknown_extent(
                                arg, var_accesses=var_accesses)
                            # Due to the nature of the stencil extent array
                            # the max size of a stencil branch must be passed
                            # from the Algorithm layer.
                            self.stencil_2d_max_extent(
                                arg, var_accesses=var_accesses)
                        else:
                            # stencil extent is not provided in the
                            # metadata so must be passed from the Algorithm
                            # layer.
                            self.stencil_unknown_extent(
                                arg, var_accesses=var_accesses)
                    if arg.descriptor.stencil['type'] == "xory1d":
                        # if "xory1d is specified then the actual
                        # direction must be passed from the Algorithm layer.
                        self.stencil_unknown_direction(arg,
                                                       var_accesses)
                    # stencil information that is always passed from the
                    # Algorithm layer.
                    if arg.descriptor.stencil['type'] == "cross2d":
                        self.stencil_2d(arg, var_accesses=var_accesses)
                    else:
                        self.stencil(arg, var_accesses=var_accesses)
            elif arg.argument_type == "gh_operator":
                self.operator(arg, var_accesses=var_accesses)
            elif arg.argument_type == "gh_columnwise_operator":
                self.cma_operator(arg, var_accesses=var_accesses)
            elif arg.is_scalar:
                self.scalar(arg, var_accesses=var_accesses)
            else:
                raise GenerationError(
                    "ArgOrdering.generate(): Unexpected argument type found. "
                    "Expected one of '{0}' but found '{1}'".
                    format(const.VALID_ARG_TYPE_NAMES,
                           arg.argument_type))
        # For each function space (in the order they appear in the
        # metadata arguments)
        for unique_fs in self._kern.arguments.unique_fss:
            # Provide arguments common to LMA operators and fields on
            # a space *unless* this is an inter-grid or CMA
            # matrix-matrix kernel
            if self._kern.cma_operation not in ["matrix-matrix"] and \
               not self._kern.is_intergrid:
                self.fs_common(unique_fs, var_accesses=var_accesses)
            # Provide additional arguments if there is a
            # field on this space
            if unique_fs.field_on_space(self._kern.arguments):
                if self._kern.is_intergrid:
                    self.fs_intergrid(unique_fs, var_accesses=var_accesses)
                else:
                    self.fs_compulsory_field(unique_fs,
                                             var_accesses=var_accesses)
            cma_op = unique_fs.cma_on_space(self._kern.arguments)
            if cma_op:
                if self._kern.cma_operation == "assembly":
                    # CMA-assembly requires banded dofmaps
                    self.banded_dofmap(unique_fs, var_accesses=var_accesses)
                elif self._kern.cma_operation == "apply":
                    # Applying a CMA operator requires indirection dofmaps
                    self.indirection_dofmap(unique_fs, operator=cma_op,
                                            var_accesses=var_accesses)

            # Provide any optional arguments. These arguments are
            # associated with the keyword arguments (basis function
            # and differential basis function) for a function space.
            if self._kern.fs_descriptors.exists(unique_fs):
                descriptors = self._kern.fs_descriptors
                descriptor = descriptors.get_descriptor(unique_fs)
                if descriptor.requires_basis:
                    self.basis(unique_fs, var_accesses=var_accesses)
                if descriptor.requires_diff_basis:
                    self.diff_basis(unique_fs, var_accesses=var_accesses)
            # Fix for boundary_dofs array to the boundary condition
            # kernel (enforce_bc_kernel) arguments
            if self._kern.name.lower() == "enforce_bc_code" and \
               unique_fs.orig_name.lower() == "any_space_1":
                self.field_bcs_kernel(unique_fs, var_accesses=var_accesses)

        # Add boundary dofs array to the operator boundary condition
        # kernel (enforce_operator_bc_kernel) arguments
        if self._kern.name.lower() == "enforce_operator_bc_code":
            # Sanity checks - this kernel should only have a single LMA
            # operator as argument
            if len(self._kern.arguments.args) > 1:
                raise GenerationError(
                    "Kernel {0} has {1} arguments when it should only have 1 "
                    "(an LMA operator)".format(self._kern.name,
                                               len(self._kern.arguments.args)))
            op_arg = self._kern.arguments.args[0]
            if op_arg.argument_type != "gh_operator":
                raise GenerationError(
                    "Expected an LMA operator from which to look-up boundary "
                    "dofs but kernel {0} has argument {1}.".
                    format(self._kern.name, op_arg.argument_type))
            if op_arg.access != AccessType.READWRITE:
                raise GenerationError(
                    "Kernel {0} is recognised as a kernel which applies "
                    "boundary conditions to an operator. However its operator "
                    "argument has access {1} rather than gh_readwrite.".
                    format(self._kern.name, op_arg.access.api_specific_name()))
            self.operator_bcs_kernel(op_arg.function_space_to,
                                     var_accesses=var_accesses)

        # Reference-element properties
        if self._kern.reference_element:
            self.ref_element_properties(var_accesses=var_accesses)

        # Mesh properties
        if self._kern.mesh:
            self.mesh_properties(var_accesses=var_accesses)

        # Provide qr arguments if required
        if self._kern.qr_required:
            self.quad_rule(var_accesses=var_accesses)

    @abc.abstractmethod
    def cell_position(self, var_accesses=None):
        '''Add cell position information.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''

    @abc.abstractmethod
    def cell_map(self, var_accesses=None):
        '''Add cell-map and related cell counts (for inter-grid kernels)
        to the argument list. If supplied it also stores these accesses to the
        var_access object.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''

    @abc.abstractmethod
    def mesh_height(self, var_accesses=None):
        '''Add mesh height (nlayers) to the argument list and if supplied
        stores this access in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''

    @abc.abstractmethod
    def _mesh_ncell2d(self, var_accesses=None):
        '''Add the number of columns in the mesh (including halos) to the
        argument list and stores this access in var_accesses (if supplied).

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''

    @abc.abstractmethod
    def _mesh_ncell2d_no_halos(self, var_accesses=None):
        '''Add the number of columns in the mesh (excluding halos) to the
        argument list and stores this access in var_accesses (if supplied).

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''

    @abc.abstractmethod
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

    @abc.abstractmethod
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

    @abc.abstractmethod
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

    @abc.abstractmethod
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

    @abc.abstractmethod
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

    @abc.abstractmethod
    def stencil_2d_max_extent(self, arg, var_accesses=None):
        '''Add 2D stencil information to the argument list associated with the
        argument 'arg' if the stencil extent (from which it is calculated) is
        passed from the Algorithm layer rather than being specified in kernel
        metadata. If supplied it also stores this access in var_accesses.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''

    @abc.abstractmethod
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

    @abc.abstractmethod
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

    @abc.abstractmethod
    def stencil_2d(self, arg, var_accesses=None):
        '''Add 2D stencil information associated with the argument 'arg'
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

    @abc.abstractmethod
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

    def scalar(self, scalar_arg, var_accesses=None):
        '''Add the name associated with the scalar argument to the argument
        list and optionally add this scalar to the variable access
        information.

        :param scalar_arg: the kernel argument.
        :type scalar_arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance that \
            stores information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises InternalError: if the argument is not a recognised scalar type.

        '''
        const = LFRicConstants()
        if not scalar_arg.is_scalar:
            raise InternalError(
                "Expected argument type to be one of {0} but got '{1}'".
                format(const.VALID_SCALAR_NAMES,
                       scalar_arg.argument_type))

        self.append(scalar_arg.name, var_accesses, mode=scalar_arg.access)

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
        # There is currently one argument: "ndf"
        ndf_name = function_space.ndf_name
        self.append(ndf_name, var_accesses)

    @abc.abstractmethod
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

    @abc.abstractmethod
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

    @abc.abstractmethod
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

    @abc.abstractmethod
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

    @abc.abstractmethod
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

    @abc.abstractmethod
    def operator_bcs_kernel(self, function_space, var_accesses=None):
        '''Supply necessary additional arguments for the kernel that
        applies boundary conditions to a LMA operator. If supplied it
        also stores this access in var_accesses.

        :param function_space: the function space of the operator.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''

    @abc.abstractmethod
    def mesh_properties(self, var_accesses=None):
        '''Provide the kernel arguments required for the mesh properties
        specified in the kernel metadata. If supplied it also stores this
        access in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''

    @abc.abstractmethod
    def quad_rule(self, var_accesses=None):
        '''Add quadrature-related information to the kernel argument list.
        Adds the necessary arguments to the argument list, and optionally
        adds variable access information to the var_accesses object.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''

    def banded_dofmap(self, function_space, var_accesses=None):
        '''Add banded dofmap (required for CMA operator assembly).

        :param function_space: the function space for which banded dofmap
            is added.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        # Note that the necessary ndf values will already have been added
        # to the argument list as they are mandatory for every function
        # space that appears in the meta-data.
        self.append(function_space.cbanded_map_name, var_accesses)

    def indirection_dofmap(self, function_space, operator=None,
                           var_accesses=None):
        '''Add indirection dofmap required when applying a CMA operator. If
        supplied it also stores this access in var_accesses.

        :param function_space: the function space for which the indirect \
            dofmap is required.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param operator: the CMA operator (not used at the moment).
        :type operator: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        # pylint: disable=unused-argument
        map_name = function_space.cma_indirection_map_name
        self.append(map_name, var_accesses)

    def ref_element_properties(self, var_accesses=None):
        '''Add kernel arguments relating to properties of the reference
        element. If supplied it also stores this access in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        if self._kern.reference_element.properties:
            # Avoid circular import
            # pylint: disable=import-outside-toplevel
            from psyclone.dynamo0p3 import DynReferenceElement
            refelem_args = DynReferenceElement(self._kern).kern_args()
            self.extend(refelem_args, var_accesses)


# ============================================================================
# For automatic documentation creation:
__all__ = ["ArgOrdering"]
