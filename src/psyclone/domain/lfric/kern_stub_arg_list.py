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

'''This module implements a class that creates the argument list
for a kernel subroutine.
'''

from __future__ import print_function, absolute_import

from psyclone.core.access_type import AccessType
from psyclone.domain.lfric import ArgOrdering
from psyclone.errors import GenerationError, InternalError
from psyclone.psyir.symbols import SymbolTable


class KernStubArgList(ArgOrdering):
    # pylint: disable=too-many-public-methods
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
        self._first_arg = True
        self._first_arg_decl = None
        ArgOrdering.__init__(self, kern)
        # TODO 719 The stub_symtab is not connected to other parts of the
        # Stub generation. Also the symboltable already has an
        # argument_list that may be able to replace the _arglist below.
        self._stub_symtab = SymbolTable()

    def cell_position(self, var_accesses=None):
        '''Adds a cell argument to the argument list and if supplied stores
        this access in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        self._arglist.append("cell")
        if var_accesses:
            var_accesses.add_access("cell", AccessType.READ, self._kern)

    def mesh_height(self, var_accesses=None):
        ''' Add mesh height (nlayers) to the argument list and if supplied
        stores this access in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        self._arglist.append("nlayers")
        if var_accesses:
            var_accesses.add_access("nlayers", AccessType.READ, self._kern)

    def mesh_ncell2d(self, var_accesses=None):
        '''Add the number of columns in the mesh to the argument list and if
        supplied stores this access in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        self._arglist.append("ncell_2d")
        if var_accesses:
            var_accesses.add_access("ncell_2d", AccessType.READ, self._kern)

    def field_vector(self, argvect, var_accesses=None):
        '''Add the field vector associated with the argument 'argvect' to the
        argument list. If supplied it also stores these accesses to the
        var_access object.

        :param argvect: the corresponding kernel argument.
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
            if self._first_arg:
                self._first_arg = False
            self._arglist.append(text)
            if var_accesses:
                # Each argument is array, so mark the array access:
                var_accesses.add_access(text, AccessType.READ, self._kern, [1])

    def field(self, arg, var_accesses=None):
        '''Add the field associated with the argument 'arg' to the argument
        list. If supplied it also stores this access in var_accesses.

        :param arg: the kernel argument (field).
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        text = arg.name + "_" + arg.function_space.mangled_name
        self._arglist.append(text)
        if var_accesses:
            # It's an array, so add an arbitrary index value for the
            # stored indices (which is at this stage the only way to
            # indicate an array access).
            var_accesses.add_access(text, arg.access, self._kern, [1])

    def stencil_unknown_extent(self, arg, var_accesses=None):
        '''Add stencil information associated with a kernel argument if the
        extent is unknown. If supplied it also stores this access in
        var_accesses.

        :param arg: the meta-data description of the kernel argument with \
                    which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        from psyclone.dynamo0p3 import DynStencils
        name = DynStencils.dofmap_size_name(self._stub_symtab, arg)
        self._arglist.append(name)
        if var_accesses:
            var_accesses.add_access(name, AccessType.READ, self._kern, [1])

    def stencil_unknown_direction(self, arg, var_accesses=None):
        '''
        Add stencil information associated with the argument 'arg' if the
        direction is unknown. If supplied it also stores this access in
        var_accesses.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        from psyclone.dynamo0p3 import DynStencils
        name = DynStencils.direction_name(self._stub_symtab, arg)
        self._arglist.append(name)
        if var_accesses:
            var_accesses.add_access(name, AccessType.READ, self._kern)

    def stencil(self, arg, var_accesses=None):
        '''Add general stencil information associated with a kernel
        argument. If supplied it also stores this access in var_accesses.

        :param arg: the meta-data description of the kernel argument with \
                    which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
         :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        from psyclone.dynamo0p3 import DynStencils
        name = DynStencils.dofmap_name(self._stub_symtab, arg)
        self._arglist.append(name)
        if var_accesses:
            # It's an array, so add an arbitrary index value for the
            # stored indices (which is at this stage the only way to
            # indicate an array access).
            var_accesses.add_access(name, AccessType.READ, self._kern, [1])

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
        size = arg.name + "_ncell_3d"
        self._arglist.append(size)
        # If this is the first argument in the kernel then keep a
        # note so that we can put subsequent declarations in the
        # correct location
        if self._first_arg:
            self._first_arg = False
        text = arg.name
        self._arglist.append(text)
        if var_accesses:
            var_accesses.add_access(size, AccessType.READ, self._kern)
            var_accesses.add_access(text, AccessType.READ, self._kern)

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
        self._arglist.append(arg.name)
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
        self._arglist += _local_args
        if var_accesses:
            var_accesses.add_access(arg.name, AccessType.READ, self._kern)
            for var_name in _local_args:
                var_accesses.add_access(var_name, AccessType.READ, self._kern)

        if self._first_arg:
            self._first_arg = False

    def banded_dofmap(self, function_space, var_accesses=None):
        ''' Declare the banded dofmap required for a CMA operator
        that maps to/from the specified function space. If supplied it also
        stores this access in var_accesses.

         :param function_space: the function space for which banded dofmap
            is added.
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

         '''
        dofmap = function_space.cbanded_map_name
        self._arglist.append(dofmap)
        if var_accesses:
            var_accesses.add_access(dofmap, AccessType.READ, self._kern)

    def indirection_dofmap(self, function_space, operator=None,
                           var_accesses=None):
        '''Declare the indirection dofmaps required when applying a
        CMA operator. If supplied it also stores this access in var_accesses.

        :param function_space: the function space for which the indirect \
            dofmap is required.
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        :param operator: the CMA operator.
        :type operator: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises GenerationError: if no kernel argument is supplied.
        :raises GenerationError: if the supplied kernel argument is not a \
                                 CMA operator.

        '''
        if not operator:
            raise GenerationError("Internal error: no CMA operator supplied.")
        if operator.type != "gh_columnwise_operator":
            raise GenerationError(
                "Internal error: a CMA operator (gh_columnwise_operator) must "
                "be supplied but got {0}".format(operator.type))
        map_name = function_space.cma_indirection_map_name
        self._arglist.append(map_name)
        if var_accesses:
            var_accesses.add_access(map_name, AccessType.READ, self._kern)

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
        if not scalar_arg.is_scalar():
            from psyclone.dynamo0p3 import GH_VALID_SCALAR_NAMES
            raise InternalError(
                "Expected argument type to be one of '{0}' but got '{1}'".
                format(GH_VALID_SCALAR_NAMES, scalar_arg.type))
        self._arglist.append(scalar_arg.name)
        if var_accesses:
            var_accesses.add_access(scalar_arg.name, AccessType.READ,
                                    self._kern)

    def fs_common(self, function_space, var_accesses=None):
        '''Provide arguments common to LMA operators and fields on a space.
        There is one: "ndf". If supplied it also stores this access in
        var_accesses.

        :param function_space: the function space for which the related \
            arguments common to LMA operators and fields are added.
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        ndf_name = function_space.ndf_name
        self._arglist.append(ndf_name)
        if var_accesses:
            var_accesses.add_access(ndf_name, AccessType.READ, self._kern)

    def fs_compulsory_field(self, function_space, var_accesses=None):
        ''' Provide compulsory arguments if there is a field on this
        function space. If supplied it also stores this access in
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
        self._arglist.append(map_name)
        if var_accesses:
            var_accesses.add_access(undf_name, AccessType.READ, self._kern)
            var_accesses.add_access(map_name, AccessType.READ, self._kern)

    def basis(self, function_space, var_accesses=None):
        '''
        Add the necessary declarations for basis function(s) on the supplied
        function space. There can be more than one if this is an evaluator
        and/or multiple 'gh_shape's have been requested in the kernel metadata.
        If supplied it also stores these accesses in var_accesses.

        :param function_space: the function space for which to provide \
                               the basis functions
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises InternalError: if the evaluator shape is not recognised.

        '''
        from psyclone.dynamo0p3 import VALID_EVALUATOR_SHAPES, \
            VALID_QUADRATURE_SHAPES
        for shape in self._kern.eval_shapes:
            if shape in VALID_QUADRATURE_SHAPES:
                # A kernel stub won't have a name for the corresponding
                # quadrature argument so we create one by appending the last
                # part of the shape name to "qr_".
                basis_name = function_space.get_basis_name(
                    qr_var="qr_"+shape.split("_")[-1])
                self._arglist.append(basis_name)
                if var_accesses:
                    var_accesses.add_access(basis_name, AccessType.READ,
                                            self._kern)

            elif shape in VALID_EVALUATOR_SHAPES:
                # Need a basis array for each target space upon which the basis
                # functions have been evaluated. _kern.eval_targets is a dict
                # where the values are 2-tuples of (FunctionSpace, argument).
                for _, target in self._kern.eval_targets.items():
                    basis_name = \
                        function_space.get_basis_name(on_space=target[0])
                    self._arglist.append(basis_name)
                    if var_accesses:
                        var_accesses.add_access(basis_name, AccessType.READ,
                                                self._kern)
            else:
                raise InternalError(
                    "Unrecognised evaluator shape ('{0}'). Expected one of: "
                    "{1}".format(shape, VALID_EVALUATOR_SHAPES))

    def diff_basis(self, function_space, var_accesses=None):
        '''
        Provide the necessary declarations for the differential basis function
        on the supplied function space. If supplied it also stores this access
        in var_accesses.

        :param function_space: the function space for which to provide the \
                               differential basis function
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises InternalError: if the evaluator shape is not recognised.

        '''
        from psyclone.dynamo0p3 import VALID_EVALUATOR_SHAPES, \
            VALID_QUADRATURE_SHAPES
        for shape in self._kern.eval_shapes:
            if shape in VALID_QUADRATURE_SHAPES:
                # We need differential basis functions for quadrature. A
                # kernel stub won't have a name for the corresponding
                # quadrature argument so we create one by appending the
                # last part of the shape name to "qr_".
                diff_basis_name = function_space.get_diff_basis_name(
                    qr_var="qr_"+shape.split("_")[-1])
                self._arglist.append(diff_basis_name)
                if var_accesses:
                    var_accesses.add_access(diff_basis_name, AccessType.READ,
                                            self._kern)

            elif shape in VALID_EVALUATOR_SHAPES:
                # We need differential basis functions for an evaluator,
                # potentially for multiple target spaces. _kern.eval_targets is
                # a dict where the values are 2-tuples of
                # (FunctionSpace, argument).
                for _, target in self._kern.eval_targets.items():
                    diff_basis_name = function_space.get_diff_basis_name(
                        on_space=target[0])
                    self._arglist.append(diff_basis_name)
                    if var_accesses:
                        var_accesses.add_access(diff_basis_name,
                                                AccessType.READ, self._kern)
            else:
                raise InternalError("Unrecognised evaluator shape ('{0}'). "
                                    "Expected one of: {1}".format(
                                        shape, VALID_EVALUATOR_SHAPES))

    def orientation(self, function_space, var_accesses=None):
        '''
        Provide orientation information for the function space. If supplied
        it also stores this access in var_accesses.

        :param function_space: the function space for which orientation \
            is required.
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
                                    self._kern)

    def field_bcs_kernel(self, function_space, var_accesses=None):
        '''Implement the boundary_dofs array fix for a field. If supplied it
        also stores this access in var_accesses.

        :param function_space: the function space for which boundary dofs \
            are required.
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        arg = self._kern.arguments.get_arg_on_space(function_space)
        name = "boundary_dofs_"+arg.name
        self._arglist.append(name)
        if var_accesses:
            var_accesses.add_access(name, AccessType.READ, self._kern)

    def operator_bcs_kernel(self, function_space, var_accesses=None):
        '''Implement the boundary_dofs array fix for operators. This is the
        same as for fields with the function space set to the 'to' space of
        the operator. If supplied it also stores this access in
        var_accesses.

        :param function_space: the 'to' function space of the operator.
        :type function_space: :py:class:`psyclone.dynamo3.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        self.field_bcs_kernel(function_space, var_accesses=var_accesses)

    def mesh_properties(self, var_accesses=None):
        ''' Provide the kernel arguments required for the mesh properties
        specified in the kernel metadata. If supplied it also stores this
        access in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        if self._kern.mesh.properties:
            from psyclone.dynamo0p3 import LFRicMeshProperties
            self._arglist.extend(
                LFRicMeshProperties(self._kern).
                kern_args(stub=True, var_accesses=var_accesses))

    def quad_rule(self, var_accesses=None):
        '''Provide quadrature information for this kernel stub (necessary
        arguments). If supplied it also stores this access in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        for rule in self._kern.qr_rules.values():
            if var_accesses:
                for var_name in rule.kernel_args:
                    var_accesses.add_access(var_name, AccessType.READ,
                                            self._kern)
            self._arglist.extend(rule.kernel_args)

    @property
    def arglist(self):
        '''return the kernel argument list. The generate function must be
        called first'''
        if not self._arglist:
            raise GenerationError(
                "Internal error. The argument list in KernStubArgList:"
                "arglist() is empty. Has the generate() method been called?")
        return self._arglist
