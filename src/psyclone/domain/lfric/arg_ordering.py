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

'''This module implements the base class for managing arguments to
kernel calls.
'''

from __future__ import print_function, absolute_import
import abc

from psyclone.errors import GenerationError
from psyclone.core.access_type import AccessType


class ArgOrdering(object):
    # pylint: disable=too-many-public-methods
    '''Base class capturing the arguments, type and ordering of data in
    a Kernel call.'''
    def __init__(self, kern):
        self._kern = kern
        self._generate_called = False
        self._arglist = []

    def generate(self, var_accesses=None):
        # pylint: disable=too-many-statements, too-many-branches
        '''
        Specifies which arguments appear in an argument list, their type
        and their ordering. Calls methods for each type of argument
        that can be specialised by a child class for its particular need.
        If the optional argument var_accesses is supplied, this function
        will also add variable access information for each implicit argument
        that is added. These accesses will be marked as read.

        :param var_accesses: optional VariablesAccessInfo instance that \
            stores the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises GenerationError: if the kernel arguments break the
                                 rules for the Dynamo 0.3 API.
        '''
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
        # Pass the number of columns in the mesh if this kernel has a CMA
        # operator argument
        if self._kern.arguments.has_operator(op_type="gh_columnwise_operator"):
            self.mesh_ncell2d(var_accesses=var_accesses)

        if self._kern.is_intergrid:
            # Inter-grid kernels require special arguments
            # The cell-map for the current column providing the mapping from
            # the coarse to the fine mesh.
            self.cell_map(var_accesses=var_accesses)

        # for each argument in the order they are specified in the
        # kernel metadata, call particular methods depending on what
        # type of argument we find (field, field vector, operator or
        # scalar). If the argument is a field or field vector and also
        # has a stencil access then also call appropriate stencil
        # methods.

        # Import here to avoid circular depondency
        from psyclone.dynamo0p3 import GH_VALID_ARG_TYPE_NAMES, \
            GH_VALID_SCALAR_NAMES

        for arg in self._kern.arguments.args:
            if arg.type == "gh_field":
                if arg.vector_size > 1:
                    self.field_vector(arg, var_accesses=var_accesses)
                else:
                    self.field(arg, var_accesses=var_accesses)
                if arg.descriptor.stencil:
                    if not arg.descriptor.stencil['extent']:
                        # stencil extent is not provided in the
                        # metadata so must be passed
                        self.stencil_unknown_extent(arg,
                                                    var_accesses=var_accesses)
                    if arg.descriptor.stencil['type'] == "xory1d":
                        # if "xory1d is specified then the actual
                        # direction must be passed
                        self.stencil_unknown_direction(arg,
                                                       var_accesses)
                    # stencil information that is always passed
                    self.stencil(arg, var_accesses=var_accesses)
            elif arg.type == "gh_operator":
                self.operator(arg, var_accesses=var_accesses)
            elif arg.type == "gh_columnwise_operator":
                self.cma_operator(arg, var_accesses=var_accesses)
            elif arg.type in GH_VALID_SCALAR_NAMES:
                self.scalar(arg, var_accesses=var_accesses)
            else:
                raise GenerationError(
                    "Unexpected arg type found in dynamo0p3.py:"
                    "ArgOrdering:generate(). Expected one of '{0}' "
                    "but found '{1}'".format(GH_VALID_ARG_TYPE_NAMES,
                                             arg.type))
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
            # associated with the keyword arguments (basis function,
            # differential basis function and orientation) for a
            # function space.
            if self._kern.fs_descriptors.exists(unique_fs):
                descriptors = self._kern.fs_descriptors
                descriptor = descriptors.get_descriptor(unique_fs)
                if descriptor.requires_basis:
                    self.basis(unique_fs, var_accesses=var_accesses)
                if descriptor.requires_diff_basis:
                    self.diff_basis(unique_fs, var_accesses=var_accesses)
                if descriptor.requires_orientation:
                    self.orientation(unique_fs, var_accesses=var_accesses)
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
            if op_arg.type != "gh_operator":
                raise GenerationError(
                    "Expected a LMA operator from which to look-up boundary "
                    "dofs but kernel {0} has argument {1}.".
                    format(self._kern.name, op_arg.type))
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

    def cell_position(self, var_accesses=None):
        '''
        Add cell position information

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.cell_position() must be implemented by "
            "subclass")

    def cell_map(self, var_accesses=None):
        '''
        Add cell-map information (for inter-grid kernels)

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.cell_map() must be implemented by subclass")

    def mesh_height(self, var_accesses=None):
        '''
        Add height information (i.e. no. of layers)

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.mesh_height() must be implemented by subclass")

    def mesh_ncell2d(self, var_accesses=None):
        '''
        Add the number of columns in the mesh

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.mesh_ncell2d() must be implemented by"
            "subclass")

    def cma_operator(self, arg, var_accesses=None):
        '''
        Add information on the CMA operator

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError("Error: ArgOrdering.cma_operator() must "
                                  "be implemented by subclass")

    def field_vector(self, argvect, var_accesses=None):
        '''
        Add field-vector information for this field-vector argument

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.field_vector() must be implemented by "
            "subclass")

    def field(self, arg, var_accesses=None):
        '''
        Add field information for this field argument

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.field() must be implemented by subclass")

    def stencil_unknown_extent(self, arg, var_accesses=None):
        '''
        Add stencil extent information for this stencil argument

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.stencil_unknown_extent() must be implemented "
            "by subclass")

    def stencil_unknown_direction(self, arg, var_accesses=None):
        '''
        Add stencil direction information for this stencil argument

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.stencil_unknown_direction() must be "
            "implemented by subclass")

    def stencil(self, arg, var_accesses=None):
        '''
        Add stencil information for this stencil argument

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.stencil() must be implemented by subclass")

    def operator(self, arg, var_accesses=None):
        '''
        Add operator information for this operator argument

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.operator() must be implemented by subclass")

    def scalar(self, scalar_arg, var_accesses=None):
        '''Add scalar information for this scalar argument.

        :param var_accesses: optional VariablesAccessInfo instance that \
            stores information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.scalar() must be implemented by subclass")

    def fs_common(self, function_space, var_accesses=None):
        '''
        Add information common to LMA operators and fields for this
        function space

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.fs_common() must be implemented by "
            "subclass")

    def fs_compulsory_field(self, function_space, var_accesses=None):
        '''
        Add compulsory information for this function space

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.fs_compulsory_field() must be implemented "
            "by subclass")

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
        raise NotImplementedError(
            "Error: ArgOrdering.fs_intergrid() must be implemented "
            "by subclass")

    def basis(self, function_space, var_accesses=None):
        '''
        Add basis function information for this function space

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.basis() must be implemented by subclass")

    def diff_basis(self, function_space, var_accesses=None):
        '''
        Add differential basis function information for this function
        space

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.diff_basis() must be implemented by subclass")

    def orientation(self, function_space, var_accesses=None):
        '''
        Add orientation information for this function space

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.orientation() must be implemented by subclass")

    def field_bcs_kernel(self, function_space, var_accesses=None):
        '''
        Add boundary condition information for a field on this function
        space

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.field_bcs_kernel() must be implemented by "
            "subclass")

    def operator_bcs_kernel(self, function_space, var_accesses=None):
        '''
        Add boundary condition information for an operator on this function
        space

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.operator_bcs_kernel() must be implemented by "
            "subclass")

    def ref_element_properties(self, var_accesses=None):
        ''' Add kernel arguments relating to properties of the reference
        element. '''
        if self._kern.reference_element.properties:
            from psyclone.dynamo0p3 import DynReferenceElement
            refelem_args = DynReferenceElement(self._kern).kern_args()
            self._arglist.extend(refelem_args)
            if var_accesses is not None:
                for var_name in refelem_args:
                    var_accesses.add_access(var_name, AccessType.READ,
                                            self._kern)

    @abc.abstractmethod
    def mesh_properties(self, var_accesses=None):
        ''' Provide the kernel arguments required for the mesh properties
        specified in the kernel metadata. '''

    @abc.abstractmethod
    def quad_rule(self, var_accesses=None):
        ''' Add kernel arguments required for quadrature. '''

    def banded_dofmap(self, function_space, var_accesses=None):
        '''
        Add banded dofmap (required for CMA operator assembly)

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError("Error: ArgOrdering.banded_dofmap() must"
                                  " be implemented by subclass")

    def indirection_dofmap(self, function_space, operator=None,
                           var_accesses=None):
        '''
        Add indirection dofmap required when applying a CMA operator

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError("Error: ArgOrdering.indirection_dofmap() "
                                  "must be implemented by subclass")
