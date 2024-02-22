# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
# Modified I. Kavcic, A. Coughtrie and L. Turner, Met Office
# Modified J. Henrichs, Bureau of Meteorology

'''This module implements the base class for managing arguments to
kernel calls.
'''

import abc

from psyclone import psyGen
from psyclone.core import AccessType, Signature
# The next two imports cannot be merged, since this would create
# a circular dependency.
from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.lfric_symbol_table import LFRicSymbolTable
from psyclone.domain.lfric.metadata_to_arguments_rules import (
    MetadataToArgumentsRules)
from psyclone.errors import GenerationError, InternalError
from psyclone.psyir.nodes import ArrayReference, Reference
from psyclone.psyir.symbols import ScalarType


class ArgOrdering:
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
    :type kern: :py:class:`psyclone.domain.lfric.LFRicKern`

    '''
    def __init__(self, kern):
        self._kern = kern
        self._generate_called = False
        # If available, get an existing symbol table to create unique names
        # and symbols required for PSyIR. Otherwise just create a new
        # symbol table (required for stub generation atm).
        invoke_sched = None
        if kern:
            invoke_sched = kern.ancestor(psyGen.InvokeSchedule)

        # TODO #1934 - we should not keep a reference to a SymbolTable here
        # as this creates a double reference (with
        # self._kernel.ancestor(InvokeSchedule)._symbol_table) to that table
        # and might go stale e.g. if the tree is copied.
        # In fact, using the same symbol table as the Invoke is a bit odd as
        # we are describing kernel *arguments* here so they will have a
        # different interface to those in the Schedule of the invoke.
        if invoke_sched:
            self._symtab = invoke_sched.symbol_table
        else:
            self._symtab = LFRicSymbolTable()

        # TODO #1934 Completely remove the usage of strings, instead
        # use the PSyIR representation.
        self._arglist = []

        # This stores the PSyIR representation of the arguments
        self._psyir_arglist = []
        self._arg_index_to_metadata_index = {}

    def psyir_append(self, node):
        '''Appends a PSyIR node to the PSyIR argument list.

        :param node: the node to append.
        :type node: :py:class:`psyclone.psyir.nodes.Node`

        '''
        self._psyir_arglist.append(node)

    def append(self, var_name, var_accesses=None, var_access_name=None,
               mode=AccessType.READ, metadata_posn=None):
        # pylint: disable=too-many-arguments
        '''Appends the specified variable name to the list of all arguments and
        stores the mapping between the position of this actual argument and
        the corresponding metadata entry. If var_accesses is given, it will
        also record the access to the variable. The name of the variable
        accessed can be overwritten by specifying var_access_name. By default
        it is assumed that access mode is READ (which can be set with
        ``mode``).

        :param str var_name: the name of the variable.
        :param var_accesses: optional class to store variable access \
            information.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`
        :param str var_access_name: optional name of the variable for \
            which access information is stored (used e.g. when the \
            actual argument is field_proxy, but the access is to be \
            recorded for field).
        :param mode: optional access mode (defaults to READ).
        :type mode: :py:class:`psyclone.core.access_type.AccessType`
        :param int metadata_posn: the location of the corresponding entry in \
            the list of arguments in the kernel metadata (if any).

        '''
        # Keep track of which metadata argument this actual argument
        # corresponds to.
        self._arg_index_to_metadata_index[len(self._arglist)] = metadata_posn

        self._arglist.append(var_name)

        if var_accesses is not None:
            if var_access_name:
                var_accesses.add_access(Signature(var_access_name), mode,
                                        self._kern)
            else:
                var_accesses.add_access(Signature(var_name), mode,
                                        self._kern)

    def extend(self, list_var_name, var_accesses=None,
               mode=AccessType.READ, list_metadata_posn=None):
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
            :py:class:`psyclone.core.VariablesAccessInfo`
        :param mode: optional access mode (defaults to READ).
        :type mode: Optional[:py:class:`psyclone.core.access_type.AccessType`]
        :param Optional[List[int]] list_metadata_posn: list of metadata \
            argument positions.

        '''
        for idx, var in enumerate(list_var_name):
            if list_metadata_posn:
                self.append(var, var_accesses=var_accesses, mode=mode,
                            metadata_posn=list_metadata_posn[idx])
            else:
                self.append(var, mode=mode, var_accesses=var_accesses)

    def append_integer_reference(self, name, tag=None):
        '''This function adds a reference to an integer variable to the list
        of PSyIR nodes. If the symbol does not exist, it will be added to the
        symbol table. If no tag is specified, is uses the name as tag. It also
        returns the symbol.

        :param str name: name of the integer variable to declare.
        :param tag: optional tag of the integer variable to declare.
        :type tag: Optional[str]

        :returns: the symbol to which a reference was added.
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        if tag is None:
            tag = name
        sym = self._symtab.find_or_create_integer_symbol(name, tag)
        self.psyir_append(Reference(sym))
        return sym

    def get_array_reference(self, array_name, indices, intrinsic_type,
                            tag=None, symbol=None):
        # pylint: disable=too-many-arguments
        '''This function creates an array reference. If there is no symbol
        with the given tag, a new array symbol will be defined using the given
        intrinsic_type. If a symbol already exists but has no type, it will
        be replaced.

        :param str array_name: the name and tag of the array.
        :param indices: the indices to be used in the PSyIR reference. It \
            must either be ":", or a PSyIR node.
        :type indices: List[Union[str, py:class:`psyclone.psyir.nodes.Node`]]
        :param intrinsic_type: the intrinsic type of the array.
        :type intrinsic_type: \
            :py:class:`psyclone.psyir.symbols.datatypes.ScalarType.Intrinsic`
        :param tag: optional tag for the symbol.
        :type tag: Optional[str]
        :param symbol: optional the symbol to use.
        :type: Optional[:py:class:`psyclone.psyir.symbols.Symbol`]

        :returns: a reference to the symbol used.
        :rtype: :py:class:`psyclone.psyir.nodes.Reference`

        '''
        if not tag:
            tag = array_name
        if not symbol:
            symbol = self._symtab.find_or_create_array(array_name,
                                                       len(indices),
                                                       intrinsic_type,
                                                       tag)
        else:
            if symbol.name != array_name:
                raise InternalError(f"Specified symbol '{symbol.name}' has a "
                                    f"different name than the specified array "
                                    f"name '{array_name}'.")

        # If all indices are specified as ":", just use the name itself
        # to reproduce the current look of the code.
        if indices == [":"]*len(indices):
            ref = Reference(symbol)
        else:
            ref = ArrayReference.create(symbol, indices)
        return ref

    def append_array_reference(self, array_name, indices, intrinsic_type,
                               tag=None, symbol=None):
        # pylint: disable=too-many-arguments
        '''This function adds an array reference. If there is no symbol with
        the given tag, a new array symbol will be defined using the given
        intrinsic_type. If a symbol already exists but has no type, it will
        be replaced. The created reference is added to the list of PSyIR
        expressions, and the symbol is returned to the user.

        :param str array_name: the name and tag of the array.
        :param indices: the indices to be used in the PSyIR reference. It \
            must either be ":", or a PSyIR node.
        :type indices: List[Union[str, py:class:`psyclone.psyir.nodes.Node`]]
        :param intrinsic_type: the intrinsic type of the array.
        :type intrinsic_type: \
            :py:class:`psyclone.psyir.symbols.datatypes.ScalarType.Intrinsic`
        :param tag: optional tag for the symbol.
        :type tag: Optional[str]
        :param symbol: optional the symbol to use.
        :type symbol: Optional[:py:class:`psyclone.psyir.symbols.Symbol`]

        :returns: the symbol used in the added reference.
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`

        '''

        ref = self.get_array_reference(array_name, indices, intrinsic_type,
                                       tag=tag, symbol=symbol)
        self.psyir_append(ref)
        return ref.symbol

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
        :rtype: List[str]

        :raises InternalError: if the generate() method has not been \
                               called.

        '''
        if not self._generate_called:
            raise InternalError(
                f"The argument list in {type(self).__name__} is empty. "
                f"Has the generate() method been called?")
        return self._arglist

    @property
    def psyir_arglist(self):
        '''
        :return: the kernel argument list as PSyIR expressions. The generate \
            method must be called first.
        :rtype: List[:py:class:`psyclone.psyir.nodes.Reference`]

        :raises InternalError: if the generate() method has not been called.

        '''
        if not self._psyir_arglist:
            raise InternalError(
                f"The PSyIR argument list in {type(self).__name__} is empty. "
                f"Has the generate() method been called?")
        return self._psyir_arglist

    def metadata_index_from_actual_index(self, idx):
        '''
        Returns the index of the entry in the meta_args list from which the
        actual subroutine argument at `idx` originated.

        :param int idx: the index of an actual argument to the kernel \
                        subroutine.

        :returns: the 0-indexed position of the corresponding metadata entry \
                  or None if there isn't one.
        :rtype: Optional[int]

        '''
        return self._arg_index_to_metadata_index[idx]

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
            :py:class:`psyclone.core.VariablesAccessInfo`

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
                    f"ArgOrdering.generate(): Unexpected argument type found. "
                    f"Expected one of '{const.VALID_ARG_TYPE_NAMES}' but "
                    f"found '{arg.argument_type}'")
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
            if (MetadataToArgumentsRules.bc_kern_regex.match(self._kern.name)
                    and unique_fs.orig_name.lower() == "any_space_1"):
                self.field_bcs_kernel(unique_fs, var_accesses=var_accesses)

        # Add boundary dofs array to the operator boundary condition
        # kernel (enforce_operator_bc_kernel) arguments
        if self._kern.name.lower() == "enforce_operator_bc_code":
            # Sanity checks - this kernel should only have a single LMA
            # operator as argument
            if len(self._kern.arguments.args) > 1:
                raise GenerationError(
                    f"Kernel {self._kern.name} has "
                    f"{len(self._kern.arguments.args)} arguments when it "
                    f"should only have 1 (an LMA operator)")
            op_arg = self._kern.arguments.args[0]
            if op_arg.argument_type != "gh_operator":
                raise GenerationError(
                    f"Expected an LMA operator from which to look-up boundary "
                    f"dofs but kernel {self._kern.name} has argument "
                    f"{op_arg.argument_type}.")
            if op_arg.access != AccessType.READWRITE:
                raise GenerationError(
                    f"Kernel {self._kern.name} is recognised as a kernel which"
                    f" applies boundary conditions to an operator. However its"
                    f" operator argument has access "
                    f"{op_arg.access.api_specific_name()} rather than "
                    f"gh_readwrite.")
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
        '''Add cell position information.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''

    def cell_map(self, var_accesses=None):
        '''Add cell-map and related cell counts (for inter-grid kernels)
        to the argument list. If supplied it also stores these accesses to the
        var_access object.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''

    def mesh_height(self, var_accesses=None):
        '''Add mesh height (nlayers) to the argument list and if supplied
        stores this access in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''

    def _mesh_ncell2d(self, var_accesses=None):
        '''Add the number of columns in the mesh (including halos) to the
        argument list and stores this access in var_accesses (if supplied).

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''

    def _mesh_ncell2d_no_halos(self, var_accesses=None):
        '''Add the number of columns in the mesh (excluding halos) to the
        argument list and stores this access in var_accesses (if supplied).

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

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
            :py:class:`psyclone.core.VariablesAccessInfo`

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
            :py:class:`psyclone.core.VariablesAccessInfo`

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
            :py:class:`psyclone.core.VariablesAccessInfo`

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
            :py:class:`psyclone.core.VariablesAccessInfo`

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
            :py:class:`psyclone.core.VariablesAccessInfo`

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
            :py:class:`psyclone.core.VariablesAccessInfo`

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
            :py:class:`psyclone.core.VariablesAccessInfo`

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
            :py:class:`psyclone.core.VariablesAccessInfo`

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
            :py:class:`psyclone.core.VariablesAccessInfo`

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
            :py:class:`psyclone.core.VariablesAccessInfo`

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
            :py:class:`psyclone.core.VariablesAccessInfo`

        :raises InternalError: if the argument is not a recognised scalar type.

        '''
        const = LFRicConstants()
        if not scalar_arg.is_scalar:
            raise InternalError(
                f"Expected argument type to be one of "
                f"{const.VALID_SCALAR_NAMES} but got "
                f"'{scalar_arg.argument_type}'")

        if scalar_arg.is_literal:
            # If we have a literal, do not add it to the variable access
            # information. We do this by providing None as var access.
            self.append(scalar_arg.name, None, mode=scalar_arg.access,
                        metadata_posn=scalar_arg.metadata_index)
        else:
            self.append(scalar_arg.name, var_accesses, mode=scalar_arg.access,
                        metadata_posn=scalar_arg.metadata_index)

    def fs_common(self, function_space, var_accesses=None):
        '''Add function-space related arguments common to LMA operators and
        fields. If supplied it also stores this access in var_accesses.

        :param function_space: the function space for which the related \
            arguments common to LMA operators and fields are added.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        # There is currently one argument: "ndf"
        sym = self.append_integer_reference(function_space.ndf_name)
        self.append(sym.name, var_accesses)

    def fs_compulsory_field(self, function_space, var_accesses=None):
        '''Add compulsory arguments associated with this function space to
        the list. If supplied it also stores this access in var_accesses.

        :param function_space: the function space for which the compulsory \
            arguments are added.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

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
            :py:class:`psyclone.core.VariablesAccessInfo`

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
            :py:class:`psyclone.core.VariablesAccessInfo`

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
            :py:class:`psyclone.core.VariablesAccessInfo`

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
            :py:class:`psyclone.core.VariablesAccessInfo`

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
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''

    @abc.abstractmethod
    def mesh_properties(self, var_accesses=None):
        '''Provide the kernel arguments required for the mesh properties
        specified in the kernel metadata. If supplied it also stores this
        access in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''

    @abc.abstractmethod
    def quad_rule(self, var_accesses=None):
        '''Add quadrature-related information to the kernel argument list.
        Adds the necessary arguments to the argument list, and optionally
        adds variable access information to the var_accesses object.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''

    def banded_dofmap(self, function_space, var_accesses=None):
        '''Add banded dofmap (required for CMA operator assembly).

        :param function_space: the function space for which banded dofmap
            is added.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        # Note that the necessary ndf values will already have been added
        # to the argument list as they are mandatory for every function
        # space that appears in the meta-data.
        sym = self.append_array_reference(
            function_space.cbanded_map_name, indices=[":", ":"],
            intrinsic_type=ScalarType.Intrinsic.INTEGER)
        self.append(sym.name, var_accesses)

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
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        # pylint: disable=unused-argument
        map_name = function_space.cma_indirection_map_name
        self.append_array_reference(map_name, [":"],
                                    ScalarType.Intrinsic.INTEGER, tag=map_name)
        self.append(map_name, var_accesses)

    def ref_element_properties(self, var_accesses=None):
        '''Add kernel arguments relating to properties of the reference
        element. If supplied it also stores this access in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        if self._kern.reference_element.properties:
            # Avoid circular import
            # pylint: disable=import-outside-toplevel
            from psyclone.dynamo0p3 import DynReferenceElement
            refelem_args_symbols = \
                DynReferenceElement(self._kern).kern_args_symbols()
            for symbol in refelem_args_symbols:
                # All kernel arguments are simple references:
                self.psyir_append(Reference(symbol))
                self.append(symbol.name, var_accesses)


# ============================================================================
# For automatic documentation creation:
__all__ = ["ArgOrdering"]
