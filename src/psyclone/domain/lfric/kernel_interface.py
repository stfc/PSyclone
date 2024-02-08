# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
# Modified: I. Kavcic and L. Turner, Met Office
#           A. R. Porter and N. Nobre, STFC Daresbury Lab
# Modified: J. Henrichs, Bureau of Meteorology

'''This module creates the expected arguments for an LFRic coded
kernel based on the kernel metadata.

'''
from psyclone.core import AccessType
from psyclone.domain.lfric import ArgOrdering, LFRicConstants
from psyclone.domain.lfric.lfric_symbol_table import LFRicSymbolTable
from psyclone.domain.lfric.lfric_types import LFRicTypes
from psyclone.errors import InternalError
from psyclone.psyir.frontend.fparser2 import INTENT_MAPPING
from psyclone.psyir.nodes import Reference
from psyclone.psyir.symbols import ArgumentInterface


# pylint: disable=too-many-public-methods, no-member
class KernelInterface(ArgOrdering):
    '''Create the kernel arguments for the supplied kernel as specified by
    the associated kernel metadata and the kernel ordering rules
    encoded in the ArgOrdering base class as method callbacks.

    A PSyIR symbol table is created containing appropriate LFRic PSyIR
    symbols to specify the arguments. If an argument is an array with
    one or more dimension sizes specified by another argument, then
    the associated array symbol is created so that it references the
    appropriate symbol.

    Related arguments - e.g. a field has an associated dofmap - are
    not directly connected, they must be inferred from the function
    space names. It is not yet clear whether this would be useful or
    not.

    TODO #928: This class should replace the current kernel stub generation
    code when all of its methods are implemented.

    :param kern: the kernel for which to create arguments.
    :type kern: :py:class:`psyclone.domain.lfric.LFRicKern`

    '''
    #: Mapping from a generic PSyIR datatype to the equivalent
    #: LFRic-specific field datasymbol.
    field_mapping = {
        "integer": "IntegerFieldDataSymbol",
        "real": "RealFieldDataSymbol",
        "logical": "LogicalFieldDataSymbol"}
    #: Mapping from a generic PSyIR datatype to the equivalent
    #: LFRic-specific vector field datasymbol.
    vector_field_mapping = {
        "integer": "IntegerVectorFieldDataSymbol",
        "real": "RealVectorFieldDataSymbol",
        "logical": "LogicalVectorFieldDataSymbol"}
    #: Mapping from the LFRic metadata description of quadrature to the
    #: associated LFRic-specific basis function datasymbol.
    basis_mapping = {
        "gh_quadrature_xyoz": "BasisFunctionQrXyozDataSymbol",
        "gh_quadrature_face": "BasisFunctionQrFaceDataSymbol",
        "gh_quadrature_edge": "BasisFunctionQrEdgeDataSymbol"}
    #: Mapping from the LFRic metadata description of quadrature to the
    #: associated LFRic-specific differential basis function datasymbol.
    diff_basis_mapping = {
        "gh_quadrature_xyoz": "DiffBasisFunctionQrXyozDataSymbol",
        "gh_quadrature_face": "DiffBasisFunctionQrFaceDataSymbol",
        "gh_quadrature_edge": "DiffBasisFunctionQrEdgeDataSymbol"}
    _read_access = ArgumentInterface(ArgumentInterface.Access.READ)

    def __init__(self, kern):
        super().__init__(kern)
        # We need a brand new symbol table, not a reference to the one for
        # the Schedule containing this kernel call (which is what the
        # ArgOrdering constructor defaults to). This is so that we can
        # specify the correct interface for those symbols passed
        # as arguments.
        # TODO #1934 - we should not keep a reference to a SymbolTable here.
        # We should just be using the one associated with the kernel routine
        # but currently a Kern is not a Call to a Routine.
        self._symtab = LFRicSymbolTable()

    def generate(self, var_accesses=None):
        '''Call the generate base class then add the argument list as it can't
        be appended as we go along.

        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.VariablesAccessInfo`

        '''
        super().generate(var_accesses=var_accesses)
        # Set the argument list for the symbol table. This is done at
        # the end after incrementally adding symbols to the _args
        # list, as it is not possible to incrementally add symbols to
        # the symbol table argument list.
        self._symtab.specify_argument_list(self._arglist)
        # While data dependence analysis does not use the symbol
        # table, see #845, we have to provide variable information
        # separately. This is done by using the base class append()
        # method. However, this method also adds the variable names to the
        # internal _arglist list which we do not want as we have
        # already added our symbols there. Therefore we need to remove
        # them afterwards.
        # Map from symbol table accesses to dependence analysis accesses.
        mapping = {ArgumentInterface.Access.READ: AccessType.READ,
                   ArgumentInterface.Access.READWRITE: AccessType.READWRITE,
                   ArgumentInterface.Access.WRITE: AccessType.WRITE}
        len_arglist = len(self._arglist)
        for symbol in self._symtab.symbols:
            self.append(symbol.name, var_accesses,
                        mode=mapping[symbol.interface.access])
        self._arglist = self._arglist[:len_arglist]

    def cell_position(self, var_accesses=None):
        '''Create an LFRic cell-position object and add it to the symbol table
        and argument list.

        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.VariablesAccessInfo`

        '''
        symbol = self._symtab.find_or_create_tag(
            "cell", symbol_type=LFRicTypes("CellPositionDataSymbol"),
            interface=self._read_access)
        self._arglist.append(symbol)

    def mesh_height(self, var_accesses=None):
        '''Create an LFRic mesh height object and add it to the symbol table
        and argument list.

        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.VariablesAccessInfo`

        '''
        symbol = self._symtab.find_or_create_tag(
            "nlayers", symbol_type=LFRicTypes("MeshHeightDataSymbol"),
            interface=self._read_access)
        self._arglist.append(symbol)

    def _mesh_ncell2d(self, var_accesses=None):
        '''Not implemented.

        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.VariablesAccessInfo`

        :raises NotImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError("TODO #928: _mesh_ncell2d not implemented")

    def _mesh_ncell2d_no_halos(self, var_accesses=None):
        '''Not implemented.

        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.VariablesAccessInfo`

        :raises NotImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError(
            "TODO #928: _mesh_ncell2d_no_halos not implemented")

    def cell_map(self, var_accesses=None):
        '''Not implemented.

        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.VariablesAccessInfo`

        :raises NotImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError("TODO #928: cell_map not implemented")

    def field_vector(self, argvect, var_accesses=None):
        '''Create LFRic field vector arguments and add them to the symbol
        table and argument list. Also declare the associated "undf"
        symbol if it has not already been declared so that it can be
        used to dimension the field vector arguments.

        :param argvect: the field vector to add.
        :type argvect: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.VariablesAccessInfo`

        :raises NotImplementedError: if the datatype of the vector \
            field is not supported.

        '''
        fs_name = argvect.function_space.orig_name
        undf_symbol = self._symtab.find_or_create_tag(
            f"undf_{fs_name}", fs=fs_name,
            symbol_type=LFRicTypes("NumberOfUniqueDofsDataSymbol"),
            interface=self._read_access)

        interface = ArgumentInterface(INTENT_MAPPING[argvect.intent])
        try:
            type_name = self.vector_field_mapping[argvect.intrinsic_type]
            field_class = LFRicTypes(type_name)
        except KeyError as info:
            message = (f"kernel interface does not support a vector field of "
                       f"type '{argvect.intrinsic_type}'.")
            raise NotImplementedError(message) from info
        for idx in range(argvect.vector_size):
            tag = f"{argvect.name}_v{idx}"
            field_data_symbol = self._symtab.find_or_create_tag(
                tag, symbol_type=field_class, dims=[Reference(undf_symbol)],
                fs=fs_name, interface=interface)

            self._arg_index_to_metadata_index[len(self._arglist)] = (
                argvect.metadata_index)
            self._arglist.append(field_data_symbol)

    def field(self, arg, var_accesses=None):
        '''Create an LFRic field argument and add it to the symbol table and
        argument list. Also declare the associated "undf" symbol if it
        has not already been declared so that it can be used to
        dimension the field argument.

        :param arg: the field to add.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.VariablesAccessInfo`

        :raises NotImplementedError: if the datatype of the field is \
            not supported.

        '''
        fs_name = arg.function_space.orig_name

        undf_symbol = self._symtab.find_or_create_tag(
            f"undf_{fs_name}",
            symbol_type=LFRicTypes("NumberOfUniqueDofsDataSymbol"),
            fs=fs_name, interface=self._read_access)

        try:
            type_name = self.field_mapping[arg.intrinsic_type]
            field_class = LFRicTypes(type_name)
        except KeyError as info:
            message = (f"kernel interface does not support a field of type "
                       f"'{arg.intrinsic_type}'.")
            raise NotImplementedError(message) from info
        field_data_symbol = self._symtab.find_or_create_tag(
            arg.name, interface=ArgumentInterface(INTENT_MAPPING[arg.intent]),
            symbol_type=field_class, dims=[Reference(undf_symbol)], fs=fs_name)

        self._arg_index_to_metadata_index[len(self._arglist)] = (
            arg.metadata_index)
        self._arglist.append(field_data_symbol)

    def stencil_unknown_extent(self, arg, var_accesses=None):
        '''Not implemented.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.VariablesAccessInfo`

        :raises NotImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError(
            "TODO #928: stencil_unknown_extent not implemented")

    def stencil_unknown_direction(self, arg, var_accesses=None):
        '''Not implemented.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.VariablesAccessInfo`

        :raises NotImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError(
            "TODO #928: stencil_unknown_direction not implemented")

    def stencil(self, arg, var_accesses=None):
        '''Not implemented.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.VariablesAccessInfo`

        :raises NotImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError("TODO #928: stencil not implemented")

    def operator(self, arg, var_accesses=None):
        '''Create an LFRic operator argument and an ncells argument and add
        them to the symbol table and argument list. Also declare the
        associated 'fs_from', 'fs_to' symbols if they have not already
        been declared so that they can be used to dimension the
        operator symbol (as well as ncells).

        :param arg: the operator to add.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: an unused optional argument that stores
            information about variable accesses.
        :type var_accesses: :py:class:`psyclone.core.VariablesAccessInfo`

        :raises NotImplementedError: if the datatype of the field is
            not supported.

        '''
        fs_from_name = arg.function_space_from.orig_name

        ndf_symbol_from = self._symtab.find_or_create_tag(
            f"ndf_{fs_from_name}", fs=fs_from_name,
            symbol_type=LFRicTypes("NumberOfDofsDataSymbol"),
            interface=self._read_access)
        fs_to_name = arg.function_space_to.orig_name
        ndf_symbol_to = self._symtab.find_or_create_tag(
            f"ndf_{fs_to_name}", fs=fs_to_name,
            symbol_type=LFRicTypes("NumberOfDofsDataSymbol"),
            interface=self._read_access)

        # We may already have a symbol for this argument as we add it for each
        # operator (TODO #2074).
        ncells = self._symtab.find_or_create(
            "ncell_3d",
            symbol_type=LFRicTypes("NumberOfCellsDataSymbol"),
            interface=self._read_access)
        self._arglist.append(ncells)

        op_arg_symbol = self._symtab.find_or_create_tag(
            arg.name, symbol_type=LFRicTypes("OperatorDataSymbol"),
            dims=[Reference(ndf_symbol_from), Reference(ndf_symbol_to),
                  Reference(ncells)],
            fs_from=fs_from_name, fs_to=fs_to_name,
            interface=ArgumentInterface(INTENT_MAPPING[arg.intent]))

        self._arg_index_to_metadata_index[len(self._arglist)] = (
            arg.metadata_index)
        self._arglist.append(op_arg_symbol)

    def cma_operator(self, arg, var_accesses=None):
        '''Not implemented.

        :param arg: the CMA operator argument.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.VariablesAccessInfo`

        :raises NotImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError("TODO #928: cma_operator not implemented")

    def scalar(self, scalar_arg, var_accesses=None):
        '''Create an LFRic scalar argument and add it to the symbol table and
        argument list.

        :param scalar_arg: the scalar to add.
        :type scalar_arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.VariablesAccessInfo`

        :raises NotImplementedError: if the datatype of the scalar is \
            not supported.

        '''
        mapping = {
            "integer": LFRicTypes("LFRicIntegerScalarDataSymbol"),
            "real": LFRicTypes("LFRicRealScalarDataSymbol"),
            "logical": LFRicTypes("LFRicLogicalScalarDataSymbol")}
        try:
            symbol = self._symtab.find_or_create_tag(
                scalar_arg.name,
                symbol_type=mapping[scalar_arg.intrinsic_type],
                interface=ArgumentInterface(INTENT_MAPPING[scalar_arg.intent]))
        except KeyError as info:
            message = (
                f"scalar of type '{scalar_arg.intrinsic_type}' not implemented"
                f" in KernelInterface class.")
            raise NotImplementedError(message) from info
        self._arg_index_to_metadata_index[len(self._arglist)] = (
            scalar_arg.metadata_index)
        self._arglist.append(symbol)

    def fs_common(self, function_space, var_accesses=None):
        '''Create any arguments that are common to a particular function
        space. At this time the only common argument is the number of
        degrees of freedom. Create the associated LFRic symbol, and
        add it to the symbol table and argument list.

        :param function_space: the function space for any common arguments.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.VariablesAccessInfo`

        '''
        fs_name = function_space.orig_name
        ndf_symbol = self._symtab.find_or_create_tag(
            f"ndf_{fs_name}", fs=fs_name,
            symbol_type=LFRicTypes("NumberOfDofsDataSymbol"),
            interface=self._read_access)
        self._arglist.append(ndf_symbol)

    def fs_intergrid(self, function_space, var_accesses=None):
        '''Not implemented.

        :param arg: the CMA operator argument.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.VariablesAccessInfo`

        :raises NotImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError("TODO #928: fs_intergrid not implemented")

    def fs_compulsory_field(self, function_space, var_accesses=None):
        '''Create any arguments that are compulsory for a field on a
        particular function space. At this time the compulsory
        arguments are the unique number of degrees of freedom and the
        dofmap. Create the associated LFRic symbol, and add it to the
        symbol table and argument list. Also declare the number of
        degrees of freedom and add to the symbol table if one has not
        yet been added.

        :param function_space: the function space for any compulsory arguments.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.VariablesAccessInfo`

        '''
        fs_name = function_space.orig_name
        undf_symbol = self._symtab.find_or_create_tag(
            f"undf_{fs_name}", fs=fs_name,
            symbol_type=LFRicTypes("NumberOfUniqueDofsDataSymbol"),
            interface=self._read_access)
        self._arglist.append(undf_symbol)

        fs_name = function_space.orig_name
        ndf_symbol = self._symtab.find_or_create_tag(
            f"ndf_{fs_name}", fs=fs_name,
            symbol_type=LFRicTypes("NumberOfDofsDataSymbol"),
            interface=self._read_access)

        dofmap_symbol = self._symtab.find_or_create_tag(
            f"dofmap_{fs_name}", fs=fs_name,
            symbol_type=LFRicTypes("DofMapDataSymbol"),
            dims=[Reference(ndf_symbol)], interface=self._read_access)
        self._arglist.append(dofmap_symbol)

    def banded_dofmap(self, function_space, var_accesses=None):
        '''Not implemented.

        :param function_space: the function space for this dofmap.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.VariablesAccessInfo`

        :raises NotImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError("TODO #928: banded_dofmap not implemented")

    def indirection_dofmap(self, function_space, operator=None,
                           var_accesses=None):
        '''Not implemented.

        :param function_space: the function space for this dofmap.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param operator: the CMA operator.
        :type operator: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.VariablesAccessInfo`

        :raises NotImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError(
            "TODO #928: indirection_dofmap not implemented")

    def basis(self, function_space, var_accesses=None):
        '''Create an LFRic basis function argument and add it to the symbol
        table and argument list.

        :param function_space: the function space for this basis function.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.VariablesAccessInfo`

        '''
        basis_name_func = function_space.get_basis_name
        # This import must be placed here to avoid circular dependencies
        # pylint: disable=import-outside-toplevel
        from psyclone.dynamo0p3 import DynBasisFunctions
        first_dim_value_func = DynBasisFunctions.basis_first_dim_value
        self._create_basis(function_space, self.basis_mapping,
                           basis_name_func, first_dim_value_func)

    def diff_basis(self, function_space, var_accesses=None):
        '''Create an LFRic differential basis function argument and add it to
        the symbol table and argument list.

        :param function_space: the function space for this \
            differential basis function.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.VariablesAccessInfo`

        '''
        basis_name_func = function_space.get_diff_basis_name
        # This import must be placed here to avoid circular dependencies
        # pylint: disable=import-outside-toplevel
        from psyclone.dynamo0p3 import DynBasisFunctions
        first_dim_value_func = DynBasisFunctions.diff_basis_first_dim_value
        self._create_basis(function_space, self.diff_basis_mapping,
                           basis_name_func, first_dim_value_func)

    def field_bcs_kernel(self, function_space, var_accesses=None):
        '''Create the boundary-dofs mask argument required for the
        enforce_bc_code kernel. Adds it to the symbol table and the argument
        list.

        :param function_space: the function space for this boundary condition.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: an unused optional argument that stores
            information about variable accesses.
        :type var_accesses: :py:class:`psyclone.core.VariablesAccessInfo`

        :raises InternalError: if the kernel does not have a single field as
                               argument.
        :raises InternalError: if the field argument is not on the
                               'ANY_SPACE_1' function space.

        '''
        # Sanity check - expect the enforce_bc_code to have a single argument
        # that is a field.
        if (len(self._kern.arguments.args) != 1 or
                not self._kern.arguments.args[0].is_field):
            const = LFRicConstants()
            raise InternalError(
                f"Kernel '{self._kern.name}' applies boundary conditions to a "
                f"field and therefore should have a single, field argument "
                f"(one of {const.VALID_FIELD_NAMES}) but got "
                f"{[arg.argument_type for arg in self._kern.arguments.args]}")
        farg = self._kern.arguments.args[0]
        fspace = farg.function_space
        # Sanity check - expect the field argument to the enforce_bc_code
        # kernel to be on the ANY_SPACE_1 space.
        if fspace.orig_name != "any_space_1":
            raise InternalError(
                f"Kernel '{self._kern.name}' applies boundary conditions to a "
                f"field but the supplied argument, '{farg.name}', is on "
                f"'{fspace.orig_name}' rather than the expected 'ANY_SPACE_1'")

        ndf_symbol = self._symtab.find_or_create_tag(
            f"ndf_{fspace.orig_name}", fs=fspace.orig_name,
            symbol_type=LFRicTypes("NumberOfDofsDataSymbol"),
            interface=self._read_access)

        # Add the boundary-dofs array argument.
        sym = self._symtab.find_or_create_tag(
            f"boundary_dofs_{farg.name}",
            interface=self._read_access,
            symbol_type=LFRicTypes("VerticalBoundaryDofMaskDataSymbol"),
            dims=[Reference(ndf_symbol), 2])
        self._arglist.append(sym)

    def operator_bcs_kernel(self, function_space, var_accesses=None):
        '''Not implemented.

        :param function_space: the function space for this bcs kernel
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.VariablesAccessInfo`

        :raises NotImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError(
            "TODO #928: operator_bcs_kernel not implemented")

    def ref_element_properties(self, var_accesses=None):
        ''' Properties associated with the reference element

        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.VariablesAccessInfo`

        '''
        # This callback does not contribute any kernel arguments

    def mesh_properties(self, var_accesses=None):
        ''' Properties associated with the mesh

        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.VariablesAccessInfo`

        '''
        # This callback does not contribute any kernel arguments

    def quad_rule(self, var_accesses=None):
        '''Create LFRic arguments associated with the required quadrature, if
        they do not already exist, and add them to the symbol table
        and argument list. The arguments depend on the type of
        quadrature requested.

        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.VariablesAccessInfo`

        :raises InternalError: if an unsupported quadrature shape is \
            found.

        '''
        # The kernel captures all the required quadrature shapes
        for shape in self._kern.qr_rules:
            if shape == "gh_quadrature_xyoz":
                nqp_xy = self._symtab.find_or_create_tag(
                    "nqp_xy",
                    symbol_type=LFRicTypes("NumberOfQrPointsInXyDataSymbol"),
                    interface=self._read_access)
                nqp_z = self._symtab.find_or_create_tag(
                    "nqp_z",
                    symbol_type=LFRicTypes("NumberOfQrPointsInZDataSymbol"),
                    interface=self._read_access)
                weights_xy = self._symtab.find_or_create_tag(
                    "weights_xy",
                    symbol_type=LFRicTypes("QrWeightsInXyDataSymbol"),
                    dims=[Reference(nqp_xy)], interface=self._read_access)
                weights_z = self._symtab.find_or_create_tag(
                    "weights_z",
                    symbol_type=LFRicTypes("QrWeightsInZDataSymbol"),
                    dims=[Reference(nqp_z)], interface=self._read_access)
                self._arglist.extend([nqp_xy, nqp_z, weights_xy, weights_z])
            elif shape == "gh_quadrature_face":
                nfaces = self._symtab.find_or_create_tag(
                    "nfaces",
                    symbol_type=LFRicTypes("NumberOfFacesDataSymbol"),
                    interface=self._read_access)
                nqp = self._symtab.find_or_create_tag(
                    "nqp_faces",
                    symbol_type=LFRicTypes(
                        "NumberOfQrPointsInFacesDataSymbol"),
                    interface=self._read_access)
                weights = self._symtab.find_or_create_tag(
                    "weights_faces",
                    symbol_type=LFRicTypes("QrWeightsInFacesDataSymbol"),
                    dims=[Reference(nqp)], interface=self._read_access)
                self._arglist.extend([nfaces, nqp, weights])
            elif shape == "gh_quadrature_edge":
                nedges = self._symtab.find_or_create_tag(
                    "nedges",
                    symbol_type=LFRicTypes("NumberOfEdgesDataSymbol"),
                    interface=self._read_access)
                nqp = self._symtab.find_or_create_tag(
                    "nqp_edges",
                    symbol_type=LFRicTypes(
                        "NumberOfQrPointsInEdgesDataSymbol"),
                    interface=self._read_access)
                weights = self._symtab.find_or_create_tag(
                    "weights_edges",
                    symbol_type=LFRicTypes("QrWeightsInEdgesDataSymbol"),
                    dims=[Reference(nqp)], interface=self._read_access)
                self._arglist.extend([nedges, nqp, weights])
            else:
                raise InternalError(f"Unsupported quadrature shape '{shape}' "
                                    f"found in kernel_interface.")

    def _create_basis(self, function_space, mapping, basis_name_func,
                      first_dim_value_func):
        '''Internal utility to create an LFRic basis or differential basis
        function argument specific to the particular quadrature that
        is being used and add it to the symbol table and argument
        list. Also declare the associated "ndf" symbol and any
        quadrature-specific symbols if they have not already been
        declared so that they can be used to dimension the basis or
        differential basis symbol.

        This utility function is used to avoid code replication as the
        structure of a basis function is very similar to the structure
        of a differential basis function.

        :param function_space: the function space that this basis or \
            differential basis function is on.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param dict mapping: a mapping from quadrature type to basis \
            or differential basis class name.
        :param method basis_name_func: a method that returns the name \
            of the basis or differential basis function for the \
            current function space.
        :param function first_dim_value_func: a function that returns \
            the size of the first dimension of the basis or \
            differential basis function for the current function \
            space.

        :raises NotImplementedError: if an evaluator shape is found \
            that is not a quadrature shape (currently just \
            'gh_evaluator').
        :raises InternalError: if the supplied evaluator shape is not \
            recognised.

        '''
        # pylint: disable=too-many-locals
        const = LFRicConstants()
        for shape in self._kern.eval_shapes:
            fs_name = function_space.orig_name
            ndf_symbol = self._symtab.find_or_create_tag(
                f"ndf_{fs_name}",
                symbol_type=LFRicTypes("NumberOfDofsDataSymbol"),
                fs=fs_name, interface=self._read_access)

            # Create the qr tag by appending the last part of the shape
            # name to "qr_".
            quad_name = shape.split("_")[-1]
            basis_tag = basis_name_func(qr_var="qr_"+quad_name)
            if shape == "gh_quadrature_xyoz":
                nqp_xy = self._symtab.find_or_create_tag(
                    "nqp_xy",
                    symbol_type=LFRicTypes("NumberOfQrPointsInXyDataSymbol"),
                    interface=self._read_access)
                nqp_z = self._symtab.find_or_create_tag(
                    "nqp_z",
                    symbol_type=LFRicTypes("NumberOfQrPointsInZDataSymbol"),
                    interface=self._read_access)
                type_name = mapping["gh_quadrature_xyoz"]
                arg = LFRicTypes(type_name)(
                    basis_tag, [int(first_dim_value_func(function_space)),
                                Reference(ndf_symbol), Reference(nqp_xy),
                                Reference(nqp_z)],
                    fs_name, interface=self._read_access)
            elif shape == "gh_quadrature_face":
                nfaces = self._symtab.find_or_create_tag(
                    "nfaces",
                    symbol_type=LFRicTypes("NumberOfFacesDataSymbol"),
                    interface=self._read_access)
                nqp = self._symtab.find_or_create_tag(
                    "nqp_faces",
                    symbol_type=LFRicTypes(
                        "NumberOfQrPointsInFacesDataSymbol"),
                    interface=self._read_access)
                type_name = mapping["gh_quadrature_face"]
                arg = LFRicTypes(type_name)(
                    basis_tag, [int(first_dim_value_func(function_space)),
                                Reference(ndf_symbol), Reference(nqp),
                                Reference(nfaces)],
                    fs_name, interface=self._read_access)
            elif shape == "gh_quadrature_edge":
                nedges = self._symtab.find_or_create_tag(
                    "nedges",
                    symbol_type=LFRicTypes("NumberOfEdgesDataSymbol"),
                    interface=self._read_access)
                nqp = self._symtab.find_or_create_tag(
                    "nqp_edges",
                    symbol_type=LFRicTypes(
                        "NumberOfQrPointsInEdgesDataSymbol"),
                    interface=self._read_access)
                type_name = mapping["gh_quadrature_edge"]
                arg = LFRicTypes(type_name)(
                    basis_tag, [int(first_dim_value_func(function_space)),
                                Reference(ndf_symbol), Reference(nqp),
                                Reference(nedges)],
                    fs_name, interface=self._read_access)
            elif shape in const.VALID_EVALUATOR_SHAPES:
                # Need a (diff) basis array for each target space upon
                # which the basis functions have been
                # evaluated. _kern.eval_targets is a dict where the
                # values are 2-tuples of (FunctionSpace, argument).
                for _, _ in self._kern.eval_targets.items():
                    raise NotImplementedError(
                        "TODO #928: Evaluator shapes not implemented in "
                        "kernel_interface class.")
            else:
                raise InternalError(
                    f"Unrecognised quadrature or evaluator shape '{shape}'. "
                    f"Expected one of: {const.VALID_EVALUATOR_SHAPES}.")
            self._symtab.add(arg)
            self._arglist.append(arg)
