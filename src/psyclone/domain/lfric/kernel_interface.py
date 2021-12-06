# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council.
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
# Author R. W. Ford STFC Daresbury Lab
# Modified: I. Kavcic, Met Office

'''This module creates the expected arguments for an LFRic coded
kernel based on the kernel metadata.

'''
from __future__ import absolute_import
import six
from psyclone.domain.lfric import ArgOrdering, LFRicConstants
from psyclone.domain.lfric import psyir as lfric_psyir
from psyclone.psyir.symbols import SymbolTable, ArgumentInterface
from psyclone.psyir.nodes import Reference
from psyclone.psyir.frontend.fparser2 import INTENT_MAPPING
from psyclone.errors import InternalError
from psyclone.core import AccessType


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

    TBD: This class should replace the current kernel stub generation
    code when all of its methods are implemented, see issue #928.

    :param kern: the kernel for which to create arguments.
    :type kern: :py:class:`psyclone.dynamo0p3.DynKern`

    '''
    #: Mapping from a generic PSyIR datatype to the equivalent
    #: LFRic-specific field datasymbol.
    field_mapping = {
        "integer": lfric_psyir.IntegerFieldDataDataSymbol,
        "real": lfric_psyir.RealFieldDataDataSymbol,
        "logical": lfric_psyir.LogicalFieldDataDataSymbol}
    #: Mapping from a generic PSyIR datatype to the equivalent
    #: LFRic-specific vector field datasymbol.
    vector_field_mapping = {
        "integer": lfric_psyir.IntegerVectorFieldDataDataSymbol,
        "real": lfric_psyir.RealVectorFieldDataDataSymbol,
        "logical": lfric_psyir.LogicalVectorFieldDataDataSymbol}
    #: Mapping from the LFRic metadata description of quadrature to the
    #: associated LFRic-specific basis function datasymbol.
    basis_mapping = {
        "gh_quadrature_xyoz": lfric_psyir.BasisFunctionQrXyozDataSymbol,
        "gh_quadrature_face": lfric_psyir.BasisFunctionQrFaceDataSymbol,
        "gh_quadrature_edge": lfric_psyir.BasisFunctionQrEdgeDataSymbol}
    #: Mapping from the LFRic metadata description of quadrature to the
    #: associated LFRic-specific differential basis function datasymbol.
    diff_basis_mapping = {
        "gh_quadrature_xyoz": lfric_psyir.DiffBasisFunctionQrXyozDataSymbol,
        "gh_quadrature_face": lfric_psyir.DiffBasisFunctionQrFaceDataSymbol,
        "gh_quadrature_edge": lfric_psyir.DiffBasisFunctionQrEdgeDataSymbol}
    _read_access = ArgumentInterface(ArgumentInterface.Access.READ)

    def __init__(self, kern):
        super(KernelInterface, self).__init__(kern)
        self._symbol_table = SymbolTable()
        self._arglist = []

    def generate(self, var_accesses=None):
        '''Call the generate base class then add the argument list as it can't
        be appended as we go along.

        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        super(KernelInterface, self).generate(var_accesses=var_accesses)
        # Set the argument list for the symbol table. This is done at
        # the end after incrementally adding symbols to the _args
        # list, as it is not possible to incrementally add symbols to
        # the symbol table argument list.
        self._symbol_table.specify_argument_list(self._arglist)
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
        for symbol in self._symbol_table.symbols:
            self.append(symbol.name, var_accesses,
                        mode=mapping[symbol.interface.access])
        self._arglist = self._arglist[:len_arglist]

    def cell_position(self, var_accesses=None):
        '''Create an LFRic cell-position object and add it to the symbol table
        and argument list.

        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        symbol = self._symbol_table.find_or_create_tag(
            "cell", symbol_type=lfric_psyir.CellPositionDataSymbol,
            interface=self._read_access)
        self._arglist.append(symbol)

    def mesh_height(self, var_accesses=None):
        '''Create an LFRic mesh height object and add it to the symbol table
        and argument list.

        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        symbol = self._symbol_table.find_or_create_tag(
            "nlayers", symbol_type=lfric_psyir.MeshHeightDataSymbol,
            interface=self._read_access)
        self._arglist.append(symbol)

    def _mesh_ncell2d(self, var_accesses=None):
        '''Not implemented.

        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises NotImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError("_mesh_ncell2d not implemented")

    def _mesh_ncell2d_no_halos(self, var_accesses=None):
        '''Not implemented.

        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises NotImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError("_mesh_ncell2d_no_halos not implemented")

    def cell_map(self, var_accesses=None):
        '''Not implemented.

        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises NotImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError("cell_map not implemented")

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
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises NotImplementedError: if the datatype of the vector \
            field is not supported.

        '''
        fs_name = argvect.function_space.orig_name
        undf_symbol = self._symbol_table.find_or_create_tag(
            "undf_{0}".format(fs_name), fs=fs_name,
            symbol_type=lfric_psyir.NumberOfUniqueDofsDataSymbol,
            interface=self._read_access)

        interface = ArgumentInterface(INTENT_MAPPING[argvect.intent])
        try:
            field_class = self.vector_field_mapping[
                argvect.intrinsic_type]
        except KeyError as info:
            message = ("kernel interface does not support a vector field of "
                       "type '{0}'.".format(argvect.intrinsic_type))
            six.raise_from(NotImplementedError(message), info)
        for idx in range(argvect.vector_size):
            tag = "{0}_v{1}".format(argvect.name, idx)
            field_data_symbol = self._symbol_table.find_or_create_tag(
                tag, symbol_type=field_class, dims=[Reference(undf_symbol)],
                fs=fs_name, interface=interface)
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
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises NotImplementedError: if the datatype of the field is \
            not supported.

        '''
        fs_name = arg.function_space.orig_name
        undf_symbol = self._symbol_table.find_or_create_tag(
            "undf_{0}".format(fs_name),
            symbol_type=lfric_psyir.NumberOfUniqueDofsDataSymbol,
            fs=fs_name, interface=self._read_access)

        try:
            field_class = self.field_mapping[arg.intrinsic_type]
        except KeyError as info:
            message = ("kernel interface does not support a field of type "
                       "'{0}'.".format(arg.intrinsic_type))
            six.raise_from(NotImplementedError(message), info)
        field_data_symbol = self._symbol_table.find_or_create_tag(
            arg.name, interface=ArgumentInterface(INTENT_MAPPING[arg.intent]),
            symbol_type=field_class, dims=[Reference(undf_symbol)], fs=fs_name)
        self._arglist.append(field_data_symbol)

    def stencil_unknown_extent(self, arg, var_accesses=None):
        '''Not implemented.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises NotImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError("stencil_unknown_extent not implemented")

    def stencil_unknown_direction(self, arg, var_accesses=None):
        '''Not implemented.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises NotImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError("stencil_unknown_direction not implemented")

    def stencil(self, arg, var_accesses=None):
        '''Not implemented.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises NotImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError("stencil not implemented")

    def operator(self, arg, var_accesses=None):
        '''Create an LFRic operator argument and an ncells argument and add
        them to the symbol table and argument list. Also declare the
        associated 'fs_from', 'fs_to' symbols if they have not already
        been declared so that they can be used to dimension the
        operator symbol (as well as ncells).

        :param arg: the operator to add.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises NotImplementedError: if the datatype of the field is \
            not supported.

        '''
        fs_from_name = arg.function_space_from.orig_name
        ndf_symbol_from = self._symbol_table.find_or_create_tag(
            "ndf_{0}".format(fs_from_name), fs=fs_from_name,
            symbol_type=lfric_psyir.NumberOfDofsDataSymbol,
            interface=self._read_access)
        fs_to_name = arg.function_space_to.orig_name
        ndf_symbol_to = self._symbol_table.find_or_create_tag(
            "ndf_{0}".format(fs_to_name), fs=fs_to_name,
            symbol_type=lfric_psyir.NumberOfDofsDataSymbol,
            interface=self._read_access)

        ncells = lfric_psyir.NumberOfCellsDataSymbol(
            "ncell_3d", interface=self._read_access)
        self._symbol_table.add(ncells)
        self._arglist.append(ncells)

        op_arg_symbol = self._symbol_table.find_or_create_tag(
            arg.name, symbol_type=lfric_psyir.OperatorDataSymbol,
            dims=[Reference(ndf_symbol_from), Reference(ndf_symbol_to),
                  Reference(ncells)],
            fs_from=fs_from_name, fs_to=fs_to_name,
            interface=ArgumentInterface(INTENT_MAPPING[arg.intent]))
        self._arglist.append(op_arg_symbol)

    def cma_operator(self, arg, var_accesses=None):
        '''Not implemented.

        :param arg: the CMA operator argument.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises NotImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError("cma_operator not implemented")

    def scalar(self, scalar_arg, var_accesses=None):
        '''Create an LFRic scalar argument and add it to the symbol table and
        argument list.

        :param scalar_arg: the scalar to add.
        :type scalar_arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises NotImplementedError: if the datatype of the scalar is \
            not supported.

        '''
        mapping = {
            "integer": lfric_psyir.LfricIntegerScalarDataSymbol,
            "real": lfric_psyir.LfricRealScalarDataSymbol,
            "logical": lfric_psyir.LfricLogicalScalarDataSymbol}
        try:
            symbol = self._symbol_table.find_or_create_tag(
                scalar_arg.name,
                symbol_type=mapping[scalar_arg.intrinsic_type],
                interface=ArgumentInterface(INTENT_MAPPING[scalar_arg.intent]))
        except KeyError as info:
            message = (
                "scalar of type '{0}' not implemented in KernelInterface "
                "class.".format(scalar_arg.intrinsic_type))
            six.raise_from(NotImplementedError(message), info)
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
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        fs_name = function_space.orig_name
        ndf_symbol = self._symbol_table.find_or_create_tag(
            "ndf_{0}".format(fs_name), fs=fs_name,
            symbol_type=lfric_psyir.NumberOfDofsDataSymbol,
            interface=self._read_access)
        self._arglist.append(ndf_symbol)

    def fs_intergrid(self, function_space, var_accesses=None):
        '''Not implemented.

        :param arg: the CMA operator argument.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises NotImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError("fs_intergrid not implemented")

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
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        fs_name = function_space.orig_name
        undf_symbol = self._symbol_table.find_or_create_tag(
            "undf_{0}".format(fs_name), fs=fs_name,
            symbol_type=lfric_psyir.NumberOfUniqueDofsDataSymbol,
            interface=self._read_access)
        self._arglist.append(undf_symbol)

        fs_name = function_space.orig_name
        ndf_symbol = self._symbol_table.find_or_create_tag(
            "ndf_{0}".format(fs_name), fs=fs_name,
            symbol_type=lfric_psyir.NumberOfDofsDataSymbol,
            interface=self._read_access)

        dofmap_symbol = self._symbol_table.find_or_create_tag(
            "dofmap_{0}".format(fs_name), fs=fs_name,
            symbol_type=lfric_psyir.DofMapDataSymbol,
            dims=[Reference(ndf_symbol)], interface=self._read_access)
        self._arglist.append(dofmap_symbol)

    def banded_dofmap(self, function_space, var_accesses=None):
        '''Not implemented.

        :param function_space: the function space for this dofmap.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises NotImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError("banded_dofmap not implemented")

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
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises NotImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError("indirection_dofmap not implemented")

    def basis(self, function_space, var_accesses=None):
        '''Create an LFRic basis function argument and add it to the symbol
        table and argument list.

        :param function_space: the function space for this basis function.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

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
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        basis_name_func = function_space.get_diff_basis_name
        # This import must be placed here to avoid circular dependencies
        # pylint: disable=import-outside-toplevel
        from psyclone.dynamo0p3 import DynBasisFunctions
        first_dim_value_func = DynBasisFunctions.diff_basis_first_dim_value
        self._create_basis(function_space, self.diff_basis_mapping,
                           basis_name_func, first_dim_value_func)

    def field_bcs_kernel(self, function_space, var_accesses=None):
        '''Not implemented.

        :param function_space: the function space for this boundary condition.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises NotImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError("field_bcs_kernel not implemented")

    def operator_bcs_kernel(self, function_space, var_accesses=None):
        '''Not implemented.

        :param function_space: the function space for this bcs kernel
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises NotImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError("operator_bcs_kernel not implemented")

    def ref_element_properties(self, var_accesses=None):
        ''' Properties associated with the reference element

        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        # This callback does not contribute any kernel arguments

    def mesh_properties(self, var_accesses=None):
        ''' Properties associated with the mesh

        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

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
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises InternalError: if an unsupported quadrature shape is \
            found.

        '''
        # The kernel captures all the required quadrature shapes
        for shape in self._kern.qr_rules:
            if shape == "gh_quadrature_xyoz":
                nqp_xy = self._symbol_table.find_or_create_tag(
                    "nqp_xy",
                    symbol_type=lfric_psyir.NumberOfQrPointsInXyDataSymbol,
                    interface=self._read_access)
                nqp_z = self._symbol_table.find_or_create_tag(
                    "nqp_z",
                    symbol_type=lfric_psyir.NumberOfQrPointsInZDataSymbol,
                    interface=self._read_access)
                weights_xy = self._symbol_table.find_or_create_tag(
                    "weights_xy",
                    symbol_type=lfric_psyir.QrWeightsInXyDataSymbol,
                    dims=[Reference(nqp_xy)], interface=self._read_access)
                weights_z = self._symbol_table.find_or_create_tag(
                    "weights_z",
                    symbol_type=lfric_psyir.QrWeightsInZDataSymbol,
                    dims=[Reference(nqp_z)], interface=self._read_access)
                self._arglist.extend([nqp_xy, nqp_z, weights_xy, weights_z])
            elif shape == "gh_quadrature_face":
                nfaces = self._symbol_table.find_or_create_tag(
                    "nfaces",
                    symbol_type=lfric_psyir.NumberOfFacesDataSymbol,
                    interface=self._read_access)
                nqp = self._symbol_table.find_or_create_tag(
                    "nqp_faces",
                    symbol_type=lfric_psyir.NumberOfQrPointsInFacesDataSymbol,
                    interface=self._read_access)
                weights = self._symbol_table.find_or_create_tag(
                    "weights_faces",
                    symbol_type=lfric_psyir.QrWeightsInFacesDataSymbol,
                    dims=[Reference(nqp)], interface=self._read_access)
                self._arglist.extend([nfaces, nqp, weights])
            elif shape == "gh_quadrature_edge":
                nedges = self._symbol_table.find_or_create_tag(
                    "nedges",
                    symbol_type=lfric_psyir.NumberOfEdgesDataSymbol,
                    interface=self._read_access)
                nqp = self._symbol_table.find_or_create_tag(
                    "nqp_edges",
                    symbol_type=lfric_psyir.NumberOfQrPointsInEdgesDataSymbol,
                    interface=self._read_access)
                weights = self._symbol_table.find_or_create_tag(
                    "weights_edges",
                    symbol_type=lfric_psyir.QrWeightsInEdgesDataSymbol,
                    dims=[Reference(nqp)], interface=self._read_access)
                self._arglist.extend([nedges, nqp, weights])
            else:
                raise InternalError("Unsupported quadrature shape '{0}' "
                                    "found in kernel_interface.".format(shape))

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
            ndf_symbol = self._symbol_table.find_or_create_tag(
                "ndf_{0}".format(fs_name),
                symbol_type=lfric_psyir.NumberOfDofsDataSymbol,
                fs=fs_name, interface=self._read_access)

            # Create the qr tag by appending the last part of the shape
            # name to "qr_".
            quad_name = shape.split("_")[-1]
            basis_tag = basis_name_func(qr_var="qr_"+quad_name)
            if shape == "gh_quadrature_xyoz":
                nqp_xy = self._symbol_table.find_or_create_tag(
                    "nqp_xy",
                    symbol_type=lfric_psyir.NumberOfQrPointsInXyDataSymbol,
                    interface=self._read_access)
                nqp_z = self._symbol_table.find_or_create_tag(
                    "nqp_z",
                    symbol_type=lfric_psyir.NumberOfQrPointsInZDataSymbol,
                    interface=self._read_access)
                arg = mapping["gh_quadrature_xyoz"](
                    basis_tag, [int(first_dim_value_func(function_space)),
                                Reference(ndf_symbol), Reference(nqp_xy),
                                Reference(nqp_z)],
                    fs_name, interface=self._read_access)
            elif shape == "gh_quadrature_face":
                nfaces = self._symbol_table.find_or_create_tag(
                    "nfaces",
                    symbol_type=lfric_psyir.NumberOfFacesDataSymbol,
                    interface=self._read_access)
                nqp = self._symbol_table.find_or_create_tag(
                    "nqp_faces",
                    symbol_type=lfric_psyir.NumberOfQrPointsInFacesDataSymbol,
                    interface=self._read_access)
                arg = mapping["gh_quadrature_face"](
                    basis_tag, [int(first_dim_value_func(function_space)),
                                Reference(ndf_symbol), Reference(nqp),
                                Reference(nfaces)],
                    fs_name, interface=self._read_access)
            elif shape == "gh_quadrature_edge":
                nedges = self._symbol_table.find_or_create_tag(
                    "nedges",
                    symbol_type=lfric_psyir.NumberOfEdgesDataSymbol,
                    interface=self._read_access)
                nqp = self._symbol_table.find_or_create_tag(
                    "nqp_edges",
                    symbol_type=lfric_psyir.NumberOfQrPointsInEdgesDataSymbol,
                    interface=self._read_access)
                arg = mapping["gh_quadrature_edge"](
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
                        "Evaluator shapes not implemented in kernel_interface "
                        "class.")
            else:
                raise InternalError(
                    "Unrecognised quadrature or evaluator shape '{0}'. "
                    "Expected one of: {1}.".format(
                        shape, const.VALID_EVALUATOR_SHAPES))
            self._symbol_table.add(arg)
            self._arglist.append(arg)
