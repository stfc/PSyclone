# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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

'''This module creates the expected arguments for an LFRic coded
kernel based on the kernel metadata.

'''
from psyclone.domain.lfric import ArgOrdering
# pylint: disable=no-name-in-module
from psyclone.domain.lfric.psyir import CellPositionDataSymbol, \
    MeshHeightDataSymbol, NumberOfCellsDataSymbol, NumberOfDofsDataSymbol, \
    NumberOfUniqueDofsDataSymbol, DofMapDataSymbol, RealFieldDataDataSymbol, \
    IntegerFieldDataDataSymbol, LogicalFieldDataDataSymbol, \
    RealVectorFieldDataDataSymbol, IntegerVectorFieldDataDataSymbol, \
    LogicalVectorFieldDataDataSymbol, OperatorDataSymbol, \
    LfricIntegerScalarDataSymbol, LfricRealScalarDataSymbol, \
    LfricLogicalScalarDataSymbol, BasisFunctionQrXyozDataSymbol, \
    BasisFunctionQrFaceDataSymbol, BasisFunctionQrEdgeDataSymbol, \
    DiffBasisFunctionQrXyozDataSymbol, DiffBasisFunctionQrFaceDataSymbol, \
    DiffBasisFunctionQrEdgeDataSymbol, \
    NumberOfQrPointsInHorizontalDataSymbol, \
    NumberOfQrPointsInVerticalDataSymbol, NumberOfQrPointsDataSymbol, \
    QrWeightsInHorizontalDataSymbol, QrWeightsInVerticalDataSymbol, \
    NumberOfFacesDataSymbol, QrWeightsDataSymbol, NumberOfEdgesDataSymbol
from psyclone.psyir.symbols import SymbolTable, ArgumentInterface
from psyclone.psyir.frontend.fparser2 import INTENT_MAPPING
from psyclone.errors import InternalError, GenerationError
from psyclone.domain.lfric import FunctionSpace


# pylint: disable=too-many-public-methods
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
    field_mapping = {
        "integer": IntegerFieldDataDataSymbol,
        "real": RealFieldDataDataSymbol,
        "logical": LogicalFieldDataDataSymbol}
    vector_field_mapping = {
        "integer": IntegerVectorFieldDataDataSymbol,
        "real": RealVectorFieldDataDataSymbol,
        "logical": LogicalVectorFieldDataDataSymbol}
    basis_mapping = {
        "gh_quadrature_xyoz": BasisFunctionQrXyozDataSymbol,
        "gh_quadrature_face": BasisFunctionQrFaceDataSymbol,
        "gh_quadrature_edge": BasisFunctionQrEdgeDataSymbol}
    diff_basis_mapping = {
        "gh_quadrature_xyoz": DiffBasisFunctionQrXyozDataSymbol,
        "gh_quadrature_face": DiffBasisFunctionQrFaceDataSymbol,
        "gh_quadrature_edge": DiffBasisFunctionQrEdgeDataSymbol}

    def __init__(self, kern):
        # pylint: disable=super-with-arguments
        super(KernelInterface, self).__init__(kern)
        self._read_access = ArgumentInterface(ArgumentInterface.Access.READ)
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
        # pylint: disable=super-with-arguments
        super(KernelInterface, self).generate()
        # Set the argument list for the symbol table. This is done at
        # the end after incrementally adding symbols to the _arglist
        # list, as it is not possible to incrementally add symbols to
        # the symbol table argument list.
        self._symbol_table.specify_argument_list(self._arglist)

    def cell_position(self, var_accesses=None):
        '''Create an LFRic cell-position object and add it to the symbol table
        and argument list.

        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        symbol = self._create_symbol("cell", CellPositionDataSymbol)
        self._arglist.append(symbol)

    def mesh_height(self, var_accesses=None):
        '''Create an LFRic mesh height object and add it to the symbol table
        and argument list.

        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        symbol = self._create_symbol("nlayers", MeshHeightDataSymbol)
        self._arglist.append(symbol)

    def mesh_ncell2d(self, var_accesses=None):
        '''Not implemented.

        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises NotImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError("mesh_ncell2d not implemented")

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
        # self._create_symbol(...) will create and add a undf symbol
        # to the symbol table if one does not already exist or return
        # the existing one if one does.
        fs_name = argvect.function_space.orig_name
        undf_symbol = self._create_symbol(
            "undf_{0}".format(fs_name), NumberOfUniqueDofsDataSymbol,
            extra_args=[fs_name])

        interface = ArgumentInterface(INTENT_MAPPING[argvect.intent])
        try:
            field_class = KernelInterface.vector_field_mapping[
                argvect.intrinsic_type]
        except KeyError:
            # pylint: disable=raise-missing-from
            raise NotImplementedError(
                "kernel interface does not support a vector field of type "
                "'{0}'.".format(argvect.intrinsic_type))
        for idx in range(argvect.vector_size):
            tag = "{0}_v{1}".format(argvect.name, idx)
            field_data_symbol = self._create_symbol(
                tag, field_class, dims=[undf_symbol], extra_args=[fs_name],
                interface=interface)
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
        # self._create_symbol(...) will create and add a undf symbol
        # to the symbol table if one does not already exist or return
        # the existing one if one does.
        fs_name = arg.function_space.orig_name
        undf_symbol = self._create_symbol(
            "undf_{0}".format(fs_name), NumberOfUniqueDofsDataSymbol,
            extra_args=[fs_name])

        try:
            field_class = KernelInterface.field_mapping[arg.intrinsic_type]
        except KeyError:
            # pylint: disable=raise-missing-from
            raise NotImplementedError(
                "kernel interface does not support a field of type "
                "'{0}'.".format(arg.intrinsic_type))
        field_data_symbol = self._create_symbol(
            arg.name, field_class, dims=[undf_symbol], extra_args=[fs_name],
            interface=ArgumentInterface(INTENT_MAPPING[arg.intent]))
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
        # The self._create_symbol(...) calls will create and add ndf
        # symbols to the symbol table if they do not already exist or
        # return the existing ones if they do.
        fs_from_name = arg.function_space_from.orig_name
        ndf_symbol_from = self._create_symbol(
            "ndf_{0}".format(fs_from_name), NumberOfDofsDataSymbol,
            extra_args=[fs_from_name])
        fs_to_name = arg.function_space_to.orig_name
        ndf_symbol_to = self._create_symbol(
            "ndf_{0}".format(fs_to_name), NumberOfDofsDataSymbol,
            extra_args=[fs_to_name])

        ncells = NumberOfCellsDataSymbol(
            "ncell_3d", interface=self._read_access)
        self._symbol_table.add(ncells)
        self._arglist.append(ncells)

        op_arg_symbol = self._create_symbol(
            arg.name, OperatorDataSymbol,
            dims=[ndf_symbol_from, ndf_symbol_to, ncells],
            extra_args=[fs_from_name, fs_to_name],
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
            "integer": LfricIntegerScalarDataSymbol,
            "real": LfricRealScalarDataSymbol,
            "logical": LfricLogicalScalarDataSymbol}
        try:
            symbol = self._create_symbol(
                scalar_arg.name, mapping[scalar_arg.intrinsic_type],
                interface=ArgumentInterface(INTENT_MAPPING[scalar_arg.intent]))
        except KeyError:
            # pylint: disable=raise-missing-from
            raise NotImplementedError(
                "scalar of type '{0}' not implemented in KernelInterface "
                "class.".format(scalar_arg.intrinsic_type))
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
        ndf_symbol = self._create_symbol(
            "ndf_{0}".format(fs_name), NumberOfDofsDataSymbol,
            extra_args=[fs_name])
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
        undf_symbol = self._create_symbol(
            "undf_{0}".format(fs_name), NumberOfUniqueDofsDataSymbol,
            extra_args=[fs_name])
        self._arglist.append(undf_symbol)

        fs_name = function_space.orig_name
        ndf_symbol = self._create_symbol(
            "ndf_{0}".format(fs_name), NumberOfDofsDataSymbol,
            extra_args=[fs_name])

        dofmap_symbol = self._create_symbol(
            "dofmap_{0}".format(fs_name), DofMapDataSymbol, dims=[ndf_symbol],
            extra_args=[fs_name])
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
        first_dim_value_func = self._basis_first_dim_value
        self._create_basis(function_space, KernelInterface.basis_mapping,
                           basis_name_func, first_dim_value_func)

    def diff_basis(self, function_space, var_accesses=None):
        '''Create an LFRic differential basis function argument and add it to
        the symbol table and argument list.

        :param function_space: the function space for this basis function.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        basis_name_func = function_space.get_diff_basis_name
        first_dim_value_func = self._diff_basis_first_dim_value
        self._create_basis(function_space, KernelInterface.diff_basis_mapping,
                           basis_name_func, first_dim_value_func)

    def orientation(self, function_space, var_accesses=None):
        '''Not implemented.

        :param function_space: the function space for orientation.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises NotImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError("orientation not implemented")

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
                nqp_h = self._create_symbol(
                    "nqp_h", NumberOfQrPointsInHorizontalDataSymbol)
                nqp_v = self._create_symbol(
                    "nqp_v", NumberOfQrPointsInVerticalDataSymbol)
                weights_h = self._create_symbol(
                    "weights_h", QrWeightsInHorizontalDataSymbol, dims=[nqp_h])
                weights_v = self._create_symbol(
                    "weights_v", QrWeightsInVerticalDataSymbol, dims=[nqp_v])
                self._arglist.extend([nqp_h, nqp_v, weights_h, weights_v])
            elif shape == "gh_quadrature_face":
                nfaces = self._create_symbol("nfaces", NumberOfFacesDataSymbol)
                nqp = self._create_symbol("nqp", NumberOfQrPointsDataSymbol)
                weights = self._create_symbol(
                    "weights", QrWeightsDataSymbol, dims=[nqp])
                self._arglist.extend([nfaces, nqp, weights])
            elif shape == "gh_quadrature_edge":
                nedges = self._create_symbol("nedges", NumberOfEdgesDataSymbol)
                nqp = self._create_symbol("nqp", NumberOfQrPointsDataSymbol)
                weights = self._create_symbol(
                    "weights", QrWeightsDataSymbol, dims=[nqp])
                self._arglist.extend([nedges, nqp, weights])
            else:
                raise InternalError("Unsupported quadrature shape '{0}' "
                                    "found in kernel_interface.".format(shape))

    # pylint: disable=too-many-arguments
    def _create_symbol(self, tag, data_symbol, extra_args=None, dims=None,
                       interface=None):
        '''Internal utility to create a symbol. If a symbol is found in the
        symbol table with the supplied tag then that symbol is
        returned, otherwise a new symbol of type 'data_symbol' is
        created and added to the symbol table with the supplied
        tag. If the symbol requires any arguments then these are
        supplied via the extra_args and dims arguments. The latter
        also specifies the dimensions of the symbol if it is an array. By
        default it is assumed that the access to the symbol will be
        read only; if the access is different to this then the
        interface argument must be provided with the appropriate
        access type.

        As this is an internal utility, we assume that the argument
        datatypes and content are correct.

        :param str tag: the name to use as a tag in the symbol table \
            and also as the base name when creating a new symbol table \
            name.
        :param data_symbol: the symbol class that we are going to create.
        :type data_symbol: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param extra_args: an optional list of strings specifying the \
            values of any additional arguments to provide to the \
            data_symbol class on creation or None if there are \
            none. Defaults to None.
        :type extra_args: list of str or NoneType
        :param dims: an optional list of dimensions used to dimension \
            the data_symbol class if it is an array, or None if it is \
            not an array. Defaults to None.
        :type dims: list of DataSymbol, int or ArrayType.Extent or NoneType
        :param interface: an optional ArgumentInterface specifying the \
            intent of the symbol, or None if the default it \
            suitable. Defaults to read access.
        :type interface: \
            :py:class:`psyclone.psyir.symbols.ArgumentInterface` or \
            NoneType

        :returns: the appropriate symbol and properties as specified \
            by this methods arguments.
        :type: :py:class:`psyclone.psyir.symbols.DataSymbol`

        '''
        try:
            symbol = self._symbol_table.lookup_with_tag(tag)
            if not isinstance(symbol, data_symbol):
                raise InternalError(
                    "Expected symbol with tag '{0}' to be of type '{1}' but "
                    "found type '{2}'.".format(
                        tag, data_symbol.__name__, type(symbol).__name__))
        except KeyError:
            if interface is None:
                interface = self._read_access
            name = self._symbol_table.new_symbol_name(tag)
            args = [name]
            if dims:
                args.append(dims)
            if extra_args:
                args.extend(extra_args)
            symbol = data_symbol(*args, interface=interface)
            self._symbol_table.add(symbol, tag=tag)
        return symbol

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
        from psyclone.dynamo0p3 import VALID_EVALUATOR_SHAPES
        for shape in self._kern.eval_shapes:
            # self._create_symbol(...) will create and add an ndf
            # symbol to the symbol table if one does not already exist
            # or return the existing one if one does.
            fs_name = function_space.orig_name
            ndf_symbol = self._create_symbol(
                "ndf_{0}".format(fs_name), NumberOfDofsDataSymbol,
                extra_args=[fs_name])

            # Create the qr tag by appending the last part of the shape
            # name to "qr_".
            quad_name = shape.split("_")[-1]
            basis_tag = basis_name_func(qr_var="qr_"+quad_name)
            if shape == "gh_quadrature_xyoz":
                nqp_h = self._create_symbol(
                    "nqp_h", NumberOfQrPointsInHorizontalDataSymbol)
                nqp_v = self._create_symbol(
                    "nqp_v", NumberOfQrPointsInVerticalDataSymbol)
                arg = mapping["gh_quadrature_xyoz"](
                    basis_tag, [int(first_dim_value_func(function_space)),
                                ndf_symbol, nqp_h, nqp_v],
                    fs_name, interface=self._read_access)
            elif shape == "gh_quadrature_face":
                nfaces = self._create_symbol("nfaces", NumberOfFacesDataSymbol)
                nqp = self._create_symbol("nqp", NumberOfQrPointsDataSymbol)
                arg = mapping["gh_quadrature_face"](
                    basis_tag, [int(first_dim_value_func(function_space)),
                                ndf_symbol, nqp, nfaces],
                    fs_name, interface=self._read_access)
            elif shape == "gh_quadrature_edge":
                nedges = self._create_symbol("nedges", NumberOfEdgesDataSymbol)
                nqp = self._create_symbol("nqp", NumberOfQrPointsDataSymbol)
                arg = mapping["gh_quadrature_edge"](
                    basis_tag, [int(first_dim_value_func(function_space)),
                                ndf_symbol, nqp, nedges],
                    fs_name, interface=self._read_access)
            elif shape in VALID_EVALUATOR_SHAPES:
                # Need a diff basis array for each target space upon
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
                        shape, VALID_EVALUATOR_SHAPES))
            self._symbol_table.add(arg)
            self._arglist.append(arg)

    @staticmethod
    def _basis_first_dim_value(function_space):
        '''Internal utility to return the size of the first dimension of a
        basis function.

        :param function_space: the function space the basis function is on.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`

        :returns: an integer size as a string.
        :rtype: str

        :raises GenerationError: if an unsupported function space is \
            supplied (e.g. ANY_SPACE_*, ANY_DISCONTINUOUS_SPACE_*)

        '''
        if function_space.has_scalar_basis:
            first_dim = "1"
        elif function_space.has_vector_basis:
            first_dim = "3"
        else:
            # It is not possible to determine explicitly the first basis
            # function array dimension from the metadata for any_space or
            # any_discontinuous_space. This information needs to be passed
            # from the PSy layer to the kernels (see issue #461).
            raise GenerationError(
                "Unsupported space for basis function, "
                "expecting one of {0} but found "
                "'{1}'".format(FunctionSpace.VALID_FUNCTION_SPACES,
                               function_space.orig_name))
        return first_dim

    @staticmethod
    def _diff_basis_first_dim_value(function_space):
        '''Internal utility to return the size of the first dimension of a
        differential basis function.

        :param function_space: the function space the differential \
            basis function is on.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`

        :returns: an integer length as a string.
        :rtype: str

        :raises GenerationError: if an unsupported function space is \
            supplied (e.g. ANY_SPACE_*, ANY_DISCONTINUOUS_SPACE_*)

        '''
        if function_space.has_scalar_diff_basis:
            first_dim = "1"
        elif function_space.has_vector_diff_basis:
            first_dim = "3"
        else:
            # It is not possible to determine explicitly the first
            # differential basis function array dimension from the metadata
            # for any_space or any_discontinuous_space. This information
            # needs to be passed from the PSy layer to the kernels
            # (see issue #461).
            raise GenerationError(
                "Unsupported space for differential basis function, expecting "
                "one of {0} but found '{1}'"
                .format(FunctionSpace.VALID_FUNCTION_SPACES,
                        function_space.orig_name))
        return first_dim
