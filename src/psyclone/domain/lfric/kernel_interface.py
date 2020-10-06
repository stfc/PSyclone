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

'''This module creates the expected arguments for an LFRic kernel
based on the kernel metadata.

'''
# pylint: disable=no-name-in-module, too-many-public-methods
# pylint: disable=too-many-instance-attributes, raise-missing-from
# pylint: disable=super-with-arguments, import-outside-toplevel

from psyclone.domain.lfric import ArgOrdering
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

    :param kern: the kernel that requires its kernel arguments to be \
        created.
    :type kern: :py:class:`psyclone.dynamo0p3.DynKern`

    '''
    def __init__(self, kern):
        ArgOrdering.__init__(self, kern)
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
        super(KernelInterface, self).generate()
        # Set the argument list for the symbol table. This is done at
        # the end after incrementally adding symbols to the _arglist
        # list, as it is not possible to add symbols to the symbol
        # table argument list.
        self._symbol_table.specify_argument_list(self._arglist)

    def cell_position(self, var_accesses=None):
        '''Create an LFRic cell position object and add it to the argument
        list.

        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        symbol = self._create_symbol("cell", CellPositionDataSymbol)
        self._arglist.append(symbol)

    def mesh_height(self, var_accesses=None):
        '''Create an LFRic mesh height object and add it to the argument list.

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

        :raises NoImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError("mesh_ncell2d not implemented")

    def cell_map(self, var_accesses=None):
        '''Not implemented.

        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises NoImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError("cell_map not implemented")

    def field_vector(self, argvect, var_accesses=None):
        '''Create LFRic field vector arguments and add them to the argument
        list. Also declare the associated "undf" symbol if it has not
        already been declared so that it can be used to dimension the
        field vector arguments.

        :param argvect: the field vector to add.
        :type argvect: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises NotImplementedError: if the datatype of the vector \
            field is not supported.

        '''
        # Create and add a undf symbol to the symbol table if one does
        # not already exist or return the existing one if one does.
        fs_name = argvect.function_space.orig_name
        undf_symbol = self._create_symbol(
            "undf_{0}".format(fs_name), NumberOfUniqueDofsDataSymbol,
            extra_args=[fs_name])

        mapping = {
            "integer": IntegerVectorFieldDataDataSymbol,
            "real": RealVectorFieldDataDataSymbol,
            "logical": LogicalVectorFieldDataDataSymbol}
        interface = ArgumentInterface(INTENT_MAPPING[argvect.intent])
        try:
            field_class = mapping[argvect.intrinsic_type]
        except KeyError:
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
        '''Create LFRic a field argument and add it to the argument
        list. Also declare the associated "undf" symbol if it has not
        already been declared so that it can be used to dimension the
        field argument.

        :param arg: the field to add.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises NotImplementedError: if the datatype of the field is \
            not supported.

        '''
        # Create and add a undf symbol to the symbol table if one does
        # not already exist or return the existing one if one does.
        fs_name = arg.function_space.orig_name
        undf_symbol = self._create_symbol(
            "undf_{0}".format(fs_name), NumberOfUniqueDofsDataSymbol,
            extra_args=[fs_name])

        mapping = {
            "integer": IntegerFieldDataDataSymbol,
            "real": RealFieldDataDataSymbol,
            "logical": LogicalFieldDataDataSymbol}
        try:
            field_class = mapping[arg.intrinsic_type]
        except KeyError:
            raise NotImplementedError(
                "kernel interface does not support a field of type "
                "'{0}'.".format(arg.intrinsic_type))
        field_data_symbol = self._create_symbol(
            arg.name, field_class, dims=[undf_symbol], extra_args=[fs_name],
            interface=ArgumentInterface(INTENT_MAPPING[arg.intent]))
        self._arglist.append(field_data_symbol)

    def stencil_unknown_extent(self, arg, var_accesses=None):
        '''Not implemented.

        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises NoImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError("stencil_unknown_extent not implemented")

    def stencil_unknown_direction(self, arg, var_accesses=None):
        '''Not implemented.

        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises NoImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError("stencil_unknown_direction not implemented")

    def stencil(self, arg, var_accesses=None):
        '''Not implemented.

        :param var_accesses: an unused optional argument that stores \
            information about variable accesses.
        :type var_accesses: :\
            py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises NoImplementedError: as this method is not implemented.

        '''
        raise NotImplementedError("stencil not implemented")

    def operator(self, arg, var_accesses=None):
        ''' Create LFRic size and operator data objects '''

        # Create and add ndf symbols to the symbol table if they do
        # not already exist or return the existing ones if they do.
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
        ''' Not implemented '''
        raise NotImplementedError("cma_operator not implemented")

    def scalar(self, scalar_arg, var_accesses=None):
        ''' Create LFRic scalar symbol '''
        mapping = {
            "integer": LfricIntegerScalarDataSymbol,
            "real": LfricRealScalarDataSymbol,
            "logical": LfricLogicalScalarDataSymbol}
        try:
            symbol = self._create_symbol(
                scalar_arg.name, mapping[scalar_arg.intrinsic_type],
                interface=ArgumentInterface(INTENT_MAPPING[scalar_arg.intent]))
        except KeyError:
            raise NotImplementedError(
                "scalar of type '{0}' not implemented in KernelInterface "
                "class.".format(scalar_arg.intrinsic_type))
        self._arglist.append(symbol)

    def fs_common(self, function_space, var_accesses=None):
        ''' Create LFRic Number of Dofs object '''

        # Create and add an ndf symbol to the symbol table if one does
        # not already exist or return the existing one if one does.
        fs_name = function_space.orig_name
        ndf_symbol = self._create_symbol(
            "ndf_{0}".format(fs_name), NumberOfDofsDataSymbol,
            extra_args=[fs_name])
        self._arglist.append(ndf_symbol)

    def fs_intergrid(self, function_space, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("fs_integrated not implemented")

    def fs_compulsory_field(self, function_space, var_accesses=None):
        '''Create LFRic Number of Unique dofs and dofmap objects'''

        # create and add a undf symbol to the symbol table if one does
        # not already exist or return the existing one if one does.
        fs_name = function_space.orig_name
        undf_symbol = self._create_symbol(
            "undf_{0}".format(fs_name), NumberOfUniqueDofsDataSymbol,
            extra_args=[fs_name])
        self._arglist.append(undf_symbol)

        # Create and add an ndf symbol to the symbol table if one does
        # not already exist or return the existing one if one does.
        fs_name = function_space.orig_name
        ndf_symbol = self._create_symbol(
            "ndf_{0}".format(fs_name), NumberOfDofsDataSymbol,
            extra_args=[fs_name])
        dofmap_symbol = self._create_symbol(
            "dofmap_{0}".format(fs_name), DofMapDataSymbol, dims=[ndf_symbol],
            extra_args=[fs_name])
        self._arglist.append(dofmap_symbol)

    def banded_dofmap(self, function_space, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("banded_dofmap not implemented")

    def indirection_dofmap(self, function_space, operator=None,
                           var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("indirection_dofmap not implemented")

    def basis(self, function_space, var_accesses=None):
        ''' Implemented '''
        mapping = {
            "gh_quadrature_xyoz": BasisFunctionQrXyozDataSymbol,
            "gh_quadrature_face": BasisFunctionQrFaceDataSymbol,
            "gh_quadrature_edge": BasisFunctionQrEdgeDataSymbol}
        basis_name_func = function_space.get_basis_name
        first_dim_value_func = self._basis_first_dim_value
        self._create_basis(function_space, mapping, basis_name_func,
                           first_dim_value_func)

    def diff_basis(self, function_space, var_accesses=None):
        ''' Implemented '''
        mapping = {
            "gh_quadrature_xyoz": DiffBasisFunctionQrXyozDataSymbol,
            "gh_quadrature_face": DiffBasisFunctionQrFaceDataSymbol,
            "gh_quadrature_edge": DiffBasisFunctionQrEdgeDataSymbol}
        basis_name_func = function_space.get_diff_basis_name
        first_dim_value_func = self._diff_basis_first_dim_value
        self._create_basis(function_space, mapping, basis_name_func,
                           first_dim_value_func)

    def orientation(self, function_space, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("orientation not implemented")

    def field_bcs_kernel(self, function_space, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("field_bcs_kernel not implemented")

    def operator_bcs_kernel(self, function_space, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("operator_bcs_kernel not implemented")

    def ref_element_properties(self, var_accesses=None):
        ''' Properties associated with the reference element '''
        # This callback does not contribute any kernel arguments

    def mesh_properties(self, var_accesses=None):
        ''' Properties associated with the mesh '''
        # This callback does not contribute any kernel arguments

    def quad_rule(self, var_accesses=None):
        ''' Implemented '''
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

    def _create_symbol(self, tag, data_symbol, extra_args=None, dims=None,
                       interface=None):
        '''Internal utility to create a symbol. If a symbol is found in the
        symbol table with the supplied tag then that symbol is
        returned, otherwise a new symbol of type 'data_symbol' is
        created and added to the symbol table with the supplied
        tag. If the symbol requires any arguments then these are
        supplied via the extra_args and dims arguments. The latter
        specifies the dimensions of the symbol if it is an array. By
        default it is assumed that the access to the symbol will be
        read only. If the access is different to this then the
        interface argument must be provided with the appropriate
        access type.

        As this is an internal utility we assume that the argument
        datatypes and content are correct.

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
        ''' xxx '''
        from psyclone.dynamo0p3 import VALID_EVALUATOR_SHAPES
        for shape in self._kern.eval_shapes:
            # Create and add an ndf symbol to the symbol table if one does
            # not already exist or return the existing one if one does.
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
        '''
        Get the size of the first dimension of a basis function.

        :param function_space: the function space the basis function is for
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :return: an integer length.
        :rtype: string

        :raises GenerationError: if an unsupported function space is supplied \
                                 (e.g. ANY_SPACE_*, ANY_DISCONTINUOUS_SPACE_*)
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
        '''
        Get the size of the first dimension of an array for a
        differential basis function.

        :param function_space: the function space the diff-basis function \
                               is for.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :return: an integer length.
        :rtype: str

        :raises GenerationError: if an unsupported function space is \
                                 supplied (e.g. ANY_SPACE_*, \
                                 ANY_DISCONTINUOUS_SPACE_*)

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
