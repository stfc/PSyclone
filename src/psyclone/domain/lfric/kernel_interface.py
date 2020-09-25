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

'''This module creates information about the expected arguments for a
generated LFRic kernel.

'''
# pylint: disable=no-name-in-module, too-many-public-methods
# pylint: disable=too-many-instance-attributes, raise-missing-from
# pylint: disable=super-with-arguments, import-outside-toplevel

from psyclone.domain.lfric import ArgOrdering
from psyclone.domain.lfric.psyir import CellPositionDataSymbol, \
    MeshHeightDataSymbol, OperatorSizeDataSymbol, NumberOfDofsDataSymbol, \
    NumberOfUniqueDofsDataSymbol, DofMapDataSymbol, RealFieldDataDataSymbol, \
    IntegerFieldDataDataSymbol, LogicalFieldDataDataSymbol, \
    RealVectorFieldDataDataSymbol, IntegerVectorFieldDataDataSymbol, \
    LogicalVectorFieldDataDataSymbol, OperatorDataSymbol, \
    LfricIntegerScalarDataSymbol, LfricRealScalarDataSymbol, \
    LfricLogicalScalarDataSymbol, BasisFunctionDataSymbol, \
    DiffBasisFunctionDataSymbol, NumberOfQrPointsInHorizontalDataSymbol, \
    NumberOfQrPointsInVerticalDataSymbol, NumberOfQrPointsDataSymbol, \
    QrWeightsInHorizontalDataSymbol, QrWeightsInVerticalDataSymbol, \
    NumberOfFacesDataSymbol, QrWeightsDataSymbol, NumberOfEdgesDataSymbol
from psyclone.psyir.symbols import SymbolTable, ArgumentInterface
from psyclone.psyir.frontend.fparser2 import INTENT_MAPPING
from psyclone.errors import InternalError


class KernelInterface(ArgOrdering):
    '''Create a kernel interface as specified by kernel metadata and the
    LFRic kernel rules.

    LFRic PSyIR symbols are used to specify the arguments. This class
    is responsible for 1) creating the required symbols, 2) creating
    the symbol argument order and 3) connecting related arguments. For
    example, the dimension of a field array is passed as a separate
    argument. Therefore the field argument should have a reference to
    the appropriate dimension size argument.

    TBD: This should replace the current kernel stub gen
    implementation when , see issue #XXX

    '''
    def __init__(self, kern):
        ArgOrdering.__init__(self, kern)
        self._read_access = ArgumentInterface(ArgumentInterface.Access.READ)
        self._symbol_table = SymbolTable()
        self._arglist = []

    def generate(self, var_accesses=None):
        '''Call the generate base class then add the argument list as it can't
        be appended as we go along.

        '''
        super(KernelInterface, self).generate()
        # Set the argument list for the symbol table
        self._symbol_table.specify_argument_list(self._arglist)

    def _create_undf_symbol(self, function_space):
        ''' xxx '''
        fs_name = function_space.orig_name
        undf_tag = ("undf_{0}".format(fs_name))
        try:
            undf_symbol = self._symbol_table.lookup_with_tag(undf_tag)
        except KeyError:
            undf_name = self._symbol_table.new_symbol_name(undf_tag)
            undf_symbol = NumberOfUniqueDofsDataSymbol(
                undf_name, fs_name, interface=self._read_access)
            self._symbol_table.add(undf_symbol, tag=undf_tag)
        return undf_symbol

    def _create_ndf_symbol(self, function_space):
        ''' xxx '''
        fs_name = function_space.orig_name
        ndf_tag = ("ndf_{0}".format(fs_name))
        try:
            ndf_symbol = self._symbol_table.lookup_with_tag(ndf_tag)
        except KeyError:
            ndf_name = self._symbol_table.new_symbol_name(ndf_tag)
            ndf_symbol = NumberOfDofsDataSymbol(
                ndf_name, fs_name, interface=self._read_access)
            self._symbol_table.add(ndf_symbol, tag=ndf_tag)
        return ndf_symbol

    def _create_symbol_fs(self, tag, data_symbol, function_space, interface=None):
        ''' xxx '''
        fs_name = function_space.orig_name
        try:
            symbol = self._symbol_table.lookup_with_tag(tag)
        except KeyError:
            if interface is None:
                interface = self._read_access
            name = self._symbol_table.new_symbol_name(tag)
            symbol = data_symbol(
                name, fs_name, interface=interface)
            self._symbol_table.add(ndf_symbol, tag=tag)
        return symbol

    def _create_symbol(self, tag, data_symbol, dims=None, interface=None):
        ''' xxx '''
        try:
            symbol = self._symbol_table.lookup_with_tag(tag)
        except KeyError:
            if interface is None:
                interface = self._read_access
            name = self._symbol_table.new_symbol_name(tag)
            if dims:
                symbol = data_symbol(name, dims, interface=interface)
            else:
                symbol = data_symbol(name, interface=interface)
            self._symbol_table.add(symbol, tag=tag)
            self._arglist.append(symbol)
        return symbol

    def cell_position(self, var_accesses=None):
        ''' Create an LFRic cell position object '''
        self._create_symbol("cell", CellPositionDataSymbol)

    def mesh_height(self, var_accesses=None):
        ''' Create an LFRic mesh height object '''
        self._create_symbol("nlayers", MeshHeightDataSymbol)

    def mesh_ncell2d(self, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("mesh_ncell2d not implemented")

    def cell_map(self, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("cell_map not implemented")

    def field_vector(self, argvect, var_accesses=None):
        ''' Implemented '''
        mapping = {
            "integer": IntegerVectorFieldDataDataSymbol,
            "real": RealVectorFieldDataDataSymbol,
            "logical": LogicalVectorFieldDataDataSymbol}
        # Create and add a undf symbol to the symbol table if one does
        # not already exist or return the existing one if one does.
        undf_symbol = self._create_undf_symbol(argvect.function_space)
        for idx in range(argvect.vector_size):
            tag = "{0}_v{1}".format(argvect.name, idx)
            name = self._symbol_table.new_symbol_name(tag)
            try:
                field_data = mapping[argvect.intrinsic_type](
                    name, [undf_symbol],
                    argvect.function_space.orig_name,
                    interface=ArgumentInterface(
                        INTENT_MAPPING[argvect.intent]))
            except KeyError:
                raise NotImplementedError(
                    "vector field of type {0} not implemented"
                    "".format(argvect.intrinsic_type))
            self._symbol_table.add(field_data, tag=tag)
            self._arglist.append(field_data)

    def field(self, arg, var_accesses=None):
        ''' Create an LFRic field data object '''
        mapping = {
            "integer": IntegerFieldDataDataSymbol,
            "real": RealFieldDataDataSymbol,
            "logical": LogicalFieldDataDataSymbol}
        # Create and add a undf symbol to the symbol table if one does
        # not already exist or return the existing one if one does.
        undf_symbol = self._create_undf_symbol(arg.function_space)
        try:
            field_data = mapping[arg.intrinsic_type](
                arg.name, [undf_symbol], arg.function_space.orig_name,
                interface=ArgumentInterface(INTENT_MAPPING[arg.intent]))
        except KeyError:
            raise NotImplementedError(
                "field of type {0} not implemented"
                "".format(arg.intrinsic_type))
        self._symbol_table.add(field_data)
        self._arglist.append(field_data)

    def stencil_unknown_extent(self, arg, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("stencil_unknown_extent not implemented")

    def stencil_unknown_direction(self, arg, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("stencil_unknown_direction not implemented")

    def stencil(self, arg, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("stencil not implemented")

    def operator(self, arg, var_accesses=None):
        ''' Create LFRic size and operator data objects '''

        # Create and add ndf symbols to the symbol table if they do
        # not already exist or return the existing ones if they do.
        ndf_symbol_from = self._create_ndf_symbol(arg.function_space_from)
        ndf_symbol_to = self._create_ndf_symbol(arg.function_space_to)

        op_size = OperatorSizeDataSymbol(
            "ncell_3d", interface=self._read_access)
        self._symbol_table.add(op_size)
        self._arglist.append(op_size)
        op_arg = OperatorDataSymbol(
            arg.name, [ndf_symbol_from, ndf_symbol_to, op_size],
            arg.function_space_from.orig_name,
            arg.function_space_to.orig_name,
            interface=ArgumentInterface(INTENT_MAPPING[arg.intent]))
        self._symbol_table.add(op_arg)
        self._arglist.append(op_arg)

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
            arg = mapping[scalar_arg.intrinsic_type](
                scalar_arg.name, interface=ArgumentInterface(
                    INTENT_MAPPING[scalar_arg.intent]))
        except KeyError:
            raise NotImplementedError(
                "scalar of type {0} not implemented"
                "".format(scalar_arg.intrinsic_type))
        self._symbol_table.add(arg)
        self._arglist.append(arg)

    def fs_common(self, function_space, var_accesses=None):
        ''' Create LFRic Number of Dofs object '''

        # Create and add an ndf symbol to the symbol table if one does
        # not already exist or return the existing one if one does.
        ndf_symbol = self._create_ndf_symbol(function_space)
        self._arglist.append(ndf_symbol)

    def fs_intergrid(self, function_space, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("fs_integrated not implemented")

    def fs_compulsory_field(self, function_space, var_accesses=None):
        '''Create LFRic Number of Unique dofs and dofmap objects'''

        # create and add a undf symbol to the symbol table if one does
        # not already exist or return the existing one if one does.
        undf_symbol = self._create_undf_symbol(function_space)
        self._arglist.append(undf_symbol)

        # Create and add an ndf symbol to the symbol table if one does
        # not already exist or return the existing one if one does.
        ndf_symbol = self._create_ndf_symbol(function_space)
        fs_name = function_space.orig_name
        arg = DofMapDataSymbol("dofmap_{0}".format(fs_name), [ndf_symbol],
                               fs_name, interface=self._read_access)
        self._symbol_table.add(arg)
        self._arglist.append(arg)

    def banded_dofmap(self, function_space, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("banded_dofmap not implemented")

    def indirection_dofmap(self, function_space, operator=None,
                           var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("indirection_dofmap not implemented")

    def basis(self, function_space, var_accesses=None):
        ''' Implemented '''
        from psyclone.dynamo0p3 import VALID_EVALUATOR_SHAPES, \
            VALID_QUADRATURE_SHAPES
        for shape in self._kern.eval_shapes:
            if shape in VALID_QUADRATURE_SHAPES:
                # A kernel stub won't have a name for the corresponding
                # quadrature argument so we create one by appending the last
                # part of the shape name to "qr_".
                quad_name = shape.split("_")[-1]
                basis_name = function_space.get_basis_name(
                    qr_var="qr_"+quad_name)
                fs_name = function_space.orig_name
                arg = BasisFunctionDataSymbol(
                    basis_name, [1, 1, 1], fs_name, quad_name,
                    interface=self._read_access)
                self._symbol_table.add(arg)
                self._arglist.append(arg)
            elif shape in VALID_EVALUATOR_SHAPES:
                # Need a basis array for each target space upon which the basis
                # functions have been evaluated. _kern.eval_targets is a dict
                # where the values are 2-tuples of (FunctionSpace, argument).
                for _, _ in self._kern.eval_targets.items():
                    raise NotImplementedError(
                        "evaluator shapes not implemented")
            else:
                raise InternalError(
                    "Unrecognised evaluator shape ('{0}'). Expected one of: "
                    "{1}".format(shape, VALID_EVALUATOR_SHAPES))

    def diff_basis(self, function_space, var_accesses=None):
        ''' Implemented '''
        from psyclone.dynamo0p3 import VALID_EVALUATOR_SHAPES, \
            VALID_QUADRATURE_SHAPES
        for shape in self._kern.eval_shapes:
            if shape in VALID_QUADRATURE_SHAPES:
                # We need differential basis functions for quadrature. A
                # kernel stub won't have a name for the corresponding
                # quadrature argument so we create one by appending the
                # last part of the shape name to "qr_".
                quad_name = shape.split("_")[-1]
                diff_basis_name = function_space.get_diff_basis_name(
                    qr_var="qr_"+quad_name)
                fs_name = function_space.orig_name
                arg = DiffBasisFunctionDataSymbol(
                    diff_basis_name, [1, 1, 1], fs_name, quad_name,
                    interface=self._read_access)
                self._symbol_table.add(arg)
                self._arglist.append(arg)

            elif shape in VALID_EVALUATOR_SHAPES:
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
                                        shape, VALID_EVALUATOR_SHAPES))

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
                _ = self._create_symbol(
                    "weights_h", QrWeightsInHorizontalDataSymbol, dims=[nqp_h])
                _ = self._create_symbol(
                    "weights_v", QrWeightsInVerticalDataSymbol, dims=[nqp_v])
            elif shape == "gh_quadrature_face":
                _ = self._create_symbol("nfaces", NumberOfFacesDataSymbol)
                nqp = self._create_symbol("nqp", NumberOfQrPointsDataSymbol)
                _ = self._create_symbol(
                    "weights", QrWeightsDataSymbol, dims=[nqp])
            elif shape == "gh_quadrature_edge":
                _ = self._create_symbol("nfaces", NumberOfEdgesDataSymbol)
                nqp = self._create_symbol("nqp", NumberOfQrPointsDataSymbol)
                _ = self._create_symbol(
                    "weights", QrWeightsDataSymbol, dims=[nqp])
            else:
                raise InternalError("Unsupported quadrature shape ('{0}') "
                                    "found in kernel_interface".format(shape))
