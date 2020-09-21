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

# pylint: disable=no-name-in-module
# pylint: disable=too-many-public-methods
# pylint: disable=too-many-instance-attributes

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
    NumberOfQrPointsInVerticalDataSymbol, QrWeightsInHorizontalDataSymbol, \
    QrWeightsInVerticalDataSymbol, NumberOfFacesDataSymbol, \
    NumberOfFacesDataSymbol, QrWeightsDataSymbol, NumberOfEdgesDataSymbol
from psyclone.psyir.symbols import SymbolTable, ArgumentInterface
from psyclone.psyir.frontend.fparser2 import INTENT_MAPPING

class KernelInterface(ArgOrdering):
    '''Create the list of actual argument types that a kernel has.

    Also add function space information where appropriate (e.g. the
    number of degrees of freedom of a cell (ndf) will be associated
    with a function space) and associate arguments with each other
    where appropriate (e.g. a particular dofmap will be associated
    with a particular ndf dimension argument.

    RF TBD: This should replace the current Kernel stub gen
    implementation at some point.

    '''
    def __init__(self, kern):
        ArgOrdering.__init__(self, kern)
        self._symbol_table = SymbolTable()
        self._ndofs_map = {}
        self._undfs_map = {}
        self._operator_dims_map = {}
        self._arglist = []
        self._dofmaps_list = []
        self._fields_list = []
        self._operators_list = []

    def generate(self, var_accesses=None):
        '''Call the generate base class then call the connect method after it
        has completed.'''
        super(KernelInterface, self).generate()
        self.connect()

    def connect(self):
        '''Add the symbol table argument list and connect the interface
        arguments together. This is performed at the end once all
        interface arguments have been created.

        Here we must know the number of dimensions that a array object
        has but not the order of the dimensions.

        '''
        # Set the argument list for the symbol table
        self._symbol_table.specify_argument_list(self._arglist)

        # dofmaps are dimensioned by ndofs
        for dofmap in self._dofmaps_list:
            function_space = dofmap.fs
            ndofs = self._ndofs_map[function_space]
            dofmap.datatype._shape = [ndofs]

        # fields are dimensioned by undf
        for field in self._fields_list:
            function_space = field.fs
            undf = self._undfs_map[function_space]
            field.datatype._shape = [undf]

        # operators have 3 dimensions, 2 of which are ndf and one of
        # which is ncell_3d
        for operator in self._operators_list:
            fs_from = operator.fs_from
            fs_to = operator.fs_to
            ndf_from = self._ndofs_map[fs_from]
            ndf_to = self._ndofs_map[fs_to]
            op_dim = self._operator_dims_map[operator]
            # RF hard code array ordering until we work out how to
            # encode it (perhaps store it within the class)?
            dims = [ndf_from, ndf_to, op_dim]
            operator.datatype._shape = dims

    def cell_position(self, var_accesses=None):
        ''' Create an LFRic cell position object '''

        arg = CellPositionDataSymbol(
            "cell", interface=ArgumentInterface(ArgumentInterface.Access.READ))
        self._symbol_table.add(arg)
        self._arglist.append(arg)

    def mesh_height(self, var_accesses=None):
        ''' Create an LFRic mesh height object '''

        arg = MeshHeightDataSymbol(
            "nlayers",
            interface=ArgumentInterface(ArgumentInterface.Access.READ))
        self._symbol_table.add(arg)
        self._arglist.append(arg)

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
        for idx in range(argvect.vector_size):
            dim = idx + 1
            try:
                field_data = mapping[argvect.intrinsic_type](
                    "{0}_v{1}".format(argvect.name, idx), [1],
                    argvect.function_space.orig_name,
                    interface=ArgumentInterface(
                        INTENT_MAPPING[argvect.intent]))
            except KeyError:
                raise NotImplementedError(
                    "vector field of type {0} not implemented"
                    "".format(argvect.intrinsic_type))

            self._symbol_table.add(field_data)
            self._arglist.append(field_data)
            self._fields_list.append(field_data)

    def field(self, arg, var_accesses=None):
        ''' Create an LFRic field data object '''
        mapping = {
            "integer": IntegerFieldDataDataSymbol,
            "real": RealFieldDataDataSymbol,
            "logical": LogicalFieldDataDataSymbol}
        try:
            field_data = mapping[arg.intrinsic_type](
                arg.name, [1], arg.function_space.orig_name,
                interface=ArgumentInterface(INTENT_MAPPING[arg.intent]))
        except KeyError:
            raise NotImplementedError(
                "field of type {0} not implemented"
                "".format(arg.intrinsic_type))

        self._symbol_table.add(field_data)
        self._arglist.append(field_data)
        self._fields_list.append(field_data)

    def stencil_unknown_extent(self, arg, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("stencil_unknown_extent not implemented")

    def stencil_unknown_direction(self, arg, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("stencil_unknown_direction not implemented")

    def stencil(self, arg, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("stencil not implemented")

    def operator(self, operator_node, var_accesses=None):
        ''' Create LFRic size and operator data objects '''

        op_size = OperatorSizeDataSymbol(
            "ncell_3d",
            interface=ArgumentInterface(ArgumentInterface.Access.READ))
        self._symbol_table.add(op_size)
        self._arglist.append(op_size)
        arg = OperatorDataSymbol(
            operator_node.name, [1, 1, 1],
            operator_node.function_space_from.orig_name,
            operator_node.function_space_to.orig_name,
            interface=ArgumentInterface(INTENT_MAPPING[operator_node.intent]))
        self._symbol_table.add(arg)
        self._arglist.append(arg)
        self._operators_list.append(arg)
        self._operator_dims_map[arg] = op_size

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
            arg = mapping[scalar_arg.intrinsic_type](scalar_arg.name, \
                interface=ArgumentInterface(INTENT_MAPPING[scalar_arg.intent]))
        except KeyError:
            raise NotImplementedError(
                "scalar of type {0} not implemented"
                "".format(scalar_arg.intrinsic_type))
        self._symbol_table.add(arg)
        self._arglist.append(arg)

    def fs_common(self, fs, var_accesses=None):
        ''' Create LFRic Number of Dofs object '''

        fs_name = fs.orig_name
        arg = NumberOfDofsDataSymbol(
            "ndf_{0}".format(fs_name), fs_name,
            interface=ArgumentInterface(ArgumentInterface.Access.READ))
        self._symbol_table.add(arg)
        self._arglist.append(arg)
        self._ndofs_map[fs_name] = arg

    def fs_intergrid(self, function_space, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("fs_integrated not implemented")

    def fs_compulsory_field(self, fs, var_accesses=None):
        '''Create LFRic Number of Unique dofs and dofmap objects'''

        fs_name = fs.orig_name
        arg = NumberOfUniqueDofsDataSymbol(
            "undf_{0}".format(fs_name), fs_name,
            interface=ArgumentInterface(ArgumentInterface.Access.READ))
        self._symbol_table.add(arg)
        self._arglist.append(arg)
        self._undfs_map[fs_name] = arg

        # The dimension argument may not have been declared yet so use
        # an integer as a placeholder.
        arg = DofMapDataSymbol(
            "dofmap_{0}".format(fs_name), [1], fs_name,
            interface=ArgumentInterface(ArgumentInterface.Access.READ))
        self._symbol_table.add(arg)
        self._arglist.append(arg)
        self._dofmaps_list.append(arg)

    def banded_dofmap(self, function_space, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("banded_dofmap not implemented")

    def indirection_dofmap(self, function_space, operator=None,
                           var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("indirection_dofmap not implemented")

    def basis(self, function_space, var_accesses=None):
        ''' Not implemented '''
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
                arg = BasisFunctionDataSymbol(basis_name, [1, 1, 1], fs_name, quad_name, interface=ArgumentInterface(ArgumentInterface.Access.READ))
                self._symbol_table.add(arg)
                self._arglist.append(arg)
            elif shape in VALID_EVALUATOR_SHAPES:
                # Need a basis array for each target space upon which the basis
                # functions have been evaluated. _kern.eval_targets is a dict
                # where the values are 2-tuples of (FunctionSpace, argument).
                for _, target in self._kern.eval_targets.items():
                    raise NotImplementedError("evaluator shapes not implemented")
            else:
                raise InternalError(
                    "Unrecognised evaluator shape ('{0}'). Expected one of: "
                    "{1}".format(shape, VALID_EVALUATOR_SHAPES))

    def diff_basis(self, function_space, var_accesses=None):
        ''' Not implemented '''
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
                arg = DiffBasisFunctionDataSymbol(diff_basis_name, [1, 1, 1], fs_name, quad_name, interface=ArgumentInterface(ArgumentInterface.Access.READ))
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
        ''' Not implemented '''
        args = []
        read_access = ArgumentInterface(ArgumentInterface.Access.READ)
        for shape in self._kern.qr_rules:
            if shape == "gh_quadrature_xyoz":
                nqp_h = NumberOfQrPointsInHorizontalDataSymbol(
                    "nqp_h", interface=read_access)
                nqp_v = NumberOfQrPointsInVerticalDataSymbol(
                    "nqp_v", interface=read_access)
                args.extend(
                    [nqp_h, nqp_v,
                     QrWeightsInHorizontalDataSymbol(
                         "weights_h", [nqp_h], interface=read_access),
                     QrWeightsInVerticalDataSymbol(
                         "weights_v", [nqp_v], interface=read_access)])
            elif shape == "gh_quadrature_face":
                nfaces = NumberOfFacesDataSymbol(
                    "nfaces", interface=read_access)
                nqp = NumberOfQrPointsDataSymbol(
                    "nqp", interface=read_access)
                args.extend(
                    [nfaces, nqp,
                     QrWeightsDataSymbol(
                         "weights", [nqp], interface=read_access)])
            elif shape == "gh_quadrature_edge":
                nedges = NumberOfEdgesDataSymbol(
                    "nedges", interface=read_access)
                nqp = NumberOfQrPointsDataSymbol(
                    "nqp", interface=read_access)
                args.extend(
                    [nedges, nqp,
                     QrWeightsDataSymbol(
                         "weights", [nqp], interface=read_access)])
            else:
                raise InternalError("Unsupported quadrature shape ('{0}') "
                                    "found in kernel_interface".format(shape))
        for arg in args:
            self._symbol_table.add(arg)
            self._arglist.append(arg)
