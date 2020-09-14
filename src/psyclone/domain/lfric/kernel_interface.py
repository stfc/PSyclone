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
from psyclone.domain.lfric import ArgOrdering
from psyclone.psyir.symbols import ScalarType, ContainerSymbol, DataSymbol, DeferredType, GlobalInterface, ArrayType
from psyclone.psyir.nodes import Reference
from psyclone.configuration import Config

from psyclone.domain.lfric.lfric_ir import I_DEF, R_DEF, CellPositionDataType, MeshHeightDataType, OperatorSizeDataType, NumberOfDofsDataType, DofMapDataType, NumberOfUniqueDofsDataType

from psyclone.domain.lfric.lfric_ir import CellPositionDataSymbol, MeshHeightDataSymbol, OperatorSizeDataSymbol, NumberOfDofsDataSymbol, NumberOfUniqueDofsDataSymbol, DofMapDataSymbol, FieldDataDataSymbol, OperatorDataSymbol
from psyclone.psyir.symbols import SymbolTable, ArgumentInterface

class KernelInterface(ArgOrdering):
    '''Create the list of actual argument types that a kernel has'''
    def __init__(self, kern):
        # RF TBD: TURN THIS INTO A KERNEL STUB GENERATOR WHICH USES LFRic PSYIR
        ArgOrdering.__init__(self, kern)
        self._symbol_table = SymbolTable()
        self._arglist = []
        self._dofmaps_list = [] 
        self._ndofs_map = {}
        self._fields_list = []
        self._undfs_map = {}
        self._operators_list = []
        self._operator_dims_map = {}
       
    def generate(self):
        '''Call the generate base class then call the connect method after it
        has completed.'''
        super(KernelInterface, self).generate()
        self.connect()

    def connect(self):
        '''Connect the interface arguments together. This is performed after
        all the interface arguments have been created to avoid the
        problem of trying to connect before an argument has been
        created.

        '''
        # TODO: Array dimensions and their order are specified here,
        # so what if they change (particularly operators as they are
        # multi-dimensional).
        
        # dofmaps are dimensioned by ndofs
        for dofmap in self._dofmaps_list:
            fs = dofmap.fs
            ndofs = self._ndofs_map[fs]
            dofmap.datatype._shape = [ndofs]

        # fields are dimensioned by undf
        for field in self._fields_list:
            fs = field.fs
            undf = self._undfs_map[fs]
            field.datatype._shape = [undf]

        # operators are dimensioned by 2*ndf + ncell_3d
        for operator in self._operators_list:
            fs_from = operator.fs_from
            fs_to = operator.fs_to
            ndf_from = self._ndofs_map[fs_from]
            ndf_to = self._ndofs_map[fs_to]
            op_dim = self._operator_dims_map[operator]
            operator.datatype._shape = [ndf_from, ndf_to, op_dim]


    def cell_position(self, var_accesses=None):
        ''' Create an LFRic cell position object '''

        arg = CellPositionDataSymbol(
            "cell", interface=ArgumentInterface(ArgumentInterface.Access.READ))
        self._symbol_table.add(arg)
        self._arglist.append(arg)
        
    def mesh_height(self, var_accesses=None):
        ''' Create an LFRic mesh height object '''

        arg = MeshHeightDataSymbol(
            "nlayers", interface=ArgumentInterface(ArgumentInterface.Access.READ))
        self._symbol_table.add(arg)
        self._arglist.append(arg)


    def mesh_ncell2d(self, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("mesh_ncell2d not implemented")

    def cell_map(self, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("cell_map not implemented")

    def field_vector(self, arg, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("field_vector not implemented")

    def field(self, field_node, var_accesses=None):
        ''' Create an LFRic field data object '''

        #intrinsic_mapping = {"real": ScalarType.Intrinsic.REAL,
        #                     "integer": ScalarType.Intrinsic.INTEGER,
        #                     "logical": ScalarType.Intrinsic.BOOLEAN}
        intent_mapping = {"in": ArgumentInterface.Access.READ,
                          "out": ArgumentInterface.Access.WRITE,
                          "inout": ArgumentInterface.Access.READWRITE}
        arg = FieldDataDataSymbol(field_node.name, [1], field_node.function_space.orig_name, interface=ArgumentInterface(intent_mapping[field_node.intent]))
        self._symbol_table.add(arg)
        self._arglist.append(arg)
        self._fields_list.append(arg)

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
            "ncell_3d", interface=ArgumentInterface(ArgumentInterface.Access.READ))
        self._symbol_table.add(op_size)
        self._arglist.append(op_size)

        intent_mapping = {"in": ArgumentInterface.Access.READ,
                          "out": ArgumentInterface.Access.WRITE,
                          "inout": ArgumentInterface.Access.READWRITE}
        arg = OperatorDataSymbol(operator_node.name, [1, 1, 1], operator_node.function_space_from.orig_name, operator_node.function_space_to.orig_name, interface=ArgumentInterface(intent_mapping[operator_node.intent]))
        self._symbol_table.add(arg)
        self._arglist.append(arg)
        self._operators_list.append(arg)
        self._operator_dims_map[arg] = op_size

    def cma_operator(self, arg, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("cma_operator not implemented")

    def scalar(self, arg, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("scalar not implemented")

    def fs_common(self, fs, var_accesses=None):
        ''' Create LFRic Number of Dofs object '''

        fs_name = fs.orig_name
        arg = NumberOfDofsDataSymbol(
            "ndf_{0}".format(fs_name), fs_name,
            interface=ArgumentInterface(ArgumentInterface.Access.READ))
        self._symbol_table.add(arg)
        self._arglist.append(arg)
        self._ndofs_map[fs_name] = arg

    def fs_intergrid(self, fs, var_accesses=None):
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

    def banded_dofmap(self, fs, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("banded_dofmap not implemented")

    def indirection_dofmap(self, fs, operator=None, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("indirection_dofmap not implemented")

    def basis(self, fs, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("basis not implemented")

    def diff_basis(self, fs, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("diff_basis not implemented")

    def orientation(self, fs, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("orientation not implemented")

    def field_bcs_kernel(self, fs, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("field_bcs_kernel not implemented")

    def operator_bcs_kernel(self, fs, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("operator_bcs_kernel not implemented")

    def ref_element_properties(self, var_accesses=None):
        ''' Properties associated with the reference element '''
        # This callback does not contribute any kernel arguments
        pass

    def mesh_properties(self, var_accesses=None):
        ''' Properties associated with the mesh '''
        # This callback does not contribute any kernel arguments
        pass

    def quad_rule(self, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("quad_rule not implemented")
