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

from psyclone.domain.lfric.lfric_ir import i_def, r_def, CellPositionDataType, MeshHeightDataType, OperatorSizeDataType, NumberOfDofsDataType, DofMapDataType, NumberOfUniqueDofsDataType


class KernelInterface(ArgOrdering):
    '''Create the list of actual argument types that a kernel has'''
    def __init__(self, kern):
        ArgOrdering.__init__(self, kern)
        self._index = 0
        self._ndofs_map = {}
        self._undfs_map = {}
        self._operator_dims_map = {}
        self._dofmaps_list = []
        self._fields_list = []
        self._operators_list = []

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
            fs = dofmap.function_space
            ndofs = self._ndofs_map[fs]
            dofmap.dims = [ndofs]
        # fields are dimensioned by undf
        for field in self._fields_list:
            fs = field.function_space
            undf = self._undfs_map[fs]
            field.dims = [undf]
        # operators have 
        for operator in self._operators_list:
            fs_from = operator.function_space_from
            ndofs_from = self._ndofs_map[fs_from]
            fs_to = operator.function_space_to
            ndofs_to = self._ndofs_map[fs_to]
            op_dim = self._operator_dims_map[operator]
            operator.dims = [ndofs_from, ndofs_to, op_dim]

    def cell_position(self, var_accesses=None):
        ''' Create an LFRic cell position object '''
        #self._arglist.append(CellPositionDescription())
        self._arglist.append(
            CellPositionArgInfo(self._index, "in"))
        self._index += 1
        
    def mesh_height(self, var_accesses=None):
        ''' Create an LFRic mesh height object '''
        self._arglist.append(
            MeshHeightArgInfo(self._index, "in"))
        self._index += 1

    def mesh_ncell2d(self, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("mesh_ncell2d not implemented")

    def cell_map(self, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("cell_map not implemented")

    def field_vector(self, arg, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("field_vector not implemented")

    def field(self, arg, var_accesses=None):
        ''' Create an LFRic field data object '''
        field_data_info = FieldDataArgInfo(self._index, arg)
        self._fields_list.append(field_data_info)
        self._arglist.append(field_data_info)
        self._index += 1

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
        operator_size = OperatorSizeArgInfo(self._index, "in")
        self._index += 1
        operator_data = OperatorDataArgInfo(self._index, arg)
        self._operators_list.append(operator_data)
        self._index += 1
        self._operator_dims_map[operator_data] = operator_size
        self._arglist.extend([operator_size, operator_data])

    def cma_operator(self, arg, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("cma_operator not implemented")

    def scalar(self, arg, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("scalar not implemented")

    def fs_common(self, fs, var_accesses=None):
        ''' Create LFRic Number of Dofs object '''
        ndofs_info = NumberOfDofsArgInfo(self._index, "in", fs)
        self._ndofs_map[ndofs_info.function_space] = ndofs_info
        self._arglist.append(ndofs_info)
        self._index += 1

    def fs_intergrid(self, fs, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("fs_integrated not implemented")

    def fs_compulsory_field(self, fs, var_accesses=None):
        '''Create LFRic Number of Unique dofs and dofmap objects'''
        undf_info = NumberOfUniqueDofsArgInfo(self._index, "in", fs)
        self._undfs_map[undf_info.function_space] = undf_info
        self._arglist.append(undf_info)
        self._index += 1
        dofmap_info = DofMapArgInfo(self._index, "in", fs)
        self._dofmaps_list.append(dofmap_info)
        self._arglist.append(dofmap_info)
        self._index += 1

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

''' These classes capture the structure of LFRic datatypes. The
structure can be used by PSyclone to create new code, i.e. declare and
use a particular variable. PSyclone can also use this to check the
validity of existing kernel code and to transform a PSyIR
representation into an LFRicIR representation.'''


class ScalarArgInfo():
    ''' xxx '''
    def __init__(self, index, intent):
        self.index = index
        self.intent = intent
        self.name = None
        self.datatype = None
    def __str__(self):
        return ("[{0}] {1} [type='{3}', precision='{4}', intent='{5}']".format(self.index, self.name, self.description, self.datatype.intrinsic.name.lower(), self.datatype.precision.name, self.intent))


class ArrayArgInfo():
    ''' xxx '''

    intrinsic_mapping = {"real": ScalarType.Intrinsic.REAL,
                         "integer": ScalarType.Intrinsic.INTEGER,
                         "logical": ScalarType.Intrinsic.BOOLEAN}
    precision_mapping = {"i_def": i_def,
                         "r_def": r_def}

    def __init__(self, index, intent):
        self.index = index
        self.intent = intent
        self.dims = None
        self.name = None
        self.description = None
        self.datatype = None
        self.array_type = None
    def __str__(self):
        array_indices = [str(dim.name) for dim in self.dims]
        text = ""
        if self.array_type:
            text = self.array_type + " "
        return ("[{0}] {7}{1}({6}) [type='{3}', precision='{4}', intent='{5}']".format(self.index, self.name, self.description, self.datatype.intrinsic.name.lower(), self.datatype.precision.name, self.intent, ",".join(array_indices), text))


class CellPositionArgInfo(ScalarArgInfo):
    '''xxx'''
    def __init__(self, index, intent):
        super(CellPositionArgInfo, self).__init__(index, intent)
        self.datatype = CellPositionDataType()
        self.name = "cell"
        self.description = "cell position"


class MeshHeightArgInfo(ScalarArgInfo):
    '''xxx'''
    def __init__(self, index, intent):
        super(MeshHeightArgInfo, self).__init__(index, intent)
        self.datatype = MeshHeightDataType()
        self.name = "nlayers"
        self.description = "number of cells in a column"


class FieldDataArgInfo(ArrayArgInfo):
    '''xxx'''
    def __init__(self, index, field):
        super(FieldDataArgInfo, self).__init__(index, field.intent)
        self.ndims = 1
        intrinsic = FieldDataArgInfo.intrinsic_mapping[field.intrinsic_type]
        api_config = Config.get().api_conf("dynamo0.3")
        precision_str = api_config.default_kind[field.intrinsic_type]
        precision_kind = FieldDataArgInfo.precision_mapping[precision_str]
        scalar_type = ScalarType(intrinsic, precision_kind)
        self.datatype = ArrayType(scalar_type, [1]*self.ndims)
        self.description = "field data"
        self.name = field.name
        self.function_space = field.function_space.orig_name
        self.array_type = "field data"


class OperatorSizeArgInfo(ScalarArgInfo):
    '''xxx'''
    def __init__(self, index, intent):
        super(OperatorSizeArgInfo, self).__init__(index, intent)
        self.datatype = OperatorSizeDataType()
        self.name = "ncell_3d"
        self.description = "operator data size"


class OperatorDataArgInfo(ArrayArgInfo):
    '''xxx'''
    def __init__(self, index, operator):
        super(OperatorDataArgInfo, self).__init__(index, operator.intent)
        self.ndims = 3
        intrinsic = OperatorDataArgInfo.intrinsic_mapping[operator.intrinsic_type]
        api_config = Config.get().api_conf("dynamo0.3")
        precision_str = api_config.default_kind[operator.intrinsic_type]
        precision_kind = OperatorDataArgInfo.precision_mapping[precision_str]
        scalar_type = ScalarType(intrinsic, precision_kind)
        self.datatype = ArrayType(scalar_type, [1]*self.ndims)
        self.description = "operator data"
        self.name = operator.name
        self.function_space_from = operator.function_space_from.orig_name
        self.function_space_to = operator.function_space_to.orig_name
        self.array_type = "operator"


class OperatorData():
    ''' operator data '''

    mapping = {"real": ScalarType.Intrinsic.REAL,
               "integer": ScalarType.Intrinsic.INTEGER,
               "logical": ScalarType.Intrinsic.BOOLEAN}

    def __init__(self, arg, operator_size):
        self.form = "array"
        self.ndims = 3
        self.dims = [None, None, operator_size]
        # TODO: link the first dimension to the ndf1, the second to
        # ndf2 and the third to OperatorSize arguments
        self.intrinsic = OperatorData.mapping[arg.intrinsic_type]
        api_config = Config.get().api_conf("dynamo0.3")
        self.precision = api_config.default_kind[arg.intrinsic_type]
        self.intent = arg.intent
        self.default_name = arg.name
    def __str__(self):
        return ("operator data [name='{0}', type='{1}', precision='{2}',"
                " intent='{3}']".format(self.default_name, self.intrinsic,
                                        self.precision, self.intent))


class NumberOfDofsArgInfo(ScalarArgInfo):
    '''xxx'''
    def __init__(self, index, intent, fs):
        super(NumberOfDofsArgInfo, self).__init__(index, intent)
        self.datatype = NumberOfDofsDataType()
        self.name = "ndf"
        self.description = "number of dofs in a cell"
        self.function_space = fs.orig_name


class NumberOfUniqueDofsArgInfo(ScalarArgInfo):
    '''xxx'''
    def __init__(self, index, intent, fs):
        super(NumberOfUniqueDofsArgInfo, self).__init__(index, intent)
        self.datatype = NumberOfUniqueDofsDataType()
        self.description = "number of unique dofs in a column"
        self.name = "undf"
        self.function_space = fs.orig_name


class DofMapArgInfo(ArrayArgInfo):
    '''xxx'''
    def __init__(self, index, intent, fs):
        super(DofMapArgInfo, self).__init__(index, intent)
        # Make each array index size 1 as we don't care about the
        # sizes here
        self.ndims = 1
        self.datatype = DofMapDataType([1]*self.ndims)
        self.description = "dof ids for the column base cell"
        self.name = "dofmap"
        self.function_space = fs.orig_name
        self.array_type = ""
