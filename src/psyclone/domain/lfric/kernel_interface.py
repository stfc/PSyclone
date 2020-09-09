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

# LFRic-PSyIR

precision_mod = ContainerSymbol("constants_mod")
I_DEF = DataSymbol("i_def", DeferredType(),
                   interface=GlobalInterface(precision_mod))
R_DEF = DataSymbol("r_def", DeferredType(),
                   interface=GlobalInterface(precision_mod))

# LFRic general types
class LfricIntegerScalarType(ScalarType):
    ''' xxx '''
    def __init__(self):
        super(LfricIntegerScalarType, self).__init__(
            ScalarType.Intrinsic.INTEGER, I_DEF)


class LfricIntegerArrayType(ArrayType):
    ''' xxx '''
    def __init__(self, dims):
        super(LfricIntegerArrayType, self).__init__(
            LfricIntegerScalarType(), dims)


class CellPositionDataType(LfricIntegerScalarType):
    ''' xxx '''


class MeshHeightDataType(LfricIntegerScalarType):
    ''' xxx '''


class OperatorSizeDataType(LfricIntegerScalarType):
    ''' xxx '''


class NumberOfDofsDataType(LfricIntegerScalarType):
    ''' xxx '''


class NumberOfUniqueDofsDataType(LfricIntegerScalarType):
    ''' xxx '''


class DofMapDataType(LfricIntegerArrayType):
    ''' xxx '''

# LFRic general data symbols
class LfricIntegerDataSymbol(DataSymbol):
    ''' xxx '''
    def __init__(self, name):
        super(LfricIntegerDataSymbol, self).__init__(
            name, LfricIntegerScalarType())


# LFRic specific data symbols
class CellPositionSymbol(LfricIntegerDataSymbol):
    ''' xxx '''


class NumberOfDofsSymbol(LfricIntegerDataSymbol):
    ''' xxx '''


class NumberOfUniqueDofsSymbol(LfricIntegerDataSymbol):
    ''' xxx '''


class DofMapSymbol(DataSymbol):
    ''' xxx '''
    def __init__(self, name, ndofs_symbol):
        assert isinstance(ndofs_symbol, NumberOfDofsSymbol)
        super(DofMapSymbol, self).__init__(
            name, ArrayType(LfricIntegerScalarType(), [ndofs_symbol]))


class FieldDataSymbol(DataSymbol):
    '''Should be part of a field object'''
    def __init__(self, name, n_unique_dofs_symbol, fs):
        self._fs = fs
        super(self, DataSymbol).__init__(
            name, ArrayType(LfricIntegerScalarType(), [n_unique_dofs_symbol]))

cell_position_symbol = CellPositionSymbol("cell")
ndofs_symbol = NumberOfDofsSymbol("ndofs")
dofmap_symbol = DofMapSymbol("dofmap", ndofs_symbol)

#undofs_symbol = ???
#field_data_symbol = FieldDataSymbol("field1", undofs_symbol, W3)

cell_position_reference = Reference(cell_position_symbol)

# mesh_height_reference = Reference(mesh_height_symbol)

class KernelInterface(ArgOrdering):
    '''Create the list of actual argument types that a kernel has'''
    def __init__(self, kern):
        ArgOrdering.__init__(self, kern)
        self._index = 0

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
        self._arglist.append(FieldData(arg))

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

        operator_data = OperatorData(arg, 10)
        operator_data.operator_size = operator_size
        self._arglist.extend([operator_size, operator_data])

    def cma_operator(self, arg, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("cma_operator not implemented")

    def scalar(self, arg, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("scalar not implemented")

    def fs_common(self, fs, var_accesses=None):
        ''' Create LFRic Number of Dofs object '''
        self._arglist.append(NumberOfDofsArgInfo(self._index, "in", fs))
        self._index += 1

    def fs_intergrid(self, fs, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("fs_integrated not implemented")

    def fs_compulsory_field(self, fs, var_accesses=None):
        '''Create LFRic Number of Unique dofs and dofmap objects'''
        undf_info = NumberOfUniqueDofsArgInfo(self._index, "in", fs)
        self._arglist.append(undf_info)
        self._index += 1
        self._arglist.append(DofMapArgInfo(self._index, "in", fs, [undf_info]))
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
        return ("[{0}] {1} : {2} [type='{3}', precision='{4}', intent='{5}']".format(self.index, self.name, self.description, self.datatype.intrinsic.name.lower(), self.datatype.precision.name, self.intent))


class ArrayArgInfo():
    ''' xxx '''
    def __init__(self, index, intent, dims):
        self.index = index
        self.intent = intent
        self.dims = dims
        self.name = None
        self.description = None
        self.datatype = None
    def __str__(self):
        array_indices = [str(dim.name) for dim in self.dims]
        return ("[{0}] {1} : {2} [type='{3}', precision='{4}', intent='{5}', dims=[{6}]]".format(self.index, self.name, self.description, self.datatype.intrinsic.name.lower(), self.datatype.precision.name, self.intent, ",".join(array_indices)))


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


class FieldData():
    ''' data array that contains the data in a field. '''

    mapping = {"real": ScalarType.Intrinsic.REAL,
               "integer": ScalarType.Intrinsic.INTEGER,
               "logical": ScalarType.Intrinsic.BOOLEAN}

    def __init__(self, arg):
        # TODO: link the array dimension to the undf interface
        # argument and add links to dofmap and nlayers interface args
        self.form = "array"
        self.ndims = 1
        self.intrinsic = FieldData.mapping[arg.intrinsic_type]
        api_config = Config.get().api_conf("dynamo0.3")
        self.precision = api_config.default_kind[arg.intrinsic_type]
        self.intent = arg.intent
        self.default_name = arg.name
    def __str__(self):
        return ("field data [name='{0}', type='{1}', precision='{2}',"
                " intent='{3}']".format(self.default_name, self.intrinsic,
                                        self.precision, self.intent))


class OperatorSizeArgInfo(ScalarArgInfo):
    '''xxx'''
    def __init__(self, index, intent):
        super(OperatorSizeArgInfo, self).__init__(index, intent)
        self.datatype = OperatorSizeDataType()
        self.name = "opsize"
        self.description = "operator data size"


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
        self.name = "ndofs"
        self.description = "number of dofs in a cell"
        self.function_space = fs


class NumberOfUniqueDofsArgInfo(ScalarArgInfo):
    '''xxx'''
    def __init__(self, index, intent, fs):
        super(NumberOfUniqueDofsArgInfo, self).__init__(index, intent)
        self.datatype = NumberOfUniqueDofsDataType()
        self.description = "number of unique dofs in a column"
        self.name = "cell3d"
        self.function_space = fs


class DofMapArgInfo(ArrayArgInfo):
    '''xxx'''
    def __init__(self, index, intent, fs, dims):
        super(DofMapArgInfo, self).__init__(index, intent, dims)
        self.datatype = DofMapDataType([1 for dim in dims])
        self.description = "dof ids for the column base cell"
        self.name = "dofmap"
        self.function_space = fs


class DofMap():
    '''dofmap for the cell at the base of the kernel column.'''
    def __init__(self, fs):
        self.fs_name = fs.orig_name
        self.form = "array"
        self.ndims = 1
        # TODO: link the first dimension to the ndf arg
        self.intrinsic = ScalarType.Intrinsic.INTEGER
        self.precision = "i_def"
        self.intent = "in"
        self.default_name = "dofmap"
    def __str__(self):
        return ("dofmap")

