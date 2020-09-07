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
from psyclone.psyir.symbols import ScalarType
from psyclone.configuration import Config

class KernelInterface(ArgOrdering):
    '''Create the list of actual argument types that a kernel has'''
    def __init__(self, kern):
        ArgOrdering.__init__(self, kern)

    def cell_position(self, var_accesses=None):
        ''' Create an LFRic cell position object '''
        self._arglist.append(CellPosition())
        
    def mesh_height(self, var_accesses=None):
        ''' Create an LFRic mesh height object '''
        self._arglist.append(MeshHeight())

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
        operator_size = OperatorSize()
        operator_data = OperatorData(arg)
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
        self._arglist.append(NumberOfDofs(fs))

    def fs_intergrid(self, fs, var_accesses=None):
        ''' Not implemented '''
        raise NotImplementedError("fs_integrated not implemented")

    def fs_compulsory_field(self, fs, var_accesses=None):
        '''Create LFRic Number of Unique dofs and dofmap objects'''
        self._arglist.append(NumberOfUniqueDofs(fs))
        self._arglist.append(DofMap(fs))

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


class CellPosition():
    '''Scalar integer that contains the cell-position for the kernel
    call.

    '''
    def __init__(self):
        self.form = "scalar"
        self.intrinsic = ScalarType.Intrinsic.INTEGER
        self.precision = "i_def"
        self.intent = "in"
        self.default_name = "cell"
    def __str__(self):
        return ("cell position")


class MeshHeight():
    ''' Scalar integer that contains the height of the mesh. '''
    def __init__(self):
        self.form = "scalar"
        self.intrinsic = ScalarType.Intrinsic.INTEGER
        self.precision = "i_def"
        self.intent = "in"
        self._default_name = "nlayers"
    def __str__(self):
        return ("mesh height")


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


class OperatorSize():
    ''' the size of the operator data. '''
    def __init__(self):
        self.form = "scalar"
        self.intrinsic = ScalarType.Intrinsic.INTEGER
        self.precision = "i_def"
        self.intent = "in"
        self.default_name = "ncell_3d"
    def __str__(self):
        return ("operator data size")


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


class NumberOfDofs():
    ''' number of degrees of freedom per cell. '''
    def __init__(self, fs):
        self.fs_name = fs.orig_name
        self.form = "scalar"
        self.intrinsic = ScalarType.Intrinsic.INTEGER
        self.precision = "i_def"
        self.intent = "in"
        self.default_name = "ndf"
    def __str__(self):
        return ("ndf")


class NumberOfUniqueDofs():
    ''' number of unique degrees of freedom per cell. '''
    def __init__(self, fs):
        self.fs_name = fs.orig_name
        self.form = "scalar"
        self.intrinsic = ScalarType.Intrinsic.INTEGER
        self.precision = "i_def"
        self.intent = "in"
        self.default_name = "undf"
    def __str__(self):
        return ("undf")


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
