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

'''This module generates LFRic-specific PSyIR classes from a '''

from psyclone.domain.lfric import ArgOrdering
from psyclone.psyir.symbols import ScalarType, ContainerSymbol, DataSymbol, \
    DeferredType, GlobalInterface, ArrayType
from psyclone.psyir.nodes import Reference
from psyclone.configuration import Config

# Define the LFRic datatypes
modules = [{"name": "constants_mod", "vars": ["i_def", "r_def"]}]
scalar_datatypes = [
    {"name": "lfric integer scalar", "intrinsic": "integer",
     "precision": "i_def"},
    {"name": "lfric real scalar", "intrinsic": "integer",
     "precision": "r_def", "short_name": "real_scalar"},
    {"name": "cell position", "intrinsic": "integer",
     "precision": "i_def", "short_name": "nlayers"},
    {"name": "mesh height", "intrinsic": "integer",
     "precision": "i_def", "short_name": "nlayers"},
    {"name": "operator size", "intrinsic": "integer",
     "precision": "i_def", "short_name": "ncell_3d"},
    {"name": "number of dofs", "intrinsic": "integer",
     "precision": "i_def", "short_name": "ndf"},
    {"name": "number of unique dofs", "intrinsic": "integer",
     "precision": "i_def", "short_name": "undf"}]

# TODO: 1st dim of operator is ndf_to, 2nd is ndf_from. Not sure
# whether we should capture this and if so, how.
array_datatypes = [
    {"name": "field data", "scalar_type": "lfric real scalar",
     "dims": ["number of unique dofs"]},
    {"name": "operator", "scalar_type": "lfric real scalar",
     "dims": ["ndf", "ndf", "ncell_3d"]},
    {"name": "dof map", "scalar_type": "lfric integer scalar",
     "dims": ["ndf"]}]

# Generate LFRic module symbols from definitions
for module in modules:
    module_name = module["name"]
    exec("{0} = ContainerSymbol('{0}')\n".format(module_name))
    for module_var in module["vars"]:
        exec("{0} = DataSymbol('{0}', DeferredType(), interface="
             "GlobalInterface({1}))".format(module_var, module_name))

# Generate LFRic scalar datatypes and symbols from definitions
for scalar_type in scalar_datatypes:
    name = "".join(scalar_type["name"].title().split())
    intrinsic = scalar_type["intrinsic"].upper()
    precision = scalar_type["precision"]
    exec('''
class {0}DataType(ScalarType):
    def __init__(self):
        super({0}DataType, self).__init__(
            ScalarType.Intrinsic.{1}, {2})
'''.format(name, intrinsic, precision))
    exec('''
class {0}DataSymbol(DataSymbol):
    def __init__(self, name):
        super({0}DataSymbol, self).__init__(
            name, {0}DataType())
'''.format(name))

# Generate LFRic array datatypes and symbols from definitions
for array_type in array_datatypes:
    name = "".join(array_type["name"].title().split())
    dims = array_type["dims"],
    scalar_type = "".join(array_type["scalar_type"].title().split())
    exec('''
class {0}DataType(ArrayType):
    def __init__(self, dims):
        assert (len(dims) == {1}), 'Error, ...'
        # TBD check type of dims ...
        super({0}DataType, self).__init__(
            {2}DataType(), dims)
'''.format(name, len(dims), scalar_type))
    exec('''
class {0}DataSymbol(DataSymbol):
    def __init__(self, name, dims):
        super({0}DataSymbol, self).__init__(
            name, {0}DataType(dims))
'''.format(name))

# TODO SYmbols that are arguments (the assumption here is local)
# TODO Store fs information?
