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

'''This module generates LFRic-specific PSyIR classes from a list of
definitions.'''

from psyclone.psyir.symbols import ContainerSymbol, DataSymbol, \
    DeferredType, GlobalInterface, ScalarType, ArrayType

# pylint: disable=exec-used

# Define LFRic symbols and datatypes
modules = [{"name": "constants_mod", "vars": ["i_def", "r_def", "l_def"]}]

# Generate LFRic module symbols from definitions
for module in modules:
    MODULE_NAME = module["name"]
    exec("{0} = ContainerSymbol('{1}')\n".format(
        MODULE_NAME.upper(), MODULE_NAME))
    for module_var in module["vars"]:
        exec("{0} = DataSymbol('{1}', DeferredType(), interface="
             "GlobalInterface({2}))".format(
                 module_var.upper(), module_var, MODULE_NAME.upper()))

scalar_datatypes = [
    {"name": "lfric integer scalar", "intrinsic": "integer",
     "precision": "i_def", "vars": [], "short_name": "integer_scalar"},
    {"name": "lfric real scalar", "intrinsic": "real",
     "precision": "r_def", "vars": [], "short_name": "real_scalar"},
    {"name": "lfric logical scalar", "intrinsic": "boolean",
     "precision": "l_def", "vars": [], "short_name": "logical_scalar"},
    {"name": "cell position", "intrinsic": "integer",
     "precision": "i_def", "vars": [], "short_name": "nlayers"},
    {"name": "mesh height", "intrinsic": "integer",
     "precision": "i_def", "vars": [], "short_name": "nlayers"},
    {"name": "operator size", "intrinsic": "integer",
     "precision": "i_def", "vars": [], "short_name": "ncell_3d"},
    {"name": "number of dofs", "intrinsic": "integer",
     "precision": "i_def", "vars": ["fs"], "short_name": "ndf"},
    {"name": "number of unique dofs", "intrinsic": "integer",
     "precision": "i_def", "vars": ["fs"], "short_name": "undf"}]

for info in scalar_datatypes:
    NAME = "".join(info["name"].title().split())
    INTRINSIC = info["intrinsic"].upper()
    PRECISION = info["precision"]    
    ARGS = ["self", "name"] + info["vars"] + ["interface=None"]
    VARS = ["        self.{0} = {0}".format(var) for var in info["vars"]]
    exec ('''
class {0}DataType(ScalarType):
    def __init__(self):
        super({0}DataType, self).__init__(
            ScalarType.Intrinsic.{1}, {2})
    '''.format(NAME, INTRINSIC, PRECISION.upper()))

    exec ('''
class {0}DataSymbol(DataSymbol):
    def __init__({1}):
{2}
        super({0}DataSymbol, self).__init__(
            name, {0}DataType(), interface=interface)
    '''.format(NAME, ", ".join(ARGS), "\n".join(VARS)))

field_datatypes = [
    {"name": "real field data", "scalar_type": "lfric real scalar",
     "dims": ["number of unique dofs"], "vars": ["fs"]},
    {"name": "integer field data", "scalar_type": "lfric integer scalar",
     "dims": ["number of unique dofs"], "vars": ["fs"]},
    {"name": "logical field data", "scalar_type": "lfric logical scalar",
     "dims": ["number of unique dofs"], "vars": ["fs"]}]

array_datatypes = [
    {"name": "operator", "scalar_type": "lfric real scalar",
     "dims": ["ndf", "ndf", "ncell_3d"], "vars": ["fs_from", "fs_to"], "fs_from_to": [0, 1]},
    {"name": "dof map", "scalar_type": "lfric integer scalar",
     "dims": ["ndf"], "vars": ["fs"]},
    {"name": "basis function", "scalar_type": "lfric integer scalar",
     "dims": ["ndf", "ndf", "ndf"], "vars": ["fs", "quad"]},
    {"name": "diff basis function", "scalar_type": "lfric integer scalar",
     "dims": ["ndf", "ndf", "ndf"], "vars": ["fs", "quad"]}]

for array_type in array_datatypes + field_datatypes:
    NAME = "".join(array_type["name"].title().split())
    DIMS = array_type["dims"]
    SCALAR_TYPE = "".join(array_type["scalar_type"].title().split())
    ARGS = ["self", "name", "dims"] + array_type["vars"] + ["interface=None"]
    VARS = ["        self.{0} = {0}".format(var) for var in array_type["vars"]]
    exec('''
class {0}DataType(ArrayType):
    def __init__(self, dims):
        assert (len(dims) == {1}), ('Error, expected {1} dimension(s) but '
            'got {{0}}'.format(len(dims)))
        super({0}DataType, self).__init__(
            {2}DataType(), dims)
    '''.format(NAME, len(DIMS), SCALAR_TYPE))
    exec('''
class {0}DataSymbol(DataSymbol):
    def __init__({1}):
{2}
        super({0}DataSymbol, self).__init__(
            name, {0}DataType(dims), interface=interface)
    '''.format(NAME, ", ".join(ARGS), "\n".join(VARS)))

# RF TODO DECLARE DIMENSION ORDERING IN CLASSES
#        self.fs_from_dim = {1}
#        self.fs_to_dim = {2}
# RF TODO ADD CORRECT BASIS and DIFF BASIS DIM SIZES

# Generate LFRic vector-field-data symbols as subclasses of field-data symbols
for array_type in field_datatypes:
    NAME = "".join(array_type["name"].title().split())
    VECTOR_NAME = NAME.replace("Field","VectorField")
    exec('''
class {0}DataSymbol({1}DataSymbol):
    pass
    '''.format(VECTOR_NAME, NAME))
