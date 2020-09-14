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
modules = [{"name": "constants_mod", "vars": ["i_def", "r_def"]}]

scalar_datatypes = [
    {"name": "lfric integer scalar", "intrinsic": "integer",
     "precision": "i_def"},
    {"name": "lfric real scalar", "intrinsic": "real",
     "precision": "r_def", "short_name": "real_scalar"},
    {"name": "cell position", "intrinsic": "integer",
     "precision": "i_def", "short_name": "nlayers"},
    {"name": "mesh height", "intrinsic": "integer",
     "precision": "i_def", "short_name": "nlayers"},
    {"name": "operator size", "intrinsic": "integer",
     "precision": "i_def", "short_name": "ncell_3d"},
    {"name": "number of dofs", "intrinsic": "integer",
     "precision": "i_def", "short_name": "ndf", "fs": True},
    {"name": "number of unique dofs", "intrinsic": "integer",
     "precision": "i_def", "short_name": "undf", "fs": True}]

array_datatypes = [
    {"name": "field data", "scalar_type": "lfric real scalar",
     "dims": ["number of unique dofs"], "fs": True},
    {"name": "operator", "scalar_type": "lfric real scalar",
     "dims": ["ndf", "ndf", "ncell_3d"], "fs_from_to": [0, 1]},
    {"name": "dof map", "scalar_type": "lfric integer scalar",
     "dims": ["ndf"], "fs":True}]

# Generate LFRic module symbols from definitions
for module in modules:
    MODULE_NAME = module["name"]
    exec("{0} = ContainerSymbol('{1}')\n".format(
        MODULE_NAME.upper(), MODULE_NAME))
    for module_var in module["vars"]:
        exec("{0} = DataSymbol('{1}', DeferredType(), interface="
             "GlobalInterface({2}))".format(
                 module_var.upper(), module_var, MODULE_NAME.upper()))

# Generate LFRic scalar datatypes and symbols from definitions
for scalar_type in scalar_datatypes:
    NAME = "".join(scalar_type["name"].title().split())
    INTRINSIC = scalar_type["intrinsic"].upper()
    PRECISION = scalar_type["precision"]
    try:
        FUNCTION_SPACE = scalar_type["fs"]
    except KeyError:
        FUNCTION_SPACE = False
    exec('''
class {0}DataType(ScalarType):
    def __init__(self):
        super({0}DataType, self).__init__(
            ScalarType.Intrinsic.{1}, {2})
'''.format(NAME, INTRINSIC, PRECISION.upper()))
    if FUNCTION_SPACE:
        exec('''
class {0}DataSymbol(DataSymbol):
    def __init__(self, name, fs, interface=None):
        self.fs = fs
        super({0}DataSymbol, self).__init__(
            name, {0}DataType(), interface=interface)
        '''.format(NAME))
    else:
        exec('''
class {0}DataSymbol(DataSymbol):
    def __init__(self, name, interface=None):
        super({0}DataSymbol, self).__init__(
            name, {0}DataType(), interface=interface)
        '''.format(NAME))

# Generate LFRic array datatypes and symbols from definitions
for array_type in array_datatypes:
    NAME = "".join(array_type["name"].title().split())
    DIMS = array_type["dims"]
    SCALAR_TYPE = "".join(array_type["scalar_type"].title().split())
    try:
        FUNCTION_SPACE = array_type["fs"]
    except KeyError:
        FUNCTION_SPACE = False
    try:
        FS_FROM_TO = array_type["fs_from_to"]
    except KeyError:
        FS_FROM_TO = None
    exec('''
class {0}DataType(ArrayType):
    def __init__(self, dims):
        assert (len(dims) == {1}), ('Error, expected {1} dimension(s) but '
            'got {{0}}'.format(len(dims)))
        # TBD check type of dims ...
        super({0}DataType, self).__init__(
            {2}DataType(), dims)
    '''.format(NAME, len(DIMS), SCALAR_TYPE))
    if FUNCTION_SPACE:
        exec('''
class {0}DataSymbol(DataSymbol):
    def __init__(self, name, dims, fs, interface=None):
        self.fs = fs
        super({0}DataSymbol, self).__init__(
            name, {0}DataType(dims), interface=interface)
        '''.format(NAME))
    elif FS_FROM_TO:
        print ("FS_FROM_TO")
        exec('''
class {0}DataSymbol(DataSymbol):
    def __init__(self, name, dims, fs_from, fs_to, interface=None):
        self.fs_from = fs_from
        self.fs_to = fs_to
        self.fs_from_dim = {1}
        self.fs_to_dim = {2}
        super({0}DataSymbol, self).__init__(
            name, {0}DataType(dims), interface=interface)
        '''.format(NAME, FS_FROM_TO[0], FS_FROM_TO[1]))
    else:
        exec('''
class {0}DataSymbol(DataSymbol):
    def __init__(self, name, dims, interface=None):
        super({0}DataSymbol, self).__init__(
            name, {0}DataType(dims), interface=interface)
        '''.format(NAME))
