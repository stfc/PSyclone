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
definitions.

'''
# pylint: disable=unused-import
# pylint: disable=exec-used
from psyclone.psyir.symbols import ContainerSymbol, DataSymbol, \
    DeferredType, GlobalInterface, ScalarType, ArrayType
from collections import namedtuple  

# Define LFRic module symbols
Module = namedtuple('Module',["name","vars"])
modules = [
    Module("constants_mod", ["i_def", "r_def", "l_def"])]

# Generate LFRic module symbols from definitions
for module in modules:
    MODULE_NAME = module.name
    exec("{0} = ContainerSymbol('{1}')\n".format(
        MODULE_NAME.upper(), MODULE_NAME))
    for module_var in module.vars:
        exec("{0} = DataSymbol('{1}', DeferredType(), interface="
             "GlobalInterface({2}))".format(
                 module_var.upper(), module_var, MODULE_NAME.upper()))

# Define generic LFRic scalar datatypes and symbols
ScalarDatatype = namedtuple('ScalarDatatype',["name", "intrinsic", "precision"])
generic_scalar_datatypes = [
    ScalarDatatype("lfric integer scalar", "integer", "i_def"),
    ScalarDatatype("lfric real scalar", "real", "r_def"),
    ScalarDatatype("lfric logical scalar", "boolean", "l_def")]

# Generate generic LFRic scalar datatypes and symbols
for info in generic_scalar_datatypes:
    NAME = "".join(info.name.title().split())
    INTRINSIC = info.intrinsic.upper()
    PRECISION = info.precision
    ARGS = ["self", "name", "interface=None"]
    exec(
        "class {0}DataType(ScalarType):\n"
        "    def __init__(self):\n"
        "        super({0}DataType, self).__init__(\n"
        "            ScalarType.Intrinsic.{1}, {2})\n"
        "".format(NAME, INTRINSIC, PRECISION.upper()))
    exec(
        "class {0}DataSymbol(DataSymbol):\n"
        "    def __init__(self, name, interface=None):\n"
        "        super({0}DataSymbol, self).__init__(\n"
        "            name, {0}DataType(), interface=interface)\n"
        "".format(NAME))

# Define specific LFRic scalar datatypes and symbols
ScalarDatatype = namedtuple('ScalarDatatype',["name", "intrinsic", "precision"])
specific_scalar_datatypes = [
    {"name": "cell position", "type": "lfric integer scalar", "vars": []},
    {"name": "mesh height", "type": "lfric integer scalar", "vars": []},
    {"name": "number of cells", "type": "lfric integer scalar", "vars": []},
    {"name": "number of dofs", "type": "lfric integer scalar", "vars": ["fs"]},
    {"name": "number of unique dofs", "type": "lfric integer scalar",
     "vars": ["fs"]},
    {"name": "number of faces", "type": "lfric integer scalar", "vars": []},
    {"name": "number of edges", "type": "lfric integer scalar", "vars": []},
    {"name": "number of qr points in horizontal",
     "type": "lfric integer scalar", "vars": []},
    {"name": "number of qr points in vertical",
     "type": "lfric integer scalar", "vars": []},
    {"name": "number of qr points", "type": "lfric integer scalar",
     "vars": []}]

# Generate specific LFRic scalar datatypes and symbols
for info in specific_scalar_datatypes:
    NAME = "".join(info["name"].title().split())
    TYPE = "".join(info["type"].title().split())
    ARGS = ["self", "name"] + info["vars"] + ["interface=None"]
    VARS = ["        self.{0} = {0}".format(var) for var in info["vars"]]
    exec(
        "class {0}DataType({1}DataType):\n"
        "    pass\n"
        "".format(NAME, TYPE))
    exec(
        "class {0}DataSymbol({1}DataSymbol):\n"
        "    def __init__({2}):\n"
        "{3}\n"
        "        super({0}DataSymbol, self).__init__(\n"
        "            name, interface=interface)\n"
        "".format(NAME, TYPE, ", ".join(ARGS), "\n".join(VARS)))

# Define LFRic field datatypes and symbols
# Note, field_datatypes are no different to array_datatypes and are
# treated in the same way. They are only separated into a different
# list because they are used to create vector field datatypes and
# symbols.
field_datatypes = [
    {"name": "real field data", "scalar_type": "lfric real scalar",
     "dims": ["number of unique dofs"], "vars": ["fs"]},
    {"name": "integer field data", "scalar_type": "lfric integer scalar",
     "dims": ["number of unique dofs"], "vars": ["fs"]},
    {"name": "logical field data", "scalar_type": "lfric logical scalar",
     "dims": ["number of unique dofs"], "vars": ["fs"]}]

# Define all other LFRic array datatypes and symbols
# TBD: #xxx the dimension datatypes and their ordering is captured in
# field_datatypes and array_datatypes but is not stored in the
# generated classes.
array_datatypes = [
    {"name": "operator", "scalar_type": "lfric real scalar",
     "dims": ["number of dofs", "number of dofs", "number of cells"],
     "vars": ["fs_from", "fs_to"]},
    # "dim_info": {"fs_from": 0, "fs_to": 1, "ncells": 2}},
    {"name": "dof map", "scalar_type": "lfric integer scalar",
     "dims": ["number of dofs"], "vars": ["fs"]},
    {"name": "basis function qr xyoz", "scalar_type": "lfric real scalar",
     "dims": ["lfric integer scalar", "number of dofs",
              "number of qr points in horizontal",
              "number of qr points in vertical"],
     "vars": ["fs"]},
    {"name": "basis function qr face", "scalar_type": "lfric real scalar",
     "dims": ["lfric integer scalar", "number of dofs", "number of qr points",
              "number of faces"],
     "vars": ["fs"]},
    {"name": "basis function qr edge", "scalar_type": "lfric real scalar",
     "dims": ["lfric integer scalar", "number of dofs", "number of qr points",
              "number of edges"],
     "vars": ["fs"]},
    {"name": "diff basis function qr xyoz", "scalar_type": "lfric real scalar",
     "dims": ["lfric integer scalar", "number of dofs",
              "number of qr points in horizontal",
              "number of qr points in vertical"],
     "vars": ["fs"]},
    {"name": "diff basis function qr face", "scalar_type": "lfric real scalar",
     "dims": ["lfric integer scalar", "number of dofs", "number of qr points",
              "number of faces"],
     "vars": ["fs"]},
    {"name": "diff basis function qr edge", "scalar_type": "lfric real scalar",
     "dims": ["lfric integer scalar", "number of dofs", "number of qr points",
              "number of edges"],
     "vars": ["fs"]},
    {"name": "qr weights in horizontal", "scalar_type": "lfric real scalar",
     "dims": ["number of qr points in horizontal"], "vars": []},
    {"name": "qr weights in vertical", "scalar_type": "lfric real scalar",
     "dims": ["number of qr points in vertical"], "vars": []},
    {"name": "qr weights", "scalar_type": "lfric real scalar",
     "dims": ["number of qr points"], "vars": []}]

# Generate LFRic array (including field) datatypes and symbols
for array_type in array_datatypes + field_datatypes:
    NAME = "".join(array_type["name"].title().split())
    DIMS = array_type["dims"]
    SCALAR_TYPE = "".join(array_type["scalar_type"].title().split())
    ARGS = ["self", "name", "dims"] + array_type["vars"] + ["interface=None"]
    VARS = ["        self.{0} = {0}".format(var) for var in array_type["vars"]]
    exec('''
class {0}DataType(ArrayType):
    def __init__(self, dims):
        super({0}DataType, self).__init__(
            {1}DataType(), dims)
    '''.format(NAME, SCALAR_TYPE))
    exec(
        "class {0}DataSymbol(DataSymbol):\n"
        "    def __init__({1}):\n"
        "        if (len(dims) != {3}):\n"
        "            raise Exception(\n"
        "                \"{0}DataSymbol expected the number of supplied \"\n"
        "                \"dimensions to be {3} but found {{0}}\"\n"
        "                \"\".format(len(dims)))\n"
        "{2}\n"
        "        super({0}DataSymbol, self).__init__(\n"
        "            name, {0}DataType(dims), interface=interface)\n"
        "".format(NAME, ", ".join(ARGS), "\n".join(VARS), len(DIMS)))

# Generate LFRic vector-field-data symbols as subclasses of field-data symbols
for array_type in field_datatypes:
    NAME = "".join(array_type["name"].title().split())
    VECTOR_NAME = NAME.replace("Field", "VectorField")
    exec('''
class {0}DataSymbol({1}DataSymbol):
    pass
    '''.format(VECTOR_NAME, NAME))
