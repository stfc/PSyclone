# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council.
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
# Author R. W. Ford, STFC Daresbury Lab
# Modified I. Kavcic, Met Office

'''This module generates LFRic-specific PSyIR classes from lists of
definitions.

'''
# pylint: disable=unused-import
# pylint: disable=exec-used
from __future__ import absolute_import
from collections import namedtuple
from psyclone.psyir.symbols import ContainerSymbol, DataSymbol, DeferredType, \
    ImportInterface, ScalarType, ArrayType
from psyclone.psyir.nodes import Literal
from psyclone.domain.lfric import LFRicConstants

# Define LFRic module symbols.

# The first Module namedtuple argument specifies the name of the
# module and the second argument declares the name(s) of any symbols
# declared by the module.

Module = namedtuple('Module', ["name", "vars"])
MODULES = [
    Module(LFRicConstants().UTILITIES_MOD_MAP["constants"]["module"],
           ["i_def", "r_def", "l_def"])]

# Generate LFRic module symbols from definitions
for module in MODULES:
    MODULE_NAME = module.name
    # Create the module (using a PSyIR ContainerSymbol)
    exec("{0} = ContainerSymbol('{1}')\n".format(
        MODULE_NAME.upper(), MODULE_NAME))
    # Create the variables specified by the module (using PSyIR DataSymbols)
    for module_var in module.vars:
        exec("{0} = DataSymbol('{1}', DeferredType(), interface="
             "ImportInterface({2}))".format(
                 module_var.upper(), module_var, MODULE_NAME.upper()))

# Define generic LFRic scalar datatypes and symbols

# The GenericScalar namedtuple has 3 properties: the first determines
# the names of the resultant datatype and datasymbol classes, the
# second specifies the intrinsic PSyIR type and the third specifies
# the precision required by referencing symbols already declared
# above.

GenericScalar = namedtuple('GenericScalar', ["name", "intrinsic", "precision"])
GENERIC_SCALAR_DATATYPES = [
    GenericScalar("lfric integer scalar", "integer", "i_def"),
    GenericScalar("lfric real scalar", "real", "r_def"),
    GenericScalar("lfric logical scalar", "boolean", "l_def")]

# Generate generic LFRic scalar datatypes and symbols from definitions
for info in GENERIC_SCALAR_DATATYPES:
    NAME = "".join(info.name.title().split())
    INTRINSIC = info.intrinsic.upper()
    PRECISION = info.precision
    # Create the specific datatype
    exec(
        "class {0}DataType(ScalarType):\n"
        "    def __init__(self, precision=None):\n"
        "        if not precision:\n"
        "            precision = {1}\n"
        "        super({0}DataType, self).__init__(\n"
        "            ScalarType.Intrinsic.{2}, precision)\n"
        "".format(NAME, PRECISION.upper(), INTRINSIC))
    # Create the specific symbol
    exec(
        "class {0}DataSymbol(DataSymbol):\n"
        "    def __init__(self, name, precision=None, **kwargs):\n"
        "        super().__init__(\n"
        "            name, {0}DataType(precision=precision),\n"
        "            **kwargs)\n"
        "".format(NAME))


# Define any LFRic-specific scalar literals
class LfricDimension(Literal):
    '''An Lfric-specific scalar integer that captures a literal array
    dimension which can either have the value 1 or 3. This is used for
    one of the dimensions in basis and differential basis
    functions.

    :param str value: the value of the scalar integer.

    :raises ValueError: if the supplied value is not '1 or '3'.

    '''
    # pylint: disable=undefined-variable
    def __init__(self, value):
        super(LfricDimension, self).__init__(
            value, LfricIntegerScalarDataType())
        if value not in ['1', '3']:
            raise ValueError(
                "An LFRic dimension object must be '1' or '3', but "
                "found '{0}'.".format(value))


LFRIC_SCALAR_DIMENSION = LfricDimension("1")
LFRIC_VECTOR_DIMENSION = LfricDimension("3")

# Define specific LFRic scalar datatypes and symbols

# The Scalar namedtuple has 3 properties: the first
# determines the names of the resultant datatype and datasymbol
# classes, the second references the generic scalar type
# classes declared above and the third specifies any
# additional class properties that should be declared in the generated
# datasymbol class.

Scalar = namedtuple('Scalar', ["name", "generic_type", "properties"])
SPECIFIC_SCALAR_DATATYPES = [
    Scalar("cell position", "lfric integer scalar", []),
    Scalar("mesh height", "lfric integer scalar", []),
    Scalar("number of cells", "lfric integer scalar", []),
    Scalar("number of dofs", "lfric integer scalar", ["fs"]),
    Scalar("number of unique dofs", "lfric integer scalar", ["fs"]),
    Scalar("number of faces", "lfric integer scalar", []),
    Scalar("number of edges", "lfric integer scalar", []),
    Scalar("number of qr points in xy", "lfric integer scalar", []),
    Scalar("number of qr points in z", "lfric integer scalar", []),
    Scalar("number of qr points in faces", "lfric integer scalar", []),
    Scalar("number of qr points in edges", "lfric integer scalar", [])]

# Generate specific LFRic scalar datatypes and symbols from definitions
for info in SPECIFIC_SCALAR_DATATYPES:
    NAME = "".join(info.name.title().split())
    TYPE = "".join(info.generic_type.title().split())
    ARGS = ["self", "name"] + info.properties
    VARS = ["        self.{0} = {0}".format(var) for var in info.properties]
    # Create the specific datatype
    exec(
        "class {0}DataType({1}DataType):\n"
        "    pass\n"
        "".format(NAME, TYPE))
    # Create the specific symbol
    exec(
        "class {0}DataSymbol({1}DataSymbol):\n"
        "    def __init__({2}, **kwargs):\n"
        "{3}\n"
        "        super().__init__(name, **kwargs)\n"
        "".format(NAME, TYPE, ", ".join(ARGS), "\n".join(VARS)))

# Define LFRic field datatypes and symbols

# Note, field_datatypes are no different to array_datatypes and are
# treated in the same way. They are only separated into a different
# list because they are used to create vector field datatypes and
# symbols.

# The Array namedtuple has 4 properties: the first determines the
# names of the resultant datatype and datasymbol classes, the second
# references the generic scalar type classes declared above, the third
# specifies the dimensions of the array by specifying a list of scalar
# type classes declared above, and the fourth specifies any additional
# class properties that should be declared in the generated datasymbol
# class.

Array = namedtuple('Array', ["name", "scalar_type", "dims", "properties"])
FIELD_DATATYPES = [
    Array("real field data", "lfric real scalar", ["number of unique dofs"],
          ["fs"]),
    Array("integer field data", "lfric integer scalar",
          ["number of unique dofs"], ["fs"]),
    Array("logical field data", "lfric logical scalar",
          ["number of unique dofs"], ["fs"])]

# Define all other LFRic array datatypes and symbols

# TBD: #918 the dimension datatypes and their ordering is captured in
# field_datatypes and array_datatypes but is not stored in the
# generated classes.

# TBD: #926 attributes will be constrained to certain datatypes and
# values. For example, a function space attribute should be a string
# containing the name of a supported function space. These are not
# currently checked.

# TBD: #927 in some cases the values of attributes can be inferred, or
# at least must be consistent. For example, a field datatype has an
# associated function space attribute, its dimension symbol (if there
# is one) must be a NumberOfUniqueDofsDataSymbol which also has a
# function space attribute and the two function spaces must be
# the same. This is not curently checked.

ARRAY_DATATYPES = [
    Array("operator", "lfric real scalar",
          ["number of dofs", "number of dofs", "number of cells"],
          ["fs_from", "fs_to"]),
    Array("dof map", "lfric integer scalar", ["number of dofs"], ["fs"]),
    Array("basis function qr xyoz", "lfric real scalar",
          [LfricDimension, "number of dofs",
           "number of qr points in xy",
           "number of qr points in z"], ["fs"]),
    Array("basis function qr face", "lfric real scalar",
          [LfricDimension, "number of dofs", "number of qr points in faces",
           "number of faces"], ["fs"]),
    Array("basis function qr edge", "lfric real scalar",
          [LfricDimension, "number of dofs", "number of qr points in edges",
           "number of edges"], ["fs"]),
    Array("diff basis function qr xyoz", "lfric real scalar",
          [LfricDimension, "number of dofs",
           "number of qr points in xy",
           "number of qr points in z"], ["fs"]),
    Array("diff basis function qr face", "lfric real scalar",
          [LfricDimension, "number of dofs", "number of qr points in faces",
           "number of faces"], ["fs"]),
    Array("diff basis function qr edge", "lfric real scalar",
          [LfricDimension, "number of dofs", "number of qr points in edges",
           "number of edges"], ["fs"]),
    Array("qr weights in xy", "lfric real scalar",
          ["number of qr points in xy"], []),
    Array("qr weights in z", "lfric real scalar",
          ["number of qr points in z"], []),
    Array("qr weights in faces", "lfric real scalar",
          ["number of qr points in faces"], []),
    Array("qr weights in edges", "lfric real scalar",
          ["number of qr points in edges"], [])]

# Generate LFRic array (including field) datatypes and symbols from definitions
for array_type in ARRAY_DATATYPES + FIELD_DATATYPES:
    NAME = "".join(array_type.name.title().split())
    DIMS = array_type.dims
    SCALAR_TYPE = "".join(array_type.scalar_type.title().split())
    ARGS = (["self", "name", "dims"] + array_type.properties)
    VARS = ["        self.{0} = {0}".format(var) for var in
            array_type.properties]
    # Create the specific datatype
    exec(
        "class {0}DataType(ArrayType):\n"
        "    def __init__(self, dims):\n"
        "        if (len(dims) != {2}):\n"
        "            raise TypeError(\n"
        "                \"{0}DataType expected the number of supplied \"\n"
        "                \"dimensions to be {2} but found {{0}}.\"\n"
        "                \"\".format(len(dims)))\n"
        "        super({0}DataType, self).__init__(\n"
        "            {1}DataType(), dims)\n"
        "".format(NAME, SCALAR_TYPE, len(DIMS)))
    # Create the specific symbol
    exec(
        "class {0}DataSymbol(DataSymbol):\n"
        "    def __init__({1}, **kwargs):\n"
        "{2}\n"
        "        super().__init__(name, {0}DataType(dims),  **kwargs)\n"
        "".format(NAME, ", ".join(ARGS), "\n".join(VARS)))

# Generate LFRic vector-field-data symbols as subclasses of field-data symbols
for array_type in FIELD_DATATYPES:
    NAME = "".join(array_type.name.title().split())
    VECTOR_NAME = NAME.replace("Field", "VectorField")
    exec(
        "class {0}DataSymbol({1}DataSymbol):\n"
        "    pass\n"
        "".format(VECTOR_NAME, NAME))
