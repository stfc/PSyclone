# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2022, Science and Technology Facilities Council
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
# Author: A. R. Porter, STFC Daresbury Lab
# Modified: R. W. Ford, STFC Daresbury Lab

'''A Python script showing how to create and manipulate symbols of structure
type within the PSyIR. In order to use it you must first install PSyclone.
See README.md in the top-level psyclone directory.

Once you have psyclone installed, this script may be run by doing:

>>> python create_structure_types.py

'''
from __future__ import print_function
from psyclone.psyir.nodes import Literal, KernelSchedule, Container, \
    StructureReference, ArrayOfStructuresReference, Assignment, \
    BinaryOperation, Range
from psyclone.psyir.symbols import DataSymbol, SymbolTable, StructureType, \
    ContainerSymbol, ArgumentInterface, ScalarType, ArrayType, DataTypeSymbol,\
    ImportInterface, INTEGER_TYPE, INTEGER4_TYPE, INTEGER8_TYPE, \
    DeferredType, Symbol
from psyclone.psyir.backend.fortran import FortranWriter


# Symbol table for container (container itself created after kernel)
CONTAINER_SYMBOL_TABLE = SymbolTable()
REAL_KIND = CONTAINER_SYMBOL_TABLE.new_symbol(
        root_name="RKIND", symbol_type=DataSymbol, datatype=INTEGER_TYPE,
        constant_value=8)

# Shorthand for a scalar type with REAL_KIND precision
SCALAR_TYPE = ScalarType(ScalarType.Intrinsic.REAL, REAL_KIND)

# Derived-type definition in container
GRID_TYPE = StructureType.create([
    ("dx", SCALAR_TYPE, Symbol.Visibility.PUBLIC),
    ("dy", SCALAR_TYPE, Symbol.Visibility.PUBLIC)])
GRID_TYPE_SYMBOL = DataTypeSymbol("grid_type", GRID_TYPE)
CONTAINER_SYMBOL_TABLE.add(GRID_TYPE_SYMBOL)

# Kernel symbol table, symbols and scalar datatypes
SYMBOL_TABLE = SymbolTable()

CONT = ContainerSymbol("kernel_mod")
SYMBOL_TABLE.add(CONT)

DTYPE_SYMBOL = DataTypeSymbol("other_type", DeferredType(),
                              interface=ImportInterface(CONT))
SYMBOL_TABLE.add(DTYPE_SYMBOL)

# Create the definition of the 'field_type'
FIELD_TYPE_DEF = StructureType.create(
    [("data", ArrayType(SCALAR_TYPE, [10]), Symbol.Visibility.PUBLIC),
     ("grid", GRID_TYPE_SYMBOL, Symbol.Visibility.PUBLIC),
     ("sub_meshes", ArrayType(GRID_TYPE_SYMBOL, [3]),
      Symbol.Visibility.PUBLIC),
     ("flag", INTEGER4_TYPE, Symbol.Visibility.PUBLIC)])
FIELD_TYPE_SYMBOL = DataTypeSymbol("field_type", FIELD_TYPE_DEF)
CONTAINER_SYMBOL_TABLE.add(FIELD_TYPE_SYMBOL)

# Create an argument of this derived type. At this point we know only that
# DTYPE_SYMBOL refers to a type defined in the CONT container.
FIELD_SYMBOL = DataSymbol("wind", FIELD_TYPE_SYMBOL,
                          interface=ArgumentInterface(
                              ArgumentInterface.Access.READWRITE))
SYMBOL_TABLE.add(FIELD_SYMBOL)
SYMBOL_TABLE.specify_argument_list([FIELD_SYMBOL])

# Create an array of these derived types
FIELD_BUNDLE_SYMBOL = DataSymbol("chi", ArrayType(FIELD_TYPE_SYMBOL, [3]))
SYMBOL_TABLE.add(FIELD_BUNDLE_SYMBOL)

print("Container Symbol Table:")
print(str(CONTAINER_SYMBOL_TABLE))

print("Kernel Symbol Table:")
print(str(SYMBOL_TABLE))

INDEX_SYMBOL = SYMBOL_TABLE.new_symbol(root_name="i", symbol_type=DataSymbol,
                                       datatype=INTEGER4_TYPE)


# Some predefined scalar literal nodes
def real_two():
    ''' Generate a Literal'''
    return Literal("2.0", SCALAR_TYPE)


def int_one():
    ''' Generate a Literal'''
    return Literal("1", INTEGER8_TYPE)


# Reference to the "flag" scalar component of FIELD_SYMBOL, "field%flag"
FLAG_REF = StructureReference.create(FIELD_SYMBOL, ["flag"])

# Reference to "field%grid%dx"
DX_REF = StructureReference.create(FIELD_SYMBOL, ["grid", "dx"])

# Array reference to component of derived type using a range
LBOUND = BinaryOperation.create(
    BinaryOperation.Operator.LBOUND,
    StructureReference.create(FIELD_SYMBOL, ["data"]), int_one())
UBOUND = BinaryOperation.create(
    BinaryOperation.Operator.UBOUND,
    StructureReference.create(FIELD_SYMBOL, ["data"]), int_one())
MY_RANGE = Range.create(LBOUND, UBOUND)

DATA_REF = StructureReference.create(FIELD_SYMBOL, [("data", [MY_RANGE])])

# Reference to "field%sub_meshes(1)%dx"
DX_REF2 = StructureReference.create(FIELD_SYMBOL, [("sub_meshes", [int_one()]),
                                                   "dx"])

# Reference to "chi(1)%sub_meshes(1)%dx"
DX_REF3 = ArrayOfStructuresReference.create(
    FIELD_BUNDLE_SYMBOL, [int_one()], [("sub_meshes", [int_one()]), "dx"])

ASSIGNMENTS = [
    Assignment.create(DX_REF, real_two()),
    Assignment.create(FLAG_REF, int_one()),
    Assignment.create(DATA_REF, real_two()),
    Assignment.create(DX_REF2, real_two()),
    Assignment.create(DX_REF3, real_two())]

# KernelSchedule
KERNEL_SCHEDULE = KernelSchedule.create("work", SYMBOL_TABLE, ASSIGNMENTS)

# Container
CONTAINER = Container.create("CONTAINER", CONTAINER_SYMBOL_TABLE,
                             [KERNEL_SCHEDULE])
print(CONTAINER.view())

# Write out the code as Fortran.
WRITER = FortranWriter()
RESULT = WRITER(CONTAINER)
print("\n")
print(RESULT)
