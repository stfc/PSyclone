# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council
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

'''A simple Python script showing how to create and manipulate
aggregate types within the PSyIR. In order to use it you must first install
PSyclone. See README.md in the top-level psyclone directory.

Once you have psyclone installed, this script may be run by doing:

>>> python create_aggregate_types.py

TODO #363 Ultimately this will output a Fortran representation of the PSyIR but
this is a work in progress.

'''
from __future__ import print_function
# TODO #363 these classes will be needed once the implementation is
# complete.
# from psyclone.psyir.nodes import Array, Assignment, BinaryOperation, Range, \
#    Reference
from psyclone.psyir.nodes import Literal, KernelSchedule, Container
from psyclone.psyir.symbols import DataSymbol, SymbolTable, StructureType, \
    ContainerSymbol, ArgumentInterface, ScalarType, ArrayType, TypeSymbol, \
    GlobalInterface, INTEGER_TYPE, INTEGER4_TYPE, INTEGER8_TYPE, \
    DeferredType, Symbol
# TODO #363 once the PSyIR support is complete we will write out Fortran.
# from psyclone.psyir.backend.fortran import FortranWriter


# Symbol table for container (container itself created after kernel)
CONTAINER_SYMBOL_TABLE = SymbolTable()
REAL_KIND_NAME = CONTAINER_SYMBOL_TABLE.new_symbol_name(root_name="RKIND")
REAL_KIND = DataSymbol(REAL_KIND_NAME, INTEGER_TYPE, constant_value=8)
CONTAINER_SYMBOL_TABLE.add(REAL_KIND)

# Shorthand for a scalar type with REAL_KIND precision
SCALAR_TYPE = ScalarType(ScalarType.Intrinsic.REAL, REAL_KIND)

# Derived-type definition in container
GRID_TYPE = StructureType.create([
    ("dx", ArrayType(SCALAR_TYPE, [10]), Symbol.Visibility.PUBLIC),
    ("dy", ArrayType(SCALAR_TYPE, [10]), Symbol.Visibility.PUBLIC)])
GRID_TYPE_SYMBOL = TypeSymbol("grid_type", GRID_TYPE)
CONTAINER_SYMBOL_TABLE.add(GRID_TYPE_SYMBOL)

# Kernel symbol table, symbols and scalar datatypes
SYMBOL_TABLE = SymbolTable()

CONT = ContainerSymbol("kernel_mod")
SYMBOL_TABLE.add(CONT)

DTYPE_SYMBOL = TypeSymbol("field_type", DeferredType(),
                          interface=GlobalInterface(CONT))
SYMBOL_TABLE.add(DTYPE_SYMBOL)
# Create an argument of this derived type. At this point we know only that
# DTYPE_SYMBOL refers to a type defined in the CONT container.
FIELD_SYMBOL = DataSymbol("wind", DTYPE_SYMBOL,
                          interface=ArgumentInterface(
                              ArgumentInterface.Access.READWRITE))
SYMBOL_TABLE.add(FIELD_SYMBOL)
SYMBOL_TABLE.specify_argument_list([FIELD_SYMBOL])

# For now we can't do much more than print out the symbol tables as
# there's a lot of functionality still to implement.
# TODO #363 remove these prints and update example to use the Fortran
# backend.
print("Kernel Symbol Table:")
print(str(SYMBOL_TABLE))
print("Container Symbol Table:")
print(str(CONTAINER_SYMBOL_TABLE))

INDEX_NAME = SYMBOL_TABLE.new_symbol_name(root_name="i")
INDEX_SYMBOL = DataSymbol(INDEX_NAME, INTEGER4_TYPE)
SYMBOL_TABLE.add(INDEX_SYMBOL)

# Symbol representing a component of FIELD_SYMBOL. The name "data" must exist
# in the type definition associated with FIELD_SYMBOL.
# TODO #363 implement ComponentSymbol
# DATA_SYMBOL = ComponentSymbol(FIELD_SYMBOL, "data")

# Some predefined scalar datatypes
TWO = Literal("2.0", SCALAR_TYPE)
INT_ONE = Literal("1", INTEGER8_TYPE)

# Array reference to component of derived type using a range
# TODO #363 implement ComponentSymbol
# LBOUND = BinaryOperation.create(
#    BinaryOperation.Operator.LBOUND,
#    Reference(DATA_SYMBOL), INT_ONE)
# TODO #363 implement ComponentSymbol
# UBOUND = BinaryOperation.create(
#    BinaryOperation.Operator.UBOUND,
#    Reference(DATA_SYMBOL), INT_ONE)
# TODO #363 implement ComponentSymbol
# MY_RANGE = Range.create(LBOUND, UBOUND)

# TODO #363 implement ComponentSymbol
# TMPARRAY = Array.create(DATA_SYMBOL, [MY_RANGE])

# Routine consists of a single Assignment to the "data" component of the
# "wind" argument.
# TODO #363 implement ComponentSymbol
# ASSIGN = Assignment.create(TMPARRAY, TWO)

# KernelSchedule
# TODO #363 implement ComponentSymbol - put ASSIGN into list of children
# passed to create().
KERNEL_SCHEDULE = KernelSchedule.create(
    "work", SYMBOL_TABLE, [])

# Container
CONTAINER = Container.create("CONTAINER", CONTAINER_SYMBOL_TABLE,
                             [KERNEL_SCHEDULE])

# Write out the code as Fortran.
# TODO #363 Backend does not currently support derived types
# WRITER = FortranWriter()
# RESULT = WRITER(CONTAINER)
# print(RESULT)
