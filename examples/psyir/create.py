# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council
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
# Author: R. W. Ford, STFC Daresbury Lab
# Modifications: A. R. Porter, STFC Daresbury Lab

'''A simple Python script showing how to create a PSyIR tree using the
create methods. In order to use it you must first install
PSyclone. See README.md in the top-level psyclone directory.

Once you have psyclone installed, this script may be run by doing:

>>> python create.py

This should output a Fortran representation of the PSyIR and part of a
C representation of the PSyIR.

'''
from __future__ import print_function
from psyclone.psyir.nodes import Reference, Literal, UnaryOperation, \
    BinaryOperation, NaryOperation, Assignment, IfBlock, Loop, \
    Container, Range, Array
from psyclone.psyGen import KernelSchedule
from psyclone.psyir.symbols import DataSymbol, SymbolTable, ContainerSymbol, \
    ArgumentInterface, ScalarType, ArrayType, GlobalInterface, REAL_TYPE, \
    REAL4_TYPE, REAL_DOUBLE_TYPE, INTEGER_TYPE, INTEGER_SINGLE_TYPE, \
    INTEGER4_TYPE, INTEGER8_TYPE
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.backend.c import CWriter

# Symbol table, symbols and scalar datatypes
SYMBOL_TABLE = SymbolTable()
TMP_NAME1 = SYMBOL_TABLE.new_symbol_name()
ARG1 = DataSymbol(TMP_NAME1, REAL_TYPE, interface=ArgumentInterface(
    ArgumentInterface.Access.READWRITE))
SYMBOL_TABLE.add(ARG1)
TMP_NAME2 = SYMBOL_TABLE.new_symbol_name()
TMP_SYMBOL = DataSymbol(TMP_NAME2, REAL_DOUBLE_TYPE)
SYMBOL_TABLE.add(TMP_SYMBOL)
INDEX_NAME = SYMBOL_TABLE.new_symbol_name(root_name="i")
SYMBOL_TABLE.add(DataSymbol(INDEX_NAME, INTEGER4_TYPE))
SYMBOL_TABLE.specify_argument_list([ARG1])
REAL_KIND_NAME = SYMBOL_TABLE.new_symbol_name(root_name="RKIND")
REAL_KIND = DataSymbol(REAL_KIND_NAME, INTEGER_TYPE, constant_value=8)
SYMBOL_TABLE.add(REAL_KIND)

# Array using precision defined by another symbol
ARRAY_NAME = SYMBOL_TABLE.new_symbol_name(root_name="a")
SCALAR_TYPE = ScalarType(ScalarType.Intrinsic.REAL, REAL_KIND)
ARRAY = DataSymbol(ARRAY_NAME, ArrayType(SCALAR_TYPE, [10]))
SYMBOL_TABLE.add(ARRAY)

# Nodes which do not have Nodes as children and (some) predefined
# scalar datatypes
ZERO = Literal("0.0", REAL_TYPE)
ONE = Literal("1.0", REAL4_TYPE)
TWO = Literal("2.0", SCALAR_TYPE)
INT_ZERO = Literal("0", INTEGER_SINGLE_TYPE)
INT_ONE = Literal("1", INTEGER8_TYPE)
TMP1 = Reference(ARG1)
TMP2 = Reference(TMP_SYMBOL)

# Unary Operation
OPER = UnaryOperation.Operator.SIN
UNARYOPERATION = UnaryOperation.create(OPER, TMP2)

# Binary Operation
OPER = BinaryOperation.Operator.ADD
BINARYOPERATION = BinaryOperation.create(OPER, ONE, UNARYOPERATION)

# Nary Operation
OPER = NaryOperation.Operator.MAX
NARYOPERATION = NaryOperation.create(OPER, [TMP1, TMP2, ONE])

# Array reference using a range
LBOUND = BinaryOperation.create(
    BinaryOperation.Operator.LBOUND,
    Reference(ARRAY), INT_ONE)
UBOUND = BinaryOperation.create(
    BinaryOperation.Operator.UBOUND,
    Reference(ARRAY), INT_ONE)
MY_RANGE = Range.create(LBOUND, UBOUND)
TMPARRAY = Array.create(ARRAY, [MY_RANGE])

# Assignments
ASSIGN1 = Assignment.create(TMP1, ZERO)
ASSIGN2 = Assignment.create(TMP2, ZERO)
ASSIGN3 = Assignment.create(TMP2, BINARYOPERATION)
ASSIGN4 = Assignment.create(TMP1, TMP2)
ASSIGN5 = Assignment.create(TMP1, NARYOPERATION)
ASSIGN6 = Assignment.create(TMPARRAY, TWO)

# If statement
IF_CONDITION = BinaryOperation.create(BinaryOperation.Operator.GT, TMP1, ZERO)
IFBLOCK = IfBlock.create(IF_CONDITION, [ASSIGN3, ASSIGN4])

# Loop
LOOP = Loop.create(INDEX_NAME, INT_ZERO, INT_ONE, INT_ONE, [IFBLOCK])

# KernelSchedule
KERNEL_SCHEDULE = KernelSchedule.create("work", SYMBOL_TABLE,
                                        [ASSIGN2, LOOP, ASSIGN5, ASSIGN6])

# Container
CONTAINER_SYMBOL_TABLE = SymbolTable()
CONTAINER = Container.create("CONTAINER", CONTAINER_SYMBOL_TABLE,
                             [KERNEL_SCHEDULE])

# Import data from another container
EXTERNAL_CONTAINER = ContainerSymbol("some_mod")
EXTERNAL_VAR = DataSymbol("some_var", INTEGER_TYPE,
                          interface=GlobalInterface(EXTERNAL_CONTAINER))
CONTAINER_SYMBOL_TABLE.add(EXTERNAL_CONTAINER)

# Write out the code as Fortran
WRITER = FortranWriter()
RESULT = WRITER(CONTAINER)
print(RESULT)

# Write out the code as C. At the moment NaryOperator, KernelSchedule
# and Container are not supported in the C backend so the full example
# can't be output.
WRITER = CWriter()
RESULT = WRITER(LOOP)
print(RESULT)
