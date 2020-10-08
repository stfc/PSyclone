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
# Author: R. W. Ford, STFC Daresbury Lab

'''A simple Python script showing how to create an LFRic-specific
PSyIR tree using the create methods. In order to use it you must first
install PSyclone. See README.md in the top-level psyclone directory.

Once you have psyclone installed, this script may be run by doing:

>>> python create.py

This should output a Fortran representation of the LFRic-PSyIR.

'''
# pylint: disable=no-name-in-module
from __future__ import print_function
from psyclone.psyir.nodes import Call, Reference, Container, KernelSchedule
from psyclone.psyir.symbols import RoutineSymbol, SymbolTable, \
    ArgumentInterface
from psyclone.domain.lfric.psyir import CONSTANTS_MOD, \
    NumberOfDofsDataSymbol, RealFieldDataDataSymbol, I_DEF, R_DEF, \
    NumberOfCellsDataSymbol, OperatorDataSymbol, NumberOfUniqueDofsDataSymbol, \
    BasisFunctionQrXyozDataSymbol, DiffBasisFunctionQrXyozDataSymbol, \
    NumberOfQrPointsInHorizontalDataSymbol, \
    NumberOfQrPointsInVerticalDataSymbol, QrWeightsInHorizontalDataSymbol, \
    QrWeightsInVerticalDataSymbol

from psyclone.psyir.backend.fortran import FortranWriter

# Add LFRic precision symbols and the module in which they are
# contained to the symbol table
SYMBOL_TABLE = SymbolTable()
for symbol in [I_DEF, R_DEF, CONSTANTS_MOD]:
    SYMBOL_TABLE.add(symbol)

# Create LFRic ndf and undf symbols and add them to the symbol table
NDF_W3 = NumberOfDofsDataSymbol("ndf_w3", "w3")
UNDF_W3 = NumberOfUniqueDofsDataSymbol("undf_w3", "w3")
for symbol in [NDF_W3, UNDF_W3]:
    SYMBOL_TABLE.add(symbol)

# Create LFRic field data symbols and add them to the symbol table
FIELD1 = RealFieldDataDataSymbol("field1", [UNDF_W3], "w3")
FIELD2 = RealFieldDataDataSymbol(
    "field2", [UNDF_W3], "w3",
    interface=ArgumentInterface(ArgumentInterface.Access.READWRITE))
for symbol in [FIELD1, FIELD2]:
    SYMBOL_TABLE.add(symbol)
SYMBOL_TABLE.specify_argument_list([FIELD2])

# Create an LFRic operator and it to the symbol table
NCELL_3D = NumberOfCellsDataSymbol("ncell_3d")
OPERATOR = OperatorDataSymbol("oper1", [NDF_W3, NDF_W3, NCELL_3D],
                              fs_from="w3", fs_to="w3")
for symbol in [NCELL_3D, OPERATOR]:
    SYMBOL_TABLE.add(symbol)

# Create LFRic basis and differential basis functions with gaussian
# quadrature (xyoz) and add them to the symbol table. Also create the
# quadrature weights
NQP_H = NumberOfQrPointsInHorizontalDataSymbol("nqp_h")
NQP_V = NumberOfQrPointsInVerticalDataSymbol("nqp_v")
WEIGHTS_H = QrWeightsInHorizontalDataSymbol("wh", [NQP_H])
WEIGHTS_V = QrWeightsInVerticalDataSymbol("wv", [NQP_V])
BASIS_W3 = BasisFunctionQrXyozDataSymbol(
    "basis_w3", [1, NDF_W3, NQP_H, NQP_V], "w3")
DIFF_BASIS_W3 = DiffBasisFunctionQrXyozDataSymbol(
    "diff_basis_w3", [3, NDF_W3, NQP_H, NQP_V], "w3")
for symbol in [NQP_H, NQP_V, WEIGHTS_H, WEIGHTS_V, BASIS_W3, DIFF_BASIS_W3]:
    SYMBOL_TABLE.add(symbol)

# Routine symbol
ROUTINE_SYMBOL = RoutineSymbol("my_sub")

# Call
CALL = Call.create(ROUTINE_SYMBOL,
                   [Reference(FIELD1), Reference(FIELD2), Reference(OPERATOR)])

# KernelSchedule
KERNEL_SCHEDULE = KernelSchedule.create(
    "work", SYMBOL_TABLE, [CALL])

# Container
CONTAINER_SYMBOL_TABLE = SymbolTable()
CONTAINER = Container.create("CONTAINER", CONTAINER_SYMBOL_TABLE,
                             [KERNEL_SCHEDULE])

# Write out the code as Fortran
WRITER = FortranWriter()
RESULT = WRITER(CONTAINER)
print(RESULT)
