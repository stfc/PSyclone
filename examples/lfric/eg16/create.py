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

This should output a Fortran representation of the LFRic-PSyIR and
part of a C representation of the LFRic-PSyIR.

'''
from __future__ import print_function
from psyclone.psyGen import KernelSchedule
from psyclone.psyir.nodes import Call, Reference, Container
from psyclone.psyir.symbols import RoutineSymbol, SymbolTable
from psyclone.domain.lfric.lfric_ir import constants_mod, \
    NumberOfDofsDataSymbol, FieldDataDataSymbol, i_def, r_def

from psyclone.psyir.backend.fortran import FortranWriter

# Add an LFRic module to the symbol table
SYMBOL_TABLE = SymbolTable()
SYMBOL_TABLE.add(i_def)
SYMBOL_TABLE.add(r_def)
SYMBOL_TABLE.add(constants_mod)
# Create and add an LFRic ndofs symbol to the symbol table
ndf_w3 = NumberOfDofsDataSymbol("ndf_w3")
SYMBOL_TABLE.add(ndf_w3)
# Create and add LFRic field data to the symbol table
field1 = FieldDataDataSymbol("field1", [ndf_w3])
SYMBOL_TABLE.add(field1)

# TBD symbols as arguments - just have an optional arg.

# Symbol table and LFRic symbols and scalar datatypes
ROUTINE_SYMBOL = RoutineSymbol("my_sub")

# Call
CALL = Call.create(ROUTINE_SYMBOL, [Reference(field1)])

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

# Not yet supported in C
