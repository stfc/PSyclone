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
from psyclone.psyGen import KernelSchedule
from psyclone.psyir.nodes import Call, Reference, Container
from psyclone.psyir.symbols import RoutineSymbol, SymbolTable, \
    ArgumentInterface
from psyclone.domain.lfric.psyir import CONSTANTS_MOD, \
    NumberOfDofsDataSymbol, RealFieldDataDataSymbol, I_DEF, R_DEF, \
    OperatorSizeDataSymbol, OperatorDataSymbol, NumberOfUniqueDofsDataSymbol, \
    BasisFunctionDataSymbol, DiffBasisFunctionDataSymbol

from psyclone.psyir.backend.fortran import FortranWriter

# Add LFRic precision symbols and the module in which they are
# contained to the symbol table
SYMBOL_TABLE = SymbolTable()
SYMBOL_TABLE.add(I_DEF)
SYMBOL_TABLE.add(R_DEF)
SYMBOL_TABLE.add(CONSTANTS_MOD)

# Create LFRic ndf and undf symbols and add them to the symbol table
NDF_W3 = NumberOfDofsDataSymbol("ndf_w3", "w3")
SYMBOL_TABLE.add(NDF_W3)
UNDF_W3 = NumberOfUniqueDofsDataSymbol("undf_w3", "w3")
SYMBOL_TABLE.add(UNDF_W3)

# Create LFRic field data symbols and add them to the symbol table
FIELD1 = RealFieldDataDataSymbol("field1", [UNDF_W3], "w3")
SYMBOL_TABLE.add(FIELD1)
FIELD2 = RealFieldDataDataSymbol(
    "field2", [UNDF_W3], "w3",
    interface=ArgumentInterface(ArgumentInterface.Access.READWRITE))
SYMBOL_TABLE.add(FIELD2)
SYMBOL_TABLE.specify_argument_list([FIELD2])

# Create an LFRic operator and it to the symbol table
NCELL_3D = OperatorSizeDataSymbol("ncell_3d")
SYMBOL_TABLE.add(NCELL_3D)
OPERATOR = OperatorDataSymbol("oper1", [NDF_W3, NDF_W3, NCELL_3D],
                              fs_from="w3", fs_to="w3")
SYMBOL_TABLE.add(OPERATOR)


nqp_h = QrDataSymbol("np_xyz", "xyoz",
                     interface=ArgumentInterface(ArgumentInterface.Access.READ))
nqp_v =
wh = wh(nqp_h)
wv = wv(nqp_v)
basis(3,ndf,nqp_h,nqp_v)
      INTEGER(KIND=i_def), intent(in) :: np_xy_qr_xyoz, np_z_qr_xyoz
      REAL(KIND=r_def), intent(in), dimension(3,ndf_w1,np_xy_qr_xyoz,np_z_qr_xyoz) :: basis_w1_qr_xyoz
      REAL(KIND=r_def), intent(in), dimension(1,ndf_w2,np_xy_qr_xyoz,np_z_qr_xyoz) :: diff_basis_w2_qr_xyoz
      REAL(KIND=r_def), intent(in), dimension(1,ndf_w3,np_xy_qr_xyoz,np_z_qr_xyoz) :: basis_w3_qr_xyoz
      REAL(KIND=r_def), intent(in), dimension(3,ndf_w3,np_xy_qr_xyoz,np_z_qr_xyoz) :: diff_basis_w3_qr_xyoz
      REAL(KIND=r_def), intent(in), dimension(np_xy_qr_xyoz) :: weights_xy_qr_xyoz
      REAL(KIND=r_def), intent(in), dimension(np_z_qr_xyoz) :: weights_z_qr_xyoz


# Create a basis function and add it to the symbol table
BASIS = BasisFunctionDataSymbol("basis", [NDF_W3, NDF_W3, NDF_W3], "w3", "xox")
SYMBOL_TABLE.add(BASIS)

# Create a differential basis function and add it to the symbol table
DIFF_BASIS = DiffBasisFunctionDataSymbol(
    "diff_basis", [1 or 3, NDF_W3, NDF_W3], "w3", "xyoz")
SYMBOL_TABLE.add(DIFF_BASIS)

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
