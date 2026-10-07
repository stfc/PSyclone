#!/usr/bin/env python3
# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''A very simple Python program that reads and writes a Fortran program
'''
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.backend.fortran import FortranWriter

# This could also be read from a file:
code = """
subroutine test(a, b, i, j, x)
   real, dimension(:,:) :: a
   real :: b, x
   integer :: i,j

   b = a(j + 2*i - j - i, j*3 - 2*j)*a(i,j)  + 5*b - b - 3*b - 3.14_8
   b = 3*x*x - 2*x - 1
end subroutine test
"""
reader = FortranReader()

# Use the reader to convert source code to PSyIR
psyir = reader.psyir_from_source(code)

# You can now transform the PSyIR

# Output Fortran source code:
writer = FortranWriter()
fortran = writer(psyir)
print("Source code:")
print(fortran)
