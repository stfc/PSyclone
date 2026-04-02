#!/usr/bin/env python3
# flake8: noqa

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
psyir = 

# You can now transform the PSyIR

# Output Fortran source code:
writer = FortranWriter()
fortran = # write out the Fortran source code
print("Source code:")
print(fortran)
