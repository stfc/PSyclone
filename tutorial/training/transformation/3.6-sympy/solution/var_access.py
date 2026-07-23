#!/usr/bin/env python3

'''A simple test program that shows the usage of the
variable access information tools in PSyclone.
'''

from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.backend.fortran import FortranWriter

CODE = """
subroutine foo(a, b)
real, intent(inout) :: a
real, intent(inout) :: b
real :: c, d, e, f
c = a + 1.0
e = a**2
f = cos(e)
d = c + 2.0
c = d * a
b = c + d
call bar(c, b)
b = b + c
end subroutine foo
"""
reader = FortranReader()
writer = FortranWriter()

# Use the reader to convert source code to PSyIR
psyir = reader.psyir_from_source(CODE)

# Get and print variable access info:
# -----------------------------------
# Use the `reference_accesses` method to get all info of the psyir
var_access_map = psyir.reference_accesses()

print("Variable Access Map - Summary:")
print(var_access_map)
print("===============================")
for signature in var_access_map:
    print()
    print(signature, ":", var_access_map[signature])
    print("----------------------------")
    for access in var_access_map[signature]:
        print(f"Type: {access.access_type} - node {access.node}")
print("===============================")
