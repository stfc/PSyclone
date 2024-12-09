#!/usr/bin/env python3 

from sympy import diff, simplify

from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.frontend.sympy_reader import SymPyReader
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.backend.sympy_writer import SymPyWriter
from psyclone.core import VariablesAccessInfo
from psyclone.psyir.nodes import Assignment, Reference

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
writer = FortranWriter()

# Use the reader to convert source code to PSyIR
psyir = reader.psyir_from_source(code)

# Get and print variable access info:
# -----------------------------------
# Use VariablesAccessInfo to get all info of the psyir
var_info = VariablesAccessInfo(psyir)
print("Variable Access Info - Summary:")
print(var_info)
print("-------------------------------")
for signature in var_info:
   print(signature, ":", var_info[signature])
   for access in var_info[signature].all_accesses:
      print(f"Type: {access.access_type} - location {access.location} - "
            f"node {access.node}")
print("-------------------------------")


# Simplify the assignment
# -----------------------

# Get assignment, and convert from Fortran to SymPy
# Use walk to find the first assignment
assign = psyir.walk(Assignment)[0]

# Use the SymPy writer to convert the right hand sign of the
# assignment to sympy. Use assign.rhs
sympy_writer = SymPyWriter()
math_expr = sympy_writer(assign.rhs)

# Use SymPy function to simplify RHS (de-facto, SymPy will already
# have simplified the expression when converting it, but with more
# complex operation this would be required)
simple_sympy = simplify(math_expr)

# Now convert back to PSyIR. The reader needs the writer
# (since the writer stores information about mapping of
# Fortran names to SymPy names and back, esp. derived types)
# So you need to provide the sympy_writer instance to the
# SymPyReader constructor:
sympy_reader = SymPyReader(sympy_writer)

# We need to provide the symbol table when parsing an expression
symbol_table = assign.scope.symbol_table

# Convert the new sympy expression to PSyIR (which is detached)
# Use sympy_reader.psyir_from_expression on the sympy expression,
# and provide the symbol table as second parameter:
simple_psyir = sympy_reader.psyir_from_expression(simple_sympy, symbol_table)

# Replace the old expression with the new expression using
# `replace_with`
assign.rhs.replace_with(simple_psyir)


# Differentiation the second assignment
# -------------------------------------
# Use walk to get second assignment
assign = psyir.walk(Assignment)[1]

# We need to tell SymPy which variable to use when differentiating.
# Walk the rhs for a reference that has the name 'x' (in this case
# all References will have the name 'x'.
for ref in assign.rhs.walk(Reference):
   if ref.name == "x":
      break

# Convert both the rhs and the symbol to SymPy. This can be done in one
# step (making sure that all symbols will match up)
math_expr, x = sympy_writer([assign.rhs, ref])

# Differentiate. You can also use sympy.Symbol('x') as second parameter
derived_sympy = diff(math_expr, x)

# Convert back (see above) and replace in the right-hand-side
derived_psyir = sympy_reader.psyir_from_expression(derived_sympy, symbol_table)
assign.rhs.replace_with(derived_psyir)


# Output results
# --------------
fortran = writer(psyir)
print("Simplified and differentiated:\n", fortran)
