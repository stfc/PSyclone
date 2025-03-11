#!/usr/bin/env python3

'''A simple PSyclone program that shows the usage of SymPy to
modify Fortran expression. It first simplifies an expression,
and then also adds the derivative of an expression to the code.
'''

from sympy import diff, simplify

from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.frontend.sympy_reader import SymPyReader
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.backend.sympy_writer import SymPyWriter
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
# have simplified this expression when converting it, but with more
# complex expressions this call is required)
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
# `replace_with` on the rhs of the assignment
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
else:
    raise RuntimeError("Could not find access to 'x'.")

# Convert both the rhs and the symbol to SymPy. This can be done in one
# step (making sure that all symbols will match up)
math_expr, x = sympy_writer([assign.rhs, ref])

# Differentiate. You could also use sympy.Symbol('x') as second parameter
derived_sympy = diff(math_expr, x)

# Convert back (see above) and replace in the right-hand-side
derived_psyir = sympy_reader.psyir_from_expression(derived_sympy, symbol_table)

# Create a copy of the original assignment statement ...
assign_copy = assign.copy()

# which is then appended to the program
assign.parent.children.append(assign_copy)

# Now insert the derivate as the rhs of the copy of the assignment statement:
assign_copy.rhs.replace_with(derived_psyir)

# And add a nice comment
assign_copy.preceding_comment = "The derivative is:"

# Output results
# --------------
fortran = writer(psyir)
print("Simplified and differentiated:\n", fortran)
