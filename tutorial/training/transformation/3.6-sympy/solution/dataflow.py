#! /usr/bin/env python3

# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This programs creates a graphviz dataflow diagram for a given subroutine.
It is only a draft implementation, but can already create useful graphs
in many cases. To use it:

./dataflow.py >out
dot -Tjpeg out >out.jpg
'''

from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import Reference, Statement
from psyclone.psyir.tools. definition_use_chains import DefinitionUseChain


code = """
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
subroutine bar(x, y)
real, intent(in) :: x
real, intent(inout) :: y
!x = x + 1.0
y = exp(x**2)
end subroutine bar
"""

# Create the PSyIR, and get the variable access information:
reader = FortranReader()
psyir = reader.psyir_from_source(code)
routine = psyir.find_routine_psyir("foo")
varinfo = routine.reference_accesses()

# Create a writer to be able to include code in the graph
writer = FortranWriter()

print("digraph {")

# Handle each variable
for var in varinfo:
    accesses = varinfo[var]
    for written in accesses.all_write_accesses:
        statement = written.node.ancestor(Statement)

        # Now get all variables used in this statement:
        all_accessed = statement.reference_accesses()
        for read_var in all_accessed:
            # Ignore the variable with the write access we
            # are currently looking at:
            if not all_accessed.is_read(read_var):
                continue
            # If we have a write access to a variable, but it's not
            # the variable we are currently analysing, ignore it
            # (happens if we call a subroutine with several variables written)
            if all_accessed.is_written(read_var) and read_var != var:
                continue
            # Now we have a variable that is read in the current
            # statement. Find if and where it was previously
            # written:
            node = all_accessed[read_var][0].node
            # TODO: #3143 atm requires to provide a stop_point,
            # otherwise the call itself is returned.
            if not isinstance(node, Statement):
                stop_position = node.ancestor(Statement).abs_position
            else:
                stop_position = node.abs_position
            chain = DefinitionUseChain([node], stop_point=stop_position)
            sig = node.get_signature_and_indices()[0]
            all_prev = chain.find_backward_accesses()[sig]

            # Keep track if a write was found (if not, we will add the
            # variable as a node by itself)
            prev_write_found = False
            for prev in all_prev:
                # DUC will return all accesses, including reads. We are
                # looking for previous write statements only, so ignore
                # the read accesses:
                if isinstance(prev, Reference) and not prev.is_write:
                    continue
                prev_write_found = True
                if not isinstance(prev, Statement):
                    prev = prev.ancestor(Statement)
                print(f'"{writer(prev).strip()}" -> '
                      f'"{writer(statement).strip()}" [label="{read_var}"]')

            if not prev_write_found:
                # If no previous write access was found, add the variable
                # itself as a node
                print(f'{read_var} -> "{writer(statement).strip()}" '
                      f'[label="{read_var}"]')

print("}")
