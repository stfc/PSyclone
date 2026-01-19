#! /usr/bin/env python3

# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025-2026, Science and Technology Facilities Council.
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
# Author J. Henrichs, Bureau of Meteorology

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
varinfo = psyir.children[0].reference_accesses()

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
            chain = DefinitionUseChain(node, stop_point=stop_position)
            all_prev = chain.find_backward_accesses()

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
