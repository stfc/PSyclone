# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
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
# Author: J. Henrichs, Bureau of Meteorology

''' Module containing  tests for the SymPy reader.'''

import pytest

from psyclone.psyir.backend.sympy_writer import SymPyWriter
from psyclone.psyir.frontend.sympy_reader import SymPyReader


def test_sympy_reader_constructor():
    '''Tests that the constructor sets the variables from the provided
    Writer as expected.
    '''
    sympy_writer = SymPyWriter()
    sympy_writer._lower_bound = "new_lower_bound"
    sympy_writer._upper_bound = "new_upper_bound"
    _ = SymPyReader(sympy_writer)
    assert SymPyReader._lower_bound == "new_lower_bound"
    assert SymPyReader._upper_bound == "new_upper_bound"


@pytest.mark.parametrize("expressions", [("a", "a"),
                                         ("b(i)", "b(i)"),
                                         ("d%e(i)", "d%e(i)"),
                                         ("e(i)%e", "e(i)%e"),
                                         ("e(i)%e(j)", "e(i)%e(j)"),
                                         ("e(1:9:3)%e(i:j:k)",
                                          "e(1:9:3)%e(i:j:k)"),
                                         ("c(i,j)", "c(i,j)"),
                                         ("b(2:3:4)", "b(2:3:4)"),
                                         ("b(2:3:1)", "b(2:3)"),
                                         ("b", "b(:)"),
                                         ("b(:)", "b(:)"),
                                         ("b(::)", "b(:)"),
                                         ("b(::1)", "b(:)"),
                                         ("b(5::1)", "b(5:)"),
                                         ("b(5::2)", "b(5::2)"),
                                         ("b(:5:1)", "b(:5)"),
                                         ("b(:5:2)", "b(:5:2)"),
                                         ("b(2:5:1)", "b(2:5)"),
                                         ("b(2:5:2)", "b(2:5:2)"),
                                         ])
def test_sympy_psyir_from_expression(fortran_reader, fortran_writer,
                                     expressions):
    '''Test conversion from a SymPy expression back to PSyIR. We use the
    SymPyWriter to convert the Fortran string to a SymPy expression (we need
    a symbol table to convert from SymPy to PSyIR, which we get this way
    automatically).
    Check the conversion of indices, e.g. a Fortran `b(i)` becomes a SymPy
    `b(i,i,1)` (to support array expressions), which then needs to be
    converted back to `b(i`).

    '''
    source = f'''program test_prog
                use my_mod
                integer :: i, j
                integer :: a, b(10), c(10, 10)
                type(my_mod_type) :: d, e(10)
                x = {expressions[0]}
                end program test_prog'''

    psyir = fortran_reader.psyir_from_source(source)
    symbol_table = psyir.children[0].symbol_table
    psyir_expr = psyir.children[0].children[0].rhs
    # Convert the PSyIR expression to a string that can be parsed by SymPy
    sympy_writer = SymPyWriter()
    sympy_expr = sympy_writer(psyir_expr)
    sympy_reader = SymPyReader(sympy_writer)
    new_psyir = sympy_reader.psyir_from_expression(sympy_expr, symbol_table)
    assert fortran_writer(new_psyir) == expressions[1]
