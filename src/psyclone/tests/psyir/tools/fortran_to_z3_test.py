# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, University of Cambridge, UK
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
# Author M. Naylor, University of Cambridge, UK

''' Module containing tests for Fortran-to-Z3 translation.'''

import pytest
from psyclone.psyir.nodes import (Loop, Assignment, Reference)
from psyclone.psyir.symbols import Symbol
from psyclone.psyir.tools.fortran_to_z3 import FortranToZ3
import z3


# -----------------------------------------------------------------------------
@pytest.mark.parametrize("use_bv", [True, False])
@pytest.mark.parametrize("num_sweep_threads", [1, 4])
def test_translate_expr(use_bv,
                        num_sweep_threads,
                        fortran_reader,
                        fortran_writer):
    '''Test that Fortran expressions are being correctly translated to SMT.
    '''
    trans = FortranToZ3(
                use_bv=use_bv,
                num_sweep_threads=num_sweep_threads,
                prohibit_overflow=True,
                handle_array_intrins=True)

    def test(expr):
        psyir = fortran_reader.psyir_from_source(f'''
                  subroutine sub(x)
                    logical, intent(inout) :: x
                    integer :: arr(10)
                    integer :: i
                    x = {expr}
                  end subroutine''')
        for assign in psyir.walk(Assignment):
            (rhs_smt, cs) = trans.translate_logical_expr(assign.rhs)
            (result, expr_vals) = trans.solve(
                                      constraints = [rhs_smt] + cs,
                                      exprs_to_eval = [rhs_smt])
            assert result == z3.sat

    test("+1 == 1")
    test("1+1 == 2")
    test("1-1 == 0")
    test("2*2 == 4")
    test("4/2 == 2")
    test("abs(-1) == 1")
    test("min(1,2) == 1")
    test("max(1,2) == 2")
    # TODO: when fparser supports shift operations (#428), we can uncomment
    # these tests and remove "no cover" blocks in FortranToZ3
    # test("shiftl(2,1) == 4")
    # test("shiftr(2,1) == 1")
    # test("shifta(-2,1) == -1")
    test("iand(5,1) == 1")
    test("ior(1,2) == 3")
    test("ieor(3,1) == 2")
    test("max(3,1) == 3")
    test("i == 3")
    test(".true.")
    test(".not. .false.")
    test(".true. .and. .true.")
    test(".true. .or. .false.")
    test(".false. .eqv. .false.")
    test(".false. .neqv. .true.")
    test("1 /= 2")
    test("1 < 2")
    test("10 > 2")
    test("1 <= 1 .and. 0 <= 1")
    test("1 >= 1 .and. 2 >= 1")
    test("1 * 1 == 1")
    test("mod(-8, 3) == -2")
    test("modulo(-8, 3) == 1")
    test("foo(1)")
    test("foo(1) == 1")
    test("size(arr,1) == 10")
    test("lbound(arr,1) == 10")
    test("ubound(arr,1) == 10")
    test("ubound(arr,i) == 10")
    test("sum(arr) == 10")
    test("size(arr(1:2)) == 10")
    test("x")
