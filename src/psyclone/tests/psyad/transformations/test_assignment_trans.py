# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab
''' xxx '''

from psyclone.psyir.symbols import DataSymbol, REAL_TYPE, SymbolTable
from psyclone.psyir.nodes import BinaryOperation, Reference, Assignment, Routine
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.backend.fortran import FortranWriter

from psyclone.psyad.transformations import AssignmentTrans


def check_adjoint(tl_fortran, active_variable_names, expected_ad_fortran):
    ''' Utility xxx '''
    
    input_code = ("subroutine test()\n{0}end subroutine test\n"
                  "".format(tl_fortran))
    expected_output_code = ("subroutine test()\n{0}end subroutine test\n"
                            "".format(expected_ad_fortran))

    reader = FortranReader()
    psyir = reader.psyir_from_source(input_code)
    assignment = psyir.children[0]
    assert isinstance(assignment, Assignment)

    symbol_table = psyir.symbol_table
    active_variables = []
    for variable_name in active_variable_names:
        active_variables.append(symbol_table.lookup(variable_name))
    
    trans = AssignmentTrans(active_variables)
    trans.apply(assignment)

    writer = FortranWriter()
    ad_fortran = writer(psyir)

    assert ad_fortran == expected_output_code


def test_zero():
    '''Test that the adjoint transformation works for the following case:

    A=0 -> A*=0

    '''
    tl_fortran = (
        "  real :: a\n"
        "  a = 0.0\n")
    active_variables = ["a"]
    ad_fortran = (
        "  real :: a\n\n"
        "  a = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)

# a=b
# def test_single_assign():

# a=xb
def test_single_valued_assign():
    '''Test that the adjoint transformation works for the following case:

    A=xB -> B*=B*+xA*;A*=0.0

    '''
    tl_fortran = (
        "  real a, b, n\n"
        "  a = 3*n*b\n")
    active_variables = ["a", "b"]
    ad_fortran = (
        "  real :: a\n  real :: b\n  real :: n\n"
        "  b = b + 3 * n * a\n"
        "  a = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


# a+=b
# a=xa+yb
# a=b+xc+d
# a+=xc+yd


def test_multi_add():
    '''Test that the adjoint transformation with an assignment of the form
    A = B + C + D. This tests that the transformation works when there
    is more than one addition on the rhs with the lhs being a write,
    not an increment.

    '''
    tl_fortran = (
        "subroutine test\n"
        "  real a, b, c, d\n"
        "  a = b + c + d\n"
        "end subroutine test\n")

    expected = (
        "subroutine test()\n"
        "  real :: a\n  real :: b\n  real :: c\n  real :: d\n\n"
        "  d = d + a\n"
        "  c = c + a\n"
        "  b = b + a\n"
        "  a = 0.0\n\n"
        "end subroutine test\n")

    reader = FortranReader()
    psyir = reader.psyir_from_source(tl_fortran)

    assignment = psyir.children[0]
    assert isinstance(assignment, Assignment)

    symbol_table = psyir.symbol_table
    a_symbol = symbol_table.lookup("a")
    b_symbol = symbol_table.lookup("b")
    c_symbol = symbol_table.lookup("c")
    d_symbol = symbol_table.lookup("d")    
    active_variables = [a_symbol, b_symbol, c_symbol, d_symbol]

    trans = AssignmentTrans(active_variables)
    trans.apply(assignment)

    writer = FortranWriter()
    ad_fortran = writer(psyir)
    assert ad_fortran == expected


# * errors (not all terms active, not linear, ...)
# * array accesses
# * datatypes (assuming all real for the moment) and ignoring precision
