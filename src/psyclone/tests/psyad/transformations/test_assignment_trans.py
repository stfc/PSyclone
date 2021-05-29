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
    print (input_code)
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

    print (ad_fortran)
    assert ad_fortran == expected_output_code


def test_zero():
    '''Test that the adjoint transformation works for the following case:

    A=0 -> A*=0

    '''
    # Scalar
    tl_fortran = (
        "  real :: a\n"
        "  a = 0.0\n")
    active_variables = ["a"]
    ad_fortran = (
        "  real :: a\n\n"
        "  a = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)
    # Array
    tl_fortran = (
        "  real :: a(n)\n"
        "  integer :: n\n"
        "  a(n) = 0.0\n\n")
    active_variables = ["a"]
    ad_fortran = (
        "  integer :: n\n"
        "  real, dimension(n) :: a\n\n"
        "  a(n) = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


def test_single_assign():
    '''Test that the adjoint transformation works for the following case:

    A=B -> B*=B*+A*;A*=0.0

    '''
    # Scalar
    tl_fortran = (
        "  real :: a,b\n"
        "  a = b\n")
    active_variables = ["a", "b"]
    ad_fortran = (
        "  real :: a\n  real :: b\n\n"
        "  b = b + a\n"
        "  a = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)
    # Array
    tl_fortran = (
        "  real :: a(n),b(n)\n"
        "  integer :: i,n\n"
        "  a(2*i) = b(n+1)\n")
    active_variables = ["a", "b"]
    ad_fortran = (
        "  integer :: n\n  real, dimension(n) :: a\n"
        "  real, dimension(n) :: b\n  integer :: i\n\n"
        "  b(n + 1) = b(n + 1) + a(2 * i)\n"
        "  a(2 * i) = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


def test_single_valued_assign():
    '''Test that the adjoint transformation works for the following case:

    A=xB -> B*=B*+xA*;A*=0.0

    '''
    # Scalar
    tl_fortran = (
        "  real a, b, n\n"
        "  a = 3*n*b\n")
    active_variables = ["a", "b"]
    ad_fortran = (
        "  real :: a\n  real :: b\n  real :: n\n\n"
        "  b = b + 3 * n * a\n"
        "  a = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)
    # Array
    tl_fortran = (
        "  real a(10), b(10), n\n"
        "  integer :: i,j\n"
        "  a(i) = 3*n*b(j)\n")
    active_variables = ["a", "b"]
    ad_fortran = (
        "  real, dimension(10) :: a\n  real, dimension(10) :: b\n"
        "  real :: n\n  integer :: i\n  integer :: j\n\n"
        "  b(j) = b(j) + 3 * n * a(i)\n"
        "  a(i) = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


def test_multi_add():
    '''Test that the adjoint transformation works for the following case:

    Test that the adjoint transformation with an assignment of the form
    A = B + xC + D. This tests that the transformation works when there
    is more than one addition on the rhs with the lhs being a write,
    not an increment.

    A=xB+yC+D -> D*=D*+A; C*=C*+yA*; B*=B*+xA*; A*=0.0

    '''
    # Scalar
    tl_fortran = (
        "  real a, b, c, d\n"
        "  integer n\n"
        "  a = 3*n*b + c/4 + d\n")
    active_variables = ["a", "b", "c", "d"]
    ad_fortran = (
        "  real :: a\n  real :: b\n  real :: c\n  real :: d\n"
        "  integer :: n\n\n"
        "  d = d + a\n"
        "  c = c + a / 4\n"
        "  b = b + 3 * n * a\n"
        "  a = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)
    # Array
    tl_fortran = (
        "  real a(10), b(10), c(10), d(10)\n"
        "  integer :: i, j, n\n"
        "  a(i+2) = (3/n)*b(j) + c(1)/(2*n) + d(n)\n")
    active_variables = ["a", "b", "c", "d"]
    ad_fortran = (
        "  real, dimension(10) :: a\n  real, dimension(10) :: b\n"
        "  real, dimension(10) :: c\n  real, dimension(10) :: d\n"
        "  integer :: i\n  integer :: j\n  integer :: n\n\n"
        "  d(n) = d(n) + a(i + 2)\n"
        "  c(1) = c(1) + a(i + 2) / (2 * n)\n"
        "  b(j) = b(j) + 3 / n * a(i + 2)\n"
        "  a(i + 2) = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


def test_increment():
    '''Test that the adjoint transformation with an assignment of the form
    A = A. This tests that the transformation works when there are no
    additions on the rhs with the lhs being an increment.

    A=A -> A*=A*

    As A does not change we output nothing.

    '''
    # Scalar
    tl_fortran = (
        "  real a\n"
        "  a = a\n")
    active_variables = ["a"]
    ad_fortran = (
        "  real :: a\n\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


def test_increment_mult():
    '''Test that the adjoint transformation with an assignment of the form
    A = xA. This tests that the transformation works when there are no
    additions on the rhs with the lhs being a scaled increment.

    A=xA -> A*=xA*

    '''
    # Scalar
    tl_fortran = (
        "  real a\n"
        "  a = 5*a\n")
    active_variables = ["a"]
    ad_fortran = (
        "  real :: a\n\n"
        "  a = 5 * a\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


def test_increment_add():
    '''Test that the adjoint transformation with an assignment of the form
    A = A + B. This tests that the transformation works when there is
    a single addition on the rhs with the lhs being a scaled
    increment.

    A+=B -> B*=A*; A*=A*

    '''
    # Scalar
    tl_fortran = (
        "  real a, b\n"
        "  a = a+b\n")
    active_variables = ["a", "b"]
    ad_fortran = (
        "  real :: a\n"
        "  real :: b\n\n"
        "  b = b + a\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


def test_increment_add_reorder():
    '''Test that the adjoint transformation with an assignment of the form
    A = B + A. This tests that the transformation works when there is
    a single addition on the rhs with the lhs being a scaled increment
    and the increment not being on the lhs of the rhs.

    A=B+kA -> B*=A*; A*=kA*

    '''
    # Scalar
    tl_fortran = (
        "  real a, b\n"
        "  integer k\n"
        "  a = b+k*a\n")
    active_variables = ["a", "b"]
    ad_fortran = (
        "  real :: a\n"
        "  real :: b\n"
        "  integer :: k\n\n"
        "  b = b + a\n"
        "  a = k * a\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


def test_increment_multi_add():
    '''Test that the adjoint transformation with an assignment of the form
    A += xB + yC + zD. This tests that the transformation works when
    there are multiple additions on the rhs with the lhs being a
    scaled increment.

    A=wA+xB+yC+zD -> D*=D*+zA*; C*=C*+yA*; B*=B*+xA*; A*=wA*

    '''
    # Scalar
    tl_fortran = (
        "  real a, b, c, d\n"
        "  real w, x, y, z\n"
        "  a = w*a+x*b+y*c+z*d\n")
    active_variables = ["a", "b", "c", "d"]
    ad_fortran = (
        "  real :: a\n  real :: b\n  real :: c\n  real :: d\n"
        "  real :: w\n  real :: x\n  real :: y\n  real :: z\n\n"
        "  d = d + z * a\n"
        "  c = c + y * a\n"
        "  b = b + x * a\n"
        "  a = w * a\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


# TODO
# check a = b + ya as a should be assigned after (test error)
# arrays for increment tests
# a = -b -yc
# a(i) = a(i+1) + b(i) + b(i+1)
# * errors (not all terms active, not linear, ...)
# * structures (builtin examples)
# * indirection (kernel examples)
# * datatypes (assuming all real for the moment) and ignoring precision
