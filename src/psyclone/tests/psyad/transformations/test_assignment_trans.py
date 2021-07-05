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
#
'''Module to test the psyad assignment transformation.'''

import pytest

from psyclone.psyir.symbols import DataSymbol, REAL_TYPE, SymbolTable
from psyclone.psyir.nodes import BinaryOperation, Reference, Assignment, \
    Routine, Literal, UnaryOperation
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.transformations import TransformationError

from psyclone.psyad.transformations import AssignmentTrans, TangentLinearError


def check_adjoint(tl_fortran, active_variable_names, expected_ad_fortran):
    '''Utility routine that takes tangent linear fortran code as input in
    the argument 'tl_fortran', transforms this code into its adjoint
    using the active variables specified in the
    'active_variable_names' argument and tests whether the result is
    the same as the expected result in the 'expected_ad_fortran'
    argument.

    To help keep the test code short this routine also adds the
    subroutine / end subroutine lines to the incoming code.

    :param str tl_fortran: tangent linear code.
    :param list of str active_variable_names: a list of active \
        variable names.
    :param str tl_fortran: the expected adjoint code to be produced.

    '''
    # Add "subroutine / end subroutine" lines to the incoming code.
    input_code = ("subroutine test()\n{0}end subroutine test\n"
                  "".format(tl_fortran))
    expected_output_code = ("subroutine test()\n{0}end subroutine test\n"
                            "".format(expected_ad_fortran))

    # Translate the tangent linear code to PSyIR.
    reader = FortranReader()
    psyir = reader.psyir_from_source(input_code)

    # Find the assignment line in the PSyIR. The hardcoded assumption
    # is that it is the first line in the provided tangent linear
    # code.
    assignment = psyir.children[0][0]
    assert isinstance(assignment, Assignment)

    # Find the symbols in the symbol table associated with the
    # supplied active variable strings as the transformation expects
    # active variables to be provided as symbols.
    symbol_table = assignment.scope.symbol_table
    active_variables = []
    for variable_name in active_variable_names:
        active_variables.append(symbol_table.lookup(variable_name))

    # Apply the tangent linear to adjoint transformation.
    trans = AssignmentTrans(active_variables)
    trans.apply(assignment)

    # Translate the adjoint code to Fortran.
    writer = FortranWriter()
    ad_fortran = writer(psyir)

    # Check that the code produced is the same as the expected code
    # provided.
    assert ad_fortran == expected_output_code


def test_zero():
    '''Test that the adjoint transformation with an assignment of the form
    A = 0. This tests that the transformation works in the special
    case of when an active variable is set to zero. This is the only
    valid value that an active variable can be set to in a tangent
    linear code and apparently represents multiplying an active
    variable by zero on the rhs. Scalars, directly addressed arrays,
    indirectly addressed arrays and structure array accesses are
    tested.

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
    # Direct Addressed Array
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
    # Indirect Addressed Array
    tl_fortran = (
        "  real :: a(n)\n"
        "  integer :: b(n), n\n"
        "  a(b(n)) = 0.0\n\n")
    active_variables = ["a"]
    ad_fortran = (
        "  integer :: n\n"
        "  real, dimension(n) :: a\n"
        "  integer, dimension(n) :: b\n\n"
        "  a(b(n)) = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)
    # Structure
    tl_fortran = (
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: a\n"
        "  integer :: n\n"
        "  a%data(n) = 0.0\n\n")
    active_variables = ["a"]
    ad_fortran = (
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: a\n"
        "  integer :: n\n\n"
        "  a%data(n) = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


def test_single_assign():
    '''Test that the adjoint transformation with an assignment of the form
    A = B. This tests that the transformation works when there is one
    active variable on the rhs and with the active variable on the lhs
    being a write, not an increment. Scalars, directly addressed
    arrays, indirectly addressed arrays and structure array accesses
    are tested.

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
    # Direct addressed array
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
    # Indirect addressed array
    tl_fortran = (
        "  real :: a(n),b(n)\n"
        "  integer :: i,n,lookup(n)\n"
        "  a(lookup(2*i)) = b(lookup(n)+1)\n")
    active_variables = ["a", "b"]
    ad_fortran = (
        "  integer :: n\n  real, dimension(n) :: a\n"
        "  real, dimension(n) :: b\n  integer :: i\n"
        "  integer, dimension(n) :: lookup\n\n"
        "  b(lookup(n) + 1) = b(lookup(n) + 1) + a(lookup(2 * i))\n"
        "  a(lookup(2 * i)) = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)
    # Structure
    tl_fortran = (
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: a,b\n"
        "  integer :: i,n\n"
        "  a%data(2*i) = b%data(n+1)\n")
    active_variables = ["a", "b"]
    ad_fortran = (
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: a\n  type(field_type) :: b\n"
        "  integer :: i\n  integer :: n\n\n"
        "  b%data(n + 1) = b%data(n + 1) + a%data(2 * i)\n"
        "  a%data(2 * i) = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


def test_single_valued_assign():
    '''Test that the adjoint transformation with an assignment of the form
    A = xB. This tests that the transformation works when there is one
    active variable on the rhs that is multipled by a factor and with
    the active variable on the lhs being a write, not an
    increment. Also test mixed case active variables list and actual
    variables.

    A=xB -> B*=B*+xA*;A*=0.0

    '''
    tl_fortran = (
        "  real a(10), b(10), n\n"
        "  integer :: i,j\n"
        "  a(i) = 3*n*b(j)\n")
    active_variables = ["A", "B"]
    ad_fortran = (
        "  real, dimension(10) :: a\n  real, dimension(10) :: b\n"
        "  real :: n\n  integer :: i\n  integer :: j\n\n"
        "  b(j) = b(j) + 3 * n * a(i)\n"
        "  a(i) = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


@pytest.mark.xfail(name="issue #xxx literal math_equal() does "
                   "not work properly.")
def test_multi_add():
    '''Test that the adjoint transformation with an assignment of the form
    A = xB + yC + D. This tests that the transformation works when
    there are many active variables on the rhs with some of them being
    multipled by a factor and with the active variable on the lhs
    being a write, not an increment. Also test mixed case declarations.

    A=xB+yC+D -> D*=D*+A; C*=C*+yA*; B*=B*+xA*; A*=0.0

    '''
    tl_fortran = (
        "  real A(10), B(10), C(10), D(10)\n"
        "  integer :: i, j, n\n"
        "  a(i+2) = (3/n)*b(j) + c(1)/(2*n) + d(n)\n")
    active_variables = ["a", "b", "c", "d"]
    ad_fortran = (
        "  real, dimension(10) :: a\n  real, dimension(10) :: b\n"
        "  real, dimension(10) :: c\n  real, dimension(10) :: d\n"
        "  integer :: i\n  integer :: j\n  integer :: n\n\n"
        "  b(j) = b(j) + a(i + 2) * (3 / n)\n"
        "  c(1) = c(1) + a(i + 2) / (2 * n)\n"
        "  d(n) = d(n) + a(i + 2)\n"
        "  a(i + 2) = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


def test_increment():
    '''Test that the adjoint transformation with an assignment of the form
    A = A. This tests that the transformation works when there are no
    additions on the rhs with the lhs being an increment.

    A=A -> A*=A*

    As A does not change we output nothing.

    '''
    tl_fortran = (
        "  integer :: n\n"
        "  real a(n)\n"
        "  a(n) = a(n)\n")
    active_variables = ["a"]
    ad_fortran = (
        "  integer :: n\n"
        "  real, dimension(n) :: a\n\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


def test_increment_mult():
    '''Test that the adjoint transformation with an assignment of the form
    A = xA. This tests that the transformation works when there are no
    additions on the rhs with the lhs being a scaled increment. Also
    test mixed case variables.

    A=xA -> A*=xA*

    '''
    tl_fortran = (
        "  integer :: n\n"
        "  real a(n)\n"
        "  A(n) = 5*A(n)\n")
    active_variables = ["a"]
    ad_fortran = (
        "  integer :: n\n"
        "  real, dimension(n) :: a\n\n"
        "  a(n) = 5 * a(n)\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


def test_increment_add():
    '''Test that the adjoint transformation with an assignment of the form
    A = A + B. This tests that the transformation works when there is
    a single addition on the rhs with the lhs being an increment. ALso
    test mixed case variables.

    A+=B -> B*+=A*; A*=A*

    '''
    tl_fortran = (
        "  real a(10), b(10)\n"
        "  a(1) = A(1)+B(1)\n")
    active_variables = ["a", "b"]
    ad_fortran = (
        "  real, dimension(10) :: a\n"
        "  real, dimension(10) :: b\n\n"
        "  b(1) = b(1) + a(1)\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


def test_increment_add_reorder():
    '''Test that the adjoint transformation with an assignment of the form
    A = B + kA. This tests that the transformation works when there is
    a single addition on the rhs with the lhs being a scaled increment
    and the increment not being on the lhs of the rhs.

    A=B+kA -> B*+=A*; A*=kA*

    '''
    tl_fortran = (
        "  real a(10), b(10)\n"
        "  integer k\n"
        "  a(1) = b(1)+k*a(1)\n")
    active_variables = ["a", "b"]
    ad_fortran = (
        "  real, dimension(10) :: a\n"
        "  real, dimension(10) :: b\n"
        "  integer :: k\n\n"
        "  b(1) = b(1) + a(1)\n"
        "  a(1) = k * a(1)\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


def test_increment_multi_add():
    '''Test that the adjoint transformation with an assignment of the form
    A = wA + xB + yC + zD. This tests that the transformation works
    when there are multiple additions on the rhs with the lhs being a
    scaled increment.

    A=wA+xB+yC+zD -> D*=D*+zA*; C*=C*+yA*; B*=B*+xA*; A*=wA*

    '''
    tl_fortran = (
        "  real a(10), b(10), c(10), d(10)\n"
        "  real w(10), x, y(10), z\n"
        "  a(1) = w(1)*a(1)+x*b(1)+y(1)*c(1)+d(1)*z\n")
    active_variables = ["a", "b", "c", "d"]
    ad_fortran = (
        "  real, dimension(10) :: a\n  real, dimension(10) :: b\n"
        "  real, dimension(10) :: c\n  real, dimension(10) :: d\n"
        "  real, dimension(10) :: w\n  real :: x\n"
        "  real, dimension(10) :: y\n  real :: z\n\n"
        "  b(1) = b(1) + x * a(1)\n"
        "  c(1) = c(1) + y(1) * a(1)\n"
        "  d(1) = d(1) + a(1) * z\n"
        "  a(1) = w(1) * a(1)\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


def test_multi_increment():
    '''Test that the adjoint transformation with an assignment containing
    multiple increments 
    A = A + xA.

    '''
    tl_fortran = (
        "  real a(10)\n"
        "  real x\n"
        "  a(1) = a(1)+x*a(1)\n")
    active_variables = ["a"]
    ad_fortran = (
        "  real, dimension(10) :: a\n"
        "  real :: x\n\n"
        "  a(1) = a(1) + x * a(1)\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


def test_single_valued_sub():
    '''Test that the adjoint transformation with an assignment of the form
    A = -B. This tests that the transformation works when there is one
    active variable on the rhs that is negated with
    the active variable on the lhs being a write, not an
    increment.

    A=-B -> B*=B*-A*;A*=0.0

    '''
    tl_fortran = (
        "  real a(10), b(10)\n"
        "  integer :: i,j\n"
        "  a(i) = -b(j)\n")
    active_variables = ["a", "b"]
    ad_fortran = (
        "  real, dimension(10) :: a\n  real, dimension(10) :: b\n"
        "  integer :: i\n  integer :: j\n\n"
        "  b(j) = b(j) + -a(i)\n"
        "  a(i) = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


def test_multi_valued_sub():
    '''Test that the adjoint transformation with an assignment of the form
    A = -B -x(+C) + (-D)y. This tests that the transformation works
    when there are multiple active variable on the rhs that have unary
    plus and minus operators as well as minus separating the
    terms. The active variable on the lhs is a write, not an
    increment.

    A=-B-x(+C)+(-D)y -> B*=B*-A*;C*=C*-xA*; D*=D*-yA*; A*=0.0

    '''
    tl_fortran = (
        "  real a(10), b(10), c(10), d(10)\n"
        "  real :: x, y\n"
        "  integer :: i,j\n"
        "  a(i) = -b(j)-x*(+c(i))+(-d(j))*y\n")
    active_variables = ["a", "b", "c", "d"]
    ad_fortran = (
        "  real, dimension(10) :: a\n  real, dimension(10) :: b\n"
        "  real, dimension(10) :: c\n  real, dimension(10) :: d\n"
        "  real :: x\n  real :: y\n  integer :: i\n  integer :: j\n\n"
        "  b(j) = b(j) + -a(i)\n"
        "  c(i) = c(i) - x * +a(i)\n"
        "  d(j) = d(j) + -a(i) * y\n"
        "  a(i) = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


def test_inc_sub():
    '''Test that the adjoint transformation with an assignment of the form
    A = -A. This tests that the transformation works
    when there is a single increment with a minus operator.

    A=-A -> A*=-A*

    '''
    tl_fortran = (
        "  real a(10)\n"
        "  integer :: i\n"
        "  a(i) = -a(i)\n")
    active_variables = ["a"]
    ad_fortran = (
        "  real, dimension(10) :: a\n"
        "  integer :: i\n\n"
        "  a(i) = -a(i)\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


def test_multi_inc_sub():
    '''Test that the adjoint transformation with an assignment of the form
    A = -A -xA + B + A/y. This tests that the transformation works
    when there is are multiple increments with and without a minus
    operator interspersed with another active variable.

    A=-A-xA+B+A/y -> B*=B*+A*; A*=A*(-1.0-x+1.0/y)

    '''
    tl_fortran = (
        "  real a(10), b(10)\n"
        "  integer :: i\n"
        "  real :: x,y\n"
        "  a(i) = -a(i)-x*a(i)+b(i)+a(i)/y\n")
    active_variables = ["a", "b"]
    ad_fortran = (
        "  real, dimension(10) :: a\n  real, dimension(10) :: b\n"
        "  integer :: i\n"
        "  real :: x\n  real :: y\n\n"
        "  b(i) = b(i) + a(i)\n"
        "  a(i) = -a(i) - x * a(i) + a(i) / y\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


def test_multi_rhs():
    '''Test that the adjoint transformation with an assignment of the form
    A = B + xB +B/y. This tests that the transformation works when
    there is are multiple terms on the rhs of an assignment with the
    same active variable.

    A=B+xB+B/y -> B*=B*+A*;B*=B*+xA*;B*=B*+A*/y

    '''
    tl_fortran = (
        "  real a(10), b(10)\n"
        "  real :: x,y\n"
        "  a = b + x*b + b/y\n")
    active_variables = ["a", "b"]
    ad_fortran = (
        "  real, dimension(10) :: a\n  real, dimension(10) :: b\n"
        "  real :: x\n  real :: y\n\n"
        "  b = b + a\n"
        "  b = b + x * a\n"
        "  b = b + a / y\n"
        "  a = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


def test_different_indices():
    '''Test that the adjoint transformation recognises that an access is
    not an increment when the indices of an array are different. For
    example a(i) = a(i+1) + b(i)

    A(i)=A(i+1)+B(i)+B(i-1) -> B*(i-1)=B*(i-1)+A*(i);B*(i)=B*(i)*+A*(i);
                                       A*(i+1)=A*(i+1)+A*(i);A*(i)=0.0

    '''
    tl_fortran = (
        "  real a(10), b(10)\n"
        "  integer :: i\n"
        "  a(i) = a(i+1)+b(i)+b(i-1)\n")
    active_variables = ["a", "b"]
    ad_fortran = (
        "  real, dimension(10) :: a\n  real, dimension(10) :: b\n"
        "  integer :: i\n\n"
        "  a(i + 1) = a(i + 1) + a(i)\n"
        "  b(i) = b(i) + a(i)\n"
        "  b(i - 1) = b(i - 1) + a(i)\n"
        "  a(i) = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


@pytest.mark.xfail(name="issue #xxx literal math_equal() does "
                   "not work properly.")
def test_same_indices_ordering():
    '''Test that the adjoint transformation recognises that an access is
    an increment when the indices of an array are the same but in a
    different order. For example a(i+1) = a(1+i) + b(i)

    A(i+1)=A(1+i)+B(i) -> B*(i)=B*(i)*+A*(i+1)

    '''
    tl_fortran = (
        "  real a(10), b(10)\n"
        "  integer :: i\n"
        "  a(i+1) = a(1+i)+b(i)\n")
    active_variables = ["a", "b"]
    ad_fortran = (
        "  real, dimension(10) :: a\n  real, dimension(10) :: b\n"
        "  integer :: i\n\n"
        "  b(i) = b(i) + a(i + 1)\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


@pytest.mark.xfail(name="issue #1075: Better symbolic comparison of indices "
                   "is needed.")
def test_same_indices_ordering2():
    '''Test that the adjoint transformation recognises that an access is
    an increment when the indices of an array are the same but are
    written in a different way. For example a(2*i) = a(i+i) + b(i)

    A(2*i)=A(i+i)+B(i) -> B*(i)=B*(i)*+A*(2 * i);

    '''
    tl_fortran = (
        "  real a(10), b(10)\n"
        "  integer :: i\n"
        "  a(2*i) = a(i+i)+b(i)\n")
    active_variables = ["a", "b"]
    ad_fortran = (
        "  real, dimension(10) :: a\n  real, dimension(10) :: b\n"
        "  integer :: i\n\n"
        "  b(i) = b(i) + a(2 * i)\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


@pytest.mark.xfail(name="issue #xxx structure_reference math_equal() does "
                   "not work properly.")
def test_different_structures():
    '''Test that the adjoint transformation recognises that an access is
    not an increment when a structure access differs. For
    example a%data(i) = a%data(i+1) + a%x(i)

    '''
    tl_fortran = (
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: a\n"
        "  integer :: n\n"
        "  a%data(n) = a%data(n+1) + a%atad(n)\n\n")
    active_variables = ["a"]
    ad_fortran = (
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: a\n"
        "  integer :: n\n\n"
        "  a%data(n+1) = a%data(n+1) + a%data(n)\n"
        "  a%atad(n) = a%atad(n) + a%data(n)\n"
        "  a%data(n) = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


@pytest.mark.parametrize("in_op1,in_op2,out_op", [
    ("+", "+", "+"), ("+", "-", "-"),
    ("-", "+", "-"), ("-", "-", "+")])
def test_validate_precedence_active_vars(in_op1, in_op2, out_op):
    '''Test that precedence of active variables is taken into account (as
    the sign may need to be flipped in certain cases). For example, see
    'd' below:
    
    a = b+(c+d) => b=b-a;c=c+a;d=d+a;a=0
    a = b+(c-d) => b=b-a;c=c+a;d=d-a;a=0
    a = b-(c+d) => b=b-a;c=c-a;d=d-a;a=0
    a = b-(c-d) => b=b-a;c=c-a;d=d+a;a=0

    '''
    tl_fortran = (
        "  real :: a,b,c,d\n"
        "  a = b {1} (c {0} d)\n".format(in_op1, in_op2))
    active_variables = ["a", "b", "c", "d"]
    ad_fortran = (
        "  real :: a\n  real :: b\n"
        "  real :: c\n  real :: d\n\n"
        "  b = b + a\n"
        "  c = c {1} a\n"
        "  d = d {0} a\n"
        "  a = 0.0\n\n".format(out_op, in_op2))
    check_adjoint(tl_fortran, active_variables, ad_fortran)

# Validate method

# TODO Create issues for failing tests
# TODO Check specified active variables are real (if their type is known).


def test_validate_node():
    '''Check that the expected exception is raised if the provided node
    argument is not a PSyIR Assignment node.

    '''
    trans = AssignmentTrans(active_variables=[])
    with pytest.raises(TransformationError) as info:
        trans.validate(None)
    assert ("Node argument in assignment transformation should be a PSyIR "
            "Assignment, but found 'NoneType'." in str(info.value))


def test_validate_not_active():
    '''Test that the validate method returns without error if there are no
    active variables in the assignment.

    active vars = ["c", "aa", "ab"]
    a = b

    '''
    lhs_symbol = DataSymbol("a", REAL_TYPE)
    rhs_symbol = DataSymbol("b", REAL_TYPE)
    assignment = Assignment.create(Reference(lhs_symbol), Reference(rhs_symbol))
    trans = AssignmentTrans(active_variables=[
        DataSymbol("c", REAL_TYPE), DataSymbol("aa", REAL_TYPE),
        DataSymbol("ab", REAL_TYPE)])
    trans.validate(assignment)


def test_validate_active_rhs():
    '''Test that the validate method returns the expected exception if
    there is at least one active variable on the RHS of an assignment
    but the LHS is not an active variable.

    active vars = ["c", "aa", "b"]
    a = b

    '''
    lhs_symbol = DataSymbol("a", REAL_TYPE)
    rhs_symbol = DataSymbol("b", REAL_TYPE)
    assignment = Assignment.create(Reference(lhs_symbol), Reference(rhs_symbol))
    trans = AssignmentTrans(active_variables=[
        DataSymbol("c", REAL_TYPE), DataSymbol("aa", REAL_TYPE),
        rhs_symbol])
    with pytest.raises(TangentLinearError) as info:
        trans.validate(assignment)
    assert ("Assignment node 'a = b\n' has the following active variables on "
            "its RHS '['b']' but its LHS 'a' is not an active variable."
            in str(info.value))


@pytest.mark.parametrize("operator, string",
                         [(BinaryOperation.Operator.ADD, "+"),
                          (BinaryOperation.Operator.SUB, "-")])
def test_validate_rhs_term_active(operator, string):
    '''Test that the validate method returns the expected exception if one
    of the terms on the rhs does not contain an active variable. Split
    rhs terms with + and - to show both work.

    active vars = ["a", "b"]
    a = b + c
    a = b - c

    '''
    lhs_symbol = DataSymbol("a", REAL_TYPE)
    rhs_symbol1 = DataSymbol("b", REAL_TYPE)
    rhs_symbol2 = DataSymbol("c", REAL_TYPE)
    binary_op = BinaryOperation.create(
        operator, Reference(rhs_symbol1), Reference(rhs_symbol2))
    assignment = Assignment.create(Reference(lhs_symbol), binary_op)
    trans = AssignmentTrans(active_variables=[lhs_symbol, rhs_symbol1])
    with pytest.raises(TangentLinearError) as info:
        trans.validate(assignment)
    assert ("Each term on the RHS of the assigment 'a = b {0} c\n' must have "
            "an active variable but 'c' does not.".format(string)
            in str(info.value))


def test_validate_rhs_assign():
    '''Test that the validate method returns the expected exception if an
    active variable is assigned a non-zero value. This is really just
    a special case of the previous test
    (test_validate_rhs_term_active) but is worth checking separately
    as the assignment of a literal is treated slightly differently in
    the implementation because assigning zero is valid (see
    test_zero).

    active vars = ["a"]
    a = 1.0

    '''
    lhs_symbol = DataSymbol("a", REAL_TYPE)
    rhs_node = Literal("1.0", REAL_TYPE)
    assignment = Assignment.create(Reference(lhs_symbol), rhs_node)
    trans = AssignmentTrans(active_variables=[lhs_symbol])
    with pytest.raises(TangentLinearError) as info:
        trans.validate(assignment)
    assert ("Each term on the RHS of the assigment 'a = 1.0\n' must have "
            "an active variable but '1.0' does not.")


def test_validate_rhs_term_multi_active():
    '''Test that the validate method returns the expected exception if one
    of the 'terms' on the rhs contains more than one active variable.

    active vars = ["a", "b", "c"]
    a = b * c

    '''
    lhs_symbol = DataSymbol("a", REAL_TYPE)
    rhs_symbol1 = DataSymbol("b", REAL_TYPE)
    rhs_symbol2 = DataSymbol("c", REAL_TYPE)
    multiply = BinaryOperation.create(
        BinaryOperation.Operator.MUL, Reference(
            rhs_symbol1), Reference(rhs_symbol2))
    assignment = Assignment.create(Reference(lhs_symbol), multiply)
    trans = AssignmentTrans(active_variables=[
        lhs_symbol, rhs_symbol1, rhs_symbol2])
    with pytest.raises(TangentLinearError) as info:
        trans.validate(assignment)
    assert ("Each term on the RHS of the assigment 'a = b * c\n' must not "
            "have more than one active variable but 'b * c' has 2."
            in str(info.value))


def test_validate_rhs_single_active_var():
    '''Test that the validate method returns successfully if the terms on
    the RHS of an assignment are single active variables.

    active vars = ["a", "b", "c"]
    a = b * c

    '''
    lhs_symbol = DataSymbol("a", REAL_TYPE)
    rhs_symbol = DataSymbol("b", REAL_TYPE)
    assignment = Assignment.create(Reference(lhs_symbol), Reference(rhs_symbol))
    trans = AssignmentTrans(active_variables=[lhs_symbol, rhs_symbol])
    trans.validate(assignment)


@pytest.mark.parametrize("operator", [BinaryOperation.Operator.MUL,
                                      BinaryOperation.Operator.DIV])
def test_validate_rhs_active_var_mul_div(operator):
    '''Test that the validate method returns successfully if the term on
    the RHS of an assignment contains an active variable that is part
    of a set of multiplications or divides and the active variable is
    not part of the divisor. Check when the active var is both to the
    lhs and rhs of a divide.

    active vars = ["a", "b"]
    a = (x*b)*y
    a = (x*b)/y
    a = (x*y)*b
    a = (x/y)*b

    '''
    lhs_symbol = DataSymbol("a", REAL_TYPE)
    rhs_symbol1 = DataSymbol("b", REAL_TYPE)
    rhs_symbol2 = DataSymbol("x", REAL_TYPE)
    rhs_symbol3 = DataSymbol("y", REAL_TYPE)

    multiply = BinaryOperation.create(
        BinaryOperation.Operator.MUL, Reference(
            rhs_symbol2), Reference(rhs_symbol1))
    binary_op = BinaryOperation.create(
        operator, multiply, Reference(rhs_symbol3))
    assignment = Assignment.create(Reference(lhs_symbol), binary_op)
    trans = AssignmentTrans(active_variables=[lhs_symbol, rhs_symbol1])
    trans.validate(assignment)

    binary_op = BinaryOperation.create(
        operator, Reference(rhs_symbol2), Reference(rhs_symbol3))
    multiply = BinaryOperation.create(
        BinaryOperation.Operator.MUL, binary_op, Reference(rhs_symbol1))
    trans = AssignmentTrans(active_variables=[lhs_symbol, rhs_symbol1])
    trans.validate(assignment)


def test_validate_rhs_active_divisor_direct():
    '''Test that the validate method raises the expected exception if a
    term on the RHS of an assignment has an active variable as a
    direct divisor.

    active vars = ["a", "b"]
    a = x/b

    '''
    lhs_symbol = DataSymbol("a", REAL_TYPE)
    rhs_symbol1 = DataSymbol("b", REAL_TYPE)
    rhs_symbol2 = DataSymbol("x", REAL_TYPE)
    divide = BinaryOperation.create(
        BinaryOperation.Operator.DIV, Reference(
            rhs_symbol2), Reference(rhs_symbol1))
    assignment = Assignment.create(Reference(lhs_symbol), divide)
    trans = AssignmentTrans(active_variables=[lhs_symbol, rhs_symbol1])
    with pytest.raises(TangentLinearError) as info:
        trans.validate(assignment)
    assert ("A term on the RHS of the assignment 'a = x / b\n' with a "
            "division must not have the active variable as part of the "
            "divisor but found 'x / b'." in str(info.value))


def test_validate_rhs_active_divisor_indirect():
    '''Test that the validate method raises the expected exception if a
    term on the RHS of an assignment has an active variable as part of
    the divisor.

    active vars = ["a", "b"]
    a = x/(y*b)

    '''
    lhs_symbol = DataSymbol("a", REAL_TYPE)
    rhs_symbol1 = DataSymbol("b", REAL_TYPE)
    rhs_symbol2 = DataSymbol("x", REAL_TYPE)
    rhs_symbol3 = DataSymbol("y", REAL_TYPE)

    multiply = BinaryOperation.create(
        BinaryOperation.Operator.MUL, Reference(
            rhs_symbol3), Reference(rhs_symbol1))
    divide = BinaryOperation.create(
        BinaryOperation.Operator.DIV, Reference(rhs_symbol3), multiply)
    assignment = Assignment.create(Reference(lhs_symbol), divide)
    trans = AssignmentTrans(active_variables=[lhs_symbol, rhs_symbol1])
    with pytest.raises(TangentLinearError) as info:
        trans.validate(assignment)
    assert ("A term on the RHS of the assignment 'a = y / (y * b)\n' with a "
            "division must not have the active variable as part of the "
            "divisor but found 'y / (y * b)'." in str(info.value))


def test_validate_rhs_active_var_no_mul():
    '''Test that the validate method fails if the term on the RHS of the
    assignment contains an active variable that is not part of a set
    of multiplications or divides.

    active vars = ["a", "b"]
    a = b**x

    '''
    lhs_symbol = DataSymbol("a", REAL_TYPE)
    rhs_symbol1 = DataSymbol("b", REAL_TYPE)
    rhs_symbol2 = DataSymbol("x", REAL_TYPE)
    power = BinaryOperation.create(
        BinaryOperation.Operator.POW, Reference(
            rhs_symbol1), Reference(rhs_symbol2))
    assignment = Assignment.create(Reference(lhs_symbol), power)
    trans = AssignmentTrans(active_variables=[lhs_symbol, rhs_symbol1])
    with pytest.raises(TangentLinearError) as info:
        trans.validate(assignment)
    assert ("Each term on the RHS of the assignment 'a = b ** x\n' must be "
            "an active variable multiplied or divided by an expression, but "
            "found 'b ** x'." in str(info.value))


def test_validate_mixed_mul_add():
    '''Test that the validate method fails if the term on the RHS of the
    assignment contains an active variable that is not part of a pure set
    of multiplications or divides.

    active vars = ["a", "b", "c"]
    a = x*(b+y) + c

    '''
    lhs_symbol = DataSymbol("a", REAL_TYPE)
    rhs_symbol1 = DataSymbol("b", REAL_TYPE)
    rhs_symbol2 = DataSymbol("c", REAL_TYPE)
    rhs_symbol3 = DataSymbol("x", REAL_TYPE)
    rhs_symbol4 = DataSymbol("y", REAL_TYPE)
    add1 = BinaryOperation.create(
        BinaryOperation.Operator.ADD, Reference(
            rhs_symbol1), Reference(rhs_symbol4))
    multiply = BinaryOperation.create(
        BinaryOperation.Operator.MUL, Reference(
            rhs_symbol3), add1)
    add2 = BinaryOperation.create(
        BinaryOperation.Operator.ADD, multiply, Reference(rhs_symbol2))
    assignment = Assignment.create(Reference(lhs_symbol), add2)
    trans = AssignmentTrans(active_variables=[
        lhs_symbol, rhs_symbol1, rhs_symbol2])
    with pytest.raises(TangentLinearError) as info:
        trans.validate(assignment)
    assert ("Each term on the RHS of the assignment 'a = x * (b + y) + c\n' "
            "must be an active variable multiplied or divided by an "
            "expression, but found 'x * (b + y)'." in str(info.value))


def test_validate_unaryop():
    '''Test that the validate test fails if a unaryoperation, other than +
    or - is applied directly to an active variable.

    active vars = ["a", "b", "c"]
    a = sqrt(b)

    '''
    lhs_symbol = DataSymbol("a", REAL_TYPE)
    rhs_symbol = DataSymbol("b", REAL_TYPE)
    sqrt = UnaryOperation.create(
        UnaryOperation.Operator.SQRT, Reference(rhs_symbol))
    assignment = Assignment.create(Reference(lhs_symbol), sqrt)
    trans = AssignmentTrans(active_variables=[
        lhs_symbol, rhs_symbol])
    with pytest.raises(TangentLinearError) as info:
        trans.validate(assignment)
    assert ("Each term on the RHS of the assignment 'a = SQRT(b)\n' must be "
            "an active variable multiplied or divided by an expression, but "
            "found 'SQRT(b)'." in str(info.value))


# TODO check that datatypes are all real and raise exception if not.

# TODO 100% coverage: 294, 403, 411, 420

# _split_nodes()


def test_splitnodes_single():
    '''Test that _split_node returns a single entry node_list and an empty
    op_list when there is nothing to split.

    '''
    trans = AssignmentTrans([])
    node = Literal("0.0", REAL_TYPE)
    node_list, op_list = trans._split_nodes(node, [BinaryOperation.Operator.ADD])
    assert isinstance(node_list, list)
    assert len(node_list) == 1
    assert node_list[0] == node
    assert isinstance(op_list, list)
    assert not op_list


def test_splitnodes_multi():
    '''Test that _split_node returns a multiple entry node_list and
    op_list when there are multiple things to split in both lhs and
    rhs.

    '''
    trans = AssignmentTrans([])
    term1 = Literal("1.0", REAL_TYPE)
    term2 = Literal("2.0", REAL_TYPE)
    term3 = Literal("3.0", REAL_TYPE)
    term4 = Literal("4.0", REAL_TYPE)
    add_lhs = BinaryOperation.create(BinaryOperation.Operator.ADD, term1, term2)
    add_rhs = BinaryOperation.create(BinaryOperation.Operator.ADD, term3, term4)
    add = BinaryOperation.create(BinaryOperation.Operator.ADD, add_lhs, add_rhs)
    node_list, op_list = trans._split_nodes(add, [BinaryOperation.Operator.ADD])
    assert isinstance(node_list, list)
    assert node_list == [term1, term2, term3, term4]
    assert isinstance(op_list, list)
    assert op_list == [BinaryOperation.Operator.ADD, BinaryOperation.Operator.ADD, BinaryOperation.Operator.ADD]


def test_splitnodes_multiop():
    '''Test that _split_node returns a multiple entry node_list and
    op_list when there are multiple things to split in both lhs and
    rhs and multiple operators.

    '''
    trans = AssignmentTrans([])
    term1 = Literal("1.0", REAL_TYPE)
    term2 = Literal("2.0", REAL_TYPE)
    term3 = Literal("3.0", REAL_TYPE)
    term4 = Literal("4.0", REAL_TYPE)
    add_lhs = BinaryOperation.create(BinaryOperation.Operator.ADD, term1, term2)
    add_rhs = BinaryOperation.create(BinaryOperation.Operator.ADD, term3, term4)
    add = BinaryOperation.create(BinaryOperation.Operator.SUB, add_lhs, add_rhs)
    node_list, op_list = trans._split_nodes(add, [BinaryOperation.Operator.ADD, BinaryOperation.Operator.SUB])
    assert isinstance(node_list, list)
    assert node_list == [term1, term2, term3, term4]
    assert isinstance(op_list, list)
    assert op_list == [BinaryOperation.Operator.ADD, BinaryOperation.Operator.SUB, BinaryOperation.Operator.ADD]

# TODO str and name
