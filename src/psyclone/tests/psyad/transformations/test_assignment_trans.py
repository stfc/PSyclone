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
    Routine
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.transformations import TransformationError

from psyclone.psyad.transformations import AssignmentTrans, TangentLinearError


def check_adjoint(tl_fortran, active_variable_names, expected_ad_fortran):
    '''Utility routine that takes tangent linear fortran code as input in
    the argument tl_fortran, transforms this code into its adjoint
    using the active variables specified in the active variable_names
    argument and tests whether the result is the same as the expected
    result in the expected_ad_fortran argument.

    :param str tl_fortran: tangent linear code.
    :param list of str active_variable_names: a list of active \
        variable names.
    :param str tl_fortran: the expected adjoint code to be produced.

    '''
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
    '''Test that the adjoint transformation with an assignment of the form
    A = 0. This tests that the transformation works when there are no
    active variables on the rhs and with the active variable on the lhs
    being a write, not an increment. Scalars, directly addressed
    arrays, indirectly addressed arrays and structure array accesses
    are tested.

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
    increment.

    A=xB -> B*=B*+xA*;A*=0.0

    '''
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
    '''Test that the adjoint transformation with an assignment of the form
    A = xB + yC + D. This tests that the transformation works when
    there are many active variables on the rhs with some of them being
    multipled by a factor and with the active variable on the lhs
    being a write, not an increment.

    A=xB+yC+D -> D*=D*+A; C*=C*+yA*; B*=B*+xA*; A*=0.0

    '''
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
    additions on the rhs with the lhs being a scaled increment.

    A=xA -> A*=xA*

    '''
    # Scalar
    tl_fortran = (
        "  integer :: n\n"
        "  real a(n)\n"
        "  a(n) = 5*a(n)\n")
    active_variables = ["a"]
    ad_fortran = (
        "  integer :: n\n"
        "  real, dimension(n) :: a\n\n"
        "  a(n) = 5 * a(n)\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


def test_increment_add():
    '''Test that the adjoint transformation with an assignment of the form
    A = A + B. This tests that the transformation works when there is
    a single addition on the rhs with the lhs being an increment.

    A+=B -> B*=A*; A*=A*

    '''
    tl_fortran = (
        "  real a(10), b(10)\n"
        "  a(1) = a(1)+b(1)\n")
    active_variables = ["a", "b"]
    ad_fortran = (
        "  real, dimension(10) :: a\n"
        "  real, dimension(10) :: b\n\n"
        "  b(1) = b(1) + a(1)\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)


def test_increment_add_reorder():
    '''Test that the adjoint transformation with an assignment of the form
    A = B + A. This tests that the transformation works when there is
    a single addition on the rhs with the lhs being a scaled increment
    and the increment not being on the lhs of the rhs.

    A=B+kA -> B*=A*; A*=kA*

    *** FAILS ***

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
        "  a(1) = w(1)*a(1)+x*b(1)+y(1)*c(1)+z*d(1)\n")
    active_variables = ["a", "b", "c", "d"]
    ad_fortran = (
        "  real, dimension(10) :: a\n  real, dimension(10) :: b\n"
        "  real, dimension(10) :: c\n  real, dimension(10) :: d\n"
        "  real, dimension(10) :: w\n  real :: x\n"
        "  real, dimension(10) :: y\n  real :: z\n\n"
        "  d(1) = d(1) + z * a(1)\n"
        "  c(1) = c(1) + y(1) * a(1)\n"
        "  b(1) = b(1) + x * a(1)\n"
        "  a(1) = w(1) * a(1)\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran)

# TODO
# check a = b + ya as a should be assigned after (test error)
# a = -b -yc
# a(i) = a(i+1) + b(i) + b(i+1)
# * other datatypes (assuming all real for the moment) and ignoring precision

# Validate method

def test_validate_node():
    '''Check that the expected exception is raised if the provided node
    argument is not a PSyIR Assignment node.'''
    trans = AssignmentTrans(active_variables=[])
    with pytest.raises(TransformationError) as info:
        trans.validate(None)
    assert ("Node argument in assignment transformation should be a PSyIR "
            "Assignment, but found 'NoneType'." in str(info.value))


def test_validate_not_active():
    '''Test that the validate method returns without error if there are no
    active variables in the assignment.'''
    lhs_symbol = DataSymbol("a", REAL_TYPE)
    rhs_symbol = DataSymbol("b", REAL_TYPE)
    assignment = Assignment.create(Reference(lhs_symbol), Reference(rhs_symbol))
    trans = AssignmentTrans(active_variables=["c", "aa", "ab"])
    trans.validate(assignment)

def test_validate_active_rhs():
    '''Test that the validate method returns the expected exception if
    there is at least one active variable on the RHS of an assignment
    but the LHS is not an active variable.'''
    lhs_symbol = DataSymbol("a", REAL_TYPE)
    rhs_symbol = DataSymbol("b", REAL_TYPE)
    assignment = Assignment.create(Reference(lhs_symbol), Reference(rhs_symbol))
    trans = AssignmentTrans(active_variables=["c", "aa", "b"])
    with pytest.raises(TangentLinearError) as info:
        trans.validate(assignment)
    assert ("Assignment node has the following active variables on its RHS "
            "'['b']' but its LHS 'a' is not an active variable."
            in str(info.value))
