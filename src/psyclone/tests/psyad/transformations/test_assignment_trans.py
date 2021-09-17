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

from __future__ import absolute_import
import pytest

from psyclone.psyad.transformations import AssignmentTrans, TangentLinearError

from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import BinaryOperation, Reference, Assignment, \
    Literal, UnaryOperation
from psyclone.psyir.symbols import DataSymbol, REAL_TYPE, INTEGER_TYPE, \
    ScalarType
from psyclone.psyir.transformations import TransformationError
from psyclone.tests.utilities import Compile


def check_adjoint(tl_fortran, active_variable_names, expected_ad_fortran,
                  tmpdir):
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
    :param str tmpdir: temporary directory created by pytest in which \
        to perform compilation.

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

    # Check that the code produced will compile.
    assert Compile(tmpdir).string_compiles(ad_fortran)

# apply() method


def test_zero(tmpdir):
    '''Test the adjoint transformation with an assignment of the form A =
    0. This is the only valid value that an active variable can be set
    to in a tangent linear code and represents multiplying an active
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
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)
    # Direct Addressed Array
    tl_fortran = (
        "  integer, parameter :: n=10\n"
        "  real :: a(n)\n"
        "  a(n) = 0.0\n\n")
    active_variables = ["a"]
    ad_fortran = (
        "  integer, parameter :: n = 10\n"
        "  real, dimension(n) :: a\n\n"
        "  a(n) = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)
    # Indirect Addressed Array
    tl_fortran = (
        "  integer, parameter :: n=10\n"
        "  real :: a(n)\n"
        "  integer :: b(n)\n"
        "  a(b(n)) = 0.0\n\n")
    active_variables = ["a"]
    ad_fortran = (
        "  integer, parameter :: n = 10\n"
        "  real, dimension(n) :: a\n"
        "  integer, dimension(n) :: b\n\n"
        "  a(b(n)) = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


@pytest.mark.xfail(reason="issue #1347, type declaration is being written in "
                   "the wrong order causing compilation failure.")
def test_zero_fail(tmpdir):
    '''This test is split from the previous one as it is currently failing
    when the compile option is set to True due to the typedef being
    written in the wrong order. Once this problem is fixed this test
    should be merged back with the previous test_zero() test.

    '''
    # Structure
    tl_fortran = (
        "  type :: field_type\n"
        "    real :: data(10)\n"
        "  end type\n"
        "  type(field_type) :: a\n"
        "  integer, parameter :: n=10\n"
        "  a%data(n) = 0.0\n\n")
    active_variables = ["a"]
    ad_fortran = (
        "  type :: field_type\n"
        "    real, dimension(10) :: data\n"
        "  end type field_type\n\n"
        "  type(field_type) :: a\n"
        "  integer, parameter :: n = 10\n"
        "  a%data(n) = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


def test_single_assign(tmpdir):
    '''Test the transformation works when there is one active variable on
    the rhs (B) and with the active variable on the lhs (A) only being
    written to, i.e. not also read on the rhs. Scalars, directly
    addressed arrays, indirectly addressed arrays and structure array
    accesses are tested.

    A=B -> B*=B*+A*; A*=0.0

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
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)
    # Direct addressed array
    tl_fortran = (
        "  integer, parameter :: n=10\n"
        "  real :: a(n),b(n)\n"
        "  integer, parameter :: i=2\n"
        "  a(2*i) = b(n-1)\n")
    active_variables = ["a", "b"]
    ad_fortran = (
        "  integer, parameter :: n = 10\n  integer, parameter :: i = 2\n"
        "  real, dimension(n) :: a\n"
        "  real, dimension(n) :: b\n\n"
        "  b(n - 1) = b(n - 1) + a(2 * i)\n"
        "  a(2 * i) = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)
    # Indirect addressed array
    tl_fortran = (
        "  integer, parameter :: n=10\n"
        "  real :: a(n),b(n)\n"
        "  integer :: i,lookup(n)\n"
        "  a(lookup(2*i)) = b(lookup(n)+1)\n")
    active_variables = ["a", "b"]
    ad_fortran = (
        "  integer, parameter :: n = 10\n  real, dimension(n) :: a\n"
        "  real, dimension(n) :: b\n  integer :: i\n"
        "  integer, dimension(n) :: lookup\n\n"
        "  b(lookup(n) + 1) = b(lookup(n) + 1) + a(lookup(2 * i))\n"
        "  a(lookup(2 * i)) = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


@pytest.mark.xfail(reason="issue #1347, type declaration is being written in "
                   "the wrong order causing compilation failure.")
def test_single_assign_fail(tmpdir):
    '''This test is split from the previous one as it is currently failing
    when the compile option is set to True due to the typedef being
    written in the wrong order. Once this problem is fixed this test
    should be merged back with the previous test_single_assign() test.

    '''
    # Structure
    tl_fortran = (
        "  type :: field_type\n"
        "    real :: data(10)\n"
        "  end type\n"
        "  type(field_type) :: a,b\n"
        "  integer, parameter :: n=2\n"
        "  integer, parameter :: i=2\n\n"
        "  a%data(2*i) = b%data(n+1)\n")
    active_variables = ["a", "b"]
    ad_fortran = (
        "  type :: field_type\n"
        "    real, dimension(10) :: data\n"
        "  end type field_type\n"
        "  type(field_type) :: a\n"
        "  type(field_type) :: b\n"
        "  integer, parameter :: n = 2\n"
        "  integer, parameter :: i = 2\n\n"
        "  b%data(n + 1) = b%data(n + 1) + a%data(2 * i)\n"
        "  a%data(2 * i) = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


def test_single_valued_assign(tmpdir):
    '''Test the transformation works when there is one active variable on
    the rhs (B) that is multipled by a factor (x) and with the active
    variable on the lhs (A) only being written to, i.e. not also read
    on the rhs. Also test mixed-case active variables list and actual
    variables.

    A=xB -> B*=B*+xA*; A*=0.0

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
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


@pytest.mark.xfail(reason="issue #1332 literal math_equal() does "
                   "Not work properly.")
def test_multi_add(tmpdir):
    '''Test the transformation works when there are many active variables
    on the rhs (B,C,D) with some of them being multipled by a factor
    and with the active variable on the lhs (A) only being written to,
    i.e. is not also read on the rhs. Also test mixed-case
    declarations.

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
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


def test_increment(tmpdir):
    '''Test the adjoint transformation with an assignment of the form
    A = A.

    A=A -> A*=A*

    As A does not change we output nothing.

    '''
    tl_fortran = (
        "  integer, parameter :: n=2\n"
        "  real a(n)\n"
        "  a(n) = a(n)\n")
    active_variables = ["a"]
    ad_fortran = (
        "  integer, parameter :: n = 2\n"
        "  real, dimension(n) :: a\n\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


def test_increment_mult(tmpdir):
    '''Test the transformation works when the active variable that is
    written to on the lhs (A) is also read (and scaled (x)) on the rhs
    and there are no other active variables on the rhs. Also test
    mixed-case variables.

    A=xA -> A*=xA*

    '''
    tl_fortran = (
        "  integer, parameter :: n=4\n"
        "  real a(n)\n"
        "  A(n) = 5*A(n)\n")
    active_variables = ["a"]
    ad_fortran = (
        "  integer, parameter :: n = 4\n"
        "  real, dimension(n) :: a\n\n"
        "  a(n) = 5 * a(n)\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


def test_increment_add(tmpdir):
    '''Test the transformation works when the active variable that is
    written to on the lhs (A) is also read on the rhs and there is a
    another active variable (B) read on the rhs. Also test mixed-case
    variables.

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
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


def test_increment_add_reorder(tmpdir):
    '''Test the transformation works when the active variable that is
    written to on the lhs (A) is also read (and scaled (k)) on the rhs
    and there is another active variable (B) read on the
    rhs. Additionally, the active variable A is not the first term on
    the rhs (where we define terms as expressions separated by an
    addition (or subtraction)).

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
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


def test_increment_multi_add(tmpdir):
    '''Test the transformation works when the active variable that is
    written to on the lhs (A) is also read (and scaled (w)) on the rhs
    and there are many other active variables on the rhs (B, C, D),
    which are also scaled.

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
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


def test_multi_increment(tmpdir):
    '''Test the code works when the active variable that is written to on
    the lhs (A) is also read on the rhs, but is read more than once in
    different terms, some of which are scaled. The resultant adjoint
    code has the same form as the tangent-linear code.

    A=A+xA -> A=A+xA.

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
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


def test_single_valued_sub(tmpdir):
    '''Test the transformation works when there is one active variable on
    the rhs (B) that is negated, with the active variable on the lhs
    (A) only being written to, i.e. not also read on the rhs.

    A=-B -> B*=B*-A*; A*=0.0

    '''
    tl_fortran = (
        "  real a(10), b(10)\n"
        "  integer :: i,j\n"
        "  a(i) = -b(j)\n")
    active_variables = ["a", "b"]
    ad_fortran = (
        "  real, dimension(10) :: a\n  real, dimension(10) :: b\n"
        "  integer :: i\n  integer :: j\n\n"
        "  b(j) = b(j) + (-a(i))\n"
        "  a(i) = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


@pytest.mark.xfail(reason="issue #1333 Unary '-' should be enclosed in "
                   "brackets.")
def test_multi_valued_sub(tmpdir):
    '''Test the transformation works when there are multiple active
    variable on the rhs (B, C, D) that have unary plus and minus
    operators as well as minus separating one of the terms. The active
    variable on the lhs (A) is only written to, i.e. is not also read
    on the rhs.

    A=-B-x(+C)+(-D)y -> B*=B*-A*; C*=C*-xA*; D*=D*-yA*; A*=0.0

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
        "  b(j) = b(j) + (-a(i))\n"
        "  c(i) = c(i) - x * (+a(i))\n"
        "  d(j) = d(j) + (-a(i)) * y\n"
        "  a(i) = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


def test_inc_sub(tmpdir):
    '''Test the transformation works when the active variable that is
    written to on the lhs is also read (and multiplied by -1) on the
    rhs. There are no other active variables on the rhs.

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
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


def test_multi_inc_sub(tmpdir):
    '''Test the transformation works when the active variable that is
    written to on the lhs (A) is also read multiple times from
    different terms on the rhs. The terms on the rhs combine the
    active variable with an inactive variable (or no inactive
    variable) in different supported combinations (multiplication,
    division, unary minus). The terms are also combined on the rhs
    with both addition and subtraction.

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
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


def test_multi_rhs(tmpdir):
    '''Test the transformation works when there are multiple terms on the
    rhs of an assignment with the same active variable (B) and the
    active variable on the lhs (A) is only written to, i.e. not also
    read on the rhs. Also includes a division rather than a
    multiplication in one of the rhs terms. The terms on the rhs
    combine the active variable (B) with an inactive variable in
    different supported combinations (division as well as
    multiplication).

    A=B+xB+B/y -> B*=B*+A*; B*=B*+xA*; B*=B*+A*/y

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
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


def test_different_indices(tmpdir):
    '''Test the adjoint transformation recognises that a write and a read
    access to the same active array (A) but with different array
    indices count as separate accesses. Also test that two reads of
    the same array (B) but with different array indices count as
    separate accesses.

    A(i)=A(i+1)+B(i)+B(i-1) -> B*(i-1)=B*(i-1)+A*(i); B*(i)=B*(i)*+A*(i);
                                       A*(i+1)=A*(i+1)+A*(i); A*(i)=0.0

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
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


def test_same_indices_ordering(tmpdir):
    '''Test the adjoint transformation recognises a write and a read
    access to the same active array (A) with the same indices, but
    where the indices are specified in a different form (the ordering
    of the addition differs).

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
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


@pytest.mark.xfail(reason="issue #1075: Better symbolic comparison of indices "
                   "is needed.")
def test_same_indices_ordering2(tmpdir):
    '''Test the adjoint transformation recognises a write and a read
    access to the same active array (A) with the same indices, but
    where the indices are specified in a different form
    (multiplication vs. addition).

    A(2*i)=A(i+i)+B(i) -> B*(i)=B*(i)*+A*(2 * i)

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
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


@pytest.mark.xfail(reason="issue #1332 math_equal thinks a%b(i) equals a%c(i)")
def test_different_structures(tmpdir):
    '''Test the adjoint transformation recognises a distinct write and
    read within a structure (A).

    A%data(i)=A%data(i+i)+A%X(i) -> A%data*(i+1)=A%data*(i+1)+A%data*(i);
                                    A%x*(i)=A%x*(i)+A%data*(i);
                                    a%data*(i)=0.0

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
        "  a%data(n + 1) = a%data(n + 1) + a%data(n)\n"
        "  a%atad(n) = a%atad(n) + a%data(n)\n"
        "  a%data(n) = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


@pytest.mark.parametrize("in_op1,in_op2,out_op", [
    ("+", "+", "+"), ("+", "-", "-"),
    ("-", "+", "-"), ("-", "-", "+")])
def test_precedence_active_vars(in_op1, in_op2, out_op, tmpdir):
    '''Test precedence of active variables is taken into account (as the
    sign may need to be flipped in certain cases). For example, see
    'd' below:

    a = b+(c+d) => b*=b*-a*; c*=c*+a*; d*=d*+a*; a*=0
    a = b+(c-d) => b*=b*-a*; c*=c*+a*; d*=d*-a*; a*=0
    a = b-(c+d) => b*=b*-a*; c*=c*-a*; d*=d*-a*; a*=0
    a = b-(c-d) => b*=b*-a*; c*=c*-a*; d*=d*+a*; a*=0

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
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


def test_unary_binary_minus(tmpdir):
    '''Test precedence of active variables is taken into account (as in
    the previous test) but also add unary operations to check they
    work together correctly.

    a=b-(c-(-d)) => b*=b*+a*; c*=c*-a*; d*=d*+(-a*); a=0.0

    '''
    tl_fortran = (
        "  real :: a,b,c,d\n"
        "  a=b-(c-(-d))\n")
    active_variables = ["a", "b", "c", "d"]
    ad_fortran = (
        "  real :: a\n  real :: b\n"
        "  real :: c\n  real :: d\n\n"
        "  b = b + a\n"
        "  c = c - a\n"
        "  d = d + (-a)\n"
        "  a = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)

# validate() method


def test_validate_node():
    '''Check that the expected exception is raised if the provided node
    argument is not a PSyIR Assignment node.

    '''
    trans = AssignmentTrans(active_variables=[DataSymbol("x", REAL_TYPE)])
    with pytest.raises(TransformationError) as info:
        trans.validate(None)
    assert ("Node argument in assignment transformation should be a PSyIR "
            "Assignment, but found 'NoneType'." in str(info.value))


def test_validate_not_active():
    '''Test the validate method returns without error if there are no
    active variables in the assignment.

    active vars = ["c", "aa", "ab"]
    a = b

    '''
    lhs_symbol = DataSymbol("a", REAL_TYPE)
    rhs_symbol = DataSymbol("b", REAL_TYPE)
    assignment = Assignment.create(
        Reference(lhs_symbol), Reference(rhs_symbol))
    trans = AssignmentTrans(active_variables=[
        DataSymbol("c", REAL_TYPE), DataSymbol("aa", REAL_TYPE),
        DataSymbol("ab", REAL_TYPE)])
    trans.validate(assignment)


def test_validate_active_rhs():
    '''Test the validate method returns the expected exception if
    there is at least one active variable on the RHS of an assignment
    but the LHS is not an active variable.

    active vars = ["c", "aa", "b"]
    a = b

    '''
    lhs_symbol = DataSymbol("a", REAL_TYPE)
    rhs_symbol = DataSymbol("b", REAL_TYPE)
    assignment = Assignment.create(
        Reference(lhs_symbol), Reference(rhs_symbol))
    trans = AssignmentTrans(active_variables=[
        DataSymbol("c", REAL_TYPE), DataSymbol("aa", REAL_TYPE),
        rhs_symbol])
    with pytest.raises(TangentLinearError) as info:
        trans.validate(assignment)
    assert ("Assignment node 'a = b\n' has the following active variables on "
            "its RHS '['b']' but its LHS 'a' is not an active variable."
            in str(info.value))


def test_validate_rhs_zero():
    '''Test the validate method returns successfully if the rhs
    contains zero. There are various representations of zero so we
    check these to make sure all are recognised as expected.

    '''
    lhs_symbol = DataSymbol("a", REAL_TYPE)
    trans = AssignmentTrans(active_variables=[lhs_symbol])
    # Various forms of 0
    for zero_str in ["0.0", "0", "0.00", "0.", "0.e0"]:
        rhs_literal = Literal(zero_str, REAL_TYPE)
        assignment = Assignment.create(Reference(lhs_symbol), rhs_literal)
        trans.validate(assignment)
    # 0 with a kind value
    real_kind = DataSymbol("r_def", INTEGER_TYPE, constant_value=8)
    scalar_type = ScalarType(ScalarType.Intrinsic.REAL, real_kind)
    rhs_literal = Literal("0.0", scalar_type)
    assignment = Assignment.create(Reference(lhs_symbol), rhs_literal)
    trans.validate(assignment)


@pytest.mark.parametrize("operator, string",
                         [(BinaryOperation.Operator.ADD, "+"),
                          (BinaryOperation.Operator.SUB, "-")])
def test_validate_rhs_term_active(operator, string):
    '''Test the validate method returns the expected exception if one
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
    assert ("Each non-zero term on the RHS of the assigment 'a = b {0} c\n' "
            "must have an active variable but 'c' does not.".format(string)
            in str(info.value))


def test_validate_rhs_assign():
    '''Test the validate method returns the expected exception if an
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
    assert ("Each non-zero term on the RHS of the assigment 'a = 1.0\n' must "
            "have an active variable but '1.0' does not." in str(info.value))


def test_validate_rhs_term_multi_active():
    '''Test the validate method returns the expected exception if one
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
    '''Test the validate method returns successfully if the terms on
    the RHS of an assignment are single active variables.

    active vars = ["a", "b"]
    a = b

    '''
    lhs_symbol = DataSymbol("a", REAL_TYPE)
    rhs_symbol = DataSymbol("b", REAL_TYPE)
    assignment = Assignment.create(
        Reference(lhs_symbol), Reference(rhs_symbol))
    trans = AssignmentTrans(active_variables=[lhs_symbol, rhs_symbol])
    trans.validate(assignment)


@pytest.mark.parametrize("operator", [BinaryOperation.Operator.MUL,
                                      BinaryOperation.Operator.DIV])
def test_validate_rhs_active_var_mul_div(operator):
    '''Test the validate method returns successfully if the term on the
    RHS of an assignment contains an active variable (b) that is part
    of a set of multiplications or divides and the active variable is
    not part of the denominator.

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

    # x*b
    multiply = BinaryOperation.create(
        BinaryOperation.Operator.MUL, Reference(
            rhs_symbol2), Reference(rhs_symbol1))
    # (x*b)*/y
    binary_op = BinaryOperation.create(
        operator, multiply, Reference(rhs_symbol3))
    # a = (x*b)*/y
    assignment = Assignment.create(Reference(lhs_symbol), binary_op)
    trans = AssignmentTrans(active_variables=[lhs_symbol, rhs_symbol1])
    trans.validate(assignment)

    # x*/y
    binary_op = BinaryOperation.create(
        operator, Reference(rhs_symbol2), Reference(rhs_symbol3))
    # (x*/y)*b
    multiply = BinaryOperation.create(
        BinaryOperation.Operator.MUL, binary_op, Reference(rhs_symbol1))
    # a = (x*/y)*b
    assignment = Assignment.create(Reference(lhs_symbol), multiply)
    trans.validate(assignment)


def test_validate_rhs_active_divisor_direct():
    '''Test the validate method raises the expected exception if a term on
    the RHS of an assignment has an active variable (b) as a direct
    divisor.

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
    assert ("In tangent-linear code an active variable cannot appear as a "
            "denominator but 'x / b' was found in 'a = x / b\n'."
            in str(info.value))


def test_validate_rhs_active_divisor_indirect():
    '''Test the validate method raises the expected exception if a term on
    the RHS of an assignment has an active variable (b) as part of the
    divisor.

    active vars = ["a", "b"]
    a = x/(y*b)

    '''
    lhs_symbol = DataSymbol("a", REAL_TYPE)
    rhs_symbol1 = DataSymbol("b", REAL_TYPE)
    rhs_symbol2 = DataSymbol("x", REAL_TYPE)
    rhs_symbol3 = DataSymbol("y", REAL_TYPE)

    # y*b
    multiply = BinaryOperation.create(
        BinaryOperation.Operator.MUL, Reference(
            rhs_symbol3), Reference(rhs_symbol1))
    # x/(y*b)
    divide = BinaryOperation.create(
        BinaryOperation.Operator.DIV, Reference(rhs_symbol2), multiply)
    # a = x/(y*b)
    assignment = Assignment.create(Reference(lhs_symbol), divide)
    trans = AssignmentTrans(active_variables=[lhs_symbol, rhs_symbol1])
    with pytest.raises(TangentLinearError) as info:
        trans.validate(assignment)
    assert ("In tangent-linear code an active variable cannot appear as a "
            "denominator but 'x / (y * b)' was found in 'a = x / (y * b)\n'."
            in str(info.value))


@pytest.mark.xfail(reason="issue #1348. If an active variable is within the "
                   "denominator for an even number of divisions it is "
                   "equivalent to a multiply.")
def test_validate_rhs_active_multi_divisor():
    '''If an active variable is within the denominator for an even number
    of divisions it is equivalent to a multiply e.g. a/b/c == (a/b)*c
    and should be considered a valid term. However, this case is not
    recognised in the current implementation.

    active vars = ["a", "b"]
    a = x/y/b

    '''
    lhs_symbol = DataSymbol("a", REAL_TYPE)
    rhs_symbol1 = DataSymbol("b", REAL_TYPE)
    rhs_symbol2 = DataSymbol("x", REAL_TYPE)
    rhs_symbol3 = DataSymbol("y", REAL_TYPE)

    # y/b
    divide1 = BinaryOperation.create(
        BinaryOperation.Operator.DIV, Reference(
            rhs_symbol3), Reference(rhs_symbol1))
    # x/(y/b)
    divide2 = BinaryOperation.create(
        BinaryOperation.Operator.DIV, Reference(rhs_symbol2), divide1)
    # a = x/(y/b)
    assignment = Assignment.create(Reference(lhs_symbol), divide2)
    trans = AssignmentTrans(active_variables=[lhs_symbol, rhs_symbol1])
    trans.validate(assignment)


def test_validate_rhs_active_var_no_mul():
    '''Test the validate method fails if the term on the RHS of the
    assignment contains an active variable (b) that is not part of a
    set of multiplications or divides.

    active vars = ["a", "b"]
    a = b**x

    '''
    lhs_symbol = DataSymbol("a", REAL_TYPE)
    rhs_symbol1 = DataSymbol("b", REAL_TYPE)
    rhs_symbol2 = DataSymbol("x", REAL_TYPE)
    # b**x
    power = BinaryOperation.create(
        BinaryOperation.Operator.POW, Reference(
            rhs_symbol1), Reference(rhs_symbol2))
    # a = b**x
    assignment = Assignment.create(Reference(lhs_symbol), power)
    trans = AssignmentTrans(active_variables=[lhs_symbol, rhs_symbol1])
    with pytest.raises(TangentLinearError) as info:
        trans.validate(assignment)
    assert ("Each term on the RHS of the assignment 'a = b ** x\n' must "
            "be linear with respect to the active variable, "
            "but found 'b ** x'." in str(info.value))


def test_validate_mixed_mul_add():
    '''Test the validate method fails if the term on the RHS of the
    assignment contains an active variable that is not part of a pure
    set of multiplications or divides (b) as the code will not be
    linear with respect to the active variables. In this particular
    case, expanding the example ends up with a constant term x*y.

    active vars = ["a", "b", "c"]
    a = x*(b+y) + c

    '''
    lhs_symbol = DataSymbol("a", REAL_TYPE)
    rhs_symbol1 = DataSymbol("b", REAL_TYPE)
    rhs_symbol2 = DataSymbol("c", REAL_TYPE)
    rhs_symbol3 = DataSymbol("x", REAL_TYPE)
    rhs_symbol4 = DataSymbol("y", REAL_TYPE)
    # b+y
    add1 = BinaryOperation.create(
        BinaryOperation.Operator.ADD, Reference(
            rhs_symbol1), Reference(rhs_symbol4))
    # x*(b+y)
    multiply = BinaryOperation.create(
        BinaryOperation.Operator.MUL, Reference(
            rhs_symbol3), add1)
    # x*(b+y)+c
    add2 = BinaryOperation.create(
        BinaryOperation.Operator.ADD, multiply, Reference(rhs_symbol2))
    # a = x*(b+y)+c
    assignment = Assignment.create(Reference(lhs_symbol), add2)
    trans = AssignmentTrans(active_variables=[
        lhs_symbol, rhs_symbol1, rhs_symbol2])
    with pytest.raises(TangentLinearError) as info:
        trans.validate(assignment)
    assert ("Each term on the RHS of the assignment 'a = x * (b + y) + c\n' "
            "must be linear with respect to the active variable, but found "
            "'x * (b + y)'." in str(info.value))


def test_validate_unaryop():
    '''Test the validate test fails if a unaryoperation, other than +
    or - is applied directly to an active variable (b).

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
            "linear with respect to the active variable, but found 'SQRT(b)'."
            in str(info.value))

# _split_nodes() method


def test_splitnodes_single():
    '''Test _split_nodes returns a single entry node_list when there
    is nothing to split.

    '''
    trans = AssignmentTrans([DataSymbol("x", REAL_TYPE)])
    node = Literal("0.0", REAL_TYPE)
    node_list = trans._split_nodes(
        node, [BinaryOperation.Operator.ADD])
    assert isinstance(node_list, list)
    assert len(node_list) == 1
    assert node_list[0] == node


def test_splitnodes_multi():
    '''Test _split_nodes returns a multiple entry node_list when there
    are multiple things to split in both lhs and rhs.

    '''
    trans = AssignmentTrans([DataSymbol("x", REAL_TYPE)])
    term1 = Literal("1.0", REAL_TYPE)
    term2 = Literal("2.0", REAL_TYPE)
    term3 = Literal("3.0", REAL_TYPE)
    term4 = Literal("4.0", REAL_TYPE)
    # 1.0+2.0
    add_lhs = BinaryOperation.create(
        BinaryOperation.Operator.ADD, term1, term2)
    # 3.0+4.0
    add_rhs = BinaryOperation.create(
        BinaryOperation.Operator.ADD, term3, term4)
    # (1.0+2.0)+(3.0+4.0)
    add = BinaryOperation.create(
        BinaryOperation.Operator.ADD, add_lhs, add_rhs)
    node_list = trans._split_nodes(
        add, [BinaryOperation.Operator.ADD])
    assert isinstance(node_list, list)
    assert node_list == [term1, term2, term3, term4]


def test_splitnodes_multiop():
    '''Test _split_nodes returns a multiple entry node_list when there
    are multiple things to split in both lhs and rhs and multiple
    operators.

    '''
    trans = AssignmentTrans([DataSymbol("x", REAL_TYPE)])
    term1 = Literal("1.0", REAL_TYPE)
    term2 = Literal("2.0", REAL_TYPE)
    term3 = Literal("3.0", REAL_TYPE)
    term4 = Literal("4.0", REAL_TYPE)
    # 1.0+2.0
    add_lhs = BinaryOperation.create(
        BinaryOperation.Operator.ADD, term1, term2)
    # 3.0+4.0
    add_rhs = BinaryOperation.create(
        BinaryOperation.Operator.ADD, term3, term4)
    # (1.0+2.0)-(3.0+4.0)
    add = BinaryOperation.create(
        BinaryOperation.Operator.SUB, add_lhs, add_rhs)
    node_list = trans._split_nodes(
        add, [BinaryOperation.Operator.ADD, BinaryOperation.Operator.SUB])
    assert isinstance(node_list, list)
    assert node_list == [term1, term2, term3, term4]


def test_splitnodes_different_op():
    '''Test _split_nodes returns a multiple entry node_list when the
    expression has binary operators not specified in the list.

    '''
    trans = AssignmentTrans([DataSymbol("x", REAL_TYPE)])
    term1 = Literal("1.0", REAL_TYPE)
    term2 = Literal("2.0", REAL_TYPE)
    term3 = Literal("3.0", REAL_TYPE)
    term4 = Literal("4.0", REAL_TYPE)
    # 1.0*2.0
    mul_lhs = BinaryOperation.create(
        BinaryOperation.Operator.MUL, term1, term2)
    # 3.0/4.0
    div_rhs = BinaryOperation.create(
        BinaryOperation.Operator.DIV, term3, term4)
    # (1.0*2.0)+(3.0/4.0)
    add = BinaryOperation.create(
        BinaryOperation.Operator.ADD, mul_lhs, div_rhs)
    node_list = trans._split_nodes(
        add, [BinaryOperation.Operator.ADD])
    assert isinstance(node_list, list)
    assert node_list == [mul_lhs, div_rhs]


def test_splitnodes_no_split():
    '''Test _split_nodes returns a single entry node_list when the
    expression has binary operators specified in the list but they are
    within other binary operators that are not specified in the list.

    '''
    trans = AssignmentTrans([DataSymbol("x", REAL_TYPE)])
    term1 = Literal("1.0", REAL_TYPE)
    term2 = Literal("2.0", REAL_TYPE)
    term3 = Literal("3.0", REAL_TYPE)
    term4 = Literal("4.0", REAL_TYPE)
    # 1.0*2.0
    mul_lhs = BinaryOperation.create(
        BinaryOperation.Operator.MUL, term1, term2)
    # 3.0*4.0
    mul_rhs = BinaryOperation.create(
        BinaryOperation.Operator.MUL, term3, term4)
    # (1.0*2.0)+(3.0*4.0)
    add = BinaryOperation.create(
        BinaryOperation.Operator.ADD, mul_lhs, mul_rhs)
    node_list = trans._split_nodes(
        add, [BinaryOperation.Operator.MUL])
    assert isinstance(node_list, list)
    assert node_list == [add]

# str() and name() methods


def test_str():
    ''' Test the str operation returns the expected result.'''

    assignment_trans = AssignmentTrans([DataSymbol("x", REAL_TYPE)])
    assert (str(assignment_trans) == "Convert a tangent-linear PSyIR "
            "Assignment to its adjoint form")


def test_name():
    ''' Test the name method returns the expected result.'''

    assignment_trans = AssignmentTrans([DataSymbol("x", REAL_TYPE)])
    assert assignment_trans.name == "AssignmentTrans"
