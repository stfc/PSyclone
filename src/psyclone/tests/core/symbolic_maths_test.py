# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2021, Science and Technology Facilities Council.
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
# Modified: A. R. Porter and R. W. Ford  STFC Daresbury Lab
# Modified: I. Kavcic, Met Office


''' Module containing py.test tests for dependency analysis.'''

from __future__ import print_function, absolute_import
import os
import pytest

from fparser.common.readfortran import FortranStringReader

from psyclone.core import SymbolicMaths
from psyclone.psyGen import PSyFactory

# Constants
API = "nemo"
# Location of the Fortran files associated with these tests
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files")


def test_sym_maths_get():
    '''Makes sure that the getter works as expected, especially
    that sympy can be imported.'''

    sym_maths = SymbolicMaths.get()
    assert sym_maths is not None


def test_math_equal(parser):
    '''Tests the math_equal function of nodes in the PSyIR.'''

    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    reader = FortranStringReader('''program test_prog
                                    integer :: x(2,2), a(2,2), b, c, i, j, k
                                    x = a                 !  0
                                    x = a                 !  1
                                    x = b                 !  2
                                    x = a+12*b*sin(c)     !  3
                                    x = 12*b*sin(c)+a     !  4
                                    x = i+j               !  5
                                    x = j+i               !  6
                                    x = i-j               !  7
                                    x = j-i               !  8
                                    x = max(1, 2, 3, 4)   !  9
                                    x = max(1, 2, 3)      ! 10
                                    x = a(1,2)            ! 11
                                    x = i+j+k             ! 12
                                    x = j+i+k             ! 13
                                    end program test_prog
                                 ''')
    prog = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(prog)
    schedule = psy.invokes.get("test_prog").schedule

    # Compare a and a
    exp0 = schedule[0].rhs
    exp1 = schedule[1].rhs
    assert exp0.math_equal(exp1)

    # Different node types: assignment and expression
    assert not schedule[0].math_equal(exp1)

    # Compare a and b
    assert not exp1.math_equal(schedule[2].rhs)

    # Compare a+12*b... and 12*b...+a - both commutative and
    # complex expression
    assert schedule[3].rhs.math_equal(schedule[4].rhs)

    # Compare i+j and j+i - we do support _simple_ commutative changes:
    exp5 = schedule[5].rhs
    exp6 = schedule[6].rhs
    assert exp5.math_equal(exp6)

    # Compare i-j and j-i
    exp7 = schedule[7].rhs
    assert not exp7.math_equal(schedule[8].rhs)

    # Same node type, but different number of children
    # max(1, 2, 3, 4) and max(1, 2, 3)
    exp9 = schedule[9].rhs
    assert not exp9.math_equal(schedule[10].rhs)

    # Compare a and a(1,2), which triggers the recursion in Reference
    # to be false.
    assert not exp0.math_equal(schedule[11].rhs)

    # Compare i+j and max(1,2,3,4) to trigger different types
    # in the recursion in BinaryOperator
    assert not exp5.math_equal(exp9)

    # Different operator: j+i vs i-j. Do not compare
    # i+j with i-j, since this will not trigger the
    # additional tests in commutative law handling
    assert not exp6.math_equal(exp7)

    # i+j+k and j+i+k are the same:
    exp12 = schedule[12].rhs
    assert exp12.math_equal(schedule[13].rhs)


@pytest.mark.parametrize("expressions", [("i", "i"),
                                         ("2", "1+1"),
                                         ("2.0", "1.1+0.9"),
                                         ("2", "1+7*i-3-4*i-3*i+4"),
                                         ("i+j", "j+i"),
                                         ("i+j+k", "i+k+j"),
                                         ("i+i", "2*i"),
                                         ("i+j-2*k+3*j-2*i", "-i+4*j-2*k"),
                                         ("a%b", "a%b"),
                                         ("a%b(i)", "a%b(i)"),
                                         ("a%b(2*i)", "a%b(3*i-i)"),
                                         ("a%b(i-1)%c(j+1)",
                                          "a%b(-1+i)%c(1+j)"),
                                         ])
def test_math_equal_sympy(parser, expressions):
    '''Test that the sympy based comparison handles complex
    expressions that are equal.

    '''
    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    reader = FortranStringReader('''program test_prog
                                    use some_mod
                                    integer :: i, j, k, x
                                    type(my_mod_type) :: a, b
                                    x = {0}
                                    x = {1}
                                    end program test_prog
                                 '''.format(expressions[0],
                                            expressions[1]))
    prog = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(prog)
    schedule = psy.invokes.get("test_prog").schedule

    sym_maths = SymbolicMaths.get()
    assert sym_maths.equal(schedule[0].rhs, schedule[1].rhs)


@pytest.mark.parametrize("expressions", [("i", "0"),
                                         ("i", "j"),
                                         ("2", "1+1-1"),
                                         ("i+j", "j+i+1"),
                                         ("a%b", "a%c"),
                                         ("a%b(i)", "a%b(i+1)"),
                                         ("a%b(i)%c(k)", "a%b(i+1)%c(k)"),
                                         ("a%b(i)%c(k)", "a%b(i)%c(k+1)"),
                                         ("a%b(i+1)%c(k)", "a%b(i)%c(k+1)"),
                                         ])
def test_math_not_equal_sympy(parser, expressions):
    '''Test that the sympy based comparison handles complex
    expressions.

    '''
    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    reader = FortranStringReader('''program test_prog
                                    use some_mod
                                    integer :: i, j, k, x
                                    type(my_mod_type) :: a, b
                                    x = {0}
                                    x = {1}
                                    end program test_prog
                                 '''.format(expressions[0],
                                            expressions[1]))

    sym_maths = SymbolicMaths.get()

    prog = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(prog)
    schedule = psy.invokes.get("test_prog").schedule

    # Note we cannot use 'is False', since sym_maths returns an
    # instance of its own boolean type.
    assert not sym_maths.equal(schedule[0].rhs, schedule[1].rhs)
