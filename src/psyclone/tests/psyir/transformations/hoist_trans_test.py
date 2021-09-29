# -----------------------------------------------------------------------------
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
# ----------------------------------------------------------------------------
# Author: R. W. Ford, STFC Daresbury Lab

'''This module tests the hoist transformation.
'''

from __future__ import absolute_import, print_function
import pytest

from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Literal, Loop, Assignment, Reference, \
    IfBlock
from psyclone.psyir.symbols import DataSymbol, REAL_TYPE, INTEGER_TYPE, \
    BOOLEAN_TYPE
from psyclone.psyir.transformations import HoistTrans, TransformationError


# init

def test_init():
    '''Test a hoist transformation can be succesfully created.'''
    hoist_trans = HoistTrans()
    assert isinstance(hoist_trans, HoistTrans)


# apply

def test_apply():
    '''Test the apply method moves the loop invariant assignment out of
    the loop and places it immediately before the loop.

    '''
    code = (
        "program test\n"
        "  integer :: i\n"
        "  real :: a\n"
        "  do i=1,1\n"
        "    a = 1.0\n"
        "  end do\n"
        "end program\n")
    reader = FortranReader()
    psyir = reader.psyir_from_source(code)
    loop = psyir.walk(Loop)[0]
    assignment = loop.loop_body[0]
    hoist_trans = HoistTrans()
    hoist_trans.apply(assignment)
    # The assignment is no longer within the loop
    assert loop.loop_body.children == []
    # The assignment is now before the loop
    previous = loop.parent.children[loop.position-1]
    assert previous is assignment


def test_apply_validate():
    '''Test the apply method calls the validate method.'''
    hoist_trans = HoistTrans()
    with pytest.raises(TransformationError) as info:
        hoist_trans.apply(None)
    assert ("The target of the HoistTrans transformation should be an "
            "assignment, but found 'NoneType'." in str(info.value))


# validate

def test_validate_node():
    '''Test the expected exception is raised if an invalid node is
    supplied to the transformation.

    '''
    hoist_trans = HoistTrans()
    with pytest.raises(TransformationError) as info:
        hoist_trans.validate(None)
    assert ("The target of the HoistTrans transformation should be an "
            "assignment, but found 'NoneType'." in str(info.value))


def test_validate_ancestor_loop():
    '''Test the expected exception is raised if the supplied assignment is
    not within a loop.

    '''
    assignment = Assignment.create(
        Reference(DataSymbol("a", REAL_TYPE)), Literal("1.0", REAL_TYPE))
    hoist_trans = HoistTrans()
    with pytest.raises(TransformationError) as info:
        hoist_trans.validate(assignment)
    assert ("The supplied assignment node 'a = 1.0\n' should be within a "
            "loop, but no loop was found." in str(info.value))


def test_validate_direct_loop():
    '''Test the expected exception is raised if the supplied assignment is
    not the direct child of a loop (ignoring schedule).

    '''
    assignment = Assignment.create(
        Reference(DataSymbol("a", REAL_TYPE)), Literal("1.0", REAL_TYPE))
    condition = Literal("true", BOOLEAN_TYPE)
    if_condition = IfBlock.create(condition, [assignment])
    one = Literal("1", INTEGER_TYPE)
    _ = Loop.create(
        DataSymbol("i", INTEGER_TYPE), one, one.copy(), one.copy(),
        [if_condition])
    hoist_trans = HoistTrans()
    with pytest.raises(TransformationError) as info:
        hoist_trans.validate(assignment)
    assert ("The supplied assignment node 'a = 1.0\n' should be directly "
            "within a loop but found 'if (.true.) then\n  a = 1.0\nend if\n'."
            in str(info.value))

# validate_dependencies

@pytest.mark.parametrize("assignment_str", ["a(i) = 1",
                                            "a(j,i,j) = 1",
                                            "a(j * 2 + 5 * i) = 1"])
def test_validate_dependencies_lhs(fortran_reader, assignment_str):
    '''Test the expected exception is raised if the supplied assignment
    depends on the loop iterator on the left-hand side.

    '''
    psyir = fortran_reader.psyir_from_source(
        '''subroutine test()
              integer :: i, j, a(10)
              do i=1, 10
                  {0}
              enddo
              end subroutine test'''.format(assignment_str))
    assignment = psyir.children[0].children[0].loop_body.children[0]
    hoist_trans = HoistTrans()
    with pytest.raises(TransformationError) as info:
        hoist_trans.validate(assignment)
    assert ("The supplied assignment node '{0}' depends directly on the "
            "parent loop iterator 'i' on the left-hand side."
            .format(assignment_str) in str(info.value))


@pytest.mark.parametrize("assignment_str", ["a = i",
                                            "a = b(j,2 * i)",
                                            "a = 1 + i * i"])
def test_validate_dependencies_rhs(fortran_reader, assignment_str):
    '''Test the expected exception is raised if the supplied assignment
    depends on the loop iterator on the right hand side.

    '''
    psyir = fortran_reader.psyir_from_source(
        '''subroutine test()
              integer :: i, j, a, b(10, 20)
              do i=1, 10
                  {0}
              enddo
              end subroutine test'''.format(assignment_str))
    assignment = psyir.children[0].children[0].loop_body.children[0]
    hoist_trans = HoistTrans()
    with pytest.raises(TransformationError) as info:
        hoist_trans.validate(assignment)
    assert ("The supplied assignment node '{0}' depends directly on the "
            "parent loop iterator 'i' on the right-hand side."
            .format(assignment_str) in str(info.value))

@pytest.mark.parametrize("assignment_str", ["a(j) = 1", "a(b(j)) = 2"])
def test_validate_dependencies_indirect_lhs(fortran_reader, assignment_str):
    '''Test the expected exception is raised if the supplied assignment
    depends on the loop iterator on the right hand side.

    '''
    psyir = fortran_reader.psyir_from_source(
        '''subroutine test()
              integer :: i, j, a(10), b(10)
              do i=1, 10
                  {0}
                  j = i+1
              enddo
              end subroutine test'''.format(assignment_str))
    assignment = psyir.children[0].children[0].loop_body.children[0]
    hoist_trans = HoistTrans()
    with pytest.raises(TransformationError) as info:
        hoist_trans.validate(assignment)
    assert ("The supplied assignment node '{0}' depends indirectly on the "
            "parent loop iterator 'i' on the left-hand side via the "
            "variable 'j'."
            .format(assignment_str) in str(info.value))

@pytest.mark.parametrize("assignment_str", ["a = j", "a = b(1,j + 1)"])
def test_validate_dependencies_indirect_rhs(fortran_reader, assignment_str):
    '''Test the expected exception is raised if the supplied assignment
    depends on the loop iterator on the right hand side.

    '''
    psyir = fortran_reader.psyir_from_source(
        '''subroutine test()
              integer :: i, j, a, b(10)
              do i=1, 10
                  {0}
                  j = i+1
              enddo
              end subroutine test'''.format(assignment_str))
    assignment = psyir.children[0].children[0].loop_body.children[0]
    hoist_trans = HoistTrans()
    with pytest.raises(TransformationError) as info:
        hoist_trans.validate(assignment)
    assert ("The supplied assignment node '{0}' depends indirectly on the "
            "parent loop iterator 'i' on the right-hand side via the "
            "variable 'j'."
            .format(assignment_str) in str(info.value))

def test_validate_dependencies_multi_write(fortran_reader):
    '''Test the expected exception is raised if the supplied assignment
    depends on the loop iterator on the right hand side.

    '''
    psyir = fortran_reader.psyir_from_source(
        '''subroutine test()
              integer :: i, j, a, b(10)
              do i=1, 10
                  a = 2
                  b(i) = a
                  a = 3
                  j = a
              enddo
              end subroutine test''')
    assignment = psyir.children[0].children[0].loop_body.children[0]
    hoist_trans = HoistTrans()
    with pytest.raises(TransformationError) as info:
        hoist_trans.validate(assignment)
    assert ("There is more than one write to the variable 'a'."
            in str(info.value))


# str

def test_str():
    '''Test the hoist transformation's str method return the
    expected results.

    '''
    hoist_trans = HoistTrans()
    assert str(hoist_trans) == "Hoist an assignment outside of its parent loop"
