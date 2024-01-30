# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and N. Nobre, STFC Daresbury Lab
#          J. Henrichs, Bureau of Meteorology

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
    '''Test a hoist transformation can be successfully created.'''
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

# _validate_dependencies


@pytest.mark.parametrize("assignment_str", ["a = a + 1",
                                            "a = b(a)+1",
                                            "a = sin(a)"])
def test_validate_error_read_and_write(fortran_reader, assignment_str):
    '''Test the expected exception is raised if the supplied assignment
    depends on the loop iteration.

    '''
    psyir = fortran_reader.psyir_from_source(
        f'''subroutine test()
              integer :: i, j, a, b(10)
              do i=1, 10
                  {assignment_str}
              enddo
            end subroutine test''')
    assignment = psyir.children[0][0].loop_body[0]
    hoist_trans = HoistTrans()
    with pytest.raises(TransformationError) as info:
        hoist_trans.validate(assignment)
    assert ("The statement can't be hoisted as it contains a variable ('a') "
            "that is both read and written." in str(info.value))


@pytest.mark.parametrize("assignment_str", ["a = 1",
                                            "a = b(j)+1",
                                            "a = sin(j)"])
def test_validate_read_and_write(fortran_reader, assignment_str):
    '''Tests valid assignments that can be hoisted.

    '''
    psyir = fortran_reader.psyir_from_source(
        f'''subroutine test()
              integer :: i, j, a, b(10)
              do i=1, 10
                  {assignment_str}
              enddo
            end subroutine test''')
    assignment = psyir.children[0][0].loop_body[0]
    hoist_trans = HoistTrans()
    hoist_trans.validate(assignment)


@pytest.mark.parametrize("assignment_str", ["a(i) = 1",
                                            "a(j,i,j) = 1",
                                            "a(j * 2 + 5 * i) = 1",
                                            "a(b(i)) = 2",
                                            "a = i",
                                            "a = b(j,2 * i)",
                                            "a = 1 + i * i"])
def test_validate_direct_dependency_errors(fortran_reader, assignment_str):
    '''Test the expected exception is raised if the supplied assignment
    depends directly on the loop variable.

    '''
    psyir = fortran_reader.psyir_from_source(
        f'''subroutine test()
              integer :: i, j, a(10), b(10)
              do i=1, 10
                  {assignment_str}
              enddo
            end subroutine test''')
    assignment = psyir.children[0][0].loop_body[0]
    hoist_trans = HoistTrans()
    with pytest.raises(TransformationError) as info:
        hoist_trans.validate(assignment)
    assert (f"The statement '{assignment_str}' can't be hoisted as it reads "
            f"variable 'i' which is written somewhere else in the loop."
            in str(info.value))


@pytest.mark.parametrize("statement_var", [("a(j) = 1", "j"),
                                           ("a(b(j)) = 2", "j"),
                                           ("a = j", "j"),
                                           ("a = b(1,j + 1)", "j"),
                                           ("a(k) = 1", "k"),
                                           ("a(b(k)) = 2", "k"),
                                           ("a = k", "k"),
                                           ("a = b(1,k + 1)", "k")])
def test_validate_indirect_dependency_errors(fortran_reader, statement_var):
    '''Test the expected exception is raised if the statement to be hoisted
    indirectly depends on the loop iteration because it reads a variable that
    is written (either before or after the statement to be hoisted).
    The 'statement_var' parameter contains a 2-tuple: first the statement
    to be hoisted, then the variable on which it depends that causes the
    dependency.

    '''
    psyir = fortran_reader.psyir_from_source(
        f'''subroutine test()
              integer :: i, j, k,  a(10), b(10)
              do i=1, 10
                  j = i+1
                  {statement_var[0]}
                  k = j + 1
              enddo
            end subroutine test''')
    assignment = psyir.children[0][0].loop_body[1]
    hoist_trans = HoistTrans()
    with pytest.raises(TransformationError) as info:
        hoist_trans.validate(assignment)
    assert (f"The statement '{statement_var[0]}' can't be hoisted as it reads "
            f"variable '{statement_var[1]}' which is written somewhere else "
            f"in the loop." in str(info.value))


def test_validate_dependencies_multi_write(fortran_reader):
    '''Test the expected exception is raised if the variable is written
    more than once.

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
    assignment = psyir.children[0][0].loop_body[0]
    hoist_trans = HoistTrans()
    with pytest.raises(TransformationError) as info:
        hoist_trans.validate(assignment)
    assert ("There is at least one additional write to the variable 'a' in "
            "the loop, outside the supplied statement." in str(info.value))


@pytest.mark.parametrize("assignment_str", ["a = 2", "b(i) = a"])
def test_validate_dependencies_read_or_write_before(assignment_str,
                                                    fortran_reader):
    '''Test that the expected exception is raised if variable is read or
    written before the assignment that is to be hoisted.

    '''
    psyir = fortran_reader.psyir_from_source(
        f'''subroutine test()
              integer :: i, j, a, b(10)
              do i=1, 10
                  {assignment_str}    ! read or write 'a' here
                  a = 3
                  j = a
              enddo
            end subroutine test''')
    assignment = psyir.children[0][0].loop_body[1]
    # Make sure we are trying to hoist the right assignment:
    assert isinstance(assignment.rhs, Literal)
    assert assignment.rhs.value == "3"

    hoist_trans = HoistTrans()
    with pytest.raises(TransformationError) as info:
        hoist_trans.validate(assignment)
    assert ("The statement 'a = 3' can't be hoisted as variable 'a' is "
            "accessed earlier within the loop." in str(info.value))


def test_validate_dependencies_if_statement(fortran_reader):
    '''Test if various if-statements pass the dependency validation to
    be allowed to be hoisted. While this is not currently supported
    in the hoist transformation, this will be added in TODO #1445.

    '''
    # A simple constant test:
    # -----------------------
    psyir = fortran_reader.psyir_from_source(
        '''subroutine test(j)
              integer :: i, j, a, b(10)
              do i=1, 10
                  if (j == 1) then
                     a = 3
                  else
                     a = 4
                  endif
                  b(i) = a
              enddo
            end subroutine test''')
    loop = psyir.children[0][0]
    ifblock = loop.loop_body[0]
    # Make sure we are trying to hoist the right statement:
    assert isinstance(ifblock, IfBlock)
    hoist_trans = HoistTrans()
    hoist_trans._validate_dependencies(ifblock, loop)

    # Test if there is more than one variable written:
    # ------------------------------------------------
    psyir = fortran_reader.psyir_from_source(
        '''subroutine test(j, a, b)
              integer :: i, j, a, b, c(10)
              do i=1, 10
                  if (j == 1) then
                     a = 3
                  else
                     b = 4
                  endif
                  c(i) = a+b
              enddo
            end subroutine test''')
    loop = psyir.children[0][0]
    ifblock = loop.loop_body[0]
    # Make sure we are trying to hoist the right statement:
    assert isinstance(ifblock, IfBlock)
    hoist_trans._validate_dependencies(ifblock, loop)

    # Now one part of the if statement contains a read-write:
    psyir = fortran_reader.psyir_from_source(
        '''subroutine test(j, a, b)
              integer :: i, j, a, b, c(10)
              do i=1, 10
                  if (j == 1) then
                     a = 3
                  else
                     a = a + 1
                  endif
                  c(i) = a+b
              enddo
            end subroutine test''')
    loop = psyir.children[0][0]
    ifblock = loop.loop_body[0]
    # Make sure we are trying to hoist the right statement:
    assert isinstance(ifblock, IfBlock)
    with pytest.raises(TransformationError) as err:
        hoist_trans._validate_dependencies(ifblock, loop)
    assert ("The statement can't be hoisted as it contains a variable ('a') "
            "that is both read and written." in str(err.value))

    # The second written variable is written again in the loop:
    psyir = fortran_reader.psyir_from_source(
        '''subroutine test(j, a, b)
              integer :: i, j, a, b, c(10)
              do i=1, 10
                  if (j == 1) then
                     a = 3
                  else
                     b = 4
                  endif
                  c(i) = a+b
                  b = 2
              enddo
            end subroutine test''')
    loop = psyir.children[0][0]
    ifblock = loop.loop_body[0]
    # Make sure we are trying to hoist the right statement:
    assert isinstance(ifblock, IfBlock)
    with pytest.raises(TransformationError) as err:
        hoist_trans._validate_dependencies(ifblock, loop)
    assert ("There is at least one additional write to the variable 'b' in "
            "the loop, outside the supplied statement." in str(err.value))


# str

def test_str():
    '''Test the hoist transformation's str method return the
    expected results.

    '''
    hoist_trans = HoistTrans()
    assert str(hoist_trans) == "Hoist an assignment outside of its parent loop"
