# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024-2024, Science and Technology Facilities Council.
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
# Author: A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------
'''This module contains the tests for the DefinitionUseChain class's
backward_accesses routine.'''

import pytest
from psyclone.psyir.nodes import (
    Routine,
    Reference,
    Assignment,
    WhileLoop,
)
from psyclone.psyir.tools.definition_use_chains import DefinitionUseChain


def test_definition_use_chain_compute_backward_uses(fortran_reader):
    """ Test the _compute_backward_uses functionality."""

    # First test is a simple Reference with a following read.
    code = """
    subroutine x()
    integer :: a, b
    a = a + 1
    b = a
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    a_3 = psyir.walk(Reference)[3]
    # Check this is the lhs of the assignment
    assert a_3 is psyir.walk(Assignment)[1].rhs

    duc = DefinitionUseChain(
        a_3, control_flow_region=[routine]
    )
    basic_block_list = routine.children[:]
    # Need to set the start point and stop points similar to what
    # backward_accesses would do
    duc._start_point = routine.children[0].abs_position
    duc._stop_point = a_3.abs_position-1
    duc._compute_backward_uses(basic_block_list)
    assert len(duc.defsout) == 1
    assert duc.defsout[0] is psyir.walk(Reference)[0]  # The lhs of a = a + 1

    # Next we test a Reference with a write then a read - we should only get
    # the write, which should be in uses and defsout.
    code = """
    subroutine x()
    integer :: a, b, c
    c = a
    a = 2
    b = a
    end subroutine"""

    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    a_3 = psyir.walk(Reference)[4]

    duc = DefinitionUseChain(
        a_3, control_flow_region=[routine]
    )
    basic_block_list = routine.children[:]
    # Need to set the start point and stop points similar to what
    # backward_accesses would do
    duc._start_point = routine.children[0].abs_position
    duc._stop_point = a_3.abs_position - 1
    duc._compute_backward_uses(basic_block_list)
    assert len(duc.uses) == 0
    assert len(duc.defsout) == 1
    assert len(duc.killed) == 0
    assert duc.defsout[0] is psyir.walk(Reference)[2]  # The lhs of a = 2


def test_definition_use_chain_find_backward_accesses_basic_example(
    fortran_reader,
):
    """Functionality test for the find_backward_accesses routine. This
    tests the basic functionality of the routine."""

    code = """
subroutine foo(a, b)
real, intent(inout) :: a
real, intent(inout) :: b
real :: c, d, e, f
c = a + 1.0
e = a**2
f = cos(e)
c = d * a
d = c + 2.0
b = c + d
call bar(c, b)
b = b + c
e = a**3
a = 2
end subroutine foo
subroutine bar(x, y)
real, intent(in) :: x
real, intent(inout) :: y
x = x + 1.0
y = exp(x**2)
end subroutine bar
"""

    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    # Creating use chain for the a in a = 2
    chains = DefinitionUseChain(
        routine.children[9].lhs, [routine]
    )
    reaches = chains.find_backward_accesses()
    # We find 2 results
    # the a in e = a**3
    # The call bar(c, b) as a isn't local and we can't guarantee its behaviour.
    assert len(reaches) == 2
    assert reaches[0] is routine.children[8].rhs.children[0]
    assert reaches[1] is routine.children[6]

    # Create use chain for c in b = c + d
    chains = DefinitionUseChain(routine.children[5].rhs.children[0], [routine])
    reaches = chains.find_backward_accesses()
    # We should find 2 results
    # C = d * a
    # d = C + 2.0
    assert reaches[0] is routine.children[4].rhs.children[0]
    assert reaches[1] is routine.children[3].lhs
    assert len(reaches) == 2


def test_definition_use_chain_find_backward_accesses_assignment(
    fortran_reader,
):

    code = """
    subroutine x()
    integer :: a
    a = 1
    a = a * a
    end subroutine
    """
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    # Start chain from A = a * a
    chains = DefinitionUseChain(routine.children[1].lhs)
    reaches = chains.find_backward_accesses()
    # We should find 3 results, both 3 references in
    # a = A * A
    # and A = 1
    assert len(reaches) == 3
    assert reaches[0] is routine.children[1].rhs.children[1]
    assert reaches[1] is routine.children[1].rhs.children[0]
    assert reaches[2] is routine.children[0].lhs


def test_definition_use_chain_find_backward_accesses_ifelse_example(
    fortran_reader,
):
    """Functionality test for the find_backward_accesses routine. This
    tests the behaviour when there is an if/else block."""

    code = """
    subroutine x()
    integer :: a, b, c, d, e, f
    a = 1
    b = a + c
    if ( d > e) then
        a = 3
    else
        a = 4
    end if
    b = a + d
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    # Start the chain from b = A + d.
    chains = DefinitionUseChain(routine.children[3].rhs.children[0])
    reaches = chains.find_backward_accesses()
    # TODO #2760 For now the if statement doesn't kill the accesses,
    # even though it will always be written to.
    assert len(reaches) == 4
    assert reaches[2] is routine.children[1].rhs.children[0]
    assert reaches[1] is routine.children[2].if_body.children[0].lhs
    assert reaches[0] is routine.children[2].else_body.children[0].lhs
    assert reaches[3] is routine.children[0].lhs

    # Also check that a = 4 backward access is not a = 3.
    a_3 = routine.children[2].if_body.children[0].lhs
    a_4 = routine.children[2].else_body.children[0].lhs
    chains = DefinitionUseChain(a_4)
    reaches = chains.find_backward_accesses()
    assert len(reaches) == 2
    assert reaches[0] is not a_3
    assert reaches[1] is not a_3


def test_definition_use_chain_find_backward_accesses_loop_example(
    fortran_reader,
):
    """Functionality test for the find_backward_accesses routine. This
    tests the behaviour when there is a loop."""
    code = """
    subroutine x()
    integer :: a, b, c, d, e, f, i

    a = 1
    do i = 1, 100
       a = a + i
       b = a + 2
    end do
    c = a + b
    end subroutine x"""

    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    # Start the chain from A = a + i.
    chains = DefinitionUseChain(
        routine.children[1].loop_body.children[0].lhs
    )
    reaches = chains.find_backward_accesses()
    # We should have 4? reaches
    # First  b = A + 2
    # Second a = A + i
    # Third (?) is A = a + i
    # Last is A = 1
    assert len(reaches) == 4
    assert (
        reaches[0] is routine.children[1].loop_body.children[1].rhs.children[0]
    )
    assert (
        reaches[2] is routine.children[1].loop_body.children[0].rhs.children[0]
    )
    assert (
        reaches[1] is routine.children[1].loop_body.children[0].lhs
    )
    assert reaches[3] is routine.children[0].lhs

    # Check if we access a loop variable
    code = """
    subroutine x()
    integer :: a, b, c, d, e, f, i

    do i = 1, 100
       a = a + i
       b = a + 2
    end do
    i = 1231
    end subroutine x"""

    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    # Start the chain from I = 1231.
    chains = DefinitionUseChain(
        routine.children[1].lhs
    )
    reaches = chains.find_backward_accesses()
    # We should have 1 reaches
    # It should be the loop
    assert len(reaches) == 1
    assert reaches[0] is routine.children[0]


def test_definition_use_chain_find_backward_accesses_while_loop_example(
    fortran_reader,
):
    """Functionality test for the find_backward_accesses routine. This
    tests the behaviour when there is a while loop."""
    code = """
    subroutine x()
    integer :: a, b, c, d, e, f, i
    i = 100
    a = 1
    do while (a < i)
        a = a + 3
    end do
    end subroutine"""

    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    # Start the chain from A = a + 3.
    chains = DefinitionUseChain(routine.children[2].loop_body.children[0].lhs)
    reaches = chains.find_backward_accesses()

    assert len(reaches) == 4
    assert reaches[0] is routine.children[2].condition.children[0]
    assert (
        reaches[2] is routine.children[2].loop_body.children[0].rhs.children[0]
    )
    assert reaches[1] is routine.children[2].loop_body.children[0].lhs
    assert reaches[3] is routine.children[1].lhs


def test_definition_use_chain_backward_accesses_nested_loop_example(
    fortran_reader,
):
    """Functionality test for the find_backward_accesses routine. This
    tests the behaviour when there is a nested loop."""
    code = """
    subroutine x()
    integer :: a, b, c, d, e, f, i
    i = 100
    a = 1
    do while(a < i)
       a = a + 3
       do while(b < 5 * i)
          b = b + a
       end do
    end do
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    # Start the chain from b = b + A.
    loops = routine.walk(WhileLoop)
    chains = DefinitionUseChain(loops[1].loop_body.children[0].rhs.children[1])
    reaches = chains.find_backward_accesses()
    # TODO #2760 The backwards accesses should not continue past a = a + 3 as
    # to reach the b = b + a statement we must have passed through the
    # a = a + 3 statement.
    # Results should be b = b + A, A = A + 3 and the a < i condition
    # then the A = 1
    assert len(reaches) == 4
    assert reaches[0] is loops[0].condition.children[0]
    assert reaches[1] is loops[1].loop_body.children[0].rhs.children[1]
    assert reaches[2] is loops[0].loop_body.children[0].lhs
    assert reaches[3] is routine.children[1].lhs


def test_definition_use_chain_find_backward_accesses_structure_example(
    fortran_reader,
):
    """Functionality test for the find_backward_accesses routine. This
    tests the behaviour when a structureReference is provided."""
    code = """
    subroutine x()
    use some_mod
    a%b = 1
    a%c = 2
    a%b = 3
    end subroutine"""

    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    chains = DefinitionUseChain(routine.children[2].lhs)
    reaches = chains.find_backward_accesses()
    assert len(reaches) == 1
    assert reaches[0] is routine.children[0].lhs


def test_definition_use_chain_find_backward_accesses_no_control_flow_example(
    fortran_reader,
):
    """Functionality test for the find_backward_accesses routine. This
    tests the behaviour for a simple case with no control flow with
    an assignment."""
    code = """
    subroutine x()
    integer :: a
    a = a + 2
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    chains = DefinitionUseChain(routine.children[0].lhs)
    reaches = chains.find_backward_accesses()
    assert len(reaches) == 1
    assert reaches[0] is routine.children[0].rhs.children[0]


def test_definition_use_chain_find_backward_accesses_codeblock(
    fortran_reader,
):
    """Functionality test for the find_backward_accesses routine. This
    tests the behaviour for a simple case with a CodeBlock."""
    code = """
    subroutine x()
    integer :: a
    a = a + 2
    print *, a
    a = 3
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    chains = DefinitionUseChain(routine.children[2].lhs)
    reaches = chains.find_backward_accesses()
    assert len(reaches) == 1
    assert reaches[0] is routine.children[1]


def test_definition_use_chain_find_backward_accesses_codeblock_and_call_nlocal(
    fortran_reader,
):
    """Functionality test for the find_backward_accesses routine. This
    tests the behaviour for a simple case with a CodeBlock and a Call, and
    where the variable is not a local variable."""
    code = """
    subroutine x()
    use some_mod
    print *, a
    call b(a)
    a = a + 2
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    chains = DefinitionUseChain(routine.children[2].rhs.children[0])
    reaches = chains.find_backward_accesses()
    assert len(reaches) == 1
    assert reaches[0] is routine.children[1].children[1]


def test_definition_use_chain_find_backward_accesses_codeblock_and_call_cflow(
    fortran_reader,
):
    """Functionality test for the find_backward_accesses routine. This
    tests the behaviour for a simple case with a CodeBlock and a Call inside
    control flow, and where the variable is not a local variable."""
    code = """
    subroutine x()
    use some_mod
    call c()
    if(cond) then
      print *, a
      call b(a)
    end if
    a = a + 2
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    chains = DefinitionUseChain(routine.children[2].rhs.children[0])
    reaches = chains.find_backward_accesses()
    assert len(reaches) == 2
    assert reaches[0] is routine.children[1].if_body.children[1].children[1]
    assert reaches[1] is routine.children[0]


def test_definition_use_chain_find_backward_accesses_codeblock_and_call_local(
    fortran_reader,
):
    """Functionality test for the find_backward_accesses routine. This
    tests the behaviour for a simple case with a CodeBlock and a Call, and
    where the variable is a local variable."""
    code = """
    subroutine x()
    use some_mod
    integer :: a
    call b(a)
    print *, a
    a = a + 2
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    chains = DefinitionUseChain(routine.children[2].rhs.children[0])
    reaches = chains.find_backward_accesses()
    assert len(reaches) == 1
    assert reaches[0] is routine.children[1]


def test_definition_use_chain_find_backward_accesses_call_and_codeblock_nlocal(
    fortran_reader,
):
    """Functionality test for the find_backward_accesses routine. This
    tests the behaviour for a simple case with a Call then a Codeblock, and
    where the variable is not a local variable."""
    code = """
    subroutine x()
    use some_mod
    print *, a
    call b()
    a = 2
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    chains = DefinitionUseChain(routine.children[2].lhs)
    reaches = chains.find_backward_accesses()
    assert len(reaches) == 1
    assert reaches[0] is routine.children[1]


def test_definition_use_chains_goto_statement(
    fortran_reader,
):
    """Tests that we get an error when a region contains a GOTO statement."""
    code = """
    subroutine x()
    integer :: a
    GOTO 100
    100 a = a + 3
    a = 2
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    chains = DefinitionUseChain(routine.children[1].lhs)
    with pytest.raises(NotImplementedError) as excinfo:
        chains.find_backward_accesses()
    assert ("DefinitionUseChains can't handle code containing GOTO statements"
            in str(excinfo.value))


def test_definition_use_chains_exit_statement(
    fortran_reader,
):
    """Check that DefinitionUseChains ignore statements after an exit statement
    in a loop."""
    code = """
    subroutine x()
    integer :: a, b, c, d, e, f, i

    a = 1
    do i = 1, 100
       a = a + i
       exit
       b = a + 2
    end do
    c = a + b
    end subroutine x"""

    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    # Start the chain from a = A +i.
    chains = DefinitionUseChain(
        routine.children[1].loop_body.children[0].rhs.children[0]
    )
    reaches = chains.find_backward_accesses()
    # We should have 2 reaches
    # First is A = a + i
    # Second is A = 1
    assert len(reaches) == 2
    assert reaches[0] is routine.children[1].loop_body.children[0].lhs
    assert reaches[1] is routine.children[0].lhs
    pytest.xfail(reason="Issue #2760: DefinitionUseChains should not search "
                        "again in a loop when there is a guaranteed exit "
                        "statement")


def test_definition_use_chains_cycle_statement(
    fortran_reader,
):
    """Check that DefinitionUseChains ignore statements after a return
    statement in a loop."""
    code = """
    subroutine x()
    integer :: a, b, c, d, e, f, i

    a = 1
    do i = 1, 100
       a = a + i
       a = b * 4
       cycle
       b = a + 2
    end do
    c = a + b
    end subroutine x"""

    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    # Start the chain from a = A +i.
    chains = DefinitionUseChain(
        routine.children[1].loop_body.children[0].rhs.children[0]
    )
    reaches = chains.find_backward_accesses()
    # We should have 2 reaches
    # A = b * 4
    # A = 1
    assert len(reaches) == 2
    assert reaches[0] is routine.children[1].loop_body.children[1].lhs
    assert reaches[1] is routine.children[0].lhs


def test_definition_use_chains_return_statement(
    fortran_reader,
):
    """Check that DefinitionUseChains ignore statements after a cycle statement
    in a loop."""
    code = """
    subroutine x()
    integer :: a, b, c, d, e, f, i

    a = 1
    do i = 1, 100
       a = a + i
       a = b + 4
       return
       b = a + 2
    end do
    c = a + b
    end subroutine x"""

    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    # Start the chain from a = A +i.
    chains = DefinitionUseChain(
        routine.children[1].loop_body.children[0].rhs.children[0]
    )
    reaches = chains.find_backward_accesses()
    # We should have 2 reaches
    # A = b * 4
    # A = 1
    assert len(reaches) == 2
    assert reaches[1] is routine.children[0].lhs
    # Search backwards in loop isn't working correctly - we're finding
    # the LHS of a = a + i instead of A = b + 4
    assert reaches[0] is routine.children[1].loop_body.children[1].lhs


def test_definition_use_chains_backward_accesses_multiple_routines(
    fortran_reader,
):
    '''Test the backward_accesses function doesn't find accesses outside of the
    containing subroutine.'''
    code = """
module my_mod
    integer :: a, b
    contains
    subroutine test()
         a = 1
    end subroutine
    subroutine test2()
        b = a
    end subroutine
end module
"""

    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[1]
    chains = DefinitionUseChain(
        routine.children[0].rhs
    )
    reaches = chains.find_backward_accesses()
    assert len(reaches) == 0


def test_definition_use_chains_backward_accesses_nonassign_reference_in_loop(
    fortran_reader,
):
    '''Coverage completion to handle the case where the passed reference is
    not part of an assignment and within a loop.'''
    code = """
    subroutine x()
    integer :: a, b, c, d, e, f, i

    a = 1
    do i = 1, 100
       a = a + i
       call p(a)
       return
       b = a + 2
    end do
    c = a + b
    end subroutine x"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    chains = DefinitionUseChain(
        routine.children[1].loop_body.children[1].children[1]
    )
    reaches = chains.find_backward_accesses()
    # TODO #2760 The backwards accesses should not continue past a = a + i
    # when searching backwards in the loop, or to a = 1
    assert len(reaches) == 3
    assert reaches[0] is routine.children[1].loop_body.children[1].children[1]
    assert reaches[1] is routine.children[1].loop_body.children[0].lhs
    assert reaches[2] is routine.children[0].lhs


def test_definition_use_chains_backward_accesses_empty_schedules(
    fortran_reader,
):
    '''Test the case where we have empty schedules inside
    various type of code.'''
    code = """
    subroutine x()
    integer :: a, i
    a = 1
    do i = 1, 100
    end do
    if(.TRUE.) then
    else
    endif
    do while(.FALSE.)
    end do
    a = a + a
    end subroutine x
    """
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    chains = DefinitionUseChain(
            routine.children[4].lhs
    )
    reaches = chains.find_backward_accesses()
    assert len(reaches) == 3
    assert reaches[0] is routine.children[4].rhs.children[1]
    assert reaches[1] is routine.children[4].rhs.children[0]
    assert reaches[2] is routine.children[0].lhs


def test_definition_use_chains_backward_accesses_inquiry_func(
    fortran_reader,
):
    '''Test the case where we have an inquiry function
    accessing the symbol of interest.'''
    code = """
    subroutine x()
    use some_mod, only: func
    integer, dimension(100) :: a
    integer :: b

    b = func(lbound(a))
    a = 1
    end subroutine
    """
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    chains = DefinitionUseChain(
            routine.children[1].lhs
    )
    reaches = chains.find_backward_accesses()
    assert len(reaches) == 0
