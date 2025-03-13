# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024-2025, Science and Technology Facilities Council.
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
forward_accesses routine.'''

import pytest
from psyclone.psyir.nodes import (
    Routine,
    IfBlock,
    Reference,
    Assignment,
    Node,
    WhileLoop,
)
from psyclone.psyir.symbols import (
    DataSymbol,
    INTEGER_TYPE,
)
from psyclone.psyir.tools.definition_use_chains import DefinitionUseChain


def test_definition_use_chain_init_and_properties(fortran_reader):
    """Test the initialisation of the DefinitionUseChain class."""
    code = """subroutine test(a)
    Integer :: a
    Integer :: b

    a = a + 1
    b = a + 2
    end subroutine test"""

    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    references = psyir.walk(Reference)
    a_1 = references[0]

    # Check the basic initialisation with default setup.
    duc = DefinitionUseChain(a_1)
    assert duc._reference is a_1
    assert duc._start_point is None
    assert duc._stop_point is None
    assert duc._reference_abs_pos == a_1.abs_position
    assert len(duc._scope) == 1
    assert duc._scope[0] is routine

    # Test the control_flow_region setter
    duc = DefinitionUseChain(a_1, routine.children[0:2])
    assert len(duc._scope) == 2
    assert duc._scope[0] is routine.children[0]
    assert duc._scope[1] is routine.children[1]

    # Test the start and stop point setters
    duc = DefinitionUseChain(a_1, start_point=0, stop_point=1)
    assert duc._start_point == 0
    assert duc._stop_point == 1

    assert len(duc.uses) == 0
    assert len(duc.defsout) == 0
    assert len(duc.killed) == 0

    # Test exception when passed a non_list
    with pytest.raises(TypeError) as excinfo:
        duc = DefinitionUseChain(a_1, control_flow_region=2)
    assert ("The control_flow_region passed into a DefinitionUseChain "
            "must be a list but found 'int'." in str(excinfo.value))

    with pytest.raises(TypeError) as excinfo:
        duc = DefinitionUseChain(a_1, control_flow_region=[2])
    assert ("Each element of the control_flow_region passed into a "
            "DefinitionUseChain must be a Node but found a non-Node "
            "element. Full input is " in str(excinfo.value))

    # Check if we don't pass a routine child then we get the root
    sym = DataSymbol("sym", INTEGER_TYPE)
    r1 = Reference(sym)
    r2 = Reference(sym)
    assign = Assignment.create(r1, r2)
    duc = DefinitionUseChain(r1)
    assert duc._scope[0] is assign.lhs
    assert duc._scope[1] is assign.rhs

    # Test remaining TypeErrors
    with pytest.raises(TypeError) as excinfo:
        duc = DefinitionUseChain("123")
    assert ("The 'reference' argument passed into a DefinitionUseChain must "
            "be a Reference but found 'str'." in str(excinfo.value))
    with pytest.raises(TypeError) as excinfo:
        duc = DefinitionUseChain(r1, start_point="123")
    assert ("The start_point passed into a DefinitionUseChain must be an "
            "int but found 'str'." in str(excinfo.value))
    with pytest.raises(TypeError) as excinfo:
        duc = DefinitionUseChain(r1, stop_point="123")
    assert ("The stop_point passed into a DefinitionUseChain must be an "
            "int but found 'str'." in str(excinfo.value))


def test_definition_use_chain_is_basic_block(fortran_reader):
    """Test the is_basic_block property gives the correct result
    for various blocks."""
    code = """
subroutine foo(a, b)
real :: a, b, c, d, e, f
integer :: i

do j = 1, 10
    c = a + 1.0
    e = a**2
    f = cos(e)
    if(b > 3) then
       a = a + 1.0
       d = a * 2
    else
       d = a * 2
       do i = 1, 100
          a = a * 1.1
       end do
       a = a + 1.0
    end if
    d = cos(d)
end do
end subroutine foo"""

    psyir = fortran_reader.psyir_from_source(code)
    block1 = [psyir.walk(Routine)[0]]
    block2 = psyir.walk(IfBlock)[0].if_body.children[:]
    block3 = psyir.walk(IfBlock)[0].else_body.children[:]
    reference = psyir.walk(Reference)[0]

    # The full routine is not a basic block as it contains control flow.
    duc1 = DefinitionUseChain(reference, control_flow_region=block1)
    assert not duc1.is_basic_block

    # The if_body of the if statement is a basic block as it contains no
    # control flow.
    duc2 = DefinitionUseChain(reference, control_flow_region=block2)
    assert duc2.is_basic_block

    # The else_body of the if statement is not a basic block as it contains
    # control flow.
    duc3 = DefinitionUseChain(reference, control_flow_region=block3)
    assert not duc3.is_basic_block


def test_definition_use_chain_compute_forward_uses(fortran_reader):
    """ Test the _compute_forward_uses functionality."""

    # First test is a simple Reference with a following read.
    code = """
    subroutine x()
    integer :: a, b
    a = a + 1
    b = a
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    a_1 = psyir.walk(Reference)[0]
    # Check this is the lhs of the assignment
    assert a_1 is psyir.walk(Assignment)[0].lhs

    duc = DefinitionUseChain(
        a_1, control_flow_region=[routine]
    )
    basic_block_list = routine.children[:]
    # Need to set the start point and stop points similar to what
    # forward_accesses would do
    duc._start_point = a_1.ancestor(Assignment).walk(Node)[-1].abs_position
    duc._stop_point = 100000000
    duc._compute_forward_uses(basic_block_list)
    assert len(duc.uses) == 1
    assert duc.uses[0] is psyir.walk(Reference)[3]  # The rhs of b=a

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
    a_1 = psyir.walk(Reference)[1]

    duc = DefinitionUseChain(
        a_1, control_flow_region=[routine]
    )
    basic_block_list = routine.children[:]
    # Need to set the start point and stop points similar to what
    # forward_accesses would do
    duc._start_point = a_1.ancestor(Assignment).walk(Node)[-1].abs_position
    duc._stop_point = 100000000
    duc._compute_forward_uses(basic_block_list)
    assert len(duc.uses) == 0
    assert len(duc.defsout) == 1
    assert len(duc.killed) == 0
    assert duc.defsout[0] is psyir.walk(Reference)[2]  # The lhs of a = 2

    # Finally test a Reference with a write then another write.
    # The defsout should be the final write and the first write should be
    # killed. The use in b=a is after a write so we should ignore it.
    code = """
    subroutine x()
    integer :: a, b, c
    c = a
    a = 2
    b = a
    a = 3
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    a_1 = psyir.walk(Reference)[1]

    duc = DefinitionUseChain(
        a_1, control_flow_region=[routine]
    )
    basic_block_list = routine.children[:]
    # Need to set the start point and stop points similar to what
    # forward_accesses would do
    duc._start_point = a_1.ancestor(Assignment).walk(Node)[-1].abs_position
    duc._stop_point = 100000000
    duc._compute_forward_uses(basic_block_list)
    assert len(duc.uses) == 0
    assert len(duc.defsout) == 1
    assert len(duc.killed) == 1
    assert duc.defsout[0] is psyir.walk(Reference)[5]  # The lhs of a = 3
    assert duc.killed[0] is psyir.walk(Reference)[2]  # The lhs of a = 2


def test_definition_use_chain_find_basic_blocks(fortran_reader):
    """ Test the _find_basic_blocks functionality."""
    code = """
    subroutine x()
    use some_mod
    if (a) then
       b = c
       do i = 1, 20
         c = c + i
         if (d(i)) then
           e = c
         end if
       end do
    else if(f) then
       b = s
       do while( b < c )
         b = b * 2
       end do
       a = f
    end if
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    duc = DefinitionUseChain(
        routine.walk(Reference)[0], control_flow_region=[routine]
    )
    # Find the basic blocks.
    cfn, blocks = duc._find_basic_blocks(routine.children[:])
    ifblock = psyir.walk(IfBlock)[0]
    assert len(cfn) == 3
    assert cfn[0] is None
    assert cfn[1] is ifblock
    assert cfn[2] is ifblock
    assert len(blocks) == 3
    assert blocks[0][0] is ifblock.condition
    assert blocks[1][0] is ifblock.if_body.children[0]
    assert blocks[2][0] is ifblock.else_body.children[0]

    # Try for the basic blocks in the if_body
    cfn, blocks = duc._find_basic_blocks(ifblock.if_body.children[:])
    # Our blocks are the first assignment, the start/top/step expressions
    # and the loop body.
    assert len(cfn) == 3
    assert cfn[0] is None
    assert cfn[1] is None
    assert cfn[2] is ifblock.if_body.children[1]
    assert len(blocks) == 3
    assert len(blocks[0]) == 1
    assert blocks[0][0] is ifblock.if_body.children[0]
    assert len(blocks[1]) == 3
    assert blocks[1][0] is ifblock.if_body.children[1].start_expr
    assert blocks[1][1] is ifblock.if_body.children[1].stop_expr
    assert blocks[1][2] is ifblock.if_body.children[1].step_expr
    assert blocks[2] == ifblock.if_body.children[1].loop_body.children[:]

    # Try for the basic blocks in the else_body. Since else if we have a
    # nested if.
    cfn, blocks = duc._find_basic_blocks(ifblock.else_body.children[:])
    assert len(cfn) == 2
    assert cfn[0] is None
    assert cfn[1] is ifblock.else_body.children[0]
    assert len(blocks) == 2
    assert blocks[0][0] is ifblock.else_body.children[0].condition
    assert blocks[1] == ifblock.else_body.children[0].if_body.children[:]

    # Now look inside the else if
    ifblock2 = ifblock.else_body.children[0]
    cfn, blocks = duc._find_basic_blocks(ifblock2.if_body.children[:])
    assert len(cfn) == 4
    assert cfn[0] is None
    assert cfn[1] is None
    assert cfn[2] is ifblock2.if_body.children[1]
    assert cfn[3] is None
    assert len(blocks) == 4
    assert blocks[0][0] is ifblock2.if_body.children[0]
    assert blocks[1][0] is ifblock2.if_body.children[1].condition
    assert blocks[2] == ifblock2.if_body.children[1].loop_body.children[:]
    assert blocks[3][0] is ifblock2.if_body.children[2]

    # Check if we're passed a Schedule into _find_basic_blocks we get whats
    # expected.
    cfn, blocks = duc._find_basic_blocks([ifblock2.if_body])
    assert len(cfn) == 4
    assert cfn[0] is None
    assert cfn[1] is None
    assert cfn[2] is ifblock2.if_body.children[1]
    assert cfn[3] is None
    assert len(blocks) == 4
    assert blocks[0][0] is ifblock2.if_body.children[0]
    assert blocks[1][0] is ifblock2.if_body.children[1].condition
    assert blocks[2] == ifblock2.if_body.children[1].loop_body.children[:]
    assert blocks[3][0] is ifblock2.if_body.children[2]


def test_definition_use_chain_find_forward_accesses_basic_example(
    fortran_reader,
):
    """Functionality test for the find_forward_accesses routine. This
    tests the basic functionality of the routine."""

    code = """
subroutine foo(a, b)
real, intent(inout) :: a
real, intent(inout) :: b
real :: c, d, e, f
c = a + 1.0
e = a**2
f = cos(e)
d = c + 2.0
c = d * a
b = c + d
call bar(c, b)
b = b + c
e = a**3
a = 2
end subroutine foo
subroutine bar(x, y)
real, intent(in) :: x
real, intent(inout) :: y
!x = x + 1.0
y = exp(x**2)
end subroutine bar
"""

    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    # Creating use chain for the a in c = a + 1.0
    chains = DefinitionUseChain(
        routine.children[0].children[1].children[0], [routine]
    )
    reaches = chains.find_forward_accesses()
    # We find 3 results
    # the a in e = a**2
    # the a in c = d * a
    # The call bar(c, b) as a isn't local and we can't guarantee its behaviour.
    assert len(reaches) == 3
    assert reaches[0] is routine.children[1].rhs.children[0]
    assert reaches[1] is routine.children[4].rhs.children[1]
    assert reaches[2] is routine.children[6]

    # Create use chain for d in d = c + 2.0
    chains = DefinitionUseChain(routine.children[3].lhs, [routine])
    reaches = chains.find_forward_accesses()
    # We should find 2 results
    # c = D * a
    # b = c + D
    assert reaches[0] is routine.children[4].rhs.children[0]
    assert reaches[1] is routine.children[5].rhs.children[1]
    assert len(reaches) == 2

    # Create use chain for c in c = d * a
    chains = DefinitionUseChain(routine.children[4].lhs, [routine])
    reaches = chains.find_forward_accesses()
    # 2 results:
    # b = C + d
    # call bar(c, d)
    assert len(reaches) == 2
    assert reaches[0] is routine.children[5].rhs.children[0]
    assert reaches[1] is routine.children[6].arguments[0]


def test_definition_use_chain_find_forward_accesses_assignment(
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
    # Start chain from A = 1
    chains = DefinitionUseChain(routine.children[0].lhs)
    reaches = chains.find_forward_accesses()
    # We should find 3 results, all 3 references in
    # A = A * A
    assert len(reaches) == 3
    assert reaches[0] is routine.children[1].rhs.children[0]
    assert reaches[1] is routine.children[1].rhs.children[1]
    assert reaches[2] is routine.children[1].lhs


def test_definition_use_chain_find_forward_accesses_ifelse_example(
    fortran_reader,
):
    """Functionality test for the find_forward_accesses routine. This
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
    # Start the chain from a = 1.
    chains = DefinitionUseChain(routine.children[0].lhs)
    reaches = chains.find_forward_accesses()
    # TODO #2760 For now the if statement doesn't kill the accesses,
    # even though it will always be written to.
    assert len(reaches) == 4
    assert reaches[0] is routine.children[1].rhs.children[0]
    assert reaches[1] is routine.children[2].if_body.children[0].lhs
    assert reaches[2] is routine.children[2].else_body.children[0].lhs
    assert reaches[3] is routine.children[3].rhs.children[0]

    # Also check that a = 3 forward access is not a = 4.
    a_3 = routine.children[2].if_body.children[0].lhs
    a_4 = routine.children[2].else_body.children[0].rhs
    chains = DefinitionUseChain(a_3)
    reaches = chains.find_forward_accesses()
    assert len(reaches) == 1
    assert reaches[0] is not a_4


def test_definition_use_chain_find_forward_accesses_loop_example(
    fortran_reader,
):
    """Functionality test for the find_forward_accesses routine. This
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
    # Start the chain from b = A +2.
    chains = DefinitionUseChain(
        routine.children[1].loop_body.children[1].rhs.children[0]
    )
    reaches = chains.find_forward_accesses()
    # We should have 3 reaches
    # First two are A = A + i
    # Second is c = a + b
    assert len(reaches) == 3
    assert (
        reaches[0] is routine.children[1].loop_body.children[0].rhs.children[0]
    )
    assert reaches[1] is routine.children[1].loop_body.children[0].lhs
    assert reaches[2] is routine.children[2].rhs.children[0]

    # Check if we access a loop variable
    code = """
    subroutine x()
    integer :: a, b, c, d, e, f, i

    i = 1231
    do i = 1, 100
       a = a + i
       b = a + 2
    end do
    c = a + b
    end subroutine x"""

    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    # Start the chain from I = 1231.
    chains = DefinitionUseChain(
        routine.children[0].lhs
    )
    reaches = chains.find_forward_accesses()
    # We should have 1 reaches
    # It should be the loop
    assert len(reaches) == 1
    assert reaches[0] is routine.children[1]


def test_definition_use_chain_find_forward_accesses_while_loop_example(
    fortran_reader,
):
    """Functionality test for the find_forward_accesses routine. This
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
    reaches = chains.find_forward_accesses()

    assert len(reaches) == 3
    assert reaches[0] is routine.children[2].condition.children[0]
    assert (
        reaches[1] is routine.children[2].loop_body.children[0].rhs.children[0]
    )
    assert reaches[2] is routine.children[2].loop_body.children[0].lhs


def test_definition_use_chain_foward_accesses_nested_loop_example(
    fortran_reader,
):
    """Functionality test for the find_forward_accesses routine. This
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
    reaches = chains.find_forward_accesses()
    # Results should be A = A + 3 and the a < i condition
    assert len(reaches) == 3
    assert reaches[0] is loops[0].condition.children[0]
    assert reaches[1] is loops[0].loop_body.children[0].rhs.children[0]
    assert reaches[2] is loops[0].loop_body.children[0].lhs


def test_definition_use_chain_find_forward_accesses_structure_example(
    fortran_reader,
):
    """Functionality test for the find_forward_accesses routine. This
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
    chains = DefinitionUseChain(routine.children[0].lhs)
    reaches = chains.find_forward_accesses()
    assert len(reaches) == 1
    assert reaches[0] is routine.children[2].lhs


def test_definition_use_chain_find_forward_accesses_no_control_flow_example(
    fortran_reader,
):
    """Functionality test for the find_forward_accesses routine. This
    tests the behaviour for a simple case with no control flow with
    an assignment."""
    code = """
    subroutine x()
    integer :: a
    a = a + 2
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    chains = DefinitionUseChain(routine.children[0].rhs.children[0])
    reaches = chains.find_forward_accesses()
    assert len(reaches) == 1
    assert reaches[0] is routine.children[0].lhs


def test_definition_use_chain_find_forward_accesses_no_control_flow_example2(
    fortran_reader,
):
    """Functionality test for the find_forward_accesses routine. This
    tests the behaviour for a simple case with no control flow with
    two assignments where the first assignment should kill the dependencies.
    """
    code = """
    subroutine x()
    integer :: a
    a = a + 2
    a = 3
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    chains = DefinitionUseChain(routine.children[0].rhs.children[0])
    reaches = chains.find_forward_accesses()
    assert len(reaches) == 1
    assert reaches[0] is routine.children[0].lhs


def test_definition_use_chain_find_forward_accesses_codeblock(
    fortran_reader,
):
    """Functionality test for the find_forward_accesses routine. This
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
    chains = DefinitionUseChain(routine.children[0].lhs)
    reaches = chains.find_forward_accesses()
    assert len(reaches) == 1
    assert reaches[0] is routine.children[1]


def test_definition_use_chain_find_forward_accesses_codeblock_and_call_nlocal(
    fortran_reader,
):
    """Functionality test for the find_forward_accesses routine. This
    tests the behaviour for a simple case with a CodeBlock and a Call, and
    where the variable is not a local variable."""
    code = """
    subroutine x()
    use some_mod
    a = a + 2
    print *, a
    call b(a)
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    chains = DefinitionUseChain(routine.children[0].lhs)
    reaches = chains.find_forward_accesses()
    assert len(reaches) == 1
    assert reaches[0] is routine.children[1]


def test_definition_use_chain_find_forward_accesses_codeblock_and_call_cflow(
    fortran_reader,
):
    """Functionality test for the find_forward_accesses routine. This
    tests the behaviour for a simple case with a CodeBlock and a Call inside
    control flow, and where the variable is not a local variable."""
    code = """
    subroutine x()
    use some_mod
    a = a + 2
    if(cond) then
      print *, a
      call b(a)
    end if
    call c(a)
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    chains = DefinitionUseChain(routine.children[0].lhs)
    reaches = chains.find_forward_accesses()
    assert len(reaches) == 2
    assert reaches[0] is routine.children[1].if_body.children[0]
    assert reaches[1] is routine.children[2]


def test_definition_use_chain_find_forward_accesses_codeblock_and_call_local(
    fortran_reader,
):
    """Functionality test for the find_forward_accesses routine. This
    tests the behaviour for a simple case with a CodeBlock and a Call, and
    where the variable is a local variable."""
    code = """
    subroutine x()
    use some_mod
    integer :: a
    a = a + 2
    print *, a
    call b(a)
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    chains = DefinitionUseChain(routine.children[0].lhs)
    reaches = chains.find_forward_accesses()
    assert len(reaches) == 1
    assert reaches[0] is routine.children[1]


def test_definition_use_chain_find_forward_accesses_call_and_codeblock_nlocal(
    fortran_reader,
):
    """Functionality test for the find_forward_accesses routine. This
    tests the behaviour for a simple case with a Call then a Codeblock, and
    where the variable is not a local variable."""
    code = """
    subroutine x()
    use some_mod
    a = a + 2
    call b(a)
    print *, a
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    chains = DefinitionUseChain(routine.children[0].lhs)
    reaches = chains.find_forward_accesses()
    assert len(reaches) == 1
    assert reaches[0] is routine.children[1]


def test_definition_use_chains_goto_statement(
    fortran_reader,
):
    """Tests that we get an error when a region contains a GOTO statement."""
    code = """
    subroutine x()
    integer :: a
    a = a + 2
    GOTO 100
    100 a = a + 3
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    chains = DefinitionUseChain(routine.children[0].lhs)
    with pytest.raises(NotImplementedError) as excinfo:
        chains.find_forward_accesses()
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
    # Start the chain from A = a +i.
    chains = DefinitionUseChain(
        routine.children[1].loop_body.children[0].lhs
    )
    reaches = chains.find_forward_accesses()
    # We should have 3 reaches
    # First two are A = A + i
    # Second is c = a + b
    assert len(reaches) == 3
    assert (
        reaches[0] is routine.children[1].loop_body.children[0].rhs.children[0]
    )
    assert reaches[1] is routine.children[1].loop_body.children[0].lhs
    assert reaches[2] is routine.children[2].rhs.children[0]
    pytest.xfail(reason="Issue #2760: DefinitionUseChains should not search "
                        "again in a loop when there is a guaranteed exit "
                        "statement")


def test_definition_use_chains_cycle_statement(
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
       a = b * 4
       cycle
       b = a + 2
    end do
    c = a + b
    end subroutine x"""

    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    # Start the chain from A = a +i.
    chains = DefinitionUseChain(
        routine.children[1].loop_body.children[0].lhs
    )
    reaches = chains.find_forward_accesses()
    # We should have 4 reaches
    # First two are A = A + i
    # Then A = b * 4
    # Then c = A + b
    assert len(reaches) == 4
    assert (
        reaches[0] is routine.children[1].loop_body.children[0].rhs.children[0]
    )
    assert reaches[1] is routine.children[1].loop_body.children[0].lhs
    assert reaches[2] is routine.children[1].loop_body.children[1].lhs
    assert reaches[3] is routine.children[2].rhs.children[0]


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
    # Start the chain from A = a +i.
    chains = DefinitionUseChain(
        routine.children[1].loop_body.children[0].lhs
    )
    reaches = chains.find_forward_accesses()
    # We should have 4 reaches
    # First two are A = A + i
    # Then A = b + 4
    # Then c = A + b
    assert len(reaches) == 4
    assert (
        reaches[0] is routine.children[1].loop_body.children[0].rhs.children[0]
    )
    assert reaches[1] is routine.children[1].loop_body.children[0].lhs
    assert reaches[2] is routine.children[1].loop_body.children[1].lhs
    assert reaches[3] is routine.children[2].rhs.children[0]


def test_definition_use_chains_forward_accesses_multiple_routines(
    fortran_reader,
):
    '''Test the forward_accesses function doesn't find accesses outside of the
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
    routine = psyir.walk(Routine)[0]
    chains = DefinitionUseChain(
        routine.children[0].lhs
    )
    reaches = chains.find_forward_accesses()
    assert len(reaches) == 0


def test_definition_use_chains_forward_accesses_empty_schedules(
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
            routine.children[0].lhs
    )
    reaches = chains.find_forward_accesses()
    assert len(reaches) == 3
    assert reaches[0] is routine.children[4].rhs.children[0]
    assert reaches[1] is routine.children[4].rhs.children[1]
    assert reaches[2] is routine.children[4].lhs


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

    a = 1
    b = func(lbound(a))
    end subroutine
    """
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    chains = DefinitionUseChain(routine.children[0].lhs)
    reaches = chains.find_forward_accesses()
    assert len(reaches) == 0
