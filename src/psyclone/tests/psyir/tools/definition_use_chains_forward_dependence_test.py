# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024-2026, Science and Technology Facilities Council.
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
from psyclone.errors import InternalError
from psyclone.psyir.nodes import (
    ArrayReference,
    Assignment,
    Call,
    CodeBlock,
    IfBlock,
    Loop,
    Reference,
    Routine,
    Node,
    OMPParallelDirective,
    WhileLoop,
)
from psyclone.psyir.transformations import OMPParallelTrans
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
    duc = DefinitionUseChain([a_1])
    assert len(duc._references) == 1
    assert duc._references[0] is a_1
    sig = a_1.get_signature_and_indices()[0]
    assert duc._reference_signatures[0] == sig
    assert duc._start_point is None
    assert duc._stop_point is None
    assert duc._references_abs_pos[sig] == a_1.abs_position
    assert len(duc._scope) == 1
    assert duc._scope[0] is routine

    # Test the control_flow_region setter
    duc = DefinitionUseChain([a_1], routine.children[0:2])
    assert len(duc._scope) == 2
    assert duc._scope[0] is routine.children[0]
    assert duc._scope[1] is routine.children[1]

    # Test the start and stop point setters
    duc = DefinitionUseChain([a_1], start_point=0, stop_point=1)
    assert duc._start_point == 0
    assert duc._stop_point == 1

    assert len(duc.uses) == 1
    assert len(duc.uses[sig]) == 0
    assert len(duc.defsout) == 1
    assert len(duc.defsout[sig]) == 0
    assert len(duc.killed) == 1
    assert len(duc.killed[sig]) == 0

    # Test exceptions when passed a non_list for various inputs.
    with pytest.raises(TypeError) as excinfo:
        duc = DefinitionUseChain(a_1)
    assert ("The 'references' argument passed into a DefinitionUseChain "
            "must be a list of References but found 'Reference'"
            in str(excinfo.value))

    with pytest.raises(TypeError) as excinfo:
        duc = DefinitionUseChain([a_1], control_flow_region=2)
    assert ("The control_flow_region passed into a DefinitionUseChain "
            "must be a list but found 'int'." in str(excinfo.value))

    with pytest.raises(TypeError) as excinfo:
        duc = DefinitionUseChain([a_1], control_flow_region=[2])
    assert ("Each element of the control_flow_region passed into a "
            "DefinitionUseChain must be a Node but found a non-Node "
            "element. Full input is " in str(excinfo.value))

    # Check if we don't pass a routine child then we get the root
    sym = DataSymbol("sym", INTEGER_TYPE)
    r1 = Reference(sym)
    r2 = Reference(sym)
    assign = Assignment.create(r1, r2)
    duc = DefinitionUseChain([r1])
    assert duc._scope[0] is assign.lhs
    assert duc._scope[1] is assign.rhs

    # Test remaining TypeErrors
    with pytest.raises(TypeError) as excinfo:
        duc = DefinitionUseChain([r1], start_point="123")
    assert ("The start_point passed into a DefinitionUseChain must be an "
            "int but found 'str'." in str(excinfo.value))
    with pytest.raises(TypeError) as excinfo:
        duc = DefinitionUseChain([r1], stop_point="123")
    assert ("The stop_point passed into a DefinitionUseChain must be an "
            "int but found 'str'." in str(excinfo.value))
    with pytest.raises(TypeError) as excinfo:
        duc = DefinitionUseChain(["a", "b"])
    assert ("The 'references' argument passed into a DefinitionUseChain must "
            "be a list of References but found 'str' in the list."
            in str(excinfo.value))

    # Create a containing schedule.
    code = """subroutine test
        integer :: r1
        r1 = 1
    end subroutine
    """
    psyir = fortran_reader.psyir_from_source(code)
    r1 = psyir.walk(Assignment)[0].lhs
    with pytest.raises(InternalError) as excinfo:
        duc = DefinitionUseChain([r1, Reference(sym)])
    assert ("All references provided into a DefinitionUseChain "
            "must have the same parent in the ancestor Schedule."
            in str(excinfo.value))


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
    duc1 = DefinitionUseChain([reference], control_flow_region=block1)
    assert not duc1.is_basic_block

    # The if_body of the if statement is a basic block as it contains no
    # control flow.
    duc2 = DefinitionUseChain([reference], control_flow_region=block2)
    assert duc2.is_basic_block

    # The else_body of the if statement is not a basic block as it contains
    # control flow.
    duc3 = DefinitionUseChain([reference], control_flow_region=block3)
    assert not duc3.is_basic_block

    # Test that regiondirectives (e.g. OMPParallelDirective) don't count.
    code = """subroutine x
    integer :: i
    integer, dimension(100) :: arr

    do i = 1, 100
        arr(i) = i
    end do
    end subroutine
    """
    par_trans = OMPParallelTrans()
    psyir = fortran_reader.psyir_from_source(code)
    par_trans.apply(psyir.walk(Routine)[0].children[:])
    reference = psyir.walk(Reference)[0]
    parallel = psyir.walk(OMPParallelDirective)[0]
    duc = DefinitionUseChain([reference], control_flow_region=[parallel])
    assert not duc.is_basic_block


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
        [a_1], control_flow_region=[routine]
    )
    sig = a_1.get_signature_and_indices()[0]
    basic_block_list = routine.children[:]
    # Need to set the start point and stop points similar to what
    # forward_accesses would do
    duc._start_point = a_1.ancestor(Assignment).walk(Node)[-1].abs_position
    duc._stop_point = 100000000
    duc._compute_forward_uses(basic_block_list)
    assert len(duc.uses[sig]) == 1
    assert duc.uses[sig][0] is psyir.walk(Reference)[3]  # The rhs of b=a

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
        [a_1], control_flow_region=[routine]
    )
    sig = a_1.get_signature_and_indices()[0]
    basic_block_list = routine.children[:]
    # Need to set the start point and stop points similar to what
    # forward_accesses would do
    duc._start_point = a_1.ancestor(Assignment).walk(Node)[-1].abs_position
    duc._stop_point = 100000000
    duc._compute_forward_uses(basic_block_list)
    assert len(duc.uses[sig]) == 0
    assert len(duc.defsout[sig]) == 1
    assert len(duc.killed[sig]) == 0
    assert duc.defsout[sig][0] is psyir.walk(Reference)[2]  # The lhs of a = 2

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
        [a_1], control_flow_region=[routine]
    )
    sig = a_1.get_signature_and_indices()[0]
    basic_block_list = routine.children[:]
    # Need to set the start point and stop points similar to what
    # forward_accesses would do
    duc._start_point = a_1.ancestor(Assignment).walk(Node)[-1].abs_position
    duc._stop_point = 100000000
    duc._compute_forward_uses(basic_block_list)
    assert len(duc.uses[sig]) == 0
    assert len(duc.defsout[sig]) == 1
    assert len(duc.killed[sig]) == 1
    assert duc.defsout[sig][0] is psyir.walk(Reference)[5]  # The lhs of a = 3
    assert duc.killed[sig][0] is psyir.walk(Reference)[2]  # The lhs of a = 2


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
        [routine.walk(Reference)[0]], control_flow_region=[routine]
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

    # Find the basic blocks for a RegionDirective
    par_trans = OMPParallelTrans()
    par_trans.apply(psyir.walk(Routine)[0].children[:])
    cfn, blocks = duc._find_basic_blocks([routine.children[0]])
    assert len(cfn) == 1
    assert cfn[0] is None
    assert len(blocks) == 1
    assert len(blocks[0]) == 1
    assert blocks[0][0] is routine.children[0].children[0]

    # Add another assignment before the RegionDirective
    assign_copy = psyir.walk(Assignment)[0].copy()
    psyir.walk(Routine)[0].children.insert(0, assign_copy)
    cfn, blocks = duc._find_basic_blocks(routine.children[:])
    assert len(cfn) == 2
    assert cfn[0] is None
    assert len(blocks) == 2
    assert len(blocks[0]) == 1
    assert blocks[0][0] is routine.walk(Assignment)[0]
    assert len(blocks[1]) == 1
    assert blocks[1][0] is routine.walk(OMPParallelDirective)[0].dir_body


def test_definition_use_chain_find_basic_blocks_inside_loops(fortran_reader):
    """ Test the _find_basic_blocks functionality for blocks inside an
    outer-level loop"""
    code = """
    subroutine my_sub()
      integer :: ji
      integer :: jj
      integer :: i
      real, dimension(10,10) :: var1
      real, dimension(10) :: ztmp
      real, dimension(10) :: ztmp2

      do i = 1, 2, 1
        var1 = 1.0
        ztmp2 = 1.0
        if (i == 1) then
          do ji = 1, 10, 1
            do jj = 1, 10, 1
              ztmp(jj) = 3
            enddo
            do jj = 1, 10, 1
              var1(ji,jj) = ztmp(jj) * 2
            enddo
          enddo
        else
          do ji = 1, 10, 1
            do jj = 1, 10, 1
              var1(ji,jj) = ztmp(jj) * 2
            enddo
          enddo
        end if
      enddo

    end subroutine my_sub
    """
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    # Get the DUC for the first ArrayRef, ztmp
    aref = psyir.walk(ArrayReference)[0]
    assert aref.symbol.name == "ztmp"
    duc = DefinitionUseChain(
        [aref], control_flow_region=[routine]
    )
    cfn, blocks = duc._find_basic_blocks(routine.walk(Loop)[0].children[:])
    # The ifblock has to be in cfn twice, once with the contents of the if
    # and once with the contents of the else, as it may loop back to them
    ifblock = routine.walk(IfBlock)[0]
    assert cfn.count(ifblock) == 2
    assert blocks.count(ifblock.if_body.children[:]) == 1
    assert blocks.count(ifblock.else_body.children[:]) == 1


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
    ref = routine.children[0].children[1].children[0]
    sig = ref.get_signature_and_indices()[0]
    chains = DefinitionUseChain(
        [ref], [routine]
    )
    reaches = chains.find_forward_accesses()
    # We find 3 results
    # the a in e = a**2 (assignment 2)
    # the a in c = d * a (assignment 4)
    # The call bar(c, b) as a isn't local and we can't guarantee its behaviour.
    assert len(reaches[sig]) == 3
    assert reaches[sig][0] is routine.walk(Assignment)[1].rhs.children[0]
    assert reaches[sig][1] is routine.walk(Assignment)[4].rhs.children[1]
    assert reaches[sig][2] is routine.walk(Call)[1]

    # Create use chain for d in d = c + 2.0
    ref = routine.children[3].lhs
    sig = ref.get_signature_and_indices()[0]
    chains = DefinitionUseChain([ref], [routine])
    reaches = chains.find_forward_accesses()
    # We should find 2 results
    # c = D * a (Assignment 5)
    # b = c + D (Assignment 6)
    assert reaches[sig][0] is routine.walk(Assignment)[4].rhs.children[0]
    assert reaches[sig][1] is routine.walk(Assignment)[5].rhs.children[1]
    assert len(reaches[sig]) == 2

    # Create use chain for c in c = d * a (Assignment 5)
    ref = routine.walk(Assignment)[4].lhs
    sig = ref.get_signature_and_indices()[0]
    chains = DefinitionUseChain([ref], [routine])
    reaches = chains.find_forward_accesses()
    # 2 results:
    # b = C + d (Assignment 6)
    # call bar(c, d) (The second Call)
    assert len(reaches[sig]) == 2
    assert reaches[sig][0] is routine.walk(Assignment)[5].rhs.children[0]
    assert reaches[sig][1] is routine.walk(Call)[1].arguments[0]


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
    ref = routine.children[0].lhs
    sig = ref.get_signature_and_indices()[0]
    chains = DefinitionUseChain([ref])
    reaches = chains.find_forward_accesses()
    # We should find 3 results, all 3 references in
    # A = A * A
    assignment = routine.walk(Assignment)[1]
    assert len(reaches[sig]) == 3
    assert reaches[sig][0] is assignment.rhs.children[0]
    assert reaches[sig][1] is assignment.rhs.children[1]
    assert reaches[sig][2] is assignment.lhs


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
    ref = routine.children[0].lhs
    sig = ref.get_signature_and_indices()[0]
    chains = DefinitionUseChain([ref])
    reaches = chains.find_forward_accesses()
    # TODO #2760 For now the if statement doesn't kill the accesses,
    # even though it will always be written to.
    assert len(reaches[sig]) == 4
    assert reaches[sig][0] is routine.walk(Assignment)[1].rhs.children[0]
    assert reaches[sig][1] is routine.walk(Assignment)[2].lhs
    assert reaches[sig][2] is routine.walk(Assignment)[3].lhs
    assert reaches[sig][3] is routine.walk(Assignment)[4].rhs.children[0]

    # Also check that a = 3 forward access is not a = 4.
    a_3 = routine.children[2].if_body.children[0].lhs
    sig = a_3.get_signature_and_indices()[0]
    a_4 = routine.children[2].else_body.children[0].rhs
    chains = DefinitionUseChain([a_3])
    reaches = chains.find_forward_accesses()
    assert len(reaches[sig]) == 1
    assert reaches[sig][0] is not a_4


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
    ref = routine.children[1].loop_body.children[1].rhs.children[0]
    sig = ref.get_signature_and_indices()[0]
    chains = DefinitionUseChain(
        [ref]
    )
    reaches = chains.find_forward_accesses()[sig]
    # We should have 3 reaches
    # First two are A = A + i
    # Second is c = a + b
    assert len(reaches) == 3
    assert (
        reaches[0] is routine.walk(Assignment)[1].rhs.children[0]
    )
    assert reaches[1] is routine.walk(Assignment)[1].lhs
    assert reaches[2] is routine.walk(Assignment)[3].rhs.children[0]

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
    ref = routine.children[0].lhs
    sig = ref.get_signature_and_indices()[0]
    chains = DefinitionUseChain(
        [ref]
    )
    reaches = chains.find_forward_accesses()[sig]
    # We should have 1 reaches
    # It should be the loop
    assert len(reaches) == 1
    assert reaches[0] is routine.walk(Loop)[0]


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
    ref = routine.walk(Assignment)[2].lhs
    sig = ref.get_signature_and_indices()[0]
    chains = DefinitionUseChain([ref])
    reaches = chains.find_forward_accesses()[sig]

    assert len(reaches) == 3
    assert reaches[0] is routine.walk(WhileLoop)[0].condition.children[0]
    assert (
        reaches[1] is routine.walk(Assignment)[2].rhs.children[0]
    )
    assert reaches[2] is routine.walk(Assignment)[2].lhs


def test_definition_use_chain_forward_accesses_nested_loop_example(
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
    ref = loops[1].loop_body.children[0].rhs.children[1]
    sig = ref.get_signature_and_indices()[0]
    chains = DefinitionUseChain([ref])
    reaches = chains.find_forward_accesses()[sig]
    # Results should be A = A + 3 and the a < i condition
    assert reaches[0] is loops[0].condition.children[0]
    assert reaches[1] is loops[0].walk(Assignment)[0].rhs.children[0]
    assert reaches[2] is loops[0].walk(Assignment)[0].lhs
    assert reaches[3] is loops[1].walk(Assignment)[0].rhs.children[1]
    assert len(reaches) == 4


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
    ref = routine.walk(Assignment)[0].lhs
    sig = ref.get_signature_and_indices()[0]
    chains = DefinitionUseChain([ref])
    reaches = chains.find_forward_accesses()[sig]
    assert len(reaches) == 1
    assert reaches[0] is routine.walk(Assignment)[2].lhs


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
    ref = routine.walk(Assignment)[0].rhs.children[0]
    sig = ref.get_signature_and_indices()[0]
    chains = DefinitionUseChain([ref])
    reaches = chains.find_forward_accesses()[sig]
    assert len(reaches) == 1
    assert reaches[0] is routine.walk(Assignment)[0].lhs


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
    ref = routine.walk(Assignment)[0].rhs.children[0]
    sig = ref.get_signature_and_indices()[0]
    chains = DefinitionUseChain([ref])
    reaches = chains.find_forward_accesses()[sig]
    assert len(reaches) == 1
    assert reaches[0] is routine.walk(Assignment)[0].lhs


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
    ref = routine.walk(Assignment)[0].lhs
    sig = ref.get_signature_and_indices()[0]
    chains = DefinitionUseChain([ref])
    reaches = chains.find_forward_accesses()[sig]
    assert len(reaches) == 1
    assert reaches[0] is routine.walk(CodeBlock)[0]


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
    ref = routine.walk(Assignment)[0].lhs
    sig = ref.get_signature_and_indices()[0]
    chains = DefinitionUseChain([ref])
    reaches = chains.find_forward_accesses()[sig]
    assert len(reaches) == 1
    assert reaches[0] is routine.walk(CodeBlock)[0]


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
    ref = routine.walk(Assignment)[0].lhs
    sig = ref.get_signature_and_indices()[0]
    chains = DefinitionUseChain([ref])
    reaches = chains.find_forward_accesses()[sig]
    assert len(reaches) == 2
    assert reaches[0] is routine.walk(CodeBlock)[0]
    assert reaches[1] is routine.walk(Call)[1]


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
    ref = routine.walk(Assignment)[0].lhs
    sig = ref.get_signature_and_indices()[0]
    chains = DefinitionUseChain([ref])
    reaches = chains.find_forward_accesses()[sig]
    assert len(reaches) == 1
    assert reaches[0] is routine.walk(CodeBlock)[0]


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
    ref = routine.walk(Assignment)[0].lhs
    sig = ref.get_signature_and_indices()[0]
    chains = DefinitionUseChain([ref])
    reaches = chains.find_forward_accesses()[sig]
    assert len(reaches) == 1
    assert reaches[0] is routine.walk(Call)[0]


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
    chains = DefinitionUseChain([routine.walk(Assignment)[0].lhs])
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
    ref = routine.walk(Assignment)[1].lhs
    sig = ref.get_signature_and_indices()[0]
    chains = DefinitionUseChain([ref])
    reaches = chains.find_forward_accesses()[sig]
    # We should have 3 reaches
    # First two are A = A + i
    # Second is c = a + b
    assert len(reaches) == 3
    assert (
        reaches[0] is routine.walk(Assignment)[1].rhs.children[0]
    )
    assert reaches[1] is routine.walk(Assignment)[1].lhs
    assert reaches[2] is routine.walk(Assignment)[3].rhs.children[0]
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
    ref = routine.walk(Assignment)[1].lhs
    sig = ref.get_signature_and_indices()[0]
    chains = DefinitionUseChain([ref])
    reaches = chains.find_forward_accesses()[sig]
    # We should have 4 reaches
    # First two are A = A + i
    # Then A = b * 4
    # Then c = A + b
    assert len(reaches) == 4
    assert (
        reaches[0] is routine.walk(Assignment)[1].rhs.children[0]
    )
    assert reaches[1] is routine.walk(Assignment)[1].lhs
    assert reaches[2] is routine.walk(Assignment)[2].lhs
    assert reaches[3] is routine.walk(Assignment)[4].rhs.children[0]


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
    ref = routine.walk(Assignment)[1].lhs
    sig = ref.get_signature_and_indices()[0]
    chains = DefinitionUseChain([ref])
    reaches = chains.find_forward_accesses()[sig]
    # We should have 4 reaches
    # First two are A = A + i
    # Then A = b + 4
    # Then c = A + b
    assert len(reaches) == 4
    assert (
        reaches[0] is routine.walk(Assignment)[1].rhs.children[0]
    )
    assert reaches[1] is routine.walk(Assignment)[1].lhs
    assert reaches[2] is routine.walk(Assignment)[2].lhs
    assert reaches[3] is routine.walk(Assignment)[4].rhs.children[0]


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
    ref = routine.walk(Assignment)[0].lhs
    sig = ref.get_signature_and_indices()[0]
    chains = DefinitionUseChain([ref])
    reaches = chains.find_forward_accesses()[sig]
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
    ref = routine.walk(Assignment)[0].lhs
    sig = ref.get_signature_and_indices()[0]
    chains = DefinitionUseChain([ref])
    reaches = chains.find_forward_accesses()[sig]
    assert len(reaches) == 3
    assert reaches[0] is routine.walk(Assignment)[1].rhs.children[0]
    assert reaches[1] is routine.walk(Assignment)[1].rhs.children[1]
    assert reaches[2] is routine.walk(Assignment)[1].lhs


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
    ref = routine.walk(Assignment)[0].lhs
    sig = ref.get_signature_and_indices()[0]
    chains = DefinitionUseChain([ref])
    reaches = chains.find_forward_accesses()[sig]
    assert len(reaches) == 0


def test_definition_use_chains_multiple_ancestor_loops(
    fortran_reader,
):
    '''Test the case where we have multiple ancestor loops
    with accesses.'''
    code = """
    subroutine test
        integer, dimension(100) :: a
        integer :: i, j, k
        do i = 1, 100
            a(i) = 1
            do j = 1, 100
                a(j) = 2
                do k = 1, 100
                  a(k) = 3
                end do
            end do
        end do
    end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    ref = routine.walk(Assignment)[1].lhs
    sig = ref.get_signature_and_indices()[0]
    chains = DefinitionUseChain([ref])
    reaches = chains.find_forward_accesses()[sig]
    assert len(reaches) == 3
    assert reaches[0] is routine.walk(Assignment)[0].lhs
    assert reaches[1] is routine.walk(Assignment)[1].lhs
    assert reaches[2] is routine.walk(Assignment)[2].lhs


def test_definition_use_chain_find_forward_accesses_pure_call(
    fortran_reader,
):
    """Functionality test for the find_forward_accesses routine. This
    tests the behaviour for a pure subrotuine call."""
    code = """
    pure subroutine y(in)
        integer :: in
        in = in + 1
    end subroutine y
    subroutine x(a, b)
    integer :: a, b
    a = 2
    b = 1
    call y(b)
    a = a + 2
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[1]
    # Start from the lhs of the first assignment
    lhs_assign1 = routine.walk(Assignment)[0].lhs
    sig = lhs_assign1.get_signature_and_indices()[0]
    chains = DefinitionUseChain([lhs_assign1])
    reaches = chains.find_forward_accesses()[sig]
    assert len(reaches) == 2
    # Should find the a in the rhs of a = a + 2 and the lhs.
    rhs_assign3 = routine.walk(Assignment)[2].rhs.children[0]
    lhs_assign3 = routine.walk(Assignment)[2].lhs
    assert reaches[0] is rhs_assign3
    assert reaches[1] is lhs_assign3

    # Start from lhs of b = 1
    lhs_assign2 = routine.walk(Assignment)[1].lhs
    sig = lhs_assign2.get_signature_and_indices()[0]
    chains = DefinitionUseChain([lhs_assign2])
    reaches = chains.find_forward_accesses()[sig]
    assert len(reaches) == 1
    # result is first argument of the pure subroutine call
    argument = routine.walk(Call)[0].children[1]
    assert reaches[0] is argument


def test_forward_accesses_nested_loop(fortran_reader):
    """Test that if we have many nested loops we don't repeat the same
    reference in the result."""
    code = """subroutine x
    integer :: i, j, k, l

    do i = 1, 100
      do j = 1, 100
        do k = 1, 100
          l = 1
        end do
      end do
    end do
    end subroutine x"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    lhs = routine.walk(Assignment)[0].lhs
    sig = lhs.get_signature_and_indices()[0]
    chains = DefinitionUseChain([lhs])
    reaches = chains.find_forward_accesses()[sig]
    assert len(reaches) == 1


def test_forward_accesses_multiple_elements(fortran_reader):
    """Test that if we have multiple inputs we get multiple outputs as
    expected."""
    code = """subroutine x
    integer :: i,j,k,l,m

    i = j + k
    j = l
    m = k
    k = i
    end subroutine x"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    assigns = routine.walk(Assignment)
    rhs = assigns[0].rhs
    chains = DefinitionUseChain([rhs.children[0],
                                 rhs.children[1]])
    reaches = chains.find_forward_accesses()
    sig0, _ = rhs.children[0].get_signature_and_indices()
    sig1, _ = rhs.children[1].get_signature_and_indices()

    assert len(reaches[sig0]) == 1
    assert reaches[sig0][0] is assigns[1].lhs
    assert len(reaches[sig1]) == 2
    assert reaches[sig1][0] is assigns[2].rhs
    assert reaches[sig1][1] is assigns[3].lhs


def test_forward_accesses_if_else(fortran_reader):
    """Test that for a reference in an if_body of an IfBlock we don't find
    dependencies from the else_body"""
    code = """subroutine x
    integer :: a
    logical :: test

    if(test) then
      a = 2
    else
      a = 4
    end if
    end subroutine x"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    assigns = routine.walk(Assignment)
    lhs = assigns[0].lhs
    sig = lhs.get_signature_and_indices()[0]
    chains = DefinitionUseChain([lhs])
    reaches = chains.find_forward_accesses()[sig]

    assert len(reaches) == 0



    from psyclone.psyir.nodes import (
        Loop, Directive, Node, Reference, CodeBlock, Call,
        Schedule, IntrinsicCall, StructureReference, IfBlock)
    from psyclone.psyir.symbols import DataSymbol
    from psyclone.psyir.transformations import (
        ArrayAssignment2LoopsTrans, HoistLoopBoundExprTrans, HoistLocalArraysTrans,
        HoistTrans, InlineTrans, Maxval2LoopTrans, ProfileTrans,
        OMPMinimiseSyncTrans, Reference2ArrayRangeTrans,
        ScalarisationTrans, IncreaseRankLoopArraysTrans, MaximalRegionTrans,
        TransformationError)
    
    def increase_rank_and_reorder_nemov5_loops(routine: Routine):
        ''' This method increases the rank of temporary arrays used inside selected
        loops (in order to parallelise the outer loop without overlapping them)
        and then rearranges the outer loop next to the inner ones (in order to
        collapse them), so that more parallelism can be leverage. This is useful
        in GPU contexts, but it increases the memory footprint and may not be
        beneficial for caching-architectures.
    
        :param routine: the target routine.
    
        '''
        irlatrans = IncreaseRankLoopArraysTrans()
    
        # Map of routines and arrays
        selection = {
            "dyn_zdf": ['zwd', 'zwi', 'zws'],
            "tra_zdf_imp": ['zwd', 'zwi', 'zws', 'zwt'],
            "tke_tke": ['zice_fra', 'zd_lw', 'zd_up', 'zdiag', 'zwlc2', 'zpelc',
                        'imlc', 'zhlc', 'zus3'],
            "tke_avn": ['zmxlm', 'zmxld']
        }
    
        if routine.name not in selection:
            return
    
        for outer_loop in routine.walk(Loop, stop_type=Loop):
            if outer_loop.variable.name == "jj":
                # Increase the rank of the temporary arrays in this loop
                irlatrans.apply(outer_loop, arrays=selection[routine.name])
                # Now reorder the code
                for child in outer_loop.loop_body[:]:
                    # Move the contents of the jj loop outside it
                    outer_loop.parent.addchild(child.detach(),
                                               index=outer_loop.position)
                    # Add a new jj loop around each inner loop that is not 'jn'
                    target_loop = []
                    for inner_loop in child.walk(Loop, stop_type=Loop):
                        if inner_loop.variable.name != "jn":
                            target_loop.append(inner_loop)
                        else:
                            for next_loop in inner_loop.loop_body.walk(
                                                Loop, stop_type=Loop):
                                target_loop.append(next_loop)
                    for inner_loop in target_loop:
                        if isinstance(inner_loop.loop_body[0], Loop):
                            inner_loop = inner_loop.loop_body[0]
                        inner_loop.replace_with(
                            Loop.create(
                                outer_loop.variable,
                                outer_loop.start_expr.copy(),
                                outer_loop.stop_expr.copy(),
                                outer_loop.step_expr.copy(),
                                children=[inner_loop.copy()]
                            )
                        )
                # Remove the now empty jj loop
                outer_loop.detach()

    def normalise_loops(
            schedule,
            hoist_local_arrays: bool = True,
            convert_array_notation: bool = True,
            loopify_array_intrinsics: bool = True,
            convert_range_loops: bool = True,
            scalarise_loops: bool = False,
            increase_array_ranks: bool = False,
            hoist_expressions: bool = True,
            ):
        ''' Normalise all loops in the given schedule so that they are in an
        appropriate form for the Parallelisation transformations to analyse
        them.
    
        :param schedule: the PSyIR Schedule to transform.
        :type schedule: :py:class:`psyclone.psyir.nodes.node`
        :param bool hoist_local_arrays: whether to hoist local arrays.
        :param bool convert_array_notation: whether to convert array notation
            to explicit loops.
        :param bool loopify_array_intrinsics: whether to convert intrinsics that
            operate on arrays to explicit loops (currently only maxval).
        :param bool convert_range_loops: whether to convert ranges to explicit
            loops.
        :param scalarise_loops: whether to attempt to convert arrays to scalars
            where possible, default is False.
        :param increase_array_ranks: whether to increase the rank of selected
            arrays.
        :param hoist_expressions: whether to hoist bounds and loop invariant
            statements out of the loop nest.
        '''
        if hoist_local_arrays and schedule.name not in CONTAINS_STMT_FUNCTIONS:
            # Apply the HoistLocalArraysTrans when possible, it cannot be applied
            # to files with statement functions because it will attempt to put the
            # allocate above it, which is not valid Fortran.
            try:
                HoistLocalArraysTrans().apply(schedule)
            except TransformationError:
                pass
    
        if convert_array_notation:
            for reference in schedule.walk(Reference):
                try:
                    Reference2ArrayRangeTrans().apply(reference)
                except TransformationError:
                    pass
    
        if loopify_array_intrinsics:
            for intr in schedule.walk(IntrinsicCall):
                if intr.intrinsic.name == "MAXVAL":
                    try:
                        Maxval2LoopTrans().apply(intr, verbose=True)
                    except TransformationError as err:
                        print(err.value)
    
        if convert_range_loops:
            # Convert all array implicit loops to explicit loops
            explicit_loops = ArrayAssignment2LoopsTrans()
            for assignment in schedule.walk(Assignment):
                try:
                    explicit_loops.apply(
                        assignment, options={'verbose': True})
                except TransformationError:
                    pass
    
        if scalarise_loops:
            # Apply scalarisation to every loop. Execute this in reverse order
            # as sometimes we can scalarise earlier loops if following loops
            # have already been scalarised.
            loops = schedule.walk(Loop)
            loops.reverse()
            scalartrans = ScalarisationTrans()
            for loop in loops:
                scalartrans.apply(loop)
    
        if increase_array_ranks:
            increase_rank_and_reorder_nemov5_loops(schedule)
    
        if hoist_expressions:
            # First hoist all possible expressions
            for loop in schedule.walk(Loop):
                try:
                    HoistLoopBoundExprTrans().apply(loop)
                except TransformationError:
                    pass
    
            # Hoist all possible assignments (in reverse order so the inner loop
            # constants are hoisted all the way out if possible)
            for loop in reversed(schedule.walk(Loop)):
                for statement in list(loop.loop_body):
                    try:
                        HoistTrans().apply(statement)
                    except TransformationError:
                        pass
    
        # TODO #1928: In order to perform better on the GPU, nested loops with two
        # sibling inner loops need to be fused or apply loop fission to the
        # top level. This would allow the collapse clause to be applied.











    code = """
   SUBROUTINE tra_asm_inc( kt, Kbb, Kmm, pts, Krhs )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE tra_asm_inc  ***
      !!
      !! ** Purpose : Apply the tracer (T and S) assimilation increments
      !!
      !! ** Method  : Direct initialization or Incremental Analysis Updating
      !!
      !! ** Action  :
      !!----------------------------------------------------------------------
      INTEGER                                  , INTENT(in   ) :: kt             ! Current time step
      INTEGER                                  , INTENT(in   ) :: Kbb, Kmm, Krhs ! Time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts,jpt), INTENT(inout) :: pts            ! active tracers and RHS of tracer equation
      !
      INTEGER  :: ji, jj, jk
      INTEGER  :: it
      REAL(wp) :: zincwgt  ! IAU weight for current time step
      REAL(wp), DIMENSION(:,:), ALLOCATABLE ::   zfzptnz, zdep2d   ! Freezing point values
      REAL(wp), DIMENSION(jpi,jpj,jpk)      ::   zvalid_bv         ! Mask representing Brunt-Vaisala (N2) checks used to reject T/S
                                                                   ! increments
      !!----------------------------------------------------------------------
         !                             !--------------------------------------
      IF ( ln_asmiau ) THEN            ! Incremental Analysis Updating
         !                             !--------------------------------------
         !
         IF ( ( kt >= nitiaustr_r ).AND.( kt <= nitiaufin_r ) ) THEN
            !
            it = kt - nit000 + 1
            zincwgt = wgtiau(it) / rn_Dt   ! IAU weight for the current time step
            !
            IF( .NOT. l_istiled .OR. ntile == 1 )  THEN                       ! Do only on the first tile
               IF(lwp) THEN
                  WRITE(numout,*)
                  WRITE(numout,*) 'tra_asm_inc : Tracer IAU at time step = ', kt,' with IAU weight = ', wgtiau(it)
                  WRITE(numout,*) '~~~~~~~~~~~~'
               ENDIF
            ENDIF
            !
            IF( ln_temnofreeze ) ALLOCATE( zfzptnz(ntsi-(0):ntei+(0),ntsj-(0):ntej+(0)), zdep2d(ntsi-(0):ntei+(0),ntsj-(0):ntej+(0)) )
            !
            ! Call Brunt-Vaisala checks to reject T/S increments
            zvalid_bv(:,:,:) = 1.0_wp
            IF ( ln_bv_check ) CALL verify_incs_bv( wgtiau(it), Kmm, pts, zvalid_bv )
            !
            ! Update the tracer tendencies
            DO jk = 1, jpkm1
               IF (ln_temnofreeze) THEN
                  ! Do not apply negative increments if the temperature will fall below freezing
                  DO jj = ntsj-( 0), ntej+(  0 ) ; DO ji = ntsi-( 0), ntei+(  0)
                     zdep2d(ji,jj) = ((gdept_1d(jk) ) *(1._wp+r3t(ji,jj,Kmm)))   ! better solution: define an interface for eos_fzp when ((gdept_1d(jk) ) *(1._wp+r3t(ji,jj,Kmm))) is a scalar
                  END DO   ;   END DO
                  CALL eos_fzp( pts(:,:,jk,jp_sal,Kmm), zfzptnz(:,:), zdep2d(:,:), kbnd=0 )
                  !
                  WHERE(t_bkginc(ntsi-(0):ntei+(0),ntsj-(0):ntej+(0),jk) > 0.0_wp .OR. &
                     &   pts(ntsi-(0):ntei+(0),ntsj-(0):ntej+(0),jk,jp_tem,Kmm) + pts(ntsi-(0):ntei+(0),ntsj-(0):ntej+(0),jk,jp_tem,Krhs) + t_bkginc(ntsi-(0):ntei+(0),ntsj-(0):ntej+(0),jk) * wgtiau(it) > zfzptnz(:,:) )
                     pts(ntsi-(0):ntei+(0),ntsj-(0):ntej+(0),jk,jp_tem,Krhs) = pts(ntsi-(0):ntei+(0),ntsj-(0):ntej+(0),jk,jp_tem,Krhs) + t_bkginc(ntsi-(0):ntei+(0),ntsj-(0):ntej+(0),jk) * zvalid_bv(ji,jj,jk) * zincwgt
                  END WHERE
               ELSE
                  DO jj = ntsj-( 0), ntej+(  0 ) ; DO ji = ntsi-( 0), ntei+(  0)
                     pts(ji,jj,jk,jp_tem,Krhs) = pts(ji,jj,jk,jp_tem,Krhs) + t_bkginc(ji,jj,jk) * zvalid_bv(ji,jj,jk) * zincwgt
                  END DO   ;   END DO
               ENDIF
               IF (ln_salfix) THEN
                  ! Do not apply negative increments if the salinity will fall below a specified
                  ! minimum value rn_salfixmin
                  WHERE(s_bkginc(ntsi-(0):ntei+(0),ntsj-(0):ntej+(0),jk) > 0.0_wp .OR. &
                     &   pts(ntsi-(0):ntei+(0),ntsj-(0):ntej+(0),jk,jp_sal,Kmm) + pts(ntsi-(0):ntei+(0),ntsj-(0):ntej+(0),jk,jp_sal,Krhs) + s_bkginc(ntsi-(0):ntei+(0),ntsj-(0):ntej+(0),jk) * wgtiau(it) > rn_salfixmin )
                     pts(ntsi-(0):ntei+(0),ntsj-(0):ntej+(0),jk,jp_sal,Krhs) = pts(ntsi-(0):ntei+(0),ntsj-(0):ntej+(0),jk,jp_sal,Krhs) + s_bkginc(ntsi-(0):ntei+(0),ntsj-(0):ntej+(0),jk) * zvalid_bv(ji,jj,jk) * zincwgt
                  END WHERE
               ELSE
                  DO jj = ntsj-( 0), ntej+(  0 ) ; DO ji = ntsi-( 0), ntei+(  0)
                     pts(ji,jj,jk,jp_sal,Krhs) = pts(ji,jj,jk,jp_sal,Krhs) + s_bkginc(ji,jj,jk) * zvalid_bv(ji,jj,jk) * zincwgt
                  END DO   ;   END DO
               ENDIF
            END DO
            !
            IF( ln_temnofreeze ) DEALLOCATE( zfzptnz, zdep2d )
            !
         ENDIF
         !
         IF( .NOT. l_istiled .OR. ntile == nijtile )  THEN                ! Do only on the last tile
            IF ( kt == nitiaufin_r + 1  ) THEN   ! For bias crcn to work
               IF (ALLOCATED(t_bkginc)) DEALLOCATE( t_bkginc )
               IF (ALLOCATED(s_bkginc)) DEALLOCATE( s_bkginc )
            ENDIF
         ENDIF
         !                             !--------------------------------------
      ELSEIF ( ln_asmdin ) THEN        ! Direct Initialization
         !                             !--------------------------------------
         !
         IF ( kt == nitdin_r ) THEN
            !
            l_1st_euler = .TRUE.  ! Force Euler forward step
            !
            ! Call Brunt-Vaisala checks to reject T/S increments
            zvalid_bv(:,:,:) = 1.0_wp
            IF ( ln_bv_check ) CALL verify_incs_bv( 1.0_wp, Kmm, pts, zvalid_bv )
            !
            ! Initialize the now fields with the background + increment
            IF (ln_temnofreeze) THEN
               ! Do not apply negative increments if the temperature will fall below freezing
               ALLOCATE( zfzptnz(ntsi-(nn_hls):ntei+(nn_hls),ntsj-(nn_hls):ntej+(nn_hls)), zdep2d(ntsi-(nn_hls):ntei+(nn_hls),ntsj-(nn_hls):ntej+(nn_hls)) )
               !
               DO jk = 1, jpkm1
                  DO jj = ntsj-( nn_hls), ntej+(  nn_hls ) ; DO ji = ntsi-( nn_hls), ntei+(  nn_hls)
                     zdep2d(ji,jj) = ((gdept_1d(jk) ) *(1._wp+r3t(ji,jj,Kmm)))   ! better solution: define an interface for eos_fzp when ((gdept_1d(jk) ) *(1._wp+r3t(ji,jj,Kmm))) is a scalar
                  END DO   ;   END DO
                  CALL eos_fzp( pts(:,:,jk,jp_sal,Kmm), zfzptnz(:,:), zdep2d(:,:) )
                  !
                  WHERE( t_bkginc(:,:,jk) > 0.0_wp .OR. pts(:,:,jk,jp_tem,Kmm) + t_bkginc(:,:,jk) > zfzptnz(:,:) )
                     pts(:,:,jk,jp_tem,Kmm) = t_bkg(:,:,jk) + t_bkginc(:,:,jk) * zvalid_bv(:,:,jk)
                  END WHERE
               END DO
               !
               DEALLOCATE( zfzptnz, zdep2d )
            ELSE
               pts(:,:,:,jp_tem,Kmm) = t_bkg(:,:,:) + t_bkginc(:,:,:) * zvalid_bv(:,:,:)
            ENDIF
            IF (ln_salfix) THEN
               ! Do not apply negative increments if the salinity will fall below a specified
               ! minimum value rn_salfixmin
               WHERE( s_bkginc(:,:,:) > 0.0_wp .OR. pts(:,:,:,jp_sal,Kmm) + s_bkginc(:,:,:) > rn_salfixmin )
                  pts(:,:,:,jp_sal,Kmm) = s_bkg(:,:,:) + s_bkginc(:,:,:) * zvalid_bv(:,:,:)
               END WHERE
            ELSE
               pts(:,:,:,jp_sal,Kmm) = s_bkg(:,:,:) + s_bkginc(:,:,:) * zvalid_bv(:,:,:)
            ENDIF

            pts(:,:,:,:,Kbb) = pts(:,:,:,:,Kmm)                 ! Update before fields
            CALL eos( pts, Kbb, rhd, rhop )                     ! Before potential and in situ densities

            DEALLOCATE( t_bkginc )
            DEALLOCATE( s_bkginc )
            DEALLOCATE( t_bkg    )
            DEALLOCATE( s_bkg    )
         ENDIF
         !
      ENDIF
      ! Perhaps the following call should be in step
      IF ( ln_sicinc )   CALL sic_asm_inc ( kt )      ! apply sea ice concentration increment
      IF ( ln_sitinc )   CALL sit_asm_inc ( kt )      ! apply sea ice thickness increment
      !
   END SUBROUTINE tra_asm_inc"""
    psyir = fortran_reader.psyir_from_source(code)
    normalise_loops(
            psyir.walk(Routine)[0],
            hoist_local_arrays=False,
            convert_array_notation=True,
            loopify_array_intrinsics=True,
            convert_range_loops=True,
            increase_array_ranks=True,
            hoist_expressions=True
    )
    references = psyir.walk(Reference)
    res = None
    for ref in references:
        if "pts" in ref.parent.debug_string():
            print(ref.parent.debug_string())
        if "pts(widx1,widx2,jk,jp_tem,kmm) = t_bkg(LBOUND(t_bkg, dim=1) + widx1 - 1,LBOUND(t_bkg, dim=2) + widx2 - 1,jk) + t_bkginc(LBOUND(t_bkginc, dim=1) + widx1 - 1,LBOUND(t_bkginc, dim=2) + widx2 - 1,jk) * zvalid_bv(widx1,widx2,jk)" in ref.parent.debug_string():
            res = ref
            break
    print(res)
    assert False
