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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#        J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, Met Office

''' Module containing tests of loop swap transformation.'''

import re
import pytest

from psyclone.domain.gocean.transformations import GOceanLoopFuseTrans
from psyclone.psyir.nodes import CodeBlock, Loop
from psyclone.psyir.symbols import ContainerSymbol
from psyclone.psyir.transformations import LoopSwapTrans, TransformationError
from psyclone.tests.gocean_build import GOceanBuild
from psyclone.tests.utilities import get_invoke


def test_loop_swap_apply(tmpdir):
    ''' Testing correct loop swapping transform. Esp. try first, middle, and
    last invokes to make sure the inserting of the inner loop happens at
    the right place.'''

    psy, _ = get_invoke("test27_loop_swap.f90", "gocean1.0", idx=0,
                        dist_mem=False)
    invoke = psy.invokes.get("invoke_loop1")
    schedule = invoke.schedule
    schedule_str = str(schedule)

    # First make sure to throw an early error if the source file
    # test27_loop_swap.f90 should have been changed
    expected = (
        r"Loop\[variable:'j'.*?"
        r"Loop\[variable:'i'.*?"
        r"kern call: bc_ssh_code.*?"
        r"Loop\[variable:'j'.*?"
        r"Loop\[variable:'i'.*?"
        r"kern call: bc_solid_u_code .*?"
        r"Loop\[variable:'j'.*?"
        r"Loop\[variable:'i'.*?"
        r"kern call: bc_solid_v_code")

    assert re.search(expected, schedule_str.replace("\n", " "))

    # Now swap the first loops
    swap = LoopSwapTrans()
    swap.apply(schedule.children[0])
    schedule_str = str(schedule)

    expected = (
        r"Loop\[variable:'i'.*?"
        r"Loop\[variable:'j'.*?"
        r"kern call: bc_ssh_code.*?"
        r"Loop\[variable:'j'.*?"
        r"Loop\[variable:'i'.*?"
        r"kern call: bc_solid_u_code .*?"
        r"Loop\[variable:'j'.*?"
        r"Loop\[variable:'i'.*?"
        r"kern call: bc_solid_v_code")

    assert re.search(expected, schedule_str.replace("\n", " "))

    # Now swap the middle loops
    swap.apply(schedule.children[1])
    schedule_str = str(schedule)

    expected = (
        r"Loop\[variable:'i'.*?"
        r"Loop\[variable:'j'.*?"
        r"kern call: bc_ssh_code.*?"
        r"Loop\[variable:'i'.*?"
        r"Loop\[variable:'j'.*?"
        r"kern call: bc_solid_u_code .*?"
        r"Loop\[variable:'j'.*?"
        r"Loop\[variable:'i'.*?"
        r"kern call: bc_solid_v_code")

    assert re.search(expected, schedule_str.replace("\n", " "))

    # Now swap the last loops
    swap.apply(schedule.children[2])
    schedule_str = str(schedule)

    expected = (
        r"Loop\[variable:'i'.*?"
        r"Loop\[variable:'j'.*?"
        r"kern call: bc_ssh_code.*?"
        r"Loop\[variable:'i'.*?"
        r"Loop\[variable:'j'.*?"
        r"kern call: bc_solid_u_code .*?"
        r"Loop\[variable:'i'.*?"
        r"Loop\[variable:'j'.*?"
        r"kern call: bc_solid_v_code")

    assert re.search(expected, schedule_str.replace("\n", " "))

    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_loop_swap_validate():
    ''' Test loop swapping transform with incorrect parameters. '''

    psy, invoke_loop1 = get_invoke("test27_loop_swap.f90", "gocean1.0",
                                   idx=1, dist_mem=False)

    schedule = invoke_loop1.schedule
    swap = LoopSwapTrans()
    assert str(swap) == "Exchange the order of two nested loops: inner "\
        "becomes outer and vice versa"

    # Test error if given node is not the outer loop of at least
    # a double nested loop:
    with pytest.raises(TransformationError) as error:
        swap.apply(schedule.children[0].loop_body[0])
    assert re.search("Transformation Error: Target of LoopSwapTrans "
                     "transformation must be a sub-class of Loop but got "
                     "'GOKern'.", str(error.value), re.S) is not None

    # Not a loop: use the call to bc_ssh_code node as example for this test:
    with pytest.raises(TransformationError) as error:
        swap.apply(schedule.children[0].loop_body[0].loop_body[0])
    assert ("Target of LoopSwapTrans transformation must be a sub-class of "
            "Loop but got 'GOKern'" in str(error.value))

    # Now create an outer loop with more than one inner statement
    # ... by fusing the first and second outer loops :(
    invoke_loop2 = psy.invokes.get("invoke_loop2")
    schedule = invoke_loop2.schedule

    fuse = GOceanLoopFuseTrans()
    fuse.apply(schedule.children[0], schedule.children[1])

    with pytest.raises(TransformationError) as error:
        swap.apply(schedule.children[0])
    assert re.search("Supplied node .* must be the outer loop of a loop nest "
                     "and must have exactly one inner loop, but this node "
                     "has 2 inner statements, the first two being .* and .*",
                     str(error.value), re.S) is not None

    # Now remove the body of the first inner loop, and pass the first
    # inner loop --> i.e. a loop with an empty body
    del schedule.children[0].loop_body[0].children[3].children[0]

    with pytest.raises(TransformationError) as error:
        swap.apply(schedule.children[0].loop_body[0])
    assert re.search("Supplied node .* must be the outer loop of a loop nest "
                     "and must have one inner loop, but this node does not "
                     "have any statements inside.",
                     str(error.value), re.S) is not None


def test_loop_swap_validate_loop_type():
    '''
    Test loop swapping transform when supplied loops are not Loops.
    '''
    swap = LoopSwapTrans()
    _, invoke = get_invoke("1.0.1_single_named_invoke.f90",
                           "dynamo0.3", idx=0, dist_mem=True)
    with pytest.raises(TransformationError) as error:
        swap.apply(invoke.schedule.children[4])

    assert re.search("Error in LoopSwap transformation. Supplied node .* "
                     "must be the outer loop of a loop nest but the first "
                     "inner statement is not a valid loop:",
                     str(error.value), re.S)


def test_loop_swap_validate_nodes_in_loop(fortran_reader):
    '''
    Tests that loops containing impure calls or codeblocks are not swapped.
    '''
    # A dummy program to easily create the PSyIR for the
    # test cases we need.
    source = '''program test_prog
                 integer :: i, j
                 do j=1, 10
                    do i=1, 10
                       call sub()
                    enddo
                 enddo
                 do j=1, 10
                    do i=1, 10
                       write(*,*) i,j
                    enddo
                 enddo
                 end program test_prog'''

    psyir = fortran_reader.psyir_from_source(source)
    schedule = psyir.children[0]
    swap = LoopSwapTrans()

    # Check with a subroutine which is not guaranteed to be pure
    with pytest.raises(TransformationError) as err:
        swap.apply(schedule[0])
    assert ("Nodes of type 'Call' cannot be enclosed by a LoopSwapTrans "
            "unless they can be guaranteed to be pure, but found:"
            in str(err.value))

    # Make sure the write statement is stored as a code block
    assert isinstance(schedule[1].loop_body[0].loop_body[0], CodeBlock)
    with pytest.raises(TransformationError) as err:
        swap.apply(schedule[1])
    assert ("Nodes of type 'CodeBlock' cannot be enclosed by a LoopSwapTrans "
            "transformation" in str(err.value))


def test_loop_swap_validate_dependent_loop(fortran_reader):
    '''
    Tests that loops containing dependencies between the inner or the outer
    loop variables and boundary expressions are not validated for swapping.
    '''
    swap_trans = LoopSwapTrans()
    psyir = fortran_reader.psyir_from_source('''
        program test_prog
            integer :: i, j
            real, dimension(10) :: a
            do j = 1, 10
                do i = 1, j
                    a = a + 1
                enddo
            enddo
            do j = 3 + 2 * i, 10
                do i = 1, 10
                    a = a + 1
                enddo
            enddo
         end program test_prog''')

    loops = psyir.walk(Loop, stop_type=Loop)

    with pytest.raises(TransformationError) as err:
        swap_trans.apply(loops[0])
    assert ("Error in LoopSwap transformation: The outer loop iteration "
            "variable 'j' is part of the inner loop boundary expressions, "
            "so their order can not be swapped." in str(err.value))

    with pytest.raises(TransformationError) as err:
        swap_trans.apply(loops[1])
    assert ("Error in LoopSwap transformation: The inner loop iteration "
            "variable 'i' is part of the outer loop boundary expressions, "
            "so their order can not be swapped." in str(err.value))


def test_loop_swap_schedule_is_kept():
    ''' Testing that the existing schedules remain in place (since they could
    contain annotations).
    '''

    psy, _ = get_invoke("test27_loop_swap.f90", "gocean1.0", idx=0,
                        dist_mem=False)
    invoke = psy.invokes.get("invoke_loop1")
    schedule = invoke.schedule
    schedule_str = str(schedule)

    # First make sure to throw an early error if the source file
    # test27_loop_swap.f90 should have been changed
    expected = (
        r"Loop\[variable:'j'.*?"
        r"Loop\[variable:'i'.*?"
        r"kern call: bc_ssh_code.*?"
        r"Loop\[variable:'j'.*?"
        r"Loop\[variable:'i'.*?"
        r"kern call: bc_solid_u_code .*?"
        r"Loop\[variable:'j'.*?"
        r"Loop\[variable:'i'.*?"
        r"kern call: bc_solid_v_code")

    assert re.search(expected, schedule_str.replace("\n", " "))

    # Save the old schedules
    outer_sched_old = schedule.children[0].loop_body
    inner_sched_old = outer_sched_old.children[0].loop_body
    # Now swap the first loops
    swap = LoopSwapTrans()
    swap.apply(schedule.children[0])

    # Get the schedules after swapping
    outer_sched_new = schedule.children[0].loop_body
    inner_sched_new = outer_sched_new.children[0].loop_body

    # Make sure we still have the same schedule.
    assert outer_sched_old is outer_sched_new
    assert inner_sched_old is inner_sched_new


def test_loop_swap_abort_if_symbols():
    ''' Testing that the transformation aborts if the symbol table for
    either the inner or outer loop contains a non-empty symbol table.
    '''

    psy, _ = get_invoke("test27_loop_swap.f90", "gocean1.0", idx=0,
                        dist_mem=False)
    invoke = psy.invokes.get("invoke_loop1")
    schedule = invoke.schedule
    schedule_str = str(schedule)

    # First make sure to throw an early error if the source file
    # test27_loop_swap.f90 should have been changed
    expected = (
        r"Loop\[variable:'j'.*?"
        r"Loop\[variable:'i'.*?"
        r"kern call: bc_ssh_code.*?"
        r"Loop\[variable:'j'.*?"
        r"Loop\[variable:'i'.*?"
        r"kern call: bc_solid_u_code .*?"
        r"Loop\[variable:'j'.*?"
        r"Loop\[variable:'i'.*?"
        r"kern call: bc_solid_v_code")

    assert re.search(expected, schedule_str.replace("\n", " "))

    # Save the old schedules
    outer_sched = schedule.children[0].loop_body
    sym = ContainerSymbol("my_mod")
    outer_sched.symbol_table.add(sym)

    swap = LoopSwapTrans()
    # Test if outer loop has a symbol table
    with pytest.raises(TransformationError) as err:
        swap.apply(schedule.children[0])

    assert ("Error in LoopSwap transformation: The outer loop has a "
            "non-empty symbol table." in str(err.value))
    outer_sched.symbol_table.remove(sym)

    # Test for symbol table in inner loop
    inner_sched = outer_sched.children[0].loop_body
    inner_sched.symbol_table.add(sym)

    with pytest.raises(TransformationError) as err:
        swap.apply(schedule.children[0])

    assert ("Error in LoopSwap transformation: The inner loop has a "
            "non-empty symbol table." in str(err.value))
