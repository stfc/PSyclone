# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2024, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' Performs py.test tests on the PSyLoop PSyIR node. '''

import pytest
from psyclone.domain.common.psylayer import PSyLoop
from psyclone.psyir.nodes import Literal, Schedule, Assignment, Reference
from psyclone.psyir.symbols import DataSymbol, REAL_SINGLE_TYPE, \
    INTEGER_SINGLE_TYPE, INTEGER_TYPE, SymbolTable
from psyclone.tests.utilities import get_invoke


def test_psyloop_init():
    '''Test that a loop instance is created as expected and that it raises
    the expected exceptions where appropriate.

    '''
    loop = PSyLoop()
    assert loop.parent is None
    assert loop._valid_loop_types == []
    assert loop.annotations == []
    assert loop._loop_type is None
    assert loop._field is None
    assert loop._field_name is None
    assert loop._field_space is None
    assert loop._iteration_space is None
    assert loop._kern is None
    assert loop._iterates_over == "unknown"
    assert loop._variable is None

    # valid variable
    loop = PSyLoop(variable=DataSymbol("var", INTEGER_TYPE))
    assert loop.variable.name == "var"

    # valid_loop_types. Note, there is no error checking for this
    # variable in the Loop class.
    loop = PSyLoop(valid_loop_types=["a"])
    assert loop.valid_loop_types == ["a"]

    parent = Schedule()
    loop = PSyLoop(parent=parent)
    assert loop.parent is parent

    annotations = ["was_where"]
    loop = PSyLoop(annotations=annotations)
    assert loop.annotations == annotations


def test_psyloop_invalid_type():
    ''' Tests assigning an invalid type to a PSyLoop object. '''
    _, invoke = get_invoke("single_invoke.f90", "gocean1.0", idx=0,
                           dist_mem=False)
    sched = invoke.schedule
    loop = sched.children[0].loop_body[0]
    assert isinstance(loop, PSyLoop)
    with pytest.raises(TypeError) as err:
        loop.loop_type = "not_a_valid_type"
    assert ("loop_type value (not_a_valid_type) is invalid. Must be one of "
            "['inner', 'outer']" in str(err.value))


def test_psyloop_halo_read_access_is_abstract():
    '''Check that the generic _halo_read_access method is abstract'''
    loop = PSyLoop()
    with pytest.raises(NotImplementedError) as excinfo:
        _ = loop._halo_read_access(None)
    assert ("This method needs to be implemented by the APIs that support "
            "distributed memory.") in str(excinfo.value)


def test_psyloop_equality():
    '''Test the __eq__ method of PSyLoop'''
    # We need to manually set the same SymbolTable instance in both loops
    # objects for their equality to be True
    symboltable = SymbolTable()
    # Set up the symbols
    tmp = DataSymbol("tmp", REAL_SINGLE_TYPE)
    i_sym = DataSymbol("i", REAL_SINGLE_TYPE)

    # Create two equal loops
    loop_sym = DataSymbol("i", INTEGER_SINGLE_TYPE)
    sched1 = Schedule(symbol_table=symboltable)
    start = Literal("0", INTEGER_SINGLE_TYPE)
    stop = Literal("1", INTEGER_SINGLE_TYPE)
    step = Literal("1", INTEGER_SINGLE_TYPE)
    child_node = Assignment.create(
        Reference(tmp),
        Reference(i_sym))
    sched1.addchild(child_node)
    loop1 = PSyLoop.create(loop_sym, start, stop, step, [])
    loop1.children[3].detach()
    loop1.addchild(sched1, 3)
    start2 = start.copy()
    stop2 = stop.copy()
    step2 = step.copy()
    sched2 = Schedule()
    # Make sure it has the same ST instance, providing it as a constructor
    # parameter would create a copy and not use the same instance.
    sched2._symbol_table = symboltable
    child_node2 = Assignment.create(
        Reference(tmp),
        Reference(i_sym))
    sched2.addchild(child_node2)
    loop2 = PSyLoop.create(loop_sym, start2, stop2, step2, [])
    loop2.children[3].detach()
    loop2.addchild(sched2, 3)
    assert loop1 == loop2

    # Set loop type for loop2
    loop2._valid_loop_types.append("fake")
    loop2.loop_type = "fake"
    assert loop1 != loop2

    # Set different field and reset loop2
    sched2.detach()
    step2.detach()
    stop2.detach()
    start2.detach()
    loop2 = PSyLoop.create(loop_sym, start2, stop2, step2, [])
    loop2.children[3].detach()
    loop2.addchild(sched2, 3)
    loop2._field = "a"
    assert loop1 != loop2

    # Set different field name
    loop2._field = None
    loop2._field_name = "a"
    assert loop1 != loop2

    # Set different field space
    loop2._field_name = None
    loop2.field_space = "v0"
    assert loop1 != loop2

    # Set different iteration spaces
    loop2.field_space = loop1.field_space
    loop2.iteration_space = "z"
    assert loop1 != loop2

    # Set different kernels
    loop2.iteration_space = loop1.iteration_space
    loop2.kernel = "z"
    assert loop1 != loop2

    # Set different variables
    loop2.kernel = loop1.kernel
    loop2.variable = DataSymbol("k", INTEGER_SINGLE_TYPE)
    assert loop1 != loop2
