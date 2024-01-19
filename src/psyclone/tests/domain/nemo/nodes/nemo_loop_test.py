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
# -----------------------------------------------------------------------------
# Authors R. W. Ford and S. Siso, STFC Daresbury Lab

'''Module containing tests for the NemoLoop node. '''

import pytest

from psyclone.nemo import NemoLoop
from psyclone.psyir.nodes import Assignment, Loop, Literal, Reference, Schedule
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, REAL_TYPE
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.errors import GenerationError


def test_instance():
    '''Check that it is possible to create an instance of
    NemoLoop and that it has the expected properties.

    '''
    nemo_loop = NemoLoop()
    assert isinstance(nemo_loop, NemoLoop)
    assert isinstance(nemo_loop, Loop)
    assert nemo_loop.parent is None
    assert nemo_loop._variable is None
    assert (nemo_loop._valid_loop_types ==
            ['lat', 'levels', 'lon', 'tracers', 'unknown'])


def test_instance_args():
    '''Check that the parent and variable arguments are stored correctly
    when values are provided.

    '''
    variable = DataSymbol("nemo_symbol", INTEGER_TYPE)
    nemo_loop = NemoLoop(variable=variable)
    assert nemo_loop._variable is variable


def test_create():
    '''Test that the static create() method creates a NemoLoop instance
    and its children corectly.

    '''
    variable = DataSymbol("ji", INTEGER_TYPE)
    start = Literal("2", INTEGER_TYPE)
    stop = Literal("10", INTEGER_TYPE)
    step = Literal("1", INTEGER_TYPE)
    x_var = DataSymbol("X", REAL_TYPE)
    children = [Assignment.create(Reference(x_var), Literal("3.0", REAL_TYPE))]
    nemo_loop = NemoLoop.create(variable, start, stop, step, children)
    assert isinstance(nemo_loop, NemoLoop)
    assert nemo_loop.loop_type == "lon"
    assert isinstance(nemo_loop.loop_body, Schedule)
    assert len(nemo_loop.loop_body.children) == 1
    assert nemo_loop.loop_body.children[0] is children[0]
    assert children[0].parent is nemo_loop.loop_body
    assert nemo_loop.loop_body.parent is nemo_loop
    assert nemo_loop.variable is variable
    assert nemo_loop.start_expr is start
    assert nemo_loop.stop_expr is stop
    assert nemo_loop.step_expr is step
    writer = FortranWriter()
    result = writer(nemo_loop)
    assert (
        "do ji = 2, 10, 1\n"
        "  X = 3.0\n"
        "enddo" in result)


def test_create_unknown():
    '''Test that the static create() method creates a NemoLoop instance
    with an unknown loop type if the loop type is not recognised.

    '''
    variable = DataSymbol("idx", INTEGER_TYPE)
    start = Literal("2", INTEGER_TYPE)
    stop = Literal("10", INTEGER_TYPE)
    step = Literal("1", INTEGER_TYPE)
    x_var = DataSymbol("X", REAL_TYPE)
    children = [Assignment.create(Reference(x_var), Literal("3.0", REAL_TYPE))]
    nemo_loop = NemoLoop.create(variable, start, stop, step, children)
    assert nemo_loop.loop_type == "unknown"


def test_create_errors():
    '''Test that the expected exceptions are raised when the arguments to
    the create method are invalid.

    '''
    variable = DataSymbol("ji", INTEGER_TYPE)
    start = Literal("2", INTEGER_TYPE)
    stop = Literal("10", INTEGER_TYPE)
    step = Literal("1", INTEGER_TYPE)
    x_var = DataSymbol("X", REAL_TYPE)
    children = Assignment.create(Reference(x_var), Literal("3.0", REAL_TYPE))
    with pytest.raises(GenerationError) as info:
        _ = NemoLoop.create(None, start, stop, step, [children.copy()])
    assert ("Generation Error: variable property in Loop class should be a "
            "DataSymbol but found 'NoneType'" in str(info.value))
    with pytest.raises(GenerationError) as info:
        _ = NemoLoop.create(variable, None, stop, step, [children.copy()])
    assert ("Generation Error: Item 'NoneType' can't be child 0 of 'Loop'. "
            "The valid format is: 'DataNode, DataNode, DataNode, Schedule'."
            in str(info.value))
    with pytest.raises(GenerationError) as info:
        _ = NemoLoop.create(variable, start, None, step, [children.copy()])
    assert ("Generation Error: Item 'NoneType' can't be child 1 of 'Loop'. "
            "The valid format is: 'DataNode, DataNode, DataNode, Schedule'."
            in str(info.value))
    with pytest.raises(GenerationError) as info:
        _ = NemoLoop.create(variable, start, stop, None, [children.copy()])
    assert ("Generation Error: Item 'NoneType' can't be child 2 of 'Loop'. "
            "The valid format is: 'DataNode, DataNode, DataNode, Schedule'."
            in str(info.value))
    with pytest.raises(GenerationError) as info:
        _ = NemoLoop.create(variable, start, stop, step, None)
    assert ("Generation Error: children argument in create method of NemoLoop "
            "class should be a list but found 'NoneType'." in str(info.value))
