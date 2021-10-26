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
# -----------------------------------------------------------------------------
# Authors A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------
''' This module includes the unit test for the BlockedLoop Psyir node'''
import pytest
from psyclone.errors import GenerationError
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import BlockedLoop, Literal, Assignment, Schedule, \
        Reference
from psyclone.psyir.symbols import INTEGER_SINGLE_TYPE, REAL_SINGLE_TYPE, \
        DataSymbol, INTEGER_TYPE
from psyclone.tests.utilities import check_links


def test_blockedloop_create():
    '''Test that the create method in the BlockedLoop class correctly
    creates a BlockedLoop instance.'''
    start = Literal("0", INTEGER_SINGLE_TYPE)
    stop = Literal("1", INTEGER_SINGLE_TYPE)
    step = Literal("1", INTEGER_SINGLE_TYPE)
    child_node = Assignment.create(
        Reference(DataSymbol("tmp", REAL_SINGLE_TYPE)),
        Reference(DataSymbol("i", REAL_SINGLE_TYPE)))
    loop = BlockedLoop.create(DataSymbol("i", INTEGER_SINGLE_TYPE),
                              start, stop, step, [child_node])
    schedule = loop.children[3]
    assert isinstance(schedule, Schedule)
    check_links(loop, [start, stop, step, schedule])
    check_links(schedule, [child_node])
    result = FortranWriter().loop_node(loop)
    assert result == "do i = 0, 1, 1\n  tmp = i\nenddo\n"


def test_blockedloop_create_invalid():
    '''Test the the create method in the BlockedLoop class correctly
    fails when provided a non-list children argument'''
    zero = Literal("0", INTEGER_SINGLE_TYPE)
    one = Literal("1", INTEGER_SINGLE_TYPE)
    variable = DataSymbol("i", INTEGER_TYPE)
    # children not a list
    with pytest.raises(GenerationError) as excinfo:
        _ = BlockedLoop.create(variable, zero, one, one, "invalid")
    assert ("children argument in create method of Loop class should "
            "be a list but found 'str'." in str(excinfo.value))
