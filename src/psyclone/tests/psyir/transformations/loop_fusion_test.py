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
# Author: J. Henrichs, Bureau of Meteorology
# Modifies R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic, Met Office

'''This module tests the loop fusion transformation.
'''

from __future__ import absolute_import, print_function

import pytest
from psyclone.psyir.nodes import Literal, Loop, Schedule, Return
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE
from psyclone.psyir.transformations import TransformationError
from psyclone.transformations import LoopFuseTrans


def test_fusetrans_error_incomplete():
    ''' Check that we reject attempts to fuse loops which are incomplete. '''
    sch = Schedule()
    loop1 = Loop(variable=DataSymbol("i", INTEGER_TYPE), parent=sch)
    loop2 = Loop(variable=DataSymbol("j", INTEGER_TYPE), parent=sch)
    sch.addchild(loop1)
    sch.addchild(loop2)

    fuse = LoopFuseTrans()

    # Check first loop
    with pytest.raises(TransformationError) as err:
        fuse.validate(loop1, loop2)
    assert "Error in LoopFuse transformation. The first loop does not have " \
        "4 children." in str(err.value)

    loop1.addchild(Literal("start", INTEGER_TYPE, parent=loop1))
    loop1.addchild(Literal("stop", INTEGER_TYPE, parent=loop1))
    loop1.addchild(Literal("step", INTEGER_TYPE, parent=loop1))
    loop1.addchild(Schedule(parent=loop1))
    loop1.loop_body.addchild(Return(parent=loop1.loop_body))

    # Check second loop
    with pytest.raises(TransformationError) as err:
        fuse.validate(loop1, loop2)
    assert "Error in LoopFuse transformation. The second loop does not have " \
        "4 children." in str(err.value)

    loop2.addchild(Literal("start", INTEGER_TYPE, parent=loop2))
    loop2.addchild(Literal("stop", INTEGER_TYPE, parent=loop2))
    loop2.addchild(Literal("step", INTEGER_TYPE, parent=loop2))
    loop2.addchild(Schedule(parent=loop2))
    loop2.loop_body.addchild(Return(parent=loop2.loop_body))

    # Validation should now pass
    fuse.validate(loop1, loop2)


def test_fusetrans_error_not_same_parent():
    ''' Check that we reject attempts to fuse loops which don't share the
    same parent '''

    sch1 = Schedule()
    sch2 = Schedule()
    loop1 = Loop(variable=DataSymbol("i", INTEGER_TYPE), parent=sch1)
    loop2 = Loop(variable=DataSymbol("j", INTEGER_TYPE), parent=sch2)
    sch1.addchild(loop1)
    sch2.addchild(loop2)

    loop1.addchild(Literal("1", INTEGER_TYPE, parent=loop1))  # start
    loop1.addchild(Literal("10", INTEGER_TYPE, parent=loop1))  # stop
    loop1.addchild(Literal("1", INTEGER_TYPE, parent=loop1))  # step
    loop1.addchild(Schedule(parent=loop1))  # loop body

    loop2.addchild(Literal("1", INTEGER_TYPE, parent=loop2))  # start
    loop2.addchild(Literal("10", INTEGER_TYPE, parent=loop2))  # stop
    loop2.addchild(Literal("1", INTEGER_TYPE, parent=loop2))  # step
    loop2.addchild(Schedule(parent=loop2))  # loop body

    fuse = LoopFuseTrans()

    # Try to fuse loops with different parents
    with pytest.raises(TransformationError) as err:
        fuse.validate(loop1, loop2)
    assert "Error in LoopFuse transformation. Loops do not have the " \
        "same parent" in str(err.value)
