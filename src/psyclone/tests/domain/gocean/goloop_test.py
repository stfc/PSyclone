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
# Authors A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified work Copyright (c) 2018-2019 by J. Henrichs, Bureau of Meteorology
# Modified R. W. Ford, STFC Daresbury Lab
# Modified: I. Kavcic, Met Office

'''Tests for the GOLoop class.'''

from __future__ import absolute_import, print_function
import pytest
from psyclone.gocean1p0 import GOKern, GOLoop, GOInvokeSchedule
from psyclone.psyir.nodes import Schedule, Reference, StructureReference, \
    Literal
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE
from psyclone.errors import GenerationError
from psyclone.tests.utilities import get_invoke

API = "gocean1.0"


def test_goloop_no_parent():
    ''' Attempt to generate code for a loop that has no GOInvokeSchedule
    as a parent '''
    # First create with a schedule as one is required to declare the
    # loop variable
    schedule = Schedule()
    goloop = GOLoop(loop_type="inner", parent=schedule)
    schedule.children = [goloop]
    # Now remove parent and children
    goloop._parent = None
    goloop.children = None
    # Try and generate the code for this loop even though it
    # has no parent schedule and no children
    with pytest.raises(GenerationError):
        goloop.gen_code(None)


def test_goloop_no_children():
    ''' Attempt to generate code for a loop that has no child
    kernel calls '''
    gosched = GOInvokeSchedule('name', [])
    gojloop = GOLoop(parent=gosched, loop_type="outer")
    gosched.addchild(gojloop)
    goiloop = GOLoop(parent=gojloop.loop_body, loop_type="inner")
    gojloop.loop_body.addchild(goiloop)
    # Try and generate the code for this loop even though it
    # has no children
    with pytest.raises(GenerationError):
        goiloop.gen_code(None)


def test_goloop_unsupp_offset():
    ''' Attempt to generate code for a loop with constant bounds with
    an unsupported index offset '''
    gosched = GOInvokeSchedule('name', [])
    # This test expects constant loop bounds
    gosched._const_loop_bounds = True
    gojloop = GOLoop(parent=gosched, loop_type="outer")
    gosched.addchild(gojloop)
    goiloop = GOLoop(parent=gojloop.loop_body, loop_type="inner")
    gojloop.loop_body.addchild(goiloop)
    gokern = GOKern()
    # Set the index-offset of this kernel to a value that is not
    # supported when using constant loop bounds
    gokern._index_offset = "offset_se"
    goiloop.loop_body.addchild(gokern)
    with pytest.raises(GenerationError):
        goiloop.gen_code(None)


def test_goloop_unmatched_offsets():
    ''' Attempt to generate code for a loop with constant bounds with
    two different index offsets '''
    gosched = GOInvokeSchedule('name', [])
    gojloop = GOLoop(parent=gosched, loop_type="outer")
    gosched.addchild(gojloop)
    goiloop = GOLoop(parent=gojloop.loop_body, loop_type="inner")
    gojloop.loop_body.addchild(goiloop)
    gokern1 = GOKern()
    gokern2 = GOKern()
    # Set the index-offset of this kernel to a value that is not
    # supported when using constant loop bounds
    gokern1._index_offset = "go_offset_ne"
    gokern2._index_offset = "go_offset_sw"
    goiloop.loop_body.addchild(gokern1)
    goiloop.loop_body.addchild(gokern2)
    with pytest.raises(GenerationError) as excinfo:
        goiloop.gen_code(None)
    # Note that the kernels do not have a name, so there is a double space
    assert "All Kernels must expect the same grid offset but kernel  " \
        "has offset go_offset_sw which does not match go_offset_ne" \
        in str(excinfo.value)


def test_goloop_bounds_invalid_iteration_space():
    ''' Check that the _upper/lower_bound() methods raise the expected error
    if the iteration space is not recognised. '''
    gosched = GOInvokeSchedule('name', [])
    gojloop = GOLoop(parent=gosched, loop_type="outer")
    # Have to turn-off constant loop bounds to get to the error condition
    gosched._const_loop_bounds = False
    # Set the iteration space to something invalid
    gojloop._iteration_space = "broken"
    with pytest.raises(GenerationError) as err:
        gojloop.upper_bound()
    assert "Unrecognised iteration space, 'broken'." in str(err.value)
    with pytest.raises(GenerationError) as err:
        gojloop.lower_bound()
    assert "Unrecognised iteration space, 'broken'." in str(err.value)


def test_goloop_grid_property_psyir_expression():
    ''' Tests for the _grid_property_psyir_expression() method. '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           API, idx=0)
    schedule = invoke.schedule
    loop = schedule.walk(GOLoop)[0]
    # A simple name should result in a new symbol and a suitable reference
    assert "hello" not in schedule.symbol_table
    href = loop._grid_property_psyir_expression("hello")
    hsym = schedule.symbol_table.lookup("hello")
    assert isinstance(hsym, DataSymbol)
    assert href.parent is loop
    assert hsym.datatype == INTEGER_TYPE
    assert isinstance(href, Reference)
    # A derived-type reference must be in the form of a format string with
    # "{0}" at the start.
    with pytest.raises(NotImplementedError) as err:
        loop._grid_property_psyir_expression("wrong%one")
    assert ("Supplied grid property is a derived-type reference but does "
            "not begin with '{0}': 'wrong%one'" in str(err.value))
    gref = loop._grid_property_psyir_expression("{0}%grid%xstart")
    assert isinstance(gref, StructureReference)
    assert gref.parent is None
    assert gref.symbol.name == "cv_fld"


def test_goloop_lower_to_language_level(monkeypatch):
    ''' Tests that the GOLoop lower_to_language_level method provides the start
    and stop expressions for the loops using the upper/lower_bound methods. '''
    schedule = Schedule()
    goloop = GOLoop(loop_type="inner", parent=schedule)
    assert goloop.start_expr.value == 'NOT_INITIALISED'
    assert goloop.stop_expr.value == 'NOT_INITIALISED'
    monkeypatch.setattr(GOLoop, "lower_bound",
                        lambda x: Literal("1", INTEGER_TYPE))
    monkeypatch.setattr(GOLoop, "upper_bound",
                        lambda x: Literal("1", INTEGER_TYPE))

    goloop.lower_to_language_level()
    assert goloop.start_expr.value == '1'
    assert goloop.stop_expr.value == '1'
