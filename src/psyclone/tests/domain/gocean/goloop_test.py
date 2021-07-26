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
from fparser.two.parser import ParserFactory

from psyclone.errors import GenerationError
from psyclone.gocean1p0 import GOKern, GOLoop, GOInvokeSchedule
from psyclone.psyir.nodes import Schedule, Reference, StructureReference, \
    Node, Literal
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE
from psyclone.tests.utilities import get_invoke

API = "gocean1.0"


def test_goloop_no_parent():
    ''' Attempt to generate code for a loop that has no GOInvokeSchedule
    as a parent '''
    # Attempt to create a GOLoop within a generic Schedule
    schedule = Schedule()
    with pytest.raises(GenerationError) as err:
        goloop = GOLoop(loop_type="inner", parent=schedule)
    assert ("GOLoops must always be constructed with a parent which is inside "
            "(directly or indirectly) of a GOInvokeSchedule" in str(err.value))

    # Now create it in a GOInvokeSchedule but then detach it
    schedule = GOInvokeSchedule('name', [])
    goloop = GOLoop(loop_type="inner", parent=schedule)
    schedule.children = [goloop]
    # Now remove parent and children
    goloop.detach()

    # Try and generate the code for this loop even though it
    # has no parent schedule and no children
    with pytest.raises(GenerationError):
        goloop.gen_code(None)


def test_goloop_no_children():
    ''' Attempt to generate code for a loop that has no child
    kernel calls '''
    gosched = GOInvokeSchedule('name', [])
    goloop = GOLoop(parent=gosched, loop_type="outer")
    # Try and generate the code for this loop even though it
    # has no children
    with pytest.raises(GenerationError) as err:
        goloop.gen_code(None)
    assert "Cannot find the GOcean Kernel enclosed by this loop" \
        in str(err.value)


def test_goloop_bounds_invalid_iteration_space():
    ''' Check that the _upper/lower_bound() methods raise the expected error
    if the iteration space is not recognised. '''
    gosched = GOInvokeSchedule('name', [])
    gojloop = GOLoop(parent=gosched, loop_type="outer")
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


def test_goloop_validate_loop():
    ''' Tests that the GOLoop _validate_loop raises the appropriate errors when
    the Loop is not valid. '''

    # We need a parent in order to create the node, but then we detach it to
    # check that the validation works as expected.
    schedule = GOInvokeSchedule('name', [])
    goloop = GOLoop(loop_type="inner", parent=schedule)
    goloop.addchild(Literal("1", INTEGER_TYPE))
    goloop.addchild(Literal("1", INTEGER_TYPE))
    goloop.addchild(Literal("1", INTEGER_TYPE))
    goloop.addchild(Schedule())
    schedule.addchild(goloop)
    goloop.detach()

    # Test that an ancestor must be GOInvokeSchedule
    with pytest.raises(GenerationError) as err:
        goloop._validate_loop()
    assert ("Cannot find a GOInvokeSchedule ancestor for this GOLoop."
            in str(err.value))

    # Test that a child must be a GOKern
    schedule = GOInvokeSchedule('name', [])
    schedule.addchild(goloop.detach())
    with pytest.raises(GenerationError) as err:
        goloop._validate_loop()
    assert ("Cannot find the GOcean Kernel enclosed by this loop"
            in str(err.value))

    class GOKernMock(GOKern):
        ''' Mock class of GOKern for this test'''
        def __init__(self):
            ''' Overrided constructor to initialize it just as a
            PSyIR node'''
            # pylint: disable=super-init-not-called, non-parent-init-called
            Node.__init__(self)  # Ignore hierarchy constructors

    # Test Loop containing kernels with different offsets
    gokern1 = GOKernMock()
    gokern1._index_offset = "offset_se"
    gokern1._name = "kernel1"
    gokern2 = GOKernMock()
    gokern2._index_offset = "offset_sw"
    gokern2._name = "kernel2"
    goloop.loop_body.addchild(gokern1)
    goloop.loop_body.addchild(gokern2)
    with pytest.raises(GenerationError) as err:
        goloop._validate_loop()
    assert ("All Kernels must expect the same grid offset but kernel 'kernel2'"
            " has offset 'offset_sw' which does not match 'offset_se'."
            in str(err.value))


@pytest.fixture()
def clear_fparser():
    ''' The next test assumes that fparser has not been initialised.
    This is achieved by calling `_setup([])` with an empty list, which
    will remove all currently existing parser classes and functions.
    At the end of the tests re-initialse parser. This must be done in
    a fixture, since in case of a failure we still have to make sure
    that fparser gets properly re-initialised.
    '''

    # Remove all fparser classes and functions
    ParserFactory()._setup([])

    # Now execute all tests
    yield

    # We need to properly initialise fparser,
    # otherwise followup tests will fail (if this test should fail)
    ParserFactory().create(std="f2008")


@pytest.mark.usefixtures("clear_fparser")
def test_loop_bound_when_fparser_not_initialised():
    '''This reproduces #1272: a gocean custom loop boundary could
    not be parsed if the parser has not been initialised previously.
    Reproduce this bug by re-initialising fparser with an empty
    class list.
    '''
    GOLoop.add_bounds("go_offset_sw:go_ct:internal_we_halo:1:2:3:4")
