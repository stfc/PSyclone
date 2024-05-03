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
# Authors A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified by J. Henrichs, Bureau of Meteorology
# Modified R. W. Ford, STFC Daresbury Lab
# Modified I. Kavcic, Met Office

'''Tests for the GOLoop class.'''

import pytest

from fparser.two.parser import ParserFactory

from psyclone.domain.gocean import GOceanConstants
from psyclone.errors import GenerationError, InternalError
from psyclone.gocean1p0 import GOKern, GOLoop, GOInvokeSchedule
from psyclone.psyir.nodes import (Schedule, Reference, StructureReference,
                                  Node, Literal)
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE
from psyclone.psyir.tools import DependencyTools
from psyclone.psyir.tools.dependency_tools import DTCode
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


def test_goloop_create(monkeypatch):
    ''' Test that the GOLoop create method populates the relevant attributes
    and creates the loop children. '''

    # Monkeypatch the called GOLoops methods as this will be tested separately
    monkeypatch.setattr(GOLoop, "lower_bound",
                        lambda x: Literal("10", INTEGER_TYPE))
    monkeypatch.setattr(GOLoop, "upper_bound",
                        lambda x: Literal("20", INTEGER_TYPE))

    # Call the create method
    gosched = GOInvokeSchedule('name', [])
    goloop = GOLoop.create(parent=gosched,
                           loop_type="inner",
                           field_name="cv_fld",
                           iteration_space="go_internal_pts",
                           field_space="go_cv")

    # Check the properties
    assert isinstance(goloop, GOLoop)
    assert goloop.loop_type == "inner"
    assert goloop.field_name == "cv_fld"
    assert goloop.iteration_space == "go_internal_pts"
    assert goloop.field_space == "go_cv"

    # Check that the created children correspond to the expected values
    assert len(goloop.children) == 4
    assert isinstance(goloop.children[0], Literal)
    assert isinstance(goloop.children[1], Literal)
    assert isinstance(goloop.children[2], Literal)
    assert isinstance(goloop.children[3], Schedule)
    assert goloop.children[0].value == '10'
    assert goloop.children[1].value == '20'
    assert goloop.children[2].value == '1'

    # Try with an invalid loop type
    with pytest.raises(TypeError) as err:
        goloop = GOLoop.create(parent=gosched,
                               loop_type="invalid",
                               field_name="cv_fld",
                               iteration_space="go_internal_pts",
                               field_space="go_cv")
    assert ("Error, loop_type value (invalid) is invalid. Must be one of "
            "['inner', 'outer']." in str(err.value))

    # Now trigger the second test: it must be a valid loop type according
    # to VALID_LOOP_TYPES, but not 'inner' or 'outer', the only values
    # the code can actually handle. So monkeypatch VALID_LOOP_TYPES:
    monkeypatch.setattr(GOceanConstants, "VALID_LOOP_TYPES",
                        ["inner", "outer", "other"])
    with pytest.raises(InternalError) as err:
        goloop = GOLoop.create(parent=gosched,
                               loop_type="other",
                               field_name="cv_fld",
                               iteration_space="go_internal_pts",
                               field_space="go_cv")
    assert ("While the loop type 'other' is valid, it is not yet supported."
            in str(err.value))


def test_goloop_properties_getters_and_setters():
    ''' Test that the GOLoop getters and setters, retrieve and set the
    expected attributes. '''
    gosched = GOInvokeSchedule('name', [])
    goloop = GOLoop(loop_type="inner", parent=gosched)

    # Set and get iteration_space
    goloop.iteration_space = "it_space"
    assert goloop.iteration_space == "it_space"

    # Provide an incorrect iteration_space
    with pytest.raises(TypeError) as err:
        goloop.iteration_space = 3
    assert ("Iteration space must be a 'str' but found 'int' instead."
            in str(err.value))

    # Set and get iteration_space
    goloop.field_space = "go_cv"
    assert goloop.field_space == "go_cv"

    # Provide an incorrect iteration_space
    with pytest.raises(TypeError) as err:
        goloop.field_space = 3
    assert ("Field space must be a 'str' but found 'int' instead."
            in str(err.value))

    with pytest.raises(ValueError) as err:
        goloop.field_space = "invalid"
    assert ("Invalid string 'invalid' provided for a GOcean field_space. The "
            "valid values are ['go_cu', 'go_cv', 'go_ct', 'go_cf', 'go_every'"
            ", '']" in str(err.value))

    # Get bounds map
    assert goloop.bounds_lookup == GOLoop._bounds_lookup


def test_goloop_get_custom_bound_string_invalid_loop_type():
    ''' Check that the get_custom_bound_string method raises the expected
    error if the loop_type has an invalid value. '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           API, idx=0)
    schedule = invoke.schedule
    loop = schedule.walk(GOLoop)[0]

    # Set the loop_type to something invalid
    loop._loop_type = "broken"  # Bypass setter validation
    with pytest.raises(GenerationError) as err:
        loop.get_custom_bound_string("start")
    assert ("Invalid loop type of 'broken'. Expected one of ['inner', 'outer']"
            in str(err.value))


def test_goloop_bounds_invalid_iteration_space():
    ''' Check that the _upper/lower_bound() methods raise the expected error
    if the iteration space is not recognised. '''
    gosched = GOInvokeSchedule('name', [])
    gojloop = GOLoop(parent=gosched, loop_type="outer")

    # Set the iteration space to something invalid
    gojloop.iteration_space = "broken"
    with pytest.raises(GenerationError) as err:
        gojloop.upper_bound()
    assert ("Cannot generate custom loop bound for loop GOLoop["
            "variable:'j', loop_type:'outer']\nEnd GOLoop. Couldn't find "
            "any suitable field." in str(err.value))

    # Create an complete invoke now
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           API, idx=0)
    schedule = invoke.schedule
    gojloop = schedule.children[0]
    # Set the iteration space to something invalid
    with pytest.raises(GenerationError) as err:
        # The setter already calls the upper/lower_bound methods
        gojloop.iteration_space = "broken"
    assert ("Cannot generate custom loop bound for a loop with an index-offset"
            " of 'go_offset_ne', a field-space of 'go_cv', an iteration-space "
            "of 'broken' and a loop-type of 'outer', for the side 'start' "
            "because this keys combination does not exist in the "
            "GOLoop.bounds_lookup table." in str(err.value))


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

    # Test that an ancestor must be GOInvokeSchedule
    goloop._parent = None  # Remove parent pointer set in the constructor
    with pytest.raises(GenerationError) as err:
        goloop._validate_loop()
    assert ("Cannot find a GOInvokeSchedule ancestor for this GOLoop."
            in str(err.value))

    # Test that a child must be a GOKern
    schedule.addchild(goloop)
    with pytest.raises(GenerationError) as err:
        goloop._validate_loop()
    assert ("Cannot find the GOcean Kernel enclosed by this loop"
            in str(err.value))

    class GOKernMock(GOKern):
        ''' Mock class of GOKern for this test'''
        def __init__(self):
            ''' Overridden constructor to initialize it just as a
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


def test_independent_iterations(monkeypatch):
    '''Test the independent_iterations method of GOLoop.'''
    schedule = GOInvokeSchedule('name', [])
    goloop = GOLoop(loop_type="inner", parent=schedule)
    assert goloop.independent_iterations()

    # Test that we can supply our own instance of DependencyTools. We do this
    # by monkeypatching the can_loop_be_parallelised() method so that it adds
    # a message.

    def fake1(deptools, _2, test_all_variables=False,
              signatures_to_ignore=None):
        # pylint: disable=unused-argument
        deptools._add_message("just a test", DTCode.WARN_SCALAR_WRITTEN_ONCE)
        return True

    monkeypatch.setattr(DependencyTools, "can_loop_be_parallelised", fake1)
    dtools = DependencyTools()
    assert goloop.independent_iterations(dep_tools=dtools)
    assert dtools.get_all_messages()[0].code == DTCode.WARN_SCALAR_WRITTEN_ONCE

    # Test when the DA raises an exception (typically because of missing
    # variables in the PSyIR - TODO #845) that this is handled by the
    # independent_iterations method.

    def fake2(_1, _2, test_all_variables=False, signatures_to_ignore=None):
        # pylint: disable=unused-argument
        raise InternalError("This is just a test")
    monkeypatch.setattr(DependencyTools, "can_loop_be_parallelised", fake2)
    assert goloop.independent_iterations()
