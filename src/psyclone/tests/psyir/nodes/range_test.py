# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council
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
# Author: A. R. Porter, STFC Daresbury Lab
# Modified by R. W. Ford, S. Siso and N. Nobre, STFC Daresbury Lab

''' pytest tests for the Range class. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.symbols import ScalarType, DataSymbol, \
    INTEGER_SINGLE_TYPE, REAL_SINGLE_TYPE
from psyclone.psyir.nodes import Range, Literal, Reference
from psyclone.errors import InternalError, GenerationError


@pytest.mark.parametrize("prop", ["start", "stop", "step"])
def test_range_getter_errors(prop):
    ''' Test that attempting to get any of the start/stop/step properties for
    an incomplete Range object raises the expected error. '''
    # pylint:disable=eval-used
    erange = Range()
    # Manually break the list of children
    erange.children = []
    with pytest.raises(InternalError) as err:
        _ = eval("erange." + prop)
    assert ("Malformed Range: should have three children but found 0"
            in str(err.value))


def test_range_init(parser):
    ''' Check that the Range constructor behaves as intended. '''
    from fparser.common.readfortran import FortranStringReader
    # When no arguments are provided
    erange = Range()
    assert not erange.children  # Children list is empty
    assert erange.parent is None
    assert erange.annotations == []
    assert erange.ast is None
    # When arguments are provided
    reader = FortranStringReader("program hello\nend program hello\n")
    prog = parser(reader)
    erange2 = Range(ast=prog)
    assert erange2.ast is prog
    with pytest.raises(InternalError) as err:
        _ = Range(annotations=["was-where"])
    assert "unrecognised annotation 'was-where'" in str(err.value)


def test_range_create():
    ''' Check that the Range.create() method behaves as intended. '''
    start = Literal("1", INTEGER_SINGLE_TYPE)
    stop = Literal("10", INTEGER_SINGLE_TYPE)
    # No step
    erange = Range.create(start, stop)
    assert erange.children[0] is start
    assert erange.children[1] is stop
    assert erange.children[2].value == "1"
    # Step supplied
    erange3 = Range.create(start.copy(), stop.copy(),
                           Literal("5", INTEGER_SINGLE_TYPE),)
    assert erange3.children[2].value == "5"


@pytest.mark.parametrize("prop", ["start", "stop", "step"])
def test_range_setter_errors(prop):
    ''' Check that the various setters reject values that are not sub-classes
    of Node. '''
    # We use exec() so that we can use the pytest parameterisation of the
    # various properties we want to test
    # pylint:disable=exec-used,unused-variable
    erange = Range()
    with pytest.raises(TypeError) as err:
        exec("erange." + prop + " = 1")
    assert "must be a sub-class of Node but got" in str(err.value)
    val = Literal("1.0", REAL_SINGLE_TYPE)
    with pytest.raises(TypeError) as err:
        exec("erange." + prop + " = val")
    assert ("value of a Range is a Literal then it must be of type "
            "INTEGER but got Scalar<REAL, SINGLE>" in str(err.value))


def test_range_literals_props():
    ''' Test that the properties of a Range return what we expect
    when the start, stop and step are Literals. '''
    start = Literal("10", INTEGER_SINGLE_TYPE)
    stop = Literal("20", INTEGER_SINGLE_TYPE)
    erange = Range.create(start, stop)
    assert erange.children[0] is start
    assert erange.children[1] is stop
    # We didn't supply an increment so check that one was created
    assert isinstance(erange.children[2], Literal)
    assert (erange.children[2].datatype.intrinsic ==
            ScalarType.Intrinsic.INTEGER)
    assert (erange.children[2].datatype.precision ==
            ScalarType.Precision.UNDEFINED)
    assert erange.children[2].value == "1"
    # Create another one with a specified step
    start = Literal("10", INTEGER_SINGLE_TYPE)
    stop = Literal("20", INTEGER_SINGLE_TYPE)
    erange2 = Range.create(start, stop, Literal("5", INTEGER_SINGLE_TYPE))
    assert erange2.children[0] is start
    assert erange2.children[1] is stop
    assert (erange2.children[2].datatype.precision ==
            ScalarType.Precision.SINGLE)
    assert erange2.step.value == "5"


def test_range_references_props():
    ''' Test that the properties of a Range return what we expect
    when the start, stop and step are references or expressions. '''
    from psyclone.psyir.nodes import BinaryOperation, KernelSchedule
    sched = KernelSchedule("test_sched")
    sym_table = sched.symbol_table
    start_symbol = DataSymbol("istart", INTEGER_SINGLE_TYPE)
    stop_symbol = DataSymbol("istop", INTEGER_SINGLE_TYPE)
    step_symbol = DataSymbol("istep", INTEGER_SINGLE_TYPE)
    sym_table.add(start_symbol)
    sym_table.add(stop_symbol)
    sym_table.add(step_symbol)
    startvar = Reference(start_symbol)
    stopvar = Reference(stop_symbol)
    start = BinaryOperation.create(BinaryOperation.Operator.SUB,
                                   startvar, Literal("1", INTEGER_SINGLE_TYPE))
    stop = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                  stopvar, Literal("1", INTEGER_SINGLE_TYPE))
    step = Reference(step_symbol)
    erange = Range.create(start, stop, step)
    assert erange.start is start
    assert erange.stop is stop
    assert erange.step is step
    assert erange.children[0] is start
    assert erange.children[1] is stop
    assert erange.children[2] is step


def test_range_out_of_order_setter():
    ''' Test that setting the start/stop/step props out of order raises the
    expected error. '''
    erange = Range()
    datanode1 = Literal("1", INTEGER_SINGLE_TYPE)
    datanode2 = Literal("2", INTEGER_SINGLE_TYPE)
    datanode3 = Literal("3", INTEGER_SINGLE_TYPE)

    # Stop before Start
    with pytest.raises(IndexError) as excinfo:
        erange.stop = datanode2
    assert ("The Stop value 'Literal[value:'2', Scalar<INTEGER, SINGLE>]' can"
            " not be inserted into range 'Range[]' before the Start value is "
            "provided." in str(excinfo.value))
    # Once start is added, setting it up again just replaces it
    erange.start = datanode1
    erange.start = datanode1.copy()
    assert len(erange.children) == 1
    # Now Stop can be accepted
    erange.stop = datanode2
    # Once added, setting it up again just replaces it
    erange.stop = datanode2.copy()
    assert len(erange.children) == 2

    # Step before Step
    del erange.children[1]
    with pytest.raises(IndexError) as excinfo:
        erange.step = datanode3
    assert ("The Step value 'Literal[value:'3', Scalar<INTEGER, SINGLE>]' can"
            " not be inserted into range 'Range[]' before the Start and Stop "
            "values are provided." in str(excinfo.value))
    erange.stop = datanode2
    erange.step = datanode3
    # Once added, setting it up again just replaces it
    erange.step = datanode3.copy()
    assert len(erange.children) == 3


def test_range_str():
    ''' Check that node_str and str work correctly for a Range node. '''
    erange = Range()
    assert 'Range[]' in erange.node_str(colour=False)
    assert 'Range' in erange.node_str(colour=True)
    assert erange.node_str(colour=False) == str(erange)


def test_range_children_validation():
    '''Test that children added to Range are validated. Range accepts
    3 DataNodes.

    '''
    erange = Range()
    datanode1 = Literal("1", INTEGER_SINGLE_TYPE)
    datanode2 = Literal("2", INTEGER_SINGLE_TYPE)
    datanode3 = Literal("3", INTEGER_SINGLE_TYPE)
    range2 = Range()

    # First child
    with pytest.raises(GenerationError) as excinfo:
        erange.addchild(range2)
    assert ("Item 'Range' can't be child 0 of 'Range'. The valid format is: "
            "'DataNode, DataNode, DataNode'." in str(excinfo.value))
    erange.addchild(datanode1)

    # Second child
    with pytest.raises(GenerationError) as excinfo:
        erange.addchild(range2)
    assert ("Item 'Range' can't be child 1 of 'Range'. The valid format is: "
            "'DataNode, DataNode, DataNode'." in str(excinfo.value))
    erange.addchild(datanode2)

    # Third child
    with pytest.raises(GenerationError) as excinfo:
        erange.addchild(range2)
    assert ("Item 'Range' can't be child 2 of 'Range'. The valid format is: "
            "'DataNode, DataNode, DataNode'." in str(excinfo.value))
    erange.addchild(datanode3)

    # Additional children
    with pytest.raises(GenerationError) as excinfo:
        erange.addchild(datanode1)
    assert ("Item 'Literal' can't be child 3 of 'Range'. The valid format is:"
            " 'DataNode, DataNode, DataNode'." in str(excinfo.value))
