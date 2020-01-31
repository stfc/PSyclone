# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council
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

''' pytest tests for the Range class. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.symbols import DataType, DataSymbol
from psyclone.psyir.nodes import Range, Literal, Reference, Node
from psyclone.psyGen import InternalError


@pytest.mark.parametrize("prop", ["start", "stop", "step"])
def test_range_getter_errors(prop):
    ''' Test that attempting to get any of the start/stop/step properties for
    an invalid Range object raises the expected errors. '''
    # pylint:disable=eval-used
    erange = Range()
    # Manually break the list of children
    erange._children = []
    with pytest.raises(InternalError) as err:
        _ = eval("erange." + prop)
    assert ("Malformed Range: should have three children but found 0"
            in str(err.value))

    # Correct number of children but wrong type (as initialised to None)
    erange = Range()
    with pytest.raises(InternalError) as err:
        _ = eval("erange." + prop)
    assert ("Malformed Range: all children must be sub-classes of "
            "Node" in str(err.value))


def test_range_init(parser):
    ''' Check that the Range constructor behaves as intended. '''
    from fparser.common.readfortran import FortranStringReader
    # When no arguments are provided
    erange = Range()
    assert erange.children == [None, None, None]
    assert erange.parent is None
    assert erange.annotations == []
    assert erange.ast is None
    # When arguments are provided
    parent = Node()
    reader = FortranStringReader("program hello\nend program hello\n")
    prog = parser(reader)
    erange2 = Range(parse_node=prog, parent=parent)
    assert erange2.parent is parent
    assert erange2.ast is prog
    with pytest.raises(InternalError) as err:
        _ = Range(annotations=["was-where"])
    assert "unrecognised annotation 'was-where'" in str(err.value)


def test_range_create():
    ''' Check that the Range.create() method behaves as intended. '''
    parent = Node()
    start = Literal("1", DataType.INTEGER)
    stop = Literal("10", DataType.INTEGER)
    # No parent and no step
    erange = Range.create(start, stop)
    assert erange.children[0] is start
    assert erange.children[1] is stop
    assert erange.parent is None
    # Parent but no step
    erange2 = Range.create(start, stop, parent=parent)
    assert erange2.parent is parent
    assert erange2.children[2].value == "1"
    # Parent and step supplied
    erange3 = Range.create(start, stop, step=Literal("5", DataType.INTEGER),
                           parent=parent)
    assert erange3.parent is parent
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
    val = Literal("1.0", DataType.REAL)
    with pytest.raises(TypeError) as err:
        exec("erange." + prop + " = val")
    assert ("value of a Range is a Literal then it must be of type "
            "INTEGER but got DataType.REAL" in str(err.value))


def test_range_literals_props():
    ''' Test that the properties of a Range return what we expect
    when the start, stop and step are Literals. '''
    start = Literal("10", DataType.INTEGER)
    stop = Literal("20", DataType.INTEGER)
    erange = Range.create(start, stop)
    assert erange.children[0] is start
    assert erange.children[1] is stop
    # We didn't supply an increment so check that one was created
    assert isinstance(erange.children[2], Literal)
    assert erange.children[2].datatype == DataType.INTEGER
    assert erange.children[2].value == "1"
    # Create another one with a specified step
    erange2 = Range.create(start, stop, Literal("5", DataType.INTEGER))
    assert erange2.children[0] is start
    assert erange2.children[1] is stop
    assert erange2.step.value == "5"


def test_range_references_props():
    ''' Test that the properties of a Range return what we expect
    when the start, stop and step are references or expressions. '''
    from psyclone.psyGen import KernelSchedule
    from psyclone.psyir.nodes import BinaryOperation
    sched = KernelSchedule("test_sched")
    sym_table = sched.symbol_table
    start_symbol = DataSymbol("istart", DataType.INTEGER)
    stop_symbol = DataSymbol("istop", DataType.INTEGER)
    step_symbol = DataSymbol("istep", DataType.INTEGER)
    sym_table.add(start_symbol)
    sym_table.add(stop_symbol)
    sym_table.add(step_symbol)
    startvar = Reference(start_symbol)
    stopvar = Reference(stop_symbol)
    start = BinaryOperation.create(BinaryOperation.Operator.SUB,
                                   startvar, Literal("1", DataType.INTEGER))
    stop = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                  stopvar, Literal("1", DataType.INTEGER))
    step = Reference(step_symbol)
    erange = Range.create(start, stop, step)
    assert erange.start is start
    assert erange.stop is stop
    assert erange.step is step
    assert erange.children[0] is start
    assert erange.children[1] is stop
    assert erange.children[2] is step


def test_range_str():
    ''' Check that node_str and str work correctly for a Range node. '''
    erange = Range()
    with pytest.raises(InternalError) as err:
        str(erange)
    assert "Malformed Range: all children must be sub-" in str(err.value)
    erange2 = Range.create(Literal("1", DataType.INTEGER),
                           Literal("10", DataType.INTEGER))
    assert 'Range[]' in erange2.node_str(colour=False)
    assert 'Range' in erange2.node_str(colour=True)
    assert erange2.node_str(colour=False) == str(erange2)


def test_range_view(capsys):
    ''' Check that calling view() on an array with a child Range works
    as expected. '''
    from psyclone.psyir.nodes import Array
    from psyclone.psyir.nodes.node import colored, SCHEDULE_COLOUR_MAP
    # Create the PSyIR for 'my_array(1, 1:10)'
    erange = Range.create(Literal("1", DataType.INTEGER),
                          Literal("10", DataType.INTEGER))
    array = Array.create(DataSymbol("my_array", DataType.REAL, [10, 10]),
                         [Literal("1", DataType.INTEGER),
                          erange])
    array.view()
    stdout, _ = capsys.readouterr()
    arrayref = colored("ArrayReference",
                       SCHEDULE_COLOUR_MAP[array._colour_key])
    literal = colored("Literal",
                      SCHEDULE_COLOUR_MAP[array.children[0]._colour_key])
    rangestr = colored("Range", SCHEDULE_COLOUR_MAP[erange._colour_key])
    indent = "    "
    assert (arrayref + "[name:'my_array']\n" +
            indent + literal + "[value:'1', DataType.INTEGER]\n" +
            indent + rangestr + "[]\n" +
            2*indent + literal + "[value:'1', DataType.INTEGER]\n" +
            2*indent + literal + "[value:'10', DataType.INTEGER]\n" +
            2*indent + literal + "[value:'1', DataType.INTEGER]\n" in stdout)
