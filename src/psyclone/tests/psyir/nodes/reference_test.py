# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' Performs py.test tests on the Reference PSyIR node. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.nodes import Reference, Array, Assignment, Literal
from psyclone.psyir.symbols import DataSymbol, DataType, SymbolError, \
    SymbolTable
from psyclone.psyGen import GenerationError, KernelSchedule
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import check_links


def test_reference_bad_init():
    '''Check that the __init__ method of the Reference class raises the
    expected exception if the symbol argument is not of the right
    type.

    '''
    with pytest.raises(TypeError) as excinfo:
        _ = Reference("hello")
    assert ("In Reference initialisation expecting a symbol but found 'str'."
            in str(excinfo.value))


def test_reference_node_str():
    ''' Check the node_str method of the Reference class.'''
    from psyclone.psyir.nodes.node import colored, SCHEDULE_COLOUR_MAP
    kschedule = KernelSchedule("kname")
    symbol = DataSymbol("rname", DataType.INTEGER)
    kschedule.symbol_table.add(symbol)
    assignment = Assignment(parent=kschedule)
    ref = Reference(symbol, assignment)
    coloredtext = colored("Reference", SCHEDULE_COLOUR_MAP["Reference"])
    assert coloredtext+"[name:'rname']" in ref.node_str()


def test_reference_can_be_printed():
    '''Test that a Reference instance can always be printed (i.e. is
    initialised fully)'''
    kschedule = KernelSchedule("kname")
    symbol = DataSymbol("rname", DataType.INTEGER)
    kschedule.symbol_table.add(symbol)
    assignment = Assignment(parent=kschedule)
    ref = Reference(symbol, assignment)
    assert "Reference[name:'rname']" in str(ref)


def test_reference_optional_parent():
    '''Test that the parent attribute is None if the optional parent
    argument is not supplied.

    '''
    ref = Reference(DataSymbol("rname", DataType.REAL))
    assert ref.parent is None


# Test Array class


def test_array_node_str():
    ''' Check the node_str method of the Array class.'''
    from psyclone.psyir.nodes.node import colored, SCHEDULE_COLOUR_MAP
    kschedule = KernelSchedule("kname")
    symbol = DataSymbol("aname", DataType.INTEGER,
                        [DataSymbol.Extent.ATTRIBUTE])
    kschedule.symbol_table.add(symbol)
    assignment = Assignment(parent=kschedule)
    array = Array(symbol, parent=assignment)
    coloredtext = colored("ArrayReference", SCHEDULE_COLOUR_MAP["Reference"])
    assert coloredtext+"[name:'aname']" in array.node_str()


def test_array_can_be_printed():
    '''Test that an Array instance can always be printed (i.e. is
    initialised fully)'''
    kschedule = KernelSchedule("kname")
    symbol = DataSymbol("aname", DataType.INTEGER)
    kschedule.symbol_table.add(symbol)
    assignment = Assignment(parent=kschedule)
    array = Array(symbol, assignment)
    assert "ArrayReference[name:'aname']\n" in str(array)


def test_array_create():
    '''Test that the create method in the Array class correctly
    creates an Array instance.

    '''
    symbol_temp = DataSymbol("temp", DataType.REAL, shape=[10, 10, 10])
    symbol_i = DataSymbol("i", DataType.INTEGER)
    symbol_j = DataSymbol("j", DataType.INTEGER)
    children = [Reference(symbol_i), Reference(symbol_j),
                Literal("1", DataType.INTEGER)]
    array = Array.create(symbol_temp, children)
    check_links(array, children)
    result = FortranWriter().array_node(array)
    assert result == "temp(i,j,1)"


def test_array_create_invalid1():
    '''Test that the create method in the Array class raises an exception
    if the provided symbol is not an array.

    '''
    symbol_i = DataSymbol("i", DataType.INTEGER)
    symbol_j = DataSymbol("j", DataType.INTEGER)
    symbol_temp = DataSymbol("temp", DataType.REAL)
    children = [Reference(symbol_i), Reference(symbol_j),
                Literal("1", DataType.INTEGER)]
    with pytest.raises(GenerationError) as excinfo:
        _ = Array.create(symbol_temp, children)
    assert ("expecting the symbol to be an array, not a scalar."
            in str(excinfo.value))


def test_array_create_invalid2():
    '''Test that the create method in the Array class raises an exception
    if the number of dimension in the provided symbol is different to
    the number of indices provided to the create method.

    '''
    symbol_temp = DataSymbol("temp", DataType.REAL, shape=[10])
    symbol_i = DataSymbol("i", DataType.INTEGER)
    symbol_j = DataSymbol("j", DataType.INTEGER)
    children = [Reference(symbol_i), Reference(symbol_j),
                Literal("1", DataType.INTEGER)]
    with pytest.raises(GenerationError) as excinfo:
        _ = Array.create(symbol_temp, children)
    assert ("the symbol should have the same number of dimensions as indices "
            "(provided in the 'children' argument). Expecting '3' but found "
            "'1'." in str(excinfo.value))


def test_array_create_invalid3():
    '''Test that the create method in an Array class raises the expected
    exception if the provided input is invalid.

    '''
    # symbol argument is not a DataSymbol
    with pytest.raises(GenerationError) as excinfo:
        _ = Array.create([], [])
    assert ("symbol argument in create method of Array class should "
            "be a DataSymbol but found 'list'."
            in str(excinfo.value))

    # children not a list
    with pytest.raises(GenerationError) as excinfo:
        _ = Array.create(DataSymbol("temp", DataType.REAL), "invalid")
    assert ("children argument in create method of Array class should "
            "be a list but found 'str'." in str(excinfo.value))

    # contents of children list are not Node
    with pytest.raises(GenerationError) as excinfo:
        _ = Array.create(DataSymbol("temp", DataType.REAL),
                         [Reference(DataSymbol("i", DataType.INTEGER)),
                          "invalid"])
    assert (
        "child of children argument in create method of Array class "
        "should be a PSyIR Node but found 'str'." in str(excinfo.value))


def test_reference_check_declared():
    '''Test that the check_declared method in the Reference class raises
    an exception if the associated symbol does not exist in a symbol
    table associated with an ancestor node. Note, this exception is
    only raised if a symbol table exists. If one does not exist then
    it silently returns (to support the NEMO API which does not have
    symbol tables see issue #500).

    '''
    ref = Reference(DataSymbol("rname", DataType.REAL))
    assignment = Assignment.create(ref, Literal("0.0", DataType.REAL))
    _ = KernelSchedule.create("test", SymbolTable(),
                              [assignment])
    with pytest.raises(SymbolError) as excinfo:
        ref.check_declared()
    assert "Undeclared reference 'rname' found." in str(excinfo.value)
