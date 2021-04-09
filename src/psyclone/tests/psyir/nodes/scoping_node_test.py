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
# Authors S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests on the ScopingNode PSyIR node. '''

from __future__ import absolute_import
from psyclone.psyir.nodes import (Schedule, Assignment, Reference, Container,
                                  Routine, ArrayReference)
from psyclone.psyir.symbols import (DataSymbol, ArrayType, INTEGER_TYPE,
                                    ArgumentInterface)
from psyclone.psyir.backend.fortran import FortranWriter


def test_scoping_node_symbol_table():
    '''Test that ScopingNodes have a symbol_table property that returns its
    associated symbol table.'''
    # Since ScopingNode is abstract we will try this with a Container
    container = Container("test")
    assert container.symbol_table is container._symbol_table


def test_scoping_node_copy():
    ''' Test that the ScopingNode copy() method creates a new symbol table
    with copied symbols and updates the children references.'''

    # Since ScopingNode is abstract we will try this with a Schedule
    schedule = Schedule()
    symbol_a = schedule.symbol_table.new_symbol("a")
    symbol_b = schedule.symbol_table.new_symbol("b")

    schedule.addchild(
        Assignment.create(Reference(symbol_a), Reference(symbol_b)))

    new_schedule = schedule.copy()

    # Check that node generic copy() and _refine_copy() have been called
    # (e.g. children are not shallow copies and tree has been copied down
    # recursively)
    assert len(new_schedule.children) == 1
    assert new_schedule[0] is not schedule[0]
    assert new_schedule[0].lhs is not schedule[0].lhs
    assert new_schedule[0].rhs is not schedule[0].rhs

    # Check that the symbol_table has been deep copied
    assert new_schedule.symbol_table is not schedule.symbol_table
    assert new_schedule.symbol_table.lookup("a") is not \
        schedule.symbol_table.lookup("a")
    assert new_schedule.symbol_table.lookup("b") is not \
        schedule.symbol_table.lookup("b")

    # Check that the children references of the copied schedule point to
    # symbols in the new schedule's symbol table
    assert new_schedule[0].lhs.symbol not in schedule.symbol_table.symbols
    assert new_schedule[0].lhs.symbol in new_schedule.symbol_table.symbols
    assert new_schedule[0].rhs.symbol not in schedule.symbol_table.symbols
    assert new_schedule[0].rhs.symbol in new_schedule.symbol_table.symbols


def test_scoping_node_copy_hierarchy():
    ''' Test that the ScopingNode copy() method creates a new symbol table
    with copied symbols and updates the children references.

    This test has 2 ScopingNodes, and the copy will only be applied to the
    inner one. This means that the References to the symbols on the outer
    scope should not be duplicated. Also it contains argument symbols and
    a reference inside another reference to make sure all this are copied
    appropriately.
    '''
    parent_node = Container("module")
    symbol_b = parent_node.symbol_table.new_symbol(
        "b", symbol_type=DataSymbol, datatype=ArrayType(INTEGER_TYPE, [5]))
    schedule = Routine("routine")
    parent_node.addchild(schedule)
    symbol_a = schedule.symbol_table.new_symbol(
        "a", symbol_type=DataSymbol, datatype=INTEGER_TYPE,
        interface=ArgumentInterface(ArgumentInterface.Access.READWRITE))
    schedule.symbol_table.specify_argument_list([symbol_a])
    symbol_i = schedule.symbol_table.new_symbol(
        "i", symbol_type=DataSymbol, datatype=INTEGER_TYPE)

    schedule.addchild(
        Assignment.create(Reference(symbol_a),
                          ArrayReference.create(symbol_b,
                                                [Reference(symbol_i)])))

    new_schedule = schedule.copy()

    # Check that the symbol_table has been deep copied
    assert new_schedule.symbol_table is not schedule.symbol_table
    assert new_schedule.symbol_table.lookup("i") is not \
        schedule.symbol_table.lookup("i")
    assert new_schedule.symbol_table.lookup("a") is not \
        schedule.symbol_table.lookup("a")

    # Check that 'a' and 'i' have been copied to the new symbol table.
    assert new_schedule[0].lhs.symbol not in schedule.symbol_table.symbols
    assert new_schedule[0].lhs.symbol in new_schedule.symbol_table.symbols
    assert new_schedule[0].rhs.children[0].symbol not in \
        schedule.symbol_table.symbols
    assert new_schedule[0].rhs.children[0].symbol in \
        new_schedule.symbol_table.symbols

    # Add the "_new" suffix to all symbol in the copied schedule
    for symbol in new_schedule.symbol_table.symbols:
        new_schedule.symbol_table.rename_symbol(symbol, symbol.name+"_new")

    # An update to a symbol to the outer scope must affect both copies of the
    # inner schedule.
    parent_node.symbol_table.rename_symbol(symbol_b, symbol_b.name+"_global")

    # Insert the schedule back to the original container
    parent_node.addchild(new_schedule)

    # Check that the expected code is generated
    # TODO #1200: the new 'routine' RoutineSymbol also needs to change.
    expected = '''\
module module
  implicit none
  integer, dimension(5) :: b_global

  contains
  subroutine routine(a)
    integer, intent(inout) :: a
    integer :: i

    a = b_global(i)

  end subroutine routine
  subroutine routine(a_new)
    integer, intent(inout) :: a_new
    integer :: i_new

    a_new = b_global(i_new)

  end subroutine routine

end module module'''
    writer = FortranWriter()
    assert expected in writer(parent_node)
