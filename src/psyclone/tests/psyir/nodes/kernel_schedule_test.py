# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Performs py.test tests on the KernelSchedule class. '''

from psyclone.psyir.nodes import Assignment, Reference, Literal, \
    KernelSchedule, Container
from psyclone.psyir.symbols import SymbolTable, DataSymbol, ScalarType, \
    RoutineSymbol
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import check_links


def test_kernelschedule_constructor():
    ''' Check that we can construct a KernelSchedule and that it has the
    expected properties. '''
    symbol = RoutineSymbol("timetable")
    ksched = KernelSchedule(symbol)
    assert ksched.name == "timetable"
    # A KernelSchedule does not represent a program
    assert not ksched.is_program
    # A KernelSchedule does not return anything
    assert ksched.return_symbol is None
    assert ksched.parent is None
    # Now create a KernelSchedule with a parent
    cnode = Container("BigBox")
    symbol = RoutineSymbol("plan")
    ksched2 = KernelSchedule(symbol, parent=cnode)
    assert ksched2.parent is cnode


def test_kernelschedule_str():
    ''' Check that the __str__ property correctly picks up the 'text_name'
    of the KernelSchedule. '''
    ksched = KernelSchedule.create("timetable")
    assert str(ksched) == ("KernelSchedule[name:'timetable']:\n"
                           "End KernelSchedule")


def test_kernelschedule_create():
    '''Test that the create method in the KernelSchedule class correctly
    creates a KernelSchedule instance.

    '''
    symbol_table = SymbolTable()
    symbol = DataSymbol("tmp", ScalarType.real_type())
    symbol_table.add(symbol)
    assignment = Assignment.create(Reference(symbol),
                                   Literal("0.0", ScalarType.real_type()))
    kschedule = KernelSchedule.create("mod_name", symbol_table, [assignment])
    assert isinstance(kschedule, KernelSchedule)
    # A KernelSchedule is not a main program and has no return value.
    assert not kschedule.is_program
    assert kschedule.return_symbol is None
    check_links(kschedule, [assignment])
    assert kschedule.symbol_table is symbol_table
    result = FortranWriter().routine_node(kschedule)
    assert result == (
        "subroutine mod_name()\n"
        "  real :: tmp\n\n"
        "  tmp = 0.0\n\n"
        "end subroutine mod_name\n")
