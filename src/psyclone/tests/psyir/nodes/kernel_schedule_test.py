# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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
# Modified I. Kavcic, Met Office
# -----------------------------------------------------------------------------

''' Performs py.test tests on the KernelSchedule class. '''

import pytest
from psyclone.errors import GenerationError
from psyclone.psyir.nodes import Assignment, Reference, Literal, KernelSchedule
from psyclone.psyir.symbols import SymbolTable, DataSymbol, INTEGER_TYPE, \
    REAL_TYPE
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import check_links


def test_kernelschedule_view(capsys):
    '''Test the view method of the KernelSchedule part.'''
    from psyclone.psyir.nodes.node import colored, SCHEDULE_COLOUR_MAP
    symbol_table = SymbolTable()
    symbol = DataSymbol("x", INTEGER_TYPE)
    symbol_table.add(symbol)
    lhs = Reference(symbol)
    rhs = Literal("1", INTEGER_TYPE)
    assignment = Assignment.create(lhs, rhs)
    kschedule = KernelSchedule.create("kname", symbol_table, [assignment])
    kschedule.view()
    coloredtext = colored("Schedule",
                          SCHEDULE_COLOUR_MAP["Schedule"])
    output, _ = capsys.readouterr()
    assert coloredtext+"[name:'kname']" in output
    assert "Assignment" in output  # Check child view method is called


def test_kernelschedule_can_be_printed():
    '''Test that a KernelSchedule instance can always be printed (i.e. is
    initialised fully)'''
    symbol = DataSymbol("x", INTEGER_TYPE)
    symbol_table = SymbolTable()
    symbol_table.add(symbol)
    lhs = Reference(symbol)
    rhs = Literal("1", INTEGER_TYPE)
    assignment = Assignment.create(lhs, rhs)
    kschedule = KernelSchedule.create("kname", symbol_table, [assignment])
    assert "Schedule[name:'kname']:\n" in str(kschedule)
    assert "Assignment" in str(kschedule)  # Check children are printed
    assert "End KernelSchedule" in str(kschedule)


def test_kernelschedule_create():
    '''Test that the create method in the KernelSchedule class correctly
    creates a KernelSchedule instance.

    '''
    symbol_table = SymbolTable()
    symbol = DataSymbol("tmp", REAL_TYPE)
    symbol_table.add(symbol)
    assignment = Assignment.create(Reference(symbol),
                                   Literal("0.0", REAL_TYPE))
    kschedule = KernelSchedule.create("mod_name", symbol_table, [assignment])
    check_links(kschedule, [assignment])
    assert kschedule.symbol_table is symbol_table
    result = FortranWriter().kernelschedule_node(kschedule)
    assert result == (
        "subroutine mod_name()\n"
        "  real :: tmp\n\n"
        "  tmp=0.0\n\n"
        "end subroutine mod_name\n")


def test_kernelschedule_create_invalid():
    '''Test that the create method in a KernelSchedule class raises the
    expected exception if the provided input is invalid.

    '''
    symbol_table = SymbolTable()
    symbol = DataSymbol("x", REAL_TYPE)
    symbol_table.add(symbol)
    children = [Assignment.create(Reference(symbol),
                                  Literal("1", REAL_TYPE))]

    # name is not a string.
    with pytest.raises(GenerationError) as excinfo:
        _ = KernelSchedule.create(1, symbol_table, children)
    assert ("name argument in create method of KernelSchedule class "
            "should be a string but found 'int'.") in str(excinfo.value)

    # symbol_table not a SymbolTable.
    with pytest.raises(GenerationError) as excinfo:
        _ = KernelSchedule.create("mod_name", "invalid", children)
    assert ("symbol_table argument in create method of KernelSchedule class "
            "should be a SymbolTable but found 'str'.") in str(excinfo.value)

    # children not a list.
    with pytest.raises(GenerationError) as excinfo:
        _ = KernelSchedule.create("mod_name", symbol_table, "invalid")
    assert ("children argument in create method of KernelSchedule class "
            "should be a list but found 'str'." in str(excinfo.value))

    # contents of children list are not Node.
    with pytest.raises(GenerationError) as excinfo:
        _ = KernelSchedule.create("mod_name", symbol_table, ["invalid"])
    assert (
        "child of children argument in create method of KernelSchedule class "
        "should be a PSyIR Node but found 'str'." in str(excinfo.value))
