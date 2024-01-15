# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#          I. Kavcic, Met Office
# -----------------------------------------------------------------------------

''' Performs py.test tests on the KernelSchedule class. '''

from __future__ import absolute_import
from psyclone.psyir.nodes import Assignment, Reference, Literal, \
    KernelSchedule, Container
from psyclone.psyir.symbols import SymbolTable, DataSymbol, REAL_TYPE
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import check_links


def test_kernelschedule_constructor():
    ''' Check that we can construct a KernelSchedule and that it has the
    expected properties. '''
    ksched = KernelSchedule("timetable")
    assert ksched.name == "timetable"
    # A KernelSchedule does not represent a program
    assert not ksched.is_program
    # A KernelSchedule does not return anything
    assert ksched.return_symbol is None
    assert ksched.parent is None
    # Now create a KernelSchedule with a parent
    cnode = Container("BigBox")
    ksched2 = KernelSchedule("plan", parent=cnode)
    assert ksched2.parent is cnode


def test_kernelschedule_str():
    ''' Check that the __str__ property correctly picks up the 'text_name'
    of the KernelSchedule. '''
    ksched = KernelSchedule("timetable")
    assert str(ksched) == ("KernelSchedule[name:'timetable']:\n"
                           "End KernelSchedule")


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
