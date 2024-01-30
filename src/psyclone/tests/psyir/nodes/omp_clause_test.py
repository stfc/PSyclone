# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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
# Author: A. B. G. Chalk, STFC Daresbury Lab
# Modified: S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests on the OpenMP PSyIR Clause nodes. '''

import pytest
from psyclone.psyir.nodes.node import colored
from psyclone.psyir.nodes.omp_clauses import OMPGrainsizeClause, \
    OMPNowaitClause, OMPNogroupClause, OMPNumTasksClause, OMPSharedClause, \
    OMPDependClause, OMPPrivateClause, OMPFirstprivateClause, \
    OMPDefaultClause, OMPScheduleClause
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE


def test_nowait_clause():
    ''' Test the OMPNowaitClause functionality. '''
    nowait = OMPNowaitClause()
    assert nowait.clause_string == "nowait"
    assert OMPNowaitClause._validate_child(0, nowait) is False


def test_grainsize_clause():
    ''' Test the OMPGrainsizeClause functionality. '''

    nowait = OMPGrainsizeClause(children=[Literal("32", INTEGER_TYPE)])
    assert nowait.clause_string == "grainsize"
    assert OMPGrainsizeClause._validate_child(0, Literal("64", INTEGER_TYPE))\
        is True
    assert OMPGrainsizeClause._validate_child(1, Literal("64", INTEGER_TYPE))\
        is False


def test_numtasks_clause():
    ''' Test the OMPNumTasksClause functionality. '''
    nowait = OMPNumTasksClause(children=[Literal("32", INTEGER_TYPE)])
    assert nowait.clause_string == "num_tasks"
    assert OMPNumTasksClause._validate_child(0, Literal("64", INTEGER_TYPE))\
        is True
    assert OMPNumTasksClause._validate_child(1, Literal("64", INTEGER_TYPE))\
        is False


def test_nogroup_clause():
    ''' Test the OMPNogroupClause functionality. '''
    nowait = OMPNogroupClause()
    assert nowait.clause_string == "nogroup"
    assert OMPNogroupClause._validate_child(0, nowait) is False


def test_schedule_clause():
    ''' Test the OMPScheduleClause functionality. '''
    sched = OMPScheduleClause()
    assert sched._schedule == "none"
    assert sched._clause_string == ""
    sched.schedule = "auto"
    assert sched._clause_string == "schedule(auto)"
    sched2 = OMPScheduleClause()
    assert sched != sched2
    sched2.schedule = "auto"
    assert sched == sched2
    coloredtext = colored("OMPScheduleClause", OMPScheduleClause._colour)
    assert coloredtext+"[schedule=auto]" in sched.node_str()
    sched.schedule = "none"
    assert coloredtext+"[schedule=none]" in sched.node_str()


def test_invalid_schedule_clause():
    ''' Test that OMPScheduleClause throws an error when given an invalid
    schedule.'''
    sched = OMPScheduleClause()
    with pytest.raises(ValueError) as excinfo:
        sched.schedule = "test"
    assert ("Schedule must be one of ['runtime', 'static', 'dynamic', "
            "'guided', 'auto', 'none']. Found 'test'." in str(excinfo.value))


def test_default_clause():
    ''' Test the OMPDefaultClause functionality. '''
    default = OMPDefaultClause()
    assert default._clause_string == "default(shared)"
    assert default.clause_type == OMPDefaultClause.DefaultClauseTypes.SHARED
    default = OMPDefaultClause(
            clause_type=OMPDefaultClause.DefaultClauseTypes.NONE)
    assert default._clause_string == "default(none)"
    assert default.clause_type == OMPDefaultClause.DefaultClauseTypes.NONE
    default = OMPDefaultClause(
            clause_type=OMPDefaultClause.DefaultClauseTypes.FIRSTPRIVATE)
    assert default._clause_string == "default(firstprivate)"
    assert (default.clause_type ==
            OMPDefaultClause.DefaultClauseTypes.FIRSTPRIVATE)

    with pytest.raises(TypeError) as excinfo:
        OMPDefaultClause(clause_type="String")
    assert ("OMPDefaultClause expected 'clause_type' argument of type "
            "OMPDefaultClause.DefaultClauseTypes but found 'str'" in
            str(excinfo.value))


def test_shared_clause():
    ''' Test the OMPSharedClause functionality. '''
    shared = OMPSharedClause()
    assert shared.clause_string == ""
    tmp = DataSymbol("tmp", INTEGER_TYPE)
    ref1 = Reference(tmp)
    shared.addchild(ref1)
    assert shared.clause_string == "shared"


@pytest.mark.parametrize("testclass, string",
                         [(OMPPrivateClause, "private"),
                          (OMPFirstprivateClause, "firstprivate")])
def test_private_and_firstprivate_clause(testclass, string):
    ''' Test the OMPPrivateClause and OMPFirstprivateClause functionality. '''
    private = testclass()
    assert private.clause_string == ""
    tmp = DataSymbol("tmp", INTEGER_TYPE)
    ref1 = Reference(tmp)
    private.addchild(ref1)
    assert private.clause_string == string


@pytest.mark.parametrize("testclass, string",
                         [(OMPPrivateClause, "OMPPrivateClause"),
                          (OMPFirstprivateClause, "OMPFirstprivateClause")])
def test_private_clause_create(testclass, string):
    ''' Test that OMPPrivateClause and OMPFirstprivateClause create methods
    accept a list of symbols and add children with references containing
    those symbols. '''

    symbol1 = DataSymbol("a", INTEGER_TYPE)
    symbol2 = DataSymbol("b", INTEGER_TYPE)
    symbol3 = DataSymbol("c", INTEGER_TYPE)

    # Check that it just accepts a list of symbols
    with pytest.raises(TypeError) as err:
        testclass.create(symbol1)
    assert (f"{string} expected the 'symbols' argument to be a list, "
            f"but found 'DataSymbol' instead." in str(err.value))

    with pytest.raises(TypeError) as err:
        testclass.create([symbol1, 4, symbol2])
    assert (f"{string} expected all the items in the 'symbols' list to "
            f"be PSyIR Symbols, but found a 'int'." in str(err.value))

    # Check a working create method
    new_clause = testclass.create([symbol1, symbol2, symbol3])
    assert isinstance(new_clause, testclass)
    assert len(new_clause.children) == 3
    assert isinstance(new_clause.children[0], Reference)
    assert isinstance(new_clause.children[1], Reference)
    assert isinstance(new_clause.children[2], Reference)
    assert new_clause.children[0].symbol.name == "a"
    assert new_clause.children[1].symbol.name == "b"
    assert new_clause.children[2].symbol.name == "c"


def test_depend_clause():
    ''' Test the OMPDependClause functionality. '''
    depend1 = OMPDependClause()

    with pytest.raises(TypeError) as excinfo:
        OMPDependClause(depend_type="badstring")
    assert ("OMPDependClause expected 'depend_type' argument of type "
            "OMPDependClause.DependClauseTypes but found 'str'" in
            str(excinfo.value))

    depend2 = OMPDependClause()
    assert depend1 == depend2
    dependin = OMPDependClause(
                depend_type=OMPDependClause.DependClauseTypes.IN)
    dependout = OMPDependClause(
                    depend_type=OMPDependClause.DependClauseTypes.OUT)
    assert dependin != dependout
    # Check operand
    assert dependin.operand == "in"
    assert dependout.operand == "out"
    assert depend1.operand == "inout"
    coloredtext = colored("OMPDependClause", OMPDependClause._colour)
    assert (coloredtext+"[operand=DependClauseTypes.INOUT]"
            in depend1.node_str())


def test_depend_validate_child():
    ''' Test the validate_child function of the OMPDependClause. '''
    tmp = DataSymbol("tmp", INTEGER_TYPE)
    ref1 = Reference(tmp)
    assert OMPDependClause._validate_child(0, ref1) is True
    assert OMPDependClause._validate_child(110, ref1) is True
    assert OMPDependClause._validate_child(0, "test") is False
