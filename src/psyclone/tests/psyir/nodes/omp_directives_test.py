# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council.
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
#         A. B. G. Chalk, STFC Daresbury Lab
# Modified I. Kavcic, Met Office
#          J. Remy, Université Grenoble Alpes, Inria
#          M. Naylor, University of Cambridge, UK
# -----------------------------------------------------------------------------

''' Performs py.test tests on the OpenMP PSyIR Directive nodes. '''

import os
import re
import pytest
import logging
from psyclone.errors import UnresolvedDependencyError
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir import nodes
from psyclone import psyGen
from psyclone.psyir.nodes import (
    OMPDoDirective, OMPParallelDirective, BinaryOperation, Call,
    ArrayReference, OMPTaskDirective, DynamicOMPTaskDirective,
    IntrinsicCall, OMPSharedClause, Range, OMPDependClause,
    OMPParallelDoDirective, OMPMasterDirective, OMPTaskloopDirective,
    OMPTaskwaitDirective, OMPTargetDirective, OMPLoopDirective, Schedule,
    Return, OMPSingleDirective, Loop, Literal, Routine, Assignment,
    Reference, OMPDeclareTargetDirective, OMPNowaitClause,
    OMPGrainsizeClause, OMPNumTasksClause, OMPNogroupClause,
    OMPPrivateClause, OMPDefaultClause, OMPReductionClause,
    OMPScheduleClause, OMPTeamsDistributeParallelDoDirective,
    OMPAtomicDirective, OMPFirstprivateClause, OMPSimdDirective,
    StructureReference, IfBlock, OMPTeamsLoopDirective, OMPBarrierDirective,
    AtomicDirectiveType)
from psyclone.psyir.symbols import (
    DataSymbol, INTEGER_TYPE, SymbolTable, ArrayType, RoutineSymbol,
    REAL_SINGLE_TYPE, INTEGER_SINGLE_TYPE, Symbol, StructureType,
    REAL_TYPE, DataTypeSymbol)
from psyclone.psyir.transformations import ChunkLoopTrans, OMPTaskTrans
from psyclone.errors import InternalError, GenerationError
from psyclone.psyir.transformations.omp_taskloop_trans import OMPTaskloopTrans
from psyclone.transformations import (
    LFRicOMPLoopTrans, OMPParallelTrans,
    OMPParallelLoopTrans, LFRicOMPParallelLoopTrans, OMPSingleTrans,
    OMPMasterTrans, OMPLoopTrans, TransformationError)

BASE_PATH = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__)))), "test_files", "lfric")
GOCEAN_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                os.pardir, os.pardir, "test_files",
                                "gocean1p0")


def test_ompparallel_lowering(fortran_reader, monkeypatch, caplog):
    ''' Check that lowering an OMP Parallel region leaves it with the
    appropriate begin_string and clauses for the backend to generate
    the right code'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(320) :: A
        integer :: i
        integer :: j
        do i = 1, 320
            A(i) = i
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    tdir = OMPDoDirective()
    loops = tree.walk(Loop)
    loop = loops[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    ptrans.apply(loops[0].parent.parent)
    assert isinstance(tree.children[0].children[0], OMPParallelDirective)
    pdir = tree.children[0].children[0]
    pdir.lower_to_language_level()
    assert pdir.begin_string() == "omp parallel"
    assert len(pdir.children) == 4
    assert isinstance(pdir.children[2], OMPPrivateClause)
    assert isinstance(pdir.children[3], OMPFirstprivateClause)
    priv_clause = pdir.children[2]

    # If the code inside the region changes after lowering, the next lowering
    # will update the clauses appropriately
    # TODO 2157: Alternatively, we could invalidate the clauses with an
    # upwards signal when changed, or not store them at all.
    new_loop = pdir.children[0].children[0].children[0].children[0].copy()
    # Change the loop variable to j
    jvar = DataSymbol("j", INTEGER_SINGLE_TYPE)
    new_loop.variable = jvar
    # Add loop
    pdir.children[0].addchild(new_loop)

    pdir.lower_to_language_level()
    assert pdir.children[2] is not priv_clause

    # Monkeypatch a case with private and firstprivate clauses
    monkeypatch.setattr(pdir, "infer_sharing_attributes",
                        lambda: ({Symbol("a")}, {Symbol("b")}, None))

    pdir.lower_to_language_level()
    assert isinstance(pdir.children[2], OMPPrivateClause)
    assert len(pdir.children[2].children) == 1
    assert pdir.children[2].children[0].name == 'a'
    assert isinstance(pdir.children[3], OMPFirstprivateClause)
    assert len(pdir.children[3].children) == 1
    assert pdir.children[3].children[0].name == 'b'

    # Monkeypatch a case with shared variables that need synchronisation
    a_sym = Symbol("a")
    monkeypatch.setattr(pdir, "infer_sharing_attributes",
                        lambda: ({}, {}, {a_sym}))
    with caplog.at_level(logging.WARNING):
        pdir.lower_to_language_level()
    assert ("Lowering 'OMPParallelDirective' detected a possible race "
            "condition for symbol 'a'. Make sure this is a false WaW "
            "dependency or the code includes the necessary synchronisations."
            "\n" in caplog.text)

    # Also a case which contains the symbol in an input dependency clause.
    task_dir = OMPTaskDirective()
    task_dir.addchild(OMPPrivateClause())
    task_dir.addchild(OMPFirstprivateClause())
    task_dir.addchild(OMPSharedClause())
    in_clause = OMPDependClause(
            depend_type=OMPDependClause.DependClauseTypes.IN)
    in_clause.addchild(Reference(a_sym))
    task_dir.addchild(in_clause)
    pdir.children[0].addchild(task_dir)

    with caplog.at_level(logging.WARNING):
        pdir.lower_to_language_level()
    assert ("Lowering 'OMPParallelDirective' detected a possible race "
            "condition for symbol 'a'. Make sure this is a false WaW "
            "dependency or the code includes the necessary synchronisations."
            "\n" in caplog.text)


def test_omp_parallel_do_lowering(fortran_reader, monkeypatch, caplog):
    ''' Check that lowering an OMP Parallel Do leaves it with the
    appropriate begin_string and clauses for the backend to generate
    the right code'''

    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j

        do i = 1, 320, 32
            A(i, 1) = B(i, 1) + 1
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelLoopTrans()
    loops = tree.walk(Loop)
    ptrans.apply(loops[0])
    assert isinstance(tree.children[0].children[0], OMPParallelDoDirective)
    pdir = tree.children[0].children[0]
    assert pdir.begin_string() == "omp parallel do"
    pdir.lower_to_language_level()
    assert len(pdir.children) == 5
    assert isinstance(pdir.children[2], OMPPrivateClause)
    assert isinstance(pdir.children[3], OMPFirstprivateClause)
    priv_clause = pdir.children[2]
    fpriv_clause = pdir.children[3]
    sched_clause = pdir.children[4]

    # If the code inside the region changes after lowering, the next lowering
    # will update the clauses appropriately
    routine = pdir.ancestor(Routine)
    routine.symbol_table.add(DataSymbol("k", INTEGER_SINGLE_TYPE))
    # Change the loop variable to j
    jvar = DataSymbol("k", INTEGER_SINGLE_TYPE)
    pdir.children[0].children[0].variable = jvar

    # Change the schedule
    pdir._omp_schedule = "dynamic"

    pdir.lower_to_language_level()
    assert pdir.children[2] is not priv_clause
    assert isinstance(pdir.children[2], OMPPrivateClause)
    assert isinstance(pdir.children[3], OMPFirstprivateClause)
    assert pdir.children[3] is not fpriv_clause
    assert pdir.children[4] is not sched_clause
    assert isinstance(pdir.children[4], OMPScheduleClause)

    # Monkeypatch a case with private and firstprivate clauses
    monkeypatch.setattr(pdir, "infer_sharing_attributes",
                        lambda: ({Symbol("a")}, {Symbol("b")}, None))

    pdir.lower_to_language_level()
    assert isinstance(pdir.children[2], OMPPrivateClause)
    assert len(pdir.children[2].children) == 1
    assert pdir.children[2].children[0].name == 'a'
    assert isinstance(pdir.children[3], OMPFirstprivateClause)
    assert len(pdir.children[3].children) == 1
    assert pdir.children[3].children[0].name == 'b'

    # Monkeypatch a case with shared variables that need synchronisation
    monkeypatch.setattr(pdir, "infer_sharing_attributes",
                        lambda: ({}, {}, {Symbol("a")}))
    with caplog.at_level(logging.WARNING):
        pdir.lower_to_language_level()
    assert ("Lowering 'OMPParallelDoDirective' detected a possible race "
            "condition for symbol 'a'. Make sure this is a false WaW "
            "dependency or the code includes the necessary synchronisations."
            "\n" in caplog.text)


def test_omp_teams_distribute_parallel_do_strings(
        fortran_reader, fortran_writer):
    ''' Check that the beginning and ending directive strings that the
    backend uses are the expected ones.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j

        do i = 1, 320, 32
            A(i, 1) = B(i, 1) + 1
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    loop = tree.walk(Loop)[0]
    new_directive = OMPTeamsDistributeParallelDoDirective()
    loop.replace_with(new_directive)
    new_directive.dir_body.addchild(loop)
    output = fortran_writer(tree)
    assert "!$omp teams distribute parallel do" in output
    assert "!$omp end teams distribute parallel do" in output


def test_ompdo_constructor():
    ''' Check that we can make an OMPDoDirective with and without
    children '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="lfric")
    psy = PSyFactory("lfric", distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    ompdo = OMPDoDirective(parent=schedule)
    # A Directive always has a Schedule
    assert len(ompdo.children) == 1
    assert isinstance(ompdo.children[0], Schedule)
    # Check the dir_body property
    assert isinstance(ompdo.dir_body, Schedule)
    # Break the directive
    del ompdo.children[0]
    with pytest.raises(InternalError) as err:
        # pylint: disable=pointless-statement
        ompdo.dir_body
    assert ("malformed or incomplete. It should have a Schedule as child 0 "
            "but found: []" in str(err.value))
    child = schedule.children[0].detach()
    ompdo = OMPDoDirective(parent=schedule, children=[child])
    assert len(ompdo.dir_body.children) == 1

    # Constructor with non-default parameters
    ompdo = OMPDoDirective(omp_schedule="dynamic", collapse=4, reprod=True)
    assert ompdo.omp_schedule == "dynamic"
    assert ompdo.collapse == 4
    assert ompdo.reprod
    assert str(ompdo) == "OMPDoDirective[omp_schedule=dynamic,collapse=4]"

    # Constructor with nowait parameter
    ompdo = OMPDoDirective(nowait=True)
    assert ompdo.nowait
    assert ompdo.end_string() == "omp end do nowait"


def test_omp_do_directive_nowait_setter():
    ''' Test the OMPDoDirective nowait property setter.'''
    ompdo = OMPDoDirective()
    ompdo.nowait = True

    with pytest.raises(TypeError) as err:
        ompdo.nowait = "string"
    assert ("The OMPDoDirective nowait clause must be a bool, but value "
            "'string' has been given." in str(err.value))


def test_omp_do_directive_collapse_getter_and_setter():
    ''' Test the OMPDODirective collapse property setter and getter.'''
    target = OMPDoDirective()
    assert target.collapse is None

    with pytest.raises(ValueError) as err:
        target.collapse = 0
    assert ("The OMPDoDirective collapse clause must be a positive integer "
            "or None, but value '0' has been given." in str(err.value))

    with pytest.raises(TypeError) as err:
        target.collapse = 'a'
    assert ("The OMPDoDirective collapse clause must be a positive integer "
            "or None, but value 'a' has been given." in str(err.value))

    # Set valid collapse values
    target.collapse = 2
    assert target.collapse == 2
    assert target.begin_string() == "omp do collapse(2)"
    target.collapse = None
    assert target.collapse is None
    assert target.begin_string() == "omp do"


def test_omp_do_directive_omp_schedule_getter_and_setter():
    ''' Test the OMPDODirective omp_schedule property setter and getter.'''
    directive = OMPDoDirective()
    # By default, no schedule is specified.
    assert directive.omp_schedule == "none"

    # But valid omp_schedules are accepted
    directive.omp_schedule = "static"
    assert directive.omp_schedule == "static"
    directive.omp_schedule = "dynamic,3"
    assert directive.omp_schedule == "dynamic,3"

    # Invalid omp_schedules raise a TypeError
    with pytest.raises(TypeError) as err:
        directive.omp_schedule = 3
    assert ("OMPDoDirective omp_schedule should be a str but found 'int'."
            in str(err.value))

    with pytest.raises(TypeError) as err:
        directive.omp_schedule = "invalid,3"
    assert ("OMPDoDirective omp_schedule should be one of ['runtime', "
            "'static', 'dynamic', 'guided', 'auto', 'none'] but found "
            "'invalid,3'." in str(err.value))


def test_omp_do_directive_validate_global_constraints(fortran_reader,
                                                      fortran_writer):
    ''' Test the OMPDoDirective with a collapse value is only valid if
    it has enough perfectly nested loops inside.'''

    code = '''
    subroutine my_subroutine()
        integer, dimension(10, 10) :: A
        integer, dimension(10, 10) :: B
        integer :: i, j, val

        do i = 1, 10
            val = 1
            do j = 1, 10
                A(i, j) = B(i, j) + 1
            end do
        end do
        do i = 1, 10
            do j = 1, 10
                A(i, j) = B(i, j) + 1
            end do
            val = 1
        end do
        do i = 1, 10
            do j = 1, 10
                A(i, j) = B(i, j) + 1
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    loops = tree.walk(Loop, stop_type=Loop)
    for loop in loops:
        parent = loop.parent
        position = loop.position
        directive = OMPParallelDoDirective(children=[loop.detach()],
                                           collapse=2)
        parent.addchild(directive, position)

    directive = tree.walk(OMPParallelDoDirective)

    # The first and second loop nests will fail the validation
    for test_directive in directive[0:2]:
        with pytest.raises(GenerationError) as err:
            _ = fortran_writer(test_directive)
        assert ("OMPParallelDoDirective must have as many immediately nested "
                "loops as the collapse clause specifies but 'OMPParallelDo"
                "Directive[collapse=2]' has a collapse=2 "
                "and the nested body at depth 1 cannot be collapsed."
                in str(err.value))

    # The third loop nest will succeed
    code = fortran_writer(directive[2])
    assert "collapse(2)" in code

    # but it will also fail if trying to collapse more loops than available
    directive[2].collapse = 3
    with pytest.raises(GenerationError) as err:
        _ = fortran_writer(directive[2])
    assert ("OMPParallelDoDirective must have as many immediately nested "
            "loops as the collapse clause specifies but 'OMPParallelDo"
            "Directive[collapse=3]' has a collapse=3 and "
            "the nested body at depth 2 cannot be collapsed."
            in str(err.value))


def test_omp_parallel_do_create():
    ''' Test the OMPParallelDoDirective create method. '''
    loop = Loop.create(DataSymbol("i", INTEGER_SINGLE_TYPE),
                       Literal("1", INTEGER_SINGLE_TYPE),
                       Literal("10", INTEGER_SINGLE_TYPE),
                       Literal("1", INTEGER_SINGLE_TYPE),
                       [])
    children = [loop]
    directive = OMPParallelDoDirective.create(children=children, collapse=2)
    assert directive.collapse == 2
    assert directive.omp_schedule == "none"
    assert str(directive) == "OMPParallelDoDirective[collapse=2]"
    assert directive.dir_body.children[0] is loop
    assert (directive.default_clause.clause_type
            == OMPDefaultClause.DefaultClauseTypes.SHARED)


def test_omp_pdo_validate_child():
    ''' Test the _validate_child method for OMPParallelDoDirective'''
    sched = Schedule()
    declause = OMPDefaultClause()
    prclause = OMPPrivateClause()
    fprclause = OMPFirstprivateClause()
    scclause = OMPScheduleClause()
    reclause = OMPReductionClause(
            operator=OMPReductionClause.ReductionClauseTypes.ADD)

    assert OMPParallelDoDirective._validate_child(0, sched) is True
    assert OMPParallelDoDirective._validate_child(1, declause) is True
    assert OMPParallelDoDirective._validate_child(2, prclause) is True
    assert OMPParallelDoDirective._validate_child(3, fprclause) is True
    assert OMPParallelDoDirective._validate_child(4, scclause) is True
    assert OMPParallelDoDirective._validate_child(5, reclause) is True
    assert OMPParallelDoDirective._validate_child(6, reclause) is True

    assert OMPParallelDoDirective._validate_child(0, "abc") is False
    assert OMPParallelDoDirective._validate_child(1, "abc") is False
    assert OMPParallelDoDirective._validate_child(2, "abc") is False
    assert OMPParallelDoDirective._validate_child(3, "abc") is False
    assert OMPParallelDoDirective._validate_child(4, "abc") is False
    assert OMPParallelDoDirective._validate_child(5, "abc") is False
    assert OMPParallelDoDirective._validate_child(6, "abc") is False


def test_ompdo_equality():
    ''' Test the __eq__ method of OMPDoDirective. '''
    # We need to manually set the same SymbolTable instance in both directives
    # for their equality to be True
    symboltable = SymbolTable()
    # Set up the symbols
    tmp = DataSymbol("tmp", REAL_SINGLE_TYPE)
    i_sym = DataSymbol("i", REAL_SINGLE_TYPE)

    # Create two equal loops
    loop_sym = DataSymbol("i", INTEGER_SINGLE_TYPE)
    sched1 = Schedule(symbol_table=symboltable)
    start = Literal("0", INTEGER_SINGLE_TYPE)
    stop = Literal("1", INTEGER_SINGLE_TYPE)
    step = Literal("1", INTEGER_SINGLE_TYPE)
    child_node = Assignment.create(
        Reference(tmp),
        Reference(i_sym))
    sched1.addchild(child_node)
    loop1 = Loop.create(loop_sym,
                        start, stop, step, [])
    loop1.children[3].detach()
    loop1.addchild(sched1, 3)
    start2 = start.copy()
    stop2 = stop.copy()
    step2 = step.copy()
    sched2 = Schedule()
    # Make sure it has the same ST instance, providing it as a constructor
    # parameter would create a copy and not use the same instance.
    sched2._symbol_table = symboltable
    child_node2 = Assignment.create(
        Reference(tmp),
        Reference(i_sym))
    sched2.addchild(child_node2)
    loop2 = Loop.create(loop_sym,
                        start2, stop2, step2, [])
    loop2.children[3].detach()
    loop2.addchild(sched2, 3)

    ompdo1 = OMPDoDirective(children=[loop1])
    ompdo2 = OMPDoDirective(children=[loop2])
    ompdo1.children[0]._symbol_table = symboltable
    ompdo2.children[0]._symbol_table = symboltable
    assert ompdo1 == ompdo2

    loop2.detach()
    ompdo2 = OMPDoDirective(children=[loop2], reprod=not ompdo1.reprod)
    assert ompdo1 != ompdo2


def test_omp_do_children_err(fortran_reader):
    ''' Tests that we raise the expected error when an OpenMP parallel do
    directive has more than one child or the child is not a loop. '''
    otrans = OMPParallelLoopTrans()
    psyir = fortran_reader.psyir_from_source('''
        subroutine my_subroutine()
            integer :: i, scalar1
            real, dimension(10) :: array
            do i = 1, 10
               array(i) = scalar1
            enddo
        end subroutine''')
    otrans.apply(psyir.walk(Loop)[0])
    directive = psyir.walk(OMPParallelDoDirective)[0]
    # Make the schedule invalid by adding a second child to the
    # OMPParallelDoDirective
    directive.dir_body.children.append(directive.dir_body[0].copy())
    with pytest.raises(GenerationError) as err:
        directive.validate_global_constraints()
    assert ("An OMPParallelDoDirective can only be applied to a single loop "
            "but this Node has 2 children:" in str(err.value))
    directive.dir_body.children = [Return()]
    with pytest.raises(GenerationError) as err:
        directive.validate_global_constraints()
    assert ("An OMPParallelDoDirective can only be applied to a loop but "
            "this Node has a child of type 'Return'" in str(err.value))


def test_directiveinfer_sharing_attributes_lfric():
    ''' Tests for the infer_sharing_attributes() method of
    OMPParallelDirective containing an LFRic kernel.

    Note: this test does not apply colouring so the loops must be over
    discontinuous function spaces.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke_w3.f90"), api="lfric")
    psy = PSyFactory("lfric",
                     distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # We use Transformations to introduce the necessary directives
    otrans = LFRicOMPLoopTrans()
    rtrans = OMPParallelTrans()
    # Apply an OpenMP do directive to the loop
    otrans.apply(schedule.children[0], {"reprod": True})
    # Apply an OpenMP Parallel directive around the OpenMP do directive
    rtrans.apply(schedule.children[0])
    directive = schedule.children[0]
    assert isinstance(directive, OMPParallelDirective)
    # TODO #1010 In the LFRic API, the loop bounds are created at code-
    # generation time and therefore we cannot generate the list of
    # private variables until that is under way. Ultimately this will be
    # replaced by a `lower_to_language_level` call.
    # pylint: disable=pointless-statement
    psy.gen
    # Now check that infer_sharing_attributes returns what we expect
    pvars, fpvars, sync = directive.infer_sharing_attributes()
    assert isinstance(pvars, set)
    assert isinstance(fpvars, set)
    assert len(pvars) == 1
    assert len(fpvars) == 0
    assert len(sync) == 0
    assert list(pvars)[0].name == 'cell'

    directive.children[1] = OMPDefaultClause(
            clause_type=OMPDefaultClause.DefaultClauseTypes.NONE)
    with pytest.raises(GenerationError) as excinfo:
        _ = directive.infer_sharing_attributes()
    assert ("OMPParallelClause cannot correctly generate the private clause "
            "when its default data sharing attribute in its default clause is "
            "not 'shared'." in str(excinfo.value))


def test_infer_sharing_attributes_with_explicitly_private_symbols(
        fortran_reader):
    ''' Tests the infer_sharing_attributes() method when some of the loops have
    explictly declared private symbols. Also test that non-data accesses to
    array symbols are ignored.
    '''
    psyir = fortran_reader.psyir_from_source('''
        subroutine my_subroutine()
            integer :: i, j, scalar1, scalar2
            real, dimension(10) :: array, array2
            do j = 1, 10
               do i = 1, size(array, 1)
                   ! Access to array2 is type information rather than data
                   array(i) = scalar2 * size(array2, 1)
               enddo
            enddo
        end subroutine''')
    omplooptrans = OMPLoopTrans()
    omplooptrans.omp_directive = "paralleldo"
    loop = psyir.walk(Loop)[0]
    routine = psyir.walk(Routine)[0]
    omplooptrans.apply(loop, options={'force': True})
    directive = psyir.walk(OMPParallelDoDirective)[0]

    # If no symbols are explicitly local, the infer sharing
    # attributes uses its default rules
    pvars, fpvars, sync = directive.infer_sharing_attributes()
    assert len(pvars) == 2
    assert len(fpvars) == 0
    assert len(sync) == 0
    assert "i" in [x.name for x in pvars]
    assert "j" in [x.name for x in pvars]

    # If the loop has some explict locals, these are listed when getting
    # the infer_sharing_attributes
    array_symbol = routine.symbol_table.lookup("array")
    loop.explicitly_private_symbols.add(array_symbol)
    pvars, fpvars, sync = directive.infer_sharing_attributes()
    assert len(pvars) == 3
    assert len(fpvars) == 0
    assert len(sync) == 0
    assert "i" in [x.name for x in pvars]
    assert "j" in [x.name for x in pvars]
    assert "array" in [x.name for x in pvars]

    # Scalar symbols can also be set as explicitly local
    scalar_symbol = routine.symbol_table.lookup("scalar2")
    loop.explicitly_private_symbols.add(scalar_symbol)
    pvars, fpvars, sync = directive.infer_sharing_attributes()
    assert len(pvars) == 4
    assert len(fpvars) == 0
    assert len(sync) == 0
    assert "i" in [x.name for x in pvars]
    assert "j" in [x.name for x in pvars]
    assert "array" in [x.name for x in pvars]
    assert "scalar2" in [x.name for x in pvars]

    # If this have a value before the loop (used in any way), they
    # are firstprivate
    routine.addchild(Assignment.create(
        lhs=Reference(array_symbol),
        rhs=Reference(scalar_symbol)
    ), 0)
    pvars, fpvars, sync = directive.infer_sharing_attributes()
    assert len(pvars) == 2
    assert len(fpvars) == 2
    assert len(sync) == 0
    assert "i" in [x.name for x in pvars]
    assert "j" in [x.name for x in pvars]
    assert "array" in [x.name for x in fpvars]
    assert "scalar2" in [x.name for x in fpvars]


def test_infer_sharing_attributes_with_codeblocks(
        fortran_reader, fortran_writer):
    ''' Tests the infer_sharing_attributes() method when some of the loops have
    Codeblocks inside it. We check that the infer_sharing attribute analysis
    succeed by assuming worst case.
    '''
    psyir = fortran_reader.psyir_from_source('''
        subroutine my_subroutine()
            use other, only: mystruct
            integer :: i, j, scalar1 = 1, scalar2 = 2
            real, dimension(10) :: array, array2
            write(*,*) scalar1, scalar2
            do j = 1, 10
               do i = 1, size(array, 1)
                   write(*,*) scalar2, mystruct(i)%field2
                   if (.true.) then
                       scalar1 = 1
                       write(*,*) scalar1, mystruct(i)%field1
                   end if
                   scalar2 = scalar1 + 1
                   write(*,*) scalar1, scalar2
               enddo
            enddo
            write(*,*) scalar1, scalar2
        end subroutine''')
    omplooptrans = OMPLoopTrans()
    omplooptrans.omp_directive = "paralleldo"
    loop = psyir.walk(Loop)[0]
    # Make sure that the write statements inside the loop are CodeBlocks,
    # otherwise we need a new test example
    assert loop.has_descendant(nodes.CodeBlock)
    loop.explicitly_private_symbols.add(
            loop.scope.symbol_table.lookup("scalar2"))
    omplooptrans.apply(loop, node_type_check=False, force=True)

    # Here we mostly check that the infer_sharing attributes doesn't fall
    # over with CodeBlocks. This will often still be defensively firstprivate
    # as we condiser all codeblock accesses as READWRITE.
    output = fortran_writer(psyir)
    assert "firstprivate(scalar1,scalar2)" in output


def test_infer_sharing_attributes(fortran_reader):
    ''' Tests for the infer_sharing_attributes() method of OpenMP directives
    with generic code inside the directive body.
    '''

    # Example with arrays, read-only and only-writen-once variables, this are
    # all shared (only the iteration index is private in this parallel region)
    psyir = fortran_reader.psyir_from_source('''
        subroutine my_subroutine()
            integer :: i, scalar1, scalar2
            real, dimension(10) :: array
            scalar2 = scalar1
            do i = 1, 10
               array(i) = scalar2
            enddo
        end subroutine''')
    omplooptrans = OMPLoopTrans()
    loop = psyir.walk(Loop)[0]
    omplooptrans.apply(loop)
    omptrans = OMPParallelTrans()
    routine = psyir.walk(Routine)[0]
    omptrans.apply(routine.children)
    directive = psyir.walk(OMPParallelDirective)[0]
    pvars, fpvars, sync = directive.infer_sharing_attributes()
    assert len(pvars) == 1
    assert list(pvars)[0].name == 'i'
    assert len(fpvars) == 0
    assert len(sync) == 0

    # Example with private and firstprivate variables on OMPParallelDoDirective
    # In this case scalar1 is firstprivate because it has a conditional-write
    # that it is not guaranteed to happen on each iteration, so the private
    # variable needs to be initialised with the value it has before the loop.
    psyir = fortran_reader.psyir_from_source('''
        subroutine my_subroutine()
            integer :: i, scalar1, scalar2
            real, dimension(10) :: array
            scalar1 = 3
            do i = 1, 10
               if (i .eq. 4) then
                  scalar1 = array(i)
               endif
               scalar2 = scalar1 + array(i)
               array(i) = scalar2
            enddo
        end subroutine''')
    omplooptrans = OMPParallelLoopTrans()
    loop = psyir.walk(Loop)[0]
    omplooptrans.apply(loop)
    directive = psyir.walk(OMPParallelDoDirective)[0]
    pvars, fpvars, sync = directive.infer_sharing_attributes()
    assert len(pvars) == 2
    assert sorted(pvars, key=lambda x: x.name)[0].name == 'i'
    assert sorted(pvars, key=lambda x: x.name)[1].name == 'scalar2'
    assert len(fpvars) == 1
    assert list(fpvars)[0].name == 'scalar1'
    assert len(sync) == 0

    # Another example with only a OMPParallelDirective (not actual worksharing)
    # and scalars set outside the loop (only-written-once), these are shared
    psyir = fortran_reader.psyir_from_source('''
        subroutine my_subroutine()
            integer :: i, scalar1, scalar2, scalar3, scalar4
            real, dimension(10) :: array
            scalar3 = 10
            scalar4 = 20
            do i = 1, scalar3
               scalar2 = scalar1 + scalar4 + array(i)
               array(i) = scalar2
            enddo
        end subroutine''')
    omptrans = OMPParallelTrans()
    routine = psyir.walk(Routine)[0]
    omptrans.apply(routine.children)
    directive = psyir.walk(OMPParallelDirective)[0]
    pvars, fpvars, sync = directive.infer_sharing_attributes()
    assert len(pvars) == 2
    assert len(fpvars) == 0
    assert len(sync) == 0
    assert sorted(pvars, key=lambda x: x.name)[0].name == 'i'
    assert sorted(pvars, key=lambda x: x.name)[1].name == 'scalar2'
    # scalar1 is shared because is read-only and scalar3 and scalar4 are
    # shared because they are set outside a loop (only written once)

    # Another example with only a OMPParallelDirective (not actual worksharing)
    # and one scalar (scalar2) it is used as a private variable inside the loop
    # but it is first read before the loop, and therefore it should be
    # firstprivate
    psyir = fortran_reader.psyir_from_source('''
        subroutine my_subroutine()
            integer :: i, scalar1, scalar2, scalar3
            real, dimension(10) :: array
            scalar3 = scalar2
            do i = 1, 10
               scalar2 = scalar1 + scalar3 + array(i)
               array(i) = scalar2
            enddo
        end subroutine''')
    omptrans = OMPParallelTrans()
    routine = psyir.walk(Routine)[0]
    omptrans.apply(routine.children)
    directive = psyir.walk(OMPParallelDirective)[0]
    pvars, fpvars, sync = directive.infer_sharing_attributes()
    assert len(pvars) == 1
    assert len(fpvars) == 1
    assert len(sync) == 0
    assert list(pvars)[0].name == 'i'
    assert list(fpvars)[0].name == 'scalar2'

    # Similar but with a OMPParallelDoDirective and the firstprivate is
    # in the same loop but before the loop body
    psyir = fortran_reader.psyir_from_source('''
        subroutine my_subroutine()
            integer :: i, scalar1
            real, dimension(10) :: array
            do i = 1, 10, scalar1
                scalar1 = array(i)
                array(i) = scalar1
            enddo
        end subroutine''')
    omplooptrans = OMPParallelLoopTrans()
    loop = psyir.walk(Loop)[0]
    omplooptrans.apply(loop, options={'force': True})
    directive = psyir.walk(OMPParallelDoDirective)[0]
    pvars, fpvars, sync = directive.infer_sharing_attributes()
    assert len(pvars) == 1
    assert len(fpvars) == 1
    assert len(sync) == 0
    assert list(pvars)[0].name == 'i'
    assert list(fpvars)[0].name == 'scalar1'

    # In this example the scalar2 variable is shared but it needs
    # synchronisation to avoid race conditions (write-after-read
    # in the same statement)
    psyir = fortran_reader.psyir_from_source('''
        subroutine my_subroutine()
            integer :: i, scalar1, scalar2
            real, dimension(10) :: array
            do i = 1, 10
               scalar2 = scalar2 + scalar1
            enddo
        end subroutine''')
    omplooptrans = OMPParallelLoopTrans()
    loop = psyir.walk(Loop)[0]
    omplooptrans.apply(loop, options={"force": True})
    directive = psyir.walk(OMPParallelDoDirective)[0]
    pvars, fpvars, sync = directive.infer_sharing_attributes()
    assert len(pvars) == 1
    assert list(pvars)[0].name == 'i'
    assert len(fpvars) == 0
    assert len(sync) == 1
    assert list(sync)[0].name == 'scalar2'

    # In this example the scalar2 variable is shared but it needs
    # synchronisation to avoid race conditions (write-after-read
    # in different statements)
    psyir = fortran_reader.psyir_from_source('''
        subroutine my_subroutine()
            integer :: i, scalar1, scalar2, tmp
            real, dimension(10) :: array
            do i = 1, 10
               tmp = scalar2 + scalar1
               scalar2 = tmp
            enddo
        end subroutine''')
    omplooptrans = OMPParallelLoopTrans()
    loop = psyir.walk(Loop)[0]
    omplooptrans.apply(loop, options={"force": True})
    directive = psyir.walk(OMPParallelDoDirective)[0]
    pvars, fpvars, sync = directive.infer_sharing_attributes()
    assert len(pvars) == 2
    assert sorted(pvars, key=lambda x: x.name)[0].name == 'i'
    assert sorted(pvars, key=lambda x: x.name)[1].name == 'tmp'
    assert len(fpvars) == 0
    assert len(sync) == 1
    assert list(sync)[0].name == 'scalar2'

    # Example tiling routine (k is a reduction - needs sync)
    psyir = fortran_reader.psyir_from_source('''
        subroutine my_subroutine(k)
            integer :: i, ii
            integer :: j, jj
            integer :: k
            do i = 1, 320, 32
                do j = 1, 320, 32
                    do ii=i, i+32
                        do jj = j,j+32
                            k = k + ii
                            k = k * jj
                        end do
                    end do
                end do
            end do
        end subroutine''')
    ptrans = OMPParallelTrans()
    loop = psyir.walk(Loop)[0]
    ptrans.apply(loop)
    directive = psyir.walk(OMPParallelDirective)[0]
    pvars, fpvars, sync = directive.infer_sharing_attributes()
    assert len(pvars) == 4
    assert sorted(pvars, key=lambda x: x.name)[0].name == 'i'
    assert sorted(pvars, key=lambda x: x.name)[1].name == 'ii'
    assert sorted(pvars, key=lambda x: x.name)[2].name == 'j'
    assert sorted(pvars, key=lambda x: x.name)[3].name == 'jj'
    assert len(fpvars) == 0
    assert len(sync) == 1
    assert list(sync)[0].name == 'k'

    # Check that kinds on literals are ignored for data sharing clauses
    psyir = fortran_reader.psyir_from_source('''
        subroutine my_subroutine()
            integer, parameter :: ikind = 4
            integer :: i, scalar1, scalar2
            real, dimension(10) :: array
            do i = 1, 10
               array(i) = 1_ikind + INT(i, ikind)
            enddo
        end subroutine''')
    omplooptrans = OMPLoopTrans()
    loop = psyir.walk(Loop)[0]
    omplooptrans.apply(loop)
    omptrans = OMPParallelTrans()
    routine = psyir.walk(Routine)[0]
    omptrans.apply(routine.children)
    directive = psyir.walk(OMPParallelDirective)[0]
    pvars, fpvars, sync = directive.infer_sharing_attributes()
    assert len(pvars) == 1
    assert list(pvars)[0].name == 'i'
    assert len(fpvars) == 0
    assert len(sync) == 0


def test_directiveinfer_sharing_attributes_with_structures(fortran_reader):
    ''' Tests for the infer_sharing_attributes() method of OpenMP directives
    with code that contains structure accesses.
    '''
    psyir = fortran_reader.psyir_from_source('''
        subroutine my_subroutine()
            use my_mod
            integer :: i, scalar1
            type(my_type) :: mt1, mt2
            real, dimension(10) :: array
            mt1%scalar1 = 3
            mt2%scalar1 = 3
            do i = 1, 10
               if (i .eq. 4) then
                  mt2%field1%scalar1 = array(i)
               endif
               scalar1 = mt2%field1%scalar1 + mt1%scalar1
               array(i) = scalar1
            enddo
        end subroutine''')
    omptrans = OMPParallelTrans()
    routine = psyir.walk(Routine)[0]
    omptrans.apply(routine.children[2])
    directive = psyir.walk(OMPParallelDirective)[0]
    pvars, fpvars, sync = directive.infer_sharing_attributes()
    pytest.xfail("#2094: Currently we only support top-level derived types"
                 "as OpenMP sharing attributes, but there are cases that "
                 "more detail is necessary.")
    assert len(pvars) == 2
    assert sorted(pvars, key=lambda x: x.name)[0].name == 'i'
    assert sorted(pvars, key=lambda x: x.name)[1].name == 'scalar1'
    assert len(fpvars) == 1
    assert list(fpvars)[0].name == 'mt2'
    assert len(sync) == 0

    # In this example a sub-part of mt1 should be shared and another
    # firstprivate
    psyir = fortran_reader.psyir_from_source('''
        subroutine my_subroutine()
            use my_mod
            integer :: i, scalar1
            type(my_type) :: mt1
            real, dimension(10) :: array
            mt1%another_scalar = 3
            do i = 1, 10
               if (i .eq. 4) then
                  mt1%scalar1 = 3
               endif
               mt1%array(i) = mt1%scalar1
            enddo
        end subroutine''')
    omptrans = OMPParallelTrans()
    routine = psyir.walk(Routine)[0]
    omptrans.apply(routine.children[1])
    directive = psyir.walk(OMPParallelDirective)[0]
    pvars, fpvars, sync = directive.infer_sharing_attributes()
    assert len(pvars) == 1
    assert list(pvars)[0].name == 'i'
    assert len(fpvars) == 1
    if list(fpvars)[0].name == 'mt2':
        pytest.xfail("#2094: Currently we only support top-level derived types"
                     "as OpenMP sharing attributes, but there are cases that "
                     "more detail is necessary.")


def testinfer_sharing_attributes_sequential_semantics(fortran_reader):
    ''' infer_sharing_attributes() tries to conserve the same semantics
    as the sequential loop, however, for loops that are not possible to
    parallelise (but we force the transformation anyway). For now this
    may return different results than the original code.

    #TODO #598: This could be a lastprivate?
    '''

    # In this example the result will take the value of the last i in
    # sequential order, but an arbitrary i in parallel.
    psyir = fortran_reader.psyir_from_source('''
        subroutine my_subroutine()
            integer :: i, result
            do i = 1, 10
               result = i
            enddo
        end subroutine''')
    omplooptrans = OMPParallelLoopTrans()
    loop = psyir.walk(Loop)[0]
    omplooptrans.apply(loop, options={"force": True})
    directive = psyir.walk(OMPParallelDoDirective)[0]
    pvars, fpvars, sync = directive.infer_sharing_attributes()
    assert len(pvars) == 1
    assert list(pvars)[0].name == 'i'
    assert len(fpvars) == 0
    assert len(sync) == 0


def test_directive_lastprivate(fortran_reader, fortran_writer):
    ''' Test to demonstrate remaining issues with the OpenMP data sharing
    clauses when we have dependencies after the OpenMP loop.

    #TODO #598: A better use of dependency analysis could fix these issues.

    '''
    psyir = fortran_reader.psyir_from_source('''
        subroutine my_subroutine()
            integer :: i, scalar1, scalar2
            real, dimension(10) :: array
            do i = 1, 10
               scalar2 = scalar1 + array(i)
               array(i) = scalar2
            enddo
            scalar1 = scalar2
        end subroutine''')
    omplooptrans = OMPParallelLoopTrans()
    loop = psyir.walk(Loop)[0]
    omplooptrans.apply(loop)
    code = fortran_writer(psyir)
    expected = '''\
  !$omp parallel do default(shared), private(i,scalar2), lastprivate(scalar2)
  do i = 1, 10, 1
    scalar2 = scalar1 + array(i)
    array(i) = scalar2
  enddo
  !$omp end parallel do
  scalar1 = scalar2'''
    if code not in expected:
        pytest.xfail("#598 We do not check yet for possible dependencies of"
                     "variables marked as private after the OpenMP region")


def test_omp_parallel_private_clause():
    '''Test the private_clause method in the OMPParallelDirective
    class.

    '''
    omp_parallel = OMPParallelDirective.create()
    clause = omp_parallel.private_clause
    assert isinstance(clause, OMPPrivateClause)


def test_omp_parallel_encloses_omp_directive():
    '''Test the _encloses_omp_directive method in the OMPParallelDirective
    class. This tests where there is no OMPRegionDirective within the
    OMPParallelirective. Nothing is actually done or returned in the
    method at the moment so this test is purely for coverage.

    '''
    omp_parallel = OMPParallelDirective.create()
    omp_parallel._encloses_omp_directive()


def test_omp_parallel_validate_child():
    ''' Test the validate_child method of OMPParallelDirective'''
    assert OMPParallelDirective._validate_child(0, Schedule()) is True
    assert OMPParallelDirective._validate_child(1, OMPDefaultClause()) is True
    assert OMPParallelDirective._validate_child(2, OMPPrivateClause()) is True
    assert (OMPParallelDirective._validate_child(3, OMPFirstprivateClause())
            is True)
    assert (OMPParallelDirective._validate_child(2, OMPReductionClause(
                operator=OMPReductionClause.ReductionClauseTypes.ADD))
            is False)
    assert (OMPParallelDirective._validate_child(4, OMPReductionClause(
                operator=OMPReductionClause.ReductionClauseTypes.ADD))
            is True)
    assert (OMPParallelDirective._validate_child(
            5, OMPReductionClause(
                operator=OMPReductionClause.ReductionClauseTypes.ADD))
            is True)
    assert OMPParallelDirective._validate_child(0, OMPDefaultClause()) is False
    assert OMPParallelDirective._validate_child(6, "test") is False


def test_omp_do_validate_child():
    ''' Test the validate_child method of OMPDoDirective'''
    assert OMPDoDirective._validate_child(-1, None) is False
    assert OMPDoDirective._validate_child(0, Schedule()) is True
    rc = OMPReductionClause(OMPReductionClause.ReductionClauseTypes.ADD)
    rc.addchild(Reference(Symbol("acc")))
    assert OMPDoDirective._validate_child(1, rc) is True


def test_omp_loop_validate_child():
    ''' Test the validate_child method of OMPLoopDirective'''
    assert OMPLoopDirective._validate_child(-1, None) is False
    assert OMPLoopDirective._validate_child(0, Schedule()) is True
    rc = OMPReductionClause(OMPReductionClause.ReductionClauseTypes.ADD)
    rc.addchild(Reference(Symbol("acc")))
    assert OMPLoopDirective._validate_child(1, rc) is True


def test_omp_forward_dependence():
    '''Test that the forward_dependence method works for Directives,
    returning the closest dependent Node after the current Node in the
    schedule or None if none are found. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.1_multi_aX_plus_Y_builtin.f90"),
        api="lfric")
    psy = PSyFactory("lfric", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    otrans = LFRicOMPParallelLoopTrans()
    for child in schedule.children:
        otrans.apply(child)
    read4 = schedule.children[4]
    # 1: returns none if none found
    # a) check many reads
    assert not read4.forward_dependence()
    # b) check no dependencies for the loop
    assert not read4.children[0].forward_dependence()
    # 2: returns first dependent kernel arg when there are many
    # dependencies
    # a) check first read returned
    writer = schedule.children[3]
    next_read = schedule.children[4]
    assert writer.forward_dependence() == next_read
    # b) check writer returned
    first_omp = schedule.children[0]
    assert first_omp.forward_dependence() == writer
    # 3: directive and globalsum dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.3_sum_setval_field_builtin.f90"),
        api="lfric")
    psy = PSyFactory("lfric", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    otrans.apply(schedule.children[0])
    otrans.apply(schedule.children[1])
    otrans.apply(schedule.children[3])
    prev_omp = schedule.children[0]
    sum_omp = schedule.children[1]
    global_sum_loop = schedule.children[2]
    next_omp = schedule.children[3]
    # a) prev omp depends on sum omp
    assert prev_omp.forward_dependence() == sum_omp
    # b) sum omp depends on global sum loop
    assert sum_omp.forward_dependence() == global_sum_loop
    # c) global sum loop depends on next omp
    assert global_sum_loop.forward_dependence() == next_omp


@pytest.mark.parametrize("nowait", [False, True])
def test_omp_single_nowait(nowait):
    ''' Test the nowait getter of the OMPSingle directive '''
    single = OMPSingleDirective(nowait=nowait)
    assert single.nowait is nowait


@pytest.mark.parametrize("nowait", [False, True])
def test_omp_single_strings(nowait):
    ''' Test the begin_string and end_string methods of the OMPSingle
        directive '''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH, "single_invoke.f90"),
                           api="gocean")
    single = OMPSingleTrans()
    psy = PSyFactory("gocean", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    single.apply(schedule[0], {"nowait": nowait})
    omp_single = schedule[0]

    assert omp_single.begin_string() == "omp single"
    assert omp_single.end_string() == "omp end single"


def test_omp_single_validate_child():
    ''' Test the validate_child method of the OMPSingle class '''
    sched = Schedule()
    nowait = OMPNowaitClause()
    lit = Literal("32", INTEGER_TYPE)
    assert OMPSingleDirective._validate_child(0, sched) is True
    assert OMPSingleDirective._validate_child(1, nowait) is True
    assert OMPSingleDirective._validate_child(0, lit) is False
    assert OMPSingleDirective._validate_child(1, lit) is False
    assert OMPSingleDirective._validate_child(2, lit) is False


def test_omp_single_validate_global_constraints():
    ''' Test the validate_global_constraints method of the OMPSingle
        directive '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="lfric")
    single = OMPSingleTrans()
    psy = PSyFactory("lfric", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    single.apply(schedule.children[0])
    with pytest.raises(GenerationError) as excinfo:
        schedule.children[0].validate_global_constraints()
    assert ("OMPSingleDirective must be inside an OMP parallel region but " +
            "could not find an ancestor OMPParallelDirective node") in \
        str(excinfo.value)


def test_omp_single_nested_validate_global_constraints(monkeypatch):
    ''' Test the validate_global_constraints method of the OMPSingle
        directive fails when nested OMPSingles happen'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="lfric")
    single = OMPSingleTrans()
    # Alternative excluded node types for monkeypatch
    excluded_node_types = (nodes.CodeBlock, nodes.Return, nodes.ACCDirective,
                           psyGen.HaloExchange, nodes.OMPParallelDirective)
    monkeypatch.setattr(single, "excluded_node_types", excluded_node_types)
    parallel = OMPParallelTrans()
    psy = PSyFactory("lfric", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    single.apply(schedule.children[0])
    single_omp = schedule.children[0]
    single.apply(schedule.children[0])
    parallel.apply(schedule.children[0])
    with pytest.raises(GenerationError) as excinfo:
        single_omp.validate_global_constraints()
    assert ("OMPSingleDirective must not be inside another OpenMP serial " +
            "region") in str(excinfo.value)


def test_omp_master_strings():
    ''' Test the begin_string and end_string methods of the OMPMaster
        directive '''
    omp_master = OMPMasterDirective()

    assert omp_master.begin_string() == "omp master"
    assert omp_master.end_string() == "omp end master"


def test_omp_master_validate_global_constraints():
    ''' Test the validate_global_constraints method of the OMPMaster
        directive '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="lfric")
    master = OMPMasterTrans()
    psy = PSyFactory("lfric", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    master.apply(schedule.children[0])
    with pytest.raises(GenerationError) as excinfo:
        schedule.children[0].validate_global_constraints()
    assert ("OMPMasterDirective must be inside an OMP parallel region but " +
            "could not find an ancestor OMPParallelDirective node") in \
        str(excinfo.value)


def test_omp_master_nested_validate_global_constraints(monkeypatch):
    ''' Test the validate_global_constraints method of the OMPMaster
        directive fails when nested OMPSingles happen'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="lfric")
    master = OMPMasterTrans()
    # Alternative excluded node types for monkeypatch
    excluded_node_types = (nodes.CodeBlock, nodes.Return, nodes.ACCDirective,
                           psyGen.HaloExchange, nodes.OMPParallelDirective)
    monkeypatch.setattr(master, "excluded_node_types", excluded_node_types)
    parallel = OMPParallelTrans()
    psy = PSyFactory("lfric", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    master.apply(schedule.children[0])
    master_omp = schedule.children[0]
    master.apply(schedule.children[0])
    parallel.apply(schedule.children[0])
    with pytest.raises(GenerationError) as excinfo:
        master_omp.validate_global_constraints()
    assert ("OMPMasterDirective must not be inside another OpenMP serial " +
            "region") in str(excinfo.value)


def test_omp_barrier_strings():
    ''' Test the begin_string method of the OMPBarrierDirective.'''
    barrier = OMPBarrierDirective()

    assert barrier.begin_string() == "omp barrier"


def test_omp_barrier_validate_global_constraints():
    ''' Test the validate_global_constraints method of the OMPBarrier
        directive '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="lfric")
    psy = PSyFactory("lfric", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    barrier = OMPBarrierDirective()
    schedule.addchild(barrier, 0)
    with pytest.raises(GenerationError) as excinfo:
        barrier.validate_global_constraints()
    assert ("OMPBarrierDirective must be inside an OMP parallel region but "
            "could not find an ancestor OMPParallelDirective node"
            in str(excinfo.value))
    # Valid case.
    barrier = OMPBarrierDirective()
    parallel = OMPParallelDirective()
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="lfric")
    psy = PSyFactory("lfric", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    parallel.children[0].addchild(barrier)
    schedule.addchild(parallel)
    barrier.validate_global_constraints()


def test_omptaskwait_strings():
    ''' Test the begin_string method of the OMPTaskwait directive '''
    taskwait = OMPTaskwaitDirective()

    assert taskwait.begin_string() == "omp taskwait"


def test_omp_taskwait_clauses():
    ''' Test the clauses property of the OMPTaskwait directive. '''
    omp_taskwait = OMPTaskwaitDirective()
    assert len(omp_taskwait.clauses) == 0


def test_omp_taskloop_strings():
    ''' Test the begin_string and end_string methods of the
        OMPTaskloop directive '''
    omp_taskloop = OMPTaskloopDirective()

    assert omp_taskloop.begin_string() == "omp taskloop"
    assert omp_taskloop.end_string() == "omp end taskloop"


def test_omp_taskloop_clauses():
    ''' Test the clauses property of the OMPTaskloop directive. '''
    omp_taskloop = OMPTaskloopDirective()
    assert omp_taskloop.clauses == ()


def test_omp_taskloop_init():
    ''' Test the constructor of the OMPTaskloop directive'''
    with pytest.raises(GenerationError) as excinfo:
        OMPTaskloopDirective(grainsize=32, num_tasks=32)
    assert ("OMPTaskloopDirective must not have both grainsize and "
            "numtasks clauses specified.") in str(excinfo.value)
    tl1 = OMPTaskloopDirective(grainsize=32)
    assert tl1.walk(OMPGrainsizeClause)
    assert not tl1.walk(OMPNumTasksClause)
    tl2 = OMPTaskloopDirective(num_tasks=32)
    assert not tl2.walk(OMPGrainsizeClause)
    assert tl2.walk(OMPNumTasksClause)


@pytest.mark.parametrize("nogroup", [False, True])
def test_omptaskloop_nogroup(nogroup):
    '''Test the nogroup method of OMPTaskloop'''
    taskwait = OMPTaskloopDirective(nogroup=nogroup)
    assert taskwait.nogroup == nogroup


def test_omp_taskloop_validate_child():
    ''' Test the validate_child method of the OMPTaskloopDirective
    Class. '''
    sched = Schedule()
    gsclause = OMPGrainsizeClause(children=[Literal("1", INTEGER_TYPE)])
    ntclause = OMPNumTasksClause(children=[Literal("1", INTEGER_TYPE)])
    ngclause = OMPNogroupClause()
    lit = Literal("1", INTEGER_TYPE)
    assert OMPTaskloopDirective._validate_child(0, sched) is True
    assert OMPTaskloopDirective._validate_child(1, gsclause) is True
    assert OMPTaskloopDirective._validate_child(1, ntclause) is True
    assert OMPTaskloopDirective._validate_child(1, ngclause) is True
    assert OMPTaskloopDirective._validate_child(2, ngclause) is True
    assert OMPTaskloopDirective._validate_child(3, ngclause) is False
    assert OMPTaskloopDirective._validate_child(4, ngclause) is False
    assert OMPTaskloopDirective._validate_child(0, lit) is False
    assert OMPTaskloopDirective._validate_child(1, lit) is False
    assert OMPTaskloopDirective._validate_child(2, lit) is False
    assert OMPTaskloopDirective._validate_child(3, lit) is False


def test_omp_taskloop_validate_global_constraints():
    ''' Test the validate_global_constraints method of the OMPTaskloop
        directive '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke_w3.f90"),
                           api="lfric")
    taskloop = OMPTaskloopTrans()
    psy = PSyFactory("lfric", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    taskloop.apply(schedule.children[0])
    with pytest.raises(GenerationError) as excinfo:
        schedule.children[0].validate_global_constraints()
    assert ("OMPTaskloopDirective must be inside an OMP "
            "Serial region but could not find an ancestor node"
            in str(excinfo.value))

    # Ensure a taskloop clause can't have two nogroup clauses.
    taskloop = schedule.children[0]
    taskloop.addchild(OMPNogroupClause())
    taskloop.addchild(OMPNogroupClause())
    singletrans = OMPSingleTrans()
    paralleltrans = OMPParallelTrans()
    singletrans.apply(taskloop)
    paralleltrans.apply(schedule.children[0])
    with pytest.raises(GenerationError) as excinfo:
        taskloop.validate_global_constraints()
    assert ("OMPTaskloopDirective has two Nogroup clauses as "
            "children which is not allowed." in str(excinfo.value))


# Test OMPTargetDirective

def test_omp_target_directive_constructor_and_strings():
    ''' Test the OMPTargetDirective constructor and its output strings.'''
    target = OMPTargetDirective()
    assert target.begin_string() == "omp target"
    assert target.end_string() == "omp end target"
    assert str(target) == "OMPTargetDirective[]"

    target = OMPTargetDirective(nowait=True)
    assert target.begin_string() == "omp target nowait"


def test_omp_target_nowait_getter_setter():
    ''' Test the OMPTargetDirective nowait getter and setter. '''
    target = OMPTargetDirective()
    target.nowait = True
    assert target.nowait
    target.nowait = False
    assert not target.nowait

    with pytest.raises(TypeError) as excinfo:
        target.nowait = 1
    assert ("The OMPTargetDirective nowait clause must be a bool, "
            "but value '1' has been given." in str(excinfo.value))


# Test OMPDeclareTargetDirective

def test_omp_declare_target_directive_constructor_and_strings(monkeypatch):
    ''' Test the OMPDeclareTargetDirective constructor and its output
    strings.'''
    target = OMPDeclareTargetDirective()
    assert target.begin_string() == "omp declare target"
    assert str(target) == "OMPDeclareTargetDirective[]"


def test_omp_declare_target_directive_validate_global_constraints():
    ''' Test the OMPDeclareTargetDirective is only valid as the first child
    of a Routine'''
    target = OMPDeclareTargetDirective()

    # If the directive is detached it passes the validation
    target.validate_global_constraints()

    # If it is the child 0 of a Routine it passes the tests
    subroutine = Routine.create("test")
    subroutine.addchild(target)
    target.validate_global_constraints()

    subroutine.children.insert(0, target.copy())
    with pytest.raises(GenerationError) as err:
        target.validate_global_constraints()
    assert ("A OMPDeclareTargetDirective must be the first child (index 0) of "
            "a Routine but found one as child 1 of a Routine."
            in str(err.value))


# Test OMPTeamsLoopDirective

def test_omp_teamsloop_directive_constructor_and_strings():
    ''' Test the OMPTeamsLoopDirective constructor and its output strings.'''
    omploop = OMPTeamsLoopDirective()
    assert omploop.begin_string() == "omp teams loop"
    assert omploop.end_string() == "omp end teams loop"
    assert str(omploop) == "OMPTeamsLoopDirective[]"
    assert omploop.collapse is None

    omploop = OMPTeamsLoopDirective(collapse=4)
    assert omploop.collapse == 4
    assert omploop.begin_string() == "omp teams loop collapse(4)"
    assert omploop.end_string() == "omp end teams loop"
    assert str(omploop) == "OMPTeamsLoopDirective[collapse=4]"


def test_omp_teamsloop_nowait_setter():
    ''' Test the nowait property setter on OMPTeamsLoopDirective.'''
    ompdo = OMPTeamsLoopDirective()
    ompdo.nowait = True

    with pytest.raises(TypeError) as err:
        ompdo.nowait = "string"
    assert ("The OMPTeamsLoopDirective nowait clause must be a bool, "
            "but value 'string' has been given." in str(err.value))


# Test OMPLoopDirective

def test_omp_loop_directive_constructor_and_strings():
    ''' Test the OMPLoopDirective constructor and its output strings.'''
    omploop = OMPLoopDirective()
    assert omploop.begin_string() == "omp loop"
    assert omploop.end_string() == "omp end loop"
    assert str(omploop) == "OMPLoopDirective[]"
    assert omploop.collapse is None

    omploop = OMPLoopDirective(collapse=4)
    assert omploop.collapse == 4
    assert omploop.begin_string() == "omp loop collapse(4)"
    assert omploop.end_string() == "omp end loop"
    assert str(omploop) == "OMPLoopDirective[collapse=4]"

    omploop = OMPLoopDirective(nowait=True)
    assert omploop.nowait
    assert omploop.begin_string() == "omp loop nowait"


def test_omp_loop_nowait_setter():
    ''' Test the nowait property setter on OMPLoopDirective.'''
    ompdo = OMPLoopDirective()
    ompdo.nowait = True

    with pytest.raises(TypeError) as err:
        ompdo.nowait = "string"
    assert ("The OMPLoopDirective nowait clause must be a bool, "
            "but value 'string' has been given." in str(err.value))


def test_omp_loop_directive_collapse_getter_and_setter():
    ''' Test the OMPLoopDirective collapse property setter and getter.'''
    target = OMPLoopDirective()
    assert target.collapse is None
    target.collapse = 3
    assert target.collapse == 3
    target.collapse = None
    assert target.collapse is None

    with pytest.raises(ValueError) as err:
        target.collapse = 0
    assert ("The OMPLoopDirective collapse clause must be a positive integer "
            "or None, but value '0' has been given." in str(err.value))

    with pytest.raises(TypeError) as err:
        target.collapse = 'a'
    assert ("The OMPLoopDirective collapse clause must be a positive integer "
            "or None, but value 'a' has been given." in str(err.value))


def test_omp_loop_directive_validate_global_constraints():
    ''' Test the OMPLoopDirective contains valid children and have as many
    immediate loops as specified by the collapse clause'''

    # Check an empty OMPLoop
    schedule = Schedule()
    omploop = OMPLoopDirective()
    schedule.addchild(omploop)
    with pytest.raises(GenerationError) as err:
        omploop.validate_global_constraints()
    assert ("OMPLoopDirective must have exactly one child in its associated"
            " schedule but found []." in str(err.value))

    # Check an OMPLoop attached to a non-loop statement
    variable = schedule.symbol_table.new_symbol("i", symbol_type=DataSymbol,
                                                datatype=INTEGER_TYPE)
    stmt = Assignment.create(Reference(variable), Literal('4', INTEGER_TYPE))
    omploop.dir_body.addchild(stmt)
    with pytest.raises(GenerationError) as err:
        omploop.validate_global_constraints()
    assert ("OMPLoopDirective must have a Loop as child of its associated "
            "schedule but found 'Assignment" in str(err.value))

    # Check with an OMPLoop and a single Loop inside but without a proper
    # region ancestor
    stmt.detach()
    loop = Loop.create(variable,
                       Literal('1', INTEGER_TYPE),
                       Literal('10', INTEGER_TYPE),
                       Literal('1', INTEGER_TYPE),
                       [stmt])
    omploop.dir_body.addchild(loop)
    with pytest.raises(GenerationError) as err:
        omploop.validate_global_constraints()
    assert ("OMPLoopDirective must be inside a OMPTargetDirective or a "
            "OMPParallelDirective, but 'OMPLoopDirective[]' is not."
            in str(err.value))

    # Insert block in a OMP Parallel region
    ompparallel = OMPParallelDirective()
    omploop.replace_with(ompparallel)
    ompparallel.dir_body.addchild(omploop)

    # Check with an OMPLoop and collapse is 2 but just one loop inside
    omploop.collapse = 2
    with pytest.raises(GenerationError) as err:
        omploop.validate_global_constraints()
    assert ("OMPLoopDirective must have as many immediately nested loops as "
            "the collapse clause specifies but 'OMPLoopDirective[collapse=2]'"
            " has a collapse=2 and the nested statement at depth 1 is a "
            "Assignment rather than a Loop."
            in str(err.value))

    # Check with an OMPLoop and collapse is 2 and 2 nested loops inside
    loop2 = loop.copy()
    loop.loop_body.children[0].replace_with(loop2)
    omploop.validate_global_constraints()  # This is valid


def test_omploop_equality():
    ''' Test the __eq__ method of OMPLoopDirective. '''
    # We need to manually set the same SymbolTable instance in both directives
    # for their equality to be True
    symboltable = SymbolTable()
    # Set up the symbols
    tmp = DataSymbol("tmp", REAL_SINGLE_TYPE)
    i_sym = DataSymbol("i", REAL_SINGLE_TYPE)

    # Create two equal loops
    loop_sym = DataSymbol("i", INTEGER_SINGLE_TYPE)
    sched1 = Schedule(symbol_table=symboltable)
    start = Literal("0", INTEGER_SINGLE_TYPE)
    stop = Literal("1", INTEGER_SINGLE_TYPE)
    step = Literal("1", INTEGER_SINGLE_TYPE)
    child_node = Assignment.create(
        Reference(tmp),
        Reference(i_sym))
    sched1.addchild(child_node)
    loop1 = Loop.create(loop_sym,
                        start, stop, step, [])
    loop1.children[3].detach()
    loop1.addchild(sched1, 3)
    start2 = start.copy()
    stop2 = stop.copy()
    step2 = step.copy()
    sched2 = Schedule()
    # Make sure it has the same ST instance, providing it as a constructor
    # parameter would create a copy and not use the same instance.
    sched2._symbol_table = symboltable
    child_node2 = Assignment.create(
        Reference(tmp),
        Reference(i_sym))
    sched2.addchild(child_node2)
    loop2 = Loop.create(loop_sym,
                        start2, stop2, step2, [])
    loop2.children[3].detach()
    loop2.addchild(sched2, 3)

    omploop1 = OMPLoopDirective(children=[loop1])
    omploop2 = OMPLoopDirective(children=[loop2])
    omploop1.children[0]._symbol_table = symboltable
    omploop2.children[0]._symbol_table = symboltable
    assert omploop1 == omploop2

    omploop1.collapse = 2
    assert omploop1 != omploop2


def test_omp_atomics_is_valid_atomic_statement(fortran_reader):
    ''' Test the OMPAtomicDirective can identify when a statement is a valid
    expression to support OpenMP atomics. '''

    code = '''
    subroutine my_subroutine()
        integer, dimension(10, 10) :: A = 1
        integer, dimension(10, 10) :: B = 2
        integer :: i, j, val

        A(1,1) = A(1,1) * 2
        A(1,1) = A(1,1) / (2 + 3 - 5)
        A(1,1) = MAX(A(1,1), A(1,2))
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    for stmt in tree.walk(Assignment):
        assert OMPAtomicDirective.is_valid_atomic_statement(stmt)

    code = '''
    subroutine my_subroutine()
        integer, dimension(10, 10) :: A = 1
        integer, dimension(10, 10) :: B = 2
        integer :: i, j, val

        A(1,1) = A(1,1) ** 2  ! Operator is not supported
        A(:,1) = A(:,1) / 2      ! It is not a scalar expression
        A(1,1) = MOD(A(1,1), 3)  ! Intrinsic is not supported
        return
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    for stmt in tree.walk(Assignment):
        assert not OMPAtomicDirective.is_valid_atomic_statement(stmt)

    # Its also not valid if its not an Assignment
    assert not OMPAtomicDirective.is_valid_atomic_statement(Return())


def test_omp_atomics_validate_global_constraints(fortran_reader, monkeypatch):
    ''' Test the OMPAtomicDirective can check the globals constraints to
    validate that the directive is correctly formed.'''

    code = '''
    subroutine my_subroutine()
        integer, dimension(10, 10) :: A = 1
        integer, dimension(10, 10) :: B = 2
        integer :: i, j, val

        A(1,1) = A(1,1) * 2
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    routine = tree.walk(Routine)[0]
    stmt = routine.children[0]
    atomic = OMPAtomicDirective()
    atomic.dir_body.addchild(stmt.detach())
    routine.addchild(atomic)

    # This is a valid atomic
    atomic.validate_global_constraints()

    # If the statement is invalid (for any reason already tested in a previous
    # test), it raises an error
    monkeypatch.setattr(atomic, "is_valid_atomic_statement", lambda _: False)
    with pytest.raises(GenerationError) as err:
        atomic.validate_global_constraints()
    assert "is not a valid OpenMP Atomic statement." in str(err.value)

    # If it does not have an associated statement
    atomic.dir_body[0].detach()
    with pytest.raises(GenerationError) as err:
        atomic.validate_global_constraints()
    assert ("Atomic directives must always have one and only one associated "
            "statement, but found: " in str(err.value))


def test_omp_atomic_init_failure():
    ''' Test the OMPAtomicDirective init routine fails when provided an
    invalid directive_type.'''
    with pytest.raises(TypeError) as excinfo:
        _ = OMPAtomicDirective(directive_type=1)

    assert ("OMPAtomicDirective expects an AtomicDirectiveType as the "
            "directive_type but found 1." in str(excinfo.value))


def test_omp_atomics_strings():
    ''' Test the OMPAtomicDirective begin and end strings '''
    atomic = OMPAtomicDirective()
    assert atomic.begin_string() == "omp atomic update"
    assert atomic.end_string() == "omp end atomic"

    atomic_read = OMPAtomicDirective(
            directive_type=AtomicDirectiveType.READ
    )
    assert atomic_read.begin_string() == "omp atomic read"

    atomic_write = OMPAtomicDirective(
            directive_type=AtomicDirectiveType.WRITE
    )
    assert atomic_write.begin_string() == "omp atomic write"
    atomic_capture = OMPAtomicDirective(
            directive_type=AtomicDirectiveType.CAPTURE
    )
    assert atomic_capture.begin_string() == "omp atomic capture"


def test_omp_simd_strings():
    ''' Test the OMPAtomicDirective begin and end strings '''
    atomic = OMPSimdDirective()
    assert atomic.begin_string() == "omp simd"
    assert atomic.end_string() == "omp end simd"


def test_omp_simd_validate_global_constraints(fortran_reader):
    ''' Test the OMPSimdDirective can check the globals constraints to
    validate that the directive is correctly formed.'''

    code = '''
    subroutine my_subroutine()
        integer, dimension(10, 10) :: A = 1
        integer, dimension(10, 10) :: B = 2
        integer :: i, j, val

        A(1,1) = A(1,1) * 2
        do i = 1, 10
            A(i,1) = 3
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    routine = tree.walk(Routine)[0]
    stmt = routine.children[0]
    simd = OMPSimdDirective()
    simd.dir_body.addchild(stmt.detach())
    routine.children.insert(0, simd)

    # If it does not have an associated loop
    with pytest.raises(GenerationError) as err:
        simd.validate_global_constraints()
    assert ("The OMP SIMD directives must always have one and only one "
            "associated loop, but found: " in str(err.value))

    # If it doesn not have an associated node at all
    simd.dir_body[0].detach()
    with pytest.raises(GenerationError) as err:
        simd.validate_global_constraints()
    assert ("The OMP SIMD directives must always have one and only one "
            "associated loop, but found: " in str(err.value))

    stmt = routine.children[1]
    simd = OMPSimdDirective()
    simd.dir_body.addchild(stmt.detach())
    routine.addchild(simd)

    # This is a valid OMPSimd expression
    simd.validate_global_constraints()


def test_omp_serial_valid_dependence_literals():
    '''
    Tests the _valid_dependence_literals function in OMPSerialDirective
    '''
    sing = OMPSingleDirective()
    lit1 = Literal("0", INTEGER_SINGLE_TYPE)
    lit2 = Literal("1", INTEGER_SINGLE_TYPE)
    assert sing._valid_dependence_literals(lit1, lit2) is True
    tmp = DataSymbol("tmp", REAL_SINGLE_TYPE)
    ref = Reference(tmp)
    assert sing._valid_dependence_literals(lit1, ref) is False


def test_omp_serial_valid_dependence_ranges():
    '''
    Tests the _valid_dependence_ranges function in OMPSerialDirective
    '''
    sing = OMPSingleDirective()
    one = Literal("1", INTEGER_SINGLE_TYPE)

    # Create an ArrayType
    array_type = ArrayType(INTEGER_SINGLE_TYPE, [100])
    tmp = DataSymbol("tmp", array_type)
    reference = Reference(tmp)

    ref = ArrayReference.create(tmp, [one.copy()])

    lbound = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.LBOUND,
        [reference.copy(), ("dim", one.copy())]
    )
    ubound = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.UBOUND,
        [reference.copy(), ("dim", one.copy())]
    )
    my_range = Range.create(lbound.copy(), ubound.copy(), one.copy())
    array_reference = ArrayReference.create(tmp, [my_range])
    array_reference_2 = ArrayReference.create(tmp, [my_range.copy()])

    # Valid run
    assert (sing._valid_dependence_ranges(array_reference,
                                          array_reference_2, 0)
            is True)

    assert sing._valid_dependence_ranges(array_reference, ref, 0) is False


def test_omp_serial_compute_accesses_bad_binop():
    '''
    Tests the first set of failure cases for _compute_accesses
    in OMPSerialDirective
    '''
    # A lot to do here, across multiple tests.
    # Inputs are a reference or Binop, a list of nodes preceding a task,
    # and that task.

    # Fail conditions:
    # 1. Literal OP Reference BinaryOperation where the operator is not ADD
    # 2. Reference OP Literal BinaryOperation where the operator is not
    # ADD or SUB
    # 3&4. BinaryOperation OP Refrence BinaryOperation where the sub
    # BinaryOperation is not a Literal MUL Literal
    # 5. BinaryOperation OP Reference where OP is not ADD
    # 6&7. Reference ADD BinaryOperation where the sub BinaryOperation is not
    # a Literal MUL Literal.
    # 8&9. Reference SUB BinaryOperation where the sub BinaryOperation is not
    # a Literal MUL Literal.
    # 10. Referebce OP BinaryOperation where the OP is not ADD or SUB.
    # 11. Reference OP Non-Literal/BinaryOperation
    # 12. Non-Literal/Reference/Binop OP ...
    # 13. BinaryOperation OP Reference where the Binop has non Literal child

    # Fail conditions 1-12 we can just test with only Ref as the input,
    # the others are a bit more complex.
    sing = OMPSingleDirective()
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)

    binop_fail1 = BinaryOperation.create(
        BinaryOperation.Operator.MUL,
        Literal("1", INTEGER_SINGLE_TYPE),
        Reference(tmp),
    )

    with pytest.raises(UnresolvedDependencyError) as excinfo:
        sing._compute_accesses(binop_fail1, [], None)
    assert (
        "Found a dependency index that is "
        "a BinaryOperation where the "
        "format is Literal OP Reference "
        "with a non-ADD operand "
        "which is not supported." in str(excinfo.value)
    )

    binop_fail2 = BinaryOperation.create(
        BinaryOperation.Operator.MUL,
        Reference(tmp),
        Literal("1", INTEGER_SINGLE_TYPE),
    )
    with pytest.raises(UnresolvedDependencyError) as excinfo:
        sing._compute_accesses(binop_fail2, [], None)
    assert (
        "Found a dependency index that is "
        "a BinaryOperation where the "
        "Operator is neither ADD not SUB "
        "which is not supported." in str(excinfo.value)
    )

    sub_binop1 = BinaryOperation.create(
        BinaryOperation.Operator.ADD,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
    )
    binop_fail3 = BinaryOperation.create(
        BinaryOperation.Operator.ADD, sub_binop1.copy(), Reference(tmp)
    )
    with pytest.raises(UnresolvedDependencyError) as excinfo:
        sing._compute_accesses(binop_fail3, [], None)
    assert (
        "Found a dependency index that is a "
        "BinaryOperation with a child "
        "BinaryOperation with a non-MUL operator "
        "which is not supported." in str(excinfo.value)
    )

    sub_binop2 = BinaryOperation.create(
        BinaryOperation.Operator.MUL,
        Literal("1", INTEGER_SINGLE_TYPE),
        Reference(tmp),
    )
    binop_fail4 = BinaryOperation.create(
        BinaryOperation.Operator.ADD, sub_binop2, Reference(tmp)
    )
    with pytest.raises(UnresolvedDependencyError) as excinfo:
        sing._compute_accesses(binop_fail4, [], None)
    assert (
        "Found a dependency index that is a BinaryOperation with a child "
        "BinaryOperation with a non-Literal child which is not supported."
        in str(excinfo.value)
    )

    sub_binop3 = BinaryOperation.create(
        BinaryOperation.Operator.MUL,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
    )
    binop_fail5 = BinaryOperation.create(
        BinaryOperation.Operator.SUB, sub_binop3, Reference(tmp)
    )
    with pytest.raises(UnresolvedDependencyError) as excinfo:
        sing._compute_accesses(binop_fail5, [], None)
    assert (
        "Found a dependency index that is "
        "a BinaryOperation where the "
        "format is BinaryOperator OP "
        "Reference with a non-ADD operand " in str(excinfo.value)
    )

    binop_fail6 = BinaryOperation.create(
        BinaryOperation.Operator.ADD, Reference(tmp), sub_binop1.copy()
    )
    with pytest.raises(UnresolvedDependencyError) as excinfo:
        sing._compute_accesses(binop_fail6, [], None)
    assert (
        "Found a dependency index that is a BinaryOperation with a child "
        "BinaryOperation with a non-MUL operator which is not supported."
        in str(excinfo.value)
    )

    binop_fail7 = BinaryOperation.create(
        BinaryOperation.Operator.SUB, Reference(tmp), sub_binop2.copy()
    )
    with pytest.raises(UnresolvedDependencyError) as excinfo:
        sing._compute_accesses(binop_fail7, [], None)
    assert (
        "Found a dependency index that is a BinaryOperation with an operand "
        "BinaryOperation with a non-Literal operand which is not supported."
        in str(excinfo.value)
    )

    binop_fail8 = BinaryOperation.create(
        BinaryOperation.Operator.SUB, Reference(tmp), sub_binop1.copy()
    )
    with pytest.raises(UnresolvedDependencyError) as excinfo:
        sing._compute_accesses(binop_fail8, [], None)
    print(str(excinfo.value))
    assert (
        "Found a dependency index that is a BinaryOperation with a child "
        "BinaryOperation with a non-MUL operator which is not supported."
        in str(excinfo.value)
    )

    binop_fail9 = BinaryOperation.create(
        BinaryOperation.Operator.SUB, Reference(tmp), sub_binop2.copy()
    )
    with pytest.raises(UnresolvedDependencyError) as excinfo:
        sing._compute_accesses(binop_fail9, [], None)
    assert (
        "Found a dependency index that is a BinaryOperation with an operand "
        "BinaryOperation with a non-Literal operand which is not supported."
        in str(excinfo.value)
    )

    binop_fail10 = BinaryOperation.create(
        BinaryOperation.Operator.MUL, Reference(tmp), sub_binop3.copy()
    )
    with pytest.raises(UnresolvedDependencyError) as excinfo:
        sing._compute_accesses(binop_fail10, [], None)
    assert (
        "Found a dependency index that is a BinaryOperation where the "
        "format is Reference OP BinaryOperation with a non-ADD, non-SUB "
        "operand which is not supported." in str(excinfo.value)
    )

    binop_fail11 = BinaryOperation.create(
        BinaryOperation.Operator.ADD, Reference(tmp), Reference(tmp)
    )
    with pytest.raises(UnresolvedDependencyError) as excinfo:
        sing._compute_accesses(binop_fail11, [], None)
    assert (
        "Found a dependency index that is a BinaryOperation where neither "
        "child is a Literal or BinaryOperation. PSyclone can't validate "
        "this dependency." in str(excinfo.value)
    )

    binop_fail12 = BinaryOperation.create(
        BinaryOperation.Operator.ADD,
        Call.create(RoutineSymbol("mycall")),
        Reference(tmp),
    )
    with pytest.raises(UnresolvedDependencyError) as excinfo:
        sing._compute_accesses(binop_fail12, [], None)
    assert (
        "Found a dependency index that is a "
        "BinaryOperation where neither child "
        "is a Literal or BinaryOperation. "
        "PSyclone can't validate "
        "this dependency." in str(excinfo.value)
    )

    binop_fail13 = BinaryOperation.create(
        BinaryOperation.Operator.ADD, Reference(tmp), sub_binop2.copy()
    )
    with pytest.raises(UnresolvedDependencyError) as excinfo:
        sing._compute_accesses(binop_fail13, [], None)
    assert (
        "Found a dependency index that is a BinaryOperation with an operand "
        "BinaryOperation with a non-Literal operand which is not supported."
        in str(excinfo.value)
    )


def test_omp_serial_compute_accesses_other_fails():
    '''
    Tests more failure cases of _compute_accesses function in
    OMPSerialDirective
    '''
    sing = OMPSingleDirective()
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)
    ref = Reference(tmp)
    # 13. If preceding_nodes contains a Call.
    # 14. If we have an access to a previous Loop variable that is not an
    # ancestor of the supplied task.
    # 15. Binaryop access to Loop variable with non-Literal step
    # (Line 435, i think this is the cause?)
    # 16. Assignment to a index of a Loop variable
    # with non-Literal step (Line 468)
    correct_binop = BinaryOperation.create(
        BinaryOperation.Operator.ADD,
        Reference(tmp),
        Literal("1", INTEGER_SINGLE_TYPE),
    )

    call_fail = Call.create(RoutineSymbol("mycall"))
    with pytest.raises(UnresolvedDependencyError) as excinfo:
        sing._compute_accesses(correct_binop, [call_fail], None)
    assert (
        "Found a Call in preceding_nodes, which is not yet supported."
        in str(excinfo.value)
    )

    # Create a task, and a Loop where the loop variable is tmp
    task = OMPTaskDirective()
    loop1 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("2", INTEGER_SINGLE_TYPE),
        Literal("3", INTEGER_SINGLE_TYPE),
        [],
    )
    with pytest.raises(UnresolvedDependencyError) as excinfo:
        sing._compute_accesses(ref, [loop1], task)
    assert (
        "Found a dependency index that was updated as a Loop variable "
        "that is not an ancestor Loop of the task. The variable is 'tmp'"
        in str(excinfo.value)
    )

    task2 = task.copy()
    loop2 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("2", INTEGER_SINGLE_TYPE),
        ref.copy(),
        [task2],
    )

    with pytest.raises(UnresolvedDependencyError) as excinfo:
        sing._compute_accesses(correct_binop, [loop2], task2)
    assert (
        "Found a dependency index that is a Loop variable with a non-"
        "Literal step which we can't resolve in PSyclone."
        in str(excinfo.value)
    )

    assign = Assignment.create(ref.copy(), Literal("1", INTEGER_SINGLE_TYPE))
    with pytest.raises(UnresolvedDependencyError) as excinfo:
        sing._compute_accesses(correct_binop, [assign], task2)
    assert (
        "Found a dependency between a BinaryOperation "
        "and a previously set constant value. PSyclone "
        "cannot yet handle this interaction." in str(excinfo.value)
    )

    with pytest.raises(UnresolvedDependencyError) as excinfo:
        sing._compute_accesses(ref, [loop2], task2)
    assert (
        "Found a dependency index that is a Loop variable with a non-"
        "Literal step which we can't resolve in PSyclone."
        in str(excinfo.value)
    )


def test_omp_serial_compute_accesses_results():
    '''
    Tests the _compute_accesses fucntion in OMPSerialDirective
    '''
    # First result output, BinaryOperation with all Literal Loop
    sing = OMPSingleDirective()
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)
    tmp3 = DataSymbol("tmp3", INTEGER_SINGLE_TYPE)
    ref = Reference(tmp)
    task = OMPTaskDirective()
    loop = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("64", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task],
    )

    binop = BinaryOperation.create(
        BinaryOperation.Operator.ADD,
        ref.copy(),
        Literal("1", INTEGER_SINGLE_TYPE),
    )

    res = sing._compute_accesses(binop, [loop], task)
    assert len(res) == 2
    assert isinstance(res[0], Literal)
    assert res[0].value == "2"
    assert isinstance(res[1], Literal)
    assert res[1].value == "34"

    # Second result output, BinaryOpeartion with reference start Loop
    tmp2 = DataSymbol("tmp2", INTEGER_SINGLE_TYPE)
    task = task.copy()
    loop = Loop.create(
        tmp,
        Reference(tmp2),
        Literal("256", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task],
    )

    res = sing._compute_accesses(binop, [loop], task)
    assert len(res) == 3
    assert isinstance(res, dict)
    assert isinstance(res["start"], BinaryOperation)
    assert res["start"].operator == BinaryOperation.Operator.ADD
    assert isinstance(res["start"].children[0], Reference)
    assert res["start"].children[0].symbol == tmp2
    assert isinstance(res["start"].children[1], Literal)
    assert res["start"].children[1].value == "1"

    assert isinstance(res["stop"], Literal)
    assert res["stop"].value == "256"

    assert isinstance(res["step"], Literal)
    assert res["step"].value == "32"

    # Third result output, Reference to Assignment
    assign = Assignment.create(Reference(tmp), Reference(tmp2))
    res = sing._compute_accesses(Reference(tmp), [assign], task)
    assert len(res) == 1
    assert isinstance(res[0], Reference)
    assert res[0].symbol == tmp2

    # Fifth result output, Reference access to loop with non Literal start
    res = sing._compute_accesses(Reference(tmp), [loop], task)
    assert len(res) == 3
    assert isinstance(res, dict)
    assert isinstance(res["start"], Reference)
    assert res["start"].symbol == tmp2
    assert isinstance(res["stop"], Literal)
    assert res["stop"].value == "256"
    assert isinstance(res["step"], Literal)
    assert res["step"].value == "32"

    # Fourth result output, Reference access to loop with all Literals
    task = task.copy()
    loop = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task],
    )
    res = sing._compute_accesses(Reference(tmp), [loop], task)
    assert len(res) == 4
    assert isinstance(res[0], Literal)
    assert res[0].value == "1"
    assert isinstance(res[1], Literal)
    assert res[1].value == "33"
    assert isinstance(res[2], Literal)
    assert res[2].value == "65"
    assert isinstance(res[3], Literal)
    assert res[3].value == "97"

    # Finally cover some code which were missed by these tests so far
    task = OMPTaskDirective()
    loop = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("64", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task],
    )

    binop = BinaryOperation.create(
        BinaryOperation.Operator.ADD,
        Literal("1", INTEGER_SINGLE_TYPE),
        ref.copy(),
    )
    res = sing._compute_accesses(binop, [loop], task)
    assert len(res) == 2
    assert isinstance(res[0], Literal)
    assert res[0].value == "2"
    assert isinstance(res[1], Literal)
    assert res[1].value == "34"

    task = OMPTaskDirective()
    loop = Loop.create(
        tmp,
        Literal("3", INTEGER_SINGLE_TYPE),
        Literal("64", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task],
    )

    binop = BinaryOperation.create(
        BinaryOperation.Operator.SUB,
        ref.copy(),
        Literal("1", INTEGER_SINGLE_TYPE),
    )

    res = sing._compute_accesses(binop, [loop], task)
    assert len(res) == 2
    assert isinstance(res[0], Literal)
    assert res[0].value == "2"
    assert isinstance(res[1], Literal)
    assert res[1].value == "34"

    task = OMPTaskDirective()
    loop = Loop.create(
        tmp,
        Reference(tmp2),
        Literal("1028", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task],
    )
    binop = BinaryOperation.create(
        BinaryOperation.Operator.MUL,
        Literal("3", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
    )
    binop2 = BinaryOperation.create(
        BinaryOperation.Operator.ADD, binop, ref.copy()
    )
    res = sing._compute_accesses(binop2, [loop], task)
    assert len(res) == 3
    assert isinstance(res, dict)
    assert isinstance(res["start"], BinaryOperation)
    assert res["start"].children[0].symbol == tmp2
    assert res["start"].children[1].value == "32"
    assert isinstance(res["stop"], Literal)
    assert res["stop"].value == "1028"
    assert isinstance(res["step"], Literal)
    assert res["step"].value == "32"

    task = OMPTaskDirective()
    loop = Loop.create(
        tmp,
        Reference(tmp2),
        Literal("1028", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task],
    )
    binop = BinaryOperation.create(
        BinaryOperation.Operator.MUL,
        Literal("3", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
    )
    binop2 = BinaryOperation.create(
        BinaryOperation.Operator.SUB, ref.copy(), binop
    )
    res = sing._compute_accesses(binop2, [loop], task)
    assert len(res) == 3
    assert isinstance(res, dict)
    assert isinstance(res["start"], BinaryOperation)
    assert res["start"].children[0].symbol == tmp2
    assert res["start"].children[1].value == "-32"
    assert isinstance(res["stop"], Literal)
    assert res["stop"].value == "1028"
    assert isinstance(res["step"], Literal)
    assert res["step"].value == "32"

    task = OMPTaskDirective()
    loop = Loop.create(
        tmp,
        Reference(tmp2),
        Literal("1028", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task],
    )
    binop = BinaryOperation.create(
        BinaryOperation.Operator.MUL,
        Literal("3", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
    )
    binop2 = BinaryOperation.create(
        BinaryOperation.Operator.ADD, ref.copy(), binop
    )
    res = sing._compute_accesses(binop2, [loop], task)
    assert len(res) == 3
    assert isinstance(res, dict)
    assert isinstance(res["start"], BinaryOperation)
    assert res["start"].children[0].symbol == tmp2
    assert res["start"].children[1].value == "32"
    assert isinstance(res["stop"], Literal)
    assert res["stop"].value == "1028"
    assert isinstance(res["step"], Literal)
    assert res["step"].value == "32"

    task = OMPTaskDirective()
    loop = Loop.create(
        tmp3,
        Reference(tmp2),
        Literal("1028", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task],
    )
    loop2 = Loop.create(
        tmp,
        Reference(tmp2),
        Literal("1028", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [loop],
    )
    binop = BinaryOperation.create(
        BinaryOperation.Operator.MUL,
        Literal("3", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
    )
    binop2 = BinaryOperation.create(
        BinaryOperation.Operator.ADD, ref.copy(), binop
    )
    res = sing._compute_accesses(binop2, [loop, loop2], task)
    assert len(res) == 3
    assert isinstance(res, dict)
    assert isinstance(res["start"], BinaryOperation)
    assert res["start"].children[0].symbol == tmp2
    assert res["start"].children[1].value == "32"
    assert isinstance(res["stop"], Literal)
    assert res["stop"].value == "1028"
    assert isinstance(res["step"], Literal)
    assert res["step"].value == "32"


def test_omp_serial_valid_dependence_ref_binop_dict_cases():
    '''
    Tests the _valid_dependence_ref_binop cases when either of the computed
    accesses is a dict.
    '''
    sing = OMPSingleDirective()
    start = DataSymbol("start", INTEGER_SINGLE_TYPE)
    stop = DataSymbol("stop", INTEGER_SINGLE_TYPE)
    outer_var = DataSymbol("outvar", INTEGER_SINGLE_TYPE)
    task = OMPTaskDirective()
    task2 = OMPTaskDirective()

    # Check the case where only 1 of the references results in a
    # dict output from compute_accesses returns False
    Loop.create(
        outer_var,
        Reference(start),
        Reference(stop),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task]
    )
    Loop.create(
        outer_var,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2]
    )
    ref1 = Reference(outer_var)
    ref2 = Reference(outer_var)
    assert (sing._valid_dependence_ref_binop(ref1, ref2, task, task2)
            is False)

    # Check the case where the steps are not equal
    task2 = OMPTaskDirective()
    Loop.create(
        outer_var,
        Reference(start),
        Reference(stop),
        Literal("33", INTEGER_SINGLE_TYPE),
        [task2]
    )
    assert (sing._valid_dependence_ref_binop(ref1, ref2, task, task2)
            is False)

    # Check the case where the starts offset is not an integer multiple
    # of the step.
    task2 = OMPTaskDirective()
    Loop.create(
        outer_var,
        BinaryOperation.create(
            BinaryOperation.Operator.ADD,
            Reference(start),
            Literal("16", INTEGER_SINGLE_TYPE)
        ),
        Reference(stop),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2]
    )
    assert (sing._valid_dependence_ref_binop(ref1, ref2, task, task2)
            is False)


def test_omp_serial_valid_dependence_ref_binop_fails():
    '''
    Tests the _valid_dependence_ref_binop failure cases of OMPSerialDirective
    '''

    # Case when ref_accesses raises an error
    sing = OMPSingleDirective()
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)
    tmp2 = DataSymbol("tmp2", INTEGER_SINGLE_TYPE)

    binop_fail1 = BinaryOperation.create(
        BinaryOperation.Operator.MUL,
        Literal("1", INTEGER_SINGLE_TYPE),
        Reference(tmp),
    )
    task = OMPTaskDirective()
    task2 = OMPTaskDirective()
    Loop.create(
        tmp,
        Reference(tmp2),
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )
    assert (sing._valid_dependence_ref_binop(binop_fail1, None, task2, task)
            is False)

    # Test the first failure. One has accesses to references
    # and the other doesn't.

    sing = OMPSingleDirective()
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)
    tmp2 = DataSymbol("tmp2", INTEGER_SINGLE_TYPE)
    ref = Reference(tmp)
    task = OMPTaskDirective()
    task2 = OMPTaskDirective()
    binop = BinaryOperation.create(
        BinaryOperation.Operator.ADD,
        ref.copy(),
        Literal("1", INTEGER_SINGLE_TYPE),
    )
    Loop.create(
        tmp,
        Reference(tmp2),
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )
    loop = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task],
    )

    assert sing._valid_dependence_ref_binop(ref, binop, task, task2) is False

    assert sing._valid_dependence_ref_binop(binop, ref, task2, task) is False

    ref2 = Reference(tmp2)
    task3 = OMPTaskDirective()
    tmp3 = DataSymbol("tmp3", INTEGER_SINGLE_TYPE)
    tmp4 = DataSymbol("tmp4", INTEGER_SINGLE_TYPE)
    Loop.create(
        tmp2,
        Reference(tmp3),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task3],
    )
    loop.children[3].pop_all_children()
    Loop.create(
        tmp,
        Reference(tmp4),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task],
    )
    assert sing._valid_dependence_ref_binop(ref, ref2, task, task3) is False

    task2 = OMPTaskDirective()
    task = OMPTaskDirective()
    Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task],
    )
    binop = BinaryOperation.create(
        BinaryOperation.Operator.ADD,
        ref.copy(),
        Literal("1", INTEGER_SINGLE_TYPE),
    )
    Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )

    assert sing._valid_dependence_ref_binop(ref, binop, task, task2) is False

    task = OMPTaskDirective()
    task2 = OMPTaskDirective()
    Loop.create(
        tmp,
        Reference(tmp2),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task],
    )
    Loop.create(
        tmp,
        Reference(tmp2),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )

    assert sing._valid_dependence_ref_binop(ref, binop, task, task2) is False


def test_omp_serial_valid_dependence_ref_binops():
    '''
    Test the _valid_dependence_ref_binops function of OMPSerialDirective.
    '''
    sing = OMPSingleDirective()
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)
    tmp2 = DataSymbol("tmp2", INTEGER_SINGLE_TYPE)
    ref = Reference(tmp)
    task = OMPTaskDirective()
    task2 = OMPTaskDirective()

    Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task],
    )
    binop = BinaryOperation.create(
        BinaryOperation.Operator.ADD,
        ref.copy(),
        Literal("1", INTEGER_SINGLE_TYPE),
    )
    Loop.create(
        tmp,
        Literal("0", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )

    sing._valid_dependence_ref_binop(ref, binop, task, task2)

    task = OMPTaskDirective()
    task2 = OMPTaskDirective()
    Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("16", INTEGER_SINGLE_TYPE),
        [task],
    )
    binop = BinaryOperation.create(
        BinaryOperation.Operator.ADD,
        ref.copy(),
        Literal("16", INTEGER_SINGLE_TYPE),
    )
    Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )

    sing._valid_dependence_ref_binop(ref, binop, task, task2)

    task = OMPTaskDirective()
    task2 = OMPTaskDirective()
    Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task],
    )
    binop = BinaryOperation.create(
        BinaryOperation.Operator.ADD,
        ref.copy(),
        Literal("1", INTEGER_SINGLE_TYPE),
    )
    Loop.create(
        tmp,
        Literal("129", INTEGER_SINGLE_TYPE),
        Literal("256", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )

    sing._valid_dependence_ref_binop(ref, binop, task, task2)

    task = OMPTaskDirective()
    task2 = OMPTaskDirective()
    Loop.create(
        tmp,
        Reference(tmp2),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task],
    )
    Loop.create(
        tmp,
        Reference(tmp2),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )
    sing._valid_dependence_ref_binop(ref, ref.copy(), task, task2)
    sing._valid_dependence_ref_binop(binop, binop.copy(), task, task2)


def test_omp_serial_validate_task_dependencies_fails():
    '''
    Test the fail cases of the check_task_dependencies function of
    OMPSerialDirective.
    '''
    # First fail, task + taskloop
    sing = OMPSingleDirective()
    task = OMPTaskDirective()
    taskloop = OMPTaskloopDirective()
    sing.children[0].addchild(task)
    sing.children[0].addchild(taskloop)
    with pytest.raises(NotImplementedError) as excinfo:
        sing._validate_task_dependencies()
    assert (
        "OMPTaskDirectives and OMPTaskloopDirectives"
        " are not currently supported inside the "
        "same parent serial region." in str(excinfo.value)
    )


def test_omp_serial_validate_task_dependencies_outout():
    '''
    Test check_task_dependencies member of OMPSerialDirective
    for outout dependency types
    '''

    # Check outout Array dependency
    subroutine = Routine.create("testsub")
    parallel = OMPParallelDirective.create()
    subroutine.addchild(parallel)
    sing = OMPSingleDirective()
    parallel.children[0].addchild(sing)
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)
    tmp2 = DataSymbol("tmp2", INTEGER_SINGLE_TYPE)
    array_type = ArrayType(INTEGER_SINGLE_TYPE, [128, 128])
    rval = DataSymbol("rval", array_type)

    subroutine.symbol_table.add(tmp)
    subroutine.symbol_table.add(tmp2)
    subroutine.symbol_table.add(rval)

    task1 = DynamicOMPTaskDirective()
    loop1 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task1],
    )
    assign1 = Assignment.create(
        ArrayReference.create(rval, [Reference(tmp), Reference(tmp)]),
        Literal("1", INTEGER_SINGLE_TYPE),
    )
    subloop1 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign1],
    )

    task2 = DynamicOMPTaskDirective()
    loop2 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("64", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )
    assign2 = Assignment.create(
        ArrayReference.create(rval, [Reference(tmp), Reference(tmp)]),
        Literal("24", INTEGER_SINGLE_TYPE),
    )
    subloop2 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign2],
    )
    sing.children[0].addchild(loop1)
    sing.children[0].addchild(loop2)

    task1.children[0].addchild(subloop1)
    task1.lower_to_language_level()
    task2.children[0].addchild(subloop2)
    task2.lower_to_language_level()

    sing._validate_task_dependencies()

    # Check outout Reference dependency
    subroutine = Routine.create("testsub")
    parallel = OMPParallelDirective.create()
    subroutine.addchild(parallel)
    sing = OMPSingleDirective()
    parallel.children[0].addchild(sing)
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)
    tmp2 = DataSymbol("tmp2", INTEGER_SINGLE_TYPE)
    rval = DataSymbol("rval", INTEGER_SINGLE_TYPE)

    subroutine.symbol_table.add(tmp)
    subroutine.symbol_table.add(tmp2)

    subroutine.symbol_table.add(rval)

    task1 = DynamicOMPTaskDirective()
    loop1 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task1],
    )
    assign1 = Assignment.create(
        Reference(rval),
        BinaryOperation.create(
            BinaryOperation.Operator.ADD, Reference(rval), Reference(tmp2)
        ),
    )
    subloop1 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign1],
    )

    task2 = DynamicOMPTaskDirective()
    loop2 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )
    assign2 = Assignment.create(
        Reference(rval),
        BinaryOperation.create(
            BinaryOperation.Operator.ADD, Reference(rval), Reference(tmp2)
        ),
    )
    subloop2 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign2],
    )
    sing.children[0].addchild(loop1)
    sing.children[0].addchild(loop2)

    task1.children[0].addchild(subloop1)
    task1.lower_to_language_level()
    task2.children[0].addchild(subloop2)
    task2.lower_to_language_level()

    sing._validate_task_dependencies()

    # StructureType for Structure tests
    grid_type = StructureType.create(
        [
            ("nx", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
            (
                "sub_grids",
                ArrayType(INTEGER_TYPE, [3]),
                Symbol.Visibility.PUBLIC,
                None,
            ),
            (
                "data",
                ArrayType(REAL_TYPE, [128, 128]),
                Symbol.Visibility.PUBLIC,
                None,
            ),
        ]
    )
    # Check outout StructureReference non-array dependency
    subroutine = Routine.create("testsub")
    parallel = OMPParallelDirective.create()
    subroutine.addchild(parallel)
    sing = OMPSingleDirective()
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)
    tmp2 = DataSymbol("tmp2", INTEGER_SINGLE_TYPE)
    rval = DataSymbol("rval", grid_type)
    subroutine.symbol_table.add(tmp)
    subroutine.symbol_table.add(tmp2)
    subroutine.symbol_table.add(rval)

    do_dir = OMPDoDirective()
    assign1 = Assignment.create(
        Reference(tmp2), StructureReference.create(rval, ["nx"])
    )
    loop1 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign1],
    )
    do_dir.children[0].addchild(loop1)

    task1 = DynamicOMPTaskDirective()
    loop1 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task1],
    )
    assign1 = Assignment.create(
        StructureReference.create(rval, ["nx"]), Reference(tmp2)
    )
    subloop1 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign1],
    )

    task2 = DynamicOMPTaskDirective()
    loop2 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )
    assign2 = Assignment.create(
        StructureReference.create(rval, ["nx"]), Reference(tmp2)
    )
    subloop2 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign2],
    )

    parallel.children[0].addchild(do_dir)
    parallel.children[0].addchild(sing)
    sing.children[0].addchild(loop1)
    sing.children[0].addchild(loop2)

    task1.children[0].addchild(subloop1)
    task1.lower_to_language_level()
    task2.children[0].addchild(subloop2)
    task2.lower_to_language_level()

    sing._validate_task_dependencies()

    # Check outout StructureReference with array access
    subroutine = Routine.create("testsub")
    parallel = OMPParallelDirective.create()
    subroutine.addchild(parallel)
    sing = OMPSingleDirective()
    parallel.children[0].addchild(sing)
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)
    tmp2 = DataSymbol("tmp2", INTEGER_SINGLE_TYPE)
    rval = DataSymbol("rval", grid_type)
    subroutine.symbol_table.add(tmp)
    subroutine.symbol_table.add(tmp2)
    subroutine.symbol_table.add(rval)

    task1 = DynamicOMPTaskDirective()
    loop1 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task1],
    )
    assign1 = Assignment.create(
        StructureReference.create(
            rval, [("data", [Reference(tmp2), Reference(tmp2)])]
        ),
        Reference(tmp2),
    )
    subloop1 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign1],
    )

    task2 = DynamicOMPTaskDirective()
    loop2 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )
    assign2 = Assignment.create(
        StructureReference.create(
            rval, [("data", [Reference(tmp2), Reference(tmp2)])]
        ),
        Reference(tmp2),
    )
    subloop2 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign2],
    )

    sing.children[0].addchild(loop1)
    sing.children[0].addchild(loop2)

    task1.children[0].addchild(subloop1)
    task1.lower_to_language_level()
    task2.children[0].addchild(subloop2)
    task2.lower_to_language_level()

    sing._validate_task_dependencies()

    # Check outout accesses to different structure member lengths
    sub_grid_type = StructureType.create(
        [("array", ArrayType(REAL_TYPE, [128, 128]), Symbol.Visibility.PUBLIC,
          None)]
    )
    grid_type = StructureType.create(
        [
            ("nx", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
            ("sub_grids", sub_grid_type, Symbol.Visibility.PUBLIC, None),
            (
                "data",
                ArrayType(REAL_TYPE, [128, 128]),
                Symbol.Visibility.PUBLIC,
                None,
            ),
        ]
    )
    subroutine = Routine.create("testsub")
    parallel = OMPParallelDirective.create()
    subroutine.addchild(parallel)
    sing = OMPSingleDirective()
    parallel.children[0].addchild(sing)
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)
    tmp2 = DataSymbol("tmp2", INTEGER_SINGLE_TYPE)
    tmp3 = DataSymbol("tmp3", INTEGER_SINGLE_TYPE)
    rval = DataSymbol("rval", grid_type)
    subroutine.symbol_table.add(tmp)
    subroutine.symbol_table.add(tmp2)
    subroutine.symbol_table.add(tmp3)
    subroutine.symbol_table.add(rval)

    task1 = DynamicOMPTaskDirective()
    loop1 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task1],
    )
    assign1 = Assignment.create(
        StructureReference.create(
            rval, [("data", [Reference(tmp2), Reference(tmp2)])]
        ),
        Reference(tmp2),
    )
    subloop1 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign1],
    )

    task2 = DynamicOMPTaskDirective()
    loop2 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )
    assign2 = Assignment.create(
        StructureReference.create(
            rval, ["sub_grids", ("array", [Reference(tmp2), Reference(tmp2)])]
        ),
        Reference(tmp2),
    )
    subloop2 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign2],
    )

    sing.children[0].addchild(loop1)
    sing.children[0].addchild(loop2)

    task1.children[0].addchild(subloop1)
    task1.lower_to_language_level()
    task2.children[0].addchild(subloop2)
    task2.lower_to_language_level()

    sing._validate_task_dependencies()

    # Check outout accesses to different structure members
    grid_type = StructureType.create(
        [
            ("nx", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
            (
                "data2",
                ArrayType(REAL_TYPE, [128, 128]),
                Symbol.Visibility.PUBLIC,
                None,
            ),
            (
                "data",
                ArrayType(REAL_TYPE, [128, 128]),
                Symbol.Visibility.PUBLIC,
                None,
            ),
        ]
    )
    subroutine = Routine.create("testsub")
    parallel = OMPParallelDirective.create()
    subroutine.addchild(parallel)
    sing = OMPSingleDirective()
    parallel.children[0].addchild(sing)
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)
    tmp2 = DataSymbol("tmp2", INTEGER_SINGLE_TYPE)
    tmp3 = DataSymbol("tmp3", INTEGER_SINGLE_TYPE)
    rval = DataSymbol("rval", grid_type)
    subroutine.symbol_table.add(tmp)
    subroutine.symbol_table.add(tmp2)
    subroutine.symbol_table.add(tmp3)
    subroutine.symbol_table.add(rval)

    task1 = DynamicOMPTaskDirective()
    loop1 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task1],
    )
    assign1 = Assignment.create(
        StructureReference.create(
            rval, [("data", [Reference(tmp2), Reference(tmp2)])]
        ),
        Reference(tmp2),
    )
    subloop1 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign1],
    )

    task2 = DynamicOMPTaskDirective()
    loop2 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )
    assign2 = Assignment.create(
        StructureReference.create(
            rval, [("data2", [Reference(tmp2), Reference(tmp2)])]
        ),
        Reference(tmp2),
    )
    subloop2 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign2],
    )

    sing.children[0].addchild(loop1)
    sing.children[0].addchild(loop2)

    task1.children[0].addchild(subloop1)
    task1.lower_to_language_level()
    task2.children[0].addchild(subloop2)
    task2.lower_to_language_level()
    sing._validate_task_dependencies()

    # Check outout accesses to range and literal indexes.
    subroutine = Routine.create("testsub")
    parallel = OMPParallelDirective.create()
    subroutine.addchild(parallel)
    sing = OMPSingleDirective()
    parallel.children[0].addchild(sing)
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)
    tmp2 = DataSymbol("tmp2", INTEGER_SINGLE_TYPE)
    tmp3 = DataSymbol("tmp3", INTEGER_SINGLE_TYPE)
    rval = DataSymbol("rval", grid_type)
    subroutine.symbol_table.add(tmp)
    subroutine.symbol_table.add(tmp2)
    subroutine.symbol_table.add(tmp3)
    subroutine.symbol_table.add(rval)

    task1 = DynamicOMPTaskDirective()
    loop1 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task1],
    )
    assign1 = Assignment.create(
        StructureReference.create(
            rval,
            [("data", [Literal("1", INTEGER_SINGLE_TYPE), Reference(tmp2)])],
        ),
        Reference(tmp2),
    )
    subloop1 = Loop.create(
        tmp2,
        Reference(tmp3),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign1],
    )

    task2 = DynamicOMPTaskDirective()
    loop2 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )
    assign2 = Assignment.create(
        StructureReference.create(
            rval,
            [("data", [Literal("1", INTEGER_SINGLE_TYPE), Reference(tmp2)])],
        ),
        Reference(tmp2),
    )
    subloop2 = Loop.create(
        tmp2,
        Reference(tmp3),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign2],
    )

    sing.children[0].addchild(loop1)
    sing.children[0].addchild(loop2)

    task1.children[0].addchild(subloop1)
    task1.lower_to_language_level()
    task2.children[0].addchild(subloop2)
    task2.lower_to_language_level()

    sing._validate_task_dependencies()


def test_omp_serial_validate_task_dependencies_inout():
    '''
    Test check_task_dependencies member of OMPSerialDirective
    for inout dependency types
    '''
    # Check inout Array dependency
    subroutine = Routine.create("testsub")
    parallel = OMPParallelDirective.create()
    subroutine.addchild(parallel)
    sing = OMPSingleDirective()
    parallel.children[0].addchild(sing)
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)
    tmp2 = DataSymbol("tmp2", INTEGER_SINGLE_TYPE)
    tmp3 = DataSymbol("tmp3", INTEGER_SINGLE_TYPE)
    array_type = ArrayType(INTEGER_SINGLE_TYPE, [128, 128])
    rval = DataSymbol("rval", array_type)

    subroutine.symbol_table.add(tmp)
    subroutine.symbol_table.add(tmp2)
    subroutine.symbol_table.add(tmp3)
    subroutine.symbol_table.add(rval)

    task1 = DynamicOMPTaskDirective()
    loop1 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task1],
    )
    assign1 = Assignment.create(
        Reference(tmp3),
        ArrayReference.create(rval, [Reference(tmp), Reference(tmp)]),
    )
    subloop1 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign1],
    )

    task2 = DynamicOMPTaskDirective()
    loop2 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("64", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )
    assign2 = Assignment.create(
        ArrayReference.create(rval, [Reference(tmp), Reference(tmp)]),
        Literal("24", INTEGER_SINGLE_TYPE),
    )
    subloop2 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign2],
    )
    sing.children[0].addchild(loop1)
    sing.children[0].addchild(loop2)

    task1.children[0].addchild(subloop1)
    task1.lower_to_language_level()
    task2.children[0].addchild(subloop2)
    task2.lower_to_language_level()

    sing._validate_task_dependencies()

    # Check inout Reference dependency
    subroutine = Routine.create("testsub")
    parallel = OMPParallelDirective.create()
    subroutine.addchild(parallel)
    sing = OMPSingleDirective()
    parallel.children[0].addchild(sing)
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)
    tmp2 = DataSymbol("tmp2", INTEGER_SINGLE_TYPE)
    tmp3 = DataSymbol("tmp3", INTEGER_SINGLE_TYPE)
    rval = DataSymbol("rval", INTEGER_SINGLE_TYPE)

    subroutine.symbol_table.add(tmp)
    subroutine.symbol_table.add(tmp2)
    subroutine.symbol_table.add(tmp3)

    subroutine.symbol_table.add(rval)

    task1 = DynamicOMPTaskDirective()
    loop1 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task1],
    )
    assign1 = Assignment.create(
        Reference(tmp3),
        BinaryOperation.create(
            BinaryOperation.Operator.ADD, Reference(rval), Reference(tmp2)
        ),
    )
    subloop1 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign1],
    )

    task2 = DynamicOMPTaskDirective()
    loop2 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )
    assign2 = Assignment.create(
        Reference(rval),
        BinaryOperation.create(
            BinaryOperation.Operator.ADD, Reference(rval), Reference(tmp2)
        ),
    )
    subloop2 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign2],
    )
    sing.children[0].addchild(loop1)
    sing.children[0].addchild(loop2)

    task1.children[0].addchild(subloop1)
    task1.lower_to_language_level()
    task2.children[0].addchild(subloop2)
    task2.lower_to_language_level()

    sing._validate_task_dependencies()

    # StructureType for Structure tests
    grid_type = StructureType.create(
        [
            ("nx", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
            (
                "sub_grids",
                ArrayType(INTEGER_TYPE, [3]),
                Symbol.Visibility.PUBLIC,
                None,
            ),
            (
                "data",
                ArrayType(REAL_TYPE, [128, 128]),
                Symbol.Visibility.PUBLIC,
                None,
            ),
        ]
    )
    # Check inout StructureReference non-array dependency
    subroutine = Routine.create("testsub")
    parallel = OMPParallelDirective.create()
    subroutine.addchild(parallel)
    sing = OMPSingleDirective()
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)
    tmp2 = DataSymbol("tmp2", INTEGER_SINGLE_TYPE)
    tmp3 = DataSymbol("tmp3", INTEGER_SINGLE_TYPE)
    rval = DataSymbol("rval", grid_type)
    subroutine.symbol_table.add(tmp)
    subroutine.symbol_table.add(tmp2)
    subroutine.symbol_table.add(tmp3)
    subroutine.symbol_table.add(rval)

    do_dir = OMPDoDirective()
    assign1 = Assignment.create(
        Reference(tmp2), StructureReference.create(rval, ["nx"])
    )
    loop1 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign1],
    )
    do_dir.children[0].addchild(loop1)

    task1 = DynamicOMPTaskDirective()
    loop1 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task1],
    )
    assign1 = Assignment.create(
        Reference(tmp3), StructureReference.create(rval, ["nx"])
    )
    subloop1 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign1],
    )

    task2 = DynamicOMPTaskDirective()
    loop2 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )
    assign2 = Assignment.create(
        StructureReference.create(rval, ["nx"]), Reference(tmp2)
    )
    subloop2 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign2],
    )

    parallel.children[0].addchild(do_dir)
    parallel.children[0].addchild(sing)
    sing.children[0].addchild(loop1)
    sing.children[0].addchild(loop2)

    task1.children[0].addchild(subloop1)
    task1.lower_to_language_level()
    task2.children[0].addchild(subloop2)
    task2.lower_to_language_level()

    sing._validate_task_dependencies()

    # Check inout StructureReference with array access
    subroutine = Routine.create("testsub")
    parallel = OMPParallelDirective.create()
    subroutine.addchild(parallel)
    sing = OMPSingleDirective()
    parallel.children[0].addchild(sing)
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)
    tmp2 = DataSymbol("tmp2", INTEGER_SINGLE_TYPE)
    tmp3 = DataSymbol("tmp3", INTEGER_SINGLE_TYPE)
    rval = DataSymbol("rval", grid_type)
    subroutine.symbol_table.add(tmp)
    subroutine.symbol_table.add(tmp2)
    subroutine.symbol_table.add(tmp3)
    subroutine.symbol_table.add(rval)

    task1 = DynamicOMPTaskDirective()
    loop1 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task1],
    )
    assign1 = Assignment.create(
        Reference(tmp3),
        StructureReference.create(
            rval, [("data", [Reference(tmp2), Reference(tmp2)])]
        ),
    )
    subloop1 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign1],
    )

    task2 = DynamicOMPTaskDirective()
    loop2 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )
    assign2 = Assignment.create(
        StructureReference.create(
            rval, [("data", [Reference(tmp2), Reference(tmp2)])]
        ),
        Reference(tmp2),
    )
    subloop2 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign2],
    )

    sing.children[0].addchild(loop1)
    sing.children[0].addchild(loop2)

    task1.children[0].addchild(subloop1)
    task1.lower_to_language_level()
    task2.children[0].addchild(subloop2)
    task2.lower_to_language_level()

    sing._validate_task_dependencies()

    # Check inout accesses to different structure member lengths
    sub_grid_type = StructureType.create(
        [("array", ArrayType(REAL_TYPE, [128, 128]), Symbol.Visibility.PUBLIC,
          None)]
    )
    grid_type = StructureType.create(
        [
            ("nx", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
            ("sub_grids", sub_grid_type, Symbol.Visibility.PUBLIC, None),
            (
                "data",
                ArrayType(REAL_TYPE, [128, 128]),
                Symbol.Visibility.PUBLIC,
                None,
            ),
        ]
    )
    subroutine = Routine.create("testsub")
    parallel = OMPParallelDirective.create()
    subroutine.addchild(parallel)
    sing = OMPSingleDirective()
    parallel.children[0].addchild(sing)
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)
    tmp2 = DataSymbol("tmp2", INTEGER_SINGLE_TYPE)
    tmp3 = DataSymbol("tmp3", INTEGER_SINGLE_TYPE)
    rval = DataSymbol("rval", grid_type)
    subroutine.symbol_table.add(tmp)
    subroutine.symbol_table.add(tmp2)
    subroutine.symbol_table.add(tmp3)
    subroutine.symbol_table.add(rval)

    task1 = DynamicOMPTaskDirective()
    loop1 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task1],
    )
    assign1 = Assignment.create(
        Reference(tmp3),
        StructureReference.create(
            rval, ["sub_grids", ("array", [Reference(tmp2), Reference(tmp2)])]
        ),
    )
    subloop1 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign1],
    )

    task2 = DynamicOMPTaskDirective()
    loop2 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )
    assign2 = Assignment.create(
        StructureReference.create(
            rval, [("data", [Reference(tmp2), Reference(tmp2)])]
        ),
        Reference(tmp2),
    )
    subloop2 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign2],
    )

    sing.children[0].addchild(loop1)
    sing.children[0].addchild(loop2)

    task1.children[0].addchild(subloop1)
    task1.lower_to_language_level()
    task2.children[0].addchild(subloop2)
    task2.lower_to_language_level()

    sing._validate_task_dependencies()

    # Check inout accesses to different structure members
    grid_type = StructureType.create(
        [
            ("nx", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
            (
                "data2",
                ArrayType(REAL_TYPE, [128, 128]),
                Symbol.Visibility.PUBLIC,
                None,
            ),
            (
                "data",
                ArrayType(REAL_TYPE, [128, 128]),
                Symbol.Visibility.PUBLIC,
                None,
            ),
        ]
    )
    subroutine = Routine.create("testsub")
    parallel = OMPParallelDirective.create()
    subroutine.addchild(parallel)
    sing = OMPSingleDirective()
    parallel.children[0].addchild(sing)
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)
    tmp2 = DataSymbol("tmp2", INTEGER_SINGLE_TYPE)
    tmp3 = DataSymbol("tmp3", INTEGER_SINGLE_TYPE)
    rval = DataSymbol("rval", grid_type)
    subroutine.symbol_table.add(tmp)
    subroutine.symbol_table.add(tmp2)
    subroutine.symbol_table.add(tmp3)
    subroutine.symbol_table.add(rval)

    task1 = DynamicOMPTaskDirective()
    loop1 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task1],
    )
    assign1 = Assignment.create(
        Reference(tmp3),
        StructureReference.create(
            rval, [("data2", [Reference(tmp2), Reference(tmp2)])]
        ),
    )
    subloop1 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign1],
    )

    task2 = DynamicOMPTaskDirective()
    loop2 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )
    assign2 = Assignment.create(
        StructureReference.create(
            rval, [("data", [Reference(tmp2), Reference(tmp2)])]
        ),
        Reference(tmp2),
    )
    subloop2 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign2],
    )

    sing.children[0].addchild(loop1)
    sing.children[0].addchild(loop2)

    task1.children[0].addchild(subloop1)
    task1.lower_to_language_level()
    task2.children[0].addchild(subloop2)
    task2.lower_to_language_level()

    sing._validate_task_dependencies()

    # Check inout accesses to range and literal indexes.
    subroutine = Routine.create("testsub")
    parallel = OMPParallelDirective.create()
    subroutine.addchild(parallel)
    sing = OMPSingleDirective()
    parallel.children[0].addchild(sing)
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)
    tmp2 = DataSymbol("tmp2", INTEGER_SINGLE_TYPE)
    tmp3 = DataSymbol("tmp3", INTEGER_SINGLE_TYPE)
    tmp4 = DataSymbol("tmp4", INTEGER_SINGLE_TYPE)
    rval = DataSymbol("rval", grid_type)
    subroutine.symbol_table.add(tmp)
    subroutine.symbol_table.add(tmp2)
    subroutine.symbol_table.add(tmp3)
    subroutine.symbol_table.add(tmp4)
    subroutine.symbol_table.add(rval)

    task1 = DynamicOMPTaskDirective()
    loop1 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task1],
    )
    assign1 = Assignment.create(
        Reference(tmp4),
        StructureReference.create(
            rval,
            [("data", [Literal("1", INTEGER_SINGLE_TYPE), Reference(tmp2)])],
        ),
    )
    subloop1 = Loop.create(
        tmp2,
        Reference(tmp3),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign1],
    )

    task2 = DynamicOMPTaskDirective()
    loop2 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )
    assign2 = Assignment.create(
        StructureReference.create(
            rval,
            [("data", [Literal("1", INTEGER_SINGLE_TYPE), Reference(tmp2)])],
        ),
        Reference(tmp2),
    )
    subloop2 = Loop.create(
        tmp2,
        Reference(tmp3),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign2],
    )

    sing.children[0].addchild(loop1)
    sing.children[0].addchild(loop2)

    task1.children[0].addchild(subloop1)
    task1.lower_to_language_level()
    task2.children[0].addchild(subloop2)
    task2.lower_to_language_level()

    sing._validate_task_dependencies()


def test_omp_serial_validate_task_dependencies_outin():
    '''
    Test check_task_dependencies member of OMPSerialDirective
    for outin dependency types
    '''
    # Check outin Array dependency
    subroutine = Routine.create("testsub")
    parallel = OMPParallelDirective.create()
    subroutine.addchild(parallel)
    sing = OMPSingleDirective()
    parallel.children[0].addchild(sing)
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)
    tmp2 = DataSymbol("tmp2", INTEGER_SINGLE_TYPE)
    tmp3 = DataSymbol("tmp3", INTEGER_SINGLE_TYPE)
    array_type = ArrayType(INTEGER_SINGLE_TYPE, [128, 128])
    rval = DataSymbol("rval", array_type)

    subroutine.symbol_table.add(tmp)
    subroutine.symbol_table.add(tmp2)
    subroutine.symbol_table.add(tmp3)
    subroutine.symbol_table.add(rval)

    task1 = DynamicOMPTaskDirective()
    loop1 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task1],
    )
    assign1 = Assignment.create(
        ArrayReference.create(rval, [Reference(tmp), Reference(tmp)]),
        Literal("24", INTEGER_SINGLE_TYPE),
    )
    subloop1 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign1],
    )

    task2 = DynamicOMPTaskDirective()
    loop2 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("64", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )
    assign2 = Assignment.create(
        Reference(tmp3),
        ArrayReference.create(rval, [Reference(tmp), Reference(tmp)]),
    )
    subloop2 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign2],
    )
    sing.children[0].addchild(loop1)
    sing.children[0].addchild(loop2)

    task1.children[0].addchild(subloop1)
    task1.lower_to_language_level()
    task2.children[0].addchild(subloop2)
    task2.lower_to_language_level()

    sing._validate_task_dependencies()

    # Check outin Reference dependency
    subroutine = Routine.create("testsub")
    parallel = OMPParallelDirective.create()
    subroutine.addchild(parallel)
    sing = OMPSingleDirective()
    parallel.children[0].addchild(sing)
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)
    tmp2 = DataSymbol("tmp2", INTEGER_SINGLE_TYPE)
    tmp3 = DataSymbol("tmp3", INTEGER_SINGLE_TYPE)
    rval = DataSymbol("rval", INTEGER_SINGLE_TYPE)

    subroutine.symbol_table.add(tmp)
    subroutine.symbol_table.add(tmp2)
    subroutine.symbol_table.add(tmp3)

    subroutine.symbol_table.add(rval)

    task1 = DynamicOMPTaskDirective()
    loop1 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task1],
    )
    assign1 = Assignment.create(
        Reference(rval),
        BinaryOperation.create(
            BinaryOperation.Operator.ADD, Reference(rval), Reference(tmp2)
        ),
    )
    subloop1 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign1],
    )

    task2 = DynamicOMPTaskDirective()
    loop2 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )
    assign2 = Assignment.create(
        Reference(tmp3),
        BinaryOperation.create(
            BinaryOperation.Operator.ADD, Reference(rval), Reference(tmp2)
        ),
    )
    subloop2 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign2],
    )
    sing.children[0].addchild(loop1)
    sing.children[0].addchild(loop2)

    task1.children[0].addchild(subloop1)
    task1.lower_to_language_level()
    task2.children[0].addchild(subloop2)
    task2.lower_to_language_level()

    sing._validate_task_dependencies()

    # StructureType for Structure tests
    grid_type = StructureType.create(
        [
            ("nx", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
            (
                "sub_grids",
                ArrayType(INTEGER_TYPE, [3]),
                Symbol.Visibility.PUBLIC,
                None,
            ),
            (
                "data",
                ArrayType(REAL_TYPE, [128, 128]),
                Symbol.Visibility.PUBLIC,
                None,
            ),
        ]
    )
    # Check inout StructureReference non-array dependency
    subroutine = Routine.create("testsub")
    parallel = OMPParallelDirective.create()
    subroutine.addchild(parallel)
    sing = OMPSingleDirective()
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)
    tmp2 = DataSymbol("tmp2", INTEGER_SINGLE_TYPE)
    tmp3 = DataSymbol("tmp3", INTEGER_SINGLE_TYPE)
    rval = DataSymbol("rval", grid_type)
    subroutine.symbol_table.add(tmp)
    subroutine.symbol_table.add(tmp2)
    subroutine.symbol_table.add(tmp3)
    subroutine.symbol_table.add(rval)

    do_dir = OMPDoDirective()
    assign1 = Assignment.create(
        Reference(tmp2), StructureReference.create(rval, ["nx"])
    )
    loop1 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign1],
    )
    do_dir.children[0].addchild(loop1)

    task1 = DynamicOMPTaskDirective()
    loop1 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task1],
    )
    assign1 = Assignment.create(
        StructureReference.create(rval, ["nx"]), Reference(tmp2)
    )
    subloop1 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign1],
    )

    task2 = DynamicOMPTaskDirective()
    loop2 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )
    assign2 = Assignment.create(
        Reference(tmp3), StructureReference.create(rval, ["nx"])
    )
    subloop2 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign2],
    )

    parallel.children[0].addchild(do_dir)
    parallel.children[0].addchild(sing)
    sing.children[0].addchild(loop1)
    sing.children[0].addchild(loop2)

    task1.children[0].addchild(subloop1)
    task1.lower_to_language_level()
    task2.children[0].addchild(subloop2)
    task2.lower_to_language_level()

    sing._validate_task_dependencies()

    # Check inout StructureReference with array access
    subroutine = Routine.create("testsub")
    parallel = OMPParallelDirective.create()
    subroutine.addchild(parallel)
    sing = OMPSingleDirective()
    parallel.children[0].addchild(sing)
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)
    tmp2 = DataSymbol("tmp2", INTEGER_SINGLE_TYPE)
    tmp3 = DataSymbol("tmp3", INTEGER_SINGLE_TYPE)
    rval = DataSymbol("rval", grid_type)
    subroutine.symbol_table.add(tmp)
    subroutine.symbol_table.add(tmp2)
    subroutine.symbol_table.add(tmp3)
    subroutine.symbol_table.add(rval)

    task1 = DynamicOMPTaskDirective()
    loop1 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task1],
    )
    assign1 = Assignment.create(
        StructureReference.create(
            rval, [("data", [Reference(tmp2), Reference(tmp2)])]
        ),
        Reference(tmp2),
    )
    subloop1 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign1],
    )

    task2 = DynamicOMPTaskDirective()
    loop2 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )
    assign2 = Assignment.create(
        Reference(tmp3),
        StructureReference.create(
            rval, [("data", [Reference(tmp2), Reference(tmp2)])]
        ),
    )
    subloop2 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign2],
    )

    sing.children[0].addchild(loop1)
    sing.children[0].addchild(loop2)

    task1.children[0].addchild(subloop1)
    task1.lower_to_language_level()
    task2.children[0].addchild(subloop2)
    task2.lower_to_language_level()

    sing._validate_task_dependencies()

    # Check outin accesses to different structure member lengths
    sub_grid_type = StructureType.create(
        [("array", ArrayType(REAL_TYPE, [128, 128]), Symbol.Visibility.PUBLIC,
          None)]
    )
    grid_type = StructureType.create(
        [
            ("nx", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
            ("sub_grids", sub_grid_type, Symbol.Visibility.PUBLIC, None),
            (
                "data",
                ArrayType(REAL_TYPE, [128, 128]),
                Symbol.Visibility.PUBLIC,
                None,
            ),
        ]
    )
    subroutine = Routine.create("testsub")
    parallel = OMPParallelDirective.create()
    subroutine.addchild(parallel)
    sing = OMPSingleDirective()
    parallel.children[0].addchild(sing)
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)
    tmp2 = DataSymbol("tmp2", INTEGER_SINGLE_TYPE)
    tmp3 = DataSymbol("tmp3", INTEGER_SINGLE_TYPE)
    rval = DataSymbol("rval", grid_type)
    subroutine.symbol_table.add(tmp)
    subroutine.symbol_table.add(tmp2)
    subroutine.symbol_table.add(tmp3)
    subroutine.symbol_table.add(rval)

    task1 = DynamicOMPTaskDirective()
    loop1 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task1],
    )
    assign1 = Assignment.create(
        StructureReference.create(
            rval, [("data", [Reference(tmp2), Reference(tmp2)])]
        ),
        Reference(tmp2),
    )
    subloop1 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign1],
    )

    task2 = DynamicOMPTaskDirective()
    loop2 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )
    assign2 = Assignment.create(
        Reference(tmp3),
        StructureReference.create(
            rval, ["sub_grids", ("array", [Reference(tmp2), Reference(tmp2)])]
        ),
    )
    subloop2 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign2],
    )

    sing.children[0].addchild(loop1)
    sing.children[0].addchild(loop2)

    task1.children[0].addchild(subloop1)
    task1.lower_to_language_level()
    task2.children[0].addchild(subloop2)
    task2.lower_to_language_level()

    sing._validate_task_dependencies()

    # Check outin accesses to different structure members
    grid_type = StructureType.create(
        [
            ("nx", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
            (
                "data2",
                ArrayType(REAL_TYPE, [128, 128]),
                Symbol.Visibility.PUBLIC,
                None,
            ),
            (
                "data",
                ArrayType(REAL_TYPE, [128, 128]),
                Symbol.Visibility.PUBLIC,
                None,
            ),
        ]
    )
    subroutine = Routine.create("testsub")
    parallel = OMPParallelDirective.create()
    subroutine.addchild(parallel)
    sing = OMPSingleDirective()
    parallel.children[0].addchild(sing)
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)
    tmp2 = DataSymbol("tmp2", INTEGER_SINGLE_TYPE)
    tmp3 = DataSymbol("tmp3", INTEGER_SINGLE_TYPE)
    rval = DataSymbol("rval", grid_type)
    subroutine.symbol_table.add(tmp)
    subroutine.symbol_table.add(tmp2)
    subroutine.symbol_table.add(tmp3)
    subroutine.symbol_table.add(rval)

    task1 = DynamicOMPTaskDirective()
    loop1 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task1],
    )
    assign1 = Assignment.create(
        StructureReference.create(
            rval, [("data", [Reference(tmp2), Reference(tmp2)])]
        ),
        Reference(tmp2),
    )
    subloop1 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign1],
    )

    task2 = DynamicOMPTaskDirective()
    loop2 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )
    assign2 = Assignment.create(
        Reference(tmp3),
        StructureReference.create(
            rval, [("data2", [Reference(tmp2), Reference(tmp2)])]
        ),
    )
    subloop2 = Loop.create(
        tmp2,
        Reference(tmp),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign2],
    )

    sing.children[0].addchild(loop1)
    sing.children[0].addchild(loop2)

    task1.children[0].addchild(subloop1)
    task1.lower_to_language_level()
    task2.children[0].addchild(subloop2)
    task2.lower_to_language_level()

    sing._validate_task_dependencies()

    # Check outin accesses to range and literal indexes.
    subroutine = Routine.create("testsub")
    parallel = OMPParallelDirective.create()
    subroutine.addchild(parallel)
    sing = OMPSingleDirective()
    parallel.children[0].addchild(sing)
    tmp = DataSymbol("tmp", INTEGER_SINGLE_TYPE)
    tmp2 = DataSymbol("tmp2", INTEGER_SINGLE_TYPE)
    tmp3 = DataSymbol("tmp3", INTEGER_SINGLE_TYPE)
    tmp4 = DataSymbol("tmp4", INTEGER_SINGLE_TYPE)
    rval = DataSymbol("rval", grid_type)
    subroutine.symbol_table.add(tmp)
    subroutine.symbol_table.add(tmp2)
    subroutine.symbol_table.add(tmp3)
    subroutine.symbol_table.add(tmp4)
    subroutine.symbol_table.add(rval)

    task1 = DynamicOMPTaskDirective()
    loop1 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task1],
    )
    assign1 = Assignment.create(
        StructureReference.create(
            rval,
            [("data", [Literal("1", INTEGER_SINGLE_TYPE), Reference(tmp2)])],
        ),
        Reference(tmp2),
    )
    subloop1 = Loop.create(
        tmp2,
        Reference(tmp3),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign1],
    )

    task2 = DynamicOMPTaskDirective()
    loop2 = Loop.create(
        tmp,
        Literal("1", INTEGER_SINGLE_TYPE),
        Literal("128", INTEGER_SINGLE_TYPE),
        Literal("32", INTEGER_SINGLE_TYPE),
        [task2],
    )
    assign2 = Assignment.create(
        Reference(tmp4),
        StructureReference.create(
            rval,
            [("data", [Literal("1", INTEGER_SINGLE_TYPE), Reference(tmp2)])],
        ),
    )
    subloop2 = Loop.create(
        tmp2,
        Reference(tmp3),
        Literal("32", INTEGER_SINGLE_TYPE),
        Literal("1", INTEGER_SINGLE_TYPE),
        [assign2],
    )

    sing.children[0].addchild(loop1)
    sing.children[0].addchild(loop2)

    task1.children[0].addchild(subloop1)
    task1.lower_to_language_level()
    task2.children[0].addchild(subloop2)
    task2.lower_to_language_level()

    sing._validate_task_dependencies()


def test_omp_serial_validate_task_dependencies_add_taskwait(fortran_reader):
    '''
    Test the task dependency chcker adds taskwaits in expected locations.
    '''
    code = '''subroutine my_subroutine(grid_max, grid_min)
        integer, dimension(100, 100) :: A, B, C, D
        integer :: i, j
        integer, intent(in) :: grid_max, grid_min

        do i = grid_min, grid_max
            do j = grid_min, grid_max
                a(i, j) = i*grid_max + j
            end do
        end do
        do i = grid_min, grid_max
            do j = grid_min, grid_max
                b(i, j) = j*grid_max + i
            end do
        end do
        do i = grid_min+1, grid_max-1
            do j = grid_min, grid_max
                c(i, j) = a(i,j) * 3
            end do
        end do
        do i = grid_min+1, grid_max-1
            do j = grid_min, grid_max
                d(i, j) = b(i,j) + a(i,j)
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)

    loop_trans = ChunkLoopTrans()
    task_trans = OMPTaskTrans()

    schedule = tree.walk(Schedule)[0]
    for child in schedule.children[:]:
        if isinstance(child, Loop):
            loop_trans.apply(child)
            assert isinstance(child.children[3].children[0], Loop)
            task_trans.apply(child, {"force": True})

    single_trans = OMPSingleTrans()
    parallel_trans = OMPParallelTrans()
    single_trans.apply(schedule.children)
    parallel_trans.apply(schedule.children)
    tree.lower_to_language_level()

    taskwaits = tree.walk(OMPTaskwaitDirective)
    assert len(taskwaits) == 1
    assert taskwaits[0].position == 2

    code = '''subroutine my_subroutine(grid_max, grid_min, runtime_parameter)
        integer, dimension(100, 100) :: A, B, C, D
        integer :: i, j
        integer, intent(in) :: grid_max, grid_min
        logical, intent(in) :: runtime_parameter

        do i = grid_min, grid_max
            do j = grid_min, grid_max
                a(i, j) = i*grid_max + j
            end do
        end do
        do i = grid_min, grid_max
            do j = grid_min, grid_max
                b(i, j) = j*grid_max + i
            end do
        end do
        if(runtime_parameter) then
            do i = grid_min+1, grid_max-1
                do j = grid_min, grid_max
                    c(i, j) = a(i,j) * 3
                end do
            end do
        end if
        do i = grid_min+1, grid_max-1
            do j = grid_min, grid_max
                d(i, j) = b(i,j) + a(i,j)
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)

    schedule = tree.walk(Schedule)[0]
    for child in schedule.children[:]:
        if isinstance(child, Loop):
            loop_trans.apply(child)
            assert isinstance(child.children[3].children[0], Loop)
            task_trans.apply(child, {"force": True})
        if isinstance(child, IfBlock):
            loop = child.if_body.children[0]
            loop_trans.apply(loop)
            task_trans.apply(loop, {"force": True})

    single_trans = OMPSingleTrans()
    parallel_trans = OMPParallelTrans()
    single_trans.apply(schedule.children)
    parallel_trans.apply(schedule.children)
    tree.lower_to_language_level()

    taskwaits = tree.walk(OMPTaskwaitDirective)
    assert len(taskwaits) == 1
    assert taskwaits[0].position == 2

    code = '''subroutine my_subroutine(grid_max, grid_min, runtime_parameter)
        integer, dimension(100, 100) :: A, B, C, D
        integer :: i, j
        integer, intent(in) :: grid_max, grid_min
        logical, intent(in) :: runtime_parameter

        do i = grid_min, grid_max
            do j = grid_min, grid_max
                b(i, j) = j*grid_max + i
            end do
        end do
        if(runtime_parameter) then
            do i = grid_min, grid_max
                do j = grid_min, grid_max
                    a(i, j) = i*grid_max + j
                end do
            end do
        else
            do i = grid_min+1, grid_max-1
                do j = grid_min, grid_max
                    c(i, j) = a(i,j) * 3
                end do
            end do
        end if
        do i = grid_min+1, grid_max-1
            do j = grid_min, grid_max
                d(i, j) = b(i,j) + a(i,j)
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)

    schedule = tree.walk(Schedule)[0]
    for child in schedule.children[:]:
        if isinstance(child, Loop):
            loop_trans.apply(child)
            assert isinstance(child.children[3].children[0], Loop)
            task_trans.apply(child, {"force": True})
        if isinstance(child, IfBlock):
            loop = child.if_body.children[0]
            loop_trans.apply(loop)
            task_trans.apply(loop, {"force": True})
            loop = child.else_body.children[0]
            loop_trans.apply(loop)
            task_trans.apply(loop, {"force": True})

    single_trans = OMPSingleTrans()
    parallel_trans = OMPParallelTrans()
    single_trans.apply(schedule.children)
    parallel_trans.apply(schedule.children)
    tree.lower_to_language_level()

    taskwaits = tree.walk(OMPTaskwaitDirective)
    assert len(taskwaits) == 1
    assert taskwaits[0].position == 2

    code = '''subroutine my_subroutine(grid_max, grid_min)
        integer, dimension(100, 100) :: A, B, C, D
        integer :: i, j
        integer, intent(in) :: grid_max, grid_min

        do i = grid_min, grid_max
            do j = grid_min, grid_max
                a(i, j) = i*grid_max + j
            end do
        end do
        do i = grid_min+1, grid_max-1
            do j = grid_min, grid_max
                c(i, j) = a(i,j) * 3
            end do
        end do
        do i = grid_min, grid_max
            do j = grid_min, grid_max
                b(i, j) = j*grid_max + i
            end do
        end do
        do i = grid_min+1, grid_max-1
            do j = grid_min, grid_max
                d(i, j) = b(i,j) + a(i,j)
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)

    loop_trans = ChunkLoopTrans()
    task_trans = OMPTaskTrans()

    schedule = tree.walk(Schedule)[0]
    for child in schedule.children[:]:
        if isinstance(child, Loop):
            loop_trans.apply(child)
            assert isinstance(child.children[3].children[0], Loop)
            task_trans.apply(child, {"force": True})

    single_trans = OMPSingleTrans()
    parallel_trans = OMPParallelTrans()
    single_trans.apply(schedule.children)
    parallel_trans.apply(schedule.children)
    tree.lower_to_language_level()

    taskwaits = tree.walk(OMPTaskwaitDirective)
    assert len(taskwaits) == 2
    assert taskwaits[0].position == 1
    assert taskwaits[1].position == 4

    code = '''subroutine my_subroutine(grid_max, grid_min)
        integer, dimension(100, 100) :: A, B, C, D
        integer :: i, j
        integer, intent(in) :: grid_max, grid_min

        do i = grid_min, grid_max
            do j = grid_min, grid_max
                a(i, j) = i*grid_max + j
            end do
        end do
        do i = grid_min+1, grid_max-1
            do j = grid_min, grid_max
                c(i, j) = a(i,j) * 3
            end do
        end do
        do i = grid_min, grid_max
            do j = grid_min, grid_max
                b(i, j) = j*grid_max + i
            end do
        end do
        do i = grid_min+1, grid_max-1
            do j = grid_min, grid_max
                d(i, j) = b(i,j) + a(i,j)
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)

    loop_trans = ChunkLoopTrans()
    task_trans = OMPTaskTrans()

    schedule = tree.walk(Schedule)[0]
    for child in schedule.children[:]:
        if isinstance(child, Loop):
            loop_trans.apply(child)
            assert isinstance(child.children[3].children[0], Loop)
            task_trans.apply(child, {"force": True})

    single_trans = OMPSingleTrans()
    parallel_trans = OMPParallelTrans()
    single_trans.apply(schedule.children)
    parallel_trans.apply(schedule.children)
    user_taskwait = OMPTaskwaitDirective()
    schedule[0].children[0].children[0].children[0].addchild(user_taskwait,
                                                             index=1)
    tree.lower_to_language_level()

    taskwaits = tree.walk(OMPTaskwaitDirective)
    assert len(taskwaits) == 2
    assert taskwaits[0].position == 1
    assert taskwaits[1].position == 4


def test_omp_serial_check_dependency_valid_pairing_edgecase():
    '''
    Tests the edge case where two Reference to the same symbol
    have different types returns False as expected.
    '''
    array_type = ArrayType(INTEGER_SINGLE_TYPE, [128, 128])
    rval = DataSymbol("rval", array_type)
    ref1 = Reference(rval)
    one = Literal("1", INTEGER_SINGLE_TYPE)
    ref2 = ArrayReference.create(rval, [one.copy(), one.copy()])

    test_dir = OMPSingleDirective()

    val = test_dir._check_dependency_pairing_valid(ref1, ref2, None, None)
    assert not val


def test_omp_serial_check_dependency_valid_multiple_arraymixin():
    '''
    Tests the case where a StructureReference contains two ArrayMixin
    children.
    '''
    region_type = StructureType.create([
        ("startx", ArrayType(REAL_TYPE, [10]), Symbol.Visibility.PUBLIC, None)
    ])
    region_type_symbol = DataTypeSymbol("region_type", region_type)
    grid_type = StructureType.create([
        ("nx", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
        ("region", region_type_symbol, Symbol.Visibility.PRIVATE, None),
        ("sub_grids", ArrayType(region_type_symbol, [3]),
         Symbol.Visibility.PUBLIC, None),
        ("data", ArrayType(REAL_TYPE, [10, 10]),
         Symbol.Visibility.PUBLIC, None)])
    grid_type_symbol = DataTypeSymbol("grid_type", grid_type)
    ssym = DataSymbol("grid", grid_type_symbol)
    # Reference to scalar member of structure
    two = Literal("2", INTEGER_SINGLE_TYPE)
    sref = StructureReference.create(
        ssym, [("sub_grids", [two.copy(), two.copy()]),
               ("startx", [two.copy()])]
    )
    sref2 = StructureReference.create(
        ssym, [("sub_grids", [two.copy(), two.copy()]),
               ("startx", [two.copy()])]
    )
    test_dir = OMPSingleDirective()
    res = test_dir._check_dependency_pairing_valid(
               sref, sref2, None, None
        )
    assert not res


def test_omp_serial_check_dependency_valid_pairing():
    '''
    Test check_dependency_valid_pairing calls the correct functions for
    various array indices.
    '''
    # Check we use the valid_dependence_literals function to get
    # the correct answer for literal indices.
    array_type = ArrayType(INTEGER_SINGLE_TYPE, [128, 128])
    rval = DataSymbol("rval", array_type)
    one = Literal("1", INTEGER_SINGLE_TYPE)
    two = Literal("2", INTEGER_SINGLE_TYPE)
    ref1 = ArrayReference.create(rval, [one.copy(), one.copy()])
    ref2 = ArrayReference.create(rval, [two.copy(), two.copy()])

    test_dir = OMPSingleDirective()
    assert test_dir._check_dependency_pairing_valid(ref1, ref2, None, None)

    # Check we use the valid_dependence_ranges function to get the
    # correct answer for range indices.
    array_type = ArrayType(INTEGER_SINGLE_TYPE, [128, 128])
    rval = DataSymbol("rval", array_type)
    lbound = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.LBOUND,
        [Reference(rval), ("dim", one.copy())]
    )
    ubound = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.UBOUND,
        [Reference(rval), ("dim", one.copy())]
    )
    my_range1 = Range.create(lbound.copy(), ubound.copy(), one.copy())
    lbound2 = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.LBOUND,
        [Reference(rval), ("dim", two.copy())]
    )
    ubound2 = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.UBOUND,
        [Reference(rval), ("dim", two.copy())]
    )
    my_range2 = Range.create(lbound2.copy(), ubound2.copy(), one.copy())
    array_reference1 = ArrayReference.create(rval, [my_range1.copy(),
                                                    my_range2.copy()])
    array_reference2 = ArrayReference.create(rval, [my_range1.copy(),
                                                    my_range2.copy()])
    assert test_dir._check_dependency_pairing_valid(
               array_reference1, array_reference2, None, None
           )


def test_add_reduction_clause_parallel_do(fortran_reader, fortran_writer):
    ''' Test adding reduction clauses to OMPParallelDoDirective.
    '''
    psyir = fortran_reader.psyir_from_source('''
        function sum_arr(arr) result (acc)
            integer, intent(in) :: arr(:)
            integer :: i
            integer :: acc = 0

            do i = 1, ubound(arr)
                acc = acc + arr(i)
            end do
        end function''')
    loop = psyir.walk(Loop)[0]
    loop_parent = loop.parent
    loop_position = loop.position
    do_directive = OMPParallelDoDirective(children=[loop.detach()])
    loop_parent.addchild(do_directive, index=loop_position)
    clause = OMPReductionClause(OMPReductionClause.ReductionClauseTypes.ADD)
    clause.addchild(Reference(Symbol("acc")))
    do_directive.addchild(clause)
    output = fortran_writer(psyir)
    assert "reduction(+: acc)" in output


def test_add_reduction_clause_do(fortran_reader, fortran_writer):
    ''' Test adding reduction clauses to OMPDoDirective.
    '''
    psyir = fortran_reader.psyir_from_source('''
        function sum_arr(arr) result (acc)
            integer, intent(in) :: arr(:)
            integer :: i
            integer :: acc = 0

            do i = 1, ubound(arr)
                acc = acc + arr(i)
            end do
        end function''')
    loop = psyir.walk(Loop)[0]
    OMPParallelTrans().apply(loop)
    loop_parent = loop.parent
    loop_position = loop.position
    do_directive = OMPDoDirective(children=[loop.detach()])
    loop_parent.addchild(do_directive, index=loop_position)
    clause = OMPReductionClause(OMPReductionClause.ReductionClauseTypes.ADD)
    clause.addchild(Reference(Symbol("acc")))
    do_directive.addchild(clause)
    output = fortran_writer(psyir)
    assert "reduction(+: acc)" in output


def test_add_reduction_clause_loop(fortran_reader, fortran_writer):
    ''' Test adding reduction clauses to OMPLoopDirective.
    '''
    psyir = fortran_reader.psyir_from_source('''
        function sum_arr(arr) result (acc)
            integer, intent(in) :: arr(:)
            integer :: i
            integer :: acc = 0

            do i = 1, ubound(arr)
                acc = acc + arr(i)
            end do
        end function''')
    loop = psyir.walk(Loop)[0]
    OMPParallelTrans().apply(loop)
    loop_parent = loop.parent
    loop_position = loop.position
    loop_directive = OMPLoopDirective(children=[loop.detach()])
    loop_parent.addchild(loop_directive, index=loop_position)
    clause = OMPReductionClause(OMPReductionClause.ReductionClauseTypes.ADD)
    clause.addchild(Reference(Symbol("acc")))
    loop_directive.addchild(clause)
    output = fortran_writer(psyir)
    assert "reduction(+: acc)" in output


def test_reduction_clause_eq(fortran_reader, fortran_writer):
    ''' Test OMPParallelDoDirective equality with reduction clauses
    '''
    do_directive1 = OMPParallelDoDirective()
    do_directive2 = OMPParallelDoDirective()
    do_directive3 = OMPParallelDoDirective()

    clause1 = OMPReductionClause(OMPReductionClause.ReductionClauseTypes.ADD)
    clause1.addchild(Reference(Symbol("foo")))
    do_directive1.addchild(clause1)

    clause2 = OMPReductionClause(OMPReductionClause.ReductionClauseTypes.ADD)
    clause2.addchild(Reference(Symbol("bar")))
    do_directive2.addchild(clause2)

    assert do_directive1 != do_directive2

    clause3 = OMPReductionClause(OMPReductionClause.ReductionClauseTypes.ADD)
    clause3.addchild(Reference(Symbol("foo")))
    do_directive3.addchild(clause3)

    assert do_directive1 == do_directive3


def test_add_reduction_clause_validation(fortran_reader, fortran_writer):
    ''' Check that adding a reduction clause with a non-reduction clause
        argument raises an error.
    '''
    do_directive = OMPDoDirective()
    with pytest.raises(GenerationError) as err:
        do_directive.addchild(OMPScheduleClause())
    assert ("Item 'OMPScheduleClause' can't be child 1 of 'OMPDoDirective'"
            in str(err.value))


@pytest.mark.parametrize("op", ["*", "-", "*"])
def test_reduction_arith_ops(op, fortran_reader, fortran_writer):
    ''' Test that reduction loops involing arithmetic reduction operators are
    parallelised.
    '''
    psyir = fortran_reader.psyir_from_source(f'''
        function sum_arr(arr) result (acc)
            integer, intent(in) :: arr(:)
            integer :: i
            integer :: acc = 0

            do i = 1, ubound(arr)
                acc = acc {op} arr(i)
            end do
        end function''')
    omplooptrans = OMPLoopTrans(omp_directive="paralleldo")
    loop = psyir.walk(Loop)[0]
    omplooptrans.apply(loop, enable_reductions=True)
    output = fortran_writer(psyir)
    assert f"reduction({op}: acc)" in output


@pytest.mark.parametrize("op", [".AND.", ".OR.", ".EQV.", ".NEQV."])
def test_reduction_logical(op, fortran_reader, fortran_writer):
    ''' Test that reduction loops involing logical reduction operators are
    parallelised.
    '''
    psyir = fortran_reader.psyir_from_source(f'''
        function sum_arr(arr) result (acc)
            integer, intent(in) :: arr(:)
            integer :: i
            logical :: acc = .false.

            do i = 1, ubound(arr)
                acc = acc {op} arr(i)
            end do
        end function''')
    omplooptrans = OMPLoopTrans(omp_directive="paralleldo")
    loop = psyir.walk(Loop)[0]
    omplooptrans.apply(loop, enable_reductions=True)
    output = fortran_writer(psyir)
    assert f"reduction({op}: acc)" in output


@pytest.mark.parametrize("op", ["MAX", "MIN", "IAND", "IOR", "IEOR"])
def test_reduction_intrins(op, fortran_reader, fortran_writer):
    ''' Test that reduction loops involing intrinsic reduction operators are
    parallelised.
    '''
    psyir = fortran_reader.psyir_from_source(f'''
        function sum_arr(arr) result (acc)
            integer, intent(in) :: arr(:)
            integer :: i
            integer :: acc = 0

            do i = 1, ubound(arr)
                acc = {op}(acc, arr(i))
            end do
        end function''')
    omplooptrans = OMPLoopTrans(omp_directive="paralleldo")
    loop = psyir.walk(Loop)[0]
    omplooptrans.apply(loop, enable_reductions=True)
    output = fortran_writer(psyir)
    assert f"reduction({op}: acc)" in output


def test_bracketed_reductions(fortran_reader, fortran_writer):
    ''' Test that bracketed reductions are correctly detected.
    '''
    psyir = fortran_reader.psyir_from_source('''
        function sum_arr(arr) result (acc)
            integer, intent(in) :: arr(:)
            integer :: i
            integer :: acc = 0

            do i = 1, ubound(arr)
                acc = (arr(i) + i) + acc
            end do
        end function''')
    omplooptrans = OMPLoopTrans(omp_directive="paralleldo")
    loop = psyir.walk(Loop)[0]
    omplooptrans.apply(loop, enable_reductions=True)
    output = fortran_writer(psyir)
    assert "reduction(+: acc)" in output


def test_multiple_reductions(fortran_reader, fortran_writer):
    ''' Test that a loop containing multiple reductions is parallelised.
    '''
    psyir = fortran_reader.psyir_from_source('''
        function sum_arr(arr) result (acc)
            integer, intent(in) :: arr(:)
            integer :: i
            integer :: acc
            integer :: acc1 = 0
            integer :: acc2 = 1

            do i = 1, ubound(arr)
                acc1 = acc1 + arr(i)
                acc2 = acc2 * arr(i)
            end do
            acc = acc1 + acc2
        end function''')
    omplooptrans = OMPLoopTrans(omp_directive="paralleldo")
    loop = psyir.walk(Loop)[0]
    omplooptrans.apply(loop, enable_reductions=True)
    output = fortran_writer(psyir)
    assert "reduction(+: acc1)" in output
    assert "reduction(*: acc2)" in output


def test_conditional_reduction(fortran_reader, fortran_writer):
    ''' Test that a loop containing a reduction inside a conditional
    is parallelised.
    '''
    psyir = fortran_reader.psyir_from_source('''
        function sum_arr(arr) result (acc)
            integer, intent(in) :: arr(:)
            integer :: i
            integer :: acc = 0

            do i = 1, ubound(arr)
                if (arr(i) > 0) then
                    acc = acc + arr(i)
                end if
            end do
        end function''')
    omplooptrans = OMPLoopTrans(omp_directive="paralleldo")
    loop = psyir.walk(Loop)[0]
    omplooptrans.apply(loop, enable_reductions=True)
    output = fortran_writer(psyir)
    assert "reduction(+: acc)" in output


def test_multiple_reduction_same_var(fortran_reader, fortran_writer):
    ''' Test that a loop containing multiple reductions of the same
    operator/variable pair is parallelised.
    '''
    psyir = fortran_reader.psyir_from_source('''
        function sum_arr(arr) result (acc)
            integer, intent(in) :: arr(:)
            integer :: i
            integer :: acc = 0

            do i = 1, ubound(arr)
                acc = acc + arr(i)
                if (arr(i) > 0) then
                    acc = acc + 1
                end if
            end do
        end function''')
    omplooptrans = OMPLoopTrans(omp_directive="paralleldo")
    loop = psyir.walk(Loop)[0]
    omplooptrans.apply(loop, enable_reductions=True)
    output = fortran_writer(psyir)
    assert "reduction(+: acc)" in output


def test_multiple_reduction_same_var_diff_op(fortran_reader, fortran_writer):
    ''' Test that a loop containing multiple reductions of the same
    variable, but involve different operators, is not parallelised.
    '''
    psyir = fortran_reader.psyir_from_source('''
        function sum_arr(arr) result (acc)
            integer, intent(in) :: arr(:)
            integer :: i
            integer :: acc = 0

            do i = 1, ubound(arr)
                acc = acc + arr(i)
                if (arr(i) > 0) then
                    acc = acc * 2
                end if
            end do
        end function''')
    omplooptrans = OMPLoopTrans(omp_directive="paralleldo")
    loop = psyir.walk(Loop)[0]
    with pytest.raises(TransformationError) as err:
        omplooptrans.apply(loop, enable_reductions=True)
    # TODO #2446 and #514: improve this error message in future
    assert ("Variable 'acc' is read first, which indicates a reduction"
            in str(err.value))


def test_nested_reductions(fortran_reader, fortran_writer):
    ''' Tests that nested loops containing reductions can be collapsed.
    '''
    psyir = fortran_reader.psyir_from_source('''
        function sum_arr(arr) result (acc)
            integer, intent(in) :: arr(:, :)
            integer :: i, j
            integer :: acc = 0

            do j = 1, size(arr, 2)
                do i = 1, size(arr, 1)
                    acc = acc + arr(i, j)
                end do
            end do
        end function''')
    omplooptrans = OMPLoopTrans(omp_directive="paralleldo")
    loop = psyir.walk(Loop)[0]
    omplooptrans.apply(loop, enable_reductions=True, collapse=2)
    output = fortran_writer(psyir)
    assert "reduction(+: acc)" in output
    assert "collapse(2)" in output


def test_non_reduction1(fortran_reader, fortran_writer):
    ''' Test that a loop that looks like it contains a reduction (but
    doesn't), is not parallelised.
    '''
    psyir = fortran_reader.psyir_from_source('''
        function sub(arr1, arr2) result (count)
            integer, intent(in) :: arr1(:)
            integer, intent(out) :: arr2(:)
            integer :: i
            integer :: count = 1

            do i = 1, size(arr)
                if (arr1(i) > 0) then
                    count = count + 1
                end if
                arr2(i) = count
            end do
        end function''')
    omplooptrans = OMPLoopTrans(omp_directive="paralleldo")
    loop = psyir.walk(Loop)[0]
    with pytest.raises(TransformationError) as err:
        omplooptrans.apply(loop, enable_reductions=True)
    # TODO #2446 and #514: improve this error message in future
    assert ("Variable 'count' is read first, which indicates a reduction"
            in str(err.value))


def test_non_reduction2(fortran_reader, fortran_writer):
    ''' Test that x = x + x does not lead to a reduction clause.
    '''
    psyir = fortran_reader.psyir_from_source('''
        function sub(arr) result (count)
            integer, intent(in) :: arr(:)
            integer :: i
            integer :: count = 1

            do i = 1, size(arr)
                count = count + count
            end do
        end function''')
    omplooptrans = OMPLoopTrans(omp_directive="paralleldo")
    loop = psyir.walk(Loop)[0]
    with pytest.raises(TransformationError) as err:
        omplooptrans.apply(loop, enable_reductions=True)
    # TODO #2446 and #514: improve this error message in future
    assert ("Variable 'count' is read first, which indicates a reduction"
            in str(err.value))


def test_non_reduction3(fortran_reader, fortran_writer):
    ''' Test that x = x / 2 does not lead to a reduction clause.
    '''
    psyir = fortran_reader.psyir_from_source('''
        function sub(arr) result (count)
            integer, intent(in) :: arr(:)
            integer :: i
            integer :: count = 1

            do i = 1, size(arr)
                count = count / 2
            end do
        end function''')
    omplooptrans = OMPLoopTrans(omp_directive="paralleldo")
    loop = psyir.walk(Loop)[0]
    with pytest.raises(TransformationError) as err:
        omplooptrans.apply(loop, enable_reductions=True)
    # TODO #2446 and #514: improve this error message in future
    assert ("Variable 'count' is read first, which indicates a reduction"
            in str(err.value))


@pytest.mark.parametrize("d", ["teamsdistributeparalleldo", "teamsloop"])
def test_reduction_teams(d, fortran_reader, fortran_writer):
    ''' Test that reduction loops with a teams directive are parallelised.
    '''
    psyir = fortran_reader.psyir_from_source('''
        function sum_arr(arr) result (acc)
            integer, intent(in) :: arr(:)
            integer :: i
            integer :: acc = 0

            do i = 1, ubound(arr)
                acc = acc + arr(i)
            end do
        end function''')
    omplooptrans = OMPLoopTrans(omp_directive=d)
    loop = psyir.walk(Loop)[0]
    omplooptrans.apply(loop, options={"enable_reductions": True})
    output = fortran_writer(psyir)
    assert "reduction(+: acc)" in output


@pytest.mark.parametrize("d", ["do", "loop"])
def test_reduction_do_loop(d, fortran_reader, fortran_writer):
    ''' Test that reduction loops with a do/loop directive inside a parallel
        region are parallelised.
    '''
    psyir = fortran_reader.psyir_from_source('''
        function sum_arr(arr) result (acc)
            integer, intent(in) :: arr(:)
            integer :: i
            integer :: acc = 0

            do i = 1, ubound(arr)
                acc = acc + arr(i)
            end do
        end function''')
    omplooptrans = OMPLoopTrans(omp_directive=d)
    loop = psyir.walk(Loop)[0]
    OMPParallelTrans().apply(loop)
    omplooptrans.apply(loop, enable_reductions=True)
    output = fortran_writer(psyir)
    assert "reduction(+: acc)" in output


def test_reduction_omp_parallel_loop_trans(fortran_reader, fortran_writer):
    ''' Test that reduction loops are inferred in OMPParallelLoopTrans
    '''
    psyir = fortran_reader.psyir_from_source('''
        function sum_arr(arr) result (acc)
            integer, intent(in) :: arr(:)
            integer :: i
            integer :: acc = 0

            do i = 1, ubound(arr)
                acc = acc + arr(i)
            end do
        end function''')
    omplooptrans = OMPParallelLoopTrans()
    loop = psyir.walk(Loop)[0]
    omplooptrans.apply(loop, options={"enable_reductions": True})
    output = fortran_writer(psyir)
    assert "reduction(+: acc)" in output


def test_reduction_struct_member(fortran_reader, fortran_writer):
    ''' Test that reduction loops involving struct members are
    not parallelised. This is not yet supported by OpenMP.
    '''
    psyir = fortran_reader.psyir_from_source('''
        function sum_arr(arr) result (struct)
            use mod
            integer, intent(in) :: arr(:)
            integer :: i
            type(struct_type) :: struct

            struct%acc = 0
            do i = 1, ubound(arr)
                struct%acc = struct%acc + arr(i)
            end do
        end function''')
    omplooptrans = OMPLoopTrans(omp_directive="paralleldo")
    loop = psyir.walk(Loop)[0]
    with pytest.raises(TransformationError) as err:
        omplooptrans.apply(loop, enable_reductions=True)
    # TODO #2446 and #514: improve this error message in future
    assert ("Variable 'struct%acc' is read first, which indicates a reduction"
            in str(err.value))


def test_reduction_private_clash(fortran_reader, fortran_writer):
    '''Test that a variable does not occur in both a reduction clause
       and a private clause.'''
    psyir = fortran_reader.psyir_from_source('''
        function or_arr(arr) result (acc)
            logical, intent(in) :: arr(:)
            integer :: i
            logical :: acc = .false.

            do i = 1, ubound(arr)
                if (arr(i)) then
                    acc = .true.
                end if
            end do
        end function''')
    loop = psyir.walk(Loop)[0]
    loop_parent = loop.parent
    loop_position = loop.position
    do_directive = OMPParallelDoDirective(children=[loop.detach()])
    loop_parent.addchild(do_directive, index=loop_position)
    clause = OMPReductionClause(OMPReductionClause.ReductionClauseTypes.OR)
    clause.addchild(Reference(Symbol("acc")))
    do_directive.addchild(clause)
    output = fortran_writer(psyir)
    assert "reduction(.OR.: acc)" in output
    private_search = re.search("private\\((.*?)\\)", output)
    if private_search:
        assert "acc" not in private_search.group(1)


def test_firstprivate_with_uninitialised(fortran_reader, fortran_writer):
    ''' Check that guaranteed uninitialised symbols are not put in
    firstprivate clauses. '''
    code = '''
    module test
        integer :: a
    contains
        subroutine my_subroutine(b, cond)
            integer, intent(inout) :: b, cond
            integer :: c = 1, d, i, result
            integer :: not_initialised

            d = 1

            do i = 10, 10, 1
                if(cond < 1) then
                    a = 1
                    b = 1
                    c = 1
                    d = 1
                    not_initialised = 1
                    result = a + b + c + d + not_initialised
                endif
            end do
        end subroutine
    end module
    '''
    psyir = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelLoopTrans()
    loops = psyir.walk(Loop)
    ptrans.apply(loops[0])
    output = fortran_writer(psyir)
    assert "private(i,not_initialised)" in output
    assert "firstprivate(a,b,c,d)" in output

    # Check that complex initialised cases, such as Codeblocks and
    # initialisations below the loop are caught as a firstprivate
    code = '''
    module test
    contains
        subroutine my_subroutine(cond)
            integer, intent(inout) :: cond
            integer :: a, b, i, j, result

            read(*,*) b

            do j = 1, 10
                if (j .neq. 1) then
                    do i = 10, 10, 1
                        if(cond < 1) then
                            a = 1
                        endif
                        result = a
                    end do
                endif
                a = 1
            end do
            do i = 10, 10, 1
                if(cond < 1) then
                    b = 1
                endif
                result = b
            end do
        end subroutine
    end module
    '''
    psyir = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelLoopTrans()
    loops = psyir.walk(Loop)
    ptrans.apply(loops[1])
    ptrans.apply(loops[2])
    output = fortran_writer(psyir)
    assert "firstprivate(a)" in output
    assert "firstprivate(b)" in output
