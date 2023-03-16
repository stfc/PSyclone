# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2023, Science and Technology Facilities Council.
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
# -----------------------------------------------------------------------------

''' Performs py.test tests on the OpenMP PSyIR Directive nodes. '''

import os
import pytest
from psyclone.f2pygen import ModuleGen
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir import nodes
from psyclone import psyGen
from psyclone.psyir.nodes import OMPDoDirective, OMPParallelDirective, \
    OMPParallelDoDirective, OMPMasterDirective, OMPTaskloopDirective, \
    OMPTaskwaitDirective, OMPTargetDirective, OMPLoopDirective, Schedule, \
    Return, OMPSingleDirective, Loop, Literal, Routine, Assignment, \
    Reference, OMPDeclareTargetDirective, OMPNowaitClause, \
    OMPGrainsizeClause, OMPNumTasksClause, OMPNogroupClause, \
    OMPPrivateClause, OMPDefaultClause, OMPReductionClause, \
    OMPScheduleClause, OMPTeamsDistributeParallelDoDirective, \
    OMPFirstprivateClause
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, SymbolTable, \
    REAL_SINGLE_TYPE, INTEGER_SINGLE_TYPE, Symbol
from psyclone.errors import InternalError, GenerationError
from psyclone.transformations import Dynamo0p3OMPLoopTrans, OMPParallelTrans, \
    OMPParallelLoopTrans, DynamoOMPParallelLoopTrans, OMPSingleTrans, \
    OMPMasterTrans, OMPTaskloopTrans
from psyclone.tests.utilities import get_invoke

BASE_PATH = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__)))), "test_files", "dynamo0p3")
GOCEAN_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                os.pardir, os.pardir, "test_files",
                                "gocean1p0")


def test_ompparallel_changes_begin_string(fortran_reader):
    ''' Check that when the code inside an OMP Parallel region changes, the
    parallel clause changes appropriately. '''
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

    # Make acopy of the loop
    new_loop = pdir.children[0].children[0].children[0].children[0].copy()
    # Change the loop variable to j
    jvar = DataSymbol("j", INTEGER_SINGLE_TYPE)
    new_loop.variable = jvar
    # Add loop
    pdir.children[0].addchild(new_loop)

    pdir.lower_to_language_level()
    assert pdir.children[2] is not priv_clause


def test_ompparallel_changes_gen_code(monkeypatch):
    ''' Check that when the code inside an OMP Parallel region changes, the
    private clause changes appropriately. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke_w3.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    tree = psy.invokes.invoke_list[0].schedule
    ptrans = OMPParallelTrans()
    tdir = OMPDoDirective()
    loops = tree.walk(Loop)
    loop = loops[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    ptrans.apply(loops[0].parent.parent)

    assert isinstance(tree.children[0], OMPParallelDirective)
    pdir = tree.children[0]
    code = str(psy.gen).lower()
    assert len(pdir.children) == 4
    assert "private(cell)" in code

    # Make acopy of the loop
    new_loop = pdir.children[0].children[0].children[0].children[0].copy()
    routine = pdir.ancestor(Routine)
    routine.symbol_table.add(DataSymbol("k", INTEGER_SINGLE_TYPE))
    # Change the loop variable to j
    jvar = DataSymbol("k", INTEGER_SINGLE_TYPE)
    new_loop.variable = jvar
    tdir2 = OMPDoDirective()
    tdir2.children[0].addchild(new_loop)
    # Add loop
    pdir.children[0].addchild(tdir2)

    code = str(psy.gen).lower()
    assert "private(cell,k)" in code

    # Monkeypatch a case with private and firstprivate clauses
    pclause = OMPPrivateClause(children=[Reference(Symbol("a"))])
    fpclause = OMPFirstprivateClause(children=[Reference(Symbol("b"))])
    monkeypatch.setattr(pdir, "_get_private_clauses",
                        lambda: (pclause, fpclause))

    code = str(psy.gen).lower()
    assert "private(a)" in code
    assert "firstprivate(b)" in code


def test_omp_paralleldo_changes_gen_code(monkeypatch):
    ''' Check that when the code inside an OMP Parallel Do region changes, the
    private clause changes appropriately. Also check that changing the schedule
    is correctly picked up.'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke_w3.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    tree = psy.invokes.invoke_list[0].schedule
    ptrans = OMPParallelLoopTrans()
    loops = tree.walk(Loop)
    ptrans.apply(loops[0])

    assert isinstance(tree.children[0], OMPParallelDoDirective)
    pdir = tree.children[0]
    code = str(psy.gen).lower()
    assert len(pdir.children) == 2
    assert "private(cell)" in code
    assert "schedule(auto)" in code
    assert "firstprivate" not in code

    # Modify the loop
    routine = pdir.ancestor(Routine)
    routine.symbol_table.add(DataSymbol("k", INTEGER_SINGLE_TYPE))
    # Change the loop variable to k
    kvar = DataSymbol("k", INTEGER_SINGLE_TYPE)
    pdir.children[0].children[0].variable = kvar
    # Change the schedule to 'none'
    pdir.omp_schedule = "none"

    # No 'schedule' clause should now be present on the OMP directive.
    code = str(psy.gen).lower()
    assert "schedule(" not in code
    assert "private(k)" in code
    assert "firstprivate" not in code

    # Monkeypatch a case with firstprivate clauses
    pclause = OMPPrivateClause(children=[Reference(Symbol("a"))])
    fpclause = OMPFirstprivateClause(children=[Reference(Symbol("b"))])
    monkeypatch.setattr(pdir, "_get_private_clauses",
                        lambda: (pclause, fpclause))

    code = str(psy.gen).lower()
    assert "private(a)" in code
    assert "firstprivate(b)" in code


def test_omp_parallel_do_changes_begin_str(fortran_reader):
    ''' Check that when the code inside an OMP Parallel Do region changes, the
    private clause changes appropriately. '''
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
    loop = loops[0]
    loop.loop_type = None
    ptrans.apply(loops[0])

    assert isinstance(tree.children[0].children[0], OMPParallelDoDirective)
    pdir = tree.children[0].children[0]
    pdir.lower_to_language_level()
    assert len(pdir.children) == 5
    assert isinstance(pdir.children[2], OMPPrivateClause)
    assert isinstance(pdir.children[3], OMPFirstprivateClause)
    priv_clause = pdir.children[2]
    fpriv_clause = pdir.children[3]
    sched_clause = pdir.children[4]

    # Make acopy of the loop
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
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
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


def test_omp_pdo_validate_child():
    ''' Test the _validate_child method for OMPParallelDoDirective'''
    sched = Schedule()
    declause = OMPDefaultClause()
    prclause = OMPPrivateClause()
    fprclause = OMPFirstprivateClause()
    scclause = OMPScheduleClause()
    reclause = OMPReductionClause()

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
    ompdo2 = OMPDoDirective(children=[loop2], reprod=(not ompdo1.reprod))
    assert ompdo1 != ompdo2


def test_omp_do_children_err():
    ''' Tests that we raise the expected error when an OpenMP parallel do
    directive has more than one child or the child is not a loop. '''
    otrans = OMPParallelLoopTrans()
    psy, invoke_info = get_invoke("imperfect_nest.f90", api="nemo", idx=0)
    schedule = invoke_info.schedule
    otrans.apply(schedule[0].loop_body[2])
    directive = schedule[0].loop_body[2]
    assert isinstance(directive, OMPParallelDoDirective)
    # Make the schedule invalid by adding a second child to the
    # OMPParallelDoDirective
    directive.dir_body.children.append(directive.dir_body[0].copy())
    with pytest.raises(GenerationError) as err:
        _ = psy.gen
    assert ("An OMPParallelDoDirective can only be applied to a single loop "
            "but this Node has 2 children:" in str(err.value))
    directive.dir_body.children = [Return()]
    with pytest.raises(GenerationError) as err:
        _ = psy.gen
    assert ("An OMPParallelDoDirective can only be applied to a loop but "
            "this Node has a child of type 'Return'" in str(err.value))


def test_directive_get_private_lfric(monkeypatch):
    ''' Tests for the _get_private_clauses() method of OMPParallelDirective.
    Note: this test does not apply colouring so the loops must be over
    discontinuous function spaces.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke_w3.f90"), api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # We use Transformations to introduce the necessary directives
    otrans = Dynamo0p3OMPLoopTrans()
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
    # Now check that _get_private_clause returns what we expect
    pvars, fpvars = directive._get_private_clauses()
    assert isinstance(pvars, OMPPrivateClause)
    assert isinstance(fpvars, OMPFirstprivateClause)
    assert len(pvars.children) == 1
    assert len(fpvars.children) == 0
    assert pvars.children[0].name == 'cell'

    directive.children[1] = OMPDefaultClause(
            clause_type=OMPDefaultClause.DefaultClauseTypes.NONE)
    with pytest.raises(GenerationError) as excinfo:
        _ = directive._get_private_clauses()
    assert ("OMPParallelClause cannot correctly generate the private clause "
            "when its default data sharing attribute in its default clause is "
            "not shared." in str(excinfo.value))


def test_directive_get_private(fortran_reader):
    ''' Tests for the _get_private_clauses() method of OpenMP directives.'''

    # Example with private and firstprivate variables on OMPParallelDoDirective
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
    pvars, fpvars = directive._get_private_clauses()
    assert len(pvars.children) == 2
    assert len(fpvars.children) == 1
    assert pvars.children[0].name == 'i'
    assert pvars.children[1].name == 'scalar2'
    assert fpvars.children[0].name == 'scalar1'

    # Another example with OMPParallelDirective (not actual worksharing)
    # and scalars set outside the loop (this should be shared by convention)
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
    omplooptrans = OMPParallelTrans()
    loop = psyir.walk(Routine)[0]
    omplooptrans.apply(loop.children)
    directive = psyir.walk(OMPParallelDirective)[0]
    pvars, fpvars = directive._get_private_clauses()
    assert len(pvars.children) == 2
    assert len(fpvars.children) == 0
    assert pvars.children[0].name == 'i'
    assert pvars.children[1].name == 'scalar2'
    # scalar 1 is shared because is read-only and scalar3 and scalar4 are
    # shared because they are set outside a loop


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
    directive = psyir.walk(OMPParallelDoDirective)[0]
    pvars, fpvars = directive._get_private_clauses()
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


def test_omp_parallel_validate_child():
    ''' Test the validate_child method of OMPParallelDirective'''
    assert OMPParallelDirective._validate_child(0, Schedule()) is True
    assert OMPParallelDirective._validate_child(1, OMPDefaultClause()) is True
    assert OMPParallelDirective._validate_child(2, OMPPrivateClause()) is True
    assert OMPParallelDirective._validate_child(3, OMPFirstprivateClause()) \
        is True
    assert OMPParallelDirective._validate_child(2, OMPReductionClause())\
        is False
    assert OMPParallelDirective._validate_child(4, OMPReductionClause())\
        is True
    assert OMPParallelDirective._validate_child(5, OMPReductionClause())\
        is True
    assert OMPParallelDirective._validate_child(0, OMPDefaultClause()) is False
    assert OMPParallelDirective._validate_child(6, "test") is False


def test_omp_forward_dependence():
    '''Test that the forward_dependence method works for Directives,
    returning the closest dependent Node after the current Node in the
    schedule or None if none are found. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.1_multi_aX_plus_Y_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    otrans = DynamoOMPParallelLoopTrans()
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
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
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
                           api="gocean1.0")
    single = OMPSingleTrans()
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
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
                           api="dynamo0.3")
    single = OMPSingleTrans()
    psy = PSyFactory("dynamo0.3", distributed_memory=False).\
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
                           api="dynamo0.3")
    single = OMPSingleTrans()
    # Alternative excluded node types for monkeypatch
    excluded_node_types = (nodes.CodeBlock, nodes.Return, nodes.ACCDirective,
                           psyGen.HaloExchange, nodes.OMPParallelDirective)
    monkeypatch.setattr(single, "excluded_node_types", excluded_node_types)
    parallel = OMPParallelTrans()
    psy = PSyFactory("dynamo0.3", distributed_memory=False).\
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


@pytest.mark.parametrize("nowait", [False, True])
def test_omp_single_gencode(nowait):
    '''Check that the gen_code method in the OMPSingleDirective class
    generates the expected code.
    '''
    subroutine = Routine("testsub")
    temporary_module = ModuleGen("test")
    parallel = OMPParallelDirective.create()
    single = OMPSingleDirective(nowait=nowait)
    parallel.dir_body.addchild(single)
    subroutine.addchild(parallel)
    parallel.gen_code(temporary_module)

    clauses = ""
    if nowait:
        clauses += " nowait"

    assert "!$omp single" + clauses + "\n" in str(temporary_module.root)
    assert "!$omp end single\n" in str(temporary_module.root)


def test_omp_master_strings():
    ''' Test the begin_string and end_string methods of the OMPMaster
        directive '''
    omp_master = OMPMasterDirective()

    assert omp_master.begin_string() == "omp master"
    assert omp_master.end_string() == "omp end master"


def test_omp_master_gencode():
    '''Check that the gen_code method in the OMPMasterDirective class
    generates the expected code.
    '''
    subroutine = Routine("testsub")
    temporary_module = ModuleGen("test")
    parallel = OMPParallelDirective.create()
    master = OMPMasterDirective()
    parallel.dir_body.addchild(master)
    subroutine.addchild(parallel)
    parallel.gen_code(temporary_module)

    assert "!$omp master\n" in str(temporary_module.root)
    assert "!$omp end master\n" in str(temporary_module.root)


def test_omp_master_validate_global_constraints():
    ''' Test the validate_global_constraints method of the OMPMaster
        directive '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    master = OMPMasterTrans()
    psy = PSyFactory("dynamo0.3", distributed_memory=False).\
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
                           api="dynamo0.3")
    master = OMPMasterTrans()
    # Alternative excluded node types for monkeypatch
    excluded_node_types = (nodes.CodeBlock, nodes.Return, nodes.ACCDirective,
                           psyGen.HaloExchange, nodes.OMPParallelDirective)
    monkeypatch.setattr(master, "excluded_node_types", excluded_node_types)
    parallel = OMPParallelTrans()
    psy = PSyFactory("dynamo0.3", distributed_memory=False).\
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


def test_omptaskwait_strings():
    ''' Test the begin_string and method of the OMPTaskwait directive '''
    taskwait = OMPTaskwaitDirective()

    assert taskwait.begin_string() == "omp taskwait"


def test_omptaskwait_gencode():
    '''Check that the gen_code method in the OMPTaskwaitDirective
    class generates the expected code.
    '''
    subroutine = Routine("testsub")
    temporary_module = ModuleGen("test")
    parallel = OMPParallelDirective.create()
    directive = OMPTaskwaitDirective()
    parallel.dir_body.addchild(directive)
    subroutine.addchild(parallel)
    parallel.gen_code(temporary_module)

    assert "!$omp taskwait\n" in str(temporary_module.root)


def test_omp_taskwait_validate_global_constraints():
    ''' Test the validate_global_constraints method of the OMPTaskwait
        directive '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    taskwait = OMPTaskwaitDirective()
    schedule.addchild(taskwait, 0)
    with pytest.raises(GenerationError) as excinfo:
        taskwait.validate_global_constraints()
    assert ("OMPTaskwaitDirective must be inside an OMP parallel region but "
            "could not find an ancestor OMPParallelDirective node"
            in str(excinfo.value))


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
    assert omp_taskloop.clauses == []


def test_omp_taskloop_init():
    ''' Test the constructor of the OMPTaskloop directive'''
    with pytest.raises(GenerationError) as excinfo:
        OMPTaskloopDirective(grainsize=32, num_tasks=32)
    assert ("OMPTaskloopDirective must not have both grainsize and "
            "numtasks clauses specified.") in str(excinfo.value)


@pytest.mark.parametrize("grainsize,num_tasks,nogroup,clauses",
                         [(None, None, False, ""),
                          (32, None, False, " grainsize(32)"),
                          (None, 32, True, " num_tasks(32), nogroup")])
def test_omp_taskloop_gencode(grainsize, num_tasks, nogroup, clauses):
    '''Check that the gen_code method in the OMPTaskloopDirective
    class generates the expected code.
    '''
    temporary_module = ModuleGen("test")
    subroutine = Routine("testsub")
    parallel = OMPParallelDirective.create()
    single = OMPSingleDirective()
    directive = OMPTaskloopDirective(grainsize=grainsize, num_tasks=num_tasks,
                                     nogroup=nogroup)
    parallel.dir_body.addchild(single)
    single.dir_body.addchild(directive)
    sym = subroutine.symbol_table.new_symbol(
            "i", symbol_type=DataSymbol, datatype=INTEGER_TYPE)
    loop = Loop.create(sym,
                       Literal("1", INTEGER_TYPE),
                       Literal("10", INTEGER_TYPE),
                       Literal("1", INTEGER_TYPE),
                       [])
    directive.dir_body.addchild(loop)
    subroutine.addchild(parallel)
    parallel.gen_code(temporary_module)

    assert "!$omp taskloop" + clauses + "\n" in str(temporary_module.root)
    assert "!$omp end taskloop\n" in str(temporary_module.root)


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
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    taskloop = OMPTaskloopTrans()
    psy = PSyFactory("dynamo0.3", distributed_memory=False).\
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


# Test OMPDeclareTargetDirective

def test_omp_declare_target_directive_constructor_and_strings(monkeypatch):
    ''' Test the OMPDeclareTargetDirective constructor and its output
    strings.'''
    target = OMPDeclareTargetDirective()
    assert target.begin_string() == "omp declare target"
    assert str(target) == "OMPDeclareTargetDirective[]"

    monkeypatch.setattr(target, "validate_global_constraints", lambda: None)
    temporary_module = ModuleGen("test")
    target.gen_code(temporary_module)
    assert "!$omp declare target\n" in str(temporary_module.root)


def test_omp_declare_target_directive_validate_global_constraints():
    ''' Test the OMPDeclareTargetDirective is only valid as the first child
    of a Routine'''
    target = OMPDeclareTargetDirective()

    # If the directive is detached it passes the validation
    target.validate_global_constraints()

    # If it is the child 0 of a Routine it passes the tests
    subroutine = Routine("test")
    subroutine.addchild(target)
    target.validate_global_constraints()

    subroutine.children.insert(0, target.copy())
    with pytest.raises(GenerationError) as err:
        target.validate_global_constraints()
    assert ("A OMPDeclareTargetDirective must be the first child (index 0) of "
            "a Routine but found one as child 1 of a Routine."
            in str(err.value))


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
