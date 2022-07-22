# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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
    Reference, OMPTaskDirective, OMPPrivateClause, OMPDefaultClause,\
    OMPReductionClause, OMPFirstprivateClause, OMPSharedClause, \
    OMPDependClause
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, SymbolTable, \
    REAL_SINGLE_TYPE, INTEGER_SINGLE_TYPE
from psyclone.errors import InternalError, GenerationError
from psyclone.transformations import Dynamo0p3OMPLoopTrans, OMPParallelTrans, \
    OMPParallelLoopTrans, DynamoOMPParallelLoopTrans, OMPSingleTrans, \
    OMPMasterTrans, OMPTaskloopTrans, OMPLoopTrans
from psyclone.tests.utilities import get_invoke

BASE_PATH = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__)))), "test_files", "dynamo0p3")
GOCEAN_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                os.pardir, os.pardir, "test_files",
                                "gocean1p0")


def test_ompparallel_changes_begin_string(fortran_reader, fortran_writer):
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
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    tdir = OMPDoDirective()
    loops = tree.walk(Loop)
    loop = loops[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    ptrans.apply(loops[0].parent.parent)
    assert type(tree.children[0].children[0]) is OMPParallelDirective
    pdir = tree.children[0].children[0]
    assert pdir.begin_string() == "omp parallel"
    assert len(pdir.children) == 3
    assert isinstance(pdir.children[2], OMPPrivateClause)
    priv_clause = pdir.children[2]

    # Make acopy of the loop
    new_loop = pdir.children[0].children[0].children[0].children[0].copy()
    # Change the loop variable to j
    jvar = DataSymbol("j", INTEGER_SINGLE_TYPE)
    new_loop.variable = jvar
    # Add loop
    pdir.children[0].addchild(new_loop)

    pdir.begin_string()
    assert pdir.children[2] != priv_clause
    pdir.children[2].detach()

    reduc = OMPReductionClause()
    pdir.addchild(reduc)
    # Check we correctly place the private clause again
    pdir.begin_string()
    assert isinstance(pdir.children[2], OMPPrivateClause)

def test_ompparallel_changes_gen_code():
    ''' Check that when the code inside an OMP Parallel region changes, the
    parallel clause changes appropriately. '''
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

    assert type(tree.children[0]) is OMPParallelDirective
    pdir = tree.children[0]
    psy.gen
    assert len(pdir.children) == 3
    assert isinstance(pdir.children[2], OMPPrivateClause)
    priv_clause = pdir.children[2]

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

    psy.gen
    assert pdir.children[2] != priv_clause
    pdir.children[2].detach()

    reduc = OMPReductionClause()
    pdir.addchild(reduc)
    # Check we correctly place the private clause again
    psy.gen
    assert isinstance(pdir.children[2], OMPPrivateClause)


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


def test_directive_get_private(monkeypatch):
    ''' Tests for the _get_private_clause() method of OMPParallelDirective.
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
    pvars = directive._get_private_clause()
    #assert pvars == ['cell']
    assert isinstance(pvars, OMPPrivateClause)
    assert len(pvars.children) == 1
    assert pvars.children[0].name == 'cell'
    # Now use monkeypatch to break the Call within the loop
    call = directive.dir_body[0].dir_body[0].loop_body[0]
    monkeypatch.setattr(call, "local_vars", lambda: [""])
    with pytest.raises(InternalError) as err:
        _ = directive._get_private_clause()
    assert ("call 'testkern_w3_code' has a local variable but its name is "
            "not set" in str(err.value))

def test_omp_private_validate_child():
    assert OMPParallelDirective._validate_child(0, Schedule()) is True
    assert OMPParallelDirective._validate_child(1, OMPDefaultClause()) is True
    assert OMPParallelDirective._validate_child(2, OMPPrivateClause()) is True
    assert OMPParallelDirective._validate_child(2, OMPReductionClause())\
           is True
    assert OMPParallelDirective._validate_child(3, OMPReductionClause())\
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
    parallel = OMPParallelDirective()
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
    parallel = OMPParallelDirective()
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
    parallel = OMPParallelDirective()
    directive = OMPTaskwaitDirective()
    sym = subroutine.symbol_table.new_symbol(
            "i", symbol_type=DataSymbol, datatype=INTEGER_TYPE)
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
    assert omp_taskwait.clauses == []


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
    assert("OMPTaskloopDirective must not have both grainsize and "
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
    parallel = OMPParallelDirective()
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
    assert OMPTaskloopDirective._validate_child(0, lit) is False
    assert OMPTaskloopDirective._validate_child(1, lit) is False
    assert OMPTaskloopDirective._validate_child(2, lit) is False


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


def test_omp_task_directive_validate_global_constraints():
    ''' Test the validate_global_constraints method of the 
    OMPTaskDirective'''
    node = OMPTaskDirective()
    with pytest.raises(GenerationError) as excinfo:
        node.validate_global_constraints()
    assert ("OMPTaskDirective must be inside an OMP Serial region but could"
            " not find an ancestor node.") in str(excinfo.value)

def test_omp_task_validate_child():
    ''' Test the validate_child method of the OMPTaskDirective'''
    assert OMPTaskDirective._validate_child(0, Schedule()) is True
    assert OMPTaskDirective._validate_child(1, OMPPrivateClause()) is True
    assert OMPTaskDirective._validate_child(2, OMPFirstprivateClause()) is True
    assert OMPTaskDirective._validate_child(3, OMPSharedClause()) is True
    assert OMPTaskDirective._validate_child(4, OMPDependClause()) is True
    assert OMPTaskDirective._validate_child(5, OMPDependClause()) is True
    assert OMPTaskDirective._validate_child(6, OMPDependClause()) is False
    assert OMPTaskDirective._validate_child(0, "string") is False
    assert OMPTaskDirective._validate_child(1, "string") is False
    assert OMPTaskDirective._validate_child(2, "string") is False
    assert OMPTaskDirective._validate_child(3, "string") is False
    assert OMPTaskDirective._validate_child(4, "string") is False
    assert OMPTaskDirective._validate_child(5, "string") is False

def test_omp_task_directive_1(fortran_reader, fortran_writer):
    ''' Test a basic code generation with the task directive applied to a 
    loop which accesses the full arrays.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(10, 10) :: A
        integer, dimension(10, 10) :: B
        integer :: i
        integer :: j
        do i = 1, 10
            do j = 1, 10
                A(i, j) = B(i, j) + 1
            end do
        end do
        do i = 1, 10
            do j = 1, 10
                A(i, j) = 0
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(parent.children)
    ptrans.apply(parent.children)
    correct = '''subroutine my_subroutine()
  integer, dimension(10,10) :: a
  integer, dimension(10,10) :: b
  integer :: i
  integer :: j

  !$omp parallel default(shared) private(i,j)
  !$omp single
  !$omp task private(i,j) shared(a,b) depend(in: b(:,:)) depend(out: a(:,:))
  do i = 1, 10, 1
    do j = 1, 10, 1
      a(i,j) = b(i,j) + 1
    enddo
  enddo
  !$omp end task
  do i = 1, 10, 1
    do j = 1, 10, 1
      a(i,j) = 0
    enddo
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_2(fortran_reader, fortran_writer):
    ''' Test the code generation fails when attempting to access an array
    when using an array element as an index.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(10, 10) :: A
        integer, dimension(10, 10) :: B
        integer :: i
        integer :: j
        do i = 1, 10
            do j = 1, 10
                A(i, j) = B(B(1,2), j) + 1
            end do
        end do
        do i = 1, 10
            do j = 1, 10
                A(i, j) = 0
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(parent.children)
    ptrans.apply(parent.children)
    with pytest.raises(GenerationError) as excinfo:
        fortran_writer(tree)
    assert ("ArrayReference object is not allowed to appear in an Array Index "
            "expression inside an OMPTaskDirective.") in str(excinfo.value)

def test_omp_task_directive_3(fortran_reader, fortran_writer):
    '''Test the code generation correctly captures if a variable should be
    declared as firstprivate.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(10, 10) :: A
        integer, dimension(10, 10) :: B
        integer :: i
        integer :: j
        integer :: k
        do i = 1, 10
            k = i
        end do
        do i = 1, 10
            do j = 1, 10
                A(i, j) = k
                A(i, j) = B(i, j) + k
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[1]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=1)
    strans.apply(parent.children[1])
    ptrans.apply(parent.children)
    correct = '''subroutine my_subroutine()
  integer, dimension(10,10) :: a
  integer, dimension(10,10) :: b
  integer :: i
  integer :: j
  integer :: k

  !$omp parallel default(shared) private(i,j,k)
  do i = 1, 10, 1
    k = i
  enddo
  !$omp single
  !$omp task private(i,j) firstprivate(k) shared(a,b) depend(in: b(:,:)) depend(out: a(:,:))
  do i = 1, 10, 1
    do j = 1, 10, 1
      a(i,j) = k
      a(i,j) = b(i,j) + k
    enddo
  enddo
  !$omp end task
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_4(fortran_reader, fortran_writer):
    ''' Test the code generation correctly makes the depend clause when
    accessing an input array shifted by the step size of the outer loop.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(10, 10) :: A
        integer, dimension(11, 10) :: B
        integer :: i
        integer :: j
        integer :: k
        do i = 1, 10
            do j = 1, 10
                A(i, j) = k
                A(i, j) = B(i+1, j) + k
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(10,10) :: a
  integer, dimension(11,10) :: b
  integer :: i
  integer :: j
  integer :: k

  !$omp parallel default(shared) private(i,j)
  !$omp single
  do i = 1, 10, 1
    !$omp task private(j) firstprivate(i) shared(a,b) depend(in: k,b(i + 1,:)) depend(out: a(i,:))
    do j = 1, 10, 1
      a(i,j) = k
      a(i,j) = b(i + 1,j) + k
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_5(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when an input array is shifted by less than a full step of the outer loop.
    This is not quite a real use-case, however its a first check for this 
    idea.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(320, 10) :: A
        integer, dimension(321, 10) :: B
        integer :: i
        integer :: j
        integer :: k
        do i = 1, 320, 32
            do j = 1, 32
                A(i, j) = k
                A(i, j) = B(i+1, j) + k
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(320,10) :: a
  integer, dimension(321,10) :: b
  integer :: i
  integer :: j
  integer :: k

  !$omp parallel default(shared) private(i,j)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(j) firstprivate(i) shared(a,b) depend(in: k,b(i + 32,:),b(i,:)) depend(out: a(i,:))
    do j = 1, 32, 1
      a(i,j) = k
      a(i,j) = b(i + 1,j) + k
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_6(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when an input array is shifted by less than a full step of the outer loop.
    This is expected to be similar to a real use-case.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j
        integer :: k
        do i = 1, 320, 32
            do ii=i, i+32
                do j = 1, 32
                    A(ii, j) = B(ii+1, j) + k
                end do
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(321,10) :: a
  integer, dimension(32,10) :: b
  integer :: i
  integer :: ii
  integer :: j
  integer :: k

  !$omp parallel default(shared) private(i,ii,j)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(ii,j) firstprivate(i) shared(a,b) depend(in: b(i + 32,:),b(i,:),k) depend(out: a(i,:))
    do ii = i, i + 32, 1
      do j = 1, 32, 1
        a(ii,j) = b(ii + 1,j) + k
      enddo
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct

def test_omp_task_directive_7(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when an input array is shifted by less than a full step of the outer loop.
    This is expected to be similar to a real use-case. In this case, we have
    multiple loops to handle. '''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 321) :: A
        integer, dimension(321, 321) :: B
        integer :: i, ii
        integer :: j, jj
        integer :: k
        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32
                    do jj = j,j+32
                        A(ii,jj) = B(ii+1,jj+1) * k
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[1].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(321,321) :: a
  integer, dimension(321,321) :: b
  integer :: i
  integer :: ii
  integer :: j
  integer :: jj
  integer :: k

  !$omp parallel default(shared) private(i,ii,j,jj)
  !$omp single
  do i = 1, 320, 32
    do j = 1, 320, 32
      !$omp task private(ii,jj) firstprivate(i,j) shared(a,b) depend(in: b(i + 32,j + 32),b(i + 32,j),b(i,j + 32),b(i,j),k) depend(out: a(i,j))
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          a(ii,jj) = b(ii + 1,jj + 1) * k
        enddo
      enddo
      !$omp end task
    enddo
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_8(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when an input array is shifted by a mixture of steps of the chunked loop.
    This is expected to be similar to a real use-case. In this case, we have
    multiple loops to handle. '''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 354) :: A
        integer, dimension(321, 354) :: B
        integer :: i, ii
        integer :: j, jj
        integer :: k
        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32
                    do jj = j,j+32
                        A(ii,jj) = B(ii+1,jj+33) * k
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[1].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(321,354) :: a
  integer, dimension(321,354) :: b
  integer :: i
  integer :: ii
  integer :: j
  integer :: jj
  integer :: k

  !$omp parallel default(shared) private(i,ii,j,jj)
  !$omp single
  do i = 1, 320, 32
    do j = 1, 320, 32
      !$omp task private(ii,jj) firstprivate(i,j) shared(a,b) depend(in: b(i + 32,j + 2 * 32),b(i + 32,j + 32),b(i,j + 2 * 32),b(i,j + 32),k) depend(out: a(i,j))
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          a(ii,jj) = b(ii + 1,jj + 33) * k
        enddo
      enddo
      !$omp end task
    enddo
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct

def test_omp_task_directive_9(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when an output array is shifted by less than a full step of the outer loop.
    This is expected to be similar to a real use-case.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 32) :: A
        integer, dimension(321, 32) :: B
        integer :: i, ii
        integer :: j
        integer :: k
        do i = 1, 320, 32
            do ii=i, i+32
                do j = 1, 32
                    A(ii+1, j) = B(ii, j) + k
                end do
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(321,32) :: a
  integer, dimension(321,32) :: b
  integer :: i
  integer :: ii
  integer :: j
  integer :: k

  !$omp parallel default(shared) private(i,ii,j)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(ii,j) firstprivate(i) shared(a,b) depend(in: b(i,:),k) depend(out: a(i + 32,:),a(i,:))
    do ii = i, i + 32, 1
      do j = 1, 32, 1
        a(ii + 1,j) = b(ii,j) + k
      enddo
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_10(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when an output array is shifted by less than a full step of the outer loop.
    This is expected to be similar to a real use-case. In this case, we have
    multiple loops to handle. '''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 321) :: A
        integer, dimension(321, 321) :: B
        integer :: i, ii
        integer :: j, jj
        integer :: k
        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32
                    do jj = j,j+32
                        A(ii+1,jj+1) = B(ii,jj) * k
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[1].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(321,321) :: a
  integer, dimension(321,321) :: b
  integer :: i
  integer :: ii
  integer :: j
  integer :: jj
  integer :: k

  !$omp parallel default(shared) private(i,ii,j,jj)
  !$omp single
  do i = 1, 320, 32
    do j = 1, 320, 32
      !$omp task private(ii,jj) firstprivate(i,j) shared(a,b) depend(in: b(i,j),k) depend(out: a(i + 32,j + 32),a(i + 32,j),a(i,j + 32),a(i,j))
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          a(ii + 1,jj + 1) = b(ii,jj) * k
        enddo
      enddo
      !$omp end task
    enddo
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_11(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when an input array is shifted by a mixture of steps of the chunked loop.
    This is expected to be similar to a real use-case. In this case, we have
    multiple loops to handle. '''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 354) :: A
        integer, dimension(321, 354) :: B
        integer :: i, ii
        integer :: j, jj
        integer :: k
        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32
                    do jj = j,j+32
                        A(ii+1,jj+33) = B(ii,jj) * k
                        A(ii+1,jj+65) = B(ii,jj) * k
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[1].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(321,354) :: a
  integer, dimension(321,354) :: b
  integer :: i
  integer :: ii
  integer :: j
  integer :: jj
  integer :: k

  !$omp parallel default(shared) private(i,ii,j,jj)
  !$omp single
  do i = 1, 320, 32
    do j = 1, 320, 32
      !$omp task private(ii,jj) firstprivate(i,j) shared(a,b) depend(in: b(i,j),k) depend(out: a(i + 32,j + 2 * 32),a(i + 32,j + 32),a(i,j + 2 * 32),a(i,j + 32),a(i + 32,j + 3 * 32),a(i,j + 3 * 32))
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          a(ii + 1,jj + 33) = b(ii,jj) * k
          a(ii + 1,jj + 65) = b(ii,jj) * k
        enddo
      enddo
      !$omp end task
    enddo
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_12(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when an if statement is present. '''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 354) :: A
        integer, dimension(321, 354) :: B
        integer :: i, ii
        integer :: j, jj
        integer :: k
        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32
                    do jj = j,j+32
                        if (A(ii, jj) > 0.0) then
                            A(ii+1,jj) = B(ii,jj) * k
                        end if
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[1].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(321,354) :: a
  integer, dimension(321,354) :: b
  integer :: i
  integer :: ii
  integer :: j
  integer :: jj
  integer :: k

  !$omp parallel default(shared) private(i,ii,j,jj)
  !$omp single
  do i = 1, 320, 32
    do j = 1, 320, 32
      !$omp task private(ii,jj) firstprivate(i,j) shared(a,b) depend(in: a(i,j),b(i,j),k) depend(out: a(i + 32,j),a(i,j))
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          if (a(ii,jj) > 0.0) then
            a(ii + 1,jj) = b(ii,jj) * k
          end if
        enddo
      enddo
      !$omp end task
    enddo
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_mul_index_fail(fortran_reader, fortran_writer):
    ''' Test the code generation throws an Error when a multiplication is inside
    an index Binop. '''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 354) :: A
        integer, dimension(321, 354) :: B
        integer :: i, ii
        integer :: j, jj
        integer :: k
        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32
                    do jj = j,j+32
                        if (A(ii, jj) > 0.0) then
                            A(ii*3,jj) = B(ii,jj) * k
                        end if
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[1].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    with pytest.raises(GenerationError) as excinfo:
        fortran_writer(tree)
    assert ("Binary Operator of type Operator.MUL used as in index inside an "
            "OMPTaskDirective which is not supported" in str(excinfo.value))


def test_omp_task_directive_refref_index_fail(fortran_reader, fortran_writer):
    ''' Test the code generation throws an Error when an index Binop is on two
    references. '''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 354) :: A
        integer, dimension(321, 354) :: B
        integer :: i, ii
        integer :: j, jj
        integer :: k
        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32
                    do jj = j,j+32
                        if (A(ii, jj) > 0.0) then
                            A(ii+ii,jj) = B(ii,jj) * k
                        end if
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[1].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    with pytest.raises(GenerationError) as excinfo:
        fortran_writer(tree)
    assert ("Children of BinaryOperation are of types Reference and Reference,"
            " expected one Reference and one Literal when used as an index "
            "inside an OMPTaskDirective." in str(excinfo.value))


def test_omp_task_directive_13(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when we have Literal+Reference.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(320, 10) :: A
        integer, dimension(321, 10) :: B
        integer :: i
        integer :: j
        integer :: k
        do i = 1, 320, 32
            do j = 1, 32
                A(i, j) = k
                A(i, j) = B(1+i, j) + k
                A(i, j) = B(33+i, j) + k
                A(i, j) = B(32+i, j) + k
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(320,10) :: a
  integer, dimension(321,10) :: b
  integer :: i
  integer :: j
  integer :: k

  !$omp parallel default(shared) private(i,j)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(j) firstprivate(i) shared(a,b) depend(in: k,b(32 + i,:),b(i,:),b(2 * 32 + i,:)) depend(out: a(i,:))
    do j = 1, 32, 1
      a(i,j) = k
      a(i,j) = b(1 + i,j) + k
      a(i,j) = b(33 + i,j) + k
      a(i,j) = b(32 + i,j) + k
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_write_index_shared(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates an error if an array index
    of a written array is a shared variable.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(320, 10) :: A
        integer, dimension(321, 10) :: B
        integer :: i
        integer :: j
        integer :: k
        do i = 1, 320, 32
            k = 4
            k = -2
            k = k + 3
            do j = 1, 32
                A(i, k) = k
                A(i, j) = B(1+i, j) + k
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[3]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)

    with pytest.raises(GenerationError) as excinfo:
        fortran_writer(tree)
    assert ("Shared variable access used as an index inside an "
            "OMPTaskDirective which is not supported. Variable name is "
            "Reference[name:'k']" in str(excinfo.value))


def test_omp_task_directive_read_index_shared(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates an error if an array index
    of a read array is a shared variable.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(320, 10) :: A
        integer, dimension(321, 10) :: B
        integer :: i
        integer :: j
        integer :: k
        do i = 1, 320, 32
            k = 4
            k = -2
            k = k + 3
            do j = 1, 32
                A(i, j) = A(i, k)
                A(i, j) = B(1+i, j) + k
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[3]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)

    with pytest.raises(GenerationError) as excinfo:
        fortran_writer(tree)
    assert ("Shared variable access used as an index inside an "
            "OMPTaskDirective which is not supported. Variable name is "
            "Reference[name:'k']" in str(excinfo.value))


def test_omp_task_directive_14(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct
    firstprivate clause when the first access to a private variable is a 
    read.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j
        integer :: k
        do i = 1, 320, 32
            k = 9
            do ii=i, i+32
                do j = 1, 32
                    A(ii, j) = B(ii+1, k) + k
                    A(ii, j) = B(ii+1, k) + 1
                end do
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[1]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=1)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(321,10) :: a
  integer, dimension(32,10) :: b
  integer :: i
  integer :: ii
  integer :: j
  integer :: k

  !$omp parallel default(shared) private(i,ii,j,k)
  !$omp single
  do i = 1, 320, 32
    k = 9
    !$omp task private(ii,j) firstprivate(i,k) shared(a,b) depend(in: b(i + 32,k),b(i,k)) depend(out: a(i,:))
    do ii = i, i + 32, 1
      do j = 1, 32, 1
        a(ii,j) = b(ii + 1,k) + k
        a(ii,j) = b(ii + 1,k) + 1
      enddo
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_15(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct
    code for a non-array shared variable.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j
        integer :: k

        k = 0
        do i = 1, 320, 32
            k = k + i
            do ii=i, i+32
                do j = 1, 32
                    k = k + B(ii+1, j) + 1
                end do
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[1]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=1)
    strans.apply(loops[0])
    ptrans.apply(tree.children[0].children[:])
    correct = '''subroutine my_subroutine()
  integer, dimension(321,10) :: a
  integer, dimension(32,10) :: b
  integer :: i
  integer :: ii
  integer :: j
  integer :: k

  !$omp parallel default(shared) private(i,ii,j)
  k = 0
  !$omp single
  do i = 1, 320, 32
    k = k + i
    !$omp task private(ii,j) firstprivate(i) shared(k,b) depend(in: k,b(i + 32,:),b(i,:)) depend(out: k)
    do ii = i, i + 32, 1
      do j = 1, 32, 1
        k = k + b(ii + 1,j) + 1
      enddo
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_16(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when an else statement is present. '''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 354) :: A
        integer, dimension(321, 354) :: B
        integer :: i, ii
        integer :: j, jj
        integer :: k
        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32
                    do jj = j,j+32
                        if (A(ii, jj) > 0.0) then
                            A(ii+1,jj) = B(ii,jj) * k
                        else
                            A(ii-1,jj) = B(ii,jj) * k
                        end if
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[1].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(321,354) :: a
  integer, dimension(321,354) :: b
  integer :: i
  integer :: ii
  integer :: j
  integer :: jj
  integer :: k

  !$omp parallel default(shared) private(i,ii,j,jj)
  !$omp single
  do i = 1, 320, 32
    do j = 1, 320, 32
      !$omp task private(ii,jj) firstprivate(i,j) shared(a,b) depend(in: a(i,j),b(i,j),k) depend(out: a(i + 32,j),a(i,j),a(i - 32,j))
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          if (a(ii,jj) > 0.0) then
            a(ii + 1,jj) = b(ii,jj) * k
          else
            a(ii - 1,jj) = b(ii,jj) * k
          end if
        enddo
      enddo
      !$omp end task
    enddo
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_17(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct clauses
    when an output variable is just a shared vairable '''
    code = '''
    subroutine my_subroutine()
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
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[1].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer :: i
  integer :: ii
  integer :: j
  integer :: jj
  integer :: k

  !$omp parallel default(shared) private(i,ii,j,jj)
  !$omp single
  do i = 1, 320, 32
    do j = 1, 320, 32
      !$omp task private(ii,jj) firstprivate(i,j) shared(k) depend(in: k) depend(out: k)
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          k = k + ii
          k = k * jj
        enddo
      enddo
      !$omp end task
    enddo
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct

def test_omp_task_directive_18(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct code if
    stepval is not yet declared firstprivate '''
    code = '''
    subroutine my_subroutine()
        integer :: i, ii
        integer :: j, jj
        integer :: k, kk
        kk = 2
        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32, kk
                    do jj = j,j+32
                        k = k + ii
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[1].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer :: i
  integer :: ii
  integer :: j
  integer :: jj
  integer :: k
  integer :: kk

  kk = 2
  !$omp parallel default(shared) private(i,ii,j,jj)
  !$omp single
  do i = 1, 320, 32
    do j = 1, 320, 32
      !$omp task private(ii,jj) firstprivate(i,kk,j) shared(k) depend(in: k) depend(out: k)
      do ii = i, i + 32, kk
        do jj = j, j + 32, 1
          k = k + ii
        enddo
      enddo
      !$omp end task
    enddo
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_19(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct code if
    stepval is not yet declared firstprivate '''
    code = '''
    subroutine my_subroutine()
      type :: x
        integer :: jp
      end type
        integer :: i, ii
        integer :: j, jj
        integer :: k
        type(x) :: ty
        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32
                    do jj = j,j+32
                        k = ty%jp + ii
                        ty%jp = ty%jp - (1 - ty%jp)
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[1].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  type :: x
    integer :: jp
  end type x
  integer :: i
  integer :: ii
  integer :: j
  integer :: jj
  integer :: k
  type(x) :: ty

  !$omp parallel default(shared) private(i,ii,j,jj)
  !$omp single
  do i = 1, 320, 32
    do j = 1, 320, 32
      !$omp task private(ii,jj) firstprivate(i,j) shared(k,ty) depend(in: ty) depend(out: k,ty)
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          k = ty%jp + ii
          ty%jp = ty%jp - (1 - ty%jp)
        enddo
      enddo
      !$omp end task
    enddo
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_20(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct
    code for a literal read-only array index.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j

        do i = 1, 320, 32
            do ii=i, i+32
                do j = 1, 32
                    A(ii, j) = B(1, j) + 1
                end do
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(tree.children[0].children[:])
    correct = '''subroutine my_subroutine()
  integer, dimension(321,10) :: a
  integer, dimension(32,10) :: b
  integer :: i
  integer :: ii
  integer :: j

  !$omp parallel default(shared) private(i,ii,j)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(ii,j) firstprivate(i) shared(a,b) depend(in: b(1,:)) depend(out: a(i,:))
    do ii = i, i + 32, 1
      do j = 1, 32, 1
        a(ii,j) = b(1,j) + 1
      enddo
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_21(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct
    code for a literal written to array index.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j

        do i = 1, 320, 32
            do ii=i, i+32
                do j = 1, 32
                    A(ii, 1) = B(ii, j) + 1
                end do
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(tree.children[0].children[:])
    correct = '''subroutine my_subroutine()
  integer, dimension(321,10) :: a
  integer, dimension(32,10) :: b
  integer :: i
  integer :: ii
  integer :: j

  !$omp parallel default(shared) private(i,ii,j)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(ii,j) firstprivate(i) shared(a,b) depend(in: b(i,:)) depend(out: a(i,1))
    do ii = i, i + 32, 1
      do j = 1, 32, 1
        a(ii,1) = b(ii,j) + 1
      enddo
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_shared_index(fortran_reader, fortran_writer):
    ''' Test the code generation correctly throws an error if a shared variable
    is used as an array index.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j
        integer :: k

        do i = 1, 320, 32
            k = 1
            do ii=i, i+32
                do j = 1, 32
                    k = k + 1
                    A(ii, k) = B(ii, j) + 1
                end do
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[1]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(tree.children[0].children[:])
    with pytest.raises(GenerationError) as excinfo:
        fortran_writer(tree)
    assert ("Shared variable access used as an index inside an "
            "OMPTaskDirective which is not supported. Variable name is "
            "Reference[name:'k']" in str(excinfo.value))


def test_omp_task_directive_non_loop(fortran_reader, fortran_writer):
    ''' Test the code generation correctly throws an error if an
    OMPTaskDirective's child is a non Loop node.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j
        integer :: k

        do i = 1, 320, 32
            k = 1
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(tree.children[0].children[:])
    with pytest.raises(GenerationError) as excinfo:
        fortran_writer(tree)
    assert ("OMPTaskDirective must have exactly one Loop child. Found <class "
            "'psyclone.psyir.nodes.assignment.Assignment'>" in str(excinfo.value))

def test_omp_task_directive_multichild(fortran_reader, fortran_writer):
    ''' Test the code generation correctly throws an error if it an
    OMPTaskDirective has multiple children.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j
        integer :: k

        do i = 1, 320, 32
            k = 1
            do ii=i, i+32
                do j = 1, 32
                    A(ii, 1) = B(ii, j) + 1
                end do
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    assign = loops[0].children[3].children[0]
    assign.detach()
    tdir.children[0].addchild(assign)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(tree.children[0].children[:])
    with pytest.raises(GenerationError) as excinfo:
        fortran_writer(tree)
    assert ("OMPTaskDirective must have exactly one Loop child. Found 2 "
            "children." in str(excinfo.value))


def test_omp_task_directive_loop_start_array(fortran_reader, fortran_writer):
    ''' Test the code generation correctly throws an error if it a
    Loop inside an OMPTaskDirective has an array start value.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j

        do i = 1, 320, 32
            do ii=i, i+32
                do j = B(3,2), 32
                    A(ii, 1) = B(ii, j) + 1
                end do
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(tree.children[0].children[:])
    with pytest.raises(GenerationError) as excinfo:
        fortran_writer(tree)
    assert ("ArrayReference not supported in the start variable of a Loop "
            "in a OMPTaskDirective node." in str(excinfo.value))


def test_omp_task_directive_loop_stop_array(fortran_reader, fortran_writer):
    ''' Test the code generation correctly throws an error if it a
    Loop inside an OMPTaskDirective has an array stop value.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j

        do i = 1, 320, 32
            do ii=i, i+32
                do j = 1, B(i,2)
                    A(ii, 1) = B(ii, j) + 1
                end do
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(tree.children[0].children[:])
    with pytest.raises(GenerationError) as excinfo:
        fortran_writer(tree)
    assert ("ArrayReference not supported in the stop variable of a Loop "
            "in a OMPTaskDirective node." in str(excinfo.value))


def test_omp_task_directive_loop_step_array(fortran_reader, fortran_writer):
    ''' Test the code generation correctly throws an error if it a
    Loop inside an OMPTaskDirective has an array step value.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j

        do i = 1, 320, 32
            do ii=i, i+32
                do j = 1, 22, B(i,2)
                    A(ii, 1) = B(ii, j) + 1
                end do
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(tree.children[0].children[:])
    with pytest.raises(GenerationError) as excinfo:
        fortran_writer(tree)
    assert ("ArrayReference not supported in the step variable of a Loop "
            "in a OMPTaskDirective node." in str(excinfo.value))


def test_omp_task_directive_22(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when we have Literal+Reference for a proxy loop variable.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(320, 10) :: A
        integer, dimension(321, 10) :: B
        integer :: i,ii
        integer :: j
        integer :: k
        do i = 1, 320, 32
            do ii = i, i + 32
                do j = 1, 32
                    A(i, j) = k
                    A(i, j) = B(1+ii, j) + k
                    A(i, j) = B(33+ii, j) + k
                    A(i, j) = B(32+ii, j) + k
                end do
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(320,10) :: a
  integer, dimension(321,10) :: b
  integer :: i
  integer :: ii
  integer :: j
  integer :: k

  !$omp parallel default(shared) private(i,ii,j)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(ii,j) firstprivate(i) shared(a,b) depend(in: k,b(32 + i,:),b(i,:),b(2 * 32 + i,:)) depend(out: a(i,:))
    do ii = i, i + 32, 1
      do j = 1, 32, 1
        a(i,j) = k
        a(i,j) = b(1 + ii,j) + k
        a(i,j) = b(33 + ii,j) + k
        a(i,j) = b(32 + ii,j) + k
      enddo
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_23(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when we have a private variable in an array reference, either as a child 
    loop member or not.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(320, 10) :: A
        integer, dimension(321, 10) :: B
        integer :: i,ii
        integer :: j
        integer :: k
        do i = 1, 320, 32
            do ii = i, i + 32
                k = 3
                do j = 1, 32
                    A(i, j+1) = k
                end do
                A(i,k + 2) = 3
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(320,10) :: a
  integer, dimension(321,10) :: b
  integer :: i
  integer :: ii
  integer :: j
  integer :: k

  !$omp parallel default(shared) private(i,ii,j,k)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(ii,k,j) firstprivate(i) shared(a) depend(out: a(i,:))
    do ii = i, i + 32, 1
      k = 3
      do j = 1, 32, 1
        a(i,j + 1) = k
      enddo
      a(i,k + 2) = 3
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_24(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when we have access to a non-proxy parent loop variable in an array index
    binary operation.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(320, 10) :: A
        integer, dimension(321, 10) :: B
        integer :: i,ii
        integer :: j
        integer :: k
        do j = 1, 320, 32
          do i = 1, 320, 32
            do ii = i, i + 32
                    A(i, j+65) = k
                end do
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(320,10) :: a
  integer, dimension(321,10) :: b
  integer :: i
  integer :: ii
  integer :: j
  integer :: k

  !$omp parallel default(shared) private(i,ii,j)
  !$omp single
  do j = 1, 320, 32
    do i = 1, 320, 32
      !$omp task private(ii) firstprivate(i,j) shared(a) depend(in: k) depend(out: a(i,j + 3 * 32),a(i,j + 2 * 32))
      do ii = i, i + 32, 1
        a(i,j + 65) = k
      enddo
      !$omp end task
    enddo
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_25(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct clauses
    when we have access to a first private constant in '''
    code = '''
    subroutine my_subroutine()
        integer, dimension(320, 10) :: A
        integer :: i,ii
        integer :: j
        integer :: k
        do i = 1, 32
          k = 1
        end do
        k = 32
        do i = 1, 320, 32
          do ii = i, i + 32
            j = k
            A(ii,k) = 20
          end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    ltrans = OMPLoopTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop)
    ltrans.apply(loops[0])
    loop = loops[1].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(tree.children[0].children[:])
    ptrans.apply(tree.children[0].children[0])
    correct = '''subroutine my_subroutine()
  integer, dimension(320,10) :: a
  integer :: i
  integer :: ii
  integer :: j
  integer :: k
  integer :: th_idx
  integer :: nthreads

  !$omp parallel default(shared) private(i,ii,k)
  !$omp single
  !$omp do schedule(static)
  do i = 1, 32, 1
    k = 1
  enddo
  !$omp end do
  k = 32
  do i = 1, 320, 32
    !$omp task private(ii) firstprivate(i,k) shared(j,a) depend(out: j,a(i,k+1))
    do ii = i, i + 32, 1
      j = k
      a(ii,k+1) = 20
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_26(fortran_reader, fortran_writer):
    ''' Test the code generation correctly throws an error if an
    index is a shared non-array variable.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j

        j = 32
        do i = 1, 320, 32
            do ii=i, i+32
                A(ii, 1) = B(ii, j+1) + 1
            end do
        end do
    end subroutine
    '''
    tree =  fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = OMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(tree.children[0].children[:])
    with pytest.raises(GenerationError) as excinfo:
        fortran_writer(tree)
    assert ("Shared variable access used as an index inside an "
            "OMPTaskDirective which is not supported. Variable "
            "name is Reference[name:'j']" in str(excinfo.value))
