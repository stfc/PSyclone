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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         A. B. G. Chalk, STFC Daresbury Lab
# Modified I. Kavcic, Met Office
# -----------------------------------------------------------------------------

''' Performs py.test tests on the OpenMP PSyIR Directive nodes. '''

from __future__ import absolute_import
import os
import pytest
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir import nodes
from psyclone import psyGen
from psyclone.psyir.nodes import OMPDoDirective, Schedule, \
    OMPParallelDoDirective, Directive, colored, OMPParallelDirective, \
    OMPSingleDirective, OMPMasterDirective, OMPTaskloopDirective, \
    OMPTaskwaitDirective
from psyclone.errors import InternalError, GenerationError
from psyclone.transformations import Dynamo0p3OMPLoopTrans, OMPParallelTrans, \
    OMPParallelLoopTrans, DynamoOMPParallelLoopTrans, OMPSingleTrans, \
    OMPMasterTrans, OMPTaskloopTrans
from psyclone.domain.gocean.transformations import GOceanExtractTrans

BASE_PATH = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__)))), "test_files", "dynamo0p3")
GOCEAN_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                os.pardir, os.pardir, "test_files",
                                "gocean1p0")


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
    assert ("malformed or incomplete. It should have a single Schedule as a "
            "child but found: []" in str(err.value))
    child = schedule.children[0].detach()
    ompdo = OMPDoDirective(parent=schedule, children=[child])
    assert len(ompdo.dir_body.children) == 1


def test_ompdo_directive_class_node_str(dist_mem):
    '''Tests the node_str method in the OMPDoDirective class. We create a
    sub-class object then call this method from it.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")

    cases = [
        {"current_class": OMPParallelDoDirective,
         "current_string": "[OMP parallel do]"},
        {"current_class": OMPDoDirective, "current_string": "[OMP do]"},
        {"current_class": OMPParallelDirective,
         "current_string": "[OMP parallel]"},
        {"current_class": Directive, "current_string": ""}]
    otrans = OMPParallelLoopTrans()

    psy = PSyFactory("dynamo0.3", distributed_memory=dist_mem).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    if dist_mem:
        idx = 4
    else:
        idx = 0

    _, _ = otrans.apply(schedule.children[idx])
    omp_parallel_loop = schedule.children[idx]

    for case in cases:
        # Call the OMPDirective node_str method
        out = case["current_class"].node_str(omp_parallel_loop)

        directive = colored("Directive", Directive._colour)
        expected_output = directive + case["current_string"]

        assert expected_output in out


def test_directive_get_private(monkeypatch):
    ''' Tests for the _get_private_list() method of OMPParallelDirective.
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
    # Now check that _get_private_list returns what we expect
    pvars = directive._get_private_list()
    assert pvars == ['cell']
    # Now use monkeypatch to break the Call within the loop
    call = directive.dir_body[0].dir_body[0].loop_body[0]
    monkeypatch.setattr(call, "local_vars", lambda: [""])
    with pytest.raises(InternalError) as err:
        _ = directive._get_private_list()
    assert ("call 'testkern_w3_code' has a local variable but its name is "
            "not set" in str(err.value))


def test_openmp_pdo_dag_name():
    '''Test that we generate the correct dag name for the OpenMP parallel
    do node'''
    _, info = parse(os.path.join(BASE_PATH,
                                 "15.7.2_setval_X_builtin.f90"),
                    api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    otrans = DynamoOMPParallelLoopTrans()
    # Apply OpenMP parallelisation to the loop
    otrans.apply(schedule.children[0])
    assert schedule.children[0].dag_name == "OMP_parallel_do_1"


def test_omp_dag_names():
    ''' Test that we generate the correct dag names for omp parallel, omp
    do, omp directive and directive nodes.
    Note: this test does not apply colouring so the loops must be over
    discontinuous function spaces.

    '''
    _, info = parse(os.path.join(BASE_PATH, "1_single_invoke_w3.f90"),
                    api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_w3_type')
    schedule = invoke.schedule
    olooptrans = Dynamo0p3OMPLoopTrans()
    ptrans = OMPParallelTrans()
    # Put an OMP PARALLEL around this loop
    child = schedule.children[0]
    ptrans.apply(child)
    # Put an OMP DO around this loop
    olooptrans.apply(schedule[0].dir_body[0])
    omp_par_node = schedule.children[0]
    assert omp_par_node.dag_name == "OMP_parallel_1"
    assert omp_par_node.dir_body[0].dag_name == "OMP_do_3"
    omp_directive = super(OMPParallelDirective, omp_par_node)
    assert omp_directive.dag_name == "region_directive_1"


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


def test_omp_single_dag_name():
    ''' Test the dag_name() method of the OMPSingle directive '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    single = OMPSingleTrans()
    psy = PSyFactory("dynamo0.3", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    _, _ = single.apply(schedule.children[0])
    assert schedule.children[0].dag_name == "OMP_single_1"


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

    nowait_str = ""
    if nowait:
        nowait_str = " nowait"

    assert omp_single.begin_string() == "omp single" + nowait_str
    assert omp_single.end_string() == "omp end single"


def test_omp_single_node_str():
    ''' Test the node_str() method of the OMPSingle directive '''
    single_directive = OMPSingleDirective()
    out = single_directive.node_str()
    directive = colored("OMPSingleDirective", Directive._colour)
    expected_output = directive + "[OMP single]"
    assert expected_output == out


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
    generates the expected code. Use the gocean API.
    '''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH, "single_invoke.f90"),
                           api="gocean1.0")
    single = OMPSingleTrans()
    parallel = OMPParallelTrans()
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    single.apply(schedule.children[0], {"nowait": nowait})
    parallel.apply(schedule.children[0])
    goceantrans = GOceanExtractTrans()
    goceantrans.apply(schedule.children[0])

    code = str(psy.gen)
    string = ""
    if nowait:
        string = " nowait"
    assert (
        "    !$omp parallel default(shared), private(i,j)\n" +
        "      !$omp single{0}\n".format(string) +
        "      DO" in code)
    assert (
        "      END DO\n" +
        "      !$omp end single\n" +
        "      !$omp end parallel" in code)


def test_omp_master_dag_name():
    ''' Test the dag_name() method of the OMPMaster directive '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    master = OMPMasterTrans()
    psy = PSyFactory("dynamo0.3", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    master.apply(schedule.children[0])
    assert schedule.children[0].dag_name == "OMP_master_1"


def test_omp_master_strings():
    ''' Test the begin_string and end_string methods of the OMPMaster
        directive '''
    omp_master = OMPMasterDirective()

    assert omp_master.begin_string() == "omp master"
    assert omp_master.end_string() == "omp end master"


def test_omp_master_node_str():
    ''' Test the node_str() method of the OMPMaster directive '''
    master_directive = OMPMasterDirective()
    out = master_directive.node_str()
    directive = colored("OMPMasterDirective", Directive._colour)
    expected_output = directive + "[OMP master]"
    assert expected_output == out


def test_omp_master_gencode():
    '''Check that the gen_code method in the OMPMasterDirective class
    generates the expected code. Use the gocean API.
    '''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH, "single_invoke.f90"),
                           api="gocean1.0")
    master = OMPMasterTrans()
    parallel = OMPParallelTrans()
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    master.apply(schedule.children[0])
    parallel.apply(schedule.children[0])
    goceantrans = GOceanExtractTrans()
    goceantrans.apply(schedule.children[0])

    code = str(psy.gen)
    assert (
        "    !$omp parallel default(shared), private(i,j)\n" +
        "      !$omp master\n" +
        "      DO" in code)
    assert (
        "      END DO\n" +
        "      !$omp end master\n" +
        "      !$omp end parallel" in code)


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


def test_omptaskwait_dag_name():
    '''Test the OMPTaskwait, OMPStandaloneDirective and StandaloneDirective
    dag_name methods'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    taskwait = OMPTaskwaitDirective()
    schedule.addchild(taskwait, 0)
    assert taskwait.dag_name == "OMP_taskwait_1"
    omp_cdirective = super(OMPTaskwaitDirective, taskwait)
    assert omp_cdirective.dag_name == "standalone_directive_1"


def test_omptaskwait_strings():
    ''' Test the begin_string and method of the OMPTaskwait directive '''
    taskwait = OMPTaskwaitDirective()

    assert taskwait.begin_string() == "omp taskwait"


def test_omptaskwait_node_str():
    '''Test the OMPTaskwaitDirective and OMPStandaloneDirective node_str
    methods'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    taskwait = OMPTaskwaitDirective()
    schedule.addchild(taskwait, 0)
    directive = colored("Directive", Directive._colour)
    expected_output = directive + "[OMP taskwait]"
    assert taskwait.node_str() == expected_output
    omp_cdirective = super(OMPTaskwaitDirective, taskwait)
    expected_output = directive + "[]"
    assert omp_cdirective.node_str() == expected_output


def test_omptaskwait_gencode():
    '''Check that the gen_code method in the OMPTaskwaitDirective
    class generates the expected code. Use the gocean API.
    '''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH, "single_invoke.f90"),
                           api="gocean1.0")
    taskwait = OMPTaskwaitDirective()
    parallel = OMPParallelTrans()
    master = OMPMasterTrans()
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    taskloop = OMPTaskloopTrans()

    schedule.addchild(taskwait, 1)
    taskloop.apply(schedule.children[0])
    master.apply(schedule.children[0:2])
    parallel.apply(schedule.children[0])
    goceantrans = GOceanExtractTrans()
    goceantrans.apply(schedule.children[0])

    code = str(psy.gen)
    assert (
        "      END DO\n" +
        "      !$omp end taskloop\n" +
        "      !$omp taskwait\n" +
        "      !$omp end master\n" +
        "      !$omp end parallel" in code)


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


def test_omp_taskloop_dag_name():
    '''Test the omp_taskloop dag_name method'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    taskloop = OMPTaskloopTrans()
    psy = PSyFactory("dynamo0.3", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    taskloop.apply(schedule.children[0])
    assert schedule.children[0].dag_name == "OMP_taskloop_1"


def test_omp_taskloop_strings():
    ''' Test the begin_string and end_string methods of the
        OMPTaskloop directive '''
    omp_taskloop = OMPTaskloopDirective()

    assert omp_taskloop.begin_string() == "omp taskloop"
    assert omp_taskloop.end_string() == "omp end taskloop"


def test_omp_taskloop_init():
    ''' Test the constructor of the OMPTaskloop directive'''
    with pytest.raises(GenerationError) as excinfo:
        OMPTaskloopDirective(grainsize=32, num_tasks=32)
    assert("OMPTaskloopDirective must not have both grainsize and "
           "numtasks clauses specified.") in str(excinfo.value)


def test_omp_taskloop_node_str():
    ''' Test the node_str() method of the OMPTaskloop directive '''
    omp_taskloop = OMPTaskloopDirective()
    out = OMPTaskloopDirective.node_str(omp_taskloop)
    directive = colored("Directive", Directive._colour)
    expected_output = directive + "[OMP taskloop]"
    assert expected_output in out


@pytest.mark.parametrize("grainsize,num_tasks", [(None, None), (32, None),
                                                 (None, 32)])
def test_omp_taskloop_gencode(grainsize, num_tasks):
    '''Check that the gen_code method in the OMPTaskloopDirective
    class generates the expected code. Use the gocean API.
    '''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH, "single_invoke.f90"),
                           api="gocean1.0")
    taskloop = OMPTaskloopTrans(grainsize, num_tasks)
    master = OMPMasterTrans()
    parallel = OMPParallelTrans()
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    taskloop.apply(schedule.children[0])
    taskloop_node = schedule.children[0]
    master.apply(schedule.children[0])
    parallel.apply(schedule.children[0])
    goceantrans = GOceanExtractTrans()
    goceantrans.apply(schedule.children[0])

    code = str(psy.gen)

    clauses = ""
    if grainsize is not None:
        clauses = " grainsize({0})".format(grainsize)
    if num_tasks is not None:
        clauses = " num_tasks({0})".format(num_tasks)

    assert (
        "    !$omp parallel default(shared), private(i,j)\n" +
        "      !$omp master\n" +
        "      !$omp taskloop{0}\n".format(clauses) +
        "      DO" in code)
    assert (
        "      END DO\n" +
        "      !$omp end taskloop\n" +
        "      !$omp end master\n" +
        "      !$omp end parallel" in code)

    assert taskloop_node.begin_string() == "omp taskloop{0}".format(clauses)


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
