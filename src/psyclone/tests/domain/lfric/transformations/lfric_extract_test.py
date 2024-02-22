# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2024, Science and Technology Facilities Council.
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
# Author I. Kavcic, Met Office
# Modified by A. R. Porter and R. W. Ford, STFC Daresbury Lab
# Modified by J. Henrichs, Bureau of Meteorology
# Modified by L. Turner and O. Brunt, Met Office
# -----------------------------------------------------------------------------

''' Module containing tests for PSyclone LFRicExtractTrans
transformations and ExtractNode.
'''

import pytest

from psyclone.configuration import Config
from psyclone.domain.lfric.transformations import LFRicExtractTrans
from psyclone.domain.lfric import LFRicConstants
from psyclone.psyir.nodes import colored, ExtractNode, Loop
from psyclone.psyir.transformations import PSyDataTrans, TransformationError
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import (Dynamo0p3ColourTrans,
                                      DynamoOMPParallelLoopTrans)

# API names
DYNAMO_API = "dynamo0.3"


@pytest.fixture(scope="function", autouse=True)
def clear_region_name_cache():
    '''All PSyData nodes keep a list of used region names as class variables
    to avoid name clashes. This needs to be cleared, otherwise the indices
    used when creating unique region identifier will change depending on the
    order in which tests are run.
    '''
    PSyDataTrans._used_kernel_names = {}
    yield
    PSyDataTrans._used_kernel_names = {}

# --------------------------------------------------------------------------- #
# ================== Extract Transformation tests =========================== #
# --------------------------------------------------------------------------- #


def test_node_list_error(tmpdir):
    ''' Test that applying Extract Transformation on objects which are not
    Nodes or a list of Nodes raises a TransformationError. Also raise
    transformation errors when the Nodes do not have the same parent
    if they are incorrectly ordered. '''
    etrans = LFRicExtractTrans()

    # First test for f1 readwrite to read dependency
    psy, _ = get_invoke("3.2_multi_functions_multi_named_invokes.f90",
                        DYNAMO_API, idx=0, dist_mem=False)
    invoke0 = psy.invokes.invoke_list[0]
    invoke1 = psy.invokes.invoke_list[1]
    # Supply an object which is not a Node or a list of Nodes
    with pytest.raises(TransformationError) as excinfo:
        etrans.apply(invoke0)
    assert ("Error in LFRicExtractTrans: Argument must be "
            "a single Node in a Schedule, a Schedule or a list of Nodes in a "
            "Schedule but have been passed an object of type: "
            "<class 'psyclone.domain.lfric.lfric_invoke.LFRicInvoke'>"
            in str(excinfo.value))

    # Supply Nodes in incorrect order or duplicate Nodes
    node_list = [invoke0.schedule.children[0],
                 invoke0.schedule.children[0],
                 invoke0.schedule.children[1]]
    with pytest.raises(TransformationError) as excinfo:
        etrans.apply(node_list)
    assert "Children are not consecutive children of one parent:" \
           in str(excinfo.value)
    assert "has position 0, but previous child had position 0."\
        in str(excinfo.value)

    # Supply Nodes which are not children of the same parent
    node_list = [invoke0.schedule.children[1],
                 invoke1.schedule.children[0],
                 invoke0.schedule.children[2]]
    with pytest.raises(TransformationError) as excinfo:
        etrans.apply(node_list)
    assert ("supplied nodes are not children of the same "
            "parent.") in str(excinfo.value)

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_distmem_error(monkeypatch):
    ''' Test that applying ExtractRegionTrans with distributed memory
    enabled raises a TransformationError. '''
    etrans = LFRicExtractTrans()

    # Test Dynamo0.3 API with distributed memory
    _, invoke = get_invoke("1_single_invoke.f90", DYNAMO_API,
                           idx=0, dist_mem=True)
    schedule = invoke.schedule
    # Try applying Extract transformation
    with pytest.raises(TransformationError) as excinfo:
        etrans.apply(schedule.children[3])
    assert ("Error in LFRicExtractTrans: Distributed memory is "
            "not supported.") in str(excinfo.value)

    # Try applying Extract transformation to Node(s) containing HaloExchange
    # We have to disable distributed memory, otherwise an earlier test
    # will be triggered
    config = Config.get()
    monkeypatch.setattr(config, "distributed_memory", False)
    with pytest.raises(TransformationError) as excinfo:
        etrans.apply(schedule.children[2:4])
    assert ("Nodes of type 'LFRicHaloExchange' cannot be enclosed by a "
            "LFRicExtractTrans transformation") in str(excinfo.value)

    # Try applying Extract transformation to Node(s) containing GlobalSum
    # This will set config.distributed_mem to True again.
    _, invoke = get_invoke("15.14.3_sum_setval_field_builtin.f90",
                           DYNAMO_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    glob_sum = schedule.children[2]

    # We have to disable distributed memory again (get_invoke before
    # will set it to true), otherwise an earlier test will be triggered
    monkeypatch.setattr(config, "distributed_memory", False)
    with pytest.raises(TransformationError) as excinfo:
        etrans.apply(glob_sum)

    assert ("Nodes of type 'DynGlobalSum' cannot be enclosed by a "
            "LFRicExtractTrans transformation") in str(excinfo.value)


def test_repeat_extract():
    ''' Test that applying Extract Transformation on Node(s) already
    containing an ExtractNode raises a TransformationError. '''
    etrans = LFRicExtractTrans()

    # Test Dynamo0.3 API
    _, invoke = get_invoke("1_single_invoke.f90", DYNAMO_API,
                           idx=0, dist_mem=False)
    schedule = invoke.schedule
    # Apply Extract transformation
    etrans.apply(schedule.children[0])
    # Now try applying it again on the ExtractNode
    with pytest.raises(TransformationError) as excinfo:
        etrans.apply(schedule.children[0])
    assert ("Nodes of type 'ExtractNode' cannot be enclosed by a "
            "LFRicExtractTrans transformation") in str(excinfo.value)


def test_kern_builtin_no_loop():
    ''' Test that applying Extract Transformation on a Kernel or Built-in
    call without its parent Loop raises a TransformationError. '''

    # Test Dynamo0.3 API for Built-in call error
    dynetrans = LFRicExtractTrans()
    _, invoke = get_invoke("15.1.2_builtin_and_normal_kernel_invoke.f90",
                           DYNAMO_API, idx=0, dist_mem=False)
    schedule = invoke.schedule
    # Test Built-in call
    builtin_call = schedule.children[1].loop_body[0]
    with pytest.raises(TransformationError) as excinfo:
        dynetrans.apply(builtin_call)
    assert "Error in LFRicExtractTrans: Application to a Kernel or a " \
           "Built-in call without its parent Loop is not allowed." \
           in str(excinfo.value)


def test_loop_no_directive_dynamo0p3():
    ''' Test that applying Extract Transformation on a Loop without its
    parent Directive when optimisations are applied in Dynamo0.3 API
    raises a TransformationError. '''
    etrans = LFRicExtractTrans()

    # Test a Loop nested within the OMP Parallel DO Directive
    _, invoke = get_invoke("4.13_multikernel_invokes_w3_anyd.f90",
                           DYNAMO_API, idx=0, dist_mem=False)
    schedule = invoke.schedule
    # Apply DynamoOMPParallelLoopTrans to the second Loop
    otrans = DynamoOMPParallelLoopTrans()
    otrans.apply(schedule[1])
    loop = schedule.children[1].dir_body[0]
    # Try extracting the Loop inside the OMP Parallel DO region
    with pytest.raises(TransformationError) as excinfo:
        etrans.apply(loop)
    assert "Error in LFRicExtractTrans: Application to a Loop without its " \
           "parent Directive is not allowed." in str(excinfo.value)


def test_no_colours_loop_dynamo0p3():
    ''' Test that applying LFRicExtractTrans on a Loop over cells
    in a colour without its parent Loop over colours in Dynamo0.3 API
    raises a TransformationError. '''

    etrans = LFRicExtractTrans()
    ctrans = Dynamo0p3ColourTrans()
    otrans = DynamoOMPParallelLoopTrans()

    _, invoke = get_invoke("1_single_invoke.f90", DYNAMO_API,
                           idx=0, dist_mem=False)
    schedule = invoke.schedule

    # Colour first loop that calls testkern_code (loop is over cells and
    # is not on a discontinuous space)
    ctrans.apply(schedule.children[0])
    colour_loop = schedule.children[0].loop_body[0]
    # Apply OMP Parallel DO Directive to the colour Loop
    otrans.apply(colour_loop)
    directive = schedule[0].loop_body
    # Try to extract the region between the Loop over cells in a colour
    # and the exterior Loop over colours
    with pytest.raises(TransformationError) as excinfo:
        etrans.apply(directive)
    assert ("Dynamo0.3 API: Extraction of a Loop over cells in a "
            "colour without its ancestor Loop over colours is not "
            "allowed.") in str(excinfo.value)

# --------------------------------------------------------------------------- #
# ================== ExtractNode tests ====================================== #
# --------------------------------------------------------------------------- #


def test_extract_node_position():
    ''' Test that Extract Transformation inserts the ExtractNode
    at the position of the first Node a Schedule in the Node list
    marked for extraction. '''

    # Test Dynamo0.3 API for extraction of a list of Nodes
    dynetrans = LFRicExtractTrans()
    _, invoke = get_invoke("15.1.2_builtin_and_normal_kernel_invoke.f90",
                           DYNAMO_API, idx=0, dist_mem=False)
    schedule = invoke.schedule
    # Apply Extract transformation to the first three Nodes and assert that
    # position and the absolute position of the ExtractNode are the same as
    # respective positions of the first Node before the transformation.
    pos = 0
    children = schedule.children[pos:pos+3]
    abspos = children[0].abs_position
    dpth = children[0].depth
    dynetrans.apply(children)
    extract_node = schedule.walk(ExtractNode)
    # The result is only one ExtractNode in the list with position 0
    assert extract_node[0].position == pos
    assert extract_node[0].abs_position == abspos
    assert extract_node[0].depth == dpth


def test_extract_node_representation():
    ''' Test that representation properties and methods of the ExtractNode
    class: view  and __str__ produce the correct results. '''

    etrans = LFRicExtractTrans()
    _, invoke = get_invoke("4.8_multikernel_invokes.f90", DYNAMO_API,
                           idx=0, dist_mem=False)
    schedule = invoke.schedule
    children = schedule.children[1:3]
    etrans.apply(children)

    # Test view() method
    output = schedule.view()
    expected_output = colored("Extract", ExtractNode._colour)
    assert expected_output in output

    # Test __str__ method

    assert "End LFRicLoop\nExtractStart[var=extract_psy_data]\nLFRicLoop[" \
        in str(schedule)
    assert "End LFRicLoop\nExtractEnd[var=extract_psy_data]\nLFRicLoop[" in \
        str(schedule)
    # Count the loops inside and outside the extract to check it is in
    # the right place
    [before, after] = str(schedule).split("ExtractStart")
    [inside, after] = after.split("ExtractEnd")
    assert before.count("Loop[") == 1
    assert inside.count("Loop[") == 2
    assert after.count("Loop[") == 2


def test_single_node_dynamo0p3():
    ''' Test that Extract Transformation on a single Node in a Schedule
    produces the correct result in Dynamo0.3 API. '''
    etrans = LFRicExtractTrans()

    psy, invoke = get_invoke("1_single_invoke.f90", DYNAMO_API,
                             idx=0, dist_mem=False)
    schedule = invoke.schedule

    etrans.apply(schedule.children[0])
    code = str(psy.gen)
    output = '''      ! ExtractStart
      !
      CALL extract_psy_data%PreStart("single_invoke_psy", \
"invoke_0_testkern_type:testkern_code:r0", 17, 2)
      CALL extract_psy_data%PreDeclareVariable("a", a)
      CALL extract_psy_data%PreDeclareVariable("f1_data", f1_data)
      CALL extract_psy_data%PreDeclareVariable("f2_data", f2_data)
      CALL extract_psy_data%PreDeclareVariable("loop0_start", loop0_start)
      CALL extract_psy_data%PreDeclareVariable("loop0_stop", loop0_stop)
      CALL extract_psy_data%PreDeclareVariable("m1_data", m1_data)
      CALL extract_psy_data%PreDeclareVariable("m2_data", m2_data)
      CALL extract_psy_data%PreDeclareVariable("map_w1", map_w1)
      CALL extract_psy_data%PreDeclareVariable("map_w2", map_w2)
      CALL extract_psy_data%PreDeclareVariable("map_w3", map_w3)
      CALL extract_psy_data%PreDeclareVariable("ndf_w1", ndf_w1)
      CALL extract_psy_data%PreDeclareVariable("ndf_w2", ndf_w2)
      CALL extract_psy_data%PreDeclareVariable("ndf_w3", ndf_w3)
      CALL extract_psy_data%PreDeclareVariable("nlayers", nlayers)
      CALL extract_psy_data%PreDeclareVariable("undf_w1", undf_w1)
      CALL extract_psy_data%PreDeclareVariable("undf_w2", undf_w2)
      CALL extract_psy_data%PreDeclareVariable("undf_w3", undf_w3)
      CALL extract_psy_data%PreDeclareVariable("cell_post", cell)
      CALL extract_psy_data%PreDeclareVariable("f1_data_post", f1_data)
      CALL extract_psy_data%PreEndDeclaration
      CALL extract_psy_data%ProvideVariable("a", a)
      CALL extract_psy_data%ProvideVariable("f1_data", f1_data)
      CALL extract_psy_data%ProvideVariable("f2_data", f2_data)
      CALL extract_psy_data%ProvideVariable("loop0_start", loop0_start)
      CALL extract_psy_data%ProvideVariable("loop0_stop", loop0_stop)
      CALL extract_psy_data%ProvideVariable("m1_data", m1_data)
      CALL extract_psy_data%ProvideVariable("m2_data", m2_data)
      CALL extract_psy_data%ProvideVariable("map_w1", map_w1)
      CALL extract_psy_data%ProvideVariable("map_w2", map_w2)
      CALL extract_psy_data%ProvideVariable("map_w3", map_w3)
      CALL extract_psy_data%ProvideVariable("ndf_w1", ndf_w1)
      CALL extract_psy_data%ProvideVariable("ndf_w2", ndf_w2)
      CALL extract_psy_data%ProvideVariable("ndf_w3", ndf_w3)
      CALL extract_psy_data%ProvideVariable("nlayers", nlayers)
      CALL extract_psy_data%ProvideVariable("undf_w1", undf_w1)
      CALL extract_psy_data%ProvideVariable("undf_w2", undf_w2)
      CALL extract_psy_data%ProvideVariable("undf_w3", undf_w3)
      CALL extract_psy_data%PreEnd
      DO cell=loop0_start,loop0_stop
        !
        CALL testkern_code(nlayers, a, f1_data, f2_data, ''' + \
        "m1_data, m2_data, ndf_w1, undf_w1, " + \
        "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, " + \
        '''undf_w3, map_w3(:,cell))
      END DO
      CALL extract_psy_data%PostStart
      CALL extract_psy_data%ProvideVariable("cell_post", cell)
      CALL extract_psy_data%ProvideVariable("f1_data_post", f1_data)
      CALL extract_psy_data%PostEnd
      !
      ! ExtractEnd'''
    assert output in code


def test_node_list_dynamo0p3():
    ''' Test that applying Extract Transformation on a list of Nodes
    produces the correct result in the Dynamo0.3 API.

    '''
    etrans = LFRicExtractTrans()
    psy, invoke = get_invoke("15.1.2_builtin_and_normal_kernel_invoke.f90",
                             DYNAMO_API, idx=0, dist_mem=False)
    schedule = invoke.schedule

    etrans.apply(schedule.children[0:3])
    code = str(psy.gen)
    output = """! ExtractStart
      !
      CALL extract_psy_data%PreStart("single_invoke_builtin_then_kernel_psy", \
"invoke_0:r0", 11, 5)
      CALL extract_psy_data%PreDeclareVariable("f3_data", f3_data)
      CALL extract_psy_data%PreDeclareVariable("loop0_start", loop0_start)
      CALL extract_psy_data%PreDeclareVariable("loop0_stop", loop0_stop)
      CALL extract_psy_data%PreDeclareVariable("loop1_start", loop1_start)
      CALL extract_psy_data%PreDeclareVariable("loop1_stop", loop1_stop)
      CALL extract_psy_data%PreDeclareVariable("loop2_start", loop2_start)
      CALL extract_psy_data%PreDeclareVariable("loop2_stop", loop2_stop)
      CALL extract_psy_data%PreDeclareVariable("map_w2", map_w2)
      CALL extract_psy_data%PreDeclareVariable("ndf_w2", ndf_w2)
      CALL extract_psy_data%PreDeclareVariable("nlayers", nlayers)
      CALL extract_psy_data%PreDeclareVariable("undf_w2", undf_w2)
      CALL extract_psy_data%PreDeclareVariable("cell_post", cell)
      CALL extract_psy_data%PreDeclareVariable("df_post", df)
      CALL extract_psy_data%PreDeclareVariable("f2_data_post", f2_data)
      CALL extract_psy_data%PreDeclareVariable("f3_data_post", f3_data)
      CALL extract_psy_data%PreDeclareVariable("f5_data_post", f5_data)
      CALL extract_psy_data%PreEndDeclaration
      CALL extract_psy_data%ProvideVariable("f3_data", f3_data)
      CALL extract_psy_data%ProvideVariable("loop0_start", loop0_start)
      CALL extract_psy_data%ProvideVariable("loop0_stop", loop0_stop)
      CALL extract_psy_data%ProvideVariable("loop1_start", loop1_start)
      CALL extract_psy_data%ProvideVariable("loop1_stop", loop1_stop)
      CALL extract_psy_data%ProvideVariable("loop2_start", loop2_start)
      CALL extract_psy_data%ProvideVariable("loop2_stop", loop2_stop)
      CALL extract_psy_data%ProvideVariable("map_w2", map_w2)
      CALL extract_psy_data%ProvideVariable("ndf_w2", ndf_w2)
      CALL extract_psy_data%ProvideVariable("nlayers", nlayers)
      CALL extract_psy_data%ProvideVariable("undf_w2", undf_w2)
      CALL extract_psy_data%PreEnd
      DO df=loop0_start,loop0_stop
        f5_data(df) = 0.0
      END DO
      DO df=loop1_start,loop1_stop
        f2_data(df) = 0.0
      END DO
      DO cell=loop2_start,loop2_stop
        !
        CALL testkern_w2_only_code(nlayers, f3_data, """ + \
        """f2_data, ndf_w2, undf_w2, map_w2(:,cell))
      END DO
      CALL extract_psy_data%PostStart
      CALL extract_psy_data%ProvideVariable("cell_post", cell)
      CALL extract_psy_data%ProvideVariable("df_post", df)
      CALL extract_psy_data%ProvideVariable("f2_data_post", f2_data)
      CALL extract_psy_data%ProvideVariable("f3_data_post", f3_data)
      CALL extract_psy_data%ProvideVariable("f5_data_post", f5_data)
      CALL extract_psy_data%PostEnd
      !
      ! ExtractEnd"""

    assert output in code


def test_dynamo0p3_builtin():
    ''' Tests the handling of builtins.

    '''
    etrans = LFRicExtractTrans()
    psy, invoke = get_invoke("15.1.2_builtin_and_normal_kernel_invoke.f90",
                             DYNAMO_API, idx=0, dist_mem=False)
    schedule = invoke.schedule

    etrans.apply(schedule.children[0:3])
    code = str(psy.gen)

    output = """CALL extract_psy_data%PreDeclareVariable("loop1_start", """\
             """loop1_start)
      CALL extract_psy_data%PreDeclareVariable("loop1_stop", loop1_stop)
      CALL extract_psy_data%PreDeclareVariable("loop2_start", loop2_start)
      CALL extract_psy_data%PreDeclareVariable("loop2_stop", loop2_stop)
      CALL extract_psy_data%PreDeclareVariable("map_w2", map_w2)
      CALL extract_psy_data%PreDeclareVariable("ndf_w2", ndf_w2)
      CALL extract_psy_data%PreDeclareVariable("nlayers", nlayers)
      CALL extract_psy_data%PreDeclareVariable("undf_w2", undf_w2)
      CALL extract_psy_data%PreDeclareVariable("cell_post", cell)
      CALL extract_psy_data%PreDeclareVariable("df_post", df)
      CALL extract_psy_data%PreDeclareVariable("f2_data_post", f2_data)
      CALL extract_psy_data%PreDeclareVariable("f3_data_post", f3_data)
      CALL extract_psy_data%PreDeclareVariable("f5_data_post", f5_data)
      CALL extract_psy_data%PreEndDeclaration
      CALL extract_psy_data%ProvideVariable("f3_data", f3_data)
      CALL extract_psy_data%ProvideVariable("loop0_start", loop0_start)
      CALL extract_psy_data%ProvideVariable("loop0_stop", loop0_stop)
      CALL extract_psy_data%ProvideVariable("loop1_start", loop1_start)
      CALL extract_psy_data%ProvideVariable("loop1_stop", loop1_stop)
      CALL extract_psy_data%ProvideVariable("loop2_start", loop2_start)
      CALL extract_psy_data%ProvideVariable("loop2_stop", loop2_stop)
      CALL extract_psy_data%ProvideVariable("map_w2", map_w2)
      CALL extract_psy_data%ProvideVariable("ndf_w2", ndf_w2)
      CALL extract_psy_data%ProvideVariable("nlayers", nlayers)
      CALL extract_psy_data%ProvideVariable("undf_w2", undf_w2)
      CALL extract_psy_data%PreEnd
      DO df=loop0_start,loop0_stop
        f5_data(df) = 0.0
      END DO
      DO df=loop1_start,loop1_stop
        f2_data(df) = 0.0
      END DO
      DO cell=loop2_start,loop2_stop
        !
        CALL testkern_w2_only_code(nlayers, f3_data, f2_data, """\
        """ndf_w2, undf_w2, map_w2(:,cell))
      END DO
      CALL extract_psy_data%PostStart
      CALL extract_psy_data%ProvideVariable("cell_post", cell)
      CALL extract_psy_data%ProvideVariable("df_post", df)
      CALL extract_psy_data%ProvideVariable("f2_data_post", f2_data)
      CALL extract_psy_data%ProvideVariable("f3_data_post", f3_data)
      CALL extract_psy_data%ProvideVariable("f5_data_post", f5_data)
      CALL extract_psy_data%PostEnd
      !
      ! ExtractEnd"""
    assert output in code


def test_extract_single_builtin_dynamo0p3():
    ''' Test that extraction of a BuiltIn in an Invoke produces the
    correct result in Dynamo0.3 API without and with optimisations.

    '''
    etrans = LFRicExtractTrans()

    otrans = DynamoOMPParallelLoopTrans()

    psy, invoke = get_invoke("15.1.2_builtin_and_normal_kernel_invoke.f90",
                             DYNAMO_API, idx=0, dist_mem=False)
    schedule = invoke.schedule

    etrans.apply(schedule.children[1])
    code = str(psy.gen)
    output = """! ExtractStart
      !
      CALL extract_psy_data%PreStart("single_invoke_builtin_then_kernel_psy", """ \
      """"invoke_0:setval_c:r0", 2, 2)
      CALL extract_psy_data%PreDeclareVariable("loop1_start", loop1_start)
      CALL extract_psy_data%PreDeclareVariable("loop1_stop", loop1_stop)
      CALL extract_psy_data%PreDeclareVariable("df_post", df)
      CALL extract_psy_data%PreDeclareVariable("f2_data_post", f2_data)
      CALL extract_psy_data%PreEndDeclaration
      CALL extract_psy_data%ProvideVariable("loop1_start", loop1_start)
      CALL extract_psy_data%ProvideVariable("loop1_stop", loop1_stop)
      CALL extract_psy_data%PreEnd
      DO df=loop1_start,loop1_stop
        f2_data(df) = 0.0
      END DO
      CALL extract_psy_data%PostStart
      CALL extract_psy_data%ProvideVariable("df_post", df)
      CALL extract_psy_data%ProvideVariable("f2_data_post", f2_data)
      CALL extract_psy_data%PostEnd
      !
      ! ExtractEnd"""
    assert output in code

    # Test extract with OMP Parallel optimisation
    psy, invoke = get_invoke("15.1.1_builtin_and_normal_kernel_invoke_2.f90",
                             DYNAMO_API, idx=0, dist_mem=False)
    schedule = invoke.schedule

    otrans.apply(schedule.children[1])
    etrans.apply(schedule.children[1])
    code_omp = str(psy.gen)
    output = """
      ! ExtractStart
      !
      CALL extract_psy_data%PreStart("single_invoke_psy", """ \
      """"invoke_0:inc_ax_plus_y:r0", 4, 2)
      CALL extract_psy_data%PreDeclareVariable("f1_data", f1_data)
      CALL extract_psy_data%PreDeclareVariable("f2_data", f2_data)
      CALL extract_psy_data%PreDeclareVariable("loop1_start", loop1_start)
      CALL extract_psy_data%PreDeclareVariable("loop1_stop", loop1_stop)
      CALL extract_psy_data%PreDeclareVariable("df_post", df)
      CALL extract_psy_data%PreDeclareVariable("f1_data_post", f1_data)
      CALL extract_psy_data%PreEndDeclaration
      CALL extract_psy_data%ProvideVariable("f1_data", f1_data)
      CALL extract_psy_data%ProvideVariable("f2_data", f2_data)
      CALL extract_psy_data%ProvideVariable("loop1_start", loop1_start)
      CALL extract_psy_data%ProvideVariable("loop1_stop", loop1_stop)
      CALL extract_psy_data%PreEnd
      !$omp parallel do default(shared), private(df), schedule(static)
      DO df=loop1_start,loop1_stop
        f1_data(df) = 0.5_r_def * f1_data(df) + f2_data(df)
      END DO
      !$omp end parallel do
      CALL extract_psy_data%PostStart
      CALL extract_psy_data%ProvideVariable("df_post", df)
      CALL extract_psy_data%ProvideVariable("f1_data_post", f1_data)
      CALL extract_psy_data%PostEnd
      !
      ! ExtractEnd"""
    assert output in code_omp


def test_extract_kernel_and_builtin_dynamo0p3():
    ''' Test that extraction of a Kernel and a BuiltIny in an Invoke
    produces the correct result in Dynamo0.3 API.

    '''
    etrans = LFRicExtractTrans()

    psy, invoke = get_invoke("15.1.2_builtin_and_normal_kernel_invoke.f90",
                             DYNAMO_API, idx=0, dist_mem=False)
    schedule = invoke.schedule

    etrans.apply(schedule.children[1:3])
    code = str(psy.gen)
    output = """
      ! ExtractStart
      !
      CALL extract_psy_data%PreStart("single_invoke_builtin_then_kernel_psy", """ \
      """"invoke_0:r0", 9, 4)
      CALL extract_psy_data%PreDeclareVariable("f3_data", f3_data)
      CALL extract_psy_data%PreDeclareVariable("loop1_start", loop1_start)
      CALL extract_psy_data%PreDeclareVariable("loop1_stop", loop1_stop)
      CALL extract_psy_data%PreDeclareVariable("loop2_start", loop2_start)
      CALL extract_psy_data%PreDeclareVariable("loop2_stop", loop2_stop)
      CALL extract_psy_data%PreDeclareVariable("map_w2", map_w2)
      CALL extract_psy_data%PreDeclareVariable("ndf_w2", ndf_w2)
      CALL extract_psy_data%PreDeclareVariable("nlayers", nlayers)
      CALL extract_psy_data%PreDeclareVariable("undf_w2", undf_w2)
      CALL extract_psy_data%PreDeclareVariable("cell_post", cell)
      CALL extract_psy_data%PreDeclareVariable("df_post", df)
      CALL extract_psy_data%PreDeclareVariable("f2_data_post", f2_data)
      CALL extract_psy_data%PreDeclareVariable("f3_data_post", f3_data)
      CALL extract_psy_data%PreEndDeclaration
      CALL extract_psy_data%ProvideVariable("f3_data", f3_data)
      CALL extract_psy_data%ProvideVariable("loop1_start", loop1_start)
      CALL extract_psy_data%ProvideVariable("loop1_stop", loop1_stop)
      CALL extract_psy_data%ProvideVariable("loop2_start", loop2_start)
      CALL extract_psy_data%ProvideVariable("loop2_stop", loop2_stop)
      CALL extract_psy_data%ProvideVariable("map_w2", map_w2)
      CALL extract_psy_data%ProvideVariable("ndf_w2", ndf_w2)
      CALL extract_psy_data%ProvideVariable("nlayers", nlayers)
      CALL extract_psy_data%ProvideVariable("undf_w2", undf_w2)
      CALL extract_psy_data%PreEnd
      DO df=loop1_start,loop1_stop
        f2_data(df) = 0.0
      END DO
      DO cell=loop2_start,loop2_stop
        !
        CALL testkern_w2_only_code(nlayers, f3_data, """ + \
        """f2_data, ndf_w2, undf_w2, map_w2(:,cell))
      END DO
      CALL extract_psy_data%PostStart
      CALL extract_psy_data%ProvideVariable("cell_post", cell)
      CALL extract_psy_data%ProvideVariable("df_post", df)
      CALL extract_psy_data%ProvideVariable("f2_data_post", f2_data)
      CALL extract_psy_data%ProvideVariable("f3_data_post", f3_data)
      CALL extract_psy_data%PostEnd
      !
      ! ExtractEnd"""

    assert output in code

    # TODO #706: Compilation for LFRic extraction not supported yet.
    # assert LFRicBuild(tmpdir).code_compiles(psy)


def test_extract_colouring_omp_dynamo0p3():
    ''' Test that extraction of a Kernel in an Invoke after applying
    colouring and OpenMP optimisations produces the correct result
    in Dynamo0.3 API. '''

    const = LFRicConstants()
    etrans = LFRicExtractTrans()
    ctrans = Dynamo0p3ColourTrans()
    otrans = DynamoOMPParallelLoopTrans()

    psy, invoke = get_invoke("4.8_multikernel_invokes.f90",
                             DYNAMO_API, idx=0, dist_mem=False)
    schedule = invoke.schedule

    # First colour all of the loops over cells unless they are on
    # discontinuous spaces
    for child in schedule.children:
        if isinstance(child, Loop) and child.field_space.orig_name \
           not in const.VALID_DISCONTINUOUS_NAMES \
           and child.iteration_space == "cell_column":
            ctrans.apply(child)
    # Then apply OpenMP to each of the colour loops
    for child in schedule.children:
        if isinstance(child, Loop):
            if child.loop_type == "colours":
                otrans.apply(child.loop_body[0])
            else:
                otrans.apply(child)

    # Extract the second instance of ru_kernel_type after colouring
    # and OpenMP are applied
    child = schedule.children[2]
    etrans.apply(child)

    code = str(psy.gen)
    output = ("""
      ! ExtractStart
      !
      CALL extract_psy_data%PreStart("multikernel_invokes_7_psy", """
              """"invoke_0:ru_code:r0", 30, 3)
      CALL extract_psy_data%PreDeclareVariable("a_data", a_data)
      CALL extract_psy_data%PreDeclareVariable("b_data", b_data)
      CALL extract_psy_data%PreDeclareVariable("basis_w0_qr", basis_w0_qr)
      CALL extract_psy_data%PreDeclareVariable("basis_w2_qr", basis_w2_qr)
      CALL extract_psy_data%PreDeclareVariable("basis_w3_qr", basis_w3_qr)
      CALL extract_psy_data%PreDeclareVariable("c_data", c_data)
      CALL extract_psy_data%PreDeclareVariable("cmap", cmap)
      CALL extract_psy_data%PreDeclareVariable("diff_basis_w0_qr", """
              """diff_basis_w0_qr)
      CALL extract_psy_data%PreDeclareVariable("diff_basis_w2_qr", """
              """diff_basis_w2_qr)
      CALL extract_psy_data%PreDeclareVariable("e", e)
      CALL extract_psy_data%PreDeclareVariable("istp", istp)
      CALL extract_psy_data%PreDeclareVariable("last_edge_cell_all_colours", \
last_edge_cell_all_colours)
      CALL extract_psy_data%PreDeclareVariable("loop4_start", loop4_start)
      CALL extract_psy_data%PreDeclareVariable("loop4_stop", loop4_stop)
      CALL extract_psy_data%PreDeclareVariable("loop5_start", loop5_start)
      CALL extract_psy_data%PreDeclareVariable("map_w0", map_w0)
      CALL extract_psy_data%PreDeclareVariable("map_w2", map_w2)
      CALL extract_psy_data%PreDeclareVariable("map_w3", map_w3)
      CALL extract_psy_data%PreDeclareVariable("ndf_w0", ndf_w0)
      CALL extract_psy_data%PreDeclareVariable("ndf_w2", ndf_w2)
      CALL extract_psy_data%PreDeclareVariable("ndf_w3", ndf_w3)
      CALL extract_psy_data%PreDeclareVariable("nlayers", nlayers)
      CALL extract_psy_data%PreDeclareVariable("np_xy_qr", np_xy_qr)
      CALL extract_psy_data%PreDeclareVariable("np_z_qr", np_z_qr)
      CALL extract_psy_data%PreDeclareVariable("rdt", rdt)
      CALL extract_psy_data%PreDeclareVariable("undf_w0", undf_w0)
      CALL extract_psy_data%PreDeclareVariable("undf_w2", undf_w2)
      CALL extract_psy_data%PreDeclareVariable("undf_w3", undf_w3)
      CALL extract_psy_data%PreDeclareVariable("weights_xy_qr", weights_xy_qr)
      CALL extract_psy_data%PreDeclareVariable("weights_z_qr", weights_z_qr)
      CALL extract_psy_data%PreDeclareVariable("b_data_post", b_data)
      CALL extract_psy_data%PreDeclareVariable("cell_post", cell)
      CALL extract_psy_data%PreDeclareVariable("colour_post", colour)
      CALL extract_psy_data%PreEndDeclaration
      CALL extract_psy_data%ProvideVariable("a_data", a_data)
      CALL extract_psy_data%ProvideVariable("b_data", b_data)
      CALL extract_psy_data%ProvideVariable("basis_w0_qr", basis_w0_qr)
      CALL extract_psy_data%ProvideVariable("basis_w2_qr", basis_w2_qr)
      CALL extract_psy_data%ProvideVariable("basis_w3_qr", basis_w3_qr)
      CALL extract_psy_data%ProvideVariable("c_data", c_data)
      CALL extract_psy_data%ProvideVariable("cmap", cmap)
      CALL extract_psy_data%ProvideVariable("diff_basis_w0_qr", """
              """diff_basis_w0_qr)
      CALL extract_psy_data%ProvideVariable("diff_basis_w2_qr", """
              """diff_basis_w2_qr)
      CALL extract_psy_data%ProvideVariable("e", e)
      CALL extract_psy_data%ProvideVariable("istp", istp)
      CALL extract_psy_data%ProvideVariable("last_edge_cell_all_colours", \
last_edge_cell_all_colours)
      CALL extract_psy_data%ProvideVariable("loop4_start", loop4_start)
      CALL extract_psy_data%ProvideVariable("loop4_stop", loop4_stop)
      CALL extract_psy_data%ProvideVariable("loop5_start", loop5_start)
      CALL extract_psy_data%ProvideVariable("map_w0", map_w0)
      CALL extract_psy_data%ProvideVariable("map_w2", map_w2)
      CALL extract_psy_data%ProvideVariable("map_w3", map_w3)
      CALL extract_psy_data%ProvideVariable("ndf_w0", ndf_w0)
      CALL extract_psy_data%ProvideVariable("ndf_w2", ndf_w2)
      CALL extract_psy_data%ProvideVariable("ndf_w3", ndf_w3)
      CALL extract_psy_data%ProvideVariable("nlayers", nlayers)
      CALL extract_psy_data%ProvideVariable("np_xy_qr", np_xy_qr)
      CALL extract_psy_data%ProvideVariable("np_z_qr", np_z_qr)
      CALL extract_psy_data%ProvideVariable("rdt", rdt)
      CALL extract_psy_data%ProvideVariable("undf_w0", undf_w0)
      CALL extract_psy_data%ProvideVariable("undf_w2", undf_w2)
      CALL extract_psy_data%ProvideVariable("undf_w3", undf_w3)
      CALL extract_psy_data%ProvideVariable("weights_xy_qr", weights_xy_qr)
      CALL extract_psy_data%ProvideVariable("weights_z_qr", weights_z_qr)
      CALL extract_psy_data%PreEnd
      DO colour=loop4_start,loop4_stop
        !$omp parallel do default(shared), private(cell), schedule(static)
        DO cell=loop5_start,last_edge_cell_all_colours(colour)
          !
          CALL ru_code(nlayers, b_data, a_data, istp, rdt, """
              "c_data, e_1_data, e_2_data, "
              "e_3_data, ndf_w2, undf_w2, "
              "map_w2(:,cmap(colour,cell)), "
              "basis_w2_qr, diff_basis_w2_qr, ndf_w3, undf_w3, "
              "map_w3(:,cmap(colour,cell)), basis_w3_qr, ndf_w0, undf_w0, "
              "map_w0(:,cmap(colour,cell)), basis_w0_qr, diff_basis_w0_qr, "
              """np_xy_qr, np_z_qr, weights_xy_qr, weights_z_qr)
        END DO
        !$omp end parallel do
      END DO
      CALL extract_psy_data%PostStart
      CALL extract_psy_data%ProvideVariable("b_data_post", b_data)
      CALL extract_psy_data%ProvideVariable("cell_post", cell)
      CALL extract_psy_data%ProvideVariable("colour_post", colour)
      CALL extract_psy_data%PostEnd
      !
      ! ExtractEnd""")
    assert output in code

    # TODO #706: Compilation for LFRic extraction not supported yet.
    # assert LFRicBuild(tmpdir).code_compiles(psy)
