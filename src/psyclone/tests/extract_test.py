# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council
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
# -----------------------------------------------------------------------------

''' Module containing tests for PSyclone DynamoExtractRegionTrans and
GOceanExtractRegionTrans transformations and ExtractNode.
'''

from __future__ import absolute_import

import os
import pytest

from dynamo0p3_build import Dynamo0p3Build
from psyclone.parse.algorithm import parse
from psyclone.extractor import ExtractNode
from psyclone.psyGen import PSyFactory, Loop
from psyclone.transformations import TransformationError, \
    DynamoExtractRegionTrans, GOceanExtractRegionTrans

# Paths to APIs
DYNAMO_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                "test_files", "dynamo0p3")
DYNAMO_API = "dynamo0.3"

GOCEAN_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                "test_files", "gocean1p0")
GOCEAN_API = "gocean1.0"

# --------------------------------------------------------------------------- #
# ================== Extract Transformation tests =========================== #
# --------------------------------------------------------------------------- #


def test_node_list_error(tmpdir):
    ''' Test that applying Extract Transformation on objects which are not
    Nodes or a list of Nodes raises a TransformationError. Also raise
    transformation errors when the Nodes do not have the same parent
    if they are incorrectly ordered. '''
    etrans = DynamoExtractRegionTrans()

    _, invoke_info = parse(
        os.path.join(DYNAMO_BASE_PATH,
                     "3.2_multi_functions_multi_named_invokes.f90"),
        api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=False).create(invoke_info)
    invoke0 = psy.invokes.invoke_list[0]
    invoke1 = psy.invokes.invoke_list[1]
    # Supply an object which is not a Node or a list of Nodes
    with pytest.raises(TransformationError) as excinfo:
        etrans.apply(invoke0)
    assert ("Error in DynamoExtractRegionTrans: Argument must be "
            "a single Node in a Schedule or a list of Nodes in a Schedule "
            "but have been passed an object of type: "
            "<class 'psyclone.dynamo0p3.DynInvoke'>") in str(excinfo)

    # Supply Nodes in incorrect order or duplicate Nodes
    node_list = [invoke0.schedule.children[0],
                 invoke0.schedule.children[0],
                 invoke0.schedule.children[1]]
    with pytest.raises(TransformationError) as excinfo:
        etrans.apply(node_list)
    assert "Children are not consecutive children of one parent:" \
           in str(excinfo)
    assert ("kern call: testkern_code\\nEndLoop\' has position 0, "
            "but previous child had position 0.") in str(excinfo)

    # Supply Nodes which are not children of the same parent
    node_list = [invoke0.schedule.children[1],
                 invoke1.schedule.children[0],
                 invoke0.schedule.children[2]]
    with pytest.raises(TransformationError) as excinfo:
        etrans.apply(node_list)
    assert ("supplied nodes are not children of the same "
            "parent.") in str(excinfo)

    assert Dynamo0p3Build(tmpdir).code_compiles(psy)


def test_distmem_error():
    ''' Test that applying ExtractRegionTrans with distributed memory
    enabled raises a TransformationError. '''
    etrans = DynamoExtractRegionTrans()

    # Test Dynamo0.3 API with distributed memory
    _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
                                        "1_single_invoke.f90"),
                           api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Try applying Extract transformation
    with pytest.raises(TransformationError) as excinfo:
        _, _ = etrans.apply(schedule.children[3])
    assert ("Error in DynamoExtractRegionTrans: Distributed memory is "
            "not supported.") in str(excinfo.value)

    # Try applying Extract transformation to Node(s) containing HaloExchange
    with pytest.raises(TransformationError) as excinfo:
        _, _ = etrans.apply(schedule.children[2:4])
    assert ("Nodes of type '<class 'psyclone.dynamo0p3.DynHaloExchange'>' "
            "cannot be enclosed by a DynamoExtractRegionTrans "
            "transformation") in str(excinfo)

    # Try applying Extract transformation to Node(s) containing GlobalSum
    _, invoke_info = parse(
        os.path.join(DYNAMO_BASE_PATH, "15.14.3_sum_setval_field_builtin.f90"),
        api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    glob_sum = schedule.children[2]
    with pytest.raises(TransformationError) as excinfo:
        _, _ = etrans.apply(glob_sum)
    assert ("Nodes of type '<class 'psyclone.dynamo0p3.DynGlobalSum'>' "
            "cannot be enclosed by a DynamoExtractRegionTrans "
            "transformation") in str(excinfo)


def test_repeat_extract():
    ''' Test that applying Extract Transformation on Node(s) already
    containing an ExtractNode raises a TransformationError. '''
    etrans = DynamoExtractRegionTrans()

    # Test Dynamo0.3 API
    _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
                                        "1_single_invoke.f90"),
                           api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Apply Extract transformation
    schedule, _ = etrans.apply(schedule.children[0])
    # Now try applying it again on the ExtractNode
    with pytest.raises(TransformationError) as excinfo:
        _, _ = etrans.apply(schedule.children[0])
    assert ("Nodes of type '<class 'psyclone.extractor.ExtractNode'>' "
            "cannot be enclosed by a DynamoExtractRegionTrans "
            "transformation") in str(excinfo)


def test_kern_builtin_no_loop():
    ''' Test that applying Extract Transformation on a Kernel or Built-in
    call without its parent Loop raises a TransformationError. '''

    # Test Dynamo0.3 API for Built-in call error
    dynetrans = DynamoExtractRegionTrans()
    _, invoke_info = parse(
        os.path.join(DYNAMO_BASE_PATH,
                     "15.1.2_builtin_and_normal_kernel_invoke.f90"),
        api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Test Built-in call
    builtin_call = schedule.children[1].children[0]
    with pytest.raises(TransformationError) as excinfo:
        _, _ = dynetrans.apply(builtin_call)
    assert ("Extraction of a Kernel or a Built-in call without its "
            "parent Loop is not allowed.") in str(excinfo)

    # Test GOcean1.0 API for Kernel call error
    gocetrans = GOceanExtractRegionTrans()
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "single_invoke_three_kernels.f90"),
                           api=GOCEAN_API)
    psy = PSyFactory(GOCEAN_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Test Kernel call
    kernel_call = schedule.children[0].children[0].children[0]
    with pytest.raises(TransformationError) as excinfo:
        _, _ = gocetrans.apply(kernel_call)
    assert ("Extraction of a Kernel or a Built-in call without its "
            "parent Loop is not allowed.") in str(excinfo)


def test_loop_no_directive_dynamo0p3():
    ''' Test that applying Extract Transformation on a Loop without its
    parent Directive when optimisations are applied in Dynamo0.3 API
    raises a TransformationError. '''
    etrans = DynamoExtractRegionTrans()

    from psyclone.transformations import DynamoOMPParallelLoopTrans

    # Test a Loop nested within the OMP Parallel DO Directive
    _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
                                        "4.13_multikernel_invokes_w3.f90"),
                           api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Apply DynamoOMPParallelLoopTrans to the second Loop
    otrans = DynamoOMPParallelLoopTrans()
    schedule, _ = otrans.apply(schedule.children[1])
    loop = schedule.children[1].children[0]
    # Try extracting the Loop inside the OMP Parallel DO region
    with pytest.raises(TransformationError) as excinfo:
        _, _ = etrans.apply(loop)
    assert ("Extraction of a Loop without its parent Directive is not "
            "allowed.") in str(excinfo)


def test_no_parent_accdirective():
    ''' Test that applying Extract Transformation on an orphaned
    ACCLoopDirective without its ancestor ACCParallelDirective
    when optimisations are applied raises a TransformationError. '''
    from psyclone.transformations import ACCParallelTrans, ACCEnterDataTrans, \
        ACCLoopTrans

    etrans = GOceanExtractRegionTrans()
    acclpt = ACCLoopTrans()
    accpara = ACCParallelTrans()
    accdata = ACCEnterDataTrans()

    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "single_invoke_three_kernels.f90"),
                           api=GOCEAN_API)
    psy = PSyFactory(GOCEAN_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    # Apply the OpenACC Loop transformation to every loop in the Schedule
    for child in schedule.children:
        if isinstance(child, Loop):
            schedule, _ = acclpt.apply(child)
    # Enclose all of these loops within a single ACC Parallel region
    schedule, _ = accpara.apply(schedule.children)
    # Add a mandatory ACC enter-data directive
    schedule, _ = accdata.apply(schedule)

    orphaned_directive = schedule.children[1].children[0]
    with pytest.raises(TransformationError) as excinfo:
        _, _ = etrans.apply(orphaned_directive)
    assert ("Extraction of Nodes enclosed within a thread parallel "
            "region is not allowed.") in str(excinfo)


def test_no_colours_loop_dynamo0p3():
    ''' Test that applying DynamoExtractRegionTrans on a Loop over cells
    in a colour without its parent Loop over colours in Dynamo0.3 API
    raises a TransformationError. '''
    from psyclone.transformations import Dynamo0p3ColourTrans, \
        DynamoOMPParallelLoopTrans

    etrans = DynamoExtractRegionTrans()
    ctrans = Dynamo0p3ColourTrans()
    otrans = DynamoOMPParallelLoopTrans()

    _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
                                        "1_single_invoke.f90"),
                           api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    # Colour first loop that calls testkern_code (loop is over cells and
    # is not on a discontinuous space)
    schedule, _ = ctrans.apply(schedule.children[0])
    colour_loop = schedule.children[0].children[0]
    # Apply OMP Parallel DO Directive to the colour Loop
    schedule, _ = otrans.apply(colour_loop)
    directive = schedule.children[0].children[0]
    # Try to extract the region between the Loop over cells in a colour
    # and the exterior Loop over colours
    with pytest.raises(TransformationError) as excinfo:
        _, _ = etrans.apply(directive)
    assert ("Dynamo0.3 API: Extraction of a Loop over cells in a "
            "colour without its ancestor Loop over colours is not "
            "allowed.") in str(excinfo)


def test_no_outer_loop_gocean1p0():
    ''' Test that applying GOceanExtractRegionTrans on an inner Loop without
    its parent outer Loop in GOcean1.0 API raises a TransformationError. '''
    etrans = GOceanExtractRegionTrans()

    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "single_invoke_three_kernels.f90"),
                           api=GOCEAN_API)
    psy = PSyFactory(GOCEAN_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    # Try to extract the region between the outer and the inner Loop
    inner_loop = schedule.children[0].children[0]
    with pytest.raises(TransformationError) as excinfo:
        _, _ = etrans.apply(inner_loop)
    assert ("GOcean1.0 API: Extraction of an inner Loop without its "
            "ancestor outer Loop is not allowed.") in str(excinfo)

# --------------------------------------------------------------------------- #
# ================== ExtractNode tests ====================================== #
# --------------------------------------------------------------------------- #


def test_extract_node_genccode():
    ''' Test that trying to apply the gen_c_code method of ExtractNode
    class raises appropriate error. '''

    etrans = DynamoExtractRegionTrans()

    _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
                                        "4.8_multikernel_invokes.f90"),
                           api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    schedule, _ = etrans.apply(schedule.children[1])
    # ExtractNode is inserted at the original place of the first
    # extracted Node
    extract_node = schedule.children[1]
    with pytest.raises(NotImplementedError) as excinfo:
        ExtractNode.gen_c_code(extract_node)
    assert ("Generation of C code is not supported "
            "for code extraction") in str(excinfo)


def test_extract_node_position():
    ''' Test that Extract Transformation inserts the ExtractNode
    at the position of the first Node a Schedule in the Node list
    marked for extraction. '''

    # Test GOcean1.0 API for extraction of a single Node
    gocetrans = GOceanExtractRegionTrans()
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "single_invoke_three_kernels.f90"),
                           api=GOCEAN_API)
    psy = PSyFactory(GOCEAN_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Apply Extract transformation to the second Node and assert that
    # position and the absolute position of the ExtractNode are the same as
    # respective positions of the second Node before the transformation.
    pos = 1
    child = schedule.children[pos]
    abspos = child.abs_position
    dpth = child.depth
    schedule, _ = gocetrans.apply(child)
    extract_node = schedule.walk(schedule.children, ExtractNode)
    # The result is only one ExtractNode in the list with position 1
    assert extract_node[0].position == pos
    assert extract_node[0].abs_position == abspos
    assert extract_node[0].depth == dpth

    # Test Dynamo0.3 API for extraction of a list of Nodes
    dynetrans = DynamoExtractRegionTrans()
    _, invoke_info = parse(
        os.path.join(DYNAMO_BASE_PATH,
                     "15.1.2_builtin_and_normal_kernel_invoke.f90"),
        api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API,
                     distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Apply Extract transformation to the first three Nodes and assert that
    # position and the absolute position of the ExtractNode are the same as
    # respective positions of the first Node before the transformation.
    pos = 0
    children = schedule.children[pos:pos+3]
    abspos = children[0].abs_position
    dpth = children[0].depth
    schedule, _ = dynetrans.apply(children)
    extract_node = schedule.walk(schedule.children, ExtractNode)
    # The result is only one ExtractNode in the list with position 0
    assert extract_node[0].position == pos
    assert extract_node[0].abs_position == abspos
    assert extract_node[0].depth == dpth


def test_extract_node_representation(capsys):
    ''' Test that representation properties and methods of the ExtractNode
    class: view, dag_name and __str__ produce the correct results. '''
    from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP

    etrans = DynamoExtractRegionTrans()

    _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
                                        "4.8_multikernel_invokes.f90"),
                           api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    children = schedule.children[1:3]
    schedule, _ = etrans.apply(children)

    # Test view() method
    schedule.view()
    output, _ = capsys.readouterr()
    expected_output = colored("Extract", SCHEDULE_COLOUR_MAP["Extract"])
    assert expected_output in output

    # Test dag_name method
    assert schedule.children[1].dag_name == "extract_1"

    # Test __str__ method
    correct = ("""Schedule:
Loop[]: cell= lower=,,
kern call: testkern_code
EndLoop
ExtractStart
Loop[]: cell= lower=,,
kern call: testkern_code
EndLoop
Loop[]: cell= lower=,,
kern call: ru_code
EndLoop
ExtractEnd
Loop[]: cell= lower=,,
kern call: ru_code
EndLoop
Loop[]: cell= lower=,,
kern call: testkern_code
EndLoop
End Schedule""")
    assert correct in str(schedule)


def test_single_node_dynamo0p3():
    ''' Test that Extract Transformation on a single Node in a Schedule
    produces the correct result in Dynamo0.3 API. '''
    etrans = DynamoExtractRegionTrans()

    _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
                                        "1_single_invoke.f90"),
                           api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API,
                     distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    schedule, _ = etrans.apply(schedule.children[0])
    code = str(psy.gen)
    output = (
        "      ! ExtractStart\n"
        "      ! CALL write_extract_arguments(argument_list)\n"
        "      !\n"
        "      DO cell=1,f1_proxy%vspace%get_ncell()\n"
        "        !\n"
        "        CALL testkern_code(nlayers, a, f1_proxy%data, f2_proxy%data, "
        "m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))\n"
        "      END DO \n"
        "      !\n"
        "      ! ExtractEnd\n")
    assert output in code


def test_node_list_dynamo0p3():
    ''' Test that applying Extract Transformation on a list of Nodes
    produces the correct result in Dynamo0.3 API. '''
    etrans = DynamoExtractRegionTrans()

    _, invoke_info = parse(
        os.path.join(DYNAMO_BASE_PATH,
                     "15.1.2_builtin_and_normal_kernel_invoke.f90"),
        api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API,
                     distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    schedule, _ = etrans.apply(schedule.children[0:3])
    code = str(psy.gen)
    output = (
        "      ! ExtractStart\n"
        "      ! CALL write_extract_arguments(argument_list)\n"
        "      !\n"
        "      DO df=1,undf_any_space_1_f5\n"
        "        f5_proxy%data(df) = 0.0\n"
        "      END DO \n"
        "      DO df=1,undf_any_space_1_f2\n"
        "        f2_proxy%data(df) = 0.0\n"
        "      END DO \n"
        "      DO cell=1,f3_proxy%vspace%get_ncell()\n"
        "        !\n"
        "        CALL testkern_code_w2_only(nlayers, f3_proxy%data, "
        "f2_proxy%data, ndf_w2, undf_w2, map_w2(:,cell))\n"
        "      END DO \n"
        "      !\n"
        "      ! ExtractEnd\n")
    assert output in code


def test_single_node_ompparalleldo_gocean1p0():
    ''' Test that applying Extract Transformation on a Node enclosed
    within an OMP Parallel DO Directive produces the correct result
    in GOcean1.0 API. '''
    from psyclone.transformations import GOceanOMPParallelLoopTrans

    etrans = GOceanExtractRegionTrans()
    otrans = GOceanOMPParallelLoopTrans()

    # Test a Loop nested within the OMP Parallel DO Directive
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "single_invoke_three_kernels.f90"),
                           api=GOCEAN_API)
    psy = PSyFactory(GOCEAN_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    # Apply GOceanOMPParallelLoopTrans to the second Loop
    schedule, _ = otrans.apply(schedule.children[1])
    # Now enclose the parallel region within an ExtractNode (inserted
    # at the previous location of the OMPParallelDoDirective
    schedule, _ = etrans.apply(schedule.children[1])

    code = str(psy.gen)
    output = (
        "      ! ExtractStart\n"
        "      ! CALL write_extract_arguments(argument_list)\n"
        "      !\n"
        "      !$omp parallel do default(shared), private(i,j), "
        "schedule(static)\n"
        "      DO j=2,jstop+1\n"
        "        DO i=2,istop\n"
        "          CALL compute_cv_code(i, j, cv_fld%data, "
        "p_fld%data, v_fld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "      !$omp end parallel do\n"
        "      !\n"
        "      ! ExtractEnd\n")
    assert output in code


def test_node_list_ompparallel_gocean1p0():
    ''' Test that applying Extract Transformation on a list of Nodes
    enclosed within an OMP Parallel Region produces the correct result
    in GOcean1.0 API. '''
    from psyclone.transformations import GOceanOMPLoopTrans, OMPParallelTrans

    etrans = GOceanExtractRegionTrans()
    ltrans = GOceanOMPLoopTrans()
    otrans = OMPParallelTrans()

    # Test a Loop nested within the OMP Parallel DO Directive
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "single_invoke_three_kernels.f90"),
                           api=GOCEAN_API)
    psy = PSyFactory(GOCEAN_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    # Apply GOceanOMPParallelLoopTrans to the first two Loops
    schedule, _ = ltrans.apply(schedule.children[0])
    schedule, _ = ltrans.apply(schedule.children[1])
    # and enclose them within a parallel region
    schedule, _ = otrans.apply(schedule.children[0:2])
    # Now enclose the parallel region within an ExtractNode (inserted
    # at the previous location of the OMPParallelDirective
    schedule, _ = etrans.apply(schedule.children[0])

    code = str(psy.gen)
    output = (
        "      ! ExtractStart\n"
        "      ! CALL write_extract_arguments(argument_list)\n"
        "      !\n"
        "      !$omp parallel default(shared), private(i,j)\n"
        "      !$omp do schedule(static)\n"
        "      DO j=2,jstop\n"
        "        DO i=2,istop+1\n"
        "          CALL compute_cu_code(i, j, cu_fld%data, "
        "p_fld%data, u_fld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "      !$omp end do\n"
        "      !$omp do schedule(static)\n"
        "      DO j=2,jstop+1\n"
        "        DO i=2,istop\n"
        "          CALL compute_cv_code(i, j, cv_fld%data, "
        "p_fld%data, v_fld%data)\n"
        "        END DO \n"
        "      END DO \n"
        "      !$omp end do\n"
        "      !$omp end parallel\n"
        "      !\n"
        "      ! ExtractEnd\n")
    assert output in code


def test_extract_single_builtin_dynamo0p3():
    ''' Test that extraction of a BuiltIn in an Invoke produces the
    correct result in Dynamo0.3 API without and with optimisations. '''
    from psyclone.transformations import DynamoOMPParallelLoopTrans

    etrans = DynamoExtractRegionTrans()
    otrans = DynamoOMPParallelLoopTrans()

    # Test extract without optimisations
    _, invoke_info = parse(
        os.path.join(DYNAMO_BASE_PATH,
                     "15.1.2_builtin_and_normal_kernel_invoke.f90"),
        api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    schedule, _ = etrans.apply(schedule.children[1])
    code = str(psy.gen)
    output = (
        "      ! ExtractStart\n"
        "      ! CALL write_extract_arguments(argument_list)\n"
        "      !\n"
        "      DO df=1,undf_any_space_1_f2\n"
        "        f2_proxy%data(df) = 0.0\n"
        "      END DO \n"
        "      !\n"
        "      ! ExtractEnd\n")
    assert output in code

    # Test extract with OMP Parallel optimisation
    _, invoke_info = parse(
        os.path.join(DYNAMO_BASE_PATH,
                     "15.1.1_builtin_and_normal_kernel_invoke_2.f90"),
        api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    schedule, _ = otrans.apply(schedule.children[1])
    schedule, _ = etrans.apply(schedule.children[1])
    code = str(psy.gen)
    output = (
        "      ! ExtractStart\n"
        "      ! CALL write_extract_arguments(argument_list)\n"
        "      !\n"
        "      !$omp parallel do default(shared), private(df), "
        "schedule(static)\n"
        "      DO df=1,undf_any_space_1_f1\n"
        "        f1_proxy%data(df) = 0.5*f1_proxy%data(df) + "
        "f2_proxy%data(df)\n"
        "      END DO \n"
        "      !$omp end parallel do\n"
        "      !\n"
        "      ! ExtractEnd\n")
    assert output in code


def test_extract_kernel_and_builtin_dynamo0p3(tmpdir):
    ''' Test that extraction of a Kernel and a BuiltIny in an Invoke
    produces the correct result in Dynamo0.3 API. '''
    etrans = DynamoExtractRegionTrans()

    _, invoke_info = parse(
        os.path.join(DYNAMO_BASE_PATH,
                     "15.1.2_builtin_and_normal_kernel_invoke.f90"),
        api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    schedule, _ = etrans.apply(schedule.children[1:3])
    code = str(psy.gen)
    output = (
        "      ! ExtractStart\n"
        "      ! CALL write_extract_arguments(argument_list)\n"
        "      !\n"
        "      DO df=1,undf_any_space_1_f2\n"
        "        f2_proxy%data(df) = 0.0\n"
        "      END DO \n"
        "      DO cell=1,f3_proxy%vspace%get_ncell()\n"
        "        !\n"
        "        CALL testkern_code_w2_only(nlayers, f3_proxy%data, "
        "f2_proxy%data, ndf_w2, undf_w2, map_w2(:,cell))\n"
        "      END DO \n"
        "      !\n"
        "      ! ExtractEnd\n")
    assert output in code

    assert Dynamo0p3Build(tmpdir).code_compiles(psy)


def test_extract_colouring_omp_dynamo0p3(tmpdir):
    ''' Test that extraction of a Kernel in an Invoke after applying
    colouring and OpenMP optimisations produces the correct result
    in Dynamo0.3 API. '''
    from psyclone.transformations import Dynamo0p3ColourTrans, \
        DynamoOMPParallelLoopTrans
    from psyclone.dynamo0p3 import DISCONTINUOUS_FUNCTION_SPACES

    etrans = DynamoExtractRegionTrans()
    ctrans = Dynamo0p3ColourTrans()
    otrans = DynamoOMPParallelLoopTrans()

    _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
                                        "4.8_multikernel_invokes.f90"),
                           api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    # First colour all of the loops over cells unless they are on
    # discontinuous spaces
    cschedule = schedule
    for child in schedule.children:
        if isinstance(child, Loop) and child.field_space.orig_name \
           not in DISCONTINUOUS_FUNCTION_SPACES \
           and child.iteration_space == "cells":
            cschedule, _ = ctrans.apply(child)
    # Then apply OpenMP to each of the colour loops
    schedule = cschedule
    for child in schedule.children:
        if isinstance(child, Loop):
            if child.loop_type == "colours":
                schedule, _ = otrans.apply(child.children[0])
            else:
                schedule, _ = otrans.apply(child)

    # Extract the second instance of ru_kernel_type after colouring
    # and OpenMP are applied
    child = schedule.children[2]
    schedule, _ = etrans.apply(child)

    code = str(psy.gen)
    output = (
        "      ! ExtractStart\n"
        "      ! CALL write_extract_arguments(argument_list)\n"
        "      !\n"
        "      DO colour=1,ncolour\n"
        "        !$omp parallel do default(shared), private(cell), "
        "schedule(static)\n"
        "        DO cell=1,mesh%get_last_edge_cell_per_colour(colour)\n"
        "          !\n"
        "          CALL ru_code(nlayers, b_proxy%data, a_proxy%data, istp, "
        "rdt, c_proxy%data, e_proxy(1)%data, e_proxy(2)%data, "
        "e_proxy(3)%data, ndf_w2, undf_w2, map_w2(:,cmap(colour, cell)), "
        "basis_w2_qr, diff_basis_w2_qr, ndf_w3, undf_w3, "
        "map_w3(:,cmap(colour, cell)), basis_w3_qr, ndf_w0, undf_w0, "
        "map_w0(:,cmap(colour, cell)), basis_w0_qr, diff_basis_w0_qr, "
        "np_xy_qr, np_z_qr, weights_xy_qr, weights_z_qr)\n"
        "        END DO \n"
        "        !$omp end parallel do\n"
        "      END DO \n"
        "      !\n"
        "      ! ExtractEnd\n")
    assert output in code

    assert Dynamo0p3Build(tmpdir).code_compiles(psy)
