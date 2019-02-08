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

''' Module containing tests for PSyclone ExtractRegionTrans transformation '''

from __future__ import absolute_import

import os
import pytest

from psyclone.parse import parse
from psyclone.extractor import ExtractNode, Extractor
from psyclone.psyGen import PSyFactory, Loop
from psyclone.transformations import ExtractRegionTrans, TransformationError
from psyclone_test_utils import code_compiles, TEST_COMPILE

# Paths to APIs
DYNAMO_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                "test_files", "dynamo0p3")
DYNAMO_API = "dynamo0.3"

GOCEAN_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                "test_files", "gocean1p0")
GOCEAN_API = "gocean1.0"


def test_node_list_error():
    ''' Test that applying ExtractRegionTrans on a Kernel or Built-in call
    without its parent Loop raises a TransformationError. '''
    etrans = ExtractRegionTrans()

    _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
                                        "15.1.2_builtin_and_normal_kernel_"
                                        "invoke.f90"),
                           api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    # Supply object which is not a Node or a list of nodes
    with pytest.raises(TransformationError) as excinfo:
        etrans.apply("somestring")
    assert ("Error in ExtractRegionTrans: Argument must be a single Node "
            "in a Schedule or a list of Nodes in a Schedule but have "
            "been passed an object of type: ") in str(excinfo)
    # Python 3 reports 'class', python 2 'type' - so just check for both
    assert "<type 'str'>" in str(excinfo) or "<class 'str'>" in str(excinfo)
    #- Add more tests...


def test_distmem_error():
    ''' Test that applying ExtractRegionTrans with distributed memory
    enabled raises a TransformationError. '''
    etrans = ExtractRegionTrans()

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
    assert ("Error in ExtractRegionTrans: Distributed memory is not "
            "supported.") in str(excinfo.value)

    # Test GOcean1.0 API with distributed memory
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "single_invoke_three_kernels.f90"),
                           api=GOCEAN_API)
    psy = PSyFactory(GOCEAN_API, distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Try applying Extract transformation
    with pytest.raises(TransformationError) as excinfo:
        _, _ = etrans.apply(schedule.children[0])
    assert ("Error in ExtractRegionTrans: Distributed memory is not "
            "supported.") in str(excinfo.value)


def test_repeat_extract():
    ''' Test that applying ExtractRegionTrans on a node list that already
    contains an ExtractNode raises a TransformationError. '''
    etrans = ExtractRegionTrans()

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
    assert ("Error in ExtractRegionTrans: Extraction of a region which "
            "already contains another Extract region is not allowed.") \
        in str(excinfo.value)

    # Test GOcean1.0 API
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "single_invoke_three_kernels.f90"),
                           api=GOCEAN_API)
    psy = PSyFactory(GOCEAN_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Apply Extract transformation
    schedule, _ = etrans.apply(schedule.children[0])
    # Now try applying it again on the ExtractNode
    with pytest.raises(TransformationError) as excinfo:
        _, _ = etrans.apply(schedule.children[0])
    assert ("Error in ExtractRegionTrans: Extraction of a region which "
            "already contains another Extract region is not allowed.") \
        in str(excinfo.value)


def test_kern_builtin_no_loop():
    ''' Test that applying ExtractRegionTrans on a Kernel or Built-in call
    without its parent Loop raises a TransformationError. '''
    etrans = ExtractRegionTrans()

    # Test Dynamo0.3 API for Built-in call error
    _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
                                        "15.1.2_builtin_and_normal_kernel_"
                                        "invoke.f90"),
                           api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Test Built-in call
    builtin_call = schedule.children[1].children[0]
    with pytest.raises(TransformationError) as excinfo:
        _, _ = etrans.apply(builtin_call)
    assert ("Extraction of a Kernel or a Built-in call without its "
            "parent Loop is not allowed.") in str(excinfo.value)

    # Test GOcean1.0 API for Kernel call error
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "single_invoke_three_kernels.f90"),
                           api=GOCEAN_API)
    psy = PSyFactory(GOCEAN_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Test Kernel call
    kernel_call = schedule.children[0].children[0].children[0]
    with pytest.raises(TransformationError) as excinfo:
        _, _ = etrans.apply(kernel_call)
    assert ("Extraction of a Kernel or a Built-in call without its "
            "parent Loop is not allowed.") in str(excinfo.value)


def test_loop_no_directive_dynamo0p3():
    ''' Test that applying ExtractRegionTrans on a Loop without its
    parent Directive when optimisations are applied in Dynamo0.3 API
    raises a TransformationError.
    Note: Needs examples for GOcean and NEMO !!!!'''
    etrans = ExtractRegionTrans()

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
            "allowed.") in str(excinfo.value)


def test_loop_no_directive_gocean1p0():
    ''' Test that applying ExtractRegionTrans on a Loop without its
    parent Directive when optimisations are applied in GOcean1.0 API
    raises a TransformationError'''
    from psyclone.transformations import GOceanOMPParallelLoopTrans

    etrans = ExtractRegionTrans()
    otrans = GOceanOMPParallelLoopTrans()

    # Test a Loop nested within the OMP Parallel DO Directive
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "single_invoke_three_kernels.f90"),
                           api=GOCEAN_API)
    psy = PSyFactory(GOCEAN_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Apply GOceanOMPParallelLoopTrans to the first Loop
    schedule, _ = otrans.apply(schedule.children[0])
    loop = schedule.children[0].children[0]
    # Try extracting the Loop inside the OMP Parallel DO region
    with pytest.raises(TransformationError) as excinfo:
        _, _ = etrans.apply(loop)
    assert ("Extraction of a Loop without its parent Directive is not "
            "allowed.") in str(excinfo.value)


def test_no_parent_ompdirective():
    ''' Test that applying ExtractRegionTrans on an orphaned
    OMPDoDirective without its ancestor OMPRegionDirective when
    optimisations are applied raises a TransformationError. '''
    from psyclone.transformations import OMPParallelTrans, \
        Dynamo0p3OMPLoopTrans

    etrans = ExtractRegionTrans()
    ltrans = Dynamo0p3OMPLoopTrans()
    otrans = OMPParallelTrans()

    _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
                                        "4.13_multikernel_invokes_w3.f90"),
                           api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    # Apply the OpenMP Loop transformation to every loop in the Schedule
    for child in schedule.children:
        if isinstance(child, Loop):
            newschedule, _ = ltrans.apply(child)
    # Enclose all of these loops within a single OpenMP Parallel region
    schedule, _ = otrans.apply(newschedule.children)

    orphaned_directive = schedule.children[0].children[1]
    with pytest.raises(TransformationError) as excinfo:
        _, _ = etrans.apply(orphaned_directive)
    assert ("Extraction of an orphaned Directive without its ancestor "
            "Directive is not allowed.") in str(excinfo.value)


def test_no_parent_accdirective():
    ''' Test that applying ExtractRegionTrans on an orphaned
    ACCLoopDirective without its ancestor ACCParallelDirective
    when optimisations are applied raises a TransformationError. '''
    from psyclone.transformations import ACCParallelTrans, ACCDataTrans, \
        ACCLoopTrans

    etrans = ExtractRegionTrans()
    acclpt = ACCLoopTrans()
    accpara = ACCParallelTrans()
    accdata = ACCDataTrans()

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
    # Add a mandatory ACC data region
    schedule, _ = accdata.apply(schedule)

    orphaned_directive = schedule.children[1].children[0]
    with pytest.raises(TransformationError) as excinfo:
        _, _ = etrans.apply(orphaned_directive)
    assert ("Extraction of an orphaned Directive without its ancestor "
            "Directive is not allowed.") in str(excinfo.value)


def test_no_colours_loop_dynamo0p3():
    ''' Test that applying ExtractRegionTrans on a Loop over cells in a
    colour without its parent Loop over colours in Dynamo0.3 API raises
    a TransformationError. '''
    from psyclone.transformations import Dynamo0p3ColourTrans, \
        DynamoOMPParallelLoopTrans

    etrans = ExtractRegionTrans()
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
    # Try to extract the region between the Loop over cells in a colour
    # and the exterior Loop over colours
    colour_loop = schedule.children[0].children[0]
    with pytest.raises(TransformationError) as excinfo:
        _, _ = etrans.apply(colour_loop)
    assert ("Dynamo0.3 API: Extraction of a Loop over cells in a "
            "colour without its ancestor Loop over colours is not "
            "allowed.") in str(excinfo.value)

    # Now apply OMP Parallel DO Directive to the colour Loop and
    # try to extract the region between the Directive and the
    # exterior Loop over colours
    schedule, _ = otrans.apply(colour_loop)
    directive = schedule.children[0].children[0]
    with pytest.raises(TransformationError) as excinfo:
        _, _ = etrans.apply(directive)
    assert ("Dynamo0.3 API: Extraction of a Loop over cells in a "
            "colour without its ancestor Loop over colours is not "
            "allowed.") in str(excinfo.value)


def test_no_outer_loop_gocean1p0():
    ''' Test that applying ExtractRegionTrans on an inner Loop without its
    parent outer Loop in GOcean1p0 API raises a TransformationError. '''
    etrans = ExtractRegionTrans()

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
            "ancestor outer Loop is not allowed.") in str(excinfo.value)


def test_extract_node():
    '''
    Test that ExtractRegionTrans inserts the ExtractNode in the position
    of the first Node a Schedule in the Node list marked for extraction. '''
    etrans = ExtractRegionTrans()

    # Test GOcean1.0 API for extraction of a single Node
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
    schedule, _ = etrans.apply(child)
    extract_node = schedule.walk(schedule.children, ExtractNode)
    # The result is only one ExtractNode in the list with position 1
    assert extract_node[0].position == pos
    assert extract_node[0].abs_position == abspos
    assert extract_node[0].depth == dpth

    # Test Dynamo0.3 API for extraction of a list of Nodes
    _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
                                        "15.1.2_builtin_and_normal_kernel_"
                                        "invoke.f90"),
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
    schedule, _ = etrans.apply(children)
    extract_node = schedule.walk(schedule.children, ExtractNode)
    # The result is only one ExtractNode in the list with position 0
    assert extract_node[0].position == pos
    assert extract_node[0].abs_position == abspos
    assert extract_node[0].depth == dpth


def test_single_node_dynamo0p3():
    '''
    Test that ExtractRegionTrans on a single Node in a Schedule produces
    the correct result in Dynamo0.3 API. '''
    etrans = ExtractRegionTrans()

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
        "      ! CALL write_extract_arguments(argument_list)\n"
        "      !\n"
        "      ! ExtractStart\n"
        "      DO cell=1,f1_proxy%vspace%get_ncell()\n"
        "        !\n"
        "        CALL testkern_code(nlayers, a, f1_proxy%data, f2_proxy%data, "
        "m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, map_w3(:,cell))\n"
        "      END DO \n"
        "      ! ExtractEnd\n")
    assert output in code


def test_node_list_dynamo0p3():
    ''' Test that applying ExtractRegionTrans on a list of Nodes produces
    the correct result in Dynamo0.3 API. '''
    etrans = ExtractRegionTrans()

    _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
                                        "15.1.2_builtin_and_normal_kernel_"
                                        "invoke.f90"),
                           api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API,
                     distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    schedule, _ = etrans.apply(schedule.children[0:3])
    code = str(psy.gen)
    output = (
        "      ! CALL write_extract_arguments(argument_list)\n"
        "      !\n"
        "      ! ExtractStart\n"
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
        "      ! ExtractEnd\n")
    assert output in code


def test_multi_kernels_dynamo0p3(tmpdir, f90, f90flags):
    ''' This tests extraction of kernels via an interface which needs
    more work !!!!!!!!!!!!!!1'''
    _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
                                        "4.8_multikernel_invokes.f90"),
                           api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    schedule.view()
    # Define name of the kernel to extract
    kernel_name = "testkern_code"
    # schedule = Extractor.extract_kernel(schedule, kernel_name, 1)
    schedule = Extractor.extract_kernel(schedule, kernel_name)
    schedule.view()
    output = str(psy.gen)
    # print(output)
    # assert "random_numbers :: qr\n" in output

    if TEST_COMPILE:
        # If compilation testing has been enabled
        # (--compile --f90="<compiler_name>" flags to py.test)
        assert code_compiles(DYNAMO_API, psy, tmpdir, f90, f90flags)


def test_omp_kernels():

    from psyclone.transformations import DynamoOMPParallelLoopTrans

    otrans = DynamoOMPParallelLoopTrans()

    _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
                                        "4.13_multikernel_invokes_w3.f90"),
                           api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    schedule.view()

    # Apply OpenMP to each of the colour loops
    oschedule = schedule
    for child in oschedule.children:
        if isinstance(child, Loop):
            schedule, _ = otrans.apply(child)

    schedule.view()

    # Define name of the kernel to extract
    kernel_name = "testkern_w3_code"
    schedule = Extractor.extract_kernel(schedule, kernel_name, 0)
    schedule.view()
    output = str(psy.gen)
    # print(output)
    # assert "random_numbers :: qr\n" in output


def test_omp_kernels_invalid():

    from psyclone.transformations import OMPParallelTrans, \
        Dynamo0p3OMPLoopTrans

    ltrans = Dynamo0p3OMPLoopTrans()
    otrans = OMPParallelTrans()
    etrans = ExtractRegionTrans()

    _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
                                        "4.13_multikernel_invokes_w3.f90"),
                           api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    schedule.view()

    # Apply the OpenMP Loop transformation to *every* loop
    # in the schedule
    for child in schedule.children:
        newschedule, _ = ltrans.apply(child)
        schedule = newschedule
    # Enclose all of these loops within a single OpenMP
    # PARALLEL region
    newschedule, _ = otrans.apply(schedule.children)
    newschedule.view()

    # child = schedule.children[0]
    # - raises TransformationError
    child = schedule.children[0].children[1]
    # - raises TransformationError
    # child = schedule.children[0].children[1].children[0]
    # schedule, _ = etrans.apply(child)

    # Define name of the kernel to extract - this brings up problems
    # when there are two kernels with a same name in a parallel region,
    # as both have the same relative and absolute positions - this
    # needs to be better programmed in Extractor.extract_kernel
    # kernel_name = "testkern_w3_code"
    # schedule = Extractor.extract_kernel(newschedule, kernel_name)
    schedule.view()
    output = str(psy.gen)
    # print(output)
    # assert "random_numbers :: qr\n" in output


# def test_extract_view(capsys):
    # ''' Test the view method in the ExtractNode class. '''
    # from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP
    # from psyclone import dynamo0p3
    # _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
    #                        "15.9.1_X_innerproduct_Y_builtin.f90"),
    #                        api=DYNAMO_API)
    # psy = PSyFactory(DYNAMO_API, distributed_memory=True).create(invoke_info)
    # psy.invokes.invoke_list[0].schedule.view()
    # output, _ = capsys.readouterr()
    # print(output)
    # expected_output = (colored("GlobalSum",
    #                             SCHEDULE_COLOUR_MAP["GlobalSum"]) +
    #                    "[scalar='asum']")
    # assert expected_output in output
    # gsum = None
    # for child in psy.invokes.invoke_list[0].schedule.children:
    #     if isinstance(child, dynamo0p3.DynGlobalSum):
    #         gsum = child
    #         break
    # assert gsum
    # ret_str = super(dynamo0p3.DynGlobalSum, gsum).coloured_text
    # assert colored("GlobalSum", SCHEDULE_COLOUR_MAP["GlobalSum"]) in ret_str
