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

''' Module containing tests for PSyclone Kernel Extractor '''

from __future__ import absolute_import

import os
import re
import pytest

from psyclone.parse import parse, ParseError
from psyclone.extractor import ExtractNode, Extractor
from psyclone.psyGen import PSyFactory, GenerationError, Loop
from psyclone.configuration import Config
from psyclone.transformations import ExtractRegionTrans, \
    TransformationError, MoveTrans, Dynamo0p3ColourTrans, \
    DynamoOMPParallelLoopTrans
from psyclone.dynamo0p3 import DynKern
from psyclone.dynamo0p3_builtins import DynBuiltIn

# constants
DYNAMO_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                "test_files", "dynamo0p3")
DYNAMO_API = "dynamo0.3"

GOCEAN_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                "test_files", "gocean1p0")
GOCEAN_API = "gocean1.0"


def test_extract_node():
    '''
    Test that ExtractRegionTrans on a single Node in a Schedule produces
    the correct result. '''
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


def test_extract_node_list():
    ''' Test that applying ExtractRegionTrans on a list of Nodes produces
    the correct result. '''
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


def test_distmem_error():
    ''' Test that applying ExtractRegionTrans with distributed memory
    enabled raises a TransformationError. '''
    etrans = ExtractRegionTrans()

    _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
                                        "1_single_invoke.f90"),
                           api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    with pytest.raises(TransformationError) as excinfo:
        _, _ = etrans.apply(schedule.children[3])
    assert ("Error in ExtractRegionTrans: Distributed memory is not "
            "supported.") in str(excinfo.value)


def test_repeat_extract_error():
    ''' Test that applying ExtractRegionTrans on a node list that already
    contains an ExtractNode raises a TransformationError. '''
    etrans = ExtractRegionTrans()

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


def test_extract_kern_builtin_no_loop_error():
    ''' Test that applying ExtractRegionTrans on a Kernel call without
    its parent Loop raises a TransformationError. '''
    etrans = ExtractRegionTrans()

    _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
                                        "15.1.2_builtin_and_normal_kernel_"
                                        "invoke.f90"),
                           api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    # Test Kernel call
    child = schedule.children[3].children[0]
    with pytest.raises(TransformationError) as excinfo:
        _, _ = etrans.apply(child)
    assert ("Extraction of a Kernel or a Built-in call without its "
            "parent Loop is not allowed.") in str(excinfo.value)
    # Test Built-in call
    child = schedule.children[1].children[0]
    with pytest.raises(TransformationError) as excinfo:
        _, _ = etrans.apply(child)
    assert ("Extraction of a Kernel or a Built-in call without its "
            "parent Loop is not allowed.") in str(excinfo.value)


def test_loop_no_directive_error():
    ''' Test that applying ExtractRegionTrans on a Loop without its parent
    Directive when optimisations are applied raises a TransformationError.
    Note: Needs examples for GOcean and NEMO !!!!'''
    etrans = ExtractRegionTrans()

    from psyclone.transformations import DynamoOMPParallelLoopTrans
    otrans = DynamoOMPParallelLoopTrans()

    _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
                                        "4.13_multikernel_invokes_w3.f90"),
                           api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    # Apply DynamoOMPParallelLoopTrans to each Loop
    for child in schedule.children:
        if isinstance(child, Loop):
            schedule, _ = otrans.apply(child)

    child = schedule.children[1].children[0]
    with pytest.raises(TransformationError) as excinfo:
        _, _ = etrans.apply(child)
    assert ("Extraction of a Loop without its parent Directive is not "
            "allowed.") in str(excinfo.value)


def test_orphaned_directive_error():
    ''' Test that applying ExtractRegionTrans on an orphaned Directive 
    without its parent Directive when optimisations are applied raises
    a TransformationError.
    Note: Needs examples for GOcean and NEMO (ACCLoopDirective) !!!!'''
    etrans = ExtractRegionTrans()

    from psyclone.transformations import OMPParallelTrans, \
        Dynamo0p3OMPLoopTrans

    ltrans = Dynamo0p3OMPLoopTrans()
    otrans = OMPParallelTrans()

    _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
                                        "4.13_multikernel_invokes_w3.f90"),
                           api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    # Apply the OpenMP Loop transformation to every loop in the schedule
    for child in schedule.children:
        newschedule, _ = ltrans.apply(child)
    # Enclose all of these loops within a single OpenMP Parallel region
    schedule, _ = otrans.apply(newschedule.children)

    child = schedule.children[0].children[1]
    with pytest.raises(TransformationError) as excinfo:
        _, _ = etrans.apply(child)
    assert ("Extraction of an orphaned Directive without its parent "
            "Directive is not allowed.") in str(excinfo.value)


def test_multi_kernels():
    ''' This tests extraction of kernels via an interface which needs more work'''
    _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
                                        "4.8_multikernel_invokes.f90"),
                           api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    schedule.view()
    # Define name of the kernel to extract
    kernel_name = "testkern_code"
    #schedule = Extractor.extract_kernel(schedule, kernel_name, 1)
    schedule = Extractor.extract_kernel(schedule, kernel_name)
    schedule.view()
    output = str(psy.gen)
    ##print(output)
    ###assert "random_numbers :: qr\n" in output


def test_omp_kernels():

    from psyclone.psyGen import Loop
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
    #print(output)
    assert "random_numbers :: qr\n" in output


def test_omp_kernels_invalid():

    from psyclone.psyGen import Loop
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

    #child = schedule.children[0]
    child = schedule.children[0].children[1] # - raises TransformationError 
    #child = schedule.children[0].children[1].children[0] # - raises TransformationError 
    schedule, _ = etrans.apply(child)

    # Define name of the kernel to extract - this brings up problems 
    # when there are two kernels with a same name in a parallel region,
    # as both have the same relative and absolute positions - this 
    # needs to be better programmed in Extractor.extract_kernel
    ##kernel_name = "testkern_w3_code"
    ##schedule = Extractor.extract_kernel(newschedule, kernel_name)
    schedule.view()
    output = str(psy.gen)
    #print(output)
    assert "random_numbers :: qr\n" in output


def test_colouring_kernels():

    from psyclone.transformations import Dynamo0p3ColourTrans

    ctrans = Dynamo0p3ColourTrans()
    etrans = ExtractRegionTrans()

    _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
                                        "4.8_multikernel_invokes.f90"),
                           api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    schedule.view()
    child = schedule.children[0]
    schedule, _ = ctrans.apply(child)
    child = schedule.children[0]
    #child = schedule.children[0].children[0] - raises TransformationError
    schedule, _ = etrans.apply(child)
    schedule.view()
    schedule.dag()
    output = str(psy.gen)
    ##print(output)
    assert "random_numbers :: qr\n" in output


def test_colouringandopenmp_kernels():

    from psyclone.psyGen import Loop
    from psyclone.dynamo0p3 import DISCONTINUOUS_FUNCTION_SPACES
    from psyclone.transformations import Dynamo0p3ColourTrans, \
        DynamoOMPParallelLoopTrans

    ctrans = Dynamo0p3ColourTrans()
    otrans = DynamoOMPParallelLoopTrans()

    _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
                                        "4.8_multikernel_invokes.f90"),
                           api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Colour all of the loops over cells unless they are on
    # discontinuous spaces (W3, WTHETA and W2V)
    cschedule = schedule
    for child in schedule.children:
        if isinstance(child, Loop) \
           and child.field_space.orig_name \
           not in DISCONTINUOUS_FUNCTION_SPACES \
           and child.iteration_space == "cells":
            cschedule, _ = ctrans.apply(child)

    # Then apply OpenMP to each of the colour loops
    schedule = cschedule
    schedule.view()
    for child in schedule.children:
        if isinstance(child, Loop):
            if child.loop_type == "colours":
                schedule, _ = otrans.apply(child.children[0])
            else:
                schedule, _ = otrans.apply(child)

    schedule.view()
    # Define name of the kernel to extract
    kernel_name = "testkern_code"
    oschedule = schedule
    schedule = Extractor.extract_kernel(oschedule, kernel_name)
    schedule.view()
    output = str(psy.gen)
    ##print(output)
    assert "random_numbers :: qr\n" in output

#def test_extract_view(capsys):
    #''' Test the view method in the ExtractNode class. '''
    #from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP
    #from psyclone import dynamo0p3
    #_, invoke_info = parse(os.path.join(BASE_PATH,
                                        #"15.9.1_X_innerproduct_Y_builtin.f90"),
                           #api="dynamo0.3")
    #psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    #psy.invokes.invoke_list[0].schedule.view()
    #output, _ = capsys.readouterr()
    #print(output)
    #expected_output = (colored("GlobalSum",
                               #SCHEDULE_COLOUR_MAP["GlobalSum"]) +
                       #"[scalar='asum']")
    #assert expected_output in output
    #gsum = None
    #for child in psy.invokes.invoke_list[0].schedule.children:
        #if isinstance(child, dynamo0p3.DynGlobalSum):
            #gsum = child
            #break
    #assert gsum
    #ret_str = super(dynamo0p3.DynGlobalSum, gsum).coloured_text
    #assert colored("GlobalSum", SCHEDULE_COLOUR_MAP["GlobalSum"]) in ret_str