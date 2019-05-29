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

''' Module containing specific tests for PSyclone DynamoExtractRegionTrans and
DynamoExtractNode.
'''

from __future__ import absolute_import

import os
import pytest

from dynamo0p3_build import Dynamo0p3Build
from psyclone.parse.algorithm import parse
from psyclone.extractor import ExtractNode
from psyclone.psyGen import PSyFactory, Loop
from psyclone.transformations import TransformationError, \
    DynamoExtractRegionTrans

# Paths to APIs
DYNAMO_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                "test_files", "dynamo0p3")
DYNAMO_API = "dynamo0.3"


def test_dynamoextract_node():
    ''' Test code generation for DynamoExtractNode '''

    etrans = DynamoExtractRegionTrans()

    _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
                                        "4.8_multikernel_invokes.f90"),
                           api=DYNAMO_API)
    # _, invoke_info = parse(
        # os.path.join(DYNAMO_BASE_PATH,
                     # "15.1.2_builtin_and_normal_kernel_invoke.f90"),
        # api=DYNAMO_API)
    psy = PSyFactory(DYNAMO_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
 
    schedule, _ = etrans.apply(schedule.children[2])
#    schedule, _ = etrans.apply(schedule.children[2:4])
    code = str(psy.gen)
    print(code)
    output = (
        "      ! ExtractStart\n"
        "      ! CALL write_extract_arguments(argument_list)\n"
        "      !\n"
        "      ! ExtractEnd\n")
    assert output in code


# def test_extract_colouring_omp():
    # ''' Test that extraction of a Kernel in an Invoke after applying
    # colouring and OpenMP optimisations produces the correct result
    # in Dynamo0.3 API. '''
    # from psyclone.transformations import Dynamo0p3ColourTrans, \
        # DynamoOMPParallelLoopTrans
    # from psyclone.dynamo0p3 import DISCONTINUOUS_FUNCTION_SPACES

    # etrans = DynamoExtractRegionTrans()
    # ctrans = Dynamo0p3ColourTrans()
    # otrans = DynamoOMPParallelLoopTrans()

    # _, invoke_info = parse(os.path.join(DYNAMO_BASE_PATH,
                                        # "4.8_multikernel_invokes.f90"),
                           # api=DYNAMO_API)
    # psy = PSyFactory(DYNAMO_API, distributed_memory=False).create(invoke_info)
    # invoke = psy.invokes.invoke_list[0]
    # schedule = invoke.schedule

    # # First colour all of the loops over cells unless they are on
    # # discontinuous spaces
    # cschedule = schedule
    # for child in schedule.children:
        # if isinstance(child, Loop) and child.field_space.orig_name \
           # not in DISCONTINUOUS_FUNCTION_SPACES \
           # and child.iteration_space == "cells":
            # cschedule, _ = ctrans.apply(child)
    # # Then apply OpenMP to each of the colour loops
    # schedule = cschedule
    # for child in schedule.children:
        # if isinstance(child, Loop):
            # if child.loop_type == "colours":
                # schedule, _ = otrans.apply(child.children[0])
            # else:
                # schedule, _ = otrans.apply(child)

    # # Extract the second instance of ru_kernel_type after colouring
    # # and OpenMP are applied
    # child = schedule.children[2]
    # schedule, _ = etrans.apply(child)

    # code = str(psy.gen)
    # print(code)
    # output = (
        # "      ! ExtractStart\n"
        # "      ! CALL write_extract_arguments(argument_list)\n"
        # "      !\n"
        # "      ! ExtractEnd\n")
    # assert output in code