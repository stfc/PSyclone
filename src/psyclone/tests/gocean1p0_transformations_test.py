# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2018, Science and Technology Facilities Council
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
# ----------------------------------------------------------------------------
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab
# Modified work Copyright (c) 2017 by J. Henrichs, Bureau of Meteorology

''' Module containing tests of Transformations when using the
    GOcean 1.0 API '''

from __future__ import absolute_import
import os
import re
import pytest
from psyclone.parse import parse
from psyclone.psyGen import PSyFactory, Loop
from psyclone.transformations import TransformationError, \
    GOConstLoopBoundsTrans, LoopFuseTrans, GOLoopSwapTrans, \
    OMPParallelTrans, GOceanOMPParallelLoopTrans, \
    GOceanOMPLoopTrans, KernelModuleInlineTrans, GOceanLoopFuseTrans, \
    ACCParallelTrans, ACCDataTrans, ACCLoopTrans
from psyclone.generator import GenerationError
from psyclone_test_utils import count_lines, get_invoke

# The version of the PSyclone API that the tests in this file
# exercise
API = "gocean1.0"


def test_const_loop_bounds_not_schedule():
    ''' Check that we raise an error if we attempt to apply the
    constant loop-bounds transformation to something that is
    not a Schedule '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           API, idx=0)
    schedule = invoke.schedule
    cbtrans = GOConstLoopBoundsTrans()

    with pytest.raises(TransformationError):
        _, _ = cbtrans.apply(schedule.children[0])


def test_const_loop_bounds_toggle():
    ''' Check that we can toggle constant loop bounds on and off and
    that the default behaviour is "on" '''
    psy, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                             API, idx=0)
    schedule = invoke.schedule
    cbtrans = GOConstLoopBoundsTrans()

    # First check that the generated code uses constant loop
    # bounds by default
    gen = str(psy.gen)

    assert "INTEGER istop, jstop" in gen
    assert "istop = cv_fld%grid%subdomain%internal%xstop" in gen
    assert "jstop = cv_fld%grid%subdomain%internal%ystop" in gen
    assert "DO j=2,jstop-1" in gen
    assert "DO i=2,istop" in gen

    # Next, check that applying the constant loop-bounds
    # transformation has no effect (in this case)
    newsched, _ = cbtrans.apply(schedule)
    invoke.schedule = newsched
    # Store the generated code as a string
    gen = str(psy.gen)

    assert "INTEGER istop, jstop" in gen
    assert "istop = cv_fld%grid%subdomain%internal%xstop" in gen
    assert "jstop = cv_fld%grid%subdomain%internal%ystop" in gen
    assert "DO j=2,jstop-1" in gen
    assert "DO i=2,istop" in gen

    # Finally, test that we can turn-off constant loop bounds
    newsched, _ = cbtrans.apply(schedule, const_bounds=False)
    invoke.schedule = newsched
    # Store the generated code as a string
    gen = str(psy.gen)

    assert "DO j=cv_fld%internal%ystart,cv_fld%internal%ystop" in gen
    assert "DO i=cv_fld%internal%xstart,cv_fld%internal%xstop" in gen
    assert "DO j=p_fld%whole%ystart,p_fld%whole%ystop" in gen
    assert "DO i=p_fld%whole%xstart,p_fld%whole%xstop" in gen


def test_const_loop_bounds_invalid_offset():
    ''' Test that we raise an appropriate error if we attempt to generate
    code with constant loop bounds for a kernel that expects an
    unsupported grid-offset '''
    psy, invoke = get_invoke("test26_const_bounds_invalid_offset.f90",
                             API, idx=0)
    cbtrans = GOConstLoopBoundsTrans()
    schedule = invoke.schedule
    newsched, _ = cbtrans.apply(schedule, const_bounds=True)
    invoke.schedule = newsched
    with pytest.raises(GenerationError):
        _ = psy.gen


def test_loop_fuse_different_iterates_over():
    ''' Test that an appropriate error is raised when we attempt to
    fuse two loops that have differing values of ITERATES_OVER '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           API, idx=0)
    schedule = invoke.schedule
    lftrans = LoopFuseTrans()
    cbtrans = GOConstLoopBoundsTrans()

    # Attempt to fuse two loops that are iterating over different
    # things
    with pytest.raises(TransformationError):
        _, _ = lftrans.apply(schedule.children[0],
                             schedule.children[1])

    # Turn off constant loop bounds (which should have no effect)
    # and repeat
    newsched, _ = cbtrans.apply(schedule, const_bounds=False)
    with pytest.raises(TransformationError):
        _, _ = lftrans.apply(newsched.children[0],
                             newsched.children[1])


def test_loop_fuse_unexpected_error():
    ''' Test that we catch an unexpected error when loop fusing '''
    _, invoke = get_invoke("test14_module_inline_same_kernel.f90", API, idx=0)
    schedule = invoke.schedule

    lftrans = GOceanLoopFuseTrans()

    # cause an unexpected error
    schedule.children[0].children = None

    # Attempt to fuse two loops that are iterating over different
    # things
    with pytest.raises(TransformationError) as excinfo:
        _, _ = lftrans.apply(schedule.children[0],
                             schedule.children[1])
    assert 'Unexpected exception' in str(excinfo.value)


def test_omp_parallel_loop():
    '''Test that we can generate an OMP PARALLEL DO correctly,
    independent of whether or not we are generating constant loop bounds '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    omp = GOceanOMPParallelLoopTrans()
    cbtrans = GOConstLoopBoundsTrans()
    omp_sched, _ = omp.apply(schedule.children[0])

    invoke.schedule = omp_sched
    gen = str(psy.gen)
    gen = gen.lower()
    expected = ("!$omp parallel do default(shared), private(j,i), "
                "schedule(static)\n"
                "      do j=2,jstop\n"
                "        do i=2,istop+1\n"
                "          call compute_cu_code(i, j, cu_fld%data, "
                "p_fld%data, u_fld%data)\n"
                "        end do \n"
                "      end do \n"
                "      !$omp end parallel do")
    assert expected in gen

    newsched, _ = cbtrans.apply(omp_sched, const_bounds=False)
    invoke.schedule = newsched
    gen = str(psy.gen)
    gen = gen.lower()
    expected = (
        "      !$omp parallel do default(shared), private(j,i), "
        "schedule(static)\n"
        "      do j=cu_fld%internal%ystart,cu_fld%internal%ystop\n"
        "        do i=cu_fld%internal%xstart,cu_fld%internal%xstop\n"
        "          call compute_cu_code(i, j, cu_fld%data, p_fld%data, "
        "u_fld%data)\n"
        "        end do \n"
        "      end do \n"
        "      !$omp end parallel do")
    assert expected in gen


def test_omp_region_with_wrong_arg_type():
    ''' Test that the OpenMP PARALLEL region transformation
        raises an appropriate error if passed something that is not
        a list of Nodes or a single Node. '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)

    ompr = OMPParallelTrans()

    with pytest.raises(TransformationError):
        _, _ = ompr.apply(invoke)


def test_omp_region_with_single_loop():
    ''' Test that we can pass the OpenMP PARALLEL region transformation
        a single node in a schedule '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    ompr = OMPParallelTrans()
    cbtrans = GOConstLoopBoundsTrans()

    omp_schedule, _ = ompr.apply(schedule.children[1])

    # Replace the original loop schedule with the transformed one
    invoke.schedule = omp_schedule
    # Store the results of applying this code transformation as
    # a string
    gen = str(psy.gen)
    gen = gen.lower()

    # Iterate over the lines of generated code
    within_omp_region = False
    call_count = 0
    for line in gen.split('\n'):
        if '!$omp parallel default' in line:
            within_omp_region = True
        if '!$omp end parallel' in line:
            within_omp_region = False
        if ' call ' in line and within_omp_region:
            call_count += 1

    assert call_count == 1

    # Repeat the test after turning off constant loop bounds
    newsched, _ = cbtrans.apply(omp_schedule, const_bounds=False)
    invoke.schedule = newsched
    gen = str(psy.gen)
    gen = gen.lower()
    within_omp_region = False
    call_count = 0
    for line in gen.split('\n'):
        if '!$omp parallel default' in line:
            within_omp_region = True
        if '!$omp end parallel' in line:
            within_omp_region = False
        if ' call ' in line and within_omp_region:
            call_count += 1

    assert call_count == 1


def test_omp_region_with_slice():
    ''' Test that we can pass the OpenMP PARALLEL region transformation
    a list of nodes specified as a slice '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    ompr = OMPParallelTrans()
    omp_schedule, _ = ompr.apply(schedule.children[1:])

    # Replace the original loop schedule with the transformed one
    invoke.schedule = omp_schedule
    # Store the results of applying this code transformation as
    # a string
    gen = str(psy.gen)
    gen = gen.lower()

    # Iterate over the lines of generated code
    within_omp_region = False
    call_count = 0
    for line in gen.split('\n'):
        if '!$omp parallel default' in line:
            within_omp_region = True
        if '!$omp end parallel' in line:
            within_omp_region = False
        if ' call ' in line and within_omp_region:
            call_count += 1

    assert call_count == 2


def test_omp_region_with_slice_change_order():
    ''' Test that the OpenMP transform does not allow to switch
    or duplicate child nodes.
    '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    code = str(psy.gen).replace("\n", "")

    # This is the correct ordering of the kernel calls:
    correct_re = ("call compute_cu_code.*"
                  "call compute_cv_code.*"
                  "call time_smooth_code.*")

    # Make sure that the test case still has the expected
    # order of kernel calls:
    assert re.search(correct_re, code, re.I)

    # Now apply the transform, but reverse the child nodes:
    # -----------------------------------------------------
    ompr = OMPParallelTrans()

    # Note that the order of the nodes is reversed, which
    # could result in changing the order of operations:
    with pytest.raises(TransformationError) as err:
        ompr.apply([schedule.children[2], schedule.children[1]])
    assert "Children are not consecutive children of one parent" in str(err)

    # Also test the case of duplicated children:
    # ------------------------------------------
    with pytest.raises(TransformationError) as err:
        ompr.apply([schedule.children[0], schedule.children[0]])
    assert "Children are not consecutive children of one parent" in str(err)


def test_omp_region_no_slice():
    ''' Test that we can pass the OpenMP PARALLEL region transformation
    a list of nodes specified as node.children '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule
    ompr = OMPParallelTrans()

    omp_schedule, _ = ompr.apply(schedule.children)
    # Replace the original loop schedule with the transformed one
    invoke.schedule = omp_schedule
    # Store the results of applying this code transformation as
    # a string
    gen = str(psy.gen)
    gen = gen.lower()
    # Iterate over the lines of generated code
    within_omp_region = False
    call_count = 0
    for line in gen.split('\n'):
        if '!$omp parallel default' in line:
            within_omp_region = True
        if '!$omp end parallel' in line:
            within_omp_region = False
        if ' call ' in line and within_omp_region:
            call_count += 1
    assert call_count == 3


def test_omp_region_no_slice_no_const_bounds():
    ''' Test that we generate the correct code when we apply an OpenMP
    PARALLEL region transformation to a list of nodes when the Schedule
    has been transformed to use loop-bound look-ups '''

    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule
    ompr = OMPParallelTrans()
    cbtrans = GOConstLoopBoundsTrans()

    newsched, _ = cbtrans.apply(schedule, const_bounds=False)
    omp_schedule, _ = ompr.apply(newsched.children)
    # Replace the original loop schedule with the transformed one
    invoke.schedule = omp_schedule
    # Store the results of applying this code transformation as
    # a string
    gen = str(psy.gen)
    gen = gen.lower()
    # Iterate over the lines of generated code
    within_omp_region = False
    call_count = 0
    for line in gen.split('\n'):
        if '!$omp parallel default' in line:
            within_omp_region = True
        if '!$omp end parallel' in line:
            within_omp_region = False
        if ' call ' in line and within_omp_region:
            call_count += 1
    assert call_count == 3


def test_omp_region_retains_kernel_order1():
    ''' Test that applying the OpenMP PARALLEL region transformation
    to a sub-set of nodes (last 2 of three) does not change their
    ordering '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    ompr = OMPParallelTrans()
    cbtrans = GOConstLoopBoundsTrans()

    omp_schedule, _ = ompr.apply(schedule.children[1:])

    # Replace the original loop schedule with the transformed one
    invoke.schedule = omp_schedule
    # Store the results of applying this code transformation as
    # a string
    gen = str(psy.gen)
    gen = gen.lower()
    # Iterate over the lines of generated code
    cu_idx = -1
    cv_idx = -1
    ts_idx = -1
    for idx, line in enumerate(gen.split('\n')):
        if 'call compute_cu' in line:
            cu_idx = idx
        if 'call compute_cv' in line:
            cv_idx = idx
        if 'call time_smooth' in line:
            ts_idx = idx

    # Kernels should be in order {compute_cu, compute_cv, time_smooth}
    assert cu_idx < cv_idx and cv_idx < ts_idx

    # Repeat after turning off constant loop bounds
    newsched, _ = cbtrans.apply(omp_schedule, const_bounds=False)
    invoke.schedule = newsched
    gen = str(psy.gen)
    gen = gen.lower()
    # Iterate over the lines of generated code
    cu_idx = -1
    cv_idx = -1
    ts_idx = -1
    for idx, line in enumerate(gen.split('\n')):
        if 'call compute_cu' in line:
            cu_idx = idx
        if 'call compute_cv' in line:
            cv_idx = idx
        if 'call time_smooth' in line:
            ts_idx = idx

    # Kernels should be in order {compute_cu, compute_cv, time_smooth}
    assert cu_idx < cv_idx and cv_idx < ts_idx


def test_omp_region_retains_kernel_order2():
    ''' Test that applying the OpenMP PARALLEL region transformation
    to a sub-set of nodes (first 2 of 3) does not change their
    ordering '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    ompr = OMPParallelTrans()

    omp_schedule, _ = ompr.apply(schedule.children[0:2])

    # Replace the original loop schedule with the transformed one
    invoke.schedule = omp_schedule
    # Store the results of applying this code transformation as
    # a string
    gen = str(psy.gen)
    gen = gen.lower()

    # Iterate over the lines of generated code
    cu_idx = -1
    cv_idx = -1
    ts_idx = -1
    for idx, line in enumerate(gen.split('\n')):
        if 'call compute_cu' in line:
            cu_idx = idx
        if 'call compute_cv' in line:
            cv_idx = idx
        if 'call time_smooth' in line:
            ts_idx = idx

    # Kernels should be in order {compute_cu, compute_cv, time_smooth}
    assert cu_idx < cv_idx and cv_idx < ts_idx


def test_omp_region_retains_kernel_order3():
    ''' Test that applying the OpenMP PARALLEL region transformation
    to a sub-set of nodes (middle 1 of 3) does not change their
    ordering '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    ompr = OMPParallelTrans()
    ompl = GOceanOMPLoopTrans()

    # Put an OMP Do around the 2nd loop of the schedule
    omp_schedule, _ = ompl.apply(schedule.children[1])

    # Put an OMP Parallel around that single OMP Do
    schedule, _ = ompr.apply([omp_schedule.children[1]])

    # Replace the original loop schedule with the transformed one
    invoke.schedule = schedule
    # Store the results of applying this code transformation as
    # a string
    gen = str(psy.gen)
    gen = gen.lower()

    # Iterate over the lines of generated code
    cu_idx = -1
    cv_idx = -1
    ts_idx = -1
    for idx, line in enumerate(gen.split('\n')):
        if 'call compute_cu' in line:
            cu_idx = idx
        if 'call compute_cv' in line:
            cv_idx = idx
        if 'call time_smooth' in line:
            ts_idx = idx

    # Kernels should be in order {compute_cu, compute_cv, time_smooth}
    assert cu_idx < cv_idx and cv_idx < ts_idx


def test_omp_region_before_loops_trans():
    ''' Test of the OpenMP PARALLEL region transformation where
    we do the region transformation before the loop
    transformations. '''
    psy, invoke = get_invoke("single_invoke_two_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    # Put all of the loops in the schedule within a single
    # OpenMP region
    ompr = OMPParallelTrans()
    omp_schedule, _ = ompr.apply(schedule.children)

    # Put an OpenMP do directive around each loop contained
    # in the region
    ompl = GOceanOMPLoopTrans()
    for child in omp_schedule.children[0].children:
        schedule, _ = ompl.apply(child)
        omp_schedule = schedule

    # Replace the original loop schedule with the transformed one
    invoke.schedule = omp_schedule

    # Store the results of applying this code transformation as
    # a string
    gen = str(psy.gen)

    # Iterate over the lines of generated code
    omp_region_idx = -1
    omp_do_idx = -1
    for idx, line in enumerate(gen.split('\n')):
        if '!$omp parallel default' in line:
            omp_region_idx = idx
        if '!$omp do' in line:
            omp_do_idx = idx
        if 'DO j=' in line:
            break

    assert omp_region_idx != -1
    assert omp_do_idx != -1
    assert omp_do_idx - omp_region_idx == 1


def test_omp_region_after_loops_trans():
    ''' Test of the OpenMP PARALLEL region transformation where we
    do the loop transformations before the region transformation '''
    psy, invoke = get_invoke("single_invoke_two_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    # Put an OpenMP do directive around each loop contained
    # in the schedule
    ompl = GOceanOMPLoopTrans()
    for child in schedule.children:
        omp_schedule, _ = ompl.apply(child)

    # Now put an OpenMP parallel region around that set of
    # loops
    ompr = OMPParallelTrans()
    schedule, _ = ompr.apply(omp_schedule.children)

    # Replace the original loop schedule with the transformed one
    invoke.schedule = schedule

    # Store the results of applying this code transformation as
    # a string
    gen = str(psy.gen)

    # Iterate over the lines of generated code
    omp_region_idx = -1
    omp_do_idx = -1
    for idx, line in enumerate(gen.split('\n')):
        if '!$omp parallel default' in line:
            omp_region_idx = idx
        if '!$omp do' in line:
            omp_do_idx = idx
        if 'DO j=' in line:
            break

    assert omp_region_idx != -1
    assert omp_do_idx != -1
    assert omp_do_idx - omp_region_idx == 1


def test_omp_region_commutes_with_loop_trans():
    ''' Test that the OpenMP PARALLEL region and (orphan) loop
    transformations commute - i.e. we get the same result
    independent of the order in which they are applied. '''
    psy, invoke = get_invoke("single_invoke_two_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    # Put an OpenMP do directive around each loop contained
    # in the schedule
    ompl = GOceanOMPLoopTrans()
    for child in schedule.children:
        omp_schedule, _ = ompl.apply(child)

    # Now put an OpenMP parallel region around that set of
    # loops
    ompr = OMPParallelTrans()
    schedule, _ = ompr.apply(omp_schedule.children)

    # Replace the original loop schedule with the transformed one
    invoke.schedule = schedule

    # Store the results of applying this code transformation as
    # a string
    loop_before_region_gen = str(psy.gen)

    # Now we do it again but in the opposite order...

    # Put all of the loops in the schedule within a single
    # OpenMP region
    psy, invoke = get_invoke("single_invoke_two_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    ompr = OMPParallelTrans()
    omp_schedule, _ = ompr.apply(schedule.children)

    # Put an OpenMP do directive around each loop contained
    # in the region
    ompl = GOceanOMPLoopTrans()
    for child in omp_schedule.children[0].children:
        schedule, _ = ompl.apply(child)
        omp_schedule = schedule

    # Replace the original loop schedule with the transformed one
    invoke.schedule = omp_schedule

    # Store the results of applying this code transformation as
    # a string
    region_before_loop_gen = str(psy.gen)

    assert region_before_loop_gen == loop_before_region_gen


def test_omp_region_commutes_with_loop_trans_bounds_lookup():
    ''' Test that the OpenMP PARALLEL region and (orphan) loop
    transformations commute after constant bounds have been
    switched off - i.e. we get the same result
    independent of the order in which they are applied. '''
    psy, invoke = get_invoke("single_invoke_two_kernels.f90", API, idx=0)
    schedule = invoke.schedule
    # Turn-off constant loop bounds
    cbtrans = GOConstLoopBoundsTrans()
    newsched, _ = cbtrans.apply(schedule, const_bounds=False)

    # Put an OpenMP do directive around each loop contained
    # in the schedule
    ompl = GOceanOMPLoopTrans()
    for child in newsched.children:
        omp_schedule, _ = ompl.apply(child)

    # Now put an OpenMP parallel region around that set of
    # loops
    ompr = OMPParallelTrans()
    schedule, _ = ompr.apply(omp_schedule.children)

    # Replace the original loop schedule with the transformed one
    invoke.schedule = schedule

    # Store the results of applying this code transformation as
    # a string
    loop_before_region_gen = str(psy.gen)

    # Now we do it again but in the opposite order...
    # ...we re-generate the original schedule here rather than
    # keeping a (deep) copy of it from earlier as that can
    # cause resource problems.
    psy, invoke = get_invoke("single_invoke_two_kernels.f90", API, idx=0)
    schedule = invoke.schedule
    # Turn-off constant loop bounds
    cbtrans = GOConstLoopBoundsTrans()
    schedule, _ = cbtrans.apply(schedule, const_bounds=False)

    # Put all of the loops in the schedule within a single
    # OpenMP region
    ompr = OMPParallelTrans()
    omp_schedule, _ = ompr.apply(schedule.children)

    # Put an OpenMP do directive around each loop contained
    # in the region
    ompl = GOceanOMPLoopTrans()
    for child in omp_schedule.children[0].children:
        schedule, _ = ompl.apply(child)
        omp_schedule = schedule

    # Replace the original loop schedule with the transformed one
    invoke.schedule = omp_schedule

    # Store the results of applying this code transformation as
    # a string
    region_before_loop_gen = str(psy.gen)

    assert region_before_loop_gen == loop_before_region_gen


def test_omp_region_nodes_not_children_of_same_parent():
    ''' Test that we raise appropriate error if user attempts
    to put a region around nodes that are not children of
    the same parent '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    ompl = GOceanOMPParallelLoopTrans()
    ompr = OMPParallelTrans()

    # Put an OpenMP parallel do around the first loop in the schedule
    _, _ = ompl.apply(schedule.children[0])

    # Attempt to put an OpenMP parallel region around that same loop
    # (which is now a child of an OpenMP loop directive) and the
    # second loop in the schedule
    with pytest.raises(TransformationError):
        _, _ = ompr.apply([schedule.children[0].children[0],
                           schedule.children[1]])


def test_omp_region_nodes_not_children_of_same_schedule():
    ''' Test that we raise appropriate error if user attempts
    to put a region around nodes that are not children of
    the same schedule '''
    alg_file = "test12_two_invokes_two_kernels.f90"
    _, invoke1 = get_invoke(alg_file, API, 0)
    schedule1 = invoke1.schedule
    _, invoke2 = get_invoke(alg_file, API, 1)
    schedule2 = invoke2.schedule

    ompr = OMPParallelTrans()

    # Attempt to put an OpenMP parallel region the loops from the
    # two different schedules
    with pytest.raises(TransformationError):
        _, _ = ompr.apply([schedule1.children[0],
                           schedule2.children[0]])


def test_omp_loop_outside_region():
    ''' Test that a generation error is raised if we try and
    have an orphaned OpenMP loop that is not enclosed
    within a parallel region '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    # Put an OpenMP do directive around each loop contained
    # in the schedule
    ompl = GOceanOMPLoopTrans()
    ompr = OMPParallelTrans()

    for child in schedule.children:
        omp_schedule, _ = ompl.apply(child)

    # Now enclose all but the last loop in a parallel region
    ompr_schedule, _ = ompr.apply(omp_schedule.children[0:-2])

    # Replace the original loop schedule with the transformed one
    invoke.schedule = ompr_schedule

    # Attempt to generate the transformed code
    with pytest.raises(GenerationError):
        _ = psy.gen


def test_omp_loop_applied_to_non_loop():
    ''' Test that we raise a TransformationError if we attempt
    to apply an OMP DO transformation to something that
    is not a loop '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    from psyclone.transformations import OMPLoopTrans
    ompl = OMPLoopTrans()
    omp_schedule, _ = ompl.apply(schedule.children[0])

    # Attempt to (erroneously) apply the OMP Loop transformation
    # to the first node in the schedule (which is now itself an
    # OMP Loop transformation)
    with pytest.raises(TransformationError):
        _, _ = ompl.apply(omp_schedule.children[0])


def test_go_omp_loop_applied_to_non_loop():
    ''' Test that we raise a TransformationError if we attempt
    to apply a GOcean OMP DO transformation to something that
    is not a loop '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    ompl = GOceanOMPLoopTrans()
    omp_schedule, _ = ompl.apply(schedule.children[0])

    # Attempt to (erroneously) apply the GO OMP Loop transformation
    # to the first node in the schedule (which is now itself an
    # OMP Loop transformation)
    with pytest.raises(TransformationError):
        _, _ = ompl.apply(omp_schedule.children[0])


def test_go_omp_loop_applied_to_wrong_loop_type():
    ''' Test that we raise a TransformationError if we attempt to
    apply a GOcean OMP  DO transformation to a loop of
    the wrong type '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    # Manually break the loop-type of the first loop in order to
    # test that this error is handled. We have to work-around
    # the setter method to do this since it has error checking
    # too!
    schedule.children[0]._loop_type = "wrong"

    ompl = GOceanOMPLoopTrans()
    # Attempt to apply the transformation to the loop that has been
    # given an incorrect type
    with pytest.raises(TransformationError):
        _, _ = ompl.apply(schedule.children[0])


def test_go_omp_parallel_loop_applied_to_non_loop():
    ''' Test that we raise a TransformationError if we attempt to
    apply a GOcean OMP Parallel DO transformation to something that
    is not a loop '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    ompl = GOceanOMPParallelLoopTrans()
    omp_schedule, _ = ompl.apply(schedule.children[0])

    # Attempt to (erroneously) apply the OMP Loop transformation
    # to the first node in the schedule (which is now itself an
    # OMP Loop transformation)
    with pytest.raises(TransformationError):
        _, _ = ompl.apply(omp_schedule.children[0])


def test_go_omp_parallel_loop_applied_to_wrong_loop_type():
    ''' Test that we raise a TransformationError if we attempt to
    apply a GOcean OMP Parallel DO transformation to a loop of
    the wrong type '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    # Manually break the loop-type of the first loop in order to
    # test that this error is handled. We have to work-around
    # the setter method to do this since it has error checking
    # too!
    schedule.children[0]._loop_type = "wrong"

    ompl = GOceanOMPParallelLoopTrans()
    # Attempt to apply the transformation to the loop that has been
    # given an incorrect type
    with pytest.raises(TransformationError):
        _, _ = ompl.apply(schedule.children[0])


def test_omp_parallel_do_inside_parallel_region():
    ''' Test that a generation error is raised if we attempt
    to have an OpenMP parallel do within an OpenMP
    parallel region '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    ompl = GOceanOMPParallelLoopTrans()
    ompr = OMPParallelTrans()

    # Put an OpenMP parallel do directive around all of the loops
    for child in schedule.children:
        omp_schedule, _ = ompl.apply(child)

    # Now enclose all of the children within a parallel region
    schedule, _ = ompr.apply(omp_schedule.children)

    # Replace the original loop schedule with the transformed one
    invoke.schedule = schedule

    # Attempt to generate the transformed code
    with pytest.raises(GenerationError):
        _ = psy.gen


def test_omp_parallel_region_inside_parallel_do():
    ''' Test that a generation error is raised if we attempt
    to have an OpenMP parallel region within an OpenMP
    parallel do (with the latter applied first) '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    ompl = GOceanOMPParallelLoopTrans()
    ompr = OMPParallelTrans()

    # Put an OpenMP parallel do directive around one of the loops
    _, _ = ompl.apply(schedule.children[1])

    # Now attempt to put a parallel region inside that parallel do
    with pytest.raises(TransformationError):
        _, _ = ompr.apply([schedule.children[1].children[0]])


def test_omp_parallel_do_around_parallel_region():
    ''' Test that a generation error is raised if we attempt
    to have an OpenMP parallel region around an OpenMP
    parallel do (with the latter applied second) '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    ompl = GOceanOMPParallelLoopTrans()
    ompr = OMPParallelTrans()

    # Put a parallel region around two of the loops
    omp_schedule, _ = ompr.apply(schedule.children[0:2])

    # Put an OpenMP parallel do directive around one of those loops
    # (which is now a child of the region directive)
    schedule, _ = ompl.apply(omp_schedule.children[0].children[0])

    # Replace the original loop schedule with the transformed one
    invoke.schedule = schedule

    # Attempt to generate the transformed code
    with pytest.raises(GenerationError):
        _ = psy.gen


@pytest.mark.xfail(reason="OMP Region with children of different types "
                   "not yet implemented")
def test_omp_region_with_children_of_different_types():
    ''' Test that we can generate code if we have an
    OpenMP parallel region enclosing children of different types. '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    ompl = GOceanOMPLoopTrans()
    ompr = OMPParallelTrans()

    # Put an OpenMP do directive around one loop
    omp_schedule, _ = ompl.apply(schedule.children[1])

    # Now enclose all of the children within a parallel region
    schedule, _ = ompr.apply(omp_schedule.children)

    # Replace the original loop schedule with the transformed one
    invoke.schedule = schedule

    # Attempt to generate the transformed code
    _ = psy.gen


def test_omp_schedule_default_static():
    ''' Test that if no OMP schedule is specified then we default
    to "static" '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    ompl = GOceanOMPLoopTrans()
    ompr = OMPParallelTrans()

    # Put an OpenMP do directive around one loop without specifying
    # the OMP schedule to use
    omp_schedule, _ = ompl.apply(schedule.children[1])

    # Now enclose it within a parallel region
    schedule, _ = ompr.apply(omp_schedule.children[1])

    # Replace the original loop schedule with the transformed one
    invoke.schedule = schedule

    # Attempt to generate the transformed code
    gen = str(psy.gen)

    assert '!$omp do schedule(static)' in gen


def test_omp_do_schedule_runtime():
    ''' Test that we can specify the schedule of an OMP do as
    "runtime" '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    ompl = GOceanOMPLoopTrans(omp_schedule="runtime")
    ompr = OMPParallelTrans()

    # Put an OpenMP do directive around one loop
    omp_schedule, _ = ompl.apply(schedule.children[1])

    # Now enclose it within a parallel region
    schedule, _ = ompr.apply(omp_schedule.children[1])

    # Replace the original loop schedule with the transformed one
    invoke.schedule = schedule

    # Attempt to generate the transformed code
    gen = str(psy.gen)

    assert '!$omp do schedule(runtime)' in gen


def test_omp_do_schedule_dynamic():
    ''' Test that we can specify the schedule of an OMP do as
    "dynamic" '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    ompl = GOceanOMPLoopTrans(omp_schedule="dynamic")
    ompr = OMPParallelTrans()

    # Put an OpenMP do directive around one loop
    omp_schedule, _ = ompl.apply(schedule.children[1])

    # Now enclose it within a parallel region
    schedule, _ = ompr.apply(omp_schedule.children[1])

    # Replace the original loop schedule with the transformed one
    invoke.schedule = schedule

    # Attempt to generate the transformed code
    gen = str(psy.gen)

    assert '!$omp do schedule(dynamic)' in gen


def test_omp_do_schedule_guided():
    ''' Test that we can specify the schedule of an OMP do as
    "guided" '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    ompl = GOceanOMPLoopTrans(omp_schedule="guided")
    ompr = OMPParallelTrans()

    # Put an OpenMP do directive around one loop
    omp_schedule, _ = ompl.apply(schedule.children[1])

    # Now enclose it within a parallel region
    schedule, _ = ompr.apply(omp_schedule.children[1])

    # Replace the original loop schedule with the transformed one
    invoke.schedule = schedule

    # Attempt to generate the transformed code
    gen = str(psy.gen)

    assert '!$omp do schedule(guided)' in gen


def test_omp_schedule_guided_with_empty_chunk():
    ''' Test that we raise an appropriate error if we miss off
    the chunksize '''
    with pytest.raises(TransformationError):
        _ = GOceanOMPLoopTrans(omp_schedule="guided, ")


def test_omp_schedule_guided_with_chunk():
    ''' Test that we can specify the schedule of an OMP do as
    "guided,n" where n is some chunk size'''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    ompl = GOceanOMPLoopTrans(omp_schedule="guided,10")
    ompr = OMPParallelTrans()

    # Put an OpenMP do directive around one loop
    omp_schedule, _ = ompl.apply(schedule.children[1])

    # Now enclose it within a parallel region
    schedule, _ = ompr.apply(omp_schedule.children[1])

    # Replace the original loop schedule with the transformed one
    invoke.schedule = schedule

    # Attempt to generate the transformed code
    gen = str(psy.gen)

    assert '!$omp do schedule(guided,10)' in gen


def test_omp_invalid_schedule():
    ''' Test that we raise an appropriate error if we specify
    an invalid omp schedule '''
    with pytest.raises(TransformationError):
        _ = GOceanOMPLoopTrans(omp_schedule="rubbish")


def test_omp_schedule_auto_with_chunk():
    ''' Test that we raise an appropriate error if we specify
    the omp schedule as "auto" but try to provide a chunk size '''
    with pytest.raises(TransformationError):
        _ = GOceanOMPLoopTrans(omp_schedule="auto,4")


def test_module_noinline_default():
    ''' Test that by default there is no module inlining '''
    psy, _ = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    gen = str(psy.gen)
    # check that the subroutine has not been inlined
    assert 'SUBROUTINE compute_cu_code(i, j, cu, p, u)' not in gen
    # check that the associated use exists (as this is removed when
    # inlining)
    assert 'USE compute_cu_mod, ONLY: compute_cu_code' in gen


def test_module_inline():
    ''' Test that we can succesfully inline a basic kernel subroutine
    routine into the PSy layer module by directly setting inline to
    true for the specified kernel. '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule
    kern_call = schedule.children[0].children[0].children[0]
    kern_call.module_inline = True
    gen = str(psy.gen)
    # check that the subroutine has been inlined correctly
    expected = (
        "    END SUBROUTINE invoke_0\n"
        "    SUBROUTINE compute_cu_code(i, j, cu, p, u)\n")
    assert expected in gen
    # check that the associated use no longer exists
    assert 'USE compute_cu_mod, ONLY: compute_cu_code' not in gen


def test_module_inline_with_transformation():
    ''' Test that we can succesfully inline a basic kernel subroutine
    routine into the PSy layer module using a transformation '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule
    kern_call = schedule.children[1].children[0].children[0]
    inline_trans = KernelModuleInlineTrans()
    schedule, _ = inline_trans.apply(kern_call)
    gen = str(psy.gen)
    # check that the subroutine has been inlined
    assert 'SUBROUTINE compute_cv_code(i, j, cv, p, v)' in gen
    # check that the associated use no longer exists
    assert 'USE compute_cv_mod, ONLY: compute_cv_code' not in gen


def test_module_no_inline_with_transformation():
    ''' Test that we can switch off the inlining of a kernel routine
    into the PSy layer module using a transformation. Relies on the
    test_module_inline() test being successful to be a valid test. '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule
    kern_call = schedule.children[0].children[0].children[0]
    # directly switch on inlining
    kern_call.module_inline = True
    inline_trans = KernelModuleInlineTrans()
    # use a transformation to switch inlining off again
    schedule, _ = inline_trans.apply(kern_call, inline=False)
    gen = str(psy.gen)
    # check that the subroutine has not been inlined
    assert 'SUBROUTINE compute_cu_code(i, j, cu, p, u)' not in gen
    # check that the associated use exists (as this is removed when
    # inlining)
    assert 'USE compute_cu_mod, ONLY: compute_cu_code' in gen


# we can not test if someone accidentally sets module_inline to True
# to an object that is not a Kernel as Python allows one to
# dynamically add new variables to an object. Therefore an error is
# never thrown. This would be testable if "inline" were a function.
# def test_inline_error_if_not_kernel():


def test_transformation_inline_error_if_not_kernel():
    ''' Test that the inline transformation fails if the object being
    passed is not a kernel'''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule
    kern_call = schedule.children[0].children[0]
    inline_trans = KernelModuleInlineTrans()
    with pytest.raises(TransformationError):
        _, _ = inline_trans.apply(kern_call)


def test_module_inline_with_sub_use():
    ''' Test that we can module inline a kernel subroutine which
    contains a use statement'''
    psy, invoke = get_invoke("single_invoke_scalar_int_arg.f90", API, idx=0)
    schedule = invoke.schedule
    kern_call = schedule.children[0].children[0].children[0]
    inline_trans = KernelModuleInlineTrans()
    schedule, _ = inline_trans.apply(kern_call)
    gen = str(psy.gen)
    # check that the subroutine has been inlined
    assert 'SUBROUTINE bc_ssh_code(ji, jj, istep, ssha, tmask)' in gen
    # check that the use within the subroutine exists
    assert 'USE model_mod, ONLY: rdt' in gen
    # check that the associated psy use does not exist
    assert 'USE bc_ssh_mod, ONLY: bc_ssh_code' not in gen


def test_module_inline_same_kernel():
    '''Tests that correct results are obtained when an invoke that uses
    the same kernel subroutine more than once has that kernel
    inlined'''
    psy, invoke = get_invoke("test14_module_inline_same_kernel.f90", API,
                             idx=0)
    schedule = invoke.schedule
    kern_call = schedule.children[0].children[0].children[0]
    inline_trans = KernelModuleInlineTrans()
    _, _ = inline_trans.apply(kern_call)
    gen = str(psy.gen)
    # check that the subroutine has been inlined
    assert 'SUBROUTINE time_smooth_code(' in gen
    # check that the associated psy "use" does not exist
    assert 'USE time_smooth_mod, ONLY: time_smooth_code' not in gen
    # check that the subroutine has only been inlined once
    count = count_lines(psy.gen, "SUBROUTINE time_smooth_code(")
    assert count == 1, "Expecting subroutine to be inlined once"


def test_module_inline_warning_no_change():
    ''' test of the warning clause in the Kernel transformation when
    no change is made to the inlining of a Kernel i.e. the inlining
    request is already what is happening. No warning is currently made
    as we have not added logging to the code but this test covers the
    clause '''
    _, invoke = get_invoke("test14_module_inline_same_kernel.f90", API, idx=0)
    schedule = invoke.schedule
    kern_call = schedule.children[0].children[0].children[0]
    inline_trans = KernelModuleInlineTrans()
    _, _ = inline_trans.apply(kern_call, inline=False)


def test_loop_swap_correct():
    ''' Testing correct loop swapping transform. Esp. try first, middle, and
    last invokes to make sure the inserting of the inner loop happens at
    the right place.'''

    psy, _ = get_invoke("test27_loop_swap.f90", API, idx=0)
    invoke = psy.invokes.get("invoke_loop1")
    schedule = invoke.schedule
    schedule_str = str(schedule)

    # First make sure to throw an early error if the source file
    # test27_loop_swap.f90 should have been changed
    expected_schedule = '''Loop[]: j= lower=2,jstop,1
Loop[]: i= lower=2,istop,1
kern call: bc_ssh_code'''
    assert expected_schedule in schedule_str

    expected_schedule = '''Loop[]: j= lower=1,jstop+1,1
Loop[]: i= lower=1,istop,1
kern call: bc_solid_u_code'''
    assert expected_schedule in schedule_str

    expected_schedule = '''Loop[]: j= lower=1,jstop,1
Loop[]: i= lower=1,istop+1,1
kern call: bc_solid_v_code'''
    assert expected_schedule in schedule_str

    # Now swap the first loops
    swap = GOLoopSwapTrans()
    swapped1, _ = swap.apply(schedule.children[0])
    psy.invokes.get('invoke_loop1').schedule = swapped1
    schedule_str = str(swapped1)

    expected_schedule = '''Loop[]: i= lower=2,istop,1
Loop[]: j= lower=2,jstop,1
kern call: bc_ssh_code'''
    assert expected_schedule in schedule_str

    # Now swap the middle loops
    swapped2, _ = swap.apply(swapped1.children[1])
    psy.invokes.get('invoke_loop1').schedule = swapped2
    schedule_str = str(swapped2)

    expected_schedule = '''Loop[]: i= lower=1,istop,1
Loop[]: j= lower=1,jstop+1,1
kern call: bc_solid_u_code'''
    assert expected_schedule in schedule_str

    # Now swap the last loops
    swapped3, _ = swap.apply(swapped2.children[2])
    psy.invokes.get('invoke_loop1').schedule = swapped3
    schedule_str = str(swapped3)

    expected_schedule = '''Loop[]: i= lower=1,istop+1,1
Loop[]: j= lower=1,jstop,1
kern call: bc_solid_v_code'''
    assert expected_schedule in schedule_str
    return


def test_go_loop_swap_errors():
    ''' Test loop swapping transform with incorrect parameters. '''

    psy, invoke_loop1 = get_invoke("test27_loop_swap.f90", API, idx=1)

    schedule = invoke_loop1.schedule
    swap = GOLoopSwapTrans()
    assert swap.name == "GOLoopSwap"
    assert str(swap) == "Exchange the order of two nested loops: inner "\
        "becomes outer and vice versa"

    # Test error if given node is not the outer loop of at least
    # a double nested loop:
    with pytest.raises(TransformationError) as error:
        swap.apply(schedule.children[0].children[0])
    assert re.search("Supplied node .* must be the outer loop of a loop nest "
                     "but the first inner statement is not a loop, got .*",
                     str(error.value)) is not None

    # Not a loop: use the cal to bc_ssh_code node as example for this test:
    with pytest.raises(TransformationError) as error:
        swap.apply(schedule.children[0].children[0].children[0])
    assert "Given node 'kern call: bc_ssh_code' is not a loop" in \
        str(error.value)

    # Now create an outer loop with more than one inner statement
    # ... by fusing the first and second outer loops :(
    invoke_loop2 = psy.invokes.get("invoke_loop2")
    schedule = invoke_loop2.schedule

    fuse = GOceanLoopFuseTrans()
    fused, _ = fuse.apply(schedule.children[0], schedule.children[1])
    invoke_loop2.schedule = fused

    with pytest.raises(TransformationError) as error:
        swap.apply(fused.children[0])
    assert re.search("Supplied node .* must be the outer loop of a loop nest "
                     "and must have exactly one inner loop, but this node "
                     "has 2 inner statements, the first two being .* and .*",
                     str(error.value)) is not None

    # Now remove the body of the first inner loop, and pass the first
    # inner loop --> i.e. a loop with an empty body
    del fused.children[0].children[0].children[0]

    with pytest.raises(TransformationError) as error:
        swap.apply(fused.children[0].children[0])
    assert re.search("Supplied node .* must be the outer loop of a loop nest "
                     "and must have one inner loop, but this node does not "
                     "have any statements inside.",
                     str(error.value)) is not None

    # Check if a non gocean1p0 API raises an error
    _, info = parse(os.path.
                    join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3",
                         "1.0.1_single_named_invoke.f90"),
                    api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(info)
    invokes = psy.invokes
    invoke = invokes.get(list(invokes.names)[0])
    with pytest.raises(TransformationError) as error:
        swap.apply(invoke.schedule.children[3])

    assert re.search("Given node .* is not a GOLoop, "
                     "but an instance of .*DynLoop",
                     str(error.value)) is not None


def test_acc_parallel_not_a_loop():
    ''' Test that we raise an appropriate error if we attempt
    to apply the OpenACC Parallel transformation to something that
    is not a loop '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    acct = ACCParallelTrans()
    # Attempt to (erroneously) apply the OpenACC Parallel transformation
    # to the schedule rather than a loop
    with pytest.raises(TransformationError):
        _, _ = acct.apply(schedule)


def test_acc_parallel_trans():
    ''' Test that we can apply an OpenACC parallel transformation
    to a loop '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    acct = ACCParallelTrans()
    # Apply the OpenACC Parallel transformation
    # to the first loop of the schedule
    new_sched, _ = acct.apply(schedule.children[0])
    invoke.schedule = new_sched

    with pytest.raises(GenerationError) as err:
        _ = str(psy.gen)
    assert ("an ACC parallel region must also contain an ACC enter data "
            "directive but none was found for invoke_0" in str(err))

    accdt = ACCDataTrans()
    new_sched, _ = accdt.apply(schedule)
    invoke.schedule = new_sched
    code = str(psy.gen)

    acc_idx = -1
    acc_end_idx = -1
    do_idx = -1
    for idx, line in enumerate(code.split('\n')):
        if "!$acc parallel default(present)" in line:
            acc_idx = idx
        if (do_idx == -1) and "DO j" in line:
            do_idx = idx
        if "!$acc end parallel" in line:
            acc_end_idx = idx

    assert acc_idx != -1 and acc_end_idx != -1
    assert acc_end_idx > acc_idx
    assert do_idx == (acc_idx + 1)


def test_acc_incorrect_parallel_trans():
    '''Test that the acc transform can not be used to change
    the order of operations.'''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    acct = ACCParallelTrans()
    # Apply the OpenACC Parallel transformation
    # to the children in the wrong order
    with pytest.raises(TransformationError) as err:
        _, _ = acct.apply([schedule.children[1], schedule.children[0]])

    assert "Children are not consecutive children" in str(err)

    with pytest.raises(TransformationError) as err:
        _, _ = acct.apply([schedule.children[0].children[0],
                           schedule.children[0]])

    assert ("supplied nodes are not children of the same Schedule/parent"
            in str(err))


def test_acc_data_not_a_schedule():
    ''' Test that we raise an appropriate error if we attempt to apply
    an OpenACC Data transformation to something that is not a Schedule '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    acct = ACCDataTrans()
    accpara = ACCParallelTrans()

    with pytest.raises(TransformationError) as err:
        _, _ = acct.apply(schedule.children[0])
    assert ("Cannot apply an OpenACC data directive to something that is not "
            "a Schedule" in str(err))

    new_sched, _ = acct.apply(schedule)

    # Add a parallel region *around* the enter-data directive so that it
    # (erroneously) comes before it...
    new_sched, _ = accpara.apply(new_sched.children[0])
    with pytest.raises(GenerationError) as err:
        _ = psy.gen
    assert ("An ACC parallel region must be preceeded by an ACC enter-data "
            "directive but in invoke_0 this is not the case." in str(err))


def test_acc_data_copyin():
    ''' Test that we correctly generate the arguments to the copyin
    clause of an OpenACC data region '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    accpt = ACCParallelTrans()
    accdt = ACCDataTrans()

    # Put each loop within an OpenACC parallel region
    for child in schedule.children:
        if isinstance(child, Loop):
            new_sched, _ = accpt.apply(child)

    # Create a data region for the whole schedule
    new_sched, _ = accdt.apply(new_sched)

    invoke.schedule = new_sched
    code = str(psy.gen)

    # Check that we've correctly declared the logical variable that
    # records whether this is the first time we've entered this invoke.
    assert "LOGICAL, save :: first_time=.True." in code

    pcopy = (
        "      IF (first_time) THEN\n"
        "        !$acc enter data copyin(p_fld,p_fld%data,cu_fld,cu_fld%data,"
        "u_fld,u_fld%data,cv_fld,cv_fld%data,v_fld,v_fld%data,unew_fld,"
        "unew_fld%data,uold_fld,uold_fld%data)\n"
        "        first_time = .false.\n")
    assert pcopy in code
    for obj in ["u_fld", "v_fld", "p_fld", "cu_fld", "cv_fld", "unew_fld"]:
        assert "{0}%data_on_device = .true.".format(obj) in code
    assert ("        uold_fld%data_on_device = .true.\n"
            "      END IF \n" in code)


def test_acc_data_grid_copyin():
    ''' Test that we correctly generate the arguments to the copyin
    clause of an OpenACC data region when grid properties are required '''
    psy, invoke = get_invoke("single_invoke_grid_props.f90", API, idx=0)
    schedule = invoke.schedule

    accpt = ACCParallelTrans()
    accdt = ACCDataTrans()

    # Put each loop within an OpenACC parallel region
    for child in schedule.children:
        if isinstance(child, Loop):
            new_sched, _ = accpt.apply(child)

    # Create a data region for the whole schedule
    new_sched, _ = accdt.apply(new_sched)

    invoke.schedule = new_sched
    code = str(psy.gen)

    # TODO grid properties are effectively duplicated in this list (but the
    # OpenACC deep-copy support should spot this).
    pcopy = ("!$acc enter data copyin(u_fld,u_fld%data,cu_fld,cu_fld%data,"
             "u_fld%grid,u_fld%grid%tmask,u_fld%grid%area_t,"
             "u_fld%grid%area_u,d_fld,d_fld%data,du_fld,du_fld%data,"
             "d_fld%grid,d_fld%grid%tmask,d_fld%grid%area_t,"
             "d_fld%grid%area_u)")
    assert pcopy in code
    # Check that we flag that the fields are now on the device
    for obj in ["u_fld", "cu_fld", "du_fld", "d_fld"]:
        assert "{0}%data_on_device = .true.".format(obj) in code
    # Check that we have no acc_update_device calls
    assert "CALL acc_update_device" not in code


def test_acc_rscalar_update():
    '''
    Check that we generate code to update any real scalar kernel arguments on
    the device.
    '''
    psy, invoke = get_invoke("single_invoke_scalar_float_arg.f90", API, idx=0)
    schedule = invoke.schedule

    accpt = ACCParallelTrans()
    accdt = ACCDataTrans()

    # Put each loop within an OpenACC parallel region
    for child in schedule.children:
        if isinstance(child, Loop):
            new_sched, _ = accpt.apply(child)

    # Create a data region for the whole schedule
    new_sched, _ = accdt.apply(new_sched)

    invoke.schedule = new_sched
    code = str(psy.gen)
    # Check that the use statement has been added
    assert ("USE kernel_scalar_float, ONLY: bc_ssh_code\n"
            "      USE openacc, ONLY: acc_update_device" in code)
    expected = '''\
      ! Ensure all scalars on the device are up-to-date
      CALL acc_update_device(a_scalar, 1)
      !
      !$acc parallel default(present)
      DO j=1,jstop+1'''
    assert expected in code


def test_acc_iscalar_update():
    '''
    Check that we generate code to update any integer scalar kernel arguments
    on the device.
    '''
    psy, invoke = get_invoke("single_invoke_scalar_int_arg.f90", API, idx=0)
    schedule = invoke.schedule

    accpt = ACCParallelTrans()
    accdt = ACCDataTrans()

    # Put each loop within an OpenACC parallel region
    for child in schedule.children:
        if isinstance(child, Loop):
            new_sched, _ = accpt.apply(child)

    # Create a data region for the whole schedule
    new_sched, _ = accdt.apply(new_sched)

    invoke.schedule = new_sched
    code = str(psy.gen)
    # Check that the use statement has been added
    assert ("USE kernel_scalar_int, ONLY: bc_ssh_code\n"
            "      USE openacc, ONLY: acc_update_device" in code)
    expected = '''\
      ! Ensure all scalars on the device are up-to-date
      CALL acc_update_device(ncycle, 1)
      !
      !$acc parallel default(present)
      DO j=1,jstop+1'''
    assert expected in code


def test_acc_update_two_scalars():
    '''
    Check that we generate two separate acc_update_device() calls when
    we have two scalars.
    '''
    psy, invoke = get_invoke("single_invoke_two_kernels_scalars.f90", API,
                             idx=0)
    schedule = invoke.schedule

    accpt = ACCParallelTrans()
    accdt = ACCDataTrans()

    # Put each loop within an OpenACC parallel region
    for child in schedule.children:
        if isinstance(child, Loop):
            new_sched, _ = accpt.apply(child)

    # Create a data region for the whole schedule
    new_sched, _ = accdt.apply(new_sched)

    invoke.schedule = new_sched
    code = str(psy.gen)

    # Check that the use statement has been added
    assert ("USE kernel_scalar_float, ONLY: bc_ssh_code\n"
            "      USE openacc, ONLY: acc_update_device" in code)
    expected = '''\
      ! Ensure all scalars on the device are up-to-date
      CALL acc_update_device(a_scalar, 1)
      CALL acc_update_device(ncycle, 1)
      !
      !$acc parallel default(present)
      DO j=1,jstop+1'''
    assert expected in code


def test_acc_data_parallel_commute():
    '''Test that we can apply the OpenACC parallel and data
    transformations in either order'''
    accpt = ACCParallelTrans()
    accdt = ACCDataTrans()

    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    # Put each loop within an OpenACC parallel region
    for child in schedule.children:
        if isinstance(child, Loop):
            new_sched, _ = accpt.apply(child)

    # Create a data region for the whole schedule
    new_sched, _ = accdt.apply(new_sched)

    invoke.schedule = new_sched
    code1 = str(psy.gen)

    # Repeat these transformations but create the region
    # before the parallel loops
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    # Create a data region for the whole schedule
    new_sched, _ = accdt.apply(schedule)

    # Put each loop within an OpenACC parallel region
    for child in schedule.children:
        if isinstance(child, Loop):
            new_sched, _ = accpt.apply(child)

    invoke.schedule = new_sched
    code2 = str(psy.gen)

    assert code1 == code2


def test_accdata_duplicate():
    ''' Check that we raise an error if we attempt to add an OpenACC
    data directive to a schedule that already contains one '''
    accdt = ACCDataTrans()
    accpt = ACCParallelTrans()

    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    # Create a data region for the whole schedule
    new_sched, _ = accdt.apply(schedule)

    # Put each loop within an OpenACC parallel region
    for child in schedule.children:
        if isinstance(child, Loop):
            new_sched, _ = accpt.apply(child)

    # Erroneously attempt to add a data region for the second time
    with pytest.raises(TransformationError):
        _, _ = accdt.apply(new_sched)


def test_accloop():
    ''' Tests that we can apply a '!$acc loop' directive to a loop '''
    acclpt = ACCLoopTrans()
    accpara = ACCParallelTrans()
    accdata = ACCDataTrans()

    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0)
    schedule = invoke.schedule

    with pytest.raises(TransformationError) as err:
        _ = acclpt.apply(schedule)
    assert ("Cannot apply a parallel-loop directive to something that is not "
            "a loop" in str(err))

    # Apply an OpenACC loop directive to each loop
    for child in schedule.children:
        if isinstance(child, Loop):
            new_sched, _ = acclpt.apply(child)

    # Code generation should fail at this point because there's no
    # enclosing parallel region
    with pytest.raises(GenerationError) as err:
        _ = psy.gen
    assert ("ACCLoopDirective must have an ACCParallelDirective as an "
            "ancestor in the Schedule" in str(err))

    # Add an enclosing parallel region
    new_sched, _ = accpara.apply(schedule.children)

    # Code generation should still fail because there's no 'enter data'
    # directive and we need one for the parallel region to work
    with pytest.raises(GenerationError) as err:
        _ = psy.gen
    assert ("A Schedule containing an ACC parallel region must also "
            "contain an ACC enter data directive but none was found for "
            "invoke_0" in str(err))

    # Add a data region
    new_sched, _ = accdata.apply(new_sched)
    invoke.schedule = new_sched

    gen = str(psy.gen)

    assert '''\
      !$acc parallel default(present)
      !$acc loop independent
      DO j=2,jstop''' in gen
    assert ("END DO \n"
            "      !$acc loop independent\n"
            "      DO j=2,jstop+1" in gen)


def test_acc_collapse():
    ''' Tests for the collapse clause to a loop directive '''
    acclpt = ACCLoopTrans()
    accpara = ACCParallelTrans()
    accdata = ACCDataTrans()

    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API,
                             name="invoke_0")
    schedule = invoke.schedule
    child = schedule.children[0]

    # Check that we reject non-integer collapse arguments
    with pytest.raises(TransformationError) as err:
        _, _ = acclpt.apply(child, collapse=child)
    assert ("The 'collapse' argument must be an integer but got an object "
            "of type" in str(err))

    # Check that we reject invalid depths
    with pytest.raises(TransformationError) as err:
        _, _ = acclpt.apply(child, collapse=1)
    assert ("It only makes sense to collapse 2 or more loops but got a "
            "value of 1" in str(err))

    # Check that we reject attempts to collapse more loops than we have
    with pytest.raises(TransformationError) as err:
        _, _ = acclpt.apply(child, collapse=3)
    assert ("Cannot apply COLLAPSE(3) clause to a loop nest containing "
            "only 2 loops" in str(err))

    # Finally, do something valid and check that we get the correct
    # generated code
    new_sched, _ = acclpt.apply(child, collapse=2)

    new_sched, _ = accpara.apply(new_sched.children)
    new_sched, _ = accdata.apply(new_sched)

    invoke.schedule = new_sched

    gen = str(psy.gen)
    assert ("      !$acc parallel default(present)\n"
            "      !$acc loop collapse(2) independent\n"
            "      DO j=2,jstop\n"
            "        DO i=2,istop+1\n"
            "          CALL compute_cu_code(i, j, cu_fld%data, p_fld%data, "
            "u_fld%data)\n" in gen)


def test_acc_indep(capsys):
    ''' Tests for the independent clause to a loop directive. '''
    acclpt = ACCLoopTrans()
    accpara = ACCParallelTrans()
    accdata = ACCDataTrans()

    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API,
                             name="invoke_0")
    schedule = invoke.schedule
    new_sched, _ = acclpt.apply(schedule.children[0], independent=False)
    new_sched, _ = acclpt.apply(schedule.children[1], independent=True)
    # Check the view method
    new_sched.view()
    output, _ = capsys.readouterr()
    assert "[ACC Loop]" in output
    assert "[ACC Loop, independent]" in output
    new_sched, _ = accpara.apply(new_sched.children)
    new_sched, _ = accdata.apply(new_sched)
    # Check the generated code
    invoke.schedule = new_sched
    gen = str(psy.gen)
    assert "!$acc loop\n      DO j=2,jstop" in gen
    assert "!$acc loop independent\n      DO j=2,jstop+1" in gen
