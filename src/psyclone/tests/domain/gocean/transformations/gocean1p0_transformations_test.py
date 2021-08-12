# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2021, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified work Copyright (c) 2017-2019 by J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, Met Office

''' Module containing tests of Transformations when using the
    GOcean 1.0 API '''

from __future__ import absolute_import
import os
import re
import inspect
from importlib import import_module
import pytest
from psyclone.configuration import Config
from psyclone.domain.gocean.transformations import GOceanLoopFuseTrans, \
    GOMoveIterationBoundariesInsideKernelTrans
from psyclone.undoredo import Memento
from psyclone.errors import GenerationError
from psyclone.gocean1p0 import GOLoop
from psyclone.psyir.nodes import Loop
from psyclone.psyir.transformations import LoopFuseTrans, LoopTrans, \
    TransformationError
from psyclone.transformations import ACCKernelsTrans, \
    GOLoopSwapTrans, OMPParallelTrans, MoveTrans, \
    GOceanOMPParallelLoopTrans, GOceanOMPLoopTrans, KernelModuleInlineTrans, \
    ACCParallelTrans, ACCEnterDataTrans, ACCDataTrans, ACCLoopTrans, \
    OCLTrans, OMPLoopTrans
from psyclone.domain.gocean.transformations import GOConstLoopBoundsTrans
from psyclone.tests.gocean1p0_build import GOcean1p0Build, GOcean1p0OpenCLBuild
from psyclone.tests.utilities import count_lines, get_invoke, Compile

# The version of the PSyclone API that the tests in this file
# exercise
API = "gocean1.0"


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use gocean1.0 as API.'''
    Config.get().api = "gocean1.0"
    yield()
    Config._instance = None


def test_loop_fuse_different_iterates_over():
    ''' Test that an appropriate error is raised when we attempt to
    fuse two loops that have differing values of ITERATES_OVER '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           API, idx=0, dist_mem=False)
    schedule = invoke.schedule
    lftrans = LoopFuseTrans()

    # Attempt to fuse two loops that are iterating over different
    # things
    with pytest.raises(TransformationError) as err:
        _, _ = lftrans.apply(schedule.children[0],
                             schedule.children[1])
    assert "Loops do not have the same iteration space" in str(err.value)


def test_loop_fuse_error(monkeypatch):
    ''' Test that we catch various errors when loop fusing '''
    _, invoke = get_invoke("test14_module_inline_same_kernel.f90", API, idx=0,
                           dist_mem=False)
    schedule = invoke.schedule

    lftrans = GOceanLoopFuseTrans()
    assert str(lftrans) == "Fuse two adjacent loops together with " \
                           "GOcean-specific validity checks"

    # Apply loop fuse, but the first node is not a loop:
    with pytest.raises(TransformationError) as err:
        _, _ = lftrans.apply(schedule.children[0].children[0],
                             schedule.children[1])
    assert "Both nodes must be of the same GOLoop class." in str(err.value)

    # Also check that we catch this for the second argument:
    with pytest.raises(TransformationError) as err:
        _, _ = lftrans.apply(schedule.children[0],
                             schedule.children[1].children[0])
    assert "Both nodes must be of the same GOLoop class." in str(err.value)

    # Cause an unexpected error. This is not easy so we resort to
    # monkeypatching the constructor of the Memento class.
    def raise_error(_1, _2, _3):
        raise NotImplementedError("Test exception")
    monkeypatch.setattr(Memento, "__init__", raise_error)

    # Attempt to fuse two loops that are iterating over different
    # things
    with pytest.raises(TransformationError) as excinfo:
        _, _ = lftrans.apply(schedule.children[0],
                             schedule.children[1])
    assert 'Unexpected exception' in str(excinfo.value)


def test_omp_parallel_loop(tmpdir, fortran_writer):
    '''Test that we can generate an OMP PARALLEL DO correctly,
    independent of whether or not we are generating constant loop bounds '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    omp = GOceanOMPParallelLoopTrans()
    cbtrans = GOConstLoopBoundsTrans()
    omp.apply(schedule[0])

    gen = fortran_writer(psy.container)
    expected = (
        "    !$omp parallel do default(shared), private(i,j), "
        "schedule(static)\n"
        "    do j = cu_fld%internal%ystart, cu_fld%internal%ystop, 1\n"
        "      do i = cu_fld%internal%xstart, cu_fld%internal%xstop, 1\n"
        "        call compute_cu_code(i, j, cu_fld%data, p_fld%data, "
        "u_fld%data)\n"
        "      enddo\n"
        "    enddo\n"
        "    !$omp end parallel do")
    assert expected in gen

    cbtrans.apply(schedule)
    gen = fortran_writer(psy.container)
    expected = ("!$omp parallel do default(shared), private(i,j), "
                "schedule(static)\n"
                "    do j = 2, jstop, 1\n"
                "      do i = 2, istop + 1, 1\n"
                "        call compute_cu_code(i, j, cu_fld%data, "
                "p_fld%data, u_fld%data)\n"
                "      enddo\n"
                "    enddo\n"
                "    !$omp end parallel do")
    assert expected in gen
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_omp_region_with_wrong_arg_type():
    ''' Test that the OpenMP PARALLEL region transformation
        raises an appropriate error if passed something that is not
        a list of Nodes or a single Node. '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                           dist_mem=False)

    ompr = OMPParallelTrans()

    with pytest.raises(TransformationError):
        _, _ = ompr.apply(invoke)


def test_omp_region_with_single_loop(tmpdir):
    ''' Test that we can pass the OpenMP PARALLEL region transformation
        a single node in a schedule '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    ompr = OMPParallelTrans()
    cbtrans = GOConstLoopBoundsTrans()

    ompr.apply(schedule[1])

    # Store the results of applying this code transformation as
    # a string
    gen = str(psy.gen)
    gen = gen.lower()

    # Iterate over the lines of generated code
    within_omp_region = False
    call_count = 0
    for line in gen.split('\n'):
        if '!$omp parallel' in line:
            within_omp_region = True
        if '!$omp end parallel' in line:
            within_omp_region = False
        if ' call ' in line and within_omp_region:
            call_count += 1

    assert call_count == 1

    # Repeat the test after turning off constant loop bounds
    cbtrans.apply(schedule, {"const_bounds": False})
    gen = str(psy.gen)
    gen = gen.lower()
    within_omp_region = False
    call_count = 0
    for line in gen.split('\n'):
        if '!$omp parallel' in line:
            within_omp_region = True
        if '!$omp end parallel' in line:
            within_omp_region = False
        if ' call ' in line and within_omp_region:
            call_count += 1

    assert call_count == 1
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_omp_region_with_slice(tmpdir):
    ''' Test that we can pass the OpenMP PARALLEL region transformation
    a list of nodes specified as a slice '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    ompr = OMPParallelTrans()
    ompr.apply(schedule[1:])

    # Store the results of applying this code transformation as
    # a string
    gen = str(psy.gen)
    gen = gen.lower()

    # Iterate over the lines of generated code
    within_omp_region = False
    call_count = 0
    for line in gen.split('\n'):
        if '!$omp parallel' in line:
            within_omp_region = True
        if '!$omp end parallel' in line:
            within_omp_region = False
        if ' call ' in line and within_omp_region:
            call_count += 1

    assert call_count == 2
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_omp_region_with_slice_change_order():
    ''' Test that the OpenMP transform does not allow to switch
    or duplicate child nodes.
    '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
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
    assert ("Children are not consecutive children of one parent"
            in str(err.value))

    # Also test the case of duplicated children:
    # ------------------------------------------
    with pytest.raises(TransformationError) as err:
        ompr.apply([schedule.children[0], schedule.children[0]])
    assert ("Children are not consecutive children of one parent"
            in str(err.value))


def test_omp_region_no_slice(tmpdir):
    ''' Test that we can pass the OpenMP PARALLEL region transformation
    a list of nodes specified as node.children '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule
    ompr = OMPParallelTrans()

    ompr.apply(schedule.children)
    # Store the results of applying this code transformation as
    # a string
    gen = str(psy.gen)
    gen = gen.lower()
    # Iterate over the lines of generated code
    within_omp_region = False
    call_count = 0
    for line in gen.split('\n'):
        if '!$omp parallel' in line:
            within_omp_region = True
        if '!$omp end parallel' in line:
            within_omp_region = False
        if ' call ' in line and within_omp_region:
            call_count += 1
    assert call_count == 3
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_omp_region_no_slice_no_const_bounds(tmpdir):
    ''' Test that we generate the correct code when we apply an OpenMP
    PARALLEL region transformation to a list of nodes when the InvokeSchedule
    has been transformed to use loop-bound look-ups '''

    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule
    ompr = OMPParallelTrans()
    cbtrans = GOConstLoopBoundsTrans()

    cbtrans.apply(schedule)
    ompr.apply(schedule.children)
    # Store the results of applying this code transformation as
    # a string
    gen = str(psy.gen)
    gen = gen.lower()
    # Iterate over the lines of generated code
    within_omp_region = False
    call_count = 0
    for line in gen.split('\n'):
        if '!$omp parallel' in line:
            within_omp_region = True
        if '!$omp end parallel' in line:
            within_omp_region = False
        if ' call ' in line and within_omp_region:
            call_count += 1
    assert call_count == 3
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_omp_region_retains_kernel_order1(tmpdir):
    ''' Test that applying the OpenMP PARALLEL region transformation
    to a sub-set of nodes (last 2 of three) does not change their
    ordering '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    ompr = OMPParallelTrans()
    cbtrans = GOConstLoopBoundsTrans()

    ompr.apply(schedule.children[1:])

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
    assert cu_idx < cv_idx < ts_idx

    # Repeat after turning off constant loop bounds
    cbtrans.apply(schedule, {"const_bounds": False})
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
    assert cu_idx < cv_idx < ts_idx
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_omp_region_retains_kernel_order2(tmpdir):
    ''' Test that applying the OpenMP PARALLEL region transformation
    to a sub-set of nodes (first 2 of 3) does not change their
    ordering '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    ompr = OMPParallelTrans()

    ompr.apply(schedule.children[0:2])

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
    assert cu_idx < cv_idx < ts_idx
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_omp_region_retains_kernel_order3(tmpdir):
    ''' Test that applying the OpenMP PARALLEL region transformation
    to a sub-set of nodes (middle 1 of 3) does not change their
    ordering '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    ompr = OMPParallelTrans()
    ompl = GOceanOMPLoopTrans()

    # Put an OMP Do around the 2nd loop of the schedule
    ompl.apply(schedule.children[1])

    # Put an OMP Parallel around that single OMP Do
    ompr.apply([schedule.children[1]])

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
    assert cu_idx < cv_idx < ts_idx
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_omp_region_before_loops_trans(tmpdir):
    ''' Test of the OpenMP PARALLEL region transformation where
    we do the region transformation before the loop
    transformations. '''
    psy, invoke = get_invoke("single_invoke_two_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    # Put all of the loops in the schedule within a single
    # OpenMP region
    ompr = OMPParallelTrans()
    ompr.apply(schedule.children)

    # Put an OpenMP do directive around each loop contained
    # in the region
    ompl = GOceanOMPLoopTrans()
    for child in schedule.children[0].dir_body[:]:
        ompl.apply(child)

    # Store the results of applying this code transformation as
    # a string
    gen = str(psy.gen)

    # Iterate over the lines of generated code
    omp_region_idx = -1
    omp_do_idx = -1
    for idx, line in enumerate(gen.split('\n')):
        if '!$omp parallel' in line:
            omp_region_idx = idx
        if '!$omp do' in line:
            omp_do_idx = idx
        if 'DO j =' in line:
            break

    assert omp_region_idx != -1
    assert omp_do_idx != -1
    assert omp_do_idx - omp_region_idx == 1
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_omp_region_after_loops_trans(tmpdir):
    ''' Test of the OpenMP PARALLEL region transformation where we
    do the loop transformations before the region transformation '''
    psy, invoke = get_invoke("single_invoke_two_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    # Put an OpenMP do directive around each loop contained
    # in the schedule
    ompl = GOceanOMPLoopTrans()
    for child in schedule.children:
        ompl.apply(child)

    # Now put an OpenMP parallel region around that set of
    # loops
    ompr = OMPParallelTrans()
    ompr.apply(schedule.children)

    gen = str(psy.gen)

    # Iterate over the lines of generated code
    omp_region_idx = -1
    omp_do_idx = -1
    for idx, line in enumerate(gen.split('\n')):
        if '!$omp parallel' in line:
            omp_region_idx = idx
        if '!$omp do' in line:
            omp_do_idx = idx
        if 'DO j =' in line:
            break

    assert omp_region_idx != -1
    assert omp_do_idx != -1
    assert omp_do_idx - omp_region_idx == 1
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_omp_region_commutes_with_loop_trans(tmpdir):
    ''' Test that the OpenMP PARALLEL region and (orphan) loop
    transformations commute - i.e. we get the same result
    independent of the order in which they are applied. '''
    psy, invoke = get_invoke("single_invoke_two_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    # Put an OpenMP do directive around each loop contained
    # in the schedule
    ompl = GOceanOMPLoopTrans()
    for child in schedule.children:
        ompl.apply(child)

    # Now put an OpenMP parallel region around that set of
    # loops
    ompr = OMPParallelTrans()
    ompr.apply(schedule.children)

    # Store the results of applying this code transformation as
    # a string
    loop_before_region_gen = str(psy.gen)

    # Now we do it again but in the opposite order...

    # Put all of the loops in the schedule within a single
    # OpenMP region
    psy, invoke = get_invoke("single_invoke_two_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    ompr = OMPParallelTrans()
    ompr.apply(schedule.children)

    # Put an OpenMP do directive around each loop contained
    # in the region
    ompl = GOceanOMPLoopTrans()
    for child in schedule.children[0].dir_body[:]:
        ompl.apply(child)

    # Store the results of applying this code transformation as
    # a string
    region_before_loop_gen = str(psy.gen)

    assert region_before_loop_gen == loop_before_region_gen
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_omp_region_commutes_with_loop_trans(tmpdir):
    ''' Test that the OpenMP PARALLEL region and (orphan) loop
    transformations commute - i.e. we get the same result
    independent of the order in which they are applied. '''
    psy, invoke = get_invoke("single_invoke_two_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    # Put an OpenMP do directive around each loop contained
    # in the schedule
    ompl = GOceanOMPLoopTrans()
    for child in schedule.children:
        ompl.apply(child)

    # Now put an OpenMP parallel region around that set of
    # loops
    ompr = OMPParallelTrans()
    ompr.apply(schedule.children)

    loop_before_region_gen = str(psy.gen)

    # Now we do it again but in the opposite order...
    # ...we re-generate the original schedule here rather than
    # keeping a (deep) copy of it from earlier as that can
    # cause resource problems.
    psy, invoke = get_invoke("single_invoke_two_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    # Put all of the loops in the schedule within a single
    # OpenMP region
    ompr = OMPParallelTrans()
    ompr.apply(schedule.children)

    # Put an OpenMP do directive around each loop contained
    # in the region
    ompl = GOceanOMPLoopTrans()
    for child in schedule.children[0].dir_body[:]:
        ompl.apply(child)

    # Store the results of applying this code transformation as
    # a string
    region_before_loop_gen = str(psy.gen)

    assert region_before_loop_gen == loop_before_region_gen
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_omp_region_nodes_not_children_of_same_parent():
    ''' Test that we raise appropriate error if user attempts
    to put a region around nodes that are not children of
    the same parent '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                           dist_mem=False)
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
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    # Put an OpenMP do directive around each loop contained
    # in the schedule
    ompl = GOceanOMPLoopTrans()
    ompr = OMPParallelTrans()

    for child in schedule.children:
        ompl.apply(child)

    # Now enclose all but the last loop in a parallel region
    ompr.apply(schedule.children[0:-2])

    # Attempt to generate the transformed code
    with pytest.raises(GenerationError):
        _ = psy.gen


def test_omp_loop_applied_to_non_loop():
    ''' Test that we raise a TransformationError if we attempt
    to apply an OMP DO transformation to something that
    is not a loop '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                           dist_mem=False)
    schedule = invoke.schedule

    ompl = OMPLoopTrans()
    ompl.apply(schedule.children[0])

    # Attempt to (erroneously) apply the OMP Loop transformation
    # to the first node in the schedule (which is now itself an
    # OMP Loop transformation)
    with pytest.raises(TransformationError):
        ompl.apply(schedule.children[0])


def test_go_omp_loop_applied_to_non_loop():
    ''' Test that we raise a TransformationError if we attempt
    to apply a GOcean OMP DO transformation to something that
    is not a loop '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                           dist_mem=False)
    schedule = invoke.schedule

    ompl = GOceanOMPLoopTrans()
    ompl.apply(schedule.children[0])

    # Attempt to (erroneously) apply the GO OMP Loop transformation
    # to the first node in the schedule (which is now itself an
    # OMPDoDirective).
    with pytest.raises(TransformationError) as err:
        ompl.validate(schedule.children[0])
    assert ("Target of GOceanOMPLoopTrans transformation must be a sub-class "
            "of Loop but got 'OMPDoDirective'" in str(err.value))
    with pytest.raises(TransformationError) as err:
        ompl.apply(schedule.children[0])
    assert ("Target of GOceanOMPLoopTrans transformation must be a sub-class "
            "of Loop but got 'OMPDoDirective'" in str(err.value))


def test_go_omp_loop_applied_to_wrong_loop_type():
    ''' Test that we raise a TransformationError if we attempt to
    apply a GOcean OMP  DO transformation to a loop of
    the wrong type '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                           dist_mem=False)
    schedule = invoke.schedule

    # Manually break the loop-type of the first loop in order to
    # test that this error is handled. We have to work-around
    # the setter method to do this since it has error checking
    # too!
    schedule.children[0]._loop_type = "wrong"

    ompl = GOceanOMPLoopTrans()
    # Attempt to apply the transformation to the loop that has been
    # given an incorrect type
    with pytest.raises(TransformationError) as err:
        ompl.validate(schedule.children[0])
    assert ("The requested loop is not of type inner or outer" in
            str(err.value))
    with pytest.raises(TransformationError) as err:
        _, _ = ompl.apply(schedule.children[0])
    assert ("The requested loop is not of type inner or outer" in
            str(err.value))


def test_go_omp_parallel_loop_applied_to_non_loop():
    ''' Test that we raise a TransformationError if we attempt to
    apply a GOcean OMP Parallel DO transformation to something that
    is not a loop '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                           dist_mem=False)
    schedule = invoke.schedule

    ompl = GOceanOMPParallelLoopTrans()
    ompl.apply(schedule.children[0])

    # Attempt to (erroneously) apply the OMP Loop transformation
    # to the first node in the schedule (which is now itself an
    # OMP Loop transformation)
    with pytest.raises(TransformationError):
        ompl.apply(schedule.children[0])


def test_go_omp_parallel_loop_applied_to_wrong_loop_type():
    ''' Test that we raise a TransformationError if we attempt to
    apply a GOcean OMP Parallel DO transformation to a loop of
    the wrong type '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                           dist_mem=False)
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
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    ompl = GOceanOMPParallelLoopTrans()
    ompr = OMPParallelTrans()

    # Put an OpenMP parallel do directive around all of the loops
    for child in schedule.children:
        ompl.apply(child)

    # Now enclose all of the children within a parallel region
    ompr.apply(schedule.children)

    # Attempt to generate the transformed code
    with pytest.raises(GenerationError):
        _ = psy.gen


def test_omp_parallel_region_inside_parallel_do():
    ''' Test that a generation error is raised if we attempt
    to have an OpenMP parallel region within an OpenMP
    parallel do (with the latter applied first). '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                           dist_mem=False)
    schedule = invoke.schedule

    ompl = GOceanOMPParallelLoopTrans()
    ompr = OMPParallelTrans()

    # Put an OpenMP parallel do directive around one of the loops
    _, _ = ompl.apply(schedule.children[1])

    # Now attempt to put a parallel region inside that parallel do
    with pytest.raises(TransformationError) as err:
        _, _ = ompr.apply([schedule.children[1].children[0]])
    assert ("cannot create an OpenMP PARALLEL region within another "
            "OpenMP region" in str(err.value))


def test_omp_parallel_do_around_parallel_region():
    ''' Test that a generation error is raised if we attempt
    to have an OpenMP parallel region around an OpenMP
    parallel do (with the latter applied second) '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    ompl = GOceanOMPParallelLoopTrans()
    ompr = OMPParallelTrans()

    # Put a parallel region around two of the loops
    ompr.apply(schedule[0:2])

    # Put an OpenMP parallel do directive around one of those loops
    # (which is now a child of the region directive)
    ompl.apply(schedule[0].dir_body[0])

    # Attempt to generate the transformed code
    with pytest.raises(GenerationError):
        _ = psy.gen


def test_omp_region_invalid_node():
    ''' Check that the OMPParallelTrans transformation rejects nodes
    of the wrong type. We use an OpenACC directive to trigger this error. '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                           dist_mem=False)
    schedule = invoke.schedule

    ompr = OMPParallelTrans()
    acct = ACCParallelTrans()
    # Apply an OpenACC parallel transformation to the first loop
    acct.apply(schedule.children[0])

    with pytest.raises(TransformationError) as err:
        ompr.apply(schedule.children)
    assert ("ACCParallelDirective' cannot be enclosed by a "
            "OMPParallelTrans transformation" in str(err.value))

    # Check that the test can be disabled with the appropriate option:
    ompr.apply(schedule.children, {"node-type-check": False})


def test_omp_region_with_children_of_different_types(tmpdir):
    ''' Test that we can generate code if we have an
    OpenMP parallel region enclosing children of different types. '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    ompl = GOceanOMPLoopTrans()
    ompr = OMPParallelTrans()

    # Put an OpenMP do directive around one loop
    ompl.apply(schedule.children[1])

    # Now enclose all of the children within a parallel region
    ompr.apply(schedule.children)

    # Attempt to generate the transformed code
    _ = psy.gen
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_omp_schedule_default_static(tmpdir):
    ''' Test that if no OMP schedule is specified then we default
    to "static" '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    ompl = GOceanOMPLoopTrans()
    ompr = OMPParallelTrans()

    # Put an OpenMP do directive around one loop without specifying
    # the OMP schedule to use
    ompl.apply(schedule.children[1])

    # Now enclose it within a parallel region
    ompr.apply(schedule.children[1])

    # Attempt to generate the transformed code
    gen = str(psy.gen)

    assert '!$omp do schedule(static)' in gen
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_omp_do_schedule_runtime(tmpdir):
    ''' Test that we can specify the schedule of an OMP do as
    "runtime" '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    ompl = GOceanOMPLoopTrans(omp_schedule="runtime")
    ompr = OMPParallelTrans()

    # Put an OpenMP do directive around one loop
    ompl.apply(schedule.children[1])

    # Now enclose it within a parallel region
    ompr.apply(schedule.children[1])

    # Attempt to generate the transformed code
    gen = str(psy.gen)

    assert '!$omp do schedule(runtime)' in gen
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_omp_do_schedule_dynamic(tmpdir):
    ''' Test that we can specify the schedule of an OMP do as
    "dynamic" '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    ompl = GOceanOMPLoopTrans(omp_schedule="dynamic")
    ompr = OMPParallelTrans()

    # Put an OpenMP do directive around one loop
    ompl.apply(schedule.children[1])

    # Now enclose it within a parallel region
    ompr.apply(schedule.children[1])

    # Attempt to generate the transformed code
    gen = str(psy.gen)

    assert '!$omp do schedule(dynamic)' in gen
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_omp_do_schedule_guided(tmpdir):
    ''' Test that we can specify the schedule of an OMP do as
    "guided" '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    ompl = GOceanOMPLoopTrans(omp_schedule="guided")
    ompr = OMPParallelTrans()

    # Put an OpenMP do directive around one loop
    ompl.apply(schedule.children[1])

    # Now enclose it within a parallel region
    ompr.apply(schedule.children[1])

    # Attempt to generate the transformed code
    gen = str(psy.gen)

    assert '!$omp do schedule(guided)' in gen
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_omp_schedule_guided_with_empty_chunk():
    ''' Test that we raise an appropriate error if we miss off
    the chunksize '''
    with pytest.raises(TransformationError):
        _ = GOceanOMPLoopTrans(omp_schedule="guided, ")


def test_omp_schedule_guided_with_chunk(tmpdir):
    ''' Test that we can specify the schedule of an OMP do as
    "guided,n" where n is some chunk size'''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    ompl = GOceanOMPLoopTrans(omp_schedule="guided,10")
    ompr = OMPParallelTrans()

    # Put an OpenMP do directive around one loop
    ompl.apply(schedule.children[1])

    # Now enclose it within a parallel region
    ompr.apply(schedule.children[1])

    # Attempt to generate the transformed code
    gen = str(psy.gen)

    assert '!$omp do schedule(guided,10)' in gen
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


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


def test_module_noinline_default(tmpdir):
    ''' Test that by default there is no module inlining '''
    psy, _ = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                        dist_mem=False)
    gen = str(psy.gen)
    # check that the subroutine has not been inlined
    assert 'SUBROUTINE compute_cu_code(i, j, cu, p, u)' not in gen
    # check that the associated use exists (as this is removed when
    # inlining)
    assert 'USE compute_cu_mod, ONLY: compute_cu_code' in gen
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_module_inline(tmpdir):
    ''' Test that we can succesfully inline a basic kernel subroutine
    routine into the PSy layer module by directly setting inline to
    true for the specified kernel. '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule
    kern_call = schedule.children[0].loop_body[0].loop_body[0]
    kern_call.module_inline = True
    gen = str(psy.gen)
    # check that the subroutine has been inlined correctly
    expected = (
        "    END SUBROUTINE invoke_0\n"
        "    SUBROUTINE compute_cu_code(i, j, cu, p, u)\n")
    assert expected in gen
    # check that the associated use no longer exists
    assert 'USE compute_cu_mod, ONLY: compute_cu_code' not in gen
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_module_inline_with_transformation(tmpdir):
    ''' Test that we can succesfully inline a basic kernel subroutine
    routine into the PSy layer module using a transformation '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule
    kern_call = schedule.children[1].loop_body[0].loop_body[0]
    inline_trans = KernelModuleInlineTrans()
    inline_trans.apply(kern_call)
    gen = str(psy.gen)
    # check that the subroutine has been inlined
    assert 'SUBROUTINE compute_cv_code(i, j, cv, p, v)' in gen
    # check that the associated use no longer exists
    assert 'USE compute_cv_mod, ONLY: compute_cv_code' not in gen
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_module_no_inline_with_transformation(tmpdir):
    ''' Test that we can switch off the inlining of a kernel routine
    into the PSy layer module using a transformation. Relies on the
    test_module_inline() test being successful to be a valid test. '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule
    kern_call = schedule.children[0].loop_body[0].loop_body[0]
    # directly switch on inlining
    kern_call.module_inline = True
    inline_trans = KernelModuleInlineTrans()
    # use a transformation to switch inlining off again
    inline_trans.apply(kern_call, {"inline": False})
    gen = str(psy.gen)
    # check that the subroutine has not been inlined
    assert 'SUBROUTINE compute_cu_code(i, j, cu, p, u)' not in gen
    # check that the associated use exists (as this is removed when
    # inlining)
    assert 'USE compute_cu_mod, ONLY: compute_cu_code' in gen
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


# we can not test if someone accidentally sets module_inline to True
# to an object that is not a Kernel as Python allows one to
# dynamically add new variables to an object. Therefore an error is
# never thrown. This would be testable if "inline" were a function.
# def test_inline_error_if_not_kernel():


def test_transformation_inline_error_if_not_kernel():
    ''' Test that the inline transformation fails if the object being
    passed is not a kernel'''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                           dist_mem=False)
    schedule = invoke.schedule
    kern_call = schedule.children[0].loop_body[0]
    inline_trans = KernelModuleInlineTrans()
    with pytest.raises(TransformationError):
        _, _ = inline_trans.apply(kern_call)


def test_module_inline_with_sub_use(tmpdir):
    ''' Test that we can module inline a kernel subroutine which
    contains a use statement'''
    psy, invoke = get_invoke("single_invoke_scalar_int_arg.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule
    kern_call = schedule.children[0].loop_body[0].loop_body[0]
    inline_trans = KernelModuleInlineTrans()
    inline_trans.apply(kern_call)
    gen = str(psy.gen)
    # check that the subroutine has been inlined
    assert 'SUBROUTINE bc_ssh_code(ji, jj, istep, ssha, tmask)' in gen
    # check that the use within the subroutine exists
    assert 'USE grid_mod' in gen
    # check that the associated psy use does not exist
    assert 'USE bc_ssh_mod, ONLY: bc_ssh_code' not in gen
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_module_inline_same_kernel(tmpdir):
    '''Tests that correct results are obtained when an invoke that uses
    the same kernel subroutine more than once has that kernel
    inlined'''
    psy, invoke = get_invoke("test14_module_inline_same_kernel.f90", API,
                             idx=0)
    schedule = invoke.schedule
    kern_call = schedule.coded_kernels()[0]
    inline_trans = KernelModuleInlineTrans()
    _, _ = inline_trans.apply(kern_call)
    gen = str(psy.gen)
    # check that the subroutine has been inlined
    assert 'SUBROUTINE compute_cu_code(' in gen
    # check that the associated psy "use" does not exist
    assert 'USE compute_cu_mod, ONLY: compute_cu_code' not in gen
    # check that the subroutine has only been inlined once
    count = count_lines(gen, "SUBROUTINE compute_cu_code(")
    assert count == 1, "Expecting subroutine to be inlined once"
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_module_inline_and_compile(tmpdir):
    '''ATM incorrect code is produced if a kernel is inlined, that
    uses variable from the original module. Proper solution would
    likely be to add a 'use' statement to the inline kernel (which
    again only works if the module variable is accessible outside
    of the module)
    '''
    Compile.skip_if_compilation_disabled()
    psy, invoke = get_invoke("test14_module_inline_same_kernel.f90", API,
                             idx=0, dist_mem=False)
    schedule = invoke.schedule
    kern_call = schedule.children[0].loop_body[0].loop_body[0]
    inline_trans = KernelModuleInlineTrans()
    _, _ = inline_trans.apply(kern_call)
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_module_inline_warning_no_change():
    '''Test of the warning clause in the Kernel transformation when
    no change is made to the inlining of a Kernel i.e. the inlining
    request is already what is happening. No warning is currently made
    as we have not added logging to the code but this test covers the
    clause '''
    _, invoke = get_invoke("test14_module_inline_same_kernel.f90", API, idx=0,
                           dist_mem=False)
    schedule = invoke.schedule
    kern_call = schedule.coded_kernels()[0]
    inline_trans = KernelModuleInlineTrans()
    _, _ = inline_trans.apply(kern_call, {"inline": False})


def test_loop_swap_correct(tmpdir):
    ''' Testing correct loop swapping transform. Esp. try first, middle, and
    last invokes to make sure the inserting of the inner loop happens at
    the right place.'''

    psy, _ = get_invoke("test27_loop_swap.f90", API, idx=0, dist_mem=False)
    invoke = psy.invokes.get("invoke_loop1")
    schedule = invoke.schedule
    schedule_str = str(schedule)

    # First make sure to throw an early error if the source file
    # test27_loop_swap.f90 should have been changed
    expected = (
        r"Loop\[id:'', variable:'j'.*?"
        r"Loop\[id:'', variable:'i'.*?"
        r"kern call: bc_ssh_code.*?"
        r"Loop\[id:'', variable:'j'.*?"
        r"Loop\[id:'', variable:'i'.*?"
        r"kern call: bc_solid_u_code .*?"
        r"Loop\[id:'', variable:'j'.*?"
        r"Loop\[id:'', variable:'i'.*?"
        r"kern call: bc_solid_v_code")

    assert re.search(expected, schedule_str.replace("\n", " "))

    # Now swap the first loops
    swap = GOLoopSwapTrans()
    swap.apply(schedule.children[0])
    schedule_str = str(schedule)

    expected = (
        r"Loop\[id:'', variable:'i'.*?"
        r"Loop\[id:'', variable:'j'.*?"
        r"kern call: bc_ssh_code.*?"
        r"Loop\[id:'', variable:'j'.*?"
        r"Loop\[id:'', variable:'i'.*?"
        r"kern call: bc_solid_u_code .*?"
        r"Loop\[id:'', variable:'j'.*?"
        r"Loop\[id:'', variable:'i'.*?"
        r"kern call: bc_solid_v_code")

    assert re.search(expected, schedule_str.replace("\n", " "))

    # Now swap the middle loops
    swap.apply(schedule.children[1])
    schedule_str = str(schedule)

    expected = (
        r"Loop\[id:'', variable:'i'.*?"
        r"Loop\[id:'', variable:'j'.*?"
        r"kern call: bc_ssh_code.*?"
        r"Loop\[id:'', variable:'i'.*?"
        r"Loop\[id:'', variable:'j'.*?"
        r"kern call: bc_solid_u_code .*?"
        r"Loop\[id:'', variable:'j'.*?"
        r"Loop\[id:'', variable:'i'.*?"
        r"kern call: bc_solid_v_code")

    assert re.search(expected, schedule_str.replace("\n", " "))

    # Now swap the last loops
    swap.apply(schedule.children[2])
    schedule_str = str(schedule)

    expected = (
        r"Loop\[id:'', variable:'i'.*?"
        r"Loop\[id:'', variable:'j'.*?"
        r"kern call: bc_ssh_code.*?"
        r"Loop\[id:'', variable:'i'.*?"
        r"Loop\[id:'', variable:'j'.*?"
        r"kern call: bc_solid_u_code .*?"
        r"Loop\[id:'', variable:'i'.*?"
        r"Loop\[id:'', variable:'j'.*?"
        r"kern call: bc_solid_v_code")

    assert re.search(expected, schedule_str.replace("\n", " "))

    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_go_loop_swap_errors():
    ''' Test loop swapping transform with incorrect parameters. '''

    psy, invoke_loop1 = get_invoke("test27_loop_swap.f90", API, idx=1,
                                   dist_mem=False)

    schedule = invoke_loop1.schedule
    swap = GOLoopSwapTrans()
    assert str(swap) == "Exchange the order of two nested loops: inner "\
        "becomes outer and vice versa"

    # Test error if given node is not the outer loop of at least
    # a double nested loop:
    with pytest.raises(TransformationError) as error:
        swap.apply(schedule.children[0].loop_body[0])
    assert re.search("Transformation Error: Target of GOLoopSwapTrans "
                     "transformation must be a sub-class of Loop but got "
                     "'GOKern'.", str(error.value), re.S) is not None

    # Not a loop: use the call to bc_ssh_code node as example for this test:
    with pytest.raises(TransformationError) as error:
        swap.apply(schedule.children[0].loop_body[0].loop_body[0])
    assert ("Target of GOLoopSwapTrans transformation must be a sub-class of "
            "Loop but got 'GOKern'" in str(error.value))

    # Now create an outer loop with more than one inner statement
    # ... by fusing the first and second outer loops :(
    invoke_loop2 = psy.invokes.get("invoke_loop2")
    schedule = invoke_loop2.schedule

    fuse = GOceanLoopFuseTrans()
    fuse.apply(schedule.children[0], schedule.children[1])

    with pytest.raises(TransformationError) as error:
        swap.apply(schedule.children[0])
    assert re.search("Supplied node .* must be the outer loop of a loop nest "
                     "and must have exactly one inner loop, but this node "
                     "has 2 inner statements, the first two being .* and .*",
                     str(error.value), re.S) is not None

    # Now remove the body of the first inner loop, and pass the first
    # inner loop --> i.e. a loop with an empty body
    del schedule.children[0].loop_body[0].children[3].children[0]

    with pytest.raises(TransformationError) as error:
        swap.apply(schedule.children[0].loop_body[0])
    assert re.search("Supplied node .* must be the outer loop of a loop nest "
                     "and must have one inner loop, but this node does not "
                     "have any statements inside.",
                     str(error.value), re.S) is not None


def test_go_loop_swap_wrong_loop_type():
    '''
    Test loop swapping transform when supplied loops are not GOLoops.
    '''
    swap = GOLoopSwapTrans()
    _, invoke = get_invoke("1.0.1_single_named_invoke.f90",
                           "dynamo0.3", idx=0, dist_mem=True)
    with pytest.raises(TransformationError) as error:
        swap.apply(invoke.schedule.children[4])

    assert re.search("Given node .* is not a GOLoop, but an instance of "
                     ".*DynLoop", str(error.value), re.S) is not None

    _, invoke_loop1 = get_invoke("test27_loop_swap.f90", API, idx=1,
                                 dist_mem=False)
    schedule = invoke_loop1.schedule
    loop = schedule[0].loop_body[0]
    assert isinstance(loop, GOLoop)
    # Change the class of the inner loop so that it is not a GOLoop
    loop.__class__ = Loop
    with pytest.raises(TransformationError) as error:
        swap.apply(schedule[0])
    assert "is not a GOLoop, but an instance of 'Loop'" in str(error.value)


def test_ocl_apply(kernel_outputdir):
    ''' Check that OCLTrans generates correct code '''
    psy, invoke = get_invoke("test11_different_iterates_over_"
                             "one_invoke.f90", API, idx=0, dist_mem=False)
    schedule = invoke.schedule
    # Currently, moving the boundaries inside the kernel is a prerequisite
    # for the GOcean gen_ocl() code generation.
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in schedule.coded_kernels():
        trans.apply(kernel)
    ocl = OCLTrans()

    # Check that we raise the correct error if we attempt to apply the
    # transformation to something that is not an InvokeSchedule
    with pytest.raises(TransformationError) as err:
        _, _ = ocl.apply(schedule.children[0])
    assert "the supplied node must be a (sub-class of) InvokeSchedule " \
        in str(err.value)

    ocl.apply(schedule)
    assert schedule.opencl

    gen = str(psy.gen)
    assert "USE clfortran" in gen
    # Check that the new kernel files have been generated
    kernel_files = os.listdir(str(kernel_outputdir))
    assert len(kernel_files) == 2
    assert "kernel_ne_offset_compute_cv_0.cl" in kernel_files
    assert "kernel_scalar_int_bc_ssh_0.cl" in kernel_files
    assert GOcean1p0OpenCLBuild(kernel_outputdir).code_compiles(psy)


def test_acc_parallel_not_a_loop():
    ''' Test that we raise an appropriate error if we attempt
    to apply the OpenACC Parallel transformation to something that
    is not a loop '''

    acct = ACCParallelTrans()
    # Provide an invalid node type (just the integer 1) to the OpenACC
    # Parallel transformation
    with pytest.raises(TransformationError) as error:
        _, _ = acct.apply(1)

    assert "Argument must be a single Node in a Schedule, a Schedule or a " \
           "list of Nodes in a Schedule but have been passed an object " \
           "of type:" in str(error.value)
    # Python2/3 differences in type string
    assert "'int'>" in str(error.value)


def test_acc_parallel_trans(tmpdir, fortran_writer):
    ''' Test that we can apply an OpenACC parallel transformation
    to a loop '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    # Apply the OpenACC Parallel transformation to the first loop of the
    # schedule
    acct = ACCParallelTrans()
    acct.apply(schedule.children[0])

    with pytest.raises(GenerationError) as err:
        _ = fortran_writer(psy.container)
    assert ("An ACC parallel region must either be preceded by an ACC enter "
            "data directive or enclosed within an ACC data region but in "
            "'invoke_0' this is not the case" in str(err.value))

    # Apply the OpenACC EnterData transformation
    accdt = ACCEnterDataTrans()
    accdt.apply(schedule)
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
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_acc_incorrect_parallel_trans():
    '''Test that the acc transform can not be used to change
    the order of operations.'''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                           dist_mem=False)
    schedule = invoke.schedule

    acct = ACCParallelTrans()
    # Apply the OpenACC Parallel transformation
    # to the children in the wrong order
    with pytest.raises(TransformationError) as err:
        _, _ = acct.apply([schedule.children[1], schedule.children[0]])

    assert "Children are not consecutive children" in str(err.value)

    with pytest.raises(TransformationError) as err:
        _, _ = acct.apply([schedule.children[0].children[0],
                           schedule.children[0]])

    assert ("supplied nodes are not children of the same parent"
            in str(err.value))


def test_acc_data_not_a_schedule():
    ''' Test that we raise an appropriate error if we attempt to apply
    an OpenACC Data transformation to something that is not an
    InvokeSchedule. '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                           dist_mem=False)
    schedule = invoke.schedule

    acct = ACCEnterDataTrans()

    with pytest.raises(TransformationError) as err:
        _, _ = acct.apply(schedule.children[0])
    assert ("Cannot apply an OpenACC enter-data directive to something that "
            "is not a Schedule" in str(err.value))


def test_acc_parallel_invalid_node():
    ''' Test that the OpenACC Parallel region transformation rejects
    unsupported node types. '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                           dist_mem=False)
    schedule = invoke.schedule

    acct = ACCEnterDataTrans()
    accpara = ACCParallelTrans()

    # Add an enter-data directive to the schedule
    acct.apply(schedule)

    # Attempt to enclose the enter-data directive within a parallel region
    with pytest.raises(TransformationError) as err:
        _, _ = accpara.apply(schedule.children[0])
    assert ("'GOACCEnterDataDirective' cannot be enclosed by a "
            "ACCParallelTrans transformation" in str(err.value))


def test_acc_data_copyin(tmpdir):
    ''' Test that we correctly generate the arguments to the copyin
    clause of an OpenACC data region '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    accpt = ACCParallelTrans()
    accdt = ACCEnterDataTrans()

    # Put each loop within an OpenACC parallel region
    for child in schedule.children:
        if isinstance(child, Loop):
            accpt.apply(child)

    # Create a data region for the whole schedule
    accdt.apply(schedule)
    code = str(psy.gen)

    assert (
        "      !$acc enter data copyin(p_fld,p_fld%data,cu_fld,cu_fld%data,"
        "u_fld,u_fld%data,cv_fld,cv_fld%data,v_fld,v_fld%data,unew_fld,"
        "unew_fld%data,uold_fld,uold_fld%data)\n" in code)

    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_acc_data_grid_copyin(tmpdir):
    ''' Test that we correctly generate the arguments to the copyin
    clause of an OpenACC data region when grid properties are required '''
    psy, invoke = get_invoke("single_invoke_grid_props.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    accpt = ACCParallelTrans()
    accdt = ACCEnterDataTrans()

    # Put each loop within an OpenACC parallel region
    for child in schedule.children:
        if isinstance(child, Loop):
            accpt.apply(child)

    # Create a data region for the whole schedule
    accdt.apply(schedule)
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
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_acc_data_parallel_commute(tmpdir):
    '''Test that we can apply the OpenACC parallel and data
    transformations in either order'''
    accpt = ACCParallelTrans()
    accdt = ACCEnterDataTrans()

    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    # Put each loop within an OpenACC parallel region
    for child in schedule.children:
        if isinstance(child, Loop):
            accpt.apply(child)

    # Create a data region for the whole schedule
    accdt.apply(schedule)
    code1 = str(psy.gen)

    # Repeat these transformations but create the region
    # before the parallel loops
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    # Create a data region for the whole schedule
    accdt.apply(schedule)

    # Put each loop within an OpenACC parallel region
    for child in schedule.children:
        if isinstance(child, Loop):
            accpt.apply(child)

    code2 = str(psy.gen)

    assert code1 == code2
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_accdata_duplicate():
    ''' Check that we raise an error if we attempt to add an OpenACC
    data directive to a schedule that already contains one '''
    accdt = ACCEnterDataTrans()
    accpt = ACCParallelTrans()

    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                           dist_mem=False)
    schedule = invoke.schedule

    # Create a data region for the whole schedule
    accdt.apply(schedule)

    # Put each loop within an OpenACC parallel region
    for child in schedule.children:
        if isinstance(child, Loop):
            accpt.apply(child)

    # Erroneously attempt to add a data region for the second time
    with pytest.raises(TransformationError):
        accdt.apply(schedule)


def test_accloop(tmpdir, fortran_writer):
    ''' Tests that we can apply a '!$acc loop' directive to a loop '''
    acclpt = ACCLoopTrans()
    accpara = ACCParallelTrans()
    accdata = ACCEnterDataTrans()

    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    with pytest.raises(TransformationError) as err:
        _ = acclpt.apply(schedule)
    assert ("Target of ACCLoopTrans transformation must be a sub-class of "
            "Loop but got 'GOInvokeSchedule'" in str(err.value))

    # Apply an OpenACC loop directive to each loop
    for child in schedule.children:
        if isinstance(child, Loop):
            acclpt.apply(child)

    # Code generation should fail at this point because there's no
    # enclosing parallel region
    with pytest.raises(GenerationError) as err:
        _ = fortran_writer(psy.container)
    assert ("ACCLoopDirective must have an ACCParallelDirective or "
            "ACCKernelsDirective as an ancestor in the Schedule" in
            str(err.value))

    # Add an enclosing parallel region
    accpara.apply(schedule.children)

    # Code generation should still fail because there's no 'enter data'
    # directive and we need one for the parallel region to work
    with pytest.raises(GenerationError) as err:
        _ = fortran_writer(psy.container)
    assert ("An ACC parallel region must either be preceded by an ACC enter "
            "data directive or enclosed within an ACC data region but in "
            "'invoke_0' this is not the case." in str(err.value))

    # Add a data region
    accdata.apply(schedule)

    gen = fortran_writer(psy.container)
    assert '''\
            !$acc parallel default(present)
            !$acc loop independent
            do j = cu_fld%internal%ystart, cu_fld%internal%ystop, 1''' in gen
    assert ("enddo\n"
            "            !$acc loop independent\n"
            "            do j = cv_fld%internal%ystart, cv_fld%internal%ystop"
            ", 1" in gen)
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_acc_loop_not_within_data_region():
    ''' Test the check that an OpenACC loop is within a data region works
    when there is a data region, but not around the loop in question. '''
    acclpt = ACCLoopTrans()
    accpara = ACCParallelTrans()
    accstaticdata = ACCDataTrans()

    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    # Apply ACCLoopTrans to just the second loop
    acclpt.apply(schedule[1])
    # Add an enclosing parallel region
    accpara.apply(schedule[1])

    # Add a static data region around the wrong loop
    accstaticdata.apply(schedule[2])
    with pytest.raises(GenerationError) as err:
        _ = psy.gen
    assert ("An ACC parallel region must either be preceded by an ACC enter "
            "data directive or enclosed within an ACC data region but in "
            "'invoke_0' this is not the case." in str(err.value))


def test_acc_loop_before_enter_data():
    ''' Test that we refuse to generate code if the enter data directive
    comes after the OpenACC region. '''
    acclpt = ACCLoopTrans()
    accpara = ACCParallelTrans()
    accdata = ACCEnterDataTrans()
    mvtrans = MoveTrans()

    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    # Apply ACCLoopTrans to just the second loop
    acclpt.apply(schedule[1])
    # Add an enclosing parallel region
    accpara.apply(schedule[1])

    # Add a data region. By default, the enter data is always added at the
    # beginning of the Schedule. We must therefore move it in order to trigger
    # the error.
    accdata.apply(schedule)
    mvtrans.apply(schedule[0], schedule[3])

    with pytest.raises(GenerationError) as err:
        _ = psy.gen
    assert ("An ACC parallel region must be preceded by an ACC enter-data "
            "directive but in 'invoke_0' this is not the case." in
            str(err.value))


def test_acc_collapse(tmpdir):
    ''' Tests for the collapse clause to a loop directive '''
    acclpt = ACCLoopTrans()
    accpara = ACCParallelTrans()
    accdata = ACCEnterDataTrans()

    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API,
                             name="invoke_0", dist_mem=False)
    schedule = invoke.schedule
    child = schedule.children[0]

    # Check that we reject non-integer collapse arguments
    with pytest.raises(TransformationError) as err:
        _, _ = acclpt.apply(child, {"collapse": child})
    assert ("The 'collapse' argument must be an integer but got an object "
            "of type" in str(err.value))

    # Check that we reject invalid depths
    with pytest.raises(TransformationError) as err:
        _, _ = acclpt.apply(child, {"collapse": 1})
    assert ("It only makes sense to collapse 2 or more loops but got a "
            "value of 1" in str(err.value))

    # Check that we reject attempts to collapse more loops than we have
    with pytest.raises(TransformationError) as err:
        _, _ = acclpt.apply(child, {"collapse": 3})
    assert ("Cannot apply COLLAPSE(3) clause to a loop nest containing "
            "only 2 loops" in str(err.value))

    # Finally, do something valid and check that we get the correct
    # generated code
    acclpt.apply(child, {"collapse": 2})

    accpara.apply(schedule.children)
    accdata.apply(schedule)

    gen = str(psy.gen)
    assert ("      !$acc parallel default(present)\n"
            "      !$acc loop independent collapse(2)\n"
            "      DO j = cu_fld%internal%ystart, cu_fld%internal%ystop, 1\n"
            "        DO i = cu_fld%internal%xstart, cu_fld%internal%xstop, 1\n"
            "          CALL compute_cu_code(i, j, cu_fld%data, p_fld%data, "
            "u_fld%data)\n" in gen)
    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_acc_indep(tmpdir):
    ''' Tests for the independent clause to a loop directive. '''
    acclpt = ACCLoopTrans()
    accpara = ACCParallelTrans()
    accdata = ACCEnterDataTrans()

    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API,
                             name="invoke_0", dist_mem=False)
    schedule = invoke.schedule
    acclpt.apply(schedule.children[0], {"independent": False})
    acclpt.apply(schedule.children[1], {"independent": True})
    accpara.apply(schedule.children)
    accdata.apply(schedule)
    # Check the generated code
    gen = str(psy.gen)
    assert "!$acc loop\n      DO j = cu_fld%internal%ystart," in gen
    assert "!$acc loop independent\n      DO j = cv_fld%internal%ystart" in gen

    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_acc_loop_seq():
    ''' Check that we can apply the sequential clause to an ACC LOOP
    directive. '''
    acclpt = ACCLoopTrans()
    accpara = ACCParallelTrans()
    accdata = ACCEnterDataTrans()
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API,
                             name="invoke_0", dist_mem=False)
    schedule = invoke.schedule
    acclpt.apply(schedule.children[0], {"sequential": True})
    accpara.apply(schedule.children)
    accdata.apply(schedule)
    # Check the generated code
    gen = str(psy.gen).lower()
    assert ("      !$acc parallel default(present)\n"
            "      !$acc loop seq\n"
            "      do j = cu_fld%internal%ystart, cu_fld%internal%ystop"
            ", 1\n" in gen)


def test_acc_loop_view(capsys):
    ''' Test for the view() method of ACCLoopDirective. '''
    acclpt = ACCLoopTrans()

    _, invoke = get_invoke("single_invoke_three_kernels.f90", API,
                           name="invoke_0", dist_mem=False)
    schedule = invoke.schedule
    acclpt.apply(schedule.children[0], {"independent": False})
    acclpt.apply(schedule.children[1], {"independent": True})
    acclpt.apply(schedule.children[2], {"sequential": True})
    # Check the view method
    schedule.view()
    output, _ = capsys.readouterr()
    assert "[ACC Loop]" in output
    assert "[ACC Loop, independent]" in output
    assert "[ACC Loop, seq]" in output


def test_acc_kernels_error():
    ''' Check that we refuse to allow the kernels transformation
    for this API. '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API,
                           name="invoke_0", dist_mem=False)
    schedule = invoke.schedule
    accktrans = ACCKernelsTrans()
    with pytest.raises(NotImplementedError) as err:
        _, _ = accktrans.apply(schedule.children)
    assert ("kernels regions are currently only supported for the nemo"
            " and dynamo0.3 front-ends" in str(err.value))


def test_all_go_loop_trans_base_validate(monkeypatch):
    ''' Check that all GOcean transformations that sub-class LoopTrans call the
    base validate() method. '''
    # First get a valid Loop object that we can pass in.
    _, invoke = get_invoke("test27_loop_swap.f90", "gocean1.0", idx=1,
                           dist_mem=False)
    loop = invoke.schedule.walk(Loop)[0]

    # GOcean-specific transformations
    transmod = import_module("psyclone.domain.gocean.transformations")
    all_trans_classes = inspect.getmembers(transmod, inspect.isclass)

    # To ensure that we identify that the validate() method in the LoopTrans
    # base class has been called, we monkeypatch it to raise an exception.

    def fake_validate(_1, _2, options=None):
        raise NotImplementedError("validate test exception")
    monkeypatch.setattr(LoopTrans, "validate", fake_validate)

    for name, cls_type in all_trans_classes:
        trans = cls_type()
        if isinstance(trans, LoopTrans):
            with pytest.raises(NotImplementedError) as err:
                if isinstance(trans, LoopFuseTrans):
                    trans.validate(loop, loop)
                else:
                    trans.validate(loop)
            assert "validate test exception" in str(err.value), \
                "{0}.validate() does not call LoopTrans.validate()".format(
                    name)
