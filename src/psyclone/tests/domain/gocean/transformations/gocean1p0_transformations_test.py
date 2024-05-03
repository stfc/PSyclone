# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
# Modified work Copyright (c) 2017-2024 by J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, Met Office

''' Module containing tests of Transformations when using the
    GOcean 1.0 API '''

import re
import inspect
from importlib import import_module
import pytest
from psyclone.configuration import Config
from psyclone.domain.gocean.transformations import GOceanLoopFuseTrans
from psyclone.errors import GenerationError
from psyclone.gocean1p0 import GOKern
from psyclone.psyGen import Kern
from psyclone.psyir.nodes import Loop, Routine
from psyclone.psyir.transformations import LoopFuseTrans, LoopTrans, \
    TransformationError
from psyclone.transformations import ACCKernelsTrans, ACCRoutineTrans, \
    OMPParallelTrans, GOceanOMPParallelLoopTrans, GOceanOMPLoopTrans, \
    OMPLoopTrans, ACCParallelTrans, ACCEnterDataTrans, ACCLoopTrans
from psyclone.domain.gocean.transformations import GOConstLoopBoundsTrans
from psyclone.tests.gocean_build import GOceanBuild
from psyclone.tests.utilities import count_lines, get_invoke

# The version of the PSyclone API that the tests in this file
# exercise
API = "gocean1.0"


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use gocean1.0 as API.'''
    Config.get().api = "gocean1.0"
    yield
    Config._instance = None


def test_loop_fuse_error():
    ''' Test that we catch various errors when loop fusing '''
    _, invoke = get_invoke("test14_module_inline_same_kernel.f90", API, idx=0,
                           dist_mem=False)
    schedule = invoke.schedule

    lftrans = GOceanLoopFuseTrans()
    assert str(lftrans) == "Fuse two adjacent loops together with " \
                           "GOcean-specific validity checks"

    # Apply loop fuse, but the first node is not a loop:
    with pytest.raises(TransformationError) as err:
        lftrans.apply(schedule.children[0].children[0], schedule.children[1])
    assert "Both nodes must be of the same GOLoop class." in str(err.value)

    # Also check that we catch this for the second argument:
    with pytest.raises(TransformationError) as err:
        lftrans.apply(schedule.children[0], schedule.children[1].children[0])
    assert "Both nodes must be of the same GOLoop class." in str(err.value)

    # Also check if they have different field_spaces
    schedule.children[1].field_space = "go_cv"
    with pytest.raises(TransformationError) as err:
        lftrans.apply(schedule.children[0], schedule.children[1])
    assert ("Cannot fuse loops that are over different grid-point types: "
            "go_cu and go_cv" in str(err.value))

    # Also check when one of them is a Loop but not of the GOcean API
    loop = schedule.children[1]
    loop.replace_with(Loop())
    with pytest.raises(TransformationError) as err:
        lftrans.apply(schedule.children[0], schedule.children[1])
    assert "Both nodes must be of the same GOLoop class." in str(err.value)


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
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_omp_region_with_wrong_arg_type():
    ''' Test that the OpenMP PARALLEL region transformation
        raises an appropriate error if passed something that is not
        a list of Nodes or a single Node. '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                           dist_mem=False)

    ompr = OMPParallelTrans()

    with pytest.raises(TransformationError):
        ompr.apply(invoke)


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
    assert GOceanBuild(tmpdir).code_compiles(psy)


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
    assert GOceanBuild(tmpdir).code_compiles(psy)


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
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_omp_region_no_slice_const_bounds(tmpdir):
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
    assert GOceanBuild(tmpdir).code_compiles(psy)


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
    assert GOceanBuild(tmpdir).code_compiles(psy)


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
    assert GOceanBuild(tmpdir).code_compiles(psy)


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
    assert GOceanBuild(tmpdir).code_compiles(psy)


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
    assert GOceanBuild(tmpdir).code_compiles(psy)


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
    assert GOceanBuild(tmpdir).code_compiles(psy)


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
    assert GOceanBuild(tmpdir).code_compiles(psy)


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
    ompl.apply(schedule.children[0])

    # Attempt to put an OpenMP parallel region around that same loop
    # (which is now a child of an OpenMP loop directive) and the
    # second loop in the schedule
    with pytest.raises(TransformationError):
        ompr.apply([schedule.children[0].children[0], schedule.children[1]])


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
        ompr.apply([schedule1.children[0], schedule2.children[0]])


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
        ompl.apply(schedule.children[0])
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
        ompl.apply(schedule.children[0])


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
    ompl.apply(schedule.children[1])

    # Now attempt to put a parallel region inside that parallel do
    with pytest.raises(TransformationError) as err:
        ompr.apply([schedule.children[1].children[0]])
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
    assert GOceanBuild(tmpdir).code_compiles(psy)


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
    assert GOceanBuild(tmpdir).code_compiles(psy)


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
    assert GOceanBuild(tmpdir).code_compiles(psy)


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
    assert GOceanBuild(tmpdir).code_compiles(psy)


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
    assert GOceanBuild(tmpdir).code_compiles(psy)


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
    assert GOceanBuild(tmpdir).code_compiles(psy)


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
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_acc_parallel_not_a_loop():
    ''' Test that we raise an appropriate error if we attempt
    to apply the OpenACC Parallel transformation to something that
    is not a loop '''

    acct = ACCParallelTrans()
    # Provide an invalid node type (just the integer 1) to the OpenACC
    # Parallel transformation
    with pytest.raises(TransformationError) as error:
        acct.apply(1)

    assert "Argument must be a single Node in a Schedule, a Schedule or a " \
           "list of Nodes in a Schedule but have been passed an object " \
           "of type:" in str(error.value)
    # Python2/3 differences in type string
    assert "'int'>" in str(error.value)


def test_acc_parallel_trans(tmpdir):
    ''' Test that we can apply an OpenACC parallel transformation
    to a loop '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    # Apply the OpenACC Parallel transformation to the first loop of the
    # schedule
    acct = ACCParallelTrans()
    acct.apply(schedule.children[0])

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
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_acc_parallel_trans_dm():
    ''' Test that the OpenACC parallel transform works correctly when
    distributed memory is enabled. '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=True)
    schedule = invoke.schedule
    acct = ACCParallelTrans()
    accdt = ACCEnterDataTrans()
    # Applying the transformation to the whole schedule should fail because
    # of the HaloExchange nodes.
    with pytest.raises(TransformationError) as err:
        acct.apply(schedule.children)
    assert ("Nodes of type 'GOHaloExchange' cannot be enclosed by a "
            "ACCParallelTrans transformation" in str(err.value))
    acct.apply(schedule.children[1:])
    # Add an enter-data region.
    accdt.apply(schedule)
    code = str(psy.gen)
    # Check that the start of the parallel region is in the right place.
    assert ("      !$acc parallel default(present)\n"
            "      DO j = cu_fld%internal%ystart, cu_fld%internal%ystop, 1\n"
            in code)
    # Check that the end parallel is generated correctly.
    assert ("        END DO\n"
            "      END DO\n"
            "      !$acc end parallel\n\n"
            "    END SUBROUTINE invoke_0\n" in code)


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
        acct.apply([schedule.children[1], schedule.children[0]])

    assert "Children are not consecutive children" in str(err.value)

    with pytest.raises(TransformationError) as err:
        acct.apply([schedule.children[0].children[0], schedule.children[0]])

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
        acct.apply(schedule.children[0])
    assert ("Cannot apply an OpenACC enter data directive to something that "
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
        accpara.apply(schedule.children[0])
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
        "      !$acc enter data copyin(cu_fld,cu_fld%data,cv_fld,cv_fld%data,"
        "p_fld,p_fld%data,u_fld,u_fld%data,unew_fld,unew_fld%data,"
        "uold_fld,uold_fld%data,v_fld,v_fld%data)\n" in code)

    assert GOceanBuild(tmpdir).code_compiles(psy)


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

    # TODO GOcean grid properties are duplicated in this set under
    # different names (the OpenACC deep copy support should spot this).
    pcopy = ("!$acc enter data copyin(cu_fld,cu_fld%data,d_fld,d_fld%data,"
             "d_fld%grid,d_fld%grid%area_t,d_fld%grid%area_u,d_fld%grid%tmask,"
             "du_fld,du_fld%data,u_fld,u_fld%data,u_fld%grid,"
             "u_fld%grid%area_t,u_fld%grid%area_u,u_fld%grid%tmask)")
    assert pcopy in code
    # Check that we flag that the fields are now on the device
    for obj in ["u_fld", "cu_fld", "du_fld", "d_fld"]:
        assert f"{obj}%data_on_device = .true." in code
    # Check that we have no acc_update_device calls
    assert "CALL acc_update_device" not in code
    assert GOceanBuild(tmpdir).code_compiles(psy)


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
    assert GOceanBuild(tmpdir).code_compiles(psy)


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
        acclpt.apply(schedule)
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
    assert ("ACCLoopDirective in routine 'invoke_0' must either have an "
            "ACCParallelDirective or ACCKernelsDirective as an ancestor in "
            "the Schedule or the routine must contain an ACCRoutineDirective"
            in str(err.value))

    # Add an enclosing parallel region
    accpara.apply(schedule.children)

    # Add a data region
    accdata.apply(schedule)

    gen = fortran_writer(psy.container)
    assert '''\
        !$acc parallel default(present)
        !$acc loop independent
        do j = cu_fld%internal%ystart, cu_fld%internal%ystop, 1''' in gen
    assert ("enddo\n"
            "        !$acc loop independent\n"
            "        do j = cv_fld%internal%ystart, cv_fld%internal%ystop"
            ", 1" in gen)
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_acc_enter_directive_infrastructure_setup():
    ''' Test that the GOcean-specific OpenACC EnterData directive also sets
    up the necessary GOcean infrastructure to keep track and update the
    data allocated on the device. '''

    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    acclpt = ACCLoopTrans()
    accpara = ACCParallelTrans()
    accdata = ACCEnterDataTrans()

    # Apply ACCLoopTrans to just the second loop
    acclpt.apply(schedule[1])
    # Add an enclosing parallel region
    accpara.apply(schedule[1])
    # Add a data region. This directive will set-up the necessary GOcean
    # infrastructure device pointers
    accdata.apply(schedule)

    # Generate the code
    gen = str(psy.gen)

    # Check that the read_from_device routine has been generated
    expected = """\
    SUBROUTINE read_from_device(from, to, startx, starty, nx, ny, blocking)
      USE iso_c_binding, ONLY: c_ptr
      USE kind_params_mod, ONLY: go_wp
      TYPE(c_ptr), intent(in) :: from
      REAL(KIND=go_wp), DIMENSION(:, :), INTENT(INOUT), TARGET :: to
      INTEGER, intent(in) :: startx
      INTEGER, intent(in) :: starty
      INTEGER, intent(in) :: nx
      INTEGER, intent(in) :: ny
      LOGICAL, intent(in) :: blocking

      !$acc update host(to)

    END SUBROUTINE read_from_device"""
    assert expected in gen

    # Check that the routine has been introduced to the tree (with the
    # appropriate tag) and only once (even if there are 3 fields)
    symbol = schedule.symbol_table.lookup_with_tag("openacc_read_func")
    assert symbol.name == "read_from_device"
    assert len(schedule.parent.children) == 2
    assert isinstance(schedule.parent.children[1], Routine)
    assert schedule.parent.children[1].name == symbol.name
    count = count_lines(gen, "SUBROUTINE read_from_device(")
    assert count == 1

    # Check that each field data_on_device and read_from_device_f have been
    # initialised
    for field in ["cv_fld", "p_fld", "v_fld"]:
        assert f"{field}%data_on_device = .true.\n" in gen
        assert f"{field}%read_from_device_f => read_from_device\n" in gen


def test_acc_enter_directive_infrastructure_setup_error():
    ''' Test that the GOcean-specific OpenACC EnterData directive also sets
    up the necessary GOcean infrastructure to keep track and update the
    data allocated on the device. '''

    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API, idx=0,
                             dist_mem=False)
    schedule = invoke.schedule

    acclpt = ACCLoopTrans()
    accpara = ACCParallelTrans()
    accdata = ACCEnterDataTrans()

    # Apply ACCLoopTrans to just the second loop
    acclpt.apply(schedule[1])
    # Add an enclosing parallel region
    accpara.apply(schedule[1])
    # Add a data region. This directive will set-up the necessary GOcean
    # infrastructure device pointers
    accdata.apply(schedule)

    # Remove the InvokeSchedule from its Container so that OpenACC will not
    # find where to add the read_from_device function.
    schedule.detach()

    # Generate the code
    with pytest.raises(GenerationError) as err:
        _ = psy.gen
    assert ("The GOACCEnterDataDirective can only be generated/lowered inside "
            "a Container in order to insert a sibling subroutine, but "
            "'GOACCEnterDataDirective[]' is not inside a Container."
            in str(err.value))


def test_acc_collapse(tmpdir):
    ''' Tests for the collapse clause to a loop directive. '''
    acclpt = ACCLoopTrans()
    accpara = ACCParallelTrans()
    accdata = ACCEnterDataTrans()

    psy, invoke = get_invoke("single_invoke_three_kernels.f90", API,
                             name="invoke_0", dist_mem=False)
    schedule = invoke.schedule
    child = schedule.children[0]

    # Apply with valid options and check that we get the correct
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
    assert GOceanBuild(tmpdir).code_compiles(psy)


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

    assert GOceanBuild(tmpdir).code_compiles(psy)


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


def test_acc_kernels_error():
    ''' Check that we refuse to allow the kernels transformation
    for this API. '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", API,
                           name="invoke_0", dist_mem=False)
    schedule = invoke.schedule
    accktrans = ACCKernelsTrans()
    with pytest.raises(NotImplementedError) as err:
        accktrans.apply(schedule.children)
    assert ("kernels regions are currently only supported for the nemo"
            " and dynamo0.3 front-ends" in str(err.value))


def test_accroutinetrans_module_use():
    ''' Check that ACCRoutineTrans rejects a kernel if it contains a module
    use statement. '''
    _, invoke = get_invoke("single_invoke_kern_with_use.f90", api="gocean1.0",
                           idx=0)
    sched = invoke.schedule
    kernels = sched.walk(Kern)
    rtrans = ACCRoutineTrans()
    with pytest.raises(TransformationError) as err:
        rtrans.apply(kernels[0])
    assert ("accesses the symbol 'rdt: Symbol<Import(container='model_mod')>' "
            "which is imported. If this symbol "
            "represents data then it must first" in str(err.value))


def test_accroutinetrans_with_kern(fortran_writer, monkeypatch):
    ''' Test that we can transform a kernel by adding a "!$acc routine"
    directive to it. '''
    _, invoke = get_invoke("nemolite2d_alg_mod.f90", api="gocean1.0", idx=0)
    sched = invoke.schedule
    kern = sched.coded_kernels()[0]
    assert isinstance(kern, GOKern)
    rtrans = ACCRoutineTrans()
    assert rtrans.name == "ACCRoutineTrans"
    rtrans.apply(kern)
    # Check that there is a acc routine directive in the kernel
    code = fortran_writer(kern.get_kernel_schedule())
    assert "!$acc routine\n" in code

    # If the kernel schedule is not accessible, the transformation fails
    def raise_gen_error():
        '''Simple function that raises GenerationError.'''
        raise GenerationError("error")
    monkeypatch.setattr(kern, "get_kernel_schedule", raise_gen_error)
    with pytest.raises(TransformationError) as err:
        rtrans.apply(kern)
    assert ("Failed to create PSyIR for kernel 'continuity_code'. Cannot "
            "transform such a kernel." in str(err.value))


def test_accroutinetrans_with_routine(fortran_writer):
    ''' Test that we can transform a routine by adding a "!$acc routine"
    directive to it. '''
    _, invoke = get_invoke("nemolite2d_alg_mod.f90", api="gocean1.0", idx=0)
    sched = invoke.schedule
    kern = sched.coded_kernels()[0]
    assert isinstance(kern, GOKern)
    rtrans = ACCRoutineTrans()
    assert rtrans.name == "ACCRoutineTrans"
    routine = kern.get_kernel_schedule()
    rtrans.apply(routine)
    # Check that there is a acc routine directive in the routine
    code = fortran_writer(routine)
    assert "!$acc routine\n" in code

    # Even if applied multiple times the Directive is only there once
    previous_num_children = len(routine.children)
    rtrans.apply(routine)
    assert previous_num_children == len(routine.children)


def test_accroutinetrans_with_invalid_node():
    ''' Test that ACCRoutineTrans raises the appropriate error when a node
    that is not a Routine or a Kern is provided.'''
    _, invoke = get_invoke("nemolite2d_alg_mod.f90", api="gocean1.0", idx=0)
    sched = invoke.schedule
    kern = sched[0]
    rtrans = ACCRoutineTrans()
    with pytest.raises(TransformationError) as err:
        rtrans.apply(kern)
    assert ("The ACCRoutineTrans must be applied to a sub-class of Kern or "
            "Routine but got 'GOLoop'." in str(err.value))


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
        if name in ["RaisePSyIR2GOceanKernTrans"]:
            # This transformation requires an argument to the
            # constructor and is not a subclass of LoopTrans so skip.
            continue
        trans = cls_type()
        if isinstance(trans, LoopTrans):
            with pytest.raises(NotImplementedError) as err:
                if isinstance(trans, LoopFuseTrans):
                    trans.validate(loop, loop)
                else:
                    trans.validate(loop)
            assert "validate test exception" in str(err.value), \
                f"{name}.validate() does not call LoopTrans.validate()"
