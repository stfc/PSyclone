# -------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -------------------------------------------------------------------------
# Authors R. Ford and A. R. Porter, STFC Daresbury Lab

''' Module containing tests of Transformations when using the
    GOcean 1.0 API '''

from __future__ import absolute_import
import os
import pytest
from psyclone.parse import parse
from psyclone.psyGen import PSyFactory
from psyclone.transformations import TransformationError, \
    GOConstLoopBoundsTrans, LoopFuseTrans, OMPParallelTrans, \
    GOceanOMPParallelLoopTrans,\
    GOceanOMPLoopTrans, KernelModuleInlineTrans, GOceanLoopFuseTrans
from psyclone.generator import GenerationError
from utils import count_lines

# The version of the PSyclone API that the tests in this file
# exercise
API = "gocean1.0"


def get_invoke(algfile, idx):
    ''' Utility method to get the idx'th invoke from the algorithm
    specified in file '''
    _, info = parse(os.path.
                    join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "gocean1p0",
                         algfile),
                    api=API)
    psy = PSyFactory(API).create(info)
    invokes = psy.invokes
    # invokes does not have a method by which to request the i'th
    # in the list so we do this rather clumsy lookup of the name
    # of the invoke that we want
    invoke = invokes.get(invokes.names[idx])
    return psy, invoke


def test_const_loop_bounds_not_schedule():
    ''' Check that we raise an error if we attempt to apply the
    constant loop-bounds transformation to something that is
    not a Schedule '''
    _, invoke = get_invoke("test11_different_iterates_over_"
                           "one_invoke.f90", 0)
    schedule = invoke.schedule
    cbtrans = GOConstLoopBoundsTrans()

    with pytest.raises(TransformationError):
        _, _ = cbtrans.apply(schedule.children[0])


def test_const_loop_bounds_toggle():
    ''' Check that we can toggle constant loop bounds on and off and
    that the default behaviour is "on" '''
    psy, invoke = get_invoke("test11_different_iterates_over_"
                             "one_invoke.f90", 0)
    schedule = invoke.schedule
    cbtrans = GOConstLoopBoundsTrans()

    # First check that the generated code uses constant loop
    # bounds by default
    gen = str(psy.gen)

    assert "INTEGER istop, jstop" in gen
    assert "istop = cv_fld%grid%simulation_domain%xstop" in gen
    assert "jstop = cv_fld%grid%simulation_domain%ystop" in gen
    assert "DO j=2,jstop-1" in gen
    assert "DO i=2,istop" in gen

    # Next, check that applying the constant loop-bounds
    # transformation has no effect (in this case)
    newsched, _ = cbtrans.apply(schedule)
    invoke.schedule = newsched
    # Store the generated code as a string
    gen = str(psy.gen)

    assert "INTEGER istop, jstop" in gen
    assert "istop = cv_fld%grid%simulation_domain%xstop" in gen
    assert "jstop = cv_fld%grid%simulation_domain%ystop" in gen
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
    psy, invoke = get_invoke("test26_const_bounds_invalid_offset.f90", 0)
    cbtrans = GOConstLoopBoundsTrans()
    schedule = invoke.schedule
    newsched, _ = cbtrans.apply(schedule, const_bounds=True)
    invoke.schedule = newsched
    with pytest.raises(GenerationError):
        _ = psy.gen


def test_loop_fuse_different_iterates_over():
    ''' Test that an appropriate error is raised when we attempt to
    fuse two loops that have differing values of ITERATES_OVER '''
    _, invoke = get_invoke("test11_different_iterates_over_"
                           "one_invoke.f90", 0)
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
    _, invoke = get_invoke("test14_module_inline_same_kernel.f90", 0)
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
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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
    _, invoke = get_invoke("single_invoke_three_kernels.f90", 0)

    ompr = OMPParallelTrans()

    with pytest.raises(TransformationError):
        _, _ = ompr.apply(invoke)


def test_omp_region_with_single_loop():
    ''' Test that we can pass the OpenMP PARALLEL region transformation
        a single node in a schedule '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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


def test_omp_region_no_slice():
    ''' Test that we can pass the OpenMP PARALLEL region transformation
    a list of nodes specified as node.children '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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

    psy, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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
    psy, invoke = get_invoke("single_invoke_two_kernels.f90", 0)
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
    psy, invoke = get_invoke("single_invoke_two_kernels.f90", 0)
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
    psy, invoke = get_invoke("single_invoke_two_kernels.f90", 0)
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
    psy, invoke = get_invoke("single_invoke_two_kernels.f90", 0)
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
    psy, invoke = get_invoke("single_invoke_two_kernels.f90", 0)
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
    psy, invoke = get_invoke("single_invoke_two_kernels.f90", 0)
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
    _, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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
    _, invoke1 = get_invoke("test12_two_invokes_two_kernels.f90", 0)
    schedule1 = invoke1.schedule
    _, invoke2 = get_invoke("test12_two_invokes_two_kernels.f90", 1)
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
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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
    _, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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
    _, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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
    _, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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
    _, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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
    _, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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
    _, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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
    psy, _ = get_invoke("single_invoke_three_kernels.f90", 0)
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
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
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
    _, invoke = get_invoke("single_invoke_three_kernels.f90", 0)
    schedule = invoke.schedule
    kern_call = schedule.children[0].children[0]
    inline_trans = KernelModuleInlineTrans()
    with pytest.raises(TransformationError):
        _, _ = inline_trans.apply(kern_call)


def test_module_inline_with_sub_use():
    ''' Test that we can module inline a kernel subroutine which
    contains a use statement'''
    psy, invoke = get_invoke("single_invoke_scalar_int_arg.f90", 0)
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
    psy, invoke = get_invoke("test14_module_inline_same_kernel.f90", 0)
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
    _, invoke = get_invoke("test14_module_inline_same_kernel.f90", 0)
    schedule = invoke.schedule
    kern_call = schedule.children[0].children[0].children[0]
    inline_trans = KernelModuleInlineTrans()
    _, _ = inline_trans.apply(kern_call, inline=False)
