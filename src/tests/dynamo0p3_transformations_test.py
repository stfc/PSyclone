# -----------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -----------------------------------------------------------------------------
# Author R. Ford and A. R. Porter, STFC Daresbury Lab

''' Tests of transformations with the Dynamo 0.3 API '''

from parse import parse
from psyGen import PSyFactory
from transformations import TransformationError,\
    OMPParallelTrans,\
    Dynamo0p3ColourTrans,\
    Dynamo0p3OMPLoopTrans,\
    DynamoOMPParallelLoopTrans,\
    DynamoLoopFuseTrans
import os
import pytest

# The version of the API that the tests in this file
# exercise.
TEST_API = "dynamo0.3"


def test_colour_trans():
    ''' test of the colouring transformation of a single loop '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_type')
    schedule = invoke.schedule
    ctrans = Dynamo0p3ColourTrans()

    # Colour the loop
    cschedule, _ = ctrans.apply(schedule.children[0])

    # Replace the original loop schedule with the transformed one
    invoke.schedule = cschedule

    # Store the results of applying this code transformation as
    # a string
    gen = str(psy.gen)

    # Check that we're calling the API to get the no. of colours
    assert "f1_proxy%vspace%get_colours(" in gen

    col_loop_idx = -1
    cell_loop_idx = -1
    for idx, line in enumerate(gen.split('\n')):
        if "DO colour=1,ncolour" in line:
            col_loop_idx = idx
        if "DO cell=1,ncp_colour(colour)" in line:
            cell_loop_idx = idx

    assert cell_loop_idx - col_loop_idx == 1

    # Check that we're using the colour map when getting the cell dof maps
    assert "get_cell_dofmap(cmap(colour, cell))" in gen


def test_colouring_not_a_loop():
    ''' Test that we raise an appropriate error if we attempt to
    colour something that is not a loop '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_type')
    schedule = invoke.schedule
    ctrans = Dynamo0p3ColourTrans()

    # Erroneously attempt to colour the schedule rather than the loop
    with pytest.raises(TransformationError):
        _, _ = ctrans.apply(schedule)


def test_omp_name():
    ''' Test the name property of the Dynamo0p3OMPLoopTrans class '''
    olooptrans = Dynamo0p3OMPLoopTrans()
    oname = olooptrans.name
    assert oname == "Dynamo0p3OMPLoopTrans"


def test_omp_str():
    ''' Test the str method of the Dynamo0p3OMPLoopTrans class '''
    olooptrans = Dynamo0p3OMPLoopTrans()
    oname = str(olooptrans)
    assert oname == "Add an OpenMP DO directive to a Dynamo 0.3 loop"


def test_omp_not_a_loop():
    ''' Test that we raise an appropriate error if we attempt to
    apply an OpenMP DO transformation to something that is not a loop '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_type')
    schedule = invoke.schedule
    otrans = Dynamo0p3OMPLoopTrans()

    # Erroneously attempt to apply OpenMP to the schedule rather than
    # the loop
    with pytest.raises(TransformationError):
        _, _ = otrans.apply(schedule)


def test_omp_do_not_over_cells():
    ''' Test that we raise an appropriate error if we attempt to
    apply an OpenMP DO transformation to a loop that is not over cells '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1.4_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_type')
    schedule = invoke.schedule
    otrans = Dynamo0p3OMPLoopTrans()

    with pytest.raises(TransformationError):
        _, _ = otrans.apply(schedule.children[0])


def test_omp_parallel_do_not_over_cells():
    ''' Test that we raise an appropriate error if we attempt to apply
    an OpenMP PARALLEL DO transformation to a loop that is not over cells '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1.4_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_type')
    schedule = invoke.schedule
    otrans = DynamoOMPParallelLoopTrans()

    with pytest.raises(TransformationError):
        _, _ = otrans.apply(schedule.children[0])


def test_omp_parallel_not_a_loop():
    '''Test that we raise an appropriate error if we attempt to apply an
    OpenMP PARALLEL DO transformation to something that is not a loop

    '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_type')
    schedule = invoke.schedule
    otrans = DynamoOMPParallelLoopTrans()

    # Erroneously attempt to apply OpenMP to the schedule rather than
    # the loop
    with pytest.raises(TransformationError):
        _, _ = otrans.apply(schedule)


def test_colour_name():
    ''' Test the name property of the Dynamo0p3ColourTrans class '''
    ctrans = Dynamo0p3ColourTrans()
    cname = ctrans.name
    assert cname == "Dynamo0p3LoopColourTrans"


def test_colour_str():
    ''' Test the str method of the Dynamo0p3ColourTrans class '''
    ctrans = Dynamo0p3ColourTrans()
    cstr = str(ctrans)
    assert cstr == "Split a Dynamo 0.3 loop into colours"


def test_omp_colour_trans():
    ''' Test the OpenMP transformation applied to a coloured loop '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_type')
    schedule = invoke.schedule

    ctrans = Dynamo0p3ColourTrans()
    otrans = DynamoOMPParallelLoopTrans()

    # Colour the loop
    cschedule, _ = ctrans.apply(schedule.children[0])

    # Then apply OpenMP to the inner loop
    schedule, _ = otrans.apply(cschedule.children[0].children[0])

    invoke.schedule = schedule
    code = str(psy.gen)

    col_loop_idx = -1
    omp_idx = -1
    cell_loop_idx = -1
    for idx, line in enumerate(code.split('\n')):
        if "DO colour=1,ncolour" in line:
            col_loop_idx = idx
        if "DO cell=1,ncp_colour(colour)" in line:
            cell_loop_idx = idx
        if "!$omp parallel do" in line:
            omp_idx = idx

    assert cell_loop_idx - omp_idx == 1
    assert omp_idx - col_loop_idx == 1

    # Check that the list of private variables is correct
    assert "private(cell,map_w1,map_w2,map_w3)" in code


def test_omp_colour_orient_trans():
    ''' Test the OpenMP transformation applied to a coloured loop
        when the kernel expects orientation information '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "9_orientation.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_orientation_type')
    schedule = invoke.schedule

    ctrans = Dynamo0p3ColourTrans()
    otrans = DynamoOMPParallelLoopTrans()

    # Colour the loop
    cschedule, _ = ctrans.apply(schedule.children[0])

    # Then apply OpenMP to the inner loop
    schedule, _ = otrans.apply(cschedule.children[0].children[0])

    invoke.schedule = schedule
    code = str(psy.gen)

    # Check that we're using the colour map when getting the orientation
    assert "get_cell_orientation(cmap(colour, cell))" in code

    # Check that the list of private variables is correct
    assert "private(cell,map_w3,map_w2,map_w0,orientation_w2)" in code


def test_omp_parallel_colouring_needed():
    '''Test that we raise an error when applying an OpenMP PARALLEL DO
    transformation to a loop that requires colouring (i.e. has a field
    with 'INC' access) but is not coloured.

    '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "11_any_space.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_any_space_1_type')
    schedule = invoke.schedule

    otrans = DynamoOMPParallelLoopTrans()
    # Apply OpenMP to the loop
    with pytest.raises(TransformationError):
        schedule, _ = otrans.apply(schedule.children[0])


def test_omp_colouring_needed():
    '''Test that we raise an error when applying an OpenMP DO
    transformation to a loop that requires colouring (i.e. has a field
    with 'INC' access) but is not coloured.

    '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "11_any_space.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_any_space_1_type')
    schedule = invoke.schedule

    otrans = Dynamo0p3OMPLoopTrans()
    # Apply OpenMP to the loop
    with pytest.raises(TransformationError):
        schedule, _ = otrans.apply(schedule.children[0])


def test_check_seq_colours_omp_parallel_do():
    '''Test that we raise an error if the user attempts to apply an
    OpenMP PARALLEL DO transformation to a loop over colours (since
    any such loop must be sequential)

    '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "9_orientation.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_orientation_type')
    schedule = invoke.schedule

    ctrans = Dynamo0p3ColourTrans()
    otrans = DynamoOMPParallelLoopTrans()

    # Colour the loop
    cschedule, _ = ctrans.apply(schedule.children[0])

    # Then erroneously attempt to apply OpenMP to the loop over
    # colours
    with pytest.raises(TransformationError):
        schedule, _ = otrans.apply(cschedule.children[0])


def test_check_seq_colours_omp_do():
    '''Test that we raise an error if the user attempts to apply an OpenMP
    DO transformation to a loop over colours (since any such loop must
    be sequential)

    '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "9_orientation.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_orientation_type')
    schedule = invoke.schedule

    ctrans = Dynamo0p3ColourTrans()
    otrans = Dynamo0p3OMPLoopTrans()

    # Colour the loop
    cschedule, _ = ctrans.apply(schedule.children[0])

    # Then erroneously attempt to apply OpenMP to the loop over
    # colours
    with pytest.raises(TransformationError):
        schedule, _ = otrans.apply(cschedule.children[0])


def test_colouring_after_openmp():
    ''' Test that we raise an error if the user attempts to
    colour a loop that is already within an OpenMP parallel region '''
    # For this test we must use a kernel that doesn't actually require
    # colouring as otherwise PSyclone won't let us apply the OpenMP
    # transformation first!
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "9_orientation.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_orientation_type')
    schedule = invoke.schedule

    ctrans = Dynamo0p3ColourTrans()
    otrans = DynamoOMPParallelLoopTrans()

    # Apply OpenMP to the loop
    schedule, _ = otrans.apply(schedule.children[0])

    # Now attempt to colour the loop within this OpenMP region
    with pytest.raises(TransformationError):
        schedule, _ = ctrans.apply(schedule.children[0].children[0])


def test_colouring_multi_kernel():
    ''' Test that we correctly generate all the map-lookups etc.
    when an invoke contains more than one kernel '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4.6_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0')
    schedule = invoke.schedule

    ctrans = Dynamo0p3ColourTrans()
    otrans = DynamoOMPParallelLoopTrans()

    for child in schedule.children:
        newsched, _ = ctrans.apply(child)

    # Apply OpenMP to each of the colour loops
    schedule = newsched
    for child in schedule.children:
        newsched, _ = otrans.apply(child.children[0])

    invoke.schedule = newsched
    gen = str(psy.gen)

    # Check that we're calling the API to get the no. of colours
    assert "a_proxy%vspace%get_colours(" in gen
    assert "f_proxy%vspace%get_colours(" in gen
    assert "private(cell,map_w2,map_w3,map_w0)" in gen


def test_omp_region_omp_do():
    ''' Test that we correctly generate code for the case of a single
    OMP DO within an OMP PARALLEL region without colouring '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_type')
    schedule = invoke.schedule
    olooptrans = Dynamo0p3OMPLoopTrans()
    ptrans = OMPParallelTrans()

    # Put an OMP PARALLEL around this loop
    child = schedule.children[0]
    oschedule, _ = ptrans.apply(child)

    # Put an OMP DO around this loop
    schedule, _ = olooptrans.apply(oschedule.children[0].children[0])

    # Replace the original loop schedule with the transformed one
    invoke.schedule = schedule

    # Store the results of applying this code transformation as
    # a string
    code = str(psy.gen)

    omp_do_idx = -1
    omp_para_idx = -1
    cell_loop_idx = -1
    for idx, line in enumerate(code.split('\n')):
        if "DO cell=1,f1_proxy%vspace%get_ncell()" in line:
            cell_loop_idx = idx
        if "!$omp do" in line:
            omp_do_idx = idx
        if "!$omp parallel default" in line:
            omp_para_idx = idx

    assert (omp_do_idx - omp_para_idx) == 1
    assert (cell_loop_idx - omp_do_idx) == 1


def test_multi_kernel_single_omp_region():
    ''' Test that we correctly generate all the map-lookups etc.
    when an invoke contains more than one kernel that are all contained
    within a single OMP region '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0')
    schedule = invoke.schedule

    otrans = Dynamo0p3OMPLoopTrans()
    rtrans = OMPParallelTrans()

    # Apply OpenMP to each of the loops
    for child in schedule.children:
        newsched, _ = otrans.apply(child)

    # Enclose all of these OpenMP'd loops within a single region
    newsched, _ = rtrans.apply(newsched.children)

    invoke.schedule = newsched
    code = str(psy.gen)

    omp_do_idx = -1
    omp_end_do_idx = -1
    omp_para_idx = -1
    omp_end_para_idx = -1
    cell_loop_idx = -1
    for idx, line in enumerate(code.split('\n')):
        if (cell_loop_idx == -1) and\
           ("DO cell=1,f1_proxy%vspace%get_ncell()" in line):
            cell_loop_idx = idx
        if (omp_do_idx == -1) and ("!$omp do" in line):
            omp_do_idx = idx
        if "!$omp end do" in line:
            omp_end_do_idx = idx
        if "!$omp parallel default(shared), " +\
           "private(cell,map_w1,map_w2,map_w3)" in line:
            omp_para_idx = idx
        if "!$omp end parallel" in line:
            omp_end_para_idx = idx

    assert (omp_do_idx - omp_para_idx) == 1
    assert (cell_loop_idx - omp_do_idx) == 1
    assert (omp_end_para_idx - omp_end_do_idx) == 1


def test_loop_fuse_different_spaces():
    ''' Test that we raise an appropriate error if the user attempts
    to fuse loops that are on different spaces '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4.7_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0')
    schedule = invoke.schedule

    ftrans = DynamoLoopFuseTrans()

    with pytest.raises(TransformationError):
        _, _ = ftrans.apply(schedule.children[0],
                            schedule.children[1])


def test_loop_fuse():
    ''' Test that we are able to fuse two loops together '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0')
    schedule = invoke.schedule

    ftrans = DynamoLoopFuseTrans()

    # Fuse the loops
    nchildren = len(schedule.children)
    idx = 1
    fschedule = schedule
    while idx < nchildren:
        fschedule, _ = ftrans.apply(fschedule.children[idx-1],
                                    fschedule.children[idx])
        idx += 1

    invoke.schedule = fschedule
    gen = str(psy.gen)

    cell_loop_idx = -1
    end_loop_idx = -1
    call_idx1 = -1
    call_idx2 = -1
    for idx, line in enumerate(gen.split('\n')):
        if "DO cell=1,f1_proxy%vspace%get_ncell()" in line:
            cell_loop_idx = idx
        if "CALL testkern_code" in line:
            if call_idx1 == -1:
                call_idx1 = idx
            else:
                call_idx2 = idx
        if "END DO" in line:
            end_loop_idx = idx

    assert cell_loop_idx != -1
    assert cell_loop_idx < call_idx1
    assert call_idx1 < call_idx2
    assert call_idx2 < end_loop_idx


def test_loop_fuse_omp():
    '''Test that we can loop-fuse two loop nests and enclose them in an
       OpenMP parallel region'''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0')
    schedule = invoke.schedule

    ftrans = DynamoLoopFuseTrans()
    otrans = DynamoOMPParallelLoopTrans()

    # Fuse the loops
    nchildren = len(schedule.children)
    idx = 1
    fschedule = schedule
    while idx < nchildren:
        fschedule, _ = ftrans.apply(fschedule.children[idx-1],
                                    fschedule.children[idx])
        idx += 1

    fschedule, _ = otrans.apply(fschedule.children[0])

    invoke.schedule = fschedule
    code = str(psy.gen)

    # Check generated code
    omp_para_idx = -1
    omp_endpara_idx = -1
    cell_do_idx = -1
    cell_enddo_idx = -1
    call1_idx = -1
    call2_idx = -1
    for idx, line in enumerate(code.split('\n')):
        if "DO cell=1,f1_proxy%vspace%get_ncell()" in line:
            cell_do_idx = idx
        if "!$omp parallel do default(shared), " +\
           "private(cell,map_w1,map_w2,map_w3), schedule(static)" in line:
            omp_para_idx = idx
        if "CALL testkern_code" in line:
            if call1_idx == -1:
                call1_idx = idx
            else:
                call2_idx = idx
        if "END DO" in line:
            cell_enddo_idx = idx
        if "!$omp end parallel do" in line:
            omp_endpara_idx = idx

    assert cell_do_idx - omp_para_idx == 1
    assert call1_idx > cell_do_idx
    assert call2_idx > call1_idx
    assert cell_enddo_idx > call2_idx
    assert omp_endpara_idx - cell_enddo_idx == 1


def test_fuse_colour_loops():
    ''' Test that we can fuse colour loops, enclose them in an OpenMP
    parallel region and preceed each by an OpenMP PARALLEL DO '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4.6_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0')
    schedule = invoke.schedule

    ctrans = Dynamo0p3ColourTrans()
    otrans = Dynamo0p3OMPLoopTrans()
    rtrans = OMPParallelTrans()
    ftrans = DynamoLoopFuseTrans()

    # Colour each of the loops
    for child in schedule.children:
        newsched, _ = ctrans.apply(child)

    # Fuse the (sequential) loops over colours
    nchildren = len(newsched.children)
    idx = 1
    fschedule = newsched
    while idx < nchildren:
        fschedule, _ = ftrans.apply(fschedule.children[idx-1],
                                    fschedule.children[idx])
        idx += 1

    # Enclose the colour loops within an OMP parallel region
    newsched, _ = rtrans.apply(fschedule.children[0].children)

    # Put an OMP DO around each of the colour loops
    for child in newsched.children[0].children[0].children:
        newsched, _ = otrans.apply(child)

    # Replace the original schedule with the transformed one and
    # generate the code
    invoke.schedule = newsched
    code = str(psy.gen)

    # Test that the generated code is as expected
    omp_para_idx = -1
    omp_do_idx1 = -1
    omp_do_idx2 = -1
    cell_loop_idx1 = -1
    cell_loop_idx2 = -1
    end_loop_idx1 = -1
    end_loop_idx2 = -1
    end_loop_idx3 = -1
    call_idx1 = -1
    call_idx2 = -1
    for idx, line in enumerate(code.split('\n')):
        if "END DO" in line:
            if end_loop_idx1 == -1:
                end_loop_idx1 = idx
            elif end_loop_idx2 == -1:
                end_loop_idx2 = idx
            else:
                end_loop_idx3 = idx
        if "DO cell=1,ncp_colour(colour)" in line:
            if cell_loop_idx1 == -1:
                cell_loop_idx1 = idx
            else:
                cell_loop_idx2 = idx
        if "DO colour=1,ncolour" in line:
            col_loop_idx = idx
        if "CALL ru_code(nlayers," in line:
            if call_idx1 == -1:
                call_idx1 = idx
            else:
                call_idx2 = idx
        if "!$omp parallel default(shared), " +\
           "private(cell,map_w2,map_w3,map_w0)" in line:
            omp_para_idx = idx
        if "!$omp do schedule(static)" in line:
            if omp_do_idx1 == -1:
                omp_do_idx1 = idx
            else:
                omp_do_idx2 = idx

    assert (omp_para_idx - col_loop_idx) == 1
    assert (omp_do_idx1 - omp_para_idx) == 1
    assert (cell_loop_idx1 - omp_do_idx1) == 1
    assert (cell_loop_idx2 - omp_do_idx2) == 1
    assert (end_loop_idx3 - end_loop_idx2) == 3
    assert call_idx2 > call_idx1
    assert call_idx1 < end_loop_idx1
    assert call_idx2 < end_loop_idx2
