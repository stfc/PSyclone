#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Author R. Ford and A. R. Porter, STFC Daresbury Lab

''' Tests of transformations with the Dynamo 0.3 API '''

from parse import parse
from psyGen import PSyFactory
from transformations import TransformationError,\
    Dynamo0p3ColourTrans,\
    DynamoOMPParallelLoopTrans
import os
import pytest

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


def test_omp_colouring_needed():
    ''' Test that we raise an error when applying an OpenMP transformation
    to a loop that requires colouring (i.e. has a field with 'INC' access)
    but is not coloured. '''
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


def test_check_colours_loop_sequential():
    ''' Test that we raise an error if the user attempts to OpenMP-
    parallelise a loop over colours (since any such loop must be
    sequential) '''
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
    _,info=parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
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
    print gen

    # Check that we're calling the API to get the no. of colours
    assert "a_proxy%vspace%get_colours(" in gen
    assert "f_proxy%vspace%get_colours(" in gen
    assert "private(cell,map_w2,map_w3,map_w0)" in gen
