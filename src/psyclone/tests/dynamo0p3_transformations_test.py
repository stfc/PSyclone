# -----------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -----------------------------------------------------------------------------
# Author R. Ford and A. R. Porter, STFC Daresbury Lab

''' Tests of transformations with the Dynamo 0.3 API '''

import os
import pytest
from psyclone.parse import parse
from psyclone.psyGen import PSyFactory, GenerationError
from psyclone.transformations import TransformationError, \
    OMPParallelTrans, \
    Dynamo0p3ColourTrans, \
    Dynamo0p3OMPLoopTrans, \
    DynamoOMPParallelLoopTrans, \
    DynamoLoopFuseTrans, \
    KernelModuleInlineTrans, \
    MoveTrans

# Since this is a file containing tests which often have to get in and
# change the internal state of objects we disable pylint's warning
# about such accesses
# pylint: disable=protected-access

# The version of the API that the tests in this file
# exercise.
TEST_API = "dynamo0.3"
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")


def test_colour_trans_declarations():
    '''Check that we generate the correct variable declarations when
    doing a colouring transformation. We check when distributed memory
    is both off and on '''
    # test of the colouring transformation of a single loop
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0_testkern_type')
        schedule = invoke.schedule
        ctrans = Dynamo0p3ColourTrans()

        if dist_mem:
            index = 3
        else:
            index = 0

        # Colour the loop
        cschedule, _ = ctrans.apply(schedule.children[index])

        # Replace the original loop schedule with the transformed one
        invoke.schedule = cschedule

        # Store the results of applying this code transformation as
        # a string
        gen = str(psy.gen)
        # Fortran is not case sensitive
        gen = gen.lower()
        print gen

        # Check that we've declared the loop-related variables
        # and colour-map pointers
        assert "integer ncolour" in gen
        assert "integer colour" in gen
        assert "integer, pointer :: cmap(:,:), ncp_colour(:)" in gen


def test_colour_trans():
    '''test of the colouring transformation of a single loop. We test
    when distributed memory is both off and on'''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0_testkern_type')
        schedule = invoke.schedule
        ctrans = Dynamo0p3ColourTrans()

        if dist_mem:
            index = 3
        else:
            index = 0

        # Colour the loop
        cschedule, _ = ctrans.apply(schedule.children[index])

        # Replace the original loop schedule with the transformed one
        invoke.schedule = cschedule

        # Store the results of applying this code transformation as
        # a string
        gen = str(psy.gen)
        # Fortran is not case sensitive
        gen = gen.lower()
        print gen
        # Check that we're calling the API to get the no. of colours
        assert "f1_proxy%vspace%get_colours(" in gen

        col_loop_idx = -1
        cell_loop_idx = -1
        for idx, line in enumerate(gen.split('\n')):
            if "do colour=1,ncolour" in line:
                col_loop_idx = idx
            if "do cell=1,ncp_colour(colour)" in line:
                cell_loop_idx = idx

        assert cell_loop_idx - col_loop_idx == 1

        # Check that we're using the colour map when getting the cell dof maps
        assert (
            "call testkern_code(nlayers, a, f1_proxy%data, f2_proxy%data, "
            "m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, "
            "map_w1(:,cmap(colour, cell)), ndf_w2, undf_w2, "
            "map_w2(:,cmap(colour, cell)), ndf_w3, undf_w3, "
            "map_w3(:,cmap(colour, cell)))" in gen)

        if dist_mem:
            # Check that we get the right number of set_dirty halo calls in
            # the correct location
            dirty_str = (
                "      end do \n"
                "      !\n"
                "      ! set halos dirty for fields modified in the "
                "above loop\n"
                "      !\n"
                "      call f1_proxy%set_dirty()\n")
            assert dirty_str in gen
            assert gen.count("set_dirty()") == 1


def test_colour_trans_operator():
    '''test of the colouring transformation of a single loop with an
    operator. We check that the first argument is a colourmap lookup,
    not a direct cell index. We test when distributed memory is both
    off and on. '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "10_operator.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0_testkern_operator_type')
        schedule = invoke.schedule
        ctrans = Dynamo0p3ColourTrans()

        if dist_mem:
            index = 3
        else:
            index = 0

        # Colour the loop
        schedule, _ = ctrans.apply(schedule.children[index])

        # Store the results of applying this code transformation as a
        # string
        gen = str(psy.gen)
        print gen

        # check the first argument is a colourmap lookup
        assert "CALL testkern_operator_code(cmap(colour, cell), nlayers" in gen


def test_colour_trans_cma_operator():  # pylint: disable=invalid-name
    '''test of the colouring transformation of a single loop with a CMA
    operator. We check that the first argument is a colourmap lookup,
    not a direct cell index. We test when distributed memory is both
    off and on. '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "20.3_cma_assembly_field.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get(
            'invoke_0_columnwise_op_asm_field_kernel_type')
        schedule = invoke.schedule
        schedule.view()
        ctrans = Dynamo0p3ColourTrans()

        if dist_mem:
            index = 1
        else:
            index = 0

        # Colour the loop
        schedule, _ = ctrans.apply(schedule.children[index])

        # Store the results of applying this code transformation as a
        # string
        gen = str(psy.gen)
        print gen

        assert (
            "      DO colour=1,ncolour\n"
            "        DO cell=1,ncp_colour(colour)\n"
            "          !\n"
            "          CALL columnwise_op_asm_field_kernel_code(cmap(colour, "
            "cell), nlayers, ncell_2d, afield_proxy%data, "
            "lma_op1_proxy%ncell_3d, lma_op1_proxy%local_stencil, "
            "cma_op1_matrix, cma_op1_nrow, cma_op1_ncol, cma_op1_bandwidth, "
            "cma_op1_alpha, cma_op1_beta, cma_op1_gamma_m, cma_op1_gamma_p, "
            "ndf_any_space_1_afield, undf_any_space_1_afield, "
            "map_any_space_1_afield(:,cmap(colour, cell)), "
            "cbanded_map_any_space_1_afield, ndf_any_space_2_lma_op1, "
            "cbanded_map_any_space_2_lma_op1)\n"
            "        END DO \n"
            "      END DO \n") in gen


def test_colour_trans_stencil():
    '''test of the colouring transformation of a single loop with a
    stencil access. We test when distributed memory is both off and
    on    '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "19.1_single_stencil.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0_testkern_stencil_type')
        schedule = invoke.schedule
        ctrans = Dynamo0p3ColourTrans()

        if dist_mem:
            index = 3
        else:
            index = 0

        # Colour the loop
        cschedule, _ = ctrans.apply(schedule.children[index])

        # Replace the original loop schedule with the transformed one
        invoke.schedule = cschedule

        # Store the results of applying this code transformation as
        # a string
        gen = str(psy.gen)
        print gen

        # Check that we index the stencil dofmap appropriately
        assert (
            "          CALL testkern_stencil_code(nlayers, f1_proxy%data, "
            "f2_proxy%data, f2_stencil_size, "
            "f2_stencil_dofmap(:,:,cmap(colour, cell)), f3_proxy%data, "
            "f4_proxy%data, ndf_w1, undf_w1, map_w1(:,cmap(colour, cell)), "
            "ndf_w2, undf_w2, map_w2(:,cmap(colour, cell)), ndf_w3, "
            "undf_w3, map_w3(:,cmap(colour, cell)))" in gen)


def test_colouring_not_a_loop():
    '''Test that we raise an appropriate error if we attempt to colour
    something that is not a loop. We test when distributed memory is
    on or off '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0_testkern_type')
        schedule = invoke.schedule
        ctrans = Dynamo0p3ColourTrans()

        # Erroneously attempt to colour the schedule rather than the loop
        with pytest.raises(TransformationError) as excinfo:
            _, _ = ctrans.apply(schedule)
        assert "Error in DynamoColour transformation" in str(excinfo.value)
        assert "The supplied node is not a loop" in str(excinfo.value)


def test_no_colour_dofs():
    ''' Test that we raise the correct exception when attempting to apply
    the loop-colouring tranformation to a loop that is over dofs rather than
    cells. '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "15_single_pointwise_invoke.f90"),
                    api=TEST_API)
    ctrans = Dynamo0p3ColourTrans()
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0')
        schedule = invoke.schedule
        with pytest.raises(TransformationError) as excinfo:
            _, _ = ctrans.apply(schedule.children[0])
        val = str(excinfo.value)
        assert "Error in DynamoColour transformation" in val
        assert ("Only loops over cells may be coloured but this loop is over "
                "dofs" in val)


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
    '''Test that we raise an appropriate error if we attempt to apply an
    OpenMP DO transformation to something that is not a loop. We test
    when distributed memory is on or off '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0_testkern_type')
        schedule = invoke.schedule
        otrans = Dynamo0p3OMPLoopTrans()

        # Erroneously attempt to apply OpenMP to the schedule rather than
        # the loop
        with pytest.raises(TransformationError) as excinfo:
            _, _ = otrans.apply(schedule)
        assert "Error in Dynamo0p3OMPLoopTrans trans" in str(excinfo.value)
        assert "The node is not a loop" in str(excinfo.value)


def test_omp_parallel_not_a_loop():
    '''Test that we raise an appropriate error if we attempt to apply an
    OpenMP PARALLEL DO transformation to something that is not a
    loop. We test when distributed memory is on or off '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0_testkern_type')
        schedule = invoke.schedule
        otrans = DynamoOMPParallelLoopTrans()

        # Erroneously attempt to apply OpenMP to the schedule rather than
        # the loop
        with pytest.raises(TransformationError) as excinfo:
            _, _ = otrans.apply(schedule)
        assert "Error in DynamoOMPParallelLoopTrans tra" in str(excinfo.value)
        assert "The node is not a loop" in str(excinfo.value)


def test_colour_name():
    ''' Test the name property of the Dynamo0p3ColourTrans class '''
    ctrans = Dynamo0p3ColourTrans()
    cname = ctrans.name
    assert cname == "Dynamo0p3LoopColourTrans"


def test_colour_str():
    ''' Test the str method of the Dynamo0p3ColourTrans class '''
    ctrans = Dynamo0p3ColourTrans()
    cstr = str(ctrans)
    assert cstr == "Split a Dynamo 0.3 loop over cells into colours"


def test_omp_colour_trans():
    '''Test the OpenMP transformation applied to a coloured loop. We test
    when distributed memory is on or off '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0_testkern_type')
        schedule = invoke.schedule

        ctrans = Dynamo0p3ColourTrans()
        otrans = DynamoOMPParallelLoopTrans()

        if dist_mem:
            index = 3
        else:
            index = 0

        # Colour the loop
        cschedule, _ = ctrans.apply(schedule.children[index])

        # Then apply OpenMP to the inner loop
        schedule, _ = otrans.apply(cschedule.children[index].children[0])

        invoke.schedule = schedule
        code = str(psy.gen)
        print code

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
        assert "private(cell)" in code


def test_omp_colour_orient_trans():
    '''Test the OpenMP transformation applied to a coloured loop when the
    kernel expects orientation information. We test when distributed
    memory is on or off '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "9_orientation.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0_testkern_orientation_type')
        schedule = invoke.schedule

        ctrans = Dynamo0p3ColourTrans()
        otrans = DynamoOMPParallelLoopTrans()

        if dist_mem:
            index = 4
        else:
            index = 0

        # Colour the loop
        cschedule, _ = ctrans.apply(schedule.children[index])

        # Then apply OpenMP to the inner loop
        schedule, _ = otrans.apply(cschedule.children[index].children[0])

        invoke.schedule = schedule
        code = str(psy.gen)

        # Check that we're using the colour map when getting the orientation
        assert "get_cell_orientation(cmap(colour, cell))" in code

        # Check that the list of private variables is correct
        assert "private(cell,orientation_w2)" in code


def test_omp_parallel_colouring_needed():
    '''Test that we raise an error when applying an OpenMP PARALLEL DO
    transformation to a loop that requires colouring (i.e. has a field
    with 'INC' access) but is not coloured. We test when distributed
    memory is on or off '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "11_any_space.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        if dist_mem:
            index = 5
        else:
            index = 0
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0_testkern_any_space_1_type')
        schedule = invoke.schedule
        otrans = DynamoOMPParallelLoopTrans()
        # Apply OpenMP to the loop
        with pytest.raises(TransformationError) as excinfo:
            schedule, _ = otrans.apply(schedule.children[index])
        assert "Error in DynamoOMPParallelLoopTrans" in str(excinfo.value)
        assert "kernel has an argument with INC access" in str(excinfo.value)
        assert "Colouring is required" in str(excinfo.value)


def test_omp_colouring_needed():
    '''Test that we raise an error when applying an OpenMP DO
    transformation to a loop that requires colouring (i.e. has a field
    with 'INC' access) but is not coloured. We test when distributed
    memory is on or off '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "11_any_space.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        if dist_mem:
            index = 5
        else:
            index = 0
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0_testkern_any_space_1_type')
        schedule = invoke.schedule

        otrans = Dynamo0p3OMPLoopTrans()
        # Apply OpenMP to the loop
        with pytest.raises(TransformationError) as excinfo:
            schedule, _ = otrans.apply(schedule.children[index])
        assert "Error in Dynamo0p3OMPLoopTrans transfo" in str(excinfo.value)
        assert "kernel has an argument with INC access" in str(excinfo.value)
        assert "Colouring is required" in str(excinfo.value)


def test_check_seq_colours_omp_parallel_do():
    '''Test that we raise an error if the user attempts to apply an OpenMP
    PARALLEL DO transformation to a loop over colours (since any such
    loop must be sequential). We test when distributed memory is on or
    off '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "9_orientation.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0_testkern_orientation_type')
        schedule = invoke.schedule
        if dist_mem:
            index = 4
        else:
            index = 0

        ctrans = Dynamo0p3ColourTrans()
        otrans = DynamoOMPParallelLoopTrans()

        # Colour the loop
        cschedule, _ = ctrans.apply(schedule.children[index])

        # Then erroneously attempt to apply OpenMP to the loop over
        # colours
        with pytest.raises(TransformationError) as excinfo:
            schedule, _ = otrans.apply(cschedule.children[index])
        assert "Error in DynamoOMPParallelLoopTrans" in str(excinfo.value)
        assert "requested loop is over colours" in str(excinfo.value)
        assert "must be computed serially" in str(excinfo.value)


def test_check_seq_colours_omp_do():
    '''Test that we raise an error if the user attempts to apply an OpenMP
    DO transformation to a loop over colours (since any such loop must
    be sequential). We test when distributed memory is on or off '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "9_orientation.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0_testkern_orientation_type')
        schedule = invoke.schedule
        if dist_mem:
            index = 4
        else:
            index = 0

        ctrans = Dynamo0p3ColourTrans()
        otrans = Dynamo0p3OMPLoopTrans()

        # Colour the loop
        cschedule, _ = ctrans.apply(schedule.children[index])

        # Then erroneously attempt to apply OpenMP to the loop over
        # colours
        with pytest.raises(TransformationError) as excinfo:
            schedule, _ = otrans.apply(cschedule.children[index])
        assert "Error in Dynamo0p3OMPLoopTrans" in str(excinfo.value)
        assert "target loop is over colours" in str(excinfo.value)
        assert "must be computed serially" in str(excinfo.value)


def test_colouring_after_openmp():
    '''Test that we raise an error if the user attempts to colour a loop
    that is already within an OpenMP parallel region. We test when
    distributed memory is on or off '''
    # For this test we must use a kernel that doesn't actually require
    # colouring as otherwise PSyclone won't let us apply the OpenMP
    # transformation first!
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "9_orientation.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0_testkern_orientation_type')
        schedule = invoke.schedule

        ctrans = Dynamo0p3ColourTrans()
        otrans = DynamoOMPParallelLoopTrans()

        if dist_mem:
            index = 4
        else:
            index = 0

        # Apply OpenMP to the loop
        schedule, _ = otrans.apply(schedule.children[index])

        # Now attempt to colour the loop within this OpenMP region
        with pytest.raises(TransformationError) as excinfo:
            schedule, _ = ctrans.apply(schedule.children[index].children[0])
        assert "Cannot have a loop over colours" in str(excinfo.value)
        assert "within an OpenMP parallel region" in str(excinfo.value)


def test_colouring_multi_kernel():
    '''Test that we correctly generate all the map-lookups etc.  when an
    invoke contains more than one kernel. We test when distributed
    memory is on or off '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4.6_multikernel_invokes.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0')
        schedule = invoke.schedule

        ctrans = Dynamo0p3ColourTrans()
        otrans = DynamoOMPParallelLoopTrans()
        mtrans = MoveTrans()

        if dist_mem:
            # We have halo exchanges inbetween the two loops which we
            # are going to get rid of for simplicity. Fields b, d and
            # the 3e's are already covered before the first loop so
            # can be removed
            del schedule.children[8:13]
            # f is required but can be moved before the first loop
            schedule, _ = mtrans.apply(schedule.children[7],
                                       schedule.children[6])
            index = 7
        else:
            index = 0

        # colour each loop
        schedule, _ = ctrans.apply(schedule.children[index])
        schedule, _ = ctrans.apply(schedule.children[index+1])

        # Apply OpenMP to each of the colour loops
        schedule, _ = otrans.apply(schedule.children[index].children[0])
        schedule, _ = otrans.apply(schedule.children[index+1].children[0])

        gen = str(psy.gen)
        print gen

        # Check that we're calling the API to get the no. of colours
        assert "a_proxy%vspace%get_colours(" in gen
        assert "f_proxy%vspace%get_colours(" in gen
        assert gen.count("_proxy%vspace%get_colours(") == 2
        assert "private(cell)" in gen
        assert gen.count("private(cell)") == 2


def test_omp_region_omp_do():
    '''Test that we correctly generate code for the case of a single OMP
    DO within an OMP PARALLEL region without colouring. We test when
    distributed memory is on or off '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0_testkern_type')
        schedule = invoke.schedule
        olooptrans = Dynamo0p3OMPLoopTrans()
        ptrans = OMPParallelTrans()

        if dist_mem:
            index = 3
        else:
            index = 0

        # Put an OMP PARALLEL around this loop
        child = schedule.children[index]
        oschedule, _ = ptrans.apply(child)

        # Put an OMP DO around this loop
        schedule, _ = olooptrans.apply(oschedule.children[index].children[0])

        # Replace the original loop schedule with the transformed one
        invoke.schedule = schedule

        # Store the results of applying this code transformation as
        # a string
        code = str(psy.gen)

        print code

        omp_do_idx = -1
        omp_para_idx = -1
        cell_loop_idx = -1
        omp_enddo_idx = -1
        if dist_mem:
            loop_str = "DO cell=1,mesh%get_last_halo_cell(1)"
        else:
            loop_str = "DO cell=1,f1_proxy%vspace%get_ncell()"
        for idx, line in enumerate(code.split('\n')):
            if loop_str in line:
                cell_loop_idx = idx
            if "!$omp do" in line:
                omp_do_idx = idx
            if "!$omp parallel default" in line:
                omp_para_idx = idx
            if "!$omp end do" in line:
                omp_enddo_idx = idx
            if "END DO" in line:
                cell_end_loop_idx = idx

        assert (omp_do_idx - omp_para_idx) == 1
        assert (cell_loop_idx - omp_do_idx) == 1
        assert (omp_enddo_idx - cell_end_loop_idx) == 1


def test_multi_kernel_single_omp_region():
    ''' Test that we correctly generate all the map-lookups etc.
    when an invoke contains more than one kernel that are all contained
    within a single OMP region'''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4_multikernel_invokes.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0')
        schedule = invoke.schedule

        if dist_mem:
            # We have halo exchanges inbetween the two loops which we
            # are going to get rid of for simplicity. Fields f2, m1 and m2
            # are already covered before the first loop so
            # can be removed
            del schedule.children[4:7]
            index = 3
        else:
            index = 0

        otrans = Dynamo0p3OMPLoopTrans()
        rtrans = OMPParallelTrans()

        # Apply OpenMP to each of the loops
        schedule, _ = otrans.apply(schedule.children[index])
        schedule, _ = otrans.apply(schedule.children[index+1])

        # Enclose all of these OpenMP'd loops within a single region
        schedule, _ = rtrans.apply(schedule.children[index:index+2])

        code = str(psy.gen)
        print code

        omp_do_idx = -1
        omp_end_do_idx = -1
        omp_para_idx = -1
        omp_end_para_idx = -1
        cell_loop_idx = -1
        end_do_idx = -1
        if dist_mem:
            loop_str = "DO cell=1,mesh%get_last_halo_cell(1)"
        else:
            loop_str = "DO cell=1,f1_proxy%vspace%get_ncell()"
        for idx, line in enumerate(code.split('\n')):
            if (cell_loop_idx == -1) and (loop_str in line):
                cell_loop_idx = idx
            if (omp_do_idx == -1) and ("!$omp do" in line):
                omp_do_idx = idx
            if "!$omp end do" in line:
                omp_end_do_idx = idx
            if "!$omp parallel default(shared), " +\
               "private(cell)" in line:
                omp_para_idx = idx
            if "END DO" in line:
                end_do_idx = idx
            if "!$omp end parallel" in line:
                omp_end_para_idx = idx

        assert (omp_do_idx - omp_para_idx) == 1
        assert (cell_loop_idx - omp_do_idx) == 1
        assert (omp_end_para_idx - omp_end_do_idx) > 0
        assert (omp_end_do_idx - end_do_idx) == 1


def test_multi_different_kernel_omp():
    '''Test that we correctly generate the OpenMP private lists when we
    have more than one kernel of a different type (requiring a different
    private list) within an invoke. Test with and without DM.'''

    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4.7_multikernel_invokes.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0')
        schedule = invoke.schedule

        if dist_mem:
            index1 = 6
            index2 = 10
        else:
            index1 = 0
            index2 = 1

        ctrans = Dynamo0p3ColourTrans()
        otrans = DynamoOMPParallelLoopTrans()

        # colour each loop
        schedule, _ = ctrans.apply(schedule.children[index1])
        schedule, _ = ctrans.apply(schedule.children[index2])

        # Apply OpenMP to each of the colour loops
        schedule, _ = otrans.apply(schedule.children[index1].children[0])
        schedule, _ = otrans.apply(schedule.children[index2].children[0])

        code = str(psy.gen)
        print code

        assert "private(cell)" in code


def test_loop_fuse_different_spaces():
    ''' Test that we raise an appropriate error if the user attempts
    to fuse loops that are on different spaces '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4.7_multikernel_invokes.f90"),
                    api=TEST_API)
    for same_space in [False, True]:
        for dist_mem in [False, True]:
            psy = PSyFactory(TEST_API,
                             distributed_memory=dist_mem).create(info)
            invoke = psy.invokes.get('invoke_0')
            schedule = invoke.schedule

            ftrans = DynamoLoopFuseTrans()
            mtrans = MoveTrans()
            if dist_mem:
                # b halo exchange between loops can be removed as access
                # in both loops is read and it is already covered by the
                # first loop
                del schedule.children[7]
                # c and g halo exchange between loops can be moved before
                # 1st loop as they are not accessed in first loop
                schedule, _ = mtrans.apply(schedule.children[7],
                                           schedule.children[6])
                schedule, _ = mtrans.apply(schedule.children[8],
                                           schedule.children[7])
                index = 8
            else:
                index = 0

            with pytest.raises(TransformationError) as excinfo:
                _, _ = ftrans.apply(schedule.children[index],
                                    schedule.children[index+1],
                                    same_space=same_space)
            assert "Error in DynamoLoopFuse transformation" in \
                str(excinfo.value)
            assert "Cannot fuse loops that are over different spaces" in \
                str(excinfo.value)
            same_space_warning = ("Note, the same_space flag was set, but "
                                  "does not apply because neither field "
                                  "is ANY_SPACE.")

            if same_space:
                assert same_space_warning in str(excinfo.value)
            else:
                assert same_space_warning not in str(excinfo.value)


def test_loop_fuse_unexpected_error():
    ''' Test that we catch an unexpected error when loop fusing '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4_multikernel_invokes.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0')
        schedule = invoke.schedule

        if dist_mem:
            # remove unecessary halos between loops. At the moment we have no
            # intra halo analysis so we add them before all loops just in
            # case.
            del schedule.children[4:7]
            index = 3
        else:
            index = 0

        ftrans = DynamoLoopFuseTrans()

        # cause an unexpected error
        schedule.children[index].children = None

        with pytest.raises(TransformationError) as excinfo:
            _, _ = ftrans.apply(schedule.children[index],
                                schedule.children[index+1])
        assert 'Unexpected exception' in str(excinfo.value)


def test_loop_fuse():
    ''' Test that we are able to fuse two loops together '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4_multikernel_invokes.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0')
        schedule = invoke.schedule

        if dist_mem:
            # remove unecessary halos between loops. At the moment we have no
            # intra halo analysis so we add them before all loops just in
            # case.
            del schedule.children[4:7]
            index = 3
        else:
            index = 0

        ftrans = DynamoLoopFuseTrans()

        # fuse the loops
        schedule, _ = ftrans.apply(schedule.children[index],
                                   schedule.children[index+1])

        gen = str(psy.gen)

        cell_loop_idx = -1
        end_loop_idx = -1
        call_idx1 = -1
        call_idx2 = -1
        if dist_mem:
            loop_str = "DO cell=1,mesh%get_last_halo_cell(1)"
        else:
            loop_str = "DO cell=1,f1_proxy%vspace%get_ncell()"
        for idx, line in enumerate(gen.split('\n')):
            if loop_str in line:
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


def test_loop_fuse_set_dirty():
    ''' Test that we are able to fuse two loops together and produce
    the expected set_dirty() calls '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.get('invoke_0')
    schedule = invoke.schedule
    ftrans = DynamoLoopFuseTrans()
    schedule.view()
    # remove unecessary halos between loops. At the moment we have no
    # intra halo analysis so we add them before all loops just in
    # case.
    del schedule.children[4:7]
    schedule.view()
    # Fuse the loops
    schedule, _ = ftrans.apply(schedule.children[3],
                               schedule.children[4])
    schedule.view()
    gen = str(psy.gen)
    print gen
    assert gen.count("set_dirty()") == 1


def test_loop_fuse_omp():
    '''Test that we can loop-fuse two loop nests and enclose them in an
       OpenMP parallel region'''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4_multikernel_invokes.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0')
        schedule = invoke.schedule

        if dist_mem:
            # remove unecessary halos between loops. At the moment we have no
            # intra halo analysis so we add them before all loops just in
            # case.
            del schedule.children[4:7]
            index = 3
        else:
            index = 0

        ftrans = DynamoLoopFuseTrans()
        otrans = DynamoOMPParallelLoopTrans()

        schedule, _ = ftrans.apply(schedule.children[index],
                                   schedule.children[index+1])

        schedule, _ = otrans.apply(schedule.children[index])

        code = str(psy.gen)
        print code

        # Check generated code
        omp_para_idx = -1
        omp_endpara_idx = -1
        cell_do_idx = -1
        cell_enddo_idx = -1
        call1_idx = -1
        call2_idx = -1
        if dist_mem:
            loop_str = "DO cell=1,mesh%get_last_halo_cell(1)"
        else:
            loop_str = "DO cell=1,f1_proxy%vspace%get_ncell()"
        for idx, line in enumerate(code.split('\n')):
            if loop_str in line:
                cell_do_idx = idx
            if "!$omp parallel do default(shared), " +\
               "private(cell), schedule(static)" in line:
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
    '''Test that we can fuse colour loops , enclose them in an OpenMP
    parallel region and preceed each by an OpenMP PARALLEL DO for
    both sequential and distributed-memory code '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4.6_multikernel_invokes.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0')
        schedule = invoke.schedule

        ctrans = Dynamo0p3ColourTrans()
        otrans = Dynamo0p3OMPLoopTrans()
        rtrans = OMPParallelTrans()
        ftrans = DynamoLoopFuseTrans()
        mtrans = MoveTrans()

        if dist_mem:
            # We have halo exchanges inbetween the two loops which we
            # are going to get rid of for simplicity. Fields b, d and
            # the 3e's are already covered before the first loop so
            # can be removed
            del schedule.children[8:13]
            # f is required but can be moved before the first loop
            schedule, _ = mtrans.apply(schedule.children[7],
                                       schedule.children[6])
            index = 7
        else:
            index = 0

        # colour each loop
        schedule, _ = ctrans.apply(schedule.children[index])
        schedule, _ = ctrans.apply(schedule.children[index+1])

        # fuse the sequential colours loop
        schedule, _ = ftrans.apply(schedule.children[index],
                                   schedule.children[index+1])

        # Enclose the colour loops within an OMP parallel region
        schedule, _ = rtrans.apply(schedule.children[index].children)

        # Put an OMP DO around each of the colour loops
        for loop in schedule.children[index].children[0].children:
            schedule, _ = otrans.apply(loop)

        code = str(psy.gen)
        print code

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
               "private(cell)" in line:
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

        if dist_mem:
            set_dirty_str = (
                "      ! Set halos dirty for fields modified in the above "
                "loop\n"
                "      !\n"
                "      CALL a_proxy%set_dirty()\n"
                "      CALL f_proxy%set_dirty()\n")
            assert set_dirty_str in code
            assert code.count("set_dirty()") == 2


def test_loop_fuse_cma():
    ''' Test that we can loop fuse two loops when one contains a
    call to a CMA-related kernel '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "20.6_multi_invoke_with_cma.f90"),
                    api=TEST_API)
    ftrans = DynamoLoopFuseTrans()
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0')
        schedule = invoke.schedule
        if dist_mem:
            # We have halo-swaps between the two loops but these can
            # all be moved before the first loop since the first
            # kernel doesn't look at the corresponding fields
            schedule.children.insert(1, schedule.children.pop(2))
            schedule.children.insert(2, schedule.children.pop(3))
            schedule.children.insert(3, schedule.children.pop(4))
            index = 4
        else:
            index = 0

        # Fuse the loops
        schedule, _ = ftrans.apply(schedule.children[index],
                                   schedule.children[index+1],
                                   same_space=True)
        code = str(psy.gen)
        print code
        assert (
            "      ! Look-up required column-banded dofmaps\n"
            "      !\n"
            "      cbanded_map_any_space_1_afield => "
            "cma_op1_proxy%column_banded_dofmap_to\n"
            "      cbanded_map_any_space_2_lma_op1 => "
            "cma_op1_proxy%column_banded_dofmap_from\n") in code
        assert (
            "      ! Look-up information for each CMA operator\n"
            "      !\n"
            "      cma_op1_matrix => cma_op1_proxy%columnwise_matrix\n"
            "      cma_op1_nrow = cma_op1_proxy%nrow\n"
            "      cma_op1_ncol = cma_op1_proxy%ncol\n"
            "      cma_op1_bandwidth = cma_op1_proxy%bandwidth\n"
            "      cma_op1_alpha = cma_op1_proxy%alpha\n"
            "      cma_op1_beta = cma_op1_proxy%beta\n"
            "      cma_op1_gamma_m = cma_op1_proxy%gamma_m\n"
            "      cma_op1_gamma_p = cma_op1_proxy%gamma_p\n"
        ) in code
        assert (
            "CALL columnwise_op_asm_field_kernel_code(cell, nlayers, "
            "ncell_2d, afield_proxy%data, lma_op1_proxy%ncell_3d, "
            "lma_op1_proxy%local_stencil, cma_op1_matrix, cma_op1_nrow, "
            "cma_op1_ncol, cma_op1_bandwidth, cma_op1_alpha, cma_op1_beta, "
            "cma_op1_gamma_m, cma_op1_gamma_p, ndf_any_space_1_afield, "
            "undf_any_space_1_afield, map_any_space_1_afield(:,cell), "
            "cbanded_map_any_space_1_afield, ndf_any_space_2_lma_op1, "
            "cbanded_map_any_space_2_lma_op1)\n"
            "        !\n"
            "        CALL testkern_code(nlayers, scalar1, "
            "afield_proxy%data, bfield_proxy%data, cfield_proxy%data, "
            "dfield_proxy%data, scalar2, ndf_w1, undf_w1, map_w1(:,cell), "
            "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
            "map_w3(:,cell))\n") in code


def test_omp_parallel_and_halo_exchange_error():
    '''Tests that we raise an error if we try to apply an omp parallel
    transformation to a list containing halo_exchange calls. If this is
    allowed then it is likely that we will get incorrect results, or that
    the code will fail. Fixing this problem is the subject of ticket #526'''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.get('invoke_0')
    schedule = invoke.schedule

    otrans = Dynamo0p3OMPLoopTrans()
    rtrans = OMPParallelTrans()

    # Apply OpenMP to each of the loops
    schedule, _ = otrans.apply(schedule.children[3])
    schedule, _ = otrans.apply(schedule.children[7])

    # Enclose the invoke code within a single region
    with pytest.raises(TransformationError) as excinfo:
        schedule, _ = rtrans.apply(schedule.children)
    assert "A halo exchange within a parallel region is not supported" \
        in str(excinfo.value)


def test_module_inline():
    '''Tests that correct results are obtained when a kernel is inlined
    into the psy-layer in the dynamo0.3 API. More in-depth tests can be
    found in the gocean1p0_transformations.py file'''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4.6_multikernel_invokes.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0')
        schedule = invoke.schedule
        if dist_mem:
            schedule.view()
            index = 13
        else:
            index = 1
        kern_call = schedule.children[index].children[0]
        inline_trans = KernelModuleInlineTrans()
        schedule, _ = inline_trans.apply(kern_call)
        gen = str(psy.gen)
        # check that the subroutine has been inlined
        assert 'SUBROUTINE ru_code()' in gen
        # check that the associated psy "use" does not exist
        assert 'USE ru_kernel_mod, only : ru_code' not in gen


def test_builtin_single_OpenMP_pdo():
    '''Test that we generate correct code if an OpenMP parallel do is
    applied to a single builtin'''
    for dist_mem in [False, True]:
        _, info = parse(os.path.join(BASE_PATH,
                                     "15.2.0_copy_field_builtin.f90"),
                        api=TEST_API, distributed_memory=dist_mem)
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        otrans = DynamoOMPParallelLoopTrans()
        # Apply OpenMP parallelisation to the loop
        schedule, _ = otrans.apply(schedule.children[0])
        invoke.schedule = schedule
        result = str(psy.gen)
        print result
        if dist_mem:
            assert (
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static)\n"
                "      DO df=1,f2_proxy%vspace%get_last_dof_owned()\n"
                "        f2_proxy%data(df) = f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      !\n"
                "      ! Set halos dirty for fields modified in the above "
                "loop\n"
                "      !\n"
                "      CALL f2_proxy%set_dirty()") in result

        else:
            assert (
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f2_proxy%data(df) = f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do") in result


def test_builtin_multiple_OpenMP_pdo():
    '''Test that we generate correct code if OpenMP parallel do's are
    applied to multiple builtins'''
    for dist_mem in [False, True]:
        _, info = parse(os.path.join(BASE_PATH,
                                     "15.0.2_multiple_set_kernels.f90"),
                        api=TEST_API, distributed_memory=dist_mem)
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        otrans = DynamoOMPParallelLoopTrans()
        # Apply OpenMP parallelisation to the loop
        for child in schedule.children:
            schedule, _ = otrans.apply(child)
        invoke.schedule = schedule
        result = str(psy.gen)
        print result
        if dist_mem:
            assert (
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = fred\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      !\n"
                "      ! Set halos dirty for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static)\n"
                "      DO df=1,f2_proxy%vspace%get_last_dof_owned()\n"
                "        f2_proxy%data(df) = 3.0\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      !\n"
                "      ! Set halos dirty for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f2_proxy%set_dirty()\n"
                "      !\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static)\n"
                "      DO df=1,f3_proxy%vspace%get_last_dof_owned()\n"
                "        f3_proxy%data(df) = ginger\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      !\n"
                "      ! Set halos dirty for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f3_proxy%set_dirty()") in result
        else:
            assert (
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = fred\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static)\n"
                "      DO df=1,undf_any_space_1_f2\n"
                "        f2_proxy%data(df) = 3.0\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static)\n"
                "      DO df=1,undf_any_space_1_f3\n"
                "        f3_proxy%data(df) = ginger\n"
                "      END DO \n"
                "      !$omp end parallel do\n") in result


def test_builtin_loop_fuse_pdo():
    '''Test that we generate correct code if an OpenMP parallel do is
    applied to multiple loop fused builtins. We have to assert that it
    is safe to loop fuse. '''
    for dist_mem in [False, True]:
        _, info = parse(os.path.join(BASE_PATH,
                                     "15.0.2_multiple_set_kernels.f90"),
                        api=TEST_API, distributed_memory=dist_mem)
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        ftrans = DynamoLoopFuseTrans()
        schedule, _ = ftrans.apply(schedule.children[0], schedule.children[1],
                                   same_space=True)
        schedule, _ = ftrans.apply(schedule.children[0], schedule.children[1],
                                   same_space=True)
        otrans = DynamoOMPParallelLoopTrans()
        # Apply OpenMP parallelisation to the loop
        schedule, _ = otrans.apply(schedule.children[0])
        invoke.schedule = schedule
        result = str(psy.gen)
        print result
        if dist_mem:
            assert (
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = fred\n"
                "        f2_proxy%data(df) = 3.0\n"
                "        f3_proxy%data(df) = ginger\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      !\n"
                "      ! Set halos dirty for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      CALL f2_proxy%set_dirty()\n"
                "      CALL f3_proxy%set_dirty()") in result
        else:
            assert (
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = fred\n"
                "        f2_proxy%data(df) = 3.0\n"
                "        f3_proxy%data(df) = ginger\n"
                "      END DO \n"
                "      !$omp end parallel do") in result


def test_builtin_single_OpenMP_do():
    '''Test that we generate correct code if an OpenMP do (with an outer
    OpenMP parallel) is applied to a single builtin '''
    for dist_mem in [False, True]:
        _, info = parse(os.path.join(BASE_PATH,
                                     "15.2.0_copy_field_builtin.f90"),
                        api=TEST_API, distributed_memory=dist_mem)
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule

        olooptrans = Dynamo0p3OMPLoopTrans()
        ptrans = OMPParallelTrans()

        # Put an OMP PARALLEL around this loop
        child = schedule.children[0]
        schedule, _ = ptrans.apply(child)
        # Put an OMP DO around this loop
        schedule, _ = olooptrans.apply(schedule.children[0].children[0])
        result = str(psy.gen)
        print result
        if dist_mem:
            assert (
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,f2_proxy%vspace%get_last_dof_owned()\n"
                "        f2_proxy%data(df) = f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !\n"
                "      ! Set halos dirty for fields modified in the "
                "above loop\n"
                "      !\n"
                "      !$omp master\n"
                "      CALL f2_proxy%set_dirty()\n"
                "      !$omp end master\n"
                "      !\n"
                "      !$omp end parallel") in result
        else:
            assert (
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f2_proxy%data(df) = f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp end parallel\n") in result


def test_builtin_multiple_OpenMP_do():
    '''Test that we generate correct code if OpenMP do's are
    applied to multiple builtins'''
    for dist_mem in [False, True]:
        _, info = parse(os.path.join(BASE_PATH,
                                     "15.0.2_multiple_set_kernels.f90"),
                        api=TEST_API, distributed_memory=dist_mem)
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule

        olooptrans = Dynamo0p3OMPLoopTrans()
        ptrans = OMPParallelTrans()

        # Put an OMP PARALLEL around the loops
        children = schedule.children
        schedule, _ = ptrans.apply(children)
        # Put an OMP DO around the loops
        for child in schedule.children[0].children:
            schedule, _ = olooptrans.apply(child)
        result = str(psy.gen)
        print result
        if dist_mem:
            assert (
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = fred\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !\n"
                "      ! Set halos dirty for fields modified in the "
                "above loop\n"
                "      !\n"
                "      !$omp master\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !$omp end master\n"
                "      !\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,f2_proxy%vspace%get_last_dof_owned()\n"
                "        f2_proxy%data(df) = 3.0\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !\n"
                "      ! Set halos dirty for fields modified in the "
                "above loop\n"
                "      !\n"
                "      !$omp master\n"
                "      CALL f2_proxy%set_dirty()\n"
                "      !$omp end master\n"
                "      !\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,f3_proxy%vspace%get_last_dof_owned()\n"
                "        f3_proxy%data(df) = ginger\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !\n"
                "      ! Set halos dirty for fields modified in the "
                "above loop\n"
                "      !\n"
                "      !$omp master\n"
                "      CALL f3_proxy%set_dirty()\n"
                "      !$omp end master\n"
                "      !\n"
                "      !$omp end parallel") in result
        else:
            assert (
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = fred\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,undf_any_space_1_f2\n"
                "        f2_proxy%data(df) = 3.0\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,undf_any_space_1_f3\n"
                "        f3_proxy%data(df) = ginger\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp end parallel") in result


def test_builtin_loop_fuse_do():
    '''Test that we generate correct code if an OpenMP do is applied to
    multiple loop fused builtins. We need to assert it is safe to
    perform loop fusion. '''
    for dist_mem in [False, True]:
        _, info = parse(os.path.join(BASE_PATH,
                                     "15.0.2_multiple_set_kernels.f90"),
                        api=TEST_API, distributed_memory=dist_mem)
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        ftrans = DynamoLoopFuseTrans()
        schedule, _ = ftrans.apply(schedule.children[0], schedule.children[1],
                                   same_space=True)
        schedule, _ = ftrans.apply(schedule.children[0], schedule.children[1],
                                   same_space=True)

        olooptrans = Dynamo0p3OMPLoopTrans()
        ptrans = OMPParallelTrans()

        # Put an OMP PARALLEL around the loop
        children = schedule.children[0]
        schedule, _ = ptrans.apply(children)
        # Put an OMP DO around the loop
        schedule, _ = olooptrans.apply(schedule.children[0].children[0])
        result = str(psy.gen)
        print result
        if dist_mem:
            assert (
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = fred\n"
                "        f2_proxy%data(df) = 3.0\n"
                "        f3_proxy%data(df) = ginger\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !\n"
                "      ! Set halos dirty for fields modified in the "
                "above loop\n"
                "      !\n"
                "      !$omp master\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      CALL f2_proxy%set_dirty()\n"
                "      CALL f3_proxy%set_dirty()\n"
                "      !$omp end master\n"
                "      !\n"
                "      !$omp end parallel") in result
        else:
            assert (
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = fred\n"
                "        f2_proxy%data(df) = 3.0\n"
                "        f3_proxy%data(df) = ginger\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp end parallel") in result


def test_reduction_real_pdo():
    '''test that we generate a correct OpenMP parallel do reduction for a
    real scalar summed in a builtin. We use inner product in this case'''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.9.0_inner_prod_builtin.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        otrans = DynamoOMPParallelLoopTrans()
        # Apply OpenMP parallelisation to the loop
        schedule, _ = otrans.apply(schedule.children[0])
        invoke.schedule = schedule
        code = str(psy.gen)
        print code
        if distmem:
            assert (
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static), reduction(+:asum)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n") in code

        else:
            assert (
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static), reduction(+:asum)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n") in code


def test_reduction_real_do():
    '''test that we generate a correct OpenMP do reduction for a real
    scalar summed in a builtin. We use inner product in this case '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.9.0_inner_prod_builtin.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        otrans = Dynamo0p3OMPLoopTrans()
        rtrans = OMPParallelTrans()
        # Apply an OpenMP do directive to the loop
        schedule, _ = otrans.apply(schedule.children[0], reprod=False)
        # Apply an OpenMP Parallel directive around the OpenMP do directive
        schedule, _ = rtrans.apply(schedule.children[0])
        invoke.schedule = schedule
        code = str(psy.gen)
        print code
        if distmem:
            assert (
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static), reduction(+:asum)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp end parallel\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n") in code
        else:
            assert (
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static), reduction(+:asum)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp end parallel\n") in code


def test_multi_reduction_real_pdo():
    '''test that we generate a correct OpenMP parallel do reduction for a
    real scalar summed in a builtin. We use inner product in this case'''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.11.0_two_same_builtin_reductions.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        otrans = DynamoOMPParallelLoopTrans()
        # Apply OpenMP parallelisation to the loop
        from psyclone.psyGen import Loop
        for child in schedule.children:
            if isinstance(child, Loop):
                schedule, _ = otrans.apply(child)
        invoke.schedule = schedule
        code = str(psy.gen)
        print code
        if distmem:
            assert (
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static), reduction(+:asum)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n"
                "      !\n"
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static), reduction(+:asum)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n") in code
        else:
            assert (
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static), reduction(+:asum)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      !\n"
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static), reduction(+:asum)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n") in code


def test_reduction_after_normal_real_do():
    '''test that we produce correct code when we have a reduction after
    a "normal" builtin and we use OpenMP DO loops for parallelisation
    with a single parallel region over all calls'''
    file_name = "15.14.0_two_builtins_standard_then_reduction.f90"
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH, file_name),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory(
            "dynamo0.3",
            distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        otrans = Dynamo0p3OMPLoopTrans()
        rtrans = OMPParallelTrans()
        # Apply an OpenMP do to the loop
        from psyclone.psyGen import Loop
        for child in schedule.children:
            if isinstance(child, Loop):
                schedule, _ = otrans.apply(child, reprod=False)
        # Apply an OpenMP Parallel for all loops
        schedule, _ = rtrans.apply(schedule.children[0:2])
        invoke.schedule = schedule
        result = str(psy.gen)
        print result
        if distmem:
            expected_output = (
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = bvalue*f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !\n"
                "      ! Set halos dirty for fields modified in the above "
                "loop\n"
                "      !\n"
                "      !$omp master\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !$omp end master\n"
                "      !\n"
                "      !$omp do schedule(static), reduction(+:asum)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        asum = asum+f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp end parallel\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()")
        else:
            expected_output = (
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = bvalue*f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp do schedule(static), reduction(+:asum)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        asum = asum+f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp end parallel")
        assert expected_output in result


def test_reprod_reduction_after_normal_real_do():
    '''test that we produce correct code when we have a reproducible
    reduction after a "normal" builtin and we use OpenMP DO loops for
    parallelisation with a single parallel region over all calls'''

    file_name = "15.14.0_two_builtins_standard_then_reduction.f90"
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH, file_name),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory(
            "dynamo0.3",
            distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        otrans = Dynamo0p3OMPLoopTrans()
        rtrans = OMPParallelTrans()
        # Apply an OpenMP do to the loop
        from psyclone.psyGen import Loop
        for child in schedule.children:
            if isinstance(child, Loop):
                schedule, _ = otrans.apply(child, reprod=True)
        # Apply an OpenMP Parallel for all loops
        schedule, _ = rtrans.apply(schedule.children[0:2])
        invoke.schedule = schedule
        result = str(psy.gen)
        print result
        if distmem:
            expected_output = (
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      ALLOCATE (l_asum(8,nthreads))\n"
                "      l_asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df,th_idx)\n"
                "      th_idx = omp_get_thread_num()+1\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = bvalue*f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !\n"
                "      ! Set halos dirty for fields modified in the above "
                "loop\n"
                "      !\n"
                "      !$omp master\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !$omp end master\n"
                "      !\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        l_asum(1,th_idx) = l_asum(1,th_idx)+"
                "f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp end parallel\n"
                "      !\n"
                "      ! sum the partial results sequentially\n"
                "      !\n"
                "      DO th_idx=1,nthreads\n"
                "        asum = asum+l_asum(1,th_idx)\n"
                "      END DO \n"
                "      DEALLOCATE (l_asum)\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()")
        else:
            expected_output = (
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      ALLOCATE (l_asum(8,nthreads))\n"
                "      l_asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df,th_idx)\n"
                "      th_idx = omp_get_thread_num()+1\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = bvalue*f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        l_asum(1,th_idx) = l_asum(1,th_idx)+"
                "f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp end parallel\n"
                "      !\n"
                "      ! sum the partial results sequentially\n"
                "      !\n"
                "      DO th_idx=1,nthreads\n"
                "        asum = asum+l_asum(1,th_idx)\n"
                "      END DO \n"
                "      DEALLOCATE (l_asum)\n")
        assert expected_output in result


def test_two_reductions_real_do():
    '''test that we produce correct code when we have more than one
    builtin with a reduction, with each reduction using a different
    variable, and we use OpenMP DO loops for parallelisation with a
    single parallel region over all calls '''
    file_name = "15.12.0_two_different_builtin_reductions.f90"
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH, file_name),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory(
            "dynamo0.3",
            distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        if distmem:
            # move the first global sum after the second loop
            mtrans = MoveTrans()
            schedule, _ = mtrans.apply(schedule.children[1],
                                       schedule.children[2],
                                       position="after")
        otrans = Dynamo0p3OMPLoopTrans()
        rtrans = OMPParallelTrans()
        # Apply an OpenMP do to the loop
        from psyclone.psyGen import Loop
        for child in schedule.children:
            if isinstance(child, Loop):
                schedule, _ = otrans.apply(child, reprod=False)
        # Apply an OpenMP Parallel for all loops
        schedule, _ = rtrans.apply(schedule.children[0:2])
        invoke.schedule = schedule
        result = str(psy.gen)
        print result
        if distmem:
            expected_output = (
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      bsum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static), reduction(+:asum)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp do schedule(static), reduction(+:bsum)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        bsum = bsum+f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp end parallel\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n"
                "      global_sum%value = bsum\n"
                "      bsum = global_sum%get_sum()")
        else:
            expected_output = (
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      bsum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static), reduction(+:asum)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp do schedule(static), reduction(+:bsum)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        bsum = bsum+f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp end parallel")
        assert expected_output in result


def test_two_reprod_reductions_real_do():
    '''test that we produce correct code when we have more than one
    builtin with a reproducible reduction, with each reduction using a
    different variable, and we use OpenMP DO loops for parallelisation
    with a single parallel region over all calls'''
    file_name = "15.12.0_two_different_builtin_reductions.f90"
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH, file_name),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory(
            "dynamo0.3",
            distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        if distmem:
            # move the first global sum after the second loop
            mtrans = MoveTrans()
            schedule, _ = mtrans.apply(schedule.children[1],
                                       schedule.children[2],
                                       position="after")
        otrans = Dynamo0p3OMPLoopTrans()
        rtrans = OMPParallelTrans()
        # Apply an OpenMP do to the loop
        from psyclone.psyGen import Loop
        for child in schedule.children:
            if isinstance(child, Loop):
                schedule, _ = otrans.apply(child, reprod=True)
        # Apply an OpenMP Parallel for all loops
        schedule, _ = rtrans.apply(schedule.children[0:2])
        invoke.schedule = schedule
        result = str(psy.gen)
        print result
        if distmem:
            expected_output = (
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      ALLOCATE (l_asum(8,nthreads))\n"
                "      l_asum = 0.0_r_def\n"
                "      bsum = 0.0_r_def\n"
                "      ALLOCATE (l_bsum(8,nthreads))\n"
                "      l_bsum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df,th_idx)\n"
                "      th_idx = omp_get_thread_num()+1\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        l_asum(1,th_idx) = l_asum(1,th_idx)+"
                "f1_proxy%data(df)*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        l_bsum(1,th_idx) = l_bsum(1,th_idx)+"
                "f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp end parallel\n"
                "      !\n"
                "      ! sum the partial results sequentially\n"
                "      !\n"
                "      DO th_idx=1,nthreads\n"
                "        asum = asum+l_asum(1,th_idx)\n"
                "      END DO \n"
                "      DEALLOCATE (l_asum)\n"
                "      DO th_idx=1,nthreads\n"
                "        bsum = bsum+l_bsum(1,th_idx)\n"
                "      END DO \n"
                "      DEALLOCATE (l_bsum)\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n"
                "      global_sum%value = bsum\n"
                "      bsum = global_sum%get_sum()")
        else:
            expected_output = (
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      ALLOCATE (l_asum(8,nthreads))\n"
                "      l_asum = 0.0_r_def\n"
                "      bsum = 0.0_r_def\n"
                "      ALLOCATE (l_bsum(8,nthreads))\n"
                "      l_bsum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df,th_idx)\n"
                "      th_idx = omp_get_thread_num()+1\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        l_asum(1,th_idx) = l_asum(1,th_idx)+"
                "f1_proxy%data(df)*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        l_bsum(1,th_idx) = l_bsum(1,th_idx)+"
                "f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp end parallel\n"
                "      !\n"
                "      ! sum the partial results sequentially\n"
                "      !\n"
                "      DO th_idx=1,nthreads\n"
                "        asum = asum+l_asum(1,th_idx)\n"
                "      END DO \n"
                "      DEALLOCATE (l_asum)\n"
                "      DO th_idx=1,nthreads\n"
                "        bsum = bsum+l_bsum(1,th_idx)\n"
                "      END DO \n"
                "      DEALLOCATE (l_bsum)")
        assert expected_output in result


def test_multi_reduction_same_name_real_do():
    '''test that we raise an exception when we have multiple reductions in
    an invoke with the same name as this is not supported (it would
    cause incorrect code to be created in certain cases). '''
    file_name = "15.11.0_two_same_builtin_reductions.f90"
    for reprod in [True, False]:
        for distmem in [False, True]:
            _, invoke_info = parse(
                os.path.join(BASE_PATH, file_name),
                distributed_memory=distmem,
                api="dynamo0.3")
            psy = PSyFactory(
                "dynamo0.3",
                distributed_memory=distmem).create(invoke_info)
            invoke = psy.invokes.invoke_list[0]
            schedule = invoke.schedule
            otrans = Dynamo0p3OMPLoopTrans()
            rtrans = OMPParallelTrans()
            # Apply an OpenMP do to the loop
            from psyclone.psyGen import Loop
            for child in schedule.children:
                if isinstance(child, Loop):
                    schedule, _ = otrans.apply(child, reprod=reprod)
            if distmem:
                # We have to move/delete a
                # global sum to get to the stage where we can raise an
                # error. This makes incorrect code in this example but
                # in general it could be valid to move the global sum
                del schedule.children[1]
            schedule, _ = rtrans.apply(schedule.children[0:2])
            invoke.schedule = schedule
            with pytest.raises(GenerationError) as excinfo:
                _ = str(psy.gen)
            assert (
                "Reduction variables can only be used once in an "
                "invoke") in str(excinfo.value)


def test_multi_reduction_real_fuse():
    '''test that we raise an exception when we loop fuse two kernels with
    reductions. We need to specify that the loop-fuse is valid in terms of
    iteration spaces.'''
    for file_name in ["15.11.0_two_same_builtin_reductions.f90",
                      "15.12.0_two_different_builtin_reductions.f90"]:

        for distmem in [False, True]:
            _, invoke_info = parse(
                os.path.join(BASE_PATH, file_name),
                distributed_memory=distmem,
                api="dynamo0.3")
            psy = PSyFactory("dynamo0.3",
                             distributed_memory=distmem).create(invoke_info)
            invoke = psy.invokes.invoke_list[0]
            schedule = invoke.schedule

            ftrans = DynamoLoopFuseTrans()
            if distmem:
                # We need to remove the global sum. This makes the
                # code invalid in this particular case but allows us
                # to perform our check
                del schedule.children[1]
            with pytest.raises(TransformationError) as excinfo:
                schedule, _ = ftrans.apply(schedule.children[0],
                                           schedule.children[1],
                                           same_space=True)
            assert (
                "Error in DynamoLoopFuse transformation. Cannot fuse loops "
                "when each loop already contains a "
                "reduction") in str(excinfo.value)


def test_multi_different_reduction_real_pdo():
    '''test that we generate a correct OpenMP parallel do reduction for
    two different builtins. We use inner product and sum_field'''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.12.0_two_different_builtin_reductions.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        otrans = DynamoOMPParallelLoopTrans()
        # Apply OpenMP parallelisation to the loop
        from psyclone.psyGen import Loop
        for child in schedule.children:
            if isinstance(child, Loop):
                schedule, _ = otrans.apply(child)
        invoke.schedule = schedule
        code = str(psy.gen)
        print code
        if distmem:
            assert (
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static), reduction(+:asum)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n"
                "      !\n"
                "      ! Zero summation variables\n"
                "      !\n"
                "      bsum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static), reduction(+:bsum)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        bsum = bsum+f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      global_sum%value = bsum\n"
                "      bsum = global_sum%get_sum()\n") in code
        else:
            assert (
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static), reduction(+:asum)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      !\n"
                "      ! Zero summation variables\n"
                "      !\n"
                "      bsum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static), reduction(+:bsum)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        bsum = bsum+f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n") in code


def test_multi_builtins_reduction_then_standard_pdo():
    '''test that we generate a correct OpenMP parallel do reduction for
    two different builtins, first a reduction then not'''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.13.0_two_builtins_reduction_then_standard.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        otrans = DynamoOMPParallelLoopTrans()
        # Apply OpenMP parallelisation to the loop
        from psyclone.psyGen import Loop
        for child in schedule.children:
            if isinstance(child, Loop):
                schedule, _ = otrans.apply(child)
        invoke.schedule = schedule
        code = str(psy.gen)
        print code
        if distmem:
            assert (
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static), reduction(+:asum)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static)\n"
                "      DO df=1,bsum_proxy%vspace%get_last_dof_owned()\n"
                "        bsum_proxy%data(df) = f1*bsum_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      !\n"
                "      ! Set halos dirty for fields modified in the above "
                "loop\n"
                "      !\n"
                "      CALL bsum_proxy%set_dirty()\n") in code
        else:
            assert (
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static), reduction(+:asum)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static)\n"
                "      DO df=1,undf_any_space_1_bsum\n"
                "        bsum_proxy%data(df) = f1*bsum_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n") in code


def test_multi_builtins_reduction_then_standard_do():
    '''test that we generate a correct OpenMP do reduction for
    two different builtins, first a reduction then not'''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.13.0_two_builtins_reduction_then_standard.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        otrans = Dynamo0p3OMPLoopTrans()
        rtrans = OMPParallelTrans()
        # Apply an OpenMP do to the loop
        from psyclone.psyGen import Loop
        for child in schedule.children:
            if isinstance(child, Loop):
                schedule, _ = otrans.apply(child, reprod=False)
        if distmem:
            mtrans = MoveTrans()
            schedule, _ = mtrans.apply(schedule.children[1],
                                       schedule.children[2],
                                       position="after")
        schedule, _ = rtrans.apply(schedule.children[0:2])
        invoke.schedule = schedule
        code = str(psy.gen)
        print code
        if distmem:
            assert (
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static), reduction(+:asum)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,bsum_proxy%vspace%get_last_dof_owned()\n"
                "        bsum_proxy%data(df) = f1*bsum_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !\n"
                "      ! Set halos dirty for fields modified in the above "
                "loop\n"
                "      !\n"
                "      !$omp master\n"
                "      CALL bsum_proxy%set_dirty()\n"
                "      !$omp end master\n"
                "      !\n"
                "      !$omp end parallel\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n") in code
        else:
            assert (
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static), reduction(+:asum)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,undf_any_space_1_bsum\n"
                "        bsum_proxy%data(df) = f1*bsum_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp end parallel\n") in code


def test_multi_builtins_reduction_then_standard_fuse_pdo():
    '''test that we generate a correct OpenMP parallel do reduction for
    two different loop-fused builtins, first a reduction then not. We
    need to specify that the fused loops are on the same iteration
    space.'''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.13.0_two_builtins_reduction_then_standard.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        if distmem:
            mtrans = MoveTrans()
            schedule, _ = mtrans.apply(schedule.children[1],
                                       schedule.children[2],
                                       position="after")
        rtrans = DynamoOMPParallelLoopTrans()
        ftrans = DynamoLoopFuseTrans()
        schedule, _ = ftrans.apply(schedule.children[0], schedule.children[1],
                                   same_space=True)
        schedule, _ = rtrans.apply(schedule.children[0])
        invoke.schedule = schedule
        code = str(psy.gen)
        print code
        if distmem:
            assert (
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static), reduction(+:asum)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                "        bsum_proxy%data(df) = f1*bsum_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      !\n"
                "      ! Set halos dirty for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL bsum_proxy%set_dirty()\n"
                "      !\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n") in code
        else:
            assert (
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static), reduction(+:asum)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                "        bsum_proxy%data(df) = f1*bsum_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n") in code


def test_multi_builtins_reduction_then_standard_fuse_do():
    '''test that we generate a correct OpenMP do reduction for
    two different loop-fused builtins, first a reduction then not. We
    need to specify that the fused loops are on the same iteration
    space.'''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.13.0_two_builtins_reduction_then_standard.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        if distmem:
            mtrans = MoveTrans()
            schedule, _ = mtrans.apply(schedule.children[1],
                                       schedule.children[2],
                                       position="after")
        rtrans = OMPParallelTrans()
        otrans = Dynamo0p3OMPLoopTrans()
        ftrans = DynamoLoopFuseTrans()
        schedule, _ = ftrans.apply(schedule.children[0], schedule.children[1],
                                   same_space=True)
        schedule, _ = otrans.apply(schedule.children[0], reprod=False)
        schedule, _ = rtrans.apply(schedule.children[0])
        invoke.schedule = schedule
        code = str(psy.gen)
        print code
        if distmem:
            assert (
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static), reduction(+:asum)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                "        bsum_proxy%data(df) = f1*bsum_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !\n"
                "      ! Set halos dirty for fields modified in the "
                "above loop\n"
                "      !\n"
                "      !$omp master\n"
                "      CALL bsum_proxy%set_dirty()\n"
                "      !$omp end master\n"
                "      !\n"
                "      !$omp end parallel\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n") in code
        else:
            assert (
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static), reduction(+:asum)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                "        bsum_proxy%data(df) = f1*bsum_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp end parallel\n") in code


def test_multi_builtins_standard_then_reduction_pdo():
    '''test that we generate a correct OpenMP parallel do reduction for
    two different builtins, first a standard builtin then a reduction'''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.14.0_two_builtins_standard_then_reduction.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        otrans = DynamoOMPParallelLoopTrans()
        # Apply OpenMP parallelisation to the loop
        from psyclone.psyGen import Loop
        for child in schedule.children:
            if isinstance(child, Loop):
                schedule, _ = otrans.apply(child)
        invoke.schedule = schedule
        code = str(psy.gen)
        print code
        if distmem:
            assert (
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = bvalue*f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      !\n"
                "      ! Set halos dirty for fields modified in the above "
                "loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !\n"
                "      !\n"
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static), reduction(+:asum)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        asum = asum+f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n") in code
        else:
            assert (
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = bvalue*f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      !\n"
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static), reduction(+:asum)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        asum = asum+f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n") in code


def test_multi_builtins_standard_then_reduction_fuse_pdo():
    '''test that we generate a correct OpenMP parallel do reduction for
    two different loop-fused builtins, first a normal builtin then a
    reduction. We need to specify that the fused loops iterate over
    the same space'''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.14.0_two_builtins_standard_then_reduction.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        otrans = DynamoOMPParallelLoopTrans()
        ftrans = DynamoLoopFuseTrans()
        schedule, _ = ftrans.apply(schedule.children[0], schedule.children[1],
                                   same_space=True)
        schedule, _ = otrans.apply(schedule.children[0])
        invoke.schedule = schedule
        code = str(psy.gen)
        print code
        if distmem:
            assert (
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static), reduction(+:asum)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = bvalue*f1_proxy%data(df)\n"
                "        asum = asum+f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      !\n"
                "      ! Set halos dirty for fields modified in the above "
                "loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n") in code
        else:
            assert (
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static), reduction(+:asum)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = bvalue*f1_proxy%data(df)\n"
                "        asum = asum+f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n") in code


def test_multi_builtins_standard_then_reduction_fuse_do():
    '''test that we generate a correct OpenMP parallel do reduction for
    two different loop-fused builtins, first a normal builtin then a
    reduction. We need to specify that the fused loops iterate over
    the same space'''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.14.0_two_builtins_standard_then_reduction.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        rtrans = OMPParallelTrans()
        otrans = Dynamo0p3OMPLoopTrans()
        ftrans = DynamoLoopFuseTrans()
        schedule, _ = ftrans.apply(schedule.children[0], schedule.children[1],
                                   same_space=True)
        schedule, _ = otrans.apply(schedule.children[0], reprod=False)
        schedule, _ = rtrans.apply(schedule.children[0])
        invoke.schedule = schedule
        code = str(psy.gen)
        print code
        if distmem:
            assert (
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static), reduction(+:asum)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = bvalue*f1_proxy%data(df)\n"
                "        asum = asum+f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !\n"
                "      ! Set halos dirty for fields modified in the above "
                "loop\n"
                "      !\n"
                "      !$omp master\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !$omp end master\n"
                "      !\n"
                "      !$omp end parallel\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n") in code
        else:
            assert (
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static), reduction(+:asum)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = bvalue*f1_proxy%data(df)\n"
                "        asum = asum+f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp end parallel\n") in code

# There are no tests requires for integer reduction and no tests
# required for a builtin with more than 1 reduction as we have no
# examples in either case


def test_multi_builtins_fuse_error():
    '''test that we raise an exception when we try to loop fuse a
    reduction with another builtin that uses the value of the
    reduction as it will give us incorrect results. Only required for
    distmem=False as the global sum stops the loop fusion for
    distmem=True. We need to assert that the loop fusion is valid as
    if we don't we get a different error. '''

    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.15.0_builtins_reduction_fuse_error.f90"),
        distributed_memory=False,
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    ftrans = DynamoLoopFuseTrans()
    schedule, _ = ftrans.apply(schedule.children[0], schedule.children[1],
                               same_space=True)
    with pytest.raises(TransformationError) as excinfo:
        schedule, _ = ftrans.apply(schedule.children[0], schedule.children[1],
                                   same_space=True)
    assert ("Cannot fuse loops as the first loop has a reduction and "
            "the second loop reads the result of the "
            "reduction") in str(excinfo.value)


def test_loop_fuse_error():
    '''Test that we raise an exception in loop fusion if one or more of
    the loops has an any_space iteration space'''
    for dist_mem in [False, True]:
        _, info = parse(os.path.join(BASE_PATH,
                                     "15.0.2_multiple_set_kernels.f90"),
                        api=TEST_API, distributed_memory=dist_mem)
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        ftrans = DynamoLoopFuseTrans()
        with pytest.raises(TransformationError) as excinfo:
            schedule, _ = ftrans.apply(schedule.children[0],
                                       schedule.children[1])
        assert ("One or more of the iteration spaces is unknown "
                "('any_space') so loop fusion might be "
                "invalid") in str(excinfo.value)

# Repeat the reduction tests for the reproducible version


def test_reprod_reduction_real_pdo():
    '''Test that we raise an exception if we try to use the reprod flag
    for an OpenMP Parallel Do'''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.9.0_inner_prod_builtin.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        otrans = DynamoOMPParallelLoopTrans()
        # Apply OpenMP parallelisation to the loop
        with pytest.raises(TypeError) as excinfo:
            schedule, _ = otrans.apply(schedule.children[0], reprod=True)
        assert "apply() got an unexpected keyword argument 'reprod'" \
            in str(excinfo.value)


def test_reprod_reduction_real_do():
    '''test that we generate a correct reproducible OpenMP do reduction
    for a real scalar summed in a builtin. We use inner product in
    this case '''
    for distmem in [True, False]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.9.0_inner_prod_builtin.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        otrans = Dynamo0p3OMPLoopTrans()
        rtrans = OMPParallelTrans()
        # Apply an OpenMP do directive to the loop
        schedule, _ = otrans.apply(schedule.children[0], reprod=True)
        # Apply an OpenMP Parallel directive around the OpenMP do directive
        schedule, _ = rtrans.apply(schedule.children[0])
        invoke.schedule = schedule
        code = str(psy.gen)
        print code
        assert (
            "      USE omp_lib, ONLY: omp_get_thread_num\n"
            "      USE omp_lib, ONLY: omp_get_max_threads\n") in code
        assert (
            "      REAL(KIND=r_def), allocatable, dimension(:,:) "
            ":: l_asum\n") in code
        assert "      INTEGER th_idx\n" in code
        assert "      INTEGER nthreads\n" in code
        assert (
            "      !\n"
            "      ! Determine the number of OpenMP threads\n"
            "      !\n"
            "      nthreads = omp_get_max_threads()\n"
            "      !\n") in code
        if distmem:
            assert (
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      ALLOCATE (l_asum(8,nthreads))\n"
                "      l_asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df,th_idx)\n"
                "      th_idx = omp_get_thread_num()+1\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        l_asum(1,th_idx) = l_asum(1,th_idx)+f1_proxy%data(df)"
                "*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp end parallel\n"
                "      !\n"
                "      ! sum the partial results sequentially\n"
                "      !\n"
                "      DO th_idx=1,nthreads\n"
                "        asum = asum+l_asum(1,th_idx)\n"
                "      END DO \n"
                "      DEALLOCATE (l_asum)\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()") in code
        else:
            assert (
                "      asum = 0.0_r_def\n"
                "      ALLOCATE (l_asum(8,nthreads))\n"
                "      l_asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df,th_idx)\n"
                "      th_idx = omp_get_thread_num()+1\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        l_asum(1,th_idx) = l_asum(1,th_idx)+f1_proxy%data(df)"
                "*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp end parallel\n"
                "      !\n"
                "      ! sum the partial results sequentially\n"
                "      !\n"
                "      DO th_idx=1,nthreads\n"
                "        asum = asum+l_asum(1,th_idx)\n"
                "      END DO \n"
                "      DEALLOCATE (l_asum)\n") in code


def test_no_global_sum_in_parallel_region():
    '''test that we raise an error if we try to put a parallel region
    around loops with a global sum'''
    for distmem in [True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.13.0_two_builtins_reduction_then_standard.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        otrans = Dynamo0p3OMPLoopTrans()
        rtrans = OMPParallelTrans()
        # Apply an OpenMP do to the loop
        from psyclone.psyGen import Loop
        for child in schedule.children:
            if isinstance(child, Loop):
                schedule, _ = otrans.apply(child, reprod=True)
        schedule, _ = rtrans.apply(schedule.children)
        invoke.schedule = schedule
        with pytest.raises(NotImplementedError) as excinfo:
            _ = str(psy.gen)
        assert(
            "Cannot correctly generate code for an OpenMP parallel region "
            "containing children of different types") in str(excinfo.value)


def test_reprod_multi_builtins_reduction_then_standard_do():
    '''test that we generate a correct reproducible OpenMP do reduction
    for two different builtins, first a reduction then not when we
    have reprod set to True '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.13.0_two_builtins_reduction_then_standard.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        otrans = Dynamo0p3OMPLoopTrans()
        rtrans = OMPParallelTrans()
        # Apply an OpenMP do to the loop
        from psyclone.psyGen import Loop
        for child in schedule.children:
            if isinstance(child, Loop):
                schedule, _ = otrans.apply(child, reprod=True)
        if distmem:
            mtrans = MoveTrans()
            schedule, _ = mtrans.apply(schedule.children[1],
                                       schedule.children[2],
                                       position="after")
        schedule, _ = rtrans.apply(schedule.children[0:2])
        invoke.schedule = schedule
        code = str(psy.gen)
        print code
        assert (
            "      USE omp_lib, ONLY: omp_get_thread_num\n"
            "      USE omp_lib, ONLY: omp_get_max_threads\n") in code
        assert (
            "      REAL(KIND=r_def), allocatable, dimension(:,:) "
            ":: l_asum\n") in code
        assert "      INTEGER th_idx\n" in code
        assert "      INTEGER nthreads\n" in code
        assert (
            "      !\n"
            "      ! Determine the number of OpenMP threads\n"
            "      !\n"
            "      nthreads = omp_get_max_threads()\n"
            "      !\n") in code
        if distmem:
            assert (
                "      asum = 0.0_r_def\n"
                "      ALLOCATE (l_asum(8,nthreads))\n"
                "      l_asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df,th_idx)\n"
                "      th_idx = omp_get_thread_num()+1\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        l_asum(1,th_idx) = l_asum(1,th_idx)+f1_proxy%data(df)"
                "*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,bsum_proxy%vspace%get_last_dof_owned()\n"
                "        bsum_proxy%data(df) = f1*bsum_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !\n"
                "      ! Set halos dirty for fields modified in the above "
                "loop\n"
                "      !\n"
                "      !$omp master\n"
                "      CALL bsum_proxy%set_dirty()\n"
                "      !$omp end master\n"
                "      !\n"
                "      !$omp end parallel\n"
                "      !\n"
                "      ! sum the partial results sequentially\n"
                "      !\n"
                "      DO th_idx=1,nthreads\n"
                "        asum = asum+l_asum(1,th_idx)\n"
                "      END DO \n"
                "      DEALLOCATE (l_asum)\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n") in code
        else:
            assert (
                "      asum = 0.0_r_def\n"
                "      ALLOCATE (l_asum(8,nthreads))\n"
                "      l_asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df,th_idx)\n"
                "      th_idx = omp_get_thread_num()+1\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        l_asum(1,th_idx) = l_asum(1,th_idx)+f1_proxy%data(df)"
                "*f2_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,undf_any_space_1_bsum\n"
                "        bsum_proxy%data(df) = f1*bsum_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp end parallel\n"
                "      !\n"
                "      ! sum the partial results sequentially\n"
                "      !\n"
                "      DO th_idx=1,nthreads\n"
                "        asum = asum+l_asum(1,th_idx)\n"
                "      END DO \n"
                "      DEALLOCATE (l_asum)\n") in code


def test_reprod_multi_builtins_reduction_then_standard_fuse_do():
    '''test that we generate a correct reproducible OpenMP do reduction
    for two different loop-fused builtins, first a reduction then
    not. We need to specify that the fused loops are on the same
    iteration space.'''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.13.0_two_builtins_reduction_then_standard.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        if distmem:
            mtrans = MoveTrans()
            schedule, _ = mtrans.apply(schedule.children[1],
                                       schedule.children[2],
                                       position="after")
        rtrans = OMPParallelTrans()
        otrans = Dynamo0p3OMPLoopTrans()
        ftrans = DynamoLoopFuseTrans()
        schedule, _ = ftrans.apply(schedule.children[0], schedule.children[1],
                                   same_space=True)
        schedule, _ = otrans.apply(schedule.children[0], reprod=True)
        schedule, _ = rtrans.apply(schedule.children[0])
        invoke.schedule = schedule
        code = str(psy.gen)
        print code
        assert (
            "      USE omp_lib, ONLY: omp_get_thread_num\n"
            "      USE omp_lib, ONLY: omp_get_max_threads\n") in code
        assert (
            "      REAL(KIND=r_def), allocatable, dimension(:,:) "
            ":: l_asum\n") in code
        assert "      INTEGER th_idx\n" in code
        assert "      INTEGER nthreads\n" in code
        assert (
            "      !\n"
            "      ! Determine the number of OpenMP threads\n"
            "      !\n"
            "      nthreads = omp_get_max_threads()\n"
            "      !\n") in code
        if distmem:
            assert (
                "      asum = 0.0_r_def\n"
                "      ALLOCATE (l_asum(8,nthreads))\n"
                "      l_asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df,th_idx)\n"
                "      th_idx = omp_get_thread_num()+1\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        l_asum(1,th_idx) = l_asum(1,th_idx)+"
                "f1_proxy%data(df)*f2_proxy%data(df)\n"
                "        bsum_proxy%data(df) = f1*bsum_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !\n"
                "      ! Set halos dirty for fields modified in the "
                "above loop\n"
                "      !\n"
                "      !$omp master\n"
                "      CALL bsum_proxy%set_dirty()\n"
                "      !$omp end master\n"
                "      !\n"
                "      !$omp end parallel\n"
                "      !\n"
                "      ! sum the partial results sequentially\n"
                "      !\n"
                "      DO th_idx=1,nthreads\n"
                "        asum = asum+l_asum(1,th_idx)\n"
                "      END DO \n"
                "      DEALLOCATE (l_asum)\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n") in code
        else:
            assert (
                "      asum = 0.0_r_def\n"
                "      ALLOCATE (l_asum(8,nthreads))\n"
                "      l_asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df,th_idx)\n"
                "      th_idx = omp_get_thread_num()+1\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        l_asum(1,th_idx) = l_asum(1,th_idx)+f1_proxy%data(df)"
                "*f2_proxy%data(df)\n"
                "        bsum_proxy%data(df) = f1*bsum_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp end parallel\n"
                "      !\n"
                "      ! sum the partial results sequentially\n"
                "      !\n"
                "      DO th_idx=1,nthreads\n"
                "        asum = asum+l_asum(1,th_idx)\n"
                "      END DO \n"
                "      DEALLOCATE (l_asum)\n") in code


def test_reprod_multi_builtins_standard_then_reduction_fuse_do():
    '''test that we generate a correct OpenMP do reduction for
    two different loop-fused builtins, first a normal builtin then a
    reduction. We need to specify that the fused loops iterate over
    the same space'''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.14.0_two_builtins_standard_then_reduction.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        rtrans = OMPParallelTrans()
        otrans = Dynamo0p3OMPLoopTrans()
        ftrans = DynamoLoopFuseTrans()
        schedule, _ = ftrans.apply(schedule.children[0], schedule.children[1],
                                   same_space=True)
        schedule, _ = otrans.apply(schedule.children[0], reprod=True)
        schedule, _ = rtrans.apply(schedule.children[0])
        invoke.schedule = schedule
        code = str(psy.gen)
        print code
        assert "      INTEGER th_idx\n" in code
        if distmem:
            assert (
                "      asum = 0.0_r_def\n"
                "      ALLOCATE (l_asum(8,nthreads))\n"
                "      l_asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df,th_idx)\n"
                "      th_idx = omp_get_thread_num()+1\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                "        f1_proxy%data(df) = bvalue*f1_proxy%data(df)\n"
                "        l_asum(1,th_idx) = l_asum(1,th_idx)+"
                "f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !\n"
                "      ! Set halos dirty for fields modified in the "
                "above loop\n"
                "      !\n"
                "      !$omp master\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !$omp end master\n"
                "      !\n"
                "      !$omp end parallel\n"
                "      !\n"
                "      ! sum the partial results sequentially\n"
                "      !\n"
                "      DO th_idx=1,nthreads\n"
                "        asum = asum+l_asum(1,th_idx)\n"
                "      END DO \n"
                "      DEALLOCATE (l_asum)\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n") in code
        else:
            assert (
                "      asum = 0.0_r_def\n"
                "      ALLOCATE (l_asum(8,nthreads))\n"
                "      l_asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df,th_idx)\n"
                "      th_idx = omp_get_thread_num()+1\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = bvalue*f1_proxy%data(df)\n"
                "        l_asum(1,th_idx) = l_asum(1,th_idx)+"
                "f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp end parallel\n"
                "      !\n"
                "      ! sum the partial results sequentially\n"
                "      !\n"
                "      DO th_idx=1,nthreads\n"
                "        asum = asum+l_asum(1,th_idx)\n"
                "      END DO \n"
                "      DEALLOCATE (l_asum)\n") in code


def test_reprod_three_builtins_two_reductions_do():
    '''test that we generate correct reproducible OpenMP do reductions
    when we have three different builtins, first a reduction, then a
    normal builtin then a reduction. '''
    from psyclone.dynamo0p3 import DynLoop
    from psyclone.psyGen import OMPDoDirective
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.16.0_three_builtins_two_reductions.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        rtrans = OMPParallelTrans()
        otrans = Dynamo0p3OMPLoopTrans()
        for child in schedule.children:
            if isinstance(child, DynLoop):
                schedule, _ = otrans.apply(child, reprod=True)
        for child in schedule.children:
            if isinstance(child, OMPDoDirective):
                schedule, _ = rtrans.apply(child)
        invoke.schedule = schedule
        code = str(psy.gen)
        print code
        assert "INTEGER th_idx\n" in code
        if distmem:
            for names in [
                    {"var": "asum", "lvar": "l_asum",
                     "bounds": "f1_proxy%vspace%get_last_dof_owned()",
                     "rhs": "f1_proxy%data(df)*f2_proxy%data(df)"},
                    {"var": "bsum", "lvar": "l_bsum",
                     "bounds": "f2_proxy%vspace%get_last_dof_owned()",
                     "rhs": "f2_proxy%data(df)"}]:
                assert (
                    "      " + names["var"] + " = 0.0_r_def\n"
                    "      ALLOCATE (" + names["lvar"] + "(8,nthreads))\n"
                    "      " + names["lvar"] + " = 0.0_r_def\n"
                    "      !\n"
                    "      !$omp parallel default(shared), "
                    "private(df,th_idx)\n"
                    "      th_idx = omp_get_thread_num()+1\n"
                    "      !$omp do schedule(static)\n"
                    "      DO df=1," + names["bounds"] + "\n"
                    "        " + names["lvar"] + "(1,th_idx) = " +
                    names["lvar"] + "(1,th_idx)+" + names["rhs"] + "\n"
                    "      END DO \n"
                    "      !$omp end do\n"
                    "      !$omp end parallel\n"
                    "      !\n"
                    "      ! sum the partial results sequentially\n"
                    "      !\n"
                    "      DO th_idx=1,nthreads\n"
                    "        " + names["var"] + " = " + names["var"] + "+" +
                    names["lvar"] + "(1,th_idx)\n"
                    "      END DO \n"
                    "      DEALLOCATE (" + names["lvar"] + ")\n"
                    "      global_sum%value = " + names["var"] + "\n"
                    "      " + names["var"] + " = "
                    "global_sum%get_sum()\n") in code
        else:
            for names in [
                    {"var": "asum", "lvar": "l_asum",
                     "bounds": "undf_any_space_1_f1",
                     "rhs": "f1_proxy%data(df)*f2_proxy%data(df)"},
                    {"var": "bsum", "lvar": "l_bsum",
                     "bounds": "undf_any_space_1_f2",
                     "rhs": "f2_proxy%data(df)"}]:
                assert (
                    "      " + names["var"] + " = 0.0_r_def\n"
                    "      ALLOCATE (" + names["lvar"] + "(8,nthreads))\n"
                    "      " + names["lvar"] + " = 0.0_r_def\n"
                    "      !\n"
                    "      !$omp parallel default(shared), "
                    "private(df,th_idx)\n"
                    "      th_idx = omp_get_thread_num()+1\n"
                    "      !$omp do schedule(static)\n"
                    "      DO df=1," + names["bounds"] + "\n"
                    "        " + names["lvar"] + "(1,th_idx) = " +
                    names["lvar"] + "(1,th_idx)+" + names["rhs"] + "\n"
                    "      END DO \n"
                    "      !$omp end do\n"
                    "      !$omp end parallel\n"
                    "      !\n"
                    "      ! sum the partial results sequentially\n"
                    "      !\n"
                    "      DO th_idx=1,nthreads\n"
                    "        " + names["var"] + " = " + names["var"] + "+" +
                    names["lvar"] + "(1,th_idx)\n"
                    "      END DO \n"
                    "      DEALLOCATE (" + names["lvar"] + ")\n") in code


def test_reprod_view(capsys):
    '''test that we generate a correct view() for OpenMP do reductions '''
    from psyclone.dynamo0p3 import DynLoop
    from psyclone.psyGen import OMPDoDirective
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.16.0_three_builtins_two_reductions.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        rtrans = OMPParallelTrans()
        otrans = Dynamo0p3OMPLoopTrans()
        for child in schedule.children:
            if isinstance(child, DynLoop):
                schedule, _ = otrans.apply(child, reprod=True)
        for child in schedule.children:
            if isinstance(child, OMPDoDirective):
                schedule, _ = rtrans.apply(child)
        invoke.schedule = schedule
        schedule.view()
        # only display reprod in schedule view if a reduction
        result, _ = capsys.readouterr()
        if distmem:
            expected = (
                "Schedule[invoke='invoke_0' dm=True]\n"
                "    Directive[OMP parallel]\n"
                "        Directive[OMP do][reprod=True]\n"
                "            Loop[type='dofs',field_space='any_space_1',"
                "it_space='dofs']\n"
                "                Call inner_product(f1,f2,asum)\n"
                "    GlobalSum[scalar='asum']\n"
                "    Directive[OMP parallel]\n"
                "        Directive[OMP do]\n"
                "            Loop[type='dofs',field_space='any_space_1',"
                "it_space='dofs']\n"
                "                Call scale_field(f1,asum)\n"
                "    Directive[OMP parallel]\n"
                "        Directive[OMP do][reprod=True]\n"
                "            Loop[type='dofs',field_space='any_space_1',"
                "it_space='dofs']\n"
                "                Call sum_field(f2,bsum)\n"
                "    GlobalSum[scalar='bsum']\n")
        else:
            expected = (
                "Schedule[invoke='invoke_0' dm=False]\n"
                "    Directive[OMP parallel]\n"
                "        Directive[OMP do][reprod=True]\n"
                "            Loop[type='dofs',field_space='any_space_1',"
                "it_space='dofs']\n"
                "                Call inner_product(f1,f2,asum)\n"
                "    Directive[OMP parallel]\n"
                "        Directive[OMP do]\n"
                "            Loop[type='dofs',field_space='any_space_1',"
                "it_space='dofs']\n"
                "                Call scale_field(f1,asum)\n"
                "    Directive[OMP parallel]\n"
                "        Directive[OMP do][reprod=True]\n"
                "            Loop[type='dofs',field_space='any_space_1',"
                "it_space='dofs']\n"
                "                Call sum_field(f2,bsum)\n")

        print "Expected ..."
        print expected
        print "Found ..."
        print result
        assert expected in result


def test_reductions_reprod():
    '''Check that the optional reprod argument to reductions() method
    works as expected'''
    for reprod in [False, True]:
        for distmem in [True, False]:
            _, invoke_info = parse(
                os.path.join(BASE_PATH,
                             "15.9.0_inner_prod_builtin.f90"),
                distributed_memory=distmem,
                api="dynamo0.3")
            psy = PSyFactory("dynamo0.3",
                             distributed_memory=distmem).create(invoke_info)
            invoke = psy.invokes.invoke_list[0]
            schedule = invoke.schedule
            otrans = Dynamo0p3OMPLoopTrans()
            rtrans = OMPParallelTrans()
            # Apply an OpenMP do directive to the loop
            schedule, _ = otrans.apply(schedule.children[0], reprod=reprod)
            # Apply an OpenMP Parallel directive around the OpenMP do directive
            schedule, _ = rtrans.apply(schedule.children[0])
            invoke.schedule = schedule
            assert len(schedule.reductions(reprod=reprod)) == 1
            assert len(schedule.reductions(reprod=not reprod)) == 0
            assert len(schedule.reductions()) == 1
            from psyclone.dynamo0p3_builtins import DynInnerProductKern
            assert (isinstance(schedule.reductions(reprod=reprod)[0],
                               DynInnerProductKern))


def test_list_multiple_reductions():
    '''test that we produce correct reduction lists when there is more
    than one reduction in a OpenMP parallel directive. As only one
    reduction per OpenMP parallel region is currently supported we
    need to modify the internal representation after the
    transformations have been performed to enable this test'''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.9.0_inner_prod_builtin.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        otrans = Dynamo0p3OMPLoopTrans()
        rtrans = OMPParallelTrans()
        # Apply an OpenMP do directive to the loop
        schedule, _ = otrans.apply(schedule.children[0], reprod=False)
        # Apply an OpenMP Parallel directive around the OpenMP do directive
        schedule, _ = rtrans.apply(schedule.children[0])
        invoke.schedule = schedule
        omp_loop_directive = schedule.children[0].children[0]
        call = omp_loop_directive.children[0].children[0]
        arg = call.arguments.args[1]
        arg._type = "gh_real"
        arg.descriptor._access = "gh_sum"
        result = omp_loop_directive._reduction_string()
        assert ", reduction(+:f2), reduction(+:asum)" in result


def test_move_name():
    ''' Test the name property of the MoveTrans class '''
    move_trans = MoveTrans()
    name = move_trans.name
    assert name == "Move"


def test_move_str():
    ''' Test the str method of the MoveTrans class '''
    move_trans = MoveTrans()
    name = str(move_trans)
    assert name == "Move a node to a different location"


def test_move_valid_node():
    '''Test that MoveTrans raises an exception if an invalid node
    argument is passed'''
    _, info = parse(os.path.join(BASE_PATH,
                                 "4.2_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    move_trans = MoveTrans()
    with pytest.raises(TransformationError) as excinfo:
        move_trans.apply(None, schedule.children[0])
    assert ("In the Move transformation apply method the "
            "first argument is not a Node") in str(excinfo)


def test_move_back():
    '''Test that MoveTrans moves the node backwards to the expected
    location'''
    _, info = parse(os.path.join(BASE_PATH,
                                 "15.0.2_multiple_set_kernels.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    move_trans = MoveTrans()
    initial_index = 2
    target_index = 0
    orig_arg = schedule.children[initial_index]
    new_arg = schedule.children[target_index]
    assert orig_arg != new_arg

    move_trans.apply(schedule.children[initial_index],
                     schedule.children[target_index])

    new_arg = schedule.children[target_index]
    assert orig_arg == new_arg


def test_move_back_after():
    '''Test that MoveTrans moves the node backwards to the expected
    location when location="after" '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "15.0.2_multiple_set_kernels.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    move_trans = MoveTrans()
    initial_index = 2
    target_index = 0
    orig_arg = schedule.children[initial_index]
    new_arg = schedule.children[target_index]
    assert orig_arg != new_arg

    move_trans.apply(schedule.children[initial_index],
                     schedule.children[target_index],
                     position="after")

    new_arg = schedule.children[target_index+1]
    assert orig_arg == new_arg


def test_move_forward():
    '''Test that MoveTrans moves the node forwards to the expected
    location'''
    _, info = parse(os.path.join(BASE_PATH,
                                 "15.0.2_multiple_set_kernels.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    move_trans = MoveTrans()
    initial_index = 0
    target_index = 2
    orig_arg = schedule.children[initial_index]
    new_arg = schedule.children[target_index]
    schedule.view()
    assert orig_arg != new_arg

    move_trans.apply(schedule.children[initial_index],
                     schedule.children[target_index])

    new_arg = schedule.children[target_index-1]
    schedule.view()
    assert orig_arg == new_arg


def test_move_forward_after():
    '''Test that MoveTrans moves the node forwards to the expected
    location when location="after" '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "15.0.2_multiple_set_kernels.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    move_trans = MoveTrans()
    initial_index = 0
    target_index = 2
    orig_arg = schedule.children[initial_index]
    new_arg = schedule.children[target_index]
    schedule.view()
    assert orig_arg != new_arg

    move_trans.apply(schedule.children[initial_index],
                     schedule.children[target_index],
                     position="after")

    new_arg = schedule.children[target_index]
    schedule.view()
    assert orig_arg == new_arg


# test that move with dependencies fails
def test_move_fail():
    '''Test that MoveTrans fails to move the node backwards and forwards
    if there is a dependence. '''
    _, info = parse(os.path.join(BASE_PATH, "15.3.4_multi_axpy_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    move_trans = MoveTrans()
    initial_index = 6
    target_index = 0
    with pytest.raises(TransformationError) as excinfo:
        move_trans.apply(schedule.children[initial_index],
                         schedule.children[target_index])
    assert "data dependencies forbid the move" in str(excinfo.value)

    initial_index = 0
    target_index = 6
    with pytest.raises(TransformationError) as excinfo:
        move_trans.apply(schedule.children[initial_index],
                         schedule.children[target_index])
    assert "data dependencies forbid the move" in str(excinfo.value)
