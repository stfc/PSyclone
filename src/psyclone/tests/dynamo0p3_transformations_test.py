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
# -----------------------------------------------------------------------------
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab
# Modified I. Kavcic, Met Office

''' Tests of transformations with the Dynamo 0.3 API '''

from __future__ import absolute_import, print_function
import os
import pytest
from psyclone.parse import parse
from psyclone import psyGen
from psyclone.psyGen import PSyFactory, GenerationError, InternalError
from psyclone.transformations import TransformationError, \
    OMPParallelTrans, \
    Dynamo0p3ColourTrans, \
    Dynamo0p3OMPLoopTrans, \
    DynamoOMPParallelLoopTrans, \
    DynamoLoopFuseTrans, \
    KernelModuleInlineTrans, \
    MoveTrans, \
    Dynamo0p3RedundantComputationTrans, \
    Dynamo0p3AsyncHaloExchangeTrans
from psyclone.configuration import Config
from psyclone_test_utils import TEST_COMPILE, code_compiles


# The version of the API that the tests in this file
# exercise.
TEST_API = "dynamo0.3"
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")


def test_colour_trans_declarations(tmpdir, f90, f90flags, dist_mem):
    '''Check that we generate the correct variable declarations when
    doing a colouring transformation. We check when distributed memory
    is both off and on. '''
    # test of the colouring transformation of a single loop
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
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
    # a string (Fortran is not case sensitive)
    gen = str(psy.gen).lower()

    # Check that we've declared the loop-related variables
    # and colour-map pointers
    assert "integer, pointer :: cmap(:,:)" in gen
    assert "integer ncolour" in gen
    assert "integer colour" in gen

    if TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag
        # to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_colour_trans(tmpdir, f90, f90flags, dist_mem):
    '''test of the colouring transformation of a single loop. We test
    when distributed memory is both off and on. '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
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
    # a string (Fortran is not case sensitive)
    gen = str(psy.gen).lower()

    # Check that we're calling the API to get the no. of colours
    # and the generated loop bounds are correct
    output = ("      ncolour = mesh%get_ncolours()\n"
              "      cmap => mesh%get_colour_map()\n")
    assert output in gen
    if dist_mem:
        output = (
            "      do colour=1,ncolour\n"
            "        do cell=1,mesh%get_last_halo_cell_per_colour("
            "colour,1)\n")
    else:  # not dist_mem
        output = (
            "      do colour=1,ncolour\n"
            "        do cell=1,mesh%get_last_edge_cell_per_colour("
            "colour)\n")
    assert output in gen

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
            "      ! set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      call f1_proxy%set_dirty()\n")
        assert dirty_str in gen
        assert gen.count("set_dirty()") == 1

    if TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag
        # to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_colour_trans_operator(tmpdir, f90, f90flags):
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
        print(gen)

        # check the first argument is a colourmap lookup
        assert "CALL testkern_operator_code(cmap(colour, cell), nlayers" in gen

        if TEST_COMPILE:
            # If compilation testing has been enabled (--compile flag
            # to py.test)
            assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_colour_trans_cma_operator(tmpdir, f90, f90flags, dist_mem):
    '''test of the colouring transformation of a single loop with a CMA
    operator. We check that the first argument is a colourmap lookup,
    not a direct cell index. We test when distributed memory is both
    off and on. '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "20.3_cma_assembly_field.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
    invoke = psy.invokes.get(
        'invoke_0_columnwise_op_asm_field_kernel_type')
    schedule = invoke.schedule
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

    if dist_mem:
        lookup = "get_last_halo_cell_per_colour(colour,1)"
    else:
        lookup = "get_last_edge_cell_per_colour(colour)"

    assert (
        "      DO colour=1,ncolour\n"
        "        DO cell=1,mesh%{0}\n"
        "          !\n"
        "          CALL columnwise_op_asm_field_kernel_code("
        "cmap(colour, ".format(lookup)) in gen

    assert (
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

    if TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag
        # to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_colour_trans_stencil():
    '''test of the colouring transformation of a single loop with a
    stencil access. We test when distributed memory is both off and
    on. '''
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
        print(gen)

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
    on or off. '''
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
                                 "15.12.3_single_pointwise_builtin.f90"),
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
    ''' Test the name property of the Dynamo0p3OMPLoopTrans class. '''
    olooptrans = Dynamo0p3OMPLoopTrans()
    oname = olooptrans.name
    assert oname == "Dynamo0p3OMPLoopTrans"


def test_omp_str():
    ''' Test the str method of the Dynamo0p3OMPLoopTrans class. '''
    olooptrans = Dynamo0p3OMPLoopTrans()
    oname = str(olooptrans)
    assert oname == "Add an OpenMP DO directive to a Dynamo 0.3 loop"


def test_omp_not_a_loop():
    '''Test that we raise an appropriate error if we attempt to apply an
    OpenMP DO transformation to something that is not a loop. We test
    when distributed memory is on or off. '''
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

        assert ("Cannot apply a parallel-loop directive to something "
                "that is not a loop" in str(excinfo))


def test_omp_parallel_not_a_loop():
    '''Test that we raise an appropriate error if we attempt to apply an
    OpenMP PARALLEL DO transformation to something that is not a
    loop. We test when distributed memory is on or off. '''
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
    ''' Test the name property of the Dynamo0p3ColourTrans class. '''
    ctrans = Dynamo0p3ColourTrans()
    cname = ctrans.name
    assert cname == "Dynamo0p3LoopColourTrans"


def test_colour_str():
    ''' Test the str method of the Dynamo0p3ColourTrans class. '''
    ctrans = Dynamo0p3ColourTrans()
    cstr = str(ctrans)
    assert cstr == "Split a Dynamo 0.3 loop over cells into colours"


def test_omp_colour_trans(tmpdir, f90, f90flags, dist_mem):
    '''Test the OpenMP transformation applied to a coloured loop. We test
    when distributed memory is on or off. '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
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

    assert ("      ncolour = mesh%get_ncolours()\n"
            "      cmap => mesh%get_colour_map()\n" in code)
    if dist_mem:
        lookup = "get_last_halo_cell_per_colour(colour,1)"
    else:
        lookup = "get_last_edge_cell_per_colour(colour)"
    output = (
        "      DO colour=1,ncolour\n"
        "        !$omp parallel do default(shared), private(cell), "
        "schedule(static)\n"
        "        DO cell=1,mesh%{0}\n".format(lookup))
    assert output in code

    if TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag
        # to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_omp_colour_orient_trans(monkeypatch, annexed):
    '''Test the OpenMP transformation applied to a coloured loop when the
    kernel expects orientation information. We test when distributed
    memory is on or off. We also test when annexed is False and True
    as it affects how many halo exchanges are generated.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "9.1_orientation2.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0_testkern_orientation2_type')
        schedule = invoke.schedule

        ctrans = Dynamo0p3ColourTrans()
        otrans = DynamoOMPParallelLoopTrans()

        if dist_mem:
            if annexed:
                index = 4
            else:
                index = 5
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


def test_omp_parallel_colouring_needed(monkeypatch, annexed):
    '''Test that we raise an error when applying an OpenMP PARALLEL DO
    transformation to a loop that requires colouring (i.e. has a field
    with 'INC' access) but is not coloured. We test when distributed
    memory is on or off. We also test when annexed is False and True
    as it affects how many halo exchanges are generated.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "11_any_space.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        if dist_mem:
            if annexed:
                index = 4
            else:
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


def test_omp_colouring_needed(monkeypatch, annexed):
    '''Test that we raise an error when applying an OpenMP DO
    transformation to a loop that requires colouring (i.e. has a field
    with 'INC' access) but is not coloured. We test when distributed
    memory is on or off. We also test when annexed is False and True
    as it affects how many halo exchanges are generated.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "11_any_space.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        if dist_mem:
            if annexed:
                index = 4
            else:
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


def test_check_seq_colours_omp_parallel_do(monkeypatch, annexed):
    '''Test that we raise an error if the user attempts to apply an OpenMP
    PARALLEL DO transformation to a loop over colours (since any such
    loop must be sequential). We test when distributed memory is on or
    off. We also test when annexed is False and True as it affects how
    many halo exchanges are generated.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "9.1_orientation2.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0_testkern_orientation2_type')
        schedule = invoke.schedule
        if dist_mem:
            if annexed:
                index = 4
            else:
                index = 5
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


def test_check_seq_colours_omp_do(tmpdir, f90, f90flags, monkeypatch, annexed):
    '''Test that we raise an error if the user attempts to apply an OpenMP
    DO transformation to a loop over colours (since any such loop must
    be sequential). We test when distributed memory is on or off. We
    also test when annexed is False and True as it affects how many
    halo exchanges are generated.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "9.1_orientation2.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0_testkern_orientation2_type')
        schedule = invoke.schedule
        if dist_mem:
            if annexed:
                index = 4
            else:
                index = 5
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

        if TEST_COMPILE:
            # If compilation testing has been enabled (--compile flag
            # to py.test) This test checks the code without OpenMP as
            # this transformation fails
            assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_colouring_after_openmp():
    '''Test that we raise an error if the user attempts to colour a loop
    that is already within an OpenMP parallel region. We test when
    distributed memory is on or off. '''
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

        # Apply OpenMP to the loop
        schedule, _ = otrans.apply(schedule.children[index])

        # Now attempt to colour the loop within this OpenMP region
        with pytest.raises(TransformationError) as excinfo:
            schedule, _ = ctrans.apply(schedule.children[index].children[0])
        assert "Cannot have a loop over colours" in str(excinfo.value)
        assert "within an OpenMP parallel region" in str(excinfo.value)


def test_colouring_multi_kernel(monkeypatch, annexed, dist_mem):
    '''Test that we correctly generate all the map-lookups etc.  when an
    invoke contains more than one kernel. We test when distributed
    memory is on or off. We also test when annexed is False and True
    as it affects how many halo exchanges are generated.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4.6_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
    invoke = psy.invokes.get('invoke_0')
    schedule = invoke.schedule

    ctrans = Dynamo0p3ColourTrans()
    otrans = DynamoOMPParallelLoopTrans()
    mtrans = MoveTrans()

    if dist_mem:
        if annexed:
            index = 5
        else:
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

    # Check that we're calling the API to get the no. of colours
    assert gen.count("cmap => mesh%get_colour_map()") == 1
    assert "private(cell)" in gen
    assert gen.count("private(cell)") == 2


def test_omp_region_omp_do():
    '''Test that we correctly generate code for the case of a single OMP
    DO within an OMP PARALLEL region without colouring. We test when
    distributed memory is on or off. '''
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

        print(code)

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


def test_omp_region_omp_do_rwdisc(monkeypatch, annexed):
    '''Test that we correctly generate code for the case of a single OMP
    DO within an OMP PARALLEL region without colouring when a
    discontinuous field has readwrite access. We test when distributed
    memory is on or off. Also test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke_w3.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0_testkern_w3_type')
        schedule = invoke.schedule
        olooptrans = Dynamo0p3OMPLoopTrans()
        ptrans = OMPParallelTrans()
        # Put an OMP PARALLEL around this loop
        if dist_mem and not annexed:
            # there are 3 halo exchange calls
            index = 3
        else:
            # there are no halo exchange calls
            index = 0
        child = schedule.children[index]
        oschedule, _ = ptrans.apply(child)

        # Put an OMP DO around this loop
        schedule, _ = olooptrans.apply(oschedule.children[index].children[0])

        # Replace the original loop schedule with the transformed one
        invoke.schedule = schedule

        # Store the results of applying this code transformation as
        # a string
        code = str(psy.gen)

        print(code)

        omp_do_idx = -1
        omp_para_idx = -1
        cell_loop_idx = -1
        omp_enddo_idx = -1
        if dist_mem:
            loop_str = "cell=1,mesh%get_last_edge_cell()"
        else:
            loop_str = "DO cell=1,m2_proxy%vspace%get_ncell()"
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
    within a single OMP region. '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4_multikernel_invokes.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0')
        schedule = invoke.schedule

        if dist_mem:
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
        print(code)

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


def test_multi_different_kernel_omp(monkeypatch, annexed):
    '''Test that we correctly generate the OpenMP private lists when we
    have more than one kernel of a different type (requiring a
    different private list) within an invoke. Test with and without
    DM. We also test when annexed is False and True as it affects how
    many halo exchanges are generated.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4.7_multikernel_invokes.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0')
        schedule = invoke.schedule

        if dist_mem:
            if annexed:
                index1 = 5
                index2 = 8
            else:
                index1 = 6
                index2 = 9
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
        print(code)

        assert "private(cell)" in code


def test_loop_fuse_different_spaces(monkeypatch):
    '''Test that we raise an appropriate error if the user attempts to
    fuse loops that are on different spaces. We test with annexed is
    False as this is how the test has been set up.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", False)
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
    ''' Test that we catch an unexpected error when loop fusing. '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4_multikernel_invokes.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0')
        schedule = invoke.schedule

        if dist_mem:
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
    ''' Test that we are able to fuse two loops together. '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4_multikernel_invokes.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0')
        schedule = invoke.schedule

        if dist_mem:
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
    the expected set_dirty() calls. '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.get('invoke_0')
    schedule = invoke.schedule
    ftrans = DynamoLoopFuseTrans()
    # Fuse the loops
    schedule, _ = ftrans.apply(schedule.children[3],
                               schedule.children[4])
    schedule.view()
    gen = str(psy.gen)
    print(gen)
    assert gen.count("set_dirty()") == 1


def test_loop_fuse_omp():
    '''Test that we can loop-fuse two loop nests and enclose them in an
       OpenMP parallel region. '''
    # pylint: disable=too-many-branches
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4_multikernel_invokes.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0')
        schedule = invoke.schedule

        if dist_mem:
            index = 3
        else:
            index = 0

        ftrans = DynamoLoopFuseTrans()
        otrans = DynamoOMPParallelLoopTrans()

        schedule, _ = ftrans.apply(schedule.children[index],
                                   schedule.children[index+1])

        schedule, _ = otrans.apply(schedule.children[index])

        code = str(psy.gen)
        print(code)

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


def test_loop_fuse_omp_rwdisc(tmpdir, f90, f90flags, monkeypatch, annexed):
    '''Test that we can loop-fuse two loop nests and enclose them in an
    OpenMP parallel region for a kernel with a discontinuous field has
    readwrite access. We test when distributed memory is on or
    off. Also test with and without annexed dofs being computed as
    this affects the generated code.

    '''
    # pylint: disable=too-many-branches
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4.13_multikernel_invokes_w3.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0')
        schedule = invoke.schedule

        ftrans = DynamoLoopFuseTrans()
        otrans = DynamoOMPParallelLoopTrans()

        if dist_mem and not annexed:
            # there are 3 halo exchange calls
            index = 3
        else:
            # there are no halo exchange calls
            index = 0
        schedule, _ = ftrans.apply(schedule.children[index],
                                   schedule.children[index+1])

        schedule, _ = otrans.apply(schedule.children[index])

        code = str(psy.gen)
        print(code)

        # Check generated code
        omp_para_idx = -1
        omp_endpara_idx = -1
        cell_do_idx = -1
        cell_enddo_idx = -1
        call1_idx = -1
        call2_idx = -1
        if dist_mem:
            loop_str = "DO cell=1,mesh%get_last_edge_cell()"
        else:
            loop_str = "DO cell=1,m2_proxy%vspace%get_ncell()"
        for idx, line in enumerate(code.split('\n')):
            if loop_str in line:
                cell_do_idx = idx
            if "!$omp parallel do default(shared), " +\
               "private(cell), schedule(static)" in line:
                omp_para_idx = idx
            if "CALL testkern_w3_code" in line:
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

        if TEST_COMPILE:
            # If compilation testing has been enabled (--compile flag
            # to py.test)
            assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_fuse_colour_loops(tmpdir, f90, f90flags, monkeypatch, annexed,
                           dist_mem):
    '''Test that we can fuse colour loops , enclose them in an OpenMP
    parallel region and preceed each by an OpenMP DO for both
    sequential and distributed-memory code. We also test when annexed
    is False and True as it affects how many halo exchanges are
    generated.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4.6_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
    invoke = psy.invokes.get('invoke_0')
    schedule = invoke.schedule

    ctrans = Dynamo0p3ColourTrans()
    otrans = Dynamo0p3OMPLoopTrans()
    rtrans = OMPParallelTrans()
    ftrans = DynamoLoopFuseTrans()
    mtrans = MoveTrans()

    if dist_mem:
        if annexed:
            index = 5
        else:
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
    assert "      ncolour = mesh%get_ncolours()" in code
    assert "      cmap => mesh%get_colour_map()\n" in code

    if dist_mem:
        lookup = "get_last_halo_cell_per_colour(colour,1)"
    else:
        lookup = "get_last_edge_cell_per_colour(colour)"

    output = (
        "      !\n"
        "      DO colour=1,ncolour\n"
        "        !$omp parallel default(shared), private(cell)\n"
        "        !$omp do schedule(static)\n"
        "        DO cell=1,mesh%{0}\n"
        "          !\n"
        "          CALL ru_code(nlayers, a_proxy%data, b_proxy%data, "
        "istp, rdt, d_proxy%data, e_proxy(1)%data, e_proxy(2)%data, "
        "e_proxy(3)%data, ndf_w2, undf_w2, map_w2(:,cmap(colour, "
        "cell)), basis_w2_qr, diff_basis_w2_qr, ndf_w3, undf_w3, "
        "map_w3(:,cmap(colour, cell)), basis_w3_qr, ndf_w0, undf_w0, "
        "map_w0(:,cmap(colour, cell)), basis_w0_qr, diff_basis_w0_qr, "
        "np_xy_qr, np_z_qr, weights_xy_qr, weights_z_qr)\n"
        "        END DO \n"
        "        !$omp end do\n"
        "        !$omp do schedule(static)\n"
        "        DO cell=1,mesh%{0}\n"
        "          !\n"
        "          CALL ru_code(nlayers, f_proxy%data, b_proxy%data, "
        "istp, rdt, d_proxy%data, e_proxy(1)%data, e_proxy(2)%data, "
        "e_proxy(3)%data, ndf_w2, undf_w2, map_w2(:,cmap(colour, "
        "cell)), basis_w2_qr, diff_basis_w2_qr, ndf_w3, undf_w3, "
        "map_w3(:,cmap(colour, cell)), basis_w3_qr, ndf_w0, undf_w0, "
        "map_w0(:,cmap(colour, cell)), basis_w0_qr, diff_basis_w0_qr, "
        "np_xy_qr, np_z_qr, weights_xy_qr, weights_z_qr)\n"
        "        END DO \n"
        "        !$omp end do\n"
        "        !$omp end parallel\n"
        "      END DO \n".format(lookup))

    assert output in code

    if dist_mem:
        set_dirty_str = (
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL a_proxy%set_dirty()\n"
            "      CALL f_proxy%set_dirty()\n")
        assert set_dirty_str in code
        assert code.count("set_dirty()") == 2

    if TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag
        # to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_loop_fuse_cma():
    ''' Test that we can loop fuse two loops when one contains a
    call to a CMA-related kernel. '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "20.6_multi_invoke_with_cma.f90"),
                    api=TEST_API)
    ftrans = DynamoLoopFuseTrans()
    mtrans = MoveTrans()
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0')
        schedule = invoke.schedule
        if dist_mem:
            # move halo exchanges before the first loop
            schedule, _ = mtrans.apply(schedule.children[2],
                                       schedule.children[1])
            schedule, _ = mtrans.apply(schedule.children[3],
                                       schedule.children[2])
            schedule, _ = mtrans.apply(schedule.children[4],
                                       schedule.children[3])
            index = 4
        else:
            index = 0

        # Fuse the loops
        schedule, _ = ftrans.apply(schedule.children[index],
                                   schedule.children[index+1],
                                   same_space=True)
        code = str(psy.gen)
        print(code)
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


def test_omp_par_and_halo_exchange_error():
    '''Tests that we raise an error if we try to apply an omp parallel
    transformation to a list containing halo_exchange calls. If this is
    allowed then it is likely that we will get incorrect results, or that
    the code will fail. Fixing this problem is the subject of ticket #526. '''
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
    schedule, _ = otrans.apply(schedule.children[4])

    # Enclose the invoke code within a single region
    with pytest.raises(TransformationError) as excinfo:
        schedule, _ = rtrans.apply(schedule.children)
    assert "A halo exchange within a parallel region is not supported" \
        in str(excinfo.value)


def test_module_inline(monkeypatch, annexed):
    '''Tests that correct results are obtained when a kernel is inlined
    into the psy-layer in the dynamo0.3 API. More in-depth tests can
    be found in the gocean1p0_transformations.py file. We also test
    when annexed is False and True as it affects how many halo
    exchanges are generated.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4.6_multikernel_invokes.f90"),
                    api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.get('invoke_0')
        schedule = invoke.schedule
        if dist_mem:
            if annexed:
                index = 6
            else:
                index = 8
        else:
            index = 1
        kern_call = schedule.children[index].children[0]
        inline_trans = KernelModuleInlineTrans()
        schedule, _ = inline_trans.apply(kern_call)
        gen = str(psy.gen)
        # check that the subroutine has been inlined
        assert 'SUBROUTINE ru_code(' in gen
        # check that the associated psy "use" does not exist
        assert 'USE ru_kernel_mod, only : ru_code' not in gen


def test_builtin_single_OpenMP_pdo(monkeypatch, annexed):
    '''Test that we generate correct code if an OpenMP parallel do is
    applied to a single builtin. Also test with and without annexed
    dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    for dist_mem in [False, True]:
        _, info = parse(os.path.join(BASE_PATH,
                                     "15.7.2_setval_X_builtin.f90"),
                        api=TEST_API, distributed_memory=dist_mem)
        psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        otrans = DynamoOMPParallelLoopTrans()
        # Apply OpenMP parallelisation to the loop
        schedule, _ = otrans.apply(schedule.children[0])
        invoke.schedule = schedule
        result = str(psy.gen)
        print(result)
        if dist_mem:  # annexed can be True or False
            code = (
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static)\n"
                "      DO df=1,f2_proxy%vspace%get_last_dof_annexed()\n"
                "        f2_proxy%data(df) = f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f2_proxy%set_dirty()")
            if not annexed:
                code = code.replace("dof_annexed", "dof_owned")
            assert code in result
        else:  # not distmem. annexed can be True or False
            assert (
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static)\n"
                "      DO df=1,undf_any_space_1_f2\n"
                "        f2_proxy%data(df) = f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do") in result


def test_builtin_multiple_OpenMP_pdo(monkeypatch, annexed):
    '''Test that we generate correct code if OpenMP parallel do's are
    applied to multiple builtins. Also test with and without annexed
    dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    for dist_mem in [False, True]:
        _, info = parse(os.path.join(BASE_PATH,
                                     "15.14.2_multiple_set_kernels.f90"),
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
        print(result)
        if dist_mem:  # annexed can be True or False
            code = (
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_annexed()\n"
                "        f1_proxy%data(df) = fred\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static)\n"
                "      DO df=1,f2_proxy%vspace%get_last_dof_annexed()\n"
                "        f2_proxy%data(df) = 3.0\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f2_proxy%set_dirty()\n"
                "      !\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static)\n"
                "      DO df=1,f3_proxy%vspace%get_last_dof_annexed()\n"
                "        f3_proxy%data(df) = ginger\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f3_proxy%set_dirty()")
            if not annexed:
                code = code.replace("dof_annexed", "dof_owned")
            assert code in result
        else:  # not distmem. annexed can be True or False
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


def test_builtin_loop_fuse_pdo(monkeypatch, annexed):
    '''Test that we generate correct code if an OpenMP parallel do is
    applied to multiple loop fused builtins. We have to assert that it
    is safe to loop fuse. Also test with and without annexed
    dofs being computed as this affects the generated code. '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    for dist_mem in [False, True]:
        _, info = parse(os.path.join(BASE_PATH,
                                     "15.14.2_multiple_set_kernels.f90"),
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
        print(result)
        if dist_mem:  # annexed can be True or False
            code = (
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_annexed()\n"
                "        f1_proxy%data(df) = fred\n"
                "        f2_proxy%data(df) = 3.0\n"
                "        f3_proxy%data(df) = ginger\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      CALL f2_proxy%set_dirty()\n"
                "      CALL f3_proxy%set_dirty()")
            if not annexed:
                code = code.replace("dof_annexed", "dof_owned")
            assert code in result
        else:  # distmem is False. annexed can be True or False
            assert (
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static)\n"
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = fred\n"
                "        f2_proxy%data(df) = 3.0\n"
                "        f3_proxy%data(df) = ginger\n"
                "      END DO \n"
                "      !$omp end parallel do") in result


def test_builtin_single_OpenMP_do(monkeypatch, annexed):
    '''Test that we generate correct code if an OpenMP do (with an outer
    OpenMP parallel) is applied to a single builtin. Also test with
    and without annexed dofs being computed as this affects the
    generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    for dist_mem in [False, True]:
        _, info = parse(os.path.join(BASE_PATH,
                                     "15.7.2_setval_X_builtin.f90"),
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
        print(result)
        if dist_mem:  # annexed can be True or False
            code = (
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,f2_proxy%vspace%get_last_dof_annexed()\n"
                "        f2_proxy%data(df) = f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      !$omp master\n"
                "      CALL f2_proxy%set_dirty()\n"
                "      !$omp end master\n"
                "      !\n"
                "      !$omp end parallel")
            if not annexed:
                code = code.replace("dof_annexed", "dof_owned")
            assert code in result
        else:  # distmem is False. annexed can be True or False
            assert (
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,undf_any_space_1_f2\n"
                "        f2_proxy%data(df) = f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp end parallel\n") in result


def test_builtin_multiple_OpenMP_do(monkeypatch, annexed):
    '''Test that we generate correct code if OpenMP do's are applied to
    multiple builtins. Also test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    for dist_mem in [False, True]:
        _, info = parse(os.path.join(BASE_PATH,
                                     "15.14.2_multiple_set_kernels.f90"),
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
        print(result)
        if dist_mem:  # annexed can be True or False
            code = (
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_annexed()\n"
                "        f1_proxy%data(df) = fred\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      !$omp master\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !$omp end master\n"
                "      !\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,f2_proxy%vspace%get_last_dof_annexed()\n"
                "        f2_proxy%data(df) = 3.0\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      !$omp master\n"
                "      CALL f2_proxy%set_dirty()\n"
                "      !$omp end master\n"
                "      !\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,f3_proxy%vspace%get_last_dof_annexed()\n"
                "        f3_proxy%data(df) = ginger\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      !$omp master\n"
                "      CALL f3_proxy%set_dirty()\n"
                "      !$omp end master\n"
                "      !\n"
                "      !$omp end parallel")
            if not annexed:
                code = code.replace("dof_annexed", "dof_owned")
            assert code in result
        else:  # distmem is False. annexed can be True or False
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


def test_builtin_loop_fuse_do(monkeypatch, annexed):
    '''Test that we generate correct code if an OpenMP do is applied to
    multiple loop fused builtins. We need to assert it is safe to
    perform loop fusion. Also test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    for dist_mem in [False, True]:
        _, info = parse(os.path.join(BASE_PATH,
                                     "15.14.2_multiple_set_kernels.f90"),
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
        print(result)
        if dist_mem:  # annexed can be True or False
            code = (
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_annexed()\n"
                "        f1_proxy%data(df) = fred\n"
                "        f2_proxy%data(df) = 3.0\n"
                "        f3_proxy%data(df) = ginger\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      !$omp master\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      CALL f2_proxy%set_dirty()\n"
                "      CALL f3_proxy%set_dirty()\n"
                "      !$omp end master\n"
                "      !\n"
                "      !$omp end parallel")
            if not annexed:
                code = code.replace("dof_annexed", "dof_owned")
            assert code in result
        else:  # distmem is False. annexed can be True or False
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
    real scalar summed in a builtin. We use inner product in this case. '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.9.1_X_innerproduct_Y_builtin.f90"),
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
        print(code)
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
    scalar summed in a builtin. We use inner product in this case. '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.9.1_X_innerproduct_Y_builtin.f90"),
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
        print(code)
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
    real scalar summed in a builtin. We use inner product in this case. '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.15.1_two_same_builtin_reductions.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        otrans = DynamoOMPParallelLoopTrans()
        # Apply OpenMP parallelisation to the loop
        for child in schedule.children:
            if isinstance(child, psyGen.Loop):
                schedule, _ = otrans.apply(child)
        invoke.schedule = schedule
        code = str(psy.gen)
        print(code)
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


def test_reduction_after_normal_real_do(monkeypatch, annexed):
    '''test that we produce correct code when we have a reduction after a
    "normal" builtin and we use OpenMP DO loops for parallelisation
    with a single parallel region over all calls. Also test with and
    without annexed dofs being computed as this affects the generated
    code.

    '''
    file_name = "15.17.2_one_standard_builtin_one_reduction.f90"
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
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
        for child in schedule.children:
            if isinstance(child, psyGen.Loop):
                schedule, _ = otrans.apply(child, reprod=False)
        # Apply an OpenMP Parallel for all loops
        schedule, _ = rtrans.apply(schedule.children[0:2])
        invoke.schedule = schedule
        result = str(psy.gen)
        print(result)
        if distmem:  # annexed can be True or False
            expected_output = (
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_annexed()\n"
                "        f1_proxy%data(df) = bvalue*f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
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
            if not annexed:
                expected_output = expected_output.replace("dof_annexed",
                                                          "dof_owned")
        else:  # not distmem. annexed can be True or False
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


def test_reprod_red_after_normal_real_do(monkeypatch, annexed):
    '''test that we produce correct code when we have a reproducible
    reduction after a "normal" builtin and we use OpenMP DO loops for
    parallelisation with a single parallel region over all calls. Also
    test with and without annexed dofs being computed as this affects
    the generated code.

    '''
    file_name = "15.17.2_one_standard_builtin_one_reduction.f90"
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
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
        for child in schedule.children:
            if isinstance(child, psyGen.Loop):
                schedule, _ = otrans.apply(child, reprod=True)
        # Apply an OpenMP Parallel for all loops
        schedule, _ = rtrans.apply(schedule.children[0:2])
        invoke.schedule = schedule
        result = str(psy.gen)
        print(result)
        if distmem:  # annexed can be True or False
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
                "      DO df=1,f1_proxy%vspace%get_last_dof_annexed()\n"
                "        f1_proxy%data(df) = bvalue*f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
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
            if not annexed:
                expected_output = expected_output.replace("dof_annexed",
                                                          "dof_owned")
        else:  # not distmem. annexed can be True or False
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
    single parallel region over all calls. '''
    file_name = "15.16.1_two_different_builtin_reductions.f90"
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
        for child in schedule.children:
            if isinstance(child, psyGen.Loop):
                schedule, _ = otrans.apply(child, reprod=False)
        # Apply an OpenMP Parallel for all loops
        schedule, _ = rtrans.apply(schedule.children[0:2])
        invoke.schedule = schedule
        result = str(psy.gen)
        print(result)
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
    with a single parallel region over all calls. '''
    file_name = "15.16.1_two_different_builtin_reductions.f90"
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
        for child in schedule.children:
            if isinstance(child, psyGen.Loop):
                schedule, _ = otrans.apply(child, reprod=True)
        # Apply an OpenMP Parallel for all loops
        schedule, _ = rtrans.apply(schedule.children[0:2])
        invoke.schedule = schedule
        result = str(psy.gen)
        print(result)
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
    file_name = "15.15.1_two_same_builtin_reductions.f90"
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
            for child in schedule.children:
                if isinstance(child, psyGen.Loop):
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
    for file_name in ["15.15.1_two_same_builtin_reductions.f90",
                      "15.16.1_two_different_builtin_reductions.f90"]:

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
    two different builtins. We use inner product and sum_X. '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.16.1_two_different_builtin_reductions.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        otrans = DynamoOMPParallelLoopTrans()
        # Apply OpenMP parallelisation to the loop
        for child in schedule.children:
            if isinstance(child, psyGen.Loop):
                schedule, _ = otrans.apply(child)
        invoke.schedule = schedule
        code = str(psy.gen)
        print(code)
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


def test_multi_builtins_red_then_pdo(monkeypatch, annexed):
    '''test that we generate a correct OpenMP parallel do reduction for
    two different builtins, first a reduction then not. Also test with
    and without annexed dofs being computed as this affects the
    generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.17.1_one_reduction_one_standard_builtin.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        otrans = DynamoOMPParallelLoopTrans()
        # Apply OpenMP parallelisation to the loop
        for child in schedule.children:
            if isinstance(child, psyGen.Loop):
                schedule, _ = otrans.apply(child)
        invoke.schedule = schedule
        result = str(psy.gen)
        print(result)
        if distmem:  # annexed can be True or False
            code = (
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
                "      DO df=1,f1_proxy%vspace%get_last_dof_annexed()\n"
                "        f1_proxy%data(df) = bsum*f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n")
            if not annexed:
                code = code.replace("dof_annexed", "dof_owned")
            assert code in result
        else:  # not distmem. annexed can be True or False
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
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = bsum*f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n") in result


def test_multi_builtins_red_then_do(monkeypatch, annexed):
    '''test that we generate a correct OpenMP do reduction for two
    different builtins, first a reduction then not. Also test with and
    without annexed dofs being computed as this affects the generated
    code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.17.1_one_reduction_one_standard_builtin.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        otrans = Dynamo0p3OMPLoopTrans()
        rtrans = OMPParallelTrans()
        # Apply an OpenMP do to the loop
        for child in schedule.children:
            if isinstance(child, psyGen.Loop):
                schedule, _ = otrans.apply(child, reprod=False)
        if distmem:  # annexed can be True or False
            mtrans = MoveTrans()
            schedule, _ = mtrans.apply(schedule.children[1],
                                       schedule.children[2],
                                       position="after")
        schedule, _ = rtrans.apply(schedule.children[0:2])
        invoke.schedule = schedule
        result = str(psy.gen)
        print(result)
        if distmem:  # annexed can be True or False
            code = (
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
                "      DO df=1,f1_proxy%vspace%get_last_dof_annexed()\n"
                "        f1_proxy%data(df) = bsum*f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
                "      !\n"
                "      !$omp master\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !$omp end master\n"
                "      !\n"
                "      !$omp end parallel\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n")
            if not annexed:
                code = code.replace("dof_annexed", "dof_owned")
            assert code in result
        else:  # not distmem. annexed can be True or False
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
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = bsum*f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp end parallel\n") in result


def test_multi_builtins_red_then_fuse_pdo(monkeypatch, annexed):
    '''test that we generate a correct OpenMP parallel do reduction for
    two different loop-fused builtins, first a reduction then not. We
    need to specify that the fused loops are on the same iteration
    space. Also test with and without annexed dofs being computed as
    this affects the validity of the transform.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.17.1_one_reduction_one_standard_builtin.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        ftrans = DynamoLoopFuseTrans()
        if distmem and annexed:
            mtrans = MoveTrans()
            schedule, _ = mtrans.apply(schedule.children[1],
                                       schedule.children[2],
                                       position="after")
            with pytest.raises(TransformationError) as excinfo:
                schedule, _ = ftrans.apply(schedule.children[0],
                                           schedule.children[1],
                                           same_space=True)
            assert ("The upper bound names are not the same"
                    in str(excinfo.value))
        else:  # not (distmem and annexed)
            if distmem:  # annexed must be False here
                # first move the loop as the global sum is in the way
                mtrans = MoveTrans()
                schedule, _ = mtrans.apply(schedule.children[1],
                                           schedule.children[2],
                                           position="after")
            rtrans = DynamoOMPParallelLoopTrans()
            schedule, _ = ftrans.apply(schedule.children[0],
                                       schedule.children[1],
                                       same_space=True)
            schedule, _ = rtrans.apply(schedule.children[0])
            invoke.schedule = schedule
            result = str(psy.gen)
            print(result)
            if distmem:  # annexed must be False here
                code = (
                    "      ! Zero summation variables\n"
                    "      !\n"
                    "      asum = 0.0_r_def\n"
                    "      !\n"
                    "      !$omp parallel do default(shared), private(df), "
                    "schedule(static), reduction(+:asum)\n"
                    "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                    "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                    "        f1_proxy%data(df) = bsum*f1_proxy%data(df)\n"
                    "      END DO \n"
                    "      !$omp end parallel do\n"
                    "      !\n"
                    "      ! Set halos dirty/clean for fields modified in the "
                    "above loop\n"
                    "      !\n"
                    "      CALL f1_proxy%set_dirty()\n"
                    "      !\n"
                    "      global_sum%value = asum\n"
                    "      asum = global_sum%get_sum()\n")
            else:  # not distmem. annexed can be True or False
                code = (
                    "      ! Zero summation variables\n"
                    "      !\n"
                    "      asum = 0.0_r_def\n"
                    "      !\n"
                    "      !$omp parallel do default(shared), private(df), "
                    "schedule(static), reduction(+:asum)\n"
                    "      DO df=1,undf_any_space_1_f1\n"
                    "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                    "        f1_proxy%data(df) = bsum*f1_proxy%data(df)\n"
                    "      END DO \n"
                    "      !$omp end parallel do\n")
            assert code in result


def test_multi_builtins_red_then_fuse_do(monkeypatch, annexed):
    '''test that we generate a correct OpenMP do reduction for two
    different loop-fused builtins, first a reduction then not. We need
    to specify that the fused loops are on the same iteration
    space. Also test with and without annexed dofs being computed as
    this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.17.1_one_reduction_one_standard_builtin.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        ftrans = DynamoLoopFuseTrans()
        if distmem and annexed:
            mtrans = MoveTrans()
            schedule, _ = mtrans.apply(schedule.children[1],
                                       schedule.children[2],
                                       position="after")
            with pytest.raises(TransformationError) as excinfo:
                schedule, _ = ftrans.apply(schedule.children[0],
                                           schedule.children[1],
                                           same_space=True)
            assert ("The upper bound names are not the same"
                    in str(excinfo.value))
        else:  # not (distmem and annexed)
            if distmem:  # annexed must be False here
                mtrans = MoveTrans()
                schedule, _ = mtrans.apply(schedule.children[1],
                                           schedule.children[2],
                                           position="after")
            rtrans = OMPParallelTrans()
            otrans = Dynamo0p3OMPLoopTrans()
            schedule, _ = ftrans.apply(schedule.children[0],
                                       schedule.children[1],
                                       same_space=True)
            schedule, _ = otrans.apply(schedule.children[0], reprod=False)
            schedule, _ = rtrans.apply(schedule.children[0])
            invoke.schedule = schedule
            result = str(psy.gen)
            print(result)
            if distmem:  # annexed must be False here
                code = (
                    "      asum = 0.0_r_def\n"
                    "      !\n"
                    "      !$omp parallel default(shared), private(df)\n"
                    "      !$omp do schedule(static), reduction(+:asum)\n"
                    "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                    "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                    "        f1_proxy%data(df) = bsum*f1_proxy%data(df)\n"
                    "      END DO \n"
                    "      !$omp end do\n"
                    "      !\n"
                    "      ! Set halos dirty/clean for fields modified in the "
                    "above loop\n"
                    "      !\n"
                    "      !$omp master\n"
                    "      CALL f1_proxy%set_dirty()\n"
                    "      !$omp end master\n"
                    "      !\n"
                    "      !$omp end parallel\n"
                    "      global_sum%value = asum\n"
                    "      asum = global_sum%get_sum()\n")
            else:  # not distmem, annexed is True or False
                code = (
                    "      asum = 0.0_r_def\n"
                    "      !\n"
                    "      !$omp parallel default(shared), private(df)\n"
                    "      !$omp do schedule(static), reduction(+:asum)\n"
                    "      DO df=1,undf_any_space_1_f1\n"
                    "        asum = asum+f1_proxy%data(df)*f2_proxy%data(df)\n"
                    "        f1_proxy%data(df) = bsum*f1_proxy%data(df)\n"
                    "      END DO \n"
                    "      !$omp end do\n"
                    "      !$omp end parallel\n")
            assert code in result


def test_multi_builtins_usual_then_red_pdo(monkeypatch, annexed):
    '''test that we generate a correct OpenMP parallel do reduction for
    two different builtins, first a standard builtin then a
    reduction. Also test with and without annexed dofs being computed
    as this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.17.2_one_standard_builtin_one_reduction.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        otrans = DynamoOMPParallelLoopTrans()
        # Apply OpenMP parallelisation to the loop
        for child in schedule.children:
            if isinstance(child, psyGen.Loop):
                schedule, _ = otrans.apply(child)
        invoke.schedule = schedule
        result = str(psy.gen)
        print(result)
        if distmem:  # annexed can be True or False
            code = (
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static)\n"
                "      DO df=1,f1_proxy%vspace%get_last_dof_annexed()\n"
                "        f1_proxy%data(df) = bvalue*f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end parallel do\n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop\n"
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
                "      asum = global_sum%get_sum()\n")
            if not annexed:
                code = code.replace("dof_annexed", "dof_owned", 1)
            assert code in result
        else:  # not distmem. annexed can be True or False
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
                "      !$omp end parallel do\n") in result


def test_builtins_usual_then_red_fuse_pdo(monkeypatch, annexed):
    '''test that we generate a correct OpenMP parallel do reduction for
    two different loop-fused builtins, first a normal builtin then a
    reduction. We need to specify that the fused loops iterate over
    the same space. Also test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.17.2_one_standard_builtin_one_reduction.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        ftrans = DynamoLoopFuseTrans()
        if distmem and annexed:
            with pytest.raises(TransformationError) as excinfo:
                schedule, _ = ftrans.apply(schedule.children[0],
                                           schedule.children[1],
                                           same_space=True)
            assert ("The upper bound names are not the same"
                    in str(excinfo.value))
        else:  # not (distmem and annexed)
            otrans = DynamoOMPParallelLoopTrans()
            schedule, _ = ftrans.apply(schedule.children[0],
                                       schedule.children[1],
                                       same_space=True)
            schedule, _ = otrans.apply(schedule.children[0])
            invoke.schedule = schedule
            result = str(psy.gen)
            print(result)
            if distmem:  # annexed is False here
                code = (
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
                    "      ! Set halos dirty/clean for fields modified in the "
                    "above loop\n"
                    "      !\n"
                    "      CALL f1_proxy%set_dirty()\n"
                    "      !\n"
                    "      global_sum%value = asum\n"
                    "      asum = global_sum%get_sum()\n")
            else:  # not distmem. annexed can be True or False
                code = (
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
                    "      !$omp end parallel do\n")
            assert code in result


def test_builtins_usual_then_red_fuse_do(monkeypatch, annexed):
    '''test that we generate a correct OpenMP parallel do reduction for
    two different loop-fused builtins, first a normal builtin then a
    reduction. We need to specify that the fused loops iterate over
    the same space. Also test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.17.2_one_standard_builtin_one_reduction.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        ftrans = DynamoLoopFuseTrans()
        if distmem and annexed:
            with pytest.raises(TransformationError) as excinfo:
                schedule, _ = ftrans.apply(schedule.children[0],
                                           schedule.children[1],
                                           same_space=True)
            assert ("The upper bound names are not the same"
                    in str(excinfo.value))
        else:  # not (distmem and annexed)
            rtrans = OMPParallelTrans()
            otrans = Dynamo0p3OMPLoopTrans()
            schedule, _ = ftrans.apply(schedule.children[0],
                                       schedule.children[1],
                                       same_space=True)
            schedule, _ = otrans.apply(schedule.children[0], reprod=False)
            schedule, _ = rtrans.apply(schedule.children[0])
            invoke.schedule = schedule
            result = str(psy.gen)
            print(result)
            if distmem:  # annexed is False here
                code = (
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
                    "      ! Set halos dirty/clean for fields modified in the "
                    "above loop\n"
                    "      !\n"
                    "      !$omp master\n"
                    "      CALL f1_proxy%set_dirty()\n"
                    "      !$omp end master\n"
                    "      !\n"
                    "      !$omp end parallel\n"
                    "      global_sum%value = asum\n"
                    "      asum = global_sum%get_sum()\n")
            else:  # not distmem. annexed can be True or False
                code = (
                    "      asum = 0.0_r_def\n"
                    "      !\n"
                    "      !$omp parallel default(shared), private(df)\n"
                    "      !$omp do schedule(static), reduction(+:asum)\n"
                    "      DO df=1,undf_any_space_1_f1\n"
                    "        f1_proxy%data(df) = bvalue*f1_proxy%data(df)\n"
                    "        asum = asum+f1_proxy%data(df)\n"
                    "      END DO \n"
                    "      !$omp end do\n"
                    "      !$omp end parallel\n")
            assert code in result

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
                     "15.18.1_builtins_reduction_fuse_error.f90"),
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
    the loops has an any_space iteration space.'''
    for dist_mem in [False, True]:
        _, info = parse(os.path.join(BASE_PATH,
                                     "15.14.2_multiple_set_kernels.f90"),
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
    for an OpenMP Parallel Do. '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.9.1_X_innerproduct_Y_builtin.f90"),
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
    this case. '''
    for distmem in [True, False]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.9.1_X_innerproduct_Y_builtin.f90"),
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
        print(code)
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
    around loops with a global sum. '''
    for distmem in [True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.17.1_one_reduction_one_standard_builtin.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        otrans = Dynamo0p3OMPLoopTrans()
        rtrans = OMPParallelTrans()
        # Apply an OpenMP do to the loop
        for child in schedule.children:
            if isinstance(child, psyGen.Loop):
                schedule, _ = otrans.apply(child, reprod=True)
        schedule, _ = rtrans.apply(schedule.children)
        invoke.schedule = schedule
        with pytest.raises(NotImplementedError) as excinfo:
            _ = str(psy.gen)
        assert(
            "Cannot correctly generate code for an OpenMP parallel region "
            "containing children of different types") in str(excinfo.value)


def test_reprod_builtins_red_then_usual_do(monkeypatch, annexed):
    '''test that we generate a correct reproducible OpenMP do reduction
    for two different builtins, first a reduction then not when we
    have reprod set to True. Also test with and without annexed dofs
    being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.17.1_one_reduction_one_standard_builtin.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        otrans = Dynamo0p3OMPLoopTrans()
        rtrans = OMPParallelTrans()
        # Apply an OpenMP do to the loop
        for child in schedule.children:
            if isinstance(child, psyGen.Loop):
                schedule, _ = otrans.apply(child, reprod=True)
        if distmem:  # annexed can be True or False
            mtrans = MoveTrans()
            schedule, _ = mtrans.apply(schedule.children[1],
                                       schedule.children[2],
                                       position="after")
        schedule, _ = rtrans.apply(schedule.children[0:2])
        invoke.schedule = schedule
        result = str(psy.gen)
        print(result)
        assert (
            "      USE omp_lib, ONLY: omp_get_thread_num\n"
            "      USE omp_lib, ONLY: omp_get_max_threads\n") in result
        assert (
            "      REAL(KIND=r_def), allocatable, dimension(:,:) "
            ":: l_asum\n") in result
        assert "      INTEGER th_idx\n" in result
        assert "      INTEGER nthreads\n" in result
        assert (
            "      !\n"
            "      ! Determine the number of OpenMP threads\n"
            "      !\n"
            "      nthreads = omp_get_max_threads()\n"
            "      !\n") in result
        if distmem:  # annexed can be True or False
            code = (
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
                "      DO df=1,f1_proxy%vspace%get_last_dof_annexed()\n"
                "        f1_proxy%data(df) = bsum*f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
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
                "      asum = global_sum%get_sum()\n")
            if not annexed:
                code = code.replace("dof_annexed", "dof_owned")
            assert code in result
        else:  # not distmem. annexed can be True or False
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
                "      DO df=1,undf_any_space_1_f1\n"
                "        f1_proxy%data(df) = bsum*f1_proxy%data(df)\n"
                "      END DO \n"
                "      !$omp end do\n"
                "      !$omp end parallel\n"
                "      !\n"
                "      ! sum the partial results sequentially\n"
                "      !\n"
                "      DO th_idx=1,nthreads\n"
                "        asum = asum+l_asum(1,th_idx)\n"
                "      END DO \n"
                "      DEALLOCATE (l_asum)\n") in result


def test_repr_bltins_red_then_usual_fuse_do(monkeypatch, annexed):
    '''test that we generate a correct reproducible OpenMP do reduction
    for two different loop-fused builtins, first a reduction then
    not. We need to specify that the fused loops are on the same
    iteration space. Also test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.17.1_one_reduction_one_standard_builtin.f90"),
            distributed_memory=distmem,
            api=TEST_API)
        psy = PSyFactory(TEST_API,
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        ftrans = DynamoLoopFuseTrans()
        if distmem:  # annexed can be True or False
            mtrans = MoveTrans()
            schedule, _ = mtrans.apply(schedule.children[1],
                                       schedule.children[2],
                                       position="after")
        if distmem and annexed:
            # we can't loop fuse as the loop bounds differ
            with pytest.raises(TransformationError) as excinfo:
                schedule, _ = ftrans.apply(schedule.children[0],
                                           schedule.children[1],
                                           same_space=True)
            assert ("The upper bound names are not the same"
                    in str(excinfo.value))
        else:  # not (distmem and annexed)
            # we can loop fuse as the loop bounds are the same
            schedule, _ = ftrans.apply(schedule.children[0],
                                       schedule.children[1],
                                       same_space=True)
            rtrans = OMPParallelTrans()
            otrans = Dynamo0p3OMPLoopTrans()
            schedule, _ = otrans.apply(schedule.children[0], reprod=True)
            schedule, _ = rtrans.apply(schedule.children[0])
            invoke.schedule = schedule
            result = str(psy.gen)
            print(result)
            assert (
                "      USE omp_lib, ONLY: omp_get_thread_num\n"
                "      USE omp_lib, ONLY: omp_get_max_threads\n") in result
            assert (
                "      REAL(KIND=r_def), allocatable, dimension(:,:) "
                ":: l_asum\n") in result
            assert "      INTEGER th_idx\n" in result
            assert "      INTEGER nthreads\n" in result
            assert (
                "      !\n"
                "      ! Determine the number of OpenMP threads\n"
                "      !\n"
                "      nthreads = omp_get_max_threads()\n"
                "      !\n") in result
            if distmem:  # annexed is False here
                assert (
                    "      asum = 0.0_r_def\n"
                    "      ALLOCATE (l_asum(8,nthreads))\n"
                    "      l_asum = 0.0_r_def\n"
                    "      !\n"
                    "      !$omp parallel default(shared), "
                    "private(df,th_idx)\n"
                    "      th_idx = omp_get_thread_num()+1\n"
                    "      !$omp do schedule(static)\n"
                    "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                    "        l_asum(1,th_idx) = l_asum(1,th_idx)+"
                    "f1_proxy%data(df)*f2_proxy%data(df)\n"
                    "        f1_proxy%data(df) = bsum*f1_proxy%data(df)\n"
                    "      END DO \n"
                    "      !$omp end do\n"
                    "      !\n"
                    "      ! Set halos dirty/clean for fields modified in the "
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
                    "      asum = global_sum%get_sum()\n") in result
            else:  # not distmem. annexed can be True or False
                assert (
                    "      asum = 0.0_r_def\n"
                    "      ALLOCATE (l_asum(8,nthreads))\n"
                    "      l_asum = 0.0_r_def\n"
                    "      !\n"
                    "      !$omp parallel default(shared), "
                    "private(df,th_idx)\n"
                    "      th_idx = omp_get_thread_num()+1\n"
                    "      !$omp do schedule(static)\n"
                    "      DO df=1,undf_any_space_1_f1\n"
                    "        l_asum(1,th_idx) = l_asum(1,th_idx)+"
                    "f1_proxy%data(df)"
                    "*f2_proxy%data(df)\n"
                    "        f1_proxy%data(df) = bsum*f1_proxy%data(df)\n"
                    "      END DO \n"
                    "      !$omp end do\n"
                    "      !$omp end parallel\n"
                    "      !\n"
                    "      ! sum the partial results sequentially\n"
                    "      !\n"
                    "      DO th_idx=1,nthreads\n"
                    "        asum = asum+l_asum(1,th_idx)\n"
                    "      END DO \n"
                    "      DEALLOCATE (l_asum)\n") in result


def test_repr_bltins_usual_then_red_fuse_do(monkeypatch, annexed):
    '''test that we generate a correct OpenMP do reduction for two
    different loop-fused builtins, first a normal builtin then a
    reduction. We need to specify that the fused loops iterate over
    the same space. Also test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.17.2_one_standard_builtin_one_reduction.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        ftrans = DynamoLoopFuseTrans()
        if distmem and annexed:
            with pytest.raises(TransformationError) as excinfo:
                schedule, _ = ftrans.apply(schedule.children[0],
                                           schedule.children[1],
                                           same_space=True)
            assert ("The upper bound names are not the same"
                    in str(excinfo.value))
        else:  # not distmem and annexed
            rtrans = OMPParallelTrans()
            otrans = Dynamo0p3OMPLoopTrans()
            schedule, _ = ftrans.apply(schedule.children[0],
                                       schedule.children[1],
                                       same_space=True)
            schedule, _ = otrans.apply(schedule.children[0], reprod=True)
            schedule, _ = rtrans.apply(schedule.children[0])
            invoke.schedule = schedule
            result = str(psy.gen)
            print(result)
            assert "      INTEGER th_idx\n" in result
            if distmem:  # annexed is False here
                assert (
                    "      asum = 0.0_r_def\n"
                    "      ALLOCATE (l_asum(8,nthreads))\n"
                    "      l_asum = 0.0_r_def\n"
                    "      !\n"
                    "      !$omp parallel default(shared), "
                    "private(df,th_idx)\n"
                    "      th_idx = omp_get_thread_num()+1\n"
                    "      !$omp do schedule(static)\n"
                    "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
                    "        f1_proxy%data(df) = bvalue*f1_proxy%data(df)\n"
                    "        l_asum(1,th_idx) = l_asum(1,th_idx)+"
                    "f1_proxy%data(df)\n"
                    "      END DO \n"
                    "      !$omp end do\n"
                    "      !\n"
                    "      ! Set halos dirty/clean for fields modified in the "
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
                    "      asum = global_sum%get_sum()\n") in result
            else:  # distmem is False. annexed can be True or False
                assert (
                    "      asum = 0.0_r_def\n"
                    "      ALLOCATE (l_asum(8,nthreads))\n"
                    "      l_asum = 0.0_r_def\n"
                    "      !\n"
                    "      !$omp parallel default(shared), "
                    "private(df,th_idx)\n"
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
                    "      DEALLOCATE (l_asum)\n") in result


def test_repr_3_builtins_2_reductions_do():
    '''test that we generate correct reproducible OpenMP do reductions
    when we have three different builtins, first a reduction, then a
    normal builtin then a reduction. '''
    from psyclone.dynamo0p3 import DynLoop
    from psyclone.psyGen import OMPDoDirective
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.19.1_three_builtins_two_reductions.f90"),
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
        print(code)
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


def test_reprod_view(capsys, monkeypatch, annexed):
    '''test that we generate a correct view() for OpenMP do
    reductions. Also test with and without annexed dofs being computed
    as this affects the output.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    from psyclone.dynamo0p3 import DynLoop
    from psyclone.psyGen import OMPDoDirective, colored, SCHEDULE_COLOUR_MAP

    # Ensure we check to text containing the correct (colour) control codes
    sched = colored("Schedule", SCHEDULE_COLOUR_MAP["Schedule"])
    directive = colored("Directive", SCHEDULE_COLOUR_MAP["Directive"])
    gsum = colored("GlobalSum", SCHEDULE_COLOUR_MAP["GlobalSum"])
    loop = colored("Loop", SCHEDULE_COLOUR_MAP["Loop"])
    call = colored("Call", SCHEDULE_COLOUR_MAP["Call"])

    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.19.1_three_builtins_two_reductions.f90"),
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
        if distmem:  # annexed can be True or False
            expected = (
                sched + "[invoke='invoke_0' dm=True]\n"
                "    " + directive+"[OMP parallel]\n"
                "        " + directive + "[OMP do][reprod=True]\n"
                "            " + loop + "[type='dofs',"
                "field_space='any_space_1',it_space='dofs', "
                "upper_bound='ndofs']\n"
                "                " + call + " x_innerproduct_y(asum,f1,f2)\n"
                "    " + gsum + "[scalar='asum']\n"
                "    " + directive + "[OMP parallel]\n"
                "        " + directive + "[OMP do]\n"
                "            " + loop + "[type='dofs',"
                "field_space='any_space_1',it_space='dofs', "
                "upper_bound='nannexed']\n"
                "                " + call + " inc_a_times_x(asum,f1)\n"
                "    " + directive + "[OMP parallel]\n"
                "        " + directive + "[OMP do][reprod=True]\n"
                "            " + loop + "[type='dofs',"
                "field_space='any_space_1',it_space='dofs', "
                "upper_bound='ndofs']\n"
                "                " + call + " sum_x(bsum,f2)\n"
                "    " + gsum + "[scalar='bsum']\n")
            if not annexed:
                expected = expected.replace("nannexed", "ndofs")
        else:  # not distmem. annexed can be True or False
            expected = (
                sched + "[invoke='invoke_0' dm=False]\n"
                "    " + directive + "[OMP parallel]\n"
                "        " + directive + "[OMP do][reprod=True]\n"
                "            " + loop + "[type='dofs',"
                "field_space='any_space_1',it_space='dofs', "
                "upper_bound='ndofs']\n"
                "                " + call + " x_innerproduct_y(asum,f1,f2)\n"
                "    " + directive + "[OMP parallel]\n"
                "        " + directive + "[OMP do]\n"
                "            " + loop + "[type='dofs',"
                "field_space='any_space_1',it_space='dofs', "
                "upper_bound='ndofs']\n"
                "                " + call + " inc_a_times_x(asum,f1)\n"
                "    " + directive + "[OMP parallel]\n"
                "        " + directive + "[OMP do][reprod=True]\n"
                "            " + loop + "[type='dofs',"
                "field_space='any_space_1',it_space='dofs', "
                "upper_bound='ndofs']\n"
                "                " + call + " sum_x(bsum,f2)\n")
        if expected not in result:
            print("Expected ...")
            print(expected)
            print("Found ...")
            print(result)
            assert 0


def test_reductions_reprod():
    '''Check that the optional reprod argument to reductions() method
    works as expected. '''
    for reprod in [False, True]:
        for distmem in [True, False]:
            _, invoke_info = parse(
                os.path.join(BASE_PATH,
                             "15.9.1_X_innerproduct_Y_builtin.f90"),
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
            assert not schedule.reductions(reprod=not reprod)
            assert len(schedule.reductions()) == 1
            from psyclone.dynamo0p3_builtins import DynXInnerproductYKern
            assert (isinstance(schedule.reductions(reprod=reprod)[0],
                               DynXInnerproductYKern))


def test_list_multiple_reductions():
    '''test that we produce correct reduction lists when there is more
    than one reduction in a OpenMP parallel directive. As only one
    reduction per OpenMP parallel region is currently supported we
    need to modify the internal representation after the
    transformations have been performed to enable this test. '''
    for distmem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.9.1_X_innerproduct_Y_builtin.f90"),
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
        arg = call.arguments.args[2]
        arg._type = "gh_real"
        arg.descriptor._access = "gh_sum"
        result = omp_loop_directive._reduction_string()
        assert ", reduction(+:asum), reduction(+:f2)" in result


def test_move_name():
    ''' Test the name property of the MoveTrans class. '''
    move_trans = MoveTrans()
    name = move_trans.name
    assert name == "Move"


def test_move_str():
    ''' Test the str method of the MoveTrans class. '''
    move_trans = MoveTrans()
    name = str(move_trans)
    assert name == "Move a node to a different location"


def test_move_valid_node():
    '''Test that MoveTrans raises an exception if an invalid node
    argument is passed. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "4.2_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    move_trans = MoveTrans()
    with pytest.raises(TransformationError) as excinfo:
        move_trans.apply(None, schedule.children[0])
    assert ("In the Move transformation apply method the "
            "first argument is not a Node") in str(excinfo)


def test_move_back():
    '''Test that MoveTrans moves the node backwards to the expected
    location. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "15.14.2_multiple_set_kernels.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
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
    location when location="after". '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "15.14.2_multiple_set_kernels.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
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
    location. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "15.14.2_multiple_set_kernels.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
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
    location when location="after". '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "15.14.2_multiple_set_kernels.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
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
    _, info = parse(os.path.join(BASE_PATH,
                                 "15.14.1_multi_aX_plus_Y_builtin.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
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


def test_rc_str():
    '''Test the str method and name property of the
    Dynamo0p3RedundantComputationTrans class. '''
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_name = str(rc_trans)
    assert rc_name == "Change iteration space to perform redundant computation"
    name = rc_trans.name
    assert name == "RedundantComputation"


def test_rc_node_not_loop():
    '''Test that Dynamo0p3RedundantComputationTrans raises an exception if the
    node argument is not a loop. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(schedule.children[0])
    assert ("In the Dynamo0p3RedundantComputation transformation apply method "
            "the first argument is not a Loop") in str(excinfo)


def test_rc_invalid_loop(monkeypatch):
    '''Test that Dynamo0p3RedundantComputationTrans raises an exception if the
    supplied loop does not iterate over cells or dofs. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[3]
    # set the loop to a type that should raise an exception
    monkeypatch.setattr(loop, "loop_type", value="colours")
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop)
    assert ("In the Dynamo0p3RedundantComputation transformation apply "
            "method the loop must iterate over cells, dofs or cells of a "
            "given colour, but found 'colours'") in str(excinfo)


def test_rc_nodm():
    '''Test that Dynamo0p3RedundantComputationTrans raises an exception if
    distributed memory is not set. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[0]
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop)
    assert ("In the Dynamo0p3RedundantComputation transformation apply method "
            "distributed memory must be switched on") in str(excinfo)


def test_rc_invalid_depth():
    '''Test that Dynamo0p3RedundantComputationTrans raises an exception if the
    supplied depth is less than 1. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[3]
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop, depth=0)
    assert ("In the Dynamo0p3RedundantComputation transformation apply method "
            "the supplied depth is less than 1") in str(excinfo)


def test_rc_invalid_depth_continuous():
    '''Test that Dynamo0p3RedundantComputationTrans raises an exception if the
    supplied depth equals 1 when modifying a continuous field. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[3]
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop, depth=1)
    assert ("In the Dynamo0p3RedundantComputation transformation apply method "
            "the supplied depth (1) must be greater than the existing halo "
            "depth (1)") in str(excinfo)


def test_rc_continuous_depth():
    '''Test that the loop bounds for a continuous kernel (iterating over
    cells) are modified appropriately, that set_clean() is added
    correctly and halo_exchange modified appropriately after applying
    the redundant computation transformation with a fixed value for
    halo depth. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[3]
    schedule, _ = rc_trans.apply(loop, depth=3)
    invoke.schedule = schedule
    result = str(psy.gen)
    print(result)
    for field_name in ["f2", "m1", "m2"]:
        assert ("IF ({0}_proxy%is_dirty(depth=3)) THEN".
                format(field_name)) in result
        assert ("CALL {0}_proxy%halo_exchange(depth=3)".
                format(field_name)) in result
    assert "DO cell=1,mesh%get_last_halo_cell(3)" in result
    assert ("      CALL f1_proxy%set_dirty()\n"
            "      CALL f1_proxy%set_clean(2)") in result


def test_rc_continuous_no_depth():
    '''Test that the loop bounds for a continuous kernel (iterating over
    cells) are modified appropriately, that set_clean() is added
    correctly and halo_exchange modified appropriately after applying
    the redundant computation transformation with no value for halo
    depth. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[3]
    schedule, _ = rc_trans.apply(loop)
    invoke.schedule = schedule
    result = str(psy.gen)
    print(result)
    for field_name in ["f2", "m1", "m2"]:
        assert ("      IF ({0}_proxy%is_dirty(depth=mesh%get_halo_"
                "depth())) THEN\n"
                "        CALL {0}_proxy%halo_exchange(depth=mesh%"
                "get_halo_depth())".format(field_name)) in result
    assert "DO cell=1,mesh%get_last_halo_cell()" in result
    assert ("      CALL f1_proxy%set_dirty()\n"
            "      CALL f1_proxy%set_clean(mesh%get_halo_depth"
            "()-1)") in result


def test_rc_discontinuous_depth(tmpdir, f90, f90flags, monkeypatch, annexed):
    '''Test that the loop bounds for a discontinuous kernel (iterating
    over cells) with continuous reads are modified appropriately and
    set_clean() added correctly and halo_exchange added appropriately
    after applying the redundant computation transformation with a
    fixed value for halo depth. Also test with and without annexed
    dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, info = parse(os.path.join(BASE_PATH,
                                 "1_single_invoke_w3.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    if annexed:
        # there are no halo exchange calls
        index = 0
    else:
        # there are 3 halo exchange calls
        index = 3
    loop = schedule.children[index]
    schedule, _ = rc_trans.apply(loop, depth=3)
    invoke.schedule = schedule
    result = str(psy.gen)
    print(result)
    for field_name in ["f1", "f2", "m1"]:
        assert ("      IF ({0}_proxy%is_dirty(depth=3)) THEN\n"
                "        CALL {0}_proxy%halo_exchange(depth=3)".
                format(field_name)) in result
    assert "DO cell=1,mesh%get_last_halo_cell(3)" in result
    assert ("      CALL m2_proxy%set_dirty()\n"
            "      CALL m2_proxy%set_clean(3)") in result

    if TEST_COMPILE:
        # If compilation testing has been enabled
        # (--compile --f90="<compiler_name>" flags to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_rc_discontinuous_no_depth(monkeypatch, annexed):
    '''Test that the loop bounds for a discontinuous kernel (iterating
    over cells) with continuous reads are modified appropriately and
    set_clean() added correctly and halo_exchange added appropriately
    after applying the redundant computation transformation with no
    halo depth value. Also test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, info = parse(os.path.join(BASE_PATH,
                                 "1_single_invoke_w3.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    if annexed:
        # there are no halo exchange calls
        index = 0
    else:
        # there are 3 halo exchange calls
        index = 3
    loop = schedule.children[index]
    schedule, _ = rc_trans.apply(loop)
    invoke.schedule = schedule
    result = str(psy.gen)
    print(result)
    for field_name in ["f1", "f2", "m1"]:
        assert ("IF ({0}_proxy%is_dirty(depth=mesh%get_halo_depth())) "
                "THEN".format(field_name)) in result
        assert ("CALL {0}_proxy%halo_exchange(depth=mesh%"
                "get_halo_depth())".format(field_name)) in result
    assert "DO cell=1,mesh%get_last_halo_cell()" in result
    assert "CALL m2_proxy%set_dirty()" not in result
    assert "CALL m2_proxy%set_clean(mesh%get_halo_depth())" in result


def test_rc_all_discontinuous_depth(tmpdir, f90, f90flags):
    ''' Test that the loop bounds for a discontinuous kernel
    (iterating over cells) with discontinuous reads are modified
    appropriately and set_clean() added correctly and halo_exchange
    added appropriately after applying the redundant computation
    transformation with a fixed value for halo depth. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "1_single_invoke_wtheta.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[0]
    schedule, _ = rc_trans.apply(loop, depth=3)
    invoke.schedule = schedule
    result = str(psy.gen)
    print(result)
    assert "IF (f2_proxy%is_dirty(depth=3)) THEN" in result
    assert "CALL f2_proxy%halo_exchange(depth=3)" in result
    assert "DO cell=1,mesh%get_last_halo_cell(3)" in result
    assert "CALL f1_proxy%set_dirty()" in result
    assert "CALL f1_proxy%set_clean(3)" in result

    if TEST_COMPILE:
        # If compilation testing has been enabled
        # (--compile --f90="<compiler_name>" flags to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_rc_all_discontinuous_no_depth(tmpdir, f90, f90flags):
    ''' Test that the loop bounds for a discontinuous kernel
    (iterating over cells) with discontinuous reads are modified
    appropriately and set_clean() added correctly and halo_exchange
    added appropriately after applying the redundant computation
    transformation with no halo depth value. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "1_single_invoke_w2v.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[0]
    schedule, _ = rc_trans.apply(loop)
    invoke.schedule = schedule
    result = str(psy.gen)
    print(result)
    assert ("IF (f2_proxy%is_dirty(depth=mesh%get_halo_depth())) "
            "THEN") in result
    assert ("CALL f2_proxy%halo_exchange(depth=mesh%get_halo_dep"
            "th())") in result
    assert "DO cell=1,mesh%get_last_halo_cell()" in result
    assert "CALL f1_proxy%set_clean(mesh%get_halo_depth())" in result

    if TEST_COMPILE:
        # If compilation testing has been enabled
        # (--compile --f90="<compiler_name>" flags to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_rc_all_discontinuous_vector_depth(tmpdir, f90, f90flags):
    ''' Test that the loop bounds for a discontinuous kernel (iterating
    over cells) are modified appropriately and set_clean() added
    correctly and halo_exchange added appropriately for vector fields
    after applying the redundant computation transformation with a
    fixed value for halo depth. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "1_single_invoke_w3_only_vector.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[0]
    schedule, _ = rc_trans.apply(loop, depth=3)
    invoke.schedule = schedule
    result = str(psy.gen)
    print(result)
    for idx in range(1, 4):
        assert ("IF (f2_proxy({0})%is_dirty(depth=3)) THEN".
                format(idx)) in result
        assert ("CALL f2_proxy({0})%halo_exchange(depth=3)".
                format(idx)) in result
    assert "DO cell=1,mesh%get_last_halo_cell(3)" in result
    for idx in range(1, 4):
        assert "CALL f1_proxy({0})%set_dirty()".format(idx) in result
        assert "CALL f1_proxy({0})%set_clean(3)".format(idx) in result

    if TEST_COMPILE:
        # If compilation testing has been enabled
        # (--compile --f90="<compiler_name>" flags to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_rc_all_discontinuous_vector_no_depth(tmpdir, f90, f90flags):
    ''' Test that the loop bounds for a discontinuous kernel (iterating
    over cells) are modified appropriately and set_clean() added
    correctly and halo_exchange added appropriately for vector fields
    after applying the redundant computation transformation with no
    halo depth value. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "1_single_invoke_wtheta_only_vector.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[0]
    schedule, _ = rc_trans.apply(loop)
    invoke.schedule = schedule
    result = str(psy.gen)
    print(result)
    for idx in range(1, 4):
        assert ("IF (f2_proxy({0})%is_dirty(depth=mesh%get_halo_depth"
                "())) THEN".format(idx)) in result
        assert ("CALL f2_proxy({0})%halo_exchange(depth=mesh%get_halo"
                "_depth())".format(idx)) in result
    assert "DO cell=1,mesh%get_last_halo_cell()" in result
    for idx in range(1, 4):
        assert ("CALL f1_proxy({0})%set_clean(mesh%get_halo_"
                "depth())".format(idx)) in result

    if TEST_COMPILE:
        # If compilation testing has been enabled
        # (--compile --f90="<compiler_name>" flags to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_rc_all_disc_prev_depend_depth(tmpdir, f90, f90flags):
    ''' Test that the loop bounds for a discontinuous kernel
    (iterating over cells) with discontinuous reads are modified
    appropriately and set_clean() added correctly and halo_exchange
    added appropriately in the case where the field requiring a halo
    exchange has a previous non-halo dependence, after applying the
    redundant computation transformation with a fixed value for halo
    depth. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "4.12_multikernel_invokes_w2v.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    schedule.view()
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[1]
    schedule, _ = rc_trans.apply(loop, depth=3)
    invoke.schedule = schedule
    result = str(psy.gen)
    print(result)
    assert "IF (f1_proxy%is_dirty(depth=3)) THEN" not in result
    assert "CALL f1_proxy%halo_exchange(depth=3)" in result
    assert "DO cell=1,mesh%get_last_halo_cell(3)" in result
    assert "CALL f1_proxy%set_dirty()" in result
    assert "CALL f3_proxy%set_dirty()" in result
    assert "CALL f3_proxy%set_clean(3)" in result

    if TEST_COMPILE:
        # If compilation testing has been enabled
        # (--compile --f90="<compiler_name>" flags to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_rc_all_disc_prev_depend_no_depth():
    ''' Test that the loop bounds for a discontinuous kernel
    (iterating over cells) are modified appropriately and set_clean()
    added correctly and halo_exchange added appropriately in the case
    where the field now requiring a halo exchange has a previous
    non-halo dependence after applying the redundant computation
    transformation with no halo depth value. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "4.12_multikernel_invokes_w2v.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[1]
    schedule, _ = rc_trans.apply(loop)
    invoke.schedule = schedule
    result = str(psy.gen)
    print(result)
    assert "CALL f1_proxy%set_dirty()" in result
    assert ("IF (f1_proxy%is_dirty(depth=mesh%get_halo_depth())) "
            "THEN") not in result
    assert ("CALL f1_proxy%halo_exchange(depth=mesh%get_halo_dept"
            "h())") in result
    assert "DO cell=1,mesh%get_last_halo_cell()" in result
    assert "CALL f3_proxy%set_clean(mesh%get_halo_depth())" in result


def test_rc_all_disc_prev_dep_depth_vector(tmpdir, f90, f90flags):
    ''' Test that the loop bounds for a discontinuous kernel (iterating
    over cells) with discontinuous reads are modified appropriately
    and set_clean() added correctly and halo_exchange added
    appropriately in the case where the vector field requiring a halo
    exchange has a previous non-halo dependence, after applying the
    redundant computation transformation with a fixed value for halo
    depth. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "8.2.1_multikernel_invokes_w3_vector.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[1]
    schedule, _ = rc_trans.apply(loop, depth=3)
    invoke.schedule = schedule
    result = str(psy.gen)
    print(result)
    for idx in range(1, 4):
        assert ("IF (f1_proxy({0})%is_dirty(depth="
                "3)) THEN".format(idx)) not in result
        assert ("CALL f1_proxy({0})%halo_exchange("
                "depth=3)".format(idx)) in result
    assert "DO cell=1,mesh%get_last_halo_cell(3)" in result
    for idx in range(1, 4):
        assert "CALL f1_proxy({0})%set_dirty()".format(idx) in result
        assert "CALL f3_proxy({0})%set_dirty()".format(idx) in result
        assert "CALL f3_proxy({0})%set_clean(3)".format(idx) in result

    if TEST_COMPILE:
        # If compilation testing has been enabled
        # (--compile --f90="<compiler_name>" flags to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_rc_all_disc_prev_dep_no_depth_vect(tmpdir, f90, f90flags):
    ''' Test that the loop bounds for a discontinuous kernel (iterating
    over cells) are modified appropriately and set_clean() added
    correctly and halo_exchange added appropriately in the case where
    the vector field now requiring a halo exchange has a previous non-halo
    dependence after applying the redundant computation transformation
    with no halo depth value. '''
    _, info = parse(
        os.path.join(BASE_PATH,
                     "8.2.1_multikernel_invokes_w3_vector.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[1]
    schedule, _ = rc_trans.apply(loop)
    invoke.schedule = schedule
    result = str(psy.gen)
    print(result)
    assert "is_dirty" not in result
    for idx in range(1, 4):
        assert ("CALL f1_proxy({0})%halo_exchange(depth=mesh%get_halo_"
                "depth())".format(idx)) in result
    assert "DO cell=1,mesh%get_last_halo_cell()" in result
    for idx in range(1, 4):
        assert "CALL f1_proxy({0})%set_dirty()".format(idx) in result
        assert ("CALL f3_proxy({0})%set_clean(mesh%get_halo_depth())".
                format(idx)) in result

    if TEST_COMPILE:
        # If compilation testing has been enabled
        # (--compile --f90="<compiler_name>" flags to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_rc_all_disc_prev_dep_no_depth_vect_readwrite(tmpdir, f90, f90flags):
    ''' Test that the loop bounds for a discontinuous kernel (iterating
    over cells) are modified appropriately and set_clean() added
    correctly and halo_exchange added appropriately in the case where
    the vector field now requiring a halo exchange has a previous halo
    dependence (readwrite access) after applying the redundant computation
    transformation with no halo depth value. '''
    _, info = parse(
        os.path.join(BASE_PATH,
                     "8.2.2_multikernel_invokes_wtheta_vector.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[1]
    schedule, _ = rc_trans.apply(loop)
    invoke.schedule = schedule
    result = str(psy.gen)
    print(result)
    # f3 has readwrite access so need to check the halos
    for idx in range(1, 4):
        assert ("IF (f3_proxy({0})%is_dirty(depth=mesh%get_halo_"
                "depth()))".format(idx)) in result
        assert ("CALL f3_proxy({0})%halo_exchange(depth=mesh%get_halo_"
                "depth())".format(idx)) in result
    # f1 has RW to W dependency
    for idx in range(1, 4):
        assert ("CALL f1_proxy({0})%halo_exchange(depth=mesh%get_halo_"
                "depth())".format(idx)) in result
    assert "DO cell=1,mesh%get_last_halo_cell()" in result
    for idx in range(1, 4):
        assert "CALL f1_proxy({0})%set_dirty()".format(idx) in result
        assert ("CALL f3_proxy({0})%set_clean(mesh%get_halo_depth())".
                format(idx)) in result

    if TEST_COMPILE:
        # If compilation testing has been enabled
        # (--compile --f90="<compiler_name>" flags to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_rc_dofs_depth():
    '''Test that the loop bounds when iterating over dofs are modified
    appropriately and set_clean() added correctly and halo_exchange
    added appropriately after applying the redundant computation
    transformation with a fixed value for halo depth where the halo
    fields have no previous dependence. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "15.1.2_inc_X_plus_Y_builtin.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[0]
    schedule, _ = rc_trans.apply(loop, depth=3)
    invoke.schedule = schedule
    result = str(psy.gen)
    print(result)
    for field_name in ["f1", "f2"]:
        assert ("IF ({0}_proxy%is_dirty(depth=3)) "
                "THEN".format(field_name)) in result
        assert ("CALL {0}_proxy%halo_exchange(depth=3"
                ")".format(field_name)) in result
    assert "DO df=1,f1_proxy%vspace%get_last_dof_halo(3)" in result
    assert "CALL f1_proxy%set_dirty()" in result
    assert "CALL f1_proxy%set_clean(3)" in result


def test_rc_dofs_no_depth():
    '''Test that the loop bounds when iterating over dofs are modified
    appropriately and set_clean() added correctly and halo_exchange
    added appropriately after applying the redundant computation
    transformation with no halo depth value where the halo fields have
    no previous dependence. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "15.1.2_inc_X_plus_Y_builtin.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[0]
    schedule, _ = rc_trans.apply(loop)
    invoke.schedule = schedule
    result = str(psy.gen)
    print(result)
    for field_name in ["f1", "f2"]:
        assert ("IF ({0}_proxy%is_dirty(depth=mesh%get_halo_depth())) "
                "THEN".format(field_name)) in result
        assert ("CALL {0}_proxy%halo_exchange(depth=mesh%"
                "get_halo_depth())".format(field_name)) in result
    assert "DO df=1,f1_proxy%vspace%get_last_dof_halo()" in result
    assert "CALL f1_proxy%set_dirty()" not in result
    assert "CALL f1_proxy%set_clean(mesh%get_halo_depth())" in result


def test_rc_dofs_depth_prev_dep(monkeypatch, annexed):
    '''Test that the loop bounds when iterating over dofs are modified
    appropriately and set_clean() added correctly and halo_exchange
    added appropriately after applying the redundant computation
    transformation with a fixed value for halo depth where the halo
    fields have a previous (non-halo-exchange) dependence. Also test
    with and without annexed dofs.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, info = parse(os.path.join(
        BASE_PATH, "15.1.1_builtin_and_normal_kernel_invoke_2.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[4]
    schedule, _ = rc_trans.apply(loop, depth=3)
    invoke.schedule = schedule
    result = str(psy.gen)
    # check the f1 halo exchange is added and the f2 halo exchange is
    # modified
    for field_name in ["f1", "f2"]:
        assert ("CALL {0}_proxy%halo_exchange(depth=3"
                ")".format(field_name)) in result
    # there is no need for a run-time is_dirty check for field f1 as
    # we know that we need a halo exchange. We know this as f1 is
    # modified in an earlier loop which leaves all of f1's halo
    # dirty. As we know that we need the halo to be clean to depth 3
    # we can be certain we need a halo exchange.
    assert ("IF (f1_proxy%is_dirty(depth=3)) "
            "THEN") not in result
    # there is a need for a run-time is_dirty check for field f2 as
    # this field is not modified in this invoke and therefore its halo
    # is in an unknown state before it is read
    assert ("IF (f2_proxy%is_dirty(depth=3)) "
            "THEN") in result

    # check the existing m1 and m2 halo exchanges (for the first
    # un-modified loop) remain unchanged
    for field_name in ["m1", "m2"]:
        assert ("IF ({0}_proxy%is_dirty(depth=1)) "
                "THEN".format(field_name)) in result
        assert ("CALL {0}_proxy%halo_exchange(depth=1"
                ")".format(field_name)) in result
    assert "DO df=1,f1_proxy%vspace%get_last_dof_halo(3)" in result
    assert "CALL f1_proxy%set_dirty()" in result
    assert "CALL f1_proxy%set_clean(3)" in result


def test_rc_dofs_no_depth_prev_dep():
    '''Test that the loop bounds when iterating over dofs are modified
    appropriately and set_clean() added correctly and halo_exchange
    added appropriately after applying the redundant computation
    transformation with no halo depth value where the halo
    fields have a previous (non-halo-exchange) dependence. '''
    _, info = parse(os.path.join(
        BASE_PATH, "15.1.1_builtin_and_normal_kernel_invoke_2.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[4]
    schedule, _ = rc_trans.apply(loop)
    invoke.schedule = schedule
    result = str(psy.gen)
    print(result)
    # check the f1 halo exchange is added and the f2 halo exchange is
    # modified
    for field_name in ["f1", "f2"]:
        assert ("CALL {0}_proxy%halo_exchange(depth=mesh%get_halo_depth()"
                ")".format(field_name)) in result
    assert ("IF (f1_proxy%is_dirty(depth=mesh%get_halo_depth())) "
            "THEN") not in result
    assert ("IF (f2_proxy%is_dirty(depth=mesh%get_halo_depth())) "
            "THEN") in result
    # check the existing m1 and m2 halo exchanges remain unchanged
    for field_name in ["m1", "m2"]:
        assert ("IF ({0}_proxy%is_dirty(depth=1)) "
                "THEN".format(field_name)) in result
        assert ("CALL {0}_proxy%halo_exchange(depth=1"
                ")".format(field_name)) in result
    assert "DO df=1,f1_proxy%vspace%get_last_dof_halo()" in result
    assert "CALL f1_proxy%set_dirty()" in result
    assert "CALL f1_proxy%set_clean(mesh%get_halo_depth())" in result


def test_continuous_no_set_clean():
    '''Test that set_clean is not added for the default iteration space of
    a continuous loop. This is probably covered from tests in
    dynamo0p3_test.py but it is good to have a specific test. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    result = str(psy.gen)
    print(result)
    assert "DO cell=1,mesh%get_last_halo_cell(1)" in result
    assert "CALL f1_proxy%set_dirty()" in result
    assert "CALL f1_proxy%set_clean(" not in result


def test_discontinuous_no_set_clean():
    ''' Test that set_clean is not added for the default iteration
    space of a discontinuous loop. This is probably covered from tests
    in dynamo0p3_test.py but it is good to have a specific test. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "1_single_invoke_w3.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    result = str(psy.gen)
    print(result)
    assert "DO cell=1,mesh%get_last_edge_cell()" in result
    assert "CALL m2_proxy%set_dirty()" in result
    assert "CALL m2_proxy%set_clean(" not in result


def test_dofs_no_set_clean(monkeypatch, annexed):
    '''Test that set_clean is not added for the default iteration space of
    a loop over dofs. This is probably covered from tests in
    dynamo0p3_builtins_test.py but it is good to have a specific
    test. Also test with and without annexed dofs being computed as
    this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, info = parse(os.path.join(BASE_PATH,
                                 "15.7.1_setval_c_builtin.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    result = str(psy.gen)
    print(result)
    assert "halo_exchange" not in result
    if annexed:
        assert "DO df=1,f1_proxy%vspace%get_last_dof_annexed()" in result
    else:
        assert "DO df=1,f1_proxy%vspace%get_last_dof_owned()" in result
    assert "CALL f1_proxy%set_dirty()" in result
    assert "CALL f1_proxy%set_clean(" not in result


def test_rc_vector_depth():
    '''Test that the loop bounds for a (continuous) vector are modified
    appropriately and set_clean() added correctly and halo_exchange
    added/modified appropriately after applying the redundant
    computation transformation with a fixed value for halo depth. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "8_vector_field.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[1]
    schedule, _ = rc_trans.apply(loop, depth=3)
    invoke.schedule = schedule
    result = str(psy.gen)
    print(result)
    assert "IF (f2_proxy%is_dirty(depth=3)) THEN" in result
    assert "CALL f2_proxy%halo_exchange(depth=3)" in result
    assert "DO cell=1,mesh%get_last_halo_cell(3)" in result
    for index in range(1, 4):
        assert "CALL chi_proxy({0})%set_dirty()".format(index) in result
    for index in range(1, 4):
        assert "CALL chi_proxy({0})%set_clean(2)".format(index) in result


def test_rc_vector_no_depth():
    '''Test that the loop bounds for a (continuous) vector are modified
    appropriately and set_clean() added correctly and halo_exchange
    added/modified appropriately after applying the redundant
    computation transformation with no halo depth value. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "8_vector_field.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[1]
    schedule, _ = rc_trans.apply(loop)
    invoke.schedule = schedule
    result = str(psy.gen)
    print(result)
    assert ("IF (f2_proxy%is_dirty(depth=mesh%get_halo_depth())) "
            "THEN") in result
    assert ("CALL f2_proxy%halo_exchange(depth=mesh%"
            "get_halo_depth())") in result
    assert "DO cell=1,mesh%get_last_halo_cell()" in result
    for index in range(1, 4):
        assert "CALL chi_proxy({0})%set_dirty()".format(index) in result
    for index in range(1, 4):
        assert ("CALL chi_proxy({0})%set_clean(mesh%get_halo_depth()"
                "-1)".format(index) in result)


def test_rc_no_halo_decrease():
    '''Test that we do not decrease an existing halo size when setting it
    to a particular value. This situation may happen when the
    redundant computation affects the same field in two different
    loops and both depend on the same halo exchange. '''
    _, info = parse(os.path.join(
        BASE_PATH, "15.1.1_builtin_and_normal_kernel_invoke_2.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    # first, change the size of the f2 halo exchange to 3 by performing
    # redundant computation in the first loop
    loop = schedule.children[3]
    schedule, _ = rc_trans.apply(loop, depth=3)
    invoke.schedule = schedule
    result = str(psy.gen)
    assert "IF (f2_proxy%is_dirty(depth=3)) THEN" in result
    assert "IF (m1_proxy%is_dirty(depth=3)) THEN" in result
    assert "IF (m2_proxy%is_dirty(depth=3)) THEN" in result
    # second, try to change the size of the f2 halo exchange to 2 by
    # performing redundant computation in the second loop
    loop = schedule.children[4]
    schedule, _ = rc_trans.apply(loop, depth=2)
    invoke.schedule = schedule
    result = str(psy.gen)
    assert "IF (f2_proxy%is_dirty(depth=3)) THEN" in result
    assert "IF (m1_proxy%is_dirty(depth=3)) THEN" in result
    assert "IF (m2_proxy%is_dirty(depth=3)) THEN" in result
    # third, set the size of the f2 halo exchange to the full halo
    # depth by performing redundant computation in the second loop
    schedule, _ = rc_trans.apply(loop)
    invoke.schedule = schedule
    result = str(psy.gen)
    assert ("IF (f2_proxy%is_dirty(depth=mesh%get_halo_depth())) "
            "THEN") in result
    assert "IF (m1_proxy%is_dirty(depth=3)) THEN" in result
    assert "IF (m2_proxy%is_dirty(depth=3)) THEN" in result
    # fourth, try to change the size of the f2 halo exchange to 4 by
    # performing redundant computation in the first loop
    loop = schedule.children[3]
    schedule, _ = rc_trans.apply(loop, depth=4)
    invoke.schedule = schedule
    result = str(psy.gen)
    print(result)
    assert ("IF (f2_proxy%is_dirty(depth=mesh%get_halo_depth())) "
            "THEN") in result
    assert "IF (m1_proxy%is_dirty(depth=4)) THEN" in result
    assert "IF (m2_proxy%is_dirty(depth=4)) THEN" in result


def test_rc_updated_dependence_analysis():
    ''' Test that the dependence analysis updates when new halo exchanges
    are added to the schedule. '''
    _, info = parse(os.path.join(
        BASE_PATH, "1_single_invoke_wtheta.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    loop = schedule.children[0]
    kernel = loop.children[0]
    f2_field = kernel.args[1]
    assert not f2_field.backward_dependence()
    # set our loop to redundantly compute to the level 2 halo. This
    # introduces a new halo exchange
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[0]
    schedule, _ = rc_trans.apply(loop, depth=2)
    invoke.schedule = schedule
    previous_field = f2_field.backward_dependence()
    previous_node = previous_field.call
    from psyclone.dynamo0p3 import DynHaloExchange
    # check f2_field has a backward dependence with the new halo
    # exchange field
    assert isinstance(previous_node, DynHaloExchange)
    # check the new halo exchange field has a forward dependence with
    # the kernel f2_field
    assert previous_field.forward_dependence() == f2_field


def test_rc_no_loop_decrease():
    ''' Test that we raise an exception if we try to reduce the size of a
    loop halo when using the redundant computation transformation. This is
    not allowed partly for simplicity but also because, in the current
    implementation we might not decrease the size of the relevant halo
    exchange as these can only be increased with the current logic. '''
    _, info = parse(os.path.join(
        BASE_PATH, "1_single_invoke_w2v.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    # first set our loop to redundantly compute to the level 2 halo
    loop = schedule.children[0]
    schedule, _ = rc_trans.apply(loop, depth=2)
    invoke.schedule = schedule
    # now try to reduce the redundant computation to the level 1 halo
    # f1 and f2 have read accesses (readwrite and read) so there
    # is one halo exchange for each before the loop
    loop = schedule.children[2]
    with pytest.raises(TransformationError) as excinfo:
        schedule, _ = rc_trans.apply(loop, depth=1)
    assert ("supplied depth (1) must be greater than the existing halo depth "
            "(2)") in str(excinfo)
    # second set our loop to redundantly compute to the maximum halo depth
    schedule, _ = rc_trans.apply(loop)
    invoke.schedule = schedule
    # now try to reduce the redundant computation to a fixed value
    with pytest.raises(TransformationError) as excinfo:
        schedule, _ = rc_trans.apply(loop, depth=2)
    assert ("loop is already set to the maximum halo depth so can't be "
            "set to a fixed value") in str(excinfo)
    # now try to set the redundant computation to the same (max) value
    # it is now
    with pytest.raises(TransformationError) as excinfo:
        schedule, _ = rc_trans.apply(loop)
    assert ("loop is already set to the maximum halo depth so this "
            "transformation does nothing") in str(excinfo)


def test_rc_remove_halo_exchange(tmpdir, f90, f90flags, monkeypatch):
    '''Test that a halo exchange is removed if redundant computation means
    that it is no longer required. Halo exchanges are not required in
    this example when we compute annexed dofs. Therefore we ensure we
    compute over owned dofs (via monkeypatch) to perform the test.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", False)
    _, info = parse(os.path.join(
        BASE_PATH, "14.7_halo_annexed.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    result = str(psy.gen)
    assert "CALL f1_proxy%halo_exchange(depth=1)" in result
    assert "CALL f2_proxy%halo_exchange(depth=1)" in result
    assert "IF (m1_proxy%is_dirty(depth=1)) THEN" in result
    assert "CALL m1_proxy%halo_exchange(depth=1)" in result

    if TEST_COMPILE:
        # If compilation testing has been enabled
        # (--compile --f90="<compiler_name>" flags to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)

    #
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    #
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[0]
    rc_trans.apply(loop, depth=1)
    result = str(psy.gen)
    assert "CALL f1_proxy%halo_exchange(depth=1)" not in result
    assert "CALL f2_proxy%halo_exchange(depth=1)" in result
    assert "IF (m1_proxy%is_dirty(depth=1)) THEN" in result
    assert "CALL m1_proxy%halo_exchange(depth=1)" in result
    #
    loop = schedule.children[1]
    rc_trans.apply(loop, depth=1)
    result = str(psy.gen)
    assert "CALL f1_proxy%halo_exchange(depth=1)" not in result
    assert "CALL f2_proxy%halo_exchange(depth=1)" not in result
    assert "IF (m1_proxy%is_dirty(depth=1)) THEN" in result
    assert "CALL m1_proxy%halo_exchange(depth=1)" in result


def test_rc_max_remove_halo_exchange(tmpdir, f90, f90flags):
    ''' Add test to redundantly compute a discontinuous (wtheta) and
    continuous (w2) field to the maximum halo depth and then check
    that a discontinuous halo exchange is removed in this case as we
    always remove the halo exchange if we write to a discontinuous
    field to maximum depth. Also check that the halo exchange is not
    removed for the continuous case as the outer halo stays dirty.
    The halo should also have an if round it as we do not know how
    much redundant computation we are doing. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "15.1.2_builtin_and_normal_kernel_"
                                 "invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    result = str(psy.gen)
    #
    # at this point we know we need a halo exchange of depth 1 for f3
    assert "CALL f3_proxy%halo_exchange(depth=1)" in result
    assert "IF (f3_proxy%is_dirty(depth=1)) THEN" not in result
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[3]
    rc_trans.apply(loop)
    result = str(psy.gen)
    print(result)
    # f3 halo exchange is not removed even though we redundantly
    # compute f3 as the redundant computation is on a continuous field
    # and therefore the outermost halo stays dirty. We can not be
    # certain whether the halo exchange is required or not as we don't
    # know the depth of the halo.
    assert "CALL f3_proxy%halo_exchange(depth=1)" in result
    # we do not know whether we need the halo exchange so we include an if
    assert "IF (f3_proxy%is_dirty(depth=1)) THEN" in result
    #
    assert "CALL f4_proxy%halo_exchange(depth=1)" in result
    loop = schedule.children[4]
    rc_trans.apply(loop)
    result = str(psy.gen)
    # f4 halo exchange is removed as it is redundantly computed to the
    # last level halo and is discontinuous so all levels of the halo
    # are clean. However, we introduce a new halo exchange for
    # f5. This could be removed by redundant computation but we don't
    # bother as that is not relevant to this test.
    assert "CALL f4_proxy%halo_exchange(depth=1)" not in result

    if TEST_COMPILE:
        # If compilation testing has been enabled
        # (--compile --f90="<compiler_name>" flags to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_rc_continuous_halo_remove():
    ''' Check that we do not remove a halo exchange when the field is
    continuous and the redundant computation depth equals the required
    halo access depth. The reason for this is that the outer halo
    remains invalid when written to for a continuous field. Also check
    that we do remove the halo exchange when the redundant computation
    depth is one more than the required halo access depth. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "15.1.2_builtin_and_normal_kernel_"
                                 "invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    result = str(psy.gen)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    f3_write_loop = schedule.children[3]
    f3_read_loop = schedule.children[7]
    assert "CALL f3_proxy%halo_exchange(depth=1)" in result
    assert "IF (f3_proxy%is_dirty(depth=1)) THEN" not in result
    rc_trans.apply(f3_read_loop, depth=3)
    rc_trans.apply(f3_write_loop, depth=3)
    result = str(psy.gen)
    assert "CALL f3_proxy%halo_exchange(depth=3)" in result
    assert "IF (f3_proxy%is_dirty(depth=3)) THEN" not in result
    #
    rc_trans.apply(f3_write_loop, depth=4)
    result = str(psy.gen)
    assert "CALL f3_proxy%halo_exchange(depth=" not in result
    assert "IF (f3_proxy%is_dirty(depth=" not in result


def test_rc_discontinuous_halo_remove(monkeypatch):
    ''' Check that we do remove a halo exchange when the field is
    discontinuous and the redundant computation depth equals the
    required halo access depth. Also check that we do not remove the
    halo exchange when the redundant computation depth is one less
    than the required halo access depth. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "15.1.2_builtin_and_normal_kernel_"
                                 "invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    result = str(psy.gen)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    f4_write_loop = schedule.children[4]
    f4_read_loop = schedule.children[7]
    assert "CALL f4_proxy%halo_exchange(depth=1)" in result
    assert "IF (f4_proxy%is_dirty(depth=1)) THEN" not in result
    rc_trans.apply(f4_read_loop, depth=3)
    rc_trans.apply(f4_write_loop, depth=2)
    result = str(psy.gen)
    assert "CALL f4_proxy%halo_exchange(depth=3)" in result
    assert "IF (f4_proxy%is_dirty(depth=3)) THEN" not in result
    # Increase RC depth to 3 and check that halo exchange is removed
    # when a discontinuous field has write access
    rc_trans.apply(f4_write_loop, depth=3)
    result = str(psy.gen)
    assert "CALL f4_proxy%halo_exchange(depth=" not in result
    assert "IF (f4_proxy%is_dirty(depth=" not in result
    # Increase RC depth to 3 and check that halo exchange is not removed
    # when a discontinuous field has readwrite access
    call = f4_write_loop.children[0]
    f4_arg = call.arguments.args[0]
    monkeypatch.setattr(f4_arg, "_access", value="gh_readwrite")
    monkeypatch.setattr(f4_write_loop, "_upper_bound_halo_depth", value=2)
    rc_trans.apply(f4_write_loop, depth=3)
    result = str(psy.gen)
    assert "CALL f4_proxy%halo_exchange(depth=" in result
    assert "IF (f4_proxy%is_dirty(depth=" in result


def test_rc_reader_halo_remove():
    '''check that we do not add an unnecessary halo exchange when we
    increase the depth of halo that a loop computes but the previous loop
    still computes deep enough into the halo to avoid needing a halo
    exchange.'''

    _, info = parse(os.path.join(BASE_PATH,
                                 "15.1.2_builtin_and_normal_kernel_"
                                 "invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    result = str(psy.gen)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    invoke.schedule = schedule
    result = str(psy.gen)
    assert "CALL f2_proxy%halo_exchange(depth=1)" in result

    rc_trans = Dynamo0p3RedundantComputationTrans()

    # redundant computation to avoid halo exchange for f2
    schedule, _ = rc_trans.apply(schedule.children[1], depth=2)
    invoke.schedule = schedule
    result = str(psy.gen)
    assert "CALL f2_proxy%halo_exchange(" not in result

    # redundant computation to depth 2 in f2 reader loop should not
    # cause a new halo exchange as it is still covered by depth=2 in
    # the writer loop
    schedule, _ = rc_trans.apply(schedule.children[2], depth=2)
    invoke.schedule = schedule
    result = str(psy.gen)
    assert "CALL f2_proxy%halo_exchange(" not in result


def test_rc_vector_reader_halo_remove():
    ''' Check that we do not add unnecessary halo exchanges for a vector
    field when we increase the depth of halo that a loop computes but
    the previous loop still computes deep enough into the halo to
    avoid needing halo exchanges. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "8.2.1_multikernel_invokes_w3_vector.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    result = str(psy.gen)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    assert "is_dirty" not in result
    assert "halo_exchange" not in result

    rc_trans = Dynamo0p3RedundantComputationTrans()

    # Redundant computation for first loop
    schedule, _ = rc_trans.apply(schedule.children[0], depth=1)
    invoke.schedule = schedule
    result = str(psy.gen)
    assert result.count("is_dirty") == 3
    assert result.count("halo_exchange") == 3

    # Redundant computation in reader loop should not
    # cause a new halo exchange as it is still covered by depth=1 in
    # the writer loop
    schedule, _ = rc_trans.apply(schedule.children[4], depth=1)
    invoke.schedule = schedule
    result = str(psy.gen)
    assert result.count("is_dirty") == 3
    assert result.count("halo_exchange") == 3


def test_rc_vector_reader_halo_readwrite():
    ''' When we increase the depth of halo that a loop computes but the
    previous loop still computes deep enough into the halo the added
    halo exchanges stem from the vector readwrite access. '''
    _, info = parse(os.path.join(
        BASE_PATH, "8.2.2_multikernel_invokes_wtheta_vector.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    result = str(psy.gen)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    assert "is_dirty" not in result
    assert "halo_exchange" not in result

    rc_trans = Dynamo0p3RedundantComputationTrans()

    # Redundant computation for first loop: both fields have
    # read dependencies for all three components
    schedule, _ = rc_trans.apply(schedule.children[0], depth=1)
    invoke.schedule = schedule
    result = str(psy.gen)
    assert result.count("is_dirty") == 6
    assert result.count("halo_exchange") == 6

    # Redundant computation in reader loop causes new halo exchanges
    # due to readwrite dependency in f3
    schedule, _ = rc_trans.apply(schedule.children[7], depth=1)
    invoke.schedule = schedule
    result = str(psy.gen)
    assert result.count("is_dirty") == 9
    assert result.count("halo_exchange") == 9

    # Now increase RC depth of the reader loop to 2 to check for
    # additional halo exchanges (3 more due to readwrite to read
    # dependency in f1)
    schedule, _ = rc_trans.apply(schedule.children[10], depth=2)
    invoke.schedule = schedule
    result = str(psy.gen)
    # Check for additional halo exchanges
    assert result.count("halo_exchange") == 12
    # Check that additional halo exchanges for all three f1
    # vector field components are of depth 2 and that they
    # do not have if tests around them
    for idvct in range(1, 4):
        idx = str(idvct)
        assert (
            "CALL f1_proxy(" + idx + ")%halo_exchange(depth=2)") in result
        assert (
            "      IF (f1_proxy(" + idx + ")%is_dirty(depth=2)) THEN\n"
            "         CALL f1_proxy(" + idx + ")%halo_exchange(depth=2)\n"
            "      END IF\n") not in result


def test_stencil_rc_max_depth_1(monkeypatch):
    '''If a loop contains a kernel with a stencil access and the loop
    attempts to compute redundantly into the halo to the maximum depth
    then the stencil will access beyond the halo bounds. This is
    therefore not allowed and exceptions are raised in the
    Dynamo0p3RedundantComputationTrans transformation and in
    _compute_single_halo_info. This test checks these exceptions are
    raised correctly. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "19.1_single_stencil.f90"),
                    api="dynamo0.3")
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[3]
    rc_trans = Dynamo0p3RedundantComputationTrans()
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop)
    assert ("In the Dynamo0p3RedundantComputation transformation apply method "
            "the loop contains field 'f2' with a stencil access in kernel "
            "'testkern_stencil_code', so it is invalid to set redundant "
            "computation to maximum depth" in str(excinfo.value))

    halo_exchange = schedule.children[0]
    monkeypatch.setattr(loop, "_upper_bound_halo_depth", None)
    with pytest.raises(GenerationError) as excinfo:
        _ = halo_exchange._compute_halo_read_info()
    assert ("redundant computation to max depth with a stencil is "
            "invalid" in str(excinfo.value))


def test_rc_invalid_depth_type():
    '''If an incorrect type is passed as a depth value to the redundant
    computation transformation an exception should be raised. This test
    checks that this exception is raised as expected.'''
    _, info = parse(os.path.join(BASE_PATH,
                                 "1_single_invoke.f90"),
                    api="dynamo0.3")
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[3]
    rc_trans = Dynamo0p3RedundantComputationTrans()
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop, depth="2")
    assert ("the supplied depth should be an integer but found "
            "type '%s'" % (type("2")) in str(excinfo.value))


def test_loop_fusion_different_loop_depth(monkeypatch, annexed):
    '''We can only loop fuse if two loops iterate over the same entities
    and iterate over the same depth. The loop fusion transformation
    raises an exception if this is not the case. This test checks that
    the exception is raised correctly. We also test when annexed is
    False and True as it affects how many halo exchanges are
    generated.

    '''

    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, info = parse(os.path.join(BASE_PATH,
                                 "4.6_multikernel_invokes.f90"),
                    api="dynamo0.3")
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    if annexed:
        index = 5
    else:
        index = 7
        # move the halo exchange between the two loops
        move_trans = MoveTrans()
        move_trans.apply(schedule.children[7], schedule.children[6])
    # make the first loop redundantly compute to halo level 3
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[index], depth=3)
    # try to fuse the loops. This should fail as the depths are different
    if annexed:
        # we now have an additional halo exchange for the gh_inc
        # access in the first loop
        index += 1
    f_trans = DynamoLoopFuseTrans()
    with pytest.raises(TransformationError) as excinfo:
        f_trans.apply(schedule.children[index], schedule.children[index+1])
    assert ("Error in DynamoLoopFuse transformation. The halo-depth indices "
            "are not the same. Found '3' and '1'" in str(excinfo.value))
    # now redundantly compute to the full halo
    rc_trans.apply(schedule.children[index+1])
    if annexed:
        # we now have a halo exchange between the 2 loops due to
        # redundant computation
        index = 7
        # move the halo exchange between the two loops
        move_trans = MoveTrans()
        move_trans.apply(schedule.children[7], schedule.children[6])
    # try to fuse the loops. This should fail as the depths are different
    f_trans = DynamoLoopFuseTrans()
    with pytest.raises(TransformationError) as excinfo:
        f_trans.apply(schedule.children[index], schedule.children[index+1])
    assert ("Error in DynamoLoopFuse transformation. The halo-depth indices "
            "are not the same. Found '3' and 'None'" in str(excinfo.value))


def test_loop_fusion_different_loop_name(monkeypatch):
    ''' We can only loop fuse if two loops iterate over the same entities
    and iterate over the same depth. The loop fusion transformation
    raises an exception if this is not the case. This test checks that
    the exception is raised correctly. '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "4.12_multikernel_invokes_w2v.f90"),
                    api="dynamo0.3")
    # First test for f1 readwrite to read dependency
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[0], depth=3)
    f_trans = DynamoLoopFuseTrans()
    with pytest.raises(TransformationError) as excinfo:
        # Indices of loops to fuse in the schedule
        f_trans.apply(schedule.children[2], schedule.children[3])
    assert ("Error in DynamoLoopFuse transformation. The upper bound names "
            "are not the same. Found 'cell_halo' and 'ncells'"
            in str(excinfo.value))
    # Now test for f1 write to read dependency
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    call = schedule.children[0].children[0]
    f1_arg = call.arguments.args[0]
    monkeypatch.setattr(f1_arg, "_access", value="gh_write")
    rc_trans.apply(schedule.children[0], depth=3)
    with pytest.raises(TransformationError) as excinfo:
        f_trans.apply(schedule.children[1], schedule.children[2])
    assert ("Error in DynamoLoopFuse transformation. The upper bound names "
            "are not the same. Found 'cell_halo' and 'ncells'"
            in str(excinfo.value))


def test_rc_max_w_to_r_continuous_known_halo(monkeypatch, annexed):
    '''If we have a continuous field being written to in one loop to the
    maximum halo depth and then being read in a following (dependent)
    loop to the maximum halo depth we can determine that we definitely
    need a halo exchange (at the outermost halo level). This test
    checks that a halo with no runtime checking is produced for this
    case. We also test when annexed is False and True as it affects
    how many halo exchanges are generated.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "14.10_halo_continuous_cell_w_to_r.f90"), api="dynamo0.3")
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    if annexed:
        index1 = 1
        index2 = 3
    else:
        index1 = 2
        index2 = 5
    w_loop = schedule.children[index1]
    r_loop = schedule.children[index2]

    # make both the writer and reader loops use the full halo
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(w_loop)
    rc_trans.apply(r_loop)

    if annexed:
        w_to_r_halo_exchange = schedule.children[3]
    else:
        w_to_r_halo_exchange = schedule.children[4]

    # sanity check that the halo exchange goes to the full halo depth
    assert (w_to_r_halo_exchange._compute_halo_depth() ==
            "mesh%get_halo_depth()")

    # the halo exchange should be both required to be added and known
    # to be needed
    required, known = w_to_r_halo_exchange.required()
    assert required
    assert known


def test_red_comp_w_to_n_r_clean_gt_cleaned():
    '''Tests the case where we have multiple (derived) read dependence
    entries and one of them has a literal depth value (and no
    associated variable) and we write redundantly into the halo with a
    literal depth. Depending on the literal values of the halo-reads
    and the halo-depth of the redundant write we may, or may not, know
    that we need a halo exchange. This test checks that we get the
    expected behaviour.

    '''
    # The initial test case writes to a field over dofs, then reads
    # the halo to depth 2 with a stencil, then reads the halo to a
    # variable depth with a stencil
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "14.11_halo_required_clean_multi.f90"), api="dynamo0.3")
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    w_loop = schedule.children[0]
    w_to_r_halo_exchange = schedule.children[1]

    # make the writer loop write redundantly into the level 1 halo. We
    # now make the level one halo clean but we still definitely need a
    # halo exchange as one of the readers reads the halo to level 2
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(w_loop, depth=1)

    # the halo exchange should be both required and known to be needed
    required, known = w_to_r_halo_exchange.required()
    assert required
    assert known

    # make the writer loop write redundantly into the level 2 halo. We
    # now make the level two halo clean so the reader that reads to
    # the level 2 halo does not need a halo exchange, but another
    # reader reads to a variable level so we are not sure if we need a
    # halo exchange or not.
    rc_trans.apply(w_loop, depth=2)

    w_to_r_halo_exchange = schedule.children[1]

    # the halo exchange should be required but not known to be needed
    required, known = w_to_r_halo_exchange.required()
    assert required
    assert not known

    # make the reader loop with a variable size also write redundantly
    # into the level 3 halo. We now know that we need a halo exchange
    # as the minimum halo depth that needs to be clean is level 3 and
    # we only clean up to the level 2 halo.
    r_var_stencil_loop = schedule.children[2]
    rc_trans.apply(r_var_stencil_loop, depth=3)

    w_to_r_halo_exchange = schedule.children[1]

    # the halo exchange should be required and known to be needed
    required, known = w_to_r_halo_exchange.required()
    assert required
    assert known


def test_rc_no_directive():
    '''When the redundant computation transformation is given a Loop whose
    parent is a directive an exception is raised as this is not
    supported (redundant computation transformations must be applied
    before directives are added). This test checks that this exception
    is raised correctly.'''
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "1_single_invoke.f90"), api="dynamo0.3")
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    # create a colouring transformation and apply this to the loop
    ctrans = Dynamo0p3ColourTrans()
    schedule, _ = ctrans.apply(schedule.children[3])

    # create an openmp transformation and apply this to the loop
    otrans = DynamoOMPParallelLoopTrans()
    schedule, _ = otrans.apply(schedule.children[3].children[0])

    # create a redundant computation transformation and apply this to the loop
    rc_trans = Dynamo0p3RedundantComputationTrans()
    with pytest.raises(TransformationError) as excinfo:
        schedule, _ = rc_trans.apply(
            schedule.children[3].children[0].children[0], depth=1)
    assert ("Redundant computation must be applied before directives are added"
            in str(excinfo.value))


def test_rc_wrong_parent(monkeypatch):
    '''When the redundant computation transformation is given a Loop which
    has the wrong parent, and that parent is not a Directive (which is
    handled in a separate case) an exception is raised. This test
    checks that this exception is raised correctly.'''
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "1_single_invoke.f90"), api="dynamo0.3")
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    # make the parent of the loop a halo exchange
    monkeypatch.setattr(schedule.children[3], "parent", schedule.children[0])

    rc_trans = Dynamo0p3RedundantComputationTrans()
    # apply redundant computation to the loop
    with pytest.raises(TransformationError) as excinfo:
        schedule, _ = rc_trans.apply(schedule.children[3], depth=1)
    assert ("the parent of the supplied loop must be the Schedule, or a Loop"
            in str(excinfo.value))


def test_rc_parent_loop_colour(monkeypatch):
    '''If the parent of the loop supplied to the redundant computation
    transformation is a loop then

    1) the parent loop's parent should be a schedule. If this is not
    the case then an exception is raised.

    2) the parent loop should iterate over 'colours'. If this is not
    the case then an exception is raised.

    3) the supplied loop should iterate over cells of a given
    colour. If this is not the case then an exception is raised.

    This test checks that the appropriate exceptions are correctly
    raised for these three situations.

    '''

    _, invoke_info = parse(os.path.join(
        BASE_PATH, "1_single_invoke.f90"), api="dynamo0.3")
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    # apply colouring
    # create colour transformation
    ctrans = Dynamo0p3ColourTrans()
    # Colour the loop
    schedule, _ = ctrans.apply(schedule.children[3])

    # make the parent of the outermost loop something other than
    # Schedule (we use halo exchange in this case)
    monkeypatch.setattr(schedule.children[3], "parent", schedule.children[0])

    rc_trans = Dynamo0p3RedundantComputationTrans()
    # apply redundant computation to the loop
    with pytest.raises(TransformationError) as excinfo:
        _, _ = rc_trans.apply(schedule.children[3].children[0], depth=1)
    assert ("if the parent of the supplied Loop is also a Loop then the "
            "parent's parent must be the Schedule" in str(excinfo.value))

    # make the outermost loop iterate over cells (it should be
    # colours). We can ignore the previous monkeypatch as this
    # exception is ecountered before the previous one.
    monkeypatch.setattr(schedule.children[3], "_loop_type", "cells")

    rc_trans = Dynamo0p3RedundantComputationTrans()
    # apply redundant computation to the loop
    with pytest.raises(TransformationError) as excinfo:
        _, _ = rc_trans.apply(schedule.children[3].children[0], depth=1)
    assert ("if the parent of the supplied Loop is also a Loop then the "
            "parent must iterate over 'colours'" in str(excinfo.value))

    # make the innermost loop iterate over cells (it should be
    # colour). We can ignore the previous monkeypatches as this
    # exception is encountered before the previous ones.
    monkeypatch.setattr(schedule.children[3].children[0], "_loop_type",
                        "cells")

    rc_trans = Dynamo0p3RedundantComputationTrans()
    # apply redundant computation to the loop
    with pytest.raises(TransformationError) as excinfo:
        _, _ = rc_trans.apply(schedule.children[3].children[0], depth=1)
    assert ("if the parent of the supplied Loop is also a Loop then the "
            "supplied Loop must iterate over 'colour'" in str(excinfo.value))


def test_rc_unsupported_loop_type(monkeypatch):
    '''When an unsupported loop type is provided to the redundant
    computation apply method an exception is raised. It is not
    possible to get to this exception in normal circumstances due to
    the validation tests so we monkey patch it. This test checks that
    the exception is raised correctly.

    '''
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "1_single_invoke.f90"), api="dynamo0.3")
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    # apply colouring
    # create colour transformation
    ctrans = Dynamo0p3ColourTrans()
    # Colour the loop
    schedule, _ = ctrans.apply(schedule.children[3])

    # make the loop type invalid
    monkeypatch.setattr(schedule.children[3].children[0], "_loop_type",
                        "invalid")

    rc_trans = Dynamo0p3RedundantComputationTrans()

    # switch off validation
    monkeypatch.setattr(rc_trans, "_validate",
                        lambda loop, depth: None)

    # apply redundant computation to the loop
    with pytest.raises(TransformationError) as excinfo:
        _, _ = rc_trans.apply(schedule.children[3].children[0], depth=1)
    assert "Unsupported loop_type 'invalid' found" in str(excinfo.value)


def test_rc_colour_no_loop_decrease():
    '''Test that we raise an exception if we try to reduce the size of a
    loop halo depth when using the redundant computation
    transformation. This is not allowed partly for simplicity but also
    because, in the current implementation we might not decrease the
    size of the relevant halo exchange as these can only be increased
    with the current logic.

    '''
    _, info = parse(os.path.join(
        BASE_PATH, "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    # create our colour transformation
    ctrans = Dynamo0p3ColourTrans()
    # Colour the loop
    schedule, _ = ctrans.apply(schedule.children[3])

    rc_trans = Dynamo0p3RedundantComputationTrans()
    # first set our loop to redundantly compute to the level 2 halo
    loop = schedule.children[3].children[0]
    schedule, _ = rc_trans.apply(loop, depth=2)
    invoke.schedule = schedule
    # now try to reduce the redundant computation to the level 1 halo
    with pytest.raises(TransformationError) as excinfo:
        schedule, _ = rc_trans.apply(loop, depth=1)
    assert ("supplied depth (1) must be greater than the existing halo depth "
            "(2)") in str(excinfo)
    # second set our loop to redundantly compute to the maximum halo depth
    schedule, _ = rc_trans.apply(loop)
    invoke.schedule = schedule
    # now try to reduce the redundant computation to a fixed value
    with pytest.raises(TransformationError) as excinfo:
        schedule, _ = rc_trans.apply(loop, depth=2)
    assert ("loop is already set to the maximum halo depth so can't be "
            "set to a fixed value") in str(excinfo)
    # now try to set the redundant computation to the same (max) value
    # it is now
    with pytest.raises(TransformationError) as excinfo:
        schedule, _ = rc_trans.apply(loop)
    assert ("loop is already set to the maximum halo depth so this "
            "transformation does nothing") in str(excinfo)


def test_rc_colour(tmpdir, f90, f90flags):
    '''Test that we can redundantly compute over a colour in a coloured
    loop.'''
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "1_single_invoke.f90"), api="dynamo0.3")
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    # create our colour transformation
    ctrans = Dynamo0p3ColourTrans()
    # Colour the loop
    cschedule, _ = ctrans.apply(schedule.children[3])

    # create our redundant computation transformation
    rc_trans = Dynamo0p3RedundantComputationTrans()
    # apply redundant computation to the colour loop
    rc_trans.apply(cschedule.children[3].children[0], depth=2)

    result = str(psy.gen)

    assert (
        "      IF (f2_proxy%is_dirty(depth=2)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=2)\n"
        "      END IF \n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=2)) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=2)\n"
        "      END IF \n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=2)) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=2)\n"
        "      END IF \n" in result)
    assert "      cmap => mesh%get_colour_map()\n" in result
    assert (
        "      DO colour=1,ncolour\n"
        "        DO cell=1,mesh%get_last_halo_cell_per_colour(colour,2)\n"
        in result)

    # We've requested redundant computation out to the level 2 halo
    # but f1 is continuous and so the outermost halo depth (2) remains
    # dirty. This means that all of the halo is dirty apart from level
    # 1.
    assert (
        "      CALL f1_proxy%set_dirty()\n"
        "      CALL f1_proxy%set_clean(1)" in result)

    if TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag
        # to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_rc_max_colour(tmpdir, f90, f90flags):
    '''Test that we can redundantly compute over a colour to the maximum
    depth in a coloured loop.'''
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "1_single_invoke.f90"), api="dynamo0.3")
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    # create our colour transformation
    ctrans = Dynamo0p3ColourTrans()
    # Colour the loop
    cschedule, _ = ctrans.apply(schedule.children[3])

    # create our redundant computation transformation
    rc_trans = Dynamo0p3RedundantComputationTrans()
    # apply redundant computation to the colour loop out to the full
    # halo depth
    rc_trans.apply(cschedule.children[3].children[0])

    result = str(psy.gen)
    assert (
        "      IF (f2_proxy%is_dirty(depth=mesh%get_halo_depth())) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=mesh%get_halo_depth())\n"
        "      END IF \n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=mesh%get_halo_depth())) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=mesh%get_halo_depth())\n"
        "      END IF \n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=mesh%get_halo_depth())) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=mesh%get_halo_depth())\n"
        "      END IF \n" in result)
    assert "      cmap => mesh%get_colour_map()\n" in result
    assert (
        "      DO colour=1,ncolour\n"
        "        DO cell=1,mesh%get_last_halo_cell_per_colour(colour)\n"
        in result)

    assert (
        "      CALL f1_proxy%set_dirty()\n"
        "      CALL f1_proxy%set_clean(mesh%get_halo_depth()-1)" in result)

    if TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag
        # to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_colour_discontinuous():
    ''' Test that we raise an exception if we try to colour a loop
    containing a kernel that modifies a discontinuous field.
    The test is performed twice: first for a discontinuous wtheta writer
    and then for a discontinuous w2v readwriter. '''
    fsnames = ["wtheta", "w2v"]
    for name in fsnames:
        filename = "1_single_invoke_" + name + ".f90"
        _, invoke_info = parse(os.path.join(BASE_PATH, filename),
                               api=TEST_API)
        psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule

        # Create our colour transformation
        ctrans = Dynamo0p3ColourTrans()

        with pytest.raises(TransformationError) as excinfo:
            # Colour the loop
            _, _ = ctrans.apply(schedule.children[0])
        assert ("Loops iterating over a discontinuous function space are "
                "not currently supported") in str(excinfo)


def test_rc_then_colour(tmpdir, f90, f90flags):
    '''Test that we generate correct code when we first perform redundant
    computation to a fixed depth then colour the loop.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    # create our colour transformation
    ctrans = Dynamo0p3ColourTrans()

    # create our redundant computation transformation
    rc_trans = Dynamo0p3RedundantComputationTrans()

    # apply redundant computation to the loop, out to the level-3 halo
    schedule, _ = rc_trans.apply(schedule.children[3], 3)

    # Colour the loop
    schedule, _ = ctrans.apply(schedule.children[3])

    psy.invokes.invoke_list[0].schedule = schedule

    result = str(psy.gen)

    assert (
        "      IF (f2_proxy%is_dirty(depth=3)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=3)\n"
        "      END IF \n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=3)) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=3)\n"
        "      END IF \n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=3)) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=3)\n"
        "      END IF \n" in result)
    assert "      cmap => mesh%get_colour_map()\n" in result
    assert (
        "      DO colour=1,ncolour\n"
        "        DO cell=1,mesh%get_last_halo_cell_per_colour(colour,3)\n"
        "          !\n"
        "          CALL testkern_code(nlayers, a, f1_proxy%data,"
        " f2_proxy%data, m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, "
        "map_w1(:,cmap(colour, cell)), ndf_w2, undf_w2, "
        "map_w2(:,cmap(colour, cell)), ndf_w3, undf_w3, "
        "map_w3(:,cmap(colour, cell)))\n" in result)

    assert (
        "      CALL f1_proxy%set_dirty()\n"
        "      CALL f1_proxy%set_clean(2)" in result)

    if TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag
        # to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_rc_then_colour2(tmpdir, f90, f90flags):
    '''Test that we generate correct code when we first perform redundant
    computation to the full depth then colour the loop.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    # create our colour transformation
    ctrans = Dynamo0p3ColourTrans()

    # create our redundant computation transformation
    rc_trans = Dynamo0p3RedundantComputationTrans()

    # apply redundant computation to the loop to the full halo depth
    schedule, _ = rc_trans.apply(schedule.children[3])

    # Colour the loop
    schedule, _ = ctrans.apply(schedule.children[3])

    psy.invokes.invoke_list[0].schedule = schedule

    result = str(psy.gen)

    assert (
        "      IF (f2_proxy%is_dirty(depth=mesh%get_halo_depth())) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=mesh%get_halo_depth())\n"
        "      END IF \n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=mesh%get_halo_depth())) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=mesh%get_halo_depth())\n"
        "      END IF \n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=mesh%get_halo_depth())) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=mesh%get_halo_depth())\n"
        "      END IF \n" in result)
    assert "      cmap => mesh%get_colour_map()\n" in result
    assert (
        "      DO colour=1,ncolour\n"
        "        DO cell=1,mesh%get_last_halo_cell_per_colour(colour)\n"
        in result)

    assert (
        "      CALL f1_proxy%set_dirty()\n"
        "      CALL f1_proxy%set_clean(mesh%get_halo_depth()-1)" in result)

    if TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag
        # to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_loop_fuse_then_rc(tmpdir, f90, f90flags):
    '''Test that we are able to fuse two loops together, perform
    redundant computation and then colour.'''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "4_multikernel_invokes.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.get('invoke_0')
    schedule = invoke.schedule

    ftrans = DynamoLoopFuseTrans()

    # fuse the loops
    schedule, _ = ftrans.apply(schedule.children[3],
                               schedule.children[4])

    # create our redundant computation transformation
    rc_trans = Dynamo0p3RedundantComputationTrans()

    # apply redundant computation to the loop
    schedule, _ = rc_trans.apply(schedule.children[3])

    # create our colour transformation
    ctrans = Dynamo0p3ColourTrans()

    # Colour the loop
    schedule, _ = ctrans.apply(schedule.children[3])

    psy.invokes.invoke_list[0].schedule = schedule

    result = str(psy.gen)

    assert (
        "      IF (f2_proxy%is_dirty(depth=mesh%get_halo_depth())) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=mesh%get_halo_depth())\n"
        "      END IF \n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=mesh%get_halo_depth())) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=mesh%get_halo_depth())\n"
        "      END IF \n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=mesh%get_halo_depth())) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=mesh%get_halo_depth())\n"
        "      END IF \n" in result)
    assert "      cmap => mesh%get_colour_map()\n" in result
    assert (
        "      DO colour=1,ncolour\n"
        "        DO cell=1,mesh%get_last_halo_cell_per_colour(colour)\n"
        in result)

    assert (
        "      CALL f1_proxy%set_dirty()\n"
        "      CALL f1_proxy%set_clean(mesh%get_halo_depth()-1)" in result)

    if TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag
        # to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_haloex_colouring(tmpdir, f90, f90flags, monkeypatch, annexed):
    '''Check that the halo exchange logic for halo exchanges between loops
    works when we colour the loops. We also test when annexed is False
    and True as it affects how many halo exchanges are generated.

    '''

    def check_halo_exchange(halo_exchange):
        '''internal function to check the validity of a halo exchange for
        field f1 which is guaranteed (has no runtime logic to
        determine whether it is needed or not) and is of depth 1.

        '''
        # check halo exchange has the expected values
        assert halo_exchange.field.name == "f1"
        assert halo_exchange._compute_stencil_type() == "region"
        assert halo_exchange._compute_halo_depth() == "1"
        assert halo_exchange.required() == (True, True)
        # check that the write_access information (information based on
        # the previous writer) has been computed correctly
        write_access = halo_exchange._compute_halo_write_info()
        assert write_access.set_by_value
        assert not write_access.var_depth
        assert not write_access.max_depth
        assert write_access.literal_depth == 1
        assert write_access.dirty_outer
        assert not write_access.annexed_only
        # check that the read_access information is correct
        depth_info_list = halo_exchange._compute_halo_read_depth_info()
        assert len(depth_info_list) == 1
        depth_info = depth_info_list[0]
        assert not depth_info.annexed_only
        assert depth_info.literal_depth == 1
        assert not depth_info.max_depth
        assert not depth_info.var_depth

    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    if annexed:
        w_loop_idx = 1
        r_loop_idx = 3
        halo_idx = 2
    else:
        w_loop_idx = 2
        r_loop_idx = 5
        halo_idx = 4
    ctrans = Dynamo0p3ColourTrans()

    # Begin with a loop which modifies the continuous field f1
    # followed by a loop which modifies the continuous field f3 and
    # reads field f1. This will produce a guaranteed halo exchange of
    # depth 1 for field f1. Next, check that loop colouring the first
    # loop makes no difference to the halo exchange.  Next, check that
    # loop colouring the first and second loops makes no difference to
    # the halo exchange.  Finally, check that loop colouring just the
    # second loop makes no difference to the halo exchange.
    for idx, cloop_idxs in enumerate([[], [r_loop_idx],
                                      [r_loop_idx, w_loop_idx], [w_loop_idx]]):

        _, invoke_info = parse(os.path.join(
            BASE_PATH, "14.10_halo_continuous_cell_w_to_r.f90"),
                               api="dynamo0.3")
        psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule

        for cloop_idx in cloop_idxs:
            schedule, _ = ctrans.apply(schedule.children[cloop_idx])

        invoke.schedule = schedule
        halo_exchange = schedule.children[halo_idx]
        check_halo_exchange(halo_exchange)

        if TEST_COMPILE:
            # If compilation testing has been enabled (--compile flag
            # to py.test)
            assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)

        print("OK for iteration ", idx)


def test_haloex_rc1_colouring(tmpdir, f90, f90flags, monkeypatch, annexed):
    '''Check that the halo exchange logic for halo exchanges between loops
    works when we colour the loops and apply redundant computation to
    the maximum depth for the reader. We first check the halo exchange
    properties are correct with no colouring then apply colouring to
    the first loop, then the second and finally both. In each case we
    check that the halo exchange properties do not change. We expect
    to see a definite (no runtime check) halo exchange to the maximum
    halo depth. We also test when annexed is False and True as it
    affects how many halo exchanges are generated.

    '''

    def check_halo_exchange(halo_exchange):
        '''Internal method to check the validity of a halo exchange for field
        f1 which is guaranteed (has no runtime logic to determine
        whether it is needed or not) and is to the full depth of the
        halo.

        '''
        # check halo exchange has the expected values
        assert halo_exchange.field.name == "f1"
        assert halo_exchange._compute_stencil_type() == "region"
        assert halo_exchange._compute_halo_depth() == "mesh%get_halo_depth()"
        assert halo_exchange.required
        # check that the write_access information (information based on
        # the previous writer) has been computed correctly
        write_access = halo_exchange._compute_halo_write_info()
        assert write_access.set_by_value
        assert not write_access.var_depth
        assert not write_access.max_depth
        assert write_access.literal_depth == 1
        assert write_access.dirty_outer
        assert not write_access.annexed_only
        # check that the read_access information is correct
        depth_info_list = halo_exchange._compute_halo_read_depth_info()
        assert len(depth_info_list) == 1
        depth_info = depth_info_list[0]
        assert not depth_info.annexed_only
        assert not depth_info.literal_depth
        assert depth_info.max_depth
        assert not depth_info.var_depth

    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)

    if annexed:
        w_loop_idx = 1
        r_loop_idx = 4
    else:
        w_loop_idx = 2
        r_loop_idx = 5
    ctrans = Dynamo0p3ColourTrans()
    rc_trans = Dynamo0p3RedundantComputationTrans()

    # Begin with a loop which modifies the continuous field f1
    # followed by a loop which modifies the continuous field f3 to the
    # maximum depth and reads field f1. This will produce a guaranteed
    # halo exchange of maximum depth for field f1. Next, check that
    # loop colouring the first loop makes no difference to the halo
    # exchange. Next, check that loop colouring the first and second
    # loops makes no difference to the halo exchange. Finally, check
    # that loop colouring just the second loop makes no difference to
    # the halo exchange.
    for idx, cloop_idxs in enumerate([[], [r_loop_idx],
                                      [w_loop_idx, r_loop_idx], [w_loop_idx]]):

        _, invoke_info = parse(os.path.join(
            BASE_PATH, "14.10_halo_continuous_cell_w_to_r.f90"),
                               api="dynamo0.3")
        psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)

        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule

        if annexed:
            index = 3
        else:
            index = 5
        schedule, _ = rc_trans.apply(schedule.children[index])

        for cloop_idx in cloop_idxs:
            schedule, _ = ctrans.apply(schedule.children[cloop_idx])

        invoke.schedule = schedule
        if annexed:
            halo_exchange = schedule.children[2]
        else:
            halo_exchange = schedule.children[4]
        check_halo_exchange(halo_exchange)

        if TEST_COMPILE:
            # If compilation testing has been enabled (--compile flag
            # to py.test)
            assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)

        print("OK for iteration ", idx)


def test_haloex_rc2_colouring(tmpdir, f90, f90flags, monkeypatch, annexed):
    '''Check that the halo exchange logic for halo exchanges between loops
    works when we colour the loops and apply redundant computation to
    the maximum depth for the writer. We first check the halo exchange
    properties are correct with no colouring then apply colouring to
    the first loop, then the second and finally both. In each case we
    check that the halo exchange properties do not change. We expect
    to see a potential (runtime check) halo exchange of depth 1. This
    is because we do not know the depth of the halo and the writer
    ends up with its outermost halo-depth dirty. So, if the maximum
    depth of the halo is 1 then we need a halo exchange but if it is 2
    or more we do not. We also test when annexed is False and True
    as it affects how many halo exchanges are generated.

    '''

    def check_halo_exchange(halo_exchange):
        '''Internal method to check the validity of a potential (has a runtime
        check) halo exchange for field f1 of depth 1.

        '''
        # check halo exchange has the expected values
        assert halo_exchange.field.name == "f1"
        assert halo_exchange._compute_stencil_type() == "region"
        assert halo_exchange._compute_halo_depth() == "1"
        assert halo_exchange.required() == (True, False)
        # check that the write_access information (information based on
        # the previous writer) has been computed correctly
        write_access = halo_exchange._compute_halo_write_info()
        assert write_access.set_by_value
        assert not write_access.var_depth
        assert write_access.max_depth
        assert not write_access.literal_depth
        assert write_access.dirty_outer
        assert not write_access.annexed_only
        # check that the read_access information is correct
        depth_info_list = halo_exchange._compute_halo_read_depth_info()
        assert len(depth_info_list) == 1
        depth_info = depth_info_list[0]
        assert not depth_info.annexed_only
        assert depth_info.literal_depth == 1
        assert not depth_info.max_depth
        assert not depth_info.var_depth

    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    w_loop_idx = 2
    if annexed:
        r_loop_idx = 4
    else:
        r_loop_idx = 5
    ctrans = Dynamo0p3ColourTrans()
    rc_trans = Dynamo0p3RedundantComputationTrans()

    # Begin with a loop which modifies the continuous field f1 to the
    # maximum depth followed by a loop which modifies the continuous
    # field f3 and reads field f1. This will produce a potential
    # (runtime check) halo exchange of depth 1 for field f1. Next,
    # check that loop colouring the first loop makes no difference to
    # the halo exchange. Next, check that loop colouring the first and
    # second loops makes no difference to the halo exchange. Finally,
    # check that loop colouring just the second loop makes no
    # difference to the halo exchange.
    for idx, cloops in enumerate([[], [r_loop_idx], [r_loop_idx, w_loop_idx],
                                  [w_loop_idx]]):

        _, invoke_info = parse(os.path.join(
            BASE_PATH, "14.10_halo_continuous_cell_w_to_r.f90"),
                               api="dynamo0.3")
        psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)

        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule

        if annexed:
            index = 1
        else:
            index = w_loop_idx
        schedule, _ = rc_trans.apply(schedule.children[index])

        for cloop in cloops:
            schedule, _ = ctrans.apply(schedule.children[cloop])

        invoke.schedule = schedule
        if annexed:
            index = 3
        else:
            index = 4
        halo_exchange = schedule.children[index]
        check_halo_exchange(halo_exchange)

        if TEST_COMPILE:
            # If compilation testing has been enabled (--compile flag
            # to py.test)
            assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)

        print("OK for iteration ", idx)


def test_haloex_rc3_colouring(tmpdir, f90, f90flags, monkeypatch, annexed):
    '''Check that the halo exchange logic for halo exchanges between loops
    works when we colour the loops and apply redundant computation to
    the maximum depth for the writer and the reader. We first check
    the halo exchange properties are correct with no colouring then
    apply colouring to the first loop, then the second and finally
    both. In each case we check that the halo exchange properties do
    not change. We expect to see a definite (no runtime check) halo
    exchange to the maximum halo depth. We could halo exchange only
    the outermost halo depth but the LFRic API does not currently
    support this option. We also test when annexed is False and True
    as it affects how many halo exchanges are generated.'''

    def check_halo_exchange(halo_exchange):
        '''internal method to check the validity of a halo exchange for field
        f1 which is guaranteed (has no runtime logic to determine
        whether it is needed or not) and is to the full depth of the
        halo.

        '''
        # check halo exchange has the expected values
        assert halo_exchange.field.name == "f1"
        assert halo_exchange._compute_stencil_type() == "region"
        assert halo_exchange._compute_halo_depth() == "mesh%get_halo_depth()"
        assert halo_exchange.required() == (True, True)
        # check that the write_access information (information based on
        # the previous writer) has been computed correctly
        write_access = halo_exchange._compute_halo_write_info()
        assert write_access.set_by_value
        assert not write_access.var_depth
        assert write_access.max_depth
        assert not write_access.literal_depth
        assert write_access.dirty_outer
        assert not write_access.annexed_only
        # check that the read_access information is correct
        depth_info_list = halo_exchange._compute_halo_read_depth_info()
        assert len(depth_info_list) == 1
        depth_info = depth_info_list[0]
        assert not depth_info.annexed_only
        assert not depth_info.literal_depth
        assert depth_info.max_depth
        assert not depth_info.var_depth

    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    w_loop_idx = 2
    r_loop_idx = 5
    ctrans = Dynamo0p3ColourTrans()
    rc_trans = Dynamo0p3RedundantComputationTrans()

    # Begin with a loop which modifies the continuous field f1 to the
    # maximum depth followed by a loop which modifies the continuous
    # field f3 to the maximum depth and reads field f1. This will
    # produce a guaranteed halo exchange of maximum depth for field
    # f1. Next, check that loop colouring the first loop makes no
    # difference to the halo exchange. Next, check that loop colouring
    # the first and second loops makes no difference to the halo
    # exchange. Finally, check that loop colouring just the second
    # loop makes no difference to the halo exchange.
    for idx, cloop_idxs in enumerate([[], [r_loop_idx],
                                      [r_loop_idx, w_loop_idx], [w_loop_idx]]):
        _, invoke_info = parse(
            os.path.join(BASE_PATH, "14.10_halo_continuous_cell_w_to_r.f90"),
            api="dynamo0.3")
        psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)

        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule

        if annexed:
            index1 = 1
            index2 = 4
        else:
            index1 = w_loop_idx
            index2 = r_loop_idx
        schedule, _ = rc_trans.apply(schedule.children[index1])
        schedule, _ = rc_trans.apply(schedule.children[index2])

        for cloop_idx in cloop_idxs:
            schedule, _ = ctrans.apply(schedule.children[cloop_idx])

        invoke.schedule = schedule
        if annexed:
            index = 3
        else:
            index = 4
        halo_exchange = schedule.children[index]
        check_halo_exchange(halo_exchange)

        if TEST_COMPILE:
            # If compilation testing has been enabled (--compile flag
            # to py.test)
            assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)

        print("OK for iteration ", idx)


def test_haloex_rc4_colouring(tmpdir, f90, f90flags, monkeypatch, annexed):
    '''Check that the halo exchange logic for halo exchanges between loops
    works when we colour the loops and apply redundant computation to
    depth 2 for the writer. We first check a halo exchange is not
    generated. We then apply colouring to the first loop, then the
    second and finally both. In each case we check that a halo
    exchange is not generated. We also test when annexed is False and
    True as it affects how many halo exchanges are generated.

    '''

    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    # At the start we have two halo exchange calls for field f1, one
    # before the first loop and one between the two loops when annexed
    # is False, and just the latter halo exchange when annexed is True
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "14.10_halo_continuous_cell_w_to_r.f90"), api="dynamo0.3")
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    result = str(psy.gen)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    from psyclone.dynamo0p3 import DynHaloExchange
    if annexed:
        assert result.count("f1_proxy%halo_exchange(depth=1)") == 1
        assert isinstance(schedule.children[2], DynHaloExchange)
        assert schedule.children[2].field.name == "f1"
    else:
        assert result.count("f1_proxy%halo_exchange(depth=1)") == 2
        assert isinstance(schedule.children[0], DynHaloExchange)
        assert schedule.children[0].field.name == "f1"
        assert isinstance(schedule.children[4], DynHaloExchange)
        assert schedule.children[4].field.name == "f1"

    w_loop_idx = 2
    if annexed:
        r_loop_idx = 3
    else:
        r_loop_idx = 4
    ctrans = Dynamo0p3ColourTrans()
    rc_trans = Dynamo0p3RedundantComputationTrans()

    # We then apply redundant computation so that the loop which
    # modifies the continuous field f1 does so to depth=2. The code
    # will no longer produce a halo exchange between the two loops for
    # field f1. We will therefore only have one halo exchange for
    # field f1 (before the first loop). Next, check that loop
    # colouring the first loop makes no difference to the halo
    # exchange. Next, check that loop colouring the first and second
    # loops makes no difference to the halo exchange. Finally, check
    # that loop colouring just the second loop makes no difference to
    # the halo exchange.
    for idx, cloop_idxs in enumerate([[], [r_loop_idx],
                                      [r_loop_idx, w_loop_idx], [w_loop_idx]]):

        _, invoke_info = parse(os.path.join(
            BASE_PATH, "14.10_halo_continuous_cell_w_to_r.f90"),
                               api="dynamo0.3")
        psy = PSyFactory("dynamo0.3").create(invoke_info)
        result = str(psy.gen)

        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule

        if annexed:
            index = 1
        else:
            index = w_loop_idx

        schedule, _ = rc_trans.apply(schedule.children[index], depth=2)

        for cloop_idx in cloop_idxs:
            schedule, _ = ctrans.apply(schedule.children[cloop_idx])

        invoke.schedule = schedule
        result = str(psy.gen)

        # the redundant computation code has one halo exchange for field f1
        assert result.count("f1_proxy%halo_exchange(depth=1)") == 1
        if annexed:
            index = 1
        else:
            index = 0
        assert isinstance(schedule.children[index], DynHaloExchange)
        assert schedule.children[index].field.name == "f1"

        if TEST_COMPILE:
            # If compilation testing has been enabled (--compile flag
            # to py.test)
            assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)

        print("OK for iteration ", idx)


def test_intergrid_colour(dist_mem):
    ''' Check that we can apply colouring to a loop containing
    an inter-grid kernel. '''
    # Use an example that contains both prolongation and restriction
    # kernels
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "22.2_intergrid_3levels.f90"), api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    # First two kernels are prolongation, last two are restriction
    loops = schedule.walk(schedule.children, psyGen.Loop)
    ctrans = Dynamo0p3ColourTrans()
    # To a prolong kernel
    _, _ = ctrans.apply(loops[1])
    # To a restrict kernel
    _, _ = ctrans.apply(loops[3])
    gen = str(psy.gen).lower()
    expected = '''\
      ncolour_fld_m = mesh_fld_m%get_ncolours()
      cmap_fld_m => mesh_fld_m%get_colour_map()'''
    assert expected in gen
    expected = '''\
      ncolour_fld_c = mesh_fld_c%get_ncolours()
      cmap_fld_c => mesh_fld_c%get_colour_map()'''
    assert expected in gen
    if dist_mem:
        expected = (
            "      do colour=1,ncolour_fld_m\n"
            "        do cell=1,mesh_fld_m%get_last_halo_cell_per_colour("
            "colour,1)\n")
    else:
        expected = (
            "      do colour=1,ncolour_fld_m\n"
            "        do cell=1,mesh_fld_m%get_last_edge_cell_per_colour("
            "colour)\n")
    assert expected in gen
    expected = (
        "          call prolong_kernel_code(nlayers, cell_map_fld_m(:,"
        "cmap_fld_m(colour, cell)), ncpc_fld_f_fld_m, ncell_fld_f, "
        "fld_f_proxy%data, fld_m_proxy%data, ndf_w1, undf_w1, map_w1, "
        "undf_w2, map_w2(:,cmap_fld_m(colour, cell)))\n")
    assert expected in gen


def test_intergrid_colour_errors(dist_mem, monkeypatch):
    ''' Check that we raise the expected error when colouring is not applied
    correctly to inter-grid kernels within a loop over colours. '''
    ctrans = Dynamo0p3ColourTrans()
    # Use an example that contains both prolongation and restriction kernels
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "22.2_intergrid_3levels.f90"), api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    # First two kernels are prolongation, last two are restriction
    loops = schedule.walk(schedule.children, psyGen.Loop)
    loop = loops[1]
    # To a prolong kernel
    new_sched, _ = ctrans.apply(loop)
    # Update our list of loops
    loops = new_sched.walk(schedule.children, psyGen.Loop)
    # Trigger the error by calling the internal method to get the upper
    # bound before the colourmaps have been set-up
    with pytest.raises(InternalError) as err:
        _ = loops[1]._upper_bound_fortran()
    assert ("All kernels within a loop over colours must have been coloured "
            "but kernel 'prolong_kernel_code' has not" in str(err))
    # Set-up the colourmaps
    psy.invokes.invoke_list[0].meshes._colourmap_init()
    # Check that the upper bound is now correct
    upperbound = loops[1]._upper_bound_fortran()
    assert upperbound == "ncolour_fld_m"
    # Manually add an un-coloured kernel to the loop that we coloured
    loop = loops[2]
    monkeypatch.setattr(loops[3].children[0], "is_coloured", lambda: True)
    loop.children.append(loops[3].children[0])
    with pytest.raises(InternalError) as err:
        _ = loops[1]._upper_bound_fortran()
    assert ("All kernels within a loop over colours must have been coloured "
            "but kernel 'restrict_kernel_code' has not" in str(err))


def test_intergrid_omp_parado(dist_mem, tmpdir, f90, f90flags):
    '''Check that we can add an OpenMP parallel loop to a loop containing
    an inter-grid kernel call.

    '''
    # Use an example that contains both prolongation and restriction
    # kernels
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "22.2_intergrid_3levels.f90"), api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    # First two kernels are prolongation, last two are restriction
    loops = schedule.walk(schedule.children, psyGen.Loop)
    ctrans = Dynamo0p3ColourTrans()
    # To a prolong kernel
    _, _ = ctrans.apply(loops[1])
    # To a restrict kernel
    _, _ = ctrans.apply(loops[3])
    loops = schedule.walk(schedule.children, psyGen.Loop)
    otrans = DynamoOMPParallelLoopTrans()
    # Apply OMP to loops over coloured cells
    _, _ = otrans.apply(loops[2])
    _, _ = otrans.apply(loops[5])
    gen = str(psy.gen)
    assert ("      DO colour=1,ncolour_fld_c\n"
            "        !$omp parallel do default(shared), private(cell), "
            "schedule(static)\n" in gen)
    if dist_mem:
        assert ("        DO cell=1,mesh_fld_c%get_last_halo_cell_per_colour("
                "colour,1)\n" in gen)
    else:
        assert ("        DO cell=1,mesh_fld_c%get_last_edge_cell_per_colour("
                "colour)\n" in gen)
    if TEST_COMPILE:
        assert code_compiles(TEST_API, psy, tmpdir, f90, f90flags)


def test_intergrid_omp_para_region1(dist_mem, tmpdir, f90, f90flags):
    ''' Check that we can create an OpenMP-parallel region containing
    a single inter-grid kernel call. '''
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "22.2_intergrid_3levels.f90"), api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    # Get the various transformations we need
    ctrans = Dynamo0p3ColourTrans()
    ptrans = OMPParallelTrans()
    otrans = Dynamo0p3OMPLoopTrans()
    # Colour the first loop
    loops = schedule.walk(schedule.children, psyGen.Loop)
    _, _ = ctrans.apply(loops[0])
    # Parallelise the loop over cells of a given colour
    loops = schedule.walk(schedule.children, psyGen.Loop)
    _, _ = otrans.apply(loops[1])
    # Put the parallel loop inside a parallel region
    dirs = schedule.walk(schedule.children, psyGen.Directive)
    _, _ = ptrans.apply(dirs[0])
    gen = str(psy.gen)
    if dist_mem:
        upper_bound = "mesh_fld_c%get_last_halo_cell_per_colour(colour,1)"
    else:
        upper_bound = "mesh_fld_c%get_last_edge_cell_per_colour(colour)"
    assert ("      DO colour=1,ncolour_fld_m\n"
            "        !$omp parallel default(shared), private(cell)\n"
            "        !$omp do schedule(static)\n"
            "        DO cell=1,{0}\n"
            "          !\n"
            "          CALL prolong_kernel_code(nlayers, cell_map_fld_c(:,"
            "cmap_fld_m(colour, cell)), ncpc_fld_m_fld_c, ncell_fld_m, "
            "fld_m_proxy%data, fld_c_proxy%data, ndf_w1, undf_w1, map_w1, "
            "undf_w2, map_w2(:,cmap_fld_m(colour, cell)))\n"
            "        END DO \n"
            "        !$omp end do\n"
            "        !$omp end parallel\n"
            "      END DO \n".format(upper_bound) in gen)
    if TEST_COMPILE:
        assert code_compiles(TEST_API, psy, tmpdir, f90, f90flags)


@pytest.mark.xfail(reason="Loop-fusion not yet supported for inter-grid "
                   "kernels")
def test_intergrid_omp_para_region2(dist_mem, tmpdir, f90, f90flags):
    ''' Check that we can create an OpenMP-parallel region containing
    multiple inter-grid kernels. '''
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "22.2_intergrid_3levels.f90"), api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    schedule.view()
    loops = schedule.walk(schedule.children, psyGen.Loop)
    ctrans = Dynamo0p3ColourTrans()
    ftrans = DynamoLoopFuseTrans()
    _, _ = ctrans.apply(loops[0])
    _, _ = ctrans.apply(loops[1])
    schedule.view()
    loops = schedule.walk(schedule.children, psyGen.Loop)
    _, _ = ftrans.apply(loops[0], loops[2])
    schedule.view()
    if TEST_COMPILE:
        assert code_compiles(TEST_API, psy, tmpdir, f90, f90flags)


def test_intergrid_err(dist_mem):
    ''' Check that we cannot apply redundant computation or loop
    fusion to loops containing inter-grid kernels. '''
    # Use an example that contains both prolongation and restriction
    # kernels
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "22.2_intergrid_3levels.f90"), api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    # First two kernels are prolongation, last two are restriction
    loops = schedule.walk(schedule.children, psyGen.Loop)

    expected_err = (
        "cannot currently be applied to nodes which have inter-grid "
        "kernels as children and ")

    if dist_mem:
        # Cannot apply redundant computation unless DM is enabled
        rc_trans = Dynamo0p3RedundantComputationTrans()
        with pytest.raises(TransformationError) as excinfo:
            rc_trans.apply(loops[2], depth=2)
            assert expected_err in str(excinfo)

    lftrans = DynamoLoopFuseTrans()
    with pytest.raises(TransformationError) as excinfo:
        lftrans.apply(loops[0], loops[1])
    assert expected_err in str(excinfo)


def test_no_acc():
    '''
    Check that attempting to add any sort of OpenACC directive to a
    dynamo0p3 Schedule causes an error.

    '''
    from psyclone.transformations import ACCDataTrans, ACCLoopTrans, \
        ACCParallelTrans
    accdt = ACCDataTrans()
    accpt = ACCParallelTrans()
    acclt = ACCLoopTrans()

    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(info)
    sched = psy.invokes.get('invoke_0_testkern_type').schedule

    with pytest.raises(NotImplementedError) as err:
        _ = accdt.apply(sched)
    assert ("ACCDataDirective not implemented for a schedule of type "
            in str(err))

    with pytest.raises(NotImplementedError) as err:
        _ = accpt.apply(sched.children)
    assert ("OpenACC parallel regions are currently only supported for "
            "the gocean 1.0 API" in str(err))

    with pytest.raises(NotImplementedError) as err:
        _ = acclt.apply(sched.children[0])
    assert ("OpenACC loop transformations are currently only supported for "
            "the gocean 1.0 API" in str(err))


def test_async_hex_wrong_node():
    '''Test that we raise the expected exception if an asynchronous halo
    exchange transformation is applied to a node that is not a halo
    exchange.

    '''
    from psyclone.psyGen import Loop
    node = Loop()
    ahex = Dynamo0p3AsyncHaloExchangeTrans()
    with pytest.raises(TransformationError) as err:
        _, _ = ahex.apply(node)
    assert "node must be a synchronous halo exchange" in str(err.value)


def test_async_hex_name():
    ''' Name test for the Dynamo0p3AsyncHaloExchangeTrans class. '''
    ahex = Dynamo0p3AsyncHaloExchangeTrans()
    assert ahex.name == "Dynamo0p3AsyncHaloExchangeTrans"


def test_async_hex_str():
    ''' String test for the Dynamo0p3AsyncHaloExchangeTrans class. '''
    ahex = Dynamo0p3AsyncHaloExchangeTrans()
    assert (str(ahex) == "Changes a synchronous halo exchange into an "
            "asynchronous one.")


def test_async_hex(tmpdir, f90, f90flags):
    '''Test that we can convert a synchronous halo exchange to an
    asynchronous one using the Dynamo0p3AsyncHaloExchangeTrans transformation.

    '''
    _, invoke_info = parse(os.path.join(
        os.path.dirname(os.path.abspath(__file__)),
        "test_files", "dynamo0p3",
        "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    f2_hex = schedule.children[0]
    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    schedule, _ = ahex_trans.apply(f2_hex)
    result = str(psy.gen)
    assert (
        "      ! Call kernels and communication routines\n"
        "      !\n"
        "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy%halo_exchange_start(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy%halo_exchange_finish(depth=1)\n"
        "      END IF \n"
        "      !\n") in result

    if TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag
        # to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_async_hex_move_1(tmpdir, f90, f90flags):
    '''Test that we can convert a synchronous halo exchange to an
    asynchronous one using the Dynamo0p3AsyncHaloExchangeTrans
    transformation and then move them to new valid locations. In this
    case we move them before and after other halo exchanges
    respectively.

    '''
    _, invoke_info = parse(os.path.join(
        os.path.dirname(os.path.abspath(__file__)),
        "test_files", "dynamo0p3",
        "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    m1_hex = schedule.children[1]
    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    schedule, _ = ahex_trans.apply(m1_hex)

    mtrans = MoveTrans()
    schedule, _ = mtrans.apply(schedule.children[1],
                               schedule.children[0])
    schedule, _ = mtrans.apply(schedule.children[3],
                               schedule.children[2])
    result = str(psy.gen)
    assert (
        "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m1_proxy%halo_exchange_start(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=1)\n"
        "      END IF \n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m1_proxy%halo_exchange_finish(depth=1)\n"
        "      END IF \n") in result

    if TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag
        # to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_async_hex_preserve_properties():
    '''Test that an asynchronous halo exchange created by the
    Dynamo0p3AsyncHaloExchangeTrans transformation maintains the properties
    of the original halo exchange.

    '''
    _, invoke_info = parse(os.path.join(
        os.path.dirname(os.path.abspath(__file__)),
        "test_files", "dynamo0p3",
        "4.3_multikernel_invokes.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    # We don't need this halo exchange
    f2_hex = schedule.children[0]
    _, known = f2_hex.required()
    field_name = f2_hex.field.name
    stencil_type = f2_hex._compute_stencil_type()
    halo_depth = f2_hex._compute_halo_depth()

    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    schedule, _ = ahex_trans.apply(f2_hex)
    f2_async_hex_start = schedule.children[0]

    _, f2_async_start_known = f2_async_hex_start.required()
    assert f2_async_start_known == known
    assert f2_async_hex_start.field.name == field_name
    assert f2_async_hex_start._compute_stencil_type() == stencil_type
    assert f2_async_hex_start._compute_halo_depth() == halo_depth

    f2_async_hex_end = schedule.children[1]
    _, f2_async_end_known = f2_async_hex_end.required()
    assert f2_async_end_known == known
    assert f2_async_hex_end.field.name == field_name
    assert f2_async_hex_end._compute_stencil_type() == stencil_type
    assert f2_async_hex_end._compute_halo_depth() == halo_depth

    # we do need this halo exchange
    f1_hex = schedule.children[6]
    _, known = f1_hex.required()
    field_name = f1_hex.field.name
    stencil_type = f1_hex._compute_stencil_type()
    halo_depth = f1_hex._compute_halo_depth()

    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    schedule, _ = ahex_trans.apply(f1_hex)

    f1_async_hex_start = schedule.children[6]
    _, f1_async_start_known = f1_async_hex_start.required()
    assert f1_async_start_known == known
    assert f1_async_hex_start.field.name == field_name
    assert f1_async_hex_start._compute_stencil_type() == stencil_type
    assert f1_async_hex_start._compute_halo_depth() == halo_depth

    f1_async_hex_end = schedule.children[7]
    _, f1_async_end_known = f1_async_hex_end.required()
    assert f1_async_end_known == known
    assert f1_async_hex_end.field.name == field_name
    assert f1_async_hex_end._compute_stencil_type() == stencil_type
    assert f1_async_hex_end._compute_halo_depth() == halo_depth


def test_async_hex_move_2(tmpdir, f90, f90flags, monkeypatch):
    '''Test that we can convert a synchronous halo exchange to an
    asynchronous one using the Dynamo0p3AsyncHaloExchangeTrans
    transformation and then move them to new valid locations. In this
    case we move a haloexchangestart before a loop.

    '''

    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", False)
    _, invoke_info = parse(os.path.join(
        os.path.dirname(os.path.abspath(__file__)),
        "test_files", "dynamo0p3",
        "4.5.2_multikernel_invokes.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    f2_hex = schedule.children[10]
    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    schedule, _ = ahex_trans.apply(f2_hex)

    mtrans = MoveTrans()
    schedule, _ = mtrans.apply(schedule.children[10],
                               schedule.children[9])
    result = str(psy.gen)
    assert (
        "      CALL f2_proxy%halo_exchange_start(depth=1)\n"
        "      !\n"
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        CALL testkern_any_space_3_code(cell, nlayers, "
        "op_proxy%ncell_3d, op_proxy%local_stencil, ndf_any_space_1_op, "
        "ndf_any_space_2_op)\n"
        "      END DO \n"
        "      CALL f2_proxy%halo_exchange_finish(depth=1)\n") in result

    if TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag
        # to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_async_hex_move_error_1():
    '''Test that an asynchronous halo exchange start can not be moved
    after its end and its end cannot be moved before its start.

    '''
    _, invoke_info = parse(os.path.join(
        os.path.dirname(os.path.abspath(__file__)),
        "test_files", "dynamo0p3",
        "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    m1_hex = schedule.children[1]
    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    schedule, _ = ahex_trans.apply(m1_hex)

    mtrans = MoveTrans()

    # end before start
    with pytest.raises(TransformationError) as excinfo:
        schedule, _ = mtrans.apply(schedule.children[2],
                                   schedule.children[1])
    assert "dependencies forbid" in str(excinfo.value)

    # start after end
    with pytest.raises(TransformationError) as excinfo:
        schedule, _ = mtrans.apply(schedule.children[1],
                                   schedule.children[3])
    assert "dependencies forbid" in str(excinfo.value)


def test_async_hex_move_error_2():
    '''Test that an asynchronous halo exchange start can not be moved
    before a kernel that modifies the field and its end cannot be
    moved after a kernel that reads the field.

    '''
    _, invoke_info = parse(os.path.join(
        os.path.dirname(os.path.abspath(__file__)),
        "test_files", "dynamo0p3",
        "4.3_multikernel_invokes.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    f1_hex = schedule.children[5]
    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    schedule, _ = ahex_trans.apply(f1_hex)

    mtrans = MoveTrans()

    # start before prev modifier
    with pytest.raises(TransformationError) as excinfo:
        schedule, _ = mtrans.apply(schedule.children[5],
                                   schedule.children[4])
    assert "dependencies forbid" in str(excinfo.value)

    # end after following reader
    with pytest.raises(TransformationError) as excinfo:
        schedule, _ = mtrans.apply(schedule.children[6],
                                   schedule.children[7],
                                   position="after")
    assert "dependencies forbid" in str(excinfo.value)


def test_rc_remove_async_halo_exchange(monkeypatch, tmpdir, f90, f90flags):
    '''Test that an asynchronous halo exchange is removed if redundant
    computation means that it is no longer required. Halo exchanges
    are not required in this example when we compute annexed
    dofs. Therefore we ensure we compute over owned dofs (via
    monkeypatch) to perform the test.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", False)
    _, info = parse(os.path.join(
        BASE_PATH, "14.7_halo_annexed.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule

    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()

    f2_hex = schedule.children[3]
    schedule, _ = ahex_trans.apply(f2_hex)
    f1_hex = schedule.children[2]
    schedule, _ = ahex_trans.apply(f1_hex)

    result = str(psy.gen)
    assert "CALL f1_proxy%halo_exchange_start(depth=1)" in result
    assert "CALL f1_proxy%halo_exchange_finish(depth=1)" in result
    assert "CALL f2_proxy%halo_exchange_start(depth=1)" in result
    assert "CALL f2_proxy%halo_exchange_finish(depth=1)" in result
    assert "IF (m1_proxy%is_dirty(depth=1)) THEN" in result
    assert "CALL m1_proxy%halo_exchange(depth=1)" in result

    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[0]
    rc_trans.apply(loop, depth=1)
    result = str(psy.gen)
    assert "CALL f1_proxy%halo_exchange_start(depth=1)" not in result
    assert "CALL f1_proxy%halo_exchange_finish(depth=1)" not in result
    assert "CALL f2_proxy%halo_exchange_start(depth=1)" in result
    assert "CALL f2_proxy%halo_exchange_finish(depth=1)" in result
    assert "IF (m1_proxy%is_dirty(depth=1)) THEN" in result
    assert "CALL m1_proxy%halo_exchange(depth=1)" in result
    #
    loop = schedule.children[1]
    rc_trans.apply(loop, depth=1)
    result = str(psy.gen)
    assert "CALL f1_proxy%halo_exchange_start(depth=1)" not in result
    assert "CALL f1_proxy%halo_exchange_finish(depth=1)" not in result
    assert "CALL f2_proxy%halo_exchange_start(depth=1)" not in result
    assert "CALL f2_proxy%halo_exchange_finish(depth=1)" not in result
    assert "IF (m1_proxy%is_dirty(depth=1)) THEN" in result
    assert "CALL m1_proxy%halo_exchange(depth=1)" in result

    if TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag
        # to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_rc_redund_async_halo_exchange(monkeypatch, tmpdir, f90, f90flags):
    '''Test that an asynchronous halo exchange works correctly with
    redundant computation being applied.
    '''

    # ensure we compute over annexed dofs so no halo exchanges are required
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", True)
    _, info = parse(os.path.join(
        BASE_PATH, "14.7_halo_annexed.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule

    # Make it so that halo exchanges are required to depth 2 for
    # fields m1, m2, f1 and f2. m2 will have a set clean for depth 1
    # after the last loop.
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[2]
    rc_trans.apply(loop, depth=2)

    # make m2 halo exchange asynchronous and check depths and set
    # clean are generated correctly for m2
    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    m2_hex = schedule.children[5]
    schedule, _ = ahex_trans.apply(m2_hex)
    result = str(psy.gen)
    assert (
        "      IF (m2_proxy%is_dirty(depth=2)) THEN\n"
        "        CALL m2_proxy%halo_exchange_start(depth=2)\n"
        "      END IF \n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=2)) THEN\n"
        "        CALL m2_proxy%halo_exchange_finish(depth=2)\n"
        "      END IF \n") in result
    assert (
        "      ! Set halos dirty/clean for fields modified in the above loop\n"
        "      !\n"
        "      CALL m2_proxy%set_dirty()\n"
        "      CALL m2_proxy%set_clean(2)\n") in result

    # move m2 async halo exchange start and end then check depths and
    # set clean are still generated correctly for m2
    mtrans = MoveTrans()
    schedule, _ = mtrans.apply(schedule.children[5],
                               schedule.children[0])
    schedule, _ = mtrans.apply(schedule.children[6],
                               schedule.children[2])
    result = str(psy.gen)
    assert (
        "      IF (m2_proxy%is_dirty(depth=2)) THEN\n"
        "        CALL m2_proxy%halo_exchange_start(depth=2)\n"
        "      END IF \n") in result
    assert (
        "      IF (m2_proxy%is_dirty(depth=2)) THEN\n"
        "        CALL m2_proxy%halo_exchange_finish(depth=2)\n"
        "      END IF \n") in result
    assert (
        "      ! Set halos dirty/clean for fields modified in the above loop\n"
        "      !\n"
        "      CALL m2_proxy%set_dirty()\n"
        "      CALL m2_proxy%set_clean(2)\n") in result

    # increase depth of redundant computation. We do this to all loops
    # to remove halo exchanges for f1 and f2 just because we can :-)
    # Check depths and set clean are still generated correctly for m2
    rc_trans = Dynamo0p3RedundantComputationTrans()
    for index in [7, 1, 3]:
        loop = schedule.children[index]
        rc_trans.apply(loop, depth=3)
    result = str(psy.gen)
    assert (
        "      IF (m2_proxy%is_dirty(depth=3)) THEN\n"
        "        CALL m2_proxy%halo_exchange_start(depth=3)\n"
        "      END IF \n") in result
    assert (
        "      IF (m2_proxy%is_dirty(depth=3)) THEN\n"
        "        CALL m2_proxy%halo_exchange_finish(depth=3)\n"
        "      END IF \n") in result
    assert (
        "      ! Set halos dirty/clean for fields modified in the above loop\n"
        "      !\n"
        "      CALL m2_proxy%set_dirty()\n"
        "      CALL m2_proxy%set_clean(3)\n") in result

    if TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag
        # to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


@pytest.mark.xfail(reason="dependence analysis thinks independent vectors "
                   "depend on each other")
def test_move_vector_halo_exchange():
    '''Test that halo exchanges for different vectors for the same field
    are independent of each other, i.e. they do not depend on
    each other.

    '''
    _, info = parse(os.path.join(
        BASE_PATH, "8.3_multikernel_invokes_vector.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    schedule.view()

    # reverse the order of the vector halo exchanges
    mtrans = MoveTrans()
    schedule, _ = mtrans.apply(schedule.children[1],
                               schedule.children[0])
    schedule, _ = mtrans.apply(schedule.children[2],
                               schedule.children[1])
    # When the test is fixed, add a check for re-ordered output here


def test_vector_halo_exchange_remove():
    '''test that we remove vector halo exchanges when they are no longer
    required.

    '''
    _, info = parse(os.path.join(
        BASE_PATH, "8.3_multikernel_invokes_vector.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule

    # remove second set of halo exchanges via redundant
    # computation. If they are removed correctly then the two loops
    # will be adjacent to each other and will follow 3 haloexchange
    # calls.
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[3], depth=2)
    assert len(schedule.children) == 5
    from psyclone.dynamo0p3 import DynHaloExchange, DynLoop
    for index in [0, 1, 2]:
        assert isinstance(schedule.children[index], DynHaloExchange)
    assert isinstance(schedule.children[3], DynLoop)
    assert isinstance(schedule.children[4], DynLoop)


def test_vector_async_halo_exchange(tmpdir, f90, f90flags):
    '''Test that an asynchronous halo exchange works correctly with
    vector fields.
    '''
    _, info = parse(os.path.join(
        BASE_PATH, "8.3_multikernel_invokes_vector.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule

    # Create vector halo exchanges after the first loop by performing
    # redundant computation.
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[4], depth=2)
    # make all f1 vector halo exchanges asynchronous before the first
    # loop and one of them before the second loop, then check depths
    # and set clean are still generated correctly
    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    for index in [5, 2, 1, 0]:
        hex = schedule.children[index]
        schedule, _ = ahex_trans.apply(hex)
    result = str(psy.gen)
    for index in [1, 2, 3]:
        assert (
            "      IF (f1_proxy({0})%is_dirty(depth=1)) THEN\n"
            "        CALL f1_proxy({0})%halo_exchange_start(depth=1)\n"
            "      END IF \n"
            "      !\n"
            "      IF (f1_proxy({0})%is_dirty(depth=1)) THEN\n"
            "        CALL f1_proxy({0})%halo_exchange_finish(depth=1)\n"
            "      END IF \n".format(index)) in result
    assert (
        "      CALL f1_proxy(1)%halo_exchange(depth=1)\n"
        "      !\n"
        "      CALL f1_proxy(2)%halo_exchange_start(depth=1)\n"
        "      !\n"
        "      CALL f1_proxy(2)%halo_exchange_finish(depth=1)\n"
        "      !\n"
        "      CALL f1_proxy(3)%halo_exchange(depth=1)\n") in result

    # we are not able to test re-ordering of vector halo exchanges as
    # the dependence analysis does not currently support it
    # mtrans = MoveTrans()
    # schedule, _ = mtrans.apply(schedule.children[2],
    #                            schedule.children[1])
    # schedule, _ = mtrans.apply(schedule.children[4],
    #                            schedule.children[2])

    # remove second set of halo exchanges via redundant
    # computation. If they are removed correctly then the two loops
    # will be adjacent to each other and will follow 6 haloexchange
    # start and end calls.
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[6], depth=2)
    schedule.view()
    from psyclone.dynamo0p3 import DynLoop, DynHaloExchangeStart, \
        DynHaloExchangeEnd
    assert len(schedule.children) == 8
    for index in [0, 2, 4]:
        assert isinstance(schedule.children[index], DynHaloExchangeStart)
        assert isinstance(schedule.children[index+1], DynHaloExchangeEnd)
    assert isinstance(schedule.children[6], DynLoop)
    assert isinstance(schedule.children[7], DynLoop)

    if TEST_COMPILE:
        # If compilation testing has been enabled (--compile flag
        # to py.test)
        assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_async_halo_exchange_nomatch1():
    '''Test that an exception is raised if an asynchronous halo exchange
    start matches with something other than the expected halo exchange
    end.

    '''
    _, info = parse(os.path.join(
        BASE_PATH, "8.3_multikernel_invokes_vector.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule

    # create vector halo exchanges after the first loop by performing
    # redundant computation
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[4], depth=2)

    # make the first vector component of the halo exchange for f1
    # asynchronous before the first loop.
    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    my_hex = schedule.children[0]
    schedule, _ = ahex_trans.apply(my_hex)

    # now remove the generated halo exchange end. This will mean that
    # the halo exchange start will now match with the halo exchange
    # for the first vector component after the loop (which is a
    # standard halo exchange). This should cause an exception to be
    # raised.
    del(schedule.children[1])

    hex_start = schedule.children[0]
    with pytest.raises(GenerationError) as excinfo:
        _ = hex_start._get_hex_end()
    assert ("Halo exchange start for field 'f1' should match with a halo "
            "exchange end, but found <class 'psyclone.dynamo0p3."
            "DynHaloExchange'>") in str(excinfo.value)


def test_async_halo_exchange_nomatch2():
    '''Test that an exception is raised if an asynchronous halo exchange
    start matches with no other halo exchange with the same name.

    '''
    _, info = parse(os.path.join(
        BASE_PATH, "8.3_multikernel_invokes_vector.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule

    # make the last vector component of the halo exchange for f1
    # asynchronous after the first loop.
    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    my_hex = schedule.children[0]
    schedule, _ = ahex_trans.apply(my_hex)

    # now remove the generated halo exchange end. This will mean that
    # the halo exchange start will now match with nothing as it is the
    # last halo exchange in the schedule. This should cause an
    # exception to be raised.
    del(schedule.children[1])

    hex_start = schedule.children[0]
    with pytest.raises(GenerationError) as excinfo:
        _ = hex_start._get_hex_end()
    assert ("Halo exchange start for field 'f1' has no matching halo "
            "exchange end") in str(excinfo.value)
