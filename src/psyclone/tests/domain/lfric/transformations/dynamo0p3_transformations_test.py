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
# -----------------------------------------------------------------------------
# Authors: R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
# Modified: I. Kavcic, A. Coughtrie, O. Brunt and L. Turner, Met Office
#          C.M. Maynard, Met Office / University of Reading
# Modified: J. Henrichs, Bureau of Meteorology
# Modified: A. B. G. Chalk, STFC Daresbury Lab

''' Tests of transformations with the LFRic (Dynamo 0.3) API '''

import inspect
from importlib import import_module
import pytest

from psyclone.configuration import Config
from psyclone.core.access_type import AccessType
from psyclone.domain.lfric.lfric_builtins import LFRicXInnerproductYKern
from psyclone.domain.lfric.transformations import LFRicLoopFuseTrans
from psyclone.domain.lfric import LFRicLoop
from psyclone.dynamo0p3 import (LFRicHaloExchangeStart, LFRicHaloExchangeEnd,
                                LFRicHaloExchange)
from psyclone.errors import GenerationError, InternalError
from psyclone.psyGen import InvokeSchedule, GlobalSum, BuiltIn
from psyclone.psyir.nodes import (colored, Loop, Schedule, Literal, Directive,
                                  OMPDoDirective, ACCEnterDataDirective)
from psyclone.psyir.symbols import (AutomaticInterface, ScalarType, ArrayType,
                                    REAL_TYPE, INTEGER_TYPE)
from psyclone.psyir.transformations import (LoopFuseTrans, LoopTrans,
                                            TransformationError)
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import OMPParallelTrans, \
    Dynamo0p3ColourTrans, \
    Dynamo0p3OMPLoopTrans, \
    DynamoOMPParallelLoopTrans, \
    MoveTrans, \
    Dynamo0p3RedundantComputationTrans, \
    Dynamo0p3AsyncHaloExchangeTrans, \
    Dynamo0p3KernelConstTrans, \
    ACCLoopTrans, ACCParallelTrans, ACCKernelsTrans, ACCEnterDataTrans


# The version of the API that the tests in this file
# exercise.
TEST_API = "dynamo0.3"


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use dynamo0.3 as API.'''
    Config.get().api = "dynamo0.3"
    yield
    Config._instance = None


def test_colour_trans_create_colours_loop(dist_mem):
    '''
    Test the '_create_colours_loop()' method of Dynamo0p3ColourTrans.
    We test with and without distributed memory and for the case where
    the kernel has a 'GH_WRITE' access to a continuous field. (The latter
    is a special case as it does not require a halo access.)

    '''
    _, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                           name="invoke_0_testkern_type", dist_mem=dist_mem)
    schedule = invoke.schedule
    ctrans = Dynamo0p3ColourTrans()
    loop = schedule.walk(Loop)[0]
    kernel = loop.loop_body[0]

    new_loop = ctrans._create_colours_loop(loop)
    assert isinstance(new_loop, LFRicLoop)
    assert new_loop.loop_type == "colours"
    colour_loop = new_loop.loop_body[0]
    assert isinstance(colour_loop, LFRicLoop)
    assert colour_loop.loop_type == "colour"
    assert new_loop.field_space == loop.field_space
    assert colour_loop.field_space == loop.field_space
    assert colour_loop.field_name == loop.field_name
    assert new_loop._lower_bound_name == "start"
    assert new_loop._upper_bound_name == "ncolours"
    assert colour_loop._lower_bound_name == "start"
    if dist_mem:
        # Since the kernel has a GH_INC access we need to loop into the
        # halo.
        assert colour_loop._upper_bound_name == "colour_halo"
    else:
        assert colour_loop._upper_bound_name == "ncolour"

    # Modify one GH_INC to be GH_WRITE. Since there is still a GH_INC this
    # should have no effect - we still need to loop into the halo if DM
    # is enabled.
    kernel.args[2]._access = AccessType.WRITE
    new_loop1 = LFRicLoop(parent=schedule)
    new_loop1.load(kernel)
    new_cloop = ctrans._create_colours_loop(new_loop1)
    colour_loop = new_cloop.loop_body[0]
    if dist_mem:
        assert colour_loop._upper_bound_name == "colour_halo"
    else:
        assert colour_loop._upper_bound_name == "ncolour"

    # Finally, change the remaining GH_INC access to be GH_WRITE. We no
    # longer need to loop into the halo.
    kernel.args[1]._access = AccessType.WRITE
    new_loop1 = LFRicLoop(parent=schedule)
    new_loop1.load(kernel)
    new_cloop = ctrans._create_colours_loop(new_loop1)
    colour_loop = new_cloop.loop_body[0]
    assert colour_loop._upper_bound_name == "ncolour"


def test_colour_trans_declarations(tmpdir, dist_mem):
    ''' Check that we generate the correct variable declarations when
    doing a colouring transformation. We check when distributed memory
    is both off and on.

    '''
    # Test of the colouring transformation of a single loop
    psy, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                             name="invoke_0_testkern_type",
                             dist_mem=dist_mem)
    schedule = invoke.schedule
    ctrans = Dynamo0p3ColourTrans()

    if dist_mem:
        index = 4
    else:
        index = 0

    # Colour the loop
    ctrans.apply(schedule.children[index])

    # Store the results of applying this code transformation as
    # a string (Fortran is not case sensitive)
    gen = str(psy.gen).lower()

    # Check that we've declared the loop-related variables
    # and colour-map pointers
    assert "integer(kind=i_def), pointer :: cmap(:,:)" in gen
    assert "integer(kind=i_def) ncolour" in gen
    assert "integer colour" in gen

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_colour_trans(tmpdir, dist_mem):
    ''' Test of the colouring transformation of a single loop. We test
    when distributed memory is both off and on. '''
    psy, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                             name="invoke_0_testkern_type",
                             dist_mem=dist_mem)
    schedule = invoke.schedule
    ctrans = Dynamo0p3ColourTrans()

    if dist_mem:
        index = 4
    else:
        index = 0

    # Colour the loop
    ctrans.apply(schedule.children[index])

    # Store the results of applying this code transformation as
    # a string (Fortran is not case sensitive)
    gen = str(psy.gen).lower()

    if dist_mem:
        assert ("integer(kind=i_def), allocatable :: "
                "last_halo_cell_all_colours(:,:)" in gen)
    else:
        assert ("integer(kind=i_def), allocatable :: "
                "last_edge_cell_all_colours(:)" in gen)

    # Check that we're calling the API to get the no. of colours
    # and the generated loop bounds are correct
    output = ("      ncolour = mesh%get_ncolours()\n"
              "      cmap => mesh%get_colour_map()\n")
    assert output in gen

    assert "loop0_start = 1" in gen
    assert "loop0_stop = ncolour" in gen
    assert "loop1_start = 1" in gen

    if dist_mem:
        assert ("last_halo_cell_all_colours = mesh%get_last_halo_cell_all_"
                "colours()" in gen)
        output = (
            "      do colour=loop0_start,loop0_stop\n"
            "        do cell=loop1_start,last_halo_cell_all_colours(colour,"
            "1)\n")
    else:  # not dist_mem
        assert ("last_edge_cell_all_colours = mesh%get_last_edge_cell_all_"
                "colours()" in gen)
        output = (
            "      do colour=loop0_start,loop0_stop\n"
            "        do cell=loop1_start,last_edge_cell_all_colours(colour)\n")
    assert output in gen

    # Check that we're using the colour map when getting the cell dof maps
    assert (
        "call testkern_code(nlayers, a, f1_data, f2_data, "
        "m1_data, m2_data, ndf_w1, undf_w1, "
        "map_w1(:,cmap(colour,cell)), ndf_w2, undf_w2, "
        "map_w2(:,cmap(colour,cell)), ndf_w3, undf_w3, "
        "map_w3(:,cmap(colour,cell)))" in gen)

    if dist_mem:
        # Check that we get the right number of set_dirty halo calls in
        # the correct location
        dirty_str = (
            "      end do\n"
            "      !\n"
            "      ! set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      call f1_proxy%set_dirty()\n")
        assert dirty_str in gen
        assert gen.count("set_dirty()") == 1

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_colour_trans_operator(tmpdir, dist_mem):
    '''test of the colouring transformation of a single loop with an
    operator. We check that the first argument is a colourmap lookup,
    not a direct cell index. We test when distributed memory is both
    off and on. '''
    psy, invoke = get_invoke("10_operator.f90", TEST_API,
                             name="invoke_0_testkern_operator_type",
                             dist_mem=dist_mem)
    schedule = invoke.schedule
    ctrans = Dynamo0p3ColourTrans()

    if dist_mem:
        index = 3
    else:
        index = 0

    # Colour the loop
    ctrans.apply(schedule.children[index])

    # Store the results of applying this code transformation as a
    # string
    gen = str(psy.gen)

    # check the first argument is a colourmap lookup
    assert "CALL testkern_operator_code(cmap(colour,cell), nlayers" in gen

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_colour_trans_cma_operator(tmpdir, dist_mem):
    ''' Test of the colouring transformation of a single loop with a CMA
    operator. We check that the first argument is a colourmap lookup,
    not a direct cell index. We test when distributed memory is both
    off and on.

    '''
    psy, invoke = get_invoke("20.3_cma_assembly_field.f90", TEST_API,
                             name="invoke_0_columnwise_op_asm_"
                             "field_kernel_type",
                             dist_mem=dist_mem)
    schedule = invoke.schedule
    ctrans = Dynamo0p3ColourTrans()
    loop = schedule.walk(Loop)[0]

    # Colour the loop
    ctrans.apply(loop)

    # Store the results of applying this code transformation as a
    # string
    gen = str(psy.gen)

    if dist_mem:
        lookup = "last_halo_cell_all_colours(colour,1)"
    else:
        lookup = "last_edge_cell_all_colours(colour)"

    assert (
        f"      DO colour=loop0_start,loop0_stop\n"
        f"        DO cell=loop1_start,{lookup}\n"
        f"          !\n"
        f"          CALL columnwise_op_asm_field_kernel_code("
        f"cmap(colour,") in gen

    assert (
        "          CALL columnwise_op_asm_field_kernel_code(cmap(colour,"
        "cell), nlayers, ncell_2d, afield_data, "
        "lma_op1_proxy%ncell_3d, lma_op1_local_stencil, "
        "cma_op1_cma_matrix, cma_op1_nrow, cma_op1_ncol, cma_op1_bandwidth, "
        "cma_op1_alpha, cma_op1_beta, cma_op1_gamma_m, cma_op1_gamma_p, "
        "ndf_aspc1_afield, undf_aspc1_afield, "
        "map_aspc1_afield(:,cmap(colour,cell)), cbanded_map_aspc1_afield, "
        "ndf_aspc2_lma_op1, cbanded_map_aspc2_lma_op1)\n"
        "        END DO\n"
        "      END DO\n") in gen

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_colour_trans_stencil(dist_mem, tmpdir):
    ''' Test of the colouring transformation of a single loop with a
    stencil access. We test when distributed memory is both off and
    on. '''
    psy, invoke = get_invoke("19.1_single_stencil.f90", TEST_API,
                             name="invoke_0_testkern_stencil_type",
                             dist_mem=dist_mem)
    schedule = invoke.schedule
    ctrans = Dynamo0p3ColourTrans()
    loop = schedule.walk(Loop)[0]

    # Colour the loop
    ctrans.apply(loop)

    # Store the results of applying this code transformation as
    # a string
    gen = str(psy.gen)

    # Check that we index the stencil dofmap appropriately
    assert (
        "          CALL testkern_stencil_code(nlayers, f1_data, "
        "f2_data, f2_stencil_size(cmap(colour,cell)), "
        "f2_stencil_dofmap(:,:,cmap(colour,cell)), f3_data, "
        "f4_data, ndf_w1, undf_w1, map_w1(:,cmap(colour,cell)), "
        "ndf_w2, undf_w2, map_w2(:,cmap(colour,cell)), ndf_w3, "
        "undf_w3, map_w3(:,cmap(colour,cell)))" in gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_colour_trans_adjacent_face(dist_mem, tmpdir):
    '''Test of the colouring transformation of a single loop with
    adjacent face mesh metadata. We test when distributed memory is
    both off and on.

    '''
    psy, invoke = get_invoke("24.1_mesh_prop_invoke.f90", TEST_API,
                             name="invoke_0_testkern_mesh_prop_type",
                             dist_mem=dist_mem)
    schedule = invoke.schedule
    ctrans = Dynamo0p3ColourTrans()

    if dist_mem:
        index = 1
    else:
        index = 0

    # Colour the loop
    ctrans.apply(schedule.children[index])

    # Store the results of applying this code transformation as
    # a string
    gen = str(psy.gen)

    # Check that we index the adjacent face dofmap appropriately
    assert (
        "CALL testkern_mesh_prop_code(nlayers, a, f1_data, ndf_w1, "
        "undf_w1, map_w1(:,cmap(colour,cell)), nfaces_re_h, "
        "adjacent_face(:,cmap(colour,cell))" in gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_colour_trans_continuous_write(dist_mem, tmpdir):
    '''
    Test the colouring transformation for a loop containing a kernel that
    has a 'GH_WRITE' access for a field on a continuous space.

    '''
    psy, invoke = get_invoke("14.1.2_stencil_w2_write.f90", TEST_API,
                             name="invoke_0", dist_mem=dist_mem)
    schedule = invoke.schedule
    ctrans = Dynamo0p3ColourTrans()
    for loop in schedule.walk(Loop):
        ctrans.apply(loop)
    gen = str(psy.gen)

    # The loop should not access the halo, irrespective of whether DM is
    # enabled.
    assert ("last_edge_cell_all_colours = "
            "mesh%get_last_edge_cell_all_colours()" in gen)
    assert "DO cell=loop1_start,last_edge_cell_all_colours(colour)" in gen

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_colour_continuous_writer_intergrid(tmpdir, dist_mem):
    '''
    Test the loop-colouring transformation for an inter-grid kernel that has
    a GH_WRITE access to a field on a continuous space. Since it has GH_WRITE
    it does not need to iterate into the halos (to get clean annexed dofs) and
    therefore should use the 'last_edge_cell' colour map.

    '''
    psy, invoke = get_invoke("22.1.1_intergrid_cont_restrict.f90",
                             TEST_API, idx=0, dist_mem=dist_mem)
    loop = invoke.schedule[0]
    ctrans = Dynamo0p3ColourTrans()
    ctrans.apply(loop)
    result = str(psy.gen).lower()
    # Declarations.
    assert ("integer(kind=i_def), allocatable :: "
            "last_edge_cell_all_colours_field1(:)" in result)
    # Initialisation.
    assert ("last_edge_cell_all_colours_field1 = mesh_field1%"
            "get_last_edge_cell_all_colours()" in result)
    # Usage. Since there is no need to loop into the halo, the upper loop
    # bound should be independent of whether or not DM is enabled.
    upper_bound = "last_edge_cell_all_colours_field1(colour)"
    assert (f"      do colour=loop0_start,loop0_stop\n"
            f"        do cell=loop1_start,{upper_bound}\n"
            f"          !\n"
            f"          call restrict_w2_code(nlayers" in result)
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_colouring_not_a_loop(dist_mem):
    '''Test that we raise an appropriate error if we attempt to colour
    something that is not a loop. We test when distributed memory is
    on or off. '''
    _, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                           name="invoke_0_testkern_type", dist_mem=dist_mem)

    schedule = invoke.schedule
    ctrans = Dynamo0p3ColourTrans()

    # Erroneously attempt to colour the schedule rather than the loop
    with pytest.raises(TransformationError) as excinfo:
        ctrans.apply(schedule)
    assert ("Target of Dynamo0p3ColourTrans transformation must be a "
            "sub-class of Loop but got 'DynInvokeSchedule'" in
            str(excinfo.value))


def test_no_colour_dofs(dist_mem):
    ''' Test that we raise the correct exception when attempting to apply
    the loop-colouring transformation to a loop that is over dofs rather than
    cells. '''
    _, invoke = get_invoke("15.12.3_single_pointwise_builtin.f90", TEST_API,
                           name="invoke_0", dist_mem=dist_mem)
    schedule = invoke.schedule
    ctrans = Dynamo0p3ColourTrans()
    with pytest.raises(TransformationError) as excinfo:
        ctrans.apply(schedule.children[0])
    val = str(excinfo.value)
    assert "Error in DynamoColour transformation" in val
    assert ("Only loops over cells may be coloured but this loop is over "
            "dof" in val)


def test_omp_str():
    ''' Test the str method of the Dynamo0p3OMPLoopTrans class. '''
    olooptrans = Dynamo0p3OMPLoopTrans()
    oname = str(olooptrans)
    assert oname == "Add an OpenMP DO directive to a Dynamo 0.3 loop"


def test_omp_not_a_loop(dist_mem):
    '''Test that we raise an appropriate error if we attempt to apply an
    OpenMP DO transformation to something that is not a loop. We test
    when distributed memory is on or off. '''
    _, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                           name="invoke_0_testkern_type", dist_mem=dist_mem)
    schedule = invoke.schedule
    otrans = Dynamo0p3OMPLoopTrans()

    # Erroneously attempt to apply OpenMP to the schedule rather than
    # the loop
    with pytest.raises(TransformationError) as excinfo:
        otrans.apply(schedule)

    assert ("Target of Dynamo0p3OMPLoopTrans transformation must be a sub-"
            "class of Loop but got 'DynInvokeSchedule'" in str(excinfo.value))


def test_omp_parallel_not_a_loop(dist_mem):
    '''Test that we raise an appropriate error if we attempt to apply an
    OpenMP PARALLEL DO transformation to something that is not a
    loop. We test when distributed memory is on or off. '''
    _, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                           name="invoke_0_testkern_type", dist_mem=dist_mem)
    schedule = invoke.schedule
    otrans = DynamoOMPParallelLoopTrans()

    # Erroneously attempt to apply OpenMP to the schedule rather than
    # the loop
    with pytest.raises(TransformationError) as excinfo:
        otrans.apply(schedule)
    assert ("Error in DynamoOMPParallelLoopTrans transformation. The "
            "supplied node must be a LFRicLoop but got 'DynInvokeSchedule'"
            in str(excinfo.value))


def test_colour_str():
    ''' Test the str method of the Dynamo0p3ColourTrans class. '''
    ctrans = Dynamo0p3ColourTrans()
    cstr = str(ctrans)
    assert cstr == "Split a Dynamo 0.3 loop over cells into colours"


def test_omp_colour_trans(tmpdir, dist_mem):
    ''' Test the OpenMP transformation applied to a coloured loop. We test
    when distributed memory is on or off. '''
    psy, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                             name="invoke_0_testkern_type",
                             dist_mem=dist_mem)
    schedule = invoke.schedule

    ctrans = Dynamo0p3ColourTrans()
    otrans = DynamoOMPParallelLoopTrans()

    if dist_mem:
        index = 4
    else:
        index = 0

    # Colour the loop
    ctrans.apply(schedule.children[index])

    # Then apply OpenMP to the inner loop
    otrans.apply(schedule.children[index].loop_body[0])

    code = str(psy.gen)

    assert ("      ncolour = mesh%get_ncolours()\n"
            "      cmap => mesh%get_colour_map()\n" in code)
    if dist_mem:
        lookup = "last_halo_cell_all_colours(colour,1)"
    else:
        lookup = "last_edge_cell_all_colours(colour)"
    output = (
        f"      DO colour=loop0_start,loop0_stop\n"
        f"        !$omp parallel do default(shared), private(cell), "
        f"schedule(static)\n"
        f"        DO cell=loop1_start,{lookup}\n")
    assert output in code

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_omp_parallel_colouring_needed(monkeypatch, annexed, dist_mem):
    '''Test that we raise an error when applying an OpenMP PARALLEL DO
    transformation to a loop that requires colouring (i.e. has a field
    with 'INC' access) but is not coloured. We test when distributed
    memory is on or off. We also test when annexed is False and True
    as it affects how many halo exchanges are generated.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, invoke = get_invoke("11_any_space.f90", TEST_API,
                           name="invoke_0_testkern_any_space_1_type",
                           dist_mem=dist_mem)
    if dist_mem:
        if annexed:
            index = 4
        else:
            index = 5
    else:
        index = 0
    schedule = invoke.schedule
    otrans = DynamoOMPParallelLoopTrans()
    # Apply OpenMP to the loop
    with pytest.raises(TransformationError) as excinfo:
        otrans.apply(schedule.children[index])
    assert "Error in DynamoOMPParallelLoopTrans" in str(excinfo.value)
    assert "kernel has an argument with INC access" in str(excinfo.value)
    assert "Colouring is required" in str(excinfo.value)


def test_omp_colouring_needed(monkeypatch, annexed, dist_mem):
    '''Test that we raise an error when applying an OpenMP DO
    transformation to a loop that requires colouring (i.e. has a field
    with 'INC' access) but is not coloured. We test when distributed
    memory is on or off. We also test when annexed is False and True
    as it affects how many halo exchanges are generated.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, invoke = get_invoke("11_any_space.f90", TEST_API,
                           name="invoke_0_testkern_any_space_1_type",
                           dist_mem=dist_mem)
    if dist_mem:
        if annexed:
            index = 4
        else:
            index = 5
    else:
        index = 0
    schedule = invoke.schedule

    otrans = Dynamo0p3OMPLoopTrans()
    # Apply OpenMP to the loop
    with pytest.raises(TransformationError) as excinfo:
        otrans.apply(schedule.children[index])
    assert "Error in Dynamo0p3OMPLoopTrans transfo" in str(excinfo.value)
    assert "kernel has an argument with INC access" in str(excinfo.value)
    assert "Colouring is required" in str(excinfo.value)


def test_check_seq_colours_omp_do(tmpdir, monkeypatch, annexed, dist_mem):
    '''Test that we raise an error if the user attempts to apply an OpenMP
    DO transformation to a loop over colours (since any such loop must
    be sequential). We test when distributed memory is on or off. We
    also test when annexed is False and True as it affects how many
    halo exchanges are generated.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    psy, invoke = get_invoke("1.1.0_single_invoke_xyoz_qr.f90", TEST_API,
                             name="invoke_0_testkern_qr_type",
                             dist_mem=dist_mem)
    schedule = invoke.schedule
    if dist_mem:
        if annexed:
            index = 3
        else:
            index = 4
    else:
        index = 0

    ctrans = Dynamo0p3ColourTrans()
    otrans = Dynamo0p3OMPLoopTrans()

    # Colour the loop
    ctrans.apply(schedule.children[index])

    # Then erroneously attempt to apply OpenMP to the loop over
    # colours
    with pytest.raises(TransformationError) as excinfo:
        otrans.apply(schedule.children[index])

    assert "Error in Dynamo0p3OMPLoopTrans" in str(excinfo.value)
    assert "target loop is over colours" in str(excinfo.value)
    assert "must be computed serially" in str(excinfo.value)

    # This test checks the code without OpenMP as this
    # transformation fails
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_colouring_after_openmp(dist_mem, monkeypatch):
    '''Test that we raise an error if the user attempts to colour a loop
    that is already within an OpenMP parallel region. We test when
    distributed memory is on or off.  Note: An OpenMP parallel
    transformation can only be applied on its own for a discontinuous
    function space. However, colouring can be applied when an argument
    has "INC" access and in the LFRic API this is only possible for
    fields on a continuous function space. Therefore we need to
    monkeypatch the function space of the loop argument to a
    continuous function space before applying colouring.

    '''
    _, invoke = get_invoke("1_single_invoke_w3.f90", TEST_API,
                           name="invoke_0_testkern_w3_type", dist_mem=dist_mem)
    schedule = invoke.schedule

    ctrans = Dynamo0p3ColourTrans()
    otrans = DynamoOMPParallelLoopTrans()

    if dist_mem:
        index = 3
    else:
        index = 0

    # Apply OpenMP to the loop
    otrans.apply(schedule[index])

    # Monkeypatch the loop argument to "INC" access and a continuous function
    # space so colouring can be applied
    write_arg = schedule[index].dir_body[0].loop_body[0].arguments.args[4]
    fspace = write_arg.function_space
    monkeypatch.setattr(write_arg, "access", AccessType.INC)
    monkeypatch.setattr(fspace, "_orig_name", "w1")

    # Now attempt to colour the loop within this OpenMP region
    with pytest.raises(TransformationError) as excinfo:
        ctrans.apply(schedule[index].dir_body[0])
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
    psy, invoke = get_invoke("4.6_multikernel_invokes.f90", TEST_API,
                             name="invoke_0",
                             dist_mem=dist_mem)

    schedule = invoke.schedule

    ctrans = Dynamo0p3ColourTrans()
    otrans = DynamoOMPParallelLoopTrans()
    mtrans = MoveTrans()

    if dist_mem:
        if annexed:
            index = 5
        else:
            # f is required but can be moved before the first loop
            mtrans.apply(schedule.children[7], schedule.children[6])
            index = 7
    else:
        index = 0

    # colour each loop
    ctrans.apply(schedule[index])
    ctrans.apply(schedule[index+1])

    # Apply OpenMP to each of the colour loops
    otrans.apply(schedule[index].loop_body[0])
    otrans.apply(schedule[index+1].loop_body[0])

    gen = str(psy.gen)

    # Check that we're calling the API to get the no. of colours
    assert gen.count("cmap => mesh%get_colour_map()") == 1
    assert "private(cell)" in gen
    assert gen.count("private(cell)") == 2


def test_omp_region_omp_do(dist_mem):
    ''' Test that we correctly generate code for the case of a single OMP
    DO within an OMP PARALLEL region without colouring. We test when
    distributed memory is on or off.
    Note: this test does not apply colouring so the loops must be over
    discontinuous function spaces.

    '''
    psy, invoke = get_invoke(
        "1_single_invoke_w3.f90", TEST_API,
        name="invoke_0_testkern_w3_type", dist_mem=dist_mem)
    schedule = invoke.schedule
    olooptrans = Dynamo0p3OMPLoopTrans()
    ptrans = OMPParallelTrans()

    if dist_mem:
        index = 3
    else:
        index = 0

    # Put an OMP PARALLEL around this loop
    child = schedule[index]
    ptrans.apply(child)

    # Put an OMP DO around this loop
    olooptrans.apply(schedule[index].dir_body[0])

    # Store the results of applying this code transformation as
    # a string
    code = str(psy.gen)

    omp_do_idx = -1
    omp_para_idx = -1
    cell_loop_idx = -1
    omp_enddo_idx = -1
    if dist_mem:
        assert "loop0_stop = mesh%get_last_edge_cell()" in code
    else:
        assert "loop0_stop = m2_proxy%vspace%get_ncell()" in code
    for idx, line in enumerate(code.split('\n')):
        if "DO cell=loop0_start,loop0_stop" in line:
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


def test_omp_region_omp_do_rwdisc(monkeypatch, annexed, dist_mem):
    ''' Test that we correctly generate code for the case of a single
    OMP DO within an OMP PARALLEL region without colouring when a
    discontinuous field has readwrite access. We test when distributed
    memory is on or off. Also test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    psy, invoke = get_invoke("1_single_invoke_any_discontinuous_space.f90",
                             TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    olooptrans = Dynamo0p3OMPLoopTrans()
    ptrans = OMPParallelTrans()
    # Put an OMP PARALLEL around this loop
    if dist_mem and not annexed:
        # there are 2 halo exchange calls
        index = 2
    else:
        # there are no halo exchange calls
        index = 0
    child = schedule[index]
    ptrans.apply(child)

    # Put an OMP DO around this loop
    olooptrans.apply(schedule[index].dir_body[0])

    # Store the results of applying this code transformation as
    # a string
    code = str(psy.gen)

    omp_do_idx = -1
    omp_para_idx = -1
    cell_loop_idx = -1
    omp_enddo_idx = -1
    if dist_mem:
        assert "loop0_stop = mesh%get_last_edge_cell()" in code
    else:
        assert "loop0_stop = f1_proxy%vspace%get_ncell()" in code
    loop_str = "DO cell=loop0_start,loop0_stop"
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


def test_multi_kernel_single_omp_region(dist_mem):
    ''' Test that we correctly generate all the map-lookups etc.
    when an invoke contains more than one kernel that are all contained
    within a single OMP region.
    Note: this test does not apply colouring so the loops must be over
    discontinuous function spaces.

    '''
    psy, invoke = get_invoke("4.13_multikernel_invokes_w3_anyd.f90", TEST_API,
                             name="invoke_0",
                             dist_mem=dist_mem)
    schedule = invoke.schedule

    if dist_mem:
        index = 3
    else:
        index = 0

    otrans = Dynamo0p3OMPLoopTrans()
    rtrans = OMPParallelTrans()

    # Apply OpenMP to each of the loops
    otrans.apply(schedule.children[index])
    otrans.apply(schedule.children[index+1])

    # Enclose all of these OpenMP'd loops within a single region
    rtrans.apply(schedule.children[index:index+2])

    code = str(psy.gen)

    omp_do_idx = -1
    omp_end_do_idx = -1
    omp_para_idx = -1
    omp_end_para_idx = -1
    cell_loop_idx = -1
    end_do_idx = -1
    if dist_mem:
        assert "loop0_stop = mesh%get_last_edge_cell()" in code
    else:
        assert "loop0_stop = m2_proxy%vspace%get_ncell()" in code
    loop_str = "DO cell=loop0_start,loop0_stop"
    for idx, line in enumerate(code.split('\n')):
        if (cell_loop_idx == -1) and (loop_str in line):
            cell_loop_idx = idx
        if (omp_do_idx == -1) and ("!$omp do" in line):
            omp_do_idx = idx
        if "!$omp end do" in line:
            omp_end_do_idx = idx
        if "!$omp parallel default(shared), private(cell)" in line:
            omp_para_idx = idx
        if "END DO" in line:
            end_do_idx = idx
        if "!$omp end parallel" in line:
            omp_end_para_idx = idx

    assert (omp_do_idx - omp_para_idx) == 1
    assert (cell_loop_idx - omp_do_idx) == 1
    assert (omp_end_para_idx - omp_end_do_idx) > 0
    assert (omp_end_do_idx - end_do_idx) == 1


def test_multi_different_kernel_omp(
        tmpdir, monkeypatch, dist_mem, annexed):
    ''' Test that we correctly generate the OpenMP private lists when we
    have more than one kernel of a different type (requiring a
    different private list) within an invoke. Test with and without
    DM. We also test when annexed is False and True as it affects how
    many halo exchanges are generated.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    psy, invoke = get_invoke("4.7_multikernel_invokes.f90", TEST_API,
                             name="invoke_0",
                             dist_mem=dist_mem)
    schedule = invoke.schedule

    if dist_mem:
        if annexed:
            index1 = 5
            index2 = 8
        else:
            index1 = 6
            index2 = 10
    else:
        index1 = 0
        index2 = 1

    ctrans = Dynamo0p3ColourTrans()
    otrans = DynamoOMPParallelLoopTrans()

    # Colour each loop
    ctrans.apply(schedule.children[index1])
    ctrans.apply(schedule.children[index2])

    # Apply OpenMP to each of the colour loops
    otrans.apply(schedule.children[index1].loop_body[0])
    otrans.apply(schedule.children[index2].loop_body[0])

    code = str(psy.gen)

    assert "private(cell)" in code

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_loop_fuse_invalid_space(monkeypatch):
    ''' Test that we raise an appropriate error if the user attempts
    to fuse loops that are on invalid spaces.
    '''
    _, first_invoke = get_invoke("4_multikernel_invokes.f90", TEST_API,
                                 idx=0, dist_mem=False)
    schedule = first_invoke.schedule
    first_kernel_args = schedule.coded_kernels()[0].arguments
    # Get argument on the "write" space w1
    _, fspace = first_kernel_args.get_arg_on_space_name("w1")
    # Make function space invalid
    monkeypatch.setattr(fspace, "_orig_name", "not_a_space_name")

    # Apply transformation and raise the error
    ftrans = LFRicLoopFuseTrans()
    with pytest.raises(TransformationError) as excinfo:
        ftrans.apply(schedule.children[0], schedule.children[1])
    assert ("One or both function spaces 'not_a_space_name' and 'w1' have "
            "invalid names" in str(excinfo.value))


def test_loop_fuse_different_spaces(monkeypatch, dist_mem):
    ''' Test that we raise an appropriate error if the user attempts
    fuse loops that are on different spaces (unless they are both on
    discontinuous spaces). We test with annexed is False as this is
    how the test has been set up.

    '''
    dyn_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", False)
    for same_space in [False, True]:
        _, invoke = get_invoke("4.7_multikernel_invokes.f90",
                               TEST_API, name="invoke_0", dist_mem=dist_mem)
        schedule = invoke.schedule

        ftrans = LFRicLoopFuseTrans()
        mtrans = MoveTrans()
        if dist_mem:
            index = 9
            # f, c and g halo exchanges between loops can be moved
            # before the 1st loop as they are not accessed in it
            for idx in range(index-3, index):
                mtrans.apply(schedule.children[idx+1],
                             schedule.children[idx])
        else:
            index = 0

        with pytest.raises(TransformationError) as excinfo:
            ftrans.apply(schedule.children[index],
                         schedule.children[index+1],
                         {"same_space": same_space})

        if same_space:
            assert ("The 'same_space' flag was set, but does not apply "
                    "because neither field is on 'ANY_SPACE'" in
                    str(excinfo.value))
        else:
            assert ("Cannot fuse loops that are over different spaces "
                    "'w2' and 'w1' unless they are both discontinuous"
                    in str(excinfo.value))


def test_loop_fuse_same_space_error():
    ''' Test that we raise an appropriate error if the user attempts
    to incorrectly set the 'same_space' property

    '''
    ftrans = LFRicLoopFuseTrans()
    with pytest.raises(TransformationError) as excinfo:
        ftrans.apply(None, None, {"same_space": "foo"})
    assert ("The value of the 'same_space' flag must be either bool or "
            "None type, but the type of flag provided was 'str'."
            in str(excinfo.value))


def test_loop_fuse(dist_mem):
    ''' Test that we are able to fuse two loops together. '''
    psy, invoke = get_invoke("4_multikernel_invokes.f90", TEST_API,
                             name="invoke_0", dist_mem=dist_mem)
    schedule = invoke.schedule

    if dist_mem:
        index = 4
    else:
        index = 0

    ftrans = LFRicLoopFuseTrans()

    # Fuse the loops
    ftrans.apply(schedule.children[index],
                 schedule.children[index+1])

    gen = str(psy.gen)

    cell_loop_idx = -1
    end_loop_idx = -1
    call_idx1 = -1
    call_idx2 = -1
    if dist_mem:
        assert "loop0_stop = mesh%get_last_halo_cell(1)" in gen
    else:
        assert "loop0_stop = f1_proxy%vspace%get_ncell()" in gen
    loop_str = "DO cell=loop0_start,loop0_stop"
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
    psy, invoke = get_invoke("4_multikernel_invokes.f90", TEST_API,
                             name="invoke_0", dist_mem=True)

    schedule = invoke.schedule
    ftrans = LFRicLoopFuseTrans()
    # Fuse the loops
    ftrans.apply(schedule.children[4], schedule.children[5])

    gen = str(psy.gen)
    assert gen.count("set_dirty()") == 1


def test_loop_fuse_omp(dist_mem):
    ''' Test that we can loop-fuse two loop nests and enclose them in an
    OpenMP parallel region.
    Note: this test does not apply colouring so the loops must be over
    discontinuous function spaces.

    '''
    psy, invoke = get_invoke("4.12_multikernel_invokes_w2v.f90", TEST_API,
                             name="invoke_0", dist_mem=dist_mem)
    schedule = invoke.schedule

    ftrans = LFRicLoopFuseTrans()
    otrans = DynamoOMPParallelLoopTrans()

    ftrans.apply(schedule.children[0], schedule.children[1])

    otrans.apply(schedule.children[0])

    code = str(psy.gen)

    # Check generated code
    omp_para_idx = -1
    omp_endpara_idx = -1
    cell_do_idx = -1
    cell_enddo_idx = -1
    call1_idx = -1
    call2_idx = -1
    if dist_mem:
        assert "loop0_stop = mesh%get_last_edge_cell()" in code
    else:
        assert "loop0_stop = f1_proxy%vspace%get_ncell()" in code
    loop_str = "DO cell=loop0_start,loop0_stop"
    for idx, line in enumerate(code.split('\n')):
        if loop_str in line:
            cell_do_idx = idx
        if ("!$omp parallel do default(shared), private(cell), "
                "schedule(static)" in line):
            omp_para_idx = idx
        if "CALL testkern_w2v_code" in line:
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


def test_loop_fuse_omp_rwdisc(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that we can loop-fuse two loop nests and enclose them in
    an OpenMP parallel region for a kernel when discontinuous fields
    have readwrite access. We test when distributed memory is on or
    off. Also test with and without annexed dofs being computed as
    this affects the generated code.
    Note: Fusing loops over discontinuous readwriters over different
    spaces is allowed unless their loop bounds differ due to a
    previous transformation.
    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    psy, invoke = get_invoke("4.13_multikernel_invokes_w3_anyd.f90", TEST_API,
                             name="invoke_0", dist_mem=dist_mem)
    schedule = invoke.schedule

    ftrans = LFRicLoopFuseTrans()
    otrans = DynamoOMPParallelLoopTrans()

    if dist_mem and not annexed:
        # there are 3 halo exchange calls
        index = 3
    else:
        # there are no halo exchange calls
        index = 0
    ftrans.apply(schedule.children[index], schedule.children[index+1])

    otrans.apply(schedule.children[index])

    code = str(psy.gen)

    # Check generated code
    omp_para_idx = -1
    omp_endpara_idx = -1
    cell_do_idx = -1
    cell_enddo_idx = -1
    call1_idx = -1
    call2_idx = -1
    if dist_mem:
        assert "loop0_stop = mesh%get_last_edge_cell()" in code
    else:
        assert "loop0_stop = m2_proxy%vspace%get_ncell()" in code
    loop_str = "DO cell=loop0_start,loop0_stop"
    for idx, line in enumerate(code.split('\n')):
        if loop_str in line:
            cell_do_idx = idx
        if "!$omp parallel do default(shared), " +\
           "private(cell), schedule(static)" in line:
            omp_para_idx = idx
        if "CALL testkern_w3_code" in line:
            call1_idx = idx
        if "CALL testkern_anyd_any_space_code" in line:
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

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_fuse_colour_loops(tmpdir, monkeypatch, annexed, dist_mem):
    '''Test that we can fuse colour loops , enclose them in an OpenMP
    parallel region and preceed each by an OpenMP DO for both
    sequential and distributed-memory code. We also test when annexed
    is False and True as it affects how many halo exchanges are
    generated.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    psy, invoke = get_invoke("4.6_multikernel_invokes.f90", TEST_API,
                             name="invoke_0", dist_mem=dist_mem)
    schedule = invoke.schedule

    ctrans = Dynamo0p3ColourTrans()
    otrans = Dynamo0p3OMPLoopTrans()
    rtrans = OMPParallelTrans()
    ftrans = LFRicLoopFuseTrans()
    mtrans = MoveTrans()

    if dist_mem:
        if annexed:
            index = 5
        else:
            # f is required but can be moved before the first loop
            mtrans.apply(schedule.children[7], schedule.children[6])
            index = 7
    else:
        index = 0

    # colour each loop
    ctrans.apply(schedule[index])
    ctrans.apply(schedule[index+1])

    # fuse the sequential colours loop
    ftrans.apply(schedule[index], schedule[index+1])

    # Enclose the colour loops within an OMP parallel region
    rtrans.apply(schedule[index].loop_body.children)

    # Put an OMP DO around each of the colour loops
    for loop in schedule[index].loop_body[0].dir_body[:]:
        otrans.apply(loop)

    code = str(psy.gen)
    assert "ncolour = mesh%get_ncolours()" in code
    assert "cmap => mesh%get_colour_map()\n" in code
    assert "loop0_stop = ncolour" in code

    if dist_mem:
        lookup = "last_halo_cell_all_colours(colour,1)"
    else:
        lookup = "last_edge_cell_all_colours(colour)"

    output = (
        f"      !\n"
        f"      DO colour=loop0_start,loop0_stop\n"
        f"        !$omp parallel default(shared), private(cell)\n"
        f"        !$omp do schedule(static)\n"
        f"        DO cell=loop1_start,{lookup}\n"
        f"          !\n"
        f"          CALL ru_code(nlayers, a_data, b_data, "
        f"istp, rdt, d_data, e_1_data, e_2_data, "
        f"e_3_data, ndf_w2, undf_w2, map_w2(:,cmap(colour,"
        f"cell)), basis_w2_qr, diff_basis_w2_qr, ndf_w3, undf_w3, "
        f"map_w3(:,cmap(colour,cell)), basis_w3_qr, ndf_w0, undf_w0, "
        f"map_w0(:,cmap(colour,cell)), basis_w0_qr, diff_basis_w0_qr, "
        f"np_xy_qr, np_z_qr, weights_xy_qr, weights_z_qr)\n"
        f"        END DO\n"
        f"        !$omp end do\n"
        f"        !$omp do schedule(static)\n"
        f"        DO cell=loop2_start,{lookup}\n"
        f"          !\n"
        f"          CALL ru_code(nlayers, f_data, b_data, "
        f"istp, rdt, d_data, e_1_data, e_2_data, "
        f"e_3_data, ndf_w2, undf_w2, map_w2(:,cmap(colour,"
        f"cell)), basis_w2_qr, diff_basis_w2_qr, ndf_w3, undf_w3, "
        f"map_w3(:,cmap(colour,cell)), basis_w3_qr, ndf_w0, undf_w0, "
        f"map_w0(:,cmap(colour,cell)), basis_w0_qr, diff_basis_w0_qr, "
        f"np_xy_qr, np_z_qr, weights_xy_qr, weights_z_qr)\n"
        f"        END DO\n"
        f"        !$omp end do\n"
        f"        !$omp end parallel\n"
        f"      END DO\n")
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

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_loop_fuse_cma(tmpdir, dist_mem):
    ''' Test that we can loop fuse two loops when one contains a
    call to a CMA-related kernel.

    '''
    psy, invoke = get_invoke("20.6_multi_invoke_with_cma.f90", TEST_API,
                             name="invoke_0", dist_mem=dist_mem)
    ftrans = LFRicLoopFuseTrans()
    mtrans = MoveTrans()
    schedule = invoke.schedule
    if dist_mem:
        # move halo exchanges before the first loop
        mtrans.apply(schedule.children[2], schedule.children[1])
        mtrans.apply(schedule.children[3], schedule.children[2])
        mtrans.apply(schedule.children[4], schedule.children[3])
        index = 4
    else:
        index = 0

    # Fuse the loops
    ftrans.apply(schedule.children[index], schedule.children[index+1],
                 {"same_space": True})
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert (
        "      ! Look-up required column-banded dofmaps\n"
        "      !\n"
        "      cbanded_map_aspc1_afield => "
        "cma_op1_proxy%column_banded_dofmap_to\n"
        "      cbanded_map_aspc2_lma_op1 => "
        "cma_op1_proxy%column_banded_dofmap_from\n") in code
    assert (
        "      ! Look-up information for each CMA operator\n"
        "      !\n"
        "      cma_op1_cma_matrix => cma_op1_proxy%columnwise_matrix\n"
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
        "ncell_2d, afield_data, lma_op1_proxy%ncell_3d, "
        "lma_op1_local_stencil, cma_op1_cma_matrix, cma_op1_nrow, "
        "cma_op1_ncol, cma_op1_bandwidth, cma_op1_alpha, cma_op1_beta, "
        "cma_op1_gamma_m, cma_op1_gamma_p, ndf_aspc1_afield, "
        "undf_aspc1_afield, map_aspc1_afield(:,cell), "
        "cbanded_map_aspc1_afield, ndf_aspc2_lma_op1, "
        "cbanded_map_aspc2_lma_op1)\n"
        "        !\n"
        "        CALL testkern_two_real_scalars_code(nlayers, scalar1, "
        "afield_data, bfield_data, cfield_data, "
        "dfield_data, scalar2, ndf_w1, undf_w1, map_w1(:,cell), "
        "ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
        "map_w3(:,cell))\n") in code


def test_omp_par_and_halo_exchange_error():
    ''' Tests that we raise an error if we try to apply an OMP parallel
    transformation to a list containing halo_exchange calls. If this is
    allowed then it is likely that we will get incorrect results, or that
    the code will fail.
    Note: this test does not apply colouring so the loops must be over
    discontinuous function spaces.

    '''
    _, invoke = get_invoke("4.13_multikernel_invokes_w3_anyd.f90", TEST_API,
                           name="invoke_0", dist_mem=True)
    schedule = invoke.schedule

    otrans = Dynamo0p3OMPLoopTrans()
    rtrans = OMPParallelTrans()

    # Apply OpenMP to each of the loops
    otrans.apply(schedule.children[3])
    otrans.apply(schedule.children[4])

    # Enclose the invoke code within a single region
    with pytest.raises(TransformationError) as excinfo:
        rtrans.apply(schedule.children)
    assert ("type 'LFRicHaloExchange' cannot be enclosed by a "
            "OMPParallelTrans transformation" in str(excinfo.value))


def test_builtin_single_omp_pdo(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that we generate correct code if an OpenMP PARALLEL DO is
    applied to a single builtin. Also test with and without annexed
    dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    psy, invoke = get_invoke("15.7.2_setval_X_builtin.f90", TEST_API,
                             idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    otrans = DynamoOMPParallelLoopTrans()
    # Apply OpenMP parallelisation to the loop
    otrans.apply(schedule.children[0])
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if dist_mem:  # annexed can be True or False
        if annexed:
            assert ("loop0_stop = f2_proxy%vspace%get_last_dof_annexed()"
                    in result)
        else:
            assert ("loop0_stop = f2_proxy%vspace%get_last_dof_owned()"
                    in result)
        code = (
            "      !$omp parallel do default(shared), private(df), "
            "schedule(static)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_data(df) = f1_data(df)\n"
            "      END DO\n"
            "      !$omp end parallel do\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop(s)\n"
            "      !\n"
            "      CALL f2_proxy%set_dirty()")
        assert code in result
    else:  # not distmem. annexed can be True or False
        assert "loop0_stop = undf_aspc1_f2" in result
        assert (
            "      !$omp parallel do default(shared), private(df), "
            "schedule(static)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_data(df) = f1_data(df)\n"
            "      END DO\n"
            "      !$omp end parallel do") in result


def test_builtin_multiple_omp_pdo(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that we generate correct code if OpenMP PARALLEL DOs are
    applied to multiple builtins. Also test with and without annexed
    dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    psy, invoke = get_invoke("15.14.2_multiple_set_kernels.f90", TEST_API,
                             idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    otrans = DynamoOMPParallelLoopTrans()
    # Apply OpenMP parallelisation to the loop
    for child in schedule.children:
        otrans.apply(child)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if dist_mem:  # annexed can be True or False
        for idx in range(1, 4):
            if annexed:
                name = "annexed"
            else:
                name = "owned"
            assert (f"loop{idx-1}_stop = f{idx}_proxy%vspace%"
                    f"get_last_dof_{name}()" in result)

        code = (
            "      !$omp parallel do default(shared), private(df), "
            "schedule(static)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = fred\n"
            "      END DO\n"
            "      !$omp end parallel do\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop(s)\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      !\n"
            "      ! End of set dirty/clean section for above loop(s)\n"
            "      !\n"
            "      !$omp parallel do default(shared), private(df), "
            "schedule(static)\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        f2_data(df) = 3.0_r_def\n"
            "      END DO\n"
            "      !$omp end parallel do\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop(s)\n"
            "      !\n"
            "      CALL f2_proxy%set_dirty()\n"
            "      !\n"
            "      ! End of set dirty/clean section for above loop(s)\n"
            "      !\n"
            "      !$omp parallel do default(shared), private(df), "
            "schedule(static)\n"
            "      DO df=loop2_start,loop2_stop\n"
            "        f3_data(df) = ginger\n"
            "      END DO\n"
            "      !$omp end parallel do\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop(s)\n"
            "      !\n"
            "      CALL f3_proxy%set_dirty()")
        assert code in result
    else:  # not distmem. annexed can be True or False
        for idx in range(1, 4):
            assert f"loop{idx-1}_stop = undf_aspc1_f{idx}" in result
        assert (
            "      !$omp parallel do default(shared), private(df), "
            "schedule(static)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = fred\n"
            "      END DO\n"
            "      !$omp end parallel do\n"
            "      !$omp parallel do default(shared), private(df), "
            "schedule(static)\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        f2_data(df) = 3.0_r_def\n"
            "      END DO\n"
            "      !$omp end parallel do\n"
            "      !$omp parallel do default(shared), private(df), "
            "schedule(static)\n"
            "      DO df=loop2_start,loop2_stop\n"
            "        f3_data(df) = ginger\n"
            "      END DO\n"
            "      !$omp end parallel do\n") in result


def test_builtin_loop_fuse_pdo(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that we generate correct code if an OpenMP PARALLEL DO is
    applied to multiple loop fused builtins. We have to assert that it
    is safe to loop fuse. Also test with and without annexed
    dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    psy, invoke = get_invoke("15.14.2_multiple_set_kernels.f90", TEST_API,
                             idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    ftrans = LFRicLoopFuseTrans()
    ftrans.apply(schedule.children[0], schedule.children[1],
                 {"same_space": True})
    ftrans.apply(schedule.children[0], schedule.children[1],
                 {"same_space": True})
    otrans = DynamoOMPParallelLoopTrans()
    # Apply OpenMP parallelisation to the loop
    otrans.apply(schedule.children[0])
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if dist_mem:  # annexed can be True or False
        if annexed:
            assert ("loop0_stop = f1_proxy%vspace%get_last_dof_annexed()"
                    in result)
        else:
            assert ("loop0_stop = f1_proxy%vspace%get_last_dof_owned()"
                    in result)
        code = (
            "      !$omp parallel do default(shared), private(df), "
            "schedule(static)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = fred\n"
            "        f2_data(df) = 3.0_r_def\n"
            "        f3_data(df) = ginger\n"
            "      END DO\n"
            "      !$omp end parallel do\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop(s)\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      CALL f2_proxy%set_dirty()\n"
            "      CALL f3_proxy%set_dirty()")
        assert code in result
    else:  # distmem is False. annexed can be True or False
        assert "loop0_stop = undf_aspc1_f1" in result
        assert (
            "      !$omp parallel do default(shared), private(df), "
            "schedule(static)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = fred\n"
            "        f2_data(df) = 3.0_r_def\n"
            "        f3_data(df) = ginger\n"
            "      END DO\n"
            "      !$omp end parallel do") in result


def test_builtin_single_omp_do(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that we generate correct code if an OpenMP DO (with an
    outer OpenMP parallel) is applied to a single builtin. Also test
    with and without annexed dofs being computed as this affects the
    generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    psy, invoke = get_invoke("15.7.2_setval_X_builtin.f90", TEST_API,
                             idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule

    olooptrans = Dynamo0p3OMPLoopTrans()
    ptrans = OMPParallelTrans()

    # Put an OMP PARALLEL around this loop
    child = schedule[0]
    ptrans.apply(child)
    # Put an OMP DO around this loop
    olooptrans.apply(schedule[0].dir_body[0])
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if dist_mem:  # annexed can be True or False
        if annexed:
            assert ("loop0_stop = f2_proxy%vspace%get_last_dof_annexed()"
                    in result)
        else:
            assert ("loop0_stop = f2_proxy%vspace%get_last_dof_owned()"
                    in result)
        assert (
            "      !$omp parallel default(shared), private(df)\n"
            "      !$omp do schedule(static)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_data(df) = f1_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp end parallel\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop(s)\n"
            "      !\n"
            "      CALL f2_proxy%set_dirty()\n"
            "      !\n") in result
    else:  # distmem is False. annexed can be True or False
        assert "loop0_stop = undf_aspc1_f2" in result
        assert (
            "      !$omp parallel default(shared), private(df)\n"
            "      !$omp do schedule(static)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f2_data(df) = f1_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp end parallel\n") in result


def test_builtin_multiple_omp_do(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that we generate correct code if OpenMP DOs are applied
    to multiple builtins. Also test with and without annexed dofs
    being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    psy, invoke = get_invoke("15.14.2_multiple_set_kernels.f90", TEST_API,
                             idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule

    olooptrans = Dynamo0p3OMPLoopTrans()
    ptrans = OMPParallelTrans()

    # Put an OMP PARALLEL around the loops
    children = schedule.children
    ptrans.apply(children)
    # Put an OMP DO around the loops
    for child in schedule[0].dir_body[:]:
        olooptrans.apply(child)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if dist_mem:  # annexed can be True or False
        if annexed:
            assert ("loop0_stop = f1_proxy%vspace%get_last_dof_annexed()"
                    in result)
            assert ("loop1_stop = f2_proxy%vspace%get_last_dof_annexed()"
                    in result)
            assert ("loop2_stop = f3_proxy%vspace%get_last_dof_annexed()"
                    in result)
        else:
            assert ("loop0_stop = f1_proxy%vspace%get_last_dof_owned()"
                    in result)
            assert ("loop1_stop = f2_proxy%vspace%get_last_dof_owned()"
                    in result)
            assert ("loop2_stop = f3_proxy%vspace%get_last_dof_owned()"
                    in result)
        code = (
            "      !$omp parallel default(shared), private(df)\n"
            "      !$omp do schedule(static)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = fred\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp do schedule(static)\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        f2_data(df) = 3.0_r_def\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp do schedule(static)\n"
            "      DO df=loop2_start,loop2_stop\n"
            "        f3_data(df) = ginger\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp end parallel\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop(s)\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      CALL f2_proxy%set_dirty()\n"
            "      CALL f3_proxy%set_dirty()\n")
        assert code in result
    else:  # distmem is False. annexed can be True or False
        assert "loop0_stop = undf_aspc1_f1" in result
        assert "loop1_stop = undf_aspc1_f2" in result
        assert "loop2_stop = undf_aspc1_f3" in result
        assert (
            "      !$omp parallel default(shared), private(df)\n"
            "      !$omp do schedule(static)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = fred\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp do schedule(static)\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        f2_data(df) = 3.0_r_def\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp do schedule(static)\n"
            "      DO df=loop2_start,loop2_stop\n"
            "        f3_data(df) = ginger\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp end parallel") in result


def test_builtin_loop_fuse_do(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that we generate correct code if an OpenMP DO is applied
    to multiple loop fused builtins. We need to assert it is safe to
    perform loop fusion. Also test with and without annexed dofs
    being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    psy, invoke = get_invoke("15.14.2_multiple_set_kernels.f90", TEST_API,
                             idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    ftrans = LFRicLoopFuseTrans()
    ftrans.apply(schedule[0], schedule[1], {"same_space": True})
    ftrans.apply(schedule[0], schedule[1], {"same_space": True})

    olooptrans = Dynamo0p3OMPLoopTrans()
    ptrans = OMPParallelTrans()

    # Put an OMP PARALLEL around the loop
    children = schedule[0]
    ptrans.apply(children)
    # Put an OMP DO around the loop
    olooptrans.apply(schedule[0].dir_body[0])
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if dist_mem:  # annexed can be True or False
        if annexed:
            assert ("loop0_stop = f1_proxy%vspace%get_last_dof_annexed()"
                    in result)
        else:
            assert ("loop0_stop = f1_proxy%vspace%get_last_dof_owned()"
                    in result)
        code = (
            "      !$omp parallel default(shared), private(df)\n"
            "      !$omp do schedule(static)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = fred\n"
            "        f2_data(df) = 3.0_r_def\n"
            "        f3_data(df) = ginger\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp end parallel\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop(s)\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      CALL f2_proxy%set_dirty()\n"
            "      CALL f3_proxy%set_dirty()\n"
            "      !\n")
        assert code in result
    else:  # distmem is False. annexed can be True or False
        assert "loop0_stop = undf_aspc1_f1" in result
        assert (
            "      !$omp parallel default(shared), private(df)\n"
            "      !$omp do schedule(static)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = fred\n"
            "        f2_data(df) = 3.0_r_def\n"
            "        f3_data(df) = ginger\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp end parallel") in result


def test_reduction_real_pdo(tmpdir, dist_mem):
    ''' Test that we generate a correct OpenMP PARALLEL DO reduction for
    a real scalar summed in a built-in. We use inner product in this case.

    '''
    psy, invoke = get_invoke("15.9.1_X_innerproduct_Y_builtin.f90",
                             TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    otrans = DynamoOMPParallelLoopTrans()
    # Apply OpenMP parallelisation to the loop
    otrans.apply(schedule.children[0])
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if dist_mem:
        assert "loop0_stop = f1_proxy%vspace%get_last_dof_owned()" in code
        assert (
            "      !$omp parallel do default(shared), private(df), "
            "schedule(static), reduction(+:asum)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        asum = asum + f1_data(df) * f2_data(df)\n"
            "      END DO\n"
            "      !$omp end parallel do\n"
            "      global_sum%value = asum\n"
            "      asum = global_sum%get_sum()\n") in code

    else:
        assert "loop0_stop = undf_aspc1_f1" in code
        assert (
            "      !$omp parallel do default(shared), private(df), "
            "schedule(static), reduction(+:asum)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        asum = asum + f1_data(df) * f2_data(df)\n"
            "      END DO\n"
            "      !$omp end parallel do\n") in code


def test_reduction_real_do(tmpdir, dist_mem):
    ''' Test that we generate a correct OpenMP DO reduction for a real
    scalar summed in a built-in. We use inner product in this case.

    '''
    psy, invoke = get_invoke("15.9.1_X_innerproduct_Y_builtin.f90",
                             TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    otrans = Dynamo0p3OMPLoopTrans()
    rtrans = OMPParallelTrans()
    # Apply an OpenMP do directive to the loop
    otrans.apply(schedule.children[0], {"reprod": False})
    # Apply an OpenMP Parallel directive around the OpenMP do directive
    rtrans.apply(schedule.children[0])
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if dist_mem:
        assert "loop0_stop = f1_proxy%vspace%get_last_dof_owned()\n" in code
        assert (
            "      !$omp parallel default(shared), private(df)\n"
            "      !$omp do schedule(static), reduction(+:asum)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        asum = asum + f1_data(df) * f2_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp end parallel\n"
            "      global_sum%value = asum\n"
            "      asum = global_sum%get_sum()\n") in code
    else:
        assert "loop0_stop = undf_aspc1_f1\n" in code
        assert (
            "      !$omp parallel default(shared), private(df)\n"
            "      !$omp do schedule(static), reduction(+:asum)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        asum = asum + f1_data(df) * f2_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp end parallel\n") in code


def test_multi_reduction_real_pdo(tmpdir, dist_mem):
    ''' Test that we generate a correct OpenMP PARALLEL DO reduction for a
    real scalar summed in a built-in. We use inner product in this case.

    '''
    psy, invoke = get_invoke("15.15.1_two_same_builtin_reductions.f90",
                             TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    otrans = DynamoOMPParallelLoopTrans()
    # Apply OpenMP parallelisation to the loop
    for child in schedule.children:
        if isinstance(child, Loop):
            otrans.apply(child)
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if dist_mem:
        assert "loop0_stop = f1_proxy%vspace%get_last_dof_owned()\n" in code
        assert "loop1_stop = f1_proxy%vspace%get_last_dof_owned()\n" in code
        assert (
            "      ! Zero summation variables\n"
            "      !\n"
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      !$omp parallel do default(shared), private(df), "
            "schedule(static), reduction(+:asum)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        asum = asum + f1_data(df) * f2_data(df)\n"
            "      END DO\n"
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
            "      DO df=loop1_start,loop1_stop\n"
            "        asum = asum + f1_data(df) * f2_data(df)\n"
            "      END DO\n"
            "      !$omp end parallel do\n"
            "      global_sum%value = asum\n"
            "      asum = global_sum%get_sum()\n") in code
    else:
        assert "loop0_stop = undf_aspc1_f1\n" in code
        assert "loop1_stop = undf_aspc1_f1\n" in code
        assert (
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      !$omp parallel do default(shared), private(df), "
            "schedule(static), reduction(+:asum)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        asum = asum + f1_data(df) * f2_data(df)\n"
            "      END DO\n"
            "      !$omp end parallel do\n"
            "      !\n"
            "      ! Zero summation variables\n"
            "      !\n"
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      !$omp parallel do default(shared), private(df), "
            "schedule(static), reduction(+:asum)\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        asum = asum + f1_data(df) * f2_data(df)\n"
            "      END DO\n"
            "      !$omp end parallel do\n") in code


def test_reduction_after_normal_real_do(tmpdir, monkeypatch, annexed,
                                        dist_mem):
    ''' Test that we produce correct code when we have a reduction after
    a "normal" built-in and we use OpenMP DO loops for parallelisation
    with a single parallel region over all calls. Also test with and
    without annexed dofs being computed as this affects the generated
    code.

    '''
    file_name = "15.17.2_one_standard_builtin_one_reduction.f90"
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    psy, invoke = get_invoke(file_name, TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    otrans = Dynamo0p3OMPLoopTrans()
    rtrans = OMPParallelTrans()
    # Apply an OpenMP do to the loop
    for child in schedule.children:
        if isinstance(child, Loop):
            otrans.apply(child, {"reprod": False})
    # Apply an OpenMP Parallel for all loops
    rtrans.apply(schedule.children[0:2])
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if dist_mem:  # annexed can be True or False
        if annexed:
            assert ("loop0_stop = f1_proxy%vspace%get_last_dof_annexed()"
                    in result)
            assert ("loop1_stop = f1_proxy%vspace%get_last_dof_owned()"
                    in result)
        else:
            assert ("loop0_stop = f1_proxy%vspace%get_last_dof_owned()"
                    in result)
            assert ("loop1_stop = f1_proxy%vspace%get_last_dof_owned()"
                    in result)
        expected_output = (
            "      ! Zero summation variables\n"
            "      !\n"
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      !$omp parallel default(shared), private(df)\n"
            "      !$omp do schedule(static)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = bvalue * f1_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp do schedule(static), reduction(+:asum)\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        asum = asum + f1_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp end parallel\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop(s)\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      !\n"
            "      ! End of set dirty/clean section for above loop(s)\n"
            "      !\n"
            "      global_sum%value = asum\n"
            "      asum = global_sum%get_sum()")
    else:  # not distmem. annexed can be True or False
        assert "loop0_stop = undf_aspc1_f1" in result
        assert "loop1_stop = undf_aspc1_f1" in result
        expected_output = (
            "      ! Zero summation variables\n"
            "      !\n"
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      !$omp parallel default(shared), private(df)\n"
            "      !$omp do schedule(static)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = bvalue * f1_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp do schedule(static), reduction(+:asum)\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        asum = asum + f1_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp end parallel")
    assert expected_output in result


def test_reprod_red_after_normal_real_do(tmpdir, monkeypatch, annexed,
                                         dist_mem):
    ''' Test that we produce correct code when we have a reproducible
    reduction after a "normal" built-in and we use OpenMP DO loops for
    parallelisation with a single parallel region over all calls. Also
    test with and without annexed dofs being computed as this affects
    the generated code.

    '''
    file_name = "15.17.2_one_standard_builtin_one_reduction.f90"
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    psy, invoke = get_invoke(file_name, TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    otrans = Dynamo0p3OMPLoopTrans()
    rtrans = OMPParallelTrans()
    # Apply an OpenMP do to the loop
    for child in schedule.children:
        if isinstance(child, Loop):
            otrans.apply(child, {"reprod": True})
    # Apply an OpenMP Parallel for all loops
    rtrans.apply(schedule.children[0:2])
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if dist_mem:  # annexed can be True or False
        if annexed:
            assert ("loop0_stop = f1_proxy%vspace%get_last_dof_annexed()"
                    in result)
        else:
            assert ("loop0_stop = f1_proxy%vspace%get_last_dof_owned()"
                    in result)
        assert "loop1_stop = f1_proxy%vspace%get_last_dof_owned()" in result
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
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = bvalue * f1_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp do schedule(static)\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        l_asum(1,th_idx) = l_asum(1,th_idx) + f1_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp end parallel\n"
            "      !\n"
            "      ! sum the partial results sequentially\n"
            "      !\n"
            "      DO th_idx=1,nthreads\n"
            "        asum = asum+l_asum(1,th_idx)\n"
            "      END DO\n"
            "      DEALLOCATE (l_asum)\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop(s)\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      !\n"
            "      ! End of set dirty/clean section for above loop(s)\n"
            "      !\n"
            "      global_sum%value = asum\n"
            "      asum = global_sum%get_sum()")
    else:  # not distmem. annexed can be True or False
        assert "loop0_stop = undf_aspc1_f1" in result
        assert "loop1_stop = undf_aspc1_f1" in result
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
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = bvalue * f1_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp do schedule(static)\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        l_asum(1,th_idx) = l_asum(1,th_idx) + f1_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp end parallel\n"
            "      !\n"
            "      ! sum the partial results sequentially\n"
            "      !\n"
            "      DO th_idx=1,nthreads\n"
            "        asum = asum+l_asum(1,th_idx)\n"
            "      END DO\n"
            "      DEALLOCATE (l_asum)\n")
    assert expected_output in result


def test_two_reductions_real_do(tmpdir, dist_mem):
    ''' Test that we produce correct code when we have more than one
    built-in with a reduction, with each reduction using a different
    variable, and we use OpenMP DO loops for parallelisation with a
    single parallel region over all calls.

    '''
    file_name = "15.16.1_two_different_builtin_reductions.f90"
    psy, invoke = get_invoke(file_name, TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    if dist_mem:
        # Move the first global sum after the second loop
        mtrans = MoveTrans()
        mtrans.apply(schedule.children[1], schedule.children[2],
                     {"position": "after"})
    otrans = Dynamo0p3OMPLoopTrans()
    rtrans = OMPParallelTrans()
    # Apply an OpenMP do to the loop
    for child in schedule.children:
        if isinstance(child, Loop):
            otrans.apply(child, {"reprod": False})
    # Apply an OpenMP Parallel for all loops
    rtrans.apply(schedule.children[0:2])
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if dist_mem:
        assert "loop0_stop = f1_proxy%vspace%get_last_dof_owned()\n" in result
        assert "loop1_stop = f1_proxy%vspace%get_last_dof_owned()\n" in result
        expected_output = (
            "      ! Zero summation variables\n"
            "      !\n"
            "      asum = 0.0_r_def\n"
            "      bsum = 0.0_r_def\n"
            "      !\n"
            "      !$omp parallel default(shared), private(df)\n"
            "      !$omp do schedule(static), reduction(+:asum)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        asum = asum + f1_data(df) * f2_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp do schedule(static), reduction(+:bsum)\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        bsum = bsum + f1_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp end parallel\n"
            "      global_sum%value = asum\n"
            "      asum = global_sum%get_sum()\n"
            "      global_sum%value = bsum\n"
            "      bsum = global_sum%get_sum()")
    else:
        assert "loop0_stop = undf_aspc1_f1" in result
        assert "loop1_stop = undf_aspc1_f1" in result
        expected_output = (
            "      ! Zero summation variables\n"
            "      !\n"
            "      asum = 0.0_r_def\n"
            "      bsum = 0.0_r_def\n"
            "      !\n"
            "      !$omp parallel default(shared), private(df)\n"
            "      !$omp do schedule(static), reduction(+:asum)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        asum = asum + f1_data(df) * f2_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp do schedule(static), reduction(+:bsum)\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        bsum = bsum + f1_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp end parallel")
    assert expected_output in result


def test_two_reprod_reductions_real_do(tmpdir, dist_mem):
    ''' Test that we produce correct code when we have more than one
    built-in with a reproducible reduction, with each reduction using a
    different variable, and we use OpenMP DO loops for parallelisation
    with a single parallel region over all calls.

    '''
    file_name = "15.16.1_two_different_builtin_reductions.f90"
    psy, invoke = get_invoke(file_name, TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    if dist_mem:
        # Move the first global sum after the second loop
        mtrans = MoveTrans()
        mtrans.apply(schedule.children[1], schedule.children[2],
                     {"position": "after"})
    otrans = Dynamo0p3OMPLoopTrans()
    rtrans = OMPParallelTrans()
    # Apply an OpenMP do to the loop
    for child in schedule.children:
        if isinstance(child, Loop):
            otrans.apply(child, {"reprod": True})
    # Apply an OpenMP Parallel for all loops
    rtrans.apply(schedule.children[0:2])
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if dist_mem:
        assert "loop0_stop = f1_proxy%vspace%get_last_dof_owned()" in result
        assert "loop1_stop = f1_proxy%vspace%get_last_dof_owned()" in result
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
            "      DO df=loop0_start,loop0_stop\n"
            "        l_asum(1,th_idx) = l_asum(1,th_idx) + "
            "f1_data(df) * f2_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp do schedule(static)\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        l_bsum(1,th_idx) = l_bsum(1,th_idx) + f1_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp end parallel\n"
            "      !\n"
            "      ! sum the partial results sequentially\n"
            "      !\n"
            "      DO th_idx=1,nthreads\n"
            "        asum = asum+l_asum(1,th_idx)\n"
            "      END DO\n"
            "      DEALLOCATE (l_asum)\n"
            "      DO th_idx=1,nthreads\n"
            "        bsum = bsum+l_bsum(1,th_idx)\n"
            "      END DO\n"
            "      DEALLOCATE (l_bsum)\n"
            "      global_sum%value = asum\n"
            "      asum = global_sum%get_sum()\n"
            "      global_sum%value = bsum\n"
            "      bsum = global_sum%get_sum()")
    else:
        assert "loop0_stop = undf_aspc1_f1" in result
        assert "loop1_stop = undf_aspc1_f1" in result
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
            "      DO df=loop0_start,loop0_stop\n"
            "        l_asum(1,th_idx) = l_asum(1,th_idx) + "
            "f1_data(df) * f2_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp do schedule(static)\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        l_bsum(1,th_idx) = l_bsum(1,th_idx) + f1_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp end parallel\n"
            "      !\n"
            "      ! sum the partial results sequentially\n"
            "      !\n"
            "      DO th_idx=1,nthreads\n"
            "        asum = asum+l_asum(1,th_idx)\n"
            "      END DO\n"
            "      DEALLOCATE (l_asum)\n"
            "      DO th_idx=1,nthreads\n"
            "        bsum = bsum+l_bsum(1,th_idx)\n"
            "      END DO\n"
            "      DEALLOCATE (l_bsum)")
    assert expected_output in result


def test_multi_reduction_same_name_real_do():
    '''test that we raise an exception when we have multiple reductions in
    an invoke with the same name as this is not supported (it would
    cause incorrect code to be created in certain cases). '''
    file_name = "15.15.1_two_same_builtin_reductions.f90"
    for reprod in [True, False]:
        for distmem in [False, True]:
            psy, invoke = get_invoke(file_name, TEST_API,
                                     idx=0, dist_mem=distmem)
            schedule = invoke.schedule
            otrans = Dynamo0p3OMPLoopTrans()
            rtrans = OMPParallelTrans()
            # Apply an OpenMP do to the loop
            for child in schedule.children:
                if isinstance(child, Loop):
                    otrans.apply(child, {"reprod": reprod})
            if distmem:
                # We have to move/delete a
                # global sum to get to the stage where we can raise an
                # error. This makes incorrect code in this example but
                # in general it could be valid to move the global sum
                del schedule.children[1]
            rtrans.apply(schedule.children[0:2])
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
            _, invoke = get_invoke(file_name, TEST_API,
                                   idx=0, dist_mem=distmem)
            schedule = invoke.schedule

            ftrans = LFRicLoopFuseTrans()
            if distmem:
                # We need to remove the global sum. This makes the
                # code invalid in this particular case but allows us
                # to perform our check
                del schedule.children[1]
            with pytest.raises(TransformationError) as excinfo:
                ftrans.apply(schedule.children[0], schedule.children[1],
                             {"same_space": True})
            assert ("Error in LFRicLoopFuseTrans transformation: Cannot "
                    "fuse loops when each loop already contains a "
                    "reduction" in str(excinfo.value))


def test_multi_different_reduction_real_pdo(tmpdir, dist_mem):
    ''' Test that we generate a correct OpenMP PARALLEL DO reduction
    for two different built-ins. We use inner product and sum_X.

    '''
    file_name = "15.16.1_two_different_builtin_reductions.f90"
    psy, invoke = get_invoke(file_name, TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    otrans = DynamoOMPParallelLoopTrans()
    # Apply OpenMP parallelisation to the loop
    for child in schedule.children:
        if isinstance(child, Loop):
            otrans.apply(child)
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if dist_mem:
        assert "loop0_stop = f1_proxy%vspace%get_last_dof_owned()" in code
        assert "loop1_stop = f1_proxy%vspace%get_last_dof_owned()" in code
        assert (
            "      ! Zero summation variables\n"
            "      !\n"
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      !$omp parallel do default(shared), private(df), "
            "schedule(static), reduction(+:asum)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        asum = asum + f1_data(df) * f2_data(df)\n"
            "      END DO\n"
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
            "      DO df=loop1_start,loop1_stop\n"
            "        bsum = bsum + f1_data(df)\n"
            "      END DO\n"
            "      !$omp end parallel do\n"
            "      global_sum%value = bsum\n"
            "      bsum = global_sum%get_sum()\n") in code
    else:
        assert "loop0_stop = undf_aspc1_f1" in code
        assert "loop1_stop = undf_aspc1_f1" in code
        assert (
            "      ! Zero summation variables\n"
            "      !\n"
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      !$omp parallel do default(shared), private(df), "
            "schedule(static), reduction(+:asum)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        asum = asum + f1_data(df) * f2_data(df)\n"
            "      END DO\n"
            "      !$omp end parallel do\n"
            "      !\n"
            "      ! Zero summation variables\n"
            "      !\n"
            "      bsum = 0.0_r_def\n"
            "      !\n"
            "      !$omp parallel do default(shared), private(df), "
            "schedule(static), reduction(+:bsum)\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        bsum = bsum + f1_data(df)\n"
            "      END DO\n"
            "      !$omp end parallel do\n") in code


def test_multi_builtins_red_then_pdo(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that we generate a correct OpenMP PARALLEL DO reduction for
    two different built-ins, first a reduction then not. Also test with
    and without annexed dofs being computed as this affects the
    generated code.

    '''
    file_name = "15.17.1_one_reduction_one_standard_builtin.f90"
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    psy, invoke = get_invoke(file_name, TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    otrans = DynamoOMPParallelLoopTrans()
    # Apply OpenMP parallelisation to the loop
    for child in schedule.children:
        if isinstance(child, Loop):
            otrans.apply(child)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if dist_mem:  # annexed can be True or False
        assert "loop0_stop = f1_proxy%vspace%get_last_dof_owned()" in result
        if annexed:
            assert ("loop1_stop = f1_proxy%vspace%get_last_dof_annexed()"
                    in result)
        else:
            assert ("loop1_stop = f1_proxy%vspace%get_last_dof_owned()"
                    in result)
        code = (
            "      ! Zero summation variables\n"
            "      !\n"
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      !$omp parallel do default(shared), private(df), "
            "schedule(static), reduction(+:asum)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        asum = asum + f1_data(df) * f2_data(df)\n"
            "      END DO\n"
            "      !$omp end parallel do\n"
            "      global_sum%value = asum\n"
            "      asum = global_sum%get_sum()\n"
            "      !$omp parallel do default(shared), private(df), "
            "schedule(static)\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        f1_data(df) = bsum * f1_data(df)\n"
            "      END DO\n"
            "      !$omp end parallel do\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop(s)\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n")
        assert code in result
    else:  # not distmem. annexed can be True or False
        assert "loop0_stop = undf_aspc1_f1" in result
        assert "loop1_stop = undf_aspc1_f1" in result
        assert (
            "      ! Zero summation variables\n"
            "      !\n"
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      !$omp parallel do default(shared), private(df), "
            "schedule(static), reduction(+:asum)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        asum = asum + f1_data(df) * f2_data(df)\n"
            "      END DO\n"
            "      !$omp end parallel do\n"
            "      !$omp parallel do default(shared), private(df), "
            "schedule(static)\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        f1_data(df) = bsum * f1_data(df)\n"
            "      END DO\n"
            "      !$omp end parallel do\n") in result


def test_multi_builtins_red_then_do(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that we generate a correct OpenMP DO reduction for two
    different built-ins, first a reduction then not. Also test with and
    without annexed dofs being computed as this affects the generated
    code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    file_name = "15.17.1_one_reduction_one_standard_builtin.f90"
    psy, invoke = get_invoke(file_name, TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    otrans = Dynamo0p3OMPLoopTrans()
    rtrans = OMPParallelTrans()
    # Apply an OpenMP do to the loop
    for child in schedule.children:
        if isinstance(child, Loop):
            otrans.apply(child, {"reprod": False})
    if dist_mem:  # annexed can be True or False
        mtrans = MoveTrans()
        mtrans.apply(schedule.children[1], schedule.children[2],
                     {"position": "after"})
    rtrans.apply(schedule.children[0:2])
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if dist_mem:  # annexed can be True or False
        assert "loop0_stop = f1_proxy%vspace%get_last_dof_owned()" in result
        if annexed:
            assert ("loop1_stop = f1_proxy%vspace%get_last_dof_annexed()"
                    in result)
        else:
            assert ("loop1_stop = f1_proxy%vspace%get_last_dof_owned()"
                    in result)
        code = (
            "      ! Zero summation variables\n"
            "      !\n"
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      !$omp parallel default(shared), private(df)\n"
            "      !$omp do schedule(static), reduction(+:asum)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        asum = asum + f1_data(df) * f2_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp do schedule(static)\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        f1_data(df) = bsum * f1_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp end parallel\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop(s)\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      !\n"
            "      ! End of set dirty/clean section for above loop(s)\n"
            "      !\n"
            "      global_sum%value = asum\n"
            "      asum = global_sum%get_sum()\n")
        if not annexed:
            code = code.replace("dof_annexed", "dof_owned")
        assert code in result
    else:  # not distmem. annexed can be True or False
        assert "loop0_stop = undf_aspc1_f1" in result
        assert "loop1_stop = undf_aspc1_f1" in result
        assert (
            "      ! Zero summation variables\n"
            "      !\n"
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      !$omp parallel default(shared), private(df)\n"
            "      !$omp do schedule(static), reduction(+:asum)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        asum = asum + f1_data(df) * f2_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp do schedule(static)\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        f1_data(df) = bsum * f1_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp end parallel\n") in result


def test_multi_builtins_red_then_fuse_pdo(tmpdir, monkeypatch, annexed,
                                          dist_mem):
    ''' Test that we generate a correct OpenMP PARALLEL DO reduction for
    two different loop-fused built-ins, first a reduction then not. We
    need to specify that the fused loops are on the same iteration
    space. Also test with and without annexed dofs being computed as
    this affects the validity of the transform.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    file_name = "15.17.1_one_reduction_one_standard_builtin.f90"
    psy, invoke = get_invoke(file_name, TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    ftrans = LFRicLoopFuseTrans()
    if dist_mem and annexed:
        mtrans = MoveTrans()
        mtrans.apply(schedule.children[1], schedule.children[2],
                     {"position": "after"})
        with pytest.raises(TransformationError) as excinfo:
            ftrans.apply(schedule.children[0], schedule.children[1],
                         {"same_space": True})
        assert "The upper bound names are not the same" in str(excinfo.value)
    else:  # not (distmem and annexed)
        if dist_mem:  # annexed must be False here
            # first move the loop as the global sum is in the way
            mtrans = MoveTrans()
            mtrans.apply(schedule.children[1], schedule.children[2],
                         {"position": "after"})
        rtrans = DynamoOMPParallelLoopTrans()
        ftrans.apply(schedule.children[0], schedule.children[1],
                     {"same_space": True})
        rtrans.apply(schedule.children[0])
        result = str(psy.gen)

        if dist_mem:  # annexed must be False here
            assert ("loop0_stop = f1_proxy%vspace%get_last_dof_owned()" in
                    result)
            code = (
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static), reduction(+:asum)\n"
                "      DO df=loop0_start,loop0_stop\n"
                "        asum = asum + f1_data(df) * f2_data(df)\n"
                "        f1_data(df) = bsum * f1_data(df)\n"
                "      END DO\n"
                "      !$omp end parallel do\n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop(s)\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !\n"
                "      ! End of set dirty/clean section for above loop(s)\n"
                "      !\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n")
        else:  # not distmem. annexed can be True or False
            assert "loop0_stop = undf_aspc1_f1" in result
            code = (
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static), reduction(+:asum)\n"
                "      DO df=loop0_start,loop0_stop\n"
                "        asum = asum + f1_data(df) * f2_data(df)\n"
                "        f1_data(df) = bsum * f1_data(df)\n"
                "      END DO\n"
                "      !$omp end parallel do\n")
        assert code in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_multi_builtins_red_then_fuse_do(tmpdir, monkeypatch, annexed,
                                         dist_mem):
    ''' Test that we generate a correct OpenMP DO reduction for two
    different loop-fused built-ins, first a reduction then not. We need
    to specify that the fused loops are on the same iteration
    space. Also test with and without annexed dofs being computed as
    this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    file_name = "15.17.1_one_reduction_one_standard_builtin.f90"
    psy, invoke = get_invoke(file_name, TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    ftrans = LFRicLoopFuseTrans()
    if dist_mem and annexed:
        mtrans = MoveTrans()
        mtrans.apply(schedule.children[1], schedule.children[2],
                     {"position": "after"})
        with pytest.raises(TransformationError) as excinfo:
            ftrans.apply(schedule.children[0], schedule.children[1],
                         {"same_space": True})
        assert "The upper bound names are not the same" in str(excinfo.value)
    else:  # not (distmem and annexed)
        if dist_mem:  # annexed must be False here
            mtrans = MoveTrans()
            mtrans.apply(schedule.children[1], schedule.children[2],
                         {"position": "after"})
        rtrans = OMPParallelTrans()
        otrans = Dynamo0p3OMPLoopTrans()
        ftrans.apply(schedule.children[0], schedule.children[1],
                     {"same_space": True})
        otrans.apply(schedule.children[0], {"reprod": False})
        rtrans.apply(schedule.children[0])
        result = str(psy.gen)

        if dist_mem:  # annexed must be False here
            assert ("loop0_stop = f1_proxy%vspace%get_last_dof_owned()"
                    in result)
            code = (
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static), reduction(+:asum)\n"
                "      DO df=loop0_start,loop0_stop\n"
                "        asum = asum + f1_data(df) * f2_data(df)\n"
                "        f1_data(df) = bsum * f1_data(df)\n"
                "      END DO\n"
                "      !$omp end do\n"
                "      !$omp end parallel\n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop(s)\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !\n"
                "      ! End of set dirty/clean section for above loop(s)\n"
                "      !\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n")
        else:  # not distmem, annexed is True or False
            assert "loop0_stop = undf_aspc1_f1" in result
            code = (
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static), reduction(+:asum)\n"
                "      DO df=loop0_start,loop0_stop\n"
                "        asum = asum + f1_data(df) * f2_data(df)\n"
                "        f1_data(df) = bsum * f1_data(df)\n"
                "      END DO\n"
                "      !$omp end do\n"
                "      !$omp end parallel\n")
        assert code in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_multi_builtins_usual_then_red_pdo(tmpdir, monkeypatch, annexed,
                                           dist_mem):
    ''' Test that we generate a correct OpenMP PARALLEL DO reduction for
    two different built-ins, first a standard built-in then a
    reduction. Also test with and without annexed dofs being computed
    as this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    file_name = "15.17.2_one_standard_builtin_one_reduction.f90"
    psy, invoke = get_invoke(file_name, TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    otrans = DynamoOMPParallelLoopTrans()
    # Apply OpenMP parallelisation to the loop
    for child in schedule.children:
        if isinstance(child, Loop):
            otrans.apply(child)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    if dist_mem:  # annexed can be True or False
        if annexed:
            assert ("loop0_stop = f1_proxy%vspace%get_last_dof_annexed()"
                    in result)
        else:
            assert ("loop0_stop = f1_proxy%vspace%get_last_dof_owned()"
                    in result)
        assert "loop1_stop = f1_proxy%vspace%get_last_dof_owned()" in result
        code = (
            "      !$omp parallel do default(shared), private(df), "
            "schedule(static)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = bvalue * f1_data(df)\n"
            "      END DO\n"
            "      !$omp end parallel do\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop(s)\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      !\n"
            "      ! End of set dirty/clean section for above loop(s)\n"
            "      !\n"
            "      !\n"
            "      ! Zero summation variables\n"
            "      !\n"
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      !$omp parallel do default(shared), private(df), "
            "schedule(static), reduction(+:asum)\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        asum = asum + f1_data(df)\n"
            "      END DO\n"
            "      !$omp end parallel do\n"
            "      global_sum%value = asum\n"
            "      asum = global_sum%get_sum()\n")
        assert code in result
    else:  # not distmem. annexed can be True or False
        assert "loop0_stop = undf_aspc1_f1" in result
        assert "loop1_stop = undf_aspc1_f1" in result
        assert (
            "      !$omp parallel do default(shared), private(df), "
            "schedule(static)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        f1_data(df) = bvalue * f1_data(df)\n"
            "      END DO\n"
            "      !$omp end parallel do\n"
            "      !\n"
            "      ! Zero summation variables\n"
            "      !\n"
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      !$omp parallel do default(shared), private(df), "
            "schedule(static), reduction(+:asum)\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        asum = asum + f1_data(df)\n"
            "      END DO\n"
            "      !$omp end parallel do\n") in result


def test_builtins_usual_then_red_fuse_pdo(tmpdir, monkeypatch, annexed,
                                          dist_mem):
    ''' Test that we generate a correct OpenMP PARALLEL DO reduction for
    two different loop-fused built-ins, first a normal built-in then a
    reduction. We need to specify that the fused loops iterate over
    the same space. Also test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    file_name = "15.17.2_one_standard_builtin_one_reduction.f90"
    psy, invoke = get_invoke(file_name, TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    ftrans = LFRicLoopFuseTrans()

    if dist_mem and annexed:
        with pytest.raises(TransformationError) as excinfo:
            ftrans.apply(schedule.children[0], schedule.children[1],
                         {"same_space": True})
        assert "The upper bound names are not the same" in str(excinfo.value)
    else:  # not (distmem and annexed)
        otrans = DynamoOMPParallelLoopTrans()
        ftrans.apply(schedule.children[0], schedule.children[1],
                     {"same_space": True})
        otrans.apply(schedule.children[0])
        result = str(psy.gen)

        if dist_mem:  # annexed is False here
            assert ("loop0_stop = f1_proxy%vspace%get_last_dof_owned()"
                    in result)
            code = (
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static), reduction(+:asum)\n"
                "      DO df=loop0_start,loop0_stop\n"
                "        f1_data(df) = bvalue * f1_data(df)\n"
                "        asum = asum + f1_data(df)\n"
                "      END DO\n"
                "      !$omp end parallel do\n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop(s)\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !\n"
                "      ! End of set dirty/clean section for above loop(s)\n"
                "      !\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n")
        else:  # not distmem. annexed can be True or False
            assert "loop0_stop = undf_aspc1_f1" in result
            code = (
                "      ! Zero summation variables\n"
                "      !\n"
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel do default(shared), private(df), "
                "schedule(static), reduction(+:asum)\n"
                "      DO df=loop0_start,loop0_stop\n"
                "        f1_data(df) = bvalue * f1_data(df)\n"
                "        asum = asum + f1_data(df)\n"
                "      END DO\n"
                "      !$omp end parallel do\n")
        assert code in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_builtins_usual_then_red_fuse_do(tmpdir, monkeypatch, annexed,
                                         dist_mem):
    ''' Test that we generate a correct OpenMP PARALLEL DO reduction for
    two different loop-fused built-ins, first a normal built-in then a
    reduction. We need to specify that the fused loops iterate over
    the same space. Also test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    file_name = "15.17.2_one_standard_builtin_one_reduction.f90"
    psy, invoke = get_invoke(file_name, TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    ftrans = LFRicLoopFuseTrans()

    if dist_mem and annexed:
        with pytest.raises(TransformationError) as excinfo:
            ftrans.apply(schedule.children[0], schedule.children[1],
                         {"same_space": True})
        assert "The upper bound names are not the same" in str(excinfo.value)
    else:  # not (distmem and annexed)
        rtrans = OMPParallelTrans()
        otrans = Dynamo0p3OMPLoopTrans()
        ftrans.apply(schedule.children[0], schedule.children[1],
                     {"same_space": True})
        otrans.apply(schedule.children[0], {"reprod": False})
        rtrans.apply(schedule.children[0])
        result = str(psy.gen)

        if dist_mem:  # annexed is False here
            assert ("loop0_stop = f1_proxy%vspace%get_last_dof_owned()"
                    in result)
            code = (
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static), reduction(+:asum)\n"
                "      DO df=loop0_start,loop0_stop\n"
                "        f1_data(df) = bvalue * f1_data(df)\n"
                "        asum = asum + f1_data(df)\n"
                "      END DO\n"
                "      !$omp end do\n"
                "      !$omp end parallel\n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop(s)\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !\n"
                "      ! End of set dirty/clean section for above loop(s)\n"
                "      !\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n")
        else:  # not distmem. annexed can be True or False
            assert "loop0_stop = undf_aspc1_f1" in result
            code = (
                "      asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df)\n"
                "      !$omp do schedule(static), reduction(+:asum)\n"
                "      DO df=loop0_start,loop0_stop\n"
                "        f1_data(df) = bvalue * f1_data(df)\n"
                "        asum = asum + f1_data(df)\n"
                "      END DO\n"
                "      !$omp end do\n"
                "      !$omp end parallel\n")
        assert code in result

    assert LFRicBuild(tmpdir).code_compiles(psy)

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

    _, invoke = get_invoke("15.18.1_builtins_reduction_fuse_error.f90",
                           TEST_API, idx=0, dist_mem=False)
    schedule = invoke.schedule
    ftrans = LFRicLoopFuseTrans()
    ftrans.apply(schedule.children[0], schedule.children[1],
                 {"same_space": True})
    with pytest.raises(TransformationError) as excinfo:
        ftrans.apply(schedule.children[0], schedule.children[1],
                     {"same_space": True})
    assert ("Cannot fuse loops as the first loop has a reduction and "
            "the second loop reads the result of the "
            "reduction") in str(excinfo.value)


def test_loop_fuse_error(dist_mem):
    '''Test that we raise an exception in loop fusion if one or more of
    the loops has an any_space iteration space.'''
    _, invoke = get_invoke("15.14.2_multiple_set_kernels.f90",
                           TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    ftrans = LFRicLoopFuseTrans()
    with pytest.raises(TransformationError) as excinfo:
        ftrans.apply(schedule.children[0], schedule.children[1])
    assert ("One or more of the iteration spaces is unknown "
            "('ANY_SPACE') so loop fusion might be "
            "invalid") in str(excinfo.value)


# Repeat the reduction tests for the reproducible version

def test_reprod_reduction_real_do(tmpdir, dist_mem):
    ''' Test that we generate a correct reproducible OpenMP DO reduction
    for a real scalar summed in a built-in. We use inner product in
    this case.

    '''
    psy, invoke = get_invoke("15.9.1_X_innerproduct_Y_builtin.f90",
                             TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    otrans = Dynamo0p3OMPLoopTrans()
    rtrans = OMPParallelTrans()
    # Apply an OpenMP do directive to the loop
    otrans.apply(schedule.children[0], {"reprod": True})
    # Apply an OpenMP Parallel directive around the OpenMP do directive
    rtrans.apply(schedule.children[0])
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

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
    if dist_mem:
        assert "loop0_stop = f1_proxy%vspace%get_last_dof_owned()" in code
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
            "      DO df=loop0_start,loop0_stop\n"
            "        l_asum(1,th_idx) = l_asum(1,th_idx) + f1_data(df) "
            "* f2_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp end parallel\n"
            "      !\n"
            "      ! sum the partial results sequentially\n"
            "      !\n"
            "      DO th_idx=1,nthreads\n"
            "        asum = asum+l_asum(1,th_idx)\n"
            "      END DO\n"
            "      DEALLOCATE (l_asum)\n"
            "      global_sum%value = asum\n"
            "      asum = global_sum%get_sum()") in code
    else:
        assert "loop0_stop = undf_aspc1_f1" in code
        assert (
            "      asum = 0.0_r_def\n"
            "      ALLOCATE (l_asum(8,nthreads))\n"
            "      l_asum = 0.0_r_def\n"
            "      !\n"
            "      !$omp parallel default(shared), private(df,th_idx)\n"
            "      th_idx = omp_get_thread_num()+1\n"
            "      !$omp do schedule(static)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        l_asum(1,th_idx) = l_asum(1,th_idx) + f1_data(df) "
            "* f2_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp end parallel\n"
            "      !\n"
            "      ! sum the partial results sequentially\n"
            "      !\n"
            "      DO th_idx=1,nthreads\n"
            "        asum = asum+l_asum(1,th_idx)\n"
            "      END DO\n"
            "      DEALLOCATE (l_asum)\n") in code


def test_no_global_sum_in_parallel_region():
    '''test that we raise an error if we try to put a parallel region
    around loops with a global sum. '''
    file_name = "15.17.1_one_reduction_one_standard_builtin.f90"
    for distmem in [True]:
        psy, invoke = get_invoke(file_name, TEST_API, idx=0,
                                 dist_mem=distmem)
        schedule = invoke.schedule
        otrans = Dynamo0p3OMPLoopTrans()
        rtrans = OMPParallelTrans()
        # Apply an OpenMP do to the loop
        for child in schedule.children:
            if isinstance(child, Loop):
                otrans.apply(child, {"reprod": True})
        rtrans.apply(schedule.children)
        with pytest.raises(NotImplementedError) as excinfo:
            _ = str(psy.gen)
        assert ("Cannot correctly generate code for an OpenMP parallel region "
                "containing children of different types") in str(excinfo.value)


def test_reprod_builtins_red_then_usual_do(tmpdir, monkeypatch, annexed,
                                           dist_mem):
    ''' Test that we generate a correct reproducible OpenMP DO reduction
    for two different built-ins, first a reduction then not when we
    have reprod set to True. Also test with and without annexed dofs
    being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    file_name = "15.17.1_one_reduction_one_standard_builtin.f90"
    psy, invoke = get_invoke(file_name, TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    otrans = Dynamo0p3OMPLoopTrans()
    rtrans = OMPParallelTrans()
    # Apply an OpenMP do to the loop
    for child in schedule.children:
        if isinstance(child, Loop):
            otrans.apply(child, {"reprod": True})
    if dist_mem:  # annexed can be True or False
        mtrans = MoveTrans()
        mtrans.apply(schedule.children[1], schedule.children[2],
                     {"position": "after"})
    rtrans.apply(schedule.children[0:2])
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

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
    if dist_mem:  # annexed can be True or False
        assert "loop0_stop = f1_proxy%vspace%get_last_dof_owned()" in result
        if annexed:
            assert ("loop1_stop = f1_proxy%vspace%get_last_dof_annexed()"
                    in result)
        else:
            assert ("loop1_stop = f1_proxy%vspace%get_last_dof_owned()"
                    in result)
        code = (
            "      asum = 0.0_r_def\n"
            "      ALLOCATE (l_asum(8,nthreads))\n"
            "      l_asum = 0.0_r_def\n"
            "      !\n"
            "      !$omp parallel default(shared), private(df,th_idx)\n"
            "      th_idx = omp_get_thread_num()+1\n"
            "      !$omp do schedule(static)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        l_asum(1,th_idx) = l_asum(1,th_idx) + f1_data(df) "
            "* f2_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp do schedule(static)\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        f1_data(df) = bsum * f1_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp end parallel\n"
            "      !\n"
            "      ! sum the partial results sequentially\n"
            "      !\n"
            "      DO th_idx=1,nthreads\n"
            "        asum = asum+l_asum(1,th_idx)\n"
            "      END DO\n"
            "      DEALLOCATE (l_asum)\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop(s)\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      !\n"
            "      ! End of set dirty/clean section for above loop(s)\n"
            "      !\n"
            "      global_sum%value = asum\n"
            "      asum = global_sum%get_sum()\n")
        assert code in result
    else:  # not distmem. annexed can be True or False
        assert "loop0_stop = undf_aspc1_f1" in result
        assert "loop1_stop = undf_aspc1_f1" in result
        assert (
            "      asum = 0.0_r_def\n"
            "      ALLOCATE (l_asum(8,nthreads))\n"
            "      l_asum = 0.0_r_def\n"
            "      !\n"
            "      !$omp parallel default(shared), private(df,th_idx)\n"
            "      th_idx = omp_get_thread_num()+1\n"
            "      !$omp do schedule(static)\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        l_asum(1,th_idx) = l_asum(1,th_idx) + f1_data(df) "
            "* f2_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp do schedule(static)\n"
            "      DO df=loop1_start,loop1_stop\n"
            "        f1_data(df) = bsum * f1_data(df)\n"
            "      END DO\n"
            "      !$omp end do\n"
            "      !$omp end parallel\n"
            "      !\n"
            "      ! sum the partial results sequentially\n"
            "      !\n"
            "      DO th_idx=1,nthreads\n"
            "        asum = asum+l_asum(1,th_idx)\n"
            "      END DO\n"
            "      DEALLOCATE (l_asum)\n") in result


def test_repr_bltins_red_then_usual_fuse_do(tmpdir, monkeypatch, annexed,
                                            dist_mem):
    ''' Test that we generate a correct reproducible OpenMP DO reduction
    for two different loop-fused built-ins, first a reduction then
    not. We need to specify that the fused loops are on the same
    iteration space. Also test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    file_name = "15.17.1_one_reduction_one_standard_builtin.f90"
    psy, invoke = get_invoke(file_name, TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    ftrans = LFRicLoopFuseTrans()

    if dist_mem:  # annexed can be True or False
        mtrans = MoveTrans()
        mtrans.apply(schedule.children[1], schedule.children[2],
                     {"position": "after"})
    if dist_mem and annexed:
        # we can't loop fuse as the loop bounds differ
        with pytest.raises(TransformationError) as excinfo:
            ftrans.apply(schedule.children[0], schedule.children[1],
                         {"same_space": True})
        assert "The upper bound names are not the same" in str(excinfo.value)
    else:  # not (distmem and annexed)
        # we can loop fuse as the loop bounds are the same
        ftrans.apply(schedule.children[0], schedule.children[1],
                     {"same_space": True})
        rtrans = OMPParallelTrans()
        otrans = Dynamo0p3OMPLoopTrans()
        otrans.apply(schedule.children[0], {"reprod": "True"})
        rtrans.apply(schedule.children[0])
        result = str(psy.gen)

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
        if dist_mem:  # annexed is False here
            assert ("loop0_stop = f1_proxy%vspace%get_last_dof_owned()"
                    in result)
            assert (
                "      asum = 0.0_r_def\n"
                "      ALLOCATE (l_asum(8,nthreads))\n"
                "      l_asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df,th_idx)\n"
                "      th_idx = omp_get_thread_num()+1\n"
                "      !$omp do schedule(static)\n"
                "      DO df=loop0_start,loop0_stop\n"
                "        l_asum(1,th_idx) = l_asum(1,th_idx) + "
                "f1_data(df) * f2_data(df)\n"
                "        f1_data(df) = bsum * f1_data(df)\n"
                "      END DO\n"
                "      !$omp end do\n"
                "      !$omp end parallel\n"
                "      !\n"
                "      ! sum the partial results sequentially\n"
                "      !\n"
                "      DO th_idx=1,nthreads\n"
                "        asum = asum+l_asum(1,th_idx)\n"
                "      END DO\n"
                "      DEALLOCATE (l_asum)\n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop(s)\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !\n"
                "      ! End of set dirty/clean section for above loop(s)\n"
                "      !\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n") in result
        else:  # not distmem. annexed can be True or False
            assert "loop0_stop = undf_aspc1_f1" in result
            assert (
                "      asum = 0.0_r_def\n"
                "      ALLOCATE (l_asum(8,nthreads))\n"
                "      l_asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df,th_idx)\n"
                "      th_idx = omp_get_thread_num()+1\n"
                "      !$omp do schedule(static)\n"
                "      DO df=loop0_start,loop0_stop\n"
                "        l_asum(1,th_idx) = l_asum(1,th_idx) + "
                "f1_data(df) * f2_data(df)\n"
                "        f1_data(df) = bsum * f1_data(df)\n"
                "      END DO\n"
                "      !$omp end do\n"
                "      !$omp end parallel\n"
                "      !\n"
                "      ! sum the partial results sequentially\n"
                "      !\n"
                "      DO th_idx=1,nthreads\n"
                "        asum = asum+l_asum(1,th_idx)\n"
                "      END DO\n"
                "      DEALLOCATE (l_asum)\n") in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_repr_bltins_usual_then_red_fuse_do(tmpdir, monkeypatch, annexed,
                                            dist_mem):
    ''' Test that we generate a correct OpenMP DO reduction for two
    different loop-fused built-ins, first a normal built-in then a
    reduction. We need to specify that the fused loops iterate over
    the same space. Also test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    file_name = "15.17.2_one_standard_builtin_one_reduction.f90"
    psy, invoke = get_invoke(file_name, TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    ftrans = LFRicLoopFuseTrans()

    if dist_mem and annexed:
        with pytest.raises(TransformationError) as excinfo:
            ftrans.apply(schedule.children[0], schedule.children[1],
                         {"same_space": True})
        assert "The upper bound names are not the same" in str(excinfo.value)
    else:  # not distmem and annexed
        rtrans = OMPParallelTrans()
        otrans = Dynamo0p3OMPLoopTrans()
        ftrans.apply(schedule.children[0], schedule.children[1],
                     {"same_space": True})
        otrans.apply(schedule.children[0], {"reprod": True})
        rtrans.apply(schedule.children[0])
        result = str(psy.gen)

        assert "      INTEGER th_idx\n" in result
        if dist_mem:  # annexed is False here
            assert ("loop0_stop = f1_proxy%vspace%get_last_dof_owned()"
                    in result)
            assert (
                "      asum = 0.0_r_def\n"
                "      ALLOCATE (l_asum(8,nthreads))\n"
                "      l_asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df,th_idx)\n"
                "      th_idx = omp_get_thread_num()+1\n"
                "      !$omp do schedule(static)\n"
                "      DO df=loop0_start,loop0_stop\n"
                "        f1_data(df) = bvalue * f1_data(df)\n"
                "        l_asum(1,th_idx) = l_asum(1,th_idx) + "
                "f1_data(df)\n"
                "      END DO\n"
                "      !$omp end do\n"
                "      !$omp end parallel\n"
                "      !\n"
                "      ! sum the partial results sequentially\n"
                "      !\n"
                "      DO th_idx=1,nthreads\n"
                "        asum = asum+l_asum(1,th_idx)\n"
                "      END DO\n"
                "      DEALLOCATE (l_asum)\n"
                "      !\n"
                "      ! Set halos dirty/clean for fields modified in the "
                "above loop(s)\n"
                "      !\n"
                "      CALL f1_proxy%set_dirty()\n"
                "      !\n"
                "      ! End of set dirty/clean section for above loop(s)\n"
                "      !\n"
                "      global_sum%value = asum\n"
                "      asum = global_sum%get_sum()\n") in result
        else:  # distmem is False. annexed can be True or False
            assert "loop0_stop = undf_aspc1_f1" in result
            assert (
                "      asum = 0.0_r_def\n"
                "      ALLOCATE (l_asum(8,nthreads))\n"
                "      l_asum = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df,th_idx)\n"
                "      th_idx = omp_get_thread_num()+1\n"
                "      !$omp do schedule(static)\n"
                "      DO df=loop0_start,loop0_stop\n"
                "        f1_data(df) = bvalue * f1_data(df)\n"
                "        l_asum(1,th_idx) = l_asum(1,th_idx) + "
                "f1_data(df)\n"
                "      END DO\n"
                "      !$omp end do\n"
                "      !$omp end parallel\n"
                "      !\n"
                "      ! sum the partial results sequentially\n"
                "      !\n"
                "      DO th_idx=1,nthreads\n"
                "        asum = asum+l_asum(1,th_idx)\n"
                "      END DO\n"
                "      DEALLOCATE (l_asum)\n") in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_repr_3_builtins_2_reductions_do(tmpdir, dist_mem):
    ''' Test that we generate correct reproducible OpenMP DO reductions
    when we have three different built-ins, first a reduction, then a
    normal built-in then a reduction. '''
    file_name = "15.19.1_three_builtins_two_reductions.f90"
    psy, invoke = get_invoke(file_name, TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    rtrans = OMPParallelTrans()
    otrans = Dynamo0p3OMPLoopTrans()
    for child in schedule.children:
        if isinstance(child, LFRicLoop):
            otrans.apply(child, {"reprod": True})
    for child in schedule.children:
        if isinstance(child, OMPDoDirective):
            rtrans.apply(child)
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert "INTEGER th_idx\n" in code
    if dist_mem:
        assert "loop0_stop = f1_proxy%vspace%get_last_dof_owned()" in code
        assert "loop1_stop = f1_proxy%vspace%get_last_dof_owned()" in code
        assert "loop2_stop = f2_proxy%vspace%get_last_dof_owned()" in code

        for names in [
                {"var": "asum", "lvar": "l_asum", "loop_idx": "0",
                 "rhs": "f1_data(df) * f2_data(df)"},
                {"var": "bsum", "lvar": "l_bsum", "loop_idx": "2",
                 "rhs": "f2_data(df)"}]:
            assert (
                "      " + names["var"] + " = 0.0_r_def\n"
                "      ALLOCATE (" + names["lvar"] + "(8,nthreads))\n"
                "      " + names["lvar"] + " = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df,th_idx)\n"
                "      th_idx = omp_get_thread_num()+1\n"
                "      !$omp do schedule(static)\n"
                "      DO df=loop"+names["loop_idx"]+"_start,"
                "loop"+names["loop_idx"]+"_stop\n"
                "        " + names["lvar"] + "(1,th_idx) = " +
                names["lvar"] + "(1,th_idx) + " + names["rhs"] + "\n"
                "      END DO\n"
                "      !$omp end do\n"
                "      !$omp end parallel\n"
                "      !\n"
                "      ! sum the partial results sequentially\n"
                "      !\n"
                "      DO th_idx=1,nthreads\n"
                "        " + names["var"] + " = " + names["var"] + "+" +
                names["lvar"] + "(1,th_idx)\n"
                "      END DO\n"
                "      DEALLOCATE (" + names["lvar"] + ")\n"
                "      global_sum%value = " + names["var"] + "\n"
                "      " + names["var"] + " = "
                "global_sum%get_sum()\n") in code
    else:
        assert "loop0_stop = undf_aspc1_f1" in code
        assert "loop1_stop = undf_aspc1_f1" in code
        assert "loop2_stop = undf_aspc1_f2" in code

        for names in [
                {"var": "asum", "lvar": "l_asum", "loop_idx": "0",
                 "rhs": "f1_data(df) * f2_data(df)"},
                {"var": "bsum", "lvar": "l_bsum",
                 "loop_idx": "2", "rhs": "f2_data(df)"}]:
            assert (
                "      " + names["var"] + " = 0.0_r_def\n"
                "      ALLOCATE (" + names["lvar"] + "(8,nthreads))\n"
                "      " + names["lvar"] + " = 0.0_r_def\n"
                "      !\n"
                "      !$omp parallel default(shared), private(df,th_idx)\n"
                "      th_idx = omp_get_thread_num()+1\n"
                "      !$omp do schedule(static)\n"
                "      DO df=loop"+names["loop_idx"]+"_start,"
                "loop" + names["loop_idx"]+"_stop\n"
                "        " + names["lvar"] + "(1,th_idx) = " +
                names["lvar"] + "(1,th_idx) + " + names["rhs"] + "\n"
                "      END DO\n"
                "      !$omp end do\n"
                "      !$omp end parallel\n"
                "      !\n"
                "      ! sum the partial results sequentially\n"
                "      !\n"
                "      DO th_idx=1,nthreads\n"
                "        " + names["var"] + " = " + names["var"] + "+" +
                names["lvar"] + "(1,th_idx)\n"
                "      END DO\n"
                "      DEALLOCATE (" + names["lvar"] + ")\n") in code


def test_reprod_view(monkeypatch, annexed, dist_mem):
    '''test that we generate a correct view() for OpenMP do
    reductions. Also test with and without annexed dofs being computed
    as this affects the output.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)

    # Ensure we check for text containing the correct (colour) control codes
    isched = colored("InvokeSchedule", InvokeSchedule._colour)
    ompparallel = colored("OMPParallelDirective", Directive._colour)
    ompdo = colored("OMPDoDirective", Directive._colour)
    ompdefault = colored("OMPDefaultClause", Directive._colour)
    ompprivate = colored("OMPPrivateClause", Directive._colour)
    ompfprivate = colored("OMPFirstprivateClause", Directive._colour)
    gsum = colored("GlobalSum", GlobalSum._colour)
    loop = colored("Loop", Loop._colour)
    call = colored("BuiltIn", BuiltIn._colour)
    sched = colored("Schedule", Schedule._colour)
    lit = colored("Literal", Literal._colour)
    lit_one = lit + "[value:'1', Scalar<INTEGER, UNDEFINED>]\n"
    indent = "    "

    _, invoke = get_invoke("15.19.1_three_builtins_two_reductions.f90",
                           TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    rtrans = OMPParallelTrans()
    otrans = Dynamo0p3OMPLoopTrans()
    for child in schedule.children:
        if isinstance(child, LFRicLoop):
            otrans.apply(child, {"reprod": True})
    for child in schedule.children:
        if isinstance(child, OMPDoDirective):
            rtrans.apply(child)
    result = schedule.view()
    if dist_mem:  # annexed can be True or False
        expected = (
            isched + "[invoke='invoke_0', dm=True]\n" +
            indent + "0: " + ompparallel + "[]\n" +
            2*indent + sched + "[]\n" +
            3*indent + "0: " + ompdo + "[omp_schedule=static,reprod=True]\n" +
            4*indent + sched + "[]\n" +
            5*indent + "0: " + loop + "[type='dof', "
            "field_space='any_space_1', it_space='dof', "
            "upper_bound='ndofs']\n" +
            6*indent + lit + "[value:'NOT_INITIALISED', " +
            "Scalar<INTEGER, UNDEFINED>]\n" +
            6*indent + lit + "[value:'NOT_INITIALISED', " +
            "Scalar<INTEGER, UNDEFINED>]\n" +
            6*indent + lit_one +
            6*indent + sched + "[]\n" +
            7*indent + "0: " + call + " x_innerproduct_y(asum,f1,f2)\n" +
            2*indent + ompdefault + "[default=DefaultClauseTypes.SHARED]\n" +
            2*indent + ompprivate + "[]\n" +
            2*indent + ompfprivate + "[]\n" +
            indent + "1: " + gsum + "[scalar='asum']\n" +
            indent + "2: " + ompparallel + "[]\n" +
            2*indent + sched + "[]\n" +
            3*indent + "0: " + ompdo + "[omp_schedule=static]\n" +
            4*indent + sched + "[]\n" +
            5*indent + "0: " + loop + "[type='dof', "
            "field_space='any_space_1', it_space='dof', "
            "upper_bound='nannexed']\n" +
            6*indent + lit + "[value:'NOT_INITIALISED', " +
            "Scalar<INTEGER, UNDEFINED>]\n" +
            6*indent + lit + "[value:'NOT_INITIALISED', " +
            "Scalar<INTEGER, UNDEFINED>]\n" +
            6*indent + lit_one +
            6*indent + sched + "[]\n" +
            7*indent + "0: " + call + " inc_a_times_x(asum,f1)\n" +
            2*indent + ompdefault + "[default=DefaultClauseTypes.SHARED]\n" +
            2*indent + ompprivate + "[]\n" +
            2*indent + ompfprivate + "[]\n" +
            indent + "3: " + ompparallel + "[]\n" +
            2*indent + sched + "[]\n" +
            3*indent + "0: " + ompdo + "[omp_schedule=static,reprod=True]\n" +
            4*indent + sched + "[]\n" +
            5*indent + "0: " + loop + "[type='dof', "
            "field_space='any_space_1', it_space='dof', "
            "upper_bound='ndofs']\n" +
            6*indent + lit + "[value:'NOT_INITIALISED', " +
            "Scalar<INTEGER, UNDEFINED>]\n" +
            6*indent + lit + "[value:'NOT_INITIALISED', " +
            "Scalar<INTEGER, UNDEFINED>]\n" +
            6*indent + lit_one +
            6*indent + sched + "[]\n" +
            7*indent + "0: " + call + " sum_x(bsum,f2)\n" +
            2*indent + ompdefault + "[default=DefaultClauseTypes.SHARED]\n" +
            2*indent + ompprivate + "[]\n" +
            2*indent + ompfprivate + "[]\n" +
            indent + "4: " + gsum + "[scalar='bsum']\n")
        if not annexed:
            expected = expected.replace("nannexed", "ndofs")
    else:  # not dist_mem. annexed can be True or False
        expected = (
            isched + "[invoke='invoke_0', dm=False]\n" +
            indent + "0: " + ompparallel + "[]\n" +
            2*indent + sched + "[]\n" +
            3*indent + "0: " + ompdo + "[omp_schedule=static,reprod=True]\n" +
            4*indent + sched + "[]\n" +
            5*indent + "0: " + loop + "[type='dof', "
            "field_space='any_space_1', it_space='dof', "
            "upper_bound='ndofs']\n" +
            6*indent + lit + "[value:'NOT_INITIALISED', " +
            "Scalar<INTEGER, UNDEFINED>]\n" +
            6*indent + lit + "[value:'NOT_INITIALISED', " +
            "Scalar<INTEGER, UNDEFINED>]\n" +
            6*indent + lit_one +
            6*indent + sched + "[]\n" +
            7*indent + "0: " + call + " x_innerproduct_y(asum,f1,f2)\n" +
            2*indent + ompdefault + "[default=DefaultClauseTypes.SHARED]\n" +
            2*indent + ompprivate + "[]\n" +
            2*indent + ompfprivate + "[]\n" +
            indent + "1: " + ompparallel + "[]\n" +
            2*indent + sched + "[]\n" +
            3*indent + "0: " + ompdo + "[omp_schedule=static]\n" +
            4*indent + sched + "[]\n" +
            5*indent + "0: " + loop + "[type='dof', "
            "field_space='any_space_1', it_space='dof', "
            "upper_bound='ndofs']\n" +
            6*indent + lit + "[value:'NOT_INITIALISED', " +
            "Scalar<INTEGER, UNDEFINED>]\n" +
            6*indent + lit + "[value:'NOT_INITIALISED', " +
            "Scalar<INTEGER, UNDEFINED>]\n" +
            6*indent + lit_one +
            6*indent + sched + "[]\n" +
            7*indent + "0: " + call + " inc_a_times_x(asum,f1)\n" +
            2*indent + ompdefault + "[default=DefaultClauseTypes.SHARED]\n" +
            2*indent + ompprivate + "[]\n" +
            2*indent + ompfprivate + "[]\n" +
            indent + "2: " + ompparallel + "[]\n" +
            2*indent + sched + "[]\n" +
            3*indent + "0: " + ompdo + "[omp_schedule=static,reprod=True]\n" +
            4*indent + sched + "[]\n" +
            5*indent + "0: " + loop + "[type='dof', "
            "field_space='any_space_1', it_space='dof', "
            "upper_bound='ndofs']\n" +
            6*indent + lit + "[value:'NOT_INITIALISED', " +
            "Scalar<INTEGER, UNDEFINED>]\n" +
            6*indent + lit + "[value:'NOT_INITIALISED', " +
            "Scalar<INTEGER, UNDEFINED>]\n" +
            6*indent + lit_one +
            6*indent + sched + "[]\n" +
            7*indent + "0: " + call + " sum_x(bsum,f2)\n" +
            2*indent + ompdefault + "[default=DefaultClauseTypes.SHARED]\n" +
            2*indent + ompprivate + "[]\n" +
            2*indent + ompfprivate + "[]\n")
    if expected not in result:
        print("Expected ...")
        print(expected)
        print("Found ...")
        print(result)
        assert 0


def test_reductions_reprod():
    ''' Check that the optional reprod argument to reductions() method
    works as expected. '''
    file_name = "15.9.1_X_innerproduct_Y_builtin.f90"
    for reprod in [False, True]:
        for distmem in [True, False]:
            _, invoke = get_invoke(file_name, TEST_API, idx=0,
                                   dist_mem=distmem)
            schedule = invoke.schedule
            otrans = Dynamo0p3OMPLoopTrans()
            rtrans = OMPParallelTrans()
            # Apply an OpenMP do directive to the loop
            otrans.apply(schedule.children[0], {"reprod": reprod})
            # Apply an OpenMP Parallel directive around the OpenMP do directive
            rtrans.apply(schedule.children[0])
            assert len(schedule.reductions(reprod=reprod)) == 1
            assert not schedule.reductions(reprod=not reprod)
            assert len(schedule.reductions()) == 1
            assert (isinstance(schedule.reductions(reprod=reprod)[0],
                               LFRicXInnerproductYKern))


def test_list_multiple_reductions(dist_mem):
    ''' Test that we produce correct reduction lists when there is more
    than one reduction in a OpenMP parallel directive. As only one
    reduction per OpenMP parallel region is currently supported we
    need to modify the internal representation after the
    transformations have been performed to enable this test.

    '''
    _, invoke = get_invoke("15.9.1_X_innerproduct_Y_builtin.f90",
                           TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    otrans = Dynamo0p3OMPLoopTrans()
    rtrans = OMPParallelTrans()
    # Apply an OpenMP do directive to the loop
    otrans.apply(schedule.children[0], {"reprod": False})
    # Apply an OpenMP Parallel directive around the OpenMP do directive
    rtrans.apply(schedule.children[0])
    omp_loop_directive = schedule[0].dir_body[0]
    call = omp_loop_directive.dir_body[0].loop_body[0]
    arg = call.arguments.args[2]
    arg._argument_type = "gh_scalar"
    arg.descriptor._access = AccessType.SUM
    result = omp_loop_directive._reduction_string()
    assert "reduction(+:asum), reduction(+:f2)" in result


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


def test_move_valid_node(tmpdir):
    ''' Test that MoveTrans raises an exception if an invalid node
    argument is passed. '''
    psy, invoke = get_invoke("4.2_multikernel_invokes.f90", TEST_API, idx=0,
                             dist_mem=True)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    schedule = invoke.schedule
    move_trans = MoveTrans()
    with pytest.raises(TransformationError) as excinfo:
        move_trans.apply(None, schedule.children[0])
    assert ("In the Move transformation apply method the "
            "first argument is not a Node") in str(excinfo.value)


def test_move_back():
    '''Test that MoveTrans moves the node backwards to the expected
    location. '''
    _, invoke = get_invoke("15.14.2_multiple_set_kernels.f90", TEST_API,
                           idx=0, dist_mem=True)
    schedule = invoke.schedule
    move_trans = MoveTrans()
    initial_index = 2
    target_index = 0
    orig_arg = schedule.children[initial_index]
    new_arg = schedule.children[target_index]
    assert orig_arg is not new_arg

    move_trans.apply(schedule.children[initial_index],
                     schedule.children[target_index])

    new_arg = schedule.children[target_index]
    assert orig_arg is new_arg


def test_move_back_after():
    '''Test that MoveTrans moves the node backwards to the expected
    location when location="after". '''
    _, invoke = get_invoke("15.14.2_multiple_set_kernels.f90", TEST_API,
                           idx=0, dist_mem=True)
    schedule = invoke.schedule
    move_trans = MoveTrans()
    initial_index = 2
    target_index = 0
    orig_arg = schedule.children[initial_index]
    new_arg = schedule.children[target_index]
    assert orig_arg is not new_arg

    move_trans.apply(schedule.children[initial_index],
                     schedule.children[target_index],
                     {"position": "after"})

    new_arg = schedule.children[target_index+1]
    assert orig_arg is new_arg


def test_move_forward():
    '''Test that MoveTrans moves the node forwards to the expected
    location. '''
    _, invoke = get_invoke("15.14.2_multiple_set_kernels.f90", TEST_API,
                           idx=0, dist_mem=True)
    schedule = invoke.schedule
    move_trans = MoveTrans()
    initial_index = 0
    target_index = 2
    orig_arg = schedule.children[initial_index]
    new_arg = schedule.children[target_index]
    assert orig_arg is not new_arg

    move_trans.apply(schedule.children[initial_index],
                     schedule.children[target_index])

    new_arg = schedule.children[target_index-1]
    assert orig_arg is new_arg


def test_move_forward_after():
    '''Test that MoveTrans moves the node forwards to the expected
    location when location="after". '''
    _, invoke = get_invoke("15.14.2_multiple_set_kernels.f90", TEST_API,
                           idx=0, dist_mem=True)
    schedule = invoke.schedule
    move_trans = MoveTrans()
    initial_index = 0
    target_index = 2
    orig_arg = schedule.children[initial_index]
    new_arg = schedule.children[target_index]
    assert orig_arg is not new_arg

    move_trans.apply(schedule.children[initial_index],
                     schedule.children[target_index],
                     {"position": "after"})

    new_arg = schedule.children[target_index]
    assert orig_arg is new_arg


# test that move with dependencies fails
def test_move_fail():
    '''Test that MoveTrans fails to move the node backwards and forwards
    if there is a dependence. '''
    _, invoke = get_invoke("15.14.1_multi_aX_plus_Y_builtin.f90", TEST_API,
                           idx=0, dist_mem=True)
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


def test_rc_node_not_loop():
    '''Test that Dynamo0p3RedundantComputationTrans raises an exception if the
    node argument is not a loop. '''
    _, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                           idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(schedule.children[0])
    assert ("Target of Dynamo0p3RedundantComputationTrans transformation must "
            "be a sub-class of Loop but got \'LFRicHaloExchange\'" in
            str(excinfo.value))


def test_rc_invalid_loop(monkeypatch):
    ''' Test that Dynamo0p3RedundantComputationTrans raises an exception if the
    supplied loop does not iterate over cells or dofs. '''
    _, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                           idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[4]
    # set the loop to a type that should raise an exception
    monkeypatch.setattr(loop, "loop_type", value="colours")
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop)
    assert ("In the Dynamo0p3RedundantComputation transformation apply "
            "method the loop type must be one of '' (cell-columns), 'dof' or "
            "'colour', but found 'colours'") in str(excinfo.value)


def test_rc_nodm():
    '''Test that Dynamo0p3RedundantComputationTrans raises an exception if
    distributed memory is not set. '''
    _, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                           idx=0, dist_mem=False)
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[0]
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop)
    assert ("In the Dynamo0p3RedundantComputation transformation apply method "
            "distributed memory must be switched on") in str(excinfo.value)


def test_rc_invalid_depth():
    ''' Test that Dynamo0p3RedundantComputationTrans raises an exception if the
    supplied depth is less than 1. '''
    _, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                           idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[4]
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop, {"depth": 0})
    assert ("In the Dynamo0p3RedundantComputation transformation apply method "
            "the supplied depth is less than 1") in str(excinfo.value)


def test_rc_invalid_depth_continuous():
    ''' Test that Dynamo0p3RedundantComputationTrans raises an exception if the
    supplied depth equals 1 when modifying a continuous field. '''
    _, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                           idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[4]
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop, {"depth": 1})
    assert ("In the Dynamo0p3RedundantComputation transformation apply method "
            "the supplied depth (1) must be greater than the existing halo "
            "depth (1)") in str(excinfo.value)


def test_rc_continuous_depth():
    ''' Test that the loop bounds for a continuous kernel (iterating over
    cells) are modified appropriately, that set_clean() is added
    correctly and halo_exchange modified appropriately after applying
    the redundant computation transformation with a fixed value for
    halo depth.

    '''
    psy, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                             idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[4]
    rc_trans.apply(loop, {"depth": 3})
    result = str(psy.gen)

    for field_name in ["f2", "m1", "m2"]:
        assert f"IF ({field_name}_proxy%is_dirty(depth=3)) THEN" in result
        assert f"CALL {field_name}_proxy%halo_exchange(depth=3)" in result
    assert "loop0_stop = mesh%get_last_halo_cell(3)" in result
    assert "DO cell=loop0_start,loop0_stop" in result
    assert ("      CALL f1_proxy%set_dirty()\n"
            "      CALL f1_proxy%set_clean(2)") in result


def test_rc_continuous_no_depth():
    ''' Test that the loop bounds for a continuous kernel (iterating over
    cells) are modified appropriately, that set_clean() is added
    correctly and halo_exchange modified appropriately after applying
    the redundant computation transformation with no value for halo
    depth.

    '''
    psy, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                             idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[4]
    rc_trans.apply(loop)
    result = str(psy.gen)

    assert ("      IF (f1_proxy%is_dirty(depth=max_halo_depth_mesh-1)) THEN\n"
            "        CALL f1_proxy%halo_exchange(depth=max_halo_depth_mesh"
            "-1)" in result)
    for fname in ["f2", "m1", "m2"]:
        assert (f"      IF ({fname}_proxy%is_dirty(depth=max_halo_depth_mesh"
                f")) THEN\n"
                f"        CALL {fname}_proxy%halo_exchange(depth=max_halo_"
                f"depth_mesh)" in result)
    assert "loop0_stop = mesh%get_last_halo_cell()" in result
    assert "DO cell=loop0_start,loop0_stop" in result
    assert ("      CALL f1_proxy%set_dirty()\n"
            "      CALL f1_proxy%set_clean(max_halo_depth_mesh-1)") in result


def test_rc_discontinuous_depth(tmpdir, monkeypatch, annexed):
    '''Test that the loop bounds for a discontinuous kernel (iterating
    over cells) with continuous reads are modified appropriately and
    set_clean() added correctly and halo_exchange added appropriately
    after applying the redundant computation transformation with a
    fixed value for halo depth. Also test with and without annexed
    dofs being computed as this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    psy, invoke = get_invoke("1_single_invoke_w3.f90", TEST_API,
                             idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    if annexed:
        # there are no halo exchange calls
        index = 0
    else:
        # there are 3 halo exchange calls
        index = 3
    loop = schedule.children[index]
    rc_trans.apply(loop, {"depth": 3})
    result = str(psy.gen)
    for field_name in ["f1", "f2", "m1"]:
        assert (f"      IF ({field_name}_proxy%is_dirty(depth=3)) THEN\n"
                f"        CALL {field_name}_proxy%halo_exchange(depth=3)"
                in result)
    assert "loop0_stop = mesh%get_last_halo_cell(3)" in result
    assert "DO cell=loop0_start,loop0_stop" in result
    assert ("      CALL m2_proxy%set_dirty()\n"
            "      CALL m2_proxy%set_clean(3)") in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


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
    psy, invoke = get_invoke("1_single_invoke_w3.f90", TEST_API,
                             idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    if annexed:
        # there are no halo exchange calls
        index = 0
    else:
        # there are 3 halo exchange calls
        index = 3
    loop = schedule.children[index]
    rc_trans.apply(loop)
    result = str(psy.gen)

    for field_name in ["f1", "f2", "m1"]:
        assert (f"IF ({field_name}_proxy%is_dirty(depth=max_halo_depth_mesh)) "
                f"THEN" in result)
        assert (f"CALL {field_name}_proxy%halo_exchange("
                f"depth=max_halo_depth_mesh)" in result)
    assert "loop0_stop = mesh%get_last_halo_cell()" in result
    assert "DO cell=loop0_start,loop0_stop" in result
    assert "CALL m2_proxy%set_dirty()" not in result
    assert "CALL m2_proxy%set_clean(max_halo_depth_mesh)" in result


def test_rc_all_discontinuous_depth(tmpdir):
    ''' Test that the loop bounds for a discontinuous kernel
    (iterating over cells) with discontinuous reads are modified
    appropriately and set_clean() added correctly and halo_exchange
    added appropriately after applying the redundant computation
    transformation with a fixed value for halo depth. '''
    psy, invoke = get_invoke("1_single_invoke_wtheta.f90", TEST_API,
                             idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[0]
    rc_trans.apply(loop, {"depth": 3})
    result = str(psy.gen)
    assert "IF (f2_proxy%is_dirty(depth=3)) THEN" in result
    assert "CALL f2_proxy%halo_exchange(depth=3)" in result
    assert "loop0_stop = mesh%get_last_halo_cell(3)" in result
    assert "DO cell=loop0_start,loop0_stop" in result
    assert "CALL f1_proxy%set_dirty()" in result
    assert "CALL f1_proxy%set_clean(3)" in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_rc_all_discontinuous_no_depth(tmpdir):
    ''' Test that the loop bounds for a discontinuous kernel
    (iterating over cells) with discontinuous reads are modified
    appropriately and set_clean() added correctly and halo_exchange
    added appropriately after applying the redundant computation
    transformation with no halo depth value. '''
    psy, invoke = get_invoke("1_single_invoke_w2v.f90", TEST_API,
                             idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[0]
    rc_trans.apply(loop)
    result = str(psy.gen)

    assert "IF (f2_proxy%is_dirty(depth=max_halo_depth_mesh)) THEN" in result
    assert "CALL f2_proxy%halo_exchange(depth=max_halo_depth_mesh)" in result
    assert "loop0_stop = mesh%get_last_halo_cell()" in result
    assert "DO cell=loop0_start,loop0_stop" in result
    assert "CALL f1_proxy%set_clean(max_halo_depth_mesh)" in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_rc_all_discontinuous_vector_depth(tmpdir):
    ''' Test that the loop bounds for a discontinuous kernel (iterating
    over cells) are modified appropriately and set_clean() added
    correctly and halo_exchange added appropriately for vector fields
    after applying the redundant computation transformation with a
    fixed value for halo depth. '''
    psy, invoke = get_invoke("1_single_invoke_w3_only_vector.f90", TEST_API,
                             idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[0]
    rc_trans.apply(loop, {"depth": 3})
    result = str(psy.gen)

    for idx in range(1, 4):
        assert f"IF (f2_proxy({idx})%is_dirty(depth=3)) THEN" in result
        assert f"CALL f2_proxy({idx})%halo_exchange(depth=3)" in result
    assert "loop0_stop = mesh%get_last_halo_cell(3)" in result
    assert "DO cell=loop0_start,loop0_stop" in result
    for idx in range(1, 4):
        assert f"CALL f1_proxy({idx})%set_dirty()" in result
        assert f"CALL f1_proxy({idx})%set_clean(3)" in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_rc_all_discontinuous_vector_no_depth(tmpdir):
    ''' Test that the loop bounds for a discontinuous kernel (iterating
    over cells) are modified appropriately and set_clean() added
    correctly and halo_exchange added appropriately for vector fields
    after applying the redundant computation transformation with no
    halo depth value. '''
    psy, invoke = get_invoke("1_single_invoke_wtheta_only_vector.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[0]
    rc_trans.apply(loop)
    result = str(psy.gen)
    for idx in range(1, 4):
        assert (f"IF (f2_proxy({idx})%is_dirty(depth=max_halo_depth_mesh"
                f")) THEN") in result
        assert (f"CALL f2_proxy({idx})%halo_exchange(depth=max_halo_depth_mesh"
                f")") in result
    assert "loop0_stop = mesh%get_last_halo_cell()" in result
    assert "DO cell=loop0_start,loop0_stop" in result
    for idx in range(1, 4):
        assert f"CALL f1_proxy({idx})%set_clean(max_halo_depth_mesh)" in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_rc_all_disc_prev_depend_depth(tmpdir):
    ''' Test that the loop bounds for a discontinuous kernel
    (iterating over cells) with discontinuous reads are modified
    appropriately and set_clean() added correctly and halo_exchange
    added appropriately in the case where the field requiring a halo
    exchange has a previous non-halo dependence, after applying the
    redundant computation transformation with a fixed value for halo
    depth. '''
    psy, invoke = get_invoke("4.12_multikernel_invokes_w2v.f90", TEST_API,
                             idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule[1]
    rc_trans.apply(loop, {"depth": 3})
    result = str(psy.gen)
    assert "IF (f1_proxy%is_dirty(depth=3)) THEN" not in result
    assert "CALL f1_proxy%halo_exchange(depth=3)" in result
    assert "loop1_stop = mesh%get_last_halo_cell(3)" in result
    assert "DO cell=loop1_start,loop1_stop" in result
    assert "CALL f1_proxy%set_dirty()" in result
    assert "CALL f3_proxy%set_dirty()" in result
    assert "CALL f3_proxy%set_clean(3)" in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_rc_all_disc_prev_depend_no_depth():
    ''' Test that the loop bounds for a discontinuous kernel
    (iterating over cells) are modified appropriately and set_clean()
    added correctly and halo_exchange added appropriately in the case
    where the field now requiring a halo exchange has a previous
    non-halo dependence after applying the redundant computation
    transformation with no halo depth value. '''
    psy, invoke = get_invoke("4.12_multikernel_invokes_w2v.f90", TEST_API,
                             idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule[1]
    rc_trans.apply(loop)
    result = str(psy.gen)
    assert "CALL f1_proxy%set_dirty()" in result
    assert ("IF (f1_proxy%is_dirty(depth=max_halo_depth_mesh)) "
            "THEN") not in result
    assert "CALL f1_proxy%halo_exchange(depth=max_halo_depth_mesh)" in result
    assert "loop1_stop = mesh%get_last_halo_cell()" in result
    assert "DO cell=loop1_start,loop1_stop" in result
    assert "CALL f3_proxy%set_clean(max_halo_depth_mesh)" in result


def test_rc_all_disc_prev_dep_depth_vector(tmpdir):
    ''' Test that the loop bounds for a discontinuous kernel (iterating
    over cells) with discontinuous reads are modified appropriately
    and set_clean() added correctly and halo_exchange added
    appropriately in the case where the vector field requiring a halo
    exchange has a previous non-halo dependence, after applying the
    redundant computation transformation with a fixed value for halo
    depth. '''
    psy, invoke = get_invoke("8.2.1_multikernel_invokes_w3_vector.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule[1]
    rc_trans.apply(loop, {"depth": 3})
    result = str(psy.gen)
    for idx in range(1, 4):
        assert f"IF (f1_proxy({idx})%is_dirty(depth=3)) THEN" not in result
        assert f"CALL f1_proxy({idx})%halo_exchange(depth=3)" in result
        assert "loop1_stop = mesh%get_last_halo_cell(3)" in result
    assert "DO cell=loop1_start,loop1_stop" in result
    for idx in range(1, 4):
        assert f"CALL f1_proxy({idx})%set_dirty()" in result
        assert f"CALL f3_proxy({idx})%set_dirty()" in result
        assert f"CALL f3_proxy({idx})%set_clean(3)" in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_rc_all_disc_prev_dep_no_depth_vect(tmpdir):
    ''' Test that the loop bounds for a discontinuous kernel (iterating
    over cells) are modified appropriately and set_clean() added
    correctly and halo_exchange added appropriately in the case where
    the vector field now requiring a halo exchange has a previous non-halo
    dependence after applying the redundant computation transformation
    with no halo depth value. '''
    psy, invoke = get_invoke("8.2.1_multikernel_invokes_w3_vector.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule[1]
    rc_trans.apply(loop)
    result = str(psy.gen)
    assert "is_dirty" not in result
    for idx in range(1, 4):
        assert (f"CALL f1_proxy({idx})%halo_exchange(depth=max_halo_depth_"
                f"mesh)") in result
    assert "loop1_stop = mesh%get_last_halo_cell()" in result
    assert "DO cell=loop1_start,loop1_stop" in result
    for idx in range(1, 4):
        assert f"CALL f1_proxy({idx})%set_dirty()" in result
        assert f"CALL f3_proxy({idx})%set_clean(max_halo_depth_mesh)" in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_rc_all_disc_prev_dep_no_depth_vect_readwrite(tmpdir):
    ''' Test that the loop bounds for a discontinuous kernel (iterating
    over cells) are modified appropriately and set_clean() added
    correctly and halo_exchange added appropriately in the case where
    the vector field now requiring a halo exchange has a previous halo
    dependence (readwrite access) after applying the redundant computation
    transformation with no halo depth value. '''
    psy, invoke = get_invoke("8.2.2_multikernel_invokes_wtheta_vector.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule[1]
    rc_trans.apply(loop)
    result = str(psy.gen)
    # f3 has readwrite access so need to check the halos
    for idx in range(1, 4):
        assert (f"IF (f3_proxy({idx})%is_dirty(depth=max_halo_depth_mesh))"
                in result)
        assert (f"CALL f3_proxy({idx})%halo_exchange(depth=max_halo_depth_mesh"
                ")" in result)
    # f1 has RW to W dependency
    for idx in range(1, 4):
        assert (f"CALL f1_proxy({idx})%halo_exchange(depth=max_halo_depth_mesh"
                f")" in result)
    assert "loop1_stop = mesh%get_last_halo_cell()" in result
    assert "DO cell=loop1_start,loop1_stop" in result
    for idx in range(1, 4):
        assert f"CALL f1_proxy({idx})%set_dirty()" in result
        assert f"CALL f3_proxy({idx})%set_clean(max_halo_depth_mesh)" in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_rc_dofs_depth():
    ''' Test that the loop bounds when iterating over DoFs are modified
    appropriately and set_clean() added correctly and halo_exchange
    added appropriately after applying the redundant computation
    transformation with a fixed value for halo depth where the halo
    fields have no previous dependence.

    '''
    psy, invoke = get_invoke("15.1.2_inc_X_plus_Y_builtin.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[0]
    rc_trans.apply(loop, {"depth": 3})
    result = str(psy.gen)
    for field in ["f1", "f2"]:
        assert f"IF ({field}_proxy%is_dirty(depth=3)) THEN" in result
        assert f"CALL {field}_proxy%halo_exchange(depth=3)" in result
    assert "loop0_stop = f1_proxy%vspace%get_last_dof_halo(3)" in result
    assert "DO df=loop0_start,loop0_stop" in result
    assert "CALL f1_proxy%set_dirty()" in result
    assert "CALL f1_proxy%set_clean(3)" in result


def test_rc_dofs_no_depth():
    ''' Test that the loop bounds when iterating over DoFs are modified
    appropriately and set_clean() added correctly and halo_exchange
    added appropriately after applying the redundant computation
    transformation with no halo depth value where the halo fields have
    no previous dependence.

    '''
    psy, invoke = get_invoke("15.1.2_inc_X_plus_Y_builtin.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[0]
    rc_trans.apply(loop)
    result = str(psy.gen)

    assert "IF (f2_proxy%is_dirty(depth=max_halo_depth_mesh)) THEN" in result
    assert "CALL f2_proxy%halo_exchange(depth=max_halo_depth_mesh)" in result
    assert "loop0_stop = f1_proxy%vspace%get_last_dof_halo()" in result
    assert "DO df=loop0_start,loop0_stop" in result
    assert "CALL f1_proxy%set_dirty()" not in result
    assert "CALL f1_proxy%set_clean(max_halo_depth_mesh)" in result


def test_rc_dofs_depth_prev_dep(monkeypatch, annexed, tmpdir):
    ''' Test that the loop bounds when iterating over DoFs are modified
    appropriately and set_clean() added correctly and halo_exchange
    added appropriately after applying the redundant computation
    transformation with a fixed value for halo depth where the halo
    fields have a previous (non-halo-exchange) dependence. Also test
    with and without annexed dofs.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    psy, invoke = get_invoke("15.1.1_builtin_and_normal_kernel_invoke_2.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    if annexed:
        index = 4
    else:
        index = 5
    loop = schedule.children[index]
    rc_trans.apply(loop, {"depth": 3})
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Check that the f2 halo exchange is modified
    assert "CALL f2_proxy%halo_exchange(depth=3)" in result
    # There is a need for a run-time is_dirty check for field f2 as
    # this field is not modified in this invoke and therefore its halo
    # is in an unknown state before it is read
    assert ("IF (f2_proxy%is_dirty(depth=3)) "
            "THEN") in result

    # Check that the existing halo exchanges (for the first un-modified
    # loop) remain unchanged. These are on f1, m1 and m2 without annexed
    # dofs and only on m1 and m2 with annexed dofs.
    fld_hex_names = ["f1", "m1", "m2"]
    if annexed:
        fld_hex_names.remove("f1")
    for field_name in fld_hex_names:
        assert f"IF ({field_name}_proxy%is_dirty(depth=1)) THEN" in result
        assert f"CALL {field_name}_proxy%halo_exchange(depth=1)" in result
    assert "loop1_stop = f1_proxy%vspace%get_last_dof_halo(3)" in result
    assert "DO df=loop1_start,loop1_stop" in result
    assert "CALL f1_proxy%set_dirty()" in result
    assert "CALL f1_proxy%set_clean(3)" in result


def test_rc_dofs_no_depth_prev_dep():
    ''' Test that the loop bounds when iterating over DoFs are modified
    appropriately and set_clean() added correctly and halo_exchange
    added appropriately after applying the redundant computation
    transformation with no halo depth value where the halo
    fields have a previous (non-halo-exchange) dependence.

    '''
    psy, invoke = get_invoke("15.1.1_builtin_and_normal_kernel_invoke_2.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule[5]
    rc_trans.apply(loop)
    result = str(psy.gen)

    # Check that the f2 halo exchange is modified
    assert "CALL f2_proxy%halo_exchange(depth=max_halo_depth_mesh)" in result
    assert "IF (f2_proxy%is_dirty(depth=max_halo_depth_mesh)) THEN" in result
    # Check that the existing f1, m1 and m2 halo exchanges remain unchanged
    for fname in ["f1", "m1", "m2"]:
        assert f"IF ({fname}_proxy%is_dirty(depth=1)) THEN" in result
        assert f"CALL {fname}_proxy%halo_exchange(depth=1)" in result
    assert "loop1_stop = f1_proxy%vspace%get_last_dof_halo()" in result
    assert "DO df=loop1_start,loop1_stop" in result
    assert "CALL f1_proxy%set_dirty()" in result
    assert "CALL f1_proxy%set_clean(max_halo_depth_mesh)" in result


def test_continuous_no_set_clean():
    '''Test that set_clean is not added for the default iteration space of
    a continuous loop. This is probably covered from tests in
    dynamo0p3_test.py but it is good to have a specific test. '''
    psy, _ = get_invoke("1_single_invoke.f90",
                        TEST_API, idx=0, dist_mem=True)
    result = str(psy.gen)
    assert "loop0_stop = mesh%get_last_halo_cell(1)" in result
    assert "DO cell=loop0_start,loop0_stop" in result
    assert "CALL f1_proxy%set_dirty()" in result
    assert "CALL f1_proxy%set_clean(" not in result


def test_discontinuous_no_set_clean():
    ''' Test that set_clean is not added for the default iteration
    space of a discontinuous loop. This is probably covered from tests
    in dynamo0p3_test.py but it is good to have a specific test. '''
    psy, _ = get_invoke("1_single_invoke_w3.f90", TEST_API,
                        idx=0, dist_mem=True)
    result = str(psy.gen)
    assert "loop0_stop = mesh%get_last_edge_cell()" in result
    assert "CALL m2_proxy%set_dirty()" in result
    assert "CALL m2_proxy%set_clean(" not in result


def test_dofs_no_set_clean(monkeypatch, annexed):
    ''' Test that set_clean is not added for the default iteration space
    of a loop over dofs. This is probably covered from tests in
    lfric_builtins_test.py but it is good to have a specific
    test. Also test with and without annexed dofs being computed as
    this affects the generated code.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    psy, _ = get_invoke("15.7.1_setval_c_builtin.f90", TEST_API,
                        idx=0, dist_mem=True)
    result = str(psy.gen)
    assert "halo_exchange" not in result
    if annexed:
        assert "loop0_stop = f1_proxy%vspace%get_last_dof_annexed()" in result
    else:
        assert "loop0_stop = f1_proxy%vspace%get_last_dof_owned()" in result
    assert "CALL f1_proxy%set_dirty()" in result
    assert "CALL f1_proxy%set_clean(" not in result


def test_rc_vector_depth(tmpdir):
    ''' Test that the loop bounds for a (continuous) vector are modified
    appropriately and set_clean() added correctly and halo_exchange
    added/modified appropriately after applying the redundant
    computation transformation with a fixed value for halo depth.

    '''
    psy, invoke = get_invoke("8_vector_field.f90", TEST_API, idx=0,
                             dist_mem=True)
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule[5]
    rc_trans.apply(loop, {"depth": 3})
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert "IF (f2_proxy%is_dirty(depth=3)) THEN" in result
    assert "CALL f2_proxy%halo_exchange(depth=3)" in result
    assert "loop0_stop = mesh%get_last_halo_cell(3)" in result
    for index in range(1, 4):
        assert f"CALL chi_proxy({index})%set_dirty()" in result
    for index in range(1, 4):
        assert f"CALL chi_proxy({index})%set_clean(2)" in result


def test_rc_vector_no_depth(tmpdir):
    ''' Test that the loop bounds for a (continuous) vector are modified
    appropriately and set_clean() added correctly and halo_exchange
    added/modified appropriately after applying the redundant
    computation transformation with no halo depth value.

    '''
    psy, invoke = get_invoke("8_vector_field.f90", TEST_API, idx=0,
                             dist_mem=True)
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule[5]
    rc_trans.apply(loop)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert "IF (f2_proxy%is_dirty(depth=max_halo_depth_mesh)) THEN" in result
    assert "CALL f2_proxy%halo_exchange(depth=max_halo_depth_mesh)" in result
    assert "loop0_stop = mesh%get_last_halo_cell()" in result
    for idx in range(1, 4):
        assert f"CALL chi_proxy({idx})%set_dirty()" in result
    for idx in range(1, 4):
        assert (f"CALL chi_proxy({idx})%set_clean(max_halo_depth_mesh-1)"
                in result)


def test_rc_no_halo_decrease():
    ''' Test that we do not decrease an existing halo size when setting it
    to a particular value. This situation may happen when the
    redundant computation affects the same field in two different
    loops and both depend on the same halo exchange.

    '''
    psy, invoke = get_invoke("15.1.1_builtin_and_normal_kernel_invoke_2.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    # First, change the size of the f2 halo exchange to 3 by performing
    # redundant computation in the first loop
    loop = schedule.children[4]
    rc_trans.apply(loop, {"depth": 3})
    result = str(psy.gen)
    assert "IF (f2_proxy%is_dirty(depth=3)) THEN" in result
    assert "IF (m1_proxy%is_dirty(depth=3)) THEN" in result
    assert "IF (m2_proxy%is_dirty(depth=3)) THEN" in result
    # Second, try to change the size of the f2 halo exchange to 2 by
    # performing redundant computation in the second loop
    loop = schedule.children[5]
    rc_trans.apply(loop, {"depth": 2})
    result = str(psy.gen)
    assert "IF (f2_proxy%is_dirty(depth=3)) THEN" in result
    assert "IF (m1_proxy%is_dirty(depth=3)) THEN" in result
    assert "IF (m2_proxy%is_dirty(depth=3)) THEN" in result
    # Third, set the size of the f2 halo exchange to the full halo
    # depth by performing redundant computation in the second loop
    rc_trans.apply(loop)
    result = str(psy.gen)
    assert "IF (f2_proxy%is_dirty(depth=max_halo_depth_mesh)) THEN" in result
    assert "IF (m1_proxy%is_dirty(depth=3)) THEN" in result
    assert "IF (m2_proxy%is_dirty(depth=3)) THEN" in result
    # Fourth, try to change the size of the f2 halo exchange to 4 by
    # performing redundant computation in the first loop
    loop = schedule.children[4]
    rc_trans.apply(loop, {"depth": 4})
    result = str(psy.gen)
    assert "IF (f2_proxy%is_dirty(depth=max_halo_depth_mesh)) THEN" in result
    assert "IF (m1_proxy%is_dirty(depth=4)) THEN" in result
    assert "IF (m2_proxy%is_dirty(depth=4)) THEN" in result


def test_rc_updated_dependence_analysis():
    ''' Test that the dependence analysis updates when new halo exchanges
    are added to the schedule. '''
    _, invoke = get_invoke("1_single_invoke_wtheta.f90", TEST_API, idx=0,
                           dist_mem=True)
    schedule = invoke.schedule
    loop = schedule.children[0]
    kernel = loop.loop_body[0]
    f2_field = kernel.args[1]
    assert not f2_field.backward_dependence()
    # set our loop to redundantly compute to the level 2 halo. This
    # introduces a new halo exchange
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[0]
    rc_trans.apply(loop, {"depth": 2})
    previous_field = f2_field.backward_dependence()
    previous_node = previous_field.call
    # check f2_field has a backward dependence with the new halo
    # exchange field
    assert isinstance(previous_node, LFRicHaloExchange)
    # check the new halo exchange field has a forward dependence with
    # the kernel f2_field
    assert previous_field.forward_dependence() == f2_field


def test_rc_no_loop_decrease():
    ''' Test that we raise an exception if we try to reduce the size of a
    loop halo when using the redundant computation transformation. This is
    not allowed partly for simplicity but also because, in the current
    implementation we might not decrease the size of the relevant halo
    exchange as these can only be increased with the current logic. '''
    _, invoke = get_invoke("1_single_invoke_w2v.f90",
                           TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    # first set our loop to redundantly compute to the level 2 halo
    loop = schedule.children[0]
    rc_trans.apply(loop, {"depth": 2})
    # now try to reduce the redundant computation to the level 1 halo
    # f1 and f2 have read accesses (readwrite and read) so there
    # is one halo exchange for each before the loop
    loop = schedule.children[2]
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop, {"depth": 1})
    assert ("supplied depth (1) must be greater than the existing halo depth "
            "(2)") in str(excinfo.value)
    # second set our loop to redundantly compute to the maximum halo depth
    rc_trans.apply(loop)
    # now try to reduce the redundant computation to a fixed value
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop, {"depth": 2})
    assert ("loop is already set to the maximum halo depth so can't be "
            "set to a fixed value") in str(excinfo.value)
    # now try to set the redundant computation to the same (max) value
    # it is now
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop)
    assert ("loop is already set to the maximum halo depth so this "
            "transformation does nothing") in str(excinfo.value)


def test_rc_remove_halo_exchange(tmpdir, monkeypatch):
    '''Test that a halo exchange is removed if redundant computation means
    that it is no longer required. Halo exchanges are not required in
    this example when we compute annexed dofs. Therefore we ensure we
    compute over owned dofs (via monkeypatch) to perform the test.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", False)
    psy, _ = get_invoke("14.7_halo_annexed.f90",
                        TEST_API, idx=0, dist_mem=True)
    result = str(psy.gen)
    assert "CALL f1_proxy%halo_exchange(depth=1)" in result
    assert "CALL f2_proxy%halo_exchange(depth=1)" in result
    assert "IF (m1_proxy%is_dirty(depth=1)) THEN" in result
    assert "CALL m1_proxy%halo_exchange(depth=1)" in result

    assert LFRicBuild(tmpdir).code_compiles(psy)

    #
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    #
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[0]
    rc_trans.apply(loop, {"depth": 1})
    result = str(psy.gen)
    assert "CALL f1_proxy%halo_exchange(depth=1)" not in result
    assert "CALL f2_proxy%halo_exchange(depth=1)" in result
    assert "IF (m1_proxy%is_dirty(depth=1)) THEN" in result
    assert "CALL m1_proxy%halo_exchange(depth=1)" in result
    #
    loop = schedule.children[1]
    rc_trans.apply(loop, {"depth": 1})
    result = str(psy.gen)
    assert "CALL f1_proxy%halo_exchange(depth=1)" not in result
    assert "CALL f2_proxy%halo_exchange(depth=1)" not in result
    assert "IF (m1_proxy%is_dirty(depth=1)) THEN" in result
    assert "CALL m1_proxy%halo_exchange(depth=1)" in result


def test_rc_max_remove_halo_exchange(tmpdir):
    ''' Add test to redundantly compute a discontinuous (wtheta) and
    continuous (w2) field to the maximum halo depth and then check
    that a discontinuous halo exchange is removed in this case as we
    always remove the halo exchange if we write to a discontinuous
    field to maximum depth. Also check that the halo exchange is not
    removed for the continuous case as the outer halo stays dirty.
    The halo should also have an if round it as we do not know how
    much redundant computation we are doing.

    '''
    psy, invoke = get_invoke("15.1.2_builtin_and_normal_kernel_invoke.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    result = str(psy.gen)
    #
    # f3 has "inc" access so there is a check for the halo exchange
    # of depth 1
    assert "CALL f3_proxy%halo_exchange(depth=1)" in result
    assert "IF (f3_proxy%is_dirty(depth=1)) THEN" in result
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[4]
    rc_trans.apply(loop)
    result = str(psy.gen)

    # f3 halo exchange is not removed even though we redundantly
    # compute f3 as the redundant computation is on a continuous field
    # and therefore the outermost halo stays dirty. We can not be
    # certain whether the halo exchange is required or not as we don't
    # know the depth of the halo.
    assert "CALL f3_proxy%halo_exchange(depth=1)" in result
    # We do not know whether we need the halo exchange so we include an if
    assert "IF (f3_proxy%is_dirty(depth=1)) THEN" in result
    #
    assert "CALL f4_proxy%halo_exchange(depth=1)" in result
    loop = schedule.children[5]
    rc_trans.apply(loop)
    result = str(psy.gen)
    # f4 halo exchange is removed as it is redundantly computed to the
    # last level halo and is discontinuous so all levels of the halo
    # are clean. However, we introduce a new halo exchange for
    # f5. This could be removed by redundant computation but we don't
    # bother as that is not relevant to this test.
    assert "CALL f4_proxy%halo_exchange(depth=1)" not in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_rc_continuous_halo_remove():
    ''' Check that we do not remove a halo exchange when the field is
    continuous and the redundant computation depth equals the required
    halo access depth. The reason for this is that the outer halo
    remains invalid when written to for a continuous field. Also check
    that we do remove the halo exchange when the redundant computation
    depth is one more than the required halo access depth.

    '''
    psy, invoke = get_invoke("15.1.2_builtin_and_normal_kernel_invoke.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    result = str(psy.gen)
    rc_trans = Dynamo0p3RedundantComputationTrans()
    f3_inc_hex = schedule.children[2]
    f3_inc_loop = schedule.children[4]
    f3_read_hex = schedule.children[7]
    f3_read_loop = schedule.children[9]
    # field "f3" has "inc" access resulting in two halo exchanges of
    # depth 1, one of which is conditional. One of these halo
    # exchanges is placed before the f3_inc_loop and one is placed
    # before the f3_read_loop (there are three other halo exchanges,
    # one each for fields f1, f2 and f4).
    assert result.count("CALL f3_proxy%halo_exchange(depth=1") == 2
    assert result.count("IF (f3_proxy%is_dirty(depth=1)) THEN") == 1
    #
    # Applying redundant computation to equal depth on f3_inc_loop and
    # f3_read_loop does not remove the initial number of halo exchanges.
    # However, the "is_dirty" check and the halo exchange before the
    # f3_inc_loop are now to depth 2.
    rc_trans.apply(f3_read_loop, {"depth": 3})
    rc_trans.apply(f3_inc_loop, {"depth": 3})
    result = str(psy.gen)
    assert result.count("CALL f3_proxy%halo_exchange(depth=") == 2
    assert f3_inc_hex._compute_halo_depth() == "2"
    assert f3_read_hex._compute_halo_depth() == "3"
    assert "IF (f3_proxy%is_dirty(depth=2)) THEN" in result
    assert "IF (f3_proxy%is_dirty(depth=3)) THEN" not in result
    #
    # Applying redundant computation to one more depth to f3_inc_loop
    # removes the halo exchange before the f3_read_loop.
    # The "is_dirty" check and the halo exchange before the
    # f3_inc_loop are now to depth 3.
    rc_trans.apply(f3_inc_loop, {"depth": 4})
    result = str(psy.gen)
    assert result.count("CALL f3_proxy%halo_exchange(depth=") == 1
    assert f3_inc_hex._compute_halo_depth() == "3"
    # Position 7 is now halo exchange on f4 instead of f3
    assert schedule.children[7].field != "f3"
    assert "IF (f3_proxy%is_dirty(depth=4)" not in result


def test_rc_discontinuous_halo_remove(monkeypatch):
    ''' Check that we do remove a halo exchange when the field is
    discontinuous and the redundant computation depth equals the
    required halo access depth. Also check that we do not remove the
    halo exchange when the redundant computation depth is one less
    than the required halo access depth.

    '''
    psy, invoke = get_invoke("15.1.2_builtin_and_normal_kernel_invoke.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    result = str(psy.gen)
    rc_trans = Dynamo0p3RedundantComputationTrans()
    f4_write_loop = schedule.children[5]
    f4_read_loop = schedule.children[9]
    assert "CALL f4_proxy%halo_exchange(depth=1)" in result
    assert "IF (f4_proxy%is_dirty(depth=1)) THEN" not in result
    rc_trans.apply(f4_read_loop, {"depth": 3})
    rc_trans.apply(f4_write_loop, {"depth": 2})
    result = str(psy.gen)
    assert "CALL f4_proxy%halo_exchange(depth=3)" in result
    assert "IF (f4_proxy%is_dirty(depth=3)) THEN" not in result
    # Increase RC depth to 3 and check that halo exchange is removed
    # when a discontinuous field has write access
    rc_trans.apply(f4_write_loop, {"depth": 3})
    result = str(psy.gen)
    assert "CALL f4_proxy%halo_exchange(depth=" not in result
    assert "IF (f4_proxy%is_dirty(depth=" not in result
    # Increase RC depth to 3 and check that halo exchange is not removed
    # when a discontinuous field has readwrite access
    call = f4_write_loop.loop_body[0]
    f4_arg = call.arguments.args[0]
    monkeypatch.setattr(f4_arg, "_access", value=AccessType.READWRITE)
    monkeypatch.setattr(f4_write_loop, "_upper_bound_halo_depth", value=2)
    rc_trans.apply(f4_write_loop, {"depth": 3})
    result = str(psy.gen)
    assert "CALL f4_proxy%halo_exchange(depth=" in result
    assert "IF (f4_proxy%is_dirty(depth=" in result


def test_rc_reader_halo_remove():
    ''' Check that we do not add an unnecessary halo exchange when we
    increase the depth of halo that a loop computes but the previous loop
    still computes deep enough into the halo to avoid needing a halo
    exchange.

    '''
    psy, invoke = get_invoke("15.1.2_builtin_and_normal_kernel_invoke.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    result = str(psy.gen)

    result = str(psy.gen)
    assert "CALL f2_proxy%halo_exchange(depth=1)" in result

    rc_trans = Dynamo0p3RedundantComputationTrans()

    # Redundant computation to avoid halo exchange for f2
    rc_trans.apply(schedule.children[1], {"depth": 2})
    result = str(psy.gen)
    assert "CALL f2_proxy%halo_exchange(" not in result

    # Redundant computation to depth 2 in f2 reader loop should not
    # cause a new halo exchange as it is still covered by depth=2 in
    # the writer loop
    rc_trans.apply(schedule.children[4], {"depth": 2})
    result = str(psy.gen)
    assert "CALL f2_proxy%halo_exchange(" not in result


def test_rc_vector_reader_halo_remove():
    ''' Check that we do not add unnecessary halo exchanges for a vector
    field when we increase the depth of halo that a loop computes but
    the previous loop still computes deep enough into the halo to
    avoid needing halo exchanges. '''
    psy, invoke = get_invoke("8.2.1_multikernel_invokes_w3_vector.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    result = str(psy.gen)

    assert "is_dirty" not in result
    assert "halo_exchange" not in result

    rc_trans = Dynamo0p3RedundantComputationTrans()

    # Redundant computation for first loop
    rc_trans.apply(schedule.children[0], {"depth": 1})
    result = str(psy.gen)
    assert result.count("is_dirty") == 3
    assert result.count("halo_exchange") == 3

    # Redundant computation in reader loop should not
    # cause a new halo exchange as it is still covered by depth=1 in
    # the writer loop
    rc_trans.apply(schedule.children[4], {"depth": 1})
    result = str(psy.gen)
    assert result.count("is_dirty") == 3
    assert result.count("halo_exchange") == 3


def test_rc_vector_reader_halo_readwrite():
    ''' When we increase the depth of halo that a loop computes but the
    previous loop still computes deep enough into the halo the added
    halo exchanges stem from the vector readwrite access. '''
    psy, invoke = get_invoke("8.2.2_multikernel_invokes_wtheta_vector.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    result = str(psy.gen)

    assert "is_dirty" not in result
    assert "halo_exchange" not in result

    rc_trans = Dynamo0p3RedundantComputationTrans()

    # Redundant computation for first loop: both fields have
    # read dependencies for all three components
    rc_trans.apply(schedule.children[0], {"depth": 1})
    result = str(psy.gen)
    assert result.count("is_dirty") == 6
    assert result.count("halo_exchange") == 6

    # Redundant computation in reader loop causes new halo exchanges
    # due to readwrite dependency in f3
    rc_trans.apply(schedule.children[7], {"depth": 1})
    result = str(psy.gen)
    assert result.count("is_dirty") == 9
    assert result.count("halo_exchange") == 9

    # Now increase RC depth of the reader loop to 2 to check for
    # additional halo exchanges (3 more due to readwrite to read
    # dependency in f1)
    rc_trans.apply(schedule.children[10], {"depth": 2})
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
    ''' If a loop contains a kernel with a stencil access and the loop
    attempts to compute redundantly into the halo to the maximum depth
    then the stencil will access beyond the halo bounds. This is
    therefore not allowed and exceptions are raised in the
    Dynamo0p3RedundantComputationTrans transformation and in
    _compute_single_halo_info. This test checks these exceptions are
    raised correctly.

    '''
    _, invoke = get_invoke("19.1_single_stencil.f90",
                           TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    loop = schedule[4]
    rc_trans = Dynamo0p3RedundantComputationTrans()
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop)
    assert ("In the Dynamo0p3RedundantComputation transformation apply method "
            "the loop contains field 'f2' with a stencil access in kernel "
            "'testkern_stencil_code', so it is invalid to set redundant "
            "computation to maximum depth" in str(excinfo.value))

    halo_exchange = schedule[1]
    monkeypatch.setattr(loop, "_upper_bound_halo_depth", None)
    with pytest.raises(GenerationError) as excinfo:
        _ = halo_exchange._compute_halo_read_info()
    assert ("redundant computation to max depth with a stencil is "
            "invalid" in str(excinfo.value))


def test_rc_invalid_depth_type():
    ''' If an incorrect type is passed as a depth value to the redundant
    computation transformation an exception should be raised. This test
    checks that this exception is raised as expected.

    '''
    _, invoke = get_invoke("1_single_invoke.f90",
                           TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    loop = schedule.children[4]
    rc_trans = Dynamo0p3RedundantComputationTrans()
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop, {"depth": "2"})
    assert (f"the supplied depth should be an integer but found "
            f"type '{type('txt')}'" in str(excinfo.value))


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
    _, invoke = get_invoke("4.6_multikernel_invokes.f90",
                           TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    if annexed:
        index = 5
    else:
        index = 7
        # move the halo exchange between the two loops
        move_trans = MoveTrans()
        move_trans.apply(schedule.children[7], schedule.children[6])
    # make the first loop redundantly compute to halo level 3
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[index], {"depth": 3})
    # try to fuse the loops. This should fail as the depths are different
    if annexed:
        # we now have an additional halo exchange for the gh_inc
        # access in the first loop
        index += 1
    f_trans = LFRicLoopFuseTrans()
    with pytest.raises(TransformationError) as excinfo:
        f_trans.apply(schedule.children[index], schedule.children[index+1])
    assert ("Error in LFRicLoopFuseTrans transformation: The halo-depth "
            "indices are not the same. Found '3' and '1'" in
            str(excinfo.value))
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
    f_trans = LFRicLoopFuseTrans()
    with pytest.raises(TransformationError) as excinfo:
        f_trans.apply(schedule.children[index], schedule.children[index+1])
    assert ("Error in LFRicLoopFuseTrans transformation: The halo-depth "
            "indices are not the same. Found '3' and 'None'" in
            str(excinfo.value))


def test_loop_fusion_different_loop_name(monkeypatch):
    ''' We can only loop fuse if two loops iterate over the same entities
    and iterate over the same depth. The loop fusion transformation
    raises an exception if this is not the case. This test checks that
    the exception is raised correctly. '''
    _, invoke = get_invoke("4.12_multikernel_invokes_w2v.f90",
                           TEST_API, idx=0, dist_mem=True)
    # First test for f1 readwrite to read dependency
    schedule = invoke.schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[0], {"depth": 3})
    f_trans = LFRicLoopFuseTrans()
    with pytest.raises(TransformationError) as excinfo:
        # Indices of loops to fuse in the schedule
        f_trans.apply(schedule.children[2], schedule.children[3])
    assert ("Error in LFRicLoopFuseTrans transformation: The upper bound "
            "names are not the same. Found 'cell_halo' and 'ncells'"
            in str(excinfo.value))
    # Now test for f1 write to read dependency
    _, invoke = get_invoke("4.12_multikernel_invokes_w2v.f90",
                           TEST_API, idx=0, dist_mem=True)
    # First test for f1 readwrite to read dependency
    schedule = invoke.schedule

    call = schedule.children[0].loop_body[0]
    f1_arg = call.arguments.args[0]
    monkeypatch.setattr(f1_arg, "_access", value=AccessType.WRITE)
    rc_trans.apply(schedule.children[0], {"depth": 3})
    with pytest.raises(TransformationError) as excinfo:
        f_trans.apply(schedule.children[1], schedule.children[2])
    assert ("Error in LFRicLoopFuseTrans transformation: The upper bound "
            "names are not the same. Found 'cell_halo' and 'ncells'"
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
    _, invoke = get_invoke("14.10_halo_continuous_cell_w_to_r.f90",
                           TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule

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
            "max_halo_depth_mesh")

    # the halo exchange should be both required to be added and known
    # to be needed
    required, known = w_to_r_halo_exchange.required()
    assert required
    assert known


def test_red_comp_w_to_n_r_clean_gt_cleaned(tmpdir):
    ''' Tests the case where we have multiple (derived) read dependence
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
    psy, invoke = get_invoke("14.11_halo_required_clean_multi.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule

    w_loop = schedule.children[0]
    w_to_r_halo_exchange = schedule.children[1]

    # make the writer loop write redundantly into the level 1 halo. We
    # now make the level one halo clean but we still definitely need a
    # halo exchange as one of the readers reads the halo to level 2
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(w_loop, {"depth": 1})

    # the halo exchange should be both required and known to be needed
    required, known = w_to_r_halo_exchange.required()
    assert required
    assert known

    # make the writer loop write redundantly into the level 2 halo. We
    # now make the level two halo clean so the reader that reads to
    # the level 2 halo does not need a halo exchange, but another
    # reader reads to a variable level so we are not sure if we need a
    # halo exchange or not.
    rc_trans.apply(w_loop, {"depth": 2})

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
    rc_trans.apply(r_var_stencil_loop, {"depth": 3})

    w_to_r_halo_exchange = schedule.children[1]

    # the halo exchange should be required and known to be needed
    required, known = w_to_r_halo_exchange.required()
    assert required
    assert known

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_rc_no_directive():
    ''' When the redundant computation transformation is given a Loop whose
    parent is a directive an exception is raised as this is not
    supported (redundant computation transformations must be applied
    before directives are added). This test checks that this exception
    is raised correctly.

    '''
    _, invoke = get_invoke("1_single_invoke.f90",
                           TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule

    # Create a colouring transformation and apply this to the loop
    ctrans = Dynamo0p3ColourTrans()
    ctrans.apply(schedule[4])

    # Create an openmp transformation and apply this to the loop
    otrans = DynamoOMPParallelLoopTrans()
    otrans.apply(schedule[4].loop_body[0])

    # Create a redundant computation transformation and apply this to the loop
    rc_trans = Dynamo0p3RedundantComputationTrans()
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(
            schedule[4].loop_body[0].dir_body[0], {"depth": 1})
    assert ("Redundant computation must be applied before directives are added"
            in str(excinfo.value))


def test_rc_wrong_parent(monkeypatch):
    ''' When the redundant computation transformation is given a Loop which
    has the wrong parent, and that parent is not a Directive (which is
    handled in a separate case) an exception is raised. This test
    checks that this exception is raised correctly.

    '''
    _, invoke = get_invoke("1_single_invoke.f90",
                           TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule

    # Make the parent of the loop a halo exchange
    monkeypatch.setattr(schedule.children[4], "_parent", schedule.children[0])

    rc_trans = Dynamo0p3RedundantComputationTrans()
    # Apply redundant computation to the loop
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(schedule.children[4], {"depth": 1})
    assert ("the parent of the supplied loop must be the DynInvokeSchedule, "
            "or a Loop") in str(excinfo.value)


def test_rc_parent_loop_colour(monkeypatch):
    ''' If the parent of the loop supplied to the redundant computation
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
    _, invoke = get_invoke("1_single_invoke.f90",
                           TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule

    # Apply colouring
    # Create colour transformation
    ctrans = Dynamo0p3ColourTrans()
    # Colour the loop
    ctrans.apply(schedule.children[4])

    # Make the parent of the outermost loop something other than
    # InvokeSchedule (we use halo exchange in this case)
    monkeypatch.setattr(schedule.children[4], "_parent", schedule.children[0])

    rc_trans = Dynamo0p3RedundantComputationTrans()
    # Apply redundant computation to the loop
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(schedule.children[4].loop_body[0], {"depth": 1})
    assert ("if the parent of the supplied Loop is also a Loop then the "
            "parent's parent must be the DynInvokeSchedule"
            in str(excinfo.value))

    # Make the outermost loop iterate over cells (it should be
    # colours). We can ignore the previous monkeypatch as this
    # exception is encountered before the previous one.
    monkeypatch.setattr(schedule.children[4], "_loop_type", "cells")

    rc_trans = Dynamo0p3RedundantComputationTrans()
    # Apply redundant computation to the loop
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(schedule.children[4].loop_body[0], {"depth": 1})
    assert ("if the parent of the supplied Loop is also a Loop then the "
            "parent must iterate over 'colours'" in str(excinfo.value))

    # Make the innermost loop iterate over cells (it should be
    # colour). We can ignore the previous monkeypatches as this
    # exception is encountered before the previous ones.
    monkeypatch.setattr(schedule.children[4].loop_body[0], "_loop_type",
                        "cells")

    rc_trans = Dynamo0p3RedundantComputationTrans()
    # Apply redundant computation to the loop
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(schedule.children[4].loop_body[0], {"depth": 1})
    assert ("if the parent of the supplied Loop is also a Loop then the "
            "supplied Loop must iterate over 'colour'" in str(excinfo.value))


def test_rc_unsupported_loop_type(monkeypatch):
    ''' When an unsupported loop type is provided to the redundant
    computation apply method an exception is raised. It is not
    possible to get to this exception in normal circumstances due to
    the validation tests so we monkey patch it. This test checks that
    the exception is raised correctly.

    '''
    _, invoke = get_invoke("1_single_invoke.f90",
                           TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule

    # Apply colouring
    # Create colour transformation
    ctrans = Dynamo0p3ColourTrans()
    # Colour the loop
    ctrans.apply(schedule.children[4])

    # Make the loop type invalid
    monkeypatch.setattr(schedule.children[4].loop_body[0], "_loop_type",
                        "invalid")

    rc_trans = Dynamo0p3RedundantComputationTrans()

    # Switch off validation
    monkeypatch.setattr(rc_trans, "validate",
                        lambda loop, options: None)

    # Apply redundant computation to the loop
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(schedule.children[4].loop_body[0], {"depth": 1})
    assert "Unsupported loop_type 'invalid' found" in str(excinfo.value)


def test_rc_colour_no_loop_decrease():
    ''' Test that we raise an exception if we try to reduce the size of a
    loop halo depth when using the redundant computation
    transformation. This is not allowed partly for simplicity but also
    because, in the current implementation we might not decrease the
    size of the relevant halo exchange as these can only be increased
    with the current logic.

    '''
    _, invoke = get_invoke("1_single_invoke.f90",
                           TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule

    # Create our colour transformation
    ctrans = Dynamo0p3ColourTrans()
    # Colour the loop
    ctrans.apply(schedule.children[4])

    rc_trans = Dynamo0p3RedundantComputationTrans()
    # First set our loop to redundantly compute to the level 2 halo
    loop = schedule.children[4].loop_body[0]
    rc_trans.apply(loop, {"depth": 2})
    # Now try to reduce the redundant computation to the level 1 halo
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop, {"depth": 1})
    assert ("supplied depth (1) must be greater than the existing halo depth "
            "(2)") in str(excinfo.value)
    # Second set our loop to redundantly compute to the maximum halo depth
    rc_trans.apply(loop)
    # Now try to reduce the redundant computation to a fixed value
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop, {"depth": 2})
    assert ("loop is already set to the maximum halo depth so can't be "
            "set to a fixed value") in str(excinfo.value)
    # Now try to set the redundant computation to the same (max) value
    # it is now
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop)
    assert ("loop is already set to the maximum halo depth so this "
            "transformation does nothing") in str(excinfo.value)


def test_rc_colour(tmpdir):
    ''' Test that we can redundantly compute over a colour in a coloured
    loop. '''
    psy, invoke = get_invoke("1_single_invoke.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule

    # Create our colour transformation
    ctrans = Dynamo0p3ColourTrans()
    # Colour the loop
    ctrans.apply(schedule.children[4])

    # Create our redundant computation transformation
    rc_trans = Dynamo0p3RedundantComputationTrans()
    # Apply redundant computation to the colour loop
    rc_trans.apply(schedule.children[4].loop_body[0], {"depth": 2})

    result = str(psy.gen)

    assert (
        "      IF (f2_proxy%is_dirty(depth=2)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=2)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=2)) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=2)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=2)) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=2)\n"
        "      END IF\n" in result)
    assert "      cmap => mesh%get_colour_map()\n" in result
    assert "loop0_stop = ncolour" in result
    assert ("last_halo_cell_all_colours = "
            "mesh%get_last_halo_cell_all_colours()" in result)
    assert (
        "      DO colour=loop0_start,loop0_stop\n"
        "        DO cell=loop1_start,last_halo_cell_all_colours(colour,2)\n"
        in result)

    # We've requested redundant computation out to the level 2 halo
    # but f1 is continuous and so the outermost halo depth (2) remains
    # dirty. This means that all of the halo is dirty apart from level
    # 1.
    assert (
        "      CALL f1_proxy%set_dirty()\n"
        "      CALL f1_proxy%set_clean(1)" in result)

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_rc_max_colour(tmpdir):
    ''' Test that we can redundantly compute over a colour to the maximum
    depth in a coloured loop. '''
    psy, invoke = get_invoke("1_single_invoke.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule

    # Create our colour transformation
    ctrans = Dynamo0p3ColourTrans()
    # Colour the loop
    ctrans.apply(schedule.children[4])

    # Create our redundant computation transformation
    rc_trans = Dynamo0p3RedundantComputationTrans()
    # Apply redundant computation to the colour loop out to the full
    # halo depth
    rc_trans.apply(schedule.children[4].loop_body[0])

    result = str(psy.gen)
    assert (
        "      IF (f2_proxy%is_dirty(depth=max_halo_depth_mesh)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=max_halo_depth_mesh)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=max_halo_depth_mesh)) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=max_halo_depth_mesh)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=max_halo_depth_mesh)) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=max_halo_depth_mesh)\n"
        "      END IF\n" in result)
    assert "      cmap => mesh%get_colour_map()\n" in result
    assert "loop0_stop = ncolour" in result
    assert ("last_halo_cell_all_colours = "
            "mesh%get_last_halo_cell_all_colours()" in result)
    assert (
        "      DO colour=loop0_start,loop0_stop\n"
        "        DO cell=loop1_start,last_halo_cell_all_colours(colour,"
        "max_halo_depth_mesh)\n"
        in result)

    assert (
        "      CALL f1_proxy%set_dirty()\n"
        "      CALL f1_proxy%set_clean(max_halo_depth_mesh-1)" in result)

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_colour_discontinuous():
    ''' Test that we raise an exception if we try to colour a loop
    containing a kernel that modifies a discontinuous field.
    The test is performed twice: first for a discontinuous wtheta writer
    and then for a discontinuous w2v readwriter. '''
    fsnames = ["wtheta", "w2v"]
    for name in fsnames:
        filename = "1_single_invoke_" + name + ".f90"
        _, invoke = get_invoke(filename, TEST_API, idx=0, dist_mem=True)
        schedule = invoke.schedule

        # Create our colour transformation
        ctrans = Dynamo0p3ColourTrans()

        with pytest.raises(TransformationError) as excinfo:
            # Colour the loop
            ctrans.apply(schedule.children[0])
        assert ("Loops iterating over a discontinuous function space are "
                "not currently supported") in str(excinfo.value)


def test_rc_then_colour(tmpdir):
    ''' Test that we generate correct code when we first perform redundant
    computation to a fixed depth then colour the loop.

    '''
    psy, invoke = get_invoke("1_single_invoke.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule

    # Create our colour transformation
    ctrans = Dynamo0p3ColourTrans()

    # Create our redundant computation transformation
    rc_trans = Dynamo0p3RedundantComputationTrans()

    # Apply redundant computation to the loop, out to the level-3 halo
    rc_trans.apply(schedule.children[4], {"depth": 3})

    # Colour the loop
    ctrans.apply(schedule.children[4])

    psy.invokes.invoke_list[0].schedule = schedule

    result = str(psy.gen)

    assert (
        "      IF (f2_proxy%is_dirty(depth=3)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=3)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=3)) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=3)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=3)) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=3)\n"
        "      END IF\n" in result)
    assert "      cmap => mesh%get_colour_map()\n" in result
    assert "loop0_stop = ncolour" in result
    assert ("last_halo_cell_all_colours = "
            "mesh%get_last_halo_cell_all_colours()" in result)
    assert (
        "      DO colour=loop0_start,loop0_stop\n"
        "        DO cell=loop1_start,last_halo_cell_all_colours(colour,3)\n"
        "          !\n"
        "          CALL testkern_code(nlayers, a, f1_data,"
        " f2_data, m1_data, m2_data, ndf_w1, undf_w1, "
        "map_w1(:,cmap(colour,cell)), ndf_w2, undf_w2, "
        "map_w2(:,cmap(colour,cell)), ndf_w3, undf_w3, "
        "map_w3(:,cmap(colour,cell)))\n" in result)

    assert (
        "      CALL f1_proxy%set_dirty()\n"
        "      CALL f1_proxy%set_clean(2)" in result)

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_rc_then_colour2(tmpdir):
    ''' Test that we generate correct code when we first perform redundant
    computation to the full depth then colour the loop.

    '''
    psy, invoke = get_invoke("1_single_invoke.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule

    # Create our colour transformation
    ctrans = Dynamo0p3ColourTrans()

    # Create our redundant computation transformation
    rc_trans = Dynamo0p3RedundantComputationTrans()

    # Apply redundant computation to the loop to the full halo depth
    rc_trans.apply(schedule[4])

    # Colour the loop
    ctrans.apply(schedule[4])

    psy.invokes.invoke_list[0].schedule = schedule

    result = str(psy.gen)

    assert (
        "      IF (f2_proxy%is_dirty(depth=max_halo_depth_mesh)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=max_halo_depth_mesh)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=max_halo_depth_mesh)) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=max_halo_depth_mesh)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=max_halo_depth_mesh)) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=max_halo_depth_mesh)\n"
        "      END IF\n" in result)
    assert "      cmap => mesh%get_colour_map()\n" in result
    assert "loop0_stop = ncolour" in result
    assert ("last_halo_cell_all_colours = mesh%"
            "get_last_halo_cell_all_colours()" in result)
    assert (
        "      DO colour=loop0_start,loop0_stop\n"
        "        DO cell=loop1_start,last_halo_cell_all_colours(colour,"
        "max_halo_depth_mesh)\n" in result)

    assert (
        "      CALL f1_proxy%set_dirty()\n"
        "      CALL f1_proxy%set_clean(max_halo_depth_mesh-1)" in result)

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_loop_fuse_then_rc(tmpdir):
    ''' Test that we are able to fuse two loops together, perform
    redundant computation and then colour. '''
    psy, invoke = get_invoke("4_multikernel_invokes.f90",
                             TEST_API, name="invoke_0", dist_mem=True)
    schedule = invoke.schedule

    ftrans = LFRicLoopFuseTrans()

    # Fuse the loops
    ftrans.apply(schedule.children[4], schedule.children[5])

    # Create our redundant computation transformation
    rc_trans = Dynamo0p3RedundantComputationTrans()

    # Apply redundant computation to the loop
    rc_trans.apply(schedule.children[4])

    # Create our colour transformation
    ctrans = Dynamo0p3ColourTrans()

    # Colour the loop
    ctrans.apply(schedule.children[4])

    psy.invokes.invoke_list[0].schedule = schedule

    result = str(psy.gen)

    assert "max_halo_depth_mesh = mesh%get_halo_depth()" in result
    assert (
        "      IF (f1_proxy%is_dirty(depth=max_halo_depth_mesh-1)) THEN\n"
        "        CALL f1_proxy%halo_exchange(depth=max_halo_depth_mesh-1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f2_proxy%is_dirty(depth=max_halo_depth_mesh)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=max_halo_depth_mesh)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=max_halo_depth_mesh)) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=max_halo_depth_mesh)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=max_halo_depth_mesh)) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=max_halo_depth_mesh)\n"
        "      END IF\n" in result)
    assert "      cmap => mesh%get_colour_map()\n" in result
    assert "loop0_stop = ncolour" in result
    assert ("last_halo_cell_all_colours = mesh%"
            "get_last_halo_cell_all_colours()" in result)
    assert (
        "      DO colour=loop0_start,loop0_stop\n"
        "        DO cell=loop1_start,last_halo_cell_all_colours(colour,"
        "max_halo_depth_mesh)\n" in result)
    assert (
        "      CALL f1_proxy%set_dirty()\n"
        "      CALL f1_proxy%set_clean(max_halo_depth_mesh-1)" in result)

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_haloex_colouring(tmpdir, monkeypatch, annexed):
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

        psy, invoke = get_invoke("14.10_halo_continuous_cell_w_to_r.f90",
                                 TEST_API, idx=0, dist_mem=True)
        schedule = invoke.schedule

        for cloop_idx in cloop_idxs:
            ctrans.apply(schedule.children[cloop_idx])

        halo_exchange = schedule.children[halo_idx]
        check_halo_exchange(halo_exchange)

        assert LFRicBuild(tmpdir).code_compiles(psy)

        print("OK for iteration ", idx)


def test_haloex_rc1_colouring(tmpdir, monkeypatch, annexed):
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
        assert halo_exchange._compute_halo_depth() == "max_halo_depth_mesh"
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

        psy, invoke = get_invoke("14.10_halo_continuous_cell_w_to_r.f90",
                                 TEST_API, idx=0, dist_mem=True)
        schedule = invoke.schedule

        if annexed:
            index = 3
        else:
            index = 5
        rc_trans.apply(schedule.children[index])

        for cloop_idx in cloop_idxs:
            ctrans.apply(schedule.children[cloop_idx])

        if annexed:
            halo_exchange = schedule.children[2]
        else:
            halo_exchange = schedule.children[4]
        check_halo_exchange(halo_exchange)

        assert LFRicBuild(tmpdir).code_compiles(psy)

        print("OK for iteration ", idx)


def test_haloex_rc2_colouring(tmpdir, monkeypatch, annexed):
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

        psy, invoke = get_invoke("14.10_halo_continuous_cell_w_to_r.f90",
                                 TEST_API, idx=0, dist_mem=True)
        schedule = invoke.schedule

        if annexed:
            index = 1
        else:
            index = w_loop_idx
        rc_trans.apply(schedule.children[index])

        for cloop in cloops:
            ctrans.apply(schedule.children[cloop])

        if annexed:
            index = 3
        else:
            index = 4
        halo_exchange = schedule.children[index]
        check_halo_exchange(halo_exchange)

        assert LFRicBuild(tmpdir).code_compiles(psy)

        print("OK for iteration ", idx)


def test_haloex_rc3_colouring(tmpdir, monkeypatch, annexed):
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
        assert halo_exchange._compute_halo_depth() == "max_halo_depth_mesh"
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
        psy, invoke = get_invoke("14.10_halo_continuous_cell_w_to_r.f90",
                                 TEST_API, idx=0, dist_mem=True)
        schedule = invoke.schedule

        if annexed:
            index1 = 1
            index2 = 4
        else:
            index1 = w_loop_idx
            index2 = r_loop_idx
        rc_trans.apply(schedule.children[index1])
        rc_trans.apply(schedule.children[index2])

        for cloop_idx in cloop_idxs:
            ctrans.apply(schedule.children[cloop_idx])

        if annexed:
            index = 3
        else:
            index = 4
        halo_exchange = schedule.children[index]
        check_halo_exchange(halo_exchange)

        assert LFRicBuild(tmpdir).code_compiles(psy)

        print("OK for iteration ", idx)


def test_haloex_rc4_colouring(tmpdir, monkeypatch, annexed):
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
    psy, invoke = get_invoke("14.10_halo_continuous_cell_w_to_r.f90",
                             TEST_API, idx=0, dist_mem=True)
    result = str(psy.gen)
    schedule = invoke.schedule

    if annexed:
        assert result.count("f1_proxy%halo_exchange(depth=1)") == 1
        assert isinstance(schedule.children[2], LFRicHaloExchange)
        assert schedule.children[2].field.name == "f1"
    else:
        assert result.count("f1_proxy%halo_exchange(depth=1)") == 2
        assert isinstance(schedule.children[0], LFRicHaloExchange)
        assert schedule.children[0].field.name == "f1"
        assert isinstance(schedule.children[4], LFRicHaloExchange)
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

        psy, invoke = get_invoke("14.10_halo_continuous_cell_w_to_r.f90",
                                 TEST_API, idx=0)
        schedule = invoke.schedule
        result = str(psy.gen)

        if annexed:
            index = 1
        else:
            index = w_loop_idx

        rc_trans.apply(schedule.children[index], {"depth": 2})

        for cloop_idx in cloop_idxs:
            ctrans.apply(schedule.children[cloop_idx])

        result = str(psy.gen)

        # the redundant computation code has one halo exchange for field f1
        assert result.count("f1_proxy%halo_exchange(depth=1)") == 1
        if annexed:
            index = 1
        else:
            index = 0
        assert isinstance(schedule.children[index], LFRicHaloExchange)
        assert schedule.children[index].field.name == "f1"

        assert LFRicBuild(tmpdir).code_compiles(psy)

        print("OK for iteration ", idx)


def test_intergrid_colour(dist_mem):
    ''' Check that we can apply colouring to a loop containing
    an inter-grid kernel. '''
    # Use an example that contains both prolongation and restriction
    # kernels
    psy, invoke = get_invoke("22.2_intergrid_3levels.f90",
                             TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    # First two kernels are prolongation, last two are restriction
    loops = schedule.walk(Loop)
    ctrans = Dynamo0p3ColourTrans()
    # To a prolong kernel
    ctrans.apply(loops[1])
    # To a restrict kernel
    ctrans.apply(loops[3])
    gen = str(psy.gen).lower()
    expected = '''\
      ncolour_fld_m = mesh_fld_m%get_ncolours()
      cmap_fld_m => mesh_fld_m%get_colour_map()'''
    assert expected in gen
    expected = '''\
      ncolour_cmap_fld_c = mesh_cmap_fld_c%get_ncolours()
      cmap_cmap_fld_c => mesh_cmap_fld_c%get_colour_map()'''
    assert expected in gen
    assert "loop1_stop = ncolour_fld_m" in gen
    assert "loop2_stop" not in gen
    if dist_mem:
        assert ("last_halo_cell_all_colours_fld_m = "
                "mesh_fld_m%get_last_halo_cell_all_colours()" in gen)
        expected = (
            "      do colour=loop1_start,loop1_stop\n"
            "        do cell=loop2_start,last_halo_cell_all_colours_fld_m"
            "(colour,1)\n")
    else:
        assert ("last_edge_cell_all_colours_fld_m = "
                "mesh_fld_m%get_last_edge_cell_all_colours()" in gen)
        expected = (
            "      do colour=loop1_start,loop1_stop\n"
            "        do cell=loop2_start,last_edge_cell_all_colours_fld_m"
            "(colour)\n")
    assert expected in gen
    expected = (
        "          call prolong_test_kernel_code(nlayers, cell_map_fld_m"
        "(:,:,cmap_fld_m(colour,cell)), ncpc_fld_f_fld_m_x, "
        "ncpc_fld_f_fld_m_y, ncell_fld_f, fld_f_data, fld_m_data, "
        "ndf_w1, undf_w1, map_w1, undf_w2, "
        "map_w2(:,cmap_fld_m(colour,cell)))\n")
    assert expected in gen


def test_intergrid_colour_errors(dist_mem, monkeypatch):
    ''' Check that we raise the expected error when colouring is not applied
    correctly to inter-grid kernels within a loop over colours. '''
    ctrans = Dynamo0p3ColourTrans()
    # Use an example that contains both prolongation and restriction kernels
    psy, invoke = get_invoke("22.2_intergrid_3levels.f90",
                             TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    # First two kernels are prolongation, last two are restriction
    loops = schedule.walk(Loop)
    loop = loops[1]
    # To a prolong kernel
    ctrans.apply(loop)
    # Update our list of loops
    loops = schedule.walk(Loop)
    # Trigger the error by calling the internal method to get the upper
    # bound before the colourmaps have been set-up
    with pytest.raises(InternalError) as err:
        _ = loops[1]._upper_bound_fortran()
    assert ("All kernels within a loop over colours must have been coloured "
            "but kernel 'prolong_test_kernel_code' has not" in str(err.value))
    # Set-up the colourmaps
    psy.invokes.invoke_list[0].meshes._colourmap_init()
    # Check that the upper bound is now correct
    upperbound = loops[1]._upper_bound_fortran()
    assert upperbound == "ncolour_fld_m"
    # Manually add an un-coloured kernel to the loop that we coloured
    loop = loops[2]
    kern = loops[3].loop_body[0].detach()
    monkeypatch.setattr(kern, "is_coloured", lambda: True)
    loop.loop_body.children.append(kern)
    with pytest.raises(InternalError) as err:
        _ = loops[1]._upper_bound_fortran()
    assert ("All kernels within a loop over colours must have been coloured "
            "but kernel 'restrict_test_kernel_code' has not" in str(err.value))


def test_intergrid_omp_parado(dist_mem, tmpdir):
    '''Check that we can add an OpenMP parallel loop to a loop containing
    an inter-grid kernel call.

    '''
    # Use an example that contains both prolongation and restriction
    # kernels
    psy, invoke = get_invoke("22.2_intergrid_3levels.f90",
                             TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    # First two kernels are prolongation, last two are restriction
    loops = schedule.walk(Loop)
    ctrans = Dynamo0p3ColourTrans()
    # To a prolong kernel
    ctrans.apply(loops[1])
    # To a restrict kernel
    ctrans.apply(loops[3])
    loops = schedule.walk(Loop)
    otrans = DynamoOMPParallelLoopTrans()
    # Apply OMP to loops over coloured cells
    otrans.apply(loops[2])
    otrans.apply(loops[5])
    gen = str(psy.gen)
    assert "loop4_stop = ncolour_cmap_fld_c" in gen
    assert ("      DO colour=loop4_start,loop4_stop\n"
            "        !$omp parallel do default(shared), private(cell), "
            "schedule(static)\n" in gen)

    if dist_mem:
        assert ("last_halo_cell_all_colours_cmap_fld_c = "
                "mesh_cmap_fld_c%get_last_halo_cell_all_colours()" in gen)
        assert ("DO cell=loop5_start,last_halo_cell_all_colours_cmap_fld_c"
                "(colour,1)\n" in gen)
    else:
        assert ("last_edge_cell_all_colours_cmap_fld_c = mesh_cmap_fld_c%"
                "get_last_edge_cell_all_colours()" in gen)
        assert ("DO cell=loop5_start,last_edge_cell_all_colours_cmap_fld_c"
                "(colour)\n" in gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_intergrid_omp_para_region1(dist_mem, tmpdir):
    ''' Check that we can create an OpenMP-parallel region containing
    a single inter-grid kernel call. '''
    psy, invoke = get_invoke("22.2_intergrid_3levels.f90",
                             TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    # Get the various transformations we need
    ctrans = Dynamo0p3ColourTrans()
    ptrans = OMPParallelTrans()
    otrans = Dynamo0p3OMPLoopTrans()
    # Colour the first loop (where 'cmap_fld_c' is the field on the coarse
    # mesh)
    loops = schedule.walk(Loop)
    ctrans.apply(loops[0])
    # Parallelise the loop over cells of a given colour
    loops = schedule.walk(Loop)
    otrans.apply(loops[1])
    # Put the parallel loop inside a parallel region
    dirs = schedule.walk(Directive)
    ptrans.apply(dirs[0])
    gen = str(psy.gen)
    if dist_mem:
        assert ("last_halo_cell_all_colours_cmap_fld_c = mesh_cmap_fld_c%"
                "get_last_halo_cell_all_colours()" in gen)
        upper_bound = "last_halo_cell_all_colours_cmap_fld_c(colour,1)"
    else:
        assert ("last_edge_cell_all_colours_cmap_fld_c = mesh_cmap_fld_c%"
                "get_last_edge_cell_all_colours()\n" in gen)
        upper_bound = "last_edge_cell_all_colours_cmap_fld_c(colour)"
    assert "loop0_stop = ncolour_cmap_fld_c\n" in gen
    assert (f"      DO colour=loop0_start,loop0_stop\n"
            f"        !$omp parallel default(shared), private(cell)\n"
            f"        !$omp do schedule(static)\n"
            f"        DO cell=loop1_start,{upper_bound}\n"
            f"          !\n"
            f"          CALL prolong_test_kernel_code(nlayers, "
            f"cell_map_cmap_fld_c(:,:,cmap_cmap_fld_c(colour,cell)), "
            f"ncpc_fld_m_cmap_fld_c_x, ncpc_fld_m_cmap_fld_c_y, ncell_fld_m, "
            f"fld_m_data, cmap_fld_c_data, ndf_w1, undf_w1, "
            f"map_w1, undf_w2, map_w2(:,cmap_cmap_fld_c(colour,cell)))\n"
            f"        END DO\n"
            f"        !$omp end do\n"
            f"        !$omp end parallel\n"
            f"      END DO\n" in gen)
    assert LFRicBuild(tmpdir).code_compiles(psy)


@pytest.mark.xfail(reason="Loop-fusion not yet supported for inter-grid "
                   "kernels")
def test_intergrid_omp_para_region2(dist_mem, tmpdir):
    ''' Check that we can create an OpenMP-parallel region containing
    multiple inter-grid kernels. '''
    psy, invoke = get_invoke("22.2_intergrid_3levels.f90",
                             TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    loops = schedule.walk(Loop)
    ctrans = Dynamo0p3ColourTrans()
    ftrans = LFRicLoopFuseTrans()
    ctrans.apply(loops[0])
    ctrans.apply(loops[1])
    loops = schedule.walk(Loop)
    ftrans.apply(loops[0], loops[2])
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_intergrid_err(dist_mem):
    ''' Check that we cannot apply redundant computation or loop
    fusion to loops containing inter-grid kernels. '''
    # Use an example that contains both prolongation and restriction
    # kernels
    _, invoke = get_invoke("22.2.1_intergrid_3levels_anyd.f90",
                           TEST_API, idx=0, dist_mem=dist_mem)
    schedule = invoke.schedule
    # First two kernels are prolongation, last two are restriction
    loops = schedule.walk(Loop)
    expected_err = (
        "cannot currently be applied to nodes which have inter-grid "
        "kernels as descendents and ")

    if dist_mem:
        # Cannot apply redundant computation unless DM is enabled
        rc_trans = Dynamo0p3RedundantComputationTrans()
        with pytest.raises(TransformationError) as excinfo:
            rc_trans.apply(loops[2], {"depth": 2})
            assert expected_err in str(excinfo.value)

    lftrans = LFRicLoopFuseTrans()
    with pytest.raises(TransformationError) as excinfo:
        lftrans.apply(loops[0], loops[1])
    assert expected_err in str(excinfo.value)

# Start OpenACC section


# Class ACCEnterDataTrans start
def test_accenterdatatrans():
    '''Test that an ACCEnterDataTrans transformation can add an OpenACC
    Enter Data directive to the PSy-layer in the dynamo0.3 API.

    '''
    acc_enter_trans = ACCEnterDataTrans()
    _, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                           name="invoke_0_testkern_type", dist_mem=False)
    sched = invoke.schedule
    acc_enter_trans.apply(sched)
    assert isinstance(sched[0], ACCEnterDataDirective)
    # This code can't be generated as ACCEnterData requires at least one
    # parallel directive within its region and this example does not
    # add one.


def test_accenterdata_builtin(tmpdir):
    ''' Check that the enter-data transformation can be applied to an invoke
    containing a call to a BuiltIn kernel.

    '''
    acc_enter_trans = ACCEnterDataTrans()
    parallel_trans = ACCParallelTrans()
    acc_loop_trans = ACCLoopTrans()
    ctrans = Dynamo0p3ColourTrans()
    psy, invoke = get_invoke("15.14.4_builtin_and_normal_kernel_invoke.f90",
                             TEST_API, name="invoke_0", dist_mem=False)
    sched = invoke.schedule
    for loop in sched.loops():
        # Colour all loops over cells so that they can be parallelised.
        if loop.loop_type == "":
            ctrans.apply(loop)
    for loop in sched.loops():
        if loop.loop_type in ["colour", "dof"]:
            acc_loop_trans.apply(loop)
    parallel_trans.apply(sched.children)
    acc_enter_trans.apply(sched)
    output = str(psy.gen).lower()

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert ("!$acc enter data copyin(f1_data,f2_data,m1_data,m2_data,"
            "map_w1,map_w2,map_w3,ndf_w1,ndf_w2,ndf_w3,"
            "nlayers,undf_w1,undf_w2,undf_w3)" in output)
    assert "loop2_stop = undf_aspc1_f1" in output
    assert ("      !$acc loop independent\n"
            "      do df=loop2_start,loop2_stop\n"
            "        f1_data(df) = 0.0_r_def\n"
            "      end do\n"
            "      !$acc end parallel\n" in output)

# Class ACCEnterDataTrans end


# Class ACCKernelsTrans start
def test_acckernelstrans():
    '''
    Test that an ACCKernelsTrans transformation can add an OpenACC
    Kernels directive to the PSy layer in the dynamo0.3 API.

    '''
    kernels_trans = ACCKernelsTrans()
    psy, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                             name="invoke_0_testkern_type", dist_mem=False)
    sched = invoke.schedule
    kernels_trans.apply(sched.children)
    code = str(psy.gen)
    assert "loop0_stop = f1_proxy%vspace%get_ncell()" in code
    assert (
        "      !$acc kernels\n"
        "      DO cell=loop0_start,loop0_stop\n" in code)
    assert (
        "      END DO\n"
        "      !$acc end kernels\n" in code)


def test_acckernelstrans_dm():
    '''
    Test that an ACCKernelsTrans transformation can add an OpenACC
    Kernels directive to the PSy layer in the LFRic (dynamo0.3) API when
    distributed memory is enabled.

    '''
    kernels_trans = ACCKernelsTrans()
    psy, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                             name="invoke_0_testkern_type", dist_mem=True)
    sched = invoke.schedule
    with pytest.raises(TransformationError) as err:
        kernels_trans.apply(sched.children)
    assert ("Nodes of type 'LFRicHaloExchange' cannot be enclosed by a "
            "ACCKernelsTrans transformation" in str(err.value))
    kernels_trans.apply(sched.walk(Loop))
    code = str(psy.gen)
    assert "loop0_stop = mesh%get_last_halo_cell(1)" in code
    assert (
        "      !$acc kernels\n"
        "      DO cell=loop0_start,loop0_stop\n" in code)
    assert (
        "      END DO\n"
        "      !$acc end kernels\n"
        "      !\n"
        "      ! Set halos dirty/clean for fields modified in the above "
        "loop(s)\n"
        "      !\n"
        "      CALL f1_proxy%set_dirty()\n" in code)

# Class ACCKernelsTrans end


# Class ACCParallelTrans start
def test_accparalleltrans(tmpdir):
    '''
    Test that an ACCParallelTrans transformation can add an OpenACC
    Parallel directive to the PSy layer in the dynamo0.3 API. An
    EnterData directive is also required otherwise the transformation
    raises an exception at code-generation time.

    '''
    acc_par_trans = ACCParallelTrans()
    acc_enter_trans = ACCEnterDataTrans()
    psy, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                             name="invoke_0_testkern_type", dist_mem=False)
    sched = invoke.schedule
    acc_par_trans.apply(sched.children)
    acc_enter_trans.apply(sched)
    code = str(psy.gen)
    assert "loop0_stop = f1_proxy%vspace%get_ncell()" in code
    assert (
        "      !$acc enter data copyin(f1_data,f2_data,m1_data,"
        "m2_data,map_w1,map_w2,map_w3,ndf_w1,ndf_w2,ndf_w3,nlayers,"
        "undf_w1,undf_w2,undf_w3)\n"
        "      !\n"
        "      !$acc parallel default(present)\n"
        "      DO cell=loop0_start,loop0_stop") in code
    assert (
        "      END DO\n"
        "      !$acc end parallel\n") in code

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_accparalleltrans_dm(tmpdir):
    '''
    Test that the ACCParallelTrans transformation works correctly when
    distributed memory is enabled. In particular any type-bound procedure
    calls related to halo exchanges must not be within the parallel region.

    '''
    acc_par_trans = ACCParallelTrans()
    acc_enter_trans = ACCEnterDataTrans()
    psy, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                             name="invoke_0_testkern_type", dist_mem=True)
    sched = invoke.schedule
    sched.view()
    # Cannot include halo-exchange nodes within an ACC parallel region.
    with pytest.raises(TransformationError) as err:
        acc_par_trans.apply(sched)
    assert ("Nodes of type 'LFRicHaloExchange' cannot be enclosed by a "
            "ACCParallelTrans transformation" in str(err.value))
    acc_par_trans.apply(sched.walk(Loop)[0])
    acc_enter_trans.apply(sched)
    code = str(psy.gen)

    assert ("      !$acc parallel default(present)\n"
            "      DO cell=loop0_start,loop0_stop\n"
            "        !\n"
            "        CALL testkern_code(nlayers, a, f1_data, "
            "f2_data, m1_data, m2_data, ndf_w1, undf_w1, "
            "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, "
            "undf_w3, map_w3(:,cell))\n"
            "      END DO\n"
            "      !$acc end parallel\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the above "
            "loop(s)\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n" in code)

    assert LFRicBuild(tmpdir).code_compiles(psy)


# Class ACCParallelTrans end
# Class ACCLoopTrans start


def test_acclooptrans():
    '''Test that an ACCLoopTrans transformation can add an OpenACC Loop
    directive to the PSy layer in the dynamo0.3 API.

    '''
    acc_par_trans = ACCParallelTrans()
    acc_loop_trans = ACCLoopTrans()
    acc_enter_trans = ACCEnterDataTrans()
    ctrans = Dynamo0p3ColourTrans()
    psy, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                             name="invoke_0_testkern_type", dist_mem=False)
    sched = invoke.schedule
    ctrans.apply(sched[0])
    acc_loop_trans.apply(sched[0].loop_body[0])
    acc_par_trans.apply(sched.children)
    acc_enter_trans.apply(sched)
    code = str(psy.gen)
    assert "loop0_stop = ncolour" in code
    assert (
        "      !$acc enter data copyin(f1_data,f2_data,m1_data,"
        "m2_data,map_w1,map_w2,map_w3,ndf_w1,ndf_w2,ndf_w3,nlayers,"
        "undf_w1,undf_w2,undf_w3)\n"
        "      !\n"
        "      !$acc parallel default(present)\n"
        "      DO colour=loop0_start,loop0_stop\n"
        "        !$acc loop independent\n"
        "        DO cell=loop1_start,last_edge_cell_all_colours(colour)"
        in code)
    assert (
        "      END DO\n"
        "      !$acc end parallel\n") in code

# Class ACCLoopTrans end

# End OpenACC section


def test_async_hex_wrong_node():
    '''Test that we raise the expected exception if an asynchronous halo
    exchange transformation is applied to a node that is not a halo
    exchange.

    '''
    node = Loop()
    ahex = Dynamo0p3AsyncHaloExchangeTrans()
    with pytest.raises(TransformationError) as err:
        ahex.apply(node)
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


def test_async_hex(tmpdir):
    ''' Test that we can convert a synchronous halo exchange to an
    asynchronous one using the Dynamo0p3AsyncHaloExchangeTrans transformation.

    '''
    psy, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                             idx=0, dist_mem=True)
    schedule = invoke.schedule
    f2_hex = schedule.children[1]
    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    ahex_trans.apply(f2_hex)
    result = str(psy.gen)
    assert (
        "      ! Call kernels and communication routines\n"
        "      !\n"
        "      IF (f1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f1_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy%halo_exchange_start(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy%halo_exchange_finish(depth=1)\n"
        "      END IF\n"
        "      !\n") in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_async_hex_move_1(tmpdir):
    ''' Test that we can convert a synchronous halo exchange to an
    asynchronous one using the Dynamo0p3AsyncHaloExchangeTrans
    transformation and then move them to new valid locations. In this
    case we move them before and after other halo exchanges
    respectively.

    '''
    psy, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                             idx=0, dist_mem=True)
    schedule = invoke.schedule
    m1_hex = schedule.children[2]
    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    ahex_trans.apply(m1_hex)

    mtrans = MoveTrans()
    mtrans.apply(schedule.children[2], schedule.children[1])
    mtrans.apply(schedule.children[4], schedule.children[3])
    result = str(psy.gen)
    assert (
        "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m1_proxy%halo_exchange_start(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m1_proxy%halo_exchange_finish(depth=1)\n"
        "      END IF\n") in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_async_hex_preserve_properties():
    '''Test that an asynchronous halo exchange created by the
    Dynamo0p3AsyncHaloExchangeTrans transformation maintains the properties
    of the original halo exchange.

    '''
    _, invoke = get_invoke("4.3_multikernel_invokes.f90", TEST_API,
                           idx=0, dist_mem=True)
    schedule = invoke.schedule

    # We don't need this halo exchange
    f2_hex = schedule.children[1]
    _, known = f2_hex.required()
    field_name = f2_hex.field.name
    stencil_type = f2_hex._compute_stencil_type()
    halo_depth = f2_hex._compute_halo_depth()

    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    ahex_trans.apply(f2_hex)
    f2_async_hex_start = schedule.children[1]

    _, f2_async_start_known = f2_async_hex_start.required()
    assert f2_async_start_known == known
    assert f2_async_hex_start.field.name == field_name
    assert f2_async_hex_start._compute_stencil_type() == stencil_type
    assert f2_async_hex_start._compute_halo_depth() == halo_depth

    f2_async_hex_end = schedule.children[2]
    _, f2_async_end_known = f2_async_hex_end.required()
    assert f2_async_end_known == known
    assert f2_async_hex_end.field.name == field_name
    assert f2_async_hex_end._compute_stencil_type() == stencil_type
    assert f2_async_hex_end._compute_halo_depth() == halo_depth

    # We do need this halo exchange
    f1_hex = schedule.children[6]
    _, known = f1_hex.required()
    field_name = f1_hex.field.name
    stencil_type = f1_hex._compute_stencil_type()
    halo_depth = f1_hex._compute_halo_depth()

    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    ahex_trans.apply(f1_hex)

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


def test_async_hex_move_2(tmpdir, monkeypatch):
    ''' Test that we can convert a synchronous halo exchange to an
    asynchronous one using the Dynamo0p3AsyncHaloExchangeTrans
    transformation and then move them to new valid locations. In this
    case we move a haloexchangestart before a loop.

    '''

    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", False)
    psy, invoke = get_invoke("4.5.2_multikernel_invokes.f90", TEST_API,
                             idx=0, dist_mem=True)
    schedule = invoke.schedule
    f2_hex = schedule.children[10]
    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    ahex_trans.apply(f2_hex)

    mtrans = MoveTrans()
    mtrans.apply(schedule.children[10], schedule.children[9])
    result = str(psy.gen)
    assert "loop3_stop = mesh%get_last_halo_cell(1)" in result
    assert (
        "      CALL f2_proxy%halo_exchange_start(depth=1)\n"
        "      !\n"
        "      DO cell=loop3_start,loop3_stop\n"
        "        !\n"
        "        CALL testkern_any_space_3_code(cell, nlayers, "
        "op_proxy%ncell_3d, op_local_stencil, ndf_aspc1_op, "
        "ndf_aspc2_op)\n"
        "      END DO\n"
        "      CALL f2_proxy%halo_exchange_finish(depth=1)\n") in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_async_hex_move_error_1():
    '''Test that an asynchronous halo exchange start can not be moved
    after its end and its end cannot be moved before its start.

    '''
    _, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                           idx=0, dist_mem=True)
    schedule = invoke.schedule

    m1_hex = schedule.children[1]
    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    ahex_trans.apply(m1_hex)

    mtrans = MoveTrans()

    # end before start
    with pytest.raises(TransformationError) as excinfo:
        mtrans.apply(schedule.children[2], schedule.children[1])
    assert "dependencies forbid" in str(excinfo.value)

    # start after end
    with pytest.raises(TransformationError) as excinfo:
        mtrans.apply(schedule.children[1], schedule.children[3])
    assert "dependencies forbid" in str(excinfo.value)


def test_async_hex_move_error_2():
    '''Test that an asynchronous halo exchange start can not be moved
    before a kernel that modifies the field and its end cannot be
    moved after a kernel that reads the field.

    '''
    _, invoke = get_invoke("4.3_multikernel_invokes.f90", TEST_API,
                           idx=0, dist_mem=True)
    schedule = invoke.schedule

    f1_hex = schedule.children[5]
    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    ahex_trans.apply(f1_hex)

    mtrans = MoveTrans()

    # Start before prev modifier
    with pytest.raises(TransformationError) as excinfo:
        mtrans.apply(schedule.children[5], schedule.children[4])
    assert "dependencies forbid" in str(excinfo.value)

    # End after following reader
    with pytest.raises(TransformationError) as excinfo:
        mtrans.apply(schedule.children[6], schedule.children[7],
                     {"position": "after"})
    assert "dependencies forbid" in str(excinfo.value)


def test_rc_remove_async_halo_exchange(monkeypatch, tmpdir):
    '''Test that an asynchronous halo exchange is removed if redundant
    computation means that it is no longer required. Halo exchanges
    are not required in this example when we compute annexed
    dofs. Therefore we ensure we compute over owned dofs (via
    monkeypatch) to perform the test.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", False)
    psy, invoke = get_invoke("14.7_halo_annexed.f90", TEST_API,
                             idx=0, dist_mem=True)
    schedule = invoke.schedule

    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()

    f2_hex = schedule.children[3]
    ahex_trans.apply(f2_hex)
    f1_hex = schedule.children[2]
    ahex_trans.apply(f1_hex)

    result = str(psy.gen)
    assert "CALL f1_proxy%halo_exchange_start(depth=1)" in result
    assert "CALL f1_proxy%halo_exchange_finish(depth=1)" in result
    assert "CALL f2_proxy%halo_exchange_start(depth=1)" in result
    assert "CALL f2_proxy%halo_exchange_finish(depth=1)" in result
    assert "IF (m1_proxy%is_dirty(depth=1)) THEN" in result
    assert "CALL m1_proxy%halo_exchange(depth=1)" in result

    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[0]
    rc_trans.apply(loop, {"depth": 1})
    result = str(psy.gen)
    assert "CALL f1_proxy%halo_exchange_start(depth=1)" not in result
    assert "CALL f1_proxy%halo_exchange_finish(depth=1)" not in result
    assert "CALL f2_proxy%halo_exchange_start(depth=1)" in result
    assert "CALL f2_proxy%halo_exchange_finish(depth=1)" in result
    assert "IF (m1_proxy%is_dirty(depth=1)) THEN" in result
    assert "CALL m1_proxy%halo_exchange(depth=1)" in result
    #
    loop = schedule.children[1]
    rc_trans.apply(loop, {"depth": 1})
    result = str(psy.gen)
    assert "CALL f1_proxy%halo_exchange_start(depth=1)" not in result
    assert "CALL f1_proxy%halo_exchange_finish(depth=1)" not in result
    assert "CALL f2_proxy%halo_exchange_start(depth=1)" not in result
    assert "CALL f2_proxy%halo_exchange_finish(depth=1)" not in result
    assert "IF (m1_proxy%is_dirty(depth=1)) THEN" in result
    assert "CALL m1_proxy%halo_exchange(depth=1)" in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_rc_redund_async_halo_exchange(monkeypatch, tmpdir):
    '''Test that an asynchronous halo exchange works correctly with
    redundant computation being applied.
    '''

    # ensure we compute over annexed dofs so no halo exchanges are required
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", True)
    psy, invoke = get_invoke("14.7_halo_annexed.f90", TEST_API,
                             idx=0, dist_mem=True)
    schedule = invoke.schedule

    # Make it so that halo exchanges are required to depth 2 for
    # fields m1, m2, f1 and f2. m2 will have a set clean for depth 1
    # after the last loop.
    rc_trans = Dynamo0p3RedundantComputationTrans()
    loop = schedule.children[2]
    rc_trans.apply(loop, {"depth": 2})

    # make m2 halo exchange asynchronous and check depths and set
    # clean are generated correctly for m2
    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    m2_hex = schedule.children[5]
    ahex_trans.apply(m2_hex)
    result = str(psy.gen)
    assert (
        "      IF (m2_proxy%is_dirty(depth=2)) THEN\n"
        "        CALL m2_proxy%halo_exchange_start(depth=2)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=2)) THEN\n"
        "        CALL m2_proxy%halo_exchange_finish(depth=2)\n"
        "      END IF\n") in result
    assert (
        "      ! Set halos dirty/clean for fields modified in the above loop\n"
        "      !\n"
        "      CALL m2_proxy%set_dirty()\n"
        "      CALL m2_proxy%set_clean(2)\n") in result

    # move m2 async halo exchange start and end then check depths and
    # set clean are still generated correctly for m2
    mtrans = MoveTrans()
    mtrans.apply(schedule.children[5], schedule.children[0])
    mtrans.apply(schedule.children[6], schedule.children[2])
    result = str(psy.gen)
    assert (
        "      IF (m2_proxy%is_dirty(depth=2)) THEN\n"
        "        CALL m2_proxy%halo_exchange_start(depth=2)\n"
        "      END IF\n") in result
    assert (
        "      IF (m2_proxy%is_dirty(depth=2)) THEN\n"
        "        CALL m2_proxy%halo_exchange_finish(depth=2)\n"
        "      END IF\n") in result
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
        rc_trans.apply(loop, {"depth": 3})
    result = str(psy.gen)
    assert (
        "      IF (m2_proxy%is_dirty(depth=3)) THEN\n"
        "        CALL m2_proxy%halo_exchange_start(depth=3)\n"
        "      END IF\n") in result
    assert (
        "      IF (m2_proxy%is_dirty(depth=3)) THEN\n"
        "        CALL m2_proxy%halo_exchange_finish(depth=3)\n"
        "      END IF\n") in result
    assert (
        "      ! Set halos dirty/clean for fields modified in the above loop\n"
        "      !\n"
        "      CALL m2_proxy%set_dirty()\n"
        "      CALL m2_proxy%set_clean(3)\n") in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


@pytest.mark.xfail(reason="dependence analysis thinks independent vectors "
                   "depend on each other")
def test_move_vector_halo_exchange():
    '''Test that halo exchanges for different vectors for the same field
    are independent of each other, i.e. they do not depend on
    each other.

    '''
    _, invoke = get_invoke("8.3_multikernel_invokes_vector.f90", TEST_API,
                           idx=0, dist_mem=True)
    schedule = invoke.schedule

    # reverse the order of the vector halo exchanges
    mtrans = MoveTrans()
    mtrans.apply(schedule.children[1], schedule.children[0])
    mtrans.apply(schedule.children[2], schedule.children[1])
    # When the test is fixed, add a check for re-ordered output here


def test_vector_halo_exchange_remove():
    '''test that we remove vector halo exchanges when they are no longer
    required.

    '''
    _, invoke = get_invoke("8.3_multikernel_invokes_vector.f90", TEST_API,
                           idx=0, dist_mem=True)
    schedule = invoke.schedule

    # remove second set of halo exchanges via redundant
    # computation. If they are removed correctly then the two loops
    # will be adjacent to each other and will follow 3 haloexchange
    # calls.
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[3], {"depth": 2})
    assert len(schedule.children) == 5
    for index in [0, 1, 2]:
        assert isinstance(schedule.children[index], LFRicHaloExchange)
    assert isinstance(schedule.children[3], LFRicLoop)
    assert isinstance(schedule.children[4], LFRicLoop)


def test_vector_async_halo_exchange(tmpdir):
    '''Test that an asynchronous halo exchange works correctly with
    vector fields.
    '''
    psy, invoke = get_invoke("8.3_multikernel_invokes_vector.f90", TEST_API,
                             idx=0, dist_mem=True)
    schedule = invoke.schedule

    # Create vector halo exchanges after the first loop by performing
    # redundant computation.
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[4], {"depth": 2})
    # make all f1 vector halo exchanges asynchronous before the first
    # loop and one of them before the second loop, then check depths
    # and set clean are still generated correctly
    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    for index in [5, 2, 1, 0]:
        my_hex = schedule.children[index]
        ahex_trans.apply(my_hex)
    result = str(psy.gen)
    for index in [1, 2, 3]:
        assert (
            f"      IF (f1_proxy({index})%is_dirty(depth=1)) THEN\n"
            f"        CALL f1_proxy({index})%halo_exchange_start(depth=1)\n"
            f"      END IF\n"
            f"      !\n"
            f"      IF (f1_proxy({index})%is_dirty(depth=1)) THEN\n"
            f"        CALL f1_proxy({index})%halo_exchange_finish(depth=1)\n"
            f"      END IF\n") in result
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
    # mtrans.apply(schedule.children[2], schedule.children[1])
    # mtrans.apply(schedule.children[4], schedule.children[2])

    # remove second set of halo exchanges via redundant
    # computation. If they are removed correctly then the two loops
    # will be adjacent to each other and will follow 6 haloexchange
    # start and end calls.
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[6], {"depth": 2})

    assert len(schedule.children) == 8
    for index in [0, 2, 4]:
        assert isinstance(schedule.children[index], LFRicHaloExchangeStart)
        assert isinstance(schedule.children[index+1], LFRicHaloExchangeEnd)
    assert isinstance(schedule.children[6], LFRicLoop)
    assert isinstance(schedule.children[7], LFRicLoop)

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_async_halo_exchange_nomatch1():
    '''Test that an exception is raised if an asynchronous halo exchange
    start matches with something other than the expected halo exchange
    end.

    '''
    _, invoke = get_invoke("8.3_multikernel_invokes_vector.f90", TEST_API,
                           idx=0, dist_mem=True)
    schedule = invoke.schedule

    # create vector halo exchanges after the first loop by performing
    # redundant computation
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[4], {"depth": 2})

    # make the first vector component of the halo exchange for f1
    # asynchronous before the first loop.
    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    my_hex = schedule.children[0]
    ahex_trans.apply(my_hex)

    # now remove the generated halo exchange end. This will mean that
    # the halo exchange start will now match with the halo exchange
    # for the first vector component after the loop (which is a
    # standard halo exchange). This should cause an exception to be
    # raised.
    del schedule.children[1]

    hex_start = schedule.children[0]
    with pytest.raises(GenerationError) as excinfo:
        hex_start._get_hex_end()
    assert ("Halo exchange start for field 'f1' should match with a halo "
            "exchange end, but found <class 'psyclone.dynamo0p3."
            "LFRicHaloExchange'>") in str(excinfo.value)


def test_async_halo_exchange_nomatch2():
    '''Test that an exception is raised if an asynchronous halo exchange
    start matches with no other halo exchange with the same name.

    '''
    _, invoke = get_invoke("8.3_multikernel_invokes_vector.f90", TEST_API,
                           idx=0, dist_mem=True)
    schedule = invoke.schedule

    # make the last vector component of the halo exchange for f1
    # asynchronous after the first loop.
    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    my_hex = schedule.children[0]
    ahex_trans.apply(my_hex)

    # now remove the generated halo exchange end. This will mean that
    # the halo exchange start will now match with nothing as it is the
    # last halo exchange in the schedule. This should cause an
    # exception to be raised.
    del schedule.children[1]

    hex_start = schedule.children[0]
    with pytest.raises(GenerationError) as excinfo:
        hex_start._get_hex_end()
    assert ("Halo exchange start for field 'f1' has no matching halo "
            "exchange end") in str(excinfo.value)

# tests for Dynamo0p3KernelConstTrans transformation


def create_kernel(file_name):
    '''Utility function that returns the first kernel object from the
    PSyIR schedule generated from processing the test code provided in
    the 'file_name' argument. Assumes that this file is a dynamo0p3
    test file and that the first kernel is the child of the first
    child in the schedule.

    :param str file_name: The name of the dynamo0p3 example algorithm \
    file.

    '''
    _, invoke = get_invoke(file_name, TEST_API,
                           idx=0, dist_mem=False)
    schedule = invoke.schedule
    kernel = schedule.children[0].loop_body[0]
    return kernel


def test_kern_const_str():
    ''' String test for the Dynamo0p3KernelConstTrans class. '''
    kct = Dynamo0p3KernelConstTrans()
    assert (str(kct) == "Makes the number of degrees of freedom, the number "
            "of quadrature points and the number of layers constant in a "
            "Kernel.")


def test_kern_const_name():
    ''' Name test for the Dynamo0p3KernelConstTrans class. '''
    kct = Dynamo0p3KernelConstTrans()
    assert kct.name == "Dynamo0p3KernelConstTrans"


def test_kern_const_apply(capsys, monkeypatch):
    '''Check that we generate the expected output from the apply method
    with different valid combinations of the element_order,
    number_of_layers and quadrature arguments.

    '''
    kernel = create_kernel("1.1.0_single_invoke_xyoz_qr.f90")

    kctrans = Dynamo0p3KernelConstTrans()

    element_order_expected = (
        "    Modified ndf_w1, arg position 8, function space w1, value 12.\n"
        "    Modified ndf_w2, arg position 12, function space w2, value 6.\n"
        "    Modified ndf_w3, arg position 16, function space w3, value 1.\n")
    number_of_layers_expected = (
        "    Modified nlayers, arg position 1, value 20.\n")
    quadrature_expected = (
        "    Modified nqp_h, arg position 21, value 3.\n"
        "    Modified nqp_v, arg position 22, value 3.\n")

    # element_order only
    kctrans.apply(kernel, {"element_order": 0})
    result, _ = capsys.readouterr()
    assert result == element_order_expected

    # nlayers only
    kernel = create_kernel("1.1.0_single_invoke_xyoz_qr.f90")
    kctrans.apply(kernel, {"number_of_layers": 20})
    result, _ = capsys.readouterr()
    assert result == number_of_layers_expected

    # element_order and quadrature
    kernel = create_kernel("1.1.0_single_invoke_xyoz_qr.f90")
    kctrans.apply(kernel, {"element_order": 0, "quadrature": True})
    result, _ = capsys.readouterr()
    assert result == quadrature_expected + element_order_expected

    # element_order and nlayers
    kernel = create_kernel("1.1.0_single_invoke_xyoz_qr.f90")
    kctrans.apply(kernel, {"element_order": 0, "number_of_layers": 20})
    result, _ = capsys.readouterr()
    assert result == number_of_layers_expected + element_order_expected

    # element_order, nlayers and quadrature
    kernel = create_kernel("1.1.0_single_invoke_xyoz_qr.f90")
    kctrans.apply(kernel, {"element_order": 0, "number_of_layers": 20,
                           "quadrature": True})
    result, _ = capsys.readouterr()
    assert result == number_of_layers_expected + quadrature_expected + \
        element_order_expected

    # Pass in no parameter. The validate function would normally
    # reject this, so disable the validation function to test
    # handling of options=None in the apply function.
    monkeypatch.setattr(kctrans, "validate",
                        lambda loop, options: None)
    kctrans.apply(kernel)
    result, _ = capsys.readouterr()
    # In case of no options, the transformation does not do anything
    assert result == ""


def test_kern_const_anyspace_anydspace_apply(capsys):
    ''' Check that we generate the expected output from the apply method
    when a function space is specified as any_space and
    any_discontinuous_space (as these are skipped by the transformation).

    '''
    kernel = create_kernel("1.5.3_single_invoke_write_any_anyd_space.f90")

    kctrans = Dynamo0p3KernelConstTrans()

    kctrans.apply(kernel, {"element_order": 0})
    result, _ = capsys.readouterr()
    assert result == (
        "    Skipped dofs, arg position 9, function space any_space_1\n"
        "    Modified ndf_w2, arg position 12, function space w2, value 6.\n"
        "    Modified ndf_w1, arg position 15, function space w1, value 12.\n"
        "    Skipped dofs, arg position 18, function space "
        "any_discontinuous_space_1\n"
        "    Modified ndf_wtheta, arg position 21, function space wtheta, "
        "value 2.\n"
        "    Modified ndf_w2h, arg position 24, function space w2h, value 4.\n"
        "    Modified ndf_w2v, arg position 27, function space w2v, "
        "value 2.\n")


def test_kern_const_anyw2_apply(capsys):
    '''Check that we generate the expected output from the apply method
    when a function space is specified as any_w2_space (as these are
    skipped by the transformation).

    '''
    kernel = create_kernel("21.1_single_invoke_multi_anyw2.f90")

    kctrans = Dynamo0p3KernelConstTrans()

    kctrans.apply(kernel, {"element_order": 0})
    result, _ = capsys.readouterr()
    assert result == (
        "    Skipped dofs, arg position 5, function space any_w2\n")


# space_to_dofs values
def test_kern_const_ndofs():
    '''Test the computed number-of-dof values per 3D cell on a quadrilateral
    element for different orders and different function spaces.
    Note: w2*trace spaces have their dofs on cell faces only.

    '''
    expected = {"w3": [1, 8, 27, 64, 125, 216, 343, 512, 729, 1000],
                "w2": [6, 36, 108, 240, 450, 756, 1176, 1728, 2430, 3300],
                "w1": [12, 54, 144, 300, 540, 882, 1344, 1944, 2700, 3630],
                "w0": [8, 27, 64, 125, 216, 343, 512, 729, 1000, 1331],
                "wtheta": [2, 12, 36, 80, 150, 252, 392, 576, 810, 1100],
                "w2h": [4, 24, 72, 160, 300, 504, 784, 1152, 1620, 2200],
                "w2v": [2, 12, 36, 80, 150, 252, 392, 576, 810, 1100],
                "w2broken": [6, 36, 108, 240, 450, 756, 1176, 1728, 2430,
                             3300],
                "wchi": [1, 8, 27, 64, 125, 216, 343, 512, 729, 1000],
                "w2trace": [6, 24, 54, 96, 150, 216, 294, 384, 486, 600],
                "w2htrace": [4, 16, 36, 64, 100, 144, 196, 256, 324, 400],
                "w2vtrace": [2, 8, 18, 32, 50, 72, 98, 128, 162, 200]}
    kct = Dynamo0p3KernelConstTrans()
    for order in range(10):
        for function_space in ["w3", "w2", "w1", "w0", "wtheta", "w2h",
                               "w2v", "w2broken", "wchi", "w2trace",
                               "w2htrace", "w2vtrace"]:
            assert kct.space_to_dofs[function_space](order) == \
                expected[function_space][order]
        # wtheta should equal w2v
        assert kct.space_to_dofs["wtheta"](order) == \
            kct.space_to_dofs["w2v"](order)
        # w2h and w2v should sum up to w2
        assert kct.space_to_dofs["w2h"](order) + \
            kct.space_to_dofs["w2v"](order) == kct.space_to_dofs["w2"](order)
        # w2htrace and w2vtrace should sum up to w2trace
        assert kct.space_to_dofs["w2htrace"](order) + \
            kct.space_to_dofs["w2vtrace"](order) == \
            kct.space_to_dofs["w2trace"](order)


def test_kern_const_invalid():
    '''Check that we generate the expected exceptions from the validate
    method when there are errors in the input arguments. We call the
    apply method as that calls the validate method in turn.

    '''
    kernel = create_kernel("1_single_invoke.f90")

    kctrans = Dynamo0p3KernelConstTrans()

    # Node is not a dynamo kernel
    with pytest.raises(TransformationError) as excinfo:
        kctrans.apply(None)
    assert "Supplied node must be a dynamo kernel" in str(excinfo.value)

    # Cell shape not quadrilateral
    with pytest.raises(TransformationError) as excinfo:
        kctrans.apply(kernel, {"cellshape": "rotund"})
    assert ("Supplied cellshape must be set to 'quadrilateral' but found "
            "'rotund'.") in str(excinfo.value)

    # Element order < 0
    with pytest.raises(TransformationError) as excinfo:
        kctrans.apply(kernel, {"element_order": -1})
    assert "The element_order argument must be >= 0 but found '-1'." \
        in str(excinfo.value)

    # Number of layers < 1
    with pytest.raises(TransformationError) as excinfo:
        kctrans.apply(kernel, {"number_of_layers": 0})
    assert "The number_of_layers argument must be > 0 but found '0'." \
        in str(excinfo.value)

    # Quadrature not a boolean
    with pytest.raises(TransformationError) as excinfo:
        kctrans.apply(kernel, {"quadrature": "hello"})
    assert "The quadrature argument must be boolean but found 'hello'." \
        in str(excinfo.value)

    # Not element order and not number of layers
    with pytest.raises(TransformationError) as excinfo:
        kctrans.apply(kernel)
    assert ("At least one of element_order or number_of_layers must be set "
            "otherwise this transformation does nothing.") \
        in str(excinfo.value)

    # Quadrature but not element order
    with pytest.raises(TransformationError) as excinfo:
        kctrans.apply(kernel, {"number_of_layers": 20,
                               "quadrature": True})
    assert "If quadrature is set then element_order must also be set" \
        in str(excinfo.value)


def test_kern_const_invalid_dofs(monkeypatch):
    '''Check that we generate the expected exception when an unexpected
    function-space name is found.

    '''
    kernel = create_kernel("1_single_invoke.f90")

    kctrans = Dynamo0p3KernelConstTrans()
    monkeypatch.setattr(Dynamo0p3KernelConstTrans, "space_to_dofs",
                        {"wa": [], "wb": []})

    with pytest.raises(InternalError) as excinfo:
        kctrans.apply(kernel, {"element_order": 0})
    assert "Unsupported function space 'w1' found. Expecting one of " \
        in str(excinfo.value)
    assert "'wa'" in str(excinfo.value)
    assert "'wb'" in str(excinfo.value)


def test_kern_const_invalid_kern(monkeypatch):
    '''Check that we raise the expected exception when the Fortran to
    PSyIR parser fails to parse a kernel.

    '''
    kernel = create_kernel("1_single_invoke.f90")

    kctrans = Dynamo0p3KernelConstTrans()

    def dummy():
        '''A dummy function that always raises an exception.'''
        raise NotImplementedError("Monkeypatch error")
    monkeypatch.setattr(kernel, "get_kernel_schedule", dummy)
    with pytest.raises(TransformationError) as excinfo:
        kctrans.apply(kernel, {"element_order": 0})
    assert (
        "Failed to parse kernel 'testkern_code'. Error reported was "
        "'Monkeypatch error'.") in str(excinfo.value)


def test_kern_const_invalid_quad(monkeypatch):
    '''Check that we raise the expected exception when the type of
    quadrature is not supported by the transformation (we are
    currently limited to XYoZ).

    '''
    kernel = create_kernel("1.1.0_single_invoke_xyoz_qr.f90")

    kctrans = Dynamo0p3KernelConstTrans()
    monkeypatch.setattr(kernel, "_eval_shapes", ["gh_quadrature_face"])
    with pytest.raises(TransformationError) as excinfo:
        kctrans.apply(kernel, {"element_order": 0, "quadrature": True})
    assert (
        "Support is currently limited to 'xyoz' quadrature but found "
        "['gh_quadrature_face'].") in str(excinfo.value)


def test_kern_const_invalid_make_constant1():
    '''Check that the expected exception is raised when the make_constant
    utility function (found in the apply method) encounters an invalid
    index. This is done by removing the argument list entries from a
    kernel.

    '''
    kernel = create_kernel("1.1.0_single_invoke_xyoz_qr.f90")

    kernel_schedule = kernel.get_kernel_schedule()
    symbol_table = kernel_schedule.symbol_table
    # Make the symbol table's argument list empty. We have to make sure that
    # the interface of any existing argument Symbols is set to
    # AutomaticInterface first otherwise we fall foul of our
    # internal-consistency checks.
    for symbol in symbol_table.argument_list:
        symbol.interface = AutomaticInterface()
    symbol_table._argument_list = []
    kctrans = Dynamo0p3KernelConstTrans()
    with pytest.raises(TransformationError) as excinfo:
        kctrans.apply(kernel, {"element_order": 0})
    assert ("The argument index '7' is greater than the number of "
            "arguments '0'.") in str(excinfo.value)


def test_kern_const_invalid_make_constant2():
    '''Check that the expected exception is raised when the make_constant
    utility function (found in the apply method) encounters a Symbol
    at the specified index that is not a scalar integer argument.
    This is done by modifying one of the Symbol entries to be a real
    rather than an integer.

    '''
    kernel = create_kernel("1.1.0_single_invoke_xyoz_qr.f90")

    kctrans = Dynamo0p3KernelConstTrans()
    kernel_schedule = kernel.get_kernel_schedule()
    symbol_table = kernel_schedule.symbol_table
    symbol = symbol_table._argument_list[7]
    # Expecting scalar integer. Set to array.
    symbol._datatype = ArrayType(INTEGER_TYPE, [10])
    with pytest.raises(TransformationError) as excinfo:
        kctrans.apply(kernel, {"element_order": 0})
    assert ("Expected entry to be a scalar argument but found "
            "'ArrayType'." in str(excinfo.value))
    # Expecting scalar integer. Set to real.
    symbol._datatype = REAL_TYPE
    with pytest.raises(TransformationError) as excinfo:
        kctrans.apply(kernel, {"element_order": 0})
    assert ("Expected entry to be a scalar integer argument but found "
            "'Scalar<REAL, UNDEFINED>'." in str(excinfo.value))
    # Expecting scalar integer. Set to constant.
    symbol._datatype = ScalarType(ScalarType.Intrinsic.INTEGER,
                                  ScalarType.Precision.UNDEFINED)
    symbol._initial_value = Literal("10", INTEGER_TYPE)
    symbol._is_constant = True
    with pytest.raises(TransformationError) as excinfo:
        kctrans.apply(kernel, {"element_order": 0})
    assert ("Expected entry to be a scalar integer argument but found "
            "a constant." in str(excinfo.value))


def test_all_loop_trans_base_validate(monkeypatch):
    ''' Check that all LFRic (Dynamo) transformations that sub-class LoopTrans
    call the base validate() method. '''
    # First get a valid Loop object that we can pass in.
    _, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                           name="invoke_0_testkern_type", dist_mem=False)
    loop = invoke.schedule.walk(Loop)[0]

    # LFRic-domain transformations should live in the `domain` module. However,
    # there are still others in psyclone.transformations but these are tested
    # by the generic tests/psyir/transformations/loop_trans_test.py file.
    transmod = import_module("psyclone.domain.lfric.transformations")
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
                f"{name}.validate() does not call LoopTrans.validate()"
