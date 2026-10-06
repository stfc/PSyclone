# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Tests of the LFRicLoopFuseTrans '''

import pytest

from psyclone.configuration import Config
from psyclone.lfric import LFRicLoop, LFRicHaloExchange
from psyclone.domain.lfric.transformations import LFRicLoopFuseTrans
from psyclone.psyir.transformations import (TransformationError, MoveTrans)
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.utilities import get_invoke

# The version of the API that the tests in this file
# exercise.
TEST_API = "lfric"


def test_loopfuse(dist_mem, tmpdir):
    ''' Tests whether loop fuse actually fuses and whether
    multiple maps are produced or not. Multiple maps are not an
    error but it would be nicer if there were only one '''
    psy, invoke = get_invoke("4_multikernel_invokes_fusable_distmem.f90",
                             api=TEST_API, dist_mem=dist_mem, idx=0)
    schedule = invoke.schedule
    loops = schedule.walk(LFRicLoop)
    halo_exchs = schedule.walk(LFRicHaloExchange)
    if dist_mem:
        # Move the second loop to after the first 2 sets of 4 halo exchanges,
        # instead of between the 3rd and 4th.
        mtrans = MoveTrans()
        mtrans.apply(loops[0], halo_exchs[7], position="after")
    trans = LFRicLoopFuseTrans()
    trans.apply(loops[0:2])
    generated_code = psy.gen
    # only one loop
    assert str(generated_code).count("do cell") == 1
    # only one map for each space
    assert str(generated_code).count("map_w1 =>") == 1
    assert str(generated_code).count("map_w2 =>") == 1
    assert str(generated_code).count("map_w3 =>") == 1
    # kernel call tests
    kern_idxs = []
    for idx, line in enumerate(str(generated_code).split('\n')):
        if "do cell" in line:
            do_idx = idx
        if "call testkern_code(" in line:
            kern_idxs.append(idx)
        if "enddo" in line:
            enddo_idx = idx
    # two kernel calls
    assert len(kern_idxs) == 2
    # both kernel calls are within the loop
    for kern_id in kern_idxs:
        assert enddo_idx > kern_id > do_idx

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_loop_fuse_invalid_space(monkeypatch):
    ''' Test that we raise an appropriate error if the user attempts
    to fuse loops that are on invalid spaces.
    '''
    _, first_invoke = get_invoke("4_multikernel_invokes.f90", TEST_API,
                                 idx=0, dist_mem=False)
    schedule = first_invoke.schedule
    loops = schedule.walk(LFRicLoop)
    first_kernel_args = schedule.coded_kernels()[0].arguments
    # Get argument on the "write" space w1
    _, fspace = first_kernel_args.get_arg_on_space_name("w1")
    # Make function space invalid
    monkeypatch.setattr(fspace, "_orig_name", "not_a_space_name")

    # Apply transformation and raise the error
    ftrans = LFRicLoopFuseTrans()
    with pytest.raises(
        TransformationError,
        match="One or both function spaces 'not_a_space_name' and 'w1' have "
              "invalid names"):
        ftrans.apply(loops[0:2])


def test_loop_fuse_different_spaces(monkeypatch, dist_mem):
    ''' Test that we raise an appropriate error if the user attempts
    fuse loops that are on different spaces (unless they are both on
    discontinuous spaces). We test with annexed is False as this is
    how the test has been set up.

    '''
    lfric_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(lfric_config, "_compute_annexed_dofs", False)
    for same_space in [False, True]:
        _, invoke = get_invoke("4.7_multikernel_invokes.f90",
                               TEST_API, name="invoke_0", dist_mem=dist_mem)
        schedule = invoke.schedule

        ftrans = LFRicLoopFuseTrans()
        mtrans = MoveTrans()
        loops = schedule.walk(LFRicLoop)
        if dist_mem:
            index = loops[0].position
            # f, c and g halo exchanges between loops can be moved
            # before the 1st loop as they are not accessed in it
            for idx in range(index, index+3):
                mtrans.apply(schedule.children[idx+1],
                             schedule.children[idx])

        # Don't use match as the error depends on whether same_sapce
        # is set or not.
        with pytest.raises(TransformationError) as excinfo:
            ftrans.apply(loops[0:2], {"same_space": same_space})

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
    # TODO #2668: Deprecate options dict.
    with pytest.raises(
        TypeError,
        match="The value of the 'same_space' flag must be either bool or "
              "None type, but the type of flag provided was 'str'."):
        ftrans.validate((None, None), options={"same_space": "foo"})


def test_loop_fuse(dist_mem, tmpdir):
    ''' Test that we are able to fuse two loops together. '''
    psy, invoke = get_invoke("4_multikernel_invokes_fusable_distmem.f90",
                             TEST_API, name="invoke_0", dist_mem=dist_mem)
    schedule = invoke.schedule
    loops = schedule.walk(LFRicLoop)
    halo_exchs = schedule.walk(LFRicHaloExchange)

    if dist_mem:
        # Move the second loop to after the first 2 sets of 4 halo exchanges,
        # instead of between the 3rd and 4th.
        mtrans = MoveTrans()
        mtrans.apply(loops[0], halo_exchs[7], position="after")

    ftrans = LFRicLoopFuseTrans()

    assert ("Fuse two adjacent loops together with LFRic-specific "
            "validity checks" in str(ftrans))

    # Fuse the loops
    ftrans.apply(loops[0:2])

    gen = str(psy.gen)

    cell_loop_idx = -1
    end_loop_idx = -1
    call_idx1 = -1
    call_idx2 = -1
    if dist_mem:
        assert "loop0_stop = mesh%get_last_halo_cell(1)" in gen
    else:
        assert "loop0_stop = f1_proxy%vspace%get_ncell()" in gen
    loop_str = "do cell = loop0_start, loop0_stop"
    for idx, line in enumerate(gen.split('\n')):
        if loop_str in line:
            cell_loop_idx = idx
        if "call testkern_code" in line:
            if call_idx1 == -1:
                call_idx1 = idx
            else:
                call_idx2 = idx
        if "enddo" in line:
            end_loop_idx = idx

    assert cell_loop_idx != -1
    assert cell_loop_idx < call_idx1
    assert call_idx1 < call_idx2
    assert call_idx2 < end_loop_idx

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_loop_fuse_set_dirty(tmpdir):
    ''' Test that we are able to fuse two loops together and produce
    the expected set_dirty() calls. '''
    psy, invoke = get_invoke("4_multikernel_invokes_fusable_distmem.f90",
                             TEST_API, name="invoke_0", dist_mem=True)

    schedule = invoke.schedule
    loops = schedule.walk(LFRicLoop)
    halo_exchs = schedule.walk(LFRicHaloExchange)
    # Move the second loop to after the first 2 sets of 4 halo exchanges,
    # instead of between the 3rd and 4th.
    mtrans = MoveTrans()
    mtrans.apply(loops[0], halo_exchs[7], position="after")
    ftrans = LFRicLoopFuseTrans()
    # Fuse the loops
    ftrans.apply(loops[0:2])

    gen = str(psy.gen)
    assert gen.count("set_dirty()") == 2

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_loop_fuse_multiwrite():
    ''' Test that validate flags loops that write to more than
    one field.'''
    _, invoke = get_invoke("15.18.2_multiwrite_field_fuse_error.f90",
                           TEST_API, name="invoke_0", dist_mem=False)
    schedule = invoke.schedule
    ftrans = LFRicLoopFuseTrans()
    loops = schedule.walk(LFRicLoop)
    # Validate fusing the first two loops
    with pytest.raises(
        TransformationError,
        match="Error in LFRicLoopFuseTrans: Kernel "
              "'testkern_write_any_anyd_code' in one of the input loops has "
              "2 write arguments. LFRicLoopFuseTrans can only currently fuse "
              "when each kernel has at most one."):
        ftrans.validate(loops[0:2])
    # Validate fusing the latter two loops
    with pytest.raises(
        TransformationError,
        match="Error in LFRicLoopFuseTrans: Kernel "
              "'testkern_write_any_anyd_code' in one of the input loops has "
              "2 write arguments. LFRicLoopFuseTrans can only currently fuse "
              "when each kernel has at most one."):
        ftrans.validate(loops[1:3])


def test_loop_fuse_different_operates_on():
    ''' Test that validate flags loops that operate on different types of
    iterator (e.g. dof vs cell_column).'''

    _, invoke = get_invoke(
        "15.18.3_anyspace_different_operateson_fuse_error.f90",
        TEST_API, name="invoke_0", dist_mem=False)
    schedule = invoke.schedule
    ftrans = LFRicLoopFuseTrans()
    loops = schedule.walk(LFRicLoop)
    # This is caught in the base LoopFuseTrans when checking
    # if node1.iteration_space != node2.iteration_space
    # Here we have cell_column and dof spaces.

    with pytest.raises(
        TransformationError,
        match="Error in LFRicLoopFuseTrans transformation. Loops do not have "
              "the same iteration space."):
        ftrans.validate(loops[0:2])


def test_loop_fuse_fail_to_resolve_space(tmpdir):
    ''' Test that we fail to fuse loops on any space if we can't find the
    space elsewhere in the invoke.'''
    psy, invoke = get_invoke(
        "15.18.4_fail_to_resolve_any_space_fuse_error.f90",
        TEST_API, name="invoke_0", dist_mem=False)
    schedule = invoke.schedule
    ftrans = LFRicLoopFuseTrans()
    loops = schedule.walk(LFRicLoop)

    # Fusion should work for the first 2 loops.
    ftrans.apply(loops[0:2])

    # Find the new loop set.
    loops = schedule.walk(LFRicLoop)
    with pytest.raises(
        TransformationError,
        match="Error in LFRicLoopFuseTrans: Couldn't lookup the field space "
              "for one or more of the ANY_SPACE fields being operated on and "
              "conditional fusion wasn't specified."):
        ftrans.apply(loops[1:3])

    # Fusion should work if we allow conditional fusion
    ftrans.apply(loops[1:3], conditional_fusion=True)
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_loop_fuse_resolved_different_spaces():
    ''' Test that we fail to fuse two loops on any space if we find that
    they're actually on different spaces from searching the invoke.'''
    _, invoke = get_invoke(
        "15.18.5_resolve_different_any_space_fuse_error.f90",
        TEST_API, name="invoke_0", dist_mem=False)
    schedule = invoke.schedule
    ftrans = LFRicLoopFuseTrans()

    loops = schedule.walk(LFRicLoop)

    # Fusion shouldn't work because the first builtin's field is on w2
    # and the second's is on w3
    with pytest.raises(
        TransformationError,
        match="Error in LFRicLoopFuseTrans: The kernels provided are on "
              "different spaces so can't be fused. Computed spaces were "
              "'w2trace' and 'w3'."):
        ftrans.apply(loops[0:2])


def test_loop_fuse_conditional_vector_fields(fortran_writer, tmpdir):
    ''' Test that the loop fusion works correctly for conditional fusion
    with vector fields.'''
    psy, invoke = get_invoke(
        "15.18.6_any_space_vector_fuse.f90",
        TEST_API, name="invoke_0", dist_mem=False)
    schedule = invoke.schedule
    ftrans = LFRicLoopFuseTrans()

    loops = schedule.walk(LFRicLoop)
    ftrans.apply(loops[0:2], conditional_fusion=True)
    correct = """  if (f1(1)%which_function_space() == \
f4(1)%which_function_space()) then
    do cell = uninitialised_loop0_start, uninitialised_loop0_stop, 1
      call testkern_anys_vector_write_code(nlayers_f1, f1_1_data, f1_2_data, \
f2_data, f3_data, ndf_as1_f1, undf_as1_f1, map_as1_f1(:,cell), ndf_any_w2, \
undf_any_w2, map_any_w2(:,cell), basis_any_w2_qr, diff_basis_any_w2_qr, \
np_xy_qr, np_z_qr, weights_xy_qr, weights_z_qr)
      call testkern_anys_vector_write_code(nlayers_f4, f4_1_data, f4_2_data, \
f2_data, f3_data, ndf_as1_f4, undf_as1_f4, map_as1_f4(:,cell), ndf_any_w2, \
undf_any_w2, map_any_w2(:,cell), basis_any_w2_qr, diff_basis_any_w2_qr, \
np_xy_qr, np_z_qr, weights_xy_qr, weights_z_qr)
    enddo
  else
    do cell = uninitialised_loop0_start, uninitialised_loop0_stop, 1
      call testkern_anys_vector_write_code(nlayers_f1, f1_1_data, f1_2_data, \
f2_data, f3_data, ndf_as1_f1, undf_as1_f1, map_as1_f1(:,cell), ndf_any_w2, \
undf_any_w2, map_any_w2(:,cell), basis_any_w2_qr, diff_basis_any_w2_qr, \
np_xy_qr, np_z_qr, weights_xy_qr, weights_z_qr)
    enddo
    do cell = uninitialised_loop1_start, uninitialised_loop1_stop, 1
      call testkern_anys_vector_write_code(nlayers_f4, f4_1_data, f4_2_data, \
f2_data, f3_data, ndf_as1_f4, undf_as1_f4, map_as1_f4(:,cell), ndf_any_w2, \
undf_any_w2, map_any_w2(:,cell), basis_any_w2_qr, diff_basis_any_w2_qr, \
np_xy_qr, np_z_qr, weights_xy_qr, weights_z_qr)
    enddo
  end if"""
    assert correct in fortran_writer(schedule)
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_loop_fuse_min_max_same_var(fortran_writer, tmpdir):
    '''Test that we fuse two builtins on the same field without any
    other invoke elements.'''
    psy, invoke = get_invoke(
        "15.10.9_min_max_X_builtin.f90",
        TEST_API, name="invoke_0", dist_mem=False)
    schedule = invoke.schedule
    ftrans = LFRicLoopFuseTrans()
    loops = schedule.walk(LFRicLoop)
    ftrans.apply(loops[0:2])
    ftrans.apply((loops[0], loops[2]))
    correct = """! Initialise reduction variable
  amin = 0.0_r_def

  ! Initialise reduction variable
  amax = 0.0_r_def
  do df = uninitialised_loop0_start, uninitialised_loop0_stop, 1
    ! Built-in: setval_c (set a real-valued field to a real scalar value)
    f1_data(df) = 1.0_r_def

    ! Built-in: minval_X (compute the global minimum value contained in a \
field)
    amin = MIN(amin, f1_data(df))

    ! Built-in: maxval_X (compute the global maximum value contained in a \
field)
    amax = MAX(amax, f1_data(df))
  enddo"""
    assert correct in fortran_writer(schedule)
    assert LFRicBuild(tmpdir).code_compiles(psy)

def test_loop_fuse_dependency(fortran_writer):
    '''
    Test that we get a transformation error when trying to fuse dependent
    kernels.
    '''
    psy, invoke = get_invoke("4_multikernel_invokes.f90",
                             TEST_API, name="invoke_0", dist_mem=True)
    schedule = invoke.schedule

    ftrans = LFRicLoopFuseTrans()
    loops = schedule.walk(LFRicLoop)

    # Fuse the loop
    with pytest.raises(
        TransformationError,
        match="Error in LFRicLoopFuseTrans: Cannot fuse loops as the "
              "loops have a dependency on the field f1."):
        ftrans.apply(loops[0:2])
