# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Tests of the LFRicLoopFuseTrans '''

import pytest

from psyclone.configuration import Config
from psyclone.domain.lfric.transformations import LFRicLoopFuseTrans
from psyclone.psyir.transformations import (TransformationError, MoveTrans)
from psyclone.tests.utilities import get_invoke

# The version of the API that the tests in this file
# exercise.
TEST_API = "lfric"


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
        ftrans.apply((schedule.children[0], schedule.children[1]))
    assert ("One or both function spaces 'not_a_space_name' and 'w1' have "
            "invalid names" in str(excinfo.value))


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
            ftrans.apply((schedule.children[index],
                         schedule.children[index+1]),
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
        ftrans.validate((None, None), {"same_space": "foo"})
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

    assert ("Fuse two adjacent loops together with LFRic-specific "
            "validity checks" in str(ftrans))

    # Fuse the loops
    ftrans.apply((schedule.children[index],
                 schedule.children[index+1]))

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


def test_loop_fuse_set_dirty():
    ''' Test that we are able to fuse two loops together and produce
    the expected set_dirty() calls. '''
    psy, invoke = get_invoke("4_multikernel_invokes.f90", TEST_API,
                             name="invoke_0", dist_mem=True)

    schedule = invoke.schedule
    ftrans = LFRicLoopFuseTrans()
    # Fuse the loops
    ftrans.apply((schedule.children[4], schedule.children[5]))

    gen = str(psy.gen)
    assert gen.count("set_dirty()") == 1


def test_loop_fuse_multiwrite():
    ''' Test that validate flags loops that write to more than
    one field.'''
    _, invoke = get_invoke("15.18.2_multiwrite_field_fuse_error.f90",
                           TEST_API, name="invoke_0", dist_mem=False)
    schedule = invoke.schedule
    ftrans = LFRicLoopFuseTrans()
    # Validate fusing the first two loops
    with pytest.raises(TransformationError) as err:
        ftrans.validate((schedule.children[0], schedule.children[1]))
    assert ("Error in LFRicLoopFuseTrans: One input loop has more than one "
            "write argument. Found '2' writes and '1' writes."
            in str(err.value))
    # Validate fusing the latter two loops
    with pytest.raises(TransformationError) as err:
        ftrans.validate((schedule.children[1], schedule.children[2]))
    assert ("Error in LFRicLoopFuseTrans: One input loop has more than one "
            "write argument. Found '1' writes and '2' writes."
            in str(err.value))
