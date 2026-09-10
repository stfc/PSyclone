# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing tests for the LFRicColourAndOMPTrans transformation.'''

import pytest

from psyclone.domain.lfric.transformations import LFRicColourAndOMPTrans
from psyclone.psyir.nodes import Directive, Loop, OMPDoDirective
from psyclone.psyir.transformations import (OMPParallelTrans, ProfileTrans,
                                            TransformationError)
from psyclone.tests.utilities import get_invoke

# The version of the API that the tests in this file
# exercise.
TEST_API = "lfric"


def test_validate_wrong_node_type():
    ''' Check that validate rejects a node that is not a Routine. '''
    _, invoke = get_invoke("1_single_invoke.f90", TEST_API, idx=0,
                           dist_mem=False)
    loop = invoke.schedule.walk(Loop)[0]
    with pytest.raises(TransformationError) as err:
        LFRicColourAndOMPTrans().validate(loop)
    assert "should be a Routine but found 'LFRicLoop'" in str(err.value)


def test_validate_rejects_existing_profile_node():
    ''' Check that validate rejects a Routine that already has profiling. '''
    _, invoke = get_invoke("1_single_invoke.f90", TEST_API, idx=0,
                           dist_mem=False)
    sched = invoke.schedule
    ProfileTrans().apply(sched.children[0])
    with pytest.raises(TransformationError) as err:
        LFRicColourAndOMPTrans().validate(sched)
    assert "already contains a ProfileNode" in str(err.value)


def test_validate_rejects_invalid_option():
    ''' Check that an unrecognised option is rejected. '''
    _, invoke = get_invoke("1_single_invoke.f90", TEST_API, idx=0,
                           dist_mem=False)
    with pytest.raises(ValueError):
        LFRicColourAndOMPTrans().validate(invoke.schedule, not_an_option=True)


def test_apply_colours_continuous_loop():
    ''' Check a loop on a continuous space (w1) is coloured. '''
    _, invoke = get_invoke("1_single_invoke.f90", TEST_API, idx=0,
                           dist_mem=False)
    sched = invoke.schedule
    LFRicColourAndOMPTrans().apply(sched)
    assert [loop.loop_type for loop in sched.walk(Loop)] == \
        ["colours", "cells_in_colour"]


def test_apply_skips_discontinuous_loop():
    ''' Check a loop on a discontinuous space (w3) is not coloured. '''
    _, invoke = get_invoke("1_single_invoke_w3.f90", TEST_API, idx=0,
                           dist_mem=False)
    sched = invoke.schedule
    LFRicColourAndOMPTrans().apply(sched)
    assert [loop.loop_type for loop in sched.walk(Loop)] == [""]


def test_apply_passes_tiling_to_colour_trans():
    ''' Check the tiling option is routed to LFRicColourTrans. '''
    _, invoke = get_invoke("1_single_invoke.f90", TEST_API, idx=0,
                           dist_mem=False)
    sched = invoke.schedule
    LFRicColourAndOMPTrans().apply(sched, tiling=True)
    assert [loop.loop_type for loop in sched.walk(Loop)] == \
        ["colours", "tiles_in_colour", "cells_in_tile"]


def test_apply_adds_openmp_directives():
    ''' Check that OpenMP directives are added around the coloured loop. '''
    _, invoke = get_invoke("1_single_invoke.f90", TEST_API, idx=0,
                           dist_mem=False)
    sched = invoke.schedule
    LFRicColourAndOMPTrans().apply(sched)
    assert [type(node).__name__ for node in sched.walk(Directive)] == \
        ["OMPParallelDirective", "OMPDoDirective"]


@pytest.mark.parametrize("reprod, expected", [(None, False), (True, True),
                                              (False, False)])
def test_apply_reprod_option(reprod, expected):
    ''' Check the reprod option is passed on to LFRicOMPLoopTrans. When it is
    None the value from the Config is used (which defaults to False). '''
    _, invoke = get_invoke("15.9.1_X_innerproduct_Y_builtin.f90", TEST_API,
                           idx=0, dist_mem=False)
    sched = invoke.schedule
    LFRicColourAndOMPTrans().apply(sched, reprod=reprod)
    assert sched.walk(OMPDoDirective)[0].reprod is expected


def test_apply_skips_loop_already_in_directive():
    ''' Check that a loop that is already inside a directive is not
    parallelised a second time. '''
    _, invoke = get_invoke("1_single_invoke_w3.f90", TEST_API, idx=0,
                           dist_mem=False)
    sched = invoke.schedule
    # w3 is discontinuous so no colouring happens. Parallelise the loop by
    # hand first so that the transformation finds it already in a directive.
    OMPParallelTrans().apply(sched.walk(Loop)[0])
    assert len(sched.walk(Directive)) == 1
    LFRicColourAndOMPTrans().apply(sched)
    assert len(sched.walk(Directive)) == 1
