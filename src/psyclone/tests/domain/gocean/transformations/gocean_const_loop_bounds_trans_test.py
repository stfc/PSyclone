# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Module containing tests of GOConstLoopBoundsTrans when using the
    GOcean 1.0 API '''

import pytest
from psyclone.errors import InternalError
from psyclone.gocean1p0 import GOLoop
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.symbols import AutomaticInterface, DataTypeSymbol
from psyclone.domain.gocean.transformations import GOConstLoopBoundsTrans
from psyclone.tests.gocean_build import GOceanBuild
from psyclone.tests.utilities import get_invoke

# The version of the PSyclone API that the tests in this file
# exercise
API = "gocean"


def test_const_loop_bounds_name_and_str():
    ''' Check that the  GOConstLoopBoundsTrans returns the expected
    name and str()'''
    cbtrans = GOConstLoopBoundsTrans()
    assert cbtrans.name == "GOConstLoopBoundsTrans"
    assert str(cbtrans) == \
        "Use constant loop bounds for all loops in a GOInvokeSchedule"


def test_const_loop_bounds_not_schedule():
    ''' Check that we raise an error if we attempt to apply the constant
    loop-bounds transformation to something that is not an InvokeSchedule.
    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           API, idx=0)
    schedule = invoke.schedule
    cbtrans = GOConstLoopBoundsTrans()

    with pytest.raises(TransformationError) as err:
        cbtrans.apply(schedule.children[0])
    assert ("GOConstLoopBoundsTrans can only be applied to 'GOInvokeSchedule' "
            "but found 'GOLoop'." in str(err.value))


def test_const_loop_bounds_trans(tmpdir):
    ''' Check that we can turn the loop bounds constant (with a single
    variable holding them). '''
    psy, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                             API, idx=0)
    schedule = invoke.schedule
    cbtrans = GOConstLoopBoundsTrans()

    # First check that the generated code doesn't use constant loop
    # bounds by default.
    gen = str(psy.gen)
    assert "do j = cv_fld%internal%ystart, cv_fld%internal%ystop" in gen
    assert "do i = cv_fld%internal%xstart, cv_fld%internal%xstop" in gen
    assert "do j = p_fld%whole%ystart, p_fld%whole%ystop" in gen
    assert "do i = p_fld%whole%xstart, p_fld%whole%xstop" in gen

    # Next, check the generated code applying the constant loop-bounds
    # transformation.
    psy, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                             API, idx=0)
    schedule = invoke.schedule
    cbtrans.apply(schedule)
    gen = str(psy.gen)
    assert "integer :: istop" in gen
    assert "integer :: jstop" in gen
    assert "istop = cv_fld%grid%subdomain%internal%xstop" in gen
    assert "jstop = cv_fld%grid%subdomain%internal%ystop" in gen
    assert "do j = 2, jstop - 1" in gen
    assert "do i = 2, istop" in gen

    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_const_loop_bounds_invalid_loop_attributes(monkeypatch):
    ''' Test that we raise an appropriate error if we attempt to generate
    code with constant loop bounds for a kernel that expects an
    unsupported loop attribute '''
    _, invoke = get_invoke("test26_const_bounds_invalid_offset.f90",
                           API, idx=0)
    cbtrans = GOConstLoopBoundsTrans()

    # Start with a schedule with invalid index offset
    schedule = invoke.schedule
    with pytest.raises(TransformationError) as err:
        cbtrans.apply(schedule)
    assert ("GOConstLoopBoundsTrans can not transform a loop with index_offset"
            " 'go_offset_nw' because it is not in the bounds lookup table, the"
            " available index_offset values are [" in str(err.value))

    # Fix index_offset and invalidate field_space
    for loop in schedule.walk(GOLoop):
        loop.index_offset = 'go_offset_ne'
        loop._field_space = 'invalid'  # Bypass setter validation
    with pytest.raises(TransformationError) as err:
        cbtrans.apply(schedule)
    assert ("GOConstLoopBoundsTrans can not transform a loop with field_space "
            "'invalid' because it is not in the bounds lookup table, the "
            "available field_space values are [" in str(err.value))

    # Fix field_space and invalidate iteration_space
    for loop in schedule.walk(GOLoop):
        loop.field_space = 'go_cu'
        loop._iteration_space = 'invalid'  # Bypass setter validation
    with pytest.raises(TransformationError) as err:
        cbtrans.apply(schedule)
    assert ("GOConstLoopBoundsTrans can not transform a loop with iteration_"
            "space 'invalid' because it is not in the bounds lookup table, the"
            " available iteration_space values are [" in str(err.value))

    # Fix iteration_space and invalidate loop_type
    for loop in schedule.walk(GOLoop):
        loop.iteration_space = 'go_internal_pts'
        loop._loop_type = 'invalid'  # Bypass setter validation
    with pytest.raises(TransformationError) as err:
        cbtrans.apply(schedule)
    assert ("GOConstLoopBoundsTrans can not transform a loop with loop_type "
            "'invalid', only 'inner' or 'outer' loop_type values are expected."
            in str(err.value))

    # Fix loop_type but delete loop_type entry from bounds lookup table
    for loop in schedule.walk(GOLoop):
        loop.loop_type = 'outer'
    del loop.bounds_lookup[loop.index_offset][loop.field_space][
            loop.iteration_space]['outer']
    with pytest.raises(TransformationError) as err:
        cbtrans.apply(schedule)
    assert ("GOConstLoopBoundsTrans can not transform a loop with loop_type "
            "'outer' because it is not in the bounds lookup table, the "
            "available loop_type values are ['inner']." in str(err.value))

    # Trigger the apply InternalError by skipping the validate
    def empty_validation(node, options=None):
        # pylint: disable=unused-argument
        pass
    monkeypatch.setattr(cbtrans, "validate", empty_validation)
    for loop in schedule.walk(GOLoop):
        loop._loop_type = 'invalid'  # Bypass setter validation
    with pytest.raises(InternalError) as err:
        cbtrans.apply(schedule)
    assert ("Found a loop with loop_type 'invalid' but the only expected "
            "values are 'inner' or 'outer'." in str(err.value))


def test_const_loop_bounds_without_field_argument():
    ''' Check that applying the loop bounds transformation to an invoke that
    doesn't have any field arguments fails with the appropriate error.'''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           API, idx=0)
    schedule = invoke.schedule
    cbtrans = GOConstLoopBoundsTrans()

    # Remove all field arguments
    keep_arguments = []
    schedule.symbol_table.specify_argument_list([])
    for arg in schedule.symbol_table.argument_datasymbols:
        if (isinstance(arg.datatype, DataTypeSymbol) and
                arg.datatype.name == "r2d_field"):
            arg.interface = AutomaticInterface()
        else:
            keep_arguments.append(arg)
    schedule.symbol_table.specify_argument_list(keep_arguments)

    with pytest.raises(TransformationError) as err:
        cbtrans.apply(schedule)
    assert ("GOConstLoopBoundsTrans can not transform invoke 'invoke_0' "
            "because it does not have any field arguments." in str(err.value))
