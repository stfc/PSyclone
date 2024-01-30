# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
# ----------------------------------------------------------------------------
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, Met Office

''' Module containing tests of GOConstLoopBoundsTrans when using the
    GOcean 1.0 API '''

from __future__ import absolute_import
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
API = "gocean1.0"


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
    assert "DO j = cv_fld%internal%ystart, cv_fld%internal%ystop" in gen
    assert "DO i = cv_fld%internal%xstart, cv_fld%internal%xstop" in gen
    assert "DO j = p_fld%whole%ystart, p_fld%whole%ystop" in gen
    assert "DO i = p_fld%whole%xstart, p_fld%whole%xstop" in gen

    # Next, check the generated code applying the constant loop-bounds
    # transformation.
    psy, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                             API, idx=0)
    schedule = invoke.schedule
    cbtrans.apply(schedule)
    gen = str(psy.gen)
    assert "INTEGER istop" in gen
    assert "INTEGER jstop" in gen
    assert "istop = cv_fld%grid%subdomain%internal%xstop" in gen
    assert "jstop = cv_fld%grid%subdomain%internal%ystop" in gen
    assert "DO j = 2, jstop - 1" in gen
    assert "DO i = 2, istop" in gen

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
