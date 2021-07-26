# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
from psyclone.psyir.transformations import TransformationError
from psyclone.domain.gocean.transformations import GOConstLoopBoundsTrans
from psyclone.tests.gocean1p0_build import GOcean1p0Build
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
    ''' Check that we raise an error if we attempt to apply the
    constant loop-bounds transformation to something somethingthat is
    not an InvokeSchedule '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           API, idx=0)
    schedule = invoke.schedule
    cbtrans = GOConstLoopBoundsTrans()

    with pytest.raises(TransformationError):
        _, _ = cbtrans.apply(schedule.children[0])


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
    print(gen)
    assert "INTEGER istop" in gen
    assert "INTEGER istop" in gen
    assert "istop = cv_fld%grid%subdomain%internal%xstop" in gen
    assert "jstop = cv_fld%grid%subdomain%internal%ystop" in gen
    assert "DO j = 2, jstop - 1" in gen
    assert "DO i = 2, istop" in gen

    # Next, check that applying the constant loop-bounds
    # transformation again has no effect.
    cbtrans.apply(schedule)
    gen = str(psy.gen)
    assert "INTEGER istop" in gen
    assert "INTEGER jstop" in gen
    assert "istop = cv_fld%grid%subdomain%internal%xstop" in gen
    assert "jstop = cv_fld%grid%subdomain%internal%ystop" in gen
    assert "DO j = 2, jstop - 1" in gen
    assert "DO i = 2, istop" in gen

    assert GOcean1p0Build(tmpdir).code_compiles(psy)


def test_const_loop_bounds_invalid_offset():
    ''' Test that we raise an appropriate error if we attempt to generate
    code with constant loop bounds for a kernel that expects an
    unsupported grid-offset '''
    _, invoke = get_invoke("test26_const_bounds_invalid_offset.f90",
                           API, idx=0)
    cbtrans = GOConstLoopBoundsTrans()
    schedule = invoke.schedule
    with pytest.raises(TransformationError) as err:
        cbtrans.apply(schedule)
    assert ("Constant bounds generation not implemented for a grid offset of "
            "'go_offset_nw'. Supported offsets are ['go_offset_ne', "
            "'go_offset_sw', 'go_offset_any']" in str(err.value))
