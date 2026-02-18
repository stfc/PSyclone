# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2026, Science and Technology Facilities Council.
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
# Authors: A. B. G. Chalk, R. W. Ford, A. R. Porter and
#          S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests on the LFRicGlobalReduction class and its
    sub-classes. '''

import pytest

from psyclone.domain.lfric.lfric_global_reductions import (
    _LFRicGlobalReduction, LFRicGlobalSum)
from psyclone.errors import GenerationError
from psyclone.tests.utilities import get_invoke

TEST_API = "lfric"


def test_lfricglobalreduction_abstract():
    '''Check that the _LFRicGlobalReduction base class cannot be
    instantiated.

    '''
    with pytest.raises(TypeError) as err:
        _ = _LFRicGlobalReduction(None)
    assert ("Only sub-classes of '_LFRicGlobalReduction' may be "
            "instantiated" in str(err.value))


def test_lfricglobalreduction_unsupported_scalar(monkeypatch):
    ''' Check that an instance of the _LFRicGlobalReduction class raises an
    exception if an unsupported scalar type is provided when distributed
    memory is enabled (dm=True).

    '''
    # Get an instance of an integer scalar kernel argument.
    _, invoke = get_invoke("1.6.1_single_invoke_1_int_scalar.f90",
                           api=TEST_API, dist_mem=True, idx=0)
    schedule = invoke.schedule
    loop = schedule.children[4]
    kernel = loop.loop_body[0]
    argument = kernel.arguments.args[1]
    # Monkeypatch its type.
    monkeypatch.setattr(argument, "_intrinsic_type", "logical")
    with pytest.raises(GenerationError) as err:
        _ = LFRicGlobalSum(argument)
    assert ("LFRicGlobalSum only supports real or integer scalars, "
            "but argument 'iflag' in Kernel 'testkern_one_int_scalar_code' "
            "has 'logical' intrinsic type." in str(err.value))


def test_lfricglobalreduction_lower(fortran_writer):
    '''Test the lower_to_language_level() method of _LFRicGlobalReduction.'''
    _, invoke = get_invoke("15.9.2_X_innerproduct_X_builtin.f90",
                           api=TEST_API, dist_mem=True, idx=0)
    output = fortran_writer(invoke.schedule)
    assert "use scalar_mod, only : scalar_type" in output
    assert "type(scalar_type) :: global_sum" in output
    assert """  ! Perform global sum
  global_sum%value = asum
  asum = global_sum%get_sum()""" in output
