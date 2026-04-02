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
# Authors: A. R. Porter, STFC Daresbury Lab

''' This module tests the LFRicCellIterators collection. '''

import os
import pytest

from psyclone.domain.lfric import LFRicCellIterators
from psyclone.errors import GenerationError
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.domain.lfric import LFRicKern

BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "../..", "test_files", "lfric")
TEST_API = "lfric"


def test_lfriccelliterators_kernel():
    '''
    Check that LFRicCellIterators is constructed correctly when supplied
    with a single LFRicKern.
    '''
    _, info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    sched = invoke.schedule
    kern = sched.walk(LFRicKern)[0]
    obj = LFRicCellIterators(kern)
    # We should have no 'nlayers' (it's up to the stub_declarations to bring
    # them when needed)
    assert isinstance(obj._nlayers_names, dict)
    assert len(obj._nlayers_names.keys()) == 0


def test_lfriccelliterators_kernel_stub_declns(fortran_writer):
    '''
    Check that LFRicCellIterators.stub_declarations() creates the correct
    declarations for an LFRicKern.
    '''
    _, info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    sched = invoke.schedule
    kern = sched.walk(LFRicKern)[0]
    output = fortran_writer(kern.gen_stub)
    assert "integer(kind=i_def), intent(in) :: nlayers" in output


def test_lfriccelliterators_invoke_codegen():
    '''
    Check that invoke_declarations() creates the right declarations and
    initialisations for an invoke containing more than one kernel.
    '''
    _, info = parse(
        os.path.join(BASE_PATH, "15.1.2_builtin_and_normal_kernel_invoke.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    output = str(psy.gen)
    # The invoke has three kernels that operate on cell columns (and two
    # builtins but they don't need nlayers).
    assert "integer(kind=i_def) :: nlayers_f1" in output
    assert "integer(kind=i_def) :: nlayers_f3" in output
    assert "integer(kind=i_def) :: nlayers_f4" in output
    assert "! Initialise number of layers" in output
    assert "nlayers_f1 = f1_proxy%vspace%get_nlayers()" in output
    assert "nlayers_f3 = f3_proxy%vspace%get_nlayers()" in output
    assert "nlayers_f4 = f4_proxy%vspace%get_nlayers()" in output


def test_lfriccelliterators_err(monkeypatch):
    ''' Check that the LFRicCellIterators constructor raises the expected
    error if it fails to find any field or operator arguments. '''
    _, info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    # The list of arguments is dynamically generated if it is empty so
    # monkeypatch it to contain a single, scalar argument.
    monkeypatch.setattr(invoke._psy_unique_vars[0], "_argument_type",
                        "gh_scalar")
    monkeypatch.setattr(invoke, "_psy_unique_vars",
                        [invoke._psy_unique_vars[0]])
    with pytest.raises(GenerationError) as err:
        _ = LFRicCellIterators(invoke)
    assert ("Cannot create an Invoke with no field/operator arguments."
            in str(err.value))
