# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2021, Science and Technology Facilities Council.
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
# Authors R. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab

''' Contains tests for transformations on the Dynamo 0.1 API '''

from __future__ import absolute_import
import pytest
from psyclone.configuration import Config
from psyclone.tests.utilities import get_invoke

from psyclone.transformations import OMPParallelTrans

TEST_API = "dynamo0.1"


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use dynamo0.1 as API.'''
    Config.get().api = "dynamo0.1"


def test_openmp_region():
    ''' Test the application of an OpenMP parallel region transformation
    to a single loop '''
    psy, invoke = get_invoke("algorithm/1_single_function.f90", TEST_API,
                             name="invoke_0_testkern_type")
    schedule = invoke.schedule
    rtrans = OMPParallelTrans()
    rtrans.apply(schedule.children[0])
    gen = str(psy.gen)

    # Check that our list of private variables is correct
    assert "!$omp parallel default(shared) private(cell,map)" in gen

    for idx, line in enumerate(gen.split('\n')):
        if "!$omp parallel default(shared)" in line:
            startpara_idx = idx
        if "DO cell=1,f1%get_ncell()" in line:
            do_idx = idx
        if "CALL f1%vspace%get_cell_dofmap(cell, map)" in line:
            dmap_idx = idx
        if "CALL testkern_code(nlayers, ndf, map, f1%data, "\
           "f2%data, m1%data)" in line:
            kcall_idx = idx
        if "END DO" in line:
            enddo_idx = idx
        if "!$omp end parallel" in line:
            endpara_idx = idx
    assert do_idx == startpara_idx + 1
    assert dmap_idx == do_idx + 1
    assert kcall_idx == dmap_idx + 1
    assert enddo_idx == kcall_idx + 1
    assert endpara_idx == enddo_idx + 1
