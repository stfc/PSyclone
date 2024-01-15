# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab

'''
pytest tests for the GOKern class.

TODO #1938 - expand the tests to fully cover the class. Currently only the
constructor and the get_kernel_schedule() method are tested.

'''

import os
import pytest

from fparser.two import Fortran2003
from fparser.two.utils import walk

from psyclone.configuration import Config
from psyclone.errors import GenerationError
from psyclone.gocean1p0 import GOKern, GOKernelSchedule
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory

API = "gocean1.0"
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__))))),
    "test_files", "gocean1p0")


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use gocean1.0 as API.'''
    Config.get().api = API
    yield
    Config._instance = None


def test_gok_construction():
    '''
    Test that a GOKern is constructed and populated as expected. This is
    currently quite hard to do in isolation as the constructor needs an
    instance of `psyclone.parse.algorithm.KernelCall`. Therefore, we use
    the full PSyFactory machinery.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "single_invoke.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kern = schedule.walk(GOKern)[0]
    assert isinstance(kern, GOKern)
    assert kern._metadata_name == "compute_cu"
    assert kern.name == "compute_cu_code"
    assert kern._index_offset == "go_offset_sw"


def test_gok_get_kernel_schedule():
    '''
    Test the get_kernel_schedule() method of GOKern.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "single_invoke.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kern = schedule.walk(GOKern)[0]
    assert kern._kern_schedule is None
    sched = kern.get_kernel_schedule()
    assert isinstance(sched, GOKernelSchedule)
    # A second call should just return the previously-obtained schedule.
    sched2 = kern.get_kernel_schedule()
    assert sched2 is sched
    # Check that the expected error is raised if the subroutine that
    # implements the kernel cannot be found.
    kern._kern_schedule = None
    # Remove the subroutine that implements the kernel from the Fortran
    # parse tree.
    subs = walk(kern.ast, Fortran2003.Subroutine_Subprogram)
    for sub in subs:
        if sub.children[0].children[1].string == "compute_cu_code":
            sub.parent.content.remove(sub)
            break
    with pytest.raises(GenerationError) as err:
        kern.get_kernel_schedule()
    err_text = str(err.value)
    assert ("Failed to raise the PSyIR for kernel 'compute_cu_code' to GOcean "
            "PSyIR" in err_text)
    assert ("does not contain the routine that it names as implementing the "
            "kernel ('compute_cu_code')" in err_text)
