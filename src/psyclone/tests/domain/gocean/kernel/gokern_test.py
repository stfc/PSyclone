# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2025, Science and Technology Facilities Council.
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
# Modified: J. Henrichs, Bureau of Meteorology

'''
pytest tests for the GOKern class.

TODO #1938 - expand the tests to fully cover the class. Currently only the
constructor and the get_callees() method are tested.

'''

import os
import pytest

from fparser.two import Fortran2003
from fparser.two.utils import walk

from psyclone.configuration import Config
from psyclone.core import Signature
from psyclone.errors import GenerationError, InternalError
from psyclone.gocean1p0 import (
    GOKern, GOKernelSchedule, GOKernCallFactory)
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir.nodes import Schedule
from psyclone.tests.utilities import get_invoke

API = "gocean"
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__))))),
    "test_files", "gocean1p0")


@pytest.fixture(scope="function", autouse=True)
def setup():
    '''Make sure that all tests here use gocean as API.'''
    Config.get().api = API


def test_gok_factory(monkeypatch):
    '''
    Test that the GOKernCallFactory only creates a GOKern inside
    GOInvokeSchedule.
    '''
    # Do not test GOKern here, this is done below, and needs valid metadata
    # which is difficult to assemble
    monkeypatch.setattr(GOKern, "__init__",
                        lambda _1, _2, _3, _4, parent: None)
    call = None

    with pytest.raises(GenerationError) as err:
        GOKernCallFactory.create(call)
    assert ("GOKern must always be constructed with a parent inside a "
            "GOInvokeSchedule" in str(err.value))
    with pytest.raises(GenerationError) as err:
        GOKernCallFactory.create(call, parent=Schedule())
    assert ("GOKern must always be constructed with a parent inside a "
            "GOInvokeSchedule" in str(err.value))


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

    # This first child represents the computation pattern of the kernel
    # This kernel has a pointwise write (cu_fld), a '000,011,000' stencil
    # (p_fld) and a pointwise read (u_fld)
    assert kern.children[0].debug_string() == (
        "cu_fld%data(i,j) = p_fld%data(i,j) + p_fld%data(i,j + 1) + "
        "u_fld%data(i,j)\n")

    # Now remove all read accesses (this is still a valid kernel), it could
    # be just assigning a value
    ref_i = schedule.symbol_table.lookup("i")
    ref_j = schedule.symbol_table.lookup("j")
    kern.arguments._args = [kern.arguments._args[0]]
    assert kern.prototype_from_metadata(ref_i, ref_j)[0].debug_string() == (
        "cu_fld%data(i,j) = 1\n"
    )

    # Now add two write fields, this will return 2 assingments
    kern.arguments._args = [kern.arguments._args[0], kern.arguments._args[0]]
    prototype = kern.prototype_from_metadata(ref_i, ref_j)
    assert len(prototype) == 2
    assert kern.prototype_from_metadata(ref_i, ref_j)[0].debug_string() == (
        "cu_fld%data(i,j) = 1\n"
    )
    assert kern.prototype_from_metadata(ref_i, ref_j)[1].debug_string() == (
        "cu_fld%data(i,j) = 1\n"
    )

    # # Now add two write fields, this is not a valid
    kern.arguments._args = []
    with pytest.raises(InternalError) as err:
        kern.prototype_from_metadata(ref_i, ref_j)
    assert ("This is not a valid kernel, a kernel must write to at least one "
            "field" in str(err.value))


def test_gok_construction_with_large_stencils():
    '''Test that GOcean kernel information when the metadata includes
    a stencil expanding to multiple depths in multiple directions.
    '''
    _, invoke = get_invoke("large_stencil.f90", "gocean", idx=0)
    schedule = invoke.schedule

    # Get the first kernel
    kern1 = schedule.walk(GOKern)[0]

    # Check the computation prototype:
    # firstargument cu_fld is pointwise (and write only)
    # secondargument p_fld is a (100, 110, 123) stencil
    # third argument u_fld is pointwise
    assert kern1.children[0].debug_string() == (
        "cu_fld%data(i,j) = p_fld%data(i - 1,j - 1) + p_fld%data(i,j - 1) + "
        "p_fld%data(i,j) + p_fld%data(i + 1,j - 1) + p_fld%data(i + 1,j) + "
        "p_fld%data(i + 2,j) + p_fld%data(i + 1,j + 1) + "
        "p_fld%data(i + 2,j + 2) + p_fld%data(i + 3,j + 3) + "
        "u_fld%data(i,j)\n")

    vam = kern1.reference_accesses()
    assert str(vam) == ("cu_fld%data: WRITE, i: READ, j: READ, "
                        "p_fld%data: READ, u_fld%data: READ")


def test_gok_get_callees():
    '''
    Test the get_callees() method of GOKern.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "single_invoke.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kern = schedule.walk(GOKern)[0]
    assert kern._schedules is None
    scheds = kern.get_callees()
    assert isinstance(scheds, list)
    assert len(scheds) == 1
    sched = scheds[0]
    assert isinstance(sched, GOKernelSchedule)
    # A second call should just return the previously-obtained schedule.
    scheds2 = kern.get_callees()
    assert scheds2[0] is sched
    # Check that the expected error is raised if the subroutine that
    # implements the kernel cannot be found.
    kern._schedules = None
    # Remove the subroutine that implements the kernel from the Fortran
    # parse tree.
    subs = walk(kern.ast, Fortran2003.Subroutine_Subprogram)
    for sub in subs:
        if sub.children[0].children[1].string == "compute_cu_code":
            sub.parent.content.remove(sub)
            break
    with pytest.raises(GenerationError) as err:
        kern.get_callees()
    err_text = str(err.value)
    assert ("Failed to raise the PSyIR for kernel 'compute_cu_code' to GOcean "
            "PSyIR" in err_text)
    assert ("does not contain the routine that it names as implementing the "
            "kernel ('compute_cu_code')" in err_text)


# -----------------------------------------------------------------------------
def test_gok_access_info_scalar_and_property():
    '''Test GOcean kernel information when using a grid property and scalar
    variables.

    '''
    _, invoke = get_invoke("test00.1_invoke_kernel_using_const_scalar.f90",
                           "gocean", idx=0)
    schedule = invoke.schedule

    # Get the first kernel (the scalar is a literal)
    kern1 = schedule.walk(GOKern)[0]

    # Literals are ignored and properties are accesses without indices
    # (because they can only be accessed as pointwise reads anyway)
    assert kern1.children[0].debug_string() == (
        "p_fld%data(i,j) = p_fld%data(i,j) + "
        "p_fld%grid%subdomain%internal%xstop + p_fld%grid%tmask\n"
    )

    # Check that we get the grid properties listed:
    vam = kern1.reference_accesses()
    assert (str(vam) ==
            "i: READ, j: READ, p_fld%data: READ+WRITE, "
            "p_fld%grid%subdomain%internal%xstop: READ, "
            "p_fld%grid%tmask: READ")

    # Kernel calls have the whole field provided, so no indices are given
    # at this level.
    tmask = vam[Signature("p_fld%grid%tmask")]
    comp_ind = tmask[0].component_indices()
    assert comp_ind == (tuple(), tuple(), tuple())

    # In the second invoke the scalar is a symbol reference
    _, invoke = get_invoke("test00.1_invoke_kernel_using_const_scalar.f90",
                           "gocean", idx=1)
    schedule = invoke.schedule
    kern1 = schedule.walk(GOKern)[0]
    assert "real_val: READ" in str(kern1.reference_accesses())
    assert "= real_val +" in kern1.children[0].debug_string()
