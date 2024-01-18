# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2024, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab

''' Module containing py.test tests for the construction of a PSy
    representation of NEMO code '''


import os
import pytest
from fparser.common.readfortran import FortranStringReader
from psyclone.psyGen import PSyFactory, InlinedKern
from psyclone.errors import InternalError
from psyclone.tests.utilities import get_invoke
from psyclone import nemo
from psyclone.psyir.nodes import Assignment, IfBlock, Literal, Loop, \
    Reference, Schedule
from psyclone.psyir.nodes.node import colored


# Constants
API = "nemo"
# Location of the Fortran files associated with these tests
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files")


def test_unamed_unit(parser):
    '''
    Test that we raise the expected internal error if we fail to find
    a name for the PSy object.
    '''
    code = ("program simple\n"
            "end program\n")
    reader = FortranStringReader(code)
    prog = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(prog)
    assert psy._name == "simple_psy"
    # We have to work quite hard to trigger the internal error that is
    # raised when no names are found.
    # Delete most of the AST
    main_prog = prog.content[0]
    del main_prog.content[1:]
    # Remove the Name object from the items tuple of the remaining AST node
    main_prog.content[0].items = (main_prog.content[0].items[0], "not a Name")
    with pytest.raises(InternalError) as err:
        _ = PSyFactory(API, distributed_memory=False).create(prog)
    assert "Found no names in supplied Fortran" in str(err.value)


def test_explicit_do_sched():
    ''' Check that we generate a correct schedule for a triply-nested,
    explicit do loop '''
    psy, _ = get_invoke("explicit_do.f90", api=API, idx=0)
    assert isinstance(psy, nemo.NemoPSy)
    assert psy._name == "explicit_do_psy"
    invoke = psy.invokes.invoke_list[0]
    sched = invoke.schedule
    # The schedule should contain 3 loop objects
    loops = sched.walk(nemo.NemoLoop)
    assert len(loops) == 3
    # The schedule should contain just assignments
    assert isinstance(loops[2].loop_body[0], Assignment)


def test_array_valued_function():
    ''' Check that we handle array notation used when there is no implicit
    loop. '''
    _, invoke_info = get_invoke("array_valued_function.f90", api=API, idx=0)
    sched = invoke_info.schedule
    # We should just have two assignments and no Kernels
    assert len(sched.children) == 2
    assert isinstance(sched.children[0], Assignment)
    assert isinstance(sched.children[1], Assignment)


def test_complex_code():
    ''' Check that we get the right schedule when the code contains
    multiple statements of different types '''
    _, invoke_info = get_invoke("code_block.f90", api=API, idx=0)
    sched = invoke_info.schedule
    loops = sched.walk(nemo.NemoLoop)
    assert len(loops) == 4
    kerns = sched.coded_kernels()
    assert len(kerns) == 0


def test_schedule_view():
    ''' Check the schedule view/str methods work as expected '''
    _, invoke_info = get_invoke("io_in_loop.f90", api=API, idx=0)
    sched = invoke_info.schedule
    sched_str = str(sched)
    assert "NemoLoop[variable:'ji', loop_type:'lon']" in sched_str
    assert "NemoLoop[variable:'jj', loop_type:'lat']" in sched_str
    assert "NemoLoop[variable:'jk', loop_type:'levels']" in sched_str
    output = sched.view()

    # Have to allow for colouring of output text
    loop_str = colored("Loop", Loop._colour)
    assign_str = colored("Assignment", Assignment._colour)
    isched_str = colored("NemoInvokeSchedule", nemo.NemoInvokeSchedule._colour)
    sched_str = colored("Schedule", Schedule._colour)
    lit_str = colored("Literal", Literal._colour)
    ref_str = colored("Reference", Reference._colour)
    indent = "    "

    expected_sched = (
        isched_str + "[invoke='io_in_loop']\n" +
        indent + "0: " + loop_str + "[type='levels', field_space='None', "
        "it_space='None']\n" +
        2*indent + lit_str + "[value:'1', Scalar<INTEGER, "
        "UNDEFINED>]\n" +
        2*indent + ref_str + "[name:'jpk']\n" +
        2*indent + lit_str + "[value:'1', Scalar<INTEGER, "
        "UNDEFINED>]\n" +
        2*indent + sched_str + "[]\n" +
        3*indent + "0: " + loop_str + "[type='lat', field_space='None', "
        "it_space='None']\n" +
        4*indent + lit_str + "[value:'1', Scalar<INTEGER, "
        "UNDEFINED>]\n" +
        4*indent + ref_str + "[name:'jpj']\n" +
        4*indent + lit_str + "[value:'1', Scalar<INTEGER, "
        "UNDEFINED>]\n" +
        4*indent + sched_str + "[]\n" +
        5*indent + "0: " + loop_str + "[type='lon', "
        "field_space='None', it_space='None']\n" +
        6*indent + lit_str + "[value:'1', Scalar<INTEGER, "
        "UNDEFINED>]\n" +
        6*indent + ref_str + "[name:'jpi']\n" +
        6*indent + lit_str + "[value:'1', Scalar<INTEGER, "
        "UNDEFINED>]\n" +
        6*indent + sched_str + "[]\n" +
        7*indent + "0: " + assign_str + "[]\n")
    assert expected_sched in output


def test_empty_routine():
    ''' Check that we handle the case where a program unit does not
    contain any executable statements. '''
    psy, _ = get_invoke("empty_routine.f90", api=API, idx=0)
    assert len(psy.invokes.invoke_list) == 1
    # We should just have an empty schedule
    assert not psy.invokes.invoke_list[0].schedule.children


def test_invoke_function():
    ''' Check that we successfully construct an Invoke if the program
    unit is a function. '''
    psy, _ = get_invoke("afunction.f90", api=API, idx=0)
    assert len(psy.invokes.invoke_list) == 1
    invoke = psy.invokes.invoke_list[0]
    assert invoke.name == "afunction"
