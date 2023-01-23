# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2023, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab

''' Module containing py.test tests for the construction of a PSy
    representation of NEMO code '''


import os
import pytest
from fparser.common.readfortran import FortranStringReader
from psyclone.psyGen import PSyFactory, InlinedKern
from psyclone.errors import InternalError
from psyclone.tests.utilities import get_invoke
from psyclone import nemo
from psyclone.psyir.nodes import Assignment, CodeBlock, IfBlock, Loop, \
    Schedule, Literal, Reference
from psyclone.psyir.nodes.node import colored


# Constants
API = "nemo"
# Location of the Fortran files associated with these tests
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files")


def test_no_gen_code():
    '''Test that we raise an exception if gen_code is called
    for a NemoKern.'''
    kern = nemo.NemoKern([])
    with pytest.raises(InternalError) as err:
        kern.gen_code(None)
    assert "NEMO kernels are assumed to be in-lined by default therefore " \
           "the gen_code method should not have been called." in str(err.value)


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
    # The schedule should contain just 1 kernel
    assert isinstance(loops[2].loop_body[0], nemo.NemoKern)


def test_array_valued_function():
    ''' Check that we handle array notation used when there is no implicit
    loop. '''
    _, invoke_info = get_invoke("array_valued_function.f90", api=API, idx=0)
    sched = invoke_info.schedule
    assert len(sched.children) == 2
    # We should just have two assignments and no Kernels
    kernels = sched.walk(nemo.NemoKern)
    assert not kernels


def test_do_while():
    ''' Check that do-while loops are put into CodeBlocks. Eventually we
    will need to recognise them as Nodes in the Schedule in their
    own right. '''

    _, invoke_info = get_invoke("do_while.f90", api=API, idx=0)
    sched = invoke_info.schedule
    # Do while loops are not currently handled and thus are put into
    # CodeBlocks.
    assert isinstance(sched[1], CodeBlock)
    assert isinstance(sched[2], Assignment)
    assert isinstance(sched[4], CodeBlock)


def test_multi_kern():
    ''' Test that having multiple kernels within a single loop raises
    the expected error. '''
    _, invoke_info = get_invoke("two_explicit_do.f90", api=API, idx=0)
    sched = invoke_info.schedule
    loops = sched.walk(nemo.NemoLoop)
    # Create and add a second kernel as a child of the first loop
    kern = nemo.NemoKern([])
    loops[0].loop_body.children.append(kern)
    with pytest.raises(NotImplementedError) as err:
        _ = loops[0].kernel
    assert ("getter method does not yet support a loop containing more than "
            "one kernel but this loop contains 2" in str(err.value))


def test_complex_code():
    ''' Check that we get the right schedule when the code contains
    multiple statements of different types '''
    _, invoke_info = get_invoke("code_block.f90", api=API, idx=0)
    sched = invoke_info.schedule
    loops = sched.walk(nemo.NemoLoop)
    assert len(loops) == 4
    kerns = sched.coded_kernels()
    assert len(kerns) == 1
    # The last loop does not contain a kernel
    assert loops[-1].kernel is None


def test_io_not_kernel():
    ''' Check that we reject a kernel candidate if a loop body contains
    a write/read statement '''
    _, invoke_info = get_invoke("io_in_loop.f90", api=API, idx=0)
    sched = invoke_info.schedule
    # We should have only 1 actual kernel
    kerns = sched.coded_kernels()
    assert len(kerns) == 1


def test_fn_call_no_kernel(parser):
    ''' Check that we don't create a kernel if the loop body contains a
    function call. '''
    reader = FortranStringReader("program fn_call\n"
                                 "integer, parameter :: wp = kind(1.0)\n"
                                 "integer :: ji, jpj\n"
                                 "real(kind=wp) :: sto_tmp(5)\n"
                                 "do ji = 1,jpj\n"
                                 "sto_tmp(ji) = my_func()\n"
                                 "end do\n"
                                 "end program fn_call\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[0]
    assert isinstance(loop, nemo.NemoLoop)
    # Child of loop should be an Assignment, not a Kernel.
    assert isinstance(loop.loop_body[0], Assignment)


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
    kern_str = colored("InlinedKern", InlinedKern._colour)
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
        7*indent + "0: " + kern_str + "[]\n")
    assert expected_sched in output


def test_kern_inside_if():
    ''' Check that we identify kernels when they are within an if block. '''
    _, invoke_info = get_invoke("imperfect_nest.f90", api=API, idx=0)
    sched = invoke_info.schedule
    kerns = sched.coded_kernels()
    assert len(kerns) == 4
    ifblock = sched.children[0].loop_body[1]
    assert isinstance(ifblock, IfBlock)
    assert isinstance(ifblock.else_body[0], IfBlock)
    kernels = ifblock.else_body.walk(nemo.NemoKern)
    assert isinstance(kernels[0], nemo.NemoKern)


def test_kern_sched_parents(parser):
    ''' Check that the children of a Kernel schedule have that schedule
    as their parent. '''
    reader = FortranStringReader("program fake_kern\n"
                                 "integer, parameter :: wp = kind(1.0)\n"
                                 "integer :: ji, jj, jpi, jpj\n"
                                 "real(kind=wp) :: sto_tmp(5,5)\n"
                                 "do ji = 1,jpi\n"
                                 "  do jj = 1,jpj\n"
                                 "    sto_tmp(ji,jj) = 1.0\n"
                                 "    sto_tmp(ji,jj) = 2.0*sto_tmp(ji,jj)\n"
                                 "    if(jj == 1)then\n"
                                 "      sto_tmp(ji,jj) = sto_tmp(ji,jj)-1.0\n"
                                 "    end if\n"
                                 "  end do\n"
                                 "end do\n"
                                 "end program fake_kern\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    # Find the (one and only) kernel
    kernels = schedule.walk(nemo.NemoKern)
    assert len(kernels) == 1
    # Get its schedule
    sched = kernels[0].get_kernel_schedule()
    # Check that the children of the schedule have it as their parent
    for child in sched.children:
        assert child.parent is sched


def test_no_inline():
    ''' Check that calling the NemoPSy.inline() method raises the expected
    error (since we haven't implemented it yet). '''
    psy, _ = get_invoke("imperfect_nest.f90", api=API, idx=0)
    with pytest.raises(NotImplementedError) as err:
        psy.inline(None)
    assert ("The NemoPSy.inline method has not yet been implemented!"
            in str(err.value))


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
