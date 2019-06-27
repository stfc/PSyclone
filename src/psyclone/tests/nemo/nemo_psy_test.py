# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2019, Science and Technology Facilities Council.
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


from __future__ import print_function, absolute_import
import os
import pytest
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory, InternalError
from psyclone import nemo
from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003

# Constants
API = "nemo"
# Location of the Fortran files associated with these tests
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files")

DO_WHILE_PROG = ("program do_while_prog\n"
                 "integer :: my_sum\n"
                 "my_sum = 0\n"
                 "do while(.true.)\n"
                 "  my_sum = my_sum + 1\n"
                 "end do\n"
                 "end program do_while_prog\n")


def test_unamed_unit(parser):
    '''
    Test that we raise the expected internal error if we fail to find
    a name for the PSy object.
    '''
    code = ("program simple\n"
            "  ztmp(:,:,:) = 0.0\n"
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
    assert "Found no names in supplied Fortran" in str(err)


def test_explicit_do_sched():
    ''' Check that we generate a correct schedule for a triply-nested,
    explicit do loop '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "explicit_do.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    assert isinstance(psy, nemo.NemoPSy)
    assert psy._name == "explicit_do_psy"
    invoke = psy.invokes.invoke_list[0]
    sched = invoke.schedule
    # The schedule should contain 3 loop objects
    loops = sched.walk(sched.children, nemo.NemoLoop)
    assert len(loops) == 3
    # The schedule should contain just 1 kernel
    assert isinstance(loops[2].children[0], nemo.NemoKern)


def test_array_valued_function():
    ''' Check that we handle array notation used when there is no implicit
    loop. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "array_valued_function.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    assert len(sched.children) == 2
    # We should just have two assignments and no Kernels
    kernels = sched.walk(sched.children, nemo.NemoKern)
    assert not kernels


def test_do_while():
    ''' Check that do-while loops are put into CodeBlocks. Eventually we
    will need to recognise them as Nodes in the Schedule in their
    own right. '''
    from psyclone.psyGen import CodeBlock
    _, invoke_info = parse(os.path.join(BASE_PATH, "do_while.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    # Do while loops are not currently handled and thus are put into
    # CodeBlocks.
    assert isinstance(sched.children[0], CodeBlock)
    assert isinstance(sched.children[1], nemo.NemoLoop)
    assert isinstance(sched.children[3], CodeBlock)


def test_reject_dowhile(parser):
    ''' Check that the NemoLoop constructor rejects DO WHILE loops. '''
    from fparser.two.utils import walk_ast
    reader = FortranStringReader(DO_WHILE_PROG)
    prog = parser(reader)
    loops = walk_ast([prog], [Fortran2003.Block_Nonlabel_Do_Construct])
    with pytest.raises(InternalError) as err:
        _ = nemo.NemoLoop(loops[0])
    assert ("NemoLoop constructor should not have been called for a DO WHILE"
            in str(err))


def test_missing_loop_control(parser):
    ''' Check that encountering a loop in the fparser parse tree that is
    missing a Loop_Control element raises an InternalError. '''
    from fparser.two.utils import walk_ast
    reader = FortranStringReader(DO_WHILE_PROG)
    prog = parser(reader)
    # We have to break the fparser2 parse tree in order to trigger the
    # internal error
    loops = walk_ast(prog.content,
                     [Fortran2003.Nonlabel_Do_Stmt])
    ctrl = walk_ast(loops[0].items, [Fortran2003.Loop_Control])
    # 'items' is a tuple and therefore immutable so make a new list
    item_list = list(loops[0].items)
    # Create a new tuple for the items member without the Loop_Control
    item_list.remove(ctrl[0])
    loops[0].items = tuple(item_list)
    with pytest.raises(InternalError) as err:
        _ = PSyFactory(API, distributed_memory=False).create(prog)
    assert ("Unrecognised form of DO loop - failed to find Loop_Control "
            "element in parse tree" in str(err))


def test_multi_kern():
    ''' Test that having multiple kernels within a single loop raises
    the expected error. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "two_explicit_do.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    loops = sched.walk(sched.children, nemo.NemoLoop)
    kerns = sched.coded_kernels()
    # Add the second kernel as a child of the first loop
    loops[0].children.append(kerns[1])
    with pytest.raises(NotImplementedError) as err:
        _ = loops[0].kernel
    assert ("getter method does not yet support a loop containing more than "
            "one kernel but this loop contains 2" in str(err))


def test_implicit_loop_assign():
    ''' Check that we only identify an implicit loop when array syntax
    is used as part of an assignment statement. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "array_syntax.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    loops = sched.walk(sched.children, nemo.NemoLoop)
    # We should have three implicit loops
    assert len(loops) == 3
    assert isinstance(sched.children[0], nemo.NemoLoop)
    # Check the __str__ property of the implicit loop
    txt = str(sched.children[0])
    assert "NemoImplicitLoop[zftv(:, :, :)]" in txt
    # The other statements (that use array syntax) are not assignments
    # and therefore are not implicit loops
    assert not isinstance(sched.children[1], nemo.NemoLoop)


def test_complex_code():
    ''' Check that we get the right schedule when the code contains
    multiple statements of different types '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "code_block.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    loops = sched.walk(sched.children, nemo.NemoLoop)
    assert len(loops) == 5
    kerns = sched.coded_kernels()
    assert len(kerns) == 1
    # The last loop does not contain a kernel
    assert loops[-1].kernel is None


def test_io_not_kernel():
    ''' Check that we reject a kernel candidate if a loop body contains
    a write/read statement '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "io_in_loop.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    # We should have only 1 actual kernel
    kerns = sched.coded_kernels()
    assert len(kerns) == 1


def test_fn_call_no_kernel(parser):
    ''' Check that we don't create a kernel if the loop body contains a
    function call. '''
    from psyclone.psyGen import Assignment
    reader = FortranStringReader("program fn_call\n"
                                 "real(kind=wp) :: sto_tmp(5)\n"
                                 "do ji = 1,jpj\n"
                                 "sto_tmp(ji) = my_func()\n"
                                 "end do\n"
                                 "end program fn_call\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    schedule.view()
    loop = schedule.children[0]
    assert isinstance(loop, nemo.NemoLoop)
    # Child of loop should be an Assignment, not a Kernel.
    assert isinstance(loop.children[0], Assignment)


def test_codeblock_no_kernel(parser, monkeypatch):
    ''' Check that we don't create a kernel if the loop body contains a
    CodeBlock. '''
    from psyclone.psyGen import CodeBlock
    reader = FortranStringReader("program fake_kern\n"
                                 "real(kind=wp) :: sto_tmp(5)\n"
                                 "do ji = 1,jpj\n"
                                 "sto_tmp(ji) = 1.0\n"
                                 "end do\n"
                                 "end program fake_kern\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[0]
    # Check that we have the expected structure
    assert isinstance(loop, nemo.NemoLoop)
    assert nemo.NemoKern.match(loop)
    # Create a fake CodeBlock
    cblock = CodeBlock([loop.children[0].ast])
    # Monkeypatch the Loop object so that it has a CodeBlock as a child
    monkeypatch.setattr(loop, "children", [cblock])
    # This should no longer match as a NemoKern
    assert not nemo.NemoKern.match(loop)


def test_no_explicit_loop_in_kernel(parser):
    ''' Check that NemoKern.match() does not match a candidate parse tree
    if it includes an explicit loop. '''
    reader = FortranStringReader("program fake_kern\n"
                                 "real(kind=wp) :: sto_tmp(5)\n"
                                 "do ji = 1,jpj\n"
                                 "  do idx = 1, 5\n"
                                 "    sto_tmp(ji) = 1.0\n"
                                 "  end do\n"
                                 "end do\n"
                                 "end program fake_kern\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[0]
    assert isinstance(loop, nemo.NemoLoop)
    assert isinstance(loop.children[0], nemo.NemoLoop)
    # 'loop' is not a valid kernel because it itself contains a loop
    assert not nemo.NemoKern.match(loop)


def test_no_implicit_loop_in_kernel(parser):
    ''' Check that NemoKern.match() does not match a candidate parse tree
    if it includes an implicit loop. '''
    reader = FortranStringReader("program fake_kern\n"
                                 "real(kind=wp) :: sto_tmp(5,5)\n"
                                 "do ji = 1,jpj\n"
                                 "  sto_tmp(:,:) = 1.0\n"
                                 "end do\n"
                                 "end program fake_kern\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[0]
    assert isinstance(loop, nemo.NemoLoop)
    assert isinstance(loop.children[0], nemo.NemoLoop)
    # 'loop' is not a valid kernel because it itself contains a loop
    assert not nemo.NemoKern.match(loop)


def test_schedule_view(capsys):
    ''' Check the schedule view/str methods work as expected '''
    from psyclone.psyGen import colored
    from psyclone.nemo import NEMO_SCHEDULE_COLOUR_MAP
    _, invoke_info = parse(os.path.join(BASE_PATH, "io_in_loop.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    sched_str = str(sched)
    assert "NemoLoop[levels]: jk=1,jpk,1" in sched_str
    assert "NemoLoop[lat]: jj=1,jpj,1" in sched_str
    assert "NemoLoop[lon]: ji=1,jpi,1" in sched_str
    sched.view()
    output, _ = capsys.readouterr()

    # Have to allow for colouring of output text
    loop_str = colored("Loop", NEMO_SCHEDULE_COLOUR_MAP["Loop"])
    kern_str = colored("CodedKern", NEMO_SCHEDULE_COLOUR_MAP["CodedKern"])
    sched_str = colored("InvokeSchedule", NEMO_SCHEDULE_COLOUR_MAP["Schedule"])

    expected_sched = (
        sched_str + "[]\n"
        "    " + loop_str + "[type='levels',field_space='None',"
        "it_space='None']\n"
        "        " + loop_str + "[type='lat',field_space='None',"
        "it_space='None']\n"
        "            " + loop_str + "[type='lon',field_space='None',"
        "it_space='None']\n"
        "                " + kern_str + "[]\n"
        "    " + loop_str + "[type='levels',field_space='None',"
        "it_space='None']\n"
        "        " + loop_str + "[type='lat',field_space='None',"
        "it_space='None']\n"
        "            " + loop_str + "[type='lon',field_space='None',"
        "it_space='None']\n"
        "                ")
    assert expected_sched in output
    expected_sched2 = (
        loop_str + "[type='levels',field_space='None',"
        "it_space='None']\n"
        "        " + loop_str + "[type='lat',field_space='None',"
        "it_space='None']\n"
        "            " + loop_str + "[type='lon',field_space='None',"
        "it_space='None']\n"
        "                ")
    assert expected_sched2 in output


def test_kern_inside_if():
    ''' Check that we identify kernels when they are within an if block. '''
    from psyclone.psyGen import IfBlock
    _, invoke_info = parse(os.path.join(BASE_PATH, "imperfect_nest.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    kerns = sched.coded_kernels()
    assert len(kerns) == 4
    ifblock = sched.children[0].children[1]
    assert isinstance(ifblock, IfBlock)
    assert isinstance(ifblock.else_body[0], IfBlock)
    kernels = ifblock.walk(ifblock.else_body, nemo.NemoKern)
    assert isinstance(kernels[0], nemo.NemoKern)


def test_kern_sched_parents(parser):
    ''' Check that the children of a Kernel schedule have that schedule
    as their parent. '''
    reader = FortranStringReader("program fake_kern\n"
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
    kernels = schedule.walk(schedule.children, nemo.NemoKern)
    assert len(kernels) == 1
    # Get its schedule
    sched = kernels[0].get_kernel_schedule()
    # Check that the children of the schedule have it as their parent
    for child in sched.children:
        assert child.parent is sched


def test_no_inline():
    ''' Check that calling the NemoPSy.inline() method raises the expected
    error (since we haven't implemented it yet). '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "imperfect_nest.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    with pytest.raises(NotImplementedError) as err:
        psy.inline(None)
    assert ("The NemoPSy.inline method has not yet been implemented!"
            in str(err))


def test_empty_routine():
    ''' Check that we handle the case where a program unit does not
    contain any executable statements. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "empty_routine.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    assert len(psy.invokes.invoke_list) == 1
    assert psy.invokes.invoke_list[0].schedule is None
    # Calling update() on this Invoke should do nothing
    psy.invokes.invoke_list[0].update()


def test_invoke_function():
    ''' Check that we successfully construct an Invoke if the program
    unit is a function. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "afunction.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    assert len(psy.invokes.invoke_list) == 1
    invoke = psy.invokes.invoke_list[0]
    assert invoke.name == "afunction"
