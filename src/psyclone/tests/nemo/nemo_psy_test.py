# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

''' Module containing py.test tests for the construction of a PSy
    representation of NEMO code '''


from __future__ import print_function, absolute_import
import os
import pytest
from psyclone.parse import parse
from psyclone.psyGen import PSyFactory, InternalError, GenerationError, \
    CodeBlock
from psyclone import nemo
from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.parser import ParserFactory
from fparser.two.utils import walk_ast

# Constants
API = "nemo"
# Location of the Fortran files associated with these tests
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files")
# Fortran parser
_PARSER = ParserFactory().create()


def test_unamed_unit():
    '''
    Test that we raise the expected internal error if we fail to find
    a name for the PSy object.
    '''
    code = ("program simple\n"
            "  ztmp(:,:,:) = 0.0\n"
            "end program\n")
    reader = FortranStringReader(code)
    prog = Fortran2003.Program_Unit(reader)
    psy = PSyFactory(API, distributed_memory=False).create(prog)
    assert psy._name == "simple_psy"
    # We have to work quite hard to trigger the internal error that is
    # raised when no names are found.
    # Delete most of the AST
    del prog.content[1:]
    # Remove the Name object from the items tuple of the remaining AST node
    prog.content[0].items = (prog.content[0].items[0], "not a Name")
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


def test_implicit_loop_sched1():
    ''' Check that we get the correct schedule for an implicit loop '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "implicit_do.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    assert isinstance(psy, nemo.NemoPSy)
    print(len(psy.invokes.invoke_list))
    sched = psy.invokes.invoke_list[0].schedule
    sched.view()
    loops = sched.walk(sched.children, nemo.NemoLoop)
    assert len(loops) == 3
    kerns = sched.kern_calls()
    assert len(kerns) == 1


def test_implicit_loop_sched2():
    ''' Check that we get the correct schedule for an explicit loop over
    levels containing an implicit loop over the i-j slab '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "explicit_over_implicit.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    sched.view()
    # We should have 3 loops (one from the explicit loop over levels and
    # the other two from the implicit loops over ji and jj).
    loops = sched.walk(sched.children, nemo.NemoLoop)
    assert len(loops) == 3
    kerns = sched.kern_calls()
    assert len(kerns) == 1


def test_multi_kern():
    ''' Test that having multiple kernels within a single loop raises
    the expected error. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "code_block.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    loops = sched.walk(sched.children, nemo.NemoLoop)
    kerns = sched.kern_calls()
    # Add the second kernel as a child of the first loop
    loops[0].children.append(kerns[1])
    with pytest.raises(NotImplementedError) as err:
        _ = loops[0].kernel
    assert ("getter method does not yet support a loop containing more than "
            "one kernel but this loop contains 2" in str(err))


@pytest.mark.xfail(reason="Do not currently check for previous variable"
                   "declarations when adding loop variables")
def test_implicit_loop_assign():
    ''' Check that we only identify an implicit loop when array syntax
    is used as part of an assignment statement. '''
    ast, invoke_info = parse(os.path.join(BASE_PATH, "array_syntax.f90"),
                             api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    loops = sched.walk(sched.children, nemo.NemoLoop)
    sched.view()
    gen = str(ast).lower()
    print(gen)
    # Our implicit loops gives us 5 explicit loops
    assert len(loops) == 5
    assert isinstance(sched.children[0], nemo.NemoLoop)
    # The other statements (that use array syntax) are not assignments
    # and therefore are not implicit loops
    assert not(isinstance(sched.children[1], nemo.NemoLoop))
    # Check that the loop variables have been declared just once
    for var in ["psy_ji", "psy_jj", "psy_jk"]:
        assert gen.count("integer :: {0}".format(var)) == 1


def test_unrecognised_implicit():
    ''' Check that we raise the expected error if we encounter an
    unrecognised form of implicit loop. '''
    from psyclone.nemo import NemoImplicitLoop, NemoInvoke
    # Array syntax used in an unsupported index location
    reader = FortranStringReader("umask(:, :, :, :) = 0.0D0")
    assign = Fortran2003.Assignment_Stmt(reader)
    with pytest.raises(GenerationError) as err:
        NemoImplicitLoop(assign)
    assert ("Array section in unsupported dimension (4) for code "
            "'umask(:, :, :, :) = 0.0D0'" in str(err))
    # and now for the case where the Program unit doesn't have a
    # specification section to modify. This is hard to trigger
    # so we manually construct some objects and put them together
    # to create an artificial example...
    reader = FortranStringReader("umask(:, :, :) = 0.0D0")
    assign = Fortran2003.Assignment_Stmt(reader)
    reader = FortranStringReader("program atest\nreal :: umask(1,1,1,1)\n"
                                 "umask(:, :, :) = 0.0\nend program atest")
    prog = Fortran2003.Program_Unit(reader)
    invoke = NemoInvoke(prog, name="atest")
    loop = NemoImplicitLoop.__new__(NemoImplicitLoop)
    loop._parent = None
    loop.invoke = invoke
    loop.root.invoke._ast = prog
    spec = walk_ast(prog.content, [Fortran2003.Specification_Part])
    prog.content.remove(spec[0])
    with pytest.raises(InternalError) as err:
        loop.__init__(assign)
    assert "No specification part found for routine atest" in str(err)


def test_implicit_range_err():
    ''' Check that we raise the expected error if we encounter an implicit
    loop with an explicit range (since we don't yet support that). '''
    # Array syntax with an explicit range
    reader = FortranStringReader("umask(1:jpi, 1, :) = 0.0D0")
    assign = Fortran2003.Assignment_Stmt(reader)
    with pytest.raises(NotImplementedError) as err:
        nemo.NemoImplicitLoop(assign)
    assert ("Support for implicit loops with specified bounds is not yet "
            "implemented: 'umask(1 : jpi, 1, :) = 0.0D0'" in str(err))


def test_complex_code():
    ''' Check that we get the right schedule when the code contains
    multiple statements of different types '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "code_block.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    loops = sched.walk(sched.children, nemo.NemoLoop)
    assert len(loops) == 5
    cblocks = sched.walk(sched.children, CodeBlock)
    assert len(cblocks) == 4
    kerns = sched.kern_calls()
    assert len(kerns) == 2
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
    kerns = sched.kern_calls()
    assert len(kerns) == 1


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
    kern_str = colored("KernCall", NEMO_SCHEDULE_COLOUR_MAP["KernCall"])
    sched_str = colored("Schedule", NEMO_SCHEDULE_COLOUR_MAP["Schedule"])

    expected_sched = (
        sched_str + "[]\n"
        "    " + loop_str + "[type='levels',field_space='None',"
        "it_space='None']\n"
        "        " + loop_str + "[type='lat',field_space='None',"
        "it_space='None']\n"
        "            " + loop_str + "[type='lon',field_space='None',"
        "it_space='None']\n"
        "                " + kern_str + "[Explicit]\n"
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
    _, invoke_info = parse(os.path.join(BASE_PATH, "imperfect_nest.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    kerns = sched.kern_calls()
    assert len(kerns) == 6
    ifblock = sched.children[0].children[1]
    assert isinstance(ifblock, nemo.NemoIfBlock)
    assert str(ifblock) == "If-block: jk == 1"
    assert isinstance(ifblock.children[1], nemo.NemoIfClause)
    assert isinstance(ifblock.children[2], nemo.NemoIfClause)


def test_invalid_if_clause():
    ''' Check that we raise the expected error if the NemoIfClause
    is passed something that isn't an if-clause. '''
    from psyclone.nemo import NemoIfClause
    reader = FortranStringReader("umask(:, :, :, :) = 0")
    assign = Fortran2003.Assignment_Stmt(reader)
    with pytest.raises(InternalError) as err:
        _ = NemoIfClause([assign])
    assert "Unrecognised member of if block: " in str(err)


def test_ifblock_ast_err():
    ''' Check that the NemoIfBlock constructor raises the expected error if
    the fparser2 AST does not have the expected structure. '''
    reader = FortranStringReader("if(.true.)then\ndone=.true.\nend if\n")
    ifblock = Fortran2003.If_Construct(reader)
    # Break the AST projected by fparser2 - delete the 'end if'
    del ifblock.content[-1]
    with pytest.raises(InternalError) as err:
        _ = nemo.NemoIfBlock(ifblock)
    assert "Failed to find closing end if" in str(err)
    # Now delete the 'if then'
    del ifblock.content[0]
    with pytest.raises(InternalError) as err:
        _ = nemo.NemoIfBlock(ifblock)
    assert "Failed to find opening if then" in str(err)


def test_ifblock_gencode_err():
    ''' Check that calling gen_code() on a NemoIfBlock raises an error. '''
    reader = FortranStringReader("if(.true.)then\ndone=.true.\nend if\n")
    ifblock = Fortran2003.If_Construct(reader)
    blk = nemo.NemoIfBlock(ifblock)
    with pytest.raises(InternalError) as err:
        blk.gen_code()
    assert "this method should not have been called" in str(err)


def test_elseif_gencode_err():
    ''' Check that calling gen_code() on a NemoIfClause raises an error.
    Generating a valid NemoIfClause artificially is difficult so we resort
    to parsing a full routine. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "imperfect_nest.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    clauses = sched.walk(sched.children, nemo.NemoIfClause)
    with pytest.raises(InternalError) as err:
        clauses[0].gen_code()
    assert "This method should not have been called" in str(err)


def test_kern_load_errors(monkeypatch):
    ''' Check that the various load methods of the NemoKern class raise
    the expected errors. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "explicit_do.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    sched = invoke.schedule
    # The schedule should contain 3 loop objects
    kerns = sched.kern_calls()
    with pytest.raises(InternalError) as err:
        kerns[0].load("Not an fparser2 AST node")
    assert ("internal error: Expecting either Block_Nonlabel_Do_Construct "
            "or Assignment_Stmt but got " in str(err))
    # TODO why haven't the Kernel or Loop objects got a valid _ast?
    loop = sched.children[0].children[0].children[0]._ast
    monkeypatch.setattr(loop, "content", ["not_a_loop"])
    with pytest.raises(InternalError) as err:
        kerns[0]._load_from_loop(loop)
    assert ("Expecting Nonlabel_Do_Stmt as first child of "
            "Block_Nonlabel_Do_Construct but got" in str(err))


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
