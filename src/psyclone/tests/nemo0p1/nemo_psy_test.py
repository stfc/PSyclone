# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2018, Science and Technology Facilities Council.
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
import fparser
import pytest
import psyclone
from psyclone.parse import parse, ParseError
from psyclone.psyGen import PSyFactory
from psyclone import nemo0p1


# Constants
API = "nemo0.1"
# Location of the Fortran files associated with these tests
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files")

def test_explicit_do_sched():
    ''' Check that we generate a correct schedule for a triply-nested,
    explicit do loop '''
    ast, invoke_info = parse(os.path.join(BASE_PATH, "explicit_do.f90"),
                             api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    assert isinstance(psy, nemo0p1.NemoPSy)
    invoke = psy.invokes.invoke_list[0]
    sched = invoke.schedule
    # The schedule should contain 3 loop objects
    loops = sched.walk(sched.children, nemo0p1.NemoLoop)
    assert len(loops) == 3
    # The schedule should contain just 1 kernel
    assert isinstance(loops[2].children[0], nemo0p1.NemoKern)


def test_implicit_loop_sched1():
    ''' Check that we get the correct schedule for an implicit loop '''
    ast, invoke_info = parse(os.path.join(BASE_PATH, "implicit_do.f90"),
                             api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    assert isinstance(psy, nemo0p1.NemoPSy)
    print(len(psy.invokes.invoke_list))
    sched = psy.invokes.invoke_list[0].schedule
    sched.view()
    loops = sched.walk(sched.children, nemo0p1.NemoLoop)
    assert len(loops) == 3
    kerns = sched.walk(sched.children, nemo0p1.NemoKern)
    assert len(kerns) == 1


def test_implicit_loop_sched2():
    ''' Check that we get the correct schedule for an explicit loop over
    levels containing an implicit loop over the i-j slab '''
    ast, invoke_info = parse(os.path.join(BASE_PATH,
                                          "explicit_over_implicit.f90"),
                             api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    sched.view()
    # We should have 3 loops (one from the explicit loop over levels and
    # the other two from the implicit loops over ji and jj).
    loops = sched.walk(sched.children, nemo0p1.NemoLoop)
    assert len(loops) == 3
    kerns = sched.walk(sched.children, nemo0p1.NemoKern)
    assert len(kerns) == 1


def test_implicit_loop_assign():
    ''' Check that we only identify an implicit loop when array syntax
    is used as part of an assignment statement. '''
    ast, invoke_info = parse(os.path.join(BASE_PATH, "array_syntax.f90"),
                             api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    loops = sched.walk(sched.children, nemo0p1.NemoLoop)
    sched.view()
    print(ast)
    # Our implicit loop gives us 3 explicit loops
    assert len(loops) == 3
    assert isinstance(sched.children[0], nemo0p1.NemoLoop)
    # The other statements (that use array syntax) are not assignments
    # and therefore are not implicit loops
    assert isinstance(sched.children[1], nemo0p1.NemoCodeBlock)


def test_codeblock():
    ''' Check that we get the right schedule when the code contains
    some unrecognised statements as well as both an explict and an
    implicit loop. '''
    ast, invoke_info = parse(os.path.join(BASE_PATH, "code_block.f90"),
                             api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    loops = sched.walk(sched.children, nemo0p1.NemoLoop)
    assert len(loops) == 4
    cblocks = sched.walk(sched.children, nemo0p1.NemoCodeBlock)
    assert len(cblocks) == 3
    kerns = sched.walk(sched.children, nemo0p1.NemoKern)
    assert len(kerns) == 2


def test_io_not_kernel():
    ''' Check that we reject a kernel candidate if a loop body contains
    a write/read statement '''
    ast, invoke_info = parse(os.path.join(BASE_PATH, "io_in_loop.f90"),
                             api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    # We should have only 1 actual kernel and 2 code blocks
    cblocks = sched.walk(sched.children, nemo0p1.NemoCodeBlock)
    assert len(cblocks) == 2
    kerns = sched.walk(sched.children, nemo0p1.NemoKern)
    assert len(kerns) == 1


def test_schedule_view(capsys):
    ''' Check the schedule view/str methods work as expected '''
    from psyclone.psyGen import colored
    from psyclone.nemo0p1 import NEMO_SCHEDULE_COLOUR_MAP
    ast, invoke_info = parse(os.path.join(BASE_PATH, "io_in_loop.f90"),
                             api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    sched_str = str(sched)
    print(sched_str)
    assert "CodeBlock[2 statements]" in sched_str
    assert "NemoLoop[levels]: jk=1,jpk,1" in sched_str
    assert "NemoLoop[lat]: jj=1,jpj,1" in sched_str
    assert "NemoLoop[lon]: ji=1,jpi,1" in sched_str
    sched.view()
    output, _ = capsys.readouterr()

    # Have to allow for colouring of output text
    loop_str = colored("Loop", NEMO_SCHEDULE_COLOUR_MAP["Loop"])
    cb_str = colored("NemoCodeBlock", NEMO_SCHEDULE_COLOUR_MAP["CodeBlock"])
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
        "                " + kern_str + "[]\n"
        "    " + loop_str + "[type='levels',field_space='None',"
        "it_space='None']\n"
        "        " + loop_str + "[type='lat',field_space='None',"
        "it_space='None']\n"
        "            " + loop_str + "[type='lon',field_space='None',"
        "it_space='None']\n"
        "                " + cb_str + "[<class 'fparser.two.Fortran2003."
        "Assignment_Stmt'>]\n"
        "    " + loop_str + "[type='levels',field_space='None',"
        "it_space='None']\n"
        "        " + loop_str + "[type='lat',field_space='None',"
        "it_space='None']\n"
        "            " + loop_str + "[type='lon',field_space='None',"
        "it_space='None']\n"
        "                " + cb_str + "[<class 'fparser.two.Fortran2003."
        "Assignment_Stmt'>]")
    assert expected_sched in output


def test_kern_inside_if():
    ''' Check that we identify kernels when they are within an if block. '''
    ast, invoke_info = parse(os.path.join(BASE_PATH, "imperfect_nest.f90"),
                             api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    sched.view()
    kerns = sched.walk(sched.children, nemo0p1.NemoKern)
    assert len(kerns) == 5
    assert isinstance(sched.children[0].children[1], nemo0p1.NemoIfBlock)
    assert isinstance(sched.children[0].children[1].children[1],
                      nemo0p1.NemoIfClause)
