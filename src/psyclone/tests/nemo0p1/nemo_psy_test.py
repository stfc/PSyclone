# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017, Science and Technology Facilities Council
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2016.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
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
# Authors: R. Ford and A. R. Porter, STFC Daresbury Lab

''' Module containing py.test tests for the construction of a PSy
    representation of NEMO code '''


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
    assert isinstance(psy, nemo0p1.NEMOPSy)
    invoke = psy.invokes.invoke_list[0]
    sched = invoke.schedule
    # The schedule should contain 3 loop objects
    loops = sched.walk(sched.children, nemo0p1.NEMOLoop)
    assert len(loops) == 3
    # The schedule should contain just 1 kernel
    assert isinstance(loops[2].children[0], nemo0p1.NEMOKern)


@pytest.mark.xfail(reason="We do not yet create the Loop objects for an "
                   "implicit loop")
def test_implicit_loop_sched():
    ''' Check that we get the correct schedule for an implicit loop '''
    ast, invoke_info = parse(os.path.join(BASE_PATH, "implicit_do.f90"),
                             api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    assert isinstance(psy, nemo0p1.NEMOPSy)
    print len(psy.invokes.invoke_list)
    sched = psy.invokes.invoke_list[0].schedule
    sched.view()
    loops = sched.walk(sched.children, nemo0p1.NEMOLoop)
    assert len(loops) == 3
    kerns = sched.walk(sched.children, nemo0p1.NEMOKern)
    assert len(kerns) == 1


@pytest.mark.xfail(reason="Loop objects not yet created for implicit loops")
def test_implicit_loop_sched():
    ''' Check that we get the correct schedule for an explicit loop over
    levels containing an implicit loop over the i-j slab '''
    ast, invoke_info = parse(os.path.join(BASE_PATH,
                                          "explicit_over_implicit.f90"),
                             api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    loops = sched.walk(sched.children, nemo0p1.NEMOLoop)
    assert len(loops) == 3
    kerns = sched.walk(sched.children, nemo0p1.NEMOKern)
    assert len(kerns) == 1


def test_codeblock():
    ''' Check that we get the right schedule when the code contains
    some unrecognised statements as well as a loop '''
    ast, invoke_info = parse(os.path.join(BASE_PATH, "code_block.f90"),
                             api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    loops = sched.walk(sched.children, nemo0p1.NEMOLoop)
    assert len(loops) == 3
    cblocks = sched.walk(sched.children, nemo0p1.NEMOCodeBlock)
    assert len(cblocks) == 2
    assert 0
