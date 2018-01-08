# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2018, Science and Technology Facilities Council
#
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
# Author: A. R. Porter, STFC Daresbury Lab

''' Tests for the CLAW interface implemented in PSyclone '''

import os
import pytest
import utils
from psyclone.transformations import TransformationError

# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "..", "test_files")

@utils.CLAW
def test_loop_reorder(tmpdir, monkeypatch):
    ''' Tests that we can use CLAW to perform a loop reordering '''
    from psyclone.parse import parse
    from psyclone.psyGen import PSyFactory
    from psyclone import claw
    _, invoke_info = parse(os.path.join(BASE_PATH, "dynamo0p3",
                                        "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    kern = invoke.schedule.children[0].children[0]
    orig_name = kern.name[:]
    script_file = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "claw_trans.py")
    # Change to the pytest-supplied tmp dir so that we don't mess up our
    # space with generated files
    _ = tmpdir.chdir()

    new_names = claw.trans([kern], script_file)
    assert new_names[orig_name] == "testkern_claw0_code"

