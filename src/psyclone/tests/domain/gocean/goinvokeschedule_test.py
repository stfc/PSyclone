# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2022, Science and Technology Facilities Council.
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
# Authors: A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified: by J. Henrichs, Bureau of Meteorology
# Modified: R. W. Ford, STFC Daresbury Lab
# Modified: I. Kavcic, Met Office

''' pytest tests for the GOInvokeSchedule class. '''

import os

from psyclone.gocean1p0 import GOInvokeSchedule
from psyclone.parse.algorithm import parse
from psyclone.psyir.nodes import Container
from psyclone.psyGen import PSyFactory
from psyclone.tests.utilities import change_dir

API = "gocean1.0"
BASE_PATH = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__)))), "test_files", "gocean1p0")


def test_gosched_parent():
    ''' Check that the GOInvokeSchedule constructor allows the parent node
    to be supplied or omitted. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "single_invoke_two_kernels.f90"),
                           api=API)
    kcalls = invoke_info.calls[0].kcalls
    # With no parent specified
    gsched = GOInvokeSchedule("my_sched", kcalls)
    assert gsched.parent is None
    # With a parent
    cont = Container("my_mod")
    gsched = GOInvokeSchedule("my_sched", kcalls, parent=cont)
    assert gsched.parent is cont


def test_writetoread_dag(tmpdir, have_graphviz):
    ''' Test that the GOInvokeSchedule::dag() method works as expected when we
    have two kernels with a write -> read dependency '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "single_invoke_write_to_read.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    with change_dir(tmpdir):
        invoke.schedule.dag()
        if have_graphviz:
            dot_file = os.path.join(str(tmpdir), "dag")
            assert os.path.isfile(dot_file)
            with open(dot_file, "r", encoding="utf-8") as dfile:
                dot = dfile.read()
            assert dot.startswith("digraph")
            # write -> read means that the second loop can only begin once the
            # first loop is complete. Check that we have the correct forwards
            # dependence (green) and backwards dependence (red).
            assert ('"loop_[outer]_1_end" -> "loop_[outer]_20_start" '
                    '[color=red]' in dot or
                    '"loop_[outer]_1_end" -> "loop_[outer]_20_start" '
                    '[color=#ff0000]' in dot)
            assert ('"loop_[outer]_1_end" -> "loop_[outer]_20_start" '
                    '[color=green]' in dot or
                    '"loop_[outer]_1_end" -> "loop_[outer]_20_start" '
                    '[color=#00ff00]' in dot)


def test_dag(tmpdir, have_graphviz):
    ''' Test that the GOInvokeSchedule::dag() method works as expected '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "nemolite2d_alg_mod.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    with change_dir(tmpdir):
        invoke.schedule.dag()
        if have_graphviz:
            assert os.path.isfile(os.path.join(str(tmpdir), "dag.svg"))
            dot_file = os.path.join(str(tmpdir), "dag")
            assert os.path.isfile(dot_file)
            with open(dot_file, "r", encoding="utf-8") as dfile:
                dot = dfile.read()
            # The two kernels in this example are independent so we should
            # have no forwards/backwards dependencies
            for col in ["red", "#ff0000", "green", "#00ff00"]:
                assert f'[color={col}]' not in dot
