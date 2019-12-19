# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council
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
# Author J. Henrichs, Bureau of Meteorology

''' Module containing tests for generating PSyData hooks'''

from __future__ import absolute_import

import re
import pytest

from psyclone.psyir.nodes import PSyDataNode
from psyclone.psyir.transformations import PSyDataTrans
from psyclone.psyGen import Loop, NameSpace
from psyclone.tests.utilities import get_invoke


@pytest.fixture(scope="function", autouse=True)
def clear_psydata_namespace():
    '''This function is called at the before any test function. It
    creates a new NameSpace manager, which is responsible to create
    unique region names - this makes sure the test works if the order
    or number of tests run is changed, otherwise the created region
    names will change.'''
    PSyDataNode._namespace = NameSpace()


# -----------------------------------------------------------------------------
def test_psy_data_basic(capsys):
    # pylint: disable=too-many-locals
    '''Check basic functionality: node names, schedule view.
    '''
    from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0)
    schedule = invoke.schedule

    data_trans = PSyDataTrans()
    data_trans.apply(schedule)

    assert isinstance(invoke.schedule[0], PSyDataNode)

    schedule.view()
    out, _ = capsys.readouterr()

    gsched = colored("GOInvokeSchedule", SCHEDULE_COLOUR_MAP["Schedule"])
    sched = colored("Schedule", SCHEDULE_COLOUR_MAP["Schedule"])
    loop = Loop().coloured_name(True)
    data = invoke.schedule[0].coloured_name(True)

    # Do one test based on schedule view, to make sure colouring
    # and indentation is correct
    expected = (
        gsched + "[invoke='invoke_0', Constant loop bounds=True]\n"
        "    0: " + data + "[]\n"
        "        " + sched + "[]\n"
        "            0: " + loop + "[type='outer', field_space='go_cv', "
        "it_space='go_internal_pts']\n")
    assert expected in out

    # Insert a DataTrans call between outer and inner loop.
    # This tests that we find the subroutine node even
    # if it is not the immediate parent.
    new_sched, _ = data_trans.apply(invoke.schedule[0].psy_data_body[0]
                                    .loop_body[0])

    new_sched_str = str(new_sched)
    correct = ("""GOInvokeSchedule[invoke='invoke_0', \
Constant loop bounds=True]:
PSyDataStart[var=psy_data]
GOLoop[id:'', variable:'j', loop_type:'outer']
Literal[value:'2', DataType.INTEGER]
Literal[value:'jstop-1', DataType.INTEGER]
Literal[value:'1', DataType.INTEGER]
Schedule:
PSyDataStart[var=psy_data_1]
GOLoop[id:'', variable:'i', loop_type:'inner']
Literal[value:'2', DataType.INTEGER]
Literal[value:'istop', DataType.INTEGER]
Literal[value:'1', DataType.INTEGER]
Schedule:
kern call: compute_cv_code
End Schedule
End GOLoop
PSyDataEnd[var=psy_data_1]
End Schedule
End GOLoop
GOLoop[id:'', variable:'j', loop_type:'outer']
Literal[value:'1', DataType.INTEGER]
Literal[value:'jstop+1', DataType.INTEGER]
Literal[value:'1', DataType.INTEGER]
Schedule:
GOLoop[id:'', variable:'i', loop_type:'inner']
Literal[value:'1', DataType.INTEGER]
Literal[value:'istop+1', DataType.INTEGER]
Literal[value:'1', DataType.INTEGER]
Schedule:
kern call: bc_ssh_code
End Schedule
End GOLoop
End Schedule
End GOLoop
PSyDataEnd[var=psy_data]
End Schedule""")

    assert correct in new_sched_str


# -----------------------------------------------------------------------------
def test_c_code_creation():
    '''Tests the handling when trying to create C code, which is not supported
    at this stage.
    '''

    data_node = PSyDataNode()
    with pytest.raises(NotImplementedError) as excinfo:
        data_node.gen_c_code()
    assert "Generation of C code is not supported for PSyDataNode" \
        in str(excinfo)


# -----------------------------------------------------------------------------
def test_psy_data_invokes_gocean1p0():
    '''Check that an invoke is instrumented correctly
    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0)
    schedule = invoke.schedule
    data_trans = PSyDataTrans()

    data_trans.apply(schedule[0])

    code = invoke.gen()
    print(code)

    # Convert the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")
    # First a simple test that the nesting is correct - the
    # PSyData regions include both loops. Note that indeed
    # the function 'compute_cv_code' is in the module file
    # kernel_ne_offset_mod.
    # Since this is only PSyData, which by default does not supply
    # variable inforation, the parameters to PreStart are both 0.
    correct_re = ("subroutine invoke.*"
                  "use psy_data_mod, only: PSyDataType.*"
                  r"TYPE\(PSyDataType\), save :: psy_data.*"
                  r"call psy_data%PreStart\(\"kernel_ne_offset_mod\", "
                  r"\"compute_cv_code\", 0, 0\).*"
                  "do j.*"
                  "do i.*"
                  "call.*"
                  "end.*"
                  "end.*"
                  r"call psy_data%PostEnd")
    assert re.search(correct_re, code, re.I) is not None

    # Check that if gen() is called more than once the same PSyDataNode
    # variables and region names are created:
    code_again = str(invoke.gen()).replace("\n", "")
    assert code == code_again
