# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council
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

from psyclone.psyir.nodes import PSyDataNode
from psyclone.psyir.transformations import PSyDataTrans
from psyclone.psyGen import Loop
from psyclone.tests.utilities import get_invoke


# -----------------------------------------------------------------------------
def test_psy_data_trans_basic(capsys):
    # pylint: disable=too-many-locals
    '''Check basic functionality: node names, schedule view.
    '''
    from psyclone.psyir.nodes.node import colored, SCHEDULE_COLOUR_MAP
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0)
    schedule = invoke.schedule

    data_trans = PSyDataTrans()
    assert "Insert a PSyData node" in str(data_trans)
    assert data_trans.name == "PSyDataTrans"
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
def test_class_definitions():
    '''Tests if the class-prefix can be set and behaves as expected.
    '''

    psy, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                             "gocean1.0", idx=0)
    schedule = invoke.schedule

    data_trans = PSyDataTrans()
    data_trans.apply(schedule)
    code = str(psy.gen)

    # By default, no prefix should be used:
    assert "TYPE(PSyDataType), target, save :: psy_data" in code
    assert "CALL psy_data" in code

    # This puts the new PSyDataNode with prefix "extract" around the
    # previous PSyDataNode, but the prefix was not used previously.
    data_trans.apply(schedule, {"class": "extract"})
    code = str(psy.gen)
    assert "TYPE(extract_PSyDataType), target, save :: extract_psy_data" \
        in code
    assert "CALL extract_psy_data" in code
    # The old call must still be there (e.g. not somehow be changed
    # by setting the prefix)
    assert "TYPE(PSyDataType), target, save :: psy_data" in code
    assert "CALL psy_data" in code
