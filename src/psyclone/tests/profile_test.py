# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2019, Science and Technology Facilities Council
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
# Modified by R. W. Ford, STFC Daresbury Lab
# Modified by A. R. Porter, STFC Daresbury Lab

''' Module containing tests for generating monitoring hooks'''

from __future__ import absolute_import

import re
import pytest

from psyclone.generator import GenerationError
from psyclone.profiler import Profiler
from psyclone.psyGen import Loop, NameSpace, InternalError
from psyclone.psyir.nodes import ProfileNode
from psyclone.psyir.transformations import ProfileTrans
from psyclone.psyir.transformations import TransformationError
from psyclone.transformations import GOceanOMPLoopTrans, OMPParallelTrans
from psyclone.tests.utilities import get_invoke


# -----------------------------------------------------------------------------
def teardown_function():
    '''This function is called at the end of any test function. It disables
    any automatic profiling set. This is necessary in case of a test failure
    to make sure any further tests will not be ran with profiling enabled.
    It also creates a new NameSpace manager, which is responsible to create
    unique region names - this makes sure the test works if the order or
    number of tests run is changed, otherwise the created region names will
    change.
    '''
    Profiler.set_options([])
    Profiler._namespace = NameSpace()


def test_malformed_profile_node(monkeypatch):
    ''' Check that we raise the expected error if a ProfileNode does not have
    a single Schedule node as its child. '''
    from psyclone.psyGen import Node
    pnode = ProfileNode()
    monkeypatch.setattr(pnode, "_children", [])
    with pytest.raises(InternalError) as err:
        _ = pnode.profile_body
    assert "malformed or incomplete. It should have a " in str(err.value)
    monkeypatch.setattr(pnode, "_children", [Node(), Node()])
    with pytest.raises(InternalError) as err:
        _ = pnode.profile_body
    assert "malformed or incomplete. It should have a " in str(err.value)


# -----------------------------------------------------------------------------
def test_profile_basic(capsys):
    '''Check basic functionality: node names, schedule view.
    '''
    from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP
    Profiler.set_options([Profiler.INVOKES])
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0)
    Profiler.add_profile_nodes(invoke.schedule, Loop)

    assert isinstance(invoke.schedule[0], ProfileNode)

    invoke.schedule.view()
    out, _ = capsys.readouterr()

    gsched = colored("GOInvokeSchedule", SCHEDULE_COLOUR_MAP["Schedule"])
    sched = colored("Schedule", SCHEDULE_COLOUR_MAP["Schedule"])
    loop = Loop().coloured_name(True)
    profile = invoke.schedule[0].coloured_name(True)

    # Do one test based on schedule view, to make sure colouring
    # and indentation is correct
    expected = (
        gsched + "[invoke='invoke_0', Constant loop bounds=True]\n"
        "    0: " + profile + "[]\n"
        "        " + sched + "[]\n"
        "            0: " + loop + "[type='outer', field_space='go_cv', "
        "it_space='go_internal_pts']\n")
    assert expected in out

    prt = ProfileTrans()

    # Insert a profile call between outer and inner loop.
    # This tests that we find the subroutine node even
    # if it is not the immediate parent.
    new_sched, _ = prt.apply(invoke.schedule[0].profile_body[0].loop_body[0])

    new_sched_str = str(new_sched)
    correct = ("""GOInvokeSchedule[invoke='invoke_0', \
Constant loop bounds=True]:
ProfileStart[var=psy_data]
GOLoop[id:'', variable:'j', loop_type:'outer']
Literal[value:'2']
Literal[value:'jstop-1']
Literal[value:'1']
Schedule:
ProfileStart[var=psy_data_1]
GOLoop[id:'', variable:'i', loop_type:'inner']
Literal[value:'2']
Literal[value:'istop']
Literal[value:'1']
Schedule:
kern call: compute_cv_code
End Schedule
End GOLoop
ProfileEnd
End Schedule
End GOLoop
GOLoop[id:'', variable:'j', loop_type:'outer']
Literal[value:'1']
Literal[value:'jstop+1']
Literal[value:'1']
Schedule:
GOLoop[id:'', variable:'i', loop_type:'inner']
Literal[value:'1']
Literal[value:'istop+1']
Literal[value:'1']
Schedule:
kern call: bc_ssh_code
End Schedule
End GOLoop
End Schedule
End GOLoop
ProfileEnd
End Schedule""")
    assert correct in new_sched_str

    Profiler.set_options(None)


# -----------------------------------------------------------------------------
def test_profile_errors2():
    '''Test various error handling.'''

    with pytest.raises(GenerationError) as gen_error:
        Profiler.set_options(["invalid"])
    assert ("options must be one of 'invokes', 'kernels'"
            in str(gen_error.value))


# -----------------------------------------------------------------------------
def test_c_code_creation():
    '''Tests the handling when trying to create C code, which is not supported
    at this stage.
    '''

    profile_node = ProfileNode()
    with pytest.raises(NotImplementedError) as excinfo:
        profile_node.gen_c_code()
    assert "Generation of C code is not supported for profiling" \
        in str(excinfo.value)


# -----------------------------------------------------------------------------
def test_profile_invokes_gocean1p0():
    '''Check that an invoke is instrumented correctly
    '''
    Profiler.set_options([Profiler.INVOKES])
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0)
    Profiler.add_profile_nodes(invoke.schedule, Loop)

    # Convert the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    # First a simple test that the nesting is correct - the
    # profile regions include both loops. Note that indeed
    # the function 'compute_cv_code' is in the module file
    # kernel_ne_offset_mod.
    correct_re = ("subroutine invoke.*"
                  "use psy_data_mod, ONLY: PSyDataType.*"
                  r"TYPE\(PsyDataType\), save :: psy_data.*"
                  r"call psy_data%PreStart\(\"kernel_ne_offset_mod\", "
                  r"\"compute_cv_code\", 0, 0\).*"
                  "do j.*"
                  "do i.*"
                  "call.*"
                  "end.*"
                  "end.*"
                  r"call psy_data%PostEnd")
    assert re.search(correct_re, code, re.I) is not None

    # Check that if gen() is called more than once the same profile
    # variables and region names are created:
    code_again = str(invoke.gen()).replace("\n", "")
    assert code == code_again

    # Test that two kernels in one invoke get instrumented correctly.
    _, invoke = get_invoke("single_invoke_two_kernels.f90", "gocean1.0", 0)
    Profiler.add_profile_nodes(invoke.schedule, Loop)

    # Convert the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    correct_re = ("subroutine invoke.*"
                  "use psy_data_mod, only: PSyDataType.*"
                  r"TYPE\(PSyDataType\), save :: psy_data.*"
                  r"call psy_data%PreStart\(\"compute_cu_mod\", "
                  r"\"compute_cu_code\", 0, 0\).*"
                  "do j.*"
                  "do i.*"
                  "call.*"
                  "end.*"
                  "end.*"
                  "do j.*"
                  "do i.*"
                  "call.*"
                  "end.*"
                  "end.*"
                  r"call psy_data%PostEnd")
    assert re.search(correct_re, code, re.I) is not None
    Profiler.set_options(None)


# -----------------------------------------------------------------------------
def test_unique_region_names():
    '''Test that unique region names are created even when the kernel
    names are identical.'''

    Profiler.set_options([Profiler.KERNELS])
    _, invoke = get_invoke("single_invoke_two_identical_kernels.f90",
                           "gocean1.0", 0)
    Profiler.add_profile_nodes(invoke.schedule, Loop)

    # Convert the invoke to code, and remove all new lines, to make
    # regex matching easier

    code = str(invoke.gen()).replace("\n", "")

    # This regular expression puts the region names into groups.
    # Make sure even though the kernels have the same name, that
    # the created regions have different names. In order to be
    # flexible for future changes, we get the region names from
    # the ProfileStart calls using a regular expressions (\w*
    # being the group name enclosed in "") group. Python will store
    # those two groups and they can be accessed using the resulting
    # re object.group(n).
    correct_re = ("subroutine invoke.*"
                  "use psy_Data_mod, only: PSyDataType.*"
                  r"TYPE\(PSyDataType\), save :: psy_data.*"
                  r"TYPE\(PSyDataType\), save :: psy_data.*"
                  r"call psy_data.*%PreStart\(\"compute_cu_mod\", "
                  r"\"(\w*)\", 0, 0\).*"
                  "do j.*"
                  "do i.*"
                  "call compute_cu_code.*"
                  "end.*"
                  "end.*"
                  r"call psy_Data.*%PostEnd.*"
                  r"call psy_data.*%PreStart\(\"compute_cu_mod\", "
                  r"\"(\w*)\", 0, 0\).*"
                  "do j.*"
                  "do i.*"
                  "call compute_cu_code.*"
                  "end.*"
                  "end.*"
                  r"call psy_data.*%PostEnd")

    groups = re.search(correct_re, code, re.I)
    assert groups is not None

    # Check that the region names are indeed different: group(1)
    # is the first kernel region name crated by PSyclone, and
    # group(2) the name used in the second ProfileStart.
    # Those names must be different (otherwise the profiling tool
    # would likely combine the two different regions into one).
    assert groups.group(1) != groups.group(2)


# -----------------------------------------------------------------------------
def test_profile_kernels_gocean1p0():
    '''Check that all kernels are instrumented correctly
    '''
    Profiler.set_options([Profiler.KERNELS])
    _, invoke = get_invoke("single_invoke_two_kernels.f90", "gocean1.0",
                           idx=0)
    Profiler.add_profile_nodes(invoke.schedule, Loop)

    # Convert the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    # Test that kernel profiling works in case of two kernel calls
    # in a single invoke subroutine - i.e. we need to have one profile
    # start call before two nested loops, and one profile end call
    # after that.
    # Also note that the '.*' after compute_cu_code is necessary since
    # the name could be changed to avoid duplicates (depending on order
    # in which the tests are executed).
    correct_re = ("subroutine invoke.*"
                  "use psy_data_mod, only: PSyDataType.*"
                  r"TYPE\(PSyDataType\), save :: psy_data.*"
                  r"TYPE\(PSyDataType\), save :: psy_data.*"
                  r"call (?P<profile1>\w*)%PreStart\(\"compute_cu_mod\", "
                  r"\"compute_cu_code.*\", 0, 0\).*"
                  "do j.*"
                  "do i.*"
                  "call.*"
                  "end.*"
                  "end.*"
                  r"call (?P=profile1)%PostEnd.*"
                  r"call (?P<profile2>\w*)%PreStart\(\"time_smooth_mod\", "
                  r"\"time_smooth_code\", 0, 0\).*"
                  "do j.*"
                  "do i.*"
                  "call.*"
                  "end.*"
                  "end.*"
                  r"call (?P=profile2)%PostEnd")
    groups = re.search(correct_re, code, re.I)
    assert groups is not None
    # Check that the variables are different
    assert groups.group(1) != groups.group(2)

    Profiler.set_options(None)


# -----------------------------------------------------------------------------
def test_profile_invokes_dynamo0p3():
    '''Check that a Dynamo 0.3 invoke is instrumented correctly
    '''
    Profiler.set_options([Profiler.INVOKES])

    # First test for a single invoke with a single kernel work as expected:
    _, invoke = get_invoke("1_single_invoke.f90", "dynamo0.3", idx=0)
    Profiler.add_profile_nodes(invoke.schedule, Loop)

    # Convert the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    correct_re = ("subroutine invoke.*"
                  "use psy_data_mod, only: PSyDataType.*"
                  r"TYPE\(PSyDataType\), save :: psy_data.*"
                  r"call psy_data%PreStart\(\"testkern_mod\", "
                  r"\"testkern_code\", 0, 0\).*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  r"call psy_data%PostEnd")
    assert re.search(correct_re, code, re.I) is not None

    # Next test two kernels in one invoke:
    _, invoke = get_invoke("1.2_multi_invoke.f90", "dynamo0.3", idx=0)
    Profiler.add_profile_nodes(invoke.schedule, Loop)
    # Convert the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    # The .* after testkern_code is necessary since the name can be changed
    # by PSyclone to avoid name duplications.
    correct_re = ("subroutine invoke.*"
                  "use psy_data_mod, only: PSyDataType.*"
                  r"TYPE\(PSyDataType\), save :: psy_data.*"
                  r"call psy_data%PreStart\(\"testkern_mod\", "
                  "\"testkern_code.*\","
                  r" 0, 0\).*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  r"call psy_data%PostEnd")
    assert re.search(correct_re, code, re.I) is not None

    # Lastly, test an invoke whose first kernel is a builtin
    _, invoke = get_invoke("15.1.1_X_plus_Y_builtin.f90", "dynamo0.3", idx=0)
    Profiler.add_profile_nodes(invoke.schedule, Loop)
    code = str(invoke.gen())
    assert "USE psy_data_mod, ONLY: PSyDataType" in code
    assert "TYPE(PSyDataType), save :: psy_data" in code
    assert "CALL psy_data%PreStart(\"unknown-module\", \"x_plus_y\", 0, 0)" \
        in code
    assert "CALL psy_data%PostEnd" in code

    Profiler.set_options(None)


# -----------------------------------------------------------------------------
def test_profile_kernels_dynamo0p3():
    '''Check that all kernels are instrumented correctly in a
    Dynamo 0.3 invoke.
    '''
    Profiler.set_options([Profiler.KERNELS])
    _, invoke = get_invoke("1_single_invoke.f90", "dynamo0.3", idx=0)
    Profiler.add_profile_nodes(invoke.schedule, Loop)

    # Convert the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    correct_re = ("subroutine invoke.*"
                  "use psy_data_mod, only: PSyDataType.*"
                  r"TYPE\(PSyDataType\), save :: psy_data.*"
                  r"call psy_data%PreStart\(\"testkern_mod\", "
                  r"\"testkern_code.*\", 0, 0\).*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  r"call psy_data%PostEnd")
    assert re.search(correct_re, code, re.I) is not None

    _, invoke = get_invoke("1.2_multi_invoke.f90", "dynamo0.3", idx=0)
    Profiler.add_profile_nodes(invoke.schedule, Loop)

    # Convert the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    correct_re = ("subroutine invoke.*"
                  "use psy_data_mod, only: PSyDataType.*"
                  r"TYPE\(PSyDataType\), save :: psy_data.*"
                  r"TYPE\(PSyDataType\), save :: psy_data.*"
                  r"call (?P<profile1>\w*)%PreStart\(\"testkern_mod\", "
                  r"\"testkern_code.*\", 0, 0\).*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  r"call (?P=profile1)%PostEnd.*"
                  r"call (?P<profile2>\w*)%PreStart\(.*, .*, 0, 0\).*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  r"call (?P=profile2)%PostEnd")
    groups = re.search(correct_re, code, re.I)
    assert groups is not None
    # Check that the variables are different
    assert groups.group(1) != groups.group(2)
    Profiler.set_options(None)


# -----------------------------------------------------------------------------
def test_transform(capsys):
    '''Tests normal behaviour of profile region transformation.'''

    _, invoke = get_invoke("test27_loop_swap.f90", "gocean1.0",
                           name="invoke_loop1")
    schedule = invoke.schedule

    prt = ProfileTrans()
    assert str(prt) == "Insert a profile start and end call."
    assert prt.name == "ProfileTrans"

    # Try applying it to a list
    sched1, _ = prt.apply(schedule.children)

    correct = ("""GOInvokeSchedule[invoke='invoke_loop1', \
Constant loop bounds=True]:
ProfileStart[var=psy_data]
GOLoop[id:'', variable:'j', loop_type:'outer']
Literal[value:'2']
Literal[value:'jstop']
Literal[value:'1']
Schedule:
GOLoop[id:'', variable:'i', loop_type:'inner']
Literal[value:'2']
Literal[value:'istop']
Literal[value:'1']
Schedule:
kern call: bc_ssh_code
End Schedule
End GOLoop
End Schedule
End GOLoop
GOLoop[id:'', variable:'j', loop_type:'outer']
Literal[value:'1']
Literal[value:'jstop+1']
Literal[value:'1']
Schedule:
GOLoop[id:'', variable:'i', loop_type:'inner']
Literal[value:'1']
Literal[value:'istop']
Literal[value:'1']
Schedule:
kern call: bc_solid_u_code
End Schedule
End GOLoop
End Schedule
End GOLoop
GOLoop[id:'', variable:'j', loop_type:'outer']
Literal[value:'1']
Literal[value:'jstop']
Literal[value:'1']
Schedule:
GOLoop[id:'', variable:'i', loop_type:'inner']
Literal[value:'1']
Literal[value:'istop+1']
Literal[value:'1']
Schedule:
kern call: bc_solid_v_code
End Schedule
End GOLoop
End Schedule
End GOLoop
ProfileEnd
End Schedule""")
    assert correct in str(sched1)

    # Now only wrap a single node - the middle loop:
    sched2, _ = prt.apply(schedule[0].profile_body[1])

    correct = ("""GOInvokeSchedule[invoke='invoke_loop1', \
Constant loop bounds=True]:
ProfileStart[var=psy_data]
GOLoop[id:'', variable:'j', loop_type:'outer']
Literal[value:'2']
Literal[value:'jstop']
Literal[value:'1']
Schedule:
GOLoop[id:'', variable:'i', loop_type:'inner']
Literal[value:'2']
Literal[value:'istop']
Literal[value:'1']
Schedule:
kern call: bc_ssh_code
End Schedule
End GOLoop
End Schedule
End GOLoop
ProfileStart[var=psy_data_1]
GOLoop[id:'', variable:'j', loop_type:'outer']
Literal[value:'1']
Literal[value:'jstop+1']
Literal[value:'1']
Schedule:
GOLoop[id:'', variable:'i', loop_type:'inner']
Literal[value:'1']
Literal[value:'istop']
Literal[value:'1']
Schedule:
kern call: bc_solid_u_code
End Schedule
End GOLoop
End Schedule
End GOLoop
ProfileEnd
GOLoop[id:'', variable:'j', loop_type:'outer']
Literal[value:'1']
Literal[value:'jstop']
Literal[value:'1']
Schedule:
GOLoop[id:'', variable:'i', loop_type:'inner']
Literal[value:'1']
Literal[value:'istop+1']
Literal[value:'1']
Schedule:
kern call: bc_solid_v_code
End Schedule
End GOLoop
End Schedule
End GOLoop
ProfileEnd
End Schedule""")
    assert correct in str(sched2)

    # Check that a sublist created from individual elements
    # can be wrapped
    sched3, _ = prt.apply([sched2[0].profile_body[0],
                           sched2[0].profile_body[1]])
    sched3.view()
    out, _ = capsys.readouterr()

    from psyclone.psyGen import SCHEDULE_COLOUR_MAP, colored
    gsched = colored("GOInvokeSchedule", SCHEDULE_COLOUR_MAP["Schedule"])
    prof = colored("Profile", SCHEDULE_COLOUR_MAP["Profile"])
    sched = colored("Schedule", SCHEDULE_COLOUR_MAP["Schedule"])
    loop = colored("Loop", SCHEDULE_COLOUR_MAP["Loop"])

    indent = 4*" "
    correct = (gsched+"[invoke='invoke_loop1', Constant loop bounds=True]\n" +
               indent + "0: " + prof + "[]\n" +
               2*indent + sched + "[]\n" +
               3*indent + "0: " + prof + "[]\n" +
               4*indent + sched + "[]\n" +
               5*indent + "0: " + loop + "[type='outer', field_space='go_ct',"
               " it_space='go_internal_pts']\n")
    assert correct in out
    correct2 = (5*indent + "1: " + prof + "[]\n" +
                6*indent + sched + "[]\n" +
                7*indent + "0: " + loop + "[type='outer', field_space='go_cu',"
                " it_space='go_all_pts']\n")
    assert correct2 in out


# -----------------------------------------------------------------------------
def test_transform_errors(capsys):
    '''Tests error handling of the profile region transformation.'''

    # This has been imported and tested before, so we can assume
    # here that this all works as expected/
    _, invoke = get_invoke("test27_loop_swap.f90", "gocean1.0",
                           name="invoke_loop1")

    schedule = invoke.schedule
    prt = ProfileTrans()

    with pytest.raises(TransformationError) as excinfo:
        prt.apply([schedule.children[0].children[0], schedule.children[1]])
    assert "supplied nodes are not children of the same parent." \
           in str(excinfo.value)

    # Supply not a node object:
    with pytest.raises(TransformationError) as excinfo:
        prt.apply(5)
    assert "Argument must be a single Node in a schedule or a list of Nodes " \
           "in a schedule but have been passed an object of type: " \
           in str(excinfo.value)
    # Python 3 reports 'class', python 2 'type' - so just check for both
    assert ("<type 'int'>" in str(excinfo.value) or "<class 'int'>"
            in str(excinfo.value))

    # Test that it will only allow correctly ordered nodes:
    with pytest.raises(TransformationError) as excinfo:
        sched1, _ = prt.apply([schedule.children[1], schedule.children[0]])
    assert "Children are not consecutive children of one parent:" \
           in str(excinfo.value)

    with pytest.raises(TransformationError) as excinfo:
        sched1, _ = prt.apply([schedule.children[0], schedule.children[2]])
    assert "Children are not consecutive children of one parent:" \
           in str(excinfo.value)

    # Test 3 element lists: first various incorrect ordering:
    with pytest.raises(TransformationError) as excinfo:
        sched1, _ = prt.apply([schedule.children[0],
                               schedule.children[2],
                               schedule.children[1]])
    assert "Children are not consecutive children of one parent:" \
           in str(excinfo.value)

    with pytest.raises(TransformationError) as excinfo:
        sched1, _ = prt.apply([schedule.children[1],
                               schedule.children[0],
                               schedule.children[2]])
    assert "Children are not consecutive children of one parent:" \
           in str(excinfo.value)

    # Just to be sure: also check that the right order does indeed work!
    sched1, _ = prt.apply([schedule.children[0],
                           schedule.children[1],
                           schedule.children[2]])
    sched1.view()
    out, _ = capsys.readouterr()
    # out is unicode, and has no replace function, so convert to string first
    out = str(out).replace("\n", "")

    correct_re = (".*GOInvokeSchedule.*"
                  r"    .*Profile.*"
                  r"        .*Loop.*\[type='outer'.*"
                  r"        .*Loop.*\[type='outer'.*"
                  r"        .*Loop.*\[type='outer'.*")
    assert re.search(correct_re, out)

    # Test that we don't add a profile node inside a OMP do loop (which
    # would be invalid syntax):
    _, invoke = get_invoke("test27_loop_swap.f90", "gocean1.0",
                           name="invoke_loop1")
    schedule = invoke.schedule

    prt = ProfileTrans()
    omp_loop = GOceanOMPLoopTrans()

    # Parallelise the first loop:
    sched1, _ = omp_loop.apply(schedule[0])

    # Inserting a ProfileRegion inside a omp do loop is syntactically
    # incorrect, the inner part must be a do loop only:
    with pytest.raises(TransformationError) as excinfo:
        prt.apply(sched1[0].dir_body[0])

    assert "A ProfileNode cannot be inserted between an OpenMP/ACC directive "\
           "and the loop(s) to which it applies!" in str(excinfo.value)


# -----------------------------------------------------------------------------
def test_omp_transform():
    '''Tests that the profiling transform works correctly with OMP
     parallelisation.'''

    _, invoke = get_invoke("test27_loop_swap.f90", "gocean1.0",
                           name="invoke_loop1")
    schedule = invoke.schedule

    prt = ProfileTrans()
    omp_loop = GOceanOMPLoopTrans()
    omp_par = OMPParallelTrans()

    # Parallelise the first loop:
    sched1, _ = omp_loop.apply(schedule[0])
    sched2, _ = omp_par.apply(sched1[0])
    sched3, _ = prt.apply(sched2[0])

    correct = (
        "      CALL psy_data%PreStart(\"boundary_conditions_ne_offset_mod\", "
        "\"bc_ssh_code\", 0, 0)\n"
        "      !$omp parallel default(shared), private(i,j)\n"
        "      !$omp do schedule(static)\n"
        "      DO j=2,jstop\n"
        "        DO i=2,istop\n"
        "          CALL bc_ssh_code(i, j, 1, t%data, t%grid%tmask)\n"
        "        END DO \n"
        "      END DO \n"
        "      !$omp end do\n"
        "      !$omp end parallel\n"
        "      CALL psy_data%PostEnd")
    code = str(invoke.gen())
    assert correct in code

    # Now add another profile node between the omp parallel and omp do
    # directives:
    sched3, _ = prt.apply(sched3[0].profile_body[0].dir_body[0])

    code = str(invoke.gen())

    correct = \
        "CALL psy_data%PreStart(\"boundary_conditions_ne_offset_mod\", " + \
        '''"bc_ssh_code", 0, 0)
      !$omp parallel default(shared), private(i,j)
      CALL psy_data_1%PreStart("boundary_conditions_ne_offset_mod", ''' + \
        '''"bc_ssh_code_1", 0, 0)
      !$omp do schedule(static)
      DO j=2,jstop
        DO i=2,istop
          CALL bc_ssh_code(i, j, 1, t%data, t%grid%tmask)
        END DO 
      END DO 
      !$omp end do
      CALL psy_data_1%PostEnd
      !$omp end parallel
      CALL psy_data%PostEnd'''

    assert correct in code
